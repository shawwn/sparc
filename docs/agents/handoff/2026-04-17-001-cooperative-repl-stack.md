# Handoff: Cooperative REPL stack (`interact` + `dbg` share stdin)

**Date:** 2026-04-17
**Branch:** master
**Files touched:** `ac.scm`, `arc.arc`, `dbg.arc`, `test.arc`

## Why

Before this change, `(dbg)` inside a request handler entered `debugger` on a
background thread while the main thread was blocked in Racket's
`read-eval-print-loop`. Two prompts (`arc> ` and `> `) raced for the terminal;
whichever `read` won got the user's next line — usually the wrong one. Ctrl-C
could tear down the REPL because SIGINT was uncaught on the main thread.

## Architecture

**One dispatcher thread owns stdin. No REPL reads stdin directly.**

- `ar-repl` struct (in `ac.scm`) — each REPL is a record with
  `(channel done interrupt prompt thread label)`.
- `ar-repl-stack*` — list; top is the currently active REPL. Guarded by
  `ar-repl-stack-sema`. Push/pop also post `ar-repl-stack-change` so the
  dispatcher can notice via `semaphore-peek-evt`.
- `ar-dispatcher-loop` — single long-lived thread. Each iteration:
  1. Wait until the top REPL signals `done` (i.e., its previous eval has
     finished) or the stack changes.
  2. Print the top's prompt.
  3. Spawn a short-lived reader thread that does one `sread` and posts the
     form to a local channel.
  4. `sync` on `(form-ready | stack-changed | interrupted)`.
  5. Deliver via synchronous `channel-put` and loop.
- `ar-run-repl label prompt eval-fn` — driver every REPL uses. Pushes a
  record, `channel-get`s forms, calls `eval-fn` on each, loops until EOF or
  `(quit)`. Pops via `dynamic-wind` (exception-safe).

`interact` and `debugger` are both thin wrappers over `ar-run-repl`. The old
`tl2` / `ac-read-interaction` / `ac-prompt-read` / Racket
`read-eval-print-loop` path is gone.

## Subtleties worth knowing

### Two semaphores per REPL, both load-bearing

- **`done`** (count 1 at creation) — the dispatcher consumes it before
  prompting. A `dynamic-wind` cleanup around each eval re-posts it on normal
  return or caught break. **It is NOT posted on the quit-tag escape path**
  (see below). Without `done` the dispatcher would race ahead and print the
  next prompt before eval finished — the original symptom was
  `arc> 3\narc> ` collapsing to `arc> 3arc> `.
- **`interrupt`** (count 0) — posted by the eval thread's `exn:break`
  handler when a break hits at the idle prompt. The dispatcher syncs on it
  to kill the in-flight reader and re-prompt. Without `interrupt`, Ctrl-C
  was caught by the eval thread but the reader was still parked in `sread`,
  so nothing visible changed.

### The `quitting` flag (avoids a stale-top race)

When a REPL calls `(repl-quit)`, the raise unwinds through two
`dynamic-wind`s:

1. Inner (around eval) — if it reposted `done`, the dispatcher's sync would
   see `done` fire for a REPL that's about to be popped, consume its done,
   and print its prompt one more time before noticing the stack change.
2. Outer — `ar-repl-pop!` + stack-change post.

So the quit-tag handler sets `quitting = #t`, and the inner
`dynamic-wind` skips the `done` post when `quitting` is true. Only
`stack-change` fires, and the dispatcher loops cleanly to the new top. The
visible symptom without the flag was a stray `> ` appearing after `'c` in a
`(dbg)` before the outer `arc> ` came back.

### Arc eval-fns get datums, not syntax

`sread` → `read-syntax` → syntax objects. `ar-interact-eval` (Scheme)
handles syntax because `ac` accepts either. **But Arc-level code like
`dbg-eval-fn` does `(is expr ''c)` and that fails on a syntax-wrapped
symbol.** The xdef `run-repl` wraps its Arc eval-fn with
`(if (syntax? expr) (syntax->datum expr) expr)` so Arc never sees syntax
objects. If you add another eval-fn written in Scheme that wants syntax,
call `ar-run-repl` directly (like `interact` does), not the xdef.

### `(quit)` is REPL-aware

`(xdef quit ...)` now branches: empty stack → `exit`; non-empty → `raise
ar-repl-quit-tag`. Inside a `(dbg)`, `(quit)` pops just the debugger.
Inside interact with no dbg active, `(quit)` pops interact → `as.scm`
returns → process exits naturally. `(quit)` inside a loaded file (no
REPL running) still calls `exit`.

### `display-error` does NOT take the stdout lock

Errors in `dbg-eval-fn` propagate through `(on-err tlerr ...)` which is
*already inside* `w/stdout-lock`. The `display-error` xdef delegates to
Racket's `error-display-handler` but does not acquire `ar-stdout-sema`
because it's not reentrant — doing so deadlocked the REPL when the first
error fired inside `(dbg)`. The caller is responsible for holding the
lock. `ar-interact-eval` acquires the lock inside its own `exn:fail?`
handler around its `error-display-handler` call.

### `set-original-ports` (in `arc.arc`)

`ar-run-repl` parameterises `current-output-port` / `current-error-port` to
the terminal ports so a `(dbg)` fired from a request handler (whose
dynamic ports are pointing at a socket) still writes to the terminal.
`arc.arc` captures `original-stdout*`/`original-stderr*` and pushes them
into `ar-original-stdout` / `ar-original-stderr` via `set-original-ports`.
If you find debugger output going to a socket, this wiring is the first
place to check.

### Piped stdin stays on the old path

`(interact)` checks `(terminal-port? (current-input-port))`. If false, it
falls through to `aload1` — the dispatcher is never started. All the
stack machinery is terminal-only.

## What is NOT solved (acceptable v1 paper cuts)

- **Partial-input loss on push.** If you're mid-line (`(+ 1 `) when a new
  REPL is pushed (e.g. a `(dbg)` fires from another thread), the bytes you
  typed are lost. The user sees a fresh prompt. Fix later by draining the
  input buffer and echoing onto the new prompt line.
- **Log lines can still scroll the prompt.** The stdout lock is only held
  around prompt-print and REPL result-print, not across eval or server log
  lines. Verbose `srv-noisy*` logging will still push prompts up the
  screen; it just won't visually split a prompt mid-word. Wrap
  `noisy-header` et al. if you need full isolation.
- **A tiny break-between-handlers window in `ar-run-repl`.** Between the
  outer `with-handlers` (around `channel-get`) and the inner one (around
  `ar-apply eval-fn`), an `exn:break` could land uncaught. Very hard to
  hit in practice; if it becomes real, wrap both in a single outer break
  handler or use `break-enabled` around the transition.

## Verification

- `720 passed, 0 failed` in `make test` (includes a new `define-test
  repl-stack` covering push / pop-by-identity / idempotent pop).
- PTY smoke tests driven from Python (`pty.fork` + `select`):
  - prompt now follows eval result (`arc> 3\narc> `, not `arc> 3arc> `)
  - `(+ 1` then Ctrl-C → `^C` + fresh `arc> `, next form evals normally
  - `(require "dbg.arc") (let x 42 (dbg))` → `> 'h` reprints locals,
    `> 'c` returns to `arc>`, `> (quit)` pops dbg (outer arc> continues),
    final `(quit)` exits
  - `(car 99)` inside dbg prints full Racket stack trace via
    `error-display-handler`

Not automated (need a real TTY or interactive user):

- Server-on-main flip (`srv.arc:24-27`) — `interact` in a thread, `serving`
  on main. The forwarder is ready but I didn't make the flip; it's a
  drop-in change if wanted.
- Multi-level nested `(dbg)` inside `(dbg)` — the stack handles it and the
  test covers push/pop semantics, but live-typing the scenario is worth
  doing if this area changes again.

## Files at a glance

- `ac.scm` — everything dispatcher, `ar-run-repl`, `xdef run-repl`,
  `xdef repl-push/pop/top/quit`, `xdef with-stdout-lock`,
  `xdef display-error`, `xdef set-original-ports`, `xdef ensure-dispatcher`,
  `interact` wrapper, `ar-interact-eval`.
- `arc.arc` — calls `set-original-ports` after `original-stdout*` is set.
- `dbg.arc` — `tlread` deleted; `debugger` delegates to `run-repl`;
  `tlerr` uses `display-error`; new `w/stdout-lock` macro.
- `test.arc` — `define-test repl-stack`.
