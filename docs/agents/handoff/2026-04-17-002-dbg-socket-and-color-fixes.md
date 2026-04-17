# Handoff: `(dbg)` preamble routing + prnblue/prnred color fix

**Date:** 2026-04-17
**Branch:** master
**Files touched:** `dbg.arc`

Two follow-up fixes after the cooperative REPL stack landed (see
`2026-04-17-001-cooperative-repl-stack.md`). Both were reported from live
testing against the news server.

## Fix 1: `(dbg)` preamble wrote to the request socket

### Symptom

A request handler that called `(dbg)` printed the "locals: ... type 'h ..."
preamble into the HTTP response body instead of the terminal. Only the
*interactive* `> ` prompt landed on the terminal.

### Cause

`srv.arc` handlers run inside `w/stdout str` / `w/stdout sock` — their
`current-output-port` parameter points at a string port or socket. `run-repl`
rebinds the port to `original-stdout*` internally, but only once the REPL
loop starts. The `debugger` function prints its preamble *before* calling
`run-repl`, so the preamble still writes to whatever the caller had bound.

Separately, `debugger`'s `o`/`i` optional args (used by `dbg-prexpr` /
`dbg-eval-fn` to route retexpr-result printing) defaulted to `(stdout)` /
`(stdin)` — the dynamic parameters — so even after `run-repl` took over, the
retexpr printer would have fallen back to the socket if the defaults had
been captured earlier.

### Fix (`dbg.arc:178`, `dbg.arc:193-197`)

- Defaults: `(o o (stdout)) (o i (stdin))` → `(o o original-stdout*) (o i original-stdin*)`.
- Preamble: wrap the `dbg-prn` call in `(w/stdout original-stdout* ...)` so
  the locals printout is forced to the terminal regardless of the caller's
  dynamic port.

`run-repl` continues to handle the REPL loop's port rebinding itself.

## Fix 2: `prnblue` / `prnred` emitted plain text instead of ANSI color

### Symptom

`(prnblue "hi")` printed `hi` — no escapes — whenever `dbg.arc` had been
loaded (i.e., always, because `srv.arc` requires it). In a bare `bin/arc`
session that didn't load `dbg.arc`, the same call correctly produced
`\x1b[1;44mhi\x1b[1;0m`.

### Cause

`dbg.arc` loads `colors.arc` at the top, which defines `prnblue`, `prnred`,
etc. via `makeprs` to emit `\x1b[1;<code>m…\x1b[1;0m`. Then, four lines
below, `dbg.arc` had two stale defensive assigns:

```arc
(assign prnred prn)
(assign prnblue prn)
```

These were presumably a fallback from when `dbg.arc` didn't require
`colors.arc`, but with the `require` in place they unconditionally clobber
the color versions with plain `prn`. Net effect: every caller of these two
names got uncolored output, site-wide.

### Fix (`dbg.arc:154-155`)

Delete both assigns. `colors.arc` now wins because it's required at the top
and nothing overrides it.

### Verified with

```
$ python3 -c 'PTY harness: require dbg.arc, then (prnblue "hi")'
→ b'...\x1b[1;44mhi\x1b[1;0m\r\n...'
```

Escapes survive after `(require "dbg.arc")`.

## Unrelated staged-but-not-committed bits

`srv.arc` has local unstaged edits left over from unrelated work — not part
of this commit.

## Files at a glance

- `dbg.arc` — two small, independent fixes in one commit; no other files.
