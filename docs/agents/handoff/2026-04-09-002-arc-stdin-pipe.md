# Handoff: Arc stdin pipe support

**Date:** 2026-04-09

## What was accomplished

Added support for evaluating Arc expressions piped via stdin, matching standard interpreter behavior (Python, Ruby, etc.).

```
echo '(prn "hi")' | arc
echo '(prn "hi") (prn "there")' | arc
```

Multiple top-level forms are supported without wrapping in `(do ...)`.

**Files changed:**

- `as.scm` — In `arc-main`, when no filename is given and no `-i`/`--interact` flag is set, check `terminal-port?` on `(current-input-port)`. If stdin is a pipe (not a terminal), call `aload1` to read and eval all expressions from stdin. If stdin is a terminal, fall through to `(interact)` as before.

## Key decisions

**`aload1` already existed:** `ac.scm` already defines `aload1` which reads and evals all s-expressions from a port until EOF — used for loading `.arc` files. Reused it directly rather than writing new logic.

**`terminal-port?` for detection:** Racket's built-in `terminal-port?` predicate cleanly distinguishes a pipe from a terminal without any platform-specific hacks.

**`-i` flag overrides:** If the user passes `-i` or `--interact`, the REPL always launches even when stdin is a pipe (consistent with how `arc -i somefile.arc` works). This lets users force interactive mode if needed.

**Stdin remains available for scripts:** When a filename argument is given, `aload1` is not called and stdin flows normally to the script — `(readline)` and `(read)` in user code still work. The pipe detection only triggers in the no-file-argument path.

## Context for future sessions

- Change is entirely in `as.scm` (the entry point), not in `ac.scm` or `arc.scm`.
- There is no `-e` / `--eval` flag for evaluating a single expression from a command-line argument — the pipe approach is the only stdin-eval mechanism added.
- Behavior when stdin is a pipe and no filename is given: evaluates all expressions, prints nothing extra (unlike the REPL which pretty-prints results). Side effects (e.g. `prn`) still produce output.
