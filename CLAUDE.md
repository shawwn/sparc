# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`sparclisp` — an implementation of Arc (Paul Graham's Lisp) on top of Racket. Published to npm as `sparclisp`; the primary user-facing app is `news.arc`, a Hacker News-style site. Runs on Linux/macOS/Windows (incl. ARM64 Windows via x64 emulation).

## Commands

```bash
make                    # build bcrypt, install Racket 9.1, compile *.scm → compiled/*.zo
make test               # bin/arc test.arc  (full Arc test suite)
make scm                # wipe compiled/ and rebuild (do this whenever *.scm changes)
make clean              # rm -rf compiled/
make dist-clean         # clean + remove bcrypt build + bundled Racket

bin/arc                 # Arc REPL
bin/arc path/to/foo.arc # run an Arc file
echo '(+ 1 2)' | bin/arc   # stdin-pipe eval (multiple top-level forms OK)
bin/arc -i foo.arc      # run file then drop into REPL

bin/sparc               # = bin/arc news.arc (runs the news site on :8080)
npm run dev             # DEV=1 bin/sparc
npm start               # PULL=10 FLUSH=nil bin/sparc (production: git-pulls every 10s)
```

Run a single test: tests are defined with `define-test NAME` in `test.arc`. To run one, either grep for `define-test foo` and invoke that block interactively in `bin/arc`, or temporarily narrow `test.arc`. There is no built-in single-test CLI flag.

The bundled Racket lives at `bin/racket/bin/racket` and is auto-installed by `bin/setup.sh` (or `bin/setup.cmd` on Windows) the first time `bin/arc` runs. `npm install sparclisp` runs `bin/postinstall.js` which dispatches to the same setup scripts.

## Architecture

### The stack, bottom-up

- **`ar.scm`** — Arc **runtime** primitives. Everything prefixed `ar-` (e.g. `ar-nil?`, `ar-kwproc`, `ar-apply`). Lives in `#lang racket/base` and is `(require)`d by `ac.scm`.
- **`ac.scm`** — Arc **compiler**: s-expression translation from Arc to Racket. `ac` + `ac-*` functions (e.g. `ac-fn`, `ac-simple-fn`, `ac-complex-fn`, `ac-kwargs?`). Also defines `aload`/`aload1` (file/port readers) and the FFI bridge to bcrypt.
- **`brackets.scm`** — custom **readtable**: `[...]` → `(fn (_) ...)`, `{...}` → `(%braces ...)`, and **read-time** parsing of numeric literals (`0x42`, `0b101`, `0o17`, `1_000`). Activated by `(use-arc-readtable)`. Important: pipe-quoted `|0x42|` bypasses our handlers and stays a symbol — that distinction is why parsing is at read time, not compile time.
- **`arc.scm`** — orchestrator: loads `ac.scm`, `ar.scm`, `brackets.scm`, activates the readtable, then `(aload "./arc.arc")` and `(arc-eval '(load "./libs.arc"))`.
- **`as.scm`** — CLI entry point. Handles arg parsing (`-i`/`--interact`), stdin-pipe detection (via `terminal-port?`), and either loads a file + exits, enters REPL, or `aload1`s stdin.
- **`arc.arc`** — main Arc stdlib: macros (`def`, `mac`, `each`, etc.), core functions, reader helpers, and the `require` macro / `npm-resolve` for loading Arc npm packages.
- **`libs.arc`** — thin loader that pulls in `ext.arc`, `glob.arc`, `strings.arc`. Other libs (`srv.arc`, `app.arc`, `html.arc`, `prompt.arc`, `git.arc`, etc.) are loaded on demand via `(require "foo.arc")`.
- **`news.arc`** — the HN-style app, built on `app.arc` (login/forms/users) → `srv.arc` (HTTP) → `html.arc` (HTML gen).
- **`test.arc`** — Arc test harness (`define-test`, `test?`, `test!`). Run via `bin/arc test.arc`.

### Compilation caches

`*.scm` files compile to `compiled/*.zo` via `raco make`. **Stale `.zo` from a different Racket version produces a version-mismatch error at startup** — run `make scm` to nuke and rebuild. Pure `.arc` changes do not require recompilation; only `.scm` changes do.

The readtable (`brackets.scm`) is **not** active while the `.scm` files themselves are being compiled — only after `(use-arc-readtable)` is called inside `arc.scm`. Don't expect Arc readtable behavior in `.scm` source.

### Keyword args (`:foo`, `:kws`)

Arc functions can declare keyword params `:foo` and a rest-kwargs collector `:kws`. `ac-kwargs?` in `ac.scm` detects `#:kws` in the compiled arglist and wraps the lambda with `ar-kwproc` (in `ar.scm`), which partitions incoming kwargs between explicitly-declared params and the `:kws` extras plist. This only applies to the **simple** fn path (`ac-simple-fn`); `ac-complex-fn` handles `:kws` separately via `ac-complex-args` / `ar-without`.

### bcrypt FFI

`src/bcrypt/` builds a shared library (`libbcrypt.so`/`libbcrypt.dylib`) via `make bcrypt` (direct `c++ -std=c++14 -shared -fPIC`, no cmake). On Windows, the prebuilt `src/bcrypt/bcrypt.dll` is x86_64 but loads fine on ARM64 Windows via built-in x64 emulation. `ac.scm` tries `src/bcrypt/bcrypt` first, then `src/bcrypt/build/libbcrypt`.

### npm bin wrappers + Arc npm libraries

`bin/arc` and `bin/sparc` are bash scripts; `bin/arc.cmd` and `bin/sparc.cmd` are their Windows counterparts; `bin/arc.js` and `bin/sparc.js` are thin Node wrappers registered in `package.json` `"bin"` so `npx arc` / `npx sparc` work. The Node wrappers must pass `shell: process.platform === 'win32'` to `spawnSync` — Windows cannot spawn `.cmd` files directly.

`(require 'foo)` in Arc resolves via `npm-resolve` (in `arc.arc`) by walking up from `cwd` looking for `node_modules/arc-foo/`, reading `package.json` `main`, falling back to `foo.arc`. Package convention: a directory named `arc-foo/` containing `package.json` + `foo.arc`.

## Workflow notes

- **Session context:** read `docs/agents/handoff/*.md` at session start — these capture non-obvious decisions from recent work (read-time number parsing, stdin pipe, `:kws` fix, npm deployment, Windows fixes). Add a new handoff doc when finishing a substantial session.
- **`.arc` indentation is 2 spaces**, Lisp-style; no trailing whitespace.
- **News app state** lives in files under `arc/` (e.g. `arc/news/`, `arc/logs/`), not in a database.
