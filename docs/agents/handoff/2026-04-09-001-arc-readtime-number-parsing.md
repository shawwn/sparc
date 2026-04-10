# Handoff: Arc read-time number literal parsing

**Date:** 2026-04-09
**Commit:** 4e61196

## What was accomplished

Moved hex/binary/octal/underscore number literal parsing from compile time to read time in the Arc interpreter.

**Files changed:**

- `brackets.scm` — Extended `arc-readtable` with `non-terminating-macro` handlers for `0-9`, `+`, and `-`. Each handler reads the full token from the port and tries to parse it as a number (supporting `0x`/`0b`/`0o` prefix and `_` digit separators). Falls back to a symbol if parsing fails. New helpers: `arc-token-delim?`, `arc-read-rest-of-token`, `arc-string-prefix?`, `arc-strip-underscores`, `arc-parse-uint`, `arc-parse-number-string`, `arc-read-number-or-symbol`.

- `ac.scm` — Removed `(ac-number-literal x)` from `literal?` and `ac-quoted`. The functions `ac-tonumber`, `ac-parse-number`, `ac-number-literal` are retained (they are exported via `(all-defined-out)`) but no longer called internally.

All 706 existing tests pass. The `define-test reader` suite in `test.arc` already covered these cases.

## Key decisions

**Why read time?** Before this change, `0x42` (bare token) and `|0x42|` (pipe-quoted) both produced the same Racket symbol `0x42` after reading — they were indistinguishable by the time `ac-number-literal` ran. Moving parsing to the readtable level fixes this because Racket's `|...|` reader bypasses character macro handlers entirely, so pipe-quoted forms are never seen by our digit/sign handlers and remain symbols.

**`non-terminating-macro` vs `terminating-macro`:** Digit and sign characters are non-terminating in standard Racket (they can appear in the middle of symbols like `abc5` or `x+y`). Using `non-terminating-macro` preserves this: our handlers only fire when the character *starts* a new token, not when it appears mid-token.

**`arc-parse-uint` rejects leading `+`/`-`:** `string->number` in Racket accepts signed strings, so after stripping one leading sign, a second sign (e.g. `--1` → strip `-` → `arc-parse-uint "-1"`) would have parsed as a number. Added explicit guards so `arc-parse-uint` only handles unsigned values; `arc-parse-number-string` handles the optional sign wrapper.

**`racket/string` not required:** `string-prefix?` and `string-replace` were inlined as `arc-string-prefix?` and `arc-strip-underscores` to keep `brackets.scm` self-contained. `brackets.scm` uses `#lang racket/load` and was previously dependency-free.

## Context for future sessions

- The readtable is defined in `brackets.scm` and activated by `(use-arc-readtable)` in `arc.scm` after all three `.scm` files are loaded. It is not active while the `.scm` files themselves are being compiled.
- `arc-parse-number` / `ac-number-literal` in `ac.scm` still exist and are exported. They are dead code internally but kept to avoid breaking any external callers. They could be removed in a future cleanup.
- The `define-test reader` suite in `test.arc` is the authoritative test coverage for read-time number behavior. Run with `arc test.arc`.
- Compiled `.zo` files in `compiled/` need to be regenerated (`raco make arc.scm`) after any change to the `.scm` files; stale `.zo` files from a different Racket version produce a version-mismatch error at startup.
