# Handoff: npm Windows fixes and Arc npm library system

**Date:** 2026-04-16
**Branch:** master
**npm version published:** `sparclisp@0.0.6`

## What was accomplished

### 1. Windows `arc.cmd` — auto-setup Racket on first run

`bin/arc.cmd` previously assumed Racket was already installed. Added a `CALL "%home%\bin\setup.cmd"` before launching Racket so the first run on a fresh Windows install automatically downloads and installs Racket.

### 2. `bin/setup.cmd` — fix "Access is denied" on installer

The NSIS Racket installer was failing with "Access is denied" because Windows blocks downloaded `.exe` files via the Mark of the Web (Zone.Identifier alternate data stream). Fixed by running `Unblock-File` on the downloaded installer before executing it. Also removed the pre-`mkdir` call (let the NSIS installer create the directory itself) and the `GetShortPathName` workaround (unnecessary when the path has no spaces).

### 3. `bin/sparc.cmd` — fix `%home%` lost after `endlocal`

`sparc.cmd` called `endlocal` before the `CALL "%home%\bin\arc.cmd"` line, so `%home%` was always empty and the command failed with "The system cannot find the path specified." Fixed by moving `endlocal` to after the `CALL`.

### 4. npm bin entries — JS wrappers for cross-platform `npx` support

npm 11 rejected the bare shell scripts (`bin/arc`, `bin/sparc`) as bin entries. Replaced with JS wrappers:

- `bin/arc.js` — calls `arc.cmd` on Windows, `arc` on Unix via `spawnSync`
- `bin/sparc.js` — calls `sparc.cmd` on Windows, `sparc` on Unix via `spawnSync`
- `package.json` bin entries updated to point to `.js` files

Initial version used `spawnSync` without `shell: true`, which silently fails on Windows because `.cmd` files cannot be spawned directly — they require `cmd.exe`. Fixed by adding `shell: process.platform === 'win32'` to the options.

### 5. `(require 'foo)` — Arc npm library system

Added `npm-resolve` and updated the `require` macro in `arc.arc` so that `(require 'foo)` loads Arc libraries installed via npm.

**How it works:**
- `npm-resolve` walks up from `cwd` looking for `node_modules/arc-{name}/`
- Reads `package.json` `main` field if present; falls back to `{name}.arc`
- Returns the full path or `nil` if not found
- `require` macro: if given a symbol/quoted-symbol, calls `npm-resolve` at runtime and loads the file; errors with a helpful message if not found

**Library package convention:**
```
arc-foo/
  package.json   { "name": "arc-foo", "main": "foo.arc" }
  foo.arc
```
Users do `(require 'foo)` in their scripts.

### 6. `.npmignore`

Created `.npmignore` to exclude `.claude/` and `docs/` from the published tarball (previously npm was falling back to `.gitignore`, which didn't cover these).

## Key decisions

**JS wrappers with `shell: true` on Windows only:** Using `shell: true` on all platforms is unnecessary and slightly slower. Only Windows needs it to invoke `.cmd` files. Unix can spawn the bash script directly.

**`npm-resolve` is runtime, not compile-time:** The `require` macro expands to a runtime `npm-resolve` call so that library resolution respects the cwd at the time `require` runs, not at Arc compile time.

**Old `(require 'sym)` behavior dropped:** Previously `(require 'sym)` delegated to Racket's native `require`. No existing Arc code in this repo used that path, so it was safe to replace with the npm resolution logic. If Racket module imports are ever needed, a separate form (e.g. `(rkt-require ...)`) should be added.

**NSIS `mkdir` removed:** Pre-creating the target directory before running the NSIS installer caused issues. NSIS creates the directory itself; the pre-`mkdir` was a leftover from the `GetShortPathName` workaround that is no longer needed.

## Correct commands for Windows users

```bat
REM After: npm i sparclisp  (local)
.\node_modules\sparclisp\bin\arc.cmd

REM Via npx (local install)
npx arc
npx sparc

REM Global install
npm i -g sparclisp
arc
sparc

REM Uninstall global
npm uninstall -g sparclisp
```

`npx -p sparclisp arc` works without any local/global install (uses npx cache).

## Files changed this session

| File | Change |
|------|--------|
| `arc.arc` | Added `npm-resolve` def + rewrote `require` macro |
| `.npmignore` | New; excludes `.claude/`, `docs/` from tarball |
| `bin/arc.cmd` | Added `CALL setup.cmd` at top |
| `bin/arc.js` | New JS wrapper; fixed `shell: true` for Windows |
| `bin/sparc.js` | New JS wrapper; fixed `shell: true` for Windows |
| `bin/sparc.cmd` | Moved `endlocal` to after `CALL` |
| `bin/setup.cmd` | Added `Unblock-File`; removed `mkdir`+`GetShortPathName` |
| `package.json` | Bin entries → `.js` files; version bumped to `0.0.6` |

## Known issues / open work

- **bcrypt.dll is x86_64 only, but works on ARM64** — Windows 11 ARM64 has built-in x64 emulation, so the x64 `bcrypt.dll` loads fine via Racket's FFI. No ARM64-native DLL is needed.
- The `(require 'foo)` error message if a package isn't found could be cleaner — currently `err` is called with multiple args which may not concatenate as expected depending on Arc's `err` signature.
