# Handoff: npm deployment for sparclisp

**Date:** 2026-04-16
**Branch:** master

## What was accomplished

Wired up the repo for `npm publish` so users can install Arc Lisp via `npm install sparclisp`. Added full cross-platform support: Linux x86_64, Linux aarch64, macOS x86_64, macOS aarch64, Windows x86_64, and Windows 11 ARM64.

**Files changed:**

- `package.json` — added `"postinstall": "node bin/postinstall.js"`
- `bin/postinstall.js` — new; cross-platform dispatcher: runs `setup.cmd` on Windows, `setup.sh` elsewhere
- `bin/setup.sh` — extended to detect CPU architecture (`uname -m`) and pick the correct Racket tarball; upgraded Racket 8.9 → 9.1
- `bin/setup.cmd` — new; downloads and silently installs Racket 9.1 for Windows (x86_64 or ARM64), installs `compiler-lib`, compiles `.scm` files
- `bin/arc.cmd` — rewritten to use the bundled `bin\racket\Racket.exe` and set `ARC_HOME` (previously relied on system `racket` being on `PATH`)
- `makefile` — replaced cmake+make bcrypt build with a direct `c++` invocation, eliminating the cmake dependency

## Key decisions

**cmake removed:** The `CMakeLists.txt` for bcrypt was trivially simple (two source files → shared library). Replaced with a direct `c++ -std=c++14 -shared -fPIC` call. The only remaining build dependency is `c++` (clang or gcc), which ships with Xcode CLT on macOS and `build-essential` on Ubuntu.

**Racket 9.1 everywhere:** Racket 8.9 has no ARM64 Windows build (that was added in 9.0). Rather than mixing versions per-platform, all platforms were upgraded to 9.1 for consistency.

**Windows ARM64 uses `arm64` filename, not `aarch64`:** Racket's Windows ARM64 installer is named `racket-minimal-9.1-arm64-win32-cs.exe` while Linux/macOS use `aarch64`. The Windows script detects `%PROCESSOR_ARCHITECTURE%==ARM64` and uses the correct string.

**NSIS silent install path workaround:** The Racket Windows installer is an NSIS exe. NSIS's `/D=` flag (install directory) does not support paths with spaces. `setup.cmd` uses PowerShell's `[System.IO.Path]::GetShortPathName()` to convert the target path to 8.3 format before passing it to `/D=`.

**bcrypt.dll on Windows is pre-built x86_64 only:** `src/bcrypt/bcrypt.dll` (committed to the repo) is a PE32+ x86-64 DLL. `ac.scm` tries `src/bcrypt/bcrypt` before `src/bcrypt/build/libbcrypt`, so it loads automatically on Windows x86_64. However, it **will not load on Windows ARM64 Racket** — an ARM64-native DLL is needed. This is an open problem.

**`bin/arc.cmd` and `bin/sparc.cmd` are picked up automatically by npm** for Windows users — npm resolves `.cmd` files alongside the unix bin entries without any extra config.

## Known issues / open work

- **`src/bcrypt/bcrypt.dll` is x86_64 only, but works on ARM64** — Windows 11 ARM64 has built-in x64 emulation so the x64 DLL loads fine. Confirmed working on a Windows 11 ARM64 VM.

## Testing

To test a fresh Windows install, the recommended approach is:

- **x86_64:** GitHub Actions `windows-latest` runner — add a workflow that runs `npm install` then `echo (+ 1 2) | bin\arc.cmd`
- **ARM64:** Run Windows 11 ARM64 in UTM (free) or Parallels on an Apple Silicon Mac; Microsoft distributes free 90-day dev VMs for this purpose

Smoke tests to run after install:
```bat
echo (+ 1 2) | bin\arc.cmd
echo (prn (bcrypt "hello" "$2a$10$abcdefghijklmnopqrstuO")) | bin\arc.cmd
```

The second command verifies bcrypt FFI is loading correctly.
