@echo off
setlocal EnableDelayedExpansion

set "bin=%~dp0"
pushd "%bin%\.."
set "home=%cd%"
popd

if exist "%home%\bin\racket\Racket.exe" goto :compile

if "%PROCESSOR_ARCHITECTURE%"=="ARM64" (
  set "racket_arch=arm64"
) else (
  set "racket_arch=x86_64"
)
set "racket_installer=racket-minimal-9.1-!racket_arch!-win32-cs.exe"
set "racket_url=https://download.racket-lang.org/releases/9.1/installers/!racket_installer!"

echo Downloading Racket 9.1 (!racket_arch!) for Windows...
set "installer=%home%\bin\racket-installer.exe"
powershell -NoProfile -Command "Invoke-WebRequest '!racket_url!' -OutFile '%installer%'"
if errorlevel 1 (
  echo Failed to download Racket. Visit https://download.racket-lang.org/ and install to %home%\bin\racket
  exit /b 1
)

echo Installing Racket...
mkdir "%home%\bin\racket" 2>nul
for /f "usebackq delims=" %%i in (`powershell -NoProfile -Command "[System.IO.Path]::GetShortPathName('%home%\bin\racket')"`) do set "racket_short=%%i"
"%installer%" /S /D=!racket_short!
del "%installer%"

echo Installing compiler-lib...
"%home%\bin\racket\raco.exe" pkg install --auto compiler-lib

:compile
if exist "%home%\compiled" goto :done

echo Compiling .scm files...
cd /d "%home%"
"%home%\bin\racket\raco.exe" make ac.scm ar.scm arc.scm as.scm brackets.scm

:done
endlocal
