@echo off
setlocal

set "bin=%~dp0"
pushd "%bin%\.."
set "home=%cd%"
popd

call "%home%\bin\setup.cmd"
if errorlevel 1 exit /b 1

set "ARC_HOME=%home%"
set "ARC_HOST=%home%\bin\racket\Racket.exe"

"%ARC_HOST%" -y -t "%ARC_HOME%\as.scm" -- %*
