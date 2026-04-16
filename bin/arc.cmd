@echo off
setlocal

set "bin=%~dp0"
pushd "%bin%\.."
set "home=%cd%"
popd

set "ARC_HOME=%home%"
set "ARC_HOST=%home%\bin\racket\Racket.exe"

"%ARC_HOST%" -y -t "%ARC_HOME%\as.scm" -- %*
