@echo off
setlocal

set "dir=%cd%"
cd "%~dp0"
cd ..
set "home=%cd%"
cd "%dir%"

endlocal

REM set DEV=1
REM
IF "%1"=="" ( CALL "%home%\bin\arc.cmd" "%home%\news.arc" ) ELSE ( CALL "%home%\bin\arc.cmd" %* )
