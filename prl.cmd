@echo off
echo.
echo Refreshing PATH from registry

:: Get System PATH
for /f "tokens=2*" %%A in ('reg query "HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\Environment" /v Path') do set syspath=%%B

:: Get User Path
for /f "tokens=2*" %%A in ('reg query "HKCU\Environment" /v Path') do set userpath=%%B

:: Set Refreshed Path
set PATH=%userpath%;%syspath%

echo Refreshed PATH
echo %PATH%
