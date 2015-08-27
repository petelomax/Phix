::-- Phix package script
echo off
echo.
echo Create phix.exe Windows Installer for phix.
echo ===========================================
echo  (please remember to change the version!)
echo.
pause
echo on

:::: documentation package
::copy phixzip.bat pdocs.zip /-Y
::del pdocs.zip
::pkzipc -add -max -warn -path=current pdocs.zip @pkzdocs.lst

copy phixzip.bat sfx\phix.zip /-Y
del sfx\phix.zip
::copy p.exe official.exe /Y    ;; now done earlier
::sfx\pkzipc -add -warn -max -path=current sfx\phix.zip @sfx\phixzip.lst
"C:\Program Files\7-zip\7z" a -tzip sfx\phix.zip @sfx\phix7zip.lst

sfx\makesfx.exe /zip="sfx\phix.zip" /sfx="sfx\phix.exe" /title="Install Phix" /website="http://phix.is-great.org" /intro="This program installs the Phix Programming Language" /defaultpath="$programfiles$\Phix" /exec="$targetdir$\ppw.bat" /overwrite 

:::: arwen package
::del C:\TEMP\P\PARWEN.ZIP
::pkzipc -add -warn -max -path=current C:\TEMP\P\PARWEN.ZIP @pkzarwen.lst

@echo off
echo Finished. See sfx/phix.zip and sfx/phix.exe
pause
