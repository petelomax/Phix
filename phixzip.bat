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

copy phixzip.bat sfx\phix.zip /Y
del sfx\phix.zip
::copy p.exe official.exe /Y    ;; now done earlier
::sfx\pkzipc -add -warn -max -path=current sfx\phix.zip @sfx\phixzip.lst
"C:\Program Files\7-zip\7z" a -tzip sfx\phix.zip @sfx\phix7zip.lst

sfx\makesfx.exe /zip="sfx\phix.zip" /sfx="sfx\phix.setup.exe" /title="Install Phix" /website="http://phix.is-great.org" /intro="This program installs the Phix Programming Language" /defaultpath="$programfiles$\Phix" /exec="$targetdir$\ppw.bat" /overwrite 

copy sfx\phix.zip sfx\phix1.zip /Y
copy sfx\phix.zip sfx\phix2.zip /Y
"C:\Program Files\7-zip\7z" d sfx\phix1.zip docs\phix
"C:\Program Files\7-zip\7z" d sfx\phix1.zip demo\pGUI\win64
"C:\Program Files\7-zip\7z" d sfx\phix2.zip docs\*.txt
"C:\Program Files\7-zip\7z" d sfx\phix2.zip *.*
"C:\Program Files\7-zip\7z" d sfx\phix2.zip phix
"C:\Program Files\7-zip\7z" d sfx\phix2.zip bench
"C:\Program Files\7-zip\7z" d sfx\phix2.zip builtins
"C:\Program Files\7-zip\7z" d sfx\phix2.zip demo\a*
"C:\Program Files\7-zip\7z" d sfx\phix2.zip demo\d*
"C:\Program Files\7-zip\7z" d sfx\phix2.zip demo\e*
"C:\Program Files\7-zip\7z" d sfx\phix2.zip demo\pGUI\*.*
"C:\Program Files\7-zip\7z" d sfx\phix2.zip demo\pGUI\data
"C:\Program Files\7-zip\7z" d sfx\phix2.zip demo\pGUI\icons
"C:\Program Files\7-zip\7z" d sfx\phix2.zip demo\pGUI\lnx
"C:\Program Files\7-zip\7z" d sfx\phix2.zip demo\pGUI\pdemo
"C:\Program Files\7-zip\7z" d sfx\phix2.zip demo\pGUI\win32
"C:\Program Files\7-zip\7z" d sfx\phix2.zip demo\r*
"C:\Program Files\7-zip\7z" d sfx\phix2.zip demo\t*
"C:\Program Files\7-zip\7z" d sfx\phix2.zip demo\w*
"C:\Program Files\7-zip\7z" d sfx\phix2.zip demo\x*
"C:\Program Files\7-zip\7z" d sfx\phix2.zip demo\*.*
"C:\Program Files\7-zip\7z" d sfx\phix2.zip sfx
"C:\Program Files\7-zip\7z" d sfx\phix2.zip test

:::: arwen package
::del C:\TEMP\P\PARWEN.ZIP
::pkzipc -add -warn -max -path=current C:\TEMP\P\PARWEN.ZIP @pkzarwen.lst

@echo off
echo Finished. See sfx/phix.zip and sfx/phix.exe
pause
