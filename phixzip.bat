::-- Phix package script
echo off
echo.
echo Create phix.exe Windows Installer for phix.
echo ===========================================
echo  (please remember to change the version!)
echo.
set v=0.7.5
pause
echo on

::copy phixzip.bat sfx\phix.zip /Y
::del sfx\phix.zip

"C:\Program Files\7-zip\7z" a -tzip sfx\phix.zip @sfx\phix7zip.lst

::copy phixzip.bat sfx\phix.zip.001 /Y
::del sfx\phix.zip.001
::copy phixzip.bat sfx\phix.zip.002 /Y
::del sfx\phix.zip.002
::"C:\Program Files\7-zip\7z" a -v9M sfx\phix.zip @sfx\phix7zip.lst
::"C:\Program Files\7-zip\7z" a -v9M sfx\phix.zip   -- not implemented
::"C:\Program Files\7-zip\7z" a -v9000000 sfx\phix.zip  -- ditto

::sfx\makesfx.exe /zip="sfx\phix.zip" /sfx="sfx\phix.0.7.4.setup.exe" /title="Install Phix" /website="http://phix.x10.mx" /intro="This program installs the Phix Programming Language" /defaultpath="$programfiles$\Phix" /exec="$targetdir$\ppw.bat" /overwrite 
sfx\makesfx.exe /zip="sfx\phix.zip" /sfx="sfx\phix.%v%.setup.exe" /title="Install Phix" /website="http://phix.x10.mx" /intro="This program installs the Phix Programming Language, version %v%" /defaultpath="$programfiles$\Phix" /exec="$targetdir$\ppw.bat" /overwrite 

::copy phixzip.bat sfx\phix.setup.exe /Y
::del sfx\phix.setup.exe
::"C:\Program Files\7-zip\7z" a -sfx phix.setup.exe -v9M @sfx\phix7zip.lst

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

::copy phixzip.bat sfx\phix.zip.001 /Y
::del sfx\phix.zip.001
::copy phixzip.bat sfx\phix.zip.002 /Y
::del sfx\phix.zip.002

::move /Y sfx\phix1.zip sfx\phix.0.7.4.zip.001
::move /Y sfx\phix2.zip sfx\phix.0.7.4.zip.002
move /Y sfx\phix1.zip sfx\phix.%v%.zip.001
move /Y sfx\phix2.zip sfx\phix.%v%.zip.002

:::: arwen package
::del C:\TEMP\P\PARWEN.ZIP
::pkzipc -add -warn -max -path=current C:\TEMP\P\PARWEN.ZIP @pkzarwen.lst

@echo off
echo Finished. See sfx/phix.%v%.zip and sfx/phix.%v%.setup.exe
pause
