echo off
echo ***NO LONGER IN USE*** - run phixzip.exw iunstead
pause
goto :eof
::-- Phix package script
echo.
echo Create phix.exe Windows Installer for phix.
echo ===========================================
echo  (please remember to change the version!)
echo.
set v=0.8.0
:: (and the one in pglobals.e and the one in banner.htm)
pause
echo on

p -c p
pause
p -c -norun p32.exu
p -c -norun p64.exu

::copy phixzip.bat sfx\phix.%v%.zip /Y
::del sfx\phix.%v%.zip
::copy phixzip.bat sfx\phix.%v%.zip.001 /Y
::del sfx\phix.%v%.zip.001
::copy phixzip.bat sfx\phix.%v%.zip.002 /Y
::del sfx\phix.%v%.zip.002

::dang; only works on windows...
::"C:\Program Files\7-zip\7z" a -tzip sfx\phix.%v%.zip @sfx\phix7zip.lst -v9m 

copy phixzip.bat sfx\phix.%v%.zip /Y
del sfx\phix.%v%.zip

"C:\Program Files\7-zip\7z" a -tzip sfx\phix.%v%.zip @sfx\phix7zip.lst


::copy phixzip.bat sfx\phix.zip.001 /Y
::del sfx\phix.zip.001
::copy phixzip.bat sfx\phix.zip.002 /Y
::del sfx\phix.zip.002
::"C:\Program Files\7-zip\7z" a -v9M sfx\phix.zip @sfx\phix7zip.lst
::"C:\Program Files\7-zip\7z" a -v9M sfx\phix.zip   -- not implemented
::"C:\Program Files\7-zip\7z" a -v9000000 sfx\phix.zip  -- ditto

::seeing that the above ends with "Everything is Ok" is far more helpful that watching these deletes...
echo off
echo creating phix1.zip and phix2.zip...
copy sfx\phix.%v%.zip sfx\phix.%v%.1.zip /Y > tmp.txt
copy sfx\phix.%v%.zip sfx\phix.%v%.2.zip /Y > tmp.txt
"C:\Program Files\7-zip\7z" d sfx\phix.%v%.1.zip docs\phix > tmp.txt
"C:\Program Files\7-zip\7z" d sfx\phix.%v%.1.zip demo\pGUI\win64 > tmp.txt
"C:\Program Files\7-zip\7z" d sfx\phix.%v%.2.zip docs\*.txt > tmp.txt
"C:\Program Files\7-zip\7z" d sfx\phix.%v%.2.zip *.* > tmp.txt
"C:\Program Files\7-zip\7z" d sfx\phix.%v%.2.zip phix > tmp.txt
"C:\Program Files\7-zip\7z" d sfx\phix.%v%.2.zip bench > tmp.txt
"C:\Program Files\7-zip\7z" d sfx\phix.%v%.2.zip builtins > tmp.txt
"C:\Program Files\7-zip\7z" d sfx\phix.%v%.2.zip demo\a* > tmp.txt
"C:\Program Files\7-zip\7z" d sfx\phix.%v%.2.zip demo\d* > tmp.txt
"C:\Program Files\7-zip\7z" d sfx\phix.%v%.2.zip demo\e* > tmp.txt
"C:\Program Files\7-zip\7z" d sfx\phix.%v%.2.zip demo\libcurl\*.* > tmp.txt
"C:\Program Files\7-zip\7z" d sfx\phix.%v%.2.zip demo\pGUI\*.* > tmp.txt
"C:\Program Files\7-zip\7z" d sfx\phix.%v%.2.zip demo\pGUI\data > tmp.txt
"C:\Program Files\7-zip\7z" d sfx\phix.%v%.2.zip demo\pGUI\icons > tmp.txt
"C:\Program Files\7-zip\7z" d sfx\phix.%v%.2.zip demo\pGUI\lnx > tmp.txt
"C:\Program Files\7-zip\7z" d sfx\phix.%v%.2.zip demo\pGUI\pdemo > tmp.txt
"C:\Program Files\7-zip\7z" d sfx\phix.%v%.2.zip demo\pGUI\win32 > tmp.txt
"C:\Program Files\7-zip\7z" d sfx\phix.%v%.2.zip demo\r* > tmp.txt
"C:\Program Files\7-zip\7z" d sfx\phix.%v%.2.zip demo\t* > tmp.txt
"C:\Program Files\7-zip\7z" d sfx\phix.%v%.2.zip demo\w* > tmp.txt
"C:\Program Files\7-zip\7z" d sfx\phix.%v%.2.zip demo\x* > tmp.txt
"C:\Program Files\7-zip\7z" d sfx\phix.%v%.2.zip demo\*.* > tmp.txt
"C:\Program Files\7-zip\7z" d sfx\phix.%v%.2.zip sfx > tmp.txt
"C:\Program Files\7-zip\7z" d sfx\phix.%v%.2.zip test > tmp.txt
echo on

::copy phixzip.bat sfx\phix.zip.001 /Y
::del sfx\phix.zip.001
::copy phixzip.bat sfx\phix.zip.002 /Y
::del sfx\phix.zip.002

::move /Y sfx\phix1.zip sfx\phix.0.7.4.zip.001
::move /Y sfx\phix2.zip sfx\phix.0.7.4.zip.002
::move /Y sfx\phix.zip.001 sfx\phix.%v%.zip.001
::move /Y sfx\phix2.zip sfx\phix.%v%.zip.002

::echo off
echo creating phix.docs.%v%.zip...
copy phixzip.bat sfx\phix.docs.%v%.zip /Y
del sfx\phix.docs.%v%.zip
copy docs\phix\Combined.css docs\phix\html\Combined.css /Y
copy docs\phix\Common.js docs\phix\html\Common.js /Y
"C:\Program Files\7-zip\7z" a -tzip sfx\phix.docs.%v%.zip @sfx\docs.lst
::del docs\phix\html\Combined.css
::del docs\phix\html\Common.js
::echo on

::sfx\makesfx.exe /zip="sfx\phix.zip" /sfx="sfx\phix.0.7.4.setup.exe" /title="Install Phix" /website="http://phix.x10.mx" /intro="This program installs the Phix Programming Language" /defaultpath="$programfiles$\Phix" /exec="$targetdir$\ppw.bat" /overwrite 
sfx\makesfx.exe /zip="sfx\phix.%v%.zip" /sfx="sfx\phix.%v%.setup.exe" /title="Install Phix" /website="http://phix.x10.mx" /intro="This program installs the Phix Programming Language, version %v%" /defaultpath="$programfiles$\Phix" /exec="$targetdir$\ppw.bat" /overwrite 

::copy phixzip.bat sfx\phix.setup.exe /Y
::del sfx\phix.setup.exe
::"C:\Program Files\7-zip\7z" a -sfx phix.setup.exe -v9M @sfx\phix7zip.lst

:::: arwen package
::del C:\TEMP\P\PARWEN.ZIP
::pkzipc -add -warn -max -path=current C:\TEMP\P\PARWEN.ZIP @pkzarwen.lst

@echo off
echo Finished. See sfx/phix.%v%.zip[and .001/2] and sfx/phix.%v%.setup.exe
pause
