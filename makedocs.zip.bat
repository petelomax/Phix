::temp/test
set v=0.7.6
echo creating phix.docs.%v%.zip...
copy phixzip.bat sfx\phix.docs.%v%.zip /Y
del sfx\phix.docs.%v%.zip
copy docs\phix\Combined.css docs\phix\html\Combined.css /Y
copy docs\phix\Common.js docs\phix\html\Common.js /Y
"C:\Program Files\7-zip\7z" a -tzip sfx\phix.docs.%v%.zip @sfx\docs.lst
::del docs\phix\html\Combined.css
::del docs\phix\html\Common.js
pause
