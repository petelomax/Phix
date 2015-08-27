To put some entries for Phix in the start menu (which may not be all that useful, you can test them where they stand as 
long as Phix got installed to C:\Program Files (x86)\Phix, and if not I suggest you fix em so they work before continuing):

Copy the entire Phix directory (not sfx\phix but sfx\phix\Phix) to 

C:\Users\All Users\Microsoft\Windows\Start Menu\Programs (windows 7)
  or
C:\ProgramData\Microsoft\Windows\Start Menu\Programs (windows 7, it is exactly the same place as above)
  or
%ALLUSERSPROFILE%\Microsoft\Windows\Start Menu\Programs (windows 7, same as above)
  or
C:\Users\Pete\AppData\Roaming\Microsoft\Windows\Start Menu\Programs (windows 7, but user-specific)
  or
%APPDATA%\Microsoft\Windows\Start Menu\Programs (windows 7, same as above)
  or
C:\Documents and Settings\All Users\Start Menu\Programs (windows XP)
  or
%ALLUSERSPROFILE%\Start Menu\Programs (windows XP)
  or
%USERPROFILE%\Start Menu\Programs (windows XP, but user-specific)

Yeah, tell me about it. I've got a wall, all I need now are some rifles, a blindfold, and whoever "designed" this.

Let me know if there are other entries on that sub-menu you think would be appropriate.

If someone (else) wants to write a program (using SHGetFolderPath?, or maybe a .bat file) that does this automatically, 
wherever Phix got installed, and can test it on XP/Vista/Windows 8/10 etc... And while you are there, how about an uninstall program?

