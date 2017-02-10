The first thing you should do as a new user of IDE is to investigate Configurations (found on toolbar and under Menu Options). One important tab is "Enter Key Processing For Properties" which allows you to check the fields whose data should not be updated each keystroke. Row and Col are popular choices.

If you are a prior IDE user but have not used IDE for many releases, IDE.cfg may need conversion to new format. ConvertIDEcfg.exw is provided for this purpose. 

IDE is distributed with subfolders. If you have previously used IDE but place the downloaded IDE in a different folder than your current IDE copy the \Files folder from your prior IDE location to the \Files folder you download to. If you do not do this, IDE will use default values and create new files.

Language Translations files will need to be downloaded if you wish to use them. Place in \Languages folder and do not remove English.lng.

If you want to interface with systray.ew, IDE does not supply that file.

If you wish to use transparent windows, IDE does not supply alphablend.ew.

If you wish to interface with EuGrid, IDE does not supply EuGrid.ew.

These extra include files may be found on Euphoria Recent Contributions Page.

Problem that I know about:
1- If you are using screen resolution with Large Fonts, sometimes IDE Menubar goes to multiple lines and Tool Box and Test Window in Windows Styles Explorer is not positioned correctly. So you should probably expand IDE such that the Menubar returns to one line until I know a solution.

Thanks for trying IDE and have fun!

--Judith