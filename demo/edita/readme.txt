
Installation
============
The "full" and "plus" releases allow a single download; the "source" release 
--DEV reword for Phix:
requires either Phix/Euphoria to be installed or one of the runtimes (preferably 
2.4) to be extracted to the same folder, as well as the optional components.

The "plus" release contains everything you might need to edit and run Euphoria
programs, however it is intended for demonstration only: at some point you are
expected to delete some files (those listed in pkzplus.lst) and install a full
version of Euphoria. You should not use the plus release if you have previously
installed Euphoria, since the existence of two exw.exe (etc) files may confuse.

The "full" release offers a single download for users who have previously 
installed the Euphoria run-time, or indeed anyone wishing to use the editor,
but not wishing to program in Euphoria.

Simply unzip the files to a folder of your choice (it should create bitmaps 
lang and syn subdirectories) and:

        if you are using the "full" release, run edita.exe.

        if you are using the "source" release, and as above you have installed a 
                copy of exw.exe, run "exw edita.exw".

        if you are using the "plus" release, you can do either of the above.
                (the "plus" release contains a copy of exw.exe.)


[ANN] Edita 0.3.0 uploaded
An almost complete rewrite of bookmark, fold, and wordwrap table handling, 
from a "packed table" to a "sparse table", plus many other bugfixes.

Download from 
http://phix.x10.mx
--was http://edita.is-great.org
--was http://palacebuilders.pwp.blueyonder.co.uk/edita.htm#download


Introduction
============
This file details the changes made each release. Sometimes major changes are
given a one-liner, sometimes one-line changes get a whole paragraph. Obviously
the real reason this file is written/maintained is for development, eg a bug
might (belatedly) be found which did not exist in 0.2.1 but did in 0.2.2; this
level of detail allows me to a) find the cause and b) more importantly retest
the areas I "fixed" in that release that may have lead to the bug.

It is equally a useful start point for anyone testing a new version, or
deciding whether to install it or not, or, hopefully not very often, needing 
to roll back to a previous version because of a bug in the latest release.

Unicode, intellisense

Version 0.3.6
=============
08/09/15 Bugfix: index out of bounds line 510 eafind.ew. Occurs when
         searching for xxx^p and running past end of file, if starting
         later than lines(file)-lines(searchstring), since the test is
         mid-loop rather than before it. Simple fix (for now).

Version 0.3.4
=============
03/02/12 Fixed display bug in Tabs List. Size and date were not being
         updated properly. It will also show "???" against any files
         which have been opened in the editor but subsequently deleted 
         by an external program.
19/08/12 Minor bugfix: double clicking on ##AutoComplete no longer selects
         the leading '#'; that now only happens if the entire word is 0-9,
         A-F, a-f.
15/12/12 Removed some (strange) code in eamarea.ew to allow editing of the
         message area when the mouse cursor is over it. It all just got too
         annoying when switching back to the editor and the mouse had gone
         somewhere down the screen, it would not behave as I expected. You
         now have to select/copy etc text from the message area with the 
         mouse and the right-click menu options.
         DEV/Update: I'll bet that was F12/error handling code...
29/12/12 Couple of bugfixes in preprocessErr. It was missing the quotes on
         replaced strings, resulting in mangled output, and it was trying
         to replace things inside strings. (trivial once I got stuck in)
02/03/13 Added Alt D (Drag Line Down) and Alt B (Drag Line Back Up) keys,
         because I grew tired of after keying "if <newflag>" having to cut
         and paste the (autocompleted) "end if" to a new home, especially
         since that trashes whatever is currently in the clipboard.
03/03/13 Added some very rudimentary unicode support to Edita, mainly to
         allow .reg files to be examined and edited. NB: all the display
         is still ansi, there is (as yet) no way to create a unicode file
         or convert between unicode and ansi, except perhaps by cut and
         paste. Very limited testing has been done, but it is a start.
         Update: completed on 14/07/2013.
04/03/13 Bugfix. Multiple nested block comments were not working properly.
         For example, html.syn has "BlockComment <!-- --> /* */", ie two
         (multiple) types of block comments, for php support. Problem was
         easynclr.e/scanForUrls() was only looking at blockComment[2], it
         now loops through all of them.
07/03/13 Bugfix. Changes 02/03/13 broke macro playback, because I had
         added an extra parameter to virtualKey(). VirtualKey2() added.
25/03/13 Bugfix. "--" at the start of a line within a triple-quoted string
         was (still) being coloured as a comment, and if the closing """
         was on the same line, it did not close the string properly. Eg
         constant s = """
         some
         -- thing """		-- <== wrong colour
         ?s                 -- <== wrong colour
         (note: the "--" had to start in column 1 for this to occur.)
08/06/13 Index out of bounds in ReadFile (I was creating a file i/o test
         program, so hitting an "unusual" line was not much of a shock).
         Added "if linelength!=0 then   -- added 8/6/2013"
05/07/13 Bugfix. Options\File Extensions blatted the main screen (made it
         go blank) if owner drawn tabs were disabled. Now it only invokes
         setBackColour (in arwen.ew) when they are enabled.
07/07/13 Bugfix. Pressing F1 worked on the string but not on the xXXX in:
          xEnumFontFamiliesExA = link_c_func(gdi32,"EnumFontFamiliesExA"
         (eaqj.e was testing a ch in the wrong one of word/Xword/Yword)
10/07/13 Tweaked context popup menu positioning when opened using the 
         keyboard. Switched to xTrackPopupMenuEx (was using no-Ex) as
         that has an exclusion area, not that any of it really matters.
         (Spotted the problem when investigating for the next item.)
10/07/13 Added eaisense.ew to handle intellisense. At this stage I'm 
         just trying to display a pretty ListBox full of junk, in the
         right place (hence stumbling into the above).
14/07/13 Completed basic unicode support, further to the changes dated
         03/03/13 it now displays unicode correctly. Note that at heart
         Edita is a fixed-character-width editor, and some artefacts of
         that genesis will leak through. In particular, mouse handling
         is (and will likely forever be) slightly off. There is still
         no way to create a unicode file, it just handles files with a
         recognised BOM (byte order mark, one of UTF8 = {#EF,#BB,#BF},
         UTF16LE = {#FF,#FE}, or UTF16BE = {#FE,#FF}) at the start of
         the file. (I realised I did not want to add intellisense only
         for it to fail miserably on unicode files.)
         [Update: file open/save replaced 23/09/13]
16/07/13 Bugfix. F1/Ctrl Q open their window, then Alt tab away and
         back ==> wrong focus, every time. Added a SoftSetFocus call.
17/07/13 Made Window Painter cope with assignment on declaration and 
         optional types in constant declarations. Also made Reindent 
         and Window Painter skip #ilASM and any '@' therein. Finally
         I think I fixed the problem whereby if you had say:
                constant BAD = "BAD"
                constant bad = create(Label,BAD,0,pmain,...
         the BAD (the one in the create statement) would *always* be 
         replaced with ""BAD"" (and that is not me adding ANY quotes)
         It now just quietly leaves UNKNOWN alone if With starts with
         a '\"'; bit of a fudge but seems to do the trick (and yeas I
         am trying to avoid getting sucked into a complete rewrite of 
         Window Painter). Also tweaked find of arwen\\constants.ew,
         made it skip block comments, and parse multiple assignments.
19/07/13 Fixed messed up display of ListViews on TabItems, and gave
         MenuItems an x,y,w,h of 0,0,0,0 rather than those sometimes
         negative random (utterly meaningless & pointless) numbers.
23/07/13 More block comment handling, and type shorthand parameters,
         ie/eg "procedure get_sum(integer x,y,w,h, sequence set)",
         added to window painter/eascan.ew.
04/08/13 Noticed a rather irritating focus grab: with Edita open, if I 
         clicked on another window (explorer) on the taskbar, trying
         to move the mouse from the taskbar over a bit of Edita up to
         the newly ontop window, it vanished. At first I suspected it
         might be related to 16/07/13 changes, but apparently not. In
         the end I changed softSetFocus to do nothing if the getFocus
         call initially returns NULL, which seems to be ok.
23/09/13 Finally put the replacement getOpenFileName/getSaveFileName
         routines into Edita. These have a custom drop-down to allow
         selection of ansi/utf8/utf16le/utf16be:
            ansi -- legacy/ascii text files
            utf8 -- suitable for Phix source files
            utf16le -- suitable for .reg files and similar
            utf16be -- no known use, but available if needed.
         Note that saving any unicode file in ansi format will simply
         truncate characters to the low-order byte, whereas using cut
         (or copy) and paste from a unicode file to one already saved
         as ansi will replace any non-acsii characters with '?'. No
         warning is given about such data loss, but it needs quite a
         deliberate action and is not likely to happen accidentally.
         Also note that opening a new tab (Ctrl N) defaults to ansi,
         until you save the file and select the required encoding.
         The open dialog encoding updates automatically as each file
         is selected, and you can tell whether the current file is
         unicode or ansi by the length of the hex value in the third
         segment of the status bar (except at EOL/EOF).
01/11/13 Bugfix. Tools/Compare was no longer allowing selection of
         compare target by clicking on another open tab. No idea what
         changed, I suspect it is some suble change in the win7 api;
         new code (WM_LBUTTONUP) added to edita.exw/handleTABCONTROL().
15/11/13 Bugfix. easynclr/rebuildbCfwd() was making a bit of a mess of
         triplequote (and probably backtick) multi-line strings. To put
         it briefly, it never actually looked for closing quotes. When
         this happened the only workaround was to page back up the file
         until syntaxColour() had been called for whatever lines it had
         messed up, or start at the top and patiently page down.
19/11/13 Bugfix. Ctrl Q (Quick Jump) could crash when working on files
         with very long lines (like 100K or more). It now limits asking
         allocate_StringZ() for at most 512 characters per line.
03/12/13 Changed F4 to leave the cursor in column 0, rather than have
         the '>' or '<' selected, since it was interfering with the
         help updates I was doing.
29/12/13 Completed the inline intellisense handling. When there is one
         and only one possible completion, and the cursor is at the end
         of the line, the completion is displayed in italics and comment
         colour (yeah, we should allow that to be configurable) and can
         be accepted by keying tab. One small glitch was noted: after 
         "integer t1, t2, t3," it offers t1/t2/t3 as options; I have no 
         plans to address that (it would be changes to Phix to spot that 
         case and return an error code, mainly). Intellisense now works
         directly, without having to run pgui in the background (which
         was always the plan).
30/12/13 Bugfix. Noticed the status bar flickering and CPU usage was a
         bit high, quickly traced to having a .chm file open... Found
         notHotKeyying() in IdleHandler/eaqj, could not figure out why
         that was ever put in; seems fine (and idles properly) once I
         took it out.
30/12/13 Bugfix. Hitting F12 on "include w32dcore.ew  -- only the..."
         opened "w32dcore.ew  ", with no syntax colouring and at the
         same time as "w32dcore.ew". It now trims tabs and spaces.
02/01/14 Increased the toolbar buttons by 3 pixels since they were a
         bit malnourished on Windows 7. Also completely reworked the
         size and positioning of rtnList (in recalculateClipRect).
02/01/14 Backed out a naughty little hack I had quietly slipped into
         eadir. A database corruption had been causing an ioob error,
         presumably, since after running tools/verify edita.edb, said 
         nlh broke both the project tree and recovery functions. Once
         I took it out, service returned to normal, but I shall keep 
         a wary eye on things for a while.
         Update: see phix/readme.txt 13/01/2014.
04/01/14 More fixes for F12 handling. Added "--/**/" handling to eaqj.
         c_func(xGetLongPathName,{rubbish}) returns rubbish; changed
         both get_proper_path() and F12() to guard against that. Gave
         eaqj.ew a fighting chance with "include builtins\machine.e";
         previously it had no hope of ever getting that right, unless
         the file being scanned was where p.exw lives. See eaqj.e, new 
         variable BuiltinsPath. Likewise for "include machine.e". 
         Lastly I should note that when faced with one of those nasty 
         little compatibility tricks such as:
            --/**/include thingp.e      -- Phix
            --/*
            include thinge.e            -- RDS/OpenEu
            --*/
         it is just going to plonk both files in the project tree, which
         on reflection is probably what you would really want anyway.
04/01/14 Problems with >1 chm file open (apparently phix.chm has no MSDN
         window and mini.chm has no Phix window - I know, and I did NOT 
         ask for either) led me to rewrite the handler, again in eaqj.e, 
         which made little difference. Adding sleep(1) after HH_CLOSE_ALL 
         in the new code solved it. Ugh, and Yuk, but I'll live with it.
14/01/14 Performance issue: spotted the line res[chunkMax-3] = current_len
         in easynclr.e was chewing 40%: after chunk = syntaxColour(...), 
         made sure to set chunk = {} appropriately, in half a dozen places.
22/01/14 Added src/eareinh.ew to reindent html files. This arose from some
         work I did on creating chm files, wherein I developed a fast way
         to reindent files in a separate program. Unlike reindent exw files,
         where you can undo individual changes, this replaces the entire
         file (though you can still undo that). It will also automatically
         insert closing tags as needed, which you can locate via [Shift] F4,
         and if an error occurs it tries to plant the cursor somewhere near,
         but no promises there. Lastly, I was just too lazy to try and get
         it working on OpenEu, so Tools/Reindent will remain greyed out if
         you are running Edita with that (if you still can, that is).
31/01/14 Help/xxx.chm was inoperative, added an else clause to the offending
         if sequence(word) condition. Also made eaisense skip "ex.pro" from
         the ".." entries and select the one-and-only-other ".." entry when
         possible.
02/02/14 Bugfix. Ctrl-Click on "decode" of "p2asm:decode" completely ignored the
         "p2asm:" part. Note that the bulk of the changes required were actually
         in the Phix compiler. Added trapns to pglobals.e to cope. Also needed a 
         loop in ptok.e to scan all the way down the S_Nlink chain. Lastly, added 
         tests ("or chovline!=line or chovfrom!=col") to eaisense.exw which makes 
         the intellilink appearance much more responsive: no more need to move the
         mouse over another word before going back to the one actually wanted!
13/02/14 Finally got round to making earein.ew cope with "declare anywhere". Also
         fixed a nesting level issue in multiple assignment.
23/02/14 Finally got round to making Window Painter cope with "==", ":=", declare
         anywhere, $-terminated sequences, multiple assignment, and enums. (Whew!)
15/03/14 Minor tweak to eamenus/numbConv() when converting signed numbers. Previously 
         it refused to do anything at all when a leading '-' was selected, now it 
         sets a sign flag and/or outputs in twos-complement for hex/octal/binary.
         Note that these conversions are not necessarily bidirectional. For example, 
         -#40000000 -> -1073741824 -> #C0000000 -> 3221225472 (and from then on the
         conversion will toggle between the last two), likewise -1 becomes #FFFFFFFF 
         which converts back to 4294967295. I have also put a hard dword-limit on the
         conversion to twos-complement.
04/04/14 Couple of tweaks to eaisense.ew: when procesing a ".." entry, loop until you
         get to the top of the tree (one without a ".."), otherwise eg pbinary was 
         running with pemit2 as main which gave s5 not found. Also, if there is >1
         child of ".." (still skipping ex.pro and stopping this on arwen/win32lib),
         then any will do: it is probably better to assume they all compile cleanly
         and pick one at random (in fact the first non-ex.pro) and use that, rather
         than bitch and moan about it. If the one we pick /is/ broken, then tough.
02/06/14 Bugfix. (I factored out a function getEncoding(string filename) at the very
         start to enable me to fix this.) The new file open, arwen\fileopen.ew, was 
         always using the drop-down encoding setting from the gui, even if routine
         fn_IFileDialogEvents_OnSelectionChange() had never been invoked to set it,
         which would happen if you type in/select a file from that intellisense-
         style drop-down, rather than select one from the main browse area. It now 
         saves "encfilename" and tests that before trusting the gui, or calling
         that new getEncoding() routine to see what the file actually contains.
02/06/14 Bugfix. Slice ends past end of sequence in showCaret(). Seemed to occur 
         when mousewheeling on a unicode file...
03/06/14 Partial bugfix. Got a fatal divide by 0 error in SaveFile() because 
         "unicodefile" had somehow got set to -1. Replaced the offending ?9/0 with
         a messagebox and reset to ansi (w/o saving), see whether it re-occurs.
         Update: when easynld.e hit an error, it was leaving the file open (fixed)
         which could be why newGetOpenFileName() was returning gibberish. Added an
         extra check that length(name)=2;?9/0 to cover that. Also created js.syn.
         Update: 17/6/14 newGetOpenFileName() returns an encoding 0/1/2/3/4 for 
         none/ansi/utf8/utf16le/utf16be, whereas edita.exw saves unicodefile[] as 
         0/1/2/3 for ansi/utf8/utf16le/utf16be, so it needed a >0 before -=1 (*2). 
05/06/14 Added an "Info" option to the right click menu, which is only enabled when
         Ctrl is held down and an intellink has appeared. Displays the file/line
         where the thing is defined, and the text of the line itself, as long as 
         the file is already open, rather than opening/jumping to it, which will
         hopefully tell you all you need to know, without the hassle of having to 
         navigate back to wherever you were working. There is also a "Goto" button
         on the pop-up message box, if you do decide to explore things further. 
         Note that right click now forcibly repositions CursorX/Y.
10/06/14 More fixes to Tools/Reindent, particularly for the new multiple assignment
         or typed constant declarations, or both, as in say:
            constant string {this,that} = {"the","other"}
         (with real-world/practical useage tending to involve columnize() etc) 
         Also, "then ?x" was quite unexpectedly quite badly broken.
04/08/14 Added setFocusButton to eaerror.ew, to setFocus on in WM_PAINT, since
         MB_DEFBUTTON2 etc were not working properly. Took me quite a while to 
         figure out how/where I had managed this before...
06/12/14 Minor bugfix. Made help on eg ", such as GetCommandLine." work by adding
         a few checks for '.' in eaqj.ew, as '.' is a TokenChar but only in the
         sense of 1.5, and nothing we would ever index in a help file, I think.
07/12/14 Bugfix. The statement "result = 0&{s}" was being "optimised" to append
         and failing with first argument must be sequence. Duff code, labelled 
         "12/5" (which could be as long ago as 2008), removed from Expr(). Bug
         first spotted in wee/windows.ew, line 1626.
23/07/15 Finally completed tooltips. Press Ctrl Shift Space on a routine call
         and a helpful popup reminder appears. If you want a tip for a call
         spanning several lines you'll have to cursor up a bit. Move off the 
         line or press escape to dismiss the tip. Things like c_func do not
         work particularly well just yet, but that will have to wait until
         the whole newEmit thing settles down a bit, or do it all twice.
         Some changes to eaisense.ew/intellitip(), but the main stuff is in 
         ptok.e/isDumpIds(), under trapmode=3. If you really want to address
         some of those builtin/autoinclude glitches, you could always add in
         specific tests and fixed text to isDumpIds(), or better yet do that
         up front in intellitip() itself.

Version 0.3.3
=============
27/03/10 Autocompletes on routine parameters were no longer working.
27/03/10 index out of bounds (-1) when no files could be opened. Added
         a guard to (the recently added) newcurrfile -= 1 in eaini.e.
27/03/10 loadINI() in eaini.e was opening files before linesPerPage & 
         ClipRect were properly set. Split routine in two, the second  
         half is now called from the end of recalculateClipRect().
28/03/10 Two problems in Tools/Verify edita.edb, firstly:
            puts(1,"db_validate passed OK\n")
         changed to:
            setText(Main,"db_validate passed OK")
         and secondly, if too many open files were found in the backup
         directory, it would crash messageBox. By some trial and error,
         limited the text length to 2000 bytes, and put a similar limit
         in arwen.ew/setText(), only there I made it 32000 bytes. The
         2000 is so that the messageBox still fits on screen, the 32000
         prevents low-level crashes in the more general case.
         DEV: a db_compress() problem remains... [update: see next]
07/04/10 Fixed a bug in syswait.ew: it was using "command /C" when for
         compatibility with windows 9x/XP/etc it should be %COMSPEC% /C
         Among other things this meant that Edita/Tools/Verify edita.edb
         gave some pretty wierd messages when running db_compress(), as
         for some unknown reason command.com still exists on windows XP, 
         but it does not understand double-quoted arguments. (On Windows 
         XP you are supposed to use cmd.exe instead of command.com, btw,
         and that is precisely what %COMSPEC% is for.) In addition, a
08/04/10 system() call now waits up to 0.4 seconds to allow things like
         rename to take effect, otherwise db_compress() would invoke eg
         system(ren edita.edb edita.t5) then manage to open edita.edb
         before C:\Windows\system\cmd.exe had actually done anything.
15/04/10 eaqj.ew/routine list now treats "public" as "global".
18/04/10 Possibly/probably due to above; ie 'p'ublic interfering with 
         'p'rocedure, I noticed <Ctrl Q> when editing eaqj.ew, (of all
         things!!), dropped/gained F1Help when global was added/removed.
         Hard to explain properly, I added qJ_global1 etc and changed a
         bunch of "elsif" to "c=0 end if if", seemed to fix things.
18/04/10 Removed the R() "routine_id wrapper" in eamenus.ew; replaced
         with a bunch of scattered r_xxx constants, including r_F1Help,
         which explains how I found the bug just mentioned. Doing this 
         is largely a belated reaction to recently hunting down two bugs
         in RDS Eu 4.0b3; the presence of R()(/a routine_id call which
         could not be resolved at compile-time) meant that Phix gave 
         fewer "is not used" warnings. Nowt else was gained, except
         that Phix should help a bit more when hunting such bugs down.
         The menuset sequence is also less nested and constant PARAMS
         is now 8 not 7 (which may cause the odd hiccup).
28/04/10 Finally completed incorporating get_proper_path() into Edita.
         Actually using it was a snap, but upgrading a dodgy edita.edb
         containing "c:\progra~1\edita" and "C:\Program Files\Edita"
         and "C:\PROGRAM FILES\EDITA" etc, and of course just as many
         Tfiles, Tfolds, Tbackups records for each physical file...
         When you run 0.3.3 for the first time, a message box appears
         stating that "Tools/verify edita.edb" must be run, when you
         click on OK it starts automatically. It is probably a very bad
         idea to try a full background scan at that point, just OK it.
         It will probably find much to fix, and if you keep pressing Yes
         then hundreds if not thousands of these messages will appear.
DEV fix this; disable background scan!
         They are four such beasts: renamed directory, renamed file,
         orphaned file, and file clash, ie/eg it has found information
         for both c:\progra~1\edita.exw and C:\PROGRAM FILES\EDITA.EXW
         (ie last mod, includeset, includeby, folds, backups, globals)
         so one or the other must go. If anything goes wrong in upgrade,
         the standard trick of renaming/deleting edita.edb applies.
         The following should now all target the same file and tab:
            --  edit:"C:\\PROGRAM FILES\\EDITA\\EDITA.EXW"
            --  edit:c:\\progra~1\\edita\\edita.exw
            --  edit:C:\\PROGRA~1\\EDITA\\edita.exw
            --  edit:"C:\\PROGRAM FILES\\EDITA\\Edita.EXW"
            --  edit:"C:\\PrograM FilEs\\eDita\\edItA.exw"
07/05/10 Minor problem in column paste. If you block select a column and
         press Ctrl V, the "incorrect length for non-uniform column paste"
         message might appear. Cancelling at that point left highlighting
         visible but internally off, so it would not get cleared properly.
         CursorY and cX are now left undamaged until after confirmation.
12/05/10 Bugfix probably related to 27/03/10 change. The control variable 
         newcurrfile is not accurate until we have finished opening all
         the files in loadIniPart2(), but if there was a problem, and a
         prompt occurred (via proemh, but messageBox would probably do
         much the same thing) then IdleHandler() would use newcurrfile
         before loadIniPart2() finished. Control flag finishINI now has
         three states, 1 initially, 2 when loadIniPart2() is in progress
         and 0 when done/safe to use newcurrfile.
         The actual problem was index out of bounds in restcursel(), with
         currfile being greater than length(filecursel).
12/05/10 Stopped &/</> becoming &amp;/&lt;/&gt; in Copy with BBcode.
02/06/10 Made saveCurr() and savePrompt() do a changeTo() so you can tell
         what is going on. Previously, if you have 30 files open and one 
         of them is a new/[untitled] file, pressing Alt-F4 would popup the
         Save File As dialogue, leaving you to wonder what it was exactly
         that it was demanding that you save. Now it will be visible in the 
         background, or escape/Ctrl-F4 to discard it, and try Alt-F4 again.
18/06/10 Added code to eamarea.ew to pass Ctrl-Alt-F12 (empty compiler msgs
         and hide if opened that way) through to main text area, when focus 
         (/mouse pointer) is on the message area. Let me know if it passes
         though any other keys it should not.
09/10/10 Largely rewrote HTML.syn, after spotting "DOCTYPE html PUBLIC" was
         missing. Made &lt; and &gt; operators rather than part of keywords.
         Had to make &lt;a&gt;, &lt;b&gt;, &lt;i&gt; "wrongly" coloured, to 
         stop Eg "I am a" looking awful... Left some things incomplete, as 
         I think it should only do the proper validated/able html stuff.
         Added some php, but not much. Does not handle <!--<?php ?>--> as
         anything other than all comment, nor does it only apply the php
         inside <?php ?>, as it should, nor does it handle "##" comments.
05/12/10 Bugfix: keying word left (Ctrl+Leftarrow) on a line which contains
         both a "." and several tabs crashed with an index out of bounds.
         It was using the wrong (ie tab-expanded) char index in routine
         WordLeft() in eatabs.e, when checking whether the "." was part
         of a number or not. This was introduced by the changes made on
         02/11/09, ie is in 0.3.2, not that anyone has reported it.
08/12/10 Since "default" is now a keyword in Phix (part of the new switch
         statement), changed "integer default" to "integer default_view"
         in global procedure onClickToolQjmp() in eaqj.e
28/12/10 Updated number base handling in Reindent (earein.ew) so that it 
         now recognises 0xFF, 0o377, 0(2)101, etc. Actually, I have made
         quite a few mods to earein.ew over that past few weeks, somehow 
         never logged here, mainly to get RDS 4.0 sources recognised.
23/01/11 Several fixes to reindent. First, on a line such as:
            --/**/  XXXX
         then Ctrl/Shift Tab now move the "XXXX" and leave the "--/**/"
         in column 1. This is a prerequisite for handling code such as

--/**/          kv = keyvalues(cpdata[pos],whitespace:="")
--/**/          free(addr)                          --/*
                kv = keyvalues(cpdata[pos],,,,"")
                machine_proc(M_FREE,addr)           --*/

         <aside>
            Phix does NOT support the (horrid) ",,,," handling, you have
            to use named parameters instead (ie text.e defines an entry
            keyvalues(...object whitespace), which is where I got the
            "whitespace" from - see Phix's readme.txt for more details).
            Phix _does_ support calls to machine_func/machine_proc, but 
            issues deprecated warnings, since there is always a better
            (faster/cleaner) way to do it (see builtins\pmach.e).
         </aside>
12/02/11 Fixed syntax colouring for multi-line strings.
20/02/11 Fixed arwen listview extended style handling. When you create a
         ListView, and supply an extended style (by passing a sequence as
         the last parameter to create), it now saves those bitflags for a
         later LVM_SETEXTENDEDLISTVIEWSTYLE message, rather than passing
         them to the xCreateWindowEx call, which don't work.
11/03/11 Added switch statement to Ctrl [ handling, along with treating
         "public"/"export"/"override" the same as "global".
13/03/11 What was supposed to be done and dusted on Saturday morning has
         just taken all week: Tools/Re-indent now handles:
            ifdef statements,
            switch statements,
            triple-quoted (""") and backtick (`) multiline strings,
            number bases, such as 0b1011,
            #ilasm (etc) now skipped/ignored,
            optional removal of unnecessary spaces,
            two consecutive blank lines now "break" a comment block,
             when said is (un)indented along with some code, but
             only at top-level, not inside a routine.

         You are now prompted for two options:
            Remove spaces in expressions?
            Align ifdef in column 1?
         The full effect of these is best seen by experimenting with
         src\indent.exw, provided expressly for that purpose.

         Personally, I want unnecessary spaces removed from expressions; 
         while I accept the view that such additional spaces may often
         aid readability, I very rarely see them applied in a consistent 
         manner. When I see "length ( a ) >length(b)" it is just a (very) 
         annoying distraction. I also code "if a=b then c = d end if" to
         make the (more important) assignment stand out. Be warned that 
         while you can <Ctrl Z> undo any changes, before shutting down 
         Edita, or re-open/close without saving, there is no such thing 
         as an "insert spaces" option.
        
         The general consensus is that ifdef should be aligned as part
         of the code, but my preference is left-align, make it stand out.
         Note that some (currently illegal) mixes may look rather odd if
         this is unchecked. While unchecked might be the preference of
         most people, you should be aware that considerably more testing 
         has been performed with this option checked.

         Note: Tools/Re-indent does not, and never will, handle "goto".
               Instead you need something like:

    --/**/  #ilasm{jump_rel32,%isJmp,0,0,:MULTILINE_COMMENT}    --/*    -- Phix
            goto "MULTILINE_COMMENT"                            --*/    -- RDS
    ...
    --/**/  #ilasm{::MULTILINE_COMMENT}                         --/*    -- Phix
    label "MULTILINE_COMMENT"                                   --*/    -- RDS

            (and if that deeply upsets you, then do not use this!)
02/04/11 Update: earein.ew still not finished...
02/04/11 Improved filter handling on Ctrl Q so you can now type/delete
         the filter criteria anywhere, making it much slicker to use.


Version 0.3.2
=============
14/07/07 Just as Ctrl Left and Ctrl Right jump to start of prev/next word,
         Ctrl Alt Left and Ctrl Alt Right now jump to end of prev/next word.
         Note that block selection (Ctrl Alt Shift Left/Right) works exactly
         as expected within a single line, but uses column mode because the 
         Alt key is down when selecting multiple lines. Hence a Shift L/R 
         may be needed at the end to obtain normal block selection.
18/07/07 Added Show as Hex and Show as Decimal to Edit/Case. You must block 
         select a single number, with leading # for hex nos (/the latter),
         for these options to have any effect.
         I wanted them mainly for looking at ex.err files, in my case hex file 
         addresses, but probably occasionally useful for things like WS_BORDER 
         and the like. 
         [I know this is a bit of an odd place to hide these obscure functions,
          though they are a bit like upper/lower only with number bases...
          Any suggestions for somewhere better to put them will be considered.]
20/07/07 Made Ctrl E/R (Enlarge/Reduce font size) work on F1 Help and Ctrl Q
         windows.
01/08/07 Bugfix: Using > and . to represent spaces, with tab width=4, copy
            ;>  p2.>>   ;
            ;>  p3> >   ;
         then inspection of the clipboard or a paste would show the error.
         The tab conversion routine assumed that the > in ".>" on the first
         line was a full_width tab, ie represented 4 spaces. It now does the
         correct "tab_width - remainder(logical_pos,tab_width)" calculation
         on each and every tab char encountered, instead of just the first.
         While the effects of this are not normally serious, and only occur
         in the hopefully rare case of <space><tab>, I have noticed the long
         term behaviour is accumulative, especially over repeated file saves
         and generally speaking moreso on txt/html/asm files than Eu source.
         Thankfully, at no point did this stop any programs working (which
         would probably not be true for Python sources).
         PS Tab conversion is also suspected to be dodgy when copying text
         not starting from column 1, but I haven't nailed the test case for
         that yet, sorry. [update see 24/9 entry]
12/08/07 Made Ctrl Shift L toggle Line nos.
17/08/07 Bugfix in Tools/Find in Files. Searching the current file (when 
         Current Dir, Euphoria files, and Euphoria Includes all unchecked)
         was not supplying the path and quietly failing to open the file.
19/08/07 Woohoo! Finally figured out that all those miscoloured brackets
         I get from time-to-time are a result of Paste() not updating the
         bCFwd table properly. It now performs a syntaxColour() on each
         inserted line. Huge paste ops may be a tad slower but worth it.
         [update 31/10. Has improved matters ~90% but a few remain.]
         [update 15/03. Another common cause eliminated.]
10/09/07 Escape char '#' added to Euphoria.syn, for eg "Hello\#07".
         Tweaks made to syntax colouring routine for eg '\#D4' (not that
         you need the quotes really) as a 4-char squote, and check two 
         following chars are proper hex 0-9/A-F/a-f. (For Phix only.)
24/09/07 Fixed the following bug in copy/paste:
copy the comment on first line to second.
    mov eax,[ebx+4]            ; infile
    mov eax,[b_ebx]
         It now does some convoluted fiddling when copy/paste is not on
         a tab stop or starting in column 1.
07/11/07 Bugfix: variable k has not been assigned a value when double-
         clicking on a url. [?]Must have been there for 8 months![?]
08/12/07 http: links now properly created by code publisher.
17/12/07 bugs with alt+ctrl keys fixed. Thanks to Jesus Conseugra.
18/12/07 replaced ShellExecute with ShellExecuteEx in F1Help when opening
         a .hlp file and TerminateProcess(old) to stop there being multiple
         help files being left open at the same time. If there is any demand
         for it I could make Alt-F1 leave .hlp files open.
10/01/08 Close open help files when closing Edita.
         [update 15/03: not sure this works anymore...]
10/01/08 Open new file tab next to current, rather than at end.
11/01/08 Fixed another minor tab/space error in column paste, similar to
         that fixed 24/09.
13/01/08 Suppressed the re-open pop-up if file has been externally modified.
         (ie it no longer incorrectly asserts "file has not been modified".)
15/01/08 Made tedb (Database Viewer) save the detail window position.
19/01/08 Bugfix: slice starts past end of sequence (1 > 0) in eafind.ew when
         F4 pressed and the find default is of a length l such that the next
         '>' or '<' line plus l was a blank line. The problem/fix is that it
         should not be using a previous find default; it now uses {{}}.
17/02/08 Couple of bugfixes in Code Publisher: stopped edit:xxx from appearing
         as real/clickable url, changed generated html so that tags balance.
         ie/eg <font><b>...</b></font> instead of <font><b>...</font></b>.
         Underscores were running on a bit wildly. This may be due to bugs in
         the Print code of Opera 9: while of course balanced html should have 
         been output all along, the purists may say, if it displays right but 
         prints wrong, then I still regard it a bug (in Opera 9).
02/03/08 Increased a limit of 200 to 2000 in eafind.ew, since I happened to hit
         it in normal/reasonable use. See entry dated 18/11, in 0.2.6 below.
         (btw: the code I just found (wot I wrote) actually had the above said 
          limit of 200, not the 600 as stated below.)
22/04/08 Made ctrl Q, and the toobar dropdown, work properly on ex.pro files.
05/05/08 Crash in removeLineLengths after compare. Replaced the old style
         selectAll()/deleteSelection()/addAction()/InsertBlock() with the 
         rebuild linelengths and direct update code to match that in eacca.ew,
         eafif.ew, and eaxutil.ew (DEV: should probably be common code..)
18/06/08 Bugfix: Pressing Ctrl H (show as Hex) on eg 8585199620 crashed with 
         the error "number too big for %x or %o format", a limit in sprintf().
         The value is now converted manually.
12/07/08 Added Ctrl J to toggle between sequence and string, for example if
         ex.err contains iset = "%3>?M" then if you block select the string
         pressing Ctrl J toggles between "%3>?M" and {37,51,62,63,77}.
23/07/08 Tweaked saveFile to restart the autosave timer, so that autosave 
         only really kicks in when not regularly saving files manually.
21/04/09 Index out of bounds line 472 in eafif when searching for "000004".
         A missing exit made nphits go negative.
24/04/09 Made select word (ctrl w) on hex nos (eg #4000) include the #.
26/07/09 Made "Show as Seq/Str (Ctrl J)" (on the Edit\Case menu) properly
         handle \t, \r, \n, \", \', and \\. The main purpose of this, btw,
         is that when an ex.err contains a cryptic "®\n", selecting it
         (quotes inclusive) and pressing Ctrl J shows {174,10}, which may
         hopefully be a bit more meaningful and helpful when debugging.
26/09/09 Added File/ReOpen option (Ctrl Shift O), previously this was
         only available by right-clicking on the file tab. Completely
         rewrote the "Control and Alt keys" part of F1 keyboard help 
         (in elng_ENG.txt), and you now need Shift to Enlarge/Reduce 
         font size, since it was too easy to press them by mistake.
02/11/09 Fixed WordLeft/WordRight to skip ".." (and "..." etc) properly.
         Prior to this, it treated eg text[1..4] the same as text[1234].
         It now stops properly on the start(/end) of the 1 and the 4, 
         that is when using Ctrl(+Alt) left/rightarrow.
30/11/09 After some deliberation, made ''' legal on all files. In Phix,
         this works the same as '\'', which is what you must use on RDS 
         Eu, since that barfs on '''. Hence in that sense the syntax 
         colouring is wrong if you're editing source files to run on 
         RDS Eu, but OK if they're for running on Phix. However, it is
         surely only mildly annoying and not that common. I wanted this
         mainly for list.asm, and ex.err, where a sequence of {-1,39,65} 
         is shown as {-1,39''',65'A'}.
23/12/09 Nearly drove myself nuts on this one:
         p.exe, exw.exe, exwc.exe all worked fine, but pw.exe on windows XP
         [which worked fine on windows 98] used 100% cpu time... Yet the
         only difference between p.exe and pw.exe is one byte... After four
         days of hacking, I finally figured out it was all because I had a
         pw.exe.manifest file but no p.exe.manifest file; once I created
         the latter it was clearly the cause. Prevented the looping by 
         by adding a msg!=WM_NOTIFY check in mainHandler() before restarting 
         the idle handler.
27/12/09 With a manifest file (on windows XP therefore), the radiobuttons in 
         Options/Colours appeared as black text on a black background. Added
         classStyleEx[RadioButton] = WS_EX_TRANSPARENT to classes.ew.
         Also resized the Display Options window and moved the OK and cancel
         buttons on Options/Fonts slightly.
          Update: 27/2/10 you no longer need a separate manifest file,
                          at least not when running on Phix that is.
28/12/09 Modified Phix to send compilation warning messages direct to Edita.
         These generally appear last-first, due to the fact that in pixl87.e
         routine unused_cleanup() processes symtab from 1 to symlimit, and
         in edita.exw, routine CDmessage() inserts each message it gets at
         the top. This is intentional, since then you can edit/delete line 
         9746 and line 4853 will still be exactly where it was before.
         However, there are many ways for messages to appear out-of-order, 
         for example:
            constant a={1111,2222,3333},
                     b=1111,
                     c=2222,
                     d=3333
         (assuming all are unused) gives a,d,c,b on lines 1,4,3,2. This is
         because it first creates "unnamed" constants 1111/2222/3333, and 
         then "hijacks" them for b/c/d, so symtab end up like this:
                symtab[996]:{b,S_Const,1111}
                symtab[997]:{c,S_Const,2222}
                symtab[998]:{d,S_Const,3333}
                symtab[999]:{a,S_Const,{1111,2222,3333}}
         Thus, remembering to read backwards, you get a,d,c,b. Of course in
         this example, it's only "a" which is out of order; the first time 
         I noticed anything like this it seemed to be well scrambled.

         The point I would make here is either just accept they are out of
         order, or solve the problem in CDmessage, not in Phix. Another one
         you'll get is eg {file:line\n"...+..."\n   ^ sq_add assumed}, ie
         the message is three lines, and Edita copes poorly with that, again
         something probably best solved in CDmessage, or possibly F12().
            Update 29/1: changed F12 to skip lines it does not understand;
                         naturally this may not be the perfect solution.

         You may also want to see the new peama.ew, and the modifications to
         pmsgs.e in Phics but as above probably only for fairly minor fixes. 
         Note that if lang\elng_ENG[or whatever].txt has anything other than
         "Edita"="Edita", then you'll only get warnings etc on the console 
         window, not in Edita's message area.

         Alt-F12 jumps to the source/line at the top of the message area. It
         also quietly "block-selects" that line for deletion the next time
         you press Alt-F12. Lastly, Ctrl-Alt-F12 clears the message area.
            Update 29/1: Ctrl-Alt-F12 now clears and, if the message area
            was opened via F5/CDmessage, hides it. You can of course hide
            the message area without clearing it with a Ctrl-M.

         These modifications are designed to be most useful when hacking a
         large program down to something small enough to debug, especially
         for example when it's a nastghly low-level bug in the compiler!

31/12/09 Changed "UNDEFINED" colour on tabs from COLOR_MENU to COLOR_BTNFACE
         since that was much more obviously wrong on Windows XP. After some
         experimentation with XP-style tabs I added "Owner Drawn Tabs" to 
         Options/Display, default True. Unchecking this gives you the new 
         XP-style tabs, with the following problems:
            * Coloured backgrounds and text are NOT supported; the right,
              or do I mean ONLY, way to overcome this would be to implement 
              ImageLists and have appropriate icons on the tabs instead. 
              That may take me quite some time, unfortunately. To repeat
              myself: Do NOT try to put custom colors on an XP-themed tab.
            * Tabs at bottom of screen are the same way up and look silly.
              This is of course a windows XP problem and probably different
              in Vista/Windows 7 and bound to change again in Windows 8,
              which is further ammunition for the "use icons" argument.
            * Of course, you [may] need a manifest file to see anything
              even mildly interesting.....
         I also made a minor improvement to the appearance/position of 
         (owner-drawn) coloured tabs at the bottom of the screen.
            Update 31/1: setSaveIcon() in edita.exw should not call
            setTextColour() unless isTabsOwnerDrawn is True, otherwise
            it causes a partial redraw as soon as you change text, ie
            the whole screen gets cleared but edita only redraws the 
            lines that have been altered.

01/02/10 Minor problem in eaclip.ew: getTextFromClipboard() assumed that
         any '\r' found were part of a "\r\n" pair; should the clipboard
         actually end with a \r it crashed with "slice ends past end of
         sequence". (Triggered by a bug in another clipboard function in 
         a different application.) It now takes more care.

15/02/10 Added Edit\Case\Show as Binary (Ctrl B) to go with Ctrl H (Hex)
         and Ctrl K (Decimal). Note that "0b01010" format is supported by
         Phix, whereas RDS Eu gives a compilation error.

24/02/10 Bugfix: PageUp inoperative on a long word-wrapped top line. 
         Occurred when examining a .css file I had downloaded (17K all on
         one line). Wordwrap, page down was fine but then pageup did nowt.

24/02/10 Improved status bar hints when perusing the Help menu. Also did a
         little bit of cleanup, eg replacing [5] with [TB_mid], defined as
         a new constant.

25/02/10 Rewrote string handling in rebuildbCfwd in easynclr.e to more
         closely match that of syntaxColour(). Prompted by a list.asm 
         with wrong colours that corrected themselves when cursoring up
         to a line with '\' in it (similar thing to 30/11/09 entry).

28/02/10 Fixed a problem in Macro management where it refused to parse
         a literal integer, making permanent setup impossible.

06/03/10 Allowed Ctrl V/X/Z to work on the message area (Ctrl C
         already worked) and passed other messages through to Main.
         This, in particular, now allows Ctrl M to close/hide the 
         message area when the cursor was over it. Wasted some time
         trying to get Ctrl A (for select all) to work, ditto Ctrl Y,
         before I realised they do nowt on a standard MLE anyway.

08/03/10 Advisory: HTML block comments are denoted by <!-- and -->.
         PHP block comments, within a <?php ?> block, are denoted by
         /* and */. Nesting of block comments has never worked very
         well, and as a .php file can have TWO types of block comments,
         at least one of them is going to work even less well. For now,
         */ is ONLY recognised in column 1. Anyone who fancies giving
         block comment handling in easynclr.e a major overhaul is more
         than welcome to it. ;-)

15/03/10 Mainly for list.asm, changed the way ppp.e represents '\"':
            \"blah ==> #22&"blah
            blah\"blah ==> blah"&#22&"blah
            blah\" ==> blah"&#22
         As now noted in ppp.e, maybe this should be an option, so let
         me know if this causes you a problem. The above omits the usual
         surrounding double quotes for clarity; if they are not being used
         (ie ppp_StrFmt=-1) then clearly the above is wrong, but then like 
         as not so were the \" this replaces.

15/03/10 rebuildbCfwd() in easynclr.e was using "--" as the comment, so 
         it didn't work proper on non-Eu/Phix files.


Version 0.3.1
=============
05/06/07 Bugfix. Exit full screen mode was not restoring the screen properly.
         Bugfix. Ctrl Shift F2 was not removing any bookmarks, let alone all.
         Bugfix. Common code analysis caused ioob in adjustLine.
         Bugfix. Ctrl Z after Find in files caused undo error message:
                 Standardized the code in eacca.e, eafif.ew, & eaxutil.ew
07/06/07 Bugfix. Insert/delete line updating end of folds incorrectly.
08/06/07 Added code for <br>, <meta>, <hr> tags, and <li> without a closing
         </li> to the Strip Html function.
10/06/07 Rewrote the Strip Html function, now approx 2.5 times faster (can still
         take a while on large files though), and made Copy with Html convert
         "&<>" to "&amp;&lt;&gt;".
11/06/07 Bugfix. Ctrl PageUp/Down "over" an "[untitled]" filetab would set the 
         filetab to "1". Jumping the '1' back "over" another file would seem to
         rename it as "[untitled]". Been around a while that one, just took me 
         sweet time to get round to locating it (quite trivial, in the end).
19/06/07 Completed the tab colour part of new Options/File Extensions.
         The "Run With" part is not yet used.
19/06/07 New routines added to arwen: setBackColour, getBackColour, plus the
         changes mentioned on EUforum to drawTabItems().
         Also added a new routine fixWindow() which removes the minimise and
         maximise buttons on the titlebar, and stops cursor over border from
         changing to resize_drag cursors. Since Windows prevents or at least
         just ignores the dynamic removal of SBARS_SIZEGRIP on StatusBar 
         controls, a create(StatusBar, ... {x,x,0}) statement omits it, ie
             if sequence(style) and length(style)=3 and style[3]=0 then
                dwStyle -= and_bits(dwStyle,SBARS_SIZEGRIP)
             end if.
         (I would have prefered fixWindow(SB), but sadly not to be...)
            DEV: 10/03/2010: perhaps the right way is destroy/create...
20/06/07 Added Alt Shift M to toggle multi line tabbar.
22/06/07 Almost complete rewrite of F5 handling to use the new Options/File
         Extensions info. If F5 cannot build a suitable command string, then
         instead of "unrecognised file extension" it now opens the Ctrl F5
         window. Capture console output has changed significantly; see the
         Help on the Ctrl F5 window. Doubtlessly introduced several bugs.
23/06/07 Bugfix: Alt Shift M trashing caret.
         Bugfix: Ctrl Home and End now jump to routine start/end in F11 mode.
26/06/07 Completed hlp and chm integration/configuration.
         In short, a bunch of .txt files in Edita\help allow say:
            WM_COMMAND -> win32.hlp
            db_open -> euphoria.chm
            getFileExtension ->edita.edb
            createEx -> encyeu.hlp
         For more details see
         http://palacebuilders.pwp.blueyonder.co.uk/docs/manual.htm#UDH

29/06/07 Allowed Alt keys to be specified on the Project/Directory/Recovery
         tabs of the filepanel (in the elng_XXX.txt file). In English, Alt J/D/R
         select tabs when the filepanel is visible. The Alt U/I/L and W of the
         case conversion (see Edit/Case) and Wordwrap keys are now collected 
         from the elng_XXX.txt file rather than hard-coded.

Version 0.3.0
=============
20/05/07 Bugfix. Setting background colour to black would case the help screens
         and quick jump (Ctrl W) window to display black text on black paper.
         Thanks to DB James for spotting this one.
21/05/07 Bugfix. Cursor left from column 1 in wordwrap mode would fail if the 
         previous line was longer than the first "chunk" of the current line.
21/05/07 Bugfix. Enlarge/Reduce font under wordwrap was miscalculating the 
         caret position, largely because it was doing so before the wrap table
         was correctly rebuilt. I also found that the above and wordrap toggle
         (Alt W) were not always properly forcing the caret onscreen. Migrated 
         code to a new routine (rebuildWraps) and called it appropriately.
22/05/07 Bugfix. Up key not working (at all) on a wordwrapped line 1, that is 
         when the cursor is on chunk>1.
??/05/07 GB supplied me with two fixes: delay any "missing lang" message until
         after the single instance check, and scan for exw in EUDIR etc before
         restorting to the (2.4) one shipped with Edita "plus" version.
25/05/07 Added a warning pop-up if the number of wraps exceeds 2000. For more
         details, see wordwrap.txt [since removed, see next]
28/05/07 Completely rewrote bookmark, fold, and wordwrap table handling, from 
         a "packed table" to a "sparse table". The severe slowdown noticed on
         a large file with nearly every line wrapped no longer occurs.
         Word wrapping is now based on the syntax classes, so for example the
         line "something=somefunc(var,123,456789)" can now word-break in nine 
         places, whereas previously it only broke at whitespace.
29/05/07 When editing an FTP file, made F5 (run) get the remote filename and
         use that (after re-upload if needed) instead of the local filename.
         (erm, I blindly assumed it would be a html file being edited..)
         Note that my ISP stores my home directory in /htdocs/, which I check
         for and strip (see routine Reupload() in pFTP.exw) and then build a
         http address as "http://"&username&servername[4..$]&dir&file.
         Your setup may differ, let me know.
30/05/07 Added status bar messages to background processing. I intend to try
         further improvements in this area: 1) make recalculation of bCfwd 
         and scanning for globals re-entrant, ie process approx 2000 lines 
         at a time and resume where it left off on next timeslice, and 2)
         reorganise checks by directory and avoid a complete scan/dir() of
         every single file when any is saved. If you find these messages a
         bit annoying, as I do, then my apologies, but hopefully it will
         eventually nag me to figure something out for point 2 ;-))
30/05/07 Removed the "knownLang" table in eaxlate and started using the 3-
         character lanugage abbreviation provided by Windows. ealng_en.txt
         has been renamed to elng_ENG.txt, and stubs have been provided 
         for ENU (American English) etc. It should now be possible to add
         a new language file without editing any source code. See also
         lang/locales.txt
30/05/07 Started running the 28/05 rewrite "live".
01/06/07 Added a counter for the duplicate backup warning, so it is not
         shown until it happens ten consecutive times. (It is not of any
         importance, unless it gets into an infinite loop, btw.) Also 
         fixed a bug in the new "sparse table" code (from 28/05).
03/06/07 Alt - now retains any existing folds entirely within or entirely
         outside routine start/end boundaries. Note that Alt + expands all 
         folds, as documented; it is only Ctrl + and existing folds outside 
         routines which exhibit any change in behaviour.
03/06/07 if debug=1 in edita.ini (unlikely except on my machine), if there
         are any files on the command line, they are now passed to the first
         instance rather than starting a second.
04/06/07 Minor fixes to horiz and vert scrollbar calculations, also replaced
         the ineffective postEnterIdleMsg() with direct call to IdleHandler.
         Fixed crash if Ctrl + pressed on a wordwrapped line.
05/06/07 Added errors to Options/Colour if any "Paper" colour (ie Background,
         HighLight, Current Line, or Marginbg) matches any "Text" colour,
         with Linenos & BookMarks only tested against Marginbg and the other
         three papers tested against everything else. An error prevents the 
         Colour dialogue from being closed until it is corrected. Thanks to 
         Jesus Consuegra for pointing out some unexpected colourings if such
         errors go unnoticed.

Version 0.2.9
=============
24/04/07 BUGFIX: Bad definition of xSendMessage caused type check at line
         1551 in arwen.ew.      
24/04/07 Added entries to Options/Colours for the margin background, line 
         numbers, and bookmarks.
25/04/07 Added start/end of string handling to <Ctrl [> and <Ctrl ]>. If
         the cursor is before the opening quote, it jumps to after the
         closing quote, if on the character after the opening quote, it
         jumps to before the closing quote, and vice versa when starting
         on or after the closing quote. At the same time I made bracket
         matching behave in a similar way and removed the annoyance that
         when <Ctrl [> or <Ctrl ]> is keyed at a non-matchable position,
         the cursor jumped to the start of the line for no good reason.

         Note that the string handling is based on syntax colouring, and
         hence may not function correctly editing say text or html files, 
         inside comments, or if the string colour is not unique (ie in
         Options/Colours, the fifth radio ("Strings") [which must be 
         visible, ie not under "None.sym" like .txt files are] has a 
         different colour to say Other, and all bracket colours, etc).
         If you want to improve on this, see the ten lines 101..112 in 
         eacflow.e. (I kinda only added this cause it was that trivial,
         especially with that -pchar/+(1-pchar) trick; it has in fact 
         taken me *longer* to write this up as it did to code, test, 
         and debug ;-).
25/04/07 Made up/down work properly within wordwrapped lines. Just the
         plain keys, not Crtl/Shift/Alt, that is.
27/04/07 Spend six solid hours watching crap on the telly. Result! The
         penny dropped and with two instances of approx 4 seconds each
         of actual ideas forming (vs ten+ attempts to force), starting
         display mid-line/on chunk N is now confirmed possible ;-)
28/04/07 Managed to get Ctrl up/down working with smooth scrolling.
         (By "smooth scrolling" I just mean everything is moved by one
          charHeight vs jumping about some random multiple of that.)
29/04/07 Managed to get Normal up/down working with smooth scrolling.
         Also, Alt up/down is now (properly) non-smooth scrolling.
         The vertical scrollbar now works (but probably only after all
         lines have been physically displayed at least once). Mouse
         selection now seems ok as well.
30/04/07 Erm, how do I best describe this? Basically I suppose the thing
         is I've gotten wordwrap "draft"/"prototype" all working. Then 
         again, all I've really done at this stage (kinda planned) is 
         finished the shoe-horn of some new ideas into already cluttered 
         code, created at least one known inscrutable bug (shift F3), and
         completely broken Code Folding. I am very very happy with this.
         Now I understand what I am doing, all it needs now is a complete 
         rewrite ;-)) I'll take a few days off [this] to ponder things...
30/04/07 Added C.syn to distribution, which has mostly been tested on,
         and in fact mostly created while examining, the Eu sources. 
         Additions welcome, though I would prefer that any compiler-
         implementation-specific functions go in a new section, so 
         they can be given a different colour as a hint that #ifdef 
         thingies might be needed. (Me no speakum C proper, btw)
01/05/07 Completed the integration with pFTP and added the Previous FTP
         Edits window. For excruciating detail, see pFTPandE.txt
04/05/07 Fixed the bug in F11 which saved files unnecessarily.
09/05/07 Fixed a bug in ex.err preprocessor with literal escape leadins
         whereby if one={13} and two={92,110} they would both appear as
         "\n"; now they are shown as "\n" and "\\n" respectively (ie the
         first really is CR, second as per "C:\\test\\new").
11/05/07 Completed the rewrite of wordwrap.
12/05/07 Incorrect bracket colouring in Code Publisher fixed.
         You can now also use Edit/Clipboard/Copy with Html to get the
         html-ised code into the windows clipboard. There is also a new
         option Edit/Clipboard/Strip Html to reverse the process, ie
         copying the current file/selected text to the windows clipboard
         but removing html tags first. If you have html-ised code in the
         clipboard but want stripped text out, you have to Paste, then
         key Ctrl Z, Ctrl Shift Y, Strip Html, Paste.
14/05/07 Made refresh in pFTP keep the same view (as far as possible). 
         Also added some status bar messages.
15/05/07 Keyboard help incorrectly displayed due to tab-8 mods.
17/05/07 Bugfix: Ctrl Q was putting section info into the toolbar routine
         drop-down. For example, if we have (as exists in edita.exw):
            Line 566: function adjustLine
            Line 735:    end if
            Line 736: --=======
            Line 784: end function
         The setListOfAllRoutines( , ,2) call in IdleHandler[*] sets global
         variable routines {name,start,end} to {adjustLine,566,784}, however
         the setListOfAllRoutines( , ,1) call via Ctrl Q was including the
         section and setting routines to {adjustLine,566,0},{end if,735,784}.
         (The actual data structure used is, erm, orthogonal to that shown.)
         Also moved call to setRtnIndex() to before backGroundProcessing() was
         totally finished, previously you had to wait for the red triangle to
         go grey before believing the routine drop-down.
         [*] It may prove necessary to insert another such call later on in
         the idle handler/backGroundProcessing() as the routine drop-down may
         now never be rebuilt except for opening a file and startup, though
         it should be adjusted OK by normal editing. I shall wait and see
         whether a problem actually exists/can be reproduced at will first.
18/05/07 Fixed a bug with Column select landing "mid-tab" and ending up
         deleteing the remainder of the line. (MapToByte mod)


Version 0.2.8
=============
30/12 Finnish language support added, thanks to Tapani Talvitie
      Portugese renamed from ealng_po.txt to ealng_pt.txt.
      Spanish renamed from ealng_sp.txt to ealng_es.txt.
30/12 BUGFIX: Right clicking on a file tab popped-up part of the new
      Fold submenu instead of the correct menu.
02/01 Hack: Prevented a couple of possible crashes in Ctrl [ and ] handling 
      if the routine table was out of whack with reality (which it should
      never be, in theory...).
12/01 Added ,{ #240A, "ec", "Spanish (Colombia)"} support for harold fonseca.
12/01 Made F12 open the file under the cursor if on an include statement.
      Uses Project info if possible, also supports --#include lines.
      Also allowed open on eg " D:\positive\ptok.e:" lines (ie with no line
      number, defaulting to line 1 unless file is already open) within the 
      "Global & Local Variables" section(s) of an ex.err file.
12/01 Fixed a problem with autocomplete and multiple leading characters.
      <dead PC put something of a spanner on development around here>
??/?? Find in files: added a new checkbox to search backups (default now 
      not to), and added number of files, directories, and lines searched 
      to the output.
05/05 Fixed bug in F12 handling whereby it could open C:\Edita\edita.exw
      and C:\Edita/edita.exw.
15/05 Added Tools/Re-Indent. Disabled for non-Eu sources. It may well pop
      up eg Warning: bool assumed to be external type, which I've tried to
      keep as unobtrusive as possible by merging such with final stats.
      To test this feature, after running it on some suitable code, press 
      and hold Ctrl Z until it stops, then Ctrl Y each change back in, 
      ensuring it makes sense.
      You can mark a section of code that you don't want this function to
      mess with using --#without reformat and --#with reformat directives
      to hide ranges of lines; there are a couple of examples of this to
      be found in edita.exw [possibly just highlighting my personal wacky
      notion of inventive use of whitespace ;-)].
23/05 Added resize logic for the toolbar's routine drop-down. Also stopped
      toolbar init from creating two toolseps in a row.
23/05 Bugfix. Increasing the tab setting in options and not exiting /
      restarting Edita immediately caused an index out of bounds.
20/06 Made Edita handle p.err as it does ex.err.
23/06 Added Tabs List window to the file menu. This lists the current open
      files in a sortable and filterable listview, useful when several dozen
      files are open and they tend to get lost in the crowd. Files are listed
      in the order most recently selected (from this new window) first. The
      'most recently selected' info is not saved between edit sessions. To be
      honest, I added this function for myself, as I have some 115 files open
      which requires 9 lines on a multi-line tab (you may well be asking why)
      and therefore eating up way too much screen real estate. Now that I 
      have this little ditty, no doubt I shall open every file on my disk and
      never ever shut anything ;-))
06/07 Changed stacked_dirs from append to prepend in "find in files". While 
      there is no guarantee of alphabetical order, previously it might list:
            root
            root\a
            root\b
            root\a\a
            root\a\z
            root\b\z
            root\b\a
      Now it lists grouped by directory, more like:
            root
            root\b
            root\b\a
            root\b\z
            root\a
            root\a\z
            root\a\a
      Normally this does not matter a jot, but it makes it easier to delete a
      whole block of (nested) root\old or root\backup results.
29/07 Fixed a few bugs in column paste. It will now pad short lines correctly,
      previously if the column selection was zero characters wide it would do
      nothing, and there were a few problems after column paste with undo/redo.
31/07 Pressing F1 help when on a number, say 13, would give "Local definitions/
      uses of 13 in this source". It no longer does this for numbers, but shows
      the keyboard help instead.
17/08 Added support for nested block comments.
04/09 BUGFIX: crash after F12 if line number in ex.err was past end of file.
16/10 BUGFIX: Ctrl O (open file), Ctrl N (New file), Alt F (drop-down File menu)
      and probably a whole bunch of other keys inoperative when no open files.
      Bug has apparently existed undetected since 0.2.6 (0.2.4 works, but 0.2.5
      was not checked).
22/10 Added new option 'Space after margin' to display options.
22/10 Improved F1 help to catch routine parameters which go over a line break.
23/10 F1 help on 'if', 'end' etc would display 'local definitions of/uses in this
      source'. It now displays the keyboard help (as per 31/07 entry).
23/10 Left Run enabled on '[untitled]' tabs, forcing a save if clicked.
23/10 Finally fixed the annoying display problem in F1 help where the close button
      was wrong size and everything right/below had an odd white background.
06/11 Status bar now shows help when using keyboard/cursor through menu entries,
      not just when using the mouse (?why was that?) and also does not display 
      the odd '&' in a few cases.
27/11 Added Tabs at Bottom of Screen to display options (as requested by don cole)
??/01 Messed up tab handling royally...
09/03 Allowed eg file:"C:\Program Files\Edita\edita.exw" (spaces in hyperlinks).
2?/03 Minor Swedish language mod. Swedish users should now be told that the file
      lang/ealng_sv.txt does not exist; use Tools/Translation Assistant to create 
      one (enter a language code of "sv"), if you email it to me I'll add it to
      the next release.
30/03 Added eg edit:"C:\Program Files\Edita\edita.exw" (opens file within Edita)
30/03 Added Invert case (Alt I). Eg fOR WHEN YOU TYPE hELLO instead of For when 
      you type Hello.
03/04 Added (finished) legacy tab character handling. Versions of Edita prior to
      0.2.8 stored tabs (\t,#09) wrongly. Any \t on disk (or the clipboard) should
      correspond to the defacto standard of tab-8. Edita now does the correct
      conversion, if say Tab Width is 4 in Options/Edit. However the files on your
      disk are technically wrong, so I added a cut-off date (set when you install
      and run >=0.2.8). See ltchelp.txt, which is loaded via Options/Edit/Help and
      ReOpen/Help, and on startup the first time after installation.
      There may still be some problems with this, let me know.
04/04 If Load ex.err on error is not checked, Edita now jumps direct to the line in
      error and puts an error summary in the status line. For some reason I thought
      this would be hard, but it turned out to be trivial, and all round much nicer!
      F12 still opens the ex.err if needed, so I recommend unchecking this option.
06/04 Added Ctrl E to Enlarge font and Ctrl R to Reduce font.
      Added Ragged Right Tabs option, if multiline tabs are in use.
13/04 Added right click file tab drag. Previously, the tab context menu (New/Open/
      Reopen/Save As/Save/Close) appeared on right button down, now it appears when
      a right up occurs on the same filetab as the right down. Otherwise the right
      down filetab is shuffled next to the right up filetab. If the latter is not a
      neighbour, in which case the former is always moved exactly one slot along,
      then whether the right up occured in the left or right half of the filetab is 
      used to determine whether to land to the left or right of it. Note that right 
      up should actually be on a file tab rather than space next to it, otherwise 
      it will just be ignored. NB: There is no drag icon of any kind as yet.
      For example, with database.e, dll.e, file.e, and get.e open, in that order,
      right click down on get.e, move the mouse, and release (right click up) the 
      right button anywhere on file.e, then the tabs are reordered as:
             database | dll | get | file
      If you now press right down on file.e and release within database.e, filetabs 
      are reordered as either 
             database | file | dll | get
      or     file | database | dll | get
      depending on which half of the database.e filetab the right button was released.
      You can still manually reorder tabs using Alt PageUp|PageDown (or Ctrl Shift 
      instead of Alt) the same way as before.
15/04 After a bit of an epiphany (sat in the pub, where else?), added some low-level 
      code for proper wordwrap handling, see wwDEV in edita.exw. In all probability,
      much more work is needed, the main things being displaying TopLine starting 
      mid-way through, and cursor up/down within a wordwrapped line. Some deeply
      technical details follow:
        Wordwrap info is kept in the bookmark/fold table, using bit #04.
        Obviously, fold starts are never wordwrapped.
        Wrap info is currently calculated during display only, hence the vertical 
        scrollbar may be a tad off.
        Like folds, it is possible to both wordwrap and bookmark a line.
        adjustLine() etc needed mods to handle the new entries.
        If the wordwrap depth changes, it repaints the remainder of screen.
        When word wrap is on the horizontal scrollbar is disabled [done?]
        It should be clear there are no buttons/options anywhere at all yet.
        As above, cursoring and scrolling remain problematical.
      TEST NOTES: One thing I really want to be sure of is that this new code does
      not mess with existing use. It is also less than half complete, probably.
      Hence if you want a play, you must change the constant wwDEV=0 in Edita.exw,
      set debug = 1 in edita.ini, and even then the new code only triggers when a
      second or subsequent instance of Edita is run. If you do get it running, one
      thing you must try with it is the new Ctrl E/R - I have to admit that I am
      pretty smug about the way that all works together ;-)!
      Proper and complete wordwrap handling is likely some releases away yet.
22/04 Added some very basic pFTP integration. FTP/Run pFTP will find/start pFTP
      and initialise lines of communication. Right clicking on the Right (Remote) 
      panel within pFTP and selecting "Edit with Edita" will download a file and
      open it in Edita.
      HOWEVER: Saving in Edita does nothing special (like uploading) yet.
      The FTP/"Previous FTP edits" option is a stub (planned to be similar to
      File/Tabs List) and just does a "?98" for now. See eamenus/EditpFTP; it
      would need a flag within edita.edb, and for eg CD_OPENF to be used twixt
      pFTP & Edita rather than (but quite similar to) CD_OPEN as now.
      Also some design is required re unique file handling and downloading to
      appropriate sub-directories, so that when Edita is finished the filepath
      alone should be sufficient for pFTP to figure out where to re-upload.
23/04 Added a simple "Code Publisher" to the File menu. This simply creates a file,
      (backup/cp.htm) and displays it in the browser, eg for printing (yeah!). If 
      you select a block of text (startline!=endline) then only that block is 
      converted, otherwise it converts the entire file. Note that any previous
      content in backup/cp.htm is overwritten. See eamenus/publishCode().

Version 0.2.7
=============
26/11 Prevented crash if "_F1HELP" not defined in ealng_XX.txt (JL)
      Fixed not found problem in multiple line search.
      Removed TVS_SINGLEEXPAND default from treeviews
27/11 Rewrote doEvents() to prevent it stalling.
      doEvents(id) no longer calls the idle routine.
      new routine doAllEvents() does, and stalls if msg queue empty.
      This fixes the stall in find in files, runs much faster now.
28/11 Changed select word slightly. It no longer selects A-z over a
          full stop, eg chars 8..43 (pal..uk) of url above.
01/12 Added cleanup code to F1 help. If the database states that the
          word under the cursor is defined in a particular file, but when
          scanned it is not found, it now issues the warning message
          "global database lied", deletes the entry, and re-attempts the 
          lookup. This is a common occurence if routines are moved between 
          files, if "global" is deleted from a routine definition, or if
          a routine is renamed or completely deleted. Previously it gave 
          the same message, but as an error, and made no attempt to patch 
          anything up. 
13/12 Fixed a nasty bug in F1 help which would hang the editor. If you
          typed in "hello(" and pressed F1 it would hang.
15/12 Page break characters (#0C) are now shown as § (#A7). These tend
          to occur in C/Unix sources. (If you really want to, you can enter 
          one via Alt+012, though Euphoria rejects it as an invalid char.)
20/12 Added handling of WM_[QUERY]ENDSESSION messages.
21/12 Added high ascii support (eg ß) to ex.err processing, and split 
          very long text lines at "\n" where possible.
23/12 Added Re-fold last expansion. The last expand (Ctrl +) can be 
          re-folded by keying <Ctrl Shift -> This key has no effect if no 
          fold has been expanded this session, or if the last expansion has 
          already been re-folded. Both Fold All and Expand All (Alt -/+)
          disable the re-fold last feature, rather than making this key
          effectively always act on the last routine in the file.
          Added Fold, Expand, and re-fold to the context menu.
24/12 Add Display Line Numbers option.

Version 0.2.6
=============
If Find First failed to find any occurence of the target it would move the 
 cursor to 0,0 but not repaint the screen. It no longer moves the cursor.
The Braces section has been moved below Operators in the .syn files and is
 now limited to a maximum of 7 nested colours, previously was 9, which was
 overkill and didn't fit the Colours window which now shows these levels in 
 (.(.(.(.(.(.)))))) format rather than the less intuitive Brace1, Brace2... 
 Based on a suggestion by don cole.
Arwen:setTextColour() now also works for labels with SS_OWNERDRAW style. If
 you do not call setTextColour() on a label defined with SS_OWNERDRAW, the app
 will get a WM_DRAWITEM message; if that is not handled (the hard way: peeking
 the hdc and lpRect, and calling xDrawText), the label will not appear. Note 
 that SS_OWNERDRAW cannot be used with any other styles (eg SS_RIGHT). See
 eacolor.ew for the easy way (setTextColour), and eaopts.ew which must call
 xDrawText with DT_RIGHT rather than declare the labels with SS_RIGHT.
23/11 MOSTLY TAKEN OUT/REWRITTEN (caused 100% cpu usage):
--8/11: Added a new flag to improve idling when running a program from Edita.
-- (goIdle). Previously Edita would try to suspend background processing but in
-- reality be in a 'busy loop' checking for when it can resume, which showed up
-- in relative times for programs run with/without the roadworks sign red. As
-- before the triangle usually remains red if you run a program (in turn usually
-- causing an auto save), until the program being run terminates. Also, the icon
-- is longer left red when background progressing is permanently switched off.
Find in files now opens the result (ex.err) next to the current open file.
9/11:
 Disabled print/printpreview and report/window painter in debug mode (because 
 they [may] talk to the wrong copy of Edita). Edita running in debug mode will
 no longer identify itself as Edita when sent a WM_COPYDATA/CD_EDITA message.
 (The editor runs in debug mode only if edita.ini has been manually edited, 
  to change debug to 1, and further this only takes effect if startup detects 
  that a copy of edita is already running.) Also database viewer (erm) and
  verify edita.edb (needed) no longer appear on Tools menu in debug mode.
Added Display and Edit options windows, and moved some of the main entries off
 the Options menu itself onto those screens (see eaopts.ew).
10/11 Bugfix: Moving the mouse over the routine drop-down on the toolbar when 
 another window was open (eg find, help, options etc) would 'steal' focus from 
 that window.
14/11 Bugfix: Alt +/- when no files open would crash.
14/11 Added macro (record/playback keystrokes) handling. Updated most menu 
          entries so they can be properly recorded.
14/11 Started work on the manual. See this for details of macro handling.
15/11 ealng_en.kbh no longer distributed (replaced by _F1HELP in ealng_en.txt).
15/11 modifications to ex.err are no longer backed up.
15/11 Fixed screen corruption displaying text lines longer than 4096 bytes.
15/11 F1 help now responds to cursor/page up/down home/end, when focus is on
      the close button.
15/11 Type check cdir is -1 in eacons.ew after a program crash if ex.err did 
      not already exist (probably introduced post 0.2.5, ie never 'wild').
15/11 Fixed a problem in background scan whereby include files which also
          existed in filepaths[currfile] (rather than the actual directory of 
          the file being scanned in the background) would be added to the project 
          tree, and possibly vice-versa [ie some files could be omitted unless 
          the including file was scanned when the current open file was in the 
          same directory as the file being scanned in the background, iyswim].
          Verify edita.edb with full background scan is needed to correct this,
          though it is normally only visibly wrong in the ".." entry.
16/11 Bugfix: A crash would occur if the menus were hidden (Shift F11) and 
      either Ctrl Shift H or the toobar "paragraph" button was pressed.
          Additional edits were made to guard against non-existent menu entries,
          such as might occur if menu text were set to "" in ealng_XX.txt.
16/11 decimal points in numbers were coloured illegal.
16/11 Block select zero characters (eg by Shift RightArrow, Shift LeftArrow)
          paste, undo -> undo/redo "whoops".
16/11 Alt cursor, return now work on recovery tab.
          Recovery tab was loading current dir and current file backups twice.
18/11 Unspecified run-time exception in find window. EG open win32lib, press
          Ctrl A, Ctrl F -> fails trying to stuff over 1MB of data into a single
          edittext field. It now limits such defaulting to at most 600 bytes.
19/11 Deleting an entire routine left it in the internal routines list, and
          in the toolbar drop-down. Made background processing properly rebuild
          the routine list following a file save. Removed the "routine set wrong
          (not critical)" warning.
20/11 When eg on global function sort on line 6 of sort.e, the F1 help shows
          a link to the Euphoria Reference manual (previously the examine code 
          button would jump to the same file/line, which was not very clever)
21/11 Added 'number of days to retain automatic backups for' option. Files
          are scanned/purged once at the start of each editing session.
22/11 When editing html, and block deleting a "<!--", the preceding lines
          would appear as comments. This was because the bracket carry-forward
          array was not being updated properly. I suspect some cases of block
          delete Eu code miscolouring subsequent brackets have been cleared up
          as well (I have noticed this on occasion).
22/11 When visible, the recovery panel is now automatically refreshed when
          new backups are made. (Mostly done in background processing, but the
          TVM_DELETEITEM, TVI_ROOT message causes an inevitable pause.)
          It is also possible to reload the recovery panel by pressing Alt F5,
          and also to rename a file in the directory panel by pressing Alt F2.
22/11 Added Case conversion utility to Edit menu.
23/11 Added local variable and constant lookup to F1 Help. For example,
          if the cursor is on True, F1 might show "constant True=1" along
          with other lines in the current file that use it. Note that this 
          does not handle global variables (I'll get round to it one day),
          and on a line such as setVisible(XXX,True), it will show the help
          for setVisible even when the cursor is on True. The surrounding
          function is always searched for before looking for vars/constants.
23/11 Made database handler more robust against edita.edb in use/locked.
23/11 Help/Manual now opens edita/docs/manual.htm, or goes online.
24/11 If you compare a.exw with b.exw, not checking overwrite, so the
          comparison results are in a new [untitled] tab, then because both
          files had the same file extension, it applies the syntax colouring 
          to the contents of the [untitled] tab the same.
24/11 Fixed a bug with listview sort not updating the selected item focus.

Version 0.2.5
=============
04/10: fixed machine crash updating the project tree (added anti-rentrant
flag alreadyResettingProject to eadir.ew). Also fixed an index out of bounds 
in TVN_GETDISPINFO handling (presumably something in progress at the point 
the tree contents switched), and also removed DoEvents(0) calls from DBopen, 
since that was also a primary cause (and not needed since the introduction of
gMFstate handling in 0.2.2).
5/10: Added 'full background scan' phase to verify edita.edb. [DEV: more 
testing required]
6/10: Fixed a problem with ex.err parse: strings ending in ,10} were not being
converted (to ..\n") properly.
7/10: Added directory field to bottom of project tab. This is useful in the ".."
entry, for example if you have database.e included by two copies of eaedb.e,
it shows which is D:\edita and which is C:\temp\edita\test.
10/10: Added C.syn (incomplete). Minor modification to easynld.e to make Escapes
section properly optional. The constants, types, and clib sections ought to, in
theory, contain thousands of entries; should anyone want to help fill them, let
me know (or just email me an updated C.syn file). [See also the windows API list
in fasm.syn, I'm sure that can be copied] Actually, I've not really ever seen C 
code syntax coloured before (yes, I've led a sheltered life), so I may be making 
several basic errors. Likewise, if anyone wants a go at parsing C files to create
the equivalent list of routines, let me know.
16/10 added additional call to enableMenuToolBar in idle routine, significantly 
improving responsiveness when, for example, file a is modified (not a .e* file) 
and switching to file b (which is a .e* file); previously the run option would 
remain disabled until /all/ background processing was complete.
17/10 wiped the globals database in vedb.exw prior to the 'full background scan'
(as added 5/10, also in 0.2.5). Sledgehammer to crack a walnut perhaps, but it 
certainly fixes an odd anomaly or two lying in wait (previously caused the error
global database lied, not reported externally but an occasional occurence here).
18/10 Added Y/N etc key handling to error message routine (to match messageBox).
02/11 Replaced system_exec with system as suggested by RC in eabind.ew
03/11 Updated fasm.syn with MMX, SSE, etc sections and a (large) win32 api list.
06/11 Suprious comma in eacons.exw removed (thanks to Greg Haberek),
          Finally tracked down an intermittent problem with the file open dialog
          repeatedly reappearing. It used to happen to me about once a fortnight,
          but thanks to Greg I managed to hunt it down.
          Enhanced code folding to a single if/elsif branch, unless on the end if,
          or elsif/else is on the very next line.
          Ctrl _/- and +/= keys now also trigger fold/unfold (previously only the
          numpad keys worked).
          Allowed mouse clicks on the fold/bookmark margin to expand/collapse folds.
          Added new .ini option (which will appear in edita.ini when 0.2.5 is shut 
          down for the first time) which makes the fold margin permanent, so it can 
          always accept mouse clicks ("Permanent Fold Margin", default False).

Version 0.2.4
=============
Folding two blank lines would 'trim' from both ends to create a 
 negative fold, causing a fatal error.
A bookmark hidden inside a fold prevented subsequent folds and 
 bookmarks displaying properly.
Full screen mode was misbehaving if the file panel and/or message area were 
 visible, which might well have been the problem reported by DB James aka Quark.
Return from full screen mode restores window rather than always maximising it.
Moving to end of a long line did not redraw correctly (introduced in 0.2.3)
Removed WM_CONTEXTMENU handling; replaced with check for VK_CSM (aka VK_APPS)
 in virtualkey. Previously, the control key had to be held down until the menu 
 actually appeared, to get the tab context menu rather than the edit area menu.
Removed screen flicker introduced by use of ExtTextOut (in 0.2.3)
Recovery tab now selects the current file properly, and shows '+' only if the
 file has one or more backups. Opening a backup puts it next to the current 
 file (unless isSingleDir is in force). File compare now sets the target if a 
 file tab in the background is clicked. If you restore a file and right click 
 anywhere on the edit area, a restore prompt is now offered. I should point 
 out that the Recovery tab details are not kept up to date (on my to-do list), 
 which makes any attempt to test this very confusing. Please note that work 
 on the Recovery tab is still very much in progress (eg the 'Delete' button 
 is still inoperative).
Added 'focus memory' to arwen (for use when Alt-Tabbing). It now returns 
 focus to the previous field after Alt Tab, and forces focus to the parent 
 window when a child is closed (it used to vanish). (see PLaf in arwen.ew)
Full Screen mode no longer clobbers the Windows taskbar if it does not have
 the autohide option enabled.
Removing the last fold or bookmark now removes the margin.
Added new ClearOverStrike option to .ini file to auto-disable OVR mode, since
 I often hit Insert, when aiming for Home.. See eanotes.txt for more details.
Crash on Ctrl Page Up when tabbar not visible. Ctrl Page Down was inoperative.
Added """ (triplequote) handling to language files. The file ealng_XX.kbh is 
 (or soon will be!) no longer required.
Added Verify edita.edb option to the tools menu. Previously, problems with 
 this file could only be solved by deleting it, losing bookmark, fold, and
 backup details. The program (vedb.exw) can also be run stand-alone.
New ini file option "Automatic Backup" defaults to 1. If set to 0, it does
 not make backup copies of files (in edita/backup) before overwriting them.


Version 0.2.3
=============
Bugfixes: index out of bounds, subscript atom in saveFolds
Fold all crashed if the toolbar was not visible (Steve A)
Context help crashed if EUDIR not set (Al Getz)
Double click on fold/bookmark border crashed (Tony Steward)
100% cpu usage when all files closed (Greg Haberek)
Vertical scroll bar incorrect after Alt -
Gave the bookmark/fold margin a white background.
Underscore characters not cleared when folds active:
 changed use of TextOut to ExtTextOut with ETO_OPAQUE.


Version 0.2.2 08/09/2005
========================
Added block comment handling (to .syn files, for the benefit of eg html, c/c++ 
files, not really of any use with Euphoria source files, unless you are using a 
modified run-time).

Added Html.syn and bat.syn

Added bookmark handling:
  Ctrl F2 toggles a bookmark on the current line (a '*' appears in column 1; if
        needed, text is shuffled over one column to make way for it.)
  Ctrl Shift F2 removes all bookmarks
  F2 moves to the next bookmark, if any
  Shift F2 moves to the previous bookmark, if any
Added routine and block folding.
  If Ctrl - is pressed when a block of text is selected, or the cursor is on 
  one of: global, procedure, function, type, if, for, while, end, or a bracket 
  spanning more than one line, (namely the same conditions for Ctrl [ handling, 
  less elsif and else), then the appropriate block is folded and a '>' appears 
  in column 1.
  Ctrl + unfolds a block, as do find, goto line, F12 error handling, etc.
  Alt - folds all routines and Alt + removes all folds.
Blocking over a folded line and cutting/copying/deleting/pasting incorporates
all folded lines if the (usually invisible) end of line marker is selected.

Bookmarks and folds are saved between sessions (providing the file is not 
modified externally).

Fixed problem with background processing freezing the editor, particularly at
startup on a slow machine.

Run program now calls WaitForInputIdle once, with a maximum timeout of 750ms (0.75s).
This should allow ample time for the program to load and start, before background
processing kicks in (checking for modified files etc.) Previously, the timeout was
effectively infinite, which would cause problems for any program which did not open
either a gui or a console window early doors, and cause Edita to remain almost
(but not quite) completely unresponsive until the invoked program terminated.

Toggling the scroll bar, and filetab visibility, with the File Panel visible did 
not repaint correctly.

Alt Insert and Alt Delete (comment and uncomment) were inoperative when the File
Panel was visible.

Fixed crashes when EUINC not set and Find in Files when a null selection in force.

Fixed one cause of the 'routineset wrong (not critical)' error. However, I have 
experienced a couple of other cases this occurs in (therefore it is not completely
fixed). [Ctrl Q always rebuilds the routineset from scratch; it checks, warns, and
replaces the incorrect set. The routine drop-down, Ctrl [, and now routine folding
may be out slightly when routineset is incorrect; there are very few cases where it 
could cause any real harm.]

Enhanced uncomment (Ctrl <, or Alt Delete) to remove '<' and '>' when processing 
one or more lines from the result of a file compare (based on whether the first line
begins with either '<' or '>').

Added a crash button to the 'No translation for' warning when running in debug mode.

Reorganised edit menu.
Added Cut prepend (Alt Shift X) and Copy prepend (Alt Shift C) [to the main menu only,
not the context menu]. Removed Cut append and Copy append from the toolbar.

Swapped the direction on Ctrl [ (now back) and Ctrl ] (now forward) to be more logical.

Plus version now ships with public domain 2.5 files. The bound files in both the Full 
and Plus versions are still created with 2.4, however.

Edita now logs automatic backups; a new Recovery tab has been added to the File Panel
to allow such a backup to be opened (basic features only working).

Fixed overflow (type_check failure, paint is 1073741824).


Version 0.2.1 19/08/2005
========================
BUGFIX: prevRtnIdx has not been assigned a value (reported by Bob Elia).
BUGFIX: Common Code analysis entered infinite loop (reported by Bob Elia).
                Also made it jump to the line in error.
BUGFIX: Options menu checks cleared by fullscreen mode (reported by Juergen Luethje).
Added Capture Console Output (use exwc.exe, permanent) to the Parameterised Run (Ctrl F5)
window. Console capture seems to be possible only via exwc.exe, hence a console window
will always (briefly) appear when this option is checked. The setting is saved against
the current file path and name.
Translation assistant now puts the current version in the output. (as suggested by Juergen Luethje).
Fixed a minor problem with saving global includesets on modified files.
Removed doEvents(0) calls from DBopen as they could cause problems.
Allowed Ctrl+Alt keyboard entry (reported by Juergen Luethje). 


Version 0.2.0 16/08/2005
========================
Cleaned up the code in eaclip.ew.

Fixed a display problem with non-permanent vertical scrollbars.

Switching file tabs after amending syntax colours reset them. (This bug
was introduced in 0.1.9)

Made Alt keys affect the project and directory treeviews when visible.
(To perform column selection when the file panel is visible you must use Alt 
Shift and the mouse.)

Alt Home and Alt End left the old current line highlit.

Added Ignore Whitespace option to file compare.

Added a ComboDropDownList of routines to the toolbar. The current routine is
automatically updated when the cursor moves. Thanks to Bob Elia for the idea.

Added <Ctrl [> handling (based on code/ideas from & thanks to Bob Elia).
=======================
If the cursor is on or immediately to the right of a bracket then the cursor 
moves to the corresponding open/close bracket, if one can be found. (If the 
cursor is between two bracket characters, the match for the one on the right
is found.) Otherwise, the cursor moves to the open/closing/next control flow 
statement corresponding to the one under the cursor, if there is one. Control 
flow statements are: [global|end] procedure, function, type, [end] if, for, 
while, and also elsif and else.

In the case of if/elsif/else/endif, <Ctrl [> (and <Ctrl Shift ]>) move forwards
to the end if, and then back to the if, <Ctrl ]> (and <Ctrl Shift [>) move back.
In other cases, <Ctrl [> and <Ctrl ]> effectively behave the same.

If the Shift key is down, then block selection occurs. This makes it easy to 
block an entire routine or control structure: place the cursor at the start of
line where the routine or control structure starts and press Ctrl Shift [. It
may be necessary to press Shift Home or Shift End to include leading spaces or 
trailing comments, before cutting or copying the text.

Note that when matching brackets, the trailing bracket is consistently not part
of any blocked text, whereas the opening bracket may or may not be. Additional
keystrokes such as Shift left- or right-arrow, or even Escape (to cancel the 
selection), Ctrl Shift [ (to re-block starting from the other end) and then
Shift left/right, may be required. All text between two brackets can be selected 
by starting with the cursor just to the right of the opening bracket, unless 
there is a bracket immediately after it (in which case the text is blocked to
the closing bracket for the one on the right). Pressing Ctrl Shift [ usually 
makes it much easier to see the text between matching brackets, even if you have
no intention of copying or cutting the text.

Lastly, note that this block matching assumes the source compiles cleanly; it
will happily match up an end for statement with a while or if statement if an 
end while or end if is missing, however it will issue a warning if the "jumpto"
and the initial position are a mismatch. It should be possible to use this
feature on incomplete code, hence if there is a bracket or control structure
mismatch before the [valid] code actually being edited, it should not interfere.

If no suitable matching start/end can be found (as will happen, for instance, 
if the cursor is on "do"), then nothing happens.


Undo no longer block selects re-inserted text over more than one line.
(Doing so caused the following behaviour: Key End, Return, Backspace, Undo, X.
 Just before keying the X, the cursor appears to be at the start of the line,
 but you cannot see the blocked CR (unless Show Formatting is checked), and it
 certainly feels wrong when it overstrikes the CR and puts the X at the end of 
 the previous line.)

BUGFIX: Directory list would select test.e when it existed instead of test.exw.

Added keyword translation handling
==================================
If ealng_XX.txt contains "and" = "...", then eaxlate.e starts loading keywords 
up to "abort", and then loads builtins. See ealng_en.txt for a (commented out) 
example.

The source files and the clipboard are always in English; translation occurs
whenever a file is loaded or saved, or text is copied to or pasted from the
clipboard. This means the standard English version(s) of ex.exe and exw.exe
can be used, and you can post English code to EUforum, or get English code 
from there and paste it into Edita for instant translation.

This means that with a suitable ealng_it.txt (and if the locale setting on 
Control Panel, Regional settings is Italian) one could have:
<eucode>
globale funzione get_routines(numero i)
    se sequenza(routines[i]) allora
        ritorna routines[i]
    fine se
    ritorna {}
fine funzione
</eucode>
and then run it on a standard, unmodified exw.exe. There is no logical reason
why routine and even variable names could not be translated, except that there
is a danger of ending up with a huge translation file.


Note that translations for standard functions (and also things like win32lib) 
have not yet been tested, but there is no reason to suspect that they will not 
work. Also note that (eg) Euphoria.syn should be language-neutral; the indent
and autocomplete sections should remain in English but they can be translated 
via ealng_XX.txt, for example ealng_it.txt might contain the line:
"^ i&f | then^pend if" = "^ s&e | allora^pfine se"; whereas the Euphoria.syn
line "Indents + then do else - end" relies on individual keyword translations.
<aside>
  The problem with attempting to translate "^ i&f | then^pend if" by looking
  up the individual keywords is that the trigger character (&f) position is 
  language-dependent. Eg "integer" vs "include", in English, suggests the 3rd
  character must be used as the trigger, and it is not difficult to imagine a
  case where a 2nd character trigger which works fine in English might need 
  to be the 3rd in another language.
</aside>

In honour of this momentous occasion (and frankly the risk of damage), I have
re-activated the automatic backups facility. In case of emergency, scan the
backup directory: the first character is year, 0-9, the second is month,
0-9&A-C, the next two are the day (01..31), then the hour (00..23) and the
minute (00..59), with a ".bak" extension. I am not yet logging these properly
(top of my list for next release). It is still a good idea to make a backup
of your own before unleashing this new feature on any important files.
Some time ago (last year or even earlier) I experimented with something 
similar and once mananged to "translate" a vital bespoke copy of win32lib, 
changing every instance of "then" to "than" in both code and comments...
After disabling the feature, it took me at least 90 minutes to recover the 
file by hand (and I was probably lucky that I still could). So I will repeat, 
make sure you have backups before using this feature, or be prepared to pick
through the mess in \backup. Let me know if all goes well, or not.

In a more recent test, I translated the builtin length() function to "len",
which clashed with a number of variables I had declared with the same name,
causing compilation errors. This time it only took a few moments to replace
all use of the variable(s) len with "lng" (and leave calls to len() alone!).
However you should be aware that such collisions are possible, although
quite unlikely. A program which works fine in English with English keywords
is simply not guaranteed to continue to work once translated; but hopefully
over 99% will. Thankfully, and on the plus side, it is a fair bet that all 
such problems will be immediately caught as compliation errors, rather than 
introducing (subtle) bugs at run-time.

I have tested this on edita.exw, and made the required changes to control flow
(Ctrl [), autocomplete, autoindent, and quick jump.
If F1 is keyed on "numero" above, then help from ealng_XX.exh is displayed; 
Please note that ealng_XX.exh should //actually// contain "integer",
though editing it within Edita may show "numero" if "integer" is translated
to "numero" (after "and" in ealng_XX.txt) and Euphoria.syn has .exh as a 
recognised extension (which it does in the standard release). All the comments
in ealng_XX.exh still require manual translation.

Lastly, after editing ealng_XX.txt, you may experience errors 
"global database lied". For now, please delete edita.edb.

Added new .ini file option for Don Cole, "Single Dir Tabbar". If this is set
to 1, the tabbar only displays files from a single directory. The other files
remain open, but are hidden. Opening a file in a different directory causes
the tabbar to be rebuilt.

BUGFIX: Ctrl N (New file) could hang as background processing attempted to 
process "[untitled]" (introduced 0.1.9)

Added new .ini option: "Move Cursor On Scroll". If this is zero (the default)
then using the right-hand vertical scroll bar will change the view but leave 
the cursor off-screen, so (eg) keying left/right arrow will jump the display
back. If this is one, the cursor will remain on-screen, on the same relative
line to the top of the screen, however any blocked text will be unblocked,
unless the shift key is held down while moving the scrollbar.

Found a bunch of missing translations in Quick Jump.

BUGFIX: Ctrl Z (Undo) could leave two lines highlit.

Enhancement: F1 help now scans the project tree, so for example "create" in
edita.exw will show help from arwen.ew rather than win32lib.ew because it 
includes the former not the latter. This also means that if you have several
projects which use several different versions of win32lib, it will show the
correct version details.

F1 help on builtins now offers to open the Euphoria reference manual (instead of
lang/ealng_XX.exh which was not much use to anyone).

BUGFIX: the tooltip for the backgroung processing togglebutton was always set
to (on) during initialisation, even when the option was off in the .ini file.

BUGFIX: Ctrl D was doubly-updating routines[currfile], which lead to a pop-up
error "routineset wrong (not critical)".

Added a retry loop to the db_open call.

The "Replace Tabs with spaces" .ini option now works. Note that this is on by
default, and on a slow machine, while as much of the load overhead as possible
has been moved to background processing, it may significantly increase save
time, for example on a 7-year old 233MHz PII with 48MB saving win32lib leaps 
from 4 seconds to 8 seconds. (Even then, four seconds is not an excessive
extra overhead for a 35,000 line file, and of course it will be much less on
newer machines.)

Added ".." entry to the project view if a subcomponent is included by several
other files. For example, file.e might show entries for edita.exw, easynld.e,
eadir.ew, eaxlate.e, database.e, and eafif.e. In the ".." tree, only the top-
level entries are shown; if one is selected the file will be opened and the 
full project tree for that file shown. Note that for some libraries (eg win32lib)
this tree might become /VERY/ large, so some caution might be in order before
opening it.

Added fullscreen mode. This is toggled by F11. In fullscreen mode, the current
filetab occupies the entire screen area; the titlebar, menus, toolbar, tabbar,
file panel, scrollbars, message area, and status bar are all removed. If there 
is anything you want which is not immediately available as a keyboard shortcut,
press F11 to turn off fullscreen mode first. I expect most people will have a 
pet feature they think should remain in full screen mode; if you (or I) were 
to include them all then fullscreen would end up identical to normal mode :-) .

Escape also turns fullscreen mode off.

The display of the menu bar can be toggled by pressing Shift F11. 
There is no equivalent menu entry or .ini file setting.


Version 0.1.9 01/08/2005
========================

Made Auto Complete multi-syntax, so you can now have different autocompletions
in a .exw file to a .asm file, for example. See eauto.e for more details.

Moved a freshly opened ex.err file tab next to the current open file.

Rewrote string colouring code. The following test set should illustrate the new
colouring scheme (copy and paste into a .e[x,xw] file):
--'
--'m
--'m'
--'mo
--'mo'
--'\
--'\m
--'\m'
--'\mo
--'\n
--'\n'
--'\no
--"
--"m
--"m"
--"mo
--"mo"
--"\
--"\m
--"\m"
--"\mo
--"\n
--"\n"
--"\no
--"\no"

Added single instance handling to eama.ew so that test (windows) applications
can shutdown any existing instance of that program. Here is a complete example:

<eucode>
 global function closeExisting()
        return "Press any key" -- send WM_CLOSE to any window starting thus.
 end function
 include eama.ew        -- uses routine_id("closeExisting") to see if "" defined.

 include arwen.ew

 constant Main=create(Window,"Press any key to start Test",0,0,100,100,400,400,0)

 WinMain(Main,SW_NORMAL)
</eucode>

One area I have found this to be particularly useful is when running applications
which use database.e, either before I have added locking, or when that locking
stops a second instance of the application dead in its tracks. Edita has also
been modified so that it only performs SetForegroundWindow, (when it detects that 
a program it launched has terminated), if there is only one program still running 
which was originally launched from Edita.


Keywords "then", "do", "else" and "end" in comments and strings were incorrectly
affecting the indent of subsequent lines. Added a new Indents section to the 
syntax files to make such processing as language independant as possible. It also
works for [some] single characters, eg { and } (see Euphoria.syn and FASM.syn).

New version of ealng_nl.txt (Dutch translation file) provided by Tommy Carlier.

BUG: (introduced in 0.1.8) overstrike mode was duplicating the addAction call, 
which made undo/redo get things wrong and generally throw a hissy fit.

Swapped font and colour buttons on the Tabbar and added a help button. Also made
comment/uncomment use the comment specified in the .syn file.

BUG: 0.1.8 did not handle edita.edb created under 0.1.7. (Fixed, but see below)

Find in files, file and directory compare, and translation assistant no longer
use the clipboard.

BUG: Using the scrollbars extended the selection. This now only occurs if the 
shift key is held down (see --DEV make this an option: in handleScrollBars).

The file and directory names in edita.edb were being held without case conversion,
which caused a problem if the real file was Arwen.e but a program included arwen.e
To avoid this problem, file names are now stored in lower case and directory names
in upper case. This may mean that existing edita.edb files cause problems, which
are easiest to solve at this stage by deleting any old edita.edb file before 
installing 0.1.9.

SelectWord was blocking all of "a..b" (less the quotes).

Added background working icon to the toolbar. When the triangle is red, background
tasks are outstanding, once they are complete it turns grey. It is also possible
to turn off background processing by toggling the button (or the options menu
entry), in which case the triangle will (soon) turn red and stay that way.
--The icon can be removed from the Toolbar by setting the translation for "Background
--Processing (on)" to "" in ealng_xx.txt. Note that if the Options menu entry is
removed (by setting the translation for "Background Processing" to "") then 
neither the Options menu entry nor the icon will appear, and whether or not any
background processing occurs will be determined by the value in the .ini file.

Background processing scans files for globals and include statements. Only files
with extensions specified in Euphoria.syn are processed. If the main project file
is open (eg edita.exw), Edita will find and process all include files automatically.


Added the Project Tree.
When files are saved, or first opened, they are added to the background task list.
The project tree shows the include statements detected in the source file(s).

If the file is only included by one other file then the project view will also
shown details of that parent file.

If more than one include statement has been detected for the file, only the 
subproject is shown, unless the file is already part of the project currently 
being shown.

For example if the current open file is arwen, a project tree for arwen is shown.
If eacolour.ew is opened (providing that edita.exw has previously been scanned), 
the edita project tree is shown. At this point if arwen is re-selected, the Edita 
project will remain, because arwen is recognised as a subproject of it. However 
if some other file is opened (eg readme.txt) the project tree will be replaced 
since there is no include statement for that file in the edita project tree, and 
then switching back to arwen.ew will show just the arwen project tree.

FAQ: When I show the project tree for edita.exw, it lists eaclip.ew and eafind.ew
in the project; if I switch to either of those the project tree remains. However,
if I switch to say ex.err (the project tree is replaced by a single ex.err line),
then switch back to eaclip.ew, I get a single line for eaclip.ew, whereas if, from
ex.err, I switch to eafind.ew, I get the full edita.exw tree as expected. Why does
switching to eaclip.ew not show the full edita.exw tree?
A: The short answer is that the full project tree is only guaranteed to be shown
when the main project file is open. The longer answer is:
If eaclip is included in a second project (eg test.exw), then it assumes that
eaclip.ew is a subproject/cannot (automatically) know that eaclip.ew is really
part of edita.exw and the include in test.exw is just a test. There is, as yet,
no code to "clean up" the project details if test.exw is deleted. For a short-
term fix, delete test.exw, close edita, delete edita.edb and re-start edita.
The project details will be automatically reloaded (which should take less than
20 seconds even on a very slow machine), and it will not log eaclip.ew as 
belonging to two projects.
[Update 4/10/2005: Tools/Verify edita.edb now cleans up project details]

The Projects menu has been removed, since project details are now accumulated 
automatically. Euphoria source files may also contain special comments, eg:
--#include lang\ealng_en.txt
which adds the specified file to the project tree but does not, of course,
affect the normal running of the program. Such comments must begin in column 1.
If the --#include line specifies a file ending in one of the file extensions
specified in Euphoria.syn, and that file exists and contains further --#include
lines, a subtree is created.

After changes to the include (and --#include) statements, and the file is saved,
the project tree is automatically updated.


The builtin context help file, lang/ealng_xx.exh, is now automatically processed
(by background processing, if it has been altered).

BUG: The copyTextToClipboard function was using the internal 100K label buffer,
so if you copied the whole of say win32lib.ew (at around 1MB), it would cause
severe memory corruption. Since these changes caused a few problems, the old 
style code is still available in eaclip.ew, for emergencies, but I plan to 
remove it next release.

Ctrl W (select word) was including tabs, spaces, and paragraph marks when the
"Show Formatting (Ctrl Shift H)" option was checked.

Added translation sections for keywords and builtins to ealng_XX.txt. At this
point, these only apply to a file named "test2.exw", and will depend on saving
files with tabs replaced by spaces, which also only applies to test2.exw as 
things stand. Further work is required for clipboard handling, plus I want to 
reinstate and enhance the automatic backup facility (which I disabled in 0.1.7) 
--before unleashing this on my files, let alone everyone else's.


Version 0.1.8 19/07/2005
========================

Fixed a glaring hole in the forward and backward scan of groups in common 
code analysis, leading to many matching lines not being reported. This would
happen if the backward scan thought the line was part of a previous group, 
--when in fact the forward scan hadn't picked it up (or maybe vice versa!).

--A line such as 'if isReally("function",line,idx) then' was appearing in the 
quickjump list as a real function (named "line"). Strings are now correctly 
skipped. Also html section markers (<!---->) were not being recognised.

Ctrl M now toggles the message area, and Ctrl L toggles the File Panel.

Integrated tedb (tailorable database browser) with edita.

Added record deletion ability to tedb. Note that if you modify tedb.edb, for
example to remove details of a database no longer on your hard drive, then
the next time tedb.exw is run, a warning about possible corruption is
displayed; this is expected and automatically corrected. Also added doLower
to avoid mis-sort of ints, eg 69,70,101,102.

Shift Page Up/Down now block to the first line, column 1/last line, last column.
Previously, when the cursor was on the first/last screen, it would not move.

Fixed a minor problem in ppExf (it was returning a trailing '\n', which it
is not supposed to).

Prevented the editor from opening both dir\xx.e and dir\subdir\..\xx.e.
(This condition arose from an include ..\eaxlate.e statement, which 
 in turn left D:\edita\pp\..\exlate.e:1165 in the ex.err file)

Stored directory and file details in edita.edb, for context help.
The file edita.edb preserves useful information permanently, but if any problems
arise it can be deleted and will be recreated automatically.

Invoking Quick Jump or pressing F1 now saves the global routines defined by the
current file in edita.edb.

--F1 help added. Note that unlike MEditor, which used an explicit "Parse for 
--globals" utility, the information required is gradually learnt, as noted above.
Also, Edita retains global information from more than one directory, which
MEditor could not. It may be possible to write a directory/project/current 
file scanner for edita, if demand is sufficiently high.

 (Disclaimer: the global information is not yet "purged", for example if you
  remove the global keyword, there is as yet no way to make edita remove the
  corresponding global information (short of deleting edita.edb), it will 
  still think that global is defined in that file. [WARNING: manual deletion 
  of individual records in edita.edb will foul up the uniq record numbering])

BUG: finding "\nXXX" (ie blocking the end of a line and part of the next) would crash.

Moved translation files into a new lang subdirectory. As well as the existing
ealng_XX.txt files there is also ealng_XX.kbd containing basic keyboard help, and
ealng_XX.exh containing builtin descriptions (although the latter is not yet loaded
automatically; you have to open it and press F1, which as above, stores the needed
info in edita.edb). The file extension exh has been added to Euphoria.syn.

Shift F5 (repeat previous Run) now runs from the same directory as before. Fixed a 
crash when None.syn could not be found (comment unassigned), and stopped it showing
garbage on/enabling the Toolbar when \bitmaps\eatools.bmp could not be loaded.

Rewrote the bracket colouring code. Added new "bracket carry-forward" handling to 
cope with code such as:

void = messageBox(sprintf("Error in line %d, column %d",{tokline,
                                                         tokcol}),
                                  sprintf("%s\n%s^ %s.\n",{t1,
                                                           repeat(' ',tokstart-1),
                                                           msg}),
                                  MB_ICONEXCLAMATION + MB_OKCANCEL)

This information is held in a sparse array, for example win32lib at 34,723 lines
creates a bracket carry-forward array of 602 entries. The downside is that this
adds a small additional delay on load (probably less than 0.2 seconds for win32lib 
on a 2GHz box, though).

Mismatching brackets are now coloured illegal, eg ")" in {{)}.

Fixed a bug with ComboDropDown controls (eg Find, Parameterised Run) whereby the 
drop-down icon was not responding to mouse clicks. This was caused by intercepting
the WM_COMMAND message for the subclassed edit control in SubProc.

Shift F4 now finds the previous difference.

--I'm vaguely aware that a few bugs have surfaced in file & directory compare, so 
--use those with caution in this release.

Added AutoComplete handling. There is now a new AutoComplete section in the syntax files.
The Euphoria.syn file supplied has entries for if, for, while, global, integer, atom,
object, sequence, function, procedure, and type. See eauto.e for details of the syntax used.
It is theoretically possible to define code templates by editing the .syn file.

AutoComplete can be disabled by unchecking the entry on the options menu or by using the 
togglebutton on the toolbar. Any text added by this feature can be removed using the normal
undo function (Ctrl Z). AutoComplete does not occur if any text is blocked, if the editor
is in OVRstrike mode, and/or in most cases if the cursor is not at the very end of the 
line (including invisible spaces and tabs).



Version 0.1.7 05/07/2005
========================
Reworked WM_COPYDATA to fetch filenames etc one character at a time, so this should
now work on windows XP (specifically print preview and window painter).

Added limited ListView and TreeView support to Window Painter (just enough to stop 
it crashing and overwriting stuff, full support will have to wait).

Disabled the automatic backup feature (see constant makeBackup=0 in edita.exw),
since it was not really achieving anything apart from wasting disk space.

Prevented the automatic saving and backup of [untitled] tabs.

Added the "old" style quick-jump pop-up. A new ini file option 
"New Style Ctrl Q" has been added. When this is set to 1, the quick jump table 
is "docked" to the main screen. There are a number of problems with this 
(mainly focus), which is why the old style quick jump code was resurrected.

Added directory tree to the File Panel. When first displayed, the focus is set
to the current file (if any). Files may be opened by double clicking on them,
but there is no way this can be done from the keyboard (yet).

Ex.err files are auto-preprocessed to make strings readable. There is a new
option, Reformat ex.err, on the options menu. When this is checked, and an 
ex.err file is loaded, it is preprocessed to make strings more readable. The 
ex.err file itself is not modified, so the full details can be recalled by 
unchecking the menu option and selecting ReOpen from the tab context menu. 
As an example of a case when you might need to do this, consider the Arwen
internal variables ObjectLabel and ObjectChildren. Whereas ObjectLabel is
indeed text, the latter is likely to display as eg "HIJKLM" when in fact
{72,73,74,75,76,77} would make more sense, because ObjectChldren contains
lists of (sequentially-allocated) control-ids. It is not practical to make 
the preprocessor understand that it should process one and not the other.
Note that if the ex.err file is deliberately modified, and (possibly 
automatically) saved, (eg on tab switch) then this reload technique will 
no longer be possible.

Made the automatic opening of ex.err check that dir(crashpath&"ex.err") has
altered in some way since the program was invoked. This prevents, for example,
a Ctrl C manual interruption of a program from opening an old, irrelevant file,
and similarly if the program has changed the current directory before crashing,
edita will no longer misleadingly open the wrong ex.err file.

Math function overflow in eascan, getFloat(), processing eg 1.0E-300
        I had coded exp = exp*10 * Ch-'0' instead of exp = exp*10 + Ch-'0', and 
        exp (which will tend to end up very large and negative, eg -11082288) is 
        later used in the expression tokatm = tokatm * power(10,exp*esign).
        In short, it will likely crash on almost any (real) number.

Fixed subscript crash loading files containing binary zeroes. Also made the
default text colour "Other" in easynclr.e, should syntax colouring be aborted
because a binary zero is detected.

BUG: Ctrl D (duplicate line) was not being added to the undo/redo list.

Further tweaks to undo/redo to properly block the re-inserted/undeleted text.
Made Escape cancel the current selection. The selection is now also cancelled 
when toggling from overstrike to insert mode.

Added Options/Backups; the current file can now be automatically saved at a 
user-defined time interval. Set this to 0 to disable automatic backups.

If edita.ini is amended by hand, then it is no longer automatically re-written 
(thus losing the manual updates) at shutdown.

Fixed the focus problem with translation assistant. The same methodology 
(using addFocus/removeFocus) has been applied in about 10 other places.

Internal change: the main debug flag has been moved to the ini file to remove
the risk of a release being made with it left on. If the single instance check
detects that another copy of edita is already running, and the debug flag is 
set, then it continues in debug mode (not opening pervious session and not 
saving the .ini file on exit). Hence double clicking on a file in Explorer 
will open a blank editor when the new flag is set.

Added run program to the toolbar

File compare would display the progress window over any error message.

Added ListView handling to setIndex in arwen.

"Symbols" replaced with "Operators" throughout.

Saving a syntax file (.syn) now reloads (all) the syntax details. Created
FASM.syn during testing of this feature.

F3 and F4 were not repainting the current line when no text is selected.

include subdir\file.e coloured the \ and the . red.

You could not set the current line colour on "None" (eg .txt) files.


Version 0.1.6
=============

Fixed around two dozen misc crashes when no open files (eg trying to save when
there is no open file gave a subscript 0 error).

Made single instance restore a minimised window.

Find and find in files now open with the search text selected.

Found and fixed a problem whereby resizing the file panel left the edit area blank.

Alt '<' was not unindenting properly (and crashing on .txt files).

Ctrl W did not work over accented chars (eg Exécuter)

Undo/redo now block select re-inserted text.

Added Tools/Translation Assistant.

Enhanced bind/shroud to cope with 2.5 (only tested by creating a dummy "bin/bind.il")


Version 0.1.5
=============

Enabled/disabled menu entries and toolbuttons

Added bind/shroud processing. The first line of the source being processed is
examined, unless it is exactly "--Parse:Skip" when the second line is used.
If it begins with "--" and is followed by either "Bind:" or "Shroud:", then 
this becomes the default action for that file. It may be followed by "-list"
and/or "-clear", and/or "-icon" followed by a filename which must end in ".ico".
The remainder of the line, if any, is taken to be the output file. Eg:
--Bind: -list -clear -icon 95.ico edita.exe
Note that unlike quoted strings, paths should contain single backslashes.
Also note that icons are only applied to files ending with ".exw", and, of
course, only when they are being bound as opposed to shrouded.

Extended eama.ew to allow integration of standalone utilites with Edita. 
Initially, eabind.ew was a standalone component, but since it added 128K doing
it that way instead of 6, I ditched the idea. However the source may be useful
(available on request) as a reasonable example of how to integrate a new, and
otherwise standalone utility into edita. A small mod is required to eamenus.ew
(see function ToolMenus()) which would ideally be based on information kept in
edita.ini.

Added toggle specials button to the toolbar.

Unary minus and not now correctly rejected by Window Painter.

Added Run, Parameterised Run, and Jump to Error to the tools menu.
Shift F5 runs the last command again, even if editing a different file. 
Ctrl F5 (Parameterised Run) opens a dialog window with the command line which
would normally be executed as the default. Ctrl Shift F5 opens the same dialog 
window with the last command line as the default. In both cases a list of all
previously run commands this session can be opened and selected from. It is 
also possible to enter any command required into this dialog box.

Added Auto Save On Tab Switch to edita.ini (default False). There is as yet no
way to change this setting other than by editing edita.ini.

Added file and directory compare to the Tools menu.

BUG: undo of a multi-line deletion was re-inserting the text at Min(selX,CursorX)
which is actually only valid if CursorY=selY, also if CursorY>selY then it should
always use selX and if CursorY<selY then it should always use CursorX. (A selection
is from selX,selY to CursorX,CursorY, but may be made either forwards or backwards.)

Made edit actions dynamically update the quickjump table, and removed the line
numbers.

Added Find in Files to the Tools menu.
Added Macro to the tools menu.

BUG: *.EXW (as opposed to *.exw) files were incorrectly syntax coloured.


Version 0.1.4 1/5/05
====================

Added toolbar, fixed mouse wheel direction

Version 0.1.2 20/4/2005
=======================

Bugfixes and enhancements to Window Painter:
        When a file contains more than one window definition, it now defaults to 
        the last window amended this session.
        Reduced unnecessary replace on sequences (styles).
        Made all copydata memory 4-byte aligned, which will hopefully fix a few
        sporadic machine crashes.
        Several integers converted to atoms as suggested by Pete E.
        Made Window Painter single instance, and switch back to the required file
        (re-opening it if necessary) on exit.
Added shellExecute for .html and .bat files.
Fixed several minor problems in the colour dialogue:
        CR and escape handling added,
        Accelerator keys were not working,
        Focus jump when mouse cursor on edit main window.
        Custom colour button had ceased working.
Bugfixes and enhancements to OVRstrike mode:
        index out of bounds when entering text/tab at end of line.
        Enhanced undo/redo to better combine input, eg on "fred", type "123" to
         get "123d". Ctrl Z gives "d", Ctrl Z gives "fred". Previously, Ctrl Z 
         would need to be keyed no less than six times.
        Selecting "fre" of "fred" in overstrike mode and pressing 1 would replace
         _both_ the "fre" and the "d". It now only replaces the "fre".
Added basic wordwrap facility, with toggle on Options menu. At the moment the
        wrap column can only be changed by editing edita.ini.
BUGFIX: block comment/indent was removing the selection. This bug appears to
        have been introduced in 0.0.9.
Added autoindent feature.
Added F12 ex.err file handling.
Started routine list. The dialog is somewhat experimental. The Quick Jump Focus
        button, in particular, should probably be ignored (it is intended as a way
        to select a routine via the keyboard, but it is nowhere near finished).
        The scan times are somewhat slower than MEditor, however a) they are run 
        in the background and cached, and b) the structure is designed to, one day, 
        cope with routine and block folding.
Double-buffered display changes. There are now three available modes:
        1) The original method, displaying direct to screen, is fast but with 
                significant flicker. For this setting, set oldStyle on line 14 of 
                edita.exw to 1. (useDIB, on line 15 is ignored when this is 1.)
        2) A normal off-screen bitmap (oldStyle should be 0 and useDIB 0).
--        3) Using arwen's DIB sections (oldStyle should be 0 and useDIB 1).
                There are a few problems with the palette in method 3, particularly on
                initial load and/or if you switch between a Euphoria source and a plain 
                text file, and back. The colours seem to be reinstated if you Alt-tab 
                to another app and back. It might be something in, or like, reBrush 
                in easynld.e, but I have not yet tracked it down.
        In summary, I want to keep 1, at least as an option, and make a choice 
        between 2 and 3 based on user feedback.
        Methods 2 & 3 may be amenable to significant enhancement, whereas 1 will
        never be. Feedback and suggested improvements welcome. Almost all the 
        relevant code is contained IdleHandler() in edita.exw and should be 
        instantly recognisable because of the tests on the flags mentioned.
        There is possibly some code missing from applyFont() in eafonts.ew.


Version 0.1.1 16/4/2005
=======================

Missing files: dib256.ew, imagelists.ew, ppp.e (AG)
Bound version failed with command_line[1] not exw.exe. (JE,JL)
Bound Window Painter failed with Directory Error. (JE)
MZ not declared [eacons was running "exw eawpaint.exe"] (JE)
Colour Options window off-screen on 800x600 (JE)
French translation added (thanks to Jean-Marc)

Goto command added.


Version 0.1.0 15/4/2005
=======================

Fixed an undo/redo problem with Cut/Paste:
If you had
-- The (blah...)
-- /li /clear( object )
-- /li /copy( object )
-- /li /cut( object )
-- /li /paste( object )
-- /li /undo( object )
And copied "he", then column blocked the 5 "li" and pasted, it worked fine,
however, undo put everything back in the wrong place (and redo crashed as 
it spotted the error).
Also, select 1234, then cut, undo and it was replaced four characters out.

Fixed bug: undo/redo did not always clear current selection.

Added find function, between 5 and 10 times faster than MEditor. Line breaks
can be specified using ^p, tabs using ^t, and ^^ for a single ^. An option to
ignore whitespace has been added. Searches including a ^p are exceptionally 
fast, for example searching for "then" must check a line of 75 characters in
at least 71 positions, whereas eg searching for "then^p" needs only check the
end of each line.

Replace and Replace all functions added.

Added Print and Print Preview to the File menu. (standalone optional component)
Added Window Painter to the Tool menu. (standalone optional component)

Added current line highlight. To disable, set the new Current Line entry on the
Options/Colours screen to match the background.

--New standard include, eama.ew, allows any program to send output to Edita's
message area. Just call eamsg(data). If data is not string-like then a string
representation is automatically created using sprint(). Within strings, any 
Linefeed (\n) characters are replaced with the correct CRLF pairs required 
for the edittext, and tabs are replaced with "\t".

Lastly, I have just downloaded (yet) another zip utility; I think this may be
/the/ one (yeah, love beckons!). Guaranteed I have just fouled something up!


Version 0.0.9
=============

Added message to prove identified window really is edita. (AG)
Fixed problem with I-Beam cursor and focus on main edit window (AG)
Fixed scrolling problem when typing past end of screen (AG)
Reorganised and renamed language files as ealng_de.txt and ealng_en.txt. (JL)
Removed LANG=#409 (ie English) in eaxlate.txt
Column selection undo/redo reworked.


Version 0.0.8 
=============

Added support for drag and drop files.

Added single instance handling and open file(s) specified on command line.

Now checks for files needing saving before close.

Rewrote menus and added translation (see eatext_de.e and eatext_en.e)
Language choice is based on control panel, regional settings (see also
eaxlate.e).

Made the horizontal and vertical scrollbars attach to the edit area, not
the entire window. Added "Permanent Scrollbars" option to reduce screen
flicker when switching tabs. If this is set, the horizontal and vertical
scroll bars, if in use, remain visible, so the window does not need a full
repaint when switching between long/wide sources and short/narrow ones.

Added most of the code for Run (F5), but...

Rewrote capture console output code as discussed on EUforum.
 [I seriously lost the plot here.
  a) ex vs exw vs exwc...
  b) crashes on 2.4 exwc if a gui program...
  c) think I need to start project management to finish this off
  Running a gui program on exwc (2.4) has problems... See:
  --DEV Pete's test for running 2.5 (on C:) not 2.4 (on E:):
  in eacons.ew, and -- WARNING: ReadFile may crash on 2.4:
  in eacons.exw. It works alot better on 2.5 (exwc) than 2.4.
  I will be grateful for any help/full test in this area ;-))
  d) I may have made some desperate/wrong choices here! ]

BUGFIX: undo/redo of (un)comment/indent always applied to line 1.

Completed tab context menu and associated directory handling.

Added Ctrl Shift PageUp/Down to re-order file tabs.
 (but gave up on the [minor] screen flicker issue)

Added I-Beam cursor when over edit area.

Edita now opens readme.txt when it is first installed (ie no edita.ini file)

Completed line length handling (for the horizontal scrollbar).


Version 0.0.7 17/02/2005
========================

Date and time in the status bar now show the locale user default settings
(as set in control panel, regional settings).

Added tab text colourisation to show file requires saving. The tab colour is
properly reset by undo to the exact point of the last file save.

Save as now resets the syntax (if save with a different file extension, esp. if
saving an [untitled] (aka new) file, and also resets the tab bar text.

Some bugs in text selection near the end of a line and in text deletion fixed.

Added context menu to the edit area.

Added Capture console i/o. Pressing F5 runs test2.exw as a demo. [More work is
required to complete the run program logic, esp. save files before run].
Output is in a new re-sizeable message area.


Version 0.0.6 07/02/2005
========================

Fixed a couple of problems with saving the font settings.

New version of arwen with enhanced tabbing between controls and accelerator keys.

Added file save and save as. Binary dumps of files about to be overwritten are
copied to the backups directory (automatically created if it does not exist),
with a file name derived from the year,month,day,hour, and minute (artificially
incremented until it is unique).
You may wish to periodically empty or delete this directory.

Added url highlighting and double click to launch.

Added doubleclick and Ctrl W select word.

Alt '<' added to perform unindent (as well as/equivalent to Shift Tab)
Alt '>' added to perform indent (as well as/equivalent to Ctrl Tab)
Replaced use of alt left/right (was (un)indent); now performs column selection:
Alt left/right now perform column selection,
Ctrl Shift Alt left/right perform column selection by word jump,
Alt mouse move with left mouse button held down performs column selection.
Note that you can switch between normal and column selection mode by releasing the Alt key,
and either pressing Shift (and optionally Ctrl) left/right/up/down or, if the left mouse 
button is still held down, moving the mouse pointer (potentially without moving off the 
letter it is currently on). For example, Alt Page Up and Alt Page Down are ignored; if you
want to perform column select over a number of pages, place the cursor at the start (or end)
of the selection, use Shift Page Up/Down to get roughly around the end of the required
selection, then use Alt up/down/left/right to finish.

Fixed nasty bug in undo/redo when unindent/uncomment partially applied to selected lines.


Version 0.0.5 02/02/2005
========================

Cut        (Ctrl X                        or Ctrl Delete)
Cuta        (Ctrl Shift X    or Ctrl Shift Delete)
Copy        (Ctrl C            or Ctrl Insert)
Copya    (Ctrl Shift C        or Ctrl Shift Insert)
Paste    (Ctrl V                        or Shift Insert)
Undo (Ctrl Z)
Redo (Ctrl Y)
Select all (Ctrl A)
Duplicate line (Ctrl D)
Word jump (Ctrl leftarrow/rightarrow, with Shift extends selection)
indent (Ctrl Tab [[or Alt rightarrow -- *replaced in 0.0.6 with Alt>*]])
unindent (Shift Tab [[ or Alt leftarrow -- *replaced in 0.0.6 with Alt<*]])
comment (Ctrl '>' or Alt Insert)
uncomment (Ctrl '<' or Alt Delete) [NOT Shift Ins/Del]
insert mode toggle (Insert)
column mode (via variable "columnMode" on line 12 of edita.exw),<br>
ini file: reopen previous files, save window size/pos, options.


Version 0.0.4 26/01/2005
========================


Alpha version. Still no file save, undo/redo...

New since 0.0.3:
cursor + mouse handling largely rewritten
delete, backspace, character&tab insert, home, end, return
indent/unindent
showSpecials (Ctrl+Shift+H) option added
database.e and win32lib sections added to syn/Euphoria.syn
syn load made much more robust.
new syn/None.syn file added
several integers were changed to be atoms.

http://palacebuilders.pwp.blueyonder.co.uk/euphoria.html

A modified copy of arwen is included.
Unpack to any directory and run edita.exw

Thanks to all who responded with bug reports and suggestions.
Some selected replies:

Tommy Carlier wrote:

-->I just tried Edita, and I have to say it's very fast.
Thanks
>The only problem is that it crashes a lot. You should change all the
>variables that are used for storing pointers or handles from Windows:
>use atoms instead of integers.
--I'm not a complete idiot. Parts of me are missing.

SNCR, these are fixed in 0.0.4


Juergen Luethje wrote:

>Look how things are done in ConTEXT. ;-)
--Thanks for the tip, I've installed a copy.

>I also like a keyboard shortcut and/or toolbar icon for toggling between
>normal view, and showing the characters that are "really" in the file
>(i.e. making Space, Tab, Linebreak characters visible by showing special
>symbols for them). I hope this is understandable. :-)
--I've added this for 0.0.4 ;-))

>For general purpose usage, the only thing missing for me in ConTEXT is
>automatic highlighting of URLs and making them "clickable" ...
--Interesting idea, this can be done. I've added a list of tokens (ftp,http,
--mailto,...) to the new None.syn file so I'll be able to trap that and 
--process the rest of the url, so I can underline it. I haven't written the
--doubleclick processing (as per MEditor) yet, so I'll do that at the 
same time.
>
>Make everything as simple and straight-forward as possible.
>
-->Concerning the SYN files, it's IMHO simpler when Edita defines its own
>comment character(s), which is/are the same for all SYN files, rather
>than defining individual comment character(s) in each SYN file.
>This also would avoid the following current disadvantage, that you
-->mention in 'Euphoria.syn':
-->"If the language does not support line comments then you can't comment
-->the syn file!"
The "problem" I was trying to address is that in C/C++ -- is an operator. 
If it is not in the operator list, Edita will use the illegal colour (which 
it will on ++ in Euphoria files)
--You may have a point, but I'll leave it alone for now. 

>Is it possible to add user-defined sections to a SYN file? People would
>like to add syntax higliting for Win32Lib or ARWEN routines, for global
>routines of their own often used private libraries etc.
Yes. Just add a section (use any name, it is only used on the Colour window) 
starting in column 1 and a bunch of tokens (not in column 1). 
--I've copied in sections for win32lib and database.e for 0.0.4
Feel free to rename, split up, or merge the sections, though a .syn file 
--from one machine won't work with a .clr file from somewhere else, so start 
by pasting the .clr details into the Scheme section at the end of the .syn,
and deleting the .clr file, which is (over)written on exit from the Colours
window.

>I compiled Edita version 0.04 (25/01/2005),
--Oops, I must have gotten out of sync somewhere. I've just packaged
--0.0.4 so there'll be a wee bit of confusion over version nos, but
no doubt 0.0.5 will be out fairly shortly. (not as if it matters
--in something which can't save files). I didn't get round to trying
them, maybe tomorrow.
