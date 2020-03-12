--
-- eaini.e
--
without trace

--/* 4.0.2
include builtins\misc.e
--*/

include ini.e as ini    -- NOT part of arwen, using a copy of the MEditor one
--with trace
--integer flag

function mini(sequence Name, integer Type, object Default)
    ini:defineIniHeading(Name, Type)
    return ini:getIniValue(Name,Default)
end function

--integer once
--      once = 1
--
--procedure oops()
--  if once then
--      void = messageBox("Warning","corrupt cursor info in .ini file",0)
--      once = 0    -- don't output same message 30 times
--  end if
--end procedure

function by7(sequence text)
-- convert sprint sequence into 7 (non-negative) digits, and ,0,0}
-- Now 11 with TopChunk & isWordWrapON.
integer i,c,ci,n
sequence res
    if text[1]='{' then
        i = 1
        ci = 2
        n = 0
        res = repeat(0,12)
        while 1 do
            c = text[ci]
            if c>='0' and c<='9' then
                n = n*10+c-'0'
            elsif c=',' then
                res[i] = n
                i += 1
                if i>12 then exit end if
                n = 0
            elsif c='}' then
                if i<7 then exit end if
                res[i] = n
                return res
            end if
            ci += 1
            if ci>length(text) then exit end if
        end while
    end if
--  oops()
    return repeat(0,9)
end function

--with trace
global function validCOD(sequence txt)  -- also used by eaopts
sequence d
integer v,ch
    if length(txt)!=19 then return 0 end if
    if txt[5]!='/' then return 0 end if
    -- year
    v = 0
    for i=1 to 4 do
        ch = txt[i]
        if ch<'0' or ch>'9' then return 0 end if
        v = v*10+ch-'0'
    end for
    d = repeat(0,6)
    if v<1900 then return 0 end if
    d[1] = v
    -- month
    if txt[8]!='/' then return 0 end if
    v = 0
    for i=6 to 7 do
        ch = txt[i]
        if ch<'0' or ch>'9' then return 0 end if
        v = v*10+ch-'0'
    end for
    if v>12 then return 0 end if
    d[2] = v
    -- day
    if txt[11]!=' ' then return 0 end if
    v = 0
    for i=9 to 10 do
        ch = txt[i]
        if ch<'0' or ch>'9' then return 0 end if
        v = v*10+ch-'0'
    end for
    if v>31 then return 0 end if
    d[3] = v

    -- hour
    if txt[14]!=':' then return 0 end if
    v = 0
    for i=12 to 13 do
        ch = txt[i]
        if ch<'0' or ch>'9' then return 0 end if
        v = v*10+ch-'0'
    end for
    if v>23 then return 0 end if
    d[4] = v
    -- minute
    if txt[17]!=':' then return 0 end if
    v = 0
    for i=15 to 16 do
        ch = txt[i]
        if ch<'0' or ch>'9' then return 0 end if
        v = v*10+ch-'0'
    end for
    if v>59 then return 0 end if
    d[5] = v
    -- second
    v = 0
    for i=18 to 19 do
        ch = txt[i]
        if ch<'0' or ch>'9' then return 0 end if
        v = v*10+ch-'0'
    end for
    if v>59 then return 0 end if
    d[6] = v

    isLegacyTcod = d[1..6]

    return 1
end function

global procedure createTC(integer killold)
sequence tabstyles
atom tcStyle,tcStyleEx
    if killold then
        void = destroy(TC)
    end if
    tabstyles = getClassStyles(TabControl)
    tcStyle = tabstyles[1]
    tcStyleEx = tabstyles[2]
    if isTabsOwnerDrawn then
        tcStyle = or_bits(tcStyle,TCS_OWNERDRAWFIXED)
    else
        tcStyle -= and_bits(tcStyle,TCS_OWNERDRAWFIXED)
    end if
    setClassDefaults(TabControl, tcStyle, tcStyleEx)
    tcStyle = 0
    if isMultiLineTab then
        tcStyle += TCS_MULTILINE
    end if
    if isRaggedRight then
        tcStyle += TCS_RAGGEDRIGHT
    end if
    if isTabottom then
        tcStyle += TCS_BOTTOM
    end if
    TC = create(TabControl, "" , 0, Main, 0,0,372,122,tcStyle)      -- Tab Control
    TChwnd = getHwnd(TC)
    if killold then -- else done in edita.exw (and TChandler is not yet assigned)
        setHandler(TC, TChandler)
    end if
end procedure


procedure addTool(sequence tool)
    if not find(tool,isToolSet) then
        isToolSet = append(isToolSet,tool)
    end if
end procedure

sequence prevfiles

global integer newcurrfile

integer wasDebug

sequence prevcursel

--with trace
procedure loadINI()
integer wasNewIni, lendiff
sequence cl
integer k   -- dupToolsError

--integer flag
integer x,y,w,h

    wasNewIni = isNewIni()

    ini:defineIniGroup("Main")
    isDebug                 = mini("debug", BOOL, False)
    wasDebug = isDebug

    isFaceName              = mini("Face Name", TEXT, "ANSI_FIXED_FONT")
    isPointSize             = mini("Font Tenths", INT, 100)/10
    isWindowPos             = mini("Window Position", SEQ, {20, 20})
    isWindowRect            = mini("Window Size", SEQ, {500, 380})
    x = isWindowPos[1]
    y = isWindowPos[2]
    w = isWindowRect[1]
    h = isWindowRect[2]
    void = c_func(xMoveWindow, {mainHwnd, x, y, w, h, 1})
    isWindowMaximised       = mini("Window Maximized", BOOL, False)

    isShowFullPath          = mini("Show Full Path", BOOL, True)
    isToolBarVIS            = mini("Tool Bar Visible", BOOL, True)
--  setVisible(TB,isToolBarVIS)
--  if isToolBarVIS then toggleToolBar() end if -- moved to eamenus.ew
    isTabBarVIS             = mini("Tab Bar Visible", BOOL, True)
    isSingleDir             = mini("Single Dir Tabbar", BOOL, False)
    isMultiLineTab          = mini("Multi Line Tab", BOOL, True)
    isTabottom              = mini("Tabs at Bottom of Screen", BOOL, False)
    isTabsOwnerDrawn        = mini("Owner Drawn Tabs", BOOL, True)
    isCloseLeft             = mini("Close Left", BOOL, True)
    isRaggedRight           = mini("Ragged Right Tabs", BOOL, False)
    createTC(0)
    isFilePanelVIS          = mini("File Panel Visible", BOOL, False)
    setVisible(TV,isFilePanelVIS)
--  setVisible(FP,isFilePanelVIS)
    isFilePanelWidth        = mini("File Panel Width", INT, 275)
    isMessageVIS            = mini("Message Area Visible", BOOL, False)
    setVisible(MA,isMessageVIS)
    isMessageHeight         = mini("Message Area Height", INT, 100)
    isHScrollVIS            = mini("Horizontal Scroll Bar", BOOL, True)
    isVScrollVIS            = mini("Vertical Scroll Bar", BOOL, True)
--  setVisible(HHH,isHScrollVIS)
--  setVisible(VVV,isVScrollVIS)
    isMoveCursorOnScroll    = mini("Move Cursor On Scroll", BOOL, False)
    isScrollPerm            = mini("Permanent Scrollbars", BOOL, False)
    isLineNumbers           = mini("Line Numbers", BOOL, False)
    isFoldMarginPerm        = mini("Permanent Fold Margin", BOOL, False)
    marginRqd = isFoldMarginPerm
    isMarginSpace           = mini("Space after Margin", BOOL, False)
    isStatusBar             = mini("Status Bar", BOOL, True)
    setVisible(SB,isStatusBar)
    isFormatErr             = mini("Reformat ex.err", BOOL, True)
    isSingleInstance        = mini("Single Instance", BOOL, True)

--DEV 1/9/07 moved below the flag that is supposed to control this!
--       Also, ignore the default if no edita.ini exists, to
--       at lest give the chance for a later edit...

--if not isDebug then -- 19/3/10 (no help)
    if isSingleInstance and not wasNewIni then
        if not checkSingleInstance(CD_EDITA) then
            -- (btw, above performs abort(0) if found and isDebug=0,
            --  that is after sending a CD_OPEN message if rqd.)
            isDebug = 0
        end if
    end if
--end if
    -- GB - Messages moved here from eaxlate.e, to after the single instance check.
    void = getxlstatus()
    if void[1] = -1 then    -- file could not be opened.
        void = messageBox("Warning","File lang\\elng_" &void[2]&".txt not found.\n"&
                                    "Program will continue in English.\n\n"&
                                    "See lang\\locales.txt for more info."
--              &sprintf("\n\n(hex code:%x)",loWord(c_func(xGetUserDefaultLangID,{})))
                        ,0)
--  elsif k = -2 then
--      void = messageBox("Warning",
--      sprintf("Locale %x unrecognised.\nProgram will continue in English.",void[3]),0)
    end if

    isRestoreOnOpen         = mini("Restore Files On Open", BOOL, True)
    isAutoSaveOnExit        = mini("Auto Save Files On Exit", BOOL, False)
    isAutoSaveOnTabSwitch   = mini("Auto Save On Tab Switch", BOOL, True)
    isAutoSaveTimer         = mini("Auto Save every nnn seconds", INT, 0)
    isAutoBackup            = mini("Automatic Backup", BOOL, True)
    isRetainBackupsFor      = mini("Retain backups for", INT, 2)
    isLoadErrFileOnError    = mini("Load ex.err on error", BOOL, False)
    isReplaceTabs           = mini("Replace Tabs With Spaces", BOOL, True)
    isLegacyTabHandling     = mini("Legacy Tab Handling", BOOL, not isReplaceTabs)
    isLegacyTcod            = mini("Cut-Off Date", TEXT, "")
    isTabWidth              = mini("Tab Spacing", INT, 4)
    if not validCOD(isLegacyTcod) then
        isLegacyTcod = 0
        if isReplaceTabs or isTabWidth=8 then
            isLegacyTcod    = date()
--          isLegacyTcod[1] = isLegacyTcod[1]+1900
            isLegacyTcod    = isLegacyTcod[1..6]
        end if
    end if
    initT() -- now isTabWidth is known
    isHomeFirstNonBlank     = mini("Home jumps to first non-blank character", BOOL, True)
    mouseWheelScroll        = mini("mouseWheelScroll", INT, 5)
    isCaretBlinkTime        = mini("Caret Blink Time", INT, 250)
    void = c_func(xSetCaretBlinkTime,{isCaretBlinkTime})
--  isLineLengthError       = mini("Show Line Length Errors", BOOL, False)
    isUndoTime              = mini("Retain undo for nn seconds", INT, 3600)
    isAutoComplete          = mini("Autocompletion", INT, True)
    isBackGround            = mini("Background Processing", INT, True)
    isCurrProject           = mini("Current Project", INT, 0)   --DEV unused
    isClearOverStrike       = mini("ClearOverStrike", INT, #0C) -- default is file and page
    crashpath               = mini("mostRecentErrorPath", TEXT, initialcurrentdir)

--  ini:defineIniHeading("win32libs_exh_Size", INT)
--  ini:defineIniHeading("glocals_exh_Size", INT)
--
    newcurrfile             = mini( "currfile", INT, 0)

    ini:defineIniGroup("tools")
    isToolSet = ini:getIniTextValues()
--DEV hack for don cole's problem:
--pp(isToolSet)
    k = length(isToolSet)
    if k and find(isToolSet[k],isToolSet)<k then
--      printf(1,"Duplicate tools section [%d]; restart Edita to see if problem solved\n",k)
        void = proemh("Warning",sprintf("Duplicate tools section [%d]; restart Edita to see if problem solved\n",k),0)
        while 1 do
            k -= 1
            if k=0 then exit end if
            if find(isToolSet[k],isToolSet)=k then exit end if
        end while
        isToolSet = isToolSet[1..k]
        k = 1
    else
        k = 0
    end if
--DEV: (use the version no?)
    addTool("Database Viewer:tedb")
    addTool("Verify edita.edb:vedb")
    if k then   -- dupToolsError
        pp(isToolSet)
    end if

    if isDebug=False then
        ini:defineIniGroup("previous session")
        prevfiles = ini:getIniTextValues()
        ini:defineIniGroup("session cursel")
        prevcursel = ini:getIniTextValues()

        if wasNewIni and length(prevfiles)=0 then
--          prevfiles = command_line()
--          prevfiles = prevfiles[2..2]
--          for i = length(prevfiles[1]) to 1 by -1 do
--              if prevfiles[1][i]='\\' then
--                  prevfiles[1] = prevfiles[1][1..i]&"readme.txt"  --DEV or what?
--                  exit
--              end if
--          end for
            prevfiles = {initialcurrentdir&"welcome.txt"}
        end if
    else
    --DEV
--      prevfiles = {}
        prevfiles = {initialcurrentdir&"welcome.txt"}
--      prevfiles = {initialcurrentdir&"redirect.htm"}
--      prevfiles = {initialcurrentdir&"list9.asm"}
    --  prevfiles = {initialcurrentdir&"a.exw"}
    --  prevfiles = {initialcurrentdir&"demo_lists.exw"}
    --  prevfiles = {initialcurrentdir&"aa.asm"}
    --  prevfiles = {initialcurrentdir&"test7.exw"}
    --  prevfiles = {initialcurrentdir&"test2.e"}
    --  prevfiles = {initialcurrentdir&"edita.exw"}
    --  prevfiles = {initialcurrentdir&"test.exw"}
    --  prevfiles = {initialcurrentdir&"pkd.exw"}
    --  prevfiles = {initialcurrentdir&"test.htm"}
    --  prevfiles = {initialcurrentdir&`doc\t.htm`}
--      prevfiles = {initialcurrentdir&"ccaout.e"}
--      prevfiles = {initialcurrentdir&"eamenus.ew"}
--      isLineNumbers = True
--      prevfiles = {`C:\P2\new\EUCount\database.e`}
--      prevfiles = {`C:\p5\demo\Win32Demo\Tooltip.EXW`}
--      prevfiles = {`C:\Program Files\Phix\demo\win32lib\win32lib.ew`}
--      prevfiles = {`C:\Program Files (x86)\Phix\e01.exw`}
--      prevfiles = {`C:\Program Files (x86)\Phix\test.pas`}
        prevcursel = ""
        isSingleInstance = False
    end if
    cl = command_line()
    if length(cl)>2 then
        prevfiles &= cl[3..length(cl)]
        newcurrfile = length(prevfiles)
    end if

    lendiff = length(prevfiles) - length(prevcursel)
    if lendiff>0 then
        prevcursel &= repeat("*",lendiff)
    elsif lendiff<0 then
        prevcursel = prevcursel[1..length(prevfiles)]
    end if

--27/3/2010. Routine split, now completed after ClipRect/linesPerPage set:
end procedure

global procedure loadIniPart2()
integer elen, lenfcm1
sequence filename
sequence oneline
sequence msg, fmt, args
integer report

    msg = ""
    for i = 1 to length(prevfiles) do
--      currfile=0  -- 10/1/08
        filename = prevfiles[i]
        if openFile(filename,1,isLegacyTabHandling) then
            filecursel[currfile] = by7(prevcursel[i])
            restcursel()

            -- (avoid messages for selected machine-generated files)
            report = not find(filename,{"list.asm"})

            --
            -- and ensure all cursors etc are in range
            --
            lenfcm1 = length(filetext[currfile])-1
            if CursorY>lenfcm1 then
                if report then
                    msg = "Warning: CursorY>length(filetext[currfile])-1\n"
                    fmt = " (%d>%d for %s)"
                    args = {CursorY,lenfcm1,filename}
                end if
                CursorX = 0
                CursorY = 0
                TopLine = 0
                Column = 0
                selON = 0
                savecursel()
            else
                --
                -- NB: This of course is pants if you install/run 0.2.9 say, and 
                --    then edit a file with 0.2.8 and the old tab-4 handling...
                -- (such as I do after leaving a compile error in dev sources)
                --
                oneline = ConvertTabs(filetext[currfile][CursorY+1],8,isTabWidth)
                elen = ExpLength(oneline)
                if CursorX>elen then
                    if report then
                        msg = "Warning: CursorX>elen\n"
                        fmt = " (%d>%d on line %d for %s)"
                        args = {CursorX,elen,CursorY+1,filename}
                    end if
                    CursorX = 0
                elsif selON then
                    if selY>lenfcm1 then
                        if report then
                            msg = "Warning: selY>length(filetext[currfile])-1\n"
                            fmt = " (%d>%d for %s)"
                            args = {selY,lenfcm1,filename}
                        end if
                        selON = 0
                    else
                        -- NB see note above before debugging!
                        if selY!=CursorY then
                            oneline = ConvertTabs(filetext[currfile][selY+1],8,isTabWidth)
                            elen = ExpLength(oneline)
                        end if
                        if selX>elen then
                            if report then
                                msg = "Warning: selX>elen\n"
                                fmt = " (%d>%d on line %d for %s)"
                                args = {selX,elen,selY+1,filename}
                            end if
                            selON = 0
                        end if
                    end if
                end if
            end if
-- added 22/3/10
--      else                        -- 27/03/2010
        elsif i<newcurrfile then    -- 27/03/2010
            newcurrfile -= 1
        end if
    end for
    if newcurrfile>length(filecursel) then
        newcurrfile = length(filecursel)
    end if

    if length(msg) then
        -- Keeping it down to one message per session also avoids problems
        --  with IdleHandler kicking in before this lot is finished...
        --  Alternatively, of course, we would start by setting msg/fmt/args 
        --  to {}, use append above, and a loop here.
        void = proemh("Warning",msg&sprintf(fmt,args),0)
--      puts(1,msg)
--      printf(1,fmt&"\n",args)
    end if

end procedure

loadINI()


global procedure saveINI()  -- can be called many times. [???]
integer newcf
    if currfile then
--DEV savecursel?
        filecursel[currfile] = {CursorX,CursorY,TopLine,Column,selON,selX,selY,0,0,TopChunk,isWordWrapON,isFTP}
    end if
    switchToIniGroup("Main")
    setIniValue("debug", wasDebug)  --NB saveINI() won't be called if isDebug is True;
                                    -- we use wasDebug, not isDebug here because debug
                                    -- mode is automatically switched off if another
                                    -- instance of edita is not running (see easinst.e)
    setIniValue("Face Name", isFaceName)
    setIniValue("Font Tenths", floor(isPointSize*10+0.5))
    setIniValue("Window Position", isWindowPos)
    setIniValue("Window Size", isWindowRect)
    setIniValue("Window Maximized", isWindowMaximized(Main))

    setIniValue("Show Full Path", isShowFullPath)
    setIniValue("Tool Bar Visible", isToolBarVIS)
    setIniValue("Owner Drawn Tabs", isTabsOwnerDrawn)
    setIniValue("Tab Bar Visible", isTabBarVIS)
    setIniValue("Single Dir Tabbar", isSingleDir)
    setIniValue("Multi Line Tab", isMultiLineTab)
    setIniValue("Tabs at Bottom of Screen", isTabottom)
    setIniValue("Close Left", isCloseLeft)
    setIniValue("Ragged Right Tabs", isRaggedRight)
    setIniValue("File Panel Visible", isFilePanelVIS=1) -- 2 if opened via Ctrl Q
    setIniValue("File Panel Width", isFilePanelWidth)
    setIniValue("Message Area Visible", isMessageVIS=1) -- 2 if opened via F5
    setIniValue("Message Area Height", isMessageHeight)
    setIniValue("Horizontal Scroll Bar", isHScrollVIS)
    setIniValue("Vertical Scroll Bar", isVScrollVIS)
    setIniValue("Move Cursor On Scroll", isMoveCursorOnScroll)
    setIniValue("Permanent Scrollbars", isScrollPerm)
    setIniValue("Line Numbers", isLineNumbers)
    setIniValue("Permanent Fold Margin", isFoldMarginPerm)
    setIniValue("Space after Margin", isMarginSpace)
    setIniValue("Status Bar", isStatusBar)
    setIniValue("Reformat ex.err", isFormatErr)
    setIniValue("Single Instance", isSingleInstance)
    setIniValue("Restore Files On Open", isRestoreOnOpen)
    setIniValue("Auto Save Files On Exit", isAutoSaveOnExit)
    setIniValue("Auto Save On Tab Switch", isAutoSaveOnTabSwitch)
    setIniValue("Auto Save every nnn seconds", isAutoSaveTimer)
    setIniValue("Automatic Backup", isAutoBackup)
    setIniValue("Retain backups for", isRetainBackupsFor)
    setIniValue("Load ex.err on error", isLoadErrFileOnError)
    setIniValue("Replace Tabs With Spaces", isReplaceTabs)
    setIniValue("Legacy Tab Handling", isLegacyTabHandling)
    setIniValue("Cut-Off Date", sprintf("%04d/%02d/%02d %02d:%02d:%02d",isLegacyTcod))
    setIniValue("Tab Spacing", isTabWidth)
    setIniValue("Home jumps to first non-blank character", isHomeFirstNonBlank)
    setIniValue("mouseWheelScroll", mouseWheelScroll)
    setIniValue("Caret Blink Time", isCaretBlinkTime)
--  setIniValue("Show Line Length Errors", isLineLengthError)
    setIniValue("Retain undo for nn seconds", isUndoTime)
    setIniValue("Autocompletion",isAutoComplete)
    setIniValue("Background Processing",isBackGround)
    setIniValue("Current Project", isCurrProject)
    setIniValue("ClearOverStrike", isClearOverStrike)
    setIniValue("mostRecentErrorPath", crashpath)

    --
    -- save any modified files
    --

    -- this should not have changed, but make sure it is not discarded.
    switchToIniGroup("tools")
    for i=1 to length(isToolSet) do
        setIniTextValue(isToolSet[i])
    end for

    --
    -- save the edit session
    --
    if isRestoreOnOpen then
        newcf = currfile
        --DEV presumably don't do this unless we are about to close.
        for i=length(filecursel) to 1 by -1 do
            if find(filenames[i],{"ex.err","p.err"}) -- oft open, not worth saving file
            or match(untitled,filenames[i])=1 then   -- user chose not to save
                filepaths = filepaths[1..i-1]&filepaths[i+1..length(filepaths)]
                filenames = filenames[1..i-1]&filenames[i+1..length(filenames)]
                filecursel = filecursel[1..i-1]&filecursel[i+1..length(filecursel)]
                if newcf>=i then
                    newcf -= 1
                end if
            end if
        end for
        switchToIniGroup("Main")
        setIniValue("currfile", newcf)
        switchToIniGroup("previous session")
        for i=1 to length(filecursel) do
            setIniTextValue(filepaths[i]&filenames[i])
        end for
        switchToIniGroup("session cursel")
        for i=1 to length(filecursel) do
--          setIniTextValue(sprint(filecursel[i][1..7]))
            setIniTextValue(sprint(filecursel[i]))
        end for
    end if

    saveIniToDisk()

end procedure
