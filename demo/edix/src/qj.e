?"src/qj.e incomplete!"
--object void

--
-- edix\src\qj.ew
-- ==============
--
-- code to quickly extract names of all routines in the current file
--
-- Technical note: 
--  I think this should internally work in utf8; should
--  any filetext[i] be utf32, then utf32_to_utf8() it!
--

include keyboard.e as keybd

Ihandln ROUTINEWINDOW=NULL
Ihandle All, Globals, Sections, Filtxt, Filter, ROUTINELIST

--/*
constant
    ScreenW = c_func(xGetSystemMetrics, {SM_CXSCREEN}),
    ScreenH = c_func(xGetSystemMetrics, {SM_CYSCREEN}),
    ROUTINEWINDOW = create( Window, xl("Quick Jump"), 0, Main, 100, 10, ScreenW-200, ScreenH-90, 0),
    All = create( RadioButton, xl("All"), 0, ROUTINEWINDOW, 8, 15, 42, 20, 0 ),
    Globals = create( RadioButton, xl("Globals"), 0, ROUTINEWINDOW, 58, 15, 66, 20, 0 ),
    Sections = create( RadioButton, xl("Sections"), 0, ROUTINEWINDOW, 138, 15, 72, 20, 0 ),
    Filtxt = create( Label, xl("Filter"), 0, ROUTINEWINDOW, 220, 18, 35, 20, 0 ),
    Filter = create( EditText, "", 0, ROUTINEWINDOW, 255, 12, 237, 25, 0 ),
    FltrHwnd = getHwnd(Filter),
    ROUTINELIST = create( ListBox, "", 0, ROUTINEWINDOW, 10, 40, 480, 360, LBS_NOINTEGRALHEIGHT ),
    RLhwnd = getHwnd(ROUTINELIST)

    setCheck(All, True)
    setCheck(Globals, False)
    setCheck(Sections, False)

    setVisible(ROUTINEWINDOW, False)
--*/


--atom user32, xIsWindow
include cffi.e
set_unicode(0)

sequence routinescope, 
         routinenames, 
         routineparams,
         routinelinenumbers,
         routineends
--routinetype

--sequence BuiltinsPath
string BuiltinsPath

integer scope
constant ALL=0, GLOBAL=1, SECTION=-1

procedure setScope()
--  if isChecked(All) then
    if IupGetInt(All, "VALUE") then
--?"ALL"
        scope = ALL
--  elsif isChecked(Globals) then
    elsif IupGetInt(Globals, "VALUE") then
--?"GLOBAL"
        scope = GLOBAL
    else
--?"SECTION"
        scope = SECTION
    end if
end procedure

function inScope(integer i)
integer k
    k = routinescope[i]
    if scope=ALL then   -- all = 0 and 1
        return k>=ALL   -- ie not a section
    end if
    return (k=scope)
end function

function populateRoutineList()
--27/1/18:
sequence Ftext, name
--string Ftext, name
integer tally, ind

    setScope()

--/*
        IupSetInt(toolb_rtns,"VISIBLEITEMS",length(actset)+1)   -- [no idea why it needs the +1]
        for i=1 to length(actset) do
            IupSetAttributeId(toolb_rtns, "", i, actset[i])
        end for
--      IupSetInt(toolb_rtns,"VALUE",length(actset))
--*/
--  deleteItem(ROUTINELIST,0)   -- empty list
--  IupSetInt(ROUTINELIST,"1",NULL) -- empty list
    IupSetAttribute(ROUTINELIST,"REMOVEITEM","ALL") -- empty list
    -- populate list, applying filter
    tally = 0
    ind = 0
--  Ftext = lower(getText(Filter))
    Ftext = lower(IupGetAttribute(Filter,"VALUE"))
    for i=1 to length(routinenames) do
        name = routinenames[i]

--27/1/18: (bit of an expedient bugfix...)
        if not string(name) then name = utf32_to_utf8(name) end if

        if length(Ftext)=0 or match(Ftext,lower(name)) then
            if inScope(i) then
                if sequence(routineparams[i]) then
                    name &= routineparams[i]
--27/1/18: (ditto)
--if name={-1} then name = {-1} end if  -- compiler bugfix?? [no help]
--                  if not string(name) then name = utf32_to_utf8(name) end if
                    if not string(name) then name = utf32_to_utf8(name,+1) end if
                end if
                if length(name)>512 then name = name[1..512] end if -- added 19/11/2013
--              {} = insertItem(ROUTINELIST,name,0)
                tally += 1
                IupSetAttributeId(ROUTINELIST, "", tally, name)
                if routinelinenumbers[i]<=CursorY+1 then
                    ind += 1
                end if
            end if
        end if
    end for
    -- set scroll index to be in relative to cursor place in file
    if ind>9 and ind<tally then
--      setIndex(ROUTINELIST, Min(ind+5,tally))
        IupSetInt(ROUTINELIST,"VALUE",min(ind+5,tally))
    end if
--  setIndex(ROUTINELIST, ind)
    IupSetInt(ROUTINELIST,"VALUE",ind)
    return tally
end function

--with trace
function getActualLineNumberOfRoutine(integer ind)
--sequence Ftext
string Ftext
integer dbg = ind
    setScope()

--  Ftext = lower(getText(Filter))
    Ftext = lower(IupGetAttribute(Filter,"VALUE"))
    for i = 1 to length(routinenames) do
        if inScope(i) then
            if length(Ftext)=0 or match(Ftext,lower(routinenames[i])) then
                ind -= 1
                if ind=0 then return routinelinenumbers[i] end if
            end if
        end if
    end for
--trace(1)
--  {} = messageBox("Sorry","Internal Error",MB_OK)
    IupMessage("Sorry","Internal Error")
    return 1    -- just jump to line one then
end function

--/*
--procedure setHelpFont(integer id)
procedure setHelpFont(Ihandln id)
?{"setHelpFont",id}
--/*
    if sequence(hFont) then
        {} = sendMessage( id, WM_SETFONT, hFont[4], 1 ) -- EA_Normal
    else
        {} = sendMessage( id, WM_SETFONT, hFont, 1 )
    end if
--*/
end procedure
--*/

--DEV use the jumpTo?
procedure jumpToNewLine(integer newline)
    CursorY = newline-1
    CursorX = 0
    selON = 0
    forceCursorOnscreen()
end procedure

sequence onScreen
         onScreen = {}

integer clearFilterIfEmpty  -- only set from initial onClick.

integer justDblClick
        justDblClick = 0

integer initialShow
        initialShow = 0

function action_cb(Ihandle /*ih*/, integer state)
    if state=1 then
        setScope()
        string Ftext = IupGetAttribute(Filter,"VALUE")
        if not equal(onScreen,{scope,Ftext}) then
            onScreen = {scope,Ftext}
            -- insert these into list
            if not populateRoutineList() and clearFilterIfEmpty then
                IupSetAttribute(Filter,"VALUE","")
                if populateRoutineList() then end if
            end if
        end if
    end if
    return IUP_CONTINUE
end function
constant cb_action = Icallback("action_cb")

function dblclick_cb(Ihandle /*ROUTINELIST*/, integer item, atom /*pText*/)
    -- Focus issues if I act immediately on the double click, 
    -- so instead I set a flag and act on the next mouse up.
    justDblClick = getActualLineNumberOfRoutine(item)
    return IUP_CONTINUE
end function
constant cb_dblclick = Icallback("dblclick_cb")

function button_cb(Ihandle /*ROUTINELIST*/, integer /*button*/, pressed, /*x*/, /*y*/, atom /*pStatus*/)
    if pressed=0 and justDblClick!=0 then
        jumpToNewLine(justDblClick)
        justDblClick = 0
        IupHide(ROUTINEWINDOW)
    end if
    return IUP_CONTINUE
end function
constant cb_button = Icallback("button_cb")

function rtn_keys_cb(Ihandle ih/*HelpWin*/, atom c)
    if c=K_ESC then
        IupHide(ROUTINEWINDOW)
    elsif c=K_CR then
        integer item = IupGetInt(ROUTINELIST,"VALUE"),
                line = getActualLineNumberOfRoutine(item)
        jumpToNewLine(line)
        IupHide(ROUTINEWINDOW)
    elsif c=K_DEL
       or c=K_BS then
        string Ftext = IupGetAttribute(Filter,"VALUE")
        if length(Ftext) then
            Ftext = Ftext[1..$-1]
            IupSetAttribute(Filter,"VALUE",Ftext)
            if populateRoutineList() then end if
        end if
    elsif c>=' ' and c<='~' then
        string Ftext = IupGetAttribute(Filter,"VALUE")&c
        IupSetAttribute(Filter,"VALUE",Ftext)
        if populateRoutineList() then end if
    end if
    return IUP_CONTINUE
end function
constant cb_rtn_keys = Icallback("rtn_keys_cb")

procedure create_rtnwin()
    All = IupToggle("&All",cb_action,"CANFOCUS=NO")
    Globals = IupToggle("&Globals",cb_action,"CANFOCUS=NO")
    Sections = IupToggle("&Sections",cb_action,"CANFOCUS=NO")
    Ihandle radio = IupRadio(IupHbox({All,Globals,Sections}))
    Filtxt = IupLabel("Filter")
    Filter = IupText("EXPAND=HORIZONTAL,CANFOCUS=NO,ACTIVE=NO")
--  IupSetCallback(Filter,"GETFOCUS_CB",cb_filter_getfocus)
    Ihandle hbox = IupHbox({radio,Filtxt,Filter},"ALIGNMENT=ACENTER")
    ROUTINELIST = IupList("EXPAND=YES,SIZE=500x250")
    IupSetCallback(ROUTINELIST,"DBLCLICK_CB",cb_dblclick)
    IupSetCallback(ROUTINELIST,"BUTTON_CB",cb_button)
    ROUTINEWINDOW = IupDialog(IupVbox({hbox,ROUTINELIST},"MARGIN=5x5,GAP=5"))
    IupSetCallback(ROUTINEWINDOW,"K_ANY",cb_rtn_keys)
    IupSetAttributeHandle(ROUTINEWINDOW,"PARENTDIALOG",dlg)
end procedure

--/*
without trace
function routinelistHandler(integer id, integer msg, atom wParam, object lParam)
sequence rect
integer index
sequence Ftext
--  if msg=WM_SIZE then
--      rect = getClientRect(ROUTINEWINDOW)
--      void = c_func( xMoveWindow, {RLhwnd, 2, 40, rect[3]-4, rect[4]-42, 1} )
--      void = c_func( xMoveWindow, {FltrHwnd, 255, 12, rect[3]-260, 25, 1} )
--  elsif id=ROUTINELIST then
--      if msg=WM_LBUTTONDBLCLK then
--          -- I get focus issues if I act on the double click,
--          -- (probably because if I hide the window and setfocus to
--          --  Main, then windows gets a mouseup message for the now
--          --  hidden control, it nicks focus back from Main)
--          -- so instead I set a flag and act on the next mouse up.
--          justDblClick = 1
--      elsif (msg=WM_LBUTTONUP and justDblClick)
--         or (msg=WM_CHAR and wParam=VK_RETURN) then
--          justDblClick = 0
--          index = getIndex(ROUTINELIST)
--          if index>0 then
--              index = getActualLineNumberOfRoutine(index)
--              jumpToNewLine(index)
--              msg = WM_CLOSE
--          end if
--      end if
--  elsif msg=WM_CTLCOLORLISTBOX and lParam=getHwnd(ROUTINELIST) then
--      void = c_func(xSetTextColor, {wParam, ColourTab[Other]})
--      void = c_func(xSetBkMode , {wParam, TRANSPARENT})
--      return {backBrush}
--  elsif msg=WM_COMMAND then
--      if find(id,{All,Globals,Sections}) then
--          if lParam=1 then
--              setCheck({All,Globals,Sections},False)
--              setCheck(id,True)
--          end if
--      elsif id=Filtxt then
--          setFocus(Filter)
--          return 0
--      end if
--      setScope()
--      Ftext = getText(Filter)
--      if not equal(onScreen,{scope,Ftext}) then
--          onScreen = {scope,Ftext}
--          -- insert these into list
--          if not populateRoutineList() and clearFilterIfEmpty then
--              setText(Filter,"")
--              if populateRoutineList() then end if
--          end if
--      end if
--  elsif id=All and msg=WM_SETFOCUS and initialShow then
--      -- there is an annoying focusbox on "All" which appears
--      -- on the initial display; suppress it while the 
--      -- routinelist is being built.
----DEV copy handling from tedb here.
--      return {1}
--  end if
--  if id!=Filter and msg=WM_KEYDOWN and wParam=VK_DELETE then
--      Ftext = getText(Filter)
--      if length(Ftext) then
--          setText(Filter,Ftext[1..length(Ftext)-1])
--          if populateRoutineList() then end if
--      end if
    elsif msg=WM_CHAR and wParam!=VK_ESCAPE and id!=Filter then
        if wParam>=' ' and wParam<='z' then
            setText(Filter,getText(Filter)&wParam)
            if populateRoutineList() then end if
        elsif wParam=VK_BACK then
            Ftext = getText(Filter)
            if length(Ftext) then
                setText(Filter,Ftext[1..length(Ftext)-1])
                if populateRoutineList() then end if
            end if
        end if
    elsif id=Filter and msg=WM_CHAR 
      and (wParam=VK_ESCAPE or wParam=VK_RETURN) then
        setFocus(ROUTINELIST)
    elsif (msg=WM_CLOSE)
       or (msg=WM_CHAR and wParam=VK_ESCAPE) then
        removeFocus(ROUTINEWINDOW)
        setVisible(ROUTINEWINDOW, False)
        setEnable(TC,True)
        setFocus(Main)
        call_proc(r_enableMenuToolBar,{})
    elsif msg=WM_CHAR then
        if    wParam=5  then                -- Ctrl E (Enlarge font size)
            EnlargeFontSize(+1)
            setHelpFont(ROUTINELIST)
        elsif wParam=18 then                -- Ctrl R (Reduce font size)
            EnlargeFontSize(-1)
            setHelpFont(ROUTINELIST)
        end if
    end if
    return 0
end function
setHandler({ROUTINEWINDOW,ROUTINELIST,All,Globals,Sections,Filter},
           routine_id( "routinelistHandler" ))
--*/

sequence RtnCharMap
    RtnCharMap = repeat(0,256)
    RtnCharMap['a'+1..'z'+1] = 1
    RtnCharMap['A'+1..'Z'+1] = 1
    RtnCharMap['0'+1..'9'+1] = 2
    RtnCharMap['_'+1] = 1
    RtnCharMap['$'+1] = 1   -- 17/12/20 (for js)
    RtnCharMap['-'+1] = -1
    RtnCharMap['\"'+1] = -2

sequence params

function getWord(sequence line, integer idx, integer getParams)
integer wstart
integer ch
--  while idx<=length(line) and not RtnCharMap[line[idx]+1] do
    while idx<=length(line) do
        ch = line[idx]
        if ch<128 and RtnCharMap[ch+1] then exit end if
        idx += 1
    end while

    if idx>length(line) then return "" end if

    wstart = idx

--  while idx<=length(line) and RtnCharMap[line[idx]+1]>0 do
    while idx<=length(line) do
        ch = line[idx]
        if ch>128 or not RtnCharMap[ch+1]>0 then exit end if
        idx += 1
    end while

    if getParams then
        params = line[idx..idx+find(')',line[idx..length(line)])-1]
    end if

--  string res = line[wstart..idx-1]
--  return res
    return line[wstart..idx-1]
end function

--function isReally(sequence keyword, sequence line, integer idx)
function isReally(string keyword, sequence line, integer idx)
    return equal(getWord(line,idx,0),keyword)
end function

sequence incset

--with trace
procedure foundInclude(sequence filepath)
--procedure foundInclude(string filepath)
sequence path
integer k
--if usegpp then
    filepath = get_proper_path(filepath,"")
    if not string(filepath) then ?9/0 end if
--else
--  filepath = cleanUpPath(filepath)
--end if
    path = ""
    for i=length(filepath) to 1 by -1 do
        if filepath[i]='\\' then
            path = filepath[1..i]
            filepath = filepath[i+1..length(filepath)]
            exit
        end if
    end for
    k = logFile(path,filepath,1)
    if not find(k,incset) then
        incset = append(incset,k)
    end if
end procedure

--with trace
procedure includeLine(sequence line, integer idx)
--procedure includeLine(string line, integer idx)
integer ch
object incpath, path
integer semicolon
--trace(1)
--24/1/18:
    if not string(line) then line = utf32_to_utf8(line) end if
    while idx<=length(line) and find(line[idx]," \t") do
        idx += 1
    end while
    line = line[idx..length(line)]
    idx = 1
    if length(line) and line[1] = '"' then
        -- quoted filename  
        idx = 2
        while idx<=length(line) do
            ch = line[idx]
            if ch='"' then exit end if
            if ch='\\' then
                line = line[1..idx-1]&line[idx+1..length(line)]
            end if
            idx += 1
        end while
        if idx>length(line) then return end if  -- oops
        if line[idx]!='"' then return end if        -- ""
        line = line[2..idx-1]
    else
        while idx<=length(line) do
            ch = line[idx]
            if find(ch,{' ','\t'}) then exit end if
            idx += 1
        end while
        line = line[1..idx-1]
    end if
--  if idx=1 then return end if         -- oops
    if not length(line) then return end if  -- oops
    if length(line)>length("builtins")
    and match("builtins",line)=1
    and find(line[length("builtins")+1],`\/`)
    and length(BuiltinsPath) then
        line = BuiltinsPath&line[length("builtins")+1..length(line)]
    end if
    if find(':',line) then
        -- absolute
--      if not atom (dir(line)) then
        if get_file_type(line)=FILETYPE_FILE then
            foundInclude(line)
        end if
        return
    end if
    if find(line[1],`\/`) then
        -- possibly absolute
--      if not atom(dir(line)) then
        if get_file_type(line)=FILETYPE_FILE then
            foundInclude(line)
            return
        end if
        line = line[2..length(line)]
    end if
    --
    -- Try parent directory first
    --
--DEV is this valid use of currfile?
--  if not atom(dir(filepaths[currfile]&line)) then
--  foundInclude(filepaths[currfile]&line)
--  if not atom(dir(gpath&line)) then
    string gline = join_path({gpath,line})
    if get_file_type(gline)=FILETYPE_FILE then
        foundInclude(gline)
        return
    end if
    --
    -- Try BuiltinsPath
    --
    if length(BuiltinsPath) then
        gline = join_path({BuiltinsPath,line})
--  and not atom(dir(BuiltinsPath&line)) then
        if get_file_type(gline)=FILETYPE_FILE then
            foundInclude(gline)
            return
        end if
    end if
    --
    -- OK, so try EUINC
    --
    incpath = getenv("EUINC")
    if not atom(incpath) then
        while 1 do
            semicolon = find(';',incpath)
            if semicolon!=1 then --nulls in path; skip
                if semicolon then
                    path = incpath[1..semicolon-1]
                else
                    path = incpath
                end if
--              if not find(path[length(path)],`\/`) then
--                  path &= '\\'
--              end if
                gline = join_path({path,line})
--              if not atom(dir(path&line)) then        -- FOUND IT!
                if get_file_type(gline)=FILETYPE_FILE then
                    foundInclude(gline)
                    return
                end if
            end if
            if not semicolon then exit end if
            incpath = incpath[semicolon+1..length(incpath)]
            if not length(incpath) then exit end if
        end while
    end if
    --
    -- OK, so try EUDIR/include...
    --
    path = getenv("EUDIR")
    if not atom(path) then
--      if not find(path[length(path)],`\/`) then
--          path &= '\\'
--      end if
--      if not atom(dir(path&`INCLUDE\`&line)) then     -- FOUND IT!
        gline = join_path({path,"include",line})
        if get_file_type(gline)=FILETYPE_FILE then
            foundInclude(gline)
        end if
    end if
    --oops
end procedure

--object ts -- timestamp data from eaedb.e

integer expro
--with trace

function extractAnyRoutineInformation(sequence line, integer lnumber, integer logG)
--function extractAnyRoutineInformation(string line, integer lnumber, integer logG)
-- see cycleThroughAllLines() for meaning of logG
integer idx, isGlobal, rtnType, c
sequence name
    idx = 1
    isGlobal = 0
    name = ""
    while idx<=length(line) and find(line[idx]," \t") do
        idx += 1
    end while
    if expro then
        while idx<=length(line) and find(line[idx],"1234567890") do
            idx += 1
        end while
        if idx=length(line) or line[idx]!='|' then return 0 end if
        idx += 1
    end if
    if idx<=length(line) then
        c = line[idx]
        integer k = find(c,"gp")
        if k then
            if isReally({"global","public"}[k],line,idx) then
                idx += 6 --length("global"[=="public"])
                isGlobal = 1
                while idx<=length(line) and find(line[idx]," \t") do
                    idx += 1
                end while
                if idx<=length(line) then
                    c = line[idx]
                end if
            end if
        end if
        if c='i' then
            if isReally("include",line,idx) then
                if logG then
                    includeLine(line,idx+length("include"))
                end if
            end if
        elsif c='-' then
            if logG and idx=1 and length(line)>10
            and equal(line[1..10],"--#include") then    -- NB not translated
                includeLine(line,11)
            elsif logG and idx=1 and length(line)>13
            and equal(line[1..13],"--/**/include") then -- NB not translated
                includeLine(line,14)
            end if
        elsif c='f' then
            if isReally("function",line,idx) then
                name = getWord(line,idx+length("function"),1)
                rtnType = 2
            end if
        elsif c='p' then
            if isReally("procedure",line,idx) then
                name = getWord(line,idx+length("procedure"),1)
                rtnType = 3
            end if
        elsif c='t' then
            if isReally("type",line,idx) then
                name = getWord(line,idx+length("type"),1)
                rtnType = 1
            end if
        elsif c='e' then
            if isReally("end",line,idx) then
                name = getWord(line,idx+length("end")+1,0) --DEV skip " \t"?
                if find(name,{"function","procedure","type"})
                and length(routineends) then
                    routineends[length(routineends)] = lnumber
                end if
                return 0
            end if
        end if
    end if

    if equal(name, "") then return 0 end if

    if isGlobal and logG then
        addGlobal(name)
--  ,params,lnumber,rtnType)--,ts)
    end if

--  if logG!=3 then
    routinescope &= isGlobal
--  routinetype &= rtnType
    routinenames = append(routinenames, name)
    routineparams = append(routineparams,params)
    routinelinenumbers &= lnumber
    routineends &= 0
--  end if

    return 1
end function

integer isErrFile isErrFile=0

function detab(sequence line)
integer k
    while 1 do
        k = find('\t',line)
        if k=0 then exit end if
        line[k] = ' '
    end while
    return line
end function

procedure cycleThroughAllLines(sequence text, integer logG)
--
-- logG should be zero, if the text being analysed is not from
-- filetext[currfile]. It should be 1 if we want section info,
-- or 2 if we don't need that.
--
-- logG is set to 3 for background processing.

sequence line
sequence prevline
--string line
--string prevline

    prevline = ""
    for i=1 to length(text) do
        line = text[i]
        if length(line) then
            if extractAnyRoutineInformation(line, i, logG) then
                prevline = ""
            elsif logG=1 then
                if length(line) > 5 then -- possible section underline
                    if isErrFile then
                        if find(line[3],`.\:`) and not equal(line[1..2],` "`) then
                            routinescope &= -1
--                          routinetype &= 0
                            routinenames = append(routinenames,detab(line))
                            routineparams = append(routineparams,0)
                            routinelinenumbers &= i
                            routineends &= 0
                        elsif equal(line,"Traced lines leading up to the failure:") then
                            routinescope &= -1
--                          routinetype &= 0
                            routinenames = append(routinenames,detab(line))
                            routineparams = append(routineparams,0)
                            routinelinenumbers &= i
                            routineends &= 0
                            exit
                        end if
                    elsif length(prevline)>2 and not equal(prevline[1..2],"--") then
                        if find(line[3..5],{"===","---"}) then
                            while length(prevline) and prevline[1]<=' ' do  -- strip leading tabs and spaces
                                prevline = prevline[2..length(prevline)]
                            end while
                            if match("end procedure",prevline)!=1
                            and match("end function",prevline)!=1 then
                                -- ignore spaces only etc lines above the underline:
                                for k=1 to length(prevline) do
--PL 21/03/2011 (might really want >' ' and <='~')
--                                  if prevline[k]>='1' and prevline[k]<='z' then
--                                  if prevline[k]>='0' and prevline[k]<='z' then
                                    if prevline[k]>' ' and prevline[k]<='~' then
                                        routinescope &= -1
--                                      routinetype &= 0
                                        routinenames = append(routinenames,detab(prevline[k..length(prevline)]))
--                                      routinenames = append(routinenames,detab(prevline))
                                        routineparams = append(routineparams,0)
                                        routinelinenumbers &= i-1
                                        routineends &= 0
                                        exit
                                    end if
                                end for
                            end if
                        end if
                    end if
                end if
                prevline = line
            end if
        else
            prevline = ""
        end if
--if alreadyBackGroundProcessing then doEvents(0) end if    -- no help...
    end for
end procedure

--integer rtnmsg
--      rtnmsg = 0

--with trace
--DEV global?
global procedure setListOfAllRoutines(integer target, sequence text, integer logG, sequence file_extension)
--
-- logG should be zero, if the text being analysed is not from
-- filetext[currfile]. It should be 1 if we want section info,
-- or 2 if we don't need that.
--
-- logG is set to 3 for background processing (global logging).
--integer rsfile

    routinescope = {}
--  routinetype = {}
    routinenames = {}
    routineparams = {}
    routinelinenumbers = {}
    routineends = {}

--trace(1)
    BuiltinsPath = getExtRunWith("exw")
    if length(BuiltinsPath) then
        for i=length(BuiltinsPath) to 1 by -1 do
            if find(BuiltinsPath[i],`\/`) then
                BuiltinsPath = BuiltinsPath[1..i]
                exit
            end if
        end for
        BuiltinsPath &= "builtins"
    end if

    if logG then
        incset = {}
        if logG!=3 then -- this already called from backGroundProcessing
            string path = filepaths[currfile],
                   name = filenames[currfile]
            {} = logFile(path,name,1)
            initForGlobalLogging(path,name)
        end if
    end if

    expro = find('.',file_extension)
    if expro then
        file_extension = file_extension[expro+1..length(file_extension)]
    end if
    expro = equal(file_extension,"pro") 

    -- extract routine info
    cycleThroughAllLines(text,logG)

    if logG then
        if logG=3 then
            updateIncSet(incset)
--  else
----        if logG!=3 then
----        if logG=2 then
--      if rtnmsg=0
--      and sequence(routines[currfile])
----        and length(routines[currfile][1])=length(routinenames)  --NB *WILL* trigger if paste replaces a routine.
--      and not equal(routines[currfile],{routinenames,routinelinenumbers,routineends}) then
--rsfile = open("rs.txt","w")
--puts(rsfile,"{routinenames,routinelinenumbers,routineends}\n")
--ppEx({routinenames,sprint(routinelinenumbers),sprint(routineends)},{pp_File,rsfile})
--puts(rsfile,"\nroutines[currfile]:\n")
--ppEx({routines[currfile][1],sprint(routines[currfile][2]),sprint(routines[currfile][3])},{pp_File,rsfile})
--close(rsfile)
--      if proemh("Warning: Internal error",
--          "routineset wrong (not critical, see rs.txt)",
--          {"Ignore","Crash",MB_DEFBUTTON1})=2 then ?9/0 end if
--
----            rtnmsg=1
--      end if
        end if
--  if target then
        if target and logG=2 then   -- 17/05 Avoid stuffing section info into the routine list!
            routines[target] = {routinenames,routinelinenumbers,routineends}
            if target=currfile then
                setRtnList()
            end if
        end if
    end if
    DBclose()
end procedure

--with trace
global procedure onClickToolQjmp()
sequence lcfile
integer default_view

    -- early out
    if not currfile then
        return
    end if

    if ROUTINEWINDOW=NULL then create_rtnwin() end if
    
--  if isVisible(ROUTINEWINDOW)=False then
    if IupGetInt(ROUTINEWINDOW,"VISIBLE")=0 then

--if usegpp then
        lcfile = filenames[currfile]
--else
--      lcfile = lower(filenames[currfile])
--end if

        if not equal(lcfile,"[untitled]") then

--          setHelpFont(ROUTINELIST)

            initialShow = 1
--          setEnable(TC,False)
--?"setEnable(TC,False)"
--          addFocus(ROUTINEWINDOW)
--?"addFocus(ROUTINEWINDOW)"
--          openWindow(ROUTINEWINDOW, SW_NORMAL)    
            IupShowXY(ROUTINEWINDOW,IUP_CENTERPARENT,IUP_CENTERPARENT)
--(menus get disabled)
--??        call_proc(r_enableMenuToolBar,{})
--?"call_proc(r_enableMenuToolBar,{})"

            -- default to Sections view for non-Euphoria/Phix/js files
            default_view = Sections
--DEV get_file_extension
            for i=length(lcfile) to 1 by -1 do
                if lcfile[i]='.' then
                    lcfile = lcfile[i+1..length(lcfile)]
                    if find(lower(lcfile),{"e","eu","ew","ex","exu","exw","exwc","pro","js"}) then
                        default_view = All
                    end if
                    exit
                end if
            end for

            isErrFile = equal(lcfile,"err")

            -- prepare list of routines names, scope & type (& linenumbers)
            setListOfAllRoutines(currfile,filetext[currfile],1,lcfile)


            -- pretend All or Sections radio just clicked:
            onScreen = {} -- force update!
            clearFilterIfEmpty = 1
--?9/0
--          void = routinelistHandler(default_view,WM_COMMAND,0,1)
--?"routinelistHandler(default_view,WM_COMMAND,0,1)"
            IupSetAttribute(default_view,"VALUE","ON")
            {} = action_cb(default_view, 1)
            clearFilterIfEmpty = 0

            initialShow = 0
--          setFocus(ROUTINELIST)
        end if
    end if
end procedure
--global constant r_onClickToolQjmp=routine_id("onClickToolQjmp")

--
-- Part 2: the help text window.
--
Ihandln HelpWin = NULL
Ihandle HelpList, Hexmncd, HWclose, Hkeybdh
--/*
constant
    HelpWin = create(Window, "", 0, Main, 100, 10, ScreenW-200, ScreenH-90, 0),

    HelpList = create(ListBox, "", 0, HelpWin, 3, 3, 480, 360, LBS_NOINTEGRALHEIGHT),
    HLhwnd = getHwnd(HelpList),
    Hexmncd = create(Button,xl("Examine Code"),0,HelpWin,3,367,106,30,0),
    Hechwnd = getHwnd(Hexmncd),
    HWclose = create(Button,xl("Close"),0,HelpWin,197,367,74,30,0),
    Hwchwnd = getHwnd(HWclose),
    Hkeybdh = create(Button,xl("Keyboard Help"),0,HelpWin,354,367,127,30,0),
    Hkhhwnd = getHwnd(Hkeybdh)
--*/

sequence EurefWord

object KeyHelpText
       KeyHelpText = 0

function help_keys_cb(Ihandle /*HelpWin*/, atom c)
    if c=K_ESC then
        IupHide(HelpWin)
    end if
    return IUP_CONTINUE
end function
constant cb_help_keys = Icallback("help_keys_cb")

function help_close_cb(Ihandle /*HWclose*/)
    IupHide(HelpWin)
    return IUP_CONTINUE
end function
constant cb_help_close = Icallback("help_close_cb")

procedure create_helpwin()
    HelpList = IupList("EXPAND=YES,SIZE=500x250")
--/*
list_1 = IupList();
--  IupSetAttribute(list_1,"EXPAND","YES");
IupSetAttribute(list_1,"VALUE","1");
IupSetAttribute(list_1,"1","Item 1 Text");
IupSetAttribute(list_1,"2","Item 2 Text");
IupSetAttribute(list_1,"3","Item 3 Text");
IupSetAttribute(list_1,"TIP","List 1");
--*/
    Hexmncd = IupButton("Examine code")
    HWclose = IupButton("Close",cb_help_close)
    Hkeybdh = IupButton("Keyboard Help")
    Ihandle buttons = IupHbox({Hexmncd,IupFill(),HWclose,IupFill(),Hkeybdh})
    HelpWin = IupDialog(IupVbox({HelpList,buttons},"MARGIN=5x5,GAP=5"))
    IupSetCallback(HelpWin,"K_ANY",cb_help_keys)
    IupSetAttributeHandle(HelpWin,"PARENTDIALOG",dlg)
    IupSetAttributeHandle(HelpWin,"STARTFOCUS",HWclose)
end procedure

procedure setKeyHelpText()
if not atom(KeyHelpText) then

    if HelpWin=NULL then create_helpwin() end if
--  deleteItem(HelpList,0)  -- empty list
--  IupSetInt(HelpList,"1",NULL) -- empty list
    IupSetAttribute(HelpList,"REMOVEITEM","ALL") -- empty list
    if atom(KeyHelpText) then
?9/0
        KeyHelpText = xl("_F1HELP")
        for i=1 to length(KeyHelpText) do
--DEV I should probably ship these files without tabs... [elng_ENG.txt done]
            if sequence(KeyHelpText[i]) then
--              KeyHelpText[i] = ExpandTabs(KeyHelpText[i])
                KeyHelpText[i] = ConvertTabs(KeyHelpText[i],8,0)
            end if
        end for
    end if
    if HelpWin=NULL then ?9/0 end if
--  void = insertItem(HelpList,KeyHelpText,0)
--  IupSetAttributeId(ROUTINELIST, "", tally, name)
    IupSetAttribute(HelpList, "1", KeyHelpText)
--  setText(HelpWin,xlna("Keyboard Help"))
    IupSetAttribute(HelpWin, "TITLE", "Keyboard Help")
--  setVisible({Hexmncd,Hkeybdh},False)
end if
end procedure

--DEV Phix.chm:
--/*
constant lib="abcdeghoprstuz"
--with trace
constant redirect="<html><head>"&
--"<meta http-equiv=refresh content=\"0;"&
--??
`<meta http-equiv=refresh content="0;`&
--"url=file://localhost/%s/HTML/lib_%s_%s.htm#%s\">"&
`url=file://localhost/%s/HTML/lib_%s_%s.htm#%s">`&
"</head></html>"

procedure EuRefMan(sequence name)
object helpfilepath
sequence redirectfilepath
integer k, fn
--DEV Phix?
    helpfilepath = getenv("EUDIR")
    if atom(helpfilepath) then
?       if not atom(dir(`C:\Euphoria\HTML\Refman.htm`)) then
--      if get_file_type(line)=FILETYPE_FILE then
            helpfilepath = `C:\Euphoria`
        end if
    end if
    if atom(helpfilepath) then
--      void = messageBox("Euphoria Manual not found",
--                        "EUDIR not set", 0 )
        IupMessage("Euphoria Manual not found","EUDIR not set")
    else
        if length(name) then
            for i=2 to length(lib) by 2 do
                if name[1]<=lib[i] then
                    redirectfilepath=helpfilepath&`/html/redirect.htm`
                    while 1 do
                        k = find('\\',redirectfilepath)
                        if k=0 then exit end if
                        redirectfilepath[k] = '/'
                    end while
                    fn = open(redirectfilepath,"w")
                    puts(fn,sprintf(redirect,{helpfilepath,lib[i-1],lib[i],name}))
                    close(fn)
                    helpfilepath = redirectfilepath
                    exit
                end if
            end for
        else
            -- open the HTML version of refman.doc
            helpfilepath &= `\HTML\Refman.htm`
?           if atom(dir(helpfilepath)) then
--          if get_file_type(line)=FILETYPE_FILE then
                void = messageBox("Euphoria Manual not found",
                                  "Euphoria Manual not present in Euphoria directory"&10&13&
                                  helpfilepath, 0 )
                return
            end if
        end if

        void = shellExecute(NULL, "open", helpfilepath, NULL, NULL, SW_SHOWNORMAL)
--  if void=ERROR_FILE_NOT_FOUND then end if --2
    end if
end procedure
global constant r_EuRefMan=routine_id("EuRefMan")
--*/

object targetFile
integer targetLine

sequence text

integer doConvTab

procedure loadHelp(integer idx)
integer rend, start, bcount, tally
    rend = routineends[idx]
    idx = routinelinenumbers[idx]
    targetLine = idx

--  deleteItem(HelpList,0)  -- empty list
--  IupSetInt(HelpList,"1",NULL) -- empty list
    IupSetAttribute(HelpList,"REMOVEITEM","ALL") -- empty list
--  void = insertItem(HelpList,"",0)    -- start with a blank line (DEV, spacing issue)

    start = idx
    for i=idx-1 to 1 by -1 do
        if length(text[i]) then
            if match("--",text[i])!=1 then
                start = i+1
                exit
            end if
        end if
    end for
    bcount = 0
    tally = 0
    for i=start to length(text) do
        if length(text[i]) then
            if match("--",text[i])!=1 then
                if i!=idx then
                    for j=i to rend do
--                      if doConvTab then
--                          void = insertItem(HelpList,ConvertTabs(text[j],8,0),0)
--                      else
--                          void = insertItem(HelpList,ExpandTabs(text[j]),0)
--                      end if
--29/1/18:
--                      string tj = text[j]
                        sequence tj = text[j]
                        if doConvTab then
                            tj = ConvertTabs(tj,8,0)
                        else
                            tj = ExpandTabs(tj)
                        end if
                        tally += 1
--20/12/17:
--                      IupSetAttributeId(ROUTINELIST, "", tally, tj)
--29/1/18:
                        if not string(tj) then tj = utf32_to_utf8(tj) end if
                        IupSetAttributeId(HelpList, "", tally, tj)
                    end for
                    exit
                end if
                for j=1 to length(text[i]) do
                    if text[i][j]='(' then
                        bcount += 1
                    elsif text[i][j]=')' then
                        bcount -= 1
                    end if
                end for
                if bcount!=0 then
                    idx += 1
                end if
            end if
--          if doConvTab then
--              void = insertItem(HelpList,ConvertTabs(text[i],8,0),0)
--          else
--              void = insertItem(HelpList,ExpandTabs(text[i]),0)
--          end if

--3/4/18:
--          string ti = text[i]
            sequence ti = text[i]
            if not string(ti) then ti = utf32_to_utf8(ti) end if

            if doConvTab then
                ti = ConvertTabs(ti,8,0)
            else
                ti = ExpandTabs(ti)
            end if
            tally += 1
--20/12/17:
--          IupSetAttributeId(ROUTINELIST, "", tally, ti)
            IupSetAttributeId(HelpList, "", tally, ti)
        end if
    end for
    text = ""
--20/12/17:
KeyHelpText = ""
end procedure

integer fsIdx
sequence filepathname
object fileset

sequence word
object  Xword,      -- strip leading 'x', eg xOpenFileA -> OpenFileA
        Yword       -- .. and trailing A/W, eg OpenFileA -> OpenFile
integer target

function QuickLoad(sequence filepathname, bool showErr)
sequence text
integer textlen
integer fn
object line
integer linelength
integer lineno

    fn = open(filepathname,"r")
    if fn=-1 then
        if showErr then
--          void = messageBox(xl("Edita: Error opening file"),filepathname,0)
            IupMessage("Edix: Error opening file",filepathname)
        end if
        return ""
    end if
    targetFile = filepathname
    text = repeat(0,200)
    textlen = 200
    lineno = 0
    while 1 do
        line = gets(fn)
        if atom(line) then exit end if
        linelength = length(line)
        lineno += 1
        if lineno>textlen then
            text &= repeat(0,200)
            textlen += 200
        end if
        if line[linelength]='\n' then
            linelength -= 1
            line = line[1..linelength]
        end if
        text[lineno] = line
    end while
    close(fn)
    return text[1..lineno]
end function

--/*
--DEV nicked from xlate.e:
--global 
function getexhname()
sequence exhpath, exhname
--? if tf>=0 then
    if 1 then
--      exhpath = current_dir()&`\lang\`
        exhpath = join_path({initialcurrentdir,"lang"},trailsep:=true)
--      exhname = "ealng_" & tfname & ".exh"
        exhname = "elng_" & tfname & ".exh"
--      if not atom(pdir:dir(exhpath&exhname)) then
--      if not atom(dir(exhpath&exhname)) then
--      if atom(dir(exhpath&exhname)) then
        if get_file_type(join_path(exhpath,exhname)=FILETYPE_FILE then
            return {exhpath,exhname}
        end if
    end if
    return 0
end function
--*/

function loadfsIdx()
--sequence exhname
integer idx
    text = {}
    filepathname = getNameAndDir(fileset[fsIdx],1,1)
--/*
    exhname = getexhname()
    if equal(exhname[2],filepathname[2]) then
--DEV Phix?
        if HelpWin=NULL then ?9/0 end if
--      setText(HelpWin,xl("Euphoria Builtin"))
        IupSetAttribute(HelpWin,"TITLE","Phix builtin")
--      setText(Hexmncd,xl("Reference Manual"))
?9/0
        EurefWord = word
    end if
--*/
    target = 0
--if usegpp then
    for i=1 to length(filepaths) do
        if equal(filepathname,{filepaths[i],filenames[i]}) then
            text = filetext[i]
            targetFile = i
            target = i
            exit
        end if
    end for
--else
--  for i=1 to length(filepaths) do
--      if equal(filepathname,{UpperCase(filepaths[i]),filenames[i]}) then
--          text = filetext[i]
--          targetFile = i
--          target = i
--          exit
------DEV:
----elsif isDebug then
----    pp({filepathname,{filepaths[i],filenames[i]}})
--      end if
--  end for
--end if
    filepathname = filepathname[1]&filepathname[2]
    if not length(text) then
        text = QuickLoad(filepathname,true)
--          if not length(text) then exit end if
        if not length(text) then return 0 end if
        setListOfAllRoutines(target,text,0,filepathname) -- do not update globals
        --DEV:: ?? (seems to work now)
        doConvTab = 1
        --if 0 then
        --          if Xtrans and isEu then     --??
        --              for i=1 to length(text) do
        ----                    text[i] = PackTabs(text[i],0) -- 2.4 bug??
        ----                    tmp = PackTabs(text[i],0)
        --              tmp = ConvertTabs(text[i],8,isTabWidth)
        --              text[i] = tmp
        --              end for
        --          end if
        --end if
    else
        setListOfAllRoutines(target,text,0,filepathname) -- do not update globals
        doConvTab = 0
    end if
    idx = find(word,routinenames)
    if idx!=0 then
        if not length(EurefWord) then
--triggered 20/12/17:
--          if HelpWin=NULL then ?9/0 end if
            if HelpWin=NULL then create_helpwin() end if
--          setText(HelpWin,filepathname&sprintf(" [%d/%d]",{fsIdx,length(fileset)}))
            IupSetStrAttribute(HelpWin,"TITLE",filepathname&" [%d/%d]",{fsIdx,length(fileset)})
        end if
    end if
    return idx
end function

--/*
without trace
function HelpWinHandler(integer id, integer msg, atom wParam, object lParam)
sequence rect
--  if msg=WM_SIZE then
--      rect = getClientRect(HelpWin)
--      void = c_func(xMoveWindow, {HLhwnd, 2, 2, rect[3]-4, rect[4]-40, 1})
--      void = c_func(xMoveWindow, {Hechwnd, 4, rect[4]-34, 126, 30, 1})
--      void = c_func(xMoveWindow, {Hwchwnd, floor(rect[3]/2)-57, rect[4]-34, 94, 30, 1})
--      void = c_func(xMoveWindow, {Hkhhwnd, rect[3]-151, rect[4]-34, 147, 30, 1})
--
--  elsif msg=WM_CTLCOLORLISTBOX and lParam=getHwnd(HelpList) then
--      void = c_func(xSetTextColor, {wParam, ColourTab[Other]})
--      void = c_func(xSetBkMode , {wParam, TRANSPARENT})
--      return {backBrush}
    if msg=WM_COMMAND 
      or (msg=WM_CHAR and wParam=VK_RETURN) then
        if id=Hexmncd then
            if length(EurefWord) then
--DEV Phix.chm(?)
--              EuRefMan(EurefWord)
?9/0
            else
                if sequence(targetFile) then
--                  if openFile(targetFile,1,isLegacyTabHandling)=0 then return 0 end if
                    if openFile(0,targetFile,1)=0 then return 0 end if
                else
                    changeTo(targetFile)
                end if
                CursorY = targetLine-1
                CursorX = 0
                selON = 0
                forceCursorOnscreen()
            end if
            msg = WM_CLOSE
        elsif id=HWclose then
            msg = WM_CLOSE
        elsif id=Hkeybdh then
            setKeyHelpText()
            setFocus(HWclose)
        end if
    elsif find(id,{HWclose,Hexmncd,Hkeybdh})
      and find(msg,{WM_KEYDOWN,WM_KEYUP}) then
        -- allow cursor/page up/down home/end etc when focus on close button:
        void = sendMessage(HelpList,msg,wParam,lParam)
    elsif msg=WM_CHAR then
        if    wParam=5  then                    -- Ctrl E (Enlarge font size)
            EnlargeFontSize(+1)
            setHelpFont(HelpList)
        elsif wParam=18 then                    -- Ctrl R (Reduce font size)
            EnlargeFontSize(-1)
            setHelpFont(HelpList)
        end if
    elsif id=HelpList and msg=WM_KEYDOWN and wParam=VK_F1 and fsIdx then
        fsIdx += 1
        if fsIdx>length(fileset) then
            fsIdx = 1
        end if
        id = loadfsIdx()
        if id then loadHelp(id) end if
    end if
    if (msg=WM_CLOSE)
    or (msg=WM_CHAR and wParam=VK_ESCAPE) then
        removeFocus(HelpWin)
        setVisible({Hexmncd,Hkeybdh},True)
        setVisible(HelpWin, False)
        setText(Hexmncd,xl("Examine Code"))
        -- re-enable tab switching now this is closed.
        setEnable(TC,True)
        setFocus(Main)
        call_proc(r_enableMenuToolBar,{})
    end if

    return 0
end function
setHandler({HelpWin,HelpList,Hexmncd,HWclose},routine_id( "HelpWinHandler" ))
--*/

procedure localSearch(sequence startWord, sequence text)
integer tally = 0
    targetLine = 0
    if length(startWord)
    and (startWord[1]<'0' or startWord[1]>'9') then
        if newSyntax then
            for i=1 to length(WordLists[newSyntax]) do
            -- avoid 'if', 'and' etc
                if find(startWord,WordLists[newSyntax][i]) then return end if
            end for
        end if
--      deleteItem(HelpList,0)  -- empty list
        if HelpWin=NULL then create_helpwin() end if
--      IupSetInt(HelpList,"1",NULL) -- empty list
        IupSetAttribute(HelpList,"REMOVEITEM","ALL") -- empty list
        for i=1 to length(text) do
            if match(startWord,text[i]) then
                if targetLine=0 then
                    targetLine = -i
                elsif targetLine<0 then
                    targetLine = -targetLine
                end if
--              void = insertItem(HelpList,ExpandTabs(text[i]),0)
                tally += 1
--22/2/18:
--              IupSetAttributeId(HelpList, "", tally, ExpandTabs(text[i]))
                sequence line = ExpandTabs(text[i])
                if not string(line) then
                    line = utf32_to_utf8(line)
                end if
                IupSetAttributeId(HelpList, "", tally, line)
            end if
        end for
    end if
end procedure

sequence lastHHname = {}

constant 
--  HH_DISPLAY_TOPIC      = #0000,
  HH_DISPLAY_TOC      = #0001,
  HH_KEYWORD_LOOKUP   = #000D,
  HH_ALINK_LOOKUP     = #0013 
--  HH_CLOSE_ALL          = #0012   -- close all windows opened directly or indirectly by the caller

constant tHH="""
HWND HtmlHelp(
              HWND    hwndCaller,
              LPCSTR  pszFile,
              UINT    uCommand,
              DWORD   dwData) ;"""
integer xHtmlHelp=0

constant tHH_AKLINK="""
typedef struct tagHH_AKLINK {
  int     cbStruct;
  BOOL    fReserved;
  LPCTSTR pszKeywords;
  LPCTSTR pszUrl;
  LPCTSTR pszMsgText;
  LPCTSTR pszMsgTitle;
  LPCTSTR pszWindow;
  BOOL    fIndexOnFail;
} HH_AKLINK;"""
integer idHH_AKLINK

constant tIsWindow="""
BOOL WINAPI IsWindow(
  _In_opt_  HWND hWnd
);"""
integer xIsWindow

constant tGetForegroundWindow="""
HWND WINAPI GetForegroundWindow(void);"""
integer xGetForegroundWindow

constant tRegisterHotKey = """
BOOL WINAPI RegisterHotKey(
  _In_opt_  HWND hWnd,
  _In_      int id,
  _In_      UINT fsModifiers,
  _In_      UINT vk
);"""
integer xRegisterHotKey

constant tUnregisterHotKey = """
BOOL WINAPI UnregisterHotKey(
  _In_opt_  HWND hWnd,
  _In_      int id
);"""
integer xUnregisterHotKey

constant WM_HOTKEY = 0x0312

constant tSetWindowLongPtr = """
LONG_PTR WINAPI SetWindowLongPtr(
  _In_  HWND hWnd,
  _In_  int nIndex,
  _In_  LONG_PTR dwNewLong
);"""
integer xSetWindowLongPtr

constant tCallWindowProc = """
LRESULT WINAPI CallWindowProc(
  _In_  WNDPROC lpPrevWndFunc,
  _In_  HWND hWnd,
  _In_  UINT Msg,
  _In_  WPARAM wParam,
  _In_  LPARAM lParam
);"""
integer xCallWindowProc

constant GWL_WNDPROC = -4          

constant tSendMessage = """
LRESULT WINAPI SendMessage(
  _In_  HWND hWnd,
  _In_  UINT Msg,
  _In_  WPARAM wParam,
  _In_  LPARAM lParam
);"""
integer xSendMessage

procedure initw()
-- windows-only initialisation (for F1 help chm file, esp close on esc)
    xHtmlHelp = define_cffi_func("hhctrl.ocx",tHH)
    idHH_AKLINK = define_struct(tHH_AKLINK)
    xIsWindow = define_cffi_func("user32.dll",tIsWindow)
    xGetForegroundWindow = define_cffi_func("user32.dll",tGetForegroundWindow)
    xRegisterHotKey = define_cffi_func("user32.dll",tRegisterHotKey)
    xUnregisterHotKey = define_cffi_func("user32.dll",tUnregisterHotKey)
    string tSetWindowLong = tSetWindowLongPtr
    if machine_bits()=32 then
        -- YUK: (because C handles this using a stupid macro...)
        tSetWindowLong = substitute(tSetWindowLong,"SetWindowLongPtr","SetWindowLong")
    end if      
    xSetWindowLongPtr = define_cffi_func("user32.dll",tSetWindowLong)
    xCallWindowProc = define_cffi_func("user32.dll",tCallWindowProc)
    xSendMessage = define_cffi_func("user32.dll",tSendMessage)
end procedure

atom chmwnd = NULL

atom lpPrevWndProc = NULL
atom hwnd = NULL

integer isRegistered = 0

Ihandln chmesc = NULL

procedure unregister(bool bStopTimer)
--(common code) [windows only]
    if bStopTimer then
        chmwnd = NULL
        IupSetAttribute(chmesc,"RUN","NO")
    end if
    if isRegistered then
        {} = c_func(xUnregisterHotKey,{hwnd,1})
        isRegistered = 0
    end if
end procedure

function chmesc_cb(Ihandle /*chmesc*/)
-- timer callback (10 times a second) [windows only]
bool bStopTimer = false
    if chmwnd!=NULL then
        if not c_func(xIsWindow,{chmwnd}) then
            bStopTimer = true
        elsif c_func(xGetForegroundWindow,{})=chmwnd then
            if not isRegistered then
                isRegistered = c_func(xRegisterHotKey,{hwnd,1,0,#1B})
            end if
            return IUP_DEFAULT
        end if
    end if
    unregister(bStopTimer)
    return IUP_DEFAULT
end function
constant cb_chmesc = Icallback("chmesc_cb")

constant WM_CLOSE = 0x0010

function winproc(atom hwnd, uMsg, wParam, lParam)
    if uMsg=WM_HOTKEY
    and chmwnd!=NULL
    and c_func(xIsWindow,{chmwnd})
    and c_func(xGetForegroundWindow,{})=chmwnd then
        {} = c_func(xSendMessage,{chmwnd,WM_CLOSE,0,0})
        unregister(true)
        return 0
    end if
    return c_func(xCallWindowProc,{lpPrevWndProc,hwnd,uMsg,wParam,lParam})
end function
constant cb_winproc = call_back(routine_id("winproc"))


global procedure openChm(sequence filename, object word)
--DEV <temp (trying to locate an interpret-only 80000003)>
atom maddr
    #ilASM{
            call :next
           ::next
        [32]
            pop eax
            lea edi,[maddr]
        [64]
            pop rax
            lea rdi,[maddr]
        []
            call :%pStoreMint
          }
--</temp>
    if get_file_extension(filename)!="chm" then ?9/0 end if
    if platform()=WINDOWS then
        if xHtmlHelp=0 then initw() end if
        if chmwnd!=NULL then
            if not c_func(xIsWindow,{chmwnd}) then
                chmwnd = NULL
            elsif not equal(lastHHname,filename) then
                {} = c_func(xSendMessage,{chmwnd,WM_CLOSE,0,0})
                lastHHname = ""
                chmwnd = NULL
            end if
        end if
        atom pChm = IupRawStringPtr(filename)
--15/5/19:
--      if sequence(word) then
        if sequence(word) and length(word) then
            atom ak = allocate_struct(idHH_AKLINK)
            set_struct_field(idHH_AKLINK,ak,"cbStruct",get_struct_size(idHH_AKLINK))
            set_struct_field(idHH_AKLINK,ak,"fReserved",false)
--27/1/18:
            if not string(word) then word = utf32_to_utf8(word) end if
            set_struct_field(idHH_AKLINK,ak,"pszKeywords",IupRawStringPtr(word))
            set_struct_field(idHH_AKLINK,ak,"pszUrl",NULL)
            set_struct_field(idHH_AKLINK,ak,"pszMsgText",NULL)
            set_struct_field(idHH_AKLINK,ak,"pszMsgTitle",NULL)
            set_struct_field(idHH_AKLINK,ak,"pszWindow",NULL)
            set_struct_field(idHH_AKLINK,ak,"fIndexOnFail",true)
            -- show the page we want...
            chmwnd = c_func(xHtmlHelp,{0,pChm,HH_KEYWORD_LOOKUP,ak})
            -- 30/12/13: ...and fill in the index to match
            {} = c_func(xHtmlHelp,{0,pChm,HH_ALINK_LOOKUP,ak})
            free(ak)
        else
            chmwnd = c_func(xHtmlHelp,{0,pChm,HH_DISPLAY_TOC,NULL})
        end if
        lastHHname = filename
        if lpPrevWndProc=NULL then
            hwnd = IupGetAttributePtr(dlg,"HWND")
            lpPrevWndProc = c_func(xSetWindowLongPtr,{hwnd,GWL_WNDPROC,cb_winproc})
            if chmesc=NULL then
                chmesc = IupTimer(cb_chmesc, 100)
            end if
        end if
        IupSetAttribute(chmesc,"RUN","YES")
    else
        ?"9/0 (qj.e line 1665)"
    end if
end procedure

global procedure closeChm()
    if chmwnd!=NULL then
        if c_func(xIsWindow,{chmwnd}) then
            {} = c_func(xSendMessage,{chmwnd,WM_CLOSE,0,0})
        end if
        lastHHname = ""
        unregister(true)
    end if
end procedure


global integer rcX, rcY -- set by TrackMenu (from mouse position),
                        -- and virtualKey (from CursorX,CursorY).
--with trace
global procedure F1help(integer control)
--
-- Show context help.
--
integer idx
object fWord
--, tmp

sequence oneline
integer StartCh, EndCh
integer ch, bracelevel

sequence startWord

integer post500
integer lw

    EurefWord = ""
    startWord = ""
    doConvTab = 0
    fsIdx = 0
    post500 = 1
--20/12/17:
    KeyHelpText = 0

--16/10/10:
--  if currfile and not control and match("[untitled]",filenames[currfile])!=1 then
    if currfile and not control then
        if match("[untitled]",filenames[currfile])!=1 then
            setListOfAllRoutines(currfile,filetext[currfile],2,filenames[currfile]) -- we don't need sections
        else
            routinenames = {}
        end if
        targetFile = currfile

        -- select the word under the cursor (or mouse), if any.
        oneline = ExpandTabs(filetext[currfile][rcY+1])
        if rcX>length(oneline) then
            rcX = CursorX
            oneline = ExpandTabs(filetext[currfile][CursorY+1])
        end if
        EndCh = rcX
--      while EndCh<length(oneline)
--      and wordChar[oneline[EndCh+1]+1]=TokenChar do
        while EndCh<length(oneline) do
            ch = oneline[EndCh+1]
            if ch='.' then exit end if
            if ch<=128 and wordChar[ch+1]!=TokenChar then exit end if
            EndCh += 1
        end while
        StartCh = rcX
        while 1 do
--          while StartCh>0
--          and wordChar[oneline[StartCh]+1]=TokenChar do
            while StartCh>0 do
                ch = oneline[StartCh]
                if ch='.' then exit end if
                if ch<=128 and wordChar[ch+1]!=TokenChar then exit end if
                StartCh -= 1
            end while
            word = oneline[StartCh+1..EndCh]
            if not length(startWord) then
                if not length(word) then
                    fWord = getSelection(SEL_COPY)
                    if sequence(fWord) and length(fWord)=1 then
                        word = fWord[1]
                    end if
                end if
                startWord = word
            end if

--/*
            if Xtrans and isEu then
                fWord = FtoK(word)
--      if atom(fWord) then
                if equal(fWord,word) then
                    fWord = FtoB(word)
                end if
--      if sequence(fWord) then
                word = fWord
--      end if
            end if
--*/
            Xword = 0
            lw = length(word)
            if lw and word[1]='x' then
                Xword = word[2..lw]
                lw -= 1
                Yword = Xword
            elsif lw>4 and equal(word[1..4],"IID_") then
                Xword = word[5..lw]
                lw -= 4
                Yword = Xword
            elsif lw>6 and equal(word[1..6],"CLSID_") then
                Xword = "I"&word[7..lw]
                lw -= 6
                Yword = Xword
            else
                Yword = word
            end if
            if lw and find(Yword[lw],"AW") then
                lw -= 1
--7/7/2013:
--              if lw and word[lw]>='a' and word[lw]<='z' then
--30/12/13:
--              if lw and Yword[lw]>='a' and Yword[lw]<='z' then
                if lw then
                    Yword = Yword[1..lw]
                else
                    Yword = 0
                end if
            else
                Yword = 0
            end if
--?{word,Xword,Yword}
            for i=1 to length(helpnames) do
                if compare("_",helpnames[i])<0 then exit end if
                if sequence(F1lists[i]) then
                    if find(word,F1lists[i]) then
                        openChm(helpfiles[i],word)
                        return
                    elsif sequence(Xword) and find(Xword,F1lists[i]) then
                        openChm(helpfiles[i],Xword)
                        return
                    elsif sequence(Yword) and find(Yword,F1lists[i]) then
                        openChm(helpfiles[i],Yword)
                        return
                    end if
                end if
                post500 = i+1
            end for
            idx = find(word,routinenames) -- local first
            if idx=0 then
                while 1 do
                    fileset = findGlobal(word)
                    if not sequence(fileset) then exit end if
                    if not length(fileset) then exit end if
                    fsIdx = 1
                    if length(fileset)>1 then
                        -- look for anything known to be part of this project
                        for i=1 to length(fileset) do
                            filepathname = getNameAndDir(fileset[i],1,1)
                            if inProjectSet(filepathname[1],filepathname[2],currProjFileSet) then
                                fsIdx = i
                                fileset[1] = fileset[i]
                                exit
                            end if
                        end for
                    end if
                    idx = loadfsIdx()
                    if idx != 0 then exit end if
--                  void = proemh("Warning","global database lied",0)
?{"proemh","Warning","global database lied",0}
                    ScratchGlobal(word,fileset[1])
                end while
            else
                text = filetext[currfile]
--              setText(HelpWin,xl("Locally defined in this source"))
--              if HelpWin=NULL then ?9/0 end if
                if HelpWin=NULL then create_helpwin() end if
                IupSetAttribute(HelpWin,"TITLE","Locally defined in this source")
                if idx and routinelinenumbers[idx]=CursorY+1
                and routinescope[idx]=GLOBAL
                and find(filenames[currfile],{"database.e","dll.e","file.e","get.e","graphics.e",
                                              "image.e","machine.e","misc.e","mouse.e","msgbox.e",
                                              "safe.e","sort.e","wildcard.e"}) then
?9/0
--                  setText(Hexmncd,xl("Reference Manual"))
                    EurefWord = word
                end if
            end if
            if idx then
                loadHelp(idx)
                exit
            end if
            --
            -- look in any remaining user defined help files:
            --
            for i=post500 to length(helpnames) do
                if sequence(F1lists[i]) then
                    if find(word,F1lists[i]) then
                        openChm(helpfiles[i],word)
                        return
                    elsif sequence(Xword) and find(Xword,F1lists[i]) then
                        openChm(helpfiles[i],Xword)
                        return
--30/12/13:
                    elsif sequence(Yword) and find(Yword,F1lists[i]) then
                        openChm(helpfiles[i],Yword)
                        return
                    end if
                end if
            end for
            --
            -- Scan back looking for surrounding function then.
            --
            bracelevel = 1
            while StartCh do
                ch = oneline[StartCh]
                if find(ch,")}]") then
                    bracelevel += 1
--22/7/15:
--              elsif find(ch,"({]") then
                elsif find(ch,"({[") then
                    bracelevel -= 1
                elsif bracelevel=0 then
                    ch = oneline[StartCh]
                    if ch<=128 and ch!='.' and wordChar[ch+1]=TokenChar then exit end if
                end if
                StartCh -= 1
            end while
            if StartCh=0 then
                localSearch(startWord,filetext[currfile])
                if targetLine > 0 then
                    if HelpWin=NULL then ?9/0 end if
--                  setText(HelpWin,xl("Local definition/uses of ")&startWord&xl(" in this source"))
--11/3/18:
                    if not string(startWord) then startWord = utf32_to_utf8(startWord) end if
                    IupSetStrAttribute(HelpWin,"TITLE","Local definition/uses of "&startWord&" in this source")
                else
                    setKeyHelpText()
                end if
                exit
            end if
            EndCh = StartCh
            post500 = 1
        end while
    else
        setKeyHelpText()
    end if
if atom(KeyHelpText) then
        keybd:keyboard_dialog()
else
    if HelpWin=NULL then ?9/0 end if
--  setHelpFont(HelpList)
--  setEnable(TC,False)
--?"setEnable(TC,False)"
--  addFocus(HelpWin)
--  openWindow(HelpWin,SW_NORMAL)
--  IupShow(HelpWin)
--DEV see keyboard.e and/or filelist.e for config stuff...
    IupShowXY(HelpWin,IUP_CENTERPARENT,IUP_CENTERPARENT)
--  call_proc(r_enableMenuToolBar,{})
--?"call_proc(r_enableMenuToolBar,{})"
--  setFocus(HWclose)
--?"setFocus(HWclose)"
end if
end procedure
global constant r_F1help=routine_id("F1help")

--ppOpt({pp_Pause,0})

--with trace
global procedure backGroundProcessing()
-- trawl through the database looking for things to update
-- process at most one file at a time.
-- do not process any file which is currently open and modified.
?"backGroundProcessing"
--/*
object fkey
sequence sdt, text
--atom t
--?1
--t=time()

--  if ROUTINEWINDOW=NULL then create_rtnwin() end if
    if ROUTINEWINDOW=NULL then ?9/0 end if

    if isVisible(ROUTINEWINDOW)=False then
        fkey = getModifiedFile()
    else
        fkey = -1
    end if
--setText(Main,sprint(time()-t))    -- 9 seconds!!!
--pp(fkey)
    if sequence(fkey) then
--trace(1)
        sdt = fkey[2]
        fkey = fkey[1]
        --if equal(fkey,
        --  {
        --       `C:\POSITIVE\`,
        --       "t3.exw"
        --     }
        --) then trace(1) end if

        if isShowBGprogress then
            setText(SB6,sprintf("Bg: globals:%s",fkey[2..2]))
        end if
        if bgIdx then
            text = filetext[bgIdx]
        else
            text = QuickLoad(fkey[1]&fkey[2],false)
        end if
        initForGlobalLogging(fkey[1],fkey[2])
        gsize = sdt[1]
        gdate = sdt[2..length(sdt)]

        setListOfAllRoutines(bgIdx,text, 3,fkey[2])
    elsif fkey=-1 then  -- all done
        DBclose()
        runBack = 0 -- all done
        if isShowBGprogress then
            setText(SB6,"")
        end if
    end if
--*/
end procedure
