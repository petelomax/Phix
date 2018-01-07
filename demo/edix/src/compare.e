--
-- compare.e    File comparison.
--
-- compare directories should be a new approach, list files with Y/N (already done or identical), size/date,
--  sorted to get Y top and the first auto-selected, and an OK button to run this (and set the Y to N).
--  obviously the OK button would be disabled if no N, and the list empty if src and dest dirs match.
--  (all the Y would revert to N on next edit session, then again so would dest dir)

without trace

--/* 4.0.2
include builtins\file.e
--include builtins\wildcard.e
--*/
--!/**/include builtins\pcase.e as pcase

include plan9.e
--

--DEV shelved for now, probably crib find.e to start with...

--/*
constant
    cwin       = create(Window, "",                      0, Main, 100, 100, 540, 151, {0,WS_EX_TOOLWINDOW}),
    dlab       = create(Label,xl("Directory"),           0, cwin,   8,  15, 110,  22, SS_RIGHT),
    directory  = create(ComboDropDown, "",               0, cwin, 125,  11, 300, 124, 0),
    flab       = create(Label,xl("Filename"),            0, cwin,   8,  42, 110,  22, SS_RIGHT),
    filename   = create(EditText, "",                    0, cwin, 125,  40, 300,  24, 0),
    ignores    = create(CheckBox,xl("Ignore Whitespace"),0, cwin,   8,  65, 196,  20, 0),
    Overwrite  = create(CheckBox,xl("Overwrite?"),       0, cwin,   8,  89, 170,  24, 0),
--  Rename     = create(CheckBox,xl("Rename?"),          0, cwin, 191,  89, 170,  24, 0),
    Browse     = create(Button,xl("Browse"),             0, cwin, 430,  40, 90,  25, 0),
    CompareBtn = create(Button,xl("Compare"),            0, cwin, 430,  85, 90,  25, 0)

setCheck(ignores,True)
setCheck(Overwrite,False)

constant PROGRESS = create(Window,xl("Progress"),      0, Main,     200, 300, 400, 210, 0)
         void     = create(Label,xl("Width"),          0, PROGRESS,   8,  15,  80,  24, 0)
constant wdth     = create(Label, "",                  0, PROGRESS, 100,  15, 260,  24, 0)
         void     = create(Label,xl("Stretch"),        0, PROGRESS,   8,  45,  80,  24, 0)
constant pbar     = create(ProgressBar,"",             0, PROGRESS, 100,  40, 260,  24, PBS_SMOOTH)
         void     = create(Label,xl("Complexity"),     0, PROGRESS,   8,  75,  80,  24, 0)
constant complxt  = create(Label, "",                  0, PROGRESS, 100,  75,  60,  24, 0)
         void     = create(Label,xl("Edit Distance"),  0, PROGRESS,   8, 105,  80,  24, 0)
constant edstnce  = create(Label, "",                  0, PROGRESS, 100, 105,  60,  24, 0)
constant CANCEL   = create(PushButton,xl("Cancel"),    0, PROGRESS, 160, 145,  80,  25, 0)

constant RESULTS  = create(Window,xl("Finished"),      0, cwin, 500, 400, 305, 160, 0)
constant rText    = create(Label,   "",                0, RESULTS,    8,  15, 180,  24, 0)
constant rOverite = create(CheckBox,xl("Overwrite?"),  0, RESULTS,    8,  40,  80,  24, 0)
--constant rRename  = create(CheckBox,xl("Rename?"),       0, RESULTS,  135,  40,  80,  24, 0)
constant rOK      = create(PushButton,xl("OK"),        0, RESULTS,  110,  80,  80,  25, 0)



without trace
function pHandler(integer id, integer msg, atom wParam, object lParam)
    if wParam or object(lParam) then end if -- suppress warnings
    if msg=WM_TIMER then
--      addFocus(PROGRESS)
        setVisible(PROGRESS,True)
        setText(wdth,sprintf("%d",width))
        setPos(pbar, 100*stretch/maxstretch)
        setText(complxt,sprintf("%d",complexity))
        setText(edstnce,sprintf("%d",editdistance))
    elsif msg=WM_COMMAND and id=CANCEL then
        terminate = 1
    end if
    return 0
end function
constant TIMER = createTimer()

sequence samefile
         samefile = {}

procedure setFocusBC()
--
-- Set the focus to Browse or Compare depending on the values set in the
-- window fields and whether or not a file exists, etc.
--
    string filepath = join_path({getText(directory),getText(filename)})
    if equal(samefile,filepath)
--  or atom(dir(filepath)) then
    or get_file_type(filepath)!=FILETYPE_FILE then
        setFocus(Browse)
    else
        setFocus(CompareBtn)
    end if
end procedure

integer actuallyChangeTo
        actuallyChangeTo = 0

function CompareTarget(integer c)
-- if a file tab is clicked in the background, and it is not the same as the
-- source file, set it as the target.
    if c then
        if isVisible(cwin) and not actuallyChangeTo then
            setText(directory,filepaths[c])
            setText(filename,filenames[c])
            setFocusBC()
            return 1
        end if
    end if
    actuallyChangeTo = 0
    return 0
end function
rCompareTarget = routine_id("CompareTarget")


-- used by eaxutil.ew:
--global 
function outdiff(sequence x, sequence a, sequence b)
-- result is in CRLF format, placed on the clipboard
-- (a fraction slower, but it makes undo work as we want)
integer prevx, prevy, prevdiag, thisx, thisy, diagresult
sequence text

    prevx = 1
    prevy = 1
    prevdiag = 0
    text = {}
    for i=1 to length(x) by 2 do
        thisx = x[i]
        thisy = x[i+1]
        diagresult = thisx-thisy
        if diagresult<prevdiag then
            text = append(text,"<"&b[prevy])
            prevy += 1
        end if
        if diagresult>prevdiag then
            text = append(text,">"&a[prevx])
            prevx += 1
        end if
        for j=prevx to thisx-1 do
            text = append(text,a[j])
            prevy += 1
        end for
        prevx = thisx
        prevy = thisy
        prevdiag = diagresult
    end for
    return text
end function

procedure updateDirList()
sequence text
integer k
    text = getText(directory)
    if length(text) then
        k = getCount(directory)
        while k do
            if compare(text,getItem(directory,k))=0 then exit end if
            k -= 1
        end while
        if k=0 then
            void = insertItem(directory,text,0)
        end if
    end if
end procedure

procedure derror(integer i)
    setHandler(PROGRESS,-1)
    stopTimer(TIMER)
    removeFocus(PROGRESS)
    setVisible(PROGRESS,False)
    void = messageBox("Error",sprintf("Error opening indicated file(%d).",i),MB_OK)
    setFocusBC()
    return
end procedure

integer changes
sequence a, b, result
integer notpathmod  -- set to zero to preserve overidden pathname (whole session)
        notpathmod = 1

integer initialfile, lastfile, justopenedfile, NumberOfFilesInitiallyOpen
    lastfile = 0
    NumberOfFilesInitiallyOpen = 0

procedure onclickOpen()
    if justopenedfile then
        if justopenedfile>NumberOfFilesInitiallyOpen then
            if justopenedfile!=currfile then
                actuallyChangeTo = 1
                changeTo(justopenedfile)
            end if
            closeTab()
        end if
        justopenedfile = 0
    end if
    lastfile = currfile
    string filepath = join_path({getText(directory),getText(filename)})
--  void = openFile(filepath,1,isLegacyTabHandling)
    void = openFile(0,filepath,1)
    if lastfile!=currfile then  -- file successfully opened
        justopenedfile = currfile
    end if
end procedure

function IgnoreWhitespace(sequence lines)
integer k, ch, wasSpace
sequence linei, newline
    for i=1 to length(lines) do
        linei = lines[i]
        k = find('\t',linei)
        if k=0 then
            k = match("  ",linei)
        end if
        if k then
            wasSpace = 0
            newline = repeat(' ',length(linei))
            k = 0
            for j=1 to length(linei) do
                ch = linei[j]
                if find(ch," \t") then
                    if not wasSpace then
                        k += 1
                        newline[k] = ' '
                    end if
                    wasSpace = 1
                else
                    k += 1
                    newline[k] = ch
                    wasSpace = 0
                end if
            end for
            lines[i] = newline[1..k-wasSpace]
        end if
    end for
    return lines
end function


procedure onclickCompare()
sequence x
    if not justopenedfile then
        if not equal(getText(directory),filepaths[currfile])
        or not equal(getText(filename),filenames[currfile]) then
            onclickOpen()
        else
            void = messageBox(xl("Error"),
                              xl("Cannot compare file with itself"),
                              MB_OK)
            setFocusBC()
            return
        end if
    end if
    if justopenedfile then
        --#
        --# If the user has changed the directory, set a flag to preserve it
        --#
        actuallyChangeTo = 1
        changeTo(initialfile)
        if compare(getText(directory),filepaths[currfile])!=0 then
            notpathmod = 0
        end if
        updateDirList()
        terminate = 0
        --
        --# run the file comparison on analysed lines
        --
        a = filetext[currfile]

        b = filetext[justopenedfile]

        startTimer(TIMER, PROGRESS, 1000) -- one full second
        setScrollInfo(pbar, {1, 100}, 0)

        setHandler(PROGRESS,routine_id("pHandler"))


        if isChecked(ignores) then
            -- Note that a & b are NOT altered, they are passed to outdiff later.
            result = diff(IgnoreWhitespace(a),IgnoreWhitespace(b))
        else
            result = diff(a,b)
        end if

        setHandler(PROGRESS,-1)
        stopTimer(TIMER)
        removeFocus(PROGRESS)
        setVisible(PROGRESS,False)


        if not terminate then
            changes = length(result)/2-1
            setText(rText,sprintf(xl("%d differences found."),{changes}))
            if changes then
                setEnable(rOverite,True)
            else
                setEnable(rOverite,False)
                setCheck(Overwrite,False)
            end if
            setCheck(rOverite,isChecked(Overwrite))
--          addFocus(RESULTS)
            openWindow(RESULTS,SW_NORMAL)
            setFocus(rOK)
            -- remainder of processing in onCloseRESULTS()
        else

            void = messageBox(xl("Interrupted"),
                              xl("Comparison interrupted."),
                              MB_OK)
        end if
    else
        void = messageBox(xl("Error"),
                          xl("File not opened to compare against!"),
                          MB_ICONEXCLAMATION+MB_OK)
        setFocusBC()
    end if
end procedure

procedure onCloseRESULTS()
sequence old_name, new_name
sequence ext
integer linelength

--  ext = getFileExtension(filenames[justopenedfile])
    ext = get_file_extension(filenames[justopenedfile])

    if justopenedfile>NumberOfFilesInitiallyOpen then
        if justopenedfile!=currfile then
            actuallyChangeTo = 1
            changeTo(justopenedfile)
        end if
        closeTab()
    end if
    justopenedfile = 0
    if changes then
        result = outdiff(result,a,b)
        if isChecked(Overwrite) then
            actuallyChangeTo = 1
            changeTo(initialfile)
        else
--          if not equal(ext,getFileExtension(filenames[initialfile])) then
            if not equal(ext,get_file_extension(filenames[initialfile])) then
                ext = ""
            else
                ext = '.'&ext
            end if
            newFile(ext,{""},{1},0)
        end if
        --
        -- lastly, build a new linelengths table for edita
        --
        linelengths = repeat(0,80)
        for i=1 to length(result) do
            linelength = ExpLength(result[i])
            if linelength>=length(linelengths) then
                linelengths &= repeat(0,linelength-length(linelengths)+1)
            end if
            linelengths[linelength+1] = linelengths[linelength+1]+1
        end for
        filelinelengths[currfile] = linelengths
        filetext[currfile] = result
        bookmarks[currfile] = repeat(0,length(result))
        actions[currfile] = {}
        actionptr[currfile] = 0
        actionsave[currfile] = 0
        unpacked[currfile] = -1

        TopLine = 0
        CursorY = 0
        CursorX = 0
        selON = 0
        forceCursorOnscreen()
        paintall()

    end if
end procedure



without trace
function cHandler(integer id, integer msg, atom wParam, object lParam)

    if wParam or object(lParam) then end if
--?{id,msg,wParam,lParam}
--?{id,msg,wParam,lParam,WM_NOTIFY,TCN_SELCHANGE}
    if msg=WM_CHAR then
        if wParam=VK_RETURN then
            if isVisible(RESULTS) then
                id = rOK
            elsif id!=Browse then
                id = CompareBtn
            end if
            msg = WM_COMMAND
        elsif wParam=VK_ESCAPE then
            if isVisible(RESULTS) then
                removeFocus(RESULTS)
                closeWindow(RESULTS)
            else
                removeFocus(cwin)
                setVisible(cwin,False)
                setFocus(Main)
            end if
        end if
    end if

    if msg=WM_KILLFOCUS and id=directory+1 then
        if sendMessage(directory,CB_GETDROPPEDSTATE,0,0) then
            void = sendMessage(directory,CB_SHOWDROPDOWN,0,0)
        end if
    end if
    if msg=WM_COMMAND then
        if find(id,{Overwrite,rOverite}) then
            if lParam=1 then    -- accelerator key
                setCheck(id,not isChecked(id))
            else                -- space bar or mouse click
                setCheck(id,isChecked(id))
            end if
            if find(id,{Overwrite}) then
                setFocusBC()
            elsif id=rOverite then
                setCheck(Overwrite,isChecked(rOverite))
                setFocus(rOK)
            end if
        elsif id=Browse then
            lParam = getOpenFileName(Main, getText(directory), {xl("All Files"), "*.*"}, 0, NULL)
            if sequence(lParam) and length(lParam)=1 then
                lParam = extractPathAndName(lParam[1])
                setText(directory,lParam[1])
                setText(filename,lParam[2])
            end if
            setFocusBC()
        elsif id=CompareBtn then
            removeFocus(cwin)
            setVisible(cwin,False)
            onclickCompare()
            setFocus(Main)
        elsif id=dlab then
            setFocus(directory)
----DEV ???
--      elsif id=directory+1 then
--          getFirstTarget()
        elsif id=flab then
            setFocus(filename)
        elsif id=rOK then
            removeFocus(RESULTS)
            closeWindow(RESULTS)
        end if
    end if
    if msg=WM_CLOSE then
        if id=RESULTS then
            onCloseRESULTS()
        else
            removeFocus(cwin)
            setVisible(cwin,False)
            setFocus(Main)
        end if
    end if
    return 0
end function
setHandler({cwin,directory,directory+1,filename,filename+1,
            Overwrite,Browse,CompareBtn,
            RESULTS,rOverite,rOK},routine_id("cHandler"))


procedure Compare()
sequence dfile
    if currfile then
--      setVisible(filename,true)
--      setVisible(filentxt,false)
        dfile = filenames[currfile]
        setText(cwin,sprintf("Compare  %s%s  with",{filepaths[currfile],dfile}))
        if notpathmod then
            setText(directory,filepaths[currfile])
            updateDirList()
        end if
        NumberOfFilesInitiallyOpen = length(filenames)
        samefile = filepaths[currfile]&filenames[currfile]
        initialfile = currfile
        setText(filename,filenames[currfile])
        justopenedfile = 0

        width = 0
        stretch = 0
        maxstretch = 1
        complexity = 0
        editdistance = 0

--      addFocus(cwin)
        openWindow(cwin,SW_NORMAL)
        setFocusBC()
    end if
end procedure
global constant r_Compare = routine_id("Compare")
--*/
