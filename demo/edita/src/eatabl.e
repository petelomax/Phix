--
-- eatabl.e
--
--  tabsList function.
--  Author: Pete Lomax
--
--  When you have a large number of files open, they can get lost either off 
--  the side (single line tabs option) or lost in the crowd (multiline tabs).
--  This program displays all open files in a listview, allowing sorting by
--  directory, name, extension, last mod date, size, and using a filter.
--
--  Invoked via File/Tabs List or Ctrl T.
--
-- 01/05/07 Hijacked to list previous pFTP edits....
--
without trace

--include builtins\ppp.e
--      ppOpt({pp_Maxlen,9999999})  -- stop ppf() putting \n in results.

constant TLwin=create(Window,xl("Tabs List"),0,Main,20,20,690,368,0)
constant LVstyle=or_all({LVS_OWNERDATA, LVS_SINGLESEL, LVS_SHOWSELALWAYS})
constant LVxstyle=or_all({LVS_EX_FULLROWSELECT,LVS_EX_GRIDLINES,LVS_EX_HEADERDRAGDROP})
constant TLlvw = create(ListView,
                        {{xlna("File"),160},
                         {xlna("ext"),40},
                         {xlna("Directory"),244},
                         {xlna("Size"),60,LVCFMT_RIGHT},
                         {xl("Last modified"),140,LVCFMT_CENTER},
                         {xl("rec"),30,LVCFMT_CENTER}},
                        0,TLwin,2,44,596,660,LVstyle)

    void = sendMessage(TLlvw, LVM_SETEXTENDEDLISTVIEWSTYLE, 0,LVxstyle)

constant TLhelp = create(Button,xl("Help"),0,TLwin,0,0,0,0,0)
constant TLmc = create(CheckBox,xl("Master Copy"),0,TLwin,0,0,0,0,0)
constant TLif = create(CheckBox,xl("isFTP"),0,TLwin,0,0,0,0,0)
constant TLfill = create(Label,xl("Filter"),0,TLwin,0,0,0,0,SS_RIGHT)
constant TLfilt = create(EditText,"",0,TLwin,0,0,0,0,0)
constant TLok   = create(Button,xl("OK"),0,TLwin,0,0,0,0,0)
constant TLcncl = create(Button,xl("Cancel"),0,TLwin,0,0,0,0,0)


constant TABSLIST=1, PREVFTP=2
integer action  -- one of the above

sequence files
         files = {}

sequence filessave  -- save of files when action=PREVFTP

constant F_name=1,
--       F_ext=2,   -- constant not actually used (but column is visible/valid)
         F_dir=3,
         F_size=4,
         F_date=5,
         F_rec=6

sequence fmap   -- 'real' indexes after filtering effects

function Lower(object o)
    if sequence(o) then
        for i=1 to length(o) do
            o[i]=Lower(o[i])
        end for
        return o
    end if
    if integer(o) and o>='A' and o<='Z' then
        o = o + ('a'-'A')
    end if
    return o
end function

procedure setFilters(integer clr)
-- clr is 1 if the filter should be cleared when all get filtered
sequence ftxt
integer k
    ftxt = Lower(getText(TLfilt))
    fmap = repeat(0,length(files))
    k=1
    for i=1 to length(fmap) do
        if length(ftxt)=0
--16/2/15 (a filter of "test\list.asm" was failing to match anything)
--      or match(ftxt,Lower(files[i][F_name]))
--      or match(ftxt,Lower(files[i][F_dir])) then
        or match(ftxt,Lower(files[i][F_dir]&files[i][F_name])) then
            fmap[k]=i
            k+=1
        end if
    end for
    if k=1 and clr then
        setText(TLfilt,"")
        k=length(fmap)+1
        for i=1 to k-1 do
            fmap[i]=i
        end for
    end if
    setLVItemCount(TLlvw,k-1)
end procedure

procedure select()
--object r
integer k,k2
    k = getIndex(TLlvw)
    if k then
        k = fmap[k]
        if action=PREVFTP then
            if not isChecked(TLmc) then
                FTPdownload(files[k][F_dir]&files[k][F_name])
                -- pFTP will yell back when done to start the edit.
            else
                void = openFile(files[k][F_dir]&files[k][F_name],1,isLegacyTabHandling)
                if not isChecked(TLif) then
                    isFTP=0
                    markAsFTP()
                    files=files[1..k-1]&files[k+1..length(files)]
                    setFilters(0)
                    setCheck(TLmc,False)
                    setCheck(TLif,True)
                    setEnable(TLif,False)
                    return
                end if
            end if
        else
            void = openFile(files[k][F_dir]&files[k][F_name],1,isLegacyTabHandling)
        end if
--?k
--  r = getLVItem(TLlvw,0)
--?r
--  if not equal(r,-1) then
--      ?r
--pp(files[k])
        k = files[k][F_rec]
--?k
        for i=1 to length(files) do
            k2=files[i][F_rec]
            if k2=k then
                files[i][F_rec]=1
            elsif k2<k then
                files[i][F_rec]=k2+1
            end if
        end for
--      void = sendMessage(TLlvw, LVM_UPDATE, 0, 0)

        closeWindow(TLwin)
    end if
end procedure

function flattentxt(sequence txtlines)
sequence res
    res = ""
    for i=1 to length(txtlines) do
        res&=txtlines[i]&'\n'
    end for
    return res
end function

procedure showTLhelp()
    void = messageBox(xl("Tabs List Window"),flattentxt(xl("_TLHELP")),0)
    setFocus(TLlvw)
end procedure

integer SortColumn, Sign

function CompareSortColumn(sequence s1,sequence s2)
    if SortColumn>length(s1) then return -Sign end if
    if SortColumn>length(s2) then return  Sign end if
    if SortColumn<=2 then
        return Sign*compare(Lower(s1[SortColumn]),Lower(s2[SortColumn]))
    end if
    return Sign*compare(s1[SortColumn],s2[SortColumn])
end function
constant rCSC = routine_id("CompareSortColumn")

integer switchDBfocus
        switchDBfocus=0


include builtins\file.e
function TLlvwFormat(integer lRow, integer lColumn)
object d
integer tmp

    if lRow>length(files) then return "??" end if
    lRow=fmap[lRow]
    if lColumn=0 then   -- no icon
        return 0
    elsif lColumn<=F_dir then
        return files[lRow][lColumn]
    else
        d = files[lRow][F_date]
--bugfix 3/2/2012. Size was not being updated, possibly date too...
--      if equal(d,-1) then
            d = dir(files[lRow][F_dir]&files[lRow][F_name])
-- 30/12/13:
--          if atom(d) then
            if atom(d) or length(d)=0 then
                if action=PREVFTP then
                    --DEV not here, but setEnable(TLmc,TLeu)
                end if
                return "???"
            end if
            d = d[1]
            files[lRow][F_size]=d[3] -- size
            d = d[4..9]
            files[lRow][F_date] = d
--      end if
        if lColumn=F_date then
            --DEV format date as per status bar?
            tmp=d[1] d[1]=d[3] d[3]=tmp -- swap day and year
            return sprintf("%02d\\%02d\\%04d %02d:%02d:%02d",d)
        else
            return sprintf("%d",files[lRow][lColumn])
        end if
    end if
end function
setLVFormatRoutine(TLlvw, routine_id("TLlvwFormat"))

sequence c0 c0={}
integer ctot
function TLwinHandler(integer id, integer msg, atom wParam, object lParam)
sequence rect, ftxt
integer c1, c2, cnew, crem

    if msg = WM_CHAR then
        if wParam = VK_ESCAPE
        or (id = TLcncl and wParam = VK_RETURN) then
            if id = TLfilt then
                setFocus(TLlvw)
            else
                removeFocus(TLwin)
                closeWindow(TLwin)
            end if
        elsif wParam = VK_RETURN then
            if id = TLhelp then
                showTLhelp()
            elsif id = TLfilt then
                setFocus(TLlvw)
            else
                select()
            end if
        elsif id!=TLfilt then
            if wParam>=' ' and wParam<='z' then
                setText(TLfilt,Lower(getText(TLfilt)&wParam))
                setFilters(0)
            elsif wParam=VK_BACK then
                ftxt = getText(TLfilt)
                if length(ftxt) then
                    setText(TLfilt,ftxt[1..length(ftxt)-1])
                    setFilters(0)
                end if
            end if
        end if
    elsif id!=TLfilt and msg=WM_KEYDOWN and wParam=VK_DELETE then
        ftxt = getText(TLfilt)
        if length(ftxt) then
            setText(TLfilt,ftxt[1..length(ftxt)-1])
            setFilters(0)
        end if
    elsif id = TLlvw then
        if msg = WM_NOTIFY then
            if wParam = LVN_COLUMNCLICK then
                SortColumn = lParam[1]
                Sign = lParam[2]
                files = custom_sort(rCSC,files)
                setFilters(1)
                void = sendMessage(id, LVM_UPDATE, 0, 0)
            end if
        elsif msg=WM_LBUTTONDBLCLK then
            select()
        elsif msg=WM_VSCROLL then   -- added 20/2/2011
            repaintWindow(id,False)
        end if
    elsif msg = WM_SIZE then
        rect=getClientRect(TLwin)
        void=c_func( xMoveWindow, {getHwnd(TLlvw),  rect[1]+2,  rect[2]+2, rect[3]-4, rect[4]-39, 1} )
        if action=TABSLIST then
            void=c_func( xMoveWindow, {getHwnd(TLhelp), rect[1]+5,  rect[4]-30, 60,25,1})
            void=c_func( xMoveWindow, {getHwnd(TLfill), rect[1]+70, rect[4]-25, 35,25,1})
            void=c_func( xMoveWindow, {getHwnd(TLfilt), rect[1]+110,rect[4]-30, rect[3]-rect[1]-275,25,1})
        else
            void=c_func( xMoveWindow, {getHwnd(TLmc),   rect[1]+5,  rect[4]-35, 85,35,1})
            void=c_func( xMoveWindow, {getHwnd(TLif),   rect[1]+95, rect[4]-35, 85,35,1})
            void=c_func( xMoveWindow, {getHwnd(TLfill), rect[1]+180, rect[4]-25, 35,25,1})
            void=c_func( xMoveWindow, {getHwnd(TLfilt), rect[1]+220,rect[4]-30, rect[3]-rect[1]-375,25,1})
        end if
        void=c_func( xMoveWindow, {getHwnd(TLok),   rect[3]-130,rect[4]-30, 60,25,1})
        void=c_func( xMoveWindow, {getHwnd(TLcncl), rect[3]-65, rect[4]-30, 60,25,1})
        --
        -- Resize the columns to fit, proportionally, leaving 29 pixels for 
        -- the vertical scrollbar (which may not be present)
        --
        if not getKeyState(VK_MENU) then    -- Alt key not depressed
            if not length(c0) then
                c0=repeat(0,6)
                ctot=0
                for i=1 to length(c0) do
                    c1=sendMessage(TLlvw,LVM_GETCOLUMNWIDTH,i-1,0)
                    c0[i]=c1
                    ctot+=c1
                end for
            end if
            cnew = rect[3]-8-13
            crem = ctot
            void = c_func(xSendMessage, {getHwnd(TLlvw), WM_SETREDRAW, 0, 0} )
            for i=1 to length(c0) do
                c1=c0[i]
                c2=floor((c1/crem)*cnew)
                void=sendMessage(TLlvw,LVM_SETCOLUMNWIDTH,i-1,c2)
                crem-=c1
                cnew-=c2
            end for
            void = c_func(xSendMessage, {getHwnd(TLlvw), WM_SETREDRAW, 1, 0} )
        end if
    end if
    if msg = WM_COMMAND then
        if id = TLhelp then
            showTLhelp()
        elsif id = TLmc then
            setCheck(TLif,True)
            setEnable(TLif,isChecked(TLmc))
--constant TLmc = create(Checkbox,xl("Master Copy"),0,TLwin,0,0,0,0,0)
--constant TLif = create(Checkbox,xl("Auto Upload"),0,TLwin,0,0,0,0,0)
--
        elsif id = TLfill then
            setFocus(TLfilt)
        elsif id = TLfilt then
            setFilters(0)
        elsif id = TLok then
            select()
        elsif id = TLcncl then
            removeFocus(TLwin)
            closeWindow(TLwin)
        end if
    elsif msg = WM_SETFOCUS and switchDBfocus then  -- only on startup
        switchDBfocus=0
        SortColumn = F_rec
        Sign = 1
        files = custom_sort(rCSC,files)
setFilters(1)
        setIndex(TLlvw,1)
        setFocus(TLlvw)
--      setFilters(1)
        return {0}
--  elsif msg = WM_SYSKEYDOWN then
    elsif msg = WM_SYSCHAR then
        wParam-='0'
        if wParam>=F_name and wParam<=F_rec then
            SortColumn = wParam
            Sign = 1
            files = custom_sort(rCSC,files)
            setIndex(TLlvw,1)
            setFocus(TLlvw)
            setFilters(1)
            void = sendMessage(id, LVM_UPDATE, 0, 0)
            return {0}
        end if
    elsif msg = WM_CLOSE then
        if action=PREVFTP then
            files=filessave
        end if
        removeFocus(TLwin)
    end if
    return 0
end function
setHandler( {TLwin,TLlvw,TLhelp,TLmc,TLif,TLfilt,TLok,TLcncl}, routine_id("TLwinHandler") )

global procedure tabsList(integer nahFTP)
-- nahFTP was added when I hijacked this to list previous pFTP edits.
integer found, k, k2
if nahFTP=0 then
    action=TABSLIST
    --
    -- remove any closed files...
    --
    for i = length(files) to 1 by -1 do
        found = 0
        for j = 1 to length(filenames) do
            if equal(files[i][F_name],filenames[j])
            and equal(files[i][F_dir],filepaths[j]) then
                found = 1
                exit
            end if
        end for
        if not found then
            k = files[i][F_rec]
            for j = 1 to length(files) do
                k2 = files[j][F_rec]
                if k2>k then
                    files[j][F_rec]=k2-1
                end if
            end for
            files = files[1..i-1]&files[i+1..length(files)]
        end if
    end for
    --
    -- insert any new ones...
    --
    k = 0
    for i = length(filenames) to 1 by -1 do
        found = 0
        for j = 1 to length(files)-k do
            if equal(files[j][F_name],filenames[i])
            and equal(files[j][F_dir],filepaths[i]) then
                found = 1
                exit
            end if
        end for
        if not found then
            for j = 1 to length(files) do
--              files[j][F_rec]+=1
                files[j][F_rec] = files[j][F_rec] + 1
            end for
            files = append(files,{filenames[i],getFileExtension(filenames[i]),filepaths[i],-1,-1,1})
            k+=1
        end if
    end for
    setText(TLwin,xl("Tabs List"))
    setVisible(TLhelp,True)
    setVisible({TLmc,TLif},False)
else
    action=PREVFTP
    filessave=files
    files=getPrevFTPfiles()
    setText(TLwin,xl("Previous FTP Edits"))
    setVisible(TLhelp,False)
    setVisible({TLmc,TLif},True)
    setCheck(TLmc,False)
    setCheck(TLif,True)
    setEnable(TLif,False)
end if
    switchDBfocus=1
    addFocus(TLwin)
    openWindow(TLwin,SW_NORMAL)
end procedure
global constant r_tabsList=routine_id("tabsList")

