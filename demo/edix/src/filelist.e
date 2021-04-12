--
-- filelist.e
-- ==========
--
--  fileList function.
--  Author: Pete Lomax
--
--  When you have a large number of files open, they can get lost either off 
--  the side (single line tabs option) or lost in the crowd (multiline tabs).
--  This program displays all open files in a listview, allowing sorting by
--  directory, name, extension, last mod date, size, and using a filter.
--
--  Invoked via File/File List or Ctrl T.
--
--#withtype Ihandle
--#withtype Ihandln

Ihandln filelist_dlg = NULL
Ihandle matrix, bt_help, lbl_fill, filter, bt_ok, bt_cancel

constant titles = {"File", "ext", "Directory", "Size", "Last modified", "rec"},
         twidth = { 120,    40,     400,        50,     110,             50},
                     L = "ALEFT", C = "ACENTER", R = "ARIGHT",
         talign = {  L,      C,       L,         R,       C,              C},
         minwid = { 50,     20,     100,        30,     110,             20}

sequence files
         files = {}

constant F_name=1,
--       F_ext=2,   -- constant not actually used (but column is visible/valid)
         F_dir=3,
         F_size=4,
         F_date=5,
         F_rec=6

sequence fmap   -- 'real' indexes after filtering effects

sequence tags

procedure select()
--object r
integer k,k2
--  k = getIndex(TLlvw)
    k = IupGetInt(matrix,"FOCUSCELL")
--?{"select","k",k,"fmap[k]",fmap[k],"tags[fmap[k]]",tags[fmap[k]]}
    if k and k<=length(fmap) then
        k = fmap[k]
--      k = tags[k]
--DEV??
--      k = tags[fmap[k]]
--      {} = openFile(files[k][F_dir]&files[k][F_name],1,isLegacyTabHandling)
        string filename = files[k][F_dir]&files[k][F_name]
--?filename
        {} = openFile(0,filename,-1)
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
--      closeWindow(TLwin)
        IupHide(filelist_dlg)
    end if
end procedure

procedure setFilters(integer clr)
-- clr is 1 if the filter should be cleared when all get filtered
sequence ftxt
integer k
    ftxt = lower(IupGetAttribute(filter,"VALUE"))
--  fmap = repeat(0,length(files))
    fmap = {}
    k=1
    for i=1 to length(files) do
        integer ti = tags[i]
        if length(ftxt)=0
--16/2/15 (a filter of "test\list.asm" was failing to match anything)
--      or match(ftxt,lower(files[i][F_name]))
--      or match(ftxt,lower(files[i][F_dir])) then
        or match(ftxt,lower(files[ti][F_dir]&files[ti][F_name])) then
--          fmap[k] = i
--          k += 1
            fmap &= ti
        end if
    end for
--  if k=1 and clr then
    if length(fmap)=0 and clr then
--      setText(TLfilt,"")
        IupSetAttribute(filter,"VALUE","")
--      k = length(fmap)+1
--      for i=1 to k-1 do
--          fmap[i] = i
--      end for
--      tags = tagset(length(files))
        fmap = tags
    end if
--  setLVItemCount(TLlvw,k-1)
--  IupSetInt(matrix, "NUMLIN", k-1)
    IupSetInt(matrix, "NUMLIN", length(fmap))
--  tags = custom_sort(tags)
end procedure

object semiperm

function value_cb(Ihandle /*self*/, integer l, integer c)
--?{"value_cb",l,c}
    if c>0 and c<=length(titles) then
        if l==0 then
            return IupRawStringPtr(titles[c])   -- column title
        end if
        if l<=length(fmap) then
--          l = tags[l]
            l = fmap[l]
            semiperm = files[l][c]
            if not string(semiperm) then
--/!*
                object d = dir(files[l][F_dir]&files[l][F_name])
                if atom(d) or length(d)=0 then
                    semiperm = "???"
                else
                    d = d[1]
                    files[l][F_size] = d[3]
                    d = d[4..9]
                    files[l][F_date] = d
                    if c=F_date then
                        --DEV format date as per status bar?
                        integer year = d[DT_YEAR]
                        d[DT_YEAR] = d[DT_DAY]
                        d[DT_DAY] = year
                        semiperm = sprintf(`%02d\%02d\%04d %02d:%02d:%02d`,d)
                    else
--                      semiperm = sprintf("%d",semiperm)
                        semiperm = sprintf("%d",files[l][c])
                    end if
                end if
--*!/
--/*
                if c=F_size then
                    semiperm = "F_size"         
                elsif c=F_date then
                    semiperm = "F_date"
                elsif c=F_rec then
                    semiperm = sprintf("%d",semiperm)
                else
                    ?9/0
                end if
--*/
            end if
            return IupRawStringPtr(semiperm)
        end if
    end if
    return NULL
end function

integer selected_line = 0

function enteritem_cb(Ihandle ih, integer l, integer c)
--?{"enteritem_cb",l,c}
    IupSetAttribute(ih,"MARKED", NULL);  /* clear all marks */
    IupMatSetAttribute(ih,"MARK", l, 0, "Yes");
    IupSetStrAttribute(ih,"REDRAW", "L%d", {l});
    IupSetStrAttribute(ih,"FOCUSCELL", "%d:%d", {l,c}); -- [1]
--  selected_line = tags[l]  -- no!
    selected_line = l
    return IUP_DEFAULT
end function

--integer redraw_all = 0

--integer SortColumn, Sign

integer sortcol = 0
integer sortdir = 1

function by_column(integer i, integer j)
object fi = files[i][sortcol],
       fj = files[j][sortcol]
    if string(fi) then fi = lower(fi) end if
    if string(fj) then fj = lower(fj) end if
    integer c = sortdir*compare(fi,fj)
--  if sortcol=3 then   -- descriptions
----        c = sortdir*compare(descs[i],descs[j])
--      if c=0 then ?9/0 end if -- sanity check
--  elsif sortcol=1 then -- mapping
----        c = sortdir*compare(keymaps[i],keymaps[j])
--  end if
    if c=0 then
        c = sortdir*compare(lower(files[i][F_name]),lower(files[j][F_name]))
--      if sortcol=2 then
--          c *= sortdir
--      end if
    end if
    return c
end function

function click_cb(Ihandle /*self*/, integer l, integer c, atom pStatus)
    if c>0 and c<=length(titles) then
        if l=0 then -- title clicked, so sort that column (cycle through down/up/none)
            if sortcol!=0 and sortcol!=c then
                IupSetAttributeId(matrix,"SORTSIGN",sortcol,"NO")
            end if
            string sortsign = IupGetAttributeId(matrix,"SORTSIGN",c)
            if sortsign="UP" then
                IupSetAttributeId(matrix,"SORTSIGN",c,"NO")
                sortdir = 1
                sortcol = 0
                tags = sort(tags)
            else
                IupSetAttributeId(matrix,"SORTSIGN",c,iff(sortsign="DOWN"?"UP":"DOWN"))
                sortdir = iff(sortsign="DOWN"?-1:1)
                sortcol = c
                tags = custom_sort(routine_id("by_column"),tags)
            end if
            setFilters(0)
            IupSetAttribute(matrix,"REDRAW","ALL")
            selected_line = 0
        elsif iup_isdouble(pStatus) then
            select()
        end if
    end if
    return IUP_DEFAULT
end function

constant help_text="""
Shows a list of open file (tab)s.

This may not be of much use if only a few files are open, but when there are
several dozen open, they can be difficult to find in the tabbar. The rec column
sorts files in order of most recently selected via this window (current edit
session only). Files may also be sorted by filename, extension, directory, 
file size, or date last modified, by clicking on the column heading or pressing 
Alt 1..6, and/or a filter applied to the filename or directory.

It is possible to remove the tabbar completely (via Options/Display) and use
this function to switch between files.

Double click on a file, press Return or click on OK to open the currently
selected row, or press Escape or click on Cancel to exit."""

function help_cb(Ihandle /*ih*/)
    IupMessage("Keyboard",help_text)
    return IUP_DEFAULT
end function
constant cb_help = Icallback("help_cb")

function close_cb(Ihandle /*bt_close*/)
    if IupGetInt(filelist_dlg,"MAXIMIZED")=0 then
        IupConfigDialogClosed(config, filelist_dlg, "FileList")
    end if
    IupHide(filelist_dlg) -- do not destroy, just hide
    return IUP_DEFAULT
end function
constant cb_close = Icallback("close_cb")

function ok_cb(Ihandle /*bt_ok*/)
    select()
    return IUP_DEFAULT
end function
constant cb_ok = Icallback("ok_cb")

function cancel_cb(Ihandle /*bt_cancel*/)
    return close_cb(bt_cancel)
end function
constant cb_cancel = Icallback("cancel_cb")

function resize_cb(Ihandle /*ih*/, integer width, integer height)
sequence widths = repeat(0,IupGetInt(matrix,"NUMCOL"))
integer wi, total_width = 0, new_width
    for i=1 to length(widths) do
        widths[i] = IupGetIntId(matrix,"RASTERWIDTH",i)
    end for
    total_width = sum(widths)
    {width,height} = IupGetIntInt(filelist_dlg,"RASTERSIZE")
    width -= 90
    IupSetIntId(matrix,"RASTERWIDTH",0,0)
    for i=1 to length(widths) do
        wi = widths[i]
        new_width = max(floor((wi/total_width)*width),minwid[i])
        IupSetIntId(matrix,"RASTERWIDTH",i,new_width)
    end for
    integer vlines = max(4,floor((height-1)/23)-6)
    IupSetInt(matrix,"NUMLIN",max(length(files),vlines))
    IupSetInt(matrix,"NUMLIN_VISIBLE",vlines)
    IupRefresh(filelist_dlg)
--I used this to set MINSIZE: 
--IupSetAttribute(filelist_dlg, "TITLE", IupGetAttribute(filelist_dlg,"RASTERSIZE"))
    return IUP_DEFAULT
end function

function key_cb(Ihandle /*ih*/, atom c)
    if c=K_F1 then 
        return help_cb(dlg)
    elsif c=K_ESC then
        return close_cb(filelist_dlg)
    elsif c=K_CR then
        select()
    elsif c=K_DEL
       or c=K_BS then
        string Ftext = IupGetAttribute(filter,"VALUE")
        if length(Ftext) then
            Ftext = Ftext[1..$-1]
            IupSetAttribute(filter,"VALUE",Ftext)
            setFilters(0)
        end if
    elsif c>=' ' and c<='~' then
        string Ftext = IupGetAttribute(filter,"VALUE")&c
        IupSetAttribute(filter,"VALUE",Ftext)
        setFilters(0)
    end if
--  if selected_line!=0 then
--      redraw_all = 0
--      integer l = tags[selected_line]
--      if redraw_all then
--          IupSetStrAttribute(matrix,"REDRAW", "ALL")
--      else
--          IupSetStrAttribute(matrix,"REDRAW", "L%d", {selected_line})
--      end if
--  end if
    return IUP_DEFAULT
end function

procedure create_filelist_dialog()

    matrix = IupMatrix()
    IupSetInt(matrix, "NUMCOL", length(titles))
    IupSetInt(matrix, "NUMCOL_VISIBLE", length(titles))
    IupSetInt(matrix, "NUMLIN", length(files))
    IupSetInt(matrix, "NUMLIN_VISIBLE", 15)
    IupSetIntId(matrix, "RASTERWIDTH", 1, 80)
    IupSetAttribute(matrix, "ALIGNMENT", "ALEFT")
    for i=1 to length(twidth) do
        IupSetIntId(matrix, "RASTERWIDTH", i, twidth[i])
        IupSetAttributeId(matrix, "ALIGNMENT", i, talign[i])
    end for
    --IMPORTANT: HEIGHT0 tells IupMatrix that we are gonna have column titles at line 0
    IupSetInt(matrix, "HEIGHT0", 10);
    IupSetAttribute(matrix, "RESIZEMATRIX", "YES");
    IupSetAttribute(matrix, "MARKMODE", "LIN");
    IupSetAttribute(matrix, "MARKAREA", "CONTINUOUS");

    IupSetAttribute(matrix, "HIDEFOCUS", "YES");
    IupSetAttribute(matrix, "FRAMECOLOR", "220 220 220");
    IupSetAttribute(matrix, "BORDER", "NO");
    IupSetAttribute(matrix, "CURSOR", "ARROW");

    IupSetCallback(matrix, "VALUE_CB",      Icallback("value_cb"))
    IupSetCallback(matrix, "ENTERITEM_CB",  Icallback("enteritem_cb"));
    IupSetCallback(matrix, "CLICK_CB",      Icallback("click_cb"));

    bt_help   = IupButton("Help","ACTION",cb_help,"PADDING=10x2")
    lbl_fill = IupLabel("Filter")
    filter = IupText("EXPAND=HORIZONTAL,ACTIVE=NO")

    bt_ok     = IupButton("OK","ACTION",cb_ok,"PADDING=10x2")
    bt_cancel = IupButton("Cancel","ACTION",cb_cancel,"PADDING=10x2")

    Ihandle buttons = IupHbox({bt_help,lbl_fill,filter,bt_ok,bt_cancel},
                              "MARGIN=20x0,ALIGNMENT=ACENTER")
    Ihandle box = IupVbox({matrix,
--                         IupFill(),
                           buttons
                          });
    IupSetAttribute(box, "MARGIN", "10x10");
    IupSetAttribute(box, "GAP", "5");

    filelist_dlg = IupDialog(box)

    IupSetAttribute(filelist_dlg,"MINSIZE","570x220")
    IupSetAttributePtr(filelist_dlg, "PARENTDIALOG", dlg)
    IupSetCallback(filelist_dlg, "CLOSE_CB", cb_close);
    IupSetAttribute(filelist_dlg, "TITLE", "Currently Open File Tabs");
    IupSetCallback(filelist_dlg, "RESIZE_CB", Icallback("resize_cb"));
    IupSetCallback(filelist_dlg, "K_ANY", Icallback("key_cb"));
    IupSetAttributeHandle(filelist_dlg,"PARENTDIALOG",dlg)
end procedure

global procedure fileList()

    if filelist_dlg=NULL then
        create_filelist_dialog()
--DEV why??
    elsif sortcol!=0 then
        IupSetAttributeId(matrix,"SORTSIGN",sortcol,"NO")
        sortcol = 0
    end if

    --
    -- remove any closed files...
    --
    integer found, k, k2
    for i=length(files) to 1 by -1 do
        found = 0
        for j=1 to length(filenames) do
            if equal(files[i][F_name],filenames[j])
            and equal(files[i][F_dir],filepaths[j]) then
                found = 1
                exit
            end if
        end for
        if not found then
            k = files[i][F_rec]
            for j=1 to length(files) do
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
    for i=length(filenames) to 1 by -1 do
        found = 0
        for j=1 to length(files)-k do
            if equal(files[j][F_name],filenames[i])
            and equal(files[j][F_dir],filepaths[i]) then
                found = 1
                exit
            end if
        end for
        if not found then
            for j=1 to length(files) do
                files[j][F_rec] += 1
            end for
            files = append(files,{filenames[i],get_file_extension(filenames[i]),filepaths[i],-1,-1,1})
            k += 1
        end if
    end for

    tags = tagset(length(files))

--  switchDBfocus = 1
--  SortColumn = F_rec
    sortcol = F_rec
--  Sign = 1
    sortdir = 1
--  files = custom_sort(rCSC,files)
--  files = custom_sort(routine_id("by_column"),files)
    tags = custom_sort(routine_id("by_column"),tags)

    setFilters(1)

    IupSetAttribute(matrix,"FOCUSCELL","1:1")

    IupConfigDialogShow(config, filelist_dlg, "FileList")
--  IupPopup(filelist_dlg,IUP_CURRENT,IUP_CURRENT)
--  IupPopup(filelist_dlg,IUP_CENTERPARENT,IUP_CENTERPARENT)
    integer {w,h} = IupGetIntInt(filelist_dlg,"CLIENTSIZE")
    {} = resize_cb(filelist_dlg,w,h)
    IupUpdate(filelist_dlg)
--  IupShowXY(HelpWin,IUP_CENTERPARENT,IUP_CENTERPARENT)
end procedure



--/* copy of eatabl.e:
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
constant TLfill = create(Label,xl("Filter"),0,TLwin,0,0,0,0,SS_RIGHT)
constant TLfilt = create(EditText,"",0,TLwin,0,0,0,0,0)
constant TLok   = create(Button,xl("OK"),0,TLwin,0,0,0,0,0)
constant TLcncl = create(Button,xl("Cancel"),0,TLwin,0,0,0,0,0)


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
        return Sign*compare(lower(s1[SortColumn]),lower(s2[SortColumn]))
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
            return sprintf(`%02d\%02d\%04d %02d:%02d:%02d`,d)
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
                setText(TLfilt,lower(getText(TLfilt)&wParam))
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
        void=c_func( xMoveWindow, {getHwnd(TLhelp), rect[1]+5,  rect[4]-30, 60,25,1})
        void=c_func( xMoveWindow, {getHwnd(TLfill), rect[1]+70, rect[4]-25, 35,25,1})
        void=c_func( xMoveWindow, {getHwnd(TLfilt), rect[1]+110,rect[4]-30, rect[3]-rect[1]-275,25,1})
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
        removeFocus(TLwin)
    end if
    return 0
end function
setHandler( {TLwin,TLlvw,TLhelp,TLfilt,TLok,TLcncl}, routine_id("TLwinHandler") )

global procedure fileList()
>>
integer found, k, k2

    --
    -- remove any closed files...
    --
    for i=length(files) to 1 by -1 do
        found = 0
        for j=1 to length(filenames) do
            if equal(files[i][F_name],filenames[j])
            and equal(files[i][F_dir],filepaths[j]) then
                found = 1
                exit
            end if
        end for
        if not found then
            k = files[i][F_rec]
            for j=1 to length(files) do
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
    for i=length(filenames) to 1 by -1 do
        found = 0
        for j=1 to length(files)-k do
            if equal(files[j][F_name],filenames[i])
            and equal(files[j][F_dir],filepaths[i]) then
                found = 1
                exit
            end if
        end for
        if not found then
            for j=1 to length(files) do
                files[j][F_rec] += 1
            end for
            files = append(files,{filenames[i],get_file_extension(filenames[i]),filepaths[i],-1,-1,1})
            k += 1
        end if
    end for
    switchDBfocus = 1
    SortColumn = F_rec
    Sign = 1
    files = custom_sort(rCSC,files)
    setFilters(1)

--  addFocus(TLwin)
    openWindow(TLwin,SW_NORMAL)
end procedure
--global constant r_fileList=routine_id("fileList")
--*/ --<end of copy of eatabl.e>
