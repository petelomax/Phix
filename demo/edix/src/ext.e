--
-- src/ext.e
--
--  Manage file extension --> run with options
--
--TODO:
--  actually use these (see getExtRunWith)
--  add a standard entry for .chm files
--  verify WINDOWS/LINUX handling
--  Write a suitable "while not isfile(path[1..?]) do" loop.
--  If path does not start with " but contains spaces, quote it
--  (using above loop which is also needed for quoted/nospace).
--  Refuse to save things that do not exist (disable bt_save).

--#withtype Ihandle
--#withtype Ihandln

Ihandln ext_dlg = NULL

Ihandle matrix, extname, erunwith, bt_help, bt_save, bt_okc

constant titles = {"Extension", "Run With"}

sequence extensions={},
         runwiths,
         tags

procedure set_extname_dropdown()
    IupSetAttribute(extname,"1",NULL)   -- clear
    for i=1 to length(extensions) do
        IupSetAttribute(extname,sprintf("%d",i),extensions[i])
    end for
end procedure

function is_file(string filename)
    return file_exists(filename) and get_file_type(filename)=FILETYPE_FILE
end function

--DEV make this a standard builtin? (see also demo/PGUI/pdemo.exw and filedump.exw)
function get_interpreter()
-- returns "" on failure
    string res = command_line()[1]
    string file = get_file_name(res)
    -- <hack>
    --  I make copies of pw.exe as pw2.exe..pw9.exe, so that my work on
    --  Phix does not immediately clobber anything, or need all running
    --  apps to be closed before compilation can complete. Typically, I
    --  might have a few apps running with pw7.exe, fix something, then
    --  create pw8.exe, and change any and all "Run With" to use that.
    --  (ie Edita/Edix, menu/taskbar shortcuts, registry entries, etc.)
    --  (Will also do ph1ix -> phix. A messed up file==no matter.)
    if length(file)>3 then
        integer ch = file[3]
        if ch>='0' and ch<='9' then
            file[3..3] = ""     -- eg/ie "pw7.exe" -> "pw.exe"
        end if
    end if
    -- </hack>
--  if find(file,{"pw.exe","p.exe","phix"}) then
    if find(file,{"pw.exe","p.exe","p","p32","p64"}) then
        return res
    end if
    res = get_file_path(res,dropslash:=0)
    integer k = match(iff(platform()=WINDOWS?`\Phix\`:"/Phix/"),res)
    if k!=0 then
        res = res[1..k+5]   -- eg/ie ../Phix/demo/pGUI/ -> ../Phix/
    end if
    if platform()=WINDOWS then
        file = join_path({res,"pw.exe"})
        if is_file(file) then return file end if
        file = join_path({current_dir(),"pw.exe"})
        if is_file(file) then return file end if
        file = `C:\Program Files (x86)\Phix\pw.exe`     -- 32 bit?
        if is_file(file) then return file end if
        file = `C:\Program Files\Phix\pw.exe`           -- 64 bit?
        if is_file(file) then return file end if
        sequence paths = split(getenv("PATH"),';')
        for i=1 to length(paths) do
            file = join_path({paths[i],"pw.exe"})
            if is_file(file) then return file end if
        end for
    else
        file = join_path({res,"p"})
        if is_file(file) then return file end if
        file = join_path({current_dir(),"p"})
        if is_file(file) then return file end if
        --DEV?? %HOME/phix/phix? ~/phix/phix? /usr/bin/phix/phix?
    end if
    return ""
end function

global procedure save_extensions()
if not find("exw",extensions) then ?9/0 end if
if length(extensions)!=length(runwiths) then ?9/0 end if
    switchToIniGroup("Extensions")
    for i=1 to length(extensions) do
        setIniTextValue(extensions[i])
    end for
    switchToIniGroup("RunWiths")
    for i=1 to length(extensions) do
        setIniTextValue(runwiths[i])
    end for
end procedure

procedure load_extensions()
    switchToIniGroup("Extensions")
    extensions = getIniTextValues()
    if extensions={} then
        runwiths = {}
        string exw = get_interpreter()
        if platform()=WINDOWS
        and length(exw)=0 then
            --DEV read registry to see if "open" is sensible. (see pgui/pdemo)
--          if ??? then
--              exw = "open"
--          end if
        end if
        if length(exw)!=0 then
            extensions = append(extensions,"exw")
            runwiths = append(runwiths,exw)
        end if
--DEV platform()=WINDOWS..
        extensions = append(extensions,"bat")
        runwiths = append(runwiths,"open")
        extensions = append(extensions,"htm")
        runwiths = append(runwiths,"open")
        extensions = append(extensions,"html")
        runwiths = append(runwiths,"open")
    else
        switchToIniGroup("RunWiths")
        runwiths = getIniTextValues()
    end if
    tags = tagset(length(extensions))
end procedure

function value_cb(Ihandle /*self*/, integer l, integer c)
--?{"value_cb",l,c}
    if c>0 and c<=length(titles) then
        if l==0 then
            return IupRawStringPtr(titles[c])   -- column title
        end if
        if l<=length(extensions) then
            l = tags[l]
            if c=1 then -- extension
                return IupRawStringPtr(extensions[l])
            else
                return IupRawStringPtr(runwiths[l])
            end if
        end if
    end if
    return NULL
end function

procedure set_buttons()
-- set Add/Save/Delete and OK/Cancel, both the button text and whether active
    string ext = lower(IupGetAttribute(extname, "VALUE"))
    string rw = IupGetAttribute(erunwith, "VALUE")
    bool active -- (Add/Save/Delete enabled?)
    bool okc -- (true=>OK, false=>Cancel)
    if length(ext) and ext[1]='.' then ext = ext[2..$] end if
    if length(rw)>=2 and rw[1..2]="=." then rw[1..2] = "=" end if
--  if length(rw)>=1 and rw[1]="=" then rw[1] = "#" end if
    if length(rw)>=1 and rw[1]='=' then rw[1] = '#' end if
    if length(ext)=0 then
        active = false
        okc = length(rw)=0
    else
        integer idx = find(ext,extensions)
        if idx!=0 then
            string ds = iff(rw="" or rw=runwiths[idx]?"Delete":"Save")
            IupSetAttribute(bt_save, "TITLE", ds)
            active = true
            okc = rw=runwiths[idx]
        else
            IupSetAttribute(bt_save, "TITLE", "Add")
            active = length(rw)!=0 
            okc = false
        end if
    end if
    IupSetInt(bt_save, "ACTIVE", active and find('.',ext)=0)
    IupSetAttribute(bt_okc, "TITLE", iff(okc?"OK":"Cancel"))
end procedure

function enteritem_cb(Ihandle ih, integer l, integer c)
--?{"enteritem_cb",l,c}
    IupSetAttribute(ih,"MARKED", NULL);  /* clear all marks */
    IupMatSetAttribute(ih,"MARK", l, 0, "Yes");
    IupSetStrAttribute(ih,"REDRAW", "L%d", {l});
    IupSetStrAttribute(ih,"FOCUSCELL", "%d:%d", {l,c}); -- [1]
    if l>length(extensions) then l = 0 end if
    IupSetAttribute(extname,"VALUE",iff(l>0?extensions[tags[l]]:""))
    IupSetAttribute(erunwith,"VALUE",iff(l>0?runwiths[tags[l]]:""))
    set_buttons()
    return IUP_DEFAULT
end function

integer sortcol = 0
integer sortdir = 1

function by_column(integer i, integer j)
integer c = 0
    if sortcol=2 then   -- runwith
        c = sortdir*compare(runwiths[i],runwiths[j])
    end if
    if c=0 then -- extension or duplicate runwith
        c = compare(extensions[i],extensions[j])
        if sortcol=1 then
            c *= sortdir
        end if
    end if
    return c
end function

function click_cb(Ihandle /*self*/, integer l, integer c, atom /*pStatus*/)
    if c>0 and c<=length(titles) then
        if l=0 then -- title clicked, so sort that column
            if sortcol!=0 and sortcol!=c then
                IupSetAttributeId(matrix,"SORTSIGN",sortcol,"NO")
            end if
            sortdir = iff(IupGetAttributeId(matrix,"SORTSIGN",c)="DOWN"?-1:1)
            IupSetAttributeId(matrix,"SORTSIGN",c,iff(sortdir=-1?"UP":"DOWN"))
            sortcol = c
            tags = custom_sort(routine_id("by_column"),tags)
            IupSetAttribute(matrix,"REDRAW","ALL")
        end if
    end if
    return IUP_DEFAULT
end function

function ext_edit_cb(Ihandle /*extname*/, integer /*ch*/, atom pNewValue)
    string ext = lower(peek_string(pNewValue))
    if length(ext) and ext[1]='.' then ext = ext[2..$] end if
--  if ext="keys" then -- [it now uses "KEYS" and lower() to avoid any conflict]
--      IupMessage("Invalid",`An extension of "keys" may not be specified (it is used internally).`)
--      return IUP_IGNORE
--  els
    if find('.',ext)!=0 then
        IupMessage("Invalid","An extension may not contain embedded '.' characters (an initial one is quietly stripped).")
        return IUP_IGNORE
    end if
    return IUP_DEFAULT
end function
constant cb_ext_edit = Icallback("ext_edit_cb")

function ext_valuechanged_cb(Ihandle /*extname*/)
    string ext = lower(IupGetAttribute(extname, "VALUE"))
    if length(ext) and ext[1]='.' then ext = ext[2..$] end if
    integer idx = find(ext,extensions)
    if idx!=0 and length(ext)!=0 then
        IupSetAttribute(erunwith,"VALUE",runwiths[idx])
    end if
    set_buttons()
    return IUP_DEFAULT
end function
constant cb_ext_valuechanged = Icallback("ext_valuechanged_cb")

function run_valuechanged_cb(Ihandle /*erunwith*/)
    set_buttons()
    return IUP_DEFAULT
end function
constant cb_run_valuechanged = Icallback("run_valuechanged_cb")

Ihandln browse_dlg = NULL

function browse_cb(Ihandle /*bt_browse*/)
    if browse_dlg=NULL then
        browse_dlg = IupFileDlg()
        IupSetAttribute(browse_dlg, "DIALOGTYPE", "OPEN");
        IupSetAttributePtr(browse_dlg, "PARENTDIALOG", dlg);
        IupSetAttribute(browse_dlg, "EXTFILTER", "Executable Files|*.exe|");
        IupSetAttribute(browse_dlg, "TITLE", "Run With");
    end if
    IupPopup(browse_dlg, IUP_CURRENT, IUP_CURRENT); 
    if IupGetInt(browse_dlg, "STATUS")!=-1 then
        string runwith = IupGetAttribute(browse_dlg, "VALUE")
        IupSetAttribute(erunwith,"VALUE",runwith)
        set_buttons()
    end if
    return IUP_DEFAULT
end function
constant cb_browse = Icallback("browse_cb")

constant help_text="""
Specify how to run files with a given extension.

Selecting an existing entry from the list simply updates the fields below,
and will in fact will rudely discard anything entered but not yet saved. 
Alternatively you can enter the extension directly in the lower edit box. 
There is no way to change the extension itself, but the run value can be
copied/edited, and incorrect or no longer needed extensions can be deleted.

For the Run With field, use the browse button to search for the command or 
enter a path and filename string, eg C:\Program Files (x86)\Phix\pw.exe plus 
any command line switches required. Alternatively enter "=xxx" to treat this 
file extension the same as an existing one, eg to run .exu files the same as 
.exw files simply enter "exu" and "=exw" (without quotes or dots). 
Suppose I want to run a program with a different interpreter. 
I could assign the new interpreter to say e24, and then either create a file 
myapp.e24 which is just "include myapp.exw", or map "exw" to "=e24" or "=e31" 
or whatever in here, and do things that way.
Extensions are always converted to lower case before being stored, and have
any leading '.' quietly stripped.
Paths containing spaces may need to be wrapped in quotation marks. 
The filename will be substituted for "%s" if found, otherwise it is appended 
at the end (ensuring there is at least one space).

On Windows, the special run with value of "open" causes a plain shellExecute 
to be used, the same as double clicking on a file with that extension in 
Explorer, ie using details stored (by some other mechanism) in the registry.

The Save/Add/Delete and OK/Cancel buttons automatically enable/disable and
rename as you type. Note that OK/Cancel always just simply closes the window."""

function help_cb(Ihandle /*ih*/)
    IupMessage("Extensions",help_text)
    return IUP_DEFAULT
end function
constant cb_help = Icallback("help_cb")

function save_cb(Ihandle /*ih*/)
-- (this does not care or check whether bt_save says Add/Delete/Save,
--  but should, of course, logically match what set_buttons() does.)
    string ext = lower(trim(IupGetAttribute(extname, "VALUE")))
    string rw = trim(IupGetAttribute(erunwith, "VALUE"))
    if length(ext) and ext[1]='.' then ext = ext[2..$] end if
    if length(rw)>=2 and rw[1..2]="=." then rw[1..2] = "=" end if
    if length(ext)=0 then ?9/0 end if -- (bt_save should be disabled!)
    integer idx = find(ext,extensions)
    if idx=0 then
        -- add
        extensions = append(extensions,ext)
        runwiths = append(runwiths,rw)
        tags = append(tags,length(extensions))
        set_extname_dropdown()
    elsif length(rw)=0 or rw=runwiths[idx] then
        -- delete
--if USEINI then
--      switchToIniGroup("Extensions")
--      setIniValue(ext,"??")   -- DEV no way to actually delete ("" crashes...)
--      defineIniHeading(ext,TEXT)  -- that might work... no...
--else
--      IupConfigSetVariableStr(config,"Extensions",ext,NULL)
--end if
        extensions[idx..idx] = {}
        runwiths[idx..idx] = {}
        tags = custom_sort(routine_id("by_column"),tagset(length(extensions)))
        set_extname_dropdown()
    else
        -- save
        runwiths[idx] = rw
    end if
    IupSetAttribute(matrix, "REDRAW", "ALL");
    set_buttons()
    return IUP_DEFAULT
end function
constant cb_save = Icallback("save_cb")

function close_cb(Ihandle /*bt_close*/)
    if IupGetInt(ext_dlg,"MAXIMIZED")=0 then
        IupConfigDialogClosed(config, ext_dlg, "ExtDialog")
    end if
    IupHide(ext_dlg) -- do not destroy, just hide
    return IUP_DEFAULT
end function
constant cb_close = Icallback("close_cb")

function resize_cb(Ihandle /*ih*/, integer width, integer height)
integer width1 = IupGetIntId(matrix,"RASTERWIDTH",1)
integer new_width = width-width1-37
    IupSetIntId(matrix,"RASTERWIDTH",0,0) -- ("hide" line title)
    IupSetIntId(matrix,"RASTERWIDTH",2,new_width)
    integer vlines = max(4,floor((height-10)/23)-6)
    IupSetInt(matrix,"NUMLIN",max(length(extensions),vlines))
    IupSetInt(matrix,"NUMLIN_VISIBLE",vlines)
    return IUP_DEFAULT
end function

function key_cb(Ihandle /*ih*/, atom c)
    if c=K_F1 then return help_cb(dlg) end if
    if c=K_ESC then IupHide(ext_dlg) end if
    return IUP_DEFAULT
end function

procedure create_ext_dialog()

    matrix = IupMatrix()
    IupSetInt(matrix, "NUMCOL", length(titles))
    IupSetInt(matrix, "NUMCOL_VISIBLE", length(titles))
--  IupSetInt(matrix, "NUMLIN", 15)
    IupSetInt(matrix, "NUMLIN", length(extensions))
--  IupSetInt(matrix, "NUMLIN_VISIBLE", length(extensions))
    IupSetInt(matrix, "NUMLIN_VISIBLE", 15)
    IupSetIntId(matrix, "RASTERWIDTH", 1, 80)
    IupSetAttribute(matrix, "ALIGNMENT", "ALEFT")
    --IMPORTANT: HEIGHT0 tells IupMatrix that we are gonna have column titles at line 0
    IupSetInt(matrix, "HEIGHT0", 10);
    IupSetAttribute(matrix, "RESIZEMATRIX", "YES");
    IupSetAttribute(matrix, "MARKMODE", "LIN");
    IupSetAttribute(matrix, "MARKAREA", "CONTINUOUS");

--  IupSetAttribute(matrix, "READONLY", "YES"); -- with VALUE_CB set, non-setting of VALUE_EDIT_CB (also) makes it read-only
    IupSetAttribute(matrix, "HIDEFOCUS", "YES");
    IupSetAttribute(matrix, "FRAMECOLOR", "220 220 220");
    IupSetAttribute(matrix, "BORDER", "NO");
    IupSetAttribute(matrix, "CURSOR", "ARROW");

    IupSetCallback(matrix, "VALUE_CB",      Icallback("value_cb"))
    IupSetCallback(matrix, "ENTERITEM_CB",  Icallback("enteritem_cb"));
    IupSetCallback(matrix, "CLICK_CB",      Icallback("click_cb"));

    Ihandle extlbl = IupLabel("Extension","PADDING=0x4")
    extname = IupList("VISIBLECOLUMNS=8, DROPDOWN=YES, EDITBOX=YES")
    IupSetCallback(extname, "EDIT_CB", cb_ext_edit)
    IupSetCallback(extname, "VALUECHANGED_CB", cb_ext_valuechanged)
    set_extname_dropdown()
    Ihandle bt_browse = IupButton("Browse","ACTION",cb_browse,"PADDING=10x2")
    Ihandle runlbl = IupLabel("Run With","PADDING=0x4")
    erunwith = IupText("EXPAND=HORIZONTAL")
    IupSetCallback(erunwith, "VALUECHANGED_CB", cb_run_valuechanged)

    bt_help = IupButton("Help","ACTION",cb_help,"PADDING=10x2")
    bt_save = IupButton("Save/Add","ACTION",cb_save,"PADDING=10x2")
    bt_okc  = IupButton("OK/Cancel","ACTION",cb_close,"PADDING=10x2")
    Ihandle box = IupVbox({matrix,
                           IupHbox({extlbl,extname,IupFill(),bt_browse},"MARGIN=20x0"),
--                         IupFill(), -- no help...
                           IupHbox({runlbl,erunwith},"MARGIN=20x0"),
                           IupHbox({IupFill(),
                                    bt_help,
                                    bt_save,
                                    bt_okc},
                                   "MARGIN=20x0,NORMALIZESIZE=HORIZONTAL")});
    IupSetAttribute(box, "MARGIN", "10x10");
    IupSetAttribute(box, "GAP", "5");

    ext_dlg = IupDialog(box)

    IupSetAttributePtr(ext_dlg, "PARENTDIALOG", dlg)
    IupSetCallback(ext_dlg, "CLOSE_CB", cb_close);
    IupSetAttribute(ext_dlg, "TITLE", "Extensions");
    IupSetCallback(ext_dlg, "RESIZE_CB", Icallback("resize_cb"));
    IupSetCallback(ext_dlg, "K_ANY", Icallback("key_cb"));
end procedure

procedure extensions_dialog()
    -- Aside: it is intentional that Options/Extensions, delete all, close,
    --        Options/Extensions will reset to the standard defaults.
    if extensions={} then load_extensions() end if
    if ext_dlg=NULL then
        create_ext_dialog()
    end if
    IupConfigDialogShow(config, ext_dlg, "ExtDialog")
    IupPopup(ext_dlg,IUP_CURRENT,IUP_CURRENT)
end procedure

function ext_cb(Ihandle /*item_ext*/)
    extensions_dialog()
    return IUP_DEFAULT
end function
global constant cb_ext = Icallback("ext_cb")

global function getExtRunWith(string ext)
    if extensions={} then load_extensions() end if
    string res
--1/10/18:
--  integer k = find(ext,extensions)
--  return iff(k=0?"",runwiths[k])
    while 1 do
        integer k = find(ext,extensions)
        if k=0 then res = "" exit end if
        res = runwiths[k]
        if length(res)=0 or res[1]!='#' then exit end if
        ext = res[2..$]
    end while
    return res
end function

