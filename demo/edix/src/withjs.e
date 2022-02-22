--
-- demo\edix\src\withjs.e
-- ======================
--
--DEV (used by edix and p2js [but the latter really only desktop/Phix...])
-- builtins\misc\withjs.e
-- ======================
--
-- DEV deserves testing under pwa/p2js...
--
constant help_text = """
Easy management of with/without javascript_semantics directives
(rather than re-typing them out longhand again and again and again)
(at the time of writing I still have about 750 items to be updated)

Popular settings are shown, along with a custom button when the
current setting is not present or not recognised. Pressing any of the
numbers shown automatically applies and closes this popup.

Note that otherwise any custom setting should be achieved by editing 
the text as usual, that is as opposed to typing something in here 
only so it can copy it back to where you should have typed it in the 
first place, likewise for totally deleting a directive.

This is just a quick and handy little time/typing-saver."""

--
-- Interface
-- =========
--  [global procedure] set_js(integer callback, Ihandle dlg, sequence src)
--  callback: signature should be function(integer line, bool bInsert, string s)
--              where line is 1..length(src)+1,
--                    bInsert is true/false for insert/replace
--                    s is eg "with javascript_semantics"
--            obviously this does things quite differently in edix/p2js.
--              (bInsert==false and s=="" shd prob be handled specially)
--            note it is a routine_id (or the identifier w/o any "()"),
--                 that is as opposed to an actual "call_back()".
--  dlg: the main app window (whose name differs between edix and p2js)
--  src: a sequence of lines (ie split by \n, [""])
--

Ihandln js_dlg = NULL
Ihandle main_dlg
Ihandlns btns

-- (feel free to add more here [max 36!]:)
sequence directives = {"with javascript_semantics",
                       "without js -- (file i/o)",
                       "without js -- (libcurl)",
                       "without js -- (OpenGL 1.0)",
                       "without js -- (multitasking)",
                       "without js -- (no class under p2js)",
                       "(none)"} -- (nb this[$] oft clobbered)

integer js_callback, -- see callback above
        srcline = 0, -- any/first directive found
        insline = 0  -- where if "" is left as 0

procedure get_javascript_directive(sequence src)
--DEV/SUG... (from p2js_htmilse:)
--/*
    if match("without js",hsrc)
    or match("without javascript",hsrc)
    or match("without javascript_semantics",hsrc)
    or match("requires(64)",hsrc)
    or match("requires(WINDOWS)",hsrc)
    or match("requires(LINUX)",hsrc)
    or match("requires(5)",hsrc) then //(aka WIN+LNX, ie not JS)
        -- (notonline)
--*/
    string curr = "(none)"
    srcline = 0
    insline = 0
--  for i=1 to length(src) do
    for i=1 to min(20,length(src)) do
--DEV...
--      string line = src[i]
        sequence line = src[i]
        integer ch = iff(length(line)?line[1]:' ')
--?{line,ch,match("with",line),match(" j",line)}
        if ch='w' then
            if match("with",line)=1
            and find(match(" j",line),{5,8}) then
                curr = line
                srcline = i
                exit
            end if
        end if
        if ch!='-' and insline=0 then
            insline = i
        end if
    end for     
--?{curr,srcline,insline}
    directives[$] = curr
    integer active_button = find(curr,directives)
    IupSetInt(btns[$],"ACTIVE",active_button=length(directives))
    IupSetAttribute(btns[active_button],"VALUE","ON")
end procedure

function set_javascript_directive(integer jdx)
    if jdx<=length(directives) then
        bool bInsert = (srcline=0)
        integer line = filter({srcline,insline,1},"!=",0)[1]
--      integer line = iff(srcline?srcline:
--                     iff(insline?insline:1))
--      directives[$] = ""
        string s = directives[jdx]
        if s="(none)" then s = "" end if
        if length(s) or not bInsert then
            js_callback(line,bInsert,s)
        end if
        return IUP_CLOSE
    end if
    return IUP_IGNORE -- (eg '6'..'9' keyed)
end function

function directive_cb(Ihandle ih, integer state)
    string title = IupGetAttribute(ih,"TITLE")
    integer n = scanf(title,"(%d)%s")[1][1]
    return set_javascript_directive(n)  
end function
constant cb_directive = Icallback("directive_cb")

function help_cb(Ihandln /*ih*/)
    IupSetAttributeHandle(NULL,"PARENTDIALOG",js_dlg)
    IupMessage("JavaScript Directive Helper",help_text)
    IupSetAttributeHandle(NULL,"PARENTDIALOG",main_dlg)
    return IUP_DEFAULT
end function

function key_cb(Ihandle /*ih*/, atom c)
    if c=K_F1 then 
        return help_cb(NULL)
    elsif c=K_ESC then
        return IUP_CLOSE
    elsif c>='1' and c<='9' then
        return set_javascript_directive(c-'0')
    elsif length(directives)>=10 then
        c = lower(c)
        if c>='a' and c<='z' then
            return set_javascript_directive(c-('a'-10))
        end if
    end if
    return IUP_DEFAULT
end function

global procedure set_js(integer callback, Ihandle dlg, sequence src)
-- called by main thread, when Ctrl J is pressed with nothing selected
    if js_dlg=NULL then
        js_callback = callback
        btns = repeat(NULL,length(directives))
        for i=1 to length(directives) do
            integer ch = iff(i<=9?'0'+i:'A'+(i-10))
            string title = sprintf("(%c) %s",{ch,directives[i]})
            btns[i] = IupToggle(title,cb_directive,"PADDING=10x2, CANFOCUS=NO")
        end for
        Ihandle buttons = IupRadio(IupVbox(btns)),
                helpbtn = IupButton("Help (F1)",Icallback("help_cb"),"PADDING=10x2"),
                helpbox = IupHbox({IupFill(),helpbtn,IupFill()}),
                btnhelp = IupVbox({buttons,helpbox},"MARGIN=10x10,GAP=5")
        js_dlg = IupDialog(btnhelp,"MINSIZE=450x0")
        IupSetAttribute(js_dlg, "TITLE", "with/without javascript_semantics directive")
        main_dlg = dlg
        IupSetAttributePtr(js_dlg, "PARENTDIALOG", main_dlg)
        IupSetCallback(js_dlg, "K_ANY", Icallback("key_cb"));
    end if
    get_javascript_directive(src)
    IupPopup(js_dlg, IUP_CENTERPARENT, IUP_CENTERPARENT)
end procedure

