--
-- src/find.e
--
--DEV/SUG: option: skip comment-lines 
--                    Note: This is specifically not "skip comments", but 
--                          "skip lines with LineComment in column 1",
--                          where LineComment is as defined in Xxx.syn.
--                          It was originally implemented to ease the 
--                          pain of searching listing files rather than 
--                          source files. (DEV fif too?)
--/*
--Wrap-around search algorithm
--This proved far harder than I ever imagined possible, any improvements welcome.\\

--DEV: start/wrappable need to be saved on a per-file basis, like current (aka CursorY)

--The following is a simplified testbed.\\
--First simplification: text is "0000".."1111" and "find" is whether or not text[1..4] is '1'.\\
--Second simplification: we set "start" explicitly in the testing loop, maybe unlike real use.\\
--Starting from 1..4, with N=sum of 1s, we want f3find() to get N/eof/N/eof, iyswim.\\
--(Technically we shouldn't need to reset wrappable in this testbed, but abs. rqd. in real use)
--The challenge is(was) to get all 64 test cases to work, then repeat searching backwards.

--Anyway, a second set of eyes, before I try applying this to Edix/Edita, anything that 
--simplifies it or makes it any easier to understand, or OE-compatible, thankx.

--<eucode>
sequence text
integer start, current, wrappable = 1

function f3find(integer direction)
--
-- direction should be +/-1. [existing code uses 0 to mean "from line 1"]
-- return "next/prev" '1' in text, wrapping around relative to start.
-- return -1 at "eof", aka "start", allowing restart. 
--
integer limit = iff(direction<0?1:length(text)),
        begin = iff(direction>0?1:length(text))

    current += direction
    bool high = wrappable and compare(start,current)!=direction
    limit = iff(high ? limit : start-(wrappable=0)*direction)
    for i=current to limit by direction do
        if text[i]='1' then current=i return i end if
    end for
    if high then
        wrappable = 0
        limit = start-direction
        for i=begin to limit by direction do
            if text[i]='1' then current=i return i end if
        end for
    end if
    current = start-direction
    wrappable = 1
    return -1
end function

constant TRIES=4
integer fails = 0, total = 0
for i=0 to 15 do
    text = sprintf("%04b",i)
    integer N = sum(sq_eq(text,'1'))
    ?{text,N}
    for direction=+1 to -1 by -2 do
        for j=1 to 4 do
            start = j
            current = j-direction
            wrappable = 1
            sequence s = {}
            for t=1 to (N+1)*TRIES do
                s &= f3find(direction)
            end for
            total += 1
            if s[$]!=-1 or sum(sq_eq(s,-1))!=TRIES then
                s &= {"9/0"}
                fails += 1
                ?{"s=",s}
            end if
--          ?s
        end for         
    end for         
end for
printf(1,"fails: %d, pass: %d/%d\n",{fails,total-fails,total})
--</eucode>
?"done"
{} = wait_key()
abort(0)

--*/

Ihandln find_dlg = NULL
Ihandle find_txt, find_case

sequence find_txts = {}
integer find_start = 0

procedure hide_find()
    if IupGetInt(find_dlg,"VISIBLE")
    and IupGetInt(find_dlg,"MAXIMIZED")=0 then
        IupConfigDialogClosed(config, find_dlg, "FindDialog")
        IupHide(find_dlg) -- do not destroy, just hide
    end if
end procedure

bool eof_msg = false

procedure eof_message()
    if eof_msg then
        eof_msg = false
--      IupMessage("End", "Search has reached end of document.")
        Ihandln md = IupMessageDlg()
        IupSetAttribute(md,"TITLE","End")
        IupSetAttribute(md,"VALUE","Search has reached end of document")
--      IupSetAttribute(md,"PARENTDIALOG",dlg)
--      IupSetAttributePtr(md,"PARENTDIALOG",dlg)
        IupSetAttributeHandle(md,"PARENTDIALOG",dlg)    -- WOW!!!
        IupPopup(md)
        md = IupDestroy(md)
    end if
end procedure

procedure find_text(integer direction)
    if find_dlg!=NULL then
        string str_to_find = IupGetAttribute(find_txt, "VALUE");
        -- ((DEV) above may need utf8_to_utf32())
        if length(str_to_find)!=0 then
            -- first, maintain the text dropdown:
            integer fdx = find(str_to_find,find_txts)
            if fdx=0 or fdx!=length(find_txts) then
                if fdx!=0 then
                    find_txts[fdx..fdx] = {}
                    IupSetInt(find_txt,"REMOVEITEM",fdx)
                end if
                find_txts = append(find_txts,str_to_find)
                IupSetAttribute(find_txt,sprintf("%d",length(find_txts)),str_to_find)
--?{"find_text(), find_start(was ",find_start,"):=",iff(direction=0?0:CursorY)}
--no help:
                find_start = iff(direction=0?1:CursorY+1)
--              find_start = iff(direction=0?1:min(length(filetext[currfile]),CursorY+1))
            end if
            integer casesensitive = IupGetInt(find_case, "VALUE")
            integer pos = 0, i, toline,
--           fromline
                    fromline = iff(direction=0?1:CursorY+1)
            sequence line = filetext[currfile][fromline]
            integer start = iff(selON and selY=CursorY and compare(selX,CursorX)=direction?selX:CursorX)
            if direction=-1 then
--              fromline = CursorY-1
                fromline = CursorY
                start = length(line)+1-MapToByte(line,start-1)
                str_to_find = reverse(str_to_find)
                toline = iff(fromline>find_start?find_start:1)
                for i=fromline to toline by -1 do
                    line = filetext[currfile][i]
                    pos = match(str_to_find, reverse(line), start, casesensitive)
                    if pos!=0 then exit end if
                    start = 1
                end for
                if pos=0 and fromline<find_start then
                    start = 1
                    for i=length(filetext[currfile]) to find_start+1 by -1 do
                        line = filetext[currfile][i]
                        pos = match(str_to_find, reverse(line), start, casesensitive)
                        if pos!=0 then exit end if
                    end for
                end if
            else
                fromline = iff(direction=0?1:CursorY+1)
                start = iff(direction=0?1:MapToByte(line,start))
                toline = iff(fromline<find_start?find_start:length(filetext[currfile]))
--?{"find_text(), fromline=",fromline," find_start=",find_start," toline=",toline}
--              for i=fromline to toline do
--if i>length(filetext[currfile]) then ?"find.e line 162" exit end if
                for i=fromline to length(filetext[currfile]) do
                    line = filetext[currfile][i]
                    pos = match(str_to_find, line, start, casesensitive)
                    if pos!=0 then exit end if
                    start = 1
                end for
if 0 then
                if pos=0 and fromline>=find_start then
                    start = 1
--?{"find_text(), 1 to ",find_start}
--?{"find_text(), 1 to ",fromline-1}
--                  for i=1 to find_start do
--                  for i=1 to fromline-1 do
                    for i=1 to find_start-1 do
                        line = filetext[currfile][i]
                        pos = match(str_to_find, line, start, casesensitive)
                        if pos!=0 then exit end if
                    end for
                end if
end if
            end if
            hide_find()
            if pos!=0 then
                CursorX = pos-1
                if direction=-1 then
                    CursorX = length(filetext[currfile][i])-CursorX
                end if
                CursorY = i-1
                selON = 1
                selX = ExpLength(line[1..CursorX])
                CursorX += length(str_to_find)*iff(direction=0?1:direction)
                CursorX = ExpLength(line[1..CursorX])
                selY = CursorY
                forceCursorOnscreen()
            else
                -- moved to after IupPopup returns:
--              IupMessage("End", "Search has reached end of document.");
                eof_msg = true
--              find_start = CursorY
                find_start = CursorY+1  -- NO! (makes it loop)
            end if
--          hide_find()
        end if
    end if
end procedure

function find_next_action_cb(Ihandle /*ih*/)
/* this callback can be called from the main dialog also */
    find_text(+1)
    eof_message()
    return IUP_DEFAULT
end function
global constant cb_findnext = Icallback("find_next_action_cb")

function find_prior_action_cb(Ihandle /*ih*/)
    find_text(-1)
    eof_message()
    return IUP_DEFAULT
end function
global constant cb_findprev = Icallback("find_prior_action_cb")

--DEV replace now via macros...
-- (bugger - I was going to use the drop-down as a clipboard doobrie)
--/*
function find_replace_action_cb(Ihandle /*bt_replace*/)
?"find_replace_action_cb"
Ihandle find_dlg = IupGetInt(bt_replace, "FIND_DIALOG");
Ihandle multitext = IupGetInt(find_dlg, "MULTITEXT");
integer find_pos = IupGetInt(multitext, "FIND_POS");
string selectionpos = IupGetAttribute(multitext, "SELECTIONPOS");
string find_selection = IupGetAttribute(multitext, "FIND_SELECTION");

    if find_pos== -1
    or selectionpos!=find_selection then
        {} = find_next_action_cb(bt_replace);
    else
        Ihandle replace_txt = IupGetDialogChild(find_dlg, "REPLACE_TEXT");
        string str_to_replace = IupGetAttribute(replace_txt, "VALUE");
        IupSetAttribute(multitext, "SELECTEDTEXT", str_to_replace);

        /* then find next */
        {} = find_next_action_cb(bt_replace);
    end if
    return IUP_DEFAULT;
end function
--*/

function find_close_action_cb(Ihandle /*bt_close*/)
--/*
Ihandle find_dlg = IupGetDialog(bt_close);
Ihandle multitext = IupGetInt(find_dlg, "MULTITEXT");
--Ihandle config = IupGetInt(multitext, "CONFIG");
--*/
    hide_find()
    return IUP_DEFAULT
end function
constant cb_find_close_action = Icallback("find_close_action_cb")

procedure create_find_dialog()
--Ihandle box, bt_next, bt_close
--Ihandle bt_replace;replace_txt, 
--
--  find_txt = IupText("VISIBLECOLUMNS=20, EXPAND=HORIZONTAL")
    find_txt = IupList("VISIBLECOLUMNS=20, EXPAND=HORIZONTAL, DROPDOWN=YES, EDITBOX=YES")
--  IupSetAttribute(find_txt, "NAME", "FIND_TEXT");
--  IupSetAttribute(find_txt, "VISIBLECOLUMNS", "20");
--  replace_txt = IupText("VISIBLECOLUMNS=20")
--  IupSetAttribute(replace_txt, "NAME", "REPLACE_TEXT");
--  IupSetAttribute(replace_txt, "VISIBLECOLUMNS", "20");
    find_case = IupToggle("Case Sensitive");
--  IupSetAttribute(find_case, "NAME", "FIND_CASE");
    Ihandle bt_next = IupButton("Find Next","ACTION", cb_findnext,"PADDING=10x2")
    Ihandle bt_prior = IupButton("Find Prior","ACTION", cb_findprev,"PADDING=10x2")
--  IupSetAttribute(bt_next, "PADDING", "10x2");
--  IupSetCallback(bt_next, "ACTION", cb_findnext);
--  Ihandle bt_replace = IupButton("Replace", "ACTION", Icallback("find_replace_action_cb"),"PADDING=10x2")
--  IupSetAttribute(bt_replace, "PADDING", "10x2");
--  IupSetCallback(bt_replace, "ACTION", Icallback("find_replace_action_cb"));
--  IupSetAttribute(bt_replace, "NAME", "REPLACE_BUTTON");
    Ihandle bt_close = IupButton("Close","ACTION", cb_find_close_action,"PADDING=10x2")
--  IupSetCallback(bt_close, "ACTION", Icallback("find_close_action_cb"));
--  IupSetAttribute(bt_close, "PADDING", "10x2");
--
    Ihandle box = IupVbox({IupLabel("Find What:"),
                           find_txt,
--                         IupLabel("Replace with:", "NAME=REPLACE_LABEL"),
--                         replace_txt,
                           find_case,
                           IupHbox({IupFill(),
                                    bt_next,
--                                  bt_replace,
                                    bt_prior,
                                    bt_close},
                                   "NORMALIZESIZE=HORIZONTAL")});
    IupSetAttribute(box, "MARGIN", "10x10");
    IupSetAttribute(box, "GAP", "5");

    find_dlg = IupDialog(box,"MINSIZE=x170, MAXSIZE=x170")

    IupSetAttributeHandle(find_dlg, "DEFAULTENTER", bt_next);
    IupSetAttributeHandle(find_dlg, "DEFAULTESC", bt_close);
    IupSetAttributeHandle(find_dlg, "PARENTDIALOG", dlg)
--  IupSetAttributePtr(find_dlg, "PARENTDIALOG", dlg)
    IupSetCallback(find_dlg, "CLOSE_CB", cb_find_close_action);
    IupSetAttribute(find_dlg, "TITLE", "Find");
end procedure

procedure set_find_selection(integer cY)
    object sel = getSelection(SEL_COPY)
    if sequence(sel) and length(sel)=1 then
--      str = join(str,'\n')
--      IupSetStrAttribute(find_txt, "VALUE", stringify(str));
--25/1/2018:
--      string str = sel[1]
        sequence str = sel[1]
        if not string(str) then str = utf32_to_utf8(str) end if
        if str!=IupGetAttribute(find_txt, "VALUE") then
            IupSetStrAttribute(find_txt, "VALUE", str);
?{"set_find_selection(), find_start(was ",find_start,"):=",cY}
            find_start = cY
        end if
    end if
end procedure

global procedure find_dialog()
    if find_dlg=NULL then
        create_find_dialog()
    end if
--
--  set_find_replace_visibility(find_dlg, 0);
--
    set_find_selection(CursorY)
    IupConfigDialogShow(config, find_dlg, "FindDialog")
    IupPopup(find_dlg,IUP_CURRENT,IUP_CURRENT)
    eof_message()
end procedure

function find_cb(Ihandle /*item_find*/)
    find_dialog()
    return IUP_DEFAULT;
end function
global constant cb_find = Icallback("find_cb")

global procedure F3find(integer ctrl, shift)
    if ctrl then
        if find_dlg=NULL then
            create_find_dialog()
        end if
--      CursorY = 0
        set_find_selection(0)
        find_text(0)
    else
        find_text(iff(shift?-1,+1))
    end if
    eof_message()
end procedure

global procedure F4diff(integer shift)
--?{"F4diff",shift}
    integer pos = 0, i
    sequence line 
    if shift then
        for i=CursorY to 1 by -1 do
            line = filetext[currfile][i]
            pos = length(line) and find(line[1],"<>")
            if pos!=0 then exit end if
        end for
    else
        for i=CursorY+2 to length(filetext[currfile]) do
            line = filetext[currfile][i]
            pos = length(line) and find(line[1],"<>")
            if pos!=0 then exit end if
        end for
    end if
    if pos!=0 then
        CursorX = 0
        CursorY = i-1
        selON = 0
        forceCursorOnscreen()
    else
        eof_msg = true
        eof_message()
    end if
end procedure

global function find_active()
-- should find next/prev be active? (btw, find_dlg "VISIBLE" is irrelevant)
    return find_dlg!=NULL and length(IupGetAttribute(find_txt, "VALUE"))>0
end function

--/*
function setFindDefaults(sequence blob)
    if not initFIND then createFind() end if
    setText(findtext,blob[1])
    setCheck(STOP,blob[2])
    setCheck(UP,blob[3])
    setCheck(CASE,blob[4])
    setCheck(IgnoreW,blob[5])
    return 0
end function
MacroRtns[3] = routine_id("setFindDefaults")
--*/
