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

Ihandln find_dlg = NULL
Ihandle find_txt, find_case

sequence find_txts = {}

procedure hide_find()
    IupConfigDialogClosed(config, find_dlg, "FindDialog")
    IupHide(find_dlg) -- do not destroy, just hide
end procedure

procedure find_text(integer direction)
    if find_dlg!=NULL then
        string str_to_find = IupGetAttribute(find_txt, "VALUE");
        -- ((DEV) above may need utf8_to_utf32())
        if length(str_to_find)!=0 then
            -- first, maintain the text dropdown:
            if not find(str_to_find,find_txts) then
                find_txts = append(find_txts,str_to_find)
                IupSetAttribute(find_txt,sprintf("%d",length(find_txts)),str_to_find)
            end if
            integer casesensitive = IupGetInt(find_case, "VALUE")
            integer pos = 0, i
            sequence line = filetext[currfile][CursorY+1]
            integer start = iff(selON and selY=CursorY and compare(selX,CursorX)=direction?selX:CursorX)
            if direction=1 then
                start = MapToByte(line,start)
                for i=CursorY+1 to length(filetext[currfile]) do
                    line = filetext[currfile][i]
                    pos = match(str_to_find, line, start, casesensitive)
                    if pos!=0 then exit end if
                    start = 1
                end for
            else
                start = length(line)+1-MapToByte(line,start-1)
                str_to_find = reverse(str_to_find)
                for i=CursorY+1 to 1 by -1 do
                    line = filetext[currfile][i]
                    pos = match(str_to_find, reverse(line), start, casesensitive)
                    if pos!=0 then exit end if
                    start = 1
                end for
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
                CursorX += length(str_to_find)*direction
                CursorX = ExpLength(line[1..CursorX])
                selY = CursorY
                forceCursorOnscreen()
            else
                IupMessage("End", "Search has reached end of document.");
            end if
        end if
    end if
end procedure

function find_next_action_cb(Ihandle /*ih*/)
/* this callback can be called from the main dialog also */
    find_text(1)
    return IUP_DEFAULT
end function
global constant cb_findnext = Icallback("find_next_action_cb")

function find_prior_action_cb(Ihandle /*ih*/)
    find_text(-1)
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
Ihandle config = IupGetInt(multitext, "CONFIG");
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
    IupSetAttributePtr(find_dlg, "PARENTDIALOG", dlg)
    IupSetCallback(find_dlg, "CLOSE_CB", cb_find_close_action);
    IupSetAttribute(find_dlg, "TITLE", "Find");
end procedure

procedure set_find_selection()
    object sel = getSelection(SEL_COPY)
    if sequence(sel) and length(sel)=1 then
--      str = join(str,'\n')
--      IupSetStrAttribute(find_txt, "VALUE", stringify(str));
        string str = sel[1]
        IupSetStrAttribute(find_txt, "VALUE", str);
    end if
end procedure

global procedure find_dialog()
    if find_dlg=NULL then
        create_find_dialog()
    end if
--
--  set_find_replace_visibility(find_dlg, 0);
--
    set_find_selection()
    IupConfigDialogShow(config, find_dlg, "FindDialog")
    IupPopup(find_dlg,IUP_CURRENT,IUP_CURRENT)
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
        set_find_selection()        
    end if
    find_text(iff(shift?-1,1))
end procedure

global function find_active()
    return find_dlg!=NULL and length(IupGetAttribute(find_txt, "VALUE"))>0
end function

