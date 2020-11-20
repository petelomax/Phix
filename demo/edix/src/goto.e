--
-- src/goto.e
--

Ihandln goto_dlg = NULL, 
        goto_lbl, 
        goto_txt

function goto_longest_cb(Ihandle /*bt_long*/)
integer line = 1, maxlen = 0, thislen
    for i=1 to length(filetext[currfile]) do
        thislen = ExpLength(filetext[currfile][i])
        if thislen>maxlen then
            maxlen = thislen
            line = i
        end if
    end for
    IupSetInt(goto_txt, "VALUE", line)
    IupSetInt(goto_txt, "STATUS", 1)
    return IUP_CLOSE;
end function

function goto_ok_action_cb(Ihandle /*bt_ok*/)
integer line = IupGetInt(goto_txt, "VALUE");
    if line<1 or line>=length(filetext[currfile]) then
        IupMessage("Error", "Invalid line number.");
        IupSetInt(goto_txt, "STATUS", 0)
        return IUP_DEFAULT;
    end if
    IupSetInt(goto_txt, "STATUS", 1)
    return IUP_CLOSE;
end function

function goto_cancel_action_cb(Ihandle /*bt_cancel*/)
    IupSetInt(goto_txt, "STATUS", 0);
    return IUP_CLOSE;
end function

global function goto_cb(Ihandle /*item_goto*/)
Ihandle buttons, box, bt_long, bt_ok, bt_cancel
    if goto_dlg=NULL then
        goto_lbl = IupLabel()
        goto_txt = IupText();
        IupSetAttribute(goto_txt, "MASK", IUP_MASK_UINT);   /* unsigned integer numbers only */
--      IupSetAttribute(goto_txt, "NAME", "LINE_TEXT");
        IupSetAttribute(goto_txt, "VISIBLECOLUMNS", "8");
        bt_long = IupButton("&Longest","ACTION", Icallback("goto_longest_cb"),"PADDING=10x2")
        bt_ok = IupButton("OK","ACTION", Icallback("goto_ok_action_cb"),"PADDING=10x2")
--      bt_ok = IupButton("OK");
--      IupSetAttribute(bt_ok, "PADDING", "10x2");
--      IupSetCallback(bt_ok, "ACTION", Icallback("goto_ok_action_cb"));
        bt_cancel = IupButton("Cancel","ACTION", Icallback("goto_cancel_action_cb"),"PADDING=10x2")
--      bt_cancel = IupButton("Cancel");
--      IupSetCallback(bt_cancel, "ACTION", Icallback("goto_cancel_action_cb"));
--      IupSetAttribute(bt_cancel, "PADDING", "10x2");
        buttons = IupHbox({IupFill(),bt_long,bt_ok,bt_cancel},"NORMALIZESIZE=HORIZONTAL")
        box = IupVbox({IupHbox({goto_lbl,goto_txt}),
                       buttons})
        {} = IupDestroy(IupNormalizer({goto_lbl,goto_txt},"NORMALIZE=VERTICAL"))

        IupSetAttribute(box, "MARGIN", "10x10");
        IupSetAttribute(box, "GAP", "5");

        goto_dlg = IupDialog(box);
        IupSetAttribute(goto_dlg, "TITLE", "Go To Line");
        IupSetAttribute(goto_dlg, "DIALOGFRAME", "Yes");
        IupSetAttributeHandle(goto_dlg, "DEFAULTENTER", bt_ok);
        IupSetAttributeHandle(goto_dlg, "DEFAULTESC", bt_cancel);
--      IupSetAttributeHandle(goto_dlg, "PARENTDIALOG", dlg);
        IupSetAttributePtr(goto_dlg, "PARENTDIALOG", dlg);
    end if
    IupSetStrAttribute(goto_lbl, "TITLE", "Line Number [1-%d]:", {length(filetext[currfile])});
    IupSetAttribute(goto_txt, "SELECTION", "ALL");

    IupPopup(goto_dlg, IUP_CENTERPARENT, IUP_CENTERPARENT);

    if IupGetInt(goto_txt, "STATUS")==1 then
        integer line = IupGetInt(goto_txt, "VALUE");
        CursorX = 0
        CursorY = line-1
        forceCursorOnscreen()
        paintall()
    end if

--  IupHide(goto_dlg)   -- (not needed)
--  goto_dlg = IupDestroy(goto_dlg);

    return IUP_DEFAULT;
end function
global constant cb_goto = Icallback("goto_cb")

