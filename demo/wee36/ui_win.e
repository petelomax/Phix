-- ui_win.e

include window.ew
--/*
include wee.exw
include std/text.e
include std/filesys.e
include std/sort.e
include std/machine.e
include std/dll.e
include std/get.e
--*/

-- MenuItem constants
constant
    File_New = 101,
    File_Open = 102,
    File_Save = 103,
    File_SaveAs = 104,
    File_Close = 105,
--  File_PageSetup = 106,
--  File_Print = 107,
    File_Exit = 108,
    File_Recent = 110,
    Edit_Undo = 201,
    Edit_Cut = 202,
    Edit_Copy = 203,
    Edit_Paste = 204,
    Edit_Clear = 205,
    Edit_SelectAll = 206,
    Edit_Redo = 207,
    Edit_ToggleComment = 208,
    Search_Find = 301,
    Search_Find_Next = 302,
    Search_Find_Prev = 303,
    Search_Replace = 304,
    View_Subs = 401,
    View_Error = 402,
    View_Completions = 404,
    View_Declaration = 405,
    View_GoBack = 406,
    View_SubArgs = 407,
    Run_Start = 501,
    Run_WithArgs = 502,
    Run_Arguments = 503,
    Run_Interpreter = 507,
    Run_Bind = 504,
    Run_Shroud = 505,
    Run_Translate = 506,
    Options_Font = 601,
    Options_LineNumbers = 602,
    Options_SortedSubs = 603,
    Options_Colors = 604,
    Options_LineWrap = 605,
    Options_ReopenTabs = 606,
    Options_CompleteStatements = 607,
    Options_CompleteBraces = 608,
    Options_Indent = 609,
    Help_About = 701,
    Help_Tutorial = 702,
    Help_Context = 703,
    Select_Tab = 801,
    Select_Next_Tab = 811,
    Select_Prev_Tab = 812

constant file_filters = allocate_string(
        "Euphoria files (*.ex,*.exw,*.e,*.ew)"&0&"*.EX;*.EXW;*.E;*.EW;ex.err;eu.cfg"&0&
        "Text or Document files (*.txt,*.doc)"&0&"*.TXT;*.DOC"&0&
        "All files (*.*)"&0&"*.*"&0&0)


atom hMainWnd, hFindDlg, hDlg, cls,
    hmenu, hfilemenu, heditmenu, hsearchmenu, hviewmenu,
    hrunmenu, hoptionsmenu, hhelpmenu, htabmenu,
    hstatus, htabs, hcode, hedit, htooltip,
    WM_FIND

sequence ui_hedits
         ui_hedits = {}

cls = 0
WM_FIND = 0
hMainWnd = 0  -- the main window
hFindDlg = 0
hDlg = 0
hedit = 0     -- the richedit control
hstatus = 0   -- the static control showing cursor position
htabs = 0     -- the tab control
hcode = 0     -- the listbox control for code completions
hedit = 0

constant tab_h = 24


x_pos = CW_USEDEFAULT
y_pos = CW_USEDEFAULT
x_size = CW_USEDEFAULT
y_size = CW_USEDEFAULT

--constant 
--  ansi_var_font = c_func(GetStockObject, {ANSI_VAR_FONT}),
--  ansi_fixed_font = c_func(GetStockObject, {ANSI_FIXED_FONT})

atom captionFont, smCaptionFont, menuFont, statusFont, messageFont


-- nifty shortcut, thanks Greg Haberek
function callback(sequence name, atom rid = routine_id(name))
    return call_back(rid)
end function


global procedure ui_update_window_title(sequence name)
atom result
    result = c_func(SendMessage, {hMainWnd, WM_SETTEXT, 0,
                                  alloc_string(name & " ~ " & window_title)})
    free_strings()
end procedure

global procedure ui_update_status(sequence status)
atom txt, junk
    txt = allocate_string(status)
    junk = c_func(SendMessage, {hstatus, WM_SETTEXT, 0, txt})
    free(txt)
end procedure
r_ui_update_status = routine_id("ui_update_status")

global procedure ui_refresh_file_menu(sequence recent_files)
atom junk, count
    count = c_func(GetMenuItemCount, {hfilemenu})-7
    if count=0 and length(recent_files)>0 then
        junk = c_func(AppendMenu, {hfilemenu, MF_BYPOSITION+MF_SEPARATOR, 0, 0})
    end if
    for i=1 to length(recent_files) do
        if i>=count then
            junk = c_func(AppendMenu, {hfilemenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                                       File_Recent+i, alloc_string(recent_files[i])})
        else
            junk = c_func(ModifyMenu, {hfilemenu, i+7, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                                       File_Recent+i, alloc_string(recent_files[i])})
        end if
    end for
    free_strings()
end procedure

global procedure ui_update_tab_name(integer tab, sequence name)
atom junk, tcitem
    tcitem = allocate_pack("dddsiip", {TCIF_TEXT, 0, 0, name, 0, 0, 0})
    junk = c_func(SendMessage, {htabs, TCM_SETITEMA, tab-1, tcitem})
    free(tcitem)
    free_strings()
end procedure


-- select the tab and show the hedit
global procedure ui_select_tab(integer tab)
atom junk, rect
--  integer w, h

    -- change tab, and highlight it to make it stand out
    junk = c_func(SendMessage, {htabs, TCM_HIGHLIGHTITEM,
                                c_func(SendMessage, {htabs, TCM_GETCURSEL, 0, 0}), 0})
    junk = c_func(SendMessage, {htabs, TCM_SETCURSEL, tab-1, 0})
    junk = c_func(SendMessage, {htabs, TCM_HIGHLIGHTITEM, tab-1, 1})

    if hedit then
        -- hide the previous hedit
        c_proc(ShowWindow, {hedit, SW_HIDE})
    end if

    hedit = ui_hedits[tab]

    -- update the richedit control to the window size
    rect = allocate(16)
    c_proc(GetClientRect, {hMainWnd, rect})
    c_proc(MoveWindow, {hedit, 0, tab_h, peek4u(rect+8), peek4u(rect+12)-tab_h, 1})
    free(rect)
    c_proc(ShowWindow, {hedit, SW_SHOW})
    junk = c_func(SetFocus, {hedit})

    junk = allocate_pack("zdppddddps", { -- TOOLINFO
                                        TTF_IDISHWND+TTF_SUBCLASS, -- uFlags
                                        hMainWnd, -- hwnd
                                        htabs, -- uId
                                        0,0,0,0, -- rect
                                        0, -- hinst
                                        file_name -- lpszText
                                       })
    if c_func(SendMessage, {htooltip, TTM_UPDATETIPTEXT, 0, junk}) then end if
    free(junk)

end procedure

global function ui_new_tab(sequence name)
atom junk, tcitem, tab, hedit
    tab = c_func(SendMessage, {htabs, TCM_GETITEMCOUNT, 0, 0})

    tcitem = allocate_pack("dddsiip", {TCIF_TEXT, 0, 0, name, 0, 0, 0})
    junk = c_func(SendMessage, {htabs, TCM_INSERTITEMA, tab+(tab!=0), tcitem})
    free(tcitem)
    free_strings()

    hedit = CreateWindow({
                          0,
                          "Scintilla",
                          "",
                          {WS_CHILD, WS_VISIBLE, WS_TABSTOP, WS_CLIPCHILDREN},
                          0,0,0,0,  -- position and size set later by hMainWnd
                          hMainWnd, NULL, 0, NULL})

    -- save the window handle
    ui_hedits = append(ui_hedits, hedit)
    return c_func(SendMessage, {hedit, SCI_GETDIRECTPOINTER, 0, 0})
end function

global procedure ui_close_tab(integer tab)
atom junk

-- get the edit window handle and destroy it
    junk = c_func(DestroyWindow, {hedit})
    hedit = 0
    -- delete the tab
    junk = c_func(SendMessage, {htabs, TCM_DELETEITEM, tab-1, 0})
    -- remove the window handle
    ui_hedits = ui_hedits[1..tab-1] & ui_hedits[tab+1..$]
end procedure

procedure rightclick_tab()
atom point
integer tab, x, y
    point = allocate(12)
    if c_func(GetCursorPos, {point}) then end if
    x = peek4u(point)
    y = peek4u(point+4)
    if c_func(ScreenToClient, {hMainWnd, point}) then end if
    tab = c_func(SendMessage, {htabs, TCM_HITTEST, 0, point})
    if tab>=0 then
        call_proc(r_select_tab,{tab+1})
        if c_func(TrackPopupMenu, {htabmenu, TPM_LEFTALIGN, x, y, 0, hMainWnd, 0}) then end if
    end if
    free(point)
end procedure


integer current_filter
current_filter = 1

global function ui_get_open_file_name(integer multiple = 1)
sequence temp
    temp = GetOpenFileName({hMainWnd, 0, file_filters, current_filter,
                            "", 0, 0,
                            OFN_EXPLORER+OFN_PATHMUSTEXIST+OFN_FILEMUSTEXIST+OFN_HIDEREADONLY
                            +(multiple!=0)*OFN_ALLOWMULTISELECT
            , 0})
    if length(temp)<2 then return "" end if
    current_filter = temp[1]
    return temp[2]
end function

global function ui_get_save_file_name(sequence file_name)
sequence temp
    temp = GetSaveFileName({hMainWnd, 0, file_filters, current_filter,
                            file_name, 0, 0, OFN_EXPLORER+OFN_PATHMUSTEXIST+OFN_HIDEREADONLY, 0})
    if length(temp)<2 then return "" end if
    current_filter = temp[1]
    return temp[2]
end function

-- returns yes=1 no=0
global function ui_message_box_yes_no(sequence title, sequence message)
atom result
    result = c_func(MessageBox, {hMainWnd,
                                 alloc_string(message),
                                 alloc_string(title),
                                 or_all({MB_APPLMODAL, MB_ICONINFORMATION, MB_YESNO})})
    free_strings()
    return result=IDYES
end function

-- returns yes=1 no=0 cancel=-1
global function ui_message_box_yes_no_cancel(sequence title, sequence message)
atom result
    result = c_func(MessageBox, {hMainWnd,
                                 alloc_string(message),
                                 alloc_string(title),
                                 or_all({MB_APPLMODAL, MB_ICONINFORMATION, MB_YESNOCANCEL})})
    free_strings()
    return (result=IDYES)-(result=IDCANCEL)
end function

global function ui_message_box_error(sequence title, sequence message)
atom result
    result = c_func(MessageBox, {hMainWnd,
                                 alloc_string(message),
                                 alloc_string(title),
                                 or_all({MB_APPLMODAL, MB_ICONSTOP, MB_OK})})
    free_strings()
    return result
end function

procedure get_window_size()
atom rect
    rect = allocate(16)
    c_proc(GetWindowRect, {hMainWnd, rect})
    x_pos = peek4u(rect)
    y_pos = peek4u(rect+4)
    x_size = peek4u(rect+8)-x_pos
    y_size = peek4u(rect+12)-y_pos
    free(rect)
end procedure


procedure about_box()
atom result
    result = c_func(MessageBox, {hMainWnd,
                                 alloc_string(window_title&"\n\nVersion "&weeversion&"\n\nBy "&author),
                                 alloc_string("About "&window_title),
                                 or_all({MB_APPLMODAL, MB_ICONINFORMATION, MB_OK})})
    free_strings()
end procedure

constant
    DialogListID = 1000,
    DialogLabelID = 1001,
    DialogEditID = 1002,
    DialogOpenID = 1003
sequence subs

function SubsDialogProc(atom hdlg, atom iMsg, atom wParam, atom lParam)
atom junk, hList, pos
--  sequence text, word, tmp
sequence text, word

--/**/ if lParam then end if -- suppress warnings

    --? {hdlg, iMsg, wParam, HIWORD(lParam), LOWORD(lParam)}

    if iMsg=WM_INITDIALOG then
        junk = c_func(SendMessage, {c_func(GetDlgItem, {hdlg, IDOK}),
                                    WM_SETFONT, captionFont, 0})
        junk = c_func(SendMessage, {c_func(GetDlgItem, {hdlg, IDCANCEL}),
                                    WM_SETFONT, captionFont, 0})
        hList = c_func(GetDlgItem, {hdlg, DialogListID})
        junk = c_func(SendMessage, {hList, WM_SETFONT, messageFont, 0})
        text = get_edit_text()
        pos = get_pos()
        word = word_pos(text, pos)
        subs = get_subroutines(wee_parse(text, file_name))
        if sorted_subs then
            subs = sort(subs)
        end if

        for i=1 to length(subs) do
            junk = c_func(SendMessage, {hList, LB_ADDSTRING, 0, alloc_string(subs[i][1])})
            if equal(word[1], subs[i][1]) then
                junk = c_func(SendMessage, {hList, LB_SETCURSEL, i-1, 0})
            end if
        end for
        free_strings()

    elsif iMsg=WM_COMMAND then
        if LOWORD(wParam)=IDOK then
            hList = c_func(GetDlgItem, {hdlg, DialogListID})
            pos = c_func(SendMessage, {hList, LB_GETCURSEL, 0, 0})+1
            if pos>=1 and pos<=length(subs) then
                word = subs[pos][1]
                pos = subs[pos][2]-1
              --printf(1, "%s: pos=%d line=%d\n", {word, pos, 
              --    c_func(SendMessage, {hedit, SCI_LINEFROMPOSITION, pos, 0})})
                junk = c_func(SendMessage, {hedit, SCI_SETSEL, pos, pos+length(word)})
                set_top_line(-1) -- set the top visible line to the current line
            end if
            junk = c_func(EndDialog, {hdlg, 1})
            return 1
        elsif LOWORD(wParam)=IDCANCEL then
            junk = c_func(EndDialog, {hdlg, 0})
            return 1
        end if

    elsif iMsg=WM_CLOSE then --closing dialog
        junk = c_func(EndDialog, {hdlg, 0})
    end if

    return 0
end function

constant SubsDialogCallback = callback("SubsDialogProc")

-- used for struct DLGITEMTEMPLATE
constant
    DIALOG_CLASS_BUTTON = #0080,
    DIALOG_CLASS_EDIT = #0081,
    DIALOG_CLASS_STATIC = #0082,
    DIALOG_CLASS_LIST = #0083,
--  DIALOG_CLASS_SCROLLBAR = #0084,
    DIALOG_CLASS_COMBOBOX = #0085

procedure view_subroutines()
atom junk
    junk = DialogBoxIndirectParam(c_func(GetModuleHandle, {NULL}), {
        --DLGTEMPLATE{style, exstyle, x, y, cx, cy, menu, wndclass, text}
                                                                    {{WS_POPUP, WS_BORDER, WS_SYSMENU, DS_MODALFRAME, WS_CAPTION}, 0,
                                                                     50,50, 100,124, 0, 0, "Subroutines"},
                                                                    --DLGITEMTEMPLATE{style, exstyle, x, y, cx, cy, id, dlgclass, text}
                                                                    {{WS_CHILD, WS_VISIBLE, WS_VSCROLL}, WS_EX_CLIENTEDGE,
                                                                     4,4, 92,110, DialogListID, DIALOG_CLASS_LIST, "ListBox"},
                                                                    {{WS_CHILD, WS_VISIBLE, BS_PUSHBUTTON}, 0,
                                                                     4,108, 44,12, IDCANCEL, DIALOG_CLASS_BUTTON, "Cancel"},
                                                                    {{WS_CHILD, WS_VISIBLE, BS_DEFPUSHBUTTON}, 0,
                                                                     52,108, 44,12, IDOK, DIALOG_CLASS_BUTTON, "OK"}
                                                                   },
                                  hMainWnd,
                                  SubsDialogCallback,
                                  0)
    subs = {}
end procedure


sequence err

function ViewErrorProc(atom hdlg, atom iMsg, atom wParam, atom lParam)
--  atom junk, hList, hLabel, pos, len, item
atom junk, hList, hLabel, pos

--/**/ if lParam then end if -- suppress warnings

    --? {hdlg, iMsg, wParam, HIWORD(lParam), LOWORD(lParam)}

    if iMsg=WM_INITDIALOG then
        junk = c_func(SendMessage, {c_func(GetDlgItem, {hdlg, IDOK}),
                                    WM_SETFONT, captionFont, 0})
        junk = c_func(SendMessage, {c_func(GetDlgItem, {hdlg, IDCANCEL}),
                                    WM_SETFONT, captionFont, 0})
        junk = c_func(SendMessage, {c_func(GetDlgItem, {hdlg, IDRETRY}),
                                    WM_SETFONT, captionFont, 0})
        hList = c_func(GetDlgItem, {hdlg, DialogListID})
        junk = c_func(SendMessage, {hList, WM_SETFONT, messageFont, 0})
        hLabel = c_func(GetDlgItem, {hdlg, DialogLabelID})
        junk = c_func(SendMessage, {hLabel, WM_SETFONT, messageFont, 0})

        err = get_ex_err()
        if length(err)=0    then
            junk = c_func(EndDialog, {hdlg, 0})
            return 1
        end if
        junk = c_func(SendMessage, {hLabel, WM_SETTEXT, 0, alloc_string(err[2])})

        for i=3 to length(err) do
            junk = c_func(SendMessage, {hList, LB_ADDSTRING, 0, alloc_string(err[i])})
        end for
        junk = c_func(SendMessage, {hList, LB_SETCURSEL, 0, 0})
        junk = c_func(SetFocus, {hList})

    elsif iMsg=WM_COMMAND then
        if LOWORD(wParam)=IDOK then
            hList = c_func(GetDlgItem, {hdlg, DialogListID})
            pos = c_func(SendMessage, {hList, LB_GETCURSEL, 0, 0})
            if pos!= -1 then
                call_proc(r_goto_error,{err, pos+1})
            end if
            junk = c_func(EndDialog, {hdlg, 1})
            return 1
        elsif LOWORD(wParam)=IDRETRY then
--          junk = open_file(ex_err_name, 1)
            junk = call_func(r_open_file,{ex_err_name, 1})
            junk = c_func(EndDialog, {hdlg, 0})
            return 1
        elsif LOWORD(wParam)=IDCANCEL then
            junk = c_func(EndDialog, {hdlg, 0})
            return 1
        end if

    elsif iMsg=WM_CLOSE then --closing dialog
        junk = c_func(EndDialog, {hdlg, 0})
    end if

    return 0
end function

constant ViewErrorCallback = callback("ViewErrorProc")

global procedure ui_view_error()
integer junk
    junk = DialogBoxIndirectParam(c_func(GetModuleHandle, {NULL}), {
        --DLGTEMPLATE{style, exstyle, x, y, cx, cy, menu, wndclass, text}
                                                                    {{WS_POPUP, WS_CAPTION, WS_SYSMENU, DS_MODALFRAME}, 0,
                                                                     50, 50, 200,150, 0, 0, "View Error"},
                                                                    --DLGITEMTEMPLATE{style, exstyle, x, y, cx, cy, id, dlgclass, text}
                                                                    {{WS_CHILD, WS_VISIBLE, WS_VSCROLL}, WS_EX_CLIENTEDGE,
                                                                     4,24, 192,108, DialogListID, DIALOG_CLASS_LIST, "ListBox"},
                                                                    {{WS_CHILD, WS_VISIBLE}, 0,
                                                                     4,4, 192,20, DialogLabelID, DIALOG_CLASS_STATIC, "Static"},
                                                                    {{WS_CHILD, WS_VISIBLE, BS_PUSHBUTTON}, 0,
                                                                     54,132, 44,12, IDCANCEL, DIALOG_CLASS_BUTTON, "Cancel"},
                                                                    {{WS_CHILD, WS_VISIBLE, BS_PUSHBUTTON}, 0,
                                                                     102,132, 44,12, IDRETRY, DIALOG_CLASS_BUTTON, "Open ex.err"},
                                                                    {{WS_CHILD, WS_VISIBLE, BS_DEFPUSHBUTTON}, 0,
                                                                     150,132, 44,12, IDOK, DIALOG_CLASS_BUTTON, "Goto Error"}
                                                                   },
                                  hMainWnd,
                                  ViewErrorCallback,
                                  0)
end procedure

global procedure ui_show_help(sequence html)
    -- TODO
--/**/   if sequence(html) then end if -- suppress warnings
    ?"ui_show_help"
end procedure

global procedure ui_show_uri(sequence uri)
atom result
    -- TODO: get default browser from
    --       HKEY_LOCAL_MACHINE\SOFTWARE\Classes\http\shell\open\command 
    -- note: just opening the uri won't work, since shellexecute will
    -- strip off any #anchor text, so using iexplore.exe for now
    result = c_func(ShellExecute, {
                                   hMainWnd, NULL,
                                   alloc_string("iexplore.exe"),
                                   alloc_string(uri),
                                   NULL,
                                   SW_SHOWNORMAL})
    free_strings()
end procedure


procedure process_find(atom struc)
atom flags, junk, result
integer backward

    flags = GetFindFlags(struc)
    if and_bits(flags, FR_DIALOGTERM) then
        find_phrase = GetFindWhat(struc)
        replace_phrase = GetReplaceWith(struc)
        --puts(1, find_phrase&" "&replace_phrase&"\n")
        hFindDlg = 0
        return
    end if
    --printf(1, "flags=%x\n", {flags})

    backward = not and_bits(flags, FR_DOWN)
    result = 0
    if and_bits(flags, FR_MATCHCASE) then
        result += SCFIND_MATCHCASE
    end if
    if and_bits(flags, FR_WHOLEWORD) then
        result += SCFIND_WHOLEWORD
    end if
    junk = c_func(SendMessage, {hedit, SCI_SETSEARCHFLAGS, result, 0})

    if and_bits(flags, FR_FINDNEXT) then
        if search_find(GetFindWhat(struc), backward)=0 then
            result = c_func(MessageBox, {hFindDlg,
                                         alloc_string("Unable to find a match."),
                                         alloc_string("Find"),
                                         or_all({MB_APPLMODAL, MB_ICONINFORMATION, MB_OK})})
        end if
    elsif and_bits(flags, FR_REPLACE) then
        result = search_replace(GetReplaceWith(struc))
        if search_find(GetFindWhat(struc), backward)=0 and result=0 then
            result = c_func(MessageBox, {hFindDlg,
                                         alloc_string("Unable to find a match."),
                                         alloc_string("Replace"),
                                         or_all({MB_APPLMODAL, MB_ICONINFORMATION, MB_OK})})
        end if
    elsif and_bits(flags, FR_REPLACEALL) then
        result = search_replace_all(GetFindWhat(struc), GetReplaceWith(struc))
        if result then
            result = c_func(MessageBox, {hFindDlg,
                                         alloc_string(sprintf("%d replacements.", {result})),
                                         alloc_string("Replace All"),
                                         or_all({MB_APPLMODAL, MB_ICONINFORMATION, MB_OK})})
        else
            result = c_func(MessageBox, {hFindDlg,
                                         alloc_string("Unable to find a match."),
                                         alloc_string("Replace All"),
                                         or_all({MB_APPLMODAL, MB_ICONINFORMATION, MB_OK})})
        end if
    end if
    free_strings()
end procedure


function RunArgsProc(atom hdlg, atom iMsg, atom wParam, atom lParam)
atom junk, hEdit, len, buf

--/**/ if lParam then end if -- suppress warnings

    --? {hdlg, iMsg, wParam, HIWORD(lParam), LOWORD(lParam)}

    if iMsg=WM_INITDIALOG then
        junk = c_func(SendMessage, {c_func(GetDlgItem, {hdlg, IDOK}),
                                    WM_SETFONT, captionFont, 0})
        junk = c_func(SendMessage, {c_func(GetDlgItem, {hdlg, IDCANCEL}),
                                    WM_SETFONT, captionFont, 0})
        hEdit = c_func(GetDlgItem, {hdlg, DialogEditID})
        junk = c_func(SendMessage, {hEdit, WM_SETFONT, messageFont, 0})

        junk = c_func(SendMessage, {hEdit, WM_SETTEXT, 0, alloc_string(get_tab_arguments())})
        junk = c_func(SetFocus, {hEdit})
        junk = c_func(SendMessage, {hEdit, EM_SETSEL, 0, -1})

    elsif iMsg=WM_COMMAND then
        if LOWORD(wParam)=IDOK then
            hEdit = c_func(GetDlgItem, {hdlg, DialogEditID})
            len = 1+c_func(SendMessage, {hEdit, WM_GETTEXTLENGTH, 0, 0})
            buf = allocate(len)
            junk = c_func(SendMessage, {hEdit, WM_GETTEXT, len, buf})
            junk = c_func(EndDialog, {hdlg, buf})
            return 1
        elsif LOWORD(wParam)=IDCANCEL then
            junk = c_func(EndDialog, {hdlg, 0})
            return 1
        end if

    elsif iMsg=WM_CLOSE then --closing dialog
        junk = c_func(EndDialog, {hdlg, 0})
    end if

    return 0
end function

constant RunArgsCallback = callback("RunArgsProc")

global procedure run_arguments()
integer junk
    junk = DialogBoxIndirectParam(c_func(GetModuleHandle, {NULL}), {
        --DLGTEMPLATE{style, exstyle, x, y, cx, cy, menu, wndclass, text}
                                                                    {{WS_POPUP, WS_CAPTION, WS_SYSMENU, DS_MODALFRAME}, 0,
                                                                     50,50, 200,36, 0, 0, "Set Arguments"},
                                                                    --DLGITEMTEMPLATE{style, exstyle, x, y, cx, cy, id, dlgclass, text}
                                                                    {{WS_CHILD, WS_VISIBLE, WS_BORDER, ES_AUTOHSCROLL}, 0,
                                                                     4,4, 192,12, DialogEditID, DIALOG_CLASS_EDIT, "tab arguments"},
                                                                    {{WS_CHILD, WS_VISIBLE, BS_PUSHBUTTON}, 0,
                                                                     104,20, 44,12, IDCANCEL, DIALOG_CLASS_BUTTON, "Cancel"},
                                                                    {{WS_CHILD, WS_VISIBLE, BS_DEFPUSHBUTTON}, 0,
                                                                     152,20, 44,12, IDOK, DIALOG_CLASS_BUTTON, "OK"}
                                                                   },
                                  hMainWnd,
                                  RunArgsCallback,
                                  0)
    if junk then
        set_tab_arguments(peek_string(junk))
        free(junk)
    end if
end procedure


function RunInterpreterProc(atom hdlg, atom iMsg, atom wParam, atom lParam)
atom junk, hEdit, hLabel, len, buf
sequence interpreters
integer index

--/**/ if lParam then end if -- suppress warnings

    --? {hdlg, iMsg, wParam, HIWORD(lParam), LOWORD(lParam)}

    if iMsg=WM_INITDIALOG then
        junk = c_func(SendMessage, {c_func(GetDlgItem, {hdlg, IDOK}),
                                    WM_SETFONT, captionFont, 0})
        junk = c_func(SendMessage, {c_func(GetDlgItem, {hdlg, IDCANCEL}),
                                    WM_SETFONT, captionFont, 0})
        junk = c_func(SendMessage, {c_func(GetDlgItem, {hdlg, DialogOpenID}),
                                    WM_SETFONT, captionFont, 0})
        hLabel = c_func(GetDlgItem, {hdlg, DialogLabelID})
        junk = c_func(SendMessage, {hLabel, WM_SETFONT, messageFont, 0})
        hEdit = c_func(GetDlgItem, {hdlg, DialogEditID})
        junk = c_func(SendMessage, {hEdit, WM_SETFONT, messageFont, 0})

        interpreters = get_interpreters()
        for i=1 to length(interpreters) do
            junk = c_func(SendMessage, {hEdit, CB_ADDSTRING, 0, alloc_string(interpreters[i])})
        end for
        index = find(interpreter, interpreters)
        if index then
            junk = c_func(SendMessage, {hEdit, CB_SETCURSEL, index-1, 0})
        end if

        junk = c_func(SetFocus, {hEdit})
        junk = c_func(SendMessage, {hEdit, EM_SETSEL, 0, -1})

    elsif iMsg=WM_COMMAND then
        if LOWORD(wParam)=DialogOpenID then
            sequence temp = GetOpenFileName({hMainWnd, 0, alloc_string("Executable Files (*.exe)"&0&"*.EXE"&0&0), 0,
                                             "", 0, 0,
                                             OFN_EXPLORER+OFN_PATHMUSTEXIST+OFN_FILEMUSTEXIST+OFN_HIDEREADONLY
                    , 0})
            if length(temp) then
                temp = temp[2] -- just the filename
                hEdit = c_func(GetDlgItem, {hdlg, DialogEditID})
                junk = c_func(SendMessage, {hEdit, CB_SELECTSTRING, -1, alloc_string(temp)})
                if junk=CB_ERR then
                    -- add a new filename since it wasn't found
                    junk = c_func(SendMessage, {hEdit, CB_INSERTSTRING, 0, alloc_string(temp)})
                    junk = c_func(SendMessage, {hEdit, CB_SETCURSEL, 0, 0})
                end if
            end if
            free_strings()
        elsif LOWORD(wParam)=IDOK then
            hEdit = c_func(GetDlgItem, {hdlg, DialogEditID})
            len = 1+c_func(SendMessage, {hEdit, WM_GETTEXTLENGTH, 0, 0})
            buf = allocate(len)
            junk = c_func(SendMessage, {hEdit, WM_GETTEXT, len, buf})
            junk = c_func(EndDialog, {hdlg, buf})
            return 1
        elsif LOWORD(wParam)=IDCANCEL then
            junk = c_func(EndDialog, {hdlg, 0})
            return 1
        end if

    elsif iMsg=WM_CLOSE then --closing dialog
        junk = c_func(EndDialog, {hdlg, 0})
    end if

    return 0
end function

constant RunInterpreterCallback = callback("RunInterpreterProc")

global procedure run_interpreter()
integer junk
    junk = DialogBoxIndirectParam(c_func(GetModuleHandle, {NULL}), {
        --DLGTEMPLATE{style, exstyle, x, y, cx, cy, menu, wndclass, text}
                                                                    {{WS_POPUP, WS_CAPTION, WS_SYSMENU, DS_MODALFRAME}, 0,
                                                                     50,50, 200,56, 0, 0, "Set Interpreter"},
                                                                    --DLGITEMTEMPLATE{style, exstyle, x, y, cx, cy, id, dlgclass, text}
                                                                    {{WS_CHILD, WS_VISIBLE}, 0,
                                                                     4,4, 192,20, DialogLabelID, DIALOG_CLASS_STATIC,
                                                                     "Enter an interpreter to use to run programs, or select one from the list.\n"&
                                                                     "Leave blank to use the default first item in the list."},
                                                                    {{WS_CHILD, WS_VISIBLE, WS_BORDER, WS_HSCROLL, CBS_AUTOHSCROLL, CBS_DROPDOWN}, 0,
                                                                     4,24, 176,12, DialogEditID, DIALOG_CLASS_COMBOBOX, "interpreter"},
                                                                    {{WS_CHILD, WS_VISIBLE, BS_PUSHBUTTON}, 0,
                                                                     184,24, 12,12, DialogOpenID, DIALOG_CLASS_BUTTON, "..."},
                                                                    {{WS_CHILD, WS_VISIBLE, BS_PUSHBUTTON}, 0,
                                                                     104,40, 44,12, IDCANCEL, DIALOG_CLASS_BUTTON, "Cancel"},
                                                                    {{WS_CHILD, WS_VISIBLE, BS_DEFPUSHBUTTON}, 0,
                                                                     152,40, 44,12, IDOK, DIALOG_CLASS_BUTTON, "OK"}
                                                                   },
                                  hMainWnd,
                                  RunInterpreterCallback,
                                  0)
    if junk then
        interpreter = peek_string(junk)
        free(junk)
    end if
end procedure

procedure run_start(integer with_args)
atom result
sequence args

    -- save, no confirm 
    if call_func(r_save_if_modified,{0})=0 or length(file_name)=0 then
        return -- cancelled, or no name
    end if

    run_file_name = file_name
    reset_ex_err()

    args = quote_spaces(run_file_name)
    if with_args then
        if length(get_tab_arguments())=0 then
            run_arguments()
        end if
        args &= ' ' & get_tab_arguments()
    end if

    -- if interpreter is set, use it
    -- elsif Eu file and eu.cfg points to eudir use it (.exw,.ew->euiw,exw, .ex,.e->eui,ex)
    -- else let OS handle it
    -- and if that fails use EUDIR or include_paths

    result = c_func(ShellExecute, {
                                   hMainWnd, NULL,
                                   alloc_string(get_eu_bin("")),
                                   alloc_string(args),
                                   alloc_string(dirname(run_file_name)),
                                   SW_SHOWNORMAL})
    if result<33 then
        -- shellexecute failed
        if match(".exw", lower(run_file_name)) or
           match(".ew", lower(run_file_name)) then
--          system(quote_spaces(get_eu_bin("euiw")) & ' ' & args)
            system(quote_spaces(get_eu_bin("pw")) & ' ' & args)
        else
--          system(quote_spaces(get_eu_bin("eui")) & ' ' & args)
            system(quote_spaces(get_eu_bin("p")) & ' ' & args)
        end if
    end if
    free_strings()
end procedure

procedure run_convert(sequence name, sequence cmd)
atom result

    -- save, no confirm 
    if call_func(r_save_if_modified,{0})=0 or length(file_name)=0 then
        return -- cancelled, or no name
    end if
    result = c_func(ShellExecute, {
                                   hMainWnd, NULL,
                                   alloc_string(get_eu_bin(cmd)),
                                   alloc_string(quote_spaces(file_name)),
                                   alloc_string(dirname(file_name)),
                                   SW_SHOWNORMAL})
    if result<33 then
        if ui_message_box_error(name, cmd & " failed") then end if
    end if
end procedure

function get_appdata_path()
atom junk, path
sequence appdata
    path = allocate(MAX_PATH)
    junk = c_func(SHGetFolderPath, {
                                    NULL,
                                    CSIDL_LOCAL_APPDATA+CSIDL_FLAG_CREATE,
                                    NULL,
                                    0,
                                    path})
    appdata = peek_string(path)
    free(path)
    return appdata
end function

constant wee_conf_filename = get_appdata_path() & "\\wee_conf.txt"


function ColorsDialogProc(atom hdlg, atom iMsg, atom wParam, atom lParam)
--  atom junk, hEdit, len, buf
atom junk
integer id

--/**/if lParam then end if -- suppress warnings

    --? {hdlg, iMsg, wParam, HIWORD(lParam), LOWORD(lParam)}

    if iMsg=WM_INITDIALOG then
        junk = c_func(SendMessage, {c_func(GetDlgItem, {hdlg, IDOK}),
                                    WM_SETFONT, captionFont, 0})
        for i=1000 to 1016 do
            junk = c_func(SendMessage, {c_func(GetDlgItem, {hdlg, i}),
                                        WM_SETFONT, captionFont, 0})
        end for
        for i=0 to 6 do
            if and_bits(bold_flags, power(2, i)) then
                junk = BST_CHECKED
            else
                junk = BST_UNCHECKED
            end if
            junk = c_func(SendMessage, {c_func(GetDlgItem, {hdlg, 1010+i}),
                                        BM_SETCHECK, junk, 0})
        end for

    elsif iMsg=WM_COMMAND then
        id = LOWORD(wParam)
        if id=IDOK then
            junk = c_func(EndDialog, {hdlg, 1})
            return 1
        elsif id=IDCANCEL then
            junk = c_func(EndDialog, {hdlg, 0})
            return 1
        elsif id=1000 then
            normal_color = ChooseColor(hMainWnd, normal_color)
            reinit_all_edits()
            return 0
        elsif id=1001 then
            background_color = ChooseColor(hMainWnd, background_color)
            reinit_all_edits()
            return 0
        elsif id=1002 then
            comment_color = ChooseColor(hMainWnd, comment_color)
            reinit_all_edits()
            return 0
        elsif id=1003 then
            string_color = ChooseColor(hMainWnd, string_color)
            reinit_all_edits()
            return 0
        elsif id=1004 then
            keyword_color = ChooseColor(hMainWnd, keyword_color)
            reinit_all_edits()
            return 0
        elsif id=1005 then
            builtin_color = ChooseColor(hMainWnd, builtin_color)
            reinit_all_edits()
            return 0
        elsif id=1006 then
            number_color = ChooseColor(hMainWnd, number_color)
            reinit_all_edits()
            return 0
        elsif id=1007 then
            bracelight_color = ChooseColor(hMainWnd, bracelight_color)
            reinit_all_edits()
            return 0
        elsif id=1008 then
            linenumber_color = ChooseColor(hMainWnd, linenumber_color)
            reinit_all_edits()
            return 0
        elsif id>=1010 and id<=1016 then
            junk = power(2, id-1010)
            if and_bits(bold_flags, junk) then
                bold_flags = and_bits(bold_flags, not_bits(junk))
                junk = BST_UNCHECKED
            else
                bold_flags = or_bits(bold_flags, junk)
                junk = BST_CHECKED
            end if
            junk = c_func(SendMessage, {
                                        c_func(GetDlgItem, {hdlg, id}), BM_SETCHECK, junk, 0})
            reinit_all_edits()
            return 0
        end if

    elsif iMsg=WM_CLOSE then --closing dialog
        junk = c_func(EndDialog, {hdlg, 0})
    end if

    return 0
end function

constant ColorsDialogCallback = callback("ColorsDialogProc")

global procedure choose_colors()
integer junk
    junk = DialogBoxIndirectParam(c_func(GetModuleHandle, {NULL}), {
        --DLGTEMPLATE{style, exstyle, x, y, cx, cy, menu, wndclass, text}
                                                                    {{WS_POPUP, WS_CAPTION, WS_SYSMENU, DS_MODALFRAME}, 0,
                                                                     50,50, 100,166, 0, 0, "Colors"},
                                                                    --DLGITEMTEMPLATE{style, exstyle, x, y, cx, cy, id, dlgclass, text}
                                                                    {{WS_CHILD, WS_VISIBLE, BS_PUSHBUTTON}, 0,
                                                                     4,4, 64,12, 1000, DIALOG_CLASS_BUTTON, "Normal"},
                                                                    {{WS_CHILD, WS_VISIBLE, BS_PUSHBUTTON}, 0,
                                                                     4,20, 64,12, 1001, DIALOG_CLASS_BUTTON, "Background"},
                                                                    {{WS_CHILD, WS_VISIBLE, BS_PUSHBUTTON}, 0,
                                                                     4,36, 64,12, 1002, DIALOG_CLASS_BUTTON, "Comment"},
                                                                    {{WS_CHILD, WS_VISIBLE, BS_PUSHBUTTON}, 0,
                                                                     4,52, 64,12, 1003, DIALOG_CLASS_BUTTON, "String"},
                                                                    {{WS_CHILD, WS_VISIBLE, BS_PUSHBUTTON}, 0,
                                                                     4,68, 64,12, 1004, DIALOG_CLASS_BUTTON, "Keyword"},
                                                                    {{WS_CHILD, WS_VISIBLE, BS_PUSHBUTTON}, 0,
                                                                     4,84, 64,12, 1005, DIALOG_CLASS_BUTTON, "Built-in"},
                                                                    {{WS_CHILD, WS_VISIBLE, BS_PUSHBUTTON}, 0,
                                                                     4,100, 64,12, 1006, DIALOG_CLASS_BUTTON, "Number"},
                                                                    {{WS_CHILD, WS_VISIBLE, BS_PUSHBUTTON}, 0,
                                                                     4,116, 64,12, 1007, DIALOG_CLASS_BUTTON, "Brace Highlight"},
                                                                    {{WS_CHILD, WS_VISIBLE, BS_PUSHBUTTON}, 0,
                                                                     4,132, 64,12, 1008, DIALOG_CLASS_BUTTON, "Line Number"},


                                                                    {{WS_CHILD, WS_VISIBLE, BS_CHECKBOX}, 0,
                                                                     72,4, 32,12, 1010, DIALOG_CLASS_BUTTON, "Bold"},
                                                                    {{WS_CHILD, WS_VISIBLE, BS_CHECKBOX}, 0,
                                                                     72,36, 32,12, 1011, DIALOG_CLASS_BUTTON, "Bold"},
                                                                    {{WS_CHILD, WS_VISIBLE, BS_CHECKBOX}, 0,
                                                                     72,52, 32,12, 1012, DIALOG_CLASS_BUTTON, "Bold"},
                                                                    {{WS_CHILD, WS_VISIBLE, BS_CHECKBOX}, 0,
                                                                     72,68, 32,12, 1013, DIALOG_CLASS_BUTTON, "Bold"},
                                                                    {{WS_CHILD, WS_VISIBLE, BS_CHECKBOX}, 0,
                                                                     72,84, 32,12, 1014, DIALOG_CLASS_BUTTON, "Bold"},
                                                                    {{WS_CHILD, WS_VISIBLE, BS_CHECKBOX}, 0,
                                                                     72,100, 32,12, 1015, DIALOG_CLASS_BUTTON, "Bold"},
                                                                    {{WS_CHILD, WS_VISIBLE, BS_CHECKBOX}, 0,
                                                                     72,116, 32,12, 1016, DIALOG_CLASS_BUTTON, "Bold"},

                                                                    {{WS_CHILD, WS_VISIBLE, BS_DEFPUSHBUTTON}, 0,
                                                                     4,150, 44,12, IDOK, DIALOG_CLASS_BUTTON, "Close"}
                                                                   },
                                  hMainWnd,
                                  ColorsDialogCallback,
                                  0)
end procedure

function icon_from_xpm(sequence xpm)
sequence colors = repeat(0,255), pixels = {}, andbits = {}
integer w = 0, h = 0, clear = 0
atom buf, handle, mask, iconinfo, hbmMask, hbmColor

    for i=2 to length(xpm) do
        if length(xpm[i])>=6 and xpm[i][3]='c' then
            if equal(xpm[i][5..$], "None") then
                clear = xpm[i][1]
            else
                colors[xpm[i][1]] = value(xpm[i][5..$])
            end if
        else
            w = length(xpm[i])
            h += 1
            for j=1 to length(xpm[i]) do
                if remainder(j, 8)=1 then
                    andbits &= 0
                end if
                if xpm[i][j]=clear then
                    andbits[$] = or_bits(andbits[$], power(2, 7-and_bits(j-1,7)))
                    pixels &= 0
                else
                    pixels &= #FF000000+colors[xpm[i][j]][2]
                end if
            end for
        end if
    end for
    mask = allocate(length(andbits))
    poke(mask, andbits)
    buf = allocate(length(pixels)*4)
    poke4(buf, pixels)
    hbmMask = c_func(CreateBitmap, {w, h, 1, 1, mask})
    hbmColor = c_func(CreateBitmap, {w, h, 1, 32, buf})
    iconinfo = allocate_pack("iddpp", {1,0,0,hbmMask,hbmColor})
    handle = c_func(CreateIconIndirect, {iconinfo})
    if c_func(DeleteObject, {hbmMask}) then end if
    if c_func(DeleteObject, {hbmColor}) then end if
    free(buf)
    free(mask)
    free(iconinfo)

    return handle
end function

include weeicon.e
constant wee_icon = icon_from_xpm(wee_xpm)

function IndentDialogProc(atom hdlg, atom iMsg, atom wParam, atom lParam)
--  atom junk, len, buf
atom junk, buf
integer id, tab_width, indent_width, use_tabs, check
sequence val

    if lParam then end if -- suppress warnings
    --? {hdlg, iMsg, wParam, HIWORD(lParam), LOWORD(lParam)}

    if iMsg=WM_INITDIALOG then
        junk = c_func(SendMessage, {c_func(GetDlgItem, {hdlg, IDOK}),
                                    WM_SETFONT, captionFont, 0})
        junk = c_func(SendMessage, {c_func(GetDlgItem, {hdlg, IDCANCEL}),
                                    WM_SETFONT, captionFont, 0})
        hDlg = hdlg

        tab_width = c_func(SendMessage, {hedit, SCI_GETTABWIDTH, 0, 0})
        indent_width = c_func(SendMessage, {hedit, SCI_GETINDENT, 0, 0})
        use_tabs = c_func(SendMessage, {hedit, SCI_GETUSETABS, 0, 0})
        junk = c_func(SendMessage, {c_func(GetDlgItem, {hdlg, 1001}),
                                    WM_SETTEXT, 0, alloc_string(sprintf("%d", {tab_width}))})
        junk = c_func(SendMessage, {c_func(GetDlgItem, {hdlg, 1003}),
                                    WM_SETTEXT, 0, alloc_string(sprintf("%d", {indent_width}))})

        if use_tabs then
            check = BST_CHECKED
        else
            check = BST_UNCHECKED
        end if
        junk = c_func(SendMessage, {c_func(GetDlgItem, {hdlg, 1004}),
                                    BM_SETCHECK, check, 0})
        if indentation_guides then
            check = BST_CHECKED
        else
            check = BST_UNCHECKED
        end if
        junk = c_func(SendMessage, {c_func(GetDlgItem, {hdlg, 1005}),
                                    BM_SETCHECK, check, 0})

    elsif iMsg=WM_COMMAND then
        id = LOWORD(wParam)
        if id=IDOK then
            buf = allocate(16)
            junk = c_func(SendMessage, {c_func(GetDlgItem, {hdlg, 1001}),
                                        WM_GETTEXT, 16, buf})
            val = value(peek_string(buf))
            if val[1]=0 and integer(val[2]) then
                tab_width = val[2]
            else
                tab_width = 0
            end if
            junk = c_func(SendMessage, {c_func(GetDlgItem, {hdlg, 1003}),
                                        WM_GETTEXT, 16, buf})
            val = value(peek_string(buf))
            if val[1]=0 and integer(val[2]) then
                indent_width = val[2]
            else
                indent_width = 0
            end if
            free(buf)

            if indent_width<1 or indent_width>8 or
            tab_width<1 or tab_width>8 then
                return 0
            end if

            use_tabs = BST_CHECKED=c_func(SendMessage, {
                                                        c_func(GetDlgItem, {hdlg, 1004}), BM_GETCHECK, 0, 0})
            indentation_guides = BST_CHECKED=c_func(SendMessage, {
                                                                  c_func(GetDlgItem, {hdlg, 1005}), BM_GETCHECK, 0, 0})

            junk = c_func(SendMessage, {hedit, SCI_SETTABWIDTH, tab_width, 0})
            junk = c_func(SendMessage, {hedit, SCI_SETINDENT, indent_width, 0})
            junk = c_func(SendMessage, {hedit, SCI_SETUSETABS, use_tabs, 0})
            reinit_all_edits()

            junk = c_func(EndDialog, {hdlg, 1})
            return 1
        elsif id=IDCANCEL then
            junk = c_func(EndDialog, {hdlg, 0})
            return 1
        elsif id=1004 or id=1005 then
            junk = c_func(SendMessage, {
                                        c_func(GetDlgItem, {hdlg, id}), BM_GETCHECK, 0, 0})
            if junk=BST_CHECKED then
                junk = BST_UNCHECKED
            else
                junk = BST_CHECKED
            end if
            junk = c_func(SendMessage, {
                                        c_func(GetDlgItem, {hdlg, id}), BM_SETCHECK, junk, 0})
        end if

    elsif iMsg=WM_CLOSE then --closing dialog
        junk = c_func(EndDialog, {hdlg, 0})
        hDlg = 0
    end if

    return 0
end function

constant IndentDialogCallback = callback("IndentDialogProc")

global procedure indent_dialog()
sequence template
integer junk
    template = {--DLGTEMPLATE{style, exstyle, x, y, cx, cy, menu, wndclass, text}
                {{WS_POPUP, WS_CAPTION, WS_SYSMENU, DS_MODALFRAME}, 0,  50,50, 100,86, 0, 0, "Indent"},
                --DLGITEMTEMPLATE{style, exstyle, x, y, cx, cy, id, dlgclass, text}
                {{WS_CHILD, WS_VISIBLE}, 0, 4,4, 44,12, 1002, DIALOG_CLASS_STATIC, "Indent size"},
                {{WS_CHILD, WS_VISIBLE, WS_BORDER, ES_AUTOHSCROLL, ES_NUMBER}, 0, 52,4, 44,12, 1003, DIALOG_CLASS_EDIT, ""},

                {{WS_CHILD, WS_VISIBLE, BS_CHECKBOX}, 0, 4,20, 92,12, 1005, DIALOG_CLASS_BUTTON, "Show indentation guides"},

                {{WS_CHILD, WS_VISIBLE, BS_CHECKBOX}, 0, 4,34, 92,12, 1004, DIALOG_CLASS_BUTTON, "Use tabs in indentation"},

                {{WS_CHILD, WS_VISIBLE}, 0, 4,50, 44,12, 1000, DIALOG_CLASS_STATIC, "Tab size"},
                {{WS_CHILD, WS_VISIBLE, WS_BORDER, ES_AUTOHSCROLL, ES_NUMBER}, 0, 52,50, 44,12, 1001, DIALOG_CLASS_EDIT, ""},

                {{WS_CHILD, WS_VISIBLE, BS_PUSHBUTTON}, 0, 4,66, 44,12, IDCANCEL, DIALOG_CLASS_BUTTON, "Cancel"},
                {{WS_CHILD, WS_VISIBLE, BS_DEFPUSHBUTTON}, 0, 52,66, 44,12, IDOK, DIALOG_CLASS_BUTTON, "OK"}
               }
    junk = DialogBoxIndirectParam(c_func(GetModuleHandle, {NULL}), template, hMainWnd, IndentDialogCallback, 0)
end procedure



procedure init_fonts()
atom junk, lf_facesize, lf_size, size, ncm,
     lfCaptionFont, lfSmCaptionFont, lfMenuFont, lfStatusFont, lfMessageFont

    lf_facesize = 32
    lf_size = 4*5+8+lf_facesize
    size = 4+4*5
    lfCaptionFont = size
    size += lf_size+4*2
    lfSmCaptionFont = size
    size += lf_size+4*2
    lfMenuFont = size
    size += lf_size
    lfStatusFont = size
    size += lf_size
    lfMessageFont = size
    size += lf_size
    ncm = allocate(size)

    poke4(ncm, size)
    poke4(ncm+lfCaptionFont, -1)
    poke4(ncm+lfSmCaptionFont, -1)
    poke4(ncm+lfMenuFont, -1)
    poke4(ncm+lfStatusFont, -1)
    poke4(ncm+lfMessageFont, -1)

    junk = c_func(SystemParametersInfo, {SPI_GETNONCLIENTMETRICS, size, ncm, 0})

    captionFont = c_func(CreateFontIndirect, {ncm+lfCaptionFont})
    smCaptionFont = c_func(CreateFontIndirect, {ncm+lfSmCaptionFont})
    menuFont = c_func(CreateFontIndirect, {ncm+lfMenuFont})
    statusFont = c_func(CreateFontIndirect, {ncm+lfStatusFont})
    messageFont = c_func(CreateFontIndirect, {ncm+lfMessageFont})
    free(ncm)
end procedure

procedure process_dropfiles(atom hDrop)
--  integer count, len, size
integer count, size
atom buf, junk

    -- get the number of files dropped
    count = c_func(DragQueryFile, {hDrop, #FFFFFFFF, NULL, 0})
    for i=0 to count-1 do
        -- get the length of the indexed filename and null-terminator
        size = c_func(DragQueryFile, {hDrop, i, NULL, 0})+1
        buf = allocate(size)
        -- get the indexed filename into our buffer and open it
        junk = c_func(DragQueryFile, {hDrop, i, buf, size})
--      junk = open_file(peek_string(buf), 0)
        junk = call_func(r_open_file,{peek_string(buf), 0})
        free(buf)
    end for
    c_proc(DragFinish, {hDrop})
end procedure


integer doing_setfocus_checks
doing_setfocus_checks = 0

global function WndProc(atom hwnd, atom iMsg, atom wParam, atom lParam)
-- callback routine to handle Window class
atom junk, hwndFrom, code
integer rc
    rc = 0

    if hwnd!=hMainWnd then
        --? {hwnd, iMsg, wParam, lParam}
        return c_func(DefWindowProc, {hwnd, iMsg, wParam, lParam})
    end if
    if iMsg=WM_NOTIFY then
        sequence nmhdr = unpack(lParam, "ppi")
      -- lParam is pointer to NMHDR { HWND hwndFrom; UINT_PTR idFrom; UINT code; }
        hwndFrom = nmhdr[1]
        code = nmhdr[3]
      --printf(1, "hwndFrom=%x idFrom=%x code=%x %x %x\n", nmhdr & {LOWORD(code), HIWORD(code)})

        if hwndFrom=hedit then
            junk = sci_notify(c_func(SendMessage, {hedit, SCI_GETDIRECTPOINTER, 0, 0}), 0, lParam, 0)
            return rc

        elsif hwndFrom=htabs then
            if code=TCN_SELCHANGE then
                call_proc(r_select_tab,{1+c_func(SendMessage, {htabs, TCM_GETCURSEL, 0, 0})})
            elsif code=TCN_SELCHANGING then
                -- unhighlight the current tab
                junk = c_func(SendMessage, {htabs, TCM_HIGHLIGHTITEM,
                                            c_func(SendMessage, {htabs, TCM_GETCURSEL, 0, 0}), 0})
            elsif code=NM_RCLICK then
                rightclick_tab()
            end if
        elsif hwndFrom=htooltip and code=TTN_NEEDTEXT then
            junk = allocate_pack("zdppddddps", { -- TOOLINFO
                                                TTF_IDISHWND+TTF_SUBCLASS, -- uFlags
                                                hMainWnd, -- hwnd
                                                htabs, -- uId
                                                0,0,0,0, -- rect
                                                0, -- hinst
                                                file_name -- lpszText
                                               })
            if c_func(SendMessage, {htooltip, TTM_UPDATETIPTEXT, 0, junk}) then end if
            free(junk)
        end if
    end if

    if iMsg=WM_CREATE then
        return rc
    elsif iMsg=WM_DESTROY then
        c_proc(PostQuitMessage, {0})
        get_window_size()
        save_wee_conf(wee_conf_filename)
        return rc
    elsif iMsg=WM_SIZE then  --resize the rich edit control to fit the window
        c_proc(MoveWindow, {hedit, 0, tab_h, LOWORD(lParam), HIWORD(lParam)-tab_h, 1})
        c_proc(MoveWindow, {hstatus, LOWORD(lParam)-64, 2, 64, tab_h-4, 1})
        c_proc(MoveWindow, {htabs, 0, 0, LOWORD(lParam), tab_h, 1})
        return rc
    elsif iMsg=WM_CLOSE then --closing window
        if call_func(r_save_modified_tabs,{}) then
            junk = c_func(DestroyWindow, {hwnd})
        end if
        return rc
    elsif iMsg=WM_SETFOCUS then --always pass focus on to rich edit control
        if not doing_setfocus_checks then
            doing_setfocus_checks = 1
            call_proc(r_check_ex_err,{})
            call_proc(r_check_externally_modified_tabs,{})
            junk = c_func(SetFocus, {hedit})
            doing_setfocus_checks = 0
        end if
        return rc
    elsif iMsg=WM_COMMAND then
        if lParam=0 then
            if wParam=File_New then
                new_file()
                return rc
            elsif wParam=File_Open then
--              junk = open_file("", 0)
                junk = call_func(r_open_file,{"", 0})
                return rc
            elsif wParam=File_Save then
                if call_func(r_save_if_modified,{0}) then end if    -- no confirm
                return rc
            elsif wParam=File_SaveAs then
                junk = call_func(r_save_file_as,{})
                return rc
            elsif wParam=File_Close then
                call_proc(r_close_tab,{})
                return rc
            elsif wParam=File_Exit then
                return c_func(SendMessage, {hwnd, WM_CLOSE, 0, 0})
            elsif wParam=Edit_Undo then
                return c_func(SendMessage, {hedit, WM_UNDO, 0, 0})
            elsif wParam=Edit_Redo then
                return c_func(SendMessage, {hedit, SCI_REDO, 0, 0})
            elsif wParam=Edit_Cut then
                return c_func(SendMessage, {hedit, WM_CUT, 0, 0})
            elsif wParam=Edit_Copy then
                return c_func(SendMessage, {hedit, WM_COPY, 0, 0})
            elsif wParam=Edit_Paste then
                return c_func(SendMessage, {hedit, WM_PASTE, 0, 0})
            elsif wParam=Edit_Clear then
                return c_func(SendMessage, {hedit, WM_CLEAR, 0, 0})
            elsif wParam=Edit_SelectAll then
                return c_func(SendMessage, {hedit, EM_SETSEL, 0, -1})
            elsif wParam=Edit_ToggleComment then
                toggle_comment()
                return rc
            elsif wParam=Search_Find then
                sequence s = get_selection()
                if length(s) then
                    find_phrase = s
                end if
                hFindDlg = FindText(hMainWnd,0,FR_DOWN,find_phrase)
                return rc
            elsif wParam=Search_Find_Next then
                if length(find_phrase)=0 then
                    sequence s = get_selection()
                    if length(s) then
                        find_phrase = s
                    end if
                    hFindDlg = FindText(hMainWnd,0,FR_DOWN,find_phrase)
                else
                    if search_find(find_phrase, 0) then end if
                end if
                return rc
            elsif wParam=Search_Find_Prev then
                if search_find(find_phrase, 1) then end if
                return rc
            elsif wParam=Search_Replace then
                sequence s = get_selection()
                if length(s) then
                    find_phrase = s
                end if
                hFindDlg = ReplaceText(hMainWnd,0,FR_DOWN,find_phrase,replace_phrase)
                return rc
            elsif wParam=View_Subs then
                view_subroutines()
                return rc
            elsif wParam=View_Error then
                ui_view_error()
                return rc
            elsif wParam=View_Completions then
                view_completions()
                return rc
            elsif wParam=View_Declaration then
                view_declaration()
                return rc
            elsif wParam=View_SubArgs then
                view_subroutine_arguments()
                return rc
            elsif wParam=View_GoBack then
                call_proc(r_go_back,{})
                return rc
            elsif wParam=Run_Start then
                run_start(0)
                return rc
            elsif wParam=Run_WithArgs then
                run_start(1)
                return rc
            elsif wParam=Run_Arguments then
                run_arguments()
                return rc
            elsif wParam=Run_Interpreter then
                run_interpreter()
            elsif wParam=Run_Bind then
                run_convert("Bind", "eubind")
                return rc
            elsif wParam=Run_Shroud then
                run_convert("Shroud", "eushroud")
                return rc
            elsif wParam=Run_Translate then
                run_convert("Translate", "euc")
                return rc
            elsif wParam=Options_Font then
                sequence s = ChooseFont(hMainWnd, font_name, font_height)
                if length(s) then
                    font_name = s[1]
                    font_height = s[2]
                    reinit_all_edits()
                end if
                return rc
            elsif wParam=Options_LineNumbers then
                line_numbers = not line_numbers
                reinit_all_edits()
                return c_func(CheckMenuItem, {hoptionsmenu, Options_LineNumbers, MF_CHECKED*line_numbers})
            elsif wParam=Options_SortedSubs then
                sorted_subs = not sorted_subs
                return c_func(CheckMenuItem, {hoptionsmenu, Options_SortedSubs, MF_CHECKED*sorted_subs})
            elsif wParam=Options_Colors then
                choose_colors()
                return rc
            elsif wParam=Options_LineWrap then
                line_wrap = not line_wrap
                reinit_all_edits()
                return c_func(CheckMenuItem, {hoptionsmenu, Options_LineWrap, MF_CHECKED*line_wrap})
            elsif wParam=Options_ReopenTabs then
                reopen_tabs = not reopen_tabs
                return c_func(CheckMenuItem, {hoptionsmenu, Options_ReopenTabs, MF_CHECKED*reopen_tabs})
            elsif wParam=Options_CompleteStatements then
                complete_statements = not complete_statements
                return c_func(CheckMenuItem, {hoptionsmenu, Options_CompleteStatements, MF_CHECKED*complete_statements})
            elsif wParam=Options_CompleteBraces then
                complete_braces = not complete_braces
                return c_func(CheckMenuItem, {hoptionsmenu, Options_CompleteBraces, MF_CHECKED*complete_braces})
            elsif wParam=Options_Indent then
                indent_dialog()
                return rc
            elsif wParam=Help_About then
                about_box()
                return rc
            elsif wParam=Help_Tutorial then
                open_tutorial()
                return rc
            elsif wParam=Help_Context then
                call_proc(r_context_help,{})
                return rc
            elsif wParam>File_Recent and wParam<=File_Recent+max_recent_files then
                call_proc(r_open_recent,{wParam-File_Recent})
            elsif wParam>=Select_Tab and wParam<=Select_Tab+9 then
                call_proc(r_select_tab,{wParam-Select_Tab+1})
            elsif wParam=Select_Prev_Tab then
                call_proc(r_select_tab,{get_prev_tab()})
            elsif wParam=Select_Next_Tab then
                call_proc(r_select_tab,{get_next_tab()})
            else
                printf(1, "%d %d\n", {wParam, lParam})
            end if
        else
            --printf(1, "HIWORD:%d LOWORD:%d LPARAM:%d\n", {HIWORD(wParam), LOWORD(wParam), lParam})
        end if
    elsif iMsg=WM_FIND then  -- find/replace dialog
        process_find(lParam)
    elsif iMsg=WM_DROPFILES then
        process_dropfiles(wParam)
    else
       --printf(1, "hwnd=%x iMsg=%d #%x wParam=%d lParam=%d\n", {hwnd, iMsg, iMsg, wParam, lParam})

    end if

    return c_func(DefWindowProc, {hwnd, iMsg, wParam, lParam})
end function

-- The scintilla edit control doesn't offer a way to hook accelerator keys
-- so we have to process messages in our message loop, and intercept and
-- rewrite them into WM_COMMAND messages for the main window handle.
procedure translate_editor_keys(atom msg)
atom hwnd, iMsg, wParam, lParam
sequence m
integer ctrl, shift

    m = unpack(msg, "pdpp")

    hwnd = m[1]
    iMsg = m[2]
    wParam = m[3]
    lParam = m[4]

    if hwnd!=hedit then
        if hwnd=htabs and iMsg=WM_MBUTTONDOWN then
            atom point
            integer tab
            point = allocate(12)
            poke4(point, {LOWORD(lParam), HIWORD(lParam), 0})
            tab = c_func(SendMessage, {htabs, TCM_HITTEST, 0, point})
            free(point)
            if tab>=0 then
                -- middle click to close tab
                call_proc(r_select_tab,{tab+1})
                m = {hMainWnd, WM_COMMAND, File_Close, 0}
                pack(msg, "pdpp", m)
            end if
        end if
        return
    end if

    if iMsg=WM_CHAR  then
        shift = and_bits(c_func(GetKeyState, {VK_SHIFT}), #8000)
        ctrl = and_bits(c_func(GetKeyState, {VK_CONTROL}), #8000)
        --printf(1, "%x %x %x %x\n", {hwnd, iMsg, wParam, lParam})
        if wParam=27 then -- Esc
            m = {hMainWnd, WM_COMMAND, View_GoBack, 0}
        elsif not ctrl then
          -- control key is not pressed
        elsif wParam=#13 then -- Ctrl+S
            m = {hMainWnd, WM_COMMAND, File_Save, 0}
        elsif wParam=#6 then -- Ctrl+F
            m = {hMainWnd, WM_COMMAND, Search_Find, 0}
        elsif wParam=#12 then -- Ctrl+R
            m = {hMainWnd, WM_COMMAND, Search_Replace, 0}
        elsif wParam=#7 then -- Ctrl+G
            if shift then
                m = {hMainWnd, WM_COMMAND, Search_Find_Prev, 0}
            else
                m = {hMainWnd, WM_COMMAND, Search_Find_Next, 0}
            end if
        elsif wParam=#17 then -- Ctrl+W
            m = {hMainWnd, WM_COMMAND, File_Close, 0}
        elsif wParam=#D then -- Ctrl+M
            m = {hMainWnd, WM_COMMAND, Edit_ToggleComment, 0}
        elsif wParam=#E then -- Ctrl+N
            m = {hMainWnd, WM_COMMAND, File_New, 0}
        elsif wParam=#F then -- Ctrl+O
            m = {hMainWnd, WM_COMMAND, File_Open, 0}
        elsif wParam=#11 then -- Ctrl+Q
            m = {hMainWnd, WM_COMMAND, File_Exit, 0}
        elsif wParam=VK_SPACE then
            m = {hMainWnd, WM_COMMAND, View_Completions, 0}
        elsif wParam=26 and shift then
            m = {hMainWnd, WM_COMMAND, Edit_Redo, 0}
        elsif wParam=25 then -- Ctrl+Y
            m = {hMainWnd, WM_COMMAND, Edit_Redo, 0}
        else
            return
        end if
    elsif iMsg=WM_KEYUP then
        shift = and_bits(c_func(GetKeyState, {VK_SHIFT}), #8000)
        ctrl = and_bits(c_func(GetKeyState, {VK_CONTROL}), #8000)
        if wParam=VK_F5 and shift then
            m = {hMainWnd, WM_COMMAND, Run_WithArgs, 0}
        elsif wParam=VK_F5 then
            m = {hMainWnd, WM_COMMAND, Run_Start, 0}
        elsif wParam=VK_F4 and ctrl then
            m = {hMainWnd, WM_COMMAND, File_Close, 0}
        elsif wParam=VK_F4 then
            m = {hMainWnd, WM_COMMAND, View_Error, 0}
        elsif wParam=VK_F3 then
            if shift then
                m = {hMainWnd, WM_COMMAND, Search_Find_Prev, 0}
            elsif ctrl then
                m = {hMainWnd, WM_COMMAND, Search_Find, 0}
            else
                m = {hMainWnd, WM_COMMAND, Search_Find_Next, 0}
            end if
        elsif wParam=VK_F2 and ctrl then
            m = {hMainWnd, WM_COMMAND, View_Declaration, 0}
        elsif wParam=VK_F2 and shift then
            m = {hMainWnd, WM_COMMAND, View_SubArgs, 0}
        elsif wParam=VK_F2 then
            m = {hMainWnd, WM_COMMAND, View_Subs, 0}
        elsif wParam=#5A and ctrl and shift then
            m = {hMainWnd, WM_COMMAND, Edit_Redo, 0}
        elsif (wParam=VK_PRIOR or (wParam=VK_TAB and shift)) and ctrl then
            m = {hMainWnd, WM_COMMAND, Select_Prev_Tab, 0}
        elsif (wParam=VK_NEXT or wParam=VK_TAB) and ctrl then
            m = {hMainWnd, WM_COMMAND, Select_Next_Tab, 0}
        elsif wParam=VK_F1 then
            m = {hMainWnd, WM_COMMAND, Help_Context, 0}
        else
            return
        end if
    elsif iMsg=WM_SYSCHAR then
        if wParam>='1' and wParam<='9' then
            m = {hMainWnd, WM_COMMAND, wParam-'1'+Select_Tab, 0}
        else
            return
        end if
    elsif iMsg=WM_MOUSEWHEEL then
        atom point
        point = allocate(12)
        poke4(point, {LOWORD(lParam), HIWORD(lParam), 0})
        if c_func(ScreenToClient, {hMainWnd, point}) then end if
        if c_func(SendMessage, {htabs, TCM_HITTEST, 0, point})>=0 then
            -- select prev/next tabs when scrolled by mouse wheel 
            if HIWORD(wParam)<#8000 then
                m = {hMainWnd, WM_COMMAND, Select_Prev_Tab, 0}
            else
                m = {hMainWnd, WM_COMMAND, Select_Next_Tab, 0}
            end if
        end if
        free(point)
    else
        return
    end if
  --printf(1, "%x %x %x %x\n", m)
    pack(msg, "pdpp", m)
end procedure


constant AppName = "WeeEditor"


global procedure ui_main()
-- main routine 
atom msg
integer id
atom junk
--  , tcitem

    init_fonts()

    msg = allocate(SIZE_OF_MESSAGE)

    id = routine_id("WndProc")
    if id= -1 then
        puts(1, "routine_id failed!\n")
        abort(1)
    end if
    cls = RegisterClass({
                           or_bits(CS_HREDRAW, CS_VREDRAW),
                           call_back(id), -- get 32-bit address for callback
                           0,
                           0,
                           0,
                           c_func(LoadIcon, {NULL, IDI_APPLICATION}),
                           c_func(LoadCursor, {NULL, IDC_ARROW}),
                           c_func(GetStockObject, {WHITE_BRUSH}),
                           NULL,
                           AppName,
                           c_func(LoadIcon, {NULL, IDI_APPLICATION})})
    if cls=0 then
        puts(1, "Couldn't register class\n")
        abort(1)
    end if

    -- this is required for FindText,ReplaceText common dialogs to work
    WM_FIND = c_func(RegisterWindowMessage,{alloc_string(FINDMSGSTRING)})

-- menu creation
-- file menu
    hfilemenu = c_func(CreateMenu, {})
    junk = c_func(AppendMenu, {hfilemenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               File_New, alloc_string("&New\tCtrl+N")})
    junk = c_func(AppendMenu, {hfilemenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               File_Open, alloc_string("&Open...\tCtrl+O")})
    junk = c_func(AppendMenu, {hfilemenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               File_Save, alloc_string("&Save\tCtrl+S")})
    junk = c_func(AppendMenu, {hfilemenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               File_SaveAs, alloc_string("Save &As...")})
    junk = c_func(AppendMenu, {hfilemenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               File_Close, alloc_string("&Close\tCtrl+W")})
--    junk = c_func(AppendMenu, {hfilemenu, MF_BYPOSITION + MF_SEPARATOR, 0, 0})
--    junk = c_func(AppendMenu, {hfilemenu, MF_BYPOSITION + MF_STRING + MF_GRAYED, 
--      File_Print, alloc_string("Print...")})
--    junk = c_func(AppendMenu, {hfilemenu, MF_BYPOSITION + MF_STRING + MF_GRAYED, 
--      File_PageSetup, alloc_string("Page Setup...")})
    junk = c_func(AppendMenu, {hfilemenu, MF_BYPOSITION+MF_SEPARATOR, 0, 0})
    junk = c_func(AppendMenu, {hfilemenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               File_Exit, alloc_string("E&xit\tAlt+F4")})
-- edit menu    
    heditmenu = c_func(CreateMenu, {})
    junk = c_func(AppendMenu, {heditmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               Edit_Undo, alloc_string("U&ndo\tCtrl+Z")})
    junk = c_func(AppendMenu, {heditmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               Edit_Redo, alloc_string("&Redo\tShift+Ctrl+Z")})
    junk = c_func(AppendMenu, {heditmenu, MF_BYPOSITION+MF_SEPARATOR, 0, 0})
    junk = c_func(AppendMenu, {heditmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               Edit_Cut, alloc_string("Cu&t\tCtrl+X")})
    junk = c_func(AppendMenu, {heditmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               Edit_Copy, alloc_string("&Copy\tCtrl+C")})
    junk = c_func(AppendMenu, {heditmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               Edit_Paste, alloc_string("&Paste\tCtrl+V")})
    junk = c_func(AppendMenu, {heditmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               Edit_Clear, alloc_string("Cl&ear\tDel")})
    junk = c_func(AppendMenu, {heditmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               Edit_SelectAll, alloc_string("Select &All\tCtrl+A")})
    junk = c_func(AppendMenu, {heditmenu, MF_BYPOSITION+MF_SEPARATOR, 0, 0})
    junk = c_func(AppendMenu, {heditmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               Edit_ToggleComment, alloc_string("Toggle Co&mment\tCtrl+M")})
-- search menu
    hsearchmenu = c_func(CreateMenu, {})
    junk = c_func(AppendMenu, {hsearchmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               Search_Find, alloc_string("&Find...\tCtrl+F3")})
    junk = c_func(AppendMenu, {hsearchmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               Search_Find_Next, alloc_string("Find &Next\tF3")})
    junk = c_func(AppendMenu, {hsearchmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               Search_Find_Prev, alloc_string("Find &Previous\tShift+F3")})
    junk = c_func(AppendMenu, {hsearchmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               Search_Replace, alloc_string("&Replace...")})
-- view menu
    hviewmenu = c_func(CreateMenu, {})
    junk = c_func(AppendMenu, {hviewmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               View_Subs, alloc_string("&Subroutines...\tF2")})
    junk = c_func(AppendMenu, {hviewmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               View_Declaration, alloc_string("&Declaration\tCtrl+F2")})
    junk = c_func(AppendMenu, {hviewmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               View_SubArgs, alloc_string("Subroutine &Arguments...\tShift+F2")})
    junk = c_func(AppendMenu, {hviewmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               View_Completions, alloc_string("&Completions...\tCtrl+Space")})
    junk = c_func(AppendMenu, {hviewmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               View_Error, alloc_string("Goto &Error\tF4")})
    junk = c_func(AppendMenu, {hviewmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               View_GoBack, alloc_string("Go &Back\tEsc")})
-- run menu
    hrunmenu = c_func(CreateMenu, {})
    junk = c_func(AppendMenu, {hrunmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               Run_Start, alloc_string("&Start\tF5")})
    junk = c_func(AppendMenu, {hrunmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               Run_WithArgs, alloc_string("Start with &Arguments\tShift-F5")})
    junk = c_func(AppendMenu, {hrunmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               Run_Arguments, alloc_string("Set Arguments...")})
    junk = c_func(AppendMenu, {hrunmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               Run_Interpreter, alloc_string("Set Interpreter...")})
    junk = c_func(AppendMenu, {hrunmenu, MF_BYPOSITION+MF_SEPARATOR, 0, 0})
    junk = c_func(AppendMenu, {hrunmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               Run_Bind, alloc_string("&Bind")})
    junk = c_func(AppendMenu, {hrunmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               Run_Shroud, alloc_string("S&hroud")})
    junk = c_func(AppendMenu, {hrunmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               Run_Translate, alloc_string("&Translate and Compile")})
-- options menu
    hoptionsmenu = c_func(CreateMenu, {})
    junk = c_func(AppendMenu, {hoptionsmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               Options_Font, alloc_string("&Font...")})
    junk = c_func(AppendMenu, {hoptionsmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               Options_LineNumbers, alloc_string("&Line Numbers")})
    junk = c_func(AppendMenu, {hoptionsmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               Options_SortedSubs, alloc_string("&Sort View Subroutines")})
    junk = c_func(AppendMenu, {hoptionsmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               Options_Colors, alloc_string("&Colors...")})
    junk = c_func(AppendMenu, {hoptionsmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               Options_LineWrap, alloc_string("Line &Wrap")})
    junk = c_func(AppendMenu, {hoptionsmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               Options_ReopenTabs, alloc_string("&Reopen Tabs Next Time")})
    junk = c_func(AppendMenu, {hoptionsmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               Options_CompleteStatements, alloc_string("Complete Statements")})
    junk = c_func(AppendMenu, {hoptionsmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               Options_CompleteBraces, alloc_string("Complete Braces")})
    junk = c_func(AppendMenu, {hoptionsmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               Options_Indent, alloc_string("&Indents...")})
-- help menu
    hhelpmenu = c_func(CreateMenu, {})
    junk = c_func(AppendMenu, {hhelpmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               Help_About, alloc_string("&About...")})
    junk = c_func(AppendMenu, {hhelpmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               Help_Tutorial, alloc_string("&Tutorial")})
    junk = c_func(AppendMenu, {hhelpmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               Help_Context, alloc_string("&Help\tF1")})
-- main menu
    hmenu = c_func(CreateMenu, {})
    junk = c_func(AppendMenu, {hmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED+MF_POPUP,
                               hfilemenu, alloc_string("&File")})
    junk = c_func(AppendMenu, {hmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED+MF_POPUP,
                               heditmenu, alloc_string("&Edit")})
    junk = c_func(AppendMenu, {hmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED+MF_POPUP,
                               hsearchmenu, alloc_string("&Search")})
    junk = c_func(AppendMenu, {hmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED+MF_POPUP,
                               hviewmenu, alloc_string("&View")})
    junk = c_func(AppendMenu, {hmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED+MF_POPUP,
                               hrunmenu, alloc_string("&Run")})
    junk = c_func(AppendMenu, {hmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED+MF_POPUP,
                               hoptionsmenu, alloc_string("&Options")})
    junk = c_func(AppendMenu, {hmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED+MF_POPUP,
                               hhelpmenu, alloc_string("&Help")})

-- tab popup menu
    htabmenu = c_func(CreatePopupMenu)
    junk = c_func(AppendMenu, {htabmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               File_Save, alloc_string("Save")})
    junk = c_func(AppendMenu, {htabmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               File_SaveAs, alloc_string("Save As")})
    junk = c_func(AppendMenu, {htabmenu, MF_BYPOSITION+MF_STRING+MF_ENABLED,
                               File_Close, alloc_string("Close")})

    free_strings() -- garbage collect menu strings

    load_wee_conf(wee_conf_filename)

    -- set the recent items on the file menu
    ui_refresh_file_menu(recent_files)

    -- set the checkmark on the Line Numbers menu item
    if c_func(CheckMenuItem, {hoptionsmenu, Options_LineNumbers, MF_CHECKED*line_numbers}) then end if
    if c_func(CheckMenuItem, {hoptionsmenu, Options_SortedSubs, MF_CHECKED*sorted_subs}) then end if
    if c_func(CheckMenuItem, {hoptionsmenu, Options_LineWrap, MF_CHECKED*line_wrap}) then end if
    if c_func(CheckMenuItem, {hoptionsmenu, Options_ReopenTabs, MF_CHECKED*reopen_tabs}) then end if
    if c_func(CheckMenuItem, {hoptionsmenu, Options_CompleteStatements, MF_CHECKED*complete_statements}) then end if
    if c_func(CheckMenuItem, {hoptionsmenu, Options_CompleteBraces, MF_CHECKED*complete_braces}) then end if

-- window creation
    hMainWnd = CreateWindow({0,                      -- extended style
                             AppName,              -- window class name
                             "",               -- window caption
                             WS_OVERLAPPEDWINDOW,    -- window style
                             x_pos,          -- initial x position
                             y_pos,          -- initial y position
                             x_size,          -- initial x size
                             y_size,          -- initial y size
                             NULL,                   -- parent window handle
                             hmenu,                  -- window menu handle
                             0 ,                --hInstance // program instance handle
                             NULL})             -- creation parameters

    junk = c_func(LoadLibrary, {alloc_string("RICHED32.DLL")})  --needed for richedit

    hstatus = CreateWindow({0, "STATIC", "static",
                            {WS_CHILD,WS_VISIBLE,SS_RIGHT},
                            0,0,0,0, -- set by resize
                            hMainWnd,
                            NULL,
                            0,
                            NULL})
    junk = c_func(SendMessage, {hstatus, WM_SETFONT, statusFont, 0})

    htabs = CreateWindow({0,"SysTabControl32", "tabs",
                          {WS_CHILD,WS_VISIBLE,TCS_FOCUSNEVER},
                          0,0,200,20, -- set by resize
                          hMainWnd,
                          NULL,
                          0,
                          NULL})
    junk = c_func(SendMessage, {htabs, WM_SETFONT, statusFont, 0})

    htooltip = CreateWindow({0, "tooltips_class32", 0,
                             {WS_POPUP,TTS_ALWAYSTIP},
                             CW_USEDEFAULT, CW_USEDEFAULT,
                             CW_USEDEFAULT, CW_USEDEFAULT,
                             hMainWnd, NULL,
                             0, NULL})

    --printf(1, "hMainWnd=%x hstatus=%x htabs=%x\n", {hMainWnd, hstatus, htabs})
    if hMainWnd=0 or hstatus=0 or htabs=0 or htooltip=0 then
        puts(1, "Couldn't CreateWindow\n")
        abort(1)
    end if

    if c_func(SendMessage, {hMainWnd, WM_SETICON, ICON_BIG, wee_icon}) then end if
    if c_func(SendMessage, {hMainWnd, WM_SETICON, ICON_SMALL, wee_icon}) then end if

    -- attach tooltip to tab control
    junk = allocate_pack("zdppddddps", { -- TOOLINFO
                                        TTF_IDISHWND+TTF_SUBCLASS, -- uFlags
                                        hMainWnd, -- hwnd
                                        htabs, -- uId
                                        0,0,0,0, -- rect
                                        0, -- hinst
                                        LPSTR_TEXTCALLBACK -- lpszText
                                       })
    if c_func(SendMessage, {htooltip, TTM_ADDTOOL, 0, junk}) then end if
    free(junk)

    -- open files from last time and on command line
    call_proc(r_open_tabs,{})

    --if SetThemeAppProperties != -1 then
    --    c_proc(SetThemeAppProperties, {STAP_ALLOW_NONCLIENT+STAP_ALLOW_CONTROLS})
    --    junk = c_func(SendMessage, {hMainWnd, WM_THEMECHANGED, 0, 0})
    --end if

    c_proc(ShowWindow, {hMainWnd, SW_SHOWNORMAL})
    c_proc(UpdateWindow, {hMainWnd})
    c_proc(DragAcceptFiles, {hMainWnd, 1})

    while c_func(GetMessage, {msg, NULL, 0, 0}) do
        if hFindDlg and c_func(IsDialogMessage,{hFindDlg,msg}) then
            -- this message is handled by find dialog (thanks Jacques)
            -- it makes the tab key work for find/replace dialogs
        elsif hDlg and c_func(IsDialogMessage,{hDlg,msg}) then
            -- ditto
            ? hDlg
        else
            translate_editor_keys(msg)
            junk = c_func(TranslateMessage, {msg})
            junk = c_func(DispatchMessage, {msg})
        end if
    end while

end procedure


