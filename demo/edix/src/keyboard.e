--
-- keyboard.e
-- ----------
--
--  Options/Keyboard, allows keyboard mappings/preferences to be specified.
--
--  Note: Some keys such as Ctrl Insert may get handled directly in edix.exw,
--        from a time before this was written/accidentally copied from Edita.
--
--  May benefit from a filter... [DEV - nah]

--DEV/BUG: open Options/Keyboard, navigate to Ctrl Q and press Ctrl J. Now
-- navigate to Ctrl J (correctly *'d) and press Ctrl J. It is reactivated,
-- but Ctrl Q remains. [Press Ctrl J a second time and it clears Ctrl Q,
-- and/at the point it *s Ctrl J, which seems rather silly..] [FIXED 20/12/17]

include builtins/dict.e     -- (not strictly necessary, it's an autoinclude)

integer KEYDICT = 0,
        INVKEY = 0

--#withtype Ihandle
--#withtype Ihandln

Ihandln key_dlg = NULL
Ihandle matrix, tg_act, tg_cxv, bt_help, bt_ok

constant titles = {"Mapping", "Standard", "Description"}

sequence keymaps = {},
         standards = {},
         descs = {},
         tags

function value_cb(Ihandle /*self*/, integer l, integer c)
--?{"value_cb",l,c}
    if c>0 and c<=length(titles) then
        if l==0 then
            return IupRawStringPtr(titles[c])   -- column title
        end if
        if l<=length(standards) then
            l = tags[l]
            if c=1 then
                return IupRawStringPtr(keymaps[l])
            elsif c=2 then
                return IupRawStringPtr(standards[l])
            elsif c=3 then
                return IupRawStringPtr(descs[l])
            end if
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

-- more may be required:
constant ESCAPES = "tqc",
         ESCHAR = "\t=,"

function decodech(string s)
-- Convert a string such as "<Ctrl A>" to an atom
-- Returns 0 if the string could not be decoded
atom res = 0
    if s="*" then return '*' end if -- special (disabled)
    if length(s)<3 or s[1]!='<' or s[$]!='>' then
        return 0    -- DEV message?
    end if
    s = s[2..-2]
    while 1 do
        if length(s)>4 and s[1..5]="Ctrl " then
            res = iup_XkeyCtrl(res)
            s = s[6..$]
        elsif length(s)>3 and s[1..4]="Alt " then
            res = iup_XkeyAlt(res)
            s = s[5..$]
        elsif length(s)>5 and s[1..6]="Shift " then
            res = iup_XkeyShift(res)
            s = s[7..$]
        else
            exit
        end if
    end while
    if length(s)=1 then
        -- single graphic keys (eg A, as in <Ctrl A>)
        integer ch = s[1]
        if (ch>='a' and ch<='z') 
        or ch>'~' or ch<' ' then
            -- (deliberately make <Ctrl a> illegal)
            res = 0
        else
            res += s[1]
        end if
    elsif length(s)>1
      and length(s)<=3
      and s[1]='F' then
        -- function keys (F1..F12)
        integer fkey = s[2]-'0'
        if length(s)=3 then
            if fkey=1 then
                fkey = 10+s[3]-'0'
            else
                fkey = 0
            end if
        end if
        if fkey>=1 and fkey<=12 then
            -- (relies on K_F1..K_F12 being contiguous)
            fkey = K_F1+(fkey-1)
            res += fkey
        else
            res = 0
        end if
    elsif s="Insert" then
        res += K_INS
    elsif s="Delete" then
        res += K_DEL
    elsif length(s)>2 then
        -- single quoted keys, eg '+' or '\t'
        if s[1]!='\'' or s[$]!='\'' or length(s)>4 then
            res = 0
        elsif length(s)=3 then -- eg '+'
            res += s[2]
        elsif s[2]!='\\' then
            res = 0
        else -- eg '\t'
            integer k = find(s[3],ESCAPES)
            if k=0 then
                res = 0
            else
                res += ESCHAR[k]
            end if
        end if
    else
        res = 0 -- error
    end if
    return res
end function

function keystring(atom key)
-- convert an atom keystroke to a string such as "<Ctrl A>"
-- return includes "??" on error
string res = "<"
    if iup_isCtrlXkey(key) then
        res &= "Ctrl "
    end if
    if iup_isAltXkey(key) then
        res &= "Alt "
    end if
    if iup_isShiftXkey(key) then
        res &= "Shift "
    end if
    key = iup_XkeyBase(key)
    if key=K_INS then
        res &= "Insert"
    elsif key=K_DEL then
        res &= "Delete"
    elsif key<'A' or key>'Z' then
        integer k = find(key,ESCHAR)
        if k then
            res &= `'\`&ESCAPES[k]&`'`
        elsif key>=K_F1 and key<=K_F12 then
            res &= sprintf("F%d",key-K_F1+1)
        elsif key<=#20 or key>=#7E then
            -- error
            res &= sprintf(`'???#%02x???'`,key)
        else
            res &= `'`&key&`'`
        end if
    else
        res &= key
--      res &= `'`&key&`'`
    end if
    res &= '>'
    return res
end function

integer redraw_all = 0

procedure addkey(string ks, string km)
--?{"addkey",ks,km}
    IupConfigSetVariableStr(config,"KeyMappings",ks,km)
    setd(decodech(ks),decodech(km),KEYDICT)
    sequence KEYS = split(IupConfigGetVariableStr(config, "KeyMappings", "KEYS"),',')
    integer k = find(ks,KEYS)
    if k=0 then
        KEYS = append(KEYS,ks)
--?{"ADD",KEYS}
        IupConfigSetVariableStr(config, "KeyMappings", "KEYS", join(sort(KEYS),','))
    end if
    k = find(ks,keymaps)
    if k!=0 then
        keymaps[k] = iff(find(standards[k],keymaps)?"*":"")
        redraw_all = 1
    end if
    k = find(ks,standards)
    if k!=0 and keymaps[k]="" then
        keymaps[k] = "*"
        redraw_all = 1
    end if
end procedure

procedure removekey(string km)
--?{"removekey",km}
    IupConfigSetVariableStr(config,"KeyMappings",km,NULL)
    deld(decodech(km),KEYDICT)
    sequence KEYS = split(IupConfigGetVariableStr(config, "KeyMappings", "KEYS"),',')
    integer k = find(km,KEYS)
    if k!=0 then
        KEYS[k..k] = {}
--?{"remove",KEYS}
--?{km,keymaps}
        IupConfigSetVariableStr(config, "KeyMappings", "KEYS", join(KEYS,','))
--20/12/17:
--      k = find(km,keymaps)
    end if
    k = find(km,standards)
    if k!=0 and keymaps[k]="*" then
        keymaps[k] = ""
        redraw_all = 1
    end if
end procedure

integer sortcol = 0
integer sortdir = 1

function by_column(integer i, integer j)
integer c = 0
    if sortcol=3 then   -- descriptions
        c = sortdir*compare(descs[i],descs[j])
        if c=0 then ?9/0 end if -- sanity check
    elsif sortcol=1 then -- mapping
        c = sortdir*compare(keymaps[i],keymaps[j])
    end if
    if c=0 then -- standard or blank keymap
        c = compare(standards[i],standards[j])
        if sortcol=2 then
            c *= sortdir
        end if
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
            IupSetAttribute(matrix,"REDRAW","ALL")
            selected_line = 0
        elsif iup_isdouble(pStatus) then
            redraw_all = 0
            selected_line = l
            l = tags[selected_line]
            string km = keymaps[l]
            string ks = standards[l]
            if km="" then
                keymaps[l] = "*"
                addkey(ks,"*")
            elsif km="*" and find(ks,keymaps) then
                IupMessage("Cannot enable","Key is mapped to "&standards[find(ks,keymaps)])
            else
                keymaps[l] = ""
                if km="*" then
                    removekey(ks)
                else
                    removekey(km)
                end if
            end if
            if redraw_all then
                IupSetStrAttribute(matrix,"REDRAW", "ALL")
            else
                IupSetStrAttribute(matrix,"REDRAW", "L%d", {selected_line})
            end if
        end if
    end if
    return IUP_DEFAULT
end function

constant help_text="""
Show the standard keyboard settings, and allow overrides.

Internally, Edix uses a fixed set of keystrokes, as shown in the Standard column.
This window allows say <Shift Insert> to be mapped to <Ctrl V>, so that when 
you press <Shift Insert>, Edix "sees" <Ctrl V>, and performs a Paste operation. 
That key mapping happens to be one of the ten that are set up as the defaults
(which mainly exist for illustrative purposes, but should help some users).

To change settings, simply locate the required entry and press the desired key.

Pressing the Standard key or double clicking on an entry removes any mapping if
such exists, otherwise it toggles the key between active and disabled, with the 
latter appearing as '*' in the Mapping column.

Setting a mapping removes that key from any other entry, and also disables any
standard key of that value with no mapping, likewise indicated with a '*'.

As well as tab as '\t', this also displays '=' as '\q' and ',' as '\c', which
helps avoid any confusion in the structure of the edix.cfg file.

If "Active" is unchecked, all key mappings immediately become ineffective.

Key mappings are reflected in the main menus, but only after Edix is restarted.
Uncheck "Ctrl CXV in menus" if you prefer to see Ctrl Insert, Ctrl Delete, and
Shift Insert (technically that flag is really "skip the keys in dkeymaps").

There is no way to map multiple keys to a single standard key. While you can map
<Shift Insert> to <Ctrl V> aka Paste, at which point (assuming you have not also
mapped <Ctrl V> to something else) then either key will perform that function,
you cannot also map say <Alt P> to <Ctrl V> at the same time as well. You may
or may not be able to achieve that sort of thing by editing edix.cfg directly,
but Options/Keyboard is best kept as simple and straightforward as possible.

This program may be incomplete: for instance <Ctrl Shift LeftArrow> is neither
shown nor handled properly. Naturally I selected the Standard keys to my taste
and am unlikely to ever use this myself, and therefore will never know what if
anything is missing and genuinely needed, except through appropriate feedback.
Also, at the time of writing, many as yet inoperative keys are being listed.
"""

--DEV/SUG: Add a radio Keyboard|Mappings, with Keyboard as-is and Mappings a
--  simpler two-column affair where both keys need specifying (add eic).
--  Would also allow multiple keys and possibly even graphic char mapping,
--  you could even have in-place edit for unicode, etc.

function help_cb(Ihandle /*ih*/)
    IupMessage("Keyboard",help_text)
    return IUP_DEFAULT
end function
constant cb_help = Icallback("help_cb")

function close_cb(Ihandle /*bt_close*/)
    if IupGetInt(key_dlg,"MAXIMIZED")=0 then
        IupConfigDialogClosed(config, key_dlg, "KeyDialog")
    end if
    IupHide(key_dlg) -- do not destroy, just hide
    return IUP_DEFAULT
end function
constant cb_close = Icallback("close_cb")

function resize_cb(Ihandle /*ih*/, integer width, integer height)
integer width1 = IupGetIntId(matrix,"RASTERWIDTH",1)
integer width2 = IupGetIntId(matrix,"RASTERWIDTH",2)
integer new_width = width-width1-width2-60
    IupSetIntId(matrix,"RASTERWIDTH",0,0) -- ("hide" line title)
    IupSetIntId(matrix,"RASTERWIDTH",3,new_width)
    integer vlines = max(4,floor((height-1)/23)-6)
    IupSetInt(matrix,"NUMLIN",max(length(standards),vlines))
    IupSetInt(matrix,"NUMLIN_VISIBLE",vlines)
    return IUP_DEFAULT
end function

function toggle_cb(Ihandle /*ih*/, integer state)
    IupConfigSetVariableInt(config, "KeyMappings", "Active", state)
    return IUP_DEFAULT
end function

function toggleCXV_cb(Ihandle /*ih*/, integer state)
    IupConfigSetVariableInt(config, "KeyMappings", "CtrlCXV", state)
    return IUP_DEFAULT
end function

function key_cb(Ihandle /*ih*/, atom c)
    if c=K_F1 then return help_cb(dlg) end if
    if c=K_ESC then return close_cb(key_dlg) end if
    --
    -- These range checks aren't set in stone.
    -- I have replicated the constants I meant to skip.
    --
    if c>=#FF00 and c<=#FF7F then
        --      K_MIDDLE = 0xFF0B,
        --      K_PAUSE  = 0xFF13,
        --      K_SCROLL = 0xFF14,
        --      K_ESC    = 0xFF1B,
        --      K_HOME   = 0xFF50,
        --      K_LEFT   = 0xFF51,
        --      K_UP     = 0xFF52,
        --      K_RIGHT  = 0xFF53,
        --      K_DOWN   = 0xFF54,
        --      K_PGUP   = 0xFF55,
        --      K_PGDN   = 0xFF56,
        --      K_END    = 0xFF57,
        --      K_Print  = 0xFF61,
        --      K_INS    = 0xFF63,
        --      K_Menu   = 0xFF67,
        --      K_NUM    = 0xFF7F,
        --  but these are let through:
        --  (K_F1    = 0xFFBE,
        --   K_F2    = 0xFFBF,
        --   K_F3    = 0xFFC0,
        --   K_F4    = 0xFFC1,
        --   K_F5    = 0xFFC2,
        --   K_F6    = 0xFFC3,
        --   K_F7    = 0xFFC4,
        --   K_F8    = 0xFFC5,
        --   K_F9    = 0xFFC6,
        --   K_F10   = 0xFFC7,
        --   K_F11   = 0xFFC8,
        --   K_F12   = 0xFFC9,)
    elsif c>=#FFE0 and c<=#FFFF then
        --      K_LSHIFT = 0xFFE1,
        --      K_RSHIFT = 0xFFE2,
        --      K_LCTRL  = 0xFFE3,
        --      K_RCTRL  = 0xFFE4,
        --      K_CAPS   = 0xFFE5,
        --      K_LALT   = 0xFFE9,
        --      K_RALT   = 0xFFEA,
        --      K_DEL    = 0xFFFF,
    elsif c>=#00 and c<=#FF then
        --      K_BS = 8,
        --      K_TAB = '\t',       -- 9
        --      K_LF = '\n',        -- 10 (0x0A)
        --      K_CR = '\r',        -- 13 (0x0D)
        --      plus A-Z, 0-9, etc
        --DEV filter??
    elsif selected_line!=0 then
        string ks = keystring(c)
        if match("??",ks)=0 then
            redraw_all = 0
            integer l = tags[selected_line]
            string km = keymaps[l]
            if ks=standards[l] then
                if km="" then
                    keymaps[l] = "*"
                    addkey(ks,"*")
                elsif km="*" and find(ks,keymaps) then
                    IupMessage("Cannot enable","Key is mapped to "&standards[find(ks,keymaps)])
                else
                    keymaps[l] = ""
                    if km="*" then
                        removekey(ks)
                    else
                        removekey(km)
                    end if
                end if
--          else
            elsif km!=ks then
                if km!="" then
                    removekey(km)
                end if
                km = standards[l]
                keymaps[l] = ""
                addkey(ks,km)
                keymaps[l] = ks
            end if
--?IupConfigGetVariableStr(config, "KeyMappings", "KEYS")
            if redraw_all then
                IupSetStrAttribute(matrix,"REDRAW", "ALL")
            else
                IupSetStrAttribute(matrix,"REDRAW", "L%d", {selected_line})
            end if
        end if
    end if
    return IUP_DEFAULT
end function

procedure create_key_dialog()

    matrix = IupMatrix()
    IupSetInt(matrix, "NUMCOL", length(titles))
    IupSetInt(matrix, "NUMCOL_VISIBLE", length(titles))
    IupSetInt(matrix, "NUMLIN", length(standards))
    IupSetInt(matrix, "NUMLIN_VISIBLE", 15)
    IupSetIntId(matrix, "RASTERWIDTH", 1, 80)
    IupSetAttribute(matrix, "ALIGNMENT", "ALEFT")
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

    bt_help = IupButton("Help","ACTION",cb_help,"PADDING=10x2")
    bt_ok   = IupButton("OK","ACTION",cb_close,"PADDING=10x2")
    tg_act  = IupToggle("Active",Icallback("toggle_cb"))
    -- Technically, this is really "ignore the keys in dkeymaps"...
    tg_cxv  = IupToggle("Ctrl CXV in menus",Icallback("toggleCXV_cb"))

    integer active = IupConfigGetVariableInt(config, "KeyMappings", "Active", 1)
    IupSetInt(tg_act,"VALUE",active)
    integer cxv = IupConfigGetVariableInt(config, "KeyMappings", "CtrlCXV", 1)
    IupSetInt(tg_cxv,"VALUE",cxv)
    Ihandle box = IupVbox({matrix,
                           IupFill(),
                           IupHbox({tg_act,tg_cxv,IupFill(),bt_help,bt_ok},"MARGIN=20x0")
                          });
    IupSetAttribute(box, "MARGIN", "10x10");
    IupSetAttribute(box, "GAP", "5");

    key_dlg = IupDialog(box)

    IupSetAttributePtr(key_dlg, "PARENTDIALOG", dlg)
    IupSetCallback(key_dlg, "CLOSE_CB", cb_close);
    IupSetAttribute(key_dlg, "TITLE", "Keyboard");
    IupSetCallback(key_dlg, "RESIZE_CB", Icallback("resize_cb"));
    IupSetCallback(key_dlg, "K_ANY", Icallback("key_cb"));
end procedure

constant {dkeys,dkeymaps} = columnize({{"<Alt G>","<Ctrl G>"},                      -- Goto Line
                                       {"<Ctrl Insert>","<Ctrl C>"},                -- Copy
                                       {"<Ctrl Shift Insert>","<Ctrl Shift C>"},    -- Copy Append
                                       {"<Shift Insert>","<Ctrl V>"},               -- Paste
                                       {"<Alt Insert>","<Ctrl '>'>"},               -- Comment
                                       {"<Ctrl Delete>","<Ctrl X>"},                -- Cut
                                       {"<Ctrl Shift Delete>","<Ctrl Shift X>"},    -- Cut Append
                                       {"<Alt Delete>","<Ctrl '<'>"},               -- Un-comment
                                       {"<Ctrl Tab>","<Alt '>'>"},                  -- Indent
                                       {"<Ctrl Shift Tab>","<Alt '<'>"}})           -- Un-Indent

procedure load_mapping()
-- note: load_standards() has not yet been called
--       (hence this should /not/ attempt to set keymaps)
    KEYDICT = new_dict()
    integer Active = IupConfigGetVariableInt(config, "KeyMappings", "Active", -1)
    if Active=-1 then -- initial setup
        IupConfigSetVariableStr(config, "KeyMappings", "KEYS", join(sort(dkeys),','))
        for i=1 to length(dkeys) do
            string key = dkeys[i]
            string keymap = dkeymaps[i]
            IupConfigSetVariableStr(config, "KeyMappings", key, keymap)
            setd(decodech(key),decodech(keymap),KEYDICT)
        end for
        IupConfigSetVariableInt(config, "KeyMappings", "Active", 1)
    else
        sequence Keys = split(IupConfigGetVariableStr(config, "KeyMappings", "KEYS"),',')
        for i=1 to length(Keys) do
            string key = Keys[i]
            string keymap = IupConfigGetVariableStr(config, "KeyMappings", key)
            setd(decodech(key),decodech(keymap),KEYDICT)
        end for
    end if
end procedure

function invtext(object key, object data, object /*user_data*/)
    string skey = keystring(key)
    string dkey
    if data='*' then
        dkey = "<*>"
    else
        dkey = keystring(data)
    end if
    skey = skey[2..-2]
    dkey = dkey[2..-2]
--?{dkey,skey}
    setd(dkey,skey,INVKEY)
    return 1
end function

procedure load_inverse()
--NB: not maintained as settings altered, but menu entries have already been created
    INVKEY = new_dict()
    traverse_dict(routine_id("invtext"),0,KEYDICT)
end procedure

function visitor(object key, object data, object /*user_data*/)
integer k
string sstd
    string skey = keystring(key)
    if data='*' then
        sstd = skey
        k = find(skey,standards)
        skey = "*"
    else
        sstd = keystring(data)
--      ?{key,data,skey,sstd}
        k = find(sstd,standards)
    end if
    if k=0 then
        ?"warning: "&sstd&" not found"
    else
        keymaps[k] = skey
    end if
    return 1
end function

constant stext = """
--
-- Line comments (--, not block aka /*..*/), leading/trailing whitespace, and blank lines are skipped.
-- Non-blank lines that do not start with "<", or contain "> is " trigger an error, as do combinations 
-- that cannot be deciphered. Single quotes can optionally be placed around graphic characters, for
-- example <Ctrl '+'>. They are however necessary for things like '\t'. "Insert" and "Delete" are
-- handled specially, as are F1..F12, and of course Ctrl, Shift, and Alt. Other special keys such as
-- "Home", "End, "LeftArrow" etc are not currently supported but should be fairly trivial to handle, 
-- should that ever prove necessary.
--
<Ctrl A> is Select All
<Ctrl B> is Show As Binary
<Ctrl C> is Copy
<Ctrl D> is Duplicate Line
<Ctrl F> is Find
<Ctrl G> is Goto Line
<Ctrl H> is Show As Hex
<Ctrl J> is Show As Seq/Str
<Ctrl K> is Show As Decimal
<Ctrl L> is Toggle File Panel
--<Ctrl M> is Toggle Message Area
<Ctrl N> is New File
<Ctrl O> is Open File
<Ctrl P> is Print Preview
<Ctrl Q> is Quick Jump
<Ctrl S> is Save File
<Ctrl T> is File List
<Ctrl V> is Paste
<Ctrl W> is Select Word
<Ctrl X> is Cut
<Ctrl Y> is Redo
<Ctrl Z> is Undo
<Ctrl ']'> is Next Control Structure
<Ctrl '['> is Prior Control Structure
--<Ctrl '-'> is Fold Selection Or Control Block
--<Ctrl '+'> is Expand Fold
<Ctrl '>'> is Comment
<Ctrl '<'> is Un-Comment
<Ctrl F1> is Keyboard Help
--<Ctrl F2> is Toggle Bookmark
<Ctrl F3> is Find First Selected
<Ctrl F4> is Close File
<Ctrl F5> is Parameterised Run
<Ctrl F6> is Record Macro F6
<Ctrl F7> is Record Macro F7
<Ctrl F8> is Record Macro F8
<Ctrl F9> is Record Macro F9
--<Ctrl Insert> is Copy
--<Ctrl Delete>:=<Ctrl X>               -- Cut
--<Ctrl '\t'>:=<Alt '>'>                -- Indent
<Ctrl Shift B> is Show As Octal
<Ctrl Shift C> is Copy Append
<Ctrl Shift E> is Enlarge Font
<Ctrl Shift F> is Find In Files
<Ctrl Shift H> is Show Formatting
<Ctrl Shift O> is ReOpen File
<Ctrl Shift P> is Print
<Ctrl Shift R> is Reduce Font
<Ctrl Shift X> is Cut Append
--<Ctrl Shift -> is Re-fold Last Expansion
<Ctrl Shift ']'> is Next Control Structure (with block select)
<Ctrl Shift '['> is Prior Control Structure (with block select)
--<Ctrl Shift F2> is Remove All Bookmarks
<Ctrl Shift F3> is Find Next Selected
--<Ctrl Shift F5> is Run Direct*
--<Ctrl Shift Insert>:=<Ctrl Shift C>   -- Copy Append
--<Ctrl Shift Delete>:=<Ctrl Shift X>   -- Cut Append
--<Ctrl Shift '\t'>:=<Alt '<'>          -- Un-Indent
<Alt B> is Move Line Back (up)
<Alt D> is Move Line Down
<Alt I> is Invert Case
<Alt L> is Lower Case
--<Alt P> is Print
<Alt U> is Upper Case
--<Alt W> is Wordwrap
--<Alt Z> is Zoom?
--<Alt '-'> is Fold All Routines
--<Alt '+'> is Expand All Folds
<Alt '>'> is Indent
<Alt '<'> is Un-Indent
<Alt Shift C> is Copy Prepend
<Alt Shift L> is Sentence Case
<Alt Shift U> is Capitalise Case
<Alt Shift X> is Cut Prepend
<Alt F4> is Exit
--<Alt Insert>:=<Ctrl '>'>              -- Comment
--<Alt Delete>:=<Ctrl '<'>              -- Un-comment
<F1> is Context Help
--<F2> is Next Bookmark
<F3> is Find Next
<F4> is Next Difference
<F5> is Run
<F6> is Play Macro F6
<F7> is Play Macro F7
<F8> is Play Macro F8
<F9> is Play Macro F9
<F12> is Jump To Error
--<Shift F2> is Previous Bookmark
<Shift F3> is Find Prior
<Shift F4> is Prior Difference
<Shift F5> is Run Previous
<Shift F8> is Macro Management
--<Shift Insert> is Paste
"""

procedure load_standards()
-- note: load_mapping() /has/ already been called
    sequence skeys = split(stext,'\n')
    for i=1 to length(skeys) do
        string line = skeys[i]
        line = trim(line[1..match("--",line)-1])
        if length(line) then
            integer k = match("> is ",line)
            if k!=0 and line[1]='<' then
                string skey = line[1..k]
                atom ch1 = decodech(skey)
                string desc = line[k+5..$]
                if ch1=0 or length(desc)=0 then
                    k = 0
                else
                    keymaps = append(keymaps,"") -- set below/in visitor()
                    standards = append(standards,skey)
                    descs = append(descs,desc)
                end if
            else
                k = 0
            end if
            if k=0 then
                ?{"keyboard.e line 710, error in stext",line,i}
                exit
            end if
        end if
    end for
    tags = tagset(length(standards))

    -- set keymaps[] from the prior load_mapping:
    traverse_dict(routine_id("visitor"),0,KEYDICT) 

    -- and disable anything being used elsewhere:
    for l=1 to length(keymaps) do
        if keymaps[l]="" 
        and find(standards[l],keymaps) then
            keymaps[l] = "*"
        end if
    end for
end procedure

global procedure keyboard_dialog()
    if KEYDICT=0 then load_mapping() end if
    if standards={} then load_standards() end if
    if key_dlg=NULL then
        create_key_dialog()
    end if
    IupConfigDialogShow(config, key_dlg, "KeyDialog")
    IupPopup(key_dlg,IUP_CURRENT,IUP_CURRENT)
end procedure

function keyboard_cb(Ihandle /*item_keyboard*/)
    keyboard_dialog()
    return IUP_DEFAULT
end function
global constant cb_keybd = Icallback("keyboard_cb")

global function mapkey(atom key)
    if IupConfigGetVariableInt(config, "KeyMappings", "Active", 1) then
        if KEYDICT=0 then load_mapping() end if
        integer node = getd_index(key,KEYDICT)
        if node!=0 then
            key = getd_by_index(node,KEYDICT)
            if key='*' then return NULL end if
        end if
    end if
    return key
end function

global function mapkeytext(string key)
    if IupConfigGetVariableInt(config, "KeyMappings", "Active", 1) then
        if KEYDICT=0 then load_mapping() end if
        if INVKEY=0 then load_inverse() end if
        if IupConfigGetVariableInt(config, "KeyMappings", "CtrlCXV", 1)=0
        or not find(sprintf("<%s>",{key}),dkeymaps) then
            integer node = getd_index(key,INVKEY)
            if node!=0 then
                key = getd_by_index(node,INVKEY)
            end if
        end if
    end if
    return key
end function
