--
-- builtins\xpGUI_from_pGUI.e
-----------------------------
--
--  A simple mini-helper intended to alleviate most of the tedious editing 
--  otherwise needed when porting code from pGUI to xpGUI
--  Written with simplicitly in mind, rather than outright performance.
--
local function substiqute(string res, dqname, name)
    res = substitute(res,dqname,name)
    res = substitute(res,substitute(dqname,`"`,"`"),name)
    return res
end function

-- forward function mini_porting_helper(bool bJustCheck)
-- if bJustCheck is false, converts the clipboard contents, as per the
--  "porting from pGUI" entry in the xpGUI docs (just text mangling).
-- returns true if the clipboard contains "include pGUI.e" (not xpGUI.e)

global function xpGUI_from_pGUI(string src)
    string res = substitute(src,"include pGUI.e","requires(\"1.0.3\")\ninclude xpGUI.e")
    res = substitute(res,"include IupGraph.e","")
    res = substitute(res,`IupOpen()`,``)
    res = substitute(res,`IupClose()`,``)
    sequence icb = reverse(match_all(`Icallback("`,res))
    for s in icb do
        integer e = match(`")`,res,s+11)
        res[e..e+1] = ""
        res[s..s+10] = ""
    end for
    res = substiqute(res,`"K_ANY"`, "`KEY`")
    res = substiqute(res,`"BUTTON_CB"`,"`CLICK`")
    res = substiqute(res,`"PARENTDIALOG"`,`?PARENTDIALOG?`)
    res = substiqute(res,`"MAP_CB"`,`?MAP_CB?`)
    res = substiqute(res,`"UNMAP_CB"`,`?UNMAP_CB?`)
    res = substiqute(res,`"TITLESTYLE"`,`?TITLESTYLE?`)
    res = substiqute(res,`"STARTFOCUS"`,`?STARTFOCUS(use gSetFocus)?`)
--hmm:
    res = substitute(res,`RASTERSIZE`,`RASTERZISE`)
    res = substitute(res,`SIZE`,`SIZE[*charsize]`)
    res = substitute(res,`RASTERZISE`,`SIZE`)
//let cdEncodeColor = rgba;
//let cdEncodeColorAlpha = rgba;
//let cdDecodeColor = to_rgba;
//let cdDecodeColorAlpha = to_rgba;
    -- (probably wisest to keep these in alphabetic order:) -- `Menu`,`Submenu` deemed not worth it
    for s in {`Button`,`Canvas`,`Clipboard`,`DatePick`,`Dialog`,`Frame`,`Graph`,`Hbox`,`Hide`,`Label`,`MainLoop`,
              `Map`,`ProgressBar`,`Separator`,`Show`,`Table`,`Tabs`,`Text`,`Timer`,`TreeView`,`Vbox`} do
        res = substitute(res,`Iup`&s,`g`&s)
    end for
--  `cdXxxx` is replaced with `gXxxx`..
    for s in {{{`rgb`,`cdEncodeColorAlpha`,`cdEncodeColor`},`rgba`},
--            {{`hsv_to_rgb`},`hsv_to_rgba`},
--            {{`to_rgb`,`cdDecodeColorAlpha`,`cdDecodeColor`},`to_rgba`},
              {{`CD_AMBER`,`IUP_AMBER`},`XPG_AMBER`},
              {{`CD_BLACK`,`IUP_BLACK`},`XPG_BLACK`},
              {{`CD_BLUE`,`IUP_BLUE`},`XPG_BLUE`},
              {{`CD_CYAN`,`IUP_CYAN`},`XPG_CYAN`},
              {{`CD_DARK_BLUE`,`IUP_DARK_BLUE`,`CD_NAVY`,`IUP_NAVY`},`XPG_NAVY`},
              {{`CD_DARK_CYAN`,`IUP_DARK_CYAN`,`CD_TEAL`,`IUP_TEAL`},`XPG_TEAL`},
              {{`CD_DARK_GRAY`,`CD_DARK_GREY`,`IUP_DARK_GRAY`,`IUP_DARK_GREY`},`XPG_DARK_GREY`},
              {{`CD_GRAY`,`CD_GREY`,`IUP_GRAY`,`IUP_GREY`},`XPG_GREY`},
              {{`CD_LIGHT_GRAY`,`CD_LIGHT_GREY`,`IUP_LIGHT_GRAY`,`IUP_LIGHT_GREY`},`XPG_LIGHT_GREY`},
              {{`CD_DARK_GREEN`,`IUP_DARK_GREEN`},`XPG_DARK_GREEN`},
              {{`CD_DARK_MAGENTA`,`IUP_DARK_MAGENTA`},`XPG_DARK_PURPLE`},
              {{`CD_DARK_RED`,`IUP_DARK_RED`},`XPG_DARK_RED`},
              {{`CD_DARK_YELLOW`,`IUP_DARK_YELLOW`},`XPG_DARK_YELLOW`},
              {{`CD_GREEN`,`IUP_GREEN`},`XPG_GREEN`},
              {{`CD_INDIGO`,`IUP_INDIGO`},`XPG_INDIGO`},
              {{`CD_LIGHT_BLUE`,`IUP_LIGHT_BLUE`},`XPG_LIGHT_BLUE`},
              {{`CD_LIGHT_GREEN`,`IUP_LIGHT_GREEN`},`XPG_LIGHT_GREEN`},
              {{`CD_LIGHT_PARCHMENT`,`IUP_LIGHT_PARCHMENT`},`XPG_LIGHT_PARCHMENT`},
              {{`CD_MAGENTA`,`IUP_MAGENTA`},`XPG_MAGENTA`},
              {{`CD_OLIVE`,`IUP_OLIVE`},`XPG_OLIVE`},
              {{`CD_ORANGE`,`IUP_ORANGE`},`XPG_ORANGE`},
              {{`CD_PARCHMENT`,`IUP_PARCHMENT`},`XPG_PARCHMENT`},
              {{`CD_PURPLE`,`IUP_PURPLE`},`XPG_PURPLE`},
              {{`CD_RED`,`IUP_RED`},`XPG_RED`},
              {{`CD_SILVER`,`IUP_SILVER`},`XPG_SILVER`},
              {{`CD_VIOLET`,`IUP_VIOLET`},`XPG_VIOLET`},
              {{`CD_WHITE`,`IUP_WHITE`},`XPG_WHITE`},
              {{`CD_YELLOW`,`IUP_YELLOW`},`XPG_YELLOW`},
              {{`cdDecodeColorAlpha`,`cdDecodeColor`},`to_rgba`},
              {{`Ihandles`,`Ihandle`,`Ihandlns`,`Ihandln`},`gdx`},
              {{`IupValuator`,`IupVal`},`gSlider`},
--            {{`IupMenuItem`,`IupItem`},`gMenuItem`},
              {{`IupSetAttribute`,`IupSetStrAttribute`,`IupStoreAttribute`},`gSetAttribute`},
              {{`IupSetCallback`,`IupSetCallbacks`},`gSetHandler`},
--??          {{`IupNormaliser(`,`IupNormalizer(`},`;gNormalise(""[?=NORMALIZE],`},
              {{`IupGetAttribute`},`gGetAttribute`}} do
        for t in s[1] do
            res = substitute(res,t,s[2])
        end for
    end for
    for s in {{`cdCanvasArc`,`gCanvasArc`},
              {`cdCanvasChord`,`gCanvasArc[with flags|=XPG_CHORD]`},
              {`cdCanvasSector`,`gCanvasArc[with flags|=XPG_SECTOR]`},
              {`cdCanvasBox`,`gCanvasRect[filled:=true]`},
              {`cdCanvasFlush`,`gCanvasSetBackground[original]`},
              {`cdCanvasGetBackground`,`gCanvasGetBackground`},
              {`cdCanvasGetForeground`,`gCanvasGetForeground`},
              {`cdCanvasSetBackground`,`gCanvasSetBackground`},
              {`cdCanvasSetForeground`,`gCanvasSetForeground`},
              {`cdCanvasSetTextAlignment`,`gCanvasSetTextAlignment?[use gCanvasText(align:=x) instead]`},
              {`cdCanvasSetTextOrientation`,`gCanvasSetTextOrientation?[use gCanvasText(angle:=x) instead]`},
              {`cdCanvasLine`,`gCanvasLine`},
              {`cdCanvasPixel`,`gCanvasPixel`},
              {`cdCanvasRect`,`gCanvasRect`},
              {`cdCanvasRect`,`gCanvasRect`},
              {`cdCanvasText`,`gCanvasText`},
              {`CD_BASE_CENTER`,`XPG_SOUTH`},
              {`CD_BASE_LEFT`,`XPG_SOUTHWEST`},
              {`CD_BASE_RIGHT`,`XPG_SOUTHEAST`},
              {`CD_CENTER`,`0`},
              {`CD_EAST`,`XPG_EAST`},
              {`CD_NORTH`,`XPG_NORTH`},
              {`CD_NORTH_EAST`,`XPG_NORTHEAST`},
              {`CD_NORTH_WEST`,`XPG_NORTHWEST`},
              {`CD_SOUTH`,`XPG_SOUTH`},
              {`CD_SOUTH_EAST`,`XPG_SOUTHEAST`},
              {`CD_SOUTH_WEST`,`XPG_SOUTHWEST`},
              {`CD_WEST`,`XPG_WEST`},
              {`CD_RAD2DEG`,`XPG_RAD2DEG`},
              {`CD_DEG2RAD`,`XPG_DEG2RAD`},
              {`IupGetClassName(`,`gGetAttribute(?,"CLASSNAME",`},
              {`IupGetInt`,`gGetInt`},
              {`IupGetIntInt`,`gGetAttribute`},
--            {`IupMenu`,`gMenu`},  -- not worth it...
              {`IupPopup`,`gPopup`},
              {`IupShowXY`,`gShow`},
              {`IupToggle`,`gCheckbox`},
              {`IupVersion`,`gVersion`},
              {`XPG_BUTTON1`,`"CLICK"status[1]='L'`},
              {`XPG_BUTTON2`,`"CLICK"status[1]='M'`},
              {`XPG_BUTTON3`,`"CLICK"status[1]='R'`},
              {`XCROSSORIGIN`,`XACROSS`},
              {`YCROSSORIGIN`,`YACROSS`}} do
        res = substitute(res,s[1],s[2])
    end for
    for s in {`BRANCHOPEN`,`KEY`} do
        res = substiqute(res,`"`&s&`_CB"`,"`"&s&"`")
    end for
    res = substitute(res,`IUP_`,`XPG_`)
    res = substitute(res,`K_`,`VK_`)
    return res
end function

