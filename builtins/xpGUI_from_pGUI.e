--
-- builtins\xpGUI_from_pGUI.e
-----------------------------
--
--  A simple mini-helper intended to alleviate most of the tedious editing 
--  otherwise needed when porting code from pGUI to xpGUI
--  Written with simplicitly in mind, rather than outright performance.
--  This program is expected/hoped to die a natural death mid/late 2024.
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

--function sub_comma(string res, cd, g, a, integer nc, rc)
function sub_comma(string res, sequence cr)
--DEV p2js:
--  {string {cd,g,a}, integer {nc,rc}} = cr
    string cd,g,a; integer nc,rc;
    {{cd,g,a}, {nc,rc}} = cr
    -- for each <cd> in res:
    --  replace with <g>, check there are nc commas before the ')',
    --                    and replace the rc'th comma with <a>.
    -- eg: cdCanvasRoundedBox(canvas,xmin,xmax,ymin,ymax,w,h)
    -- ==> gCanvasRect(?[gdx]canvas,xmin,xmax,ymin,ymax, [bFill,rc:]w,h)
    --  - as per the first entry in constant comma_rep[] below.
    while true do
        integer k = match(cd,res)
        if k=0 then exit end if
        res[k..k+length(cd)-1] = g
        k += length(cd)
        integer cb = find(')',res,k)
        sequence commas = find_all(',',res[k..cb])
        assert(length(commas)==nc)
        k += commas[rc]-1
        assert(res[k]=',')
        res[k..k] = a
    end while
    return res
end function

--DEV/SUG make substitute_at() a builtin? (or add rFilter=-1 to the regular?)
--  idii: a filtered list of places where needle exists (eg from find_all() initially).<br>
function substitute_at(sequence text, idii, object s, r)
    -- replace all instances of s at some idii[] in text with r
    -- each element of idii is verified to point at an s, and be in order
    -- eg res = substitute(res,`K_`,`VK_`) for say K_F1 => VK_F1 was also
    --      mangling XPG_DARK_GREEN (etc): filter(find_all('K_'),?) rqd.
    integer l = iff(atom(s)?1:length(s)),
            startidx = 1 --, and a chunkstart=1 for rFilter?
    sequence chunks = {}
    assert(l>0)
    for k in idii do
        assert(text[k..k+l-1]==s)
        assert(k>startidx)
--      if rFilter=-1 or rFilter(text,k) then
            chunks = append(chunks,text[startidx..k-1])
--          chunks = append(chunks,text[chunkstart..k-1])
--          chunkstart = k+l
--      end if
        startidx = k+l
    end for
--  if length(chunks) then
    if startidx!=1 then
--  if chunkstart!=1 then
        chunks = append(chunks,text[startidx..$])
--      chunks = append(chunks,text[chunkstart..$])
        text = chunks[1]
        for i=2 to length(chunks) do
            text &= r
            text &= chunks[i]
        end for
    end if
    return text
end function

function id_char(integer ch)
    return (ch>'A' and ch<='Z')
        or (ch>'a' and ch<='z')
        or (ch>'0' and ch<='9')
        or ch='_'
end function

global function xpGUI_from_pGUI(string src)
    string res = substitute(src,"include pGUI.e","requires(\"1.0.4\")\ninclude xpGUI.e")
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
    res = substiqute(res,`"PARENTDIALOG"`,`?PARENTDIALOG?[DEAD]`)
    res = substiqute(res,`"MAP_CB"`,`?MAP_CB?[DEAD]`)
    res = substiqute(res,`"UNMAP_CB"`,`?UNMAP_CB?[DEAD]`)
    res = substiqute(res,`"WHEEL_CB"`,`?MOUSEWHEEL?`)
    res = substiqute(res,`"TITLESTYLE"`,`?TITLESTYLE?`)
    res = substiqute(res,`"STARTFOCUS"`,`?STARTFOCUS(use gSetFocus)?`)
--  res = substiqute(res,`"DEFAULTENTER"`,`?DEFAULTENTER(use a KEY handler instead)?`)
--  res = substiqute(res,`"DEFAULTESC"`,`?DEFAULTESC(use a KEY handler instead)?`)
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
              /*`Map`,*/`ProgressBar`,/*`Separator`,*/`Table`,`Tabs`,`Text`,`TreeView`,`Vbox`} do
        res = substitute(res,`Iup`&s,`g`&s)
    end for
--  `cdXxxx` is replaced with `gXxxx`..
    -- all multiple => 1 mappings
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
              {{`IupRedraw`,`IupRefresh`,`IupUpdate`},`gRedraw`},
              {{`IupSetAttribute`,`IupSetStrAttribute`,`IupStoreAttribute`},`gSetAttribute`},
              {{`IupSetCallback`,`IupSetCallbacks`},`gSetHandler`},
              {{`IupShow`,`IupShowXY`},`gShow`},
--??          {{`IupNormaliser(`,`IupNormalizer(`},`;gNormalise(""[?=NORMALIZE],`},
              {{`IupGetAttributePtr`,`IupGetAttribute`},`gGetAttribute`}} do
        for t in s[1] do
            res = substitute(res,t,s[2])
        end for
    end for
    -- all 1:1 mappings:
    for s in {{`cdCanvasArc`,`gCanvasArc`},
              {`cdCanvasChord`,`gCanvasArc[with flags|=XPG_CHORD]`},
              {`cdCanvasCircle`,`gCanvasCircle`},
              {`cdCanvasSector`,`gCanvasArc[with flags|=XPG_SECTOR]`},
              {`cdCanvasBox`,`gCanvasRect[filled:=true]`},
              {`cdCanvasActivate`,`--cdCanvasActivate`},
              {`cdCanvasClear`,`--cdCanvasClear`},
              {`cdCanvasFlush`,`--cdCanvasFlush`},
              {`cdCanvasFont(`,`gSetAttribute(canvas?,"FONT",`},
              {`cdCanvasGetBackground(`,`gGetAttribute(?,"BGCLR",`},
              {`cdCanvasGetForeground(`,`gGetAttribute(?,"FGCLR",`},
              {`cdCanvasSetBackground(`,`gSetAttribute(?,"BGCLR",`},
              {`cdCanvasSetForeground(`,`gSetAttribute(?,"FGCLR",`},
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
--            {`CD_CENTER`,`0`},
              {`CD_CENTER`,`XPG_CENTRE`},
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
              {`RIGHTBUTTON`,`!RIGHTBUTTON:NOT SUPPORTED!`},
              {`VALUE_HANDLE`,`VALUE_HANDLE==>VALUE[POS] on gTabs(), or gRadioItem(any)...`},
              {`IupGetClassName(`,`gGetAttribute(?,"CLASSNAME",`},
              {`IupGetInt`,`gGetInt`},
--            {`IupGetIntInt`,`gGetAttribute`},
              {`IupGetIntInt`,`gGetIntInt`},
              {`IupGLCanvas`,`gCanvas`},
--            {`IupMenu`,`gMenu`},  -- not worth it...
              {`IupMessage(`,`gMsgBox(?parent?,`},
              {`IupPopup`,`gPopup`},
              {`IupRadio`,`gRadio[now a proc]`},
              {`IupSetFocus`,`gSetFocus`},
              {`IupSetInt`,`gSetInt`},
              {`IupTimer`,`gTimer[cb->proc]`},
              {`IupToggle`,`gCheckbox`},
              {`IupVersion`,`gVersion`},
              {`XPG_BUTTON1`,`"CLICK"status[1]='L'`},
              {`XPG_BUTTON2`,`"CLICK"status[1]='M'`},
              {`XPG_BUTTON3`,`"CLICK"status[1]='R'`},
              {`XCROSSORIGIN`,`XACROSS`},
              {`YCROSSORIGIN`,`YACROSS`},
              {`LEGENDPOSXY`,`LEGENDXY`},
              {`VALUECHANGED_CB`,`VALUE_CHANGED`},
              {`valuechanged_cb`,`value_changed`}} do
        res = substitute(res,s[1],s[2])
    end for

    for comma_rep in {{{`cdCanvasRoundedBox(`,`gCanvasRect(?[gdx]`,`, [bFill(true), rc(*2):]`},{6,5}}} do
        res = sub_comma(res,comma_rep)
    end for

    for s in {`BRANCHOPEN`,`KEY`} do
        res = substiqute(res,`"`&s&`_CB"`,"`"&s&"`")
    end for
    res = substitute(res,`IUP_`,`XPG_`)
--oops, XPG_DARK_GREEN -> XPG_DARVK_GREEN... (and a few others, eg CLICK_CB)
--  res = substitute(res,`XPG_DARK_`,`XPG_DAKR_`)
--  res = substitute(res,`K_`,`VK_`)
--  res = substitute(res,`XPG_DAKR_`,`XPG_DARK_`)
--  for k in reverse(find_all(`K_`,res)) do
--      if res[k-1]!='R' then
--      if find(res[k-1],", \r\n({[") then
--          res[k..k+1] = `VK_`
--      end if
--  end for
    sequence idii = match_all(`K_`,res),
             -- extend as needed... (after checking they are also valid as VK_XXX)
             ok_keys = {`K_CR`,`K_DEL`,`K_DOWN`,`K_ESC`,`K_F1`,`K_F2`,`K_F5`,`K_LEFT`,`K_RIGHT`,`K_UP`} 
--?{"idii",idii}
--/*
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:674 global constant VK_SP   = #20   -- aka ' '
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:677 local constant key_mappings = {{"VK_BS",        VK_BS       := #08,     #08, #FF08}, -- aka '\b'
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:678                                {"VK_TAB",       VK_TAB      := #09,     #09, #FF09}, -- aka '\t'
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:680                                {"VK_LF",        VK_LF       := #0A,     #0A, #FF0A}, -- aka '\n'
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:682                                {"VK_PAUSE",     VK_PAUSE    := #13,     #13, #FF13}, -- (GTK3+ only, not GTK2)
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:683                                {"VK_CAPSLOCK",  VK_CAPSLOCK := #14,     #14, #0000}, -- (not GTK)
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:686                                {"VK_LCTRL",     VK_LCTRL    := #17,     #1D, #FFE3},
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:687                                {"VK_RCTRL",     VK_RCTRL    := #18,    #11D, #FFE4},
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:688                                {"VK_LALT",      VK_LALT     := #19,   #2038, #FFE9},
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:689                                {"VK_RALT",      VK_RALT     := #1A,   #2138, #FFEA},
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:694                                {"VK_NUMLOCK",   VK_NUMLOCK  := #90,     #90, #FF7F},
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:700                                {"VK_APPS",      VK_APPS     := #E0,     #5D, #FF67},
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:701                                {"VK_INS",       VK_INS      := #E1,     #2D, #FF63},
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:702                                {"VK_SCROLL",    VK_SCROLL   := #E2,     #91, #FF14},
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:703                                {"VK_PGUP",      VK_PGUP     := #E4,     #21, #FF55},
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:704                                {"VK_PGDN",      VK_PGDN     := #E5,     #22, #FF56},
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:705                                {"VK_END",       VK_END      := #E6,     #23, #FF57},
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:706                                {"VK_HOME",      VK_HOME     := #E7,     #24, #FF50},
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:717                                {"VK_APPS",      VK_APPS     := #E0,     #5D, #FF67},
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:718                                {"VK_INS",       VK_INS      := #E1,     #2D, #FF63},
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:719                                {"VK_HOME",      VK_HOME     := #E2,     #24, #FF50},
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:720                                {"VK_END",       VK_END      := #E3,     #23, #FF57},
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:721                                {"VK_PGUP",      VK_PGUP     := #E4,     #21, #FF55},
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:722                                {"VK_PGDN",      VK_PGDN     := #E5,     #22, #FF56},
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:723                                {"VK_NUMLOCK",   VK_NUMLOCK  := #90,     #90, #FF7F},
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:724                                {"VK_SCROLL",    VK_SCROLL   := #E6,     #91, #FF14},
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:742                                {"VK_F3",        VK_F3       := #F3,     #72, #FFC0},
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:743                                {"VK_F4",        VK_F4       := #F4,     #73, #FFC1},
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:745                                {"VK_F6",        VK_F6       := #F6,     #75, #FFC3},
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:746                                {"VK_F7",        VK_F7       := #F7,     #76, #FFC4},
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:747                                {"VK_F8",        VK_F8       := #F8,     #77, #FFC5},
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:748                                {"VK_F9",        VK_F9       := #F9,     #78, #FFC6},
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:749                                {"VK_F10",       VK_F10      := #FA,     #79, #FFC7},
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:750                                {"VK_F11",       VK_F11      := #FB,     #7A, #FFC8},
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:751                                {"VK_F12",       VK_F12      := #FC,     #7B, #FFC9}}
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:5860         if ctrl or (not bGraphic and wParam!=VK_SP) then
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:5869 --      if wParam!=VK_BS then
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:5892     if wParam=VK_TAB then
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:6783 --                     (msg=WM_CHAR and wParam=VK_RETURN)) then
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:6785     elsif (msg=WM_CHAR and id=TREE1 and wParam=VK_RETURN)
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:12820             pg = (c=VK_PGUP or c=VK_PGDN),
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:12834         case VK_PGUP:           origy -= step
C:\Program Files (x86)\Phix\demo\xpGUI\xpGUI.e:12835         case VK_PGDN:           origy += step

--*/
    for i=length(idii) to 1 by -1 do
        integer k = idii[i], pch = res[k-1]
--printf(1,"i:%d, k:%d, pch:%c\n",{i,k,pch})
--      if find(pch,", \r\n") then
--      if find(pch,"") then
        if id_char(pch) then
--?"id_char"
            idii[i..i] = {}
        else
            integer l = k+2
            while id_char(res[l]) do l += 1 end while
            string k_key = res[k..l-1]
--?{"k_key",k_key}
            assert(find(k_key,ok_keys)!=0,"unknown key?:%s",{k_key})
--if not find(pch,"") then
--  printf(1,"`K_` after '%c' -> `VK_`?\n",{pch})
--  wait_key()
--end if
--          idii[i..i] = {}
        end if
    end for
--?{"idii(end for)",idii}
    res = substitute_at(res,idii,`K_`,`VK_`)

    return res
end function

--/*
erm:

--*/
