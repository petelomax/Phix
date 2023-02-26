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
    string res = substitute(src,`include pGUI.e`,`include xpGUI.e`)
    res = substitute(res,`IupOpen()`,``)
    res = substitute(res,`IupClose()`,``)
    res = substitute(res,`Ihandle`,`gdx`)
    res = substitute(res,`Ihandln`,`gdx`)
    res = substitute(res,`Ihandles`,`gdx`)
    res = substitute(res,`Ihandlns`,`gdx`)
    sequence icb = reverse(match_all(`Icallback("`,res))
    for s in icb do
        integer e = match(`")`,res,s+11)
        res[e..e+1] = ""
        res[s..s+10] = ""
    end for
    res = substitute(res,`IupSetAttribute`,   `gSetAttribute`)
    res = substitute(res,`IupSetStrAttribute`,`gSetAttribute`)
    res = substitute(res,`IupStoreAttribute`, `gSetAttribute`)
    res = substitute(res,`IupSetCallback(`,  `gSetHandler(?`)
    res = substitute(res,`IupSetCallbacks(`,  `gSetHandler(?`)
    res = substiqute(res,`"K_ANY"`, "`KEY`")
    res = substiqute(res,`"PARENTDIALOG"`,`?PARENTDIALOG?`)
    -- (probably wisest to keep these in alphabetic order:)
    for s in {`Button`,`Canvas`,`Dialog`,`Hide`,`MainLoop`,`Map`,
              `Show`,`Text`,`TreeView`} do
        res = substitute(res,`Iup`&s,`g`&s)
    end for
--  `cdXxxx` is replaced with `gXxxx`.. (none yet)
    for s in {`BRANCHOPEN`,`KEY`} do
        res = substiqute(res,`"`&s&`_CB"`,"`"&s&"`")
    end for
    res = substitute(res,`IUP_`,`XPG_`)
    res = substitute(res,`K_`,`VK_`)
    return res
end function

