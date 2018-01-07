--
-- macro.e
--
--  Implements RecordMacro(), PlayMacro(), StopMacroLearn().
--  Entry points via macrokey and cb_macro.
--
global integer  MacroLearn = 0
global integer  MacroPlaying = 0

global integer r_macro_key  -- set to routine_id of key_cb() in edix.exw (for playback)

procedure RecordMacro(integer k)
-- k is 1..4 for F6..F9
    if MacroLearn then  -- recording in progress
        if k!=MacroLearn then
            string msg = sprintf("cannot record F%s abd F%s at the same time",{'5'+k,'5'+MacroPlaying})
            IupMessage("Error",msg)
        end if
        MacroLearn=0    -- stop recording
        IupSetAttribute(sbmsg,"TITLE","")
    else
        MacroLearn = k  -- start recording
        string msg = sprintf("Press Ctrl F%s to stop recording macro",'5'+k)
        IupSetAttribute(sbmsg,"TITLE",msg)
        MacroKeys[MacroLearn]={}
    end if
end procedure

procedure PlayMacro(integer k)
-- k is 1..4 for F6..F9
integer savePlay, saveLearn
    if MacroPlaying then
        if k>=MacroPlaying then
            string msg = sprintf("cannot playback F%s from F%s",{'5'+k,'5'+MacroPlaying})
            IupMessage("Error",msg)
            MacroPlaying = 0
            return
        end if
    end if
    if MacroLearn then
        if k=MacroLearn then    -- stop recording and play back immediately
            MacroLearn = 0
            IupSetAttribute(sbmsg,"TITLE","")
        elsif k>MacroLearn then
            string msg = sprintf("cannot record F%s from F%s",{'5'+k,'5'+MacroPlaying})
            IupMessage("Error",msg)
            MacroLearn = 0
            IupSetAttribute(sbmsg,"TITLE","")
            return
        end if
    end if
    --
    -- if eg during recording of F7, the F6 key is pressed, play it back with learn mode off.
    --
    saveLearn = MacroLearn
    MacroLearn = 0
    --
    -- Use a simple integer var to detect invalid/infinite looping (see msg above)
    --
    savePlay = MacroPlaying
    MacroPlaying = k
    
    for i=1 to length(MacroKeys[k]) do
        {} = call_func(r_macro_key,{dlg,MacroKeys[k][i]})
        if MacroPlaying=0 then exit end if -- error in a nested F key? (also Find)
    end for

    if MacroPlaying then -- providing no error,
        MacroPlaying = savePlay
        --
        -- If as above, save the F6 key under F7 (rather than all the keys that F6 played)
        --
        MacroLearn = saveLearn
    end if
end procedure

--DEV alt-home, alt-end??
global procedure StopMacroLearn(integer showMsg)
    if MacroLearn then
        MacroLearn = 0
        if showMsg then
            IupMessage(xl("Note"),xl("Macro recording stopped"))
        end if
        IupSetAttribute(sbmsg,"TITLE","")
    end if
end procedure

global procedure macrokey(atom base, integer ctrl, shift, /*alt*/)
    integer key = find(base,{K_F6,K_F7,K_F8,K_F9})
    if ctrl then    -- start/stop recording
        RecordMacro(key)
    elsif shift then
        StopMacroLearn(0)
    else
        PlayMacro(key)
    end if
end procedure

function macro_cb(atom self)
integer key = IupGetInt(self,"ID")
    if key<10 then
        RecordMacro(key)
    else
        PlayMacro(key-10)
    end if
    return IUP_IGNORE
end function
global constant cb_macro = Icallback("macro_cb")


