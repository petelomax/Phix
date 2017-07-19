--
-- pCrashN.e
-- =========
--
--  Phix implementation of the crash() routine.
--  crash_message (:%pCrashMsg) is part of the optable/pDiagN.e, this is not.
--

--DEV this should perhaps have an optional parameter to pop more frames.
--without debug
global procedure crash(string fmt, object data={})  --, integer nFrames=1)
    if atom(data) or length(data) then
        fmt = sprintf(fmt, data)
    end if
    crash_message(fmt)
    #ilASM{
        [32]
--EXCEPT
--          mov edx,[ebp+16]    -- return addr
            mov edx,[ebp+28]    -- return addr
            mov ebp,[ebp+20]    -- prev_ebp
            sub edx,1
        [64]
--EXCEPT
--          mov rdx,[rbp+32]    -- return addr
            mov rdx,[rbp+56]    -- return addr
            mov rbp,[rbp+40]    -- prev_ebp
            sub rdx,1
        []
            mov al,0            -- (uses crash_msg anyway)
            jmp :!iDiag
            int3
          }
end procedure

