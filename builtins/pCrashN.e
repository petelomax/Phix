--
-- pCrashN.e
-- =========
--
--  Phix implementation of the crash() routine.
--  crash_message (:%pCrashMsg) is part of the optable/pDiagN.e, this is not.
--

--DEV this should perhaps have an optional parameter to pop more frames.
--without debug
--/*
global procedure crash(string fmt, object args={})  --, integer nFrames=1)
    if atom(args) or length(args) then
        fmt = sprintf(fmt, args)
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
--29/7/17
--          mov al,0            -- (uses crash_msg anyway)
            mov al,68           -- e68crash
            jmp :!iDiag
            int3
          }
end procedure
--*/
--/!*
global procedure crash(string msg, object args={}, integer nFrames=1)
--  if atom(args) or length(args) then
    if args!={} then
        msg = sprintf(msg, args)
    end if
    if nFrames<1 then ?9/0 end if
    crash_message(msg)  -- (yes, that increfs msg correctly, I just checked!)
    #ilASM{
        -- while e/rax do issue fake opRetf (including this routine!)
        [32]
            mov eax,[nFrames]
          @@:
            mov edx,[ebp+28]            -- return addr
            mov dword[ebp+28],:fakeRet  -- replace return address
            push edx
            jmp :%opRetf
          ::fakeRet
            pop edx
            sub eax,1
            jnz @b
            sub edx,1
        [64]
            mov rax,[nFrames]
          @@:
            mov rdx,[rbp+56]            -- return addr
            mov dword[rbp+56],:fakeRet  -- replace return address
            push rdx
            jmp :%opRetf
          ::fakeRet
            pop rdx
            sub rax,1
            jnz @b
            sub rdx,1
        []
            mov al,68           -- e68crash
            jmp :!iDiag
            int3
          }
end procedure
--*!/

