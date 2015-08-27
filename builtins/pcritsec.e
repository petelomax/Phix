--
-- pcritsec.e
-- ==========
--
--  Critical section support. Required for multithreaded programs.
--  Note that because several builtins need this, it is automatically included...
--
--  usage:
--          integer mycs = InitializeCriticalSection()
--          ...
--          EnterCriticalSection(mycs)
--          ...
--          LeaveCriticalSection(mycs)
--          ...
--          mycs = DeleteCriticalSection(mycs)
--
--  The "shorthand" methods EnterCriticalSection() and LeaveCriticalSection(),
--  ie the use of stdcs, are intended for things like initialisation in the
--  builtins, for an example see init2/inf/nan in pprntf.e: very short one-off
--  costs where it matters not one dot if you "stop the rest of the world". In
--  normal application code you should use a privately owned critical section.
--
-- Technical Note:
--  The result from allocate() is always dword-aligned, ie ends in binary 0b00,
--  and therefore can be stored /4 as an integer without loss of precision.
--  Doing so greatly simplifies subsequent code, specifically testing for atom
--  and extracting addresses>#3FFFFFFF stored as 64-bit floats, is not needed.
--
--DEV to go (use %opEnterCS, as per opMalloc):
-- Performance Note:
--  Enter/LeaveCriticalSection are exceptionally fast. For performance critical
--  code (such as a heap manager) consider replacing calls to these hll routines
--  with the inline ilASM, as that will be four times faster, or about a quarter 
--  of the overhead (if no contention occurs, that is). You are unlikely to see
--  any similar gain from Initialize/DeleteCriticalSection other than, perhaps,
--  in some pointless benchmark.
--

integer stdcs = 0 -- for very short one-off inits in \builtins.

global function InitializeCriticalSection()
integer cs
    cs = floor(allocate(24)/4)  -- (40 on 64-bit [if that ever happens])
    if cs=0 then ?9/0 end if
    #ilASM{ mov eax,[cs]
            shl eax,2
            push eax                                    -- lpCriticalSection
            call "kernel32","InitializeCriticalSection" }
    return cs
end function

global function DeleteCriticalSection(integer cs)
    if cs!=0 then
        #ilASM{ mov eax,[cs]
                shl eax,2
                push eax                                    -- lpCriticalSection
                call "kernel32","DeleteCriticalSection" }
        free(cs*4)
    end if
    return 0
end function

global procedure EnterCriticalSection(integer cs=NULL)
    if cs=NULL then
        -- (Technically this is not thread safe. To avoid any issues, you may need
        --  to make sure, by calling Enter/Leave(NULL), that stdcs gets set before 
        --  the first CreateThread() or whatever.)
        if stdcs=0 then stdcs = InitializeCriticalSection() end if
        cs = stdcs
    end if
    #ilASM{ mov eax,[cs]
            shl eax,2
--          jz %opDivf2 -- jz e02atdb0
            push eax                                    -- lpCriticalSection
            call "kernel32","EnterCriticalSection" }
end procedure

global procedure LeaveCriticalSection(integer cs=NULL)
    if cs=NULL then
        if stdcs=0 then ?9/0 end if
        cs = stdcs
    end if
    #ilASM{ mov eax,[cs]
            shl eax,2
            push eax                                    -- lpCriticalSection
            call "kernel32","LeaveCriticalSection" }
end procedure

--DEV/SUG: (with appropriate changes to psym/pilx86)
--/*
#ilASM{ jmp :fin

      :%opEnterCS
        test eax,eax
        jnz @f
            mov eax,[stdcs]
            test eax,eax
            jnz @f
--          mov eax,24  -- (40 on x64)
--          call :%opMallocX    -- maybe? (erm, definitely once pheap is done) [DEV]
            call "kernel32.dll","GetProcessHeap"
            push dword 24                               -- dwBytes
            push ebx                                    -- dwFlags (0)
            push eax                                    -- hHeap
            call "kernel32.dll","HeapAlloc"
            push eax                                    -- lpCriticalSection (for EnterCriticalSection)
            push eax                                    -- lpCriticalSection (for InitializeCriticalSection)
            shr eax,2
            mov [stdcs],eax
            call "kernel32","InitializeCriticalSection"
            jmp :ecsandret
      @@:
        shl eax,2
        push eax                                        -- lpCriticalSection
      ::ecsandret
        call "kernel32.dll","EnterCriticalSection"
        ret

      :%opLeaveCS
        test eax,eax
        jnz @f
            mov eax,[stdcs]
            test eax,eax
            jz %opDivf2 -- jz e02atdb0
      @@:
        shl eax,2
        push eax                                        -- lpCriticalSection
        call "kernel32.dll","LeaveCriticalSection"
        ret

      ::fin }
--*/

