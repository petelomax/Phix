--
-- pFEH.e
--
--  The Phix final exception handler
--
--  This can display some machine-level diagnostics, which can be useful as long as
--  you have a matching list.asm, and is always loaded, unlike pdiagN.e, which can
--  be omitted (via the -nodiag command line option), and generally concerns itself
--  with hll-level diagnostics. Obviously this division of labour is intended to
--  assist with issues in the compiler or more accurately builtins\VM, rather than
--  in a more typical hll application.
--

--without debug

include builtins\VM\puts1.e         -- low-level console i/o
include builtins\VM\pUnassigned.e   -- opCallOnceYeNot etc

--constant edetected = "exception detected, exception_pointers is #"
--constant comma = ",\n"
--constant erecord = "exception record is #"
--constant econtext = "exception context is #"

constant ecode = "exception code #"
constant sigsegv = "SIGSEGV"
constant eat = " at #"
constant eaxis = "eax: ", ebxis = "ebx: ", ecxis = "ecx: ", edxis = "edx: ", esiis = "esi: ", ediis = "edi: "
--          (the above constants are rudely patched to rax..rdi under 64-bit, but later restored, btw.)
--constant setuef = "SetUnhandledExceptionFilter is #"

integer finit = 0

#ilASM{ jmp :!opCallOnceYeNot

  :>initFEH                         -- same as :<exch64 on PE64, and /not/ called at startup in that case
-----------                         -- (I needed "" in the optable, but it only exists on PE64)
    [PE32,ELF32,ELF64]
        cmp [finit],0
        jne :dont_do_everything_twice
        mov [finit],1
    [PE32]
--DEV temp (passed...)
--      mov edi,[setuef]            -- "SetUnhandledExceptionFilter is #"
--      call :%puts1
--      mov esi,:sehcall
--      mov edx,[esi+2]
--      mov edx,[edx]
--      push 1                      -- cr
--      call :%puthex32
--DEV /temp
        mov eax,:finalExceptionHandler
        push eax                                    -- lpTopLevelExceptionFilter
--::sehcall
        call "kernel32.dll","SetUnhandledExceptionFilter"
--/*
    [PE64]
        mov rcx,rsp -- put 2 copies of rsp onto the stack...
        push rsp
        push rcx
        or rsp,8    -- [rsp] is now 1st or 2nd copy:
                    -- if on entry rsp was xxx8: both copies remain on the stack
                    -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                    -- obviously rsp is now xxx8, whatever alignment we started with
        sub rsp,8*5
        mov rcx,:finalExceptionHandler              -- lpTopLevelExceptionFilter
        mov [rsp],rcx
        call "kernel32.dll","SetUnhandledExceptionFilter"
--      add rsp,8*5
--      pop rsp     -- restore, equivalent to rsp += (either #08 or #10)
        mov rsp,[rsp+8*5]   -- equivalent to the add/pop
--*/
    [ELF32]
        push 0
        push 4  -- SA_SIGINFO
        push 0
        push :my_signal_handler
        mov eax, 67 -- SYSCALL_SIGACTION (67==#43)
        mov ebx, 11 -- SIGSEGV 
        mov ecx,esp
        xor edx, edx 
        int 0x80
        add esp,16
        xor ebx,ebx             -- (common requirement after int 0x80)
-- nah, methinks we want sigaction...
--      --; install signal handler 
--      mov eax,48  -- SYSCALL_SIGNAL ( syscall to function signal() )
--      mov ebx,11  -- signal id SIGSEGV 
--      mov ecx,:my_signal_handler 
--      int 0x80 
--      xor ebx,ebx             -- (common requirement after int 0x80)
--#     Name                        Registers                                                                                                               Definition
--                                  eax     ebx                     ecx                     edx                     esi                     edi
--67    sys_sigaction               0x43    int sig                 const struct old_sigaction *act struct old_sigaction *oact  -           -               arch/mips/kernel/signal.c:300

--48        sys_signal                  0x30    int sig                 __sighandler_t handler  -                       -                       -               kernel/signal.c:2683
--67        sys_sigaction               0x43    int sig                 const struct old_sigaction *act struct old_sigaction *oact  -           -               arch/mips/kernel/signal.c:300
--119       sys_sigreturn               0x77    struct pt_regs *regs    -                       -                       -                       -               arch/alpha/kernel/entry.S:758
--see also SYSCALL_SIGNAL below (best bet?)
-- got it!: http://syprog.blogspot.co.uk/2011/10/iterfacing-linux-signals.html (saved in edita14)
    [ELF64]
--%rax  System call             %rdi                    %rsi                            %rdx                    %rcx                    %r8                     %r9
--13        sys_rt_sigaction        int sig                 const struct sigaction *act     struct sigaction *oact  size_t sigsetsize
        -- (untested: may need more space: 8 qw (of 0) for the mask, restore function, and "push 0x04000004 -- SA_SIGINFO or SA_RESTORER", see example64.asm)
        push 4  -- SA_SIGINFO
        push :my_signal_handler
        xor r10,r10     -- (may need to be 8)
        xor rcx,rcx     -- (copy "", in case it /is/ rcx not r10 for system calls)
        xor rdx,rdx
        mov rsi,rsp
        mov rdi,11  -- SIGSEGV
        mov rax,13  -- sys_rt_sigaction
        syscall
        add rsp,16
    [PE32,ELF32,ELF64]
      ::dont_do_everything_twice
        ret


--  [PE]
    [PE32]
      ::finalExceptionHandler
        xor ebx,ebx -- important!!

--      call :lowlevel              -- (temp)

        mov esi,[esp+4]             -- EXCEPTION_POINTERS
        mov edi,[esi]               -- EXCEPTION_RECORD
        mov esi,[esi+4]             -- CONTEXT_RECORD
        mov ecx,[edi]               -- exception_code
        mov edx,[esi+184]           -- or_eip (exception_addr)
        mov ebp,[esi+180]           -- or_ebp (restore)
        mov edi,esp                 -- (in case :!fehDiag not called)
        mov esp,[esi+196]           -- or_esp

--EXCEPT
--      cmp [ebp+16],ebx
--      je @f
--          -- exception handler!=NULL:
--          mov eax,:!iDiag
--          cmp eax,ebx
--          je @f                   -- (crash in the -nodiag case)
--          mov [esi+172],ecx       -- exception code (in or_ecx)
--          mov [esi+168],edx       -- exception address (in or_edx)
--          mov [esi+184],::fhthrow -- replace or_eip
--          mov eax,-1              -- EXCEPTION_CONTINUE_EXECUTION  (0xFFFFFFFF)
--          ret
--    ::fhthrow
--          mov al,30               -- e30ume
--          jmp :!iDiag
--    @@:

        -- special cases:
        cmp edx,:!blockfound
        jne @f
            -- heap corruption: zero pTCB.pFree[idx] to minimise knock-on effects
            push edi
            push esi
            mov edi,[esi+156]   -- or_edi
            mov esi,[esi+160]   -- or_esi
            mov [esi*4+edi+20],ebx  -- pTCB.pFree[idx]:=0 (see pHeap.e)
            pop esi
            pop edi
      @@:

        --  esi is context record (save everything once we get into :!fehDiag)
        --  edx is exception address
        --  ecx is exception code
        --  ebx, ebp, and esp have been reset
        call :!fehDiag              -- pdiagN.e, if loaded
        -- (control does not return if called, unless looping)
--      add esp,4
        mov esp,edi

        call :lowlevel              -- (not temp)

--      call :%opClosem9
--      push dword[errcode]         -- uExitCode
        push 1                      -- uExitCode
        call "kernel32","ExitProcess"

      ::lowlevel
        -- 
        mov edi,[ecode]             -- "exception code #"
        call :%puts1
        mov esi,[esp+8]             -- EXCEPTION_POINTERS
        mov edi,[esi]               -- EXCEPTION_RECORD
        mov edx,[edi]               -- exception_code
        push 0                      -- no cr
        call :%puthex32
        mov edi,[eat]               -- " at #"
        call :%puts1
        mov esi,[esp+8]             -- EXCEPTION_POINTERS
        mov ecx,[esi+4]             -- CONTEXT_RECORD
        mov edx,[ecx+184]           -- or_eip
        push 1                      -- cr
        call :%puthex32

        mov edi,[eaxis]             -- "eax: "
        call :%puts1
        mov esi,[esp+8]             -- EXCEPTION_POINTERS
        mov ecx,[esi+4]             -- CONTEXT_RECORD
        mov edx,[ecx+176]           -- or_eax
        push 1                      -- cr
        call :%puthex32
        mov edi,[ebxis]             -- "ebx: "
        call :%puts1
        mov esi,[esp+8]             -- EXCEPTION_POINTERS
        mov ecx,[esi+4]             -- CONTEXT_RECORD
        mov edx,[ecx+164]           -- or_ebx
        push 1                      -- cr
        call :%puthex32
        mov edi,[ecxis]             -- "ecx: "
        call :%puts1
        mov esi,[esp+8]             -- EXCEPTION_POINTERS
        mov ecx,[esi+4]             -- CONTEXT_RECORD
        mov edx,[ecx+172]           -- or_ecx
        push 1                      -- cr
        call :%puthex32
        mov edi,[edxis]             -- "edx: "
        call :%puts1
        mov esi,[esp+8]             -- EXCEPTION_POINTERS
        mov ecx,[esi+4]             -- CONTEXT_RECORD
        mov edx,[ecx+168]           -- or_edx
        push 1                      -- cr
        call :%puthex32
        mov edi,[esiis]             -- "esi: "
        call :%puts1
        mov esi,[esp+8]             -- EXCEPTION_POINTERS
        mov ecx,[esi+4]             -- CONTEXT_RECORD
        mov edx,[ecx+160]           -- or_esi
        push 1                      -- cr
        call :%puthex32
        mov edi,[ediis]             -- "edi: "
        call :%puts1
        mov esi,[esp+8]             -- EXCEPTION_POINTERS
        mov ecx,[esi+4]             -- CONTEXT_RECORD
        mov edx,[ecx+156]           -- or_edi
        push 1                      -- cr
        call :%puthex32

        ret

    [PE64]
  :<exch64
----------
--DEV you should be able to debug this with fdbg no problem... (trap and use Ctrl F12 to pass exception to handler)
--  qword[r8+248] == context.Rip
--typedef EXCEPTION_DISPOSITION
--(*PEXCEPTION_ROUTINE)(
--  IN PEXCEPTION_RECORD ExceptionRecord,           -- rcx
--  IN ULONG64 EstablisherFrame,                    -- rdx
--  IN OUT PCONTEXT ContextRecord,                  -- r8
--  IN OUT PDISPATCHER_CONTEXT DispatcherContext);  -- r9

        xor rbx,rbx -- important!!

        mov [rsp+8],rcx             -- copy actual param into shadow space!
        mov [rsp+24],r8             -- (neither of these are temp!)

        call :lowlevel              -- (temp) [DEV]
        mov rcx,[rsp+8]             --   ""
        mov r8,[rsp+24]             --   ""

--/*
--      mov rsi,[rsp+8]             -- EXCEPTION_POINTERS
--      mov rdi,[rsi]               -- EXCEPTION_RECORD
        mov rdi,[rcx]               -- EXCEPTION_RECORD
--      mov rsi,[rsi+8]             -- CONTEXT_RECORD
        mov rsi,[rcx+8]             -- CONTEXT_RECORD
        mov rdx,[rsi+248]           -- DWORD64 Rip (exception_addr)
--  add qword[r8+248],2
        mov ecx,dword[rdi]          -- exception_code (DWORD)
        mov rbp,[rsi+160]           -- DWORD64 Rbp (restore)
        mov rdi,rsp                 -- (in case :!fehDiag not called)
        mov rsp,[rsi+152]           -- DWORD64 Rsp (restore)

        -- special cases:
--      cmp rdx,:!blockfound
        cmp edx,:!blockfound
        jne @f
            -- heap corruption: zero pTCB.pFree[idx] to minimise knock-on effects
            push rdi
            push rsi
            mov rdi,[rsi+176]       -- DWORD64 Rdi
            mov rsi,[rsi+168]       -- DWORD64 Rsi
            mov [rsi*4+rdi+32],rbx  -- pTCB.pFree[idx]:=0 (see pHeap.e)
            pop rsi
            pop rdi
      @@:
--*/
        mov rdx,[r8+248]            -- DWORD64 context.Rip
        mov ecx,[rcx]               -- exception_code (DWORD)
        mov rbp,[r8+160]            -- DWORD64 context.Rbp (restore)
        mov rdi,rsp                 -- (in case :!fehDiag not called)
        mov rsp,[r8+152]            -- DWORD64 context.Rsp (restore)
        mov rsi,r8

--EXCEPT
--      cmp [rbp+32],rbx
--      je @f
--          -- exception handler!=NULL:
--          mov rax,:!iDiag
--          cmp rax,rbx
--          je @f                   -- (crash in the -nodiag case)
--          mov [r8+128],rcx        -- exception code (in context.Rcx)
--          mov [r8+136],rdx        -- exception address (in context.Rdx)
--          mov [r8+248],::fhthrow  -- context.Rip
--          mov eax,-1              -- EXCEPTION_CONTINUE_EXECUTION  (0xFFFFFFFF)
--          ret
--    ::fhthrow
--          mov al,30               -- e30ume
--          jmp :!iDiag
--    @@:

        --  rsi is context record (save everything once we get into :!fehDiag)
        --  rdx is exception address
        --  ecx is exception code (DWORD)
        --  rbx, rbp, and rsp have been reset
        call :!fehDiag              -- pdiagN.e, if loaded
        -- (control does not return if called, unless looping)
--      add rsp,8
        mov rsp,rdi

        call :lowlevel              -- (not temp, that copy rcx is rqd)

--      call :!opClosem9
--      add rsp,8
--      push dword[errcode]         -- uExitCode

        sub rsp,8*5
        mov rcx,1                   -- uExitCode
        call "kernel32","ExitProcess"

      ::lowlevel

        mov rdi,[ecode]             -- "exception code #"
        call :%puts1
        mov rcx,[rsp+16]
        mov edx,[rcx]               -- exception_code (DWORD)
--      mov rdx,[rcx]               -- exception_code (DWORD)
        push 0                      -- no cr
        call :%puthex32
        mov rdi,[eat]               -- " at #"
        call :%puts1
        mov rcx,[rsp+16]
        mov rdx,[rcx+16]            -- exception address
        push 1                      -- cr
        call :%puthex64
--!/*
--typedef struct _EXCEPTION_RECORD {
--  DWORD                  ExceptionCode;
--  DWORD                  ExceptionFlags;
--  struct _EXCEPTION_RECORD    *ExceptionRecord;
--  PVOID                  ExceptionAddress;

--      mov rsi,[rsp+16]            -- EXCEPTION_POINTERS
--      mov rdi,[rsi]               -- EXCEPTION_RECORD
--      mov edx,[rdi]               -- exception_code (DWORD)
--      push 0                      -- no cr
--      call :%puthex32
--      mov rdi,[eat]               -- " at #"
--      call :%puts1
--      mov rsi,[rsp+16]            -- EXCEPTION_POINTERS
--      mov rcx,[rsi+8]             -- CONTEXT_RECORD
--      mov rdx,[rcx+248]           -- DWORD64 Rip

-- good, matches:
--      mov r8,[rsp+32]
--      mov rdx,[r8+248]            -- DWORD64 context.Rip
--      push 1
--      call :%puthex64

        mov rdi,[eaxis]             -- "rax: "
        call :puts1r
--      mov rsi,[rsp+16]            -- EXCEPTION_POINTERS
--      mov rcx,[rsi+8]             -- CONTEXT_RECORD
--      mov rdx,[rcx+120]           -- DWORD64 Rax
        mov r8,[rsp+32]
        mov rdx,[r8+120]            -- DWORD64 context.Rax
        push 1                      -- cr
        call :%puthex64
        mov rdi,[ebxis]             -- "rbx: "
        call :puts1r
--      mov rsi,[rsp+16]            -- EXCEPTION_POINTERS
--      mov rcx,[rsi+8]             -- CONTEXT_RECORD
--      mov rdx,[rcx+144]           -- DWORD64 Rbx
        mov r8,[rsp+32]
        mov rdx,[r8+144]            -- DWORD64 context.Rbx
        push 1                      -- cr
        call :%puthex64
        mov rdi,[ecxis]             -- "rcx: "
        call :puts1r
--      mov rsi,[rsp+16]            -- EXCEPTION_POINTERS
--      mov rcx,[rsi+8]             -- CONTEXT_RECORD
--      mov rdx,[rcx+128]           -- DWORD64 Rcx
        mov r8,[rsp+32]
        mov rdx,[r8+128]            -- DWORD64 context.Rcx
        push 1                      -- cr
        call :%puthex64
        mov rdi,[edxis]             -- "rdx: "
        call :puts1r
--      mov rsi,[rsp+16]            -- EXCEPTION_POINTERS
--      mov rcx,[rsi+8]             -- CONTEXT_RECORD
--      mov rdx,[rcx+136]           -- DWORD64 Rdx
        mov r8,[rsp+32]
        mov rdx,[r8+136]            -- DWORD64 context.Rdx
        push 1                      -- cr
        call :%puthex64
        mov rdi,[esiis]             -- "rsi: "
        call :puts1r
--      mov rsi,[rsp+16]            -- EXCEPTION_POINTERS
--      mov rcx,[rsi+8]             -- CONTEXT_RECORD
--      mov rdx,[rcx+168]           -- DWORD64 Rsi
        mov r8,[rsp+32]
        mov rdx,[r8+168]            -- DWORD64 context.Rsi
        push 1                      -- cr
        call :%puthex64
        mov rdi,[ediis]             -- "rdi: "
        call :puts1r
--      mov rsi,[rsp+16]            -- EXCEPTION_POINTERS
--      mov rcx,[rsi+8]             -- CONTEXT_RECORD
--      mov rdx,[rcx+176]           -- DWORD64 Rdi
        mov r8,[rsp+32]
        mov rdx,[r8+176]            -- DWORD64 context.Rdi
        push 1                      -- cr
        call :%puthex64
        ret

      ::puts1r
        push rdi
        mov byte[rbx+rdi*4],'r'     -- eax->rax etc
        call :%puts1
        pop rdi
        mov byte[rbx+rdi*4],'e'     -- restore (in case values are shared)
--!*/
        ret

    [ELF32]
      ::my_signal_handler
        xor ebx,ebx -- important!!

--DEV removed for now (18/4/16):
--!/*
        call :lowlevel              -- (temp)

        mov esi,[esp+12]            -- 3rd param (siginfo_t)
        mov edx,[esi+76]            -- eip
        mov ebp,[esi+44]
        mov edi,esp                 -- (in case :!fehDiag not called)
        mov esp,[esi+48]
--      mov esi,[esp+4]             -- EXCEPTION_POINTERS
--      mov edi,[esi]               -- EXCEPTION_RECORD
--      mov esi,[esi+4]             -- CONTEXT_RECORD
--      mov ecx,[edi]               -- exception_code
--      mov edx,[esi+184]           -- or_eip (exception_addr)
--      mov ebp,[esi+180]           -- or_ebp (restore)
--      mov edi,esp                 -- (in case :!fehDiag not called)
--      mov esp,[esi+196]           -- or_esp
--

--EXCEPT
--      cmp [ebp+16],ebx
--      je @f
--          -- exception handler!=NULL:
--          mov eax,:!iDiag
--          cmp eax,ebx
--          je @f                   -- (crash in the -nodiag case)
--          mov dword[esi+60],11    -- exception code, always SIGSEGV (in ecx)
--          mov [esi+56],edx        -- exception address (in edx)
--          mov [esi+76],::fhthrow
--          ret
--    ::fhthrow
--          mov al,30               -- e30ume
--          jmp :!iDiag
--    @@:

--      -- special cases:
--      cmp edx,:!blockfound
--      jne @f
--          -- heap corruption: zero pTCB.pFree[idx] to minimise knock-on effects
--          push edi
--          push esi
--          mov edi,[esi+156]   -- or_edi
--          mov esi,[esi+160]   -- or_esi
--          mov [esi*4+edi+20],ebx  -- pTCB.pFree[idx]:=0 (see pHeap.e)
--          pop esi
--          pop edi
--    @@:
--
        --  esi is context record (save everything once we get into :!fehDiag)
        --  edx is exception address
--      --  ecx is exception code (would always be SIGSEGV)
--      --  ebx, ebp, and esp have been reset
        call :!fehDiag              -- pdiagN.e, if loaded
--      -- (control does not return if called, unless looping)
----        add esp,4
        mov esp,edi
--
--!*/
        call :lowlevel              -- (not temp)

--      call :%opClosem9

        xor     ebx, ebx 
        mov     eax, 1  -- SYSCALL_EXIT 
        int     0x80 

      ::lowlevel

        mov edi,[sigsegv]           -- "SIGSEGV"
        call :%puts1
        mov edi,[eat]               -- " at #"
        call :%puts1
        mov eax,[esp+16]            -- 3rd param
        mov edx,[eax+76]            -- eip
        push 1                      -- cr
        call :%puthex32

        mov edi,[eaxis]             -- "eax: "
        call :%puts1
        mov eax,[esp+16]            -- 3rd param
        mov edx,[eax+64]            -- eax
        push 1                      -- cr
        call :%puthex32
        mov edi,[ebxis]             -- "ebx: "
        call :%puts1
        mov eax,[esp+16]            -- 3rd param
        mov edx,[eax+52]            -- ebx
        push 1                      -- cr
        call :%puthex32
        mov edi,[ecxis]             -- "ecx: "
        call :%puts1
        mov eax,[esp+16]            -- 3rd param
        mov edx,[eax+60]            -- ecx
        push 1                      -- cr
        call :%puthex32
        mov edi,[edxis]             -- "edx: "
        call :%puts1
        mov eax,[esp+16]            -- 3rd param
        mov edx,[eax+56]            -- edx
        push 1                      -- cr
        call :%puthex32
        mov edi,[esiis]             -- "esi: "
        call :%puts1
        mov eax,[esp+16]            -- 3rd param
        mov edx,[eax+40]            -- esi
        push 1                      -- cr
        call :%puthex32
        mov edi,[ediis]             -- "edi: "
        call :%puts1
        mov eax,[esp+16]            -- 3rd param
        mov edx,[eax+36]            -- edi
        push 1                      -- cr
        call :%puthex32

        ret

--      .edi           rd 1     ;36
--      .esi           rd 1     ;40
--      .ebp           rd 1     ;44
--      .esp           rd 1     ;48
--      .ebx           rd 1     ;52
--      .edx           rd 1     ;56
--      .ecx           rd 1     ;60
--      .eax           rd 1     ;64
--      .trapno        rd 1     ;68
--      .err           rd 1     ;72
--      .eip           rd 1     ;76 (correct)
--      .cs            rw 1     ;80
--      .__csh         rw 1     ;82
--      .eflags        rd 1     ;84
--      .esp_at_signal rd 1     ;88

    [ELF64]
      ::my_signal_handler

        xor rbx,rbx -- important!!

        mov [rsp+16],rdx            -- copy actual param into shadow space!

        call :lowlevel              -- (temp)

        mov rsi,[rsp+24]            -- 3rd param
        mov rdx,[rsi+0xA8]          -- rip
        mov rbp,[rsi+0x78]
        mov rdi,rsp                 -- (in case :!fehDiag not called)
        mov rsp,[rsi+0xA0]
--      mov esi,[esp+4]             -- EXCEPTION_POINTERS
--      mov edi,[esi]               -- EXCEPTION_RECORD
--      mov esi,[esi+4]             -- CONTEXT_RECORD
--      mov ecx,[edi]               -- exception_code
--      mov edx,[esi+184]           -- or_eip (exception_addr)
--      mov ebp,[esi+180]           -- or_ebp (restore)
--      mov edi,esp                 -- (in case :!fehDiag not called)
--      mov esp,[esi+196]           -- or_esp
--

--EXCEPT
--      cmp [rbp+32],rbx
--      je @f
--          -- exception handler!=NULL:
--          mov rax,:!iDiag
--          cmp rax,rbx
--          je @f                       -- (crash in the -nodiag case)
--          mov qword[rsi+0x98],11      -- exception code, always SIGSEGV (in rcx)
--          mov [rsi+0x88],rdx          -- exception address (in rdx)
--          mov [rsi+0xA8],::fhthrow    -- rip
--          ret
--    ::fhthrow
--          mov al,30               -- e30ume
--          jmp :!iDiag
--    @@:

--  if [Rbp+32]!=0 then             -- if exception handler!=NULL then
--      ret
--    ::fhthrow (or maybe :!fehDiag or similar)
--  end if

--      -- special cases:
--      cmp edx,:!blockfound
--      jne @f
--          -- heap corruption: zero pTCB.pFree[idx] to minimise knock-on effects
--          push edi
--          push esi
--          mov edi,[esi+156]   -- or_edi
--          mov esi,[esi+160]   -- or_esi
--          mov [esi*4+edi+20],ebx  -- pTCB.pFree[idx]:=0 (see pHeap.e)
--          pop esi
--          pop edi
--    @@:
--
        --  rsi is context record (save everything once we get into :!fehDiag)
        --  rdx is exception address
--      --  ecx is exception code
        --  rbx, rbp, and rsp have been reset
        call :!fehDiag              -- pdiagN.e, if loaded
--      -- (control does not return if called, unless looping)
----        add esp,4
        mov rsp,rdi
--
        call :lowlevel              -- (not temp)

--      call :%opClosem9

--      mov rax,60  -- sys_exit
        mov rax,231 -- sys_exit_group(rdi=int error_code) 
        xor rdi,rdi
        syscall

      ::lowlevel

        mov rdi,[sigsegv]           -- "SIGSEGV"
        call :%puts1
        mov rdi,[eat]               -- " at #"
        call :%puts1
        mov rdx,[rsp+32]            -- 3rd param
        mov rdx,[rdx+0xA8]          -- rip
        push 1                      -- cr
        call :%puthex64

        mov rdi,[eaxis]             -- "eax: "
        call :puts1r
        mov rdx,[rsp+32]            -- 3rd param
        mov rdx,[rdx+0x90]          -- rax
        push 1                      -- cr
        call :%puthex64
        mov rdi,[ebxis]             -- "ebx: "
        call :puts1r
        mov rdx,[rsp+32]            -- 3rd param
        mov rdx,[rdx+0x80]          -- rbx
        push 1                      -- cr
        call :%puthex64
        mov rdi,[ecxis]             -- "ecx: "
        call :puts1r
        mov rdx,[rsp+32]            -- 3rd param
        mov rdx,[rdx+0x98]          -- rcx
        push 1                      -- cr
        call :%puthex64
        mov rdi,[edxis]             -- "edx: "
        call :puts1r
        mov rdx,[rsp+32]            -- 3rd param
        mov rdx,[rdx+0x88]          -- rdx
        push 1                      -- cr
        call :%puthex64
        mov rdi,[esiis]             -- "esi: "
        call :puts1r
        mov rdx,[rsp+32]            -- 3rd param
        mov rdx,[rdx+0x70]          -- rsi
        push 1                      -- cr
        call :%puthex64
        mov edi,[ediis]             -- "edi: "
        call :puts1r
        mov rdx,[rsp+32]            -- 3rd param
        mov rdx,[rdx+0x68]          -- rdi
        push 1                      -- cr
        call :%puthex64

        ret

--  .r8             dq  ?   ;0x28
--  .r9             dq  ?   ;0x30
--  .r10            dq  ?   ;0x38
--  .r11            dq  ?   ;0x40
--  .r12            dq  ?   ;0x48
--  .r13            dq  ?   ;0x50
--  .r14            dq  ?   ;0x58
--  .r15            dq  ?   ;0x60
--  .rdi            dq  ?   ;0x68
--  .rsi            dq  ?   ;0x70
--  .rbp            dq  ?   ;0x78
--  .rbx            dq  ?   ;0x80
--  .rdx            dq  ?   ;0x88
--  .rax            dq  ?   ;0x90
--  .rcx            dq  ?   ;0x98
--  .rsp            dq  ?   ;0xA0
--  .rip            dq  ?   ;0xA8
--  .eflags         dq  ?   ;0xB0
--  .cs             dw  ?   ;0xB8
--  .gs             dw  ?   ;0xBA
--  .fs             dw  ?   ;0xBC
--  .__pad0         dw  ?   ;0xBE
--  .err            dq  ?   ;0xC0
--  .trapno         dq  ?   ;0xC8
--  .oldmask        dq  ?   ;0xD0
--  .cr2            dq  ?   ;0xD8
--  .fpstate        dq  ?   ;0xE0
--  .reserved       rq  8   ;0xE8

      ::puts1r
        push rdi
        mov byte[rbx+rdi*4],'r'     -- eax->rax etc
        call :%puts1
        pop rdi
        mov byte[rbx+rdi*4],'e'     -- restore (in case values are shared)
        ret
    []

--::fin
      }

--/*
virtual at edx  ; exception_pointers
    exception_record dd ?
    context_record   dd ?
end virtual

typedef struct _EXCEPTION_RECORD {
  DWORD                    ExceptionCode;
  DWORD                    ExceptionFlags;
  struct _EXCEPTION_RECORD  *ExceptionRecord;
  PVOID                    ExceptionAddress;
  DWORD                    NumberParameters;
  ULONG_PTR                ExceptionInformation[EXCEPTION_MAXIMUM_PARAMETERS];
} EXCEPTION_RECORD, *PEXCEPTION_RECORD;
           
virtual at eax  ; exception_record
    exception_code  dd ?
    exception_flag  dd ?
    nested          dd ?
    exception_addr  dd ?
    noof_params     dd ?
    addtnl_data     dd ?
end virtual

/* Exception record (32-bit version) */
typedef struct _EXCEPTION_RECORD32 {
    DWORD   ExceptionCode;
    DWORD   ExceptionFlags;
    DWORD   ExceptionRecord;
    DWORD   ExceptionAddress;
    DWORD   NumberParameters;
    DWORD   ExceptionInformation[EXCEPTION_MAXIMUM_PARAMETERS];
} EXCEPTION_RECORD32;
typedef EXCEPTION_RECORD32  *PEXCEPTION_RECORD32;

/* Exception record (64-bit version) */
typedef struct _EXCEPTION_RECORD64 {
    DWORD   ExceptionCode;
    DWORD   ExceptionFlags;
    DWORD64 ExceptionRecord;
    DWORD64 ExceptionAddress;
    DWORD   NumberParameters;
    DWORD   __unusedAlignment;
    DWORD64 ExceptionInformation[EXCEPTION_MAXIMUM_PARAMETERS];
} EXCEPTION_RECORD64;
typedef EXCEPTION_RECORD64  *PEXCEPTION_RECORD64;

/* Pointers to exception and context records */
typedef struct _EXCEPTION_POINTERS {
    PEXCEPTION_RECORD   ExceptionRecord;
    PCONTEXT            ContextRecord;
} EXCEPTION_POINTERS;
typedef EXCEPTION_POINTERS  *PEXCEPTION_POINTERS;


typedef struct _CONTEXT {
0       DWORD   ContextFlags;
4       DWORD   Dr0;
8       DWORD   Dr1;
12      DWORD   Dr2;
16      DWORD   Dr3;
20      DWORD   Dr6;
24      DWORD   Dr7;
+112    FLOATING_SAVE_AREA FloatSave;
136     DWORD   SegGs;
        DWORD   SegFs;
        DWORD   SegEs;
        DWORD   SegDs;
        DWORD   Edi;
        DWORD   Esi;
        DWORD   Ebx;
        DWORD   Edx;
        DWORD   Ecx;
        DWORD   Eax;
        DWORD   Ebp;
        DWORD   Eip;
        DWORD   SegCs;
        DWORD   EFlags;
        DWORD   Esp;
        DWORD   SegSs;
        BYTE    ExtendedRegisters[MAXIMUM_SUPPORTED_EXTENSION];
} CONTEXT;

virtual at eax  ; context_record
0   context_flags   dd ?
4   dr_0            dd ?    ; debug register 0
8   dr_1            dd ?
12  dr_2            dd ?
16  dr_3            dd ?
20  dr_6            dd ?
24  dr_7            dd ?
28  fp_cw           dd ?    ; control word
32  fp_sw           dd ?    ; status word
36  fp_tw           dd ?    ; tag word
40  fp_eo           dd ?    ; error offset
44  fp_es           dd ?    ; error selector
48  fp_do           dd ?    ; data offset
52  fp_ds           dd ?    ; data selector
56  fp_st0          dt ?    ; floating point register 0
66  fp_st1          dt ?
76  fp_st2          dt ?
86  fp_st3          dt ?
96  fp_st4          dt ?
106 fp_st5          dt ?
116 fp_st6          dt ?
126 fp_st7          dt ?
136 fp_cron         dd ?    ; ? Cr0NpxState ?
140 sr_gs           dd ?    ; gs register
144 sr_fs           dd ?    ; gs register
148 sr_es           dd ?    ; gs register
152 sr_ds           dd ?    ; gs register
156 or_edi          dd ?    ; edi
160 or_esi          dd ?    ; esi
164 or_ebx          dd ?
168 or_edx          dd ?
172 or_ecx          dd ?
176 or_eax          dd ?
180 or_ebp          dd ?
184 or_eip          dd ?        #B8 = 184 (tick)
188 or_cs           dd ?
192 or_eflags       dd ?
196 or_esp          dd ?
200 or_ss           dd ?
end virtual

typedef struct DECLSPEC_ALIGN(16) _CONTEXT {

    //
    // Register parameter home addresses.
    //
    // N.B. These fields are for convience - they could be used to extend the
    //      context record in the future.
    //

0   DWORD64 P1Home;
8   DWORD64 P2Home;
16  DWORD64 P3Home;
24  DWORD64 P4Home;
32  DWORD64 P5Home;
40  DWORD64 P6Home;

    //
    // Control flags.
    //

48  DWORD ContextFlags;
52  DWORD MxCsr;

    //
    // Segment Registers and processor flags.
    //

56  WORD   SegCs;
58  WORD   SegDs;
60  WORD   SegEs;
62  WORD   SegFs;
64  WORD   SegGs;
66  WORD   SegSs;
68  DWORD EFlags;

    //
    // Debug registers
    //

72  DWORD64 Dr0;
80  DWORD64 Dr1;
88  DWORD64 Dr2;
96  DWORD64 Dr3;
104 DWORD64 Dr6;
112 DWORD64 Dr7;

    //
    // Integer registers.
    //

120 DWORD64 Rax;
128 DWORD64 Rcx;
136 DWORD64 Rdx;
144 DWORD64 Rbx;
152 DWORD64 Rsp;
160 DWORD64 Rbp;
168 DWORD64 Rsi;
176 DWORD64 Rdi;
184 DWORD64 R8;
192 DWORD64 R9;
200 DWORD64 R10;
208 DWORD64 R11;
216 DWORD64 R12;
224 DWORD64 R13;
232 DWORD64 R14;
240 DWORD64 R15;

    //
    // Program counter.
    //

248 DWORD64 Rip;

    //
    // Floating point state.
    //

    union {
        XMM_SAVE_AREA32 FltSave;
        struct {
            M128A Header[2];
            M128A Legacy[8];
            M128A Xmm0;
            M128A Xmm1;
            M128A Xmm2;
            M128A Xmm3;
            M128A Xmm4;
            M128A Xmm5;
            M128A Xmm6;
            M128A Xmm7;
            M128A Xmm8;
            M128A Xmm9;
            M128A Xmm10;
            M128A Xmm11;
            M128A Xmm12;
            M128A Xmm13;
            M128A Xmm14;
            M128A Xmm15;
        } DUMMYSTRUCTNAME;
    } DUMMYUNIONNAME;

    //
    // Vector registers.
    //

    M128A VectorRegister[26];
    DWORD64 VectorControl;

    //
    // Special debug control registers.
    //

    DWORD64 DebugControl;
    DWORD64 LastBranchToRip;
    DWORD64 LastBranchFromRip;
    DWORD64 LastExceptionToRip;
    DWORD64 LastExceptionFromRip;
} CONTEXT, *PCONTEXT;


STATUS_BREAKPOINT               = 80000003h
STATUS_SINGLE_STEP              = 80000004h
EXCEPTION_BREAKPOINT            = STATUS_BREAKPOINT
EXCEPTION_SINGLE_STEP           = STATUS_SINGLE_STEP

EXCEPTION_ACCESS_VIOLATION              = C0000005h         -- <-- we get this!
EXCEPTION_DATATYPE_MISALIGNMENT         = 80000002h
EXCEPTION_BREAKPOINT                    = 80000003h         -- <-- ie an int3
EXCEPTION_SINGLE_STEP                   = 80000004h
EXCEPTION_ARRAY_BOUNDS_EXCEEDED         = C000008Ch
EXCEPTION_FLT_DENORMAL_OPERAND          = C000008Dh
EXCEPTION_FLT_DIVIDE_BY_ZERO            = C000008Eh
EXCEPTION_FLT_INEXACT_RESULT            = C000008Fh
EXCEPTION_FLT_INVALID_OPERATION         = C0000090h
EXCEPTION_FLT_OVERFLOW                  = C0000091h
EXCEPTION_FLT_STACK_CHECK               = C0000092h
EXCEPTION_FLT_UNDERFLOW                 = C0000093h
EXCEPTION_INT_DIVIDE_BY_ZERO            = C0000094h
EXCEPTION_INT_OVERFLOW                  = C0000095h
EXCEPTION_PRIV_INSTRUCTION              = C0000096h
EXCEPTION_IN_PAGE_ERROR                 = C0000006h
EXCEPTION_ILLEGAL_INSTRUCTION           = C000001Dh
EXCEPTION_NONCONTINUABLE_EXCEPTION      = C0000025h
EXCEPTION_STACK_OVERFLOW                = C00000FDh
EXCEPTION_INVALID_DISPOSITION           = C0000026h
EXCEPTION_GUARD_PAGE                    = 80000001h
EXCEPTION_INVALID_HANDLE                = C0000008h
EXCEPTION_POSSIBLE_DEADLOCK             = C0000194h
CONTROL_C_EXIT                          = C000013Ah
EXCEPTION_BCC_FATAL                     = 0EEFFACEh
EXCEPTION_BCC_NORMAL                    = 0EEFFAE6h
DBG_CONTROL_C                           = 0EEFFAE6h
--*/

--/*
; example for exception handling 
format ELF executable 
entry start 

SYSCALL_EXIT   equ 1  ; syscall to function exit() 
SYSCALL_WRITE  equ 4  ; syscall to function write() 
SYSCALL_SIGNAL equ 48 ; syscall to function signal() 
SIGSEGV        equ 11 ; signal id SIGSEGV 
STDERR         equ 2  ; standard error 

section readable writeable 
signal_handler_msg db 'Hmmm, SIGSEGV signal received', 0xa 
signal_handler_msg_size = $-signal_handler_msg 
reborn_msg db "but I'm still alive", 0xa 
reborn_msg_size = $-reborn_msg 

section readable executable 
start: 
; install signal handler 
        mov     eax, SYSCALL_SIGNAL 
        mov     ebx, SIGSEGV 
        mov     ecx, my_signal_handler 
        int     0x80 
; do something dirty 
        push    dword [fs:0] 
; print reborn message 
.print_reborn_msg: 
        mov     eax, SYSCALL_WRITE 
        mov     ebx, STDERR 
        mov     ecx, reborn_msg 
        mov     edx, reborn_msg_size 
        int     0x80 
.finish: 
        xor     ebx, ebx 
        mov     eax, SYSCALL_EXIT 
        int     0x80 

my_signal_handler: 
; print exception message 
        mov     eax, SYSCALL_WRITE 
        mov     ebx, STDERR 
        mov     ecx, signal_handler_msg 
        mov     edx, signal_handler_msg_size 
        int     0x80 
; remove return adress from the stack 
        add     esp, 4 
; jump after the erroneous code 
        jmp     start.print_reborn_msg
--*/

--/*
; example for exception handlig 
format ELF executable 
entry start 

SYSCALL_EXIT      equ 1  ; syscall to function exit() 
SYSCALL_WRITE     equ 4  ; syscall to function write() 
SYSCALL_SIGACTION equ 67 ; syscall to function sigaction() 
SA_NOCLDSTOP      equ 0x00000001 
SA_NOCLDWAIT      equ 0x00000002 ; not supported yet 
SA_SIGINFO        equ 0x00000004 ; use sa_sigaction instead of sa_handler 
SA_ONSTACK        equ 0x08000000 
SA_RESTART        equ 0x10000000 
SA_NODEFER        equ 0x40000000 
SA_RESETHAND      equ 0x80000000 
SIGSEGV           equ 11 ; signal id SIGSEGV 
STDERR            equ 2  ; standard error 

; sigaction structure for installing signal handler 
struc sigaction sa, m, f { 
        .sa_sigaction dd sa ; void (*sa_sigaction)(int, siginfo_t *, void *); 
        .sa_mask      dd m  ; sigset_t sa_mask; 
        .sa_flags     dd f  ; int sa_flags; 
        .sa_restorer  dd 0  ; void (*sa_restorer)(void); -- obsoleted, don't use it 
} 

; sigcontext structure used in ucontext below 
; it contains eip we're going to overwrite when catching signal 
struc sigcontext { 
        .gs            rw 1 
        .__gsh         rw 1 
        .fs            rw 1 
        .__fsh         rw 1 
        .es            rw 1 
        .__esh         rw 1 
        .ds            rw 1 
        .__dsh         rw 1 
        .edi           rd 1 
        .esi           rd 1 
        .ebp           rd 1 
        .esp           rd 1 
        .ebx           rd 1 
        .edx           rd 1 
        .ecx           rd 1 
        .eax           rd 1 
        .trapno        rd 1 
        .err           rd 1 
        .eip           rd 1 
        .cs            rw 1 
        .__csh         rw 1 
        .eflags        rd 1 
        .esp_at_signal rd 1 
        .ss            rw 1 
        .__ssh         rw 1 
        .fpstate       rd 1 
        .oldmask       rd 1 
        .cr2           rd 1 
} 

virtual at 0 
        sigcontext sigcontext 
end virtual 

; sigset structure used in ucontext below 
_NSIG             equ 64 
_NSIG_BPW         equ 32 
_NSIG_WORDS       equ (_NSIG / _NSIG_BPW) 

struc sigset_t { 
        .sig rd _NSIG_WORDS 
} 

; signalstack structure used in ucontext below 
struc signaltstack { 
        ss_sp    rd 1 
        ss_flags rd 1 
        ss_size  rd 1 
} 

; structure type for 3rd parameter of signal handler 
struc ucontext { 
        .uc_flags    rd 1 
        .uc_link     rd 1 
        .uc_stack    signaltstack 
        .uc_mcontext sigcontext 
        .uc_sigmask  sigset_t 
} 

virtual at 0 
        ucontext ucontext 
end virtual 

; some dangereous statements 
macro do_dirty { 
-       push    dword [ds:0] 
-       mov     dword [ds:0], eax 
-       push    dword [fs:0] 
} 

; determine size of those statements at compilation time 
virtual at 0 
        do_dirty 
        sizeof.do_dirty=$ 
end virtual 


; define section for code and constants 
section readable executable 

; new_act is constant that's why it is here 
new_act sigaction my_signal_handler, 0,  SA_SIGINFO 

start: 
; install signal handler 
        mov     eax, SYSCALL_SIGACTION 
        mov     ebx, SIGSEGV 
        mov     ecx, new_act 
        xor     edx, edx 
        int     0x80 
; check result of installing signal handler 
        test    eax, eax 
        mov     ebx, eax ; set exit value 
        jnz     .finish 
; do something dirty 
        do_dirty 
; print reborn message 
        mov     eax, SYSCALL_WRITE 
        mov     ebx, STDERR 
        mov     ecx, reborn_msg 
        mov     edx, reborn_msg_size 
        int     0x80 
        xor     ebx, ebx 
.finish: 
        mov     eax, SYSCALL_EXIT 
        int     0x80 

my_signal_handler: 
; print exception message 
        mov     eax, SYSCALL_WRITE 
        mov     ebx, STDERR 
        mov     ecx, signal_handler_msg 
        mov     edx, signal_handler_msg_size 
        int     0x80 
; get parameter ucontext 
        mov     eax, [esp+12] 
; modify eip 
        add     dword [eax+ucontext.uc_mcontext+sigcontext.eip], sizeof.do_dirty 
        ret 

; define section for data 
section readable writeable 
signal_handler_msg db 'Hmmm, SIGSEGV signal received', 0xa 
signal_handler_msg_size = $-signal_handler_msg 
reborn_msg db "but I'm still alive", 0xa 
reborn_msg_size = $-reborn_msg
--*/

--/*
Skeleton for multiprocess handling for 64-bits (easy portable to 32-bit world) - may help for trying in multithread handling

Code:
start: 

        mov     eax,sys_fork 
        syscall                         ; make second copy of program 
; child has return value=0, parent return value = child's PID 
; see: man fork 
        or      rax,rax 
        js      exit1                   ; something went wrong... 
        jnz     parent_proc 

child_proc: 
; see: 
; man ptrace 
; look for PTRACE_TRACEME 
; Any signal delivered to this process will cause it to stop and its parent to 
; be notified via wait. 
        xor     r10,r10 
        xor     edx,edx 
        xor     esi,esi 
        mov     edi,PTRACE_TRACEME 
        mov     eax,sys_ptrace 
        syscall 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; put the core of your program here ; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

parent_proc: 
        mov     [child_PID],rax 

parent_signal_wait: 
        xor     r10,r10 
        mov     edx,WUNTRACED 
        lea     rsi,[child_status] 
;       or      rdi,-1 
; use -1 for wait to child's children too, but we want to wait for child only 
        mov     rdi,qword [child_PID] 
        mov     eax,sys_wait4 
        syscall 
        or      rax,rax 
        jns     wait_L1 
msg_err_wait_exit: 
        lea     rax,[msg_err_wait] 
        jmp     exit_msg 

wait_L1: 
        mov     eax,dword [child_status] 
WIFSTOPPED              =       7Fh 
        test    al,WIFSTOPPED 
        jnz     wait_L2 
; Child exited ? Then we do the same... 
; WEXITSTATUS eax 
        and     eax,0000FF00h 
        sar     eax,8 

; al=exitstatus 
; display the exit status 
;... 
        jmp     exit0 

wait_L2: 
; WIFSIGNALED eax 
        and     eax,7Fh 
        inc     eax 
        sar     al,1 

        jle     wait_L3 

; display hexa content of AL register: 
; ... 

        jmp     exit0 

wait_L3: 
        mov     eax,dword [child_status] 

        cmp     al,WIFSTOPPED 
        jnz     wait_L7 

;       WSTOPSIG        eax 
        and     eax,0000FF00h 
        sar     eax,8 
; al holds signal now 

; 0. display message with the signal number 
; 1. then handle the exception and at the end choose only one of 1.A. or 1.B. 
; 1.A. resume program: 
;       mov     r10d,SIGCONT 
;       xor     edx,edx 
;       mov     rsi,qword [child_PID] 
;       mov     edi,PTRACE_CONT 
;       mov     eax,sys_ptrace 
;       syscall 
; 1.B. or kill the program: 
;       xor     r10,r10 
;       xor     edx,edx 
;       mov     rsi,qword [child_PID] 
;       mov     edi,PTRACE_KILL 
;       mov     eax,sys_ptrace 
;       syscall 

wait_L7: 
        jmp     parent_signal_wait 

exit0: 
        xor     edi,edi  
exit:   mov     eax,sys_exit 
        syscall 

exit1:  mov     edi,1 
        jmp     exit 



The above skeleton is from fdbg. I still don't know what to do when the child forks - something in the above code is missing. 
    The above handling idea is the same as every debugger's job (it is an auto-debugger, or self-debugger).
part 2 (for it not to be so simple...)
if you want new thread instead of new process, then use
sys_clone with CLONE_THREAD
instead of
sys_fork

you need to mmap space for new thread (new stack, ...) before sys_clone

how to skip instruction causing singal ?
0. read registers sys_ptrace with PTRACE_GETREGS and get RIP (EIP) register
(offset of instruction pointer differs in x64 and x86 platform)
1. determine size of instruction:
1A read 16 bytes (2 qwords in x64, 4 dwords in x86) sys_ptrace PTRACE_PEEKTEXT
1B determine the size of the instruction by disassembling it, the most difficult task
you can use disasm engine for x64 or simplify it not to disassemble
the whole instruction but to determine instruction size only
I don't know whether there is any disasm engine for x86 written in asm
2. add instruction size to user.user_regs.rip (or ...eip)
3. write registers sys_ptrace PTRACE_SETREGS
4. continue run sys_ptrace PTRACE_CONT

After doing this you can only hope that skipping instruction doesn't alter program run too much...
--*/
