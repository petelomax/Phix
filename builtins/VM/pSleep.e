--
-- pSleep.e
-- ========
--
--  implements :%opSleep
--

include VM\pFPU.e   -- :%down53 etc

constant onebillion = 1_000_000_000     -- (for fimul/fild)

#ilASM{ jmp :%opRetf

::e115atsmba
    [32]
        pop edx
        mov al,115  -- e115atsmba
        sub edx,1
    [64]
        pop rdx
        mov al,115  -- e115atsmba
        sub rdx,1
    []
        jmp :!iDiag
        int3
--/*
procedure :%opSleep(:%)
end procedure -- (for Edita/CtrlQ)
--*/
  :%opSleep     -- sleep(eax)
-----------
    [32]
        -- mov eax,[p1]     -- seconds (opUnassigned)
        -- call :%opSleep   -- sleep(eax)
        cmp eax,h4
        jge :opSleepN
            push eax
            fild dword[esp]
            pop eax
            jmp @f
          ::opSleepN
            cmp byte[ebx+eax*4-1],0x12
            jne :e115atsmba
            fld qword[ebx+eax*4]
      @@:
--SUG:
--  :%opSleepST0
----------------
    [PE32]
        push 1000
--DEV this should now work, but ought to be tested...
        fimul dword[esp]
--      fild dword[esp]
--      fmulp st1,st0
        call :%down53
        fistp dword[esp]
        call :%near53
        call "kernel32.dll","Sleep"
    [ELF32]
--#     Name                        Registers                                                                                                               Definition
--                                  eax     ebx                     ecx                     edx                     esi                     edi
--162   sys_nanosleep               0xa2    struct timespec *rqtp   struct timespec *rmtp   -                       -                       -                       kernel/hrtimer.c:1606
--assume timespec is two dwords: sec and nsec(0..999,999,999)
        sub esp,16
        call :%down53
        fist dword[esp]
        call :%near53
        fisub dword[esp]
        fimul dword[onebillion]
        fistp dword[esp+4]
        lea ecx,[esp+8]     -- (output, don't care)
        mov ebx,esp
        mov eax,162         -- sys_nanosleep
        int 0x80
        xor ebx,ebx         -- (common requirement after int 0x80)
        add esp,16
    [64]
        --calling convention:
        -- mov rax,[p1]     -- seconds (opUnassigned)
        -- call :%opSleep   -- sleep(eax)
        mov r15,h4
        cmp rax,r15
        jge :opSleepN
            push rax
            fild qword[rsp]
            pop rax
            jmp @f
          ::opSleepN
            cmp byte[rbx+rax*4-1],0x12
            jne :e115atsmba
            fld tbyte[rbx+rax*4]
      @@:
    [PE64]
        push 1000
--      fimul qword[rsp]    -- no, 16/32 bit only
--      fild qword[rsp]
--      fmulp st1,st0
        fimul dword[rsp]    -- (oh duh - I think 1000 fits into a dword! teehee.)
        call :%down64
        fistp qword[rsp]
        call :%near64
        pop rcx                         -- dwMilliseconds (p1)
        mov rax,rsp -- put 2 copies of rsp onto the stack...
        push rsp
        push rax
        or rsp,8    -- [rsp] is now 1st or 2nd copy:
                    -- if on entry rsp was xxx8: both copies remain on the stack
                    -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                    -- obviously rsp is now xxx8, whatever alignment we started with
        sub rsp,8*5                             -- minimum 4 param shadow space, and align
--      (rcx already set)
        call "kernel32.dll","Sleep"
--      add rsp,8*5
--      pop rsp
        mov rsp,[rsp+8*5]   -- equivalent to the add/pop
    [ELF64]
--%rax  System call             %rdi                    %rsi                            %rdx                    %rcx                    %r8                     %r9
--35    sys_nanosleep           struct timespec *rqtp   struct timespec *rmtp
--timespec is two qwords: sec and nsec(0..999,999,999)[?]
        sub rsp,32
        call :%down64
--      fist qword[rsp]     -- (not supported at the hardware level)
        fld st0
        fistp qword[esp]
        call :%near64
--      fisub qword[rsp]    -- "" (16 or 32 bit only)
        fild qword[rsp]
        fsubp st1,st0
--      fimul qword[onebillion]     -- "" ""
        fild qword[onebillion]
        fmulp st1,st0
        fistp qword[rsp+8]
        lea rsi,[rsp+16]    -- (output, don't care)
        mov rdi,rsp
        mov rax,35          -- sys_nanosleep
        syscall
        add rsp,32
    []
        ret
      }

