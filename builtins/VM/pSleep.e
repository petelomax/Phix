--
-- pSleep.e
-- ========
--
--  implements :%opSleep
--

include VM\pFPU.e   -- :%down53 etc

#ilASM{ jmp :%opRetf

--DEV:
    ::e115atsmba
        int3

--/*
procedure :%opSleep(:%)
end procedure -- (for Edita/CtrlQ)
--*/
  :%opSleep     -- sleep(eax)
-----------
    [32]
        --calling convention:
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
        push 1000
--      fimul dword[esp]
        fild dword[esp]
        fmulp st1,st0
        call :%down53
        fistp dword[esp]
        call :%near53
    [PE32]
        call "kernel32.dll","Sleep"
    [ELF32]
        pop al
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
        push 1000
--      fimul qword[rsp]
        fild qword[rsp]
        fmulp st1,st0
        call :%down64
        fistp qword[rsp]
        call :%near64
    [PE64]
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
        pop al
    []
        ret
      }

