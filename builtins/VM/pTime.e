--
-- pTime.e
-- =======
--
--  implements :%opTime
--
-- Note that the clock tick may wrap if your system has been running for around
--  16 times longer than the universe has thus far been in existence (scnr).
--

include VM\pHeap.e  -- :%pDealloc/:%pStoreFlt
include VM\pFPU.e   -- :%down53 etc

atom t0
constant ONETHOUSAND = 1000 -- (so we can "fild" it, rather than push/fild/pop)

#ilASM{ jmp :%opRetf

  :>Time0
---------
    [PE32]
        call "kernel32","GetTickCount64"
        push edx
        push eax
        fild qword[esp]
        add esp,8
    [ELF32]
--#     Name                        Registers                                                                                                               Definition
--                                  eax     ebx                     ecx                     edx                     esi                     edi
-->13   sys_time                    0x0d    time_t *tloc            -                       -                       -                       -               kernel/posix-timers.c:855
        xor ebx,ebx
        mov eax,13      -- sys_time
        int 0x80
        xor ebx,ebx
        push ebx
        push eax
        fild qword[esp]
        add esp,8
--13        sys_time                    0x0d    time_t *tloc            -                       -                       -                       -               kernel/posix-timers.c:855
    [32]
        lea edi,[t0]
--      mov edi,t0      -- lea edi,[t0]
        call :%pStoreFlt
    [PE64]
        mov rcx,rsp -- put 2 copies of rsp onto the stack...
        push rsp
        push rcx
        or rsp,8    -- [rsp] is now 1st or 2nd copy:
                    -- if on entry rsp was xxx8: both copies remain on the stack
                    -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                    -- obviously rsp is now xxx8, whatever alignment we started with
        sub rsp,8*5                             -- minimum 4 param shadow space, and align
        call "kernel32","GetTickCount64"
--      add rsp,8*5
--      pop rsp
        mov rsp,[rsp+8*5]   -- equivalent to the add/pop
        push rax
        fild qword[rsp]
        add rsp,8
    [ELF64]
--%rax  System call             %rdi                    %rsi                            %rdx                    %rcx                    %r8                     %r9
--201   sys_time                time_t *tloc
        xor rdi,rdi
        mov rax,201     -- sys_time
        syscall
        push rax
        fild qword[rsp]
        add rsp,8
    [64]
        lea rdi,[t0]
        call :%pStoreFlt    -- (also sets r15 to h4)
    []
        ret

--/*
procedure :%opTime(:%)
end procedure -- (for Edita/CtrlQ)
--*/
  :%opTime      -- [edi] := time()
---------
    [PE32]
        --calling convention:
        --  lea edi,[p1]    -- result location
        --  call :%opTime   -- [edi]=time()
        call "kernel32","GetTickCount64"
        push edx
        push eax
    [ELF32]
--#     Name                        Registers                                                                                                               Definition
--                                  eax     ebx                     ecx                     edx                     esi                     edi
-->13   sys_time                    0x0d    time_t *tloc            -                       -                       -                       -               kernel/posix-timers.c:855
        xor ebx,ebx
        mov eax,13      -- sys_time
        int 0x80
        xor ebx,ebx
        push ebx
        push eax
    [32]
        mov esi,[t0]
        fild qword[esp]
        add esp,8
        cmp esi,h4
        jl :t0int
            fld qword[ebx+esi*4]
            jmp @f
          ::t0int
            fild dword[t0]
      @@:
--DEV this should be illegal (it generates fsub st0,st0; no use to man nor beast)
--      fsub
--      fsubp   -- (good, same as next line)
        fsubp st1,st0           -- ie time()-t0
        fild dword[ONETHOUSAND]
    [PE64]
        --calling convention:
        --  lea rdi,[p1]    -- result location
        --  call :%opTime   -- [rdi]=time()
        mov rax,rsp -- put 2 copies of rsp onto the stack...
        push rsp
        push rax
        or rsp,8    -- [rsp] is now 1st or 2nd copy:
                    -- if on entry rsp was xxx8: both copies remain on the stack
                    -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                    -- obviously rsp is now xxx8, whatever alignment we started with
        sub rsp,8*5                             -- minimum 4 param shadow space, and align
        call "kernel32","GetTickCount64"
--      add rsp,8*5
--      pop rsp
        mov rsp,[rsp+8*5]   -- equivalent to the add/pop
        push rax
    [ELF64]
--%rax  System call             %rdi                    %rsi                            %rdx                    %rcx                    %r8                     %r9
--201   sys_time                time_t *tloc
        xor rdi,rdi
        mov rax,201     -- sys_time
        syscall
        push rax
    [64]
        mov rsi,[t0]
        fild qword[rsp]
        mov r15,h4
        add rsp,8
        cmp rsi,r15
        jl :t0int
            fld tbyte[ebx+esi*4]
            jmp @f
          ::t0int
            fild qword[t0]
      @@:
        fsubp st1,st0           -- ie time()-t0
        fild qword[ONETHOUSAND]
    []
        fdivp st1,st0
        jmp :%pStoreFlt

      }

