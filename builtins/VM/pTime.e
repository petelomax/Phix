--
-- pTime.e
-- =======
--
--  implements :%opTime
--
-- Note that the clock tick may wrap if your system has been running for around
--  16 times longer than the universe has thus far been in existence (scnr).
-- Actually this will fail(wrap) in 2038 on Linux 32 bit and Windows XP.
--

include VM\pHeap.e  -- :%pStoreFlt etc
include VM\pFPU.e   -- :%down53 etc

atom t0 = 0         -- (millisecs on windows, whole secs on linux)
constant ONETHOUSAND = 1000 -- (so we can fild it, rather than push/fild/pop)

integer nsec0       -- linux only (should always be 0..999,999,999)
constant NSEC_PER_SEC = 1000000000  -- (a 31-bit int, just)

--GetTickCount64 is not available on XP...
constant string szGetTickCount64 = "GetTickCount64"
atom pGetTickCount64 = NULL -- (fall back to GetTickCount if we must)

#ilASM{ jmp :%opRetf

  :>Time0
---------
        cmp [t0],0
        jne :dont_set_t0_twice

    [PE32]
        mov eax,[szGetTickCount64]
        shl eax,2
        push eax                            -- lpLibFileName
        call "kernel32.dll","LoadLibraryA"
        test eax,eax
        jz :gtc32
            push ebx
            push eax
            lea edi,[pGetTickCount64]
            fild qword[esp]
            call :%pStoreFlt                -- ([edi]:=ST0)
            pop eax
            add esp,4
            call eax      -- GetTickCount64
            push edx
            jmp @f
     ::gtc32
            call "kernel32","GetTickCount"
            push ebx
     @@:                            
        push eax
        fild qword[esp]
        add esp,8
--/*
        call "kernel32","GetTickCount65"
        push edx
        push eax
        fild qword[esp]
        add esp,8
--*/
    [ELF32]
--#     Name                        Registers                                                                                                               Definition
--                                  eax     ebx                     ecx                     edx                     esi                     edi
--265   sys_clock_gettime           0x109   clockid_t which_clock   struct timespec *tp     -                       -                       -                       kernel/posix-timers.c:954
        sub esp,8
        mov eax,265     -- sys_clock_gettime
        xor ebx,ebx     -- CLOCK_REALTIME
        mov ecx,esp     -- timespec *tp
        int 0x80
        xor ebx,ebx     -- (common requirement after int 0x80)
        mov eax,[esp+4] -- tp.tv_nsec
        fild dword[esp] -- tp.tv_sec
        mov [nsec0],eax
        add esp,8
    [32]
        lea edi,[t0]
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
--228   sys_clock_gettime       const clockid_t which_clock     struct timespec *tp
        sub rsp,16
        mov rax,228     -- sys_clock_gettime
        xor rdi,rdi     -- CLOCK_REALTIME
        mov rsi,rsp     -- timespec *tp
        syscall
        mov rax,[rsp+8] -- tp.tv_nsec
        fild qword[rsp] -- tp.tv_sec
        mov [nsec0],rax
        add rsp,16
    [64]
        lea rdi,[t0]
        call :%pStoreFlt    -- (also sets r15 to h4)
    []
      ::dont_set_t0_twice
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
        mov eax,[pGetTickCount64]
        test eax,eax
        jz :use32
            call :%pLoadMint
            call eax      -- GetTickCount64
            push edx
            jmp @f
      ::use32
            call "kernel32","GetTickCount"
            push ebx
     @@:                            
        push eax
--/*
        call "kernel32","GetTickCount64"
        push edx
        push eax
--*/
    [ELF32]
--#     Name                        Registers                                                                                                               Definition
--                                  eax     ebx                     ecx                     edx                     esi                     edi
--265   sys_clock_gettime           0x109   clockid_t which_clock   struct timespec *tp     -                       -                       -                       kernel/posix-timers.c:954
--#define CLOCK_REALTIME 0
        sub esp,8
        mov eax,265     -- sys_clock_gettime
        xor ebx,ebx     -- CLOCK_REALTIME
        mov ecx,esp     -- timespec *tp
        int 0x80
        xor ebx,ebx             -- (common requirement after int 0x80)
        fild dword[esp+4]       -- tp.tv_nsec
        fild dword[nsec0]
        fsubp
        fild dword[NSEC_PER_SEC]
        fdivp                   -- (should be in the range -1.0..+1.0)
        mov dword[esp+4],ebx    -- (for the fild qword)
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
    [PE32]
        fild dword[ONETHOUSAND]
        fdivp st1,st0
    [ELF32]
        faddp
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
--228   sys_clock_gettime       const clockid_t which_clock     struct timespec *tp
        push rdi
        sub rsp,16
        mov eax,228     -- sys_clock_gettime
        xor rdi,rdi     -- CLOCK_REALTIME
        mov rsi,rsp     -- timespec *tp
        syscall
        fild qword[rsp+8]       -- tp.tv_nsec
        fild qword[nsec0]
        pop rax                 -- (move tv_sec)
        fsubp
        fild qword[NSEC_PER_SEC]
        fdivp                   -- (should be in the range -1.0..+1.0)
        mov [rsp],rax           -- (overwrite tv_nsec with tv_sec)
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
    [PE64]
        fild qword[ONETHOUSAND]
        fdivp st1,st0
    [ELF64]
        pop rdi
        faddp
    []
        jmp :%pStoreFlt

      }

