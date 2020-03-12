--
-- pMem.e
--
--  implements :%opPeekNx, :%opPeeki, :%opPokeN, :%opPokeNS, :%opMemCopy, :%opMemSet

--SUG: if X64!=1 and emitON then abort("not supported on 32-bit") for peek/poke8
--  An atom in 32-bit Euphoria is limited to 53 bits of precision, therefore trying
--  to load/store 64 bit integers on that platform is just going to discard 11 bits, 
--  every time. Instead, simply use two peek4s/poke4 operations for each qword.

-- SUG: in peek(a)/peek({a,i})/poke(a,x), allow a to be a 2-element sequence of the
--      form {root,offset}. Ensure [root-8/16] is a pukka pRoot, ie [[root-8/16]]
--      is #00484253 aka "SBH\0", and offset >=0 and <=nSize aka [root-4/8].
--      Update the help files accordingly:
--          NEW: a can also be a 2-element sequence of the form {root,offset}, in
--               which case peek/poke ensures root is a live block from allocate
--               (that has not been freed) and 0<=offset<=sizeof(root), iyswim.
--               Obviously these forms are safer but cannot be used on memory
--               obtained from system calls, such as GetCommandLine.

include builtins\VM\pFPU.e  -- :%down53, :%near53

--include builtins\VM\pFEH.e    -- DEV/temp

#ilASM{ jmp :%opRetf

--DEV FIXME: (and the :!bang labels below, opPeekMLE, opPeeksMLE,)
    ::e41fatpmba
        int3
    ::e43atpmbaoso2a
        [32]
            mov edx,[esp+12]
            sub edx,1
        [64]
            mov rdx,[rsp+24]
            sub rdx,1
        []
        mov al,43           -- argument to peek must be atom or sequence of 2 atoms
        jmp :!iDiag
        int3
    ::e110opPeekiSeq
        int3
    ::e114stbpmoca
        int3
    ::e39atmcmba
        int3
    ::e22imcl
        int3
    ::e23imsl
        int3
    ::e40atmsmba
        int3
    ::epeeksize
        [32]
            mov edx,[esp+8]
            sub edx,1
        [64]
            mov rdx,[rsp+16]
            sub rdx,1
        []
        mov al,44           -- e44psmb1248 - peek size must be 1|2|4|8
        jmp :!iDiag
        int3

--/*
procedure :%opPeeki(:%)
end procedure -- (for Edita/CtrlQ)
--*/
  :%opPeeki     -- optimised case when result is integer (ie no dealloc)
-----------     -- (inlined in all cases except when e110opPeekiSeq might trigger...DEV)
    [32]
        --calling convention:
        --  mov esi,[p2]    -- ref addr (and not {addr,len}) (opUnassigned)
        --  call :%opPeeki
        --   only eax is altered
        --  mov [p1],eax
        xor eax,eax
        cmp esi,h4  --DEV :%pLoadMint
        jl @f
            cmp byte[ebx+esi*4-1],0x12
            jne :e110opPeekiSeq
            fld qword[ebx+esi*4]
            sub esp,8
            call :%down53
            fistp qword[esp]
            call :%near53
            mov esi,[esp]
            add esp,8
      @@:
      :!opPeekiRIMA             -- exception here mapped to e99ipmaespfeh (invalid peek memory address) [DEV]
        mov al,[esi]
        ret
    [64]
        --calling convention:
        --  mov rsi,[p2]    -- ref addr (and not {addr,len}) (opUnassigned)
        --  call :%opPeeki
        --   only rax is altered
        --  mov [p1],rax
        mov r15,h4
        xor rax,rax
        cmp rsi,r15
        jl @f
--DEV :%pLoadMint?
            cmp byte[rbx+rsi*4-1],0x12
            jne :e110opPeekiSeq
            fld tbyte[rbx+rsi*4]
            sub rsp,8
            call :%down64
            fistp qword[rsp]
            call :%near64
            pop rsi
      @@:
      :!opPeekiRIMA             -- exception here mapped to e99ipmaespfeh (invalid peek memory address)
        mov al,[rsi]
        ret
    []

--DEV:
--/*
procedure :%opPeek4x(:%DEAD)
end procedure -- (for Edita/CtrlQ)
--*/
--/*
  :%opPeek4x
------------
    [32]
      :!opPeek4sMLE         -- exception here mapped to e99ipmaespp4feh (invalid peek memory address)

      :!opPeek4ssMLE            -- exception here mapped to e99ipmaespp8feh (invalid peek memory address)

    [64]
      :!opPeek4sMLE         -- exception here mapped to e99ipmaespp4feh (invalid peek memory address)
      :!opPeek4ssMLE            -- exception here mapped to e99ipmaespp8feh (invalid peek memory address)
    []
--*/

--/*
procedure :%opPeekNx(:%)
end procedure -- (for Edita/CtrlQ)
--*/
  :%opPeekNx
------------
    [32]
        --calling convention
        --  lea edi,[p1]        -- target addr
        --  mov esi,[p2]        -- addr or {addr,len} (opUnassigned)
        --  mov ecx,-1/0        -- signed/unsigned (-1 or 0)
        --  mov edx,1/2/4/8     -- size (must be 1, 2, 4, or 8)
        --  call :%opPeekNx
        push edx
        push ecx
        cmp esi,h4  --DEV :%pLoadMint
        jl @f
            cmp byte[ebx+esi*4-1],0x12
            jne :PeekNxSeq
            fld qword[ebx+esi*4]
            sub esp,8
            call :%down53
            fistp qword[esp]
            call :%near53
            mov esi,[esp]
            add esp,8
      @@:

        xor eax,eax
        cmp edx,1
        jne @f
          :!opPeek1xMLE         -- exception here mapped to e99ipmaespp4feh (invalid peek memory address)
            mov al,[esi]        -- (era @ [esp+8])
            cmp dword[esp],0
            je :opPeekNxStore
            cbw                 -- (al -> ax)
            cwde                -- (ax -> eax)
            jmp :opPeekNxStore
      @@:
        cmp edx,2
        jne @f
          :!opPeek2xMLE         -- exception here mapped to e99ipmaespp4feh (invalid peek memory address)
            mov ax,[esi]
            cmp dword[esp],0
--DEV (spotted in passing) opPeekNxStore, surely? [insignificant performance gain/try when you got other stuff to test]
            je :opPeekNuAtom
            cwde                -- (ax -> eax)
            jmp :opPeekNxStore
      @@:
        cmp edx,4
        jne @f
          :!opPeek4xMLE         -- exception here mapped to e99ipmaespp4feh (invalid peek memory address)
            mov eax,[esi]
            cmp dword[esp],0
            je :opPeekNuAtom
--        ::opPeekNsAtom
                mov ecx,eax
                shl ecx,1           -- (this is a better test than cmp h4)
                jno :opPeekNxStore  -- store #C0000000..#3FFFFFFF (-1073741824..1073741823) in eax as short int

                push eax
                fild dword[esp]     -- load signed
                add esp,12          -- (discard eax, sign flag, and size)
                jmp :%pStoreFlt     -- store result (invokes dealloc if needed)

          ::opPeekNuAtom
                cmp eax,h4
                jb :opPeekNxStore   -- store unsigned 0..#3FFFFFFF (0..1073741823) in eax as short int
                push ebx
                push eax
                fild qword[esp]     -- load unsigned, ie as #00000000hhhhhhhh
                add esp,16          -- (discard qword, sign flag, and size)
                jmp :%pStoreFlt     -- store result (invokes dealloc if needed)
      @@:
        cmp edx,8
        jne :epeeksize
--        :!opPeek8xMLE         -- exception here mapped to e99ipmaespp4feh (invalid peek memory address)
--          cmp dword[esp+8],0
            cmp dword[esp],0        -- signed?
            je :opPeek8uItem
--            ::opPeek8sItem
                  :!opPeek8xsMLE        -- exception here mapped to e99ipmaespp8feh (invalid peek memory address)
                    fild qword[esi]
                    jmp @f

              ::opPeek8uItem
                  :!opPeek8xuMLE        -- exception here mapped to e99ipmaespp8feh (invalid peek memory address)
                    mov eax,[esi]
                    mov edx,[esi+4]
                    -- to load unsigned, right shift edx:eax by 1, save odd bit in ecx, then *2+[0|1]
                    mov ecx,ebx
                    shr edx,1
                    rcr eax,1
                    rcl ecx,1
                    push edx
                    push eax
                    push ecx
                    fild dword[esp]
                    fild qword[esp+4]
                    add esp,12
                    fadd st0,st0
                    faddp
                  @@:
--                  fist qword[esp] (oops, the hardware does not support this)
                    fld st0
                    fistp qword[esp]
                    mov eax,[esp]
                    mov edx,[esp+4]
                    add esp,8
                    cmp eax,[esi]
                    jne @f
                        cmp edx,[esi+4]
                        je :%pStoreFlt  -- store result (invokes dealloc if needed)
                  @@:
                    sub esp,8           -- (put back sign/size space, for opPeekNxStorePop2 only, no matter if they got damaged)
                    push edi            --[1]target addr (as per PeekNxSeq)
                    mov eax,[esi]
                    mov edx,[esi+4]
                    fstp st0            -- discard
                    push ebx
                    push eax            -- {hi_dword,lo_dword} order
                    fild qword[esp]
                    mov [esp],edx
                    fild qword[esp]
                    add esp,8
                    mov ecx,2
                    mov edx,[esp+12]        -- era
                    call :%pAllocSeq
                    push eax            --[2] (as per opPeekNxLoop-1)
                    mov [ebx+eax*4],ebx
                    lea edi,[ebx+eax*4]
                    call :%pStoreFlt
                    mov [edi+4],ebx
                    add edi,4
                    call :%pStoreFlt
                    jmp :opPeekNxStorePop2

      ::PeekNxSeq                       -- peek4x({addr,len}) case
-----------------
        push edi                        -- [1]target addr
        cmp byte[ebx+esi*4-1],0x80      -- sequence
        jnz :e43atpmbaoso2a             -- argument to peek must be atom or sequence of 2 atoms
        cmp dword[ebx+esi*4-12],2       -- of length 2
        jne :e43atpmbaoso2a             -- argument to peek must be atom or sequence of 2 atoms
        mov ecx,[ebx+esi*4+4]           -- len (ie p2[2])
        cmp ecx,h4  --DEV :%pLoadMint
        jl @f
            cmp byte[ebx+ecx*4-1],0x12
            jne :e43atpmbaoso2a
            fld qword[ebx+ecx*4]
            sub esp,8
            call :%down53
            fistp qword[esp]
            call :%near53
            mov ecx,[esp]
            add esp,8
      @@:
        mov esi,[ebx+esi*4]             -- addr (ie p2[1])
        cmp esi,h4 --DEV :%pLoadMint
        jl @f
            cmp byte[ebx+esi*4-1],0x12
            jne :e43atpmbaoso2a
            fld qword[ebx+esi*4]
            sub esp,8
            call :%down53
            fistp qword[esp]
            call :%near53
            mov esi,[esp]
            add esp,8
      @@:
        -- OK, len in ecx, and addr in esi
        cmp dword[esp+4],0              -- if unsigned..
        jne @f
            cmp dword[esp+8],1          --    ..bytes (ie opPeek aka opPeek1u)
            jne @f
                call :%pAllocStr        --           .. then a string will be fine
                mov [ecx+eax*4],bl      --  plant the terminating 0 now
                jmp :opPeekNxTestEcx
      @@:
        mov edx,[esp+12]                -- era
        call :%pAllocSeq                -- damages eax only
      ::opPeekNxTestEcx
        test ecx,ecx
--23/5/15
--      jz :opPeekNxStore
        jnz @f
            add esp,4                   --[1] aka pop edi
            jmp :opPeekNxStore
      @@:
        lea edi,[ebx+eax*4]
        push eax                        --[2]
      ::opPeekNxLoop
            mov edx,[esp+12]            -- size (1/2/4/8)
            cmp edx,1
            jne @f
              :!opPeek1xsMLE            -- exception here mapped to e99ipmaespp8feh (invalid peek memory address)
                lodsb                   -- (era @ [esp+16])
                cmp dword[esp+8],0      -- sign (0=unsigned, -1=signed)
                je :opPeek1us
                    cbw                 -- (al -> ax)
                    cwde                -- (ax -> eax)
                    stosd
                    jmp :opPeekNxsNxt
              ::opPeek1us
--                  and eax,#FF -- (no need)
                    stosb
                    jmp :opPeekNxsNxt
          @@:
            cmp edx,2
            jne @f
              :!opPeek2xsMLE            -- exception here mapped to e99ipmaespp8feh (invalid peek memory address)
                lodsw
                and eax,#FFFF
                cmp dword[esp+8],0
                je :opPeek2us
                    cwde                -- (ax -> eax)
              ::opPeek2us
                stosd
                jmp :opPeekNxsNxt
          @@:
            cmp edx,4
            jne @f
              :!opPeek4xsMLE            -- exception here mapped to e99ipmaespp8feh (invalid peek memory address)
                lodsd                   -- mov eax,[esi], esi+=4
                mov edx,eax
                stosd                   -- store short ints #C0000000 .. #3FFFFFFF (-1073741824..1073741823) direct
                                        -- (mov [edi],eax; edi+=4)
                cmp dword[esp+8],0
                je :opPeek4usItem
--            ::opPeek4sItem
                    shl edx,1
                    jno :opPeekNxsNxt
                    mov [edi-4],ebx         -- 0 (for StoreFlt, cleanup mess we just put there)
                    sub edi,4
                    push eax
                    fild dword[esp]
                    add esp,4
                    call :%pStoreFlt        -- all registers preserved
                    lea edi,[edi+4]
                    jmp :opPeekNxsNxt
              ::opPeek4usItem
                    cmp edx,h4
                    jb :opPeekNxsNxt
                    mov [edi-4],ebx         -- 0 (for StoreFlt, since new AllocSeq contains garbage)
                    sub edi,4
                    push ebx
                    push eax
                    fild qword[esp]
                    add esp,8
                    call :%pStoreFlt
                    lea edi,[edi+4]
                    jmp :opPeekNxsNxt
          @@:
            cmp edx,8
            jne :epeeksize
                cmp dword[esp+8],0
                je :opPeek8usItem
--            ::opPeek8ssItem
                  :!opPeek8xsMLE2           -- exception here mapped to e99ipmaespp8feh (invalid peek memory address)
                    fild qword[esi]
                    jmp @f
--pop al -- check for precision loss...
----also, we forgot edi, and [edi]
--                  call :%pStoreFlt        -- all registers preserved
--                  add esi,8
--                  jmp :opPeekNxsNxt
              ::opPeek8usItem
                  :!opPeek8xuMLE2           -- exception here mapped to e99ipmaespp8feh (invalid peek memory address)
                    push ecx                --[1]
                    mov eax,[esi]
                    mov edx,[esi+4]
                    -- to load unsigned, right shift edx:eax by 1, saving odd bit in ecx, then *2 + that[0|1]:
                    mov ecx,ebx
                    shr edx,1
                    rcr eax,1
                    rcl ecx,1
                    push edx
                    push eax
                    push ecx
                    fild dword[esp]
                    fild qword[esp+4]
                    add esp,12
                    fadd st0,st0
                    pop ecx                 --[1]
                    faddp
                  @@:
                    mov [edi],ebx       -- 0 (for StoreFlt, since new AllocSeq contains garbage)
                    -- check for precision loss (not needed on 64-bit!)
                    sub esp,8
--                  fist qword[esp] (oops, the hardware does not support this)
                    fld st0
                    fistp qword[esp]
                    mov eax,[esp]
                    mov edx,[esp+4]
                    add esp,8
                    cmp eax,[esi]
                    jne @f
                    cmp edx,[esi+4]
                    jne @f
                        call :%pStoreFlt
                        add esi,8
                        add edi,4
                        jmp :opPeekNxsNxt
                  @@:
                    add edi,4           -- next ([edi-4] stored below)
                    mov eax,[esi]
                    mov edx,[esi+4]
                    fstp st0            -- discard
                    push edi            ---[1]
                    push ebx
                    push eax            -- {hi_dword,lo_dword} order
                    fild qword[esp]
                    mov [esp],edx
                    fild qword[esp]
                    add esp,8
                    push ecx            --[2]
                    mov ecx,2
                    mov edx,[esp+16]    -- era
                    call :%pAllocSeq    -- damages eax only
                    mov [edi-4],eax
                    lea edi,[ebx+eax*4]
                    mov [ebx+eax*4],ebx
                    call :%pStoreFlt
                    mov [edi+4],ebx
                    add edi,4
                    call :%pStoreFlt
                    add esi,8
                    pop ecx             --[2]
                    pop edi             --[1]

          ::opPeekNxsNxt
            sub ecx,1
            jnz :opPeekNxLoop

      ::opPeekNxStorePop2
        pop eax                 -- [2]Newly allocated sequence
        pop edi                 -- [1]target addr

      ::opPeekNxStore
        mov edx,[edi]
        add esp,8               -- discard sign flag and size
        mov [edi],eax
        cmp edx,h4
        jle @f
            sub dword[ebx+edx*4-8],1
            jz :%pDealloc
      @@:
        ret
    [64]
        --calling convention
        --  lea rdi,[p1]        -- target addr
        --  mov rsi,[p2]        -- addr or {addr,len} (opUnassigned)
        --  mov rcx,-1/0        -- signed/unsigned (-1 or 0)
        --  mov rdx,1/2/4/8     -- size (must be 1, 2, 4, or 8)
        --  call :%opPeekNx
        mov r15,h4
        push rdx
        push rcx
        cmp rsi,r15
        jl @f
--DEV :%pLoadMint
            cmp byte[rbx+rsi*4-1],0x12
            jne :PeekNxSeq64
            fld tbyte[rbx+rsi*4]
            sub rsp,8
            call :%down64
            fistp qword[rsp]
            call :%near64
            pop rsi
      @@:

        xor rax,rax
        cmp rdx,1
        jne @f
          :!opPeek1xMLE64           -- exception here mapped to e99ipmaespp4feh (invalid peek memory address)
            mov al,[rsi]            -- (era @ [rsp+16])
            cmp qword[rsp],0
            je :opPeekNxStore64
            cbw                     -- (al -> ax)
            cwde                    -- (ax -> eax)
            cdqe                    -- (sign extend eax to rax)
            jmp :opPeekNxStore64
      @@:
        cmp rdx,2
        jne @f
          :!opPeek2xMLE64           -- exception here mapped to e99ipmaespp4feh (invalid peek memory address)
            mov ax,[rsi]
            cmp qword[rsp],0
            je :opPeekNxStore64
            cwde                    -- (ax -> eax)
            cdqe                    -- (sign extend eax to rax)
            jmp :opPeekNxStore64
      @@:
        cmp rdx,4
        jne @f
          :!opPeek4xMLE64           -- exception here mapped to e99ipmaespp4feh (invalid peek memory address)
            mov eax,[rsi]
            cmp dword[rsp],0
            je :opPeekNxStore64
            cdqe                    -- (sign extend eax to rax)
            jmp :opPeekNxStore64
--?             mov ecx,eax
--              shl ecx,1           -- (this is a better test than cmp h4)
--              jno :opPeekNxStore  -- store #C0000000..#3FFFFFFF (-1073741824..1073741823) in eax as short int

--              push eax
--              fild dword[esp]
--              add esp,12          -- (discard eax, sign flag, and size)
--              jmp :%pStoreFlt     -- store result (invokes dealloc if needed)

--?       ::opPeekNuAtom64
--              cmp rax,r15
--              jb :opPeekNxStore64 -- store unsigned 0..#3FFFFFFFFFFFFFFF in rax as short int
--              push rbx
--              fild qword[rsp]
--              add rsp,16          -- (discard qword, sign flag, and size)
--              jmp :%pStoreFlt     -- store result (invokes dealloc if needed)
      @@:
        cmp rdx,8
        jne :epeeksize
          :!opPeek8xMLE64           -- exception here mapped to e99ipmaespp4feh (invalid peek memory address)
            mov rax,[rsi]
--          cmp dword[esp+8],0
            cmp qword[rsp],0        -- signed?
            je :opPeek8uItem64
--            ::opPeek8sItem64
                mov rcx,rax
                shl rcx,1               -- (this is a better test than cmp h4)
                jno :opPeekNxStore64    -- store #C00000000000000..#3FFFFFFFFFFFFFFF in rax as short int

                fild qword[rsi]
--              jmp @f
                add rsp,16
                jmp :%pStoreFlt

          ::opPeek8uItem64
                -- to load unsigned, right shift rax by 1, save odd bit in rcx, then *2+[0|1]
                mov rcx,rbx
                shr rax,1
                rcl rcx,1
                push rax
                push rcx
                fild qword[rsp]
                fild qword[rsp+8]
                add rsp,16
                fadd st0,st0
                faddp
--            @@:
--              fist qword[rsp] (oops, the hardware does not support this)
--              fld st0
--              fistp qword[rsp]
--              mov rax,[rsp]
                add rsp,16
--              cmp rax,[rsi]
--              je :%pStoreFlt      -- store result (invokes dealloc if needed)
                jmp :%pStoreFlt     -- store result (invokes dealloc if needed)

      ::PeekNxSeq64                     -- peekNx({addr,len}) case
-----------------
        push rdi                        -- [1]target addr
        cmp byte[rbx+rsi*4-1],0x80      -- sequence
        jnz :e43atpmbaoso2a             -- argument to peek must be atom or sequence of 2 atoms
        cmp qword[rbx+rsi*4-24],2       -- of length 2
        jne :e43atpmbaoso2a             -- argument to peek must be atom or sequence of 2 atoms
        mov rcx,[rbx+rsi*4+8]           -- len (ie p2[2])
        cmp rcx,r15
        jl @f
--DEV :%pLoadMint
            cmp byte[rbx+rcx*4-1],0x12
            jne :e43atpmbaoso2a
            fld tbyte[rbx+rcx*4]
            sub rsp,8
            call :%down64
            fistp qword[rsp]
            call :%near64
            pop rcx
      @@:
        mov rsi,[rbx+rsi*4]             -- addr (ie p2[1])
        cmp rsi,r15
        jl @f
--DEV :%pLoadMint
            cmp byte[rbx+rsi*4-1],0x12
            jne :e43atpmbaoso2a
            fld tbyte[rbx+rsi*4]
            sub rsp,8
            call :%down64
            fistp qword[rsp]
            call :%near64
            pop rsi
      @@:
        -- OK, len in rcx, and addr in rsi
        cmp qword[rsp+8],0              -- if unsigned..
        jne @f
            cmp qword[rsp+16],1         --    ..bytes (ie opPeek aka opPeek1u)
            jne @f
                call :%pAllocStr        --           .. then a string will be fine
                mov [rcx+rax*4],bl      --  plant the terminating 0 now
                jmp :opPeekNxTestRcx
      @@:
        mov rdx,[rsp+24]                -- era
        call :%pAllocSeq                -- damages rax only
      ::opPeekNxTestRcx
        test rcx,rcx
--23/5/15:
--      jz :opPeekNxStore64
        jnz @f
--21/9/19:
--          sub rsp,8                   --[1] aka pop rdi
            add rsp,8                   --[1] aka pop rdi
            jmp :opPeekNxStore64
      @@:
        lea rdi,[rbx+rax*4]
        push rax                        --[2]
      ::opPeekNxLoop64
            mov rdx,[rsp+24]            -- size (1/2/4/8)
            xor rax,rax
            cmp rdx,1
            jne @f
              :!opPeek1xsMLE64          -- exception here mapped to e99ipmaespp8feh (invalid peek memory address)
                lodsb                   -- (era @ [rsp+32])
                cmp qword[rsp+16],0     -- sign (0=unsigned, -1=signed)
                je :opPeek1us64
                    cbw                 -- (al -> ax)
                  ::opPeek2ss64
                    cwde                -- (ax -> eax)
                  ::opPeek4ss64
                    cdqe                -- (sign extend eax to rax)
                    stosq
                    jmp :opPeekNxsNxt64
              ::opPeek1us64
                    stosb
                    jmp :opPeekNxsNxt64
          @@:
            cmp rdx,2
            jne @f
              :!opPeek2xsMLE64          -- exception here mapped to e99ipmaespp8feh (invalid peek memory address)
                lodsw
                cmp qword[rsp+16],0
                jne :opPeek2ss64
                stosq
                jmp :opPeekNxsNxt64
          @@:
            cmp rdx,4
            jne @f
              :!opPeek4xsMLE64          -- exception here mapped to e99ipmaespp8feh (invalid peek memory address)
                lodsd                   -- mov eax,[rsi], rsi+=4
                cmp qword[rsp+16],0
                jne :opPeek4ss64
                stosq
                jmp :opPeekNxsNxt64
          @@:
            cmp rdx,8
            jne :epeeksize
                cmp qword[rsp+16],0
                je :opPeek8usItem64
--            ::opPeek8ssItem64
                  :!opPeek8xsMLE264     -- exception here mapped to e99ipmaespp8feh (invalid peek memory address)
                    fild qword[rsi]
                    jmp @f
--                  mov [edi],rbx
--                  call :%pStoreFlt    -- all registers preserved
--                  add esi,8
--                  add edi,8
--                  jmp :opPeekNxsNxt64
              ::opPeek8usItem64
                  :!opPeek8xuMLE264     -- exception here mapped to e99ipmaespp8feh (invalid peek memory address)
                    push rcx            --[3]
                    mov rax,[rsi]
                    -- to load unsigned, right shift rax by 1, saving odd bit in rcx, then *2 + that[0|1]:
                    mov rcx,rbx
                    shr rax,1
                    rcl rcx,1
                    push rax
                    push rcx
                    fild qword[rsp]
                    fild qword[rsp+8]
                    add rsp,16
                    fadd st0,st0
                    pop rcx             --[3]
                    faddp
                  @@:
                    mov [rdi],rbx   -- 0 (for StoreFlt, since new AllocSeq contains garbage)
                    call :%pStoreFlt
                    add rsi,8
                    add rdi,8

          ::opPeekNxsNxt64
            sub rcx,1
            jnz :opPeekNxLoop64

        pop rax                 -- [2]Newly allocated sequence
        pop rdi                 -- [1]target addr

      ::opPeekNxStore64
        mov rdx,[rdi]
        add rsp,16              -- discard sign flag and size
        mov [rdi],rax
        cmp rdx,r15
        jle @f
            sub qword[rbx+rdx*4-16],1
            jz :%pDealloc
      @@:
        ret
    []

--/*
procedure :%opPokeN(:%)
end procedure -- (for Edita/CtrlQ)
--*/
  :%opPokeN
-----------
    [32]
        --calling convention
        --  mov edi,[p1] -- addr (opUnassigned)
        --  mov eax,[p2] -- object to poke (opUnassigned)
        --  mov ecx,1/2/4/8
        --  xor edx,edx     -- (poke8 only) [eax is float flag]
        --  call :%opPokeN  -- poke1/2/4/8(edi,eax)
        cmp edi,h4 --DEV :%pLoadMint
        jl @f
            cmp byte[ebx+edi*4-1],0x12
            jne :e41fatpmba     -- first argument to poke must be atom
            fld qword[ebx+edi*4]
            sub esp,8
            call :%down53
            fistp qword[esp]
            call :%near53
            mov edi,[esp]
            add esp,8
      @@:
--    ::opPokeNediset
        cmp eax,h4
        jl @f
            cmp byte[ebx+eax*4-1],0x12
            jne :PokeNSeq
            fld qword[ebx+eax*4]
            sub esp,8
--          call :%down53
            call :%trunc53
            fistp qword[esp]
            call :%near53
            mov esi,eax             -- (for poke8)
            mov eax,[esp]
--          mov edx,[esp+4]         -- (for poke8)
            mov edx,1               -- (for poke8)
            add esp,8
      @@:
        cmp ecx,1
        jne @f
          :!PokeN1E30               -- exception here mapped to e100ipmafeh
            mov byte[edi],al
            ret
      @@:
        cmp ecx,2
        jne @f
          :!PokeN2E30               -- exception here mapped to e100ipmafeh
            mov word[edi],ax
            ret
      @@:
        cmp ecx,4
        jne @f
          :!PokeN4E30               -- exception here mapped to e100ipmafeh
            mov dword[edi],eax
            ret
      @@:
        cmp ecx,8
        je @f
          ::epokesize
            pop edx
            mov al,122      -- e122ips
            sub edx,1
            jmp :!iDiag
            int3
        @@:
            cmp edx,0
            je @f
--!!eax!! (I think)
                fld qword[ebx+esi*4]
--              fld qword[ebx+eax*4]
                sub esp,12
--              call :%down64
                call :%trunc64
--              fistp tbyte[esp]    -- oh dear, no such thing...
                fistp qword[esp]
                call :%near53
                mov eax,dword[esp]
                mov edx,dword[esp+4]
                add esp,12
                jmp :opPoke8edieaxedx
          @@:
            cdq                     -- sign extend eax into edx
          ::opPoke8edieaxedx
          :!PokeN8E30               -- exception here mapped to e100ipmafeh
            mov dword[edi],eax
            mov dword[edi+4],edx
          @@:
            ret

      ::PokeNSeq
        mov edx,ecx
        mov ecx,[ebx+eax*4-12]      -- length
        lea esi,[ebx+eax*4]
        -- edi now contains addr, esi raw addr of seq/str(p2), ecx the length
        cmp ecx,0
        je @b
        cmp byte[ebx+eax*4-1],0x80
        jne :PokeNStr
--      cmp ecx,8
        cmp edx,8
        je :Poke8SeqLoop

      ::PokeNSeqLoop
            mov eax,[esi]
            cmp eax,h4  --DEV :%pLoadMint
            jl @f
            cmp byte[ebx+eax*4-1],0x12
            jne :e114stbpmoca           -- sequence to be poked must only contain atoms
            fld qword[ebx+eax*4]
            sub esp,8
--          call :%down53
            call :%trunc53
            fistp qword[esp]
            call :%near53
            mov eax,[esp]
            add esp,8
          @@:
            cmp edx,1
            jne @f
              :!Poke1SeqE30                 -- exception here mapped to e100ipmafeh
                stosb                       -- mov [edi],al; edi+=1
                jmp :PokeNSeqNext
          @@:
            cmp edx,2
            jne @f
              :!Poke2SeqE30                 -- exception here mapped to e100ipmafeh
                stosw                       -- mov [edi],ax; edi+=2
                jmp :PokeNSeqNext
          @@:
            cmp edx,4
            jne :epokesize
              :!Poke3SeqE30                 -- exception here mapped to e100ipmafeh
                stosd                       -- mov [edi],eax; edi+=4
          ::PokeNSeqNext
            lea esi,[esi+4]
            sub ecx,1
            jnz :PokeNSeqLoop
        nop
        ret

      ::Poke8SeqLoop
            mov eax,[esi]
            cmp eax,h4
            jl @f
                cmp byte[ebx+eax*4-1],0x12
--DEV or, if it is a sequence of 2 atoms, poke4 them...
                jne :e114stbpmoca           -- sequence to be poked must only contain atoms
                fld qword[ebx+eax*4]
                sub esp,8
--              call :%down64
                call :%trunc64
--              fistp tbyte[esp]    -- oh dear, no such thing...
                fistp dword[esp]
                call :%near53
                mov eax,[esp]
                mov edx,[esp+4]
                add esp,8
                jmp :Poke8sedxeax
          @@:
            cdq                 -- sign extend eax into edx
          ::Poke8sedxeax
          :!Poke8SeqE30                  -- exception here mapped to e100ipmafeh
            mov dword[edi],eax
          :!Poke1SeqE30a                 -- exception here mapped to e100ipmafeh
            mov dword[edi+4],edx
            add edi,8
            lea esi,[esi+4]
            sub ecx,1
            jnz :Poke8SeqLoop
        nop
        ret
                
      ::PokeNStr
        -- edi now contains addr, esi raw addr of string(p2), ecx the length
        cmp byte[ebx+eax*4-1],0x82
        je @f
            pop edx
            mov al,85           -- e85utb - unknown type byte (not 0x12, 0x80, or 0x82)
            sub edx,1
            jmp :!iDiag
            int3
      @@:
        cmp edx,1
        jne @f
          :!PokeN1StrE30                -- exception here mapped to e100ipmafeh
            rep movsb
            ret
      @@:
        xor eax,eax
      ::PokeNStrLoop
        lodsb                       -- mov al,[esi]; esi+=1
        cmp edx,2
        jne @f
          :!PokeN2StrE30            -- exception here mapped to e100ipmafeh
            stosw                   -- mov [edi],ax; edi+=2
            jmp :PokeNStrNext
      @@:
        cmp edx,4
        jne @f
          :!PokeN4StrE30            -- exception here mapped to e100ipmafeh
            stosd                   -- mov [edi],eax; edi+=4
            jmp :PokeNStrNext
      @@:
        cmp edx,8
        jne :epokesize
          :!PokeN8StrE30            -- exception here mapped to e100ipmafeh
            stosd                   -- mov [edi],eax; edi+=4 (+4 rsn)
            mov dword[edi],ebx      -- hi dword of 0 (fine for char->qword)
            add edi,4

      ::PokeNStrNext
        sub ecx,1
        jnz :PokeNStrLoop

        nop
        ret

    [64]
        --calling convention
        --  mov rdi,[p1] -- addr (opUnassigned)
        --  mov rax,[p2] -- object to poke (opUnassigned)
        --  mov rcx,1/2/4/8
        --  call :%opPokeN  -- poke1/2/4/8(rdi,rax)
        mov r15,h4
        xor rdx,rdx
        cmp rdi,r15
        jl @f
--DEV :%pLoadMint
            cmp byte[rbx+rdi*4-1],0x12
            jne :e41fatpmba     -- first argument to poke must be atom
            fld tbyte[rbx+rdi*4]
            sub rsp,8
            call :%down64
            fistp qword[rsp]
            call :%near64
            pop rdi
      @@:
        cmp rax,r15
        jl @f
--DEV :%pLoadMint
            cmp byte[rbx+rax*4-1],0x12
            jne :PokeNSeq64
            fld tbyte[rbx+rax*4]
            sub rsp,8
--          call :%down64
            call :%trunc64
            fistp qword[rsp]
            call :%near64
            pop rax
      @@:
        cmp rcx,1
        jne @f
          :!PokeN1E30               -- exception here mapped to e100ipmafeh
            mov byte[rdi],al
            ret
      @@:
        cmp rcx,2
        jne @f
          :!PokeN2E30               -- exception here mapped to e100ipmafeh
            mov word[rdi],ax
            ret
      @@:
        cmp rcx,4
        jne @f
          :!PokeN4E30               -- exception here mapped to e100ipmafeh
            mov dword[rdi],eax
            ret
      @@:
        cmp rcx,8
        je @f
          ::epokesize
            pop rdx
            mov al,122      -- e122ips
            sub rdx,1
            jmp :!iDiag
            int3
        @@:
          :!PokeN8E30               -- exception here mapped to e100ipmafeh
            mov qword[rdi],rax
          @@:
            ret

      ::PokeNSeq64
        mov rdx,rcx
        mov rcx,[rbx+rax*4-24]      -- length
        lea rsi,[rbx+rax*4]
        -- rdi now contains addr, rsi raw addr of seq/str(p2), rcx the length
        cmp rcx,0
        je @b
        cmp byte[rbx+rax*4-1],0x80
        jne :PokeNStr64
--      cmp rdx,8
--      je :Poke8SeqLoop

      ::PokeNSeqLoop64
            mov rax,[rsi]
            add rsi,8
            cmp rax,r15
            jl @f
--DEV :%pLoadMint
                cmp byte[rbx+rax*4-1],0x12
                jne :e114stbpmoca           -- sequence to be poked must only contain atoms
                fld tbyte[rbx+rax*4]
                sub rsp,8
--              call :%down64
                call :%trunc64
                fistp qword[rsp]
                call :%near64
                pop rax
          @@:
            cmp rdx,1
            jne @f
              :!Poke1SeqE30                 -- exception here mapped to e100ipmafeh
                stosb                       -- mov [rdi],al; rdi+=1
                jmp :PokeNSeqNext64
          @@:
            cmp rdx,2
            jne @f
              :!Poke2SeqE30                 -- exception here mapped to e100ipmafeh
                stosw                       -- mov [rdi],ax; rdi+=2
                jmp :PokeNSeqNext64
          @@:
            cmp rdx,4
            jne @f
              :!Poke4SeqE30                 -- exception here mapped to e100ipmafeh
                stosd                       -- mov [rdi],eax; rdi+=4
                jmp :PokeNSeqNext64

          @@:
            cmp rdx,8
            jne :epokesize
              :!Poke8SeqE30                 -- exception here mapped to e100ipmafeh
                stosq                       -- mov [rdi],rax; rdi+=8

          ::PokeNSeqNext64
            sub rcx,1
            jnz :PokeNSeqLoop64
        nop
        ret

      ::PokeNStr64
        -- rdi now contains addr, rsi raw addr of string(p2), rcx the length
        cmp byte[rbx+rax*4-1],0x82
        je @f
            pop rdx
            mov al,85           -- e85utb - unknown type byte (not 0x12, 0x80, or 0x82)
            sub rdx,1
            jmp :!iDiag
            int3
      @@:
        cmp rdx,1
        jne @f
          :!PokeN1StrE30            -- exception here mapped to e100ipmafeh
            rep movsb
            ret
      @@:
        xor rax,rax
      ::PokeNStrLoop64
        lodsb                       -- mov al,[rsi]; rsi+=1
        cmp rdx,2
        jne @f
          :!PokeN2StrE30            -- exception here mapped to e100ipmafeh
            stosw                   -- mov [rdi],ax; rdi+=2
            jmp :PokeNStrNext64
      @@:
        cmp rdx,4
        jne @f
          :!PokeN4StrE30            -- exception here mapped to e100ipmafeh
            stosd                   -- mov [rdi],eax; rdi+=4
            jmp :PokeNStrNext64
      @@:
        cmp rdx,8
        jne :epokesize
          :!PokeN8StrE30            -- exception here mapped to e100ipmafeh
            stosq                   -- mov [rdi],rax; rdi+=8

      ::PokeNStrNext64
        sub rcx,1
        jnz :PokeNStrLoop64

        nop
        ret
    []

--/*
procedure :%opMemCopy(:%)
end procedure -- (for Edita/CtrlQ)
--*/
  :%opMemCopy
-------------
    [32]
        -- calling convention:
        --  mov esi,[p1]        -- source addr
        --  mov edi,[p2]        -- dest addr
        --  mov ecx,[p3]        -- copy length
        --  call :%opMemCopy    -- mem_copy(esi,edi,ecx)
        cmp ecx,h4 --DEV :%pLoadMint
        jl @f
            cmp byte[ebx+ecx*4-1],0x12
            jne :e39atmcmba    -- arguments to mem_copy must be atoms
            fld qword[ebx+ecx*4]
            sub esp,8
            call :%down53
            fistp qword[esp]
            call :%near53
            mov ecx,[esp]
            add esp,8
      @@:
        cmp ecx,0
        jl :e22imcl             -- invalid mem_copy length

        cmp esi,h4 --DEV :%pLoadMint
        jl @f
            cmp byte[ebx+esi*4-1],0x12
            jne :e39atmcmba    -- arguments to mem_copy must be atoms
            fld qword[ebx+esi*4]
            sub esp,8
            call :%down53
            fistp qword[esp]
            call :%near53
            mov esi,[esp]
            add esp,8
      @@:
        cmp edi,h4 --DEV :%pLoadMint
        jl @f
            cmp byte[ebx+edi*4-1],0x12
            jne :e39atmcmba    -- arguments to mem_copy must be atoms
            fld qword[ebx+edi*4]
            sub esp,8
            call :%down53
            fistp qword[esp]
            call :%near53
            mov edi,[esp]
            add esp,8
      @@:
  :!MemCopyIMA  -- exception here mapped to e24imcmafeh
        rep movsb
        ret
    [64]
        -- calling convention:
        --  mov rsi,[p1]        -- source addr
        --  mov rdi,[p2]        -- dest addr
        --  mov rcx,[p3]        -- copy length
        --  call :%opMemCopy    -- mem_copy(rsi,rdi,rcx)
        mov r15,h4
        cmp rcx,r15
        jl @f
--DEV :%pLoadMint
            cmp byte[rbx+rcx*4-1],0x12
            jne :e39atmcmba    -- arguments to mem_copy must be atoms
            fld tbyte[rbx+rcx*4]
            sub rsp,8
            call :%down64
            fistp qword[rsp]
            call :%near64
            pop rcx
      @@:
        cmp rcx,0
        jl :e22imcl             -- invalid mem_copy length

        cmp rsi,r15
        jl @f
--DEV :%pLoadMint
            cmp byte[rbx+rsi*4-1],0x12
            jne :e39atmcmba    -- arguments to mem_copy must be atoms
            fld tbyte[rbx+rsi*4]
            sub rsp,8
            call :%down64
            fistp qword[rsp]
            call :%near64
            pop rsi
      @@:
        cmp rdi,r15
        jl @f
--DEV :%pLoadMint
            cmp byte[rbx+rdi*4-1],0x12
            jne :e39atmcmba    -- arguments to mem_copy must be atoms
            fld tbyte[rbx+rdi*4]
            sub rsp,8
            call :%down64
            fistp qword[rsp]
            call :%near64
            pop rdi
      @@:
  :!MemCopyIMA  -- exception here mapped to e24imcmafeh
        rep movsb
        ret
    []

--/*
procedure :%opMemSet(:%)
end procedure -- (for Edita/CtrlQ)
--*/
  :%opMemSet
------------
    [32]
        -- calling convention:
        --  mov edi,[p1]        -- dest addr    (opUnassigned)
        --  mov eax,[p2]        -- value (byte) (opUnassigned)
        --  mov ecx,[p3]        -- length       (opUnassigned)
        --  call :%opMemSet     -- mem_set(edi,eax,ecx)
        cmp eax,h4      -- value --DEV :%pLoadMint
        jl @f
            cmp byte[ebx+eax*4-1],0x12
            jne :e40atmsmba    -- arguments to mem_set must be atoms
            fld qword[ebx+eax*4]
            sub esp,8
            call :%down53
            fistp qword[esp]
            call :%near53
            mov eax,[esp]
            add esp,8
      @@:
        cmp ecx,h4      -- length --DEV :%pLoadMint
        jl @f
            cmp byte[ebx+ecx*4-1],0x12
            jne :e40atmsmba    -- arguments to mem_set must be atoms
            fld qword[ebx+ecx*4]
            sub esp,8
            call :%down53
            fistp qword[esp]
            call :%near53
            mov ecx,[esp]
            add esp,8
      @@:
        cmp ecx,0
        jl :e23imsl             -- invalid mem_set length

        cmp edi,h4      -- memory address --DEV :%pLoadMint
        jl @f
            cmp byte[ebx+edi*4-1],0x12
            jne :e40atmsmba     -- arguments to mem_set must be atoms
            fld qword[ebx+edi*4]
            sub esp,8
            call :%down53
            fistp qword[esp]
            call :%near53
            mov edi,[esp]
            add esp,8
      @@:
  :!MemSetIMA   -- exception here mapped to e25imsmafeh
        rep stosb
        ret
    [64]
        -- calling convention:
        --  mov rdi,[p1]        -- dest addr    (opUnassigned)
        --  mov rax,[p2]        -- value (byte) (opUnassigned)
        --  mov rcx,[p3]        -- length       (opUnassigned)
        --  call :%opMemSet     -- mem_set(rdi,rax,rcx)
        mov r15,h4
        cmp rax,r15     -- value
        jl @f
--DEV :%pLoadMint
            cmp byte[rbx+rax*4-1],0x12
            jne :e40atmsmba    -- arguments to mem_set must be atoms
            fld tbyte[rbx+rax*4]
            sub rsp,8
            call :%down64
            fistp qword[rsp]
            call :%near64
            pop rax
      @@:
        cmp rcx,r15     -- length
        jl @f
--DEV :%pLoadMint
            cmp byte[rbx+rcx*4-1],0x12
            jne :e40atmsmba    -- arguments to mem_set must be atoms
            fld tbyte[rbx+rcx*4]
            sub rsp,8
            call :%down64
            fistp qword[rsp]
            call :%near64
            pop rcx
      @@:
        cmp rcx,0
        jl :e23imsl             -- invalid mem_set length

        cmp rdi,r15     -- memory address
        jl @f
--DEV :%pLoadMint
            cmp byte[rbx+rdi*4-1],0x12
            jne :e40atmsmba     -- arguments to mem_set must be atoms
            fld tbyte[rbx+rdi*4]
            sub rsp,8
            call :%down64
            fistp qword[rsp]
            call :%near64
            pop rdi
      @@:
  :!MemSetIMA   -- exception here mapped to e25imsmafeh
        rep stosb
        ret
    []
      }

