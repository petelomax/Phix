--
-- pUnary.e
-- ========
--
--  Implements :%opUminus, :%opNot, :%opNotBits, :%opFloor
--
--  Note that floor(a/b) is handled in opMath.e (opDivf[2] etc)
--

include VM\pHeap.e  -- :%pDealloc/:%pStoreFlt
include VM\pFPU.e   -- :%down53 etc

#ilASM{ jmp :%opRetf

        ::e1406sofa
            mov edi,6   -- sq_floor
            jmp @f
        ::e1407souma
            mov edi,7   -- sq_uminus
            jmp @f
        ::e1408sona
            mov edi,8   -- sq_not
            jmp @f
        ::e1412sonba
            mov edi,12  -- sq_not_bits
          [32]
            add esp,8
          [64]
            add rsp,8
          []
        @@:
            [32]
                pop edx
                mov al,14   -- e14soa(edi)
                sub edx,1
            [64]
                pop rdx
                mov al,14   -- e14soa(edi)
--              mov rdi,6   -- (floor->sq_floor)
                sub rdx,1
            []
                jmp :!iDiag
                int3
    [32]
        ::e111bolt32besp
            [32]
                add esp,8
                pop edx
                mov al,111  -- e111bolt32besp
                sub edx,1
            [64]
                add rsp,8   -- (8 not 16)
                pop rdx
                mov al,111  -- e111bolt32besp
                sub rdx,1
            []
                jmp :!iDiag
                int3
    []

--/*
procedure :%opUminus(:%)
end procedure -- (for Edita/CtrlQ)
--*/
  :%opUminus         -- [edi] = -ecx (integers and floats only)
------------
    [32]
        --calling convention:
        --  lea edi,[p1]        -- target
        --  mov ecx,[p2]        -- ref p2 (opUnassigned)
        --  call :%opUminus
        --    all registers trashed unless result is integer, else result in ecx [DEV? (as below)]

        cmp ecx,h4
        jge :opUminusN

        cmp ecx,0xC0000000
        je :opUminusToN 

        neg ecx                 -- eax=-eax (1 clock NP)

        mov edx,[edi]           -- previous value of target
        mov [edi],ecx
        cmp edx,h4
        jle @f
            sub dword[ebx+edx*4-8],1
            jz :%pDealloc
      @@:
        ret

      ::opUminusToN             -- special case: as integers are -1073741824..+1073741823,
        push ecx                --           -(-1073741824) needs to be stored as a float.
        fild dword[esp]         -- (obviously, we want it signed)
        pop ecx
        jmp opUminusfchs

      ::opUminusN
        cmp byte[ebx+ecx*4-1],0x12
        jne :e1407souma             -- sequence op(unary minus) attempted
        fld qword[ebx+ecx*4]
      ::opUminusfchs
        fchs    
        jmp :%pStoreFlt

    [64]
        --calling convention:
        --  lea rdi,[p1]        -- target
        --  mov rcx,[p2]        -- ref p2 (opUnassigned)
        --  call :%opUminus
        --    rax/rcx/rdx/rsi/rdi/r14/r15 trashed unless result is integer, left in rcx [DEV? (not true for storeFlt anyway)]

        mov r15,h4
        cmp rcx,r15
        jge :opUminusN

--      cmp rcx,0xC000000000000000
--      mov r14,#C000000000000000
        mov r14,#C0000000 --0000 0000
        shl r14,32
        cmp rcx,r14
        je opUminusToN  

        neg rcx                 -- eax=-eax (1 clock NP)

        mov rdx,[rdi]           -- previous value of target
        mov [rdi],rcx
        cmp rdx,r15
        jle @f
            sub qword[rbx+rdx*4-16],1
            jz :%pDealloc
      @@:
        ret

      ::opUminusToN             -- special case: as integers are -4611686018427387904..+4611686018427387903,
        push rcx                --                  -(-4611686018427387904) needs to be stored as a float.
        fild qword[rsp]         -- (obviously, we want it signed)
        pop rcx
        jmp opUminusfchs

      ::opUminusN
        cmp byte[rbx+rcx*4-1],0x12
        jne :e1407souma             -- sequence op(unary minus) attempted
        fld tbyte[rbx+rcx*4]
      ::opUminusfchs
        fchs    
        jmp :%pStoreFlt
    []

--/*
procedure :%opNot(:%)
end procedure -- (for Edita/CtrlQ)
--*/
  :%opNot       -- [edi] = not(ecx), ie sets [edi] to 0 or 1
---------       -- Damages eax,edx, plus pDealloc (ie all)
    [32]
        --calling convention:
        --  lea edi,[p1]    -- result location (->0/1)
        --  mov ecx,[p2]    -- ref p2 (opUnassigned)
        --  call :%opNot        -- [edi]=not(ecx)
        xor eax,eax         -- eax=0
        mov edx,[edi]       -- prev
        cmp ecx,h4
        jge :opNotN
        test ecx,ecx
        setz al             -- eax is now 1 or 0
      @@:
        mov [edi],eax       -- 0 or 1
        cmp edx,h4
        jle :opNotNoDealloc
            sub dword[ebx+edx*4-8],1
            jz :%pDealloc
      ::opNotNoDealloc
        ret

    ::opNotN
        cmp byte[ebx+ecx*4-1],0x12
        je @b           -- all floats non-0 so result is 0
        jmp :e1408sona  -- sequence op (not) attempted
    [64]
        --calling convention:
        --  lea rdi,[p1]    -- result location (->0/1)
        --  mov rcx,[p2]    -- ref p2 (opUnassigned)
        --  call :%opNot        -- [rdi]=not(rcx)
        xor rax,rax         -- rax=0
        mov rdx,[rdi]       -- prev
--      cmp rcx,h4
        mov r15,h4
        cmp rcx,r15
        jge :opNotN
        test rcx,rcx
        setz al             -- rax is now 1 or 0
      @@:
        mov [rdi],rax       -- 0 or 1
--      cmp rdx,h4
--      mov r15,h4
        cmp rdx,r15
        jle :opNotNoDealloc
            sub qword[rbx+rdx*4-16],1
            jz :%pDealloc
      ::opNotNoDealloc
        ret

    ::opNotN
        cmp byte[rbx+rcx*4-1],0x12
        je @b           -- all floats non-0 so result is 0
        jmp :e1408sona  -- sequence op (not) attempted
    []

--/*
procedure :%opNotBits(:%)
end procedure -- (for Edita/CtrlQ)
--*/
  :%opNotBits   -- [edi] = not_bits(eax)
---------       -- Damages all registers
    [32]
        --calling convention:
        --  lea edi,[p1]    -- result location (->0/1)
        --  mov eax,[p2]    -- ref p2 (opUnassigned)
        --  call :%opNotBits    -- [edi]=not_bits(eax)
        sub esp,8
        cmp eax,h4 --DEV :%pLoadMint
        jl @f
            cmp byte[ebx+eax*4-1],0x12
            jne :e1412sonba         -- sequence op (not_bits) attempted
            fld qword[ebx+eax*4]
            call :%down53
            fistp qword[esp]    -- store as 64 bits
            call :%near53
            mov eax,[esp]       -- load top 32
--          cdq                 -- sign extend eax into edx
--23/2/10:
--          cmp edx,[esp+4]     -- same as low 32?
            mov edx,[esp+4]
            test edx,edx
            jz @f
                add edx,1
                jnz e111bolt32besp      -- bitwise operations limited to 32 bits
      @@:

--      not eax
--DEV Agner Fog says: xor eax,-1
        xor eax,-1

        mov [esp],eax
        fild dword[esp]
        add esp,8
        jmp :%pStoreFlt     -- store result (invokes dealloc if needed)
    [64]
        --calling convention:
        --  lea rdi,[p1]    -- result location (->0/1)
        --  mov rax,[p2]    -- ref p2 (opUnassigned)
        --  call :%opNotBits    -- [rdi]=not_bits(rax)
        mov r15,h4
        sub rsp,8
        cmp rax,r15
        jl @f
--DEV %pLoadMint
            cmp byte[rbx+rax*4-1],0x12
            jne :e1412sonba         -- sequence op (not_bits) attempted
            fld tbyte[rbx+rax*4]
            call :%down64
            fistp qword[rsp]    -- store as 64 bits
            call :%near64
            mov rax,[rsp]
--          mov edx,[esp+4]
--          test edx,edx
--          jz @f
--              add edx,1
--              jnz e111bolt32besp      -- bitwise operations limited to 32 bits
      @@:

--      not rax
--DEV Agner Fog says: xor eax,-1
        xor rax,-1

        mov [rsp],rax
        fild qword[rsp]
        add rsp,8
        jmp :%pStoreFlt     -- store result (invokes dealloc if needed)
    []

--/*
procedure :%opFloor(:%)
end procedure -- (for Edita/CtrlQ)
--*/
  :%opFloor
-----------
    [32]
        --calling convention:
        --  lea edi,[p1]    -- result location
        --  mov eax,[p2]    -- ref p2 (opUnassigned)
        --  call :%opFloor  -- [edi]=floor(eax)
        --  all regs trashed, unless result is integer, in eax (and [p1]), esi,ecx also trashed
        mov edx,[edi]
        cmp eax,h4
        jl :opFloorStoreInt
        cmp byte[ebx+eax*4-1],0x12
        jne e1406sofa
        sub esp,8
        --
        -- Load a 64-bit float
        --
        fld qword[ebx+eax*4]
        call :%down64
        frndint
--DEV near64? (spotted in passing) [maybe the above have been down53?]
        call :%near53

        fld st0
        fistp qword[esp]        -- store as 64-bit int
        fild dword[esp]         -- and reload lower 32-bits
        mov ecx,[esp]
        fcomp st1               -- pop one
        add esp,8
        fnstsw ax
        sahf
        mov eax,ecx
        jne :%pStoreFlt         -- not 32-bit integer
        shl ecx,1
        jo :%pStoreFlt          -- result bigger than 31 bits
        --
        -- store as short int then
        --
        fstp st0                -- discard copy

      ::opFloorStoreInt
        mov [edi],eax
        cmp edx,h4
        jle @f
            sub dword[ebx+edx*4-8],1
            jz :%pDealloc
      @@:
        ret 

    [64]
        --calling convention:
        --  lea rdi,[p1]    -- result location
        --  mov rax,[p2]    -- ref p2 (opUnassigned)
        --  call :%opFloor  -- [rdi]=floor(rax)
        --  all regs trashed, unless result is integer, in rax (and [p1]), rsi,rcx also trashed
        mov r15,h4
        mov rdx,[rdi]
        cmp rax,r15
        jl :opFloorStoreInt
--DEV %pLoadMint
        cmp byte[rbx+rax*4-1],0x12
        jne e1406sofa
        sub rsp,8
        --
        -- Load an 80-bit float
        --
        fld tbyte[rbx+rax*4]
        call :%down64
        frndint             --DEV SLOW!
        call :%near64

        fld st0
        fistp qword[rsp]        -- store as 64-bit int
        fild qword[rsp]         -- and reload
        mov rcx,[rsp]
        fcomp st1               -- pop one
        add rsp,8
        fnstsw ax
        sahf
        mov rax,rcx
        jne :%pStoreFlt         -- not 64-bit integer
        shl rcx,1
        jo :%pStoreFlt          -- result bigger than 63 bits
        --
        -- store as short int then
        --
        fstp st0                -- discard copy

      ::opFloorStoreInt
        mov [rdi],rax
        cmp rdx,r15
        jle @f
            sub qword[rbx+rdx*4-16],1
            jz :%pDealloc
      @@:
        ret 
    []
      }

--global function not_bits(atom a)
--  return xor_bits(a,-1)
--end function

