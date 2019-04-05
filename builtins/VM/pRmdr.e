--
-- pRmdr.e
-- =======
--
--  Implements :%opRmdr
--

include VM\pHeap.e  -- :%pDealloc/:%pStoreFlt

#ilASM{ jmp :%opRetf

        ::e103atgrondb0esp
        [32]
            pop edx
            mov al,103  -- e103atgrondb0
            sub edx,1
        [64]
            pop rdx
            mov al,103  -- e103atgrondb0
            sub rdx,1
        []
            jmp :!iDiag
            int3
--DEV
        ::e1405sora
        [32]
            pop edx
            mov al,14   -- e14soa(edi)
            mov edi,5   -- remainder
            sub edx,1
        [64]
            pop rdx
            mov al,14   -- e14soa(edi)
            mov rdi,5   -- remainder
            sub rdx,1
        []
            jmp :!iDiag
            int3

--/*
procedure :%opRmdr(:%)
end procedure -- (for Edita/CtrlQ)
--*/
  :%opRmdr      -- [edi] := remainder(eax,ecx) (integers and floats only)
---------
    [32]
        --calling convention:
        --  lea edi,[p1]    -- address of target
        --  mov eax,[p2]    -- ref p2 (opUnassigned)
        --  mov ecx,[p3]    -- ref p3 (opUnassigned)
        --  call :%opRmdr
        cmp eax,h4
        jge :opRmdrN
        --
        -- eax [p2] is an int
        --
        cmp ecx,h4
        jge :opRmdrIN
        test ecx,ecx
        jz :e103atgrondb0esp    -- attempt to get remainder of a number divided by 0
--DEV should use cdq instead?
--      cdq                 -- <-- (add)
        mov edx,eax         -- <-- (remove)
        mov esi,[edi]
        sar edx,31          -- <-- (remove)
        idiv ecx
        mov [edi],edx
        mov edx,esi
        cmp esi,h4
        jle @f
            sub dword[ebx+edx*4-8],1
            jz :%pDealloc
      @@:
        ret

    ::opRmdrIN
        --
        -- eax/p2 is int, ecx/p3 is (must be) a float.
        --
        push eax
        shl ecx,2
        fild dword[esp]
        add esp,4
--    opRmdre92a:               -- exception here mapped to e92vhnbaavespm9feh
        cmp byte[ecx-1],0x12
        jne :e1405sora          -- sequence op (remainder) attempted
        fld qword[ecx]
        jmp :opRmdrToN

    ::opRmdrN
        --
        -- load p2 as float, then consider p3
        --
--    opRmdre92b:               -- exception here mapped to e92vhnbaavespm15feh
        cmp byte[ebx+eax*4-1],0x12
        jne :e1405sora          -- sequence op (remainder) attempted
        fld qword[ebx+eax*4]
        cmp ecx,h4
        jge :opRmdrNN
        test ecx,ecx
        jz :e103atgrondb0esp    -- attempt to get remainder of a number divided by 0
        push ecx
        fild dword[esp]
        add esp,4
        jmp :opRmdrToN

    ::opRmdrNN
        --
        -- st0/p2 is loaded, ecx/p3 is (must be) a float
        --
--    opRmdre92c:               -- exception here mapped to e92vhnbaavespm9feh
        cmp byte[ebx+ecx*4-1],0x12
        jne :e1405sora          -- sequence op (remainder) attempted
        fld qword[ebx+ecx*4]
    ::opRmdrToN
        fxch
      @@:
        fprem
        fnstsw ax           -- Status Word into AX
        sahf                -- copy to the CPU flags
        jpe @b              -- continue reducing if C2=PF=1 (reduction incomplete)

        fstp st1            -- overwrite the 2p with the ST(0) value and POP ST(0)

        jmp :%pStoreFlt
    [64]
        --calling convention:
        --  lea rdi,[p1]    -- address of target
        --  mov rax,[p2]    -- ref p2 (opUnassigned)
        --  mov rcx,[p3]    -- ref p3 (opUnassigned)
        --  call :%opRmdr
        mov r15,h4
        cmp rax,r15
        jge :opRmdrN
        --
        -- rax [p2] is an int
        --
        cmp rcx,r15
        jge :opRmdrIN
        test rcx,rcx
        jz :e103atgrondb0esp    -- attempt to get remainder of a number divided by 0
        mov rdx,rax
        mov rsi,[rdi]
        sar rdx,63
        idiv rcx
        mov [rdi],rdx
        mov rdx,rsi
        cmp rsi,r15
        jle @f
            sub qword[rbx+rdx*4-16],1
            jz :%pDealloc
      @@:
        ret

    ::opRmdrIN
        --
        -- rax/p2 is int, rcx/p3 is (must be) a float.
        --
        push rax
        shl rcx,2
        fild qword[rsp]
        add rsp,8
--    opRmdre92a:               -- exception here mapped to e92vhnbaavespm9feh
        cmp byte[rcx-1],0x12
        jne :e1405sora          -- sequence op (remainder) attempted
        fld tbyte[rcx]
        jmp :opRmdrToN

    ::opRmdrN
        --
        -- load p2 as float, then consider p3
        --
--    opRmdre92b:               -- exception here mapped to e92vhnbaavespm15feh
        cmp byte[rbx+rax*4-1],0x12
        jne :e1405sora          -- sequence op (remainder) attempted
        fld tbyte[rbx+rax*4]
        cmp rcx,r15
        jge :opRmdrNN
        test rcx,rcx
        jz :e103atgrondb0esp    -- attempt to get remainder of a number divided by 0
        push rcx
        fild qword[rsp]
        add rsp,8
        jmp :opRmdrToN

    ::opRmdrNN
        --
        -- st0/p2 is loaded, rcx/p3 is (must be) a float
        --
--    opRmdre92c:               -- exception here mapped to e92vhnbaavespm9feh
        cmp byte[rbx+rcx*4-1],0x12
        jne :e1405sora          -- sequence op (remainder) attempted
        fld tbyte[rbx+rcx*4]
    ::opRmdrToN
        fxch
      @@:
        fprem
        fnstsw ax           -- Status Word into AX
        sahf                -- copy to the CPU flags
        jpe @b              -- continue reducing if C2=PF=1 (reduction incomplete)

        fstp st1            -- overwrite the 2p with the ST(0) value and POP ST(0)

        jmp :%pStoreFlt
    []
      }
