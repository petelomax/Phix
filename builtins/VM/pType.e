--
-- pType.e
-- =======
--
--  implements :%opInt, :%opAtom, :%opStr, :%opSq, and :%opObj, aka [edi]:=<type>(eax)
--
--  NB: opSeq, implemented via :%opScmp in pJcc.e, is "dest = (p1=p2)", whereas opSq as 
--      defined below is "dest = sequence(p1)", and obviously they must not be confused.
--
--  The vast majority of builtin type tests are implemented via opJtyp, which uses
--  :%opInt0/Atom0/Sq0/Str0(Obj0? DEV) as defined in pJcc.e rather than these.
--
--  If the variable is known to be assigned, and the target is an integer, pilx86.e
--  inlines these functions. In practice these routines are almost never used, however
--  they need to be part of the VM so that pDiagN.e can test exception addresses.
--

#ilASM{ jmp :!opCallOnceYeNot
--#ilASM{ jmp :%opRetf

--/*
procedure ::opInt(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opInt                         -- [e/rdi] = integer(e/rax)
-----------
        [32]
            -- calling convention
            --  lea edi,[dest]
            --  mov eax,[src]       -- ref of src
            --  mov ecx,src         -- var no of src
            --  call opInt          -- [edi]:=integer(eax)
--          xor ebx,ebx
            cmp eax,h4
            mov edx,[edi]
            jle :ediI1
          ::edi0
            mov [edi],ebx
          ::opIntContinue
            cmp edx,h4
            jle @f
                sub dword[ebx+edx*4-8],1
                jz :%pDealloc
          @@:
            ret
          ::ediI1
            mov dword[edi],1
            jne :opIntContinue          -- strongly taken (matches above ret, if that helps any?)
--;         jmp e92vhnbaavespm9
--          jmp :e94vhnbaavecx
            mov esi,ecx
            mov edx,[esp]
            mov al,92       -- e92vhnbaav(esi)
            sub edx,1
            jmp :!iDiag
            int3
        [64]
            -- calling convention
            --  lea rdi,[dest]
            --  mov rax,[src]       -- ref of src
            --  mov rcx,src         -- var no of src
            --  call opInt          -- [rdi]:=integer(rax)
            mov r15,h4
            mov rdx,[rdi]
            cmp rax,r15
            jle :ediI1
          ::edi0
            mov [rdi],rbx
          ::opIntContinue
            cmp rdx,r15
            jle @f
                sub qword[rbx+rdx*4-16],1
                jz :%pDealloc
          @@:
            ret
          ::ediI1
            mov qword[rdi],1
            jne :opIntContinue          -- strongly taken (matches above ret, if that helps any?)
            mov rsi,rcx
            mov rdx,[rsp]
            mov al,92       -- e92vhnbaav(esi)
            sub rdx,1
            jmp :!iDiag
            int3
        []

--/*
procedure ::opAtom(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opAtom                        -- [e/rdi]=atom(e/rax)
------------
        [32]
            -- calling convention
            --  lea edi,[dest]
            --  mov eax,[src]       -- ref of src
            --  mov ecx,src         -- var no of src
            --  call opAtom         -- [edi]:=atom(eax)
--          xor ebx,ebx
            cmp eax,h4
            mov edx,[edi]
            jl :edi1
          :!opAtome92                   -- exception mapped to e94vhnbaavecxfeh [DEV]
            cmp byte[ebx+eax*4-1],0x12
          ::opAtomContinue
            jne :edi0
          ::edi1
            mov dword[edi],1
            cmp edx,h4
            jle @f
                sub dword[ebx+edx*4-8],1
                jz :%pDealloc
          @@:
            ret
        [64]
            -- calling convention
            --  lea rdi,[dest]
            --  mov rax,[src]       -- ref of src
            --  mov rcx,src         -- var no of src
            --  call opAtom         -- [rdi]:=atom(rax)
            mov r15,h4
            mov rdx,[rdi]
            cmp rax,r15
            jl :edi1
          :!opAtome92                   -- exception mapped to e94vhnbaavecxfeh [DEV]
            cmp byte[rbx+rax*4-1],0x12
          ::opAtomContinue
            jne :edi0
          ::edi1
            mov qword[rdi],1
            cmp rdx,r15
            jle @f
                sub qword[rbx+rdx*4-16],1
                jz :%pDealloc
          @@:
            ret
        []

--/*
procedure ::opStr(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opStr                         -- [e/rdi]=string(e/rax)
-----------
        [32]
            -- calling convention
            --  lea edi,[dest]
            --  mov eax,[src]       -- ref of src
            --  mov ecx,src         -- var no of src
            --  call opStr          -- [edi]:=string(eax)
--          xor ebx,ebx
            cmp eax,h4
            mov edx,[edi]
            jl :edi0
          :!opStre92                -- exception mapped to e94vhnbaavecxfeh [DEV]
            cmp byte[ebx+eax*4-1],0x82
            jmp :opAtomContinue
--          jne :edi0
--          mov dword[edi],1
--          cmp edx,h4
--          jle @f
--              sub dword[ebx+edx*4-8],1
--              jz :%pDealloc
--        @@:
--          ret
        [64]
            -- calling convention
            --  lea rdi,[dest]
            --  mov rax,[src]       -- ref of src
            --  mov rcx,src         -- var no of src
            --  call opStr          -- [rdi]:=string(rax)
            mov r15,h4
            mov rdx,[rdi]
            cmp rax,r15
            jl :edi0
          :!opStre92                -- exception mapped to e94vhnbaavecxfeh [DEV]
            cmp byte[rbx+rax*4-1],0x82
            jmp :opAtomContinue
        []

--/*
procedure ::opSq(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opSq                          -- [e/rdi]=sequence(e/rax)  (NB: opSeq is dest=(p1=p2))
----------
        [32]
            -- calling convention
            --  lea edi,[dest]
            --  mov eax,[src]       -- ref of src
            --  mov ecx,src         -- var no of src
            --  call opSq           -- [edi]:=sequence(eax)
--          xor ebx,ebx
            cmp eax,h4
            mov edx,[edi]
            jl :edi0
          :!opSqe92                     -- exception mapped to e94vhnbaavecxfeh [DEV]
            test byte[ebx+eax*4-1],0x80
            jmp :opAtomContinue
--  jz edi0
--  mov dword[edi],1
--  cmp edx,h4
--  jle @f
--      sub dword[ebx+edx*4-8],1
--      jz :%pDealloc
--  @@:
--  ret
        [64]
            -- calling convention
            --  lea rdi,[dest]
            --  mov rax,[src]       -- ref of src
            --  mov rcx,src         -- var no of src
            --  call opSq           -- [rdi]:=sequence(rax)
            mov r15,h4
            mov rdx,[rdi]
            cmp rax,r15
            jl :edi0
          :!opSqe92                     -- exception mapped to e94vhnbaavecxfeh [DEV]
            test byte[rbx+rax*4-1],0x80
            jmp :opAtomContinue
        []

--/*
procedure ::opObj(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opObj                         -- [e/rdi] = object(e/rax)
----------
        [32]
            -- calling convention
            --  lea edi,[dest]
            --  mov eax,[src]       -- ref of src
            --  (there is no need for ecx = var no of src here, no e94)
            --  call opObj          -- [edi]:=object(eax)
--          xor ebx,ebx
            cmp eax,h4
            mov edx,[edi]
        [64]
            -- calling convention
            --  lea rdi,[dest]
            --  mov rax,[src]       -- ref of src
            --  (there is no need for rcx = var no of src here, no e94)
            --  call opObj          -- [rdi]:=object(rax)
            mov r15,h4
            mov rdx,[rdi]
            cmp rax,r15
        []
            je :edi0
            jmp :edi1
    }
