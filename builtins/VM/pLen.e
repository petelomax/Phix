--
-- pLen.e
-- ======
--
--  implements :%opLen
--
--  Note that l = length(s) is inlined if s is known to be an assigned sequence (so
--       there is no need to check for unassigned) and l is an integer (so it does 
--       not need a decref/dealloc), saving the overhead of both the call/return,
--       and those aforementioned unnecessary checks.
--

#ilASM{ jmp :%opRetf

    :%opLen
-----------
    [32]
        --calling convention:
        --  lea edi,[p1]    -- result location
        --  mov esi,[p2]    -- source ref
        --  mov edx,p2      -- var no of ref
        --  call :%opLen    -- [edi]=length(eax)
        cmp esi,h4
        jg @f
            mov ecx,edx
            jne e36loaaind
            pop edx
            mov al,91   -- e91vhnbaav(ecx)
            sub edx,1
            jmp :!iDiag
            int3
      @@:
      :!opLene36or92
        test byte[ebx+esi*4-1],0x80     -- all strings/sequences have bit #80 set...
        jnz @f
          ::e36loaaind
            pop edx
            mov al,36   -- e36loaaind
            sub edx,1
            jmp :!iDiag
            int3
      @@:
        mov edx,[edi]                   -- prev value of target
        mov ecx,[ebx+esi*4-12]          -- get length
        cmp edx,h4
        mov [edi],ecx
        jle @f
            sub dword[ebx+edx*4-8],1
            jz :%pDealloc
      @@:
        ret
    [64]
        --calling convention:
        --  lea rdi,[p1]    -- result location
        --  mov rsi,[p2]    -- source ref
        --  mov rdx,p2      -- var no of ref
        --  call :%opLen    -- [rdi]=length(rax)
        mov r15,h4
        cmp rsi,r15
        jg @f
            mov rcx,rdx
            jne :e36loaaind
            pop rdx
            mov al,91   -- e91vhnbaav(rcx)
            sub rdx,1
            jmp :!iDiag
            int3
      @@:
      :!opLene36or92
        test byte[rbx+rsi*4-1],0x80     -- all strings/sequences have bit #80 set...
        jnz @f
          ::e36loaaind
            pop rdx
            sub rdx,1
            mov al,36   -- e36loaaind
            jmp :!iDiag
            int3
      @@:
        mov rdx,[rdi]                   -- prev value of target
        mov rcx,[rbx+rsi*4-24]          -- get length
        cmp rdx,r15
        mov [rdi],rcx
        jle @f
            sub qword[rbx+rdx*4-16],1
            jz :%pDealloc
      @@:
        ret
    []
      }
