--
-- pXor.e
-- ======
--
--  implements :%opXor  (a = b xor c)
--
--  Note: "and" and "or" are always short-circuited; opAnd and opOr simply do not exist at all.
--

include builtins\VM\pFPU.e  -- :%down53, :%near53

--#ilASM{ jmp :%opRetf
#ilASM{ jmp :!opCallOnceYeNot

--DEV FIXME: (and the :!bang labels below) [should be gone now everything opUnassigned]
    ::e1414soxa
        int3

--/*
procedure :%opXor(:%)
end procedure -- (for Edita/CtrlQ)
--*/
 :%opXor                -- [edi] := eax xor esi (always a 0/1 result)
--------                -- (LogicOp, for [edi] other than 0 or 1 see opXorBits)
    [32]
        --calling convention:
        --  lea edi,[dest]  -- addr dest
        --  mov eax,[p2]    -- ref p2
        --  mov esi,[p3]    -- ref p3
        --  mov ecx,p2      -- varno of p2
        --  mov edx,p3      -- varno of p3
        --  call :%opXor    -- [edi] = eax xor esi
        -- btw "and", "or" are always short-circuited: opAnd, opOr simply do not exist at all.

        cmp eax,h4  --DEV :%pLoadMint
        jl @f
      :!opXore92a                   -- exception here mapped to e94vhnbaavecx
            cmp byte[ebx+eax*4-1],0x12
            jne :e1414soxa
            fld qword[ebx+eax*4]
            call :%down53
            sub esp,8
            fistp qword[esp]
            call :%near53
            mov eax,[esp]
            add esp,8
      @@:
        cmp esi,h4  --DEV :%pLoadMint
        jl @f
      :!opXore92b                   -- exception here mapped to e94vhnbaavedx
            cmp byte[ebx+esi*4-1],0x12
            jne :e1414soxa
            fld qword[ebx+esi*4]
            call :%down53
            sub esp,8
            fistp qword[esp]
            call :%near53
            mov esi,[esp]
            add esp,8
      @@:
        test eax,eax
        jz @f
            mov eax,1 
      @@:
        test esi,esi
        jz @f
            mov esi,1
      @@:
        mov edx,[edi]               -- prev ref, if any
    --DEV tryme:
    --test eax,eax          --1
    --jz XorEaxZero         --1
    --   test esi,esi       --2
    --   jz XorBothZero     --2
    --  XorOneOnly:
    --   mov eax,1          --3
    --   jmp XorStore       --3
    --XorEaxZero:
    --   test esi,esi       --2
    --   jnz XorOneOnly     --2
    --  XorBothZero:
    --   xor eax,eax        --3
    --XorStore:
    --(always 3 vs 3..5 clocks as it now stands) [but xor is so rare this is hardly worth it...]

        xor eax,esi
        cmp edx,h4
        mov [edi],eax               -- always 1 or 0
        jle @f
            sub dword[ebx+edx*4-8],1
            jz :%pDealloc
      @@:
        ret
    [64]
        --calling convention:
        --  lea rdi,[dest]  -- addr dest
        --  mov rax,[p2]    -- ref p2
        --  mov rsi,[p3]    -- ref p3
        --  mov rcx,p2      -- varno of p2
        --  mov rdx,p3      -- varno of p3
        --  call :%opXor    -- [rdi] = rax xor rsi
        -- btw "and", "or" are always short-circuited: opAnd, opOr simply do not exist at all.
        mov r15,h4
        cmp rax,r15
        jl @f
--DEV %pLoadMint
      :!opXore92a                   -- exception here mapped to e91vhnbaavecx
            cmp byte[rbx+rax*4-1],0x12
            jne :e1414soxa
            fld tbyte[rbx+rax*4]
            call :%down64
            sub rsp,8
            fistp qword[rsp]
            call :%near64
            pop rax
      @@:
        cmp rsi,r15
        jl @f
--DEV %pLoadMint
      :!opXore92b                   -- exception here mapped to e94vhnbaavedx
            cmp byte[rbx+rsi*4-1],0x12
            jne :e1414soxa
            fld tbyte[rbx+rsi*4]
            call :%down64
            sub rsp,8
            fistp qword[rsp]
            call :%near64
            pop rsi
      @@:
        test rax,rax
        jz @f
            mov rax,1 
      @@:
        test rsi,rsi
        jz @f
            mov rsi,1
      @@:
        mov rdx,[rdi]               -- prev ref, if any
        xor rax,rsi
        cmp rdx,r15
        mov [rdi],rax               -- always 1 or 0
        jle @f
            sub qword[rbx+rdx*4-16],1
            jz :%pDealloc
      @@:
        ret
    []

      }
