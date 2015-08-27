--
-- pMkSqN.e
-- ========
--  Make Sequence opcode
--

include builtins\VM\pHeap.e     -- (needed for :%pDealloc, :%pAllocSeq)

#ilASM{ jmp :%opRetf

--/*
procedure :%pMkSq(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pMkSq
-----------
        -- calling convention:
        --  mov r/edx,N
        --  push <return addr>
        --  push <dest addr>
        --  push [eN]..[e1]     -- pre-incref'd
        --  mov r/edi,[dest]    -- prev dest ref
        --  jmp :%pMkSq
        --<return addr>:
    [32]
        mov ecx,edx
        mov edx,[esp+edx*4+4]   -- era
        call :%pAllocSeq        -- damages eax only (needs edx set)
        mov edx,edi             -- for later dealloc
        lea edi,[ebx+eax*4]
        mov esi,esp
        lea esp,[esp+ecx*4]
        rep movsd
        pop esi                 -- dest addr
        cmp edx,h4
        jle @f
            mov ecx,[ebx+edx*4-8]   -- refcount
            mov [esi],eax
            sub ecx,1
            jz :%pDealloc
            mov [ebx+edx*4-8],ecx   -- refcount
            ret
      @@:
        mov [esi],eax
    [64]
--DEV check this zero-fills on X64: [YEP] (and what regs pAllocSeq(X64) damages)
--lea rcx,[rbx-1] -- (set rcx to -1 for the following test)
--mov ecx,edx
        mov rcx,rdx
--sub rsp,8*5? (if N is odd/even may need an aligning push/pop on the call)
        mov rdx,[rsp+rdx*8+8]   -- era
        call :%pAllocSeq        -- damages eax[DEV?] only (needs edx set)
-- (damages rax/r8/r9/r10/r11/r13/r15 [at least])
        mov rdx,rdi             -- for later dealloc
        lea rdi,[rbx+rax*4]     -- (yes that is a *4, ref->raw)
        mov rsi,rsp
        lea rsp,[rsp+rcx*8]
        rep movsq
        mov r15,h4
        pop rsi                 -- dest addr
--      cmp rdx,h4
        cmp rdx,r15
        jle @f
            mov rcx,[rbx+rdx*4-16]  -- refcount
            mov [rsi],rax
            sub rcx,1
            jz :%pDealloc
            mov [rbx+rdx*4-16],rcx  -- refcount
            ret
      @@:
        mov [rsi],rax
    []
        ret
      }

