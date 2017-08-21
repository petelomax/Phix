--
-- pJnotx.e
-- ========
--
--  implements :%opJnotx (if [not] s[idx] then)
--

include builtins\VM\pFPU.e  -- :%down53 etc

#ilASM{ jmp :%opRetf

    ::e04atssaa -- attempt to subscript an atom
    [32]
        pop edx
        mov al,4
        sub edx,1
    [64]
        pop rdx
        mov al,4
        sub rdx,1
    []
        jmp :!iDiag
        int3
    ::e03tfcmbaa
    [32]
        pop edx
        mov al,3        -- e03tfcmbaa
        sub edx,1
    [64]
        pop rdx
        mov al,3        -- e03tfcmbaa
        sub rdx,1
    []
        jmp :!iDiag
        int3

--/*
procedure :%opJnotx(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opJnotx       -- <flags> = test p2[p3]
-------------
    [32]
        --calling convention:
        --  mov edi,[p3]        -- idx
        --  mov esi,[p2]        -- s
        --  mov ecx,p3          -- var no of idx
        --  mov edx,p2          -- var no of s
        --  call opJnotx
        --eax=p2[p3], ebx=0, edx=length(p2), edi=normalised idx, esi=raw(p2)
        --  jz xxx              -- jnz for opJifx
        add edi,-1
        xor eax,eax
      :!opJnotxe92a             -- exception here mapped to e94vhnbaavedx
        mov al,[ebx+esi*4-1]    -- type byte
        mov edx,[ebx+esi*4-12]  -- length
        test al,0x80
        jz :e04atssaa
        cmp edi,edx
        jb @f                   -- unsigned jump, lets 0..len-1 through
            push eax
            mov al,4+1              -- [era] @ [esp+8] "reading from"
            call :%fixupIndex       -- idx-1 in edi, len in edx, not idx addr in ebx, al set
            pop eax
      @@:
        shl esi,2
        cmp al,0x82
        je :jnotxStr
            mov eax,[esi+edi*4]
            cmp eax,h4
            jl :jnotxend
            cmp byte[ebx+eax*4-1],0x12  -- (assumes floats are never 0.0)
            je :jnotxend
            jmp :e03tfcmbaa         -- true/false condition must be an ATOM [DEV untested]

      ::jnotxStr
            mov al,[esi+edi]
      ::jnotxend
        test eax,eax
        ret
    [64]
        --calling convention:
        --  mov rdi,[p3]        -- idx
        --  mov rsi,[p2]        -- s
        --  mov rcx,p3          -- var no of idx
        --  mov rdx,p2          -- var no of s
        --  call opJnotx
        --rax=p2[p3], rbx=0, rdx=length(p2), rdi=normalised idx, rsi=raw(p2)
        --  jz xxx              -- jnz for opJifx
        add rdi,-1
        mov r15,h4
        xor rax,rax
      :!opJnotxe92a             -- exception here mapped to e94vhnbaavedx
        mov al,[rbx+rsi*4-1]    -- type byte
        mov rdx,[rbx+rsi*4-24]  -- length
        test al,0x80
        jz :e04atssaa
        cmp rdi,rdx
        jb @f                   -- unsigned jump, lets 0..len-1 through
            push rax
            mov al,4+1              -- [era] @ [rsp+16] "reading from"
            call :%fixupIndex       -- idx-1 in edi, len in edx, not idx addr in ebx, al set
            pop rax
      @@:
        shl rsi,2
        cmp al,0x82
        je :jnotxStr
--          mov rax,[rsi+rdi*4]
            mov rax,[rsi+rdi*8]
            cmp rax,r15
            jl :jnotxend
            cmp byte[rbx+rax*4-1],0x12  -- (assumes floats are never 0.0)
            je :jnotxend
            jmp :e03tfcmbaa         -- true/false condition must be an ATOM [DEV untested]

      ::jnotxStr
            mov al,[rsi+rdi]
      ::jnotxend
        test rax,rax
        ret
    []
      }
