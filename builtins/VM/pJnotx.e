--
-- pJnotx.e
-- ========
--
--  implements :%opJnotx (if [not] s[idx] then)
--

include builtins\VM\pFPU.e  -- :%down53 etc

#ilASM{ jmp :%opRetf

--DEV FIXME: (and the :!bang labels below)
--  ::e94vhnbaavecx
--      int3
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
    :!e106ioobr
        int3
    ::e05sinaa0e10
    [32]
        pop edx
        mov al,5
        sub edx,1
    [64]
        pop rdx
        mov al,5
        sub rdx,1
    []
        jmp :!iDiag
        int3
    ::e106ioobrp1
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
--DEV use pFixup.e... (it was like this in p.asm too)
            add edi,1           -- (there was an add edi,-1 above)
            jl :opJnotxNegativeIdx
            -- but it might be a float:
--          cmp edi,h4
--          jl :e106ioobr           -- index out of bounds
----        opJnotxe92b:                    -- exception here mapped to e94vhnbaavecxfeh
--          cmp edi,h4
--          je :e94vhnbaavecx
--DEV :%pLoadMint
        :!opJnotxe92b                   -- exception here mapped to e94vhnbaavecxfeh
            cmp byte[ebx+edi*4-1],0x12
            jne :e05sinaa0e10           -- subscript is not an atom [era @ [esp]]
            fld qword[ebx+edi*4]
            call :%down53
            sub esp,8
            fistp qword[esp]
            call :%near53
            mov edi,[esp]
            add esp,8
            cmp edi,0
            jge :opJnotxNotNegative
          ::opJnotxNegativeIdx
                add edi,edx
                cmp edi,edx
                jb @f
                    sub edi,edx
                    jmp :!e106ioobr
          ::opJnotxNotNegative
            sub edi,1
            cmp edi,edx
            jae :e106ioobrp1      -- > length or still <=0
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
            add rdi,1           -- (there was an add rdi,-1 above)
            jl :opJnotxNegativeIdx
            -- but it might be a float:
--          cmp rdi,r15
--          jl :e106ioobr           -- index out of bounds
----        opJnotxe92b:                    -- exception here mapped to e94vhnbaavecxfeh [DEV merge these!]
--          cmp rdi,r15
--          je :e94vhnbaavecx
        :!opJnotxe92b                   -- exception here mapped to e94vhnbaavecxfeh
            cmp byte[rbx+rdi*4-1],0x12
            jne :e05sinaa0e10           -- subscript is not an atom [era @ [esp]]
            fld tbyte[rbx+rdi*4]
--DEV down64? (spotted in passing)
            call :%down53
            sub rsp,8
            fistp qword[rsp]
            call :%near64
            mov rdi,[rsp]
            add rsp,8
            cmp rdi,0
            jge :opJnotxNotNegative
          ::opJnotxNegativeIdx
                add rdi,rdx
                cmp rdi,rdx
                jb @f
                    sub rdi,rdx
                    jmp :!e106ioobr
          ::opJnotxNotNegative
            sub rdi,1
            cmp rdi,rdx
            jae :e106ioobrp1      -- > length or still <=0
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
