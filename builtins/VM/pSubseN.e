--
-- pSubseN.e
-- =========
--
--  Various subscripting routines:
--
--      :%pSubse            -- res = ref[idx1][idx2]..[idxn]
--      :%pSubse1           -- [ecx]=esi[edi], aka p3=p1[p2]
--      :%pSubse1i          -- [ecx]=esi[edi], aka p3=p1[p2] when [ecx] is integer (no dealloc)
--      :%pSubse1ip         -- as opSubse1i when p1 is dword-sequence of integer, and p3 is integer
--      :%pSubse1is         -- eax=esi[edi], aka p3=p1[p2] when esi is string and result is integer
--
--  The latter four are optimised forms of :%pSubse, with exactly 1 index and other strict rules.
--  If porting, you could just (temporarily) implement :%pSubse and use it for everything, but 
--  that would require matching (temp) changes to psym.e, pmain.e, and pilx86.e.
--

include builtins\VM\pHeap.e     -- :%pDealloc

include builtins\VM\pFixup.e    -- negative and floating point index handling (:%fixupIndex)

bool ma_ip = false      -- Set true under with js during {a,b} = x operations, to catch any
                        -- attempts to part-modify strings, as in {s[i]} := {ch}, since the
                        -- otherwise excellent JavaScript desequencing will just not cope,
                        -- because strings are immutable in JavaScript. (For eg s[i] = ch
                        -- when not part of multiple assignment, it gets transpiled into
                        -- s=$repe(s,i,ch) which uses substring + fromCharCode + substring 
                        -- in order to get round the whole immutable strings thingymajig.)
                        -- (shadows the one in opRepeN.e via :%pDeSeqip2)

#ilASM{ jmp :%opRetf

--DEV FIXME: (and the :!bang labels below)
--  ::e04atsaa8
--      int3
--  ::e04atsaa0
--      int3
--  ::e110ecxesp
--      int3
    ::e93edx
    [32]
        mov edi,edx
        pop edx
        mov al,93       -- e93vhnbaav(edi)
        sub edx,1
    [64]
        mov rdi,rdx
        pop rdx
        mov al,93       -- e93vhnbaav(rdi)
        sub rdx,1
    []
        jmp :!iDiag
        int3

--Fine, tested:
    [32]
     ::e52jsdnssd
        pop edx
        xor eax,eax
        call :%pDeSeqip
        mov al,52           -- e52jsdnssd
        sub edx,1
--      mov [ma_ip],ebx
        jmp :!iDiag
        int3
    [64]
     ::e52jsdnssd
        pop rdx
        xor rax,rax
        call :%pDeSeqip
        mov al,52           -- e52jsdnssd
        sub rdx,1
--      mov [ma_ip],rbx
        jmp :!iDiag
        int3
    []

--/*
procedure :%pSubse(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pSubse    -- res = ref[idx1][idx2]..[idxn]
------------
    [32]
        -- calling convention (as hard-coded in pilx86.e):
        --  push <return address>
        --  push <addr res>                                 (or lea reg [ebp-NN], push reg)
        --  push [idxn]..[idx1]     (opUnassign'd)			(or push dword[m32 or ebp+d8/32])
        --  mov ecx,n
        --  mov edx,<addr ref>                              (or lea edx,[ebp+d8/32]
        --  jmp :%pSubse            (actually a call)
        --<return address>
        --  callee must ensure ref(edx) and all indexes are assigned beforehand (ie pmain.e 
        --  should emit any required opUnassigned instructions before the opSubse).
        --  if result is integer, left in eax, otherwise all registers clobbered.
        pop edi
        mov eax,[edx]
    ::opSubseWhile
        cmp eax,h4
--      jl :e04atsaa8           -- attempt to subscript an atom, era @ [esp+ecx*4]
        jg @f
--          mov e?dx,[esp+ecx*4+4]
--          mov e??,[esp+ecx*4]
--          lea esp,[esp+ecx*4+4]
            lea esp,[esp+ecx*4]
            je :e93edx
            pop edx
            mov al,4            -- e04atssaa
            sub edx,1
            jmp :!iDiag
            int3
      @@:
--DEV
--    :!opSubsee92a             -- exception here mapped to opSubsee92aedxfeh
        mov edx,[ebx+eax*4-12]  -- length
        lea edi,[edi-1]         -- decrement edi (oh, nice comment!! :-)
        cmp byte[ebx+eax*4-1],0x80
        lea eax,[ebx+eax*4]
        jne :opSubseStr
        cmp edi,edx
        jb @f                   -- unsigned jump, lets 0..len-1 through
                                --              (we just decremented edi)
            push eax
            add ecx,1
            mov al,8+4+1        -- [era] @ [esp+ecx*4], "reading from"
            call :%fixupIndex   -- idx-1 in edi, len in edx, (not: idx addr in ebx), al set
                                -- (we have opUnassign'd all idx that we need to, because
                                --  the new calling convention has no var addr, thus ebx 
                                --  would not be useful here anyway)
            sub ecx,1
            pop eax
      @@:
        mov eax,[eax+edi*4]
        pop edi
        sub ecx,1
        jnz :opSubseWhile

        mov edx,[edi]
        cmp eax,h4
        mov [edi],eax
        jl :opSubseEndWhile
            add dword[ebx+eax*4-8],1
      ::opSubseEndWhile
        cmp edx,h4
        jle @f
            sub dword[ebx+edx*4-8],1
            jz :%pDealloc
      @@:
        ret

      ::opSubseStr
        cmp ecx,1
--      jne :e04atsaa8          -- attempt to subscript an atom, era @ [esp+ecx*4]
        je @f
            mov edx,[esp+ecx*4]
            mov al,4            -- e04atssaa
            sub edx,1
            jmp :!iDiag
            int3
      @@:
        pop ecx
        cmp byte[eax-1],0x82
--      jne :e04atsaa8          -- attempt to subscript an atom, era @ [esp+ecx*4]
        je @f
            pop edx
            mov al,4            -- e04atssaa
            sub edx,1
            jmp :!iDiag
            int3
      @@:
        cmp edi,edx
        jb @f                   -- unsigned jump, lets 0..len-1 through
                                --               (we just decremented edi)
            push eax
            mov al,4+1          -- [era] @ [esp+8], "reading from"
            call :%fixupIndex   -- idx-1 in edi, len in edx, (not: idx addr in ebx), al set
                                -- (ditto note above)
            pop eax
       @@:
        cmp [ma_ip],ebx
        jne :e52jsdnssd
        add edi,eax
        xor eax,eax
        mov edx,[ecx]
        mov al,[edi]
        cmp edx,h4
        mov [ecx],eax
        jle @f
            sub dword[ebx+edx*4-8],1
            jz :%pDealloc
      @@:
        ret
    [64]
        -- calling convention (as hard-coded in pilx86.e):
        --  push <return address>
        --  push <addr res>                                 (or lea reg [rbp-NN], push reg)
        --  push [idxn]..[idx1]     (opUnassign'd)			(or push qword[m32 or rbp+d8/32])
        --  mov rcx,n
        --  mov rdx,<addr ref>                              (or lea rdx,[rbp+d8/32]
        --  jmp :%pSubse            (actually a call)
        --<return address>
        --  callee must ensure ref(rdx) and all indexes are assigned beforehand (ie pmain.e 
        --  should emit any required opUnassigned instructions before the opSubse).
        --  if result is integer, left in rax, otherwise all registers clobbered.
        pop rdi
        mov rax,[rdx]
        mov r15,h4
    ::opSubseWhile
--      cmp rax,h4
        cmp rax,r15
--      jl :e04atsaa8           -- attempt to subscript an atom, era @ [esp+ecx*4]
        jg @f
--          mov e?dx,[esp+ecx*4+4]
--          mov e??,[esp+ecx*4]
            lea rsp,[rsp+rcx*8]
            je :e93edx
            pop rdx
            mov al,4            -- e04atssaa
            sub rdx,1
            jmp :!iDiag
            int3
      @@:
--      mov rdx,[rbx+rax*4-12]  -- length
        mov rdx,[rbx+rax*4-24]  -- length
        lea rdi,[rdi-1]         -- decrement rdi (oh, nice comment!! :-)
        cmp byte[rbx+rax*4-1],0x80
        lea rax,[rbx+rax*4]
        jne :opSubseStr
        cmp rdi,rdx
        jb @f                   -- unsigned jump, lets 0..len-1 through
                                --              (we just decremented rdi)
            push rax
            add rcx,1
            mov al,8+4+1        -- [era] @ [rsp+rcx*8], "reading from"
            call :%fixupIndex   -- idx-1 in rdi, len in rdx, (not: idx addr in rbx), al set
                                -- (we have opUnassign'd all idx that we need to, because
                                --  the new calling convention has no var addr, thus ebx 
                                --  would not be useful here anyway)
            sub rcx,1
            pop rax
      @@:
        mov rax,[rax+rdi*8]
        pop rdi
        sub rcx,1
        jnz :opSubseWhile

        mov rdx,[rdi]
--      cmp rax,h4
--      mov r15,h4
        cmp rax,r15
        mov [rdi],rax
        jl :opSubseEndWhile
            add qword[rbx+rax*4-16],1
      ::opSubseEndWhile
--      cmp rdx,h4
        mov r15,h4
        cmp rdx,r15
        jle @f
            sub qword[rbx+rdx*4-16],1
            jz :%pDealloc
      @@:
        ret

      ::opSubseStr
        cmp rcx,1
--      jne :e04atsaa8          -- attempt to subscript an atom, era @ [rsp+rcx*8]
        je @f
            mov rdx,[rsp+rcx*8]
            mov al,4            -- e04atssaa
            sub rdx,1
            jmp :!iDiag
            int3
      @@:
        pop rcx
        cmp byte[rax-1],0x82
--      jne :e04atsaa8          -- attempt to subscript an atom, era @ [rsp+rcx*4]
        je @f
            pop rdx
            mov al,4            -- e04atssaa
            sub rdx,1
            jmp :!iDiag
            int3
      @@:
        cmp rdi,rdx
        jb @f                   -- unsigned jump, lets 0..len-1 through
                                --               (we just decremented rdi)
            push rax
            mov al,4+1          -- [era] @ [rsp+16], "reading from"
            call :%fixupIndex   -- idx-1 in rdi, len in rdx, (not: idx addr in rbx), al set
                                -- (ditto note above)
            pop rax
       @@:
        cmp [ma_ip],rbx
        jne :e52jsdnssd
        add rdi,rax
        xor rax,rax
        mov rdx,[rcx]
        mov al,[rdi]
--      cmp rdx,h4
        mov r15,h4
        cmp rdx,r15
        mov [rcx],rax
        jle @f
            sub qword[rbx+rdx*4-16],1
            jz :%pDealloc
      @@:
        ret
    []

--/*
procedure :%pSubse1(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pSubse1               -- [ecx]=esi[edi], aka p3=p1[p2]
-------------
    [32]
        -- calling convention (as hard-coded in pilx86.e):
        --  mov edi,[p2]        -- idx (opUnassigned)
        --  mov ecx,p3          -- addr res
        --  mov esi,[p1]        -- s
        --  mov edx,p1          -- var no of s              
        --  call opSubse1       -- [res]:=s[idx]
        sub edi,1
--DEV
--    :!opSubse1Re92a:          -- exception here mapped to e94vhnbaavedxesifeh
      :!opSubse1e04or92
        mov edx,[ebx+esi*4-12]  -- length
        mov ah,byte[ebx+esi*4-1]
        shl esi,2
        cmp edi,edx
        jb @f
            mov al,2+1              -- [era] @ [esp+4], "reading from"
            call :%fixupIndex       -- idx-1 in edi, len in edx, (not: idx addr in ebx), al set
      @@:
        cmp ah,0x80
        je :opSubse1Seq
        cmp ah,0x82
--      jne :e04atsaa0
        je @f
            pop edx
            mov al,4    -- e04atssaa
            sub edx,1
            jmp :!iDiag
            int3
      @@:
        cmp [ma_ip],ebx
        jne :e52jsdnssd
        xor eax,eax
        mov edx,[ecx]
        mov al,[esi+edi]
        jmp @f

      ::opSubse1Seq
        mov eax,[esi+edi*4]
        mov edx,[ecx]           -- prev(res)
        cmp eax,h4
        jl @f
            add dword[ebx+eax*4-8],1
      @@:
        cmp edx,h4
        jle @f
            mov edi,[ebx+edx*4-8]   -- decref in edi
            mov [ecx],eax
            sub edi,1
            jz :%pDealloc
            mov [ebx+edx*4-8],edi
            ret
      @@:
        mov [ecx],eax
        ret
    [64]
        -- calling convention (as hard-coded in pilx86.e):
        --  mov rdi,[p2]        -- idx (opUnassigned)
        --  mov rcx,p3          -- addr res
        --  mov rsi,[p1]        -- s
        --  mov rdx,p1          -- var no of s              
        --  call opSubse1       -- [res]:=s[idx]
        sub rdi,1
--      mov r15,h4
--DEV
--    :!opSubse1Re92a:          -- exception here mapped to e94vhnbaavedxesifeh
        mov rdx,[rbx+rsi*4-24]  -- length
        mov ah,byte[rbx+rsi*4-1]
        shl rsi,2
        cmp rdi,rdx
        jb @f
            mov al,2+1              -- [era] @ [rsp+8], "reading from"
            call :%fixupIndex       -- idx-1 in rdi, len in rdx, (not: idx addr in rbx), al set
      @@:
        cmp ah,0x80
        je :opSubse1Seq
        cmp ah,0x82
--      jne :e04atsaa0
        je @f
            pop rdx
            mov al,4    -- e04atssaa
            sub rdx,1
            jmp :!iDiag
            int3
      @@:
        cmp [ma_ip],rbx
        jne :e52jsdnssd
        xor rax,rax
        mov rdx,[rcx]
        mov al,[rsi+rdi]
        jmp @f
      ::opSubse1Seq
        mov rax,[rsi+rdi*8]
        mov rdx,[rcx]           -- prev(res)
--      cmp rax,h4
        mov r15,h4
        cmp rax,r15
        jl @f
            add qword[rbx+rax*4-16],1
      @@:
--      cmp rdx,h4
        cmp rdx,r15
        jle @f
            mov rdi,[rbx+rdx*4-16]  -- decref in rdi
            mov [rcx],rax
            sub rdi,1
            jz :%pDealloc
            mov [rbx+rdx*4-16],rdi
            ret
      @@:
        mov [rcx],rax
        ret
    []

--/*
procedure :%pSubse1i(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pSubse1i          -- [ecx]=esi[edi], aka p3=p1[p2] when [ecx] is integer (no dealloc)
--------------          -- esi must be sequence [not object], (DEV test that) need not be S_Init.
    [32]
        -- calling convention (as hard-coded in pilx86.e):
        --  mov edi,[p2]        -- idx (opUnassigned)
        --  mov esi,[p1]        -- s
        --  lea ecx,[res]       -- result addr
        --  mov edx,p1          -- var no of s              
        --  call opSubse1is     -- [ecx]:=s[idx]
        --  mov [p3],eax        -- res
        sub edi,1

      :!opSubse1iRe92a          -- exception here mapped to e94vhnbaavedx (edx contains var no, esi value. e94vhnbaav or e04atsaa)
        mov edx,[ebx+esi*4-12]  -- length
        mov al,byte[ebx+esi*4-1] -- type byte
        shl esi,2
        cmp edi,edx
        jb @f
            push eax
            mov al,4+1          -- [era] @ [esp+8], "reading from"
            call :%fixupIndex   -- idx-1 in edi, len in edx, (not: idx addr in ebx), al set
            pop eax
      @@:
        and eax,2       -- test for 0x82 and clear top 3 bytes!
        jne :opSubse1iCh
            mov eax,[esi+edi*4]
            cmp eax,h4
            jl @f
--          jmp :e110ecxesp -- type check error
            -- type check error (ecx is var addr)
--          mov edi,edx
            mov [ecx],eax
            pop edx
            mov al,110          -- e110tce(ecx)
            sub edx,1
            jmp :!iDiag
            int3

      ::opSubse1iCh
        cmp [ma_ip],ebx
        jne :e52jsdnssd
        mov al,[esi+edi]
    @@:
        mov [ecx],eax
        ret
    [64]
        -- calling convention (as hard-coded in pilx86.e):
        --  mov rdi,[p2]        -- idx (opUnassigned)
        --  mov rsi,[p1]        -- s
        --  lea rcx,[res]       -- result addr
        --  mov rdx,p1          -- var no of s              
        --  call opSubse1is     -- [rcx]:=s[idx]
        --  mov [p3],rax        -- res  [DEV?]
        sub rdi,1
      :!opSubse1iRe92a          -- exception here mapped to e94vhnbaavedxesifeh (edx contains var no, esi value. e94vhnbaav or e04atsaa)
        mov rdx,[rbx+rsi*4-24]  -- length
        mov al,byte[rbx+rsi*4-1] -- type byte
        shl rsi,2
        cmp rdi,rdx
        jb @f
            push rax
            mov al,4+1          -- [era] @ [rsp+16], "reading from"
            call :%fixupIndex   -- idx-1 in rdi, len in rdx, (not: idx addr in rbx), al set
            pop rax
      @@:
        and rax,2       -- test for 0x82 and clear top 3 bytes!
        jne :opSubse1iCh
            mov rax,[rsi+rdi*8]
--          cmp rax,h4
            mov r15,h4
            cmp rax,r15
            jl @f
--          jmp :e110ecxesp -- type check error
            -- type check error (rcx is var addr)
--          mov rdi,rdx
            mov [rcx],rax
            pop rdx
            mov al,110      -- e110tce(ecx)
            sub rdx,1
            jmp :!iDiag
            int3

      ::opSubse1iCh
        cmp [ma_ip],rbx
        jne :e52jsdnssd
        mov al,[rsi+rdi]
    @@:
        mov [rcx],rax
        ret
    []

--/*
procedure :%pSubse1ip(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pSubse1ip             -- as opSubse1i when p1 is sequence of integer, and p3 is integer
---------------             -- p1/esi must be sequence [not string/atom], need not be S_Init.
    [32]
        -- calling convention (as hard-coded in pilx86.e):
        --  mov edi,[p2]        -- idx (opUnassigned)
        --  mov esi,[p1]        -- s
        --  mov edx,p1          -- var no of s              
        --  call opSubse1ip     -- eax:=s[idx]
        --  mov [p3],eax        -- store res
        sub edi,1
      :!opSubse1ipRe92a         -- exception here mapped to e94vhnbaavedxesifeh
        mov edx,[ebx+esi*4-12]  -- length
        shl esi,2
        cmp edi,edx
        jb @f
            mov al,2+1              -- [era] @ [esp+4], "reading from"
            call :%fixupIndex       -- idx-1 in edi, len in edx, (not: idx addr in ebx), al set
      @@:
        mov eax,[esi+edi*4]
        ret
    [64]
        -- calling convention (as hard-coded in pilx86.e):
        --  mov rdi,[p2]        -- idx (opUnassigned)
        --  mov rsi,[p1]        -- s
        --  mov rdx,p1          -- var no of s              
        --  call opSubse1ip     -- rax:=s[idx]
        --  mov [p3],rax        -- store res
        sub rdi,1
      :!opSubse1ipRe92a         -- exception here mapped to e94vhnbaavedxesifeh
        mov rdx,[rbx+rsi*4-24]  -- length
        shl rsi,2
        cmp rdi,rdx
        jb @f
            mov al,2+1              -- [era] @ [rsp+8], "reading from"
            call :%fixupIndex       -- idx-1 in rdi, len in rdx, (not: idx addr in rbx), al set
      @@:
        mov rax,[rsi+rdi*8]
        ret
    []

--/*
procedure :%pSubse1is(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pSubse1is             -- eax=esi[edi], aka p3=p1[p2] when esi is string and result is integer
---------------
    [32]
        -- calling convention (as hard-coded in pilx86.e):
        --  mov edi,[p2]        -- idx (opUnassigned)
        --  mov esi,[p1]        -- s
        --  mov edx,p1          -- var no of s
        --  call opSubse1is     -- eax:=s[idx]
        --  mov [p3],eax        -- store res
        sub edi,1
        xor eax,eax
--DEV
      :!opSubse1isRe92a         -- exception here mapped to e94vhnbaavedxesifeh
        mov edx,[ebx+esi*4-12]  -- length
        shl esi,2
        cmp edi,edx
        jb @f
            mov al,2+1          -- [era] @ [esp+4], "reading from"
            call :%fixupIndex   -- idx-1 in edi, len in edx, (not idx addr in ebx), al set
      @@:
        cmp [ma_ip],ebx
        jne :e52jsdnssd
        mov al,[esi+edi]
        ret
    [64]
        -- calling convention (as hard-coded in pilx86.e):
        --  mov rdi,[p2]        -- idx (opUnassigned)
        --  mov rsi,[p1]        -- s
        --  mov rdx,p1          -- var no of s
        --  call opSubse1is     -- rax:=s[idx]
        --  mov [p3],rax        -- store res
        sub rdi,1
        xor rax,rax
      :!opSubse1isRe92a         -- exception here mapped to e94vhnbaavedxesifeh
        mov rdx,[rbx+rsi*4-24]  -- length
        shl rsi,2
        cmp rdi,rdx
        jb @f
            mov al,2+1          -- [era] @ [rsp+8], "reading from"
            call :%fixupIndex   -- idx-1 in rdi, len in rdx, (not idx addr in rbx), al set
      @@:
        cmp [ma_ip],rbx
        jne :e52jsdnssd
        mov al,[rsi+rdi]
        ret
    []

    :%pDeSeqip2         -- [ma_ip]:=e/rax. A multiple assigment (aka desequence/destructure)
---------------         --                 operation is in progress under with js therefore
                        --                 string subscript (/replacements) are now illegal,
                        --                 that is at least when [ma_ip] is non-zero.
                        --                  (Called from pRepeN.e)
    [32]
        mov [ma_ip],eax
    [64]
        mov [ma_ip],rax
    []
        ret

      }

