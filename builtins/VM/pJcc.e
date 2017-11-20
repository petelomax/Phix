--
-- pJcc.e
-- ======
--
--  implements :%opJcc and :%opJccE, also [DEV] :%opFind and :%opMatch
--
--  Obviously, if the compiler knows a and b are integer, it will just emit a "cmp", 
--  but if they can be any type, that means comparing ints/floats/strings/sequences 
--  which is a 4x4 or 16-way task. Things are a little simpler when we only need to 
--  know eq/ne, as floats/ints are never the same (Phix always stores eg 3.0 as 3) 
--  and we can cop out early if the lengths do not match. The first two (internal) 
--  routines, compareSeq and equalSeq, deal with nested sequences, and strings, but 
--  are also quite happy when given two floats.
--

#ilASM{ jmp :%opRetf

--DEV FIXME: (and the :!bang labels below)
--  ::e94vhnbaavesi
--      int3
--  ::e94vhnbaavedx
--      int3
--DEV duplicate of the one in pJnotx
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
procedure ::compareSeq(::)
end procedure -- (for Edita/CtrlQ)
--*/
    [32]
      ::compareSeq
------------------
        -- general compare routine, extended from equalSeq.
        -- use when other than eq or ne (ie lt,le,gt,ge) rqd.
        -- on entry, we have two refs to compare, one in eax,
        --  the other in [edi-4]. (NB this must NOT be called
        --  with short int params or anything unassigned)
        -- Result is the condition code flags
        --  (unlike equalSeq which is just the Z flag)
        -- Assume all registers will be damaged.
        mov edi,[edi-4]
     ::compareSeqR                  -- entry point with refs in eax and edi.
        lea esi,[ebx+eax*4]
        shl edi,2
--DEV tryme once everything working (avoids an AGI):
--  mov al,[ebx+eax*4-1]
        mov al,[esi-1]
        cmp edi,esi
        je :compareSeqRet
        mov ah,[edi-1]
        cmp al,0x12
        jne :compareStrX
        cmp ah,0x12
        jne :compareSeqRet          -- sequences > atoms
        -- two floats. First, a direct binary compare allows nan compare:
--DEV this may be the wrong thing to do entirely
        mov eax,[edi]
        mov ecx,[edi+4]
        cmp eax,[esi]
        jne @f
            cmp ecx,[esi+4]
            je :compareSeqRet
      @@:
        fld qword[edi]
        fld qword[esi]
        fcompp
        fnstsw ax
        mov ch,ah
--  and ah,0x7f
--  shl ch,7
--  or ah,ch
        mov dh,ah
        and ah,0x7f     -- clear sign
        and dh,0x04     -- keep parity only
        shl ch,7
        shl dh,4        -- parity to Z position
        or ah,ch
        sub ah,dh       -- Clear Z if Parity set
        sahf
      ::compareSeqRet
        ret

      ::compareStrX

        mov ecx,[esi-12]        -- length
        test ah,0x80            -- type[edi]
--  jz compareSeqRet0       -- DEV some variation mey be needed here. cmp esi,edi springs to mind...
--  compareSeqRet0:
        jnz @f
            xor eax,eax
            add eax,1       -- ensure Z=0   [DEV try test esi,esi, since that cannot be zero here]
            ret
      @@:
        mov edx,[edi-12]        -- length

        cmp al,0x82             -- type[esi]
        jne :compareSeqX
        -- .. so esi is str
        cmp ah,0x82             -- type[edi]
        jne :compareStrSeq
        -- .. and so is edi
        add ecx,1           ---- include trailing null byte [or hit the edx<ecx case first]
        repe cmpsb
        ret

      ::compareStrSeq
        -- .. esi is str, edi is seq
        push ecx
        push edx
        cmp ecx,edx
        jle @f
            mov ecx,edx
      @@:
        add ecx,1           -- as sub ecx,1 at top of loop
        xor eax,eax         -- for lodsb
      ::compareStrVsSeqCharLoop
        sub ecx,1
        jz :compareSeqCompareLengths
        lodsb       -- mov al,[esi], esi+=1
        cmp eax,[edi]
        lea edi,[edi+4]
        je :compareStrVsSeqCharLoop
--  lea esp,[esp+8]
        pop edx
        pop ecx
        ret

      ::compareSeqX
        -- .. so esi is seq
        push ecx
        push edx
        cmp ecx,edx
        jle @f
            mov ecx,edx
      @@:
        cmp ah,0x82                 -- type[edi]
        jne :compareSeqSeq
        -- .. and edi is str
        xchg esi,edi
--  jmp :compareStrSeq
        add ecx,1           -- as sub ecx,1 at top of loop
        xor eax,eax         -- for lodsb
      ::compareSeqVsStrCharLoop
        sub ecx,1
        jz :compareSeqCompareLengths
        lodsb       -- mov al,[esi], esi+=1
--  cmp eax,[edi]
        cmp [edi],eax
        lea edi,[edi+4]
        je :compareSeqVsStrCharLoop
--  lea esp,[esp+8]
        pop edx
        pop ecx
        ret

      ::compareSeqSeq
        -- esi and edi are sequences..
        -- we *must* deal with any short ints here...
        or ecx,ecx
        jz :compareSeqCompareLengths
      @@:
        mov edx,[edi]
        lodsd                -- next element of p3 (mov eax,[esi], esi+=4)
        lea edi,[edi+4]
        cmp eax,edx
        je :compareSeqSeqNxt
        cmp eax,h4
--25/9/17
--      jl :compareSeqReset
        jl :compareSeqp3iint
        cmp edx,h4
--25/9/17
--      jl :compareSeqReset
        jl :compareSeqp2iint
        pushad
        call :compareSeq
        popad
        jnz :compareSeqPop2Ret
      ::compareSeqSeqNxt
        sub ecx,1
        jnz @b
        nop
      ::compareSeqCompareLengths
        pop edx
        pop ecx
        cmp ecx,edx
        ret

--25/9/17:
-- BUGFIX: ?sort({{0},{-3.8}}) gave completely the wrong results.
-- In compare({a},{b}) the nested compare of a,b was not handling
-- integers vs floats correctly; if {a,b} was {int,float} or
-- {float,int} it would simply assume int<float. The same bug was
-- present for infix <, <=, >, >=, though = and != were fine.
      ::compareSeqp3iint
        -- p3[i] (eax) is an integer
        -- p2[i] (edx) is of unknown type
        cmp edx,h4
        jl :compareSeqReset
        cmp byte[ebx+edx*4-1],0x12
        jne :compareSeqReset    -- all ints (eax<h4) deemed less than sequences (edi>h4)
        -- p2(edx) flt, p3(eax) int:
        push eax
        fld qword[ebx+edx*4]
        fild dword[esp]
        jmp @f
        
      ::compareSeqp2iint
        -- p2[i] (edx) is an integer
        -- p3[i] (eax) is not, but could be a float
        cmp byte[ebx+eax*4-1],0x12
        jne :compareSeqReset    -- all ints (eax<h4) deemed less than sequences (edi>h4)
        -- p2(edx) int, p3(eax) flt:
        push edx
        fild dword[esp]
        fld qword[ebx+eax*4]
      @@:
        add esp,4
        fcompp
        fnstsw ax
        mov ch,ah
        and ah,0x7f     -- clear sign
        shl ch,7
        or ah,ch
        sahf
        jmp :compareSeqPop2Ret

      ::compareSeqReset
--25/9/17 BUG!! (as per Jccp2Intp3Ref...)
        cmp eax,edx
      ::compareSeqPop2Ret
--  lea esp,[esp+8]
        pop edx
        pop ecx
        ret


--/*
procedure ::equalSeq(::)
end procedure -- (for Edita/CtrlQ)
--*/
     ::equalSeq -- general equality routine.
--------------- -- used as the nested part of opFind and opMatch.
                -- on entry, we have two refs to compare, one in eax,
                --  the other in [edi-4]. (NB this must NOT be called
                --  with short int params) Result is Z flag.
                -- Assume all registers will be damaged
        mov edi,[edi-4]
     ::equalSeqR    -- entry point with refs in eax and edi.
        lea esi,[ebx+eax*4]
        shl edi,2
        mov al,[esi-1]
        cmp edi,esi
        je :equalSeqRet
        cmp al,0x12
        jne :equalStrX
        cmp byte[edi-1],0x12
        jne :equalSeqRet
        -- (direct binary compare allows scan for nan)
--DEV this may be the wrong thing to do entirely, see above
        mov eax,[edi]
        mov ecx,[edi+4]
        cmp eax,[esi]
        jne @f
            cmp ecx,[esi+4]
--je :compareSeqRet -- not needed, removed 21/1/09
      @@:
--  fld qword[edi]
--  fld qword[esi]
--  fcompp
--  fnstsw ax
--DEV:
--  mov ch,ah
----    and ah,0x7f
----    shl ch,7
----    or ah,ch
--mov dh,ah
--  and ah,0x7f     -- clear sign
--and dh,0x04   -- keep parity only
--  shl ch,7
--shl dh,4      -- parity to Z position
--  or ah,ch
--sub ah,dh     -- Clear Z if Parity set
--  sahf
        ret

      ::equalStrX
        test al,0x80
        jz :equalSeqRet0
        mov dl,[edi-1]                -- type of 2nd param
        mov ecx,[esi-12]        -- length
        test dl,0x80
        jz :equalSeqRet0
        cmp ecx,[edi-12]
        jne :equalSeqRet
        test ecx,ecx
        jz :equalSeqRet

        cmp al,0x82
        jne :equalSeqX
        -- .. so esi is str
        cmp dl,0x82
        jne :equalStrSeq
        -- .. and so is edi
        repe cmpsb
        ret

      ::equalStrSeq
        -- .. esi is str, edi is seq
        add ecx,1       -- to get Z flag right at end
        xor eax,eax    
      ::equalStrVsSeqCharLoop
        sub ecx,1
        jz :equalSeqRet
        lodsb       -- mov al,[esi], esi+=1
        cmp eax,[edi]
        lea edi,[edi+4]
        je :equalStrVsSeqCharLoop
        nop
        ret

      ::equalSeqX
        cmp al,0x80
        jne :equalSeqRet
        -- .. so esi is seq
        cmp dl,0x82
        jne :equalSeqSeq
        -- .. and edi is str
        xchg esi,edi
        jmp :equalStrSeq

      ::equalSeqSeq
        -- esi and edi are sequences, of same length..
        -- we *must* deal with any short ints here...
      @@:
        mov edx,[edi]
        lodsd                -- next element of p3 (mov eax,[esi], esi+=4)
        lea edi,[edi+4]
        cmp eax,edx
        je :equalSeqSeqNxt
        cmp eax,h4
        jl :equalSeqRet
        cmp edx,h4
        jl :equalSeqRet
        pushad
        call :equalSeq
        popad
        jnz :equalSeqRet
      ::equalSeqSeqNxt
        sub ecx,1
        jnz @b
        nop
        ret

      ::equalSeqRet0
        --DEV esi and edi are non-zero here, try test esi,esi
        xor eax,eax
        add eax,1       -- ensure Z=0
      ::equalSeqRet
        ret

--/*
procedure ::compareSeq64(::)
end procedure -- (for Edita/CtrlQ)
--*/
    [64]
        ::compareSeq64
----------------------
        -- general compare routine.
        -- use when other than eq or ne (ie lt,le,gt,ge) rqd.
        -- on entry, we have two refs to compare, one in rax,
        --  the other in [rdi-8]. (NB this must NOT be called
        --  with short int params or anything unassigned)
        -- Result is the condition code flags
        --  (unlike equalSeq which is just the Z flag)
        -- Assume all registers will be damaged. [DEV currently it is rax/rcx/rdx/rsi/rdi only, r8 on unused]
        mov rdi,[rdi-8]
     ::compareSeq64R                    -- entry point with refs in rax and rdi.
        lea rsi,[rbx+rax*4]
        shl rdi,2
--DEV tryme once everything working (avoids an AGI):
--  mov al,[rbx+rax*4-1]
        mov al,[rsi-1]
        cmp rdi,rsi
        je :compareSeq64Ret
        mov ah,[rdi-1]
        cmp al,0x12
        jne :compareStr64X
        cmp ah,0x12
        jne :compareSeq64Ret            -- sequences > atoms
        -- two floats. First, a direct binary compare allows nan compare:
--DEV this may be the wrong thing to do entirely
        mov rax,[rdi]
        mov cx,[rdi+8]
        cmp rax,[rsi]
        jne @f
            cmp cx,[rsi+8]
            je :compareSeq64Ret
      @@:
        fld tbyte[rdi]
        fld tbyte[rsi]
        fcompp
        fnstsw ax
        mov ch,ah
        mov dh,ah
        and ah,0x7f     -- clear sign
        and dh,0x04     -- keep parity only
        shl ch,7
        shl dh,4        -- parity to Z position
        or ah,ch
        sub ah,dh       -- Clear Z if Parity set
        sahf
      ::compareSeq64Ret
        ret

      ::compareStr64X

        mov rcx,[rsi-24]        -- length
        test ah,0x80            -- type[rdi]
--  compareSeq64Ret0:
        jnz @f
            test rsi,rsi    -- ensure Z=0
            ret
      @@:
        mov rdx,[rdi-24]        -- length

        cmp al,0x82             -- type[rsi]
        jne :compareSeq64X
        -- .. so rsi is str
        cmp ah,0x82             -- type[rdi]
        jne :compareStrSeq64
        -- .. and so is rdi
        add rcx,1           ---- include trailing null byte [or hit the rdx<rcx case first]
        repe cmpsb
        ret

      ::compareStrSeq64
        -- .. rsi is str, rdi is seq
        push rcx
        push rdx
        cmp rcx,rdx
        jle @f
            mov rcx,rdx
      @@:
        add rcx,1           -- as sub rcx,1 at top of loop
        xor rax,rax         -- for lodsb
      ::compareStrVsSeq64CharLoop
        sub rcx,1
        jz :compareSeq64CompareLengths
        lodsb       -- mov al,[rsi], rsi+=1
        cmp rax,[rdi]
        lea rdi,[rdi+8]
        je :compareStrVsSeq64CharLoop
--  lea rsp,[rsp+16]
        pop rdx
        pop rcx
        ret

      ::compareSeq64X
        -- .. so rsi is seq
        push rcx
        push rdx
        cmp rcx,rdx
        jle @f
            mov rcx,rdx
      @@:
        cmp ah,0x82                 -- type[rdi]
        jne :compareSeq64Seq
        -- .. and rdi is str
        xchg rsi,rdi
--  jmp :compareStrSeq64
        add rcx,1           -- as sub ecx,1 at top of loop
        xor rax,rax         -- for lodsb
      ::compareSeq64VsStrCharLoop
        sub rcx,1
        jz :compareSeq64CompareLengths
        lodsb       -- mov al,[rsi], rsi+=1
--      cmp rax,[rdi]
        cmp [rdi],rax
        lea rdi,[rdi+8]
        je :compareSeq64VsStrCharLoop
--  lea rsp,[rsp+16]
        pop rdx
        pop rcx
        ret

      ::compareSeq64Seq
        -- rsi and rdi are sequences..
        -- we *must* deal with any short ints here...
        test rcx,rcx
        jz :compareSeq64CompareLengths
        mov r15,h4
      @@:
        mov rdx,[rdi]
        lodsq                -- next element of p3 (mov rax,[rsi], rsi+=8)
        lea rdi,[rdi+8]
        cmp rax,rdx
        je :compareSeq64SeqNxt
--      cmp rax,h4
        cmp rax,r15
--25/9/17
--      jl :compareSeq64Reset
        jl :compareSeq64p3iint
--      cmp rdx,h4
        cmp rdx,r15
--25/9/17
--      jl :compareSeq64Reset
        jl :compareSeq64p2iint
--      pushad
push rsi
push rdi
push rcx
        call :compareSeq64
pop rcx
pop rdi
pop rsi
--      popad
        jnz :compareSeq64Pop2Ret
      ::compareSeq64SeqNxt
        sub rcx,1
        jnz @b
        nop
      ::compareSeq64CompareLengths
        pop rdx
        pop rcx
        cmp rcx,rdx
        ret

--25/9/17:
      ::compareSeq64p3iint
        -- p3[i] (rax) is an integer
        -- p2[i] (rdx) is of unknown type
        cmp rdx,r15
        jl :compareSeq64Reset
        cmp byte[rbx+rdx*4-1],0x12
        jne :compareSeq64Reset  -- all ints (rax<h4) deemed less than sequences (rdi>h4)
        -- p2(rdx) flt, p3(rax) int:
        push rax
        fld tbyte[ebx+edx*4]
        fild qword[rsp]
        jmp @f
        
      ::compareSeq64p2iint
        -- p2[i] (rdx) is an integer
        -- p3[i] (rax) is not, but could be a float
        cmp byte[rbx+rax*4-1],0x12
        jne :compareSeq64Reset  -- all ints (rax<h4) deemed less than sequences (rdi>h4)
        -- p2(rdx) int, p3(rax) flt:
        push rdx
        fild qword[rsp]
        fld tbyte[rbx+rax*4]
      @@:
        add rsp,8
        fcompp
        fnstsw ax
        mov ch,ah
        and ah,0x7f     -- clear sign
        shl ch,7
        or ah,ch
        sahf
        jmp :compareSeq64Pop2Ret

      ::compareSeq64Reset
        cmp rax,rdx
      ::compareSeq64Pop2Ret
--  lea rsp,[rsp+16]
        pop rdx
        pop rcx
        ret

--/*
procedure ::equalSeq64(::)
end procedure -- (for Edita/CtrlQ)
--*/
     ::equalSeq64 -- nested part of opFind and opMatch...
----------------- -- on entry, we have two refs to compare, one in rax,
                  --    the other in [rdi-8]. (NB this must NOT be called
                  --    with short int params) Result is Z flag.
                  -- Damages rax,rcx,rdx,rsi,rdi
        mov rdi,[rdi-8]
     ::equalSeq64R  -- entry point with refs in rax and rdi.
        lea rsi,[rbx+rax*4]
        shl rdi,2
--DEV (see compare, avoid AGI)
        mov al,[rsi-1]
        cmp rdi,rsi
        je :equalSeq64Ret
        cmp al,0x12
        jne :equalStrX64
        cmp byte[rdi-1],0x12
        jne :equalSeq64Ret
        -- (direct binary compare allows scan for nan)
--DEV this may be the wrong thing to do entirely, see above
        mov rax,[rdi]
        mov cx,[rdi+8]
        cmp rax,[rsi]
        jne @f
            cmp cx,[rsi+8]
      @@:
        ret

      ::equalStrX64
        test al,0x80
        jz :equalSeq64Ret0
        mov dl,[rdi-1]              -- type of 2nd param
        mov rcx,[rsi-24]            -- length
        test dl,0x80
        jz :equalSeq64Ret0
        cmp rcx,[rdi-24]
        jne :equalSeq64Ret
        test rcx,rcx
        jz :equalSeq64Ret

        cmp al,0x82
        jne :equalSeq64X
        -- .. so rsi is str
        cmp dl,0x82
        jne :equalStrSeq64
        -- .. and so is rdi
        repe cmpsb
        ret

      ::equalStrSeq64
        -- .. rsi is str, rdi is seq
        add rcx,1       -- to get Z flag right at end
        xor rax,rax    
      ::equalStrVsSeq64CharLoop
        sub rcx,1
        jz :equalSeq64Ret
        lodsb       -- mov al,[rsi], rsi+=1
        cmp rax,[rdi]
        lea rdi,[rdi+8]
        je :equalStrVsSeq64CharLoop
        nop
        ret

      ::equalSeq64X
        cmp al,0x80
        jne :equalSeq64Ret
        -- .. so rsi is seq
        cmp dl,0x82
        jne :equalSeq64Seq
        -- .. and rdi is str
        xchg rsi,rdi
        jmp :equalStrSeq64

      ::equalSeq64Seq
        -- rsi and rdi are sequences, of same length..
        -- we *must* deal with any short ints here...
        mov r15,h4
      @@:
        mov rdx,[rdi]
        lodsq                -- next element of p3 (mov rax,[rsi], rsi+=8)
--      lea rdi,[rdi+4]
        lea rdi,[rdi+8]
        cmp rax,rdx
        je :equalSeq64SeqNxt
--      cmp rax,h4
        cmp rax,r15
        jl :equalSeq64Ret
--      cmp rdx,h4
        cmp rdx,r15
        jl :equalSeq64Ret
--      pushad
push rsi
push rdi
push rcx
        call :equalSeq64
pop rcx
pop rdi
pop rsi
--      popad
        jnz :equalSeq64Ret
      ::equalSeq64SeqNxt
        sub rcx,1
        jnz @b
        nop
        ret

      ::equalSeq64Ret0
        -- (rsi is guaranteed non-0 here)
        test rsi,rsi    -- ensure Z=0
      ::equalSeq64Ret
        ret
    []

--/*    (temp, see pFind.e/pMatch.e for now...)
--new 29/6/10:
--calling convention:                               octal:         binary:          code:
--  mov ecx,p1      -- result location              271         B9 imm32        mov ecx,imm32
--  mov esi,imm32   -- start from (known/1)         276         BE imm32        mov esi,imm32
--> or
--  mov esi,[p4]    -- start from (var)             213 065     8B 35 m32       mov esi,[m32]
--  mov eax,[p2]    -- ref of p2                    241         A1 m32          mov eax,[m32]
--  mov edi,[p3]    -- ref of p3                    213 075     8B 3D m32       mov edi,[m32]
--  mov ebx,p3      -- var no of p3
--  mov edx,p2      -- var no of p2
--  call :%opFind   -- [ecx] = find(eax,edi)        350         E8 rel32        call rel32
--      (if p4 is unassigned just get "invalid find start index")

 :%opFind
--  mov edi,[edx]           -- ref of p3
--  mov eax,[esi]           -- ref of p2
    cmp edi,h4
    jle :opFindErr          -- p3(ebx) unassigned or e112saofmbs
    shl edi,2
    push ecx                --[1] push target addr
--stack:
?!  mov [p2vno],edx
    test byte[edi-1],0x80
    jz :opFindErrPop1e112   -- second argument of find() must be a sequence
    xor ebx,ebx
    mov edx,[edi-12]        -- length
sub esi,1
jz :opFind1
cmp esi,edx                 -- if start_from is -ve/float/oob then longhand
--jb @f
jbe @f
    add esi,1
    jl :opFindfixupNegativeStart
    -- but it might be a float:
    cmp esi,h4
    jle :e21ifsi                -- 0 or >length+1
    cmp byte[ebx+esi*4-1],0x12
    jne :e21ifsi                -- invalid find start index
    fld qword[ebx+esi*4]
--see pSubse.e(?)
    fldcw [down53]
--stack
    fistp qword[FltWrk]
    fldcw [near53]
    mov esi,[FltWrk]
    cmp esi,0
    jge :opFindfixupStartNotNegative
  ::opFindfixupNegativeStart
        add esi,edx
----DEV try add esi,1
        jmp :opFindNegDone
--      xor ebx,ebx
--      cmp esi,edx
--?     ja :e10sspeosediedxN    -- slice starts past end of sequence (%d > %d)
--      ret
  ::opFindfixupStartNotNegative
    sub esi,1
  ::opFindNegDone
    xor ebx,ebx
    cmp esi,edx
    ja :e21ifsi                 -- invalid find start index
--ret
--=====

--      pop eax
  @@:
    jz :opFindNotFound

  ::opFind1
    cmp eax,h4
    jge :opFindRef
    cmp byte[edi-1],0x82
    mov ecx,edx
    je :opFindStrChr
    --
    -- good, looking for short int, in a sequence (not string).
    --
--  repne scasd         -- cmp eax,[edi], edi+=4
--  jnz :opFindNotFound
    test edx,edx
    jz :opFindNotFound
sub ecx,esi
lea edi,[edi+esi*4]
  @@:
    mov esi,[edi]
    lea edi,[edi+4]
    cmp eax,esi
    je :FindIntFound
    sub ecx,1
    jnz @b
    xor edx,edx
    jmp @f

  ::FindIntFound
    sub ecx,1

    sub edx,ecx
@@:
    pop edi             -- target addr
    mov ecx,edx
  ::FindResult
    mov edx,[edi]
    mov [edi],ecx
    cmp edx,h4
    jle @f
----DEV temp: (removed 14/10/09)
--1/1/2013 makes no sense...
--opFindE92: ---- exception here mapped to e30espfeh
        sub dword[ebx+edx*4-8],1
        jz :%pDealloc
  @@:
    ret

  ::opFindStrChr
sub ecx,esi
lea edi,[edi+esi]
    --
    -- searching a string (p3)
    --
    test eax,0xFFFFFF00
    jne :opFindNotFound         -- must be 8-bit char (and not float/str/seq)
    repne scasb
    jnz :opFindNotFound
    sub edx,ecx
    pop edi                     -- target addr
    mov ecx,edx
    jmp :FindResult

  ::opFindFltNotFound
--DEV fstp st0
--  fistp [FltWrk]              -- discard st0
    fstp st0                    -- discard st0

  ::opFindNotFound
    xor ecx,ecx
    pop edi                     -- target addr
    jmp :FindResult

  ::opFindRef
--  jz :opFinde92espm4m15       -- p2=eax=[[esp]-15]=h4
    jz :opFinde94espm4p2vno     -- era @ [esp+4], p2=[pvnvo]=h4
    shl eax,2
--BUG 2/4/08: This assumed p3 is sequence!
    cmp byte[edi-1],0x80
    jne :opFindNotFound         -- you can't find a ref of any kind in a string!
mov ecx,esi
shl esi,2
add esi,edi
    cmp byte[eax-1],0x12
    jne :opFindSeqOrStr
    --
    -- search for a float then
    --
    fld qword[eax]
    fstp qword[FltWrk]  -- store as 64-bit float/pop (2 cycle)
    mov edi,[FltWrk]
--  fld qword[ebx+eax*4]
--  xor ecx,ecx
--xor ebx,ebx
  @@:
    add ecx,1
    cmp ecx,edx
    jg :opFindFltNotFound
    lodsd   -- mov eax,[esi]-- esi+=4
    cmp eax,h4
    jl @b
    cmp byte[ebx+eax*4-1],0x12
    jne @b
-- direct binary compare allows scan for nan
    cmp edi,[ebx+eax*4]     -- first dword of FltWrk already in edi (for speed ---)
    jne @b
--stack:
    mov edi,[FltWrk+4]
    cmp edi,[ebx+eax*4+4]
    mov edi,[FltWrk]
    jne @b
--  fld qword[ebx+eax*4]
--  fcomp
--  fnstsw ax
--mov dh,ah
--  and ah,0x7f     -- clear sign
--and dh,0x04   -- keep parity only
--  shl ch,7
--shl dh,4      -- parity to Z position
--  or ah,ch
--sub ah,dh
--  sahf
--  jne @b
    pop edi                     -- target addr
--DEV fstp st0
--  fistp [FltWrk]              -- discard st0
    fstp st0                    -- discard st0
    jmp :FindResult

  ::opFindSeqOrStr
    mov edi,[eax-12]            -- length
--DEV move ecx,[eax-20] push ecx
    push eax                    -- [1] save start of string p2 on stack
    push edi                    -- [2] save length "" on stack
--  xor ecx,ecx
    cmp byte[eax-1],0x82
    jne :opFindSeq
    --
    -- searching for a string then...
    --
--xor ebx,ebx
  @@:
    add ecx,1
    cmp ecx,edx
    jg :opFindStrNotFound
    lodsd       -- mov eax,[esi], esi+=4
    cmp eax,h4                  -- skip short ints
    jl @b
    cmp byte[ebx+eax*4-1],0x82
    jne :opFindStrNotString
    --
    -- good, p2 is string, p3[i] is string
    --
    cmp edi,[ebx+eax*4-12]      -- compare lengths
    jne @b
    push esi                    --[3] save subscript element position on stack
    push ecx                    --[4] and index
    lea esi,[eax*4]
    mov ecx,edi
--DEV save the base then!
    mov edi,[esp+12]            -- [1] start of string being searched for
    repe cmpsb
    jz :opStringFound
    pop ecx                     --[4]
    pop esi                     --[3]
    mov edi,[esp]               -- [2]
    jmp @b

  ::opStringFound
    pop ecx                     --[4]
    add esp,12                  --[3..1]
    pop edi                     -- target addr
    jmp :FindResult
  ::opFindStrNotFound
    add esp,8                   --[2..1]
    jmp :opFindNotFound

  ::opFindStrNotString
    cmp byte[ebx+eax*4-1],0x80  -- skip any floats
    jne @b
    cmp edi,[ebx+eax*4-12]      -- compare lengths
    jne @b
    --
    -- p2 is string, p3[i] is sequence, with same lengths.
    -- compare each char with p3 elements as shortints.
    --
    push esi                    -- [3]save subscript element position on stack
    push ecx                    -- [4]and index
--DEV save the base then!
    mov esi,[esp+12]            --[1] start of string being searched for
    mov ecx,edi
    lea edi,[ebx+eax*4]         -- start of p3[i]
    xor eax,eax
  ::opFindStrVsSeqCharLoop
    sub ecx,1
    jl :opStringFound
    lodsb       -- mov al,[esi], esi+=1
    cmp eax,[edi]
    lea edi,[edi+4]
    je :opFindStrVsSeqCharLoop
    pop ecx                     --[4]
    pop esi                     --[3]
    mov edi,[esp]               -- [2]
    jmp @b

  ::opFindSeq
    --
    -- searching for a sequence then...
    --
    -- Scan through the toplevel, ignoring integers and floats, and
    -- sequences/strings of the wrong length. If we find a string,
    -- then compare char-for-short-int here, with the catch that if
    -- we stumble on a non-char in p2, jump to below where we also
    -- skip any further strings in p3. For sequence p3[i], compare
    -- shortints here and pass off any (both) non-ints to the
    -- recursive equalSeq function.
    --
--  cmp byte[eax-1],0x80            -- should always be sequence here
--  jne ???                         -- (just did >h4, not 0x12 and not 0x82)
-- Next four instructions already done:
--  mov edi,[eax-12]            -- length
--  push eax                        -- [1]save start of p2 (a sequence) on stack
--  push edi                        -- [2]save length of p2 on stack
--  xor ecx,ecx
  @@:
    add ecx,1
    cmp ecx,edx
    jg :opFindStrNotFound
    lodsd                           -- next element of p3 (mov eax,[esi], esi+=4)
    cmp eax,h4                      -- skip shortints
    jl @b
-- opFindodbfu7:
    cmp byte[ebx+eax*4-1],0x82
-- opFindodbfu7a:
    jne :opFindSeqNotString
    -- so eax(=p3[ecx]) is a string:
    cmp edi,[ebx+eax*4-12]          -- compare lengths
    jne @b
    --
    -- <this sentence repeated from above:>
    -- compare each char of (string) p3[i] with (sequence) p2[i] as shortint,
    -- with the catch that if p2[i] is non-char, resume in lower loop which 
    -- also skips any further strings in p3.
    --
    push esi                        -- [3]save subscript element position on stack
    push ecx                        -- [4]and index
    lea esi,[ebx+eax*4]             -- start of string p3[i]
    mov ecx,edi
    mov edi,[esp+12]                -- start of sequence being searched for
    xor eax,eax
  ::opFindSeqVsStrCharLoop
    sub ecx,1
    jl :opStringFound
    lodsb                           -- mov al,[esi], esi+=1
    cmp eax,[edi]
    lea edi,[edi+4]
    je :opFindSeqVsStrCharLoop
    mov eax,[edi-4]
    pop ecx                         --[4]
    pop esi                         --[3]
    test eax,0xFFFFFF00
    jne :opFindSeqSkipFurtherStrings
    mov edi,[esp]
    jmp @b

  ::opFindSeqNotString
    cmp byte[ebx+eax*4-1],0x80      -- skip floats
    jne @b
    -- so eax(=p3[ecx]) is a sequence, and so is p2:
    cmp edi,[ebx+eax*4-12]          -- compare lengths
    jne @b
    --
    -- <this sentence repeated from above:>
    -- compare sequence(p2) with sequence(p3[i]) then...
    -- handle shortints here, pass off any (both) non-ints to
    -- the recursive equalSeq function
    --
    push esi                        -- [3]save subscript element position on stack
    push ecx                        -- [4]and index
    lea esi,[ebx+eax*4]             -- start of sequence p3[i]
    mov ecx,edi
--DEV save the base then!
    mov edi,[esp+12]                -- [1] start of sequence being searched for
  ::opFindSeqVsSeqLoop
    sub ecx,1
    jl :opStringFound
    lodsd       -- mov eax,[esi], esi+=4
    cmp eax,[edi]
    lea edi,[edi+4]
    je :opFindSeqVsSeqLoop
    cmp eax,h4
    jl :opFSSmismatch
    cmp dword[edi-4],h4
    jl :opFSSmismatch
pushad
    call :equalSeq          -- compare ref eax with ref at [edi-4], recursively
popad                       -- result is Z flag
    je :opFindSeqVsSeqLoop
  ::opFSSmismatch
    pop ecx                 --[4]
    pop esi                 --[3]
    mov edi,[esp]
    jmp @b


  ::opFindSeqSkipFurtherStrings
    mov edi,[esp]
  @@:
    add ecx,1
    cmp ecx,edx
    jg :opFindStrNotFound
    lodsd                   -- next element of p3 (mov eax,[esi], esi+=4)
    cmp eax,h4
    jl @b
    cmp byte[ebx+eax*4-1],0x80
    jne @b
    cmp edi,[ebx+eax*4-12]
    jne @b
    --
    -- compare sequence(p2) with sequence(p3[i]) then...
    -- handle shortints here, pass off any (both) non-ints to
    -- the recursive equalSeq function
    --
    push esi                        -- [3]save subscript element position on stack
    push ecx                        -- [4]and index
    lea esi,[ebx+eax*4]             -- start of sequence p3[i]
    mov ecx,edi
--DEV save the base then!
    mov edi,[esp+12]                -- [1] start of sequence being searched for
  ::opFindSeqVsSeqLoop2
    sub ecx,1
    jl :opStringFound
    lodsd       -- mov eax,[esi], esi+=4
    cmp eax,[edi]
    lea edi,[edi+4]
    je :opFindSeqVsSeqLoop2
    cmp eax,h4
    jl :opFSSmismatch2
    cmp dword[edi-4],h4
    jl :opFSSmismatch2
pushad
    call :equalSeq          -- compare ref eax with ref at [edi-4], recursively
popad                       -- result is Z flag
    je :opFindSeqVsSeqLoop2
  ::opFSSmismatch2
    pop ecx                 --[4]
    pop esi                 --[3]
    mov edi,[esp]
    jmp @b

::FindMap
--  jz opMatchErre92esi
--  jz opMatche92espm15
    jz :opMatche92edx
--  cmp edi,h4
--  jle opMatchErr          -- unassignededx or e113saommbs
--DEV temp, until calling convention of opMatch updated to be same as opFind:
--(error handling will be wrong till then)
--mov esi,1
    cmp byte[ebx+edi*4-1],0x80
    jae :opFind
--  mov ebx,[p2vno] -- not needed!
    jmp :opMatchErr         -- e113saommbs

 :%opMatch
--calling convention: (== opFind)                   octal:         binary:          code:
--  mov edx,p3                                      272         BA imm32        mov edx,imm32
--  mov esi,p2                                      276         BE imm32        mov esi,imm32
--  mov ecx,p1                                      271         B9 imm32        mov ecx,imm32
--  call opMatch    -- p1 = match(p2,p3)            350         E8 rel32        call rel32
--new 29/6/10:
--calling convention:                               octal:         binary:          code:
--  mov ecx,p1      -- result location              271         B9 imm32        mov ecx,imm32
--  mov esi,imm32   -- start from (known/1)         276         BE imm32        mov esi,imm32
--> or
--  mov esi,[p4]    -- start from (var)             213 065     8B 35 m32       mov esi,[m32]
--  mov eax,[p2]    -- ref of p2                    241         A1 m32          mov eax,[m32]
--  mov edi,[p3]    -- ref of p3                    213 075     8B 3D m32       mov edi,[m32]
--  mov ebx,p3      -- var no of p3
--  mov edx,p2      -- var no of p2
--  call opMatch    -- p1 = match(p2,p3,esi)        350         E8 rel32        call rel32
--      (if p4 is unassigned just get "invalid match start index")

--  mov edi,[edx]           -- ref of p3
--  mov eax,[esi]           -- ref of p2
    cmp edi,h4
    jle :opMatchErr         -- p3(ebx) unassigned or e113saommbs
--stack:
    mov [p2vno],ebx
--  push ebx                -- p2vno
    xor ebx,ebx
    cmp eax,h4
    jle :FindMap            -- treat integer(p2) as find() [after some more error checks] [deviation from RDS]
--  shl eax,2
    shl edi,2
    test byte[ebx+eax*4-1],0x80
    jz :%opFind             -- atom(p2) -> find() [deviation from RDS]
    shl eax,2
    push ecx                -- result addr
--! mov esi,eax
    xchg esi,eax            -- swap(NB!):: eax:=start idx-- esi:=raw(p2)
    mov edx,[edi-12]        -- length(p3)
    mov ecx,[esi-12]        -- length(p2)
    test ecx,ecx
    jz :opMatch0            -- match({},x) gives 0 [deviation from RDS]
--DEV to go:
    cmp ecx,edx             -- check if length(p2) > length(p3)
    jg :opMatch0            -- match(x,{}) or any """ gives 0

sub eax,1
jz :opMatch1
cmp eax,edx                 -- if start_from is -ve/float/oob then longhand
--jb @f
jbe @f
    add eax,1
    jl :opMatchfixupNegativeStart
    -- but it might be a float:
    cmp eax,h4
    jle :e20imsi                -- 0 or >length+1 or unassigned
    cmp byte[ebx+eax*4-1],0x12
    jne :e20imsi                -- invalid match start index
    fld qword[ebx+eax*4]
--see pSubse.e(?)
    fldcw [down53]
--stack:
    fistp qword[FltWrk]
    fldcw [near53]
    mov eax,[FltWrk]
    cmp eax,0
    jge :opMatchfixupStartNotNegative
  ::opMatchfixupNegativeStart
        add eax,edx
----DEV try add eax,1
        jmp :opMatchNegDone
--      xor ebx,ebx
--      cmp eax,edx
--?     ja :e10sspeosediedxN    -- slice starts past end of sequence (%d > %d)
--      ret
  ::opMatchfixupStartNotNegative
    sub eax,1
  ::opMatchNegDone
    xor ebx,ebx
    cmp eax,edx
    ja :e20imsi                 -- invalid match start index
--ret
--=====

--      pop eax
  @@:
    jz :opMatch0

  ::opMatch1

--==========
    --
    -- esi = raw(p2)        -- ecx = length(p2)
    -- edi = raw(p3)        -- edx = length(p3)
    -- eax = start idx (yet to be applied)
    --
    sub ecx,1
    push esi                -- [esp+12]=raw(p2)
    push edi                -- [esp+8]=raw(p3), adjusted every loop
    push ecx                -- [esp+4]=length(p2)-1
    push edx                -- [esp]=length(p3), [NOT] adjusted every loop

    cmp byte[esi-1],0x82    -- is p2 a string?
    jne :opMatchSeqX
    cmp byte[edi-1],0x82
    jne :opMatchStrSeq

    -- match(string,string)
add edi,eax     -- adjust for start idx
sub edx,eax
  @@:
    lodsb                   -- mov al,[esi], esi+=1
    mov ecx,edx             -- remaining chars in p3 [initially all]
    repne scasb             -- find first char, from [edi] on
    jnz :opMatchNotFound
    mov edx,ecx             -- there will be less chars left to scan in the next loop!
    mov ecx,[esp+4]         -- length(p2)-1
    cmp ecx,edx
    jg :opMatchNotFound
    or ecx,ecx
    je :opMatchFound        -- (catch the length(p2)=1 case)

    mov [esp+8],edi         -- save current position in p3
    repe cmpsb              -- remainder all here?
    jz :opMatchFound
    mov esi,[esp+12]        -- reset to p2[2] (after lodsb above!)
    mov edi,[esp+8]         -- reset position in p3, as just saved
    jmp @b

  ::opMatchNotFound
    add esp,16
  ::opMatch0
    xor ecx,ecx
    jmp :opMatchResult

  ::opMatchFound
    pop ecx    
    add esp,12
    sub ecx,edx
  ::opMatchResult
    pop edi                 -- target addr
    mov edx,[edi]
    mov [edi],ecx
    cmp edx,h4
    jle @f
        sub dword[ebx+edx*4-8],1
        jz :%pDealloc
  @@:
    ret

  ::opMatchStrSeq
    -- match(string,sequence)
    -- leap-frog over any non-char found in p3 (a la Boyer-Moore)
    -- (not that this is a high-performance case, really)

lea edi,[edi+eax*4]     -- adjust for start idx
sub edx,eax

    xor eax,eax
  @@:
    lodsb                   -- mov al,[esi], esi+=1
    mov ecx,edx             -- remaining elements of p3 [initially all]
  ::opMatchStrSeqFirstCharLoop
--   repne scasb                                -- find first char, from [edi] on
--   jnz :opMatchNotFound
--   sub edx,1
--   jl :opMatchNotFound    -- DEV poss jl?
    mov edx,[edi]
    lea edi,[edi+4]
    cmp eax,edx
    je :opMatchStrSeqFirstCharFound
    nop
    sub ecx,1
    jnz :opMatchStrSeqFirstCharLoop

    nop
    jmp :opMatchNotFound

  ::opMatchStrSeqFirstCharFound
--DEV: lea edx,[ecx-1]
    sub ecx,1
--stack:
    mov [MatchEdxSave],ecx  -- there will be less elements left to scan in the next loop!
mov edx,ecx
    mov ecx,[esp+4]         -- length(p2)-1
cmp ecx,edx
jg :opMatchNotFound
or ecx,ecx
je :opMatchFound
    mov [esp+8],edi         -- save current position in p3
  ::opMatchStrSeqRemainingCharsLoop
    lodsb                   -- mov al,[esi], esi+=1
--DEV cmpsd? (change rqd in opMatchStrSeqMismatch, probably)
    mov edx,[edi]
    cmp eax,edx
    jne :opMatchStrSeqMismatch
    lea edi,[edi+4]
    sub ecx,1
    jnz :opMatchStrSeqRemainingCharsLoop
--stack:
    mov edx,[MatchEdxSave]
    jmp :opMatchFound

  ::opMatchStrSeqMismatch
    mov esi,[esp+12]                -- reset to p2[2] (after lodsb above!)
    test edx,0xFFFFFF00
    jnz :opMatchStrSeqNotChar
    mov edi,[esp+8]                 -- reset position in p3, as just saved
--stack:
    mov edx,[MatchEdxSave]
    jmp @b

  ::opMatchStrSeqNotChar
    --
    -- not a char: leave edi as is then!
    -- (but must adjust edx to match)
    --
    mov edx,[esp+8]         -- saved position in p3
    sub edx,edi             -- less where we got to (so edx now -ve)
    sar edx,2               -- at 4 bytes per element
--stack:
    add edx,[MatchEdxSave]  -- is adjust to (saved) edx
    jmp @b

  ::opMatchSeqX
    cmp byte[edi-1],0x82
    jne :opMatchSeqSeq
    -- match(sequence,string)
    -- give up the moment any non-char found in p2.

add edi,eax     -- adjust for start idx
sub edx,eax

  @@:
    lodsd       -- mov eax,[esi], esi+=4
    test eax,0xFFFFFF00
    jnz :opMatchNotFound

    mov ecx,edx                 -- remaining chars of p3 [initially all]
    repne scasb                 -- find first char, from [edi] on
    jnz :opMatchNotFound
--stack:
    mov [MatchEdxSave],ecx      -- there will be less elements left to scan in the next loop!
mov edx,ecx
    mov ecx,[esp+4]             -- length(p2)-1
cmp ecx,edx
jg :opMatchNotFound
or ecx,ecx
je :opMatchFound
    mov [esp+8],edi             -- save current position in p3
  ::opMatchSeqStrRemainingCharsLoop
    lodsd       -- mov eax,[esi], esi+=4
    test eax,0xFFFFFF00
    jnz :opMatchNotFound
--DEV cmpsb? (change rqd below, probably)
    cmp al,[edi]
    jne :opMatchSeqStrMismatch
    add edi,1
    sub ecx,1
    jnz :opMatchSeqStrRemainingCharsLoop

--stack:
    mov edx,[MatchEdxSave]
    jmp :opMatchFound

  ::opMatchSeqStrMismatch
    mov esi,[esp+12]            -- reset to p2[2] (with line+1!)
    mov edi,[esp+8]             -- reset position in p3, as just saved
--   lea esi,[esi+4]
--stack:
    mov edx,[MatchEdxSave]
    jmp @b

  ::opMatchSeqSeq
    cmp byte[edi-1],0x80
    jne :opMatchErrPop4e113
    --
--push esi                      -- [esp+12]=raw(p2)
--push edi                      -- [esp+8]=raw(p3), adjusted every loop
--push ecx                      -- [esp+4]=length(p2)-1
--push edx                      -- [esp]=length(p3), [NOT] adjusted every loop
--
    -- match(sequence,sequence)

    mov edx,1                   -- default result

lea edi,[edi+eax*4]     -- adjust for start idx
add edx,eax

  @@:                           -- Main Loop (resume scan for all of P2 from P3[edx])
--      if res+ls1-1>ls2 then return 0 end if
    lea eax,[edx+ecx]
    cmp eax,[esp]               -- length(p3)
    jg :opMatchNotFound

    add ecx,1       -- on entry ecx (and [esp+4]) is length(p2)-1
  ::opMatchSeqSeqNextElement    -- Sub-loop (continue checking p2[i] vs p3[edx+i-1])
    repe cmpsd
    jz :opMatchSeqSeqFound
    --
    -- if p2[i] is a short int, scan for it in remainder of p3
    --  eg p2=23431...,
    --    p3=2343x31...
    --  from p3[6] on, scanning fwd finds p3[7]=1, which gives
    --  the earliest point worth re-starting from (ie +=2).
    --  eg p2=  23431...,
    --    p3=2343x31...
    -- (The whole point here is to minimise the number of attempts
    --  to deep-compare non-integers, btw, and in the example given
    --  the next loop will re-enter here looking for 2(=p2[1]) and
    --  not really save anything. However if p2[1..4] and p3[1..6]
    --  were some big nested sequences, especially when of similar 
    --  lengths,the saving could be quite dramatic.)
    --
    --  if p2i does not occur anywhere later in p3, then
    --  clearly there will be no match anywhere.
    --
    mov eax,[esi-4]
    cmp eax,h4
    jg ::opMatchSeqSeqNotInt

    --
    -- first, calculate number of remaining elements in p3:
    --
    sub ecx,[esp+4] -- -(length(p2)-1)  ~ -1*(elements scanned just now)
    add ecx,[esp]   -- +length(p3)
    sub ecx,edx     -- -res

    --
    -- now do a scan, updating res by no of items skipped, (ie +ecx--scan---ecx)
    --
    add edx,ecx
    repne scasd
    jnz :opMatchNotFound 
    sub edx,ecx

    --
    -- ...and resume scan from p3[edx]:
    --
    mov edi,[esp+8]             -- start of p3
    mov esi,[esp+12]            -- reset to p2
    mov ecx,[esp+4]
    lea edi,[edi+edx*4-4]       -- resume scan from p3[edx]
    jmp @b


  ::opMatchSeqSeqNotInt
    --
    -- if p3[i] is a short int, scan for it earlier in p2
    --
--                  -- scan for s2i earlier on in s1
--                  -- eg s1=131313x...,
--                  --    s2=1313131...
--                  -- with s2[7]=1, scanning back two places to s1[5]=1
--                  -- gives the earliest point worth re-starting from.
--                  -- if s2[7] does not occur anywhere earlier in s1,
--                  -- then clearly we should restart from s2[8].
--
--      mov eax,[edi-4]     -- NO! equalSeq wants ref (from [esi-4]) in eax
--      cmp eax,h4
    cmp dword[edi-4],h4
    jg :opMatchSeqSeqRefRef
    neg ecx                 -- negate how-far-we-got,
    mov eax,[edi-4]
    add ecx,[esp+4]         -- +length(p2)-1
    lea edi,[esi-8]         -- start backwards scan from element before mismatch
    add ecx,1
    --
    -- scan, adjusting edx(res).
    --
    add edx,ecx
    std
    repne scasd
    cld
    --
    -- If we fall off the start of p2, edx adjust is maximal, 
    -- and the main loop resumes starting after the mismatch.
    -- Hence no check on result of repne scasd, info we need
    -- is in [change to] ecx only.
    --
    sub edx,ecx
    --
    -- ...and resume scan from p3[edx]:
    --
--DEV same as 37 lines earlier:
    mov edi,[esp+8]             -- start of p3
    mov esi,[esp+12]            -- reset to p2
    lea edi,[edi+edx*4-4]       -- resume scan from p3[edx]
    mov ecx,[esp+4]
    jmp @b

  ::opMatchSeqSeqRefRef
pushad
    call :equalSeq
popad
     jz :opMatchSeqSeqNextElement
----DEV this may be superfluous: vvvvvvvvvvvvvv
--      jnz :opMatchSeqSeqMismatch
--      or ecx,ecx
--      jnz :opMatchSeqSeqNextElement 
--      nop
--      jmp :opMatchSeqSeqFound
----DEV to here ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
--   cmpsd        -- esi+=4-- edi+=4
--   jmp @b
--
  ::opMatchSeqSeqMismatch
    mov edi,[esp+8]             -- start of p3
    mov esi,[esp+12]            -- reset to p2
    lea edi,[edi+edx*4]         -- resume scan from p3[edx] (NB: no -4 here!)
    mov ecx,[esp+4]             -- length(p2)-1
    add edx,1                   -- res+=1
    jmp @b

  ::opMatchSeqSeqFound
    add esp,16
    mov ecx,edx
    jmp :opMatchResult
--*/

--/*
procedure :%opJcc(:%)
end procedure -- (for Edita/CtrlQ)
--*/
 :%opJcc            -- for opJlt, opJle, opJge, opJgt
--------            -- result is condition flags
                    -- Damages eax,ecx,edx,esi,edi
    [32]
        --calling convention:
        --  mov edi,[p3]        -- ref of p3
        --  mov eax,[p2]        -- ref of p2
        --  mov esi,p3          -- var no of p3
        --  mov edx,p2          -- var no of p2
        --  call opJcc
        --  jcc xxx

--      xor ebx,ebx
        cmp eax,h4
        jge :!Jccp2NotInt
        cmp edi,h4
        jge :!Jccp2Intp3Ref
        cmp eax,edi
        ret

      :!Jccp2NotInt
--je e94vhnbaavedx
        mov cl,[ebx+eax*4-1]
        cmp edi,h4
        jge :Jccp2Refp3Ref
        -- p3 is an int
        cmp cl,0x12
        jne @f
        -- p2(eax) flt, p3(edi) int:
--use stack:
--  mov [FltWrk],edi
--  fild [FltWrk]
        push edi
        fild dword[esp]
        add esp,4
        fld qword[ebx+eax*4]
        fcompp
        fnstsw ax
        mov ch,ah
--mov dh,ah
        and ah,0x7f     -- clear sign
--and dh,0x04   -- keep parity only
        shl ch,7
--shl dh,4      -- parity to Z position
        or ah,ch
--sub ah,dh     -- clear Z if parity set
        sahf
        ret
      @@:
        cmp eax,edi     -- all sequences (eax>h4) deemed greater than int (edx<h4)
        ret

      ::Jccp2Refp3Ref
--      jne equalSeqR
        jne :compareSeqR -- (fall thru if edi/p3 unassigned, aka goto compareSeqR unless p3 unassigned)
--DEV put this back: (or is letting it fall through ok?)
--  jmp e92vhnbaavespm14    --DEV do it inline!
--  jmp e94vhnbaavesi
--  mov edi,edx
--mov edi,[esp]
--mov edi,[edi-14]
--  lea edi,[edi+4]     -- equalSeq calling convention
--  jmp equalSeq

--  Jccp2Intp3Ref:  -- exception here mapped to e92vhnbaavespm14feh
      :!Jccp2Intp3Ref
--  je :e94vhnbaavesi
        cmp byte[ebx+edi*4-1],0x12
        jne @b          -- all ints (eax<h4) deemed less than sequences (edi>h4)
        -- p2(eax) int, p3(edi) flt:
--  mov [FltWrk],eax
        push eax
        fld qword[ebx+edi*4]
--  fild [FltWrk]
        fild dword[esp]
        add esp,4
        fcompp
        fnstsw ax
        mov ch,ah
--  and ah,0x7f
--  shl ch,7
--  or ah,ch
--mov dh,ah
        and ah,0x7f     -- clear sign
--and dh,0x04   -- keep parity only
        shl ch,7
--shl dh,4      -- parity to Z position
        or ah,ch
--sub ah,dh     -- Clear Z if Parity set
        sahf
        ret

    [64]
        --calling convention:
        --  mov rdi,[p3]        -- ref of p3        (opUnassigned*2?)
        --  mov rax,[p2]        -- ref of p2
--      --  mov rsi,p3          -- var no of p3
--      --  mov rdx,p2          -- var no of p2
        --  call opJcc
        --  jcc xxx

--      xor rbx,rbx
        mov r15,h4
--      cmp rax,h4
        cmp rax,r15
        jge :!Jccp2NotInt
--      cmp rdi,h4
        cmp rdi,r15
        jge :!Jccp2Intp3Ref
        cmp rax,rdi
        ret

      :!Jccp2NotInt
--je e94vhnbaavedx
        mov cl,[rbx+rax*4-1]
--      cmp rdi,h4
        cmp rdi,r15
--      jge :Jccp2Refp3Ref
        jge :compareSeq64R
        -- p3 is an int
        cmp cl,0x12
--      cmp byte[rbx+rax*4-1],0x12
        jne @f
        -- p2(rax) flt, p3(rdi) int:
--use stack:
--  mov [FltWrk],edi
--  fild [FltWrk]
        push rdi
        fild qword[rsp]
        add rsp,8
        fld tbyte[rbx+rax*4]
        fcompp
        fnstsw ax
        mov ch,ah
        and ah,0x7f     -- clear sign
        shl ch,7
        or ah,ch
        sahf
        ret

      @@:
        cmp rax,rdi     -- all sequences (rax>h4) deemed greater than int (rdx<h4)
        ret

--    ::Jccp2Refp3Ref
--      jne :compareSeq64R -- (fall thru if rdi/p3 unassigned, aka goto compareSeqR unless p3 unassigned)
      :!Jccp2Intp3Ref
--      je :e94vhnbaavesi
        cmp byte[rbx+rdi*4-1],0x12
        jne @b          -- all ints (rax<h4) deemed less than sequences (rdi>h4)
        -- p2(rax) int, p3(rdi) flt:
        push rax
        fld tbyte[rbx+rdi*4]
        fild qword[rsp]
        add rsp,8
        fcompp
        fnstsw ax
        mov ch,ah
        and ah,0x7f     -- clear sign
        shl ch,7
        or ah,ch
        sahf
        ret
    []

--/*
procedure :%opJccE(:%)
end procedure -- (for Edita/CtrlQ)
--*/
 :%opJccE       -- optimised opJcc for opJeq, opJne 
---------       -- (no need to compare int with float, they're simply not equal,
                --  plus we can cop out early when lengths do not match)
                -- result is Z flag
                -- Damages eax,ecx,edx,esi,edi
    [32]
        --calling convention: (same as opJcc)
        --  mov edi,[p3]        -- ref of p3
        --  mov eax,[p2]        -- ref of p2
        --  mov esi,p3          -- var no of p3
        --  mov edx,p2          -- var no of p2
        --  call opJccE
        --  jcc xxx
        -- rationale used: fast path of {any,int} is 3 clocks.
        --  The first two je :JccEpNe92 will actually both be 
        --  branch mispredictions when they actually trigger, 
        --  but as that leads to a fatal error no-one cares,
        --  and, apart from some pairing loss, they /should/ 
        --  count as no-penalty nops when they don't trigger.
        cmp edi,h4
        jl :JccEp3Int
        je :JccEp3e92
        cmp eax,h4
        jg :equalSeqR
--      je :JccEp2e92
--      cmp eax,edi
--      ret

      ::JccEp3Int
        cmp eax,h4
        je :JccEp2e92
        cmp eax,edi
        ret

      ::JccEp2e92
        mov esi,edx
      ::JccEp3e92
        pop edx
        mov al,92       -- e92vhnbaav(esi)
--      mov edx,[esp]
        sub edx,1
        jmp :!iDiag
        int3

    [64]
        --calling convention:
        --  mov rdi,[p3]        -- ref of p3
        --  mov rax,[p2]        -- ref of p2
        --  mov rsi,p3          -- var no of p3
        --  mov rdx,p2          -- var no of p2
        --  call opJcc
        --  jcc xxx
-->new code:
        mov r15,h4
        cmp rdi,r15
        jl :JccEp3Int
        je :JccEp3e92
        cmp rax,r15
        jg :equalSeq64R
--      je :JccEp2e92
--      cmp rax,rdi
--      ret

      ::JccEp3Int
        cmp rax,r15
        je :JccEp2e92
        cmp rax,rdi
        ret

      ::JccEp2e92
        mov rsi,rdx
      ::JccEp3e92
        pop rdx
        mov al,92       -- e92vhnbaav(esi)
--      mov rdx,[rsp]
        sub rdx,1
        jmp :!iDiag
        int3

--<old code:
--/*
--      cmp rax,h4
--      jle :JccEp2Int
--      cmp rdi,h4
--      jle :JccEp3Int
--      nop
--      jmp :equalSeq64R
--
--    ::JccEp2Int
--      je :e94vhnbaavedx
--      cmp rdi,h4
--    ::JccEp3Int
--      je :e94vhnbaavesi
--      cmp rax,rdi
--      ret
        mov r15,h4
        cmp rax,r15
        jle @f
        cmp rdi,r15
        jg :equalSeq64R
      @@:
        cmp rax,rdi
        ret
-*/
    []

--/*
procedure :%opJif(:%)
end procedure -- (for Edita/CtrlQ)
--*/
 :%opJif
--------
    -- Check for unassigned vars and atom conditions, set Z flag.  Also used by opJnot.
    -- If we know p1 is init atom, just test [p1] directly. This is only used when p1
    -- may be unassigned or string/sequence. Exactly the same code for opJif & opJnot.
    [32]
        --calling convention
        --  mov eax,[p1]        -- ref of p1
        --  mov edx,p1          -- var no of p1
        --  call :%opJif
        --  jnz xxx             -- (opJif)
        --  jz xxx              -- (opJnot)
        --on exit, all regs unaltered, and the Z flag is 1 iff [p1]==0
        --DEV swapping "mov edx,p1" and "call:%opJif" might save a clock...
        --    (see the or_esi = peek4u(or_era+6) trick in pdiagN.e)
        test eax,eax
        jz @f                   -- if Z=1, act on it (ie jnz for opJif, jz for opJnot)
        cmp eax,h4
        jl @f                   -- (Z=0), act on it
    :!Jife92                    -- exception here mapped to e94vhnbaav(edx)
        test byte[ebx+eax*4-1],0x80
        jnz :e03tfcmbaa         -- true/false condition must be an ATOM
--20/7/15:
--                              -- (Z=0, act on it)
        test eax,eax            -- (set Z to 0, and act on it)
      @@:
        ret

    [64]
        --calling convention
        --  mov rax,[p1]        -- ref of p1
        --  mov rdx,p1          -- var no of p1
        --  call :%opJif
        --  jnz xxx             -- (opJif)
        --  jz xxx              -- (opJnot)
        --on exit, all regs unaltered, and the Z flag is 1 iff [p1]==0
        test rax,rax
        jz @f                   -- if Z=1, act on it (ie jnz for opJif, jz for opJnot)
        mov r15,h4
        cmp rax,r15
        jl @f                   -- (Z=0), act on it
    :!Jife92                    -- exception here mapped to e94vhnbaav(edx)
        test byte[rbx+rax*4-1],0x80
        jnz :e03tfcmbaa         -- true/false condition must be an ATOM
--20/7/15:
--                              -- (Z=0, act on it)
        test rax,rax            -- (set Z to 0, and act on it)
      @@:
        ret
    []

--DEV might yet need this...
----/*
--procedure :%opSeq(:%)
--end procedure -- (for Edita/CtrlQ)
----*/
-- :%opSeq
----------
--  [32]
--  []

--/*
procedure :%opScmp(:%)
end procedure -- (for Edita/CtrlQ)
--*/
 :%opScmp
---------
    [32]
        --calling convention:
        --  lea edx,[tgt]
        --  mov edi,[p3]    (opUnassigned)
        --  mov eax,[p2]    (opUnassigned)
        --  call opSgt  [tgt] := -1/0/+1
--/!* (25/9/17 sug) [14/10 went ahead anyway (w/o any new tests)]
        push dword[edx]
--*!/
        push edx
--DEV 25/9/17 (spotted in passing) this will (probably) have issues for a = compare(a,b)...
--/*
        mov edx,[edx]
        cmp edx,h4
        jle @f
            sub dword[ebx+edx*4-8],1
            jnz @f
            push edi
            push eax
            push dword[esp+12]
            call :%pDealloc0
            pop eax
            pop edi
      @@:
--*/
        call :%opJcc
        pop edx
        mov eax,ebx     -- 0                
        je @f
        mov al,1        -- +1
        jg @f
        sub eax,2       -- -1
--      mov eax,-1
      @@:
        mov [edx],eax
--/!* (25/9/17 sug)
        pop edx
        cmp edx,h4
        jle @f
            sub dword[ebx+edx*4-8],1
--          jnz @f
--          push dword[esp+12??]    -- (next line needs careful testing!)
--          push dword[esp]
--          call :%pDealloc0
            jz :%pDealloc
      @@:
--*!/
        ret
    [64]
        --calling convention:
        --  lea rdx,[tgt]
        --  mov rdi,[p3]    (opUnassigned)
        --  mov rax,[p2]    (opUnassigned)
        --  call opSgt  [tgt] := -1/0/+1
        push qword[edx]
        push rdx
--DEV as above
--      mov rdx,[rdx]
--      mov r15,h4
--      cmp rdx,r15
--      jle @f
--          sub qword[rbx+rdx*4-16],1
--          jnz @f
--          push rdi
--          push rax
--          push qword[rsp+24]
--          call :%pDealloc0
--          pop rax
--          pop rdi
--    @@:
--      call :compareSeq64R
        call :%opJcc
        pop rdx
        mov rax,rbx     -- 0                
        je @f
        mov al,1        -- +1
        jg @f
        sub rax,2       -- -1
--      mov rax,-1
      @@:
        mov [rdx],rax
        pop rdx
        cmp rdx,r15
        jle @f
            sub qword[rbx+rdx*4-16],1
            jz :%pDealloc
      @@:
        ret
    []

--/*
procedure :%opInt0(:%)
end procedure -- (for Edita/CtrlQ)
--*/
 :%opInt0               -- edx:=integer(eax)
---------
    [32]
        --calling convention:
        --  mov eax,[p1]    (opUnassigned)
        --  call :%opInt0   edx:=integer(eax)
        cmp eax,h4
--      mov edx,ebx
--      setl dl
        jle :edxI1
      ::edx0
        xor edx,edx
        ret
      ::edxI1
        mov edx,1
--      je e94vhnbaavecx
        ret
    [64]
        --calling convention:
        --  mov rax,[p1]    (opUnassigned)
        --  call :%opInt0   rdx:=integer(rax)
        mov r15,h4
        cmp rax,r15
        jle :rdxI1
      ::rdx0
        xor rdx,rdx
        ret
      ::rdxI1
        mov rdx,1
        ret
    []

--/*
procedure :%opAtom0(:%)
end procedure -- (for Edita/CtrlQ)
--*/
  :%opAtom0             -- edx:=atom(eax)
----------
    [32]
        cmp eax,h4
        jl :edx1
--  opAtom0e92:             -- exception mapped to e94vhnbaavecxfeh
        cmp byte[ebx+eax*4-1],0x12
        jne :edx0
      ::edx1
        mov edx,1
        ret
    [64]
        mov r15,h4
        cmp rax,r15
        jl :rdx1
        cmp byte[rbx+rax*4-1],0x12
        jne :rdx0
      ::rdx1
        mov rdx,1
        ret
    []

--/*
procedure :%opStr0(:%)
end procedure -- (for Edita/CtrlQ)
--*/
  :%opStr0              -- edx=string(eax)
----------
    [32]
        cmp eax,h4
        jl :edx0
--  opStr0e92:              -- exception mapped to e94vhnbaavecxfeh
        cmp byte[ebx+eax*4-1],0x82
        jne :edx0
        mov edx,1
        ret
    [64]
        mov r15,h4
        cmp rax,r15
        jl :rdx0
        cmp byte[rbx+rax*4-1],0x82
        jne :rdx0
        mov rdx,1
        ret
    []

--/*
procedure :%opSq0(:%)
end procedure -- (for Edita/CtrlQ)
--*/
  :%opSq0                   -- edx=sequence(eax)  (NB: opSeq is p1=(p2=p3))
---------
    [32]
        cmp eax,h4
        jl :edx0
--  opSq0e92:               -- exception mapped to e94vhnbaavecxfeh
        test byte[ebx+eax*4-1],0x80
        jz :edx0
        mov edx,1
        ret
    [64]
        mov r15,h4
        cmp rax,r15
        jl :rdx0
        test byte[rbx+rax*4-1],0x80
        jz :rdx0
        mov rdx,1
        ret
    []
      }
