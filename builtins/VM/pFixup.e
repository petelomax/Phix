--
--  pFixup.e
--  ========
--
--      negative and floating point index handling 
--      (:%fixupIndex, :%fixupSliceStart, :%fixupSliceEnd)
--
--  Note this is not an autoinclude. Used by pSubseN.e, pRepeN.e, pSubssN.e, pJnotx.e
--
--  All functions in this file adjust edi to a normal 0-based index, and leave 
--  all other registers undamaged.
--

include builtins\VM\pFPU.e

--#ilASM{ jmp :%opRetf
#ilASM{ jmp :!opCallOnceYeNot

--DEV FIXME: (and the :!bang labels below)
    ::e05sinaae06
        or al,#10
        jmp :e06ioobWhat
--      int3

    ::e06ioobp1WhatN
    [32]
        sub edi,edx
        sub edi,1
    [64]
        sub rdi,rdx
        sub rdi,1
    []
    ::e06ioobp1What
    [32]
        add edi,1
    [64]
        add rdi,1
    []
    ::e06ioobWhat
        -- 0 or >length
    [32]
        mov esi,edx
    [64]
        mov rsi,rdx
    []
        test al,0x08
        jz :e06not8
        test al,0x04
        jz :e06not4
    [32]
            mov edx,[esp+ecx*4+4]
    [64]
            mov rdx,[rsp+rcx*8+8]
    []
            jmp @f
      ::e06not4
        test al,0x02
        jz :e06not2
    [32]
            mov edx,[esp+ecx*4]
    [64]
            mov rdx,[rsp+rcx*8]
    []
            jmp @f
      ::e06not2
    [32]
            mov edx,[esp+ecx*4-4]
    [64]
            mov rdx,[rsp+rcx*8-8]
    []
            jmp @f
      ::e06not8
    [32]
--5/4/16: (wrong line on linux, from t39rndio/fimage[idx]=ch (line 111), when fimage hosed)
-- undone 20/6/16 (and pRepe1ip changed for "" to use #04)
            mov edx,[esp+4]
--          mov edx,[esp+8]
    [64]
--28/8/15:
            mov rdx,[rsp+8]
--          mov rdx,[rsp+16]
    []
        test al,0x04
        jz @f
    [32]
            mov edx,[esp+8]
    [64]
            mov rdx,[rsp+16]
    []
      @@:
    [32]
        sub edx,1
    [64]
        sub rdx,1
    []
--      mov [ep1],edi
--      mov [ep2],edx
--      mov [era],ecx
        test al,0x20
        jz @f
            mov al,92           -- variable has not been assigned a value
            jmp :!iDiag
            int3
      @@:
        test al,0x10
        jz @f
            mov al,5            -- subscript is not an atom
            jmp :!iDiag
            int3
      @@:
        test al,1
        jnz @f
            mov al,6            -- index %d(edi) out of bounds, assigning to a sequence of length %d(esi)
            jmp :!iDiag
            int3
      @@:
            mov al,106          -- index %d(edi) out of bounds, reading from a sequence of length %d(esi)
            jmp :!iDiag
            int3

    ::e05sinaae10
        mov al,5
        jmp @f

--    ::e06ioobp1WhatN
--      int3
--    ::e06ioobp1What
--      int3
      ::e08seinaie11
        int3
      ::e10sspeosediedx
        int3
      ::e10sspeosediedx0
--      int3
        mov al,10               -- slice starts past end of sequence (%d(edi) > %d(esi))
        jmp @f
      ::e10sspeosediedxN
--      int3
    [32]
        sub edi,edx
    [64]
        sub rdi,rdx
    []
        mov al,10
        jmp @f  
      ::e11sepeosediedxN
    [32]
        sub edi,edx
        sub edi,1
    [64]
        sub rdi,rdx
        sub rdi,1
    []
      ::e11sepeosediedx
        mov al,11               -- slice ends past end of sequence (%d(edi) > %d(esi))
      @@:
    [32]
        mov esi,edx
        and ecx,0xFF
        mov edx,[esp+ecx]       -- era
        sub edx,1
    [64]
        mov rdi,rdx
        and rcx,0xFF
        mov rdx,[rsp+rcx]       -- era
        sub rdx,1
    []
        jmp :!iDiag
        int3                    -- (nodiag)

--/*
e06ioobp1WhatN:
    sub edi,edx
    jmp e06ioobWhat 
e06ioobp1What:
    add edi,1
e06ioobWhat:
    ; bit #20 in al: 1 is e92 ("variable has not been assigned a value") [set in e92vhnbaavEbxe06feh]
    ; bit #10 in al: 1 is e05 ("subscript is not an atom") [set in e05sinaae06]
    ; bit #01 in al: 0 is e06 ("assigning to"), 1 is e106 ("reading from")
    ; bit #08 in al, plus:
    ;  bit #04=1: [era] is at [esp+ecx*4+4]
    ;  bit #02=0: [era] is at [esp+ecx*4-4]
    ;  bit #02=1: [era] is at [esp+ecx*4]
    ; bit #04 in al: [era] is at [esp+8]
    ; bit #02 in al: [era] is at [esp+4]
    mov ebx,esp
e06espinebx:
    test al,0x08
    jz e06not8
    test al,0x04
    jz e06not4
    mov ecx,[ebx+ecx*4+4]
    jmp @f
e06not4:
    test al,0x02
    jz e06not2
    mov ecx,[ebx+ecx*4]
    jmp @f
e06not2:
    mov ecx,[ebx+ecx*4-4]
    jmp @f
e06not8:        
    mov ecx,[ebx+4]
    test al,0x04
    jz @f
    mov ecx,[ebx+8]
@@:
    sub ecx,1
    mov [ep1],edi
    mov [ep2],edx
    mov [era],ecx
    test al,0x20
    jz @f
    mov al,92           ; variable has not been assigned a value
    jmp enumbset
@@:
    test al,0x10
    jz @f
    mov al,5            ; subscript is not an atom
    jmp enumbset
@@:
    test al,1
    jnz @f
    mov al,6            ; index %d out of bounds, assigning to a sequence of length %d
    jmp enumbset

e106ioobrp1:
    add edi,1
e106ioobr:
    mov [ep1],edi
    mov [ep2],edx
    pop [era]
@@:
    mov al,106          ; index %d out of bounds, reading from a sequence of length %d
    jmp enumbset
--*/


--/*
procedure :%fixupIndex(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%fixupIndex
----------------
    [32]
        -- idx-1 in edi, len in edx, NOT: idx addr in ebx, al set as below
        -- bit #01 in al: 0 is e06 ("assigning to"), 1 is e106 ("reading from")
        -- bit #08 in al, plus:
        --  bit #04=1: [era] is at [esp+ecx*4+4]
        --  bit #02=0: [era] is at [esp+ecx*4-4]
        --  bit #02=1: [era] is at [esp+ecx*4]
        -- bit #04 in al: [era] is at [esp+8]
        -- bit #02 in al: [era] is at [esp+4]

        add edi,1
        jl :opfixupNegativeIdx
        -- but it might be a float:
        cmp edi,h4
        jl :e06ioobWhat             -- 0 or >length
--DEV use [ebx+edi*2-1] instead... :%LoadMint?
        shl edi,2
--DEV
--    :!fixupIdxe92                 -- exception here mapped to e92vhnbaavEbxe06feh (see pdiagN.e)
        cmp byte[edi-1],0x12
        jne :e05sinaae06
        fld qword[edi]
        call :%down53
        sub esp,8
        fistp qword[esp]
        call :%near53
        mov edi,[esp]
        add esp,8
        cmp edi,0
        jge :opfixupIdxNotNegative
      ::opfixupNegativeIdx
            add edi,edx
            xor ebx,ebx
            cmp edi,edx
            jae :e06ioobp1WhatN
            ret
      ::opfixupIdxNotNegative
        sub edi,1
        xor ebx,ebx
        cmp edi,edx
        jae :e06ioobp1What          -- > length or still <=0
    [64]
        -- NB: calling code usually has the 32-bit comment, even in 64-bit code.
        --      if it says esp, map it from the above and update the comment,
        --      once you have proved it is correct and fully tested, that is.
        -- idx-1 in rdi, len in rdx, NOT: idx addr in rbx, al set as below
        -- bit #01 in al: 0 is e06 ("assigning to"), 1 is e106 ("reading from")
        -- bit #08 in al, plus:
        --  bit #04=1: [era] is at [rsp+rcx*8+8]
        --  bit #02=0: [era] is at [rsp+rcx*8-8]
        --  bit #02=1: [era] is at [rsp+rcx*8]
        -- bit #04 in al: [era] is at [rsp+16]
        -- bit #02 in al: [era] is at [rsp+8]

        add rdi,1
        jl :opfixupNegativeIdx
        -- but it might be a float:
--      cmp rdi,h4
        mov r15,h4  -- DEV should always already be so?...
        cmp rdi,r15
        jl :e06ioobWhat             -- 0 or >length
        shl rdi,2
--DEV
--    :!fixupIdxe92                 -- exception here mapped to e92vhnbaavEbxe06feh (see pdiagN.e)
        cmp byte[rdi-1],0x12
        jne :e05sinaae06
        fld tbyte[rdi]
--      fldcw [down53]
        call :%down53
        sub rsp,8
        fistp qword[rsp]
--      fldcw [near53]
--      call :%near53
        call :%near64
        mov rdi,[rsp]
        add rsp,8
        cmp rdi,0
        jge :opfixupIdxNotNegative
      ::opfixupNegativeIdx
            add rdi,rdx
            xor rbx,rbx
            cmp rdi,rdx
            jae :e06ioobp1WhatN
            ret
      ::opfixupIdxNotNegative
        sub rdi,1
        xor rbx,rbx
        cmp rdi,rdx
        jae :e06ioobp1What          -- > length or still <=0
    []
        ret

--/*
procedure :%fixupSliceStart(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%fixupSliceStart
---------------------
    [32]
        add edi,1
        jle :fixupNegativeSliceStart
        -- but it might be a float:
        cmp edi,h4
        jl :e10sspeosediedx0
        shl edi,2
--DEV
--  :!fixupSliceStarte92            -- exception here mapped to e92vhnbaavEbxe10feh
        cmp byte[edi-1],0x12
        jne e05sinaae10             -- subscript is not an atom [era as per e10]
        fld qword[edi]
--      fldcw [down53]
        call :%down53
        sub esp,8
        fistp qword[esp]
--      fldcw [near53]
        call :%near53
        mov edi,[esp]
        add esp,8
        cmp edi,0
        jge :fixupSliceStartNotNegative
      ::fixupNegativeSliceStart
            add edi,edx
            xor ebx,ebx
            cmp edi,edx
            ja :e10sspeosediedxN    -- slice starts past end of sequence (%d > %d)
            ret
      ::fixupSliceStartNotNegative
        sub edi,1
        xor ebx,ebx
        cmp edi,edx
        ja :e10sspeosediedx     -- slice starts past end of sequence (%d > %d)
        ret
    [64]
        add rdi,1
        jle :fixupNegativeSliceStart
        -- but it might be a float:
        mov r15,h4
        cmp rdi,r15
        jl :e10sspeosediedx0
        shl rdi,2
--DEV
--  :!fixupSliceStarte92            -- exception here mapped to e92vhnbaavEbxe10feh
        cmp byte[rdi-1],0x12
        jne e05sinaae10             -- subscript is not an atom [era as per e10]
        fld tbyte[rdi]
--      fldcw [down53]
        call :%down53
        sub rsp,8
        fistp qword[rsp]
--      fldcw [near53]
--      call :%near53
        call :%near64
        mov rdi,[rsp]
        add rsp,8
        cmp rdi,0
        jge :fixupSliceStartNotNegative
      ::fixupNegativeSliceStart
            add rdi,rdx
            xor rbx,rbx
            cmp rdi,rdx
            ja :e10sspeosediedxN    -- slice starts past end of sequence (%d > %d)
            ret
      ::fixupSliceStartNotNegative
        sub rdi,1
        xor rbx,rbx
        cmp rdi,rdx
        ja :e10sspeosediedx     -- slice starts past end of sequence (%d > %d)
        ret
    []

--/*
procedure :%fixupSliceEnd(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%fixupSliceEnd
-------------------
    [32]
        -- idx in edi, len in edx, cl=0/4/8/12 etc for era
        -- NB flags from the cmp edi,edx must be undamaged!
        jl :fixupNegativeSliceEnd

        -- but it might be a float:
        cmp edi,h4
--      jl e06ioobWhat
        jl :e11sepeosediedx
        shl edi,2
--DEV
--    :!fixupSliceEnde92                  -- exception here mapped to e92vhnbaavEbxe11feh
        cmp byte[edi-1],0x12
        jne :e08seinaie11                -- slice end is not an integer [era as per e11]
        fld qword[edi]
        call :%down53
        sub esp,8
        fistp qword[esp]
        call :%near53
        mov edi,[esp]
        add esp,8
        cmp edi,0
        jge :fixupSliceEndNotNegative
      ::fixupNegativeSliceEnd
            lea edi,[edi+edx+1]
            xor ebx,ebx
            cmp edi,edx
            ja :e11sepeosediedxN    -- slice ends past end of sequence (%d > %d)
            ret
      ::fixupSliceEndNotNegative
        cmp edi,edx
        ja :e11sepeosediedx     -- slice ends past end of sequence (%d > %d)
        xor ebx,ebx
        ret
    [64]
        -- idx in rdi, len in rdx, cl=4/8 for era
        -- NB flags from the cmp rdi,rdx must be undamaged!
        jl :fixupNegativeSliceEnd

        -- but it might be a float:
--      cmp rdi,h4
        mov r15,h4
        cmp rdi,r15
--      jl e06ioobWhat
        jl :e11sepeosediedx
        shl rdi,2
--DEV
--    :!fixupSliceEnde92                  -- exception here mapped to e92vhnbaavEbxe11feh
        cmp byte[rdi-1],0x12
        jne :e08seinaie11                -- slice end is not an integer [era as per e11]
        fld qword[rdi]
--      fldcw [down53]
        call :%down53
        sub rsp,8
        fistp qword[rsp]
--      fldcw [near53]
--      call :%near53
        call :%near64
        mov rdi,[rsp]
        add rsp,8
        cmp rdi,0
        jge :fixupSliceEndNotNegative
      ::fixupNegativeSliceEnd
            lea rdi,[rdi+rdx+1]
            xor rbx,rbx
            cmp rdi,rdx
            ja :e11sepeosediedxN    -- slice ends past end of sequence (%d > %d)
            ret
      ::fixupSliceEndNotNegative
        cmp rdi,rdx
        ja :e11sepeosediedx     -- slice ends past end of sequence (%d > %d)
        xor rbx,rbx
        ret
    []
      }

