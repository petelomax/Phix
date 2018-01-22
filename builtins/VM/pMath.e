--
-- pMath.e
-- =======
--
--  implements :%opAdd[i], :%opSub[i], :%opDiv[i/f/2], :%opMul[i],
--             :%opAndBits, :%opOrBits, :%opXorBits
--
--  Obviously, the compiler emits "add", "sub", "shr", etc when it can, but one thing we really 
--  want to avoid are the C style idioms that 2147483647+1 = -2147483648, or that 3/2 is 1. 
--  In Phix, the result is whatever type it needs to be, so (the integer) 1073741823 + 1 is 
--  (the atom) 1073741824, 3/2 is 1.5, floor(3/2) is 1, and 1.5+1.5 is (the integer) 3.
--
--  Note: These functions require atom arguments, see builtins\psqop.e for explicit function-style
--        sequence ops. I would not be totally averse to these functions being extended to cater
--        for (implicit) sequence ops, it is {=,!=,<=,<,>,>=} that I object to, and personally I
--        believe it is just not worth deliberately doing "half" (ok 26/32) of a job, iyswim. I
--        would not object to adding say {@==,@!=,@<=,@<,@>,@>=} as explicit infix relops, either.
--        I could also argue that psqop.e has proved itself to be perfectly fast enough.
--

include builtins\VM\pFPU.e      -- :%down53, :%near53

-- to force it into p.exe for the optable...
include builtins\VM\pXor.e      -- not actually used in p.exe, but in optable for pdiagN.e

constant half = 0.5

integer e14code

#ilASM{ jmp :%opRetf

    ::e14soa
        [32]
            pop edx
            mov al,14               -- e14soa(edi=?)
            mov edi,[e14code]
            sub edx,1
            jmp :!iDiag
        [64]
            pop rdx
            mov al,14               -- e14soa(edi=?)
            mov rdi,[e14code]
            sub rdx,1
            jmp :!iDiag
        []
            int3
        [32]
    ::e01tcfst0edi
            add esp,8
            jmp @f
        []
    ::e01tcfeaxedi
        [32]
            push eax
            fild dword[esp]
            add esp,4
          @@:
            call :%pStoreFlt
            pop edx
            mov al,110              -- e110tce(ecx)
            sub edx,1
            mov ecx,edi
            jmp :!iDiag
        [64]
            push rax
            fild qword[rsp]
    ::e01tcfst0rdi
            add rsp,8
            call :%pStoreFlt
            pop rdx
            mov al,110              -- e110tce(ecx)
            sub rdx,1
            mov rcx,rdi
            jmp :!iDiag
        []
            int3
    ::e02atdb0pop
        [32]
            add esp,4               -- trash return address (opMathIII)
        [64]
            add rsp,8               -- trash return address (opMathIII)
        []
    ::e02atdb0
        [32]
            pop edx
            mov al,2
            add edx,-1
        [64]
            pop rdx
            mov al,2
            add rdx,-1
        []
            jmp :!iDiag
        int3
    ::e01tcfeaxplusedxoverecxedi
        [32]
            push esi    -- saved ecx
            fild dword[esp]
            mov [esp],eax
            fild dword[esp]
            add esp,4
            fdivp
            jmp @b
        [64]
            push rsi    -- saved rcx
            fild qword[rsp]
            mov [rsp],r8
            fild qword[rsp]
            fdivp
            jmp :e01tcfst0rdi
-- courtesy of Thomas Grysztar ( https://board.flatassembler.net/topic.php?p=198279#198279 )
        ::int128_to_float80
            -- in: rdx:rax = signed 128-bit integer 
            -- out: dx:rax = 80-bit float 
            test rdx,rdx 
            jns :uint128_to_float80 
            not rdx     --\ 
            not rax     -- \  this is just a 
            add rax,1   -- /  neg rdx:rax 
            adc rdx,0   --/ 
            call :uint128_to_float80 
            or dx,#8000
            ret
        ::uint128_to_float80
            -- in: rdx:rax = unsigned 128-bit integer 
            -- out: dx:rax = 80-bit float 
            bsr r8,rdx 
            jz :low 
            mov cl,63 
            sub cl,r8l 
            shld rdx,rax,cl 
            mov rax,rdx 
--          lea rdx,[16383+64+r8] 
            lea rdx,[r8+16383+64] 
            ret
        ::low
            bsr r8,rax 
            jz :zero 
            mov cl,63 
            sub cl,r8l 
            shl rax,cl 
--          lea rdx,[16383+r8] 
            lea rdx,[r8+16383] 
            ret
        ::zero
            xor eax,eax 
            xor edx,edx 
            ret
        []
    ::e01tcfediMul
        [32]
            push ecx
            push eax
            fild qword[esp]
            call :%pStoreFlt
            add esp,12      -- trash flt & return address (opMathIII)
            mov al,110      -- e110tce(ecx)
            pop edx
            mov ecx,edi
            sub edx,1
        [64]
--          -- (unlike 32-bit, we push the original rax/rcx, rather 
--          --  than try fild the signed 128-bit integer in rcx:rax)
--          fild qword[rsp]
--          fild qword[rsp+8]
--          fmulp
            mov rdx,rcx
            call :int128_to_float80
            push rdx
            push rax
            fld tbyte[rsp]
--          add rsp,16
            call :%pStoreFlt
            add rsp,24      -- trash flt & return address (opMathIII)
            mov al,110      -- e110tce(rcx)
            pop rdx
            mov rcx,rdi
            sub rdx,1
        []
            jmp :!iDiag
        int3

--/*
procedure ::opMath(::)
end procedure -- (for Edita/CtrlQ)
--*/
    [32]
      ::opMath
--------------
        -- internal routine.
        -- on entry,
        --  edi is addr result
        --  ecx is p2 (opUnassigned)
        --  eax is p3 (opUnassigned)
        --  edx is the integer math function (eg :opAddI) (result in ecx)
        --  esi is the floating point math function (eg :opAddF) (result in st0)
        -- (NB: edx/esi are permitted to discard the (new) return address,
        --  finish the job themselves, or whatever else takes their fancy.)
        --  [esp] is the return address
        --  
        cmp ecx,h4
        jge :opMathN
            --
            -- ecx [p2] is an int
            --
            cmp eax,h4
            jl @f
                --
                -- ecx/p2 is int, eax/p3 is (must be) a float.
                --
                push ecx
                fild dword[esp]
                add esp,4
                cmp byte[ebx+eax*4-1],0x12
                jne e14soa                  -- sequence op attempted
                fld qword[ebx+eax*4]
                call esi                    -- eg faddp
                jmp :%pStoreFlt

          @@:
            call edx                        -- eg add ecx,eax
          ::opMathIII                       -- (opDivI returns via opMathIII or not at all)
            mov eax,ecx
            mov edx,[edi]
            shl ecx,1
            jo opMathIIN                    -- must store as atom
            mov [edi],eax
            cmp edx,h4
            jle @f
                sub dword[ebx+edx*4-8],1
                jz :%pDealloc
          @@:
            ret

        ::opMathIIN
            --
            -- eax is result but too big for integer.
            --
            push eax
            fild dword[esp]
            add esp,4
            jmp :%pStoreFlt

        ::opMathN
            --
            -- load p2 as float, then consider p3
            --
            cmp byte[ebx+ecx*4-1],0x12
            jne e14soa                  -- sequence op attempted
            fld qword[ebx+ecx*4]

            cmp eax,h4
            jge :opMathNN
            push eax
            fild dword[esp]
            add esp,4
            call esi                    -- eg faddp
            jmp :%pStoreFlt

        ::opMathNN
            --
            -- st0/p2 is loaded, eax/p3 is (must be) a float
            --
            cmp byte[ebx+eax*4-1],0x12
            jne e14soa                  -- sequence op attempted
            fld qword[ebx+eax*4]
            call esi                    -- eg faddp
            jmp :%pStoreFlt

    [64]
      ::opMath
--------------
        -- internal routine.
        -- on entry,
        --  rdi is addr result
        --  rcx is p2 (opUnassigned)
        --  rax is p3 (opUnassigned)
        --  rdx is the integer math function (eg :opAddI)
        --  rsi is the floating point math function (eg :opAddF)
        -- (NB: rdx/rsi are permitted to discard the (new) return address,
        --  finish the job themselves, or whatever else takes their fancy.)
        --  [rsp] is the return address
        --  
        mov r15,h4
--      cmp rcx,h4
        cmp rcx,r15
        jge :opMathN
            --
            -- rcx [p2] is an int
            --
--          cmp rax,h4
            cmp rax,r15
            jl @f
                --
                -- rcx/p2 is int, rax/p3 is (must be) a float.
                --
                push rcx
                fild qword[rsp]
                add rsp,8
                cmp byte[rbx+rax*4-1],0x12
                jne e14soa                  -- sequence op attempted
                fld tbyte[rbx+rax*4]
                call rsi                    -- eg faddp
                jmp :%pStoreFlt

          @@:
            call rdx                        -- eg add rcx,rax
          ::opMathIII                       -- (opDivI returns via opMathIII or not at all)
            mov rax,rcx
            mov rdx,[rdi]
            shl rcx,1
            jo opMathIIN                    -- must store as atom
            mov [rdi],rax
--          cmp rdx,h4
            cmp rdx,r15
            jle @f
                sub qword[rbx+rdx*4-16],1
                jz :%pDealloc
          @@:
            ret

        ::opMathIIN
            --
            -- rax is result but too big for integer.
            --
            push rax
            fild qword[rsp]
            add rsp,8
            jmp :%pStoreFlt

        ::opMathN
            --
            -- load p2 as float, then consider p3
            --
            cmp byte[rbx+rcx*4-1],0x12
            jne e14soa                  -- sequence op attempted
            fld tbyte[rbx+rcx*4]

--          cmp rax,h4
            cmp rax,r15
            jge :opMathNN
            push rax
            fild qword[rsp]
            add rsp,8
            call rsi                    -- eg faddp
            jmp :%pStoreFlt

        ::opMathNN
            --
            -- st0/p2 is loaded, rax/p3 is (must be) a float
            --
            cmp byte[rbx+rax*4-1],0x12
            jne e14soa                  -- sequence op attempted
            fld tbyte[rbx+rax*4]
            call rsi                    -- eg faddp
            jmp :%pStoreFlt

--/*
procedure ::opMathi(::)
end procedure -- (for Edita/CtrlQ)
--*/
    [32]
      ::opMathi
---------------
        -- as opMath, but result is integer, so this has no dealloc and a builtin type check failure
        --  note that (eg) i=(1.5+1.5) is perfectly valid (so we still need a float math function).
        -- on entry, (exactly the same as above)
        --  edi is addr result
        --  ecx is p2 (opUnassigned)
        --  eax is p3 (opUnassigned)
        --  edx is the integer math function (eg :opAddI)
        --  esi is the floating point math function (eg :opAddF)
        --  [esp] is the return address
        --  

        cmp ecx,h4
        jge :opMathiN
            --
            -- ecx [p2] is an int
            --
            cmp eax,h4
            jge :opMathiIN
            call edx                -- eg add ecx,eax
          ::opMathiIII
            mov eax,ecx

            shl ecx,1
            jo :e01tcfeaxedi

            mov [edi],eax
            ret

        ::opMathiIN
            --
            -- ecx/p2 is set, eax/p3 is (must be) a float.
            --
            push ecx
            fild dword[esp]
            add esp,4
            cmp byte[ebx+eax*4-1],0x12
            jne e14soa                  -- sequence op attempted
            fld qword[ebx+eax*4]
            call esi                    -- eg faddp

        ::opMathsToNi
            --
            -- result in st0; store as int if possible else typecheck
            --
            sub esp,8
            fld st0                     -- another copy (fist [no-p] cannot store 64-bit ints)
            fistp qword[esp]            -- (immediately discards that copy)
            fild qword[esp]
            fcomp st1
            fnstsw ax
            sahf
            jne :e01tcfst0edi
            mov eax,[esp]
            cdq                         -- sign extend eax into edx
            cmp edx,[esp+4]
            jne :e01tcfst0edi
            mov edx,eax
            shl edx,1
            jo :e01tcfst0edi
            fstp st0                    -- discard st0 (31-bit result now in eax)
            mov [edi],eax
            add esp,8
            ret

        ::opMathiN
            --
            -- load p2 as float, then consider p3
            --
            cmp byte[ebx+ecx*4-1],0x12
            jne e14soa                  -- sequence op attempted
            fld qword[ebx+ecx*4]

            cmp eax,h4
            jge :opMathiNN
            push eax
            fild dword[esp]
            add esp,4
            call esi                    -- eg faddp
            jmp :opMathsToNi

        ::opMathiNN
            --
            -- st0/p2 is loaded, eax/p3 is (must be) a float
            --
            cmp byte[ebx+eax*4-1],0x12
            jne e14soa                  -- sequence op attempted
            fld qword[ebx+eax*4]
            call esi                    -- eg faddp
            jmp :opMathsToNi

    [64]
      ::opMathi
---------------
        -- as opMath, but result is integer, so this has no dealloc and a builtin type check failure
        --  note that (eg) i=(1.5+1.5) is perfectly valid (so we still need a float math function).
        -- on entry, (exactly the same as above)
        --  rdi is addr result
        --  rcx is p2 (opUnassigned)
        --  rax is p3 (opUnassigned)
        --  rdx is the integer math function (eg :opAddI)
        --  rsi is the floating point math function (eg :opAddF)
        --  [rsp] is the return address
        --  
        mov r15,h4
        cmp rcx,r15
        jge :opMathiN
            --
            -- rcx [p2] is an int
            --
            cmp rax,r15
            jge :opMathiIN
            call rdx                -- eg add ecx,eax
          ::opMathiIII
            mov rax,rcx

            shl rcx,1
            jo :e01tcfeaxedi

            mov [rdi],rax
            ret

        ::opMathiIN
            --
            -- rcx/p2 is int, rax/p3 is (must be) a float.
            --
            push rcx
            fild qword[rsp]
            add rsp,8
            cmp byte[rbx+rax*4-1],0x12
            jne e14soa                  -- sequence op attempted
            fld tbyte[rbx+rax*4]
            call rsi                    -- eg faddp

        ::opMathsToNi
            --
            -- result in st0; store as int if possible else typecheck
            --
            sub rsp,8
            fld st0                     -- another copy (fist [no-p] cannot store 64-bit ints)
            fistp qword[rsp]            -- (immediately discards that copy)
            fild qword[rsp]
            fcomp st1
            fnstsw ax
            sahf
            jne :e01tcfst0rdi
            mov rax,[rsp]
            mov rdx,rax
            shl rdx,1
            jo :e01tcfst0rdi
            fstp st0                    -- discard st0 (31-bit result now in eax)
            mov [rdi],rax
            add rsp,8
            ret

        ::opMathiN
            --
            -- load rcx/p2 as float, then consider rax/p3
            --
            cmp byte[rbx+rcx*4-1],0x12
            jne e14soa                  -- sequence op attempted
            fld tbyte[rbx+rcx*4]

            mov r15,h4
            cmp rax,r15
            jge :opMathiNN
            push rax
            fild dword[rsp]
            add rsp,8
            call rsi                    -- eg faddp
            jmp :opMathsToNi

        ::opMathiNN
            --
            -- st0/p2 is loaded, rax/p3 is (must be) a float
            --
            cmp byte[rbx+rax*4-1],0x12
            jne e14soa                  -- sequence op attempted
            fld tbyte[rbx+rax*4]
            call rsi                    -- eg faddp
            jmp :opMathsToNi
    []

--/*
procedure :%opAdd(:%)
end procedure -- (for Edita/CtrlQ)
--*/
      ::opAddI
    [32]
        add ecx,eax
    [64]
        add rcx,rax
    []
        ret
      ::opAddF
        faddp
        ret
        
--constant e14ops = {"add","sub","div","mul",                   -- 1,2,3,4
--                 "remainder","floor","unary minus","not",     -- 5,6,7,8
--                 "and_bits","or_bits","xor_bits","not_bits",  -- 9,10,11,12
--                 "power","xor"}                               -- 13,14

      :%opAdd               -- [edi]:=ecx+eax
-------------
    [32]
        mov edx,:opAddI
        mov esi,:opAddF
    [64]
        mov rdx,:opAddI
        mov rsi,:opAddF
    []
        mov [e14code],1
        jmp :opMath

      :%opAddi              -- [edi]:=ecx+eax, no dealloc, builtin typecheck
--------------
    [32]
        mov edx,:opAddI
        mov esi,:opAddF
    [64]
        mov rdx,:opAddI
        mov rsi,:opAddF
    []
        mov [e14code],1
        jmp :opMathi

--/*
procedure :%opSub(:%)
end procedure -- (for Edita/CtrlQ)
--*/
      ::opSubI
    [32]
        sub ecx,eax
    [64]
        sub rcx,rax
    []
        ret
      ::opSubF
        fsubp
        ret
        
      :%opSub               -- [edi]:=ecx-eax
-------------
    [32]
        mov edx,:opSubI
        mov esi,:opSubF
    [64]
        mov rdx,:opSubI
        mov rsi,:opSubF
    []
        mov [e14code],2
        jmp :opMath

      :%opSubi              -- [edi]:=ecx-eax, no dealloc, builtin typecheck
--------------
    [32]
        mov edx,:opSubI
        mov esi,:opSubF
    [64]
        mov rdx,:opSubI
        mov rsi,:opSubF
    []
        mov [e14code],2
        jmp :opMathi

--/*
procedure :%opDiv(:%)
end procedure -- (for Edita/CtrlQ)
--*/
      ::opDivI
    [32]
        mov esi,ecx             -- save before we trash it...
        xchg eax,ecx
        add esp,4               -- trash return address (opMathIII)
        test ecx,ecx
        jz :e02atdb0            -- attempt to divide by 0
        cdq                     -- sign extend eax into edx
        idiv ecx
--      mov ecx,eax
        test edx,edx            -- check remainder is zero
--      jz opMathIII
        jnz @f
            mov ecx,eax
            jmp opMathIII
      @@:
        --
        -- two integer params but non-integer result
        --
        push esi
        fild dword[esp]
--      add esp,4
--    opDivNI:
        -- p2 in st1, p3 is integer:
--      push ecx
        mov [esp],ecx
--      test ecx,ecx
--      jz :e02atdb0
        fild dword[esp]
        add esp,4
    [64]
        mov rsi,rcx             -- save before we trash it...
        xchg rax,rcx
        add rsp,8               -- trash return address (opMathIII)
        test rcx,rcx
        jz :e02atdb0            -- attempt to divide by 0
        cqo                     -- sign extend rax into rdx
        idiv rcx
        test rdx,rdx            -- check remainder is zero
--      jz opMathIII
        jnz @f
            mov rcx,rax
            jmp opMathIII
      @@:
        --
        -- two integer params but non-integer result
        --
        push rsi
        fild qword[rsp]
        mov [rsp],rcx
        fild qword[rsp]
--      add rsp,4
        add rsp,8
    []
        fdivp
        jmp :%pStoreFlt

      ::opDiviI
    [32]
        mov esi,ecx             -- save before we trash it...
        xchg eax,ecx
        add esp,4               -- trash return address (opMathIII)
        test ecx,ecx
        jz :e02atdb0            -- attempt to divide by 0
        cdq                     -- sign extend eax into edx
        idiv ecx
--      mov ecx,eax
        xchg ecx,eax
        test edx,edx            -- check remainder is zero
        jz opMathiIII
        jmp :e01tcfeaxplusedxoverecxedi
--      --
--      -- two integer params but non-integer result
--      --
--      push esi
--      fild dword[esp]
--      add esp,4
----      opDivNI:
--      -- p2 in st1, p3 is integer:
--      push ecx
----        test ecx,ecx
----        jz :e02atdb0
--      fild dword[esp]
--      add esp,4
--      fdivp
--      jmp :%pStoreFlt
    [64]
        mov rsi,rcx             -- save before we trash it...
        mov r8,rax
        xchg rax,rcx
        add rsp,8               -- trash return address (opMathIII)
        test rcx,rcx
        jz :e02atdb0            -- attempt to divide by 0
        cqo                     -- sign extend rax into rdx
        idiv rcx
        mov rcx,rax
        test rdx,rdx            -- check remainder is zero
        jz opMathiIII
        jmp :e01tcfeaxplusedxoverecxedi
    []

      ::opDivF
        fdivp
        ret

      :%opDiv               -- [edi]:=ecx/eax
-------------
    [32]
        mov edx,:opDivI
        mov esi,:opDivF
    [64]
        mov rdx,:opDivI
        mov rsi,:opDivF
    []
        mov [e14code],3
        jmp :opMath

      :%opDivi              -- [edi]:=ecx/eax, no dealloc, builtin typecheck
--------------
    [32]
        mov edx,:opDiviI
        mov esi,:opDivF
    [64]
        mov rdx,:opDiviI
        mov rsi,:opDivF
    []
        mov [e14code],3
        jmp :opMathi

--/*
procedure :%opDivf(:%)
end procedure -- (for Edita/CtrlQ)
--*/

      ::opDivfI
---------------
    [32]
        mov esi,ecx                     -- save
        xchg eax,ecx
        test eax,eax
        js @f
            test ecx,ecx
--          jns :opDivfSubecxm1
            js :opDivfSubecxm1
      ::opDivfNormal
                mov eax,esi             -- (nop except when from opDivfSubecxm1)
                test ecx,ecx
                jz :e02atdb0pop
                cdq                     -- sign extend eax into edx
                idiv ecx
                mov ecx,eax
                ret
      @@:
        test ecx,ecx
--      jns :opDivfNormal           -- do nowt then
        js :opDivfNormal            -- do nowt then
      ::opDivfSubecxm1
        --
        -- one but not both is signed.
        -- floor(-a/b) is done as -floor((a-(b-1))/b),
        -- eg/ie floor(-3/2) is -2, not -1.
        --
--      add eax,1
--      sub eax,ecx
        lea esi,[eax+1]
        sub esi,ecx
        jno opDivfNormal
        --
        -- oops, load both short ints to FPU:
        --
        mov [esp],eax           -- (trashes the return to opMathIII)
        fild dword[esp]
        mov [esp],ecx
        fild dword[esp]
        add esp,4               -- (discard fild/ex-opMathIII)
        fdivp
        jmp :%pStoreFlt
    [64]
        mov rsi,rcx                     -- save
        xchg rax,rcx
        test rax,rax
        js @f
            test rcx,rcx
--          jns :opDivfSubecxm1
            js :opDivfSubecxm1
      ::opDivfNormal
                mov rax,rsi             -- (nop except when from opDivfSubecxm1)
                test rcx,rcx
                jz :e02atdb0pop
                cqo                     -- sign extend rax into rdx
                idiv rcx
                mov rcx,rax
                ret
      @@:
        test rcx,rcx
--      jns :opDivfNormal           -- do nowt then
        js :opDivfNormal            -- do nowt then
      ::opDivfSubecxm1
        --
        -- one but not both is signed.
        -- floor(-a/b) is done as -floor((a-(b-1))/b),
        -- eg/ie floor(-3/2) is -2, not -1.
        --
--      add rax,1
--      sub rax,rcx
        lea rsi,[rax+1]
        sub rsi,rcx
        jno opDivfNormal
        --
        -- oops, load both short ints to FPU:
        --
        mov [rsp],rax           -- (trashes the return to opMathIII)
        fild qword[rsp]
        mov [rsp],rcx
        fild qword[rsp]
        add rsp,8               -- (discard fild/ex-opMathIII)
        fdivp
        jmp :%pStoreFlt
    []

      ::opDivfF
    [32]
        fdivp
        call :%down53
        frndint
        call :%near53
    [64]
        fdivp
        call :%down64
        frndint
        call :%near64
    []
        ret

      :%opDivf              -- [edi]:=floor(ecx/eax) result may be integer, or atom (as floor(1e308/10) is 1e307)
--------------
    [32]
        mov edx,:opDivfI
        mov esi,:opDivfF
    [64]
        mov rdx,:opDivfI
        mov rsi,:opDivfF
    []
        jmp :opMath

      :%opDiv2              -- [edi]:=eax/2    -- eax is init int, result may be int or atom
--------------
    [32]
--DEV we may want an add esp,4 here??? (discard return addr) [ADDED]
--      add esp,4           -- discard return addr (opMathIII) NO!!
        mov edx,[edi]       -- target ref (old)
        sar eax,1           -- /2
        jc :opDiv2flt
        mov [edi],eax
        cmp edx,h4
        jle @f
            sub dword[ebx+edx*4-8],1
            jz :%pDealloc
      @@:
        ret

      ::opDiv2flt
        -- eax is result but missing a 0.5:
        mov ecx,[half]
        push eax
        fild dword[esp]
--DEV testme: -3/2 probably needs fsub... (suggested code completely untested)
--      test eax,eax
--      js :opDiv2fsub
--      fadd qword[ebx+ecx*4]
        fld qword[ebx+ecx*4]
        add esp,4
        faddp
        jmp :%pStoreFlt
--    ::opDiv2fsub
--      fsub qword[ebx+ecx*4]
--      jmp :%pStoreFlt
    [64]
--      add esp,8           -- discard return addr (opMathIII) NO!!
        mov rdx,[rdi]       -- target ref (old)
        sar rax,1           -- /2
        jc :opDiv2flt
        mov [rdi],rax
        mov r15,h4
        cmp rdx,r15
        jle @f
            sub qword[rbx+rdx*4-16],1
            jz :%pDealloc
      @@:
        ret

      ::opDiv2flt
        -- rax is result but missing a 0.5:
        mov rcx,[half]
        push rax
        fild qword[rsp]
--DEV testme: -3/2 probably needs fsub... (suggested code completely untested)
--      test rax,rax
--      js :opDiv2fsub
--      fadd tbyte[rbx+rcx*4]
        fld tbyte[rbx+rcx*4]
        add rsp,8
        faddp
        jmp :%pStoreFlt
--    ::opDiv2fsub
--      fsub tbyte[rbx+rcx*4]   (unsupported, fixme)
--      fld tbyte[rbx+rcx*4]
--      fsubp
--      jmp :%pStoreFlt
    []

--/*
procedure :%opMul(:%)
end procedure -- (for Edita/CtrlQ)
--*/

      ::opMulI
    [32]
        push eax
        imul ecx
        mov esi,edx
        cdq                 -- sign extend eax into edx
        cmp esi,edx         -- check for 32-bit result
        je @f
            fild dword[esp]
            mov [esp],ecx
            fild dword[esp]
            add esp,8       -- (also discard return to opMathIII)
            fmulp
            jmp :%pStoreFlt
      @@:
        add esp,4       -- discard the eax we saved, return to opMathIII
        mov ecx,eax
    [64]
        push rax
        imul rcx
        mov rsi,rdx
        cqo                 -- sign extend rax into rdx
        cmp rsi,rdx         -- check for 32-bit result
        je @f
            fild qword[rsp]
            mov [rsp],rcx
            fild qword[rsp]
            add rsp,16      -- (also discard return to opMathIII)
            fmulp
            jmp :%pStoreFlt
      @@:
        add rsp,8       -- discard the rax we saved, return to opMathIII
        mov rcx,rax
    []
        ret

      ::opMuliI
    [32]
        imul ecx
        mov ecx,edx
        cdq                     -- sign extend eax into edx
        cmp ecx,edx
        jne e01tcfediMul
        mov ecx,eax
    [64]
--      push rax
--      push rcx
        imul rcx
        mov rcx,rdx
        cqo                     -- sign extend rax into rdx
        cmp rcx,rdx
        jne e01tcfediMul
--      add rsp,16
        mov rcx,rax
    []
        ret

      ::opMulF
        fmulp
        ret

      :%opMul                   -- [edi]:=ecx*eax
-------------
    [32]
        mov edx,:opMulI
        mov esi,:opMulF
    [64]
        mov rdx,:opMulI
        mov rsi,:opMulF
    []
        mov [e14code],4
        jmp :opMath

      :%opMuli                  -- [edi]:=ecx*eax, no dealloc, builtin typecheck
--------------
    [32]
        mov edx,:opMuliI
        mov esi,:opMulF
    [64]
        mov rdx,:opMuliI
        mov rsi,:opMulF
    []
        mov [e14code],4
        jmp :opMathi

--/*
procedure :%opAndBits(:%)
end procedure -- (for Edita/CtrlQ)
--*/
      ::opAndBitsI
    [32]
        and ecx,eax
    [64]
        and rcx,rax
    []
        ret

      ::opAndBitsF
    [32]
        sub esp,8
        fistp qword[esp]
        mov ecx,[esp]
        fistp qword[esp]
        and ecx,[esp]
        add esp,12
        jmp :opMathIII
    [64]
        sub rsp,8
--DEV :%pLoadMint?
        fistp qword[rsp]
        mov rcx,[rsp]
        fistp qword[rsp]
        and rcx,[rsp]
        add rsp,16
        jmp :opMathIII
    []
        
      :%opAndBits       -- [edi]:=and_bits(ecx,eax) [inlined if ecx and eax are known to be integers]
-----------------
    [32]
        mov edx,:opAndBitsI
        mov esi,:opAndBitsF
    [64]
        mov rdx,:opAndBitsI
        mov rsi,:opAndBitsF
    []
        mov [e14code],9
        jmp :opMath

--/*
procedure :%opOrBits(:%)
end procedure -- (for Edita/CtrlQ)
--*/
      ::opOrBitsI
    [32]
        or ecx,eax
    [64]
        or rcx,rax
    []
        ret

      ::opOrBitsF
    [32]
        sub esp,8
        fistp qword[esp]
        mov ecx,[esp]
        fistp qword[esp]
        or ecx,[esp]
        add esp,12
        jmp :opMathIII
    [64]
        sub rsp,8
--DEV :%pLoadMint?
        fistp qword[rsp]
        mov rcx,[rsp]
        fistp qword[rsp]
        or rcx,[rsp]
        add rsp,16
        jmp :opMathIII
    []
        
      :%opOrBits        -- [edi]:=or_bits(ecx,eax) [inlined if ecx and eax are known to be integers]
-----------------
    [32]
        mov edx,:opOrBitsI
        mov esi,:opOrBitsF
    [64]
        mov rdx,:opOrBitsI
        mov rsi,:opOrBitsF
    []
        mov [e14code],10
        jmp :opMath

--/*
procedure :%opXorBits(:%)
end procedure -- (for Edita/CtrlQ)
--*/
      ::opXorBitsI
    [32]
        xor ecx,eax
    [64]
        xor rcx,rax
    []
        ret

      ::opXorBitsF
    [32]
        sub esp,8
        fistp qword[esp]
        mov ecx,[esp]
        fistp qword[esp]
        xor ecx,[esp]
        add esp,12
        jmp :opMathIII
    [64]
        sub rsp,8
--DEV :%pLoadMint?
        fistp qword[rsp]
        mov rcx,[rsp]
        fistp qword[rsp]
        xor rcx,[rsp]
        add rsp,16
        jmp :opMathIII
    []
        
      :%opXorBits       -- [edi]:=xor_bits(ecx,eax) [inlined if ecx and eax are known to be integers]
-----------------       -- (see builtins\VM\pXor.e for a = b xor c, the logic op)
    [32]
        mov edx,:opXorBitsI
        mov esi,:opXorBitsF
    [64]
        mov rdx,:opXorBitsI
        mov rsi,:opXorBitsF
    []
        mov [e14code],11
        jmp :opMath

      }
