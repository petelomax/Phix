--
-- pUnassigned.e
--
--  Temp. (move to pFEH?)

include builtins\VM\puts1.e         -- low-level console i/o
--DEV?
--include builtins\VM\pStack.e

--constant eBadRetf = "eBadRetf called, return address is #\n"

--#ilASM{ jmp :%opRetf
#ilASM{ jmp :!opCallOnceYeNot

    :%pUnassigned   -- aka e92vhnbaavesiesp
-----------------
        -- esi is var no for e92,
        -- edi is var no for type check error 
        -- [ie: if [esi]==h4 then (jz/jnz valid)
        --          varname(esi)&" has not been assigned a value"
        --      else
        --          "type check error:"&varname(edi)&" is "&sprint([edi])
        --           (in which case [esi] and [edi] should be the same, btw)
        --      end if]
    [32]
        pop edx
    [64]
        pop rdx
    []
        jne @f
            -- [var]==h4, esi is varno
            mov al,92   -- e92vhnbaav(esi)
            sub edx,1
            jmp :!iDiag
            int3
      @@:
            -- type check error (edi is varno)
--          mov al,110  -- e110tce(ecx)
            mov al,1    -- e01tcf(ecx)
            mov ecx,edi -- DEV (minor)
            sub edx,1
            jmp :!iDiag
            int3

--/*
  @@:
    sub eax,1
    mov [ep1],esi
    mov [era],eax
    mov al,92
    jmp enumbset

    trc e92movti
e92movti:
;;!!    trc e92movti    // wrecks the jz below!
;   reg = loadReg(p2)                               -- mov reg,[p2]
;   cmp_h4(reg)                                     -- cmp reg,h4
;   storeReg(reg,p1,1,0)                            -- mov [p1],reg
;   emitHex2(jl_rel8,?5)                            -- jl @f
;   mov esi,p2 (if reg can be = h4)                 276         BE imm32        mov esi,imm32
;   mov edx,p1                                      272         BA imm32        mov edx,imm32
;   call e92movti                                   350         E8 rel32        call rel32

;   mov eax,[esp]
;   mov esi,[eax-21]
;   jz e92vhnbaavesiesp     ; variable has not been assigned a value...
    pop eax
    jz @b                   ; variable has not been assigned a value...
    ;
    ; else type check error:
    ;
;   mov edx,[eax-11]
;   sub eax,1
    mov [ep1],edx
    mov [era],eax
;if newEBP (this may be a better idea!)
;   mov al,1    -- (old code)
;else
    mov al,110 -- (old code)
;end if
    jmp enumbset
--*/
    :%pBadRetf
        [32]
--          mov edi,[eBadRetf]          -- "eBadRetf called, return address is #"
--          call :%puts1
            mov edx,[esp]
            mov al,13                   -- e13ateafworav
            sub edx,1
--          push 1                      -- cr
--          call :%puthex32
        [64]
--          mov rdi,[eBadRetf]          -- "eBadRetf called, return address is #"
--          call :%puts1
--          mov rdx,[rsp]
--          push 1                      -- cr
--          call :%puthex64
            mov rdx,[esp]
            mov al,13                   -- e13ateafworav
            sub rdx,1
        []
            jmp :!iDiag
            int3

    :%e01tcfAddiii
--  :%pAddiii
    [32]
        -- [edi] has blown 31-bit int; store as float and typecheck
        pop edx
        fild dword[edi]
        sub edx,1
        mov [edi],ebx
        -- edi is ep2, edx is era
        mov ecx,edi
    [64]
        -- [rdi] has blown 63-bit int; store as float and typecheck
        pop rdx
        fild qword[rdi]
        sub rdx,1
        mov [rdi],rbx
        -- rdi is ep2, rdx is era
        mov rcx,rdi
    []
        call :%pStoreFlt
        mov al,110  -- e110tce(ecx)
--      mov al,1    -- e01tcf(ecx)
    [32]
        mov ecx,edi
    [64]
        mov rcx,rdi
    []
        jmp :!iDiag
        int3

    :%e01tcfediDiv
        -- calculate [edi]=(eax*ecx+edx)/ecx, as a float, and tcf it.
    [32]
        push edx
        fild dword[esp]
        mov [esp],eax
        fild dword[esp]
        mov [esp],ecx
        fild dword[esp]
        fmulp
        faddp
        fild dword[esp]
        fdivp
        add esp,4
        call :%pStoreFlt
        pop edx
        mov ecx,edi     
        mov al,110  -- e110tcf(ecx)
        sub edx,1
        jmp :!iDiag
    [64]
        push rdx
        fild qword[rsp]
        mov [rsp],rax
        fild qword[rsp]
        mov [rsp],rcx
        fild qword[rsp]
        fmulp
        faddp
        fild qword[rsp]
        fdivp
        add rsp,8
        call :%pStoreFlt
        pop rdx
        mov rcx,rdi     
        mov al,110  -- e110tcf(ecx)
        sub rdx,1
        jmp :!iDiag
    []
        int3
--  :%pRTErf
--      int3
--DEV to go:
    :%pRTErn
        -- error code in al,
        -- ep1 in edi,
        -- ep2 in esi
        pop edx -- era
        sub edx,1
        jmp :!iDiag
        int3
    :%pDiv0 
--      int3
    :%e02atdb0
--      int3
--or
--!/*
    [32]
        pop edx
--      mov edi,ebp
--      mov esi,esp
        mov al,2
        add edx,-1
    [64]
        pop rdx
--      mov rdi,rbp
--      mov rsi,rsp
        mov al,2
        add rdx,-1
    []
        jmp :!iDiag
        int3
--!*/

    :%e01tcfDivi2   -- (opDivi2)
--      int3
    [32]
        push eax
--  pop ecx             ; return address
        fild dword[esp]
--  mov [era],ecx
--  fadd [half]
        mov edi,edx
        add esp,4
    [64]
        push rax
        fild qword[rsp]
        mov rdi,rdx
        add rsp,8
    []
        fld1
        fld1
        fld1
        faddp
        fdivp
        faddp
        jmp @f

--;calling convention:
--; mov eax,[p2]
--; mov edx,p1
--; sar eax,1
--; jnc @f
--; call :%e01tcfDivi2  -- type check error, <p1> is xxx.5
    :%e01tcfediMul -- (opMuliii)
        [32]
-- this might want to be push eax:ecx; fild qword[esp]...
            push eax
            fild dword[esp]
            add esp,4
          @@:
            call :%pStoreFlt        -- [edi]!=st0
            pop edx
            mov al,110              -- e110tce(ecx)
            sub edx,1
            mov ecx,edi
            jmp :!iDiag
            int3
        [64]
            push rax
            fild qword[rsp]
--  ::e01tcfst0rdi
            add rsp,8
          @@:
            call :%pStoreFlt        -- [rdi]!=st0
            pop rdx
            mov al,110              -- e110tce(ecx)
            sub rdx,1
            mov rcx,rdi
            jmp :!iDiag
            int3
        []
--;calling convention:
--; mov ecx,[p3]
--; mov eax,[p2]
--; imul ecx
--; mov edi,p1
--; mov ecx,edx
--; cdq ; eax-> edx:eax
--; cmp ecx,edx ; blown 32 bits?
--; je @f
--; call :%e01tcfediMul
--; mov esi,eax
--; shl esi,1
--; jo (call :%e01tcfediMul) ; blown 31 bits?
--; mov [edi],eax

    :!opCallOnceYeNot
-- (new error code)
        int3

      }

