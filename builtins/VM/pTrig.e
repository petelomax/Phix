--
-- pTrig.e
-- =======
--
--  implements :%opCos, :%opSin, :%opTan, :%opArcTan, :%opLog, :%opSqrt,
--

#ilASM{ jmp :%opRetf

--DEV: (make it one error code)
    ::e2803atsmbausq
        int3

  ::LoadFlt
    [32]
        cmp eax,h4
        jl @f
            cmp byte[ebx+eax*4-1],0x12
            jne :e2803atsmbausq
            fld qword[ebx+eax*4]
            ret
      @@:
        push eax
        fild dword[esp]
        add esp,4
        ret
    [64]
        mov r15,h4
        cmp rax,r15
        jl @f
            cmp byte[rbx+rax*4-1],0x12
            jne :e2803atsmbausq
            fld tbyte[rbx+rax*4]
            ret
      @@:
        push rax
        fild qword[rsp]
        add rsp,8
        ret
    []

  :%opCos
---------
        -- calling convention
        -- lea edi/rdi,[dest]
        -- mov eax/rax,[p1] (opUnassigned)
        -- call :%opCos/Sin/Tan/
        call :LoadFlt
        fcos
        jmp :%pStoreFlt

  :%opSin
---------
        call :LoadFlt
        fsin
        jmp :%pStoreFlt

  :%opTan
---------
        call :LoadFlt
        fptan               -- st0=tan(st0); push 1.0
        fstp st0            -- get rid of the 1.0!
        jmp :%pStoreFlt

  :%opArcTan
------------
        call :LoadFlt
        fld1
        fpatan              -- st1=arctan(st1/st0)
        jmp :%pStoreFlt

  :%opLog
---------
        fld1                -- load 1
        call :LoadFlt
        fyl2x               -- st0=1*log2(f)
        fldl2e              -- load log2(e)
        fdivp st1,st0       -- gives ln(f)
        jmp :%pStoreFlt

  :%opSqrt
----------
        call :LoadFlt
    --DEV check for -ve numbers:
    --(untried)
    --  fldz
    --  fcomp
    --  fnstsw ax
    --  test ah,0x01
    --  jnz e??atsmnbn      -- argument to sqrt must not be negative (C=1 for jb)
                            -- or attempt to get square root of negative number
        fsqrt               -- st0=sqrt(st0)
        jmp :%pStoreFlt

      }

