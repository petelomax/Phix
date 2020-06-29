--
-- pPower.e
-- ========
--
--  Implements :%opPow, [edi]:=power(eax,ecx) (where [edi],eax,ecx are all int/atom [not sequence])
--

-- Quick analysis of int:=power(int,int) that we could do in the ALU. Originally written because power(3,5) is non-exact on X64, 
--  which I have now conceded as not important, it looks like "+/-177,<=4" should have been "+/-181,<=4" but otherwise seems to
--  be a perfectly reasonable compromise. 
--  What I have not done is test performance: is ALU better/worse than fyl2x/frndint/f2xm1/fscale etc? If it is (which I doubt) 
--  then some more power(x,2..3) optimisations might just have merits.
--power(<atm>,0) [already handled separately]
--power(<int>,1) [worthwhile handling separately?]
--power(-32767..32767,2)
--power(-1024..1023,3)
--power(-181..181,4)    -- (1073283121 = #3FF90031)     [who knows where +/-177 came from?]
--power(-64..63,5)
--power(-31..31,6)
--power(-19..19,7)
--power(-13..13,8)
--power(-10..10,9)
--power(-7..7,10)
--power(-6..6,11)
--power(-5..5,12)
--power(-4..4,13..14)
--power(-4..3,15)
--power(-3..3,16..18)
--power(-2..2,19..29) [power(2,0..29) already handled separately]
--power(-1..1,>=1) [worthwhile handling separately?]
--power(0,x) [worthwhile handling separately?]

--#with debug   -- (no effect: I had to disable the without in pprntfN.e instead, last time I tried)

include VM\pHeap.e  -- :%pDealloc/:%pStoreFlt
include VM\pFPU.e   -- :%down53 etc

#ilASM{ jmp :%opRetf

--DEV
--      ::e102cr0tple0   -- cannot raise 0 to power<=0
--          int3
        ::e1413sopa
            int3
        ::e34pfu
            int3
--      ::e35pfo
--          int3
--      ::e54atrnntnip
--          int3

--/*
procedure :%opPow(:%)
end procedure -- (for Edita/CtrlQ)
--*/
  :%opPow
---------
    [32]
        --calling convention:
        --  lea edi,[p1]        -- target
        --  mov ecx,[p3]        -- ref p3 (opUnassigned)
        --  mov eax,[p2]        -- ref p2 (opUnassigned)
        --  call :%opPow        -- [edi] := power(eax,ecx)
        --    all registers trashed unless result is integer, left in eax

        -- handle power(2,0..29) as 1 shl cl.
        cmp eax,2
        jne :notPow2
        cmp ecx,29
        ja :notPow2
        mov eax,1
        mov edx,[edi]
        shl eax,cl
      ::opPowRet
        cmp edx,h4
        mov [edi],eax
        jle @f
            sub dword[ebx+edx*4-8],1
            jz :%pDealloc
      @@:
        ret

      ::notPow2

        -- handle power(x,0) as 1, except for power(0,0) and power(<seq>,0) which are errors
        test ecx,ecx
        jnz :notPow0
        -- 3/11/15 let power(0,0) be 1
--      test eax,eax
--      jz :e102cr0tple0
        cmp eax,h4
        jl @f
            cmp byte[ebx+eax*4-1],0x12
            jne :e1413sopa
      @@:
        mov edx,[edi]
        mov eax,1
        jmp :opPowRet
    
      ::notPow0

        -- power(-177..177,1..4) can be done using mul to give an integer result...
--PL 181^4/7^10/10^9
        cmp ecx,4
        mov edx,-181
        jbe :testedx
--      cmp ecx,10
--      mov edx,-7
--      jbe :testrdx
        cmp ecx,9
        mov edx,-10
        ja :notIntRes
--      cmp eax,-177
      ::testedx
        cmp eax,edx
        jl :notIntRes
--      cmp eax,177
        neg edx
        cmp eax,edx
        jg :notIntRes

        mov esi,eax
        mov edx,[edi]
        sub ecx,1
        jz :opPowRet
      ::IntPowMulLoop
        imul esi
        sub ecx,1
        jnz IntPowMulLoop
        mov edx,[edi]
        jmp :opPowRet

      ::notIntRes
        cmp ecx,h4
        jge :opPowp3Flt
            push ecx
            fild dword[esp]
            add esp,4
            jmp @f
      ::opPowp3Flt
            cmp byte[ebx+ecx*4-1],0x12
            jne :e1413sopa
            fld qword[ebx+ecx*4]
      @@:
        cmp eax,h4
        jge :opPowp2Flt
            push eax
            fild dword[esp]
            add esp,4
            jmp @f
      ::opPowp2Flt
            cmp byte[ebx+eax*4-1],0x12
            jne :e1413sopa
            fld qword[ebx+eax*4]
      @@:

--    ::opPowNN
        fldz
        fcomp
        fnstsw ax
--DEV  test ah,0x41 jz (C=0 and Z=0 for ja), not sure any help since wd need test ah,0x40 next...
        sahf
        ja :opPowP2neg      -- jump if 0 > (above) p2
        fxch
        jne @f              -- if p2=0 then:
            fldz
            fcompp
            fnstsw ax
--DEV test ah,0x01/jnz (C=1 for jb)
            sahf
            jb :%pStoreFlt
--          jmp :e102cr0tple0   -- cannot raise 0 to power<=0
            pop edx
            mov al,102          -- e102cr0tple0: cannot raise 0 to power<=0
            sub edx,1
            jmp :!iDiag
            int3
      @@:
        fxch
      ::opPowP3even
        fyl2x               -- st1*=log2(st0); pop st0. st0 must be >0 ; ie log2(p2)*p3
        fld st0             -- duplicate st0
        frndint             -- round it to an integer   ;DEV SLOW!
        fsub st1,st0        -- leave only fractional portion in st1
        fxch st1            -- st1=int(p2*log2(p3)); st0=p2*log2(p3)-st1
        f2xm1               -- get the fractional power of 2 (minus 1). st0 = 2^^st0-1 st0 must be in the range -1.0 to +1.0
        fld1
        faddp               -- get rid of that minus 1
        fscale              -- multiply by 2^int(p2*log2(p3)) ; similar to shl 2 being same as mul 4

      ::opPowCont
        fxch
        fstp st0            -- discard junk
--  mov edx,edi
-- 30/1:
    -- check for under/overflow:
--  cmp [sferr],0
--  je @f
--  mov [sferr],0
--minatm        dq 0xFFEFFFFFFFFFFFFF   ; -1.7976931348623146e308
--maxatm        dq 0x7FEFFFFFFFFFFFFF   ; +1.7976931348623146e308
--DEV test this!
        push 0xFFEFFFFF
        push -1
--  fld qword[minatm]
        fld qword[esp]  -- minatm 
        fcomp
        fnstsw ax
--DEV Agner sez (look this up!) test ax,0x40 jz StoreFlt64 (nb test inverted)
--  sahf
--  ja e34pfu           -- power function underflow
        test ah,0x41
        jz :e34pfu          -- power function underflow
        mov dword[esp+4],0x7FEFFFFF
--  fld qword[maxatm]
        fld qword[esp]  -- maxatm
        add esp,8
        fcomp
        fnstsw ax
--DEV Agner sez (look this up!) test ax,0x40 jz StoreFlt64 (nb test inverted)
--  sahf
--  jae StoreFlt
        test ah,0x01
        jz :%pStoreFlt
--      jmp :e35pfo         -- power function overflow
        pop edx
        mov al,35           -- e35pfo power function overflow
        sub edx,1
        jmp :!iDiag
        int3
--  mov [sferr],1
-- @@:

--  jmp StoreFlt

      ::opPowP2neg
        fabs                -- p2 = |p2|, in this case same as *-1
        fld st1             -- duplicate p3
        frndint             -- DEV SLOW!
        fcom st2
        fnstsw ax
        sahf
--      jne :e54atrnntnip   -- attempt to raise negative number to non-integer power
        je @f
            pop edx
            mov al,54       -- e54atrnntnip: attempt to raise negative number to non-integer power
            sub edx,1
            jmp :!iDiag
            int3
      @@:
        sub esp,4
        fistp dword[esp]
        pop eax
        shr eax,1
        jnc :opPowP3even
        fyl2x               -- st1*=log2(st0); pop st0. st0 must be >0 ; ie log2(p2)*p3
        fld st0             -- duplicate st0
        frndint             -- round it to an integer   ; DEV SLOW!
        fsub st1,st0        -- leave only fractional portion in st1
        fxch st1            -- st1=int(p2*log2(p3)); st0=p2*log2(p3)-st1
        f2xm1               -- get the fractional power of 2 (minus 1). st0 = 2^^st0-1 st0 must be in the range -1.0 to +1.0
        fld1
        faddp               -- get rid of that minus 1
        fscale              -- multiply by 2^int(p2*log2(p3)) ; similar to shl 2 being same as mul 4
        fchs                -- change sign!
        jmp :opPowCont       
    [64]
        --calling convention:
        --  lea rdi,[p1]        -- target
        --  mov rcx,[p3]        -- ref p3 (opUnassigned)
        --  mov rax,[p2]        -- ref p2 (opUnassigned)
        --  call :%opPow        -- [rdi] := power(rax,rcx)
        --    all registers trashed unless result is integer, left in rax

        mov r15,h4
        -- handle power(2,0..61) as 1 shl cl. (29..61 untested!)
        cmp rax,2
        jne :notPow2
        cmp rcx,61
        ja :notPow2
        mov rax,1
        mov rdx,[rdi]
        shl rax,cl
      ::opPowRet
        cmp rdx,r15
        mov [rdi],rax
        jle @f
            sub qword[rbx+rdx*4-16],1
            jz :%pDealloc
      @@:
        ret

      ::notPow2

        -- handle power(x,0) as 1, except for power(0,0) and power(<seq>,0) which are errors
        test rcx,rcx
        jnz :notPow0
        -- 3/11/15 let power(0,0) be 1
--      test rax,rax
--      jz :e102cr0tple0
        cmp rax,r15
        jl @f
            cmp byte[rbx+rax*4-1],0x12
            jne :e1413sopa
      @@:
        mov rdx,[rdi]
        mov rax,1
        jmp :opPowRet
    
      ::notPow0

--DEV new valid int range not yet done...
        -- power(-177..177,1..4) can be done using mul to give an integer result...
--PL: 46340^4/73^10/10^17:
        cmp rcx,4
        mov rdx,-46340
        jbe :testrdx
--      cmp rcx,10
--      mov rdx,-73
--      jbe :testrdx
        cmp rcx,17
        mov rdx,-10
        ja :notIntRes
--      cmp rax,-177
      ::testrdx
        cmp rax,rdx
        jl :notIntRes
--      cmp rax,177
        neg rdx
        cmp rax,rdx
        jg :notIntRes

        mov rsi,rax
        mov rdx,[rdi]
        sub rcx,1
        jz :opPowRet
      ::IntPowMulLoop
        imul rsi
        sub rcx,1
        jnz IntPowMulLoop
        mov rdx,[rdi]
        jmp :opPowRet

      ::notIntRes
        cmp rcx,r15
        jge :opPowp3Flt
            push rcx
            fild qword[rsp]
            add rsp,8
            jmp @f
      ::opPowp3Flt
            cmp byte[rbx+rcx*4-1],0x12
            jne :e1413sopa
            fld tbyte[rbx+rcx*4]
      @@:
        cmp rax,r15
        jge :opPowp2Flt
            push rax
            fild qword[rsp]
            add rsp,8
            jmp @f
      ::opPowp2Flt
            cmp byte[rbx+rax*4-1],0x12
            jne :e1413sopa
            fld tbyte[rbx+rax*4]
      @@:

--    ::opPowNN
        fldz
        fcomp
        fnstsw ax
--DEV  test ah,0x41 jz (C=0 and Z=0 for ja), not sure any help since wd need test ah,0x40 next...
        sahf
        ja :opPowP2neg      -- jump if 0 > (above) p2
        fxch
        jne @f              -- if p2=0 then:
            fldz
            fcompp
            fnstsw ax
--DEV test ah,0x01/jnz (C=1 for jb)
            sahf
            jb :%pStoreFlt
--          jmp :e102cr0tple0   -- cannot raise 0 to power<=0
            pop rdx
            mov al,102          -- e102cr0tple0: cannot raise 0 to power<=0
            sub rdx,1
            jmp :!iDiag
            int3
      @@:
        fxch
      ::opPowP3even
        fyl2x               -- st1*=log2(st0); pop st0. st0 must be >0 ; ie log2(p2)*p3
        fld st0             -- duplicate st0
        frndint             -- round it to an integer   ;DEV SLOW!
        fsub st1,st0        -- leave only fractional portion in st1
        fxch st1            -- st1=int(p2*log2(p3)); st0=p2*log2(p3)-st1
        f2xm1               -- get the fractional power of 2 (minus 1). st0 = 2^^st0-1 st0 must be in the range -1.0 to +1.0
        fld1
        faddp               -- get rid of that minus 1
        fscale              -- multiply by 2^int(p2*log2(p3)) ; similar to shl 2 being same as mul 4

      ::opPowCont
        fxch
        fstp st0            -- discard junk
        -- check for under/overflow:
--DEV test this!
        mov rax,0x0000FFFE
        mov rcx,-1
        push rax
        push rcx
        fld tbyte[rsp]  -- minatm 
        fcomp
        fnstsw ax
--DEV Agner sez (look this up!) test ax,0x40 jz StoreFlt64 (nb test inverted)
--  sahf
--  ja e34pfu           -- power function underflow
        test ah,0x41
        jz :e34pfu          -- power function underflow
        mov rax,0x00007FFE
        mov [rsp+8],rax
        fld tbyte[rsp]  -- maxatm
        add rsp,16
        fcomp
        fnstsw ax
--DEV Agner sez (look this up!) test ax,0x40 jz StoreFlt64 (nb test inverted)
--  sahf
--  jae StoreFlt
        test ah,0x01
        jz :%pStoreFlt
--      jmp :e35pfo         -- power function overflow
        pop rdx
        mov al,35           -- e35pfo power function overflow
        sub rdx,1
        jmp :!iDiag
        int3

--  mov [sferr],1
-- @@:

--  jmp StoreFlt

      ::opPowP2neg
        fabs                -- p2 = |p2|, in this case same as *-1
        fld st1             -- duplicate p3
        frndint             -- DEV SLOW!
        fcom st2
        fnstsw ax
        sahf
--      jne :e54atrnntnip   -- attempt to raise negative number to non-integer power
        je @f
            pop rdx
            mov al,54       -- e54atrnntnip: attempt to raise negative number to non-integer power
            sub rdx,1
            jmp :!iDiag
            int3
      @@:
        sub rsp,8
        fistp qword[rsp]
        pop rax
        shr rax,1
        jnc :opPowP3even
        fyl2x               -- st1*=log2(st0); pop st0. st0 must be >0 ; ie log2(p2)*p3
        fld st0             -- duplicate st0
        frndint             -- round it to an integer   ; DEV SLOW!
        fsub st1,st0        -- leave only fractional portion in st1
        fxch st1            -- st1=int(p2*log2(p3)); st0=p2*log2(p3)-st1
        f2xm1               -- get the fractional power of 2 (minus 1). st0 = 2^^st0-1 st0 must be in the range -1.0 to +1.0
        fld1
        faddp               -- get rid of that minus 1
        fscale              -- multiply by 2^int(p2*log2(p3)) ; similar to shl 2 being same as mul 4
        fchs                -- change sign!
        jmp :opPowCont       
    []
      }

--SUG:
--/*
"Matt C." <spam...@crayne.org> wrote in message 
news:n4ags05qqkh84ivon1s5n8t2go5f2k978j@4ax.com...
- show quoted text -
Here is some code which will handle the exponentation x^y where y is a 
positive integer or zero. Extrapolation to negative integers is easy: take 
the reciprocal of x if y is negative and then take the absolute value of y. 
If you need fractional exponents, then Terje's method is probably best.

double exp(double base, unsigned int exp)
{
 double acc = 1.0;

 while(exp > 0)
 {
  if (exp & 1)
   acc *= base;
  base *= base;
  exp >>= 1;
 }

 return acc;
}

This code will not lose any bits of precision except what is inherent in 
floating-point. A simple assembly translation:

_exp:
 fld1
 fld qword [esp+4]
 mov eax, [esp+12]

 ; st1 = acc
 ; st0 = base
 test eax, eax
 jz .done

.top:
 shr eax, 1
 jnc .skip
 fmul st1, st0

.skip:
 fmul st0, st0
 jnz .top

.done:
 ffreep st0
 ; st0 = acc
 ret

If you're after speed, unrolling that should give a significant speed gain. 
The critical path is going to be 32 FP multiplies which is 160 cycles on a 
P4. You can cut that to 128 (20% faster) if you use SSE/SSE2 instead. The 
disadvantage to SSE/SSE2 is limited compatibility.

-Matt

Assuming unpredictable bit patterns in the exponent, a branchless 
version might be faster:

   mov eax,[exp]                ; Exponent
   fld1                        ; Accumulator, return value if (exp==0)
   fld [base]                ; Current power of base
   test eax,eax                ; exp > 0 ?
    jz done
next:
   fld1                        ; Multiplicator for zero power
   shr eax,1                ; Sets carry if (exp & 1), zero flag if done

   fcmovc st,st(1)        ; Overwrite 1.0 with base^power if carry

   fmulp st(2),st        ; acc *= (exp & 1)? base : 1.0;
   fmul st,st                ; base *= base;
    jnz next
done:
   fstp st                ; FPOP to get rid of base power
   ret

Since the two multiplications are independent, this version could run in 
just one or two cycles more than the time for a single FMUL, and do so 
without any lost time due to branches.

--*/

