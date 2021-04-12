--
-- pRand.e
-- =======
--
--  implements :%opSetRand, :%opRand
--
--DEV this should probably be re-implemented as a standard include (builtins\rand.e, up one level), and removed from the optable.
-- (with :>Rand0 ---> integer rinit=0)
--
--DEV/SUG:
--atom rseed
-- The original rseed was a "dd 0" in the fasm stub source; I think I went with [ds+4] to avoid issues of >31-bit ints <=> floats,
--  but that would have been before pHeap.e's pStoreMint|pLoadMint existed.

include VM\pHeap.e  -- :%pDealloc/:%pStoreFlt
--include VM\pFPU.e -- :%down53 etc

#ilASM{ jmp :%opRetf

--DEV:
--  ::e29atsrmba        -- argument to set_rand must be atom
--      int3
--  ::e2801atrmbausq    -- argument to rand() must be an atom (use sq_rand?)
--      int3
--  ::e27atrmbge1       -- argument to rand must be >=1
--      int3

  :>Rand0
---------
        rdtsc
        xor eax,0x92D68CA2
      @@:
        mov dword[ds+4],eax
        ret

--/*
procedure :%opSetRand(:%)
end procedure -- (for Edita/CtrlQ)
--*/
  :%opSetRand       -- [edi] := time()
-------------
--  [32]
        --calling convention:
        --  mov eax,[p1]        -- seed value (opUnassigned)
        --  call :%opSetRand    -- set_rand(eax)
--      cmp eax,h4
--      jae :e29atsrmba -- argument to set_rand must be atom
--      jl @f
--          cmp byte[ebx+eax*4-1],0x12
--          jne :e29atsrmba -- argument to set_rand must be atom
--          fld qword[ebx+eax*4]
--          lea rdi,[rseed]
--          jmp :%pStoreFlt
--    @@:
--      push eax
--      fild dword[esp]
--      add esp,4
--      lea rdi,[rseed]
--      jmp :%pStoreFlt
--      mov [rseed],eax
--      call :%pLoadMint
--      mov dword[ds+4],eax
--  [64]
        --calling convention:
        --  mov rax,[p1]        -- seed value (opUnassigned)
        --  call :%opSetRand    -- set_rand(rax)
--      mov r15,h4
--      cmp rax,r15
--      jae :e29atsrmba -- argument to set_rand must be atom
--      mov [rseed],rax
--  []
        call :%pLoadMint
        jmp @b
--      mov dword[ds+4],eax
--      ret

--/*
procedure :%opRand(:%)
end procedure -- (for Edita/CtrlQ)
--*/
  :%opRand      -- [edi] := rand(eax)
-------------
    [32]
        --calling convention:
        -- lea edi,[res]    -- target addr
        -- mov eax,[p1]     -- range
        -- call :%opRand    -- [edi] = rand(eax)
--16/12/16
        call :%pLoadMint -- (eax:=(int32)eax; [edx:=hi_dword], edi preserved)
--/*
        cmp eax,h4  --DEV :%pLoadMint (test this, eg on RDS Eu, rand(0.5) is an error, rand(2.5)==rand(2))
--      jae :e2801atrmbausq
        jl @f
            pop edx
            mov al,79       -- e79atrmbi,  "argument to rand() must be an integer\n"
            sub edx,1
            jmp :!iDiag
            int3
--          cmp byte[ebx+eax*4-1],0x12
--          jne :e2801atrmbausq -- argument to rand() must be an atom (use sq_rand?)
--          fld qword[ebx+eax*4]
--          sub esp,8
--          call :%down53
--          fistp qword[esp]
--          call :%near53
--          mov eax,[esp]
--          add esp,8
      @@:
--*/
        cmp eax,1
--      jle :opRand1                -- rand(1) is always 1
        jbe :opRand1                -- rand(1) is always 1
        mov esi,eax
--DEV this is not thread safe... so why not just recalc every time?
--/*
        cmp eax,[rplim]
        je :rslim
        mov [rplim],eax
        sub eax,1
        mov ecx,-1
      ::rcshft                      -- count shift
        add ecx,1
        shl eax,1
        jnc :rcshft
        mov [rshft],ecx
--*/
        push eax
        sub eax,1
        mov ecx,-1
      ::rcshft                      -- count shift
        add ecx,1
        shl eax,1
        jnc :rcshft
        mov [esp],ecx
      ::rslim                       -- same limit

        --
        -- OK, this is the bit that actually generates random eax:
        --
--      mov eax,[rseed]
        mov eax,dword[ds+4]
        mov ecx,eax
        shl ecx,13
        xor eax,ecx
        mov ecx,eax
        shr ecx,17
        xor ecx,214013  -- PL
        xor eax,ecx
        mov ecx,eax
        shl ecx,5
--      mov edx,#3FFFFFFF
        xor eax,ecx
--      and eax,#3FFFFFFF
--      mov [rseed],eax
--      and edx,eax
--      mov [rseed],edx
        mov dword[ds+4],eax
        --
        -- check for bias and retry if necessary
        -- (scrambling/smearing/shifting bits gives a reasonably even 
        --  spread/distribution over some given power of two. Taking
        --  a remainder or similar would introduce bias, instead just 
        --  ignore/loop for any values outside the required range.
        --  The worst case scenario is to loop just shy of 50% of the 
        --  time, and doubling an average of ~15 clocks matters not.)
        --
--      mov ecx,[rshft]
        mov ecx,[esp]
        shr eax,cl
        cmp eax,esi
        --jge showerror -- used as test for rand(128) and rand(1024).
--21/1/18:
--      jge :rslim
        jae :rslim
        add eax,1
        pop ecx
--      cmp eax,h4
--      jl :opRandStore
--          push eax
----            mov edx,edi         -- address of p1 (target)
--          fild dword[esp]
--          add esp,4
--          jmp :%pStoreFlt
      ::opRandStore
        push ebx
        push eax
        fild qword[esp]
        add esp,8
        jmp :%pStoreFlt

      ::opRand1
--      jl :e27atrmbge1     -- argument to rand must be >=1
        je :opRandStore
            pop edx
            mov al,27       -- e27atrmbge1, argument to rand must be >=1
            sub edx,1
            jmp :!iDiag
            int3
--    ::opRandStore
--      jmp :%pStoreMint
--      mov edx,[edi]
--      xor ebx,ebx
--      mov [edi],eax
--      cmp edx,h4
--      jle @f
--          sub dword[ebx+edx*4-8],1
--          jz :%pDealloc
--    @@:
--      ret
    [64]
        --calling convention:
        -- lea rdi,[res]    -- target addr
        -- mov rax,[p1]     -- range
        -- call :%opRand    -- [rdi] = rand(rax)
        call :%pLoadMint    -- (rax:=(int64)rax; rdi preserved, r15:=h4)
--/*
        mov r15,h4
        cmp rax,r15
--      jae :e2801atrmbausq
        jl @f
            pop rdx
            mov al,79       -- e79atrmbi,  "argument to rand() must be an integer\n"
            sub rdx,1
            jmp :!iDiag
            int3
--          cmp byte[ebx+eax*4-1],0x12
--          jne :e2801atrmbausq -- argument to rand() must be an atom (use sq_rand?)
--          fld qword[ebx+eax*4]
--          sub esp,8
--          call :%down53
--          fistp qword[esp]
--          call :%near53
--          mov eax,[esp]
--          add esp,8
      @@:
--*/
        cmp rax,1
--      jle :opRand1                -- rand(1) is always 1
        jbe :opRand1                -- rand(1) is always 1
        mov rsi,rax
--/*
        cmp rax,[rplim]
        je :rslim
        mov [rplim],rax
        sub rax,1
        mov rcx,-1
      ::rcshft                      -- count shift
        add rcx,1
        shl rax,1
        jnc :rcshft
        mov [rshft],rcx
--*/
        push rax
        sub rax,1
        mov rcx,-1
      ::rcshft                      -- count shift
        add rcx,1
        shl rax,1
        jnc :rcshft
        mov [rsp],rcx
      ::rslim                       -- same limit
        --
        -- OK, this is the bit that actually generates random eax:
        --
--      mov rax,[rseed]
        mov eax,dword[ds+4]     -- (I don't really care if hi_dword contains 0 or crud, btw)
--10/11/16:
mov rcx,rax
shl rcx,32
or rax,rcx
        mov rcx,rax
        shl rcx,13
        xor rax,rcx
        mov rcx,rax
        shr rcx,17
        xor rcx,214013  -- PL
        xor rax,rcx
        mov rcx,rax
        shl rcx,5
        xor rax,rcx
--      sub r15,1
--      and rax,r15
--      add r15,1
--      mov [rseed],rax
        mov dword[ds+4],eax
        --
        -- check for bias and retry if necessary
        --
--      mov rcx,[rshft]
        mov rcx,[rsp]
        shr rax,cl
        cmp rax,rsi
        --jge showerror -- used as test for rand(128) and rand(1024).
--21/1/18:
--      jge :rslim
        jae :rslim
        add rax,1
        pop rcx
--      cmp rax,r15
--      jl :opRandStore
--          push rax
----            mov rdx,rdi         -- address of p1 (target)
--          fild qword[rsp]
--          add rsp,8
--          jmp :%pStoreFlt
      ::opRandStore
--      push rax
--      fild qword[rsp]
--      add rsp,8
--21/1/18:
        -- to load unsigned, right shift rax by 1, save odd bit in rcx, then *2+[0|1]
        mov rcx,rbx
        shr rax,1           -- /2
        rcl rcx,1           -- save carry
        push rax
        push rcx
        fild qword[rsp]
        fild qword[rsp+8]
        add rsp,16
        fadd st0,st0        -- *2
        faddp               -- +0|1
        jmp :%pStoreFlt

      ::opRand1
--      jl :e27atrmbge1     -- argument to rand must be >=1
        je :opRandStore
            pop rdx
            mov al,27       -- e27atrmbge1, argument to rand must be >=1
            sub rdx,1
            jmp :!iDiag
            int3
--    ::opRandStore
--      mov rdx,[rdi]
--      xor rbx,rbx
--      mov [rdi],rax
--      cmp rdx,r15
--      jle @f
--          sub qword[rbx+rdx*4-16],1
--          jz :%pDealloc
--    @@:
--      ret
    []

      }

