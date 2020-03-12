--
-- prnd.e (auto-include)    --[DEV rename as prand.e?]
--
include VM\pHeap.e  -- :%pDealloc/:%pLoadMint/:%pStoreFlt

--atom rseed    -- DEV
--integer rinit = 0

--procedure initr()
--  #ilASM{
--          rdtsc
--          xor eax,0x92D68CA2
--      [32]
--          lea edi,[rseed]
--      [64]
--          lea rdi,[rseed]
--      []
--          call :%pStoreMint
--        }
--  rinit = 1
--end procedure

--DEV to do: (replace opSetRand)
global procedure set_rand2(atom seed)
    #ilASM{
--      [32]
            mov eax,[seed]
--      [64]
--          mov rax,[seed]
--      []
            call :%pLoadMint
            mov dword[ds+4],eax
--/*
        [32]
            lea edi,[rseed]
        [64]
            cdqe
            lea rdi,[rseed]
        []
            call :%pStoreMint
--*/
           }
--  rseed = seed
--  rinit = 1
end procedure

global function get_rand()
--  if not rinit then initr() end if
atom seed
    #ilASM{
            mov eax,dword[ds+4]
        [32]
            lea edi,[seed]
        [64]
            lea rdi,[seed]
        []
            call :%pStoreMint
          }
    return seed
--  return rseed
end function

--DEV to do: (replace opRand) [NB incomplete! (no StoreMint)]
global function rand2(atom range)
atom res
--  if not rinit then initr() end if
--grr:
    if 0 then res=range end if
    #ilASM{
        [32]
            mov eax,[range]
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
        mov rax,[range]
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
--DEV:
-- lea eax/rax,[res]
--      call :%pStoreMint
      }
    return res
end function

--DEV I should probably test this for bias/even distribution...
global function rnd()
-- Returns a random floating point number in the range 0.0 to 1.0 inclusive.
atom a,b,r
--  if not rinit then initr() end if
    a = rand(#3FFFFFFF)
    if a = 1 then return 0 end if
    b = rand(#3FFFFFFF)
    if b = 1 then return 0 end if
    if a > b then
        r = b / a
    else
        r = a / b
    end if
    return r
end function

--DEV doc/syntax colour: {NO, see below}
--note: the optional argument allows/returns the same <i>shape</i> as a of 0.0..1.0,
--      for example, using 0.5 to denote a result in the range 0.0 to 1.0, then
--          rnd() -> 0.5
--          rnd({1,{2,3}}) -> {0.5, {0.5, 0.5}}
--erm,no...
--      whereas non-1 integers denote a sequence of that length, 
--      nb: an argument of 1 yields a different shape result to {1}, whereas 
--          an argument of 2 yields the same shape result as {1,1}.
-- erm: could be put on the std then...
global function sq_rnd(object shape=1)
    if atom(shape) then return rnd() end if
    for i=1 to length(shape) do
        shape[i] = sq_rnd(shape[i])
    end for
    return shape
end function

--better[?]:
--/* global 
function rnd(object shape=1)
--
-- returns random floating point number(s) in the range 0.0 to 1.0 inclusive,
--         of the same shape as the argument. For example, using 0.5 to denote 
--         any result in the range 0.0 to 1.0, then
--          rnd() [==rnd(1), and ==rnd(9.53)] -> 0.5,
--          rnd({1,{2,3,4}}) -> {0.5, {0.5, 0.5, 0.5}}
-- nb: rnd(0.3) [still] yields an atom result in the range 0.0 to 1.0, rather
--     than a result in the range 0.0 to 0.3, which needs something bespoke.     [[see maybe below...]]
--     (in that very specific case I might suggest loop till <=0.9, then /3)
-- nb: the shape of a (nested) string is a dword-sequence of the same length.
--
--atom a,b,r = 0
--  if not rinit then initr() end if
    if sequence(shape) then
        for i=1 to length(shape) do
            shape[i] = rnd(shape[i])
        end for
        return shape
    end if
    atom a = rand(#3FFFFFFF),
         b = rand(#3FFFFFFF),
         r = 0
    if a!=1 and b!=1 then
        r = iff(a>b?b/a:a/b)
    end if
    return r
--maybe:
--  return r*shape
end function
--*/
