--
-- pStack.e
-- ========

--constant freesym = "free symtab["
--constant closebr = "] \n"

--
--  Call stack management. 
--
--  Phix uses a virtual stack, allocated in ~12K (~11K for 64-bit) blocks on the heap, rather than 
--  the system stack, which avoids having to deal with stack exceptions, and/or decide how big the 
--  stack should be before execution begins. This means that Phix can perform deeper nested calls, 
--  and/or use a larger heap, than a traditional system-stack based approach. The virtual stack 
--  block sizes were taken directly from the tables in pHeap.e, namely 12280 for 32 bit and 11248 
--  for 64 bit, and we use %:pGetPool rather than allocate, which would be 4/8 off those figures.
--  (In reality :%pGetPool existed long before :%pAlloc and was chosen for that singular reason;
--   it would not be a terrible idea to switch to :%pAlloc, albeit resulting in a tiny loss.)
--
--  Creating a new frame and invoking a routine is not that much different to how it would be done 
--  traditionally (on the system stack), however return has to decrease reference counts and when
--  they drop to zero deallocate the object. Additionally, specialised methods are used to perform 
--  type checking and "callonce" any top-level code. To be fair, there is very little difference 
--  between using the system stack or virtual stack blocks, not that I ever quite got the hang of 
--  catching some exception or other in order to extend the stack, but one idea I rather like
--  about vsbs is that when a program runs out of memory it is quite possible to free up all but 
--  the head and tail(2) of the vsb chain, compared to freeing up the middle of the stack, and of
--  course that little bit of extra available memory may well be all that pDiag.e needs.
--
--
--  A virtual stack block (32-bit) is:
--      dd vsb_prev                 [vsb_root]
--      dd vsb_next                 [vsb_root+4]
--      dd spare (was symtabptr)    [vsb_root+8]
--      dd spare (was gvarptr)      [vsb_root+12]
--      dd magic                    [vsb_root+16]       -- (#40565342 = "@VSB")
--      (frames in remaining 12280-24 bytes, leaving unused chunks rather than spanning)
--      dd magic                    [vsb_root+12276]    -- (#3C565342 = "<VSB")
--
--  A (32-bit) frame is:
--      dd items N..2               [ebp-n*4+4]
--      dd item 1                   [ebp]
--      dd N                        [ebp+4] (if N==0 then [ebp] is junk/off-limits)
--      dd routine being called     [ebp+8] (there is no longer a calling routine)
--      dd called from addr         [ebp+12]
--EXCEPT
--X     dd return address           [ebp+16] (0 means callback)
--      dd exception handler        [ebp+16] (0 means none, 1 means lower down)
--      dd prev_ebp                 [ebp+20] (0 means top-level quit [maybe?])
--      dd vsb_root                 [ebp+24]
--EXCEPT
--X>?   dd ehand                    [ebp+28] (exception handler/flag) [proposed]
--      dd return address           [ebp+28] (0 means callback)
--old:
--      dd symtabptr                [ebp+24] \
--?     dd threadstack              [ebp+28]  } may be copied/overwritten every frame
--      dd vsb_root                 [ebp+32] /

--DEV have this make ddthreadstack 0 and see if we really need it anymore...
--    can we not just use [ds+8] to load symptr? - I believe we now do...   [ARGH!]

--
--  A virtual stack block (64-bit) is:
--      dq vsb_prev                 [vsb_root]
--      dq vsb_next                 [vsb_root+8]
--<     dq symtabptr                [vsb_root+16]       -- (raw address of symtab[1])   [DEV]
--<     dq gvarptr                  [vsb_root+24]       -- (raw address of gvar[1])     [DEV]
--      dq spare (was symtabptr)    [vsb_root+16]
--      dq spare (was gvarptr)      [vsb_root+24]
--      dq magic                    [vsb_root+32]       -- (#40565342 = "@VSB")
--pHeapD:
--<     <frames in remaining 9208-48 bytes, leaving unused chunks rather than spanning>
--      <frames in remaining 11248-48 bytes, leaving unused chunks rather than spanning>
--<     dq magic    ( [vsb_root+(9200)] )       -- (#3C565342 = "<VSB")
--      dq magic    ( [vsb_root+(11240)] )      -- (#3C565342 = "<VSB")
--
--  A (64-bit) frame is:
--      dq items N..2               [rbp-n*8+8]
--      dq item 1                   [rbp]
--      dq N                        [rbp+8] (if N==0 then [rbp] is junk/off-limits)
--      dq routine being called     [rbp+16] (there is no longer a calling routine)
--      dq called from addr         [rbp+24]
--EXCEPT
--X     dq return address           [rbp+32] (0 means callback)
--      dq exception handler        [rbp+32] (0 means none, 1 means lower down)
--      dq prev_ebp                 [rbp+40] (0 means top-level quit [maybe?])
--      dq vsb_root                 [rbp+48]
--EXCEPT
--X>?   dq ehand                    [rbp+56] (exception handler/flag) [proposed]
--      dq return address           [rbp+56] (0 means callback)
--

--without debug

include builtins\VM\pFEH.e
include builtins\VM\pHeap.e     -- :%pDealloc, :%pGetPool

integer sinit = 0   -- enforce once-only init

integer pArg = 0    -- [ELF] save of r|esp/4 (for command_line) at load
                    -- (when non-null) [pArg*w+w] is argc, where w=machine_word()
                    -- see syswait.ew for more details/environment handling

integer nocleanup = 0   -- set to 1 if (eg) :!iDiag has been called,
                        -- so abort proper like, rather than try and
                        -- "return when interpreting" etc.

integer CClean = 0      -- cleanup code for pcfunc.e [DEV togo]

constant oom = "Your program has run out of memory, one moment please\n"

#ilASM{ jmp :!opCallOnceYeNot

--
-- The following two routines are needed for p.exw and pcfunc.e: in the "parlor trick"
--  that is "p p p -test" we need to be sure that when we access [ds+8] we are talking
--  about the same one the VM is using. pDiagN/pStack/pTrace can use [ds+8] directly,
--  as they are part of the VM (and one day soon pcfunc.e should be as well?).
--

--/*
procedure :%pGetSymPtr(:%)
end procedure -- (for Edita/CtrlQ)
--*/
:%pGetSymPtr        -- [e/rsi] := raw(symtab)
-------------
    [32]
        mov esi,[ds+8]      -- (esi:=raw addr of symtab[1])
    [64]
        mov rsi,[ds+8]      -- (rsi:=raw addr of symtab[1])
    []
        ret

--/*
procedure :%pSetSymPtr(:%)
end procedure -- (for Edita/CtrlQ)
--*/
:%pSetSymPtr        -- raw(symtab) := [e/rsi]
-------------
    [32]
        mov [ds+8],esi      -- (raw addr of symtab[1]:=esi)
    [64]
        mov [ds+8],rsi      -- (raw addr of symtab[1]:=rsi)
    []
        ret

--/*
procedure :%opGetST(:%)
end procedure -- (for Edita/CtrlQ)
--*/
--DEV/temp...
:!opGetST       -- [e/rdi] := symtab
    [32]
        push dword[esp]
    [64]
        push qword[rsp]
    []
:%opGetST       -- [e/rdi] := symtab
---------       -- (trashes all registers, if [edi] needs dealloc, else just eax/edx)
    [32]
        mov eax,[ds+8]      -- (eax:=raw addr of symtab[1])
        mov edx,[edi]
        add dword[eax-8],1  -- inc refcount of symtab
        add eax,1           -- \
        ror eax,2           -- / make a #40000000+ ref
        mov [edi],eax
        cmp edx,h4
        jle @f
            sub dword[ebx+edx*4-8],1
--          jnz @f
--          pushad
----      if debugmem2
----        mov eax,[esp+32]
----        mov [dmFera],eax
----      end if
--          push dword[esp+32]
--          call :%pDealloc0
--          popad
            jz :%pDealloc
    [64]
        mov rax,[ds+8]      -- (rax:=raw addr of symtab[1])
        mov rdx,[rdi]
        add qword[rax-16],1 -- inc refcount of symtab
        add rax,1           -- \
        ror rax,2           -- / make a #40000..00+ ref
        mov r15,h4
        mov [rdi],rax
        cmp rdx,r15
        jle @f
            sub qword[rbx+rdx*4-16],1
            jz :%pDealloc
    []
      @@:
        ret

--/*
procedure ::newVSB(::)
end procedure -- (for Edita/CtrlQ)
--*/
::newVSB    -- (called from pNewStack and opFrame)
--------
    -- Allocate a new page for the virtual stack or re-use vsb_next.
    --
    -- On entry, edi(/rdi) is vsb_root (may be stack00), nowt else matters.
    -- On exit, eax(/rax) is the new ebp_root, ebp is unaltered, as are ecx, ebx, edx, esi, edi.
    --
    [32]
        mov eax,[edi+4]                 -- vsb_next
        test eax,eax
        jnz @f
        push ecx                        -- save
        push edx                        -- save
        push esi                        -- save
        push edi                        -- save (vsb_root)
        mov ecx,12280
        call :%pGetPool                 -- sets eax,edx, trashes ecx,esi,edi
        pop edi                         -- restore (vsb_root)
        pop esi                         -- restore
        pop edx                         -- restore
        pop ecx                         -- restore
        test eax,eax
        jz :trimStackPop
        mov [edi+4],eax                 -- prev_block.vsb_next:=new_block
        mov [eax],edi                   -- new_block.vsb_prev:=prev_block
        mov [eax+4],ebx                 -- new_block.vsb_next:=0
        mov [eax+8],ebx                 -- unused
        mov [eax+12],ebx                -- unused
        mov dword[eax+16],#40565342     -- magic ("@VSB")
        mov dword[eax+12276],#3C565342  -- magic ("<VSB")
      @@:
--  mov edi,eax
    [64]
        mov rax,[rdi+8]                 -- vsb_next
        test rax,rax
        jnz @f
        push rcx                        -- save
        push rdx                        -- save
        push rsi                        -- save
        push rdi                        -- save (vsb_root)
--pHeapD:
--<     mov rcx,9208
        mov rcx,11248
        call :%pGetPool                 -- sets rax,rdx, trashes rcx,rsi,rdi[?]
        pop rdi                         -- restore
        pop rsi                         -- restore
--  test rax,rax
--  jz :trimStack
--< cmp rdx,9208
--  cmp rdx,11248
--  jne ???
        pop rdx                         -- restore
        pop rcx                         -- restore
        test rax,rax
        jz :trimStackPop
        mov [rdi+8],rax                 -- set prev_block's vsb_next
        mov [rax],rdi                   -- set new_block's vsb_prev
        mov [rax+8],rbx                 -- set new_block's vsb_next = 0
        mov [rax+16],rbx
        mov [rax+24],rbx
        mov qword[rax+32],#40565342     -- magic ("@VSB")
        mov qword[rax+11240],#3C565342  -- magic ("<VSB")
      @@:
--  mov rdi,rax
    []
        ret

--/*
procedure :%pNewStack(:%)
end procedure -- (for Edita/CtrlQ)
--*/
:%pNewStack     -- (called from :>initStack, :%opInterp, task_yield, and CreateThread [DEV]) [20/1/18 now also cbhand.e]
----------
    [32]
        -- first, create a dummy vsb_root (vsb_next,vsb_prev@=0) on the stack
        --DEV edx:=era?
        xor ebx,ebx
--27/2/15:
--!     push ecx                -- gvarptr          [DEV]
--!     push eax                -- symtabptr        [DEV]
        push ebx
        push ebx
        mov edi,esp
        call :newVSB            -- allocate a new virtual stack block
        add esp,8               -- discard that dummy vsb_root
--      lea esi,[eax+8]         -- new ebp (N=0, [ebp] is <magic> ie "@VSB")
        lea esi,[eax+16]        -- new ebp (N=0, [ebp] is <magic> ie "@VSB")
        mov [eax],ebx           -- clear new vsb_prev link
        mov [esi+4],ebx         -- N
        mov dword[esi+8],21     -- routine being called (T_maintls)
        mov [esi+12],ebx        -- called from address (0)  [DEV remove?]
--EXCEPT
--X     mov [esi+16],ebx        -- return address (0)
        mov [esi+16],ebx        -- exception handler (0)
        mov [esi+20],ebx        -- prev_ebp
        mov [esi+24],eax        -- vsb_root
        mov [esi+28],ebx        -- return address (0)
        mov ebp,esi
    [64]
        -- first, create a dummy vsb_root (vsb_next,vsb_prev@=0) on the stack
        --DEV rdx:=era?
        xor rbx,rbx
        mov r15,h4
--27/2/15:
--      push rcx                -- gvarptr
--      push rax                -- symtabptr
        push rbx
        push rbx
        mov rdi,rsp
        call :newVSB            -- allocate a new virtual stack block
        add rsp,16              -- discard that dummy vsb_root
--      add rsp,32              -- discard that dummy vsb_root
--      lea rsi,[rax+16]        -- new ebp (N=0, [ebp] is <magic> ie "@VSB")
        lea rsi,[rax+32]        -- new ebp (N=0, [ebp] is <magic> ie "@VSB")
        mov [rax],rbx           -- clear new vsb_prev link
        mov [rsi+8],rbx         -- N
        mov qword[rsi+16],21    -- routine being called
        mov [rsi+24],rbx        -- called from address (0)
--EXCEPT
--X     mov [rsi+32],rbx        -- return address (0)
        mov [rsi+32],rbx        -- exception handler (0)
        mov [rsi+40],rbx        -- prev_ebp
        mov [rsi+48],rax        -- vsb_root
        mov [rsi+56],rbx        -- return address (0)
        mov rbp,rsi
    []
        ret

--/*
procedure :>initStack(:>)
end procedure -- (for Edita/CtrlQ)
--*/
:>initStack
-----------
        cmp [sinit],0
        jne :dont_do_everything_twice
        mov [sinit],1

    [PE32,ELF32,ELF64]  -- (not PE64) [DEV try without this...]
        call :>initFEH
    [32]
        call :%pNewStack
--15/9/16:
        mov edx,ebp             -- edx:=ebp
        call :%pSetSaveEBP      -- (eax<-pTCB.SaveEBP<-edx, all regs trashed)

--EXCEPT
--X     mov dword[ebp+16],:Exit0    -- return address (0)
        mov dword[ebp+28],:Exit0    -- return address (0)
    [ELF32] -- save esp for command line ... (assumes it is undamaged)  [DEV not interpret?]
        mov eax,esp
        shr eax,2
        mov [pArg],eax
    [64]
        call :%pNewStack

--DEV (spotted in passing) wot, no pSetSaveEBP?

--EXCEPT
--X     mov qword[rbp+32],:Exit0    -- return address (0)
        mov qword[rbp+56],:Exit0    -- return address (0)
    [ELF64]
--/*
DEV/In case it is useful, from flatassembler.net:
yes, that's right 
if you try to load ELF64 under debugger, you can find how and why 
in ELF64 created directly by FASM you get args in the stack 
dword [rsp]=argc 
qword [rsp+8]=pointer to arg0 
... 
qword [rsp+8 + argc*8]=0 end of args 
qword [rsp+8 + argc*8 + 8]=pointer to first string of environment 
... 
qword [...]=0 end of envirnment 
--*/
        mov rax,rsp
        shr rax,2
        mov [pArg],rax

        -- and any dll ref relocations (after pNewStack has zeroed e/rbx!):

    [32]
        mov esi,[ds+12]     -- relocs
        test esi,esi
        jz :loopend
        mov [ds+12],ebx
    [64]
        mov rsi,[ds+16]     -- relocs
        test rsi,rsi
        jz :loopend
        mov [ds+16],rbx
        --
        -- So this must be a dll/so, first time.
        -- The relocs table is in packed offset delta format:
        --  bytes 4..255 are offset deltas (two refs cannot overlap, and a
        --        constant sequence of length N causes N-1 4/8's to occur,
        --        at least, that is, when every element of it is a ref)
        --  3 is unused/illegal (no dll data segment is ever >4GB!)
        --  2 means a word (16-bit) offset delta follows,
        --  1 means a dword (32-bit) offset delta follows,
        --  0 means end of table.
        -- There should be no fixed literal refs anywhere in the code section.
        --  (ie pilx86.e should avoid isConstRef and similar when DLL=1)
        --
    [32]
        lea edi,[ds+0]      -- base (addr sigPhx)
        mov edx,esi
        sub edx,[esi-4]     -- a non-relocated address...
        xor eax,eax
        shr edx,2           --> ref fixup value (add to all refs)
    ::looptop
        lodsb
        cmp al,4
        jb @f
            add dword[edi+eax],edx
            add edi,eax
            jmp looptop
      @@:
        cmp al,3
        jne @f
            int3
      @@:
        cmp al,2
        jne @f
            lodsw
            add dword[edi+eax],edx
            add edi,eax
            xor eax,eax
            jmp looptop
      @@:
        cmp al,1
        jne loopend
            lodsd
            add dword[edi+eax],edx
            add edi,eax
            xor eax,eax
            jmp looptop
      ::loopend
    [64]
        lea rdi,[ds+0]      -- base (addr sigPhx)
        mov rdx,rsi
        sub rdx,[rsi-4]     -- a non-relocated address...
        xor rax,rax
        shr rdx,2           --> ref fixup value (add to all refs)
    ::looptop
        lodsb
        cmp al,8
        jb @f
            add qword[rdi+rax],rdx
            add rdi,rax
            jmp looptop
      @@:
        cmp al,3
        jb @f
            -- deltas of 3..7 are all illegal on 64-bit
            int3
--          lodsq
--          add qword[rdi+rax],rdx
--          add rdi,rax
--          xor rax,rax
--          jmp looptop
      @@:
        cmp al,2
        jne @f
            lodsw
            add qword[rdi+rax],rdx
            add rdi,rax
            xor rax,rax
            jmp looptop
      @@:
        cmp al,1
        jne loopend
            lodsd
            add qword[rdi+rax],rdx
            add rdi,rax
            xor rax,rax
            jmp looptop
      ::loopend
    []

      ::dont_do_everything_twice
        ret

--/*
procedure :%opGetArg(:%)
end procedure -- (for Edita/CtrlQ)
--*/
:%opGetArgELF       -- (meaningless on PE/WINDOWS)
-------------
    [ELF32]
--:%opGetArgELF32
        mov eax,[pArg]
    [ELF64]
--:%opGetArgELF64
        mov rax,[pArg]
    []
        ret

--makeFrameX:
--  mov esi,[esp]       -- grab a copy of the "called from" address, for use in debug reporting
--                      -- (note the "return address" may be branch straightened to miles away)

--/*
procedure :%opFrame(:%)
end procedure -- (for Edita/CtrlQ)
--*/
:%opFrame
---------
    [32]
        -- new style vsb:
        --  vsb_next
        --  vsb_prev
        --  spare (was symtabptr)
        --  spare (was gvarptr)
        --  magic == #40565342 aka "@VSB"
        --  <frames in next 12280-24 bytes>
        --  magic == #3C565342 aka "<VSB"
        --
        -- new style frame: (see above)
        --  items N..2              [ebp-n*4] (p2 is at [ebp-4], p3 at [ebp-8], etc)
        --ebp:
        --  item 1                  [ebp]   p1 (may not exist)
        --  N                       [ebp+4] number of parameters and locals (may be 0)
        --  routine being called    [ebp+8] (there is no longer a calling routine)
        --  called from addr        [ebp+12] (see note below)
--EXCEPT
--X     --  return address          [ebp+16] (0 means callback)
        --  exception handler       [ebp+16] (0 means none, 1 means lower down)
        --  prev_ebp                [ebp+20] (0 means top-level quit)
        --  vsb_root                [ebp+24]
        --  return address          [ebp+28] (0 means callback)
        --
        --  Obviously, if N [ebp+4] is zero, then [ebp] is magic/vsb_root of previous frame
        --              and should not be read let alone overwritten.
        --
        -- On Entry, ecx is number of parameters and locals
        --           edx is routineNo to call (nb was edi) (a symtab index)
        --           [esp] is "called from" addr 
        -- On Exit, eax is h4,
        --          ebx is 0, (as usual)
        --          ecx is 0,
--      --              edx is threadstack (==[ebp+28]) [DEV???]
        --          edx is still routineNo
        --          esi is ebp-N*4 or thereabouts
        --          edi is prev_ebp (==[ebp+20])
        --  Note "return address"=="called from" is correct for opCallOnce and opTChk, 
        --       but replaced after all direct calls to :%opFrame from "normal" code,
--EXCEPT
--X     --       via a mov dword[ebp+16],imm32 instruction.
        --       via a mov dword[ebp+28],imm32 instruction.
--      push esi                    -- save called from addr (we ran out of registers!)
-- ::makeFrameX [DEV killme]
        mov edi,[ebp+24]            -- vsb_root
--DEV test:
--cmp dword[edi+16],#40565342   -- magic ("@VSB")
--je @f
--  int3
--@@:
--cmp dword[edi+12276],#3C565342    -- magic ("<VSB")
--je @f
--  int3
--@@:
--  lea esi,[ebp+ecx*4+36]      -- new ebp (provisional!)   
--EXCEPT
--X     lea esi,[ebp+ecx*4+28]      -- new ebp (provisional!)   
        lea esi,[ebp+ecx*4+32]      -- new ebp (provisional!)   

-- if (newebp+28)<=(vsb_root+12280) then frame fits OK in current virtual stack block
-- if [ebp+ecx*4+28+28-12280]<=edi
-- if [ebp+ecx*4-12224]<=edi
--DEV I am sure we can simplify this, to say esi<=edi+12224-36, try the following:
--      lea eax,[ebp+ecx*4-12224]
--      cmp eax,edi                 -- check space
--EXCEPT
--      lea eax,[edi+12248] -- (12280-32 = 12248)       (DEV AGI)
--      lea eax,[edi+12244] -- (12280-36 = 12244)       (DEV AGI)
        lea eax,[edi+12240] -- (12280-36 = 12244)       (DEV AGI)
        cmp esi,eax         -- (if newebp > vsb_root+12280-32 then newVSB)
        jle @f
--DEV (untried)
--        push edx
--        mov edx,[esp+4]
          call :newVSB              -- sets eax (new vsb_root)
--        pop edx
          mov edi,eax
          lea esi,[eax+ecx*4+16]    -- new ebp (for N=0, [ebp] is <magic> ie "@VSB")
        @@:
        xor ebx,ebx
        mov eax,[esp]               -- called from address
        mov [esi+4],ecx             -- N
cmp [ebp+16],ebx
        mov [esi+8],edx             -- routine being called
setne bl
        mov [esi+12],eax            -- called from address
--EXCEPT
--X     mov [esi+16],eax            -- return address (see note above)
        mov [esi+16],ebx            -- exception handler (0/1)
--mov edx,[ebp+16]
        mov [esi+20],ebp            -- prev_ebp
xor ebx,ebx
        mov [esi+24],edi            -- vsb_root
        mov [esi+28],eax            -- return address (see note above)
--DEV test:
--lea edx,[edi+12276]
        mov eax,h4
        mov edi,esi
        std
        rep stosd                   -- (while ecx-- do mov[edi--],eax)
        cld
        mov edi,ebp
        mov ebp,esi
--cmp dword[edx],#3C565342
--je @f
--  int3
--@@:
    [64]
        -- new style vsb:
        --  vsb_next
        --  vsb_prev
        --  spare (was symtabptr)
        --  spare (was gvarptr)
        --  magic == #40565342 aka "@VSB" (plus dword 0 padding)
        --  <frames in next 11248-48 bytes>
        --  magic == #3C565342 aka "<VSB" (plus dword 0 padding)
        --
        -- new style frame: (see above)
        --  items N..2              [rbp-n*8] (p2 is at [rbp-8], p3 at [rbp-16], etc)
        --rbp:
        --  item 1                  [rbp]   p1 (may not exist)
        --  N                       [rbp+8] number of parameters and locals (may be 0)
        --  routine being called    [rbp+16] (there is no longer a calling routine)
        --  called from addr        [rbp+24] (see note below)   [DEV remove??]
--EXCEPT
--X     --  return address          [rbp+32] (0 means callback)
        --  exception handler       [rbp+32] (0 means none, 1 means lower down)
        --  prev_ebp                [rbp+40] (0 means top-level quit)
        --  vsb_root                [rbp+48]
        --  return address          [rbp+56] (0 means callback)
        --
        --  Obviously, if N [rbp+8] is zero, then [rbp] is magic/vsb_root of previous frame
        --              and should not be read let alone overwritten.
        --
        -- On Entry, rcx is number of parameters and locals (N, >=0)
        --           rdx is routineNo to call (a symtab index)
        --           [rsp] is "called from" addr 
        -- On Exit, rax is h4,
        --          rbx is 0, (as usual)
        --          rcx is 0,
        --          rdx is still routineNo
        --          rsi is rbp-N*8 or thereabouts
        --          rdi is prev_ebp (==[rbp+40])
        --          rbp is the new frame, such that [rbp+8] is N, 
        --                                          [rbp+16] is routine no, etc
        --  Note "return address"=="called from" is correct for opCallOnce and opTChk, 
        --       but replaced after all direct calls to :%opFrame from "normal" code,
        --       via a mov dword[rbp+32],imm32 instruction. [DEV check this]
--  push rsi                    -- save called from addr (we ran out of registers!)
--      mov r12,rsi                 -- save called from addr
-- ::makeFrameX
        mov rdi,[rbp+48]            -- vsb_root
--DEV test:
--cmp qword[rdi+32],#40565342   -- magic ("@VSB")
--je @f
--  int3
--@@:
--cmp qword[rdi+11240],#3C565342    -- magic ("<VSB")
--je @f
--  int3
--@@:

--EXCEPT
--X     lea rsi,[rbp+rcx*8+56]      -- new ebp (provisional!)  --DEV try this again!
        lea rsi,[rbp+rcx*8+64]      -- new ebp (provisional!)  --DEV try this again!

--      shl rsi,2               --DEV? shl rsi,1??
--      add rsi,rbp                 -- new ebp (provisional!)
-- if (newebp+56)<=(vsb_root+9208) then frame fits OK in current virtual stack block
-- if [rbp+rcx*8+56+56-9208]<=rdi
-- if [rbp+rcx*8-9096]<=rdi
--      lea r13,[rbp+rcx*8-9096]        --DEV try this again!
-- if (newebp+56)<=(vsb_root+11248) then frame fits OK in current virtual stack block
-- if [rbp+rcx*8+56+56-11248]<=rdi
-- if [rbp+rcx*8-11136]<=rdi
--      lea r13,[rbp+rcx*8-11136]       --DEV try this again!
--pHeapD: (9208->11248)
--      lea r13,[rbx+rcx*4-4548]
--      lea r13,[rbx+rcx*4-4548?!]
--x!    shl r13,2   -- (spotted in passing!)
--      shl r13,1
--      add r13,rbp                 
--      cmp r13,rdi                 -- check space
--DEV I am sure we can simplify this, to say rsi<=rdi+9208-56, try the following: (after updating the numbers!)
--  lea rax,[rdi+9152]  -- (9208-56 = 9152)
--      lea rax,[rdi+9152]  -- (9208-56 = 9152)
--      lea rax,[rdi+11080] -- (11248-56 = 11080!)
--EXCEPT
--      lea rax,[rdi+11184] -- (11248-64 = 11184)       (DEV AGI)
--      lea rax,[rdi+11176] -- (11248-72 = 11176)       (DEV AGI)
        lea rax,[rdi+11168] -- (11248-72 = 11176)       (DEV AGI)
--      cmp rsi,rax         -- (if newebp > vsb_root+9208-56 then newVSB)
        cmp rsi,rax         -- (if newebp > vsb_root+11248-64 then newVSB)
        jle @f
--DEV (untried)
--        push rdx
--        mov rdx,[rsp+8]
          call :newVSB              -- sets rax (new vsb_root)
--        pop rdx
          mov rdi,rax
--DEV have I gone mad here?
          lea rsi,[rax+rcx*8+32]    -- new ebp (for N=0, [ebp] is <magic> ie "@VSB")
--        lea rsi,[rbx+rcx*4+8]
--        lea rsi,[rbx+rcx*4+16]
--x!      shl rsi,2 -- (ditto)
--        shl rsi,1
--        add rsi,rax
      @@:
--  xor rbx,rbx
--  pop eax                     -- called from address
        mov rax,[rsp]               -- called from address
        mov [rsi+8],rcx             -- N
cmp [rbp+32],rbx
        mov [rsi+16],rdx            -- routine being called
setne bl
        mov [rsi+24],rax            -- called from address
--EXCEPT
--X     mov [rsi+32],rax            -- return address (see note above)
        mov [rsi+32],rbx            -- exception handler
        mov [rsi+40],rbp            -- prev_ebp
xor rbx,rbx
        mov [rsi+48],rdi            -- vsb_root
        mov [rsi+56],rax            -- return address (see note above)
--DEV test: (removed 6/3/16 - NB callback error handling now relies on rdx being preserved)
--lea rdx,[rdi+11240]
        mov rax,h4
        mov rdi,rsi
        std
        rep stosq                   -- (while rcx-- do mov[rdi--],rax)
        cld
        mov rdi,rbp
        mov rbp,rsi
--cmp qword[rdx],#3C565342
--je @f
--  int3
--@@:
--      mov qword[rax+11240],#3C565342  -- magic ("<VSB")
    []
        ret

--/*
procedure :%opCallOnce(:%)
end procedure -- (for Edita/CtrlQ)
--*/
:%opCallOnce
------------
--
-- Used for top level subroutine (tls) calls. A tls is created for each and every 
-- include file, with the one for the main file living in symtab[T_maintls(=21)],
-- which is the only one never invoked via :%pCallOnce. The compiler will however
-- try to optimise them away, if it turns out that all they do is invoke opRetf.
-- Effectively, the "include" statement generates an opCallOnce in the 'calling tls'.
-- Eg inc0.exw includes inc0b.e, and the [top-level] code from inc0b.e ends up
-- in symtab[362] so symtab[T_maintls] contains {...opCallOnce,362...}.
-- See also file:../docs/pfeat.htm#fwdDZ and the section after that.
-- A tls routine is parameterless, opCallOnce is opFrame+opCall rolled into one.
--
-- opCallOnce may also be used when invoking a forward (global) routine, to try and
-- ensure that any top-level initialisation in the include file has been done.
--
    [32]
        -- routine no passed in edi, all registers trashed
        mov edx,[ds+8]              -- symtabptr
        xor ebx,ebx
--DEV try nop nop
        mov edx,[edx+edi*4-4]       -- symtab[edi]              (AGI unavoidable... unless fixup does it!)
--DEV try nop nop nop
--6/7/17:
--      mov eax,[ebx+edx*4+12]      -- symtab[edi][S_State=4]   (AGI unavoidable...)
--      mov ecx,[ebx+edx*4+40]      -- symtab[edi][S_il=11] - execute after makeFrame
--      test eax,#800               -- K_ran
--      jnz :justRetX               -- already called!
--      pop esi                     -- return address === called from addr
--      add eax,#800
--      push ecx                    -- symtab[edi][S_il] - execute after makeFrame
--      mov [ebx+edx*4+12],eax      -- update symtab[edi][S_State]
        mov ecx,[ebx+edx*4+12]      -- symtab[edi][S_State=4]   (AGI unavoidable...)
        mov eax,[ebx+edx*4+40]      -- symtab[edi][S_il=11] - execute after makeFrame
        test ecx,#800               -- K_ran
        jnz :justRetX               -- already called!
        pop esi                     -- return address === called from addr
        add ecx,#800
--      push ecx                    -- symtab[edi][S_il] - execute after makeFrame
        mov [ebx+edx*4+12],ecx      -- update symtab[edi][S_State]
        call :%pLoadMint    
        push eax
--<6/7/17 ends>
        xor ecx,ecx                 -- no of params (0)
        mov edx,edi
        push esi
        call :%opFrame
        pop eax                     -- called from address (was esi)
        mov [esi+12],eax            -- called from address
--EXCEPT
--X     mov [esi+16],eax            -- return address (see note above)
        mov [esi+28],eax            -- return address (see note above)
--      add esp,4
    [64]
        -- routine no passed in rdi, all registers trashed
        mov rdx,[ds+8]              -- symtabptr
        xor rbx,rbx
--DEV try nop nop
        mov rdx,[rdx+rdi*8-8]       -- symtab[rdi]              (AGI unavoidable... unless fixup does it!)
--!     mov rdx,[rdx+r12]
--DEV try nop nop nop
--6/7/17:
--      mov rax,[rbx+rdx*4+24]      -- symtab[rdi][S_State=4]   (AGI unavoidable...)
--      mov rcx,[rbx+rdx*4+80]      -- symtab[rdi][S_il=11] - execute after makeFrame
--      test rax,#800               -- K_ran
--      jnz :justRetX               -- already called!
--      pop rsi                     -- return address === called from addr
--      add rax,#800
--      push rcx                    -- symtab[rdi][S_il] - execute after makeFrame
--      mov [rbx+rdx*4+24],rax      -- update symtab[rdi][S_State]
        mov rcx,[rbx+rdx*4+24]      -- symtab[rdi][S_State=4]   (AGI unavoidable...)
        mov rax,[rbx+rdx*4+80]      -- symtab[rdi][S_il=11] - execute after makeFrame
        test rcx,#800               -- K_ran
        jnz :justRetX               -- already called!
        pop rsi                     -- return address === called from addr
        add rcx,#800
--      push rcx                    -- symtab[rdi][S_il] - execute after makeFrame
        mov [rbx+rdx*4+24],rcx      -- update symtab[rdi][S_State]
        call :%pLoadMint    
        push rax
--<6/7/17 ends>
        xor rcx,rcx                 -- no of params (0)
        mov rdx,rdi
        push rsi
        call :%opFrame
        pop rax                     -- called from address (was rsi)
        mov [rsi+24],rax            -- called from address
--EXCEPT
--X     mov [rsi+32],rax            -- return address (see note above)
        mov [rsi+56],rax            -- return address (see note above)
--      add rsp,8
    []
      ::justRetX
        ret

--      jmp :%opFrame


--DEV sweeeeeeet! (untested) (can now patch [S_State]+=K_ran for {opRetf} jobs as well)
--  -- edi is routine no
--  -- edx is raw symtab[edi] as well
--  mov eax,[edx+12]            -- symtab[edi][S_State=4]       (AGI unavoidable...)
--  mov ecx,[edx+40]            -- symtab[edx][S_il=11] - execute after makeFrame
--  test eax,#800               -- K_ran
--  jnz @f                      -- already called!
--  pop esi                     -- return address === called from addr
--  add eax,#800
--  push ecx                    -- symtab[edx][S_il] - execute after makeFrame
--  mov [edx+12],eax            -- update S_State
--  xor ecx,ecx                 -- no of params (0)
--  xor eax,eax                 -- addr 1st (n/a)
--  jmp makeFrame               -- (esi,edi already set)
--  @@:
--  ret

--/*
procedure :%opRetf(:%)
end procedure -- (for Edita/CtrlQ)
--*/
:%opRetf
--------
--nb must preserve eax/rax
-- new style vsb:
--  items N..2
--ebp: // so p1 is at [ebp], p2 at [ebp-4] etc...
--  item 1
--  N                       +4
--  routine being called    +8 (there is no longer a calling routine)
--  called from addr        +12 (see note below)
--< return address          +16 (0 means callback)
--  exception handler       +16 (0 means none, 1 means lower down)
--  prev_ebp                +20
--! symtab                  +24
--! threadstack             +28     
--! vsb_root                +32
--  vsb_root                +24
--  return address          +28 (0 means callback)

    [32]
--EXCEPT
--X     mov edx,[ebp+16]        -- return address
--cmp [ebp+16],ebx
--je @f
--  int3
--@@:
        mov edx,[ebp+28]        -- return address
        mov ecx,[ebp+4]         -- N
--      mov e?x,[ebp+20]        -- vsb_root (not needed??)
        test edx,edx
        jz @f
          push edx
      @@:
--      push eax                -- preserve eax
        mov esi,ebp
--SUG:
--    ::opRetLoop
      @@:
        sub ecx,1               -- no of items saved
        js :opRetLoopEnd
        mov edx,[esi]
--      sub esi,4
        lea esi,[esi-4]
        cmp edx,h4
        jle @b
        sub dword[ebx+edx*4-8],1
        jnz @b
        pushad                  -- (needs to preserve ecx,esi, may as well do eax too)
                                -- (NB a single (/outer) pushad would not work anyway)
--?     call deallocY
        push dword[esp+32]
        call :%pDealloc0
        popad
        jmp @b
      ::opRetLoopEnd
        mov ecx,[ebp+20]        -- prev_ebp
        test ecx,ecx
        jz @f
            mov ebp,ecx
--          xor eax,eax --no!!
            ret
      @@:
        xor eax,eax  -- 11/2/15 (makes final top-level exit==abort(0), not abort(rand))
        ret

    [64]
--EXCEPT
--X     mov rdx,[rbp+32]        -- return address
--cmp [rbp+32],rbx
--je @f
--  int3
--@@:
        mov rdx,[rbp+56]        -- return address
        mov rcx,[rbp+8]         -- N
        test rdx,rdx
        jz @f
          push rdx
      @@:
        mov rsi,rbp
        mov r15,h4
      @@:
        sub rcx,1               -- no of items saved
        js opRetLoopEnd
        mov rdx,[rsi]
        lea rsi,[rsi-8]
--      cmp rdx,h4
        cmp rdx,r15
        jle @b
        sub qword[rbx+rdx*4-16],1
        jnz @b
        push rcx
        push rsi
        push rax
--DEV now assuming this preserves r15 (h4):
        push qword[rsp+24]
        call :%pDealloc0
        pop rax
        pop rsi
        pop rcx
        jmp @b
      ::opRetLoopEnd
        mov rcx,[rbp+40]        -- prev_ebp
        test rcx,rcx
        jz @f
            mov rbp,rcx
            ret
      @@:
        xor rax,rax  -- 11/2/15 (makes final top-level exit==abort(0), not abort(rand))
        ret
    []

--/*
procedure :%pFreeStack(:%)
end procedure -- (for Edita/CtrlQ)
--*/
:%pFreeStack    -- called from pThread.e
------------
--::tFreeStack
    [32]
        -- call :%opRetf until T_maintls
        cmp dword[ebp+8],21     -- T_maintls
        je @f
--X         mov dword[ebp+16],:%pFreeStack  -- replace return address (DEV bug in pilasm.e)
            mov dword[ebp+28],:%pFreeStack  -- replace return address
--EXCEPT
--X         mov dword[ebp+16],:tFreeStack   -- replace return address
--X         mov dword[ebp+28],:tFreeStack   -- replace return address
            jmp :%opRetf
      @@:
        mov eax,[ebp+24]        -- vsb_root
      @@:
        push dword[eax+4]       -- vsb_next
        mov ecx,12280
--DEV edx:=?                    -- era
        call :%pFreePool        -- release ecx bytes of memory at eax.
        pop eax
        test eax,eax
        jnz @b
        xor ebp,ebp
    [64]
        -- call :%opRetf until T_maintls
        cmp qword[rbp+16],21    -- T_maintls
        je @f
--X         mov qword[rbp+32],:%pFreeStack  -- replace return address (DEV bug in pilasm.e)
            mov qword[rbp+56],:%pFreeStack  -- replace return address
--EXCEPT
--X         mov qword[rbp+32],:tFreeStack   -- replace return address
--X         mov qword[rbp+56],:tFreeStack   -- replace return address
            jmp :%opRetf
      @@:
        mov rax,[rbp+48]        -- vsb_root
      @@:
        push qword[rax+8]       -- vsb_next
--pHeapD:
--<     mov rcx,9208
        mov rcx,11248
--DEV rdx:=?                    -- era
        call :%pFreePool        -- release rcx bytes of memory at rax.
        pop rax
        test rax,rax
        jnz @b
        xor rbp,rbp
    []
        ret

--/*
procedure :%trimStack(:%)
end procedure -- (for Edita/CtrlQ)
--*/
  ::trimStackPop
----------------
    -- (assumes this is from opFrame/newVSB, not pNewStack/newVSB):
    [32]
        add esp,4
    [64]
        add rsp,8
    []
  :%trimStack
-------------
    -- cleanup the call stack and raise e33maf. era @ [esp].
    [32]
        mov edi,[oom]           -- "Your program has run out of memory, one moment please\n"
        call :%puts1
        mov eax,[ebp+24]        -- vsb_root
        cmp [eax],ebx           -- vsb_prev
        je :trimNP              -- no trimming possible
        push eax                --[1]
        push ebp                --[2]
        --
        -- skip down the stack until we've got a different vsb_root
        --
      @@:
        mov edx,ebp
        mov ebp,[ebp+20]        -- prev_ebp
        cmp eax,[ebp+24]        -- vsb_root
        je @b
        --
        -- call opRetf until vsb_prev is 0
        --
        push dword[ebp+24]      --[3] save vsb_root of last_but_one block
        push edx                --[4] save last[1] for prev_ebp relink
      ::trimRetLoop
        mov eax,[ebp+24]        -- vsb_root
        cmp [eax],ebx           -- vsb_prev
        je @f
--EXCEPT
--X         mov dword[ebp+16],:trimRetLoop  -- replace return address
            mov dword[ebp+28],:trimRetLoop  -- replace return address
            jmp :%opRetf
      @@:
        --
        -- free any stack blocks emptied by the previous loop
        --
        pop edx                 --[4] last[1] (first frame in last block)
        mov [edx+20],ebp        -- reroute prev_ebp chain around removed blocks
        pop eax                 --[3] last_but_one (a vsb_root)
      ::trimFreeLoop    
        cmp [eax],ebx           -- vsb_prev
        je @f
            push dword[eax]     --[3] vsb_prev
            mov ecx,12280
            -- (no sensible edx/era here)
            call :%pFreePool        -- release ecx bytes of memory at eax.
            pop eax             --[3]
            jmp :trimFreeLoop
      @@:
        --
        -- re-attach the last block, and link that last[1] up.
        --
        pop ebp                 --[2] original
        pop dword[eax+4]        --[1] vsb_next
        mov edx,[ebp+24]        -- vsb_root
        mov [edx],eax           -- vsb_prev

      ::trimNP
        --
        -- and finally trigger e33maf
        --
        pop edx
        mov al,33   -- e33maf (memory allocation failure)
        sub edx,1
        jmp :!iDiag
        int3
    [64]
        mov rdi,[oom]           -- "Your program has run out of memory, one moment please\n"
        call :%puts1
        mov rax,[rbp+48]        -- vsb_root
        cmp [rax],rbx           -- vsb_prev
        je :trimNP              -- no trimming possible
        push rax                --[1]
        push rbp                --[2]
        --
        -- skip down the stack until we've got a different vsb_root
        --
      @@:
        mov rdx,rbp
        mov rbp,[rbp+40]        -- prev_ebp
        cmp rax,[rbp+48]        -- vsb_root
        je @b
        --
        -- call opRetf until vsb_prev is 0
        --
        push qword[rbp+48]      --[3] save vsb_root of last_but_one block
        push rdx                --[4] save last[1] for prev_ebp relink
      ::trimRetLoop
        mov rax,[rbp+48]        -- vsb_root
        cmp [rax],rbx           -- vsb_prev
        je @f
--EXCEPT
--X         mov dword[rbp+32],:trimRetLoop  -- replace return address
            mov dword[rbp+56],:trimRetLoop  -- replace return address
            jmp :%opRetf
      @@:
        --
        -- free any stack blocks emptied by the previous loop
        --
        pop rdx                 --[4] last[1] (first frame in last block)
        mov [rdx+40],rbp        -- reroute prev_ebp chain around removed blocks
        pop rax                 --[3] last_but_one (a vsb_root)
      ::trimFreeLoop    
        cmp [rax],rbx           -- vsb_prev
        je @f
            push qword[rax]     --[3] vsb_prev
            mov rcx,11248
            -- (no sensible rdx/era here)
            call :%pFreePool        -- release rcx bytes of memory at rax.
            pop rax             --[3]
            jmp :trimFreeLoop
      @@:
        --
        -- re-attach the last block, and link that last[1] up.
        --
        pop rbp                 --[2] original
        pop qword[rax+8]        --[1] vsb_next
        mov rdx,[rbp+48]        -- vsb_root
        mov [rdx],rax           -- vsb_prev

      ::trimNP
        --
        -- and finally trigger e33maf
        --
        pop rdx
        mov al,33   -- e33maf (memory allocation failure)
        sub rdx,1
        jmp :!iDiag
        int3
    []
--  A virtual stack block (32-bit) is:
--      dd vsb_prev                 [vsb_root]
--      dd vsb_next                 [vsb_root+4]
--  A (32-bit) frame is:
--EXCEPT
--X     dd return address           [ebp+16] (0 means callback)
--      dd exception handler        [ebp+16] (0 means none, 1 means lower down)
--      dd prev_ebp                 [ebp+20] (0 means top-level quit [maybe?])
--      dd vsb_root                 [ebp+24]
--      dd return address           [ebp+28] (0 means callback)

--  A virtual stack block (64-bit) is:
--      dq vsb_prev                 [vsb_root]
--      dq vsb_next                 [vsb_root+8]
--  A (64-bit) frame is:
--EXCEPT
--X     dq return address           [rbp+32] (0 means callback)
--      dq exception handler        [rbp+32] (0 means nojne, 1 means lower down)
--      dq prev_ebp                 [rbp+40] (0 means top-level quit [maybe?])
--      dq vsb_root                 [rbp+48]
--      dq return address           [rbp+56] (0 means callback)
----    puts(1,"Your program has run out of memory, one moment please\n")
--! mov eax,[ebp+32]                -- vsb_root
--  --
--  -- Keep the (full) vsb at edi, free everything below it.
--  -- (We may also want to keep the first vsb, but that is for another day)
--  -- First job: scan down the prev_ebp links looking for the first in this (edi) block,
--  --            then zero that prev_ebp (which is pointing to a different vsb).
--  mov edx,[ebp+24]                -- vsb_root
--  mov esi,ebp
--  mov eax,[ebp+20]                -- prev_ebp
-- ::tsloop1    -- while eax is between edx and esi, shadow eax in esi
--DEV should test this does what I want when a vsb straddles #80000000:
--  cmp eax,edx
--  jb @f
--  cmp eax,esi
--  ja @f
--  mov esi,eax
--  mov eax,[eax+20]                -- prev_ebp
--  jmp :tsloop1
-- @@:
--  mov [esi+20],ebx                -- prev_ebp:=null, on the first frame in this vsb
--  -- Second job: unlink and free all vsb_prev
--  push esi
-- ::tsloop2
--  mov eax,[edx]                   -- vsb_prev
--  mov [edx],ebx                   -- vsb_prev:=0 (only rqd on first)
--  test eax,eax
--? jz @f
--  jz e77phroom
--  push eax
--  mov ecx,12280
--  mov eax,edx
--  mov edx,??? (era)
--  call :%pFreePool    -- release ecx bytes at eax
--  pop edx
--  jmp :tsloop2
--? @@:
--? push ebp
--? mov eax,[eax]                   -- vsb_prev
--? test eax,eax
--? jz :trimdone
--? ::tslooptop
--? mov eax,[eax+32]                -- vsb_root
--? mov [esp],eax
--? mov eax,[eax]                   -- vsb_prev
--? test eax,eax
--? jz :trimdone
--? 
--? ::trimdone
--? add esp,4
--? jmp e77phroom

--/*
procedure :%opTchk(:%)
end procedure -- (for Edita/CtrlQ)
--*/
:%opTchk
--------
-- calling convention (as used in pilx86.e)
--  mov ecx,nparms      -- no of params/locals/tmps (aka symtab[edx][S_Ltot])
--  mov edx,N           -- type check routineNo (using isVno)
--  push :return addr
--  push <varno>        -- var number (using isVno)
--  push <code>         -- addr of typecheck code (aka symtab[edx][S_il])
--  push dword[var]     -- value to check
--  jmp %:pTchk                                     351         E9 rel32        jmp rel32
-- ::return_addr
    [32]
        mov esi,[esp+12]    -- return address (==called from address)
        call :%opFrame      -- leaves ecx=0, ?esi=addr first (was in eax)
        pop eax             -- value to check
--X     mov edx,:!opTchkRetAddr  -- return address
        mov ecx,[esp+8]     -- real return addr
--X     mov dword[ebp+16],:!opTchkRetAddr  -- return address
        mov dword[ebp+28],:!opTchkRetAddr  -- return address
--EXCEPT
--X     mov dword[ebp+16],edx
--x^    mov dword[ebp+28],edx
        mov dword[ebp+12],ecx
        cmp eax,h4
        jl @f
          add dword[ebx+eax*4-8],1  -- incref
      @@:
-- (minor listing issue)
--      mov [ebp],eax       -- [addr first] := value
        mov [ebp+ebx],eax   -- [addr first] := value
        ret                 -- execute typecheck code!

--DEV if era is opTchkRetAddr, then replace it with the one on the stack... (very messy if there be stuff on the stack...)
--      (better: stash it in [ebp+12]/[rbp+24] before the ret above)
      :!opTchkRetAddr
        pop ecx             -- var no
        cmp eax,0
        jne @f
    :%opTcFail
--------------
--(no)      pop eax         -- return address
--DEV:
--          mov [ep1],ecx   -- var no (just leave it in ecx)
--          mov [era],eax   -- (just leave it on the stack)
            pop edx
            mov al,1        -- e01tcf(ecx==idx)
            sub edx,1
            jmp :!iDiag
            int3
      @@:
        ret
    [64]
        mov rsi,[rsp+24]    -- return address (==called from address)
        call :%opFrame      -- leaves ecx=0, ?esi=addr first (was in eax)
        pop rax             -- value to check
        mov rcx,[rsp+16]    -- real return addr
--X     mov rdx,:!opTchkRetAddr  -- return address
        mov r15,h4
--X     mov qword[rbp+32],:!opTchkRetAddr  -- return address
        mov qword[rbp+56],:!opTchkRetAddr  -- return address
--EXCEPT
--X     mov qword[rbp+32],rdx
--x^    mov qword[rbp+56],rdx
        mov qword[rbp+24],rcx
        cmp rax,r15
        jl @f
          add qword[rbx+rax*4-16],1 -- incref
      @@:
--ditto (minor listing issue)
--      mov [rbp],rax       -- [addr first] := value
        mov [rbp+rbx],rax   -- [addr first] := value
        ret                 -- execute typecheck code!

      :!opTchkRetAddr
        pop rcx             -- var no
        cmp rax,0
        jne @f
    :%opTcFail
--(no)      pop rax         -- return address
--DEV:
--          mov [ep1],rcx   -- var no (just leave it in rcx)
--          mov [era],rax   -- (just leave in on the stack)
            pop rdx
            mov al,1
--DEV pdiag?
--          jmp :enumbset
            sub rdx,1
            jmp :!iDiag
            int3 
      @@:
        ret
    []

--/*
:%opInterp (should probably just be in p.exw/main())
----------
    -- note: the compiler should not have any open files when this is called.
    --       interpretation is NOT thread safe
--  push [symtabptr] -- now [ds+8] (no longer needed)
--  mov [symtabptr],??? ""
    [level]++
--27/2/15: eax:=symtabptr, ecx:=gvarptr
    call :%pNewStack
    call ???
:%opIaborted
    [level]--
--  pop [symtabptr] ""
    call :%pFreeStack
    ret
--*/

--::fin

--
-- pAbort.e
-- ========
--
--  The abort() builtin.
--  Close any open files, free the console, ... [DEV]
--

--include builtins\VM\pFEH.e    -- possibly temp

--  ::e87acmbi
--      [32]
--          pop edx
--          mov al,87
--          sub edx,1
--      [64]
--          pop rdx...
--      []
--          jmp :!iDiag [DEV]
--          int3

--newEmit/optable:
--/*
stack effects: #ilasm: any...!!!
file        entry points
pAbort.e
========
        :%opAbort
--      (pFPU.e)    :%down53
--      call :!opClosem9                -- (duplicates return addr, if called)
--      call opFreeCons
--      jmp :%opIaborted
--*/

--DEV togo:
--/*
procedure :%SetCCleanup(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%SetCCleanup       -- cleanup code for pcfunc.e
-----------------
    [32]
        mov [CClean],eax
    [64]
        mov [CClean],rax
    []
        ret

--/*
procedure :%RunCleanup(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%RunCleanup
----------------
::FreeStack
    [32]
        -- call :%opRetf until T_maintls
        cmp dword[ebp+8],21     -- T_maintls
        je @f
--EXCEPT
--X         mov dword[ebp+16],:FreeStack    -- replace return address
            mov dword[ebp+28],:FreeStack    -- replace return address
            jmp :%opRetf
      @@:

--      mov edi,[freesym]
--      call :%puts1
--      mov edi,[closebr]
--      call :%puts1

        mov eax,[CClean]
        shr eax,2
        jz @f
            call eax
      @@:

        mov esi,[ds+8]              -- (esi:=raw addr of symtab[1])

        mov ecx,[esi-12]            -- symlen
      ::CleanupLoop
--pushad
--      push ecx
--      mov edi,[freesym]
--      call :%puts1
--      pop eax
--      push 0
--      call :%putsint
--      mov edi,[closebr]
--      call :%puts1
--popad

        mov edi,[esi+ecx*4-4]       -- symtab[ecx]
        sub ecx,1
        cmp edi,h4
        jle @f
            mov edx,[ebx+edi*4+4]       -- symtab[ecx][S_NTyp]
            cmp edx,1                   -- <=2 (S_Const or S_GVar2)
            jne :opAgvar
                test dword[ebx+edi*4+12],#200   -- K_noclr
                jnz @f
          ::opAgvar
            cmp edx,2
            jg @f
                -- S_Const w/o K_noclr and all S_Gvar2
                mov eax,[esi+88]        -- eax:=symtab[T_ds4=23]
--              mov edx,[ebx+edi*4+20]  -- symtab[ecx][S_Slink]
                mov edi,[ebx+edi*4+20]  -- edi:=symtab[ecx][S_Slink]
--              lea edi,[ds+8] -- no!
                cmp [esi+84],ebx        -- symtab[T_EBP=22]=0?
                je :compiled
                    shl eax,2
              ::compiled
--              mov edx,[eax+edx*4+16]  -- gvar[symtab[ecx][S_Slink]]
                mov edx,[eax+edi*4+16]  -- edx:=gvar[symtab[ecx][S_Slink]]
                cmp edx,h4
                jle @f
                    sub dword[ebx+edx*4-8],1
                    jnz @f
                        mov [eax+edi*4+16],ebx
                        pushad
                        call :%pDealloc

--                      mov edi,[freesym]
--                      call :%puts1
--                      mov eax,[esp+24]    -- (ecx)
--                      push 0
--                      add eax,1
--                      call :%putsint
--                      mov edi,[closebr]
--                      call :%puts1

                        popad
--/*
        mov edi,[ecode]             -- "exception code #"
        call :%puts1

        mov edx,[edi]               -- exception_code
        push 0                      -- no cr
        call :%puthex32

        mov eax,[i]
        push [showcr]
        call :%putsint
--*/
      @@:
        cmp ecx,26                  -- T_const1
        jg :CleanupLoop
--      pop eax

        mov eax,[ebp+24]        -- vsb_root
      @@:
        push dword[eax+4]       -- vsb_next
        mov ecx,12280
--DEV edx:=?                    -- era
        call :%pFreePool        -- release ecx bytes of memory at eax.
        pop eax
        test eax,eax
        jnz @b
        xor ebp,ebp

    [64]
        -- call :%opRetf until T_maintls
        cmp qword[rbp+16],21    -- T_maintls
        je @f
--EXCEPT
--X         mov qword[rbp+32],:FreeStack    -- replace return address
            mov qword[rbp+56],:FreeStack    -- replace return address
            jmp :%opRetf
      @@:

        mov rax,[CClean]
        shr rax,2
        jz @f
            call rax
      @@:

        mov rsi,[ds+8]              -- (rsi:=raw addr of symtab[1])

        mov rcx,[rsi-24]            -- symlen
        mov r15,h4
      ::CleanupLoop
        mov rdi,[rsi+rcx*8-8]       -- symtab[rcx]
        sub rcx,1
        cmp rdi,r15
        jle @f
            mov rdx,[rbx+rdi*8+8]       -- symtab[rcx][S_NTyp]
            cmp rdx,1                   -- <=2 (S_Const or S_GVar2)
            jne :opAgvar
                test qword[rbx+rdi*8+24],#200   -- K_noclr
                jnz @f
          ::opAgvar
            cmp rdx,2
            jg @f
                -- S_Const w/o K_noclr and all S_Gvar2
                mov rax,[rsi+22*8]      -- rax:=symtab[T_ds4=23]
                mov rdi,[rbx+rdi*8+40]  -- rdi:=symtab[rcx][S_Slink]
                cmp [rsi+21*8],rbx      -- symtab[T_EBP=22]=0?
                je :compiled
                    shl rax,2
              ::compiled
                mov rdx,[rax+rdi*8+32]  -- rdx:=gvar[symtab[rcx][S_Slink]]
                cmp rdx,r15
                jle @f
                    sub qword[rbx+rdx*4-16],1
                    jnz @f
--6/11/16:
--                      mov [rax+rdi*8+32],ebx
                        mov [rax+rdi*8+32],rbx
--                      pushad
                        push rsi
                        push rcx
                        call :%pDealloc
--                      popad
                        pop rcx
                        pop rsi
      @@:
        cmp rcx,26                  -- T_const1
        jg :CleanupLoop

        mov rax,[rbp+48]        -- vsb_root
      @@:
        push qword[rax+8]       -- vsb_next
--pHeapD:
--<     mov rcx,9208
        mov rcx,11248
--DEV rdx:=?                    -- era
        call :%pFreePool        -- release rcx bytes of memory at rax.
        pop rax
        test rax,rax
        jnz @b
        xor rbp,rbp
    []
        ret

--/*
procedure :%NoCleanup(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%NoCleanup     -- ( this is not explicitly part of the VM )
---------------
        -- called by (eg) :!iDiag to make abort() etc invoke ExitProcess properly, 
        --  rather than try and return an error code to p.exw/pmain.e, which will
        --  then have a fit since c_cleanup() etc have not been called...
        -- Note this should stop :%RunCleanup being called, rather than change the
        --  way it works or otherwise bothering to test this flag.
        mov [nocleanup],1
        ret

--/*
procedure :%opAbort(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opAbort
-------------

--DEV cleanup code...


--/*
--      call :%pLoadMint (with era mods, or as below)
    [32]
        cmp eax,h4
        jl @f
            shl eax,2
--              jz e92vhnbaavespm9  -- or opUnassigned [DEV]
            jz :e87acmbi
            cmp byte[eax-1],0x12
            jne :e87acmbi   -- abort condition must be integer
            sub esp,8
            fld qword[eax]
            call :%down53
            fistp qword[esp]
            call :%near53
            mov eax,[esp]
            add esp,8
        @@:
    [64]
        pop al
    []
--*/

--!/*
    [32]
        mov esi,[ds+8]              -- esi:=raw addr of symtab[1]
        cmp eax,h4
        jl @f
            pop edx
            mov al,87               -- e87acmbi (abort code must be integer)
            sub edx,1
            jmp :!iDiag
            int3
      @@:
        cmp [ebp+16],ebx            -- exception handler
        je @f
            mov ecx,eax
            pop edx
            mov al,42               -- e42a
            sub edx,1
            jmp :!iDiag
            int3
      @@:
        mov ecx,[nocleanup]
        mov edx,[esi+84]            -- edx:=symtab[T_EBP=22]
        test ecx,ecx
        jnz @f
        test edx,edx
        jz @f
            -- interpreted
            mov ecx,[ebx+edx*4+4]   -- symtab[T_EBP][2] = esp4
            lea esp,[ebx+ecx*4-8]
            ret
          ::Exit0
            xor eax,eax
      @@:
--29/10/16:
        push eax                    -- save
        --
        -- If pfileioN.e has not been included this resolves to "call 0", aka "push <addr next instruction>"
        --  whereas, obviously, if pfileioN.e /has/ been included this does the expected thing, however it
        --  (ie ":!opClosem9") deliberately leaves the return address on the stack, for the "add esp,4" 
        --  after the call.
        --
        call :!opClosem9            -- (duplicates return addr, if called)
        add esp,4                   -- (discard "", or a call 0's "this")
        pop eax                     -- restore
    [64]
        mov r15,h4
        mov rsi,[ds+8]              -- rsi:=raw addr of symtab[1]
        cmp rax,r15
        jl @f
            pop rdx
            mov al,87               -- e87acmbi (abort code must be integer)
            sub rdx,1
            jmp :!iDiag
            int3
      @@:
        cmp [rbp+32],rbx            -- exception handler
        je @f
            mov rcx,rax
            pop rdx
            mov al,42               -- e42a
            sub rdx,1
            jmp :!iDiag
            int3
      @@:
        mov rcx,[nocleanup]
        mov rdx,[rsi+168]           -- rdx:=symtab[T_EBP=22]
        test rcx,rcx
        jnz @f
        test rdx,rdx
        jz @f
            -- interpreted
            mov rcx,[rbx+rdx*4+8]   -- symtab[T_EBP][2] = esp4
            lea rsp,[rbx+rcx*4-16]
--DEV (temp) for now, abort means abort (see exit_cb in edix) [FIXED]
            ret
--jmp @f
          ::Exit0
            xor rax,rax
      @@:
--29/10/16:
        push rax
        call :!opClosem9
        add rsp,8
        pop rax
    []


    [PE32]
        cmp eax,259
        jne @f
--          mov al,?? -- e??ia259: "illegal: abort(259)\n"
            int3    -- [DEV]
      @@:
        push eax                        -- uExitCode (for ExitProcess)
        call "kernel32.dll","ExitProcess"
--          ret
    [PE64]
        cmp rax,259
        jne @f
--          mov al,?? -- e??ia259: "illegal: abort(259)\n"
            int3    -- [DEV]
      @@:
        mov rcx,rsp -- put 2 copies of rsp onto the stack...
        push rsp
        push rcx
        or rsp,8    -- [rsp] is now 1st or 2nd copy:
                    -- if on entry rsp was xxx8: both copies remain on the stack
                    -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                    -- obviously rsp is now xxx8, whatever alignment we started with
        sub rsp,8*5
        mov rcx,rax                     -- uExitCode
        call "kernel32.dll","ExitProcess"
--          add rsp,8*5
--          pop rsp     -- restore, equivalent to rsp += (either #08 or #10)
--          mov rsp,[rsp+8*5]   -- equivalent to the add/pop
    [ELF32]
        mov ebx,eax                     -- error_code (p1)
--      mov eax,1                       -- sys_exit(ebx=int error_code)
        mov eax,252                     -- sys_exit_group(ebx=int error_code)
        int 0x80
--      xor ebx,ebx                     -- (common requirement after int 0x80)
    [ELF64]
        mov rdi,rax                     -- error_code (p1)
--      mov rax,60                      -- sys_exit(rdi=int error_code)
        mov rax,231                     -- sys_exit_group(rdi=int error_code) 
        syscall
    []

--/*
:%opAbort
---------

if debugleak
    mov [echk],0        ;; not all cleanup code has run; skip the memory check.
end if

    cmp edx,h4
    jl @f
        shl edx,2
        jz e92vhnbaavespm9
        cmp byte[edx-1],0x12
        jne e87acmbi    ; abort condition must be integer
        fld qword[edx]
        mov edx,FltWrk
        fldcw [down53]
        fistp qword[edx]
        fldcw [near53]
        mov edx,[edx]
    @@:

    push edx
    call opClosem9
cmp [ebpidx],0
jne @f
    call opFreeCons
    pop edx
    invoke ExitProcess,edx
@@:
    pop edx
    mov [ecode],edx
mov esp,[resp]
jmp :%opIaborted
--*/

}


