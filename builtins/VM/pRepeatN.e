DEAD - replaced with builtins/repeat.e
--
-- builtins/VM/pRepeatN.e
-- ======================
--
--without debug
include builtins\VM\pHeap.e     -- :%pAllocStr, :%pAllocSeq
-- include VM\pDiag.e   -- :%e52rcmbnni

--global function repeat(object o, integer count, integer type=0)
--  type=0: decide based on (integer(o) && 7<=o<=255)
--  type=8/string: error if o is not integer or <0 or >255; create a string
--  type=12/sequence: create a dword-sequence (do not test the value of o)
--  Note that the standard builtin type names of string and sequence can 
--       (specially) be used for p3 of repeat(), eg:
--          s = repeat(ch,len,string)
--          q = repeat(x,len,sequence)
--       but if you want a variable/parameter to call repeat with, you will 
--       have to define your own constants (with the values 0, 8, and/or 12).
--
--  if type=0 then
--      type = (integer(o) and o>=7 and o<=255) -- 1:create string, 0:create dword-sequence
--  elsif type=8 then -- string
--      if not integer(o) or o<0 or o>255 then fatal("repeat element not character")
--      type = 1
--  else
--      if type!=12 then fatal("repeat type must be any/string/sequence(0/8/12)")
--      type = 0
--  end if
--  if type then

--OR

--  global function repeatch(integer ch, integer type)
--  -- returns a string. Note the compiler automatically maps eg repeat('=',80) to
--  --  repeatch('=',80), and consequently improves type inference. It will also
--  --  map repeat('\0',80) to repeatch (ie using toktype=SQUOTE). Obviously
--  --  without appropriate clues, the dseq/str decision is made at run-time.


--/*
global function repeat(object o, integer count)
sequence res
    if count<0 then ?9/0 end if --DEV runtime error
--  if count<0 then fatal("repeat count must not be negative") end if
--  if count<0 then #ilASM{ jmp :%e52rcmbnni } end if
--  #ilASM{ cmp [count],0
--          jl :!e52rcmbnni }   -- (if -nodiag, jl 0 == next instruction, which is obviously
--                              --  going to crash and burn on error, but you did say -nodiag)
    if integer(o) and o>=7 and o<=255 then
--res = ""
        -- create a string (one byte per character/raw binary)
        #ilASM {
            [32]
                mov ecx,[count]
                call :%pAllocStr
                mov [res],eax
                lea edi,[ebx+eax*4]
                mov ecx,[count]
                mov eax,[o]
                rep stosb
            [64]
                mov rcx,[count]
                call :%pAllocStr
                mov [res],rax
                lea rdi,[rbx+rax*4]
                mov rcx,[count]
                mov rax,[o]
                rep stosb
            []
              }
    else
--res = {}
        -- create a d/qword-sequence
        #ilASM {
            [32]
                mov ecx,[count]
                call :%pAllocSeq
                mov [res],eax
                lea edi,[ebx+eax*4]
                mov eax,[o]
                mov ecx,[count]
                cmp eax,h4
                jl @f
                    add dword[ebx+eax*4-8],ecx
              @@:
                rep stosd
            [64]
                mov rcx,[count]
                call :%pAllocSeq
                mov [res],rax
                lea rdi,[rbx+rax*4]
                mov rax,[o]
                mov rcx,[count]
--              cmp rax,h4
                mov r15,h4
                cmp rax,r15
                jl @f
                    add qword[rbx+rax*4-16],rcx
              @@:
                rep stosq
            []
              }
    end if
    return res
end function
--*/

#ilASM{ jmp :%opRetf

--/*
procedure :%opRepeatCh(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opRepCh
-------------
        -- convention as below
        -- only called when the compiler knows eax is a character, eg repeat('\0',n)
        -- (maybe there could be a repeatch() builtin, but it would not check ch...)
    [32]
        cmp ecx,h4
    [64]
        mov r15,h4
        cmp rcx,r15
    []
        jb :repeatCh
     ::e52rcmbnni  -- repeat count must be non negative integer
    [32]
        pop edx
        mov al,52
        sub edx,1
        jmp :!iDiag
    [64]
        pop rdx
        mov al,52
        sub rdx,1
        jmp :!iDiag
    []
        int3

--/*
procedure :%opRepeat(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opRepeat
--------------
    [32]
        --calling convention:
        --  lea edi,[p1]    -- dest
        --  mov eax,[p2]    -- item to be repeated (opUnassigned)
        --  mov ecx,[p3]    -- count (opUnassigned)
        --  call :%opRepeat -- [edi]:=repeat(eax,ecx)
        cmp ecx,h4
        jae e52rcmbnni  -- repeat count must be non negative integer
        cmp eax,h4
        jg :repeatRef
        cmp eax,255
        ja :repeatSeq   -- (-ve and >255)
        -- DEV: Ideally, repeat(0,count) should return a dword-sequence,
        --      whereas repeat('\0',count) should return a string of nulls.
        --      maybe we sould have :%pRepeatCh which rejoins here
        cmp eax,7
        jl :repeatSeq   -- (0..6)
      ::repeatCh
        push eax
        call :%pAllocStr
        mov edx,[edi]
        mov [edi],eax
        lea edi,[ebx+eax*4]
        pop eax
        rep stosb
        mov [edi],bl
        jmp :repdealloc

      ::repeatRef
        add dword[ebx+eax*4-8],ecx
      ::repeatSeq
        mov edx,[esp]       -- era
        push eax
        call :%pAllocSeq
        mov edx,[edi]
        mov [edi],eax
        lea edi,[ebx+eax*4]
        pop eax
        rep stosd
      ::repdealloc
        cmp edx,h4
        jle @f
            sub dword[ebx+edx*4-8],1
            jz :%pDealloc
      @@:
        ret
    [64]
        --calling convention:
        --  lea rdi,[p1]    -- dest
        --  mov rax,[p2]    -- item to be repeated (opUnassigned)
        --  mov rcx,[p3]    -- count (opUnassigned)
        --  call :%opRepeat -- [edi]:=repeat(eax,ecx)
        mov r15,h4
        cmp rcx,r15
        jae e52rcmbnni  -- repeat count must be non negative integer
        cmp rax,r15
        jg :repeatRef
        cmp rax,255
        ja :repeatSeq   -- (-ve and >255)
        cmp rax,7
        jl :repeatSeq   -- (0..6)
      ::repeatCh
        push rax
        call :%pAllocStr
        mov rdx,[rdi]
        mov [rdi],rax
        lea rdi,[rbx+rax*4]
        pop rax
        rep stosb
        mov [rdi],bl
        jmp :repdealloc

      ::repeatRef
        add qword[rbx+rax*4-16],rcx
      ::repeatSeq
        mov rdx,[rsp]       -- era
        push rax
        call :%pAllocSeq
        mov rdx,[rdi]
        mov [rdi],rax
        lea rdi,[rbx+rax*4]
        pop rax
        rep stosq
      ::repdealloc
        cmp rdx,r15
        jle @f
            sub qword[rbx+rdx*4-16],1
            jz :%pDealloc
      @@:
        ret
    []
      }
