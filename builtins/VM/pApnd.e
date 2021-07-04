--
-- pApnd.e
-- =======
--
--  Implements :%opApnd (append()/prepend()), opConcat(&), opConcatN, and opCatsi.
--
--  The latter two are optimised forms of opConcat, respectively for the a&b&c[&...] 
--  case, and when concatenating sequences of integer, so refcounting can be avoided.
--

-- Note: originally commented as a = append(b,c), if you find any such references
--       remaining, map them to p1 = append(p2,p3) (pretty please)

-- Technical note: As per pfileioN.e, append guarantees to have all content in place
--                  before the ref is updated. Similar does NOT apply to prepend or
--                  concat. (see "Multithreading issues" in pfileioN.e)

include VM\pHeap.e  -- :%pDealloc/:%pAllocSeq/:%pAllocStr/:%pFreePool


#ilASM{ jmp :%opRetf

--DEV fixme... (new error code, unknown type byte [internal error])
 ::eNNunknown
    int3

--/*
procedure :%opApnd(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opApnd                    -- p1=append/prepend(p2,p3); also several p1=p2&p3 cases, see opConcat
------------
    [32]
        --calling convention (p1 = append/prepend(p2,p3))
        --  lea edx,[p1]
        --  lea edi,[p2]    (opUnassigned)???[DEV]
        --  lea ecx,[p3]    (opUnassigned)
        --  mov eax,0       or 1 for prepend
        --  call :%opApnd               -- [edx]=append([edi],[ecx])
        push eax    -- flag@[esp+12] (1=prepend, 0=append)
        push ecx    -- addr p3@[esp+8]
        push edi    -- addr p2@[esp+4]
        push edx    -- addr p1@[esp]
        mov esi,[edi]           -- ref of p2
        mov ecx,[ecx]           -- ref of p3
    ::opApndA   -- (entry point for p1=p2&p3 -> p1=append(p2,p3) when p3 atom.)
        cmp esi,h4
        jl :ApndNewSeq2
    :%opPpndSA  -- (entry point for p1=p2&p3 -> p1=prepend(p3,p2) when p2 atom, see ppMap below.)
                -- exception here mapped to e93vhnbaav(edi)
        mov al,[ebx+esi*4-1]    -- type byte
        mov edx,[ebx+esi*4-12]  -- length
        cmp al,0x12
        je :ApndNewSeq2
        shl esi,2
        
        cmp al,0x80
        je :ApndSeq
        cmp al,0x82
        jne :eNNunknown
        cmp ecx,#FF
        ja :ApndExpandString    -- (if not #00..#FF)
        mov eax,ecx             -- save char
        cmp dword[esi-8],1      -- check ref count of 1
        jne :ApndNewStr
        cmp dword[esp+12],0     -- (check prepend flag)
        jne :ApndNewStr         -- (prepend must create a new string)   [the old opPpnd did it in situ, but I doubt that gains any]
        cmp edi,[esp]           -- must also be pbr-optimise (x=append(x,y))
        jne :ApndNewStr         --  (we already know it is not circular, ie not x=append(x,x), 
                                --   as p3 is a char here, and p1(==p2) is a string)
        lea ecx,[edx+16+1]
--  mov edi,[esi-20]        -- slack (at start/left)
--  sub ecx,edi     -- DOH! no slack on strings!
        cmp ecx,[esi-16]        -- maxlen
        je :ApndNewStr          -- and have space/check maxlen
        -- (jge would also be fine, though it will/shd never be g)
        -- so expand string by one char in situ:
        lea ecx,[edx+1]
        mov word[esi+edx],ax    -- includes a new trailing null
        mov [esi-12],ecx        -- increase length
        add esp,16
        ret
    [64]
        --calling convention (p1 = append/prepend(p2,p3))
        --  lea rdx,[p1]
        --  lea rdi,[p2]    (opUnassigned)
        --  lea rcx,[p3]    (opUnassigned)
        --  mov rax,0       or 1 for prepend
        --  call :%opApnd               -- [rdx]=append([rdi],[rcx])
        push rax    -- flag@[rsp+24] (1=prepend, 0=append)
        push rcx    -- p3@[rsp+16]
        push rdi    -- p2@[rsp+8]
        push rdx    -- p1@[rsp]
        mov rsi,[rdi]           -- ref of p2
        mov rcx,[rcx]           -- ref of p3
    ::opApndA   -- (entry point for p1=p2&p3 -> p1=append(p2,p3) when p3 atom.)
        mov r15,h4
        cmp rsi,r15
        jl :ApndNewSeq2
    :%opPpndSA  -- (entry point for p1=p2&p3 -> p1=prepend(p3,p2) when p2 atom, see ppMap below.)
                -- exception here mapped to e93vhnbaav(rdi)
        mov al,[rbx+rsi*4-1]    -- type byte
        mov rdx,[rbx+rsi*4-24]  -- length
        cmp al,0x12
        je :ApndNewSeq2
        shl rsi,2
        
        cmp al,0x80
        je :ApndSeq
        cmp al,0x82
        jne :eNNunknown
        cmp rcx,#FF
        ja :ApndExpandString    -- (if not #00..#FF)
        mov rax,rcx             -- save char
        cmp qword[rsi-16],1     -- check ref count of 1
        jne :ApndNewStr
        cmp qword[rsp+24],0     -- (check prepend flag)
        jne :ApndNewStr         -- (prepend must create a new string)   [the old opPpnd did it in situ, but I doubt that gains any]
        cmp rdi,[rsp]           -- must also be pbr-optimise (x=append(x,y))
        jne :ApndNewStr         --  (we already know it is not circular, ie not x=append(x,x), 
                                --   as p3 is a char here, and p1(==p2) is a string)
        lea rcx,[rdx+32+1]
--  mov edi,[esi-20]        -- slack (at start/left)
--  sub ecx,edi     -- DOH! no slack on strings!
        cmp rcx,[rsi-32]        -- maxlen
        je :ApndNewStr          -- and have space/check maxlen
        -- (jge would also be fine, though it will/shd never be g)
        -- so expand string by one char in situ:
        lea rcx,[rdx+1]
        mov word[rsi+rdx],ax    -- includes a new trailing null
        mov [rsi-24],rcx        -- increase length
        add rsp,32
        ret
    []

  ::ApndNewSeq2
---------------
        -- new sequence length 2 (p2 is atom)
    [32]
        mov edi,ecx
        cmp dword[esp+12],0         -- prepend?
        je @f
--      jne @f
            xchg esi,edi
      @@:
        cmp esi,#FF
        ja @f
            cmp edi,#FF
            jbe :ApndNewStr2
      @@:
        mov ecx,2
        mov edx,[esp+16]            -- era
        call :%pAllocSeq
        mov [ebx+eax*4],esi         -- new[1]:=p2
--      mov [ebx+eax*4],edi         -- new[1]:=p2
        mov edx,[esp]
        cmp esi,h4
        jl @f
            add dword[ebx+esi*4-8],1
      @@:
        mov [ebx+eax*4+4],edi       -- new[2]:=p3
        cmp edi,h4
        jl @f
            add dword[ebx+edi*4-8],1
      @@:
        mov ecx,[edx]
        add esp,16
        mov [edx],eax
        cmp ecx,h4
--18/2/15!
--      jl @f
        jle @f
            mov edx,ecx
            sub dword[ebx+ecx*4-8],1
            jz :%pDealloc
      @@:
        ret
    [64]
        mov rdi,rcx
        cmp qword[rsp+24],0         -- prepend?
        je @f
--      jne @f
            xchg rsi,rdi
      @@:
        cmp rsi,#FF
        ja @f
            cmp rdi,#FF
            jbe :ApndNewStr2
      @@:
        mov rcx,2
        mov rdx,[rsp+32]            -- era
        call :%pAllocSeq
        mov [rbx+rax*4],rsi         -- new[1]:=p2
--      mov [ebx+eax*4],edi         -- new[1]:=p2
        mov rdx,[rsp]
        cmp rsi,r15
        jl @f
            add qword[rbx+rsi*4-16],1
      @@:
        mov [rbx+rax*4+8],rdi       -- new[2]:=p3
        cmp rdi,r15
        jl @f
            add qword[rbx+rdi*4-16],1
      @@:
        mov rcx,[rdx]
        add rsp,32
        mov [rdx],rax
        cmp rcx,r15
--18/2/15!
--      jl @f
        jle @f
            mov rdx,rcx
            sub qword[rbx+rcx*4-16],1
            jz :%pDealloc
      @@:
        ret
    []
    
  ::ApndNewStr2
---------------
        -- new string length 2 (p2 and p3 are both #00..#FF)
        --
        --  (you could probably drop this if it causes any problems, just
        --   as long as you also document append(chr,chr)->dword_sequence; 
        --   it was more because of that not really sounding quite right, 
        --   as opposed to it being clearly wrong, that this went in..)
        --
    [32]
--DEV (this might want to be shl edi,8)
--      shl esi,8                   -- (to make a 16-bit word of the two chars)
        shl edi,8
        mov ecx,2
        add esi,edi
        mov edi,[esp]
        call :%pAllocStr
        mov edx,[edi]
        mov ecx,esi
        add esp,16
--DEV try ecx (min string length is 19), or for that matter esi (null terminator and 1 more 0)
        mov [ebx+eax*4],cx
        mov [ebx+eax*4+2],bl        -- and a null terminator
        mov [edi],eax
        cmp edx,h4
--26/1/15!
--      jl @f
        jle @f
            sub dword[ebx+edx*4-8],1
            jz :%pDealloc
      @@:
        ret
    [64]
        shl rdi,8                   -- (to make a 16-bit word of the two chars)
        mov rcx,2
        add rsi,rdi
        mov rdi,[rsp]
        call :%pAllocStr
        mov rdx,[rdi]
        mov rcx,rsi
        add rsp,32
--DEV try rcx (min string lenth is 31), or for that matter, rsi (null terminator and 5 more 0s)
        mov [rbx+rax*4],cx
        mov [rbx+rax*4+2],bl        -- and a null terminator
        mov [rdi],rax
        cmp rdx,r15
--26/1/15!
--      jl @f
        jle @f
            sub qword[rbx+rdx*4-16],1
            jz :%pDealloc
      @@:
        ret
    []

  ::ApndNewStr
--------------
    [32]
        mov edi,[esp+12]        -- prepend flag
        push eax                --[1] save char
        lea ecx,[edx+1]         -- original length+1
        call :%pAllocStr        -- damages eax only
        lea edi,[edi+eax*4]     -- edi -> new[1] for append, new[2] for prepend
        mov ecx,edx
----DEV:
--                   -- This shaves about 9% off the total time for append():
--                   shr ecx,2
--                   and dl,0x3
--                   rep movsd
--                   mov cl,dl
        rep movsb
        mov esi,[esp+4]         -- target address
        pop ecx                 --[1] restore char
        cmp dword[esp+12],0
        je :normalappend
            mov [edi],bl        -- trailing null
            mov [ebx+eax*4],cl  -- plant prepended char at new[1]
            jmp @f
      ::normalappend
            mov [edi],cx        -- includes trailing null
      @@:
        mov edx,[esi]           -- previous value (if any)
        add esp,16
        mov [esi],eax           -- newly allocated ref
        cmp edx,h4
        jle @f
            sub dword[ebx+edx*4-8],1
            jz :%pDealloc
      @@:
        ret
    [64]
        mov rdi,[rsp+24]        -- prepend flag
        push rax                --[1] save char
        lea rcx,[rdx+1]         -- original length+1
        call :%pAllocStr        -- damages eax only
        lea rdi,[rdi+rax*4]     -- rdi -> new[1] for append, new[2] for prepend
        mov rcx,rdx
----DEV:
--                   -- This shaves about 9% off the total time for append():
--                   shr rcx,3
--                   and dl,0x7
--                   rep movsq
--                   mov cl,dl
        rep movsb
        mov rsi,[rsp+8]         -- target address
        pop rcx                 --[1] restore char
        cmp qword[rsp+24],0
        je :normalappend
            mov [rdi],bl        -- trailing null
            mov [rbx+rax*4],cl  -- plant prepended char at new[1]
            jmp @f
      ::normalappend
            mov [rdi],cx        -- includes trailing null
      @@:
        mov rdx,[rsi]           -- previous value (if any)
        add rsp,32
        mov [rsi],rax           -- newly allocated ref
        cmp rdx,r15
        jle @f
            sub qword[rbx+rdx*4-16],1
            jz :%pDealloc
      @@:
        ret
    []

  ::ApndExpandString
--------------------
    [32]
        mov edi,[esp+12]        -- prepend flag...
        push ecx                --[2] save p3 ref
        shl edi,2               --              ...->dword
        lea ecx,[edx+1]         -- length+1
        mov edx,[esp+16]        -- era
        call :%pAllocSeq        -- damages eax only
--  mov edi,eax
--  shl edi,2
--DEV flag (the old PpndExpandString put slack at start, but I doubt that gains any) (done)
        lea edi,[edi+eax*4]     -- edi->(append:new[1], or prepend:new[2])
--      lea edi,[ebx+eax*4]
        sub ecx,1
        je :ApndExpandZero      -- (append(<empty string>,<non-char>) case)
        push eax                --[3] save new ref
        xor eax,eax
      @@:
        lodsb                   -- mov al,[esi], esi+=1
        stosd                   -- mov [edi],eax; edi+=4
--      mov al,[esi]
--      inc esi
--      mov [edi],eax
--      add edi,4
        sub ecx,1
        jnz @b
        pop eax                 --[3] restore new ref
     ::ApndExpandZero
        mov esi,[esp+4]         -- target address
        pop ecx                 --[2] restore p3 ref
--DEV flag (done)
        cmp dword[esp+12],0     -- if prepend
        je @f
            lea edi,[ebx+eax*4] -- replace new[1] (not new[$])
      @@:
        add esp,16
        mov edx,[esi]           -- previous value (if any)
        cmp ecx,h4
        jl @f
--    ::ApndExpandStrUa:            -- exception here mapped to e92vhnbaavap3feh (now opUnassigned [DEV])
            add dword[ebx+ecx*4-8],1
      @@:
        mov [edi],ecx
        mov [esi],eax           -- newly allocated ref
        cmp edx,h4
        jle @f
            sub dword[ebx+edx*4-8],1
            jz :%pDealloc
      @@:
        ret
    [64]
        mov rdi,[rsp+24]        -- prepend flag...
        push rcx                --[2] save p3 ref
--      shl rdi,2               --              ...->dword
        shl rdi,3               --              ...->qword
        lea rcx,[rdx+1]         -- length+1
        mov rdx,[rsp+32]        -- era
        call :%pAllocSeq        -- damages eax only
        lea rdi,[rdi+rax*4]     -- rdi->(append:new[1], or prepend:new[2])
        sub rcx,1
        je :ApndExpandZero      -- (append(<empty string>,<non-char>) case)
        push rax                --[3] save new ref
        xor rax,rax
      @@:
        lodsb                   -- mov al,[rsi], rsi+=1
        stosq                   -- mov [rdi],rax; rdi+=8
        sub rcx,1
        jnz @b
        pop rax                 --[3] restore new ref
     ::ApndExpandZero
--      mov rsi,[rsp+4]         -- target address
        mov rsi,[rsp+8]         -- target address
        pop rcx                 --[2] restore p3 ref
        cmp dword[rsp+24],0     -- if prepend
        je @f
            lea rdi,[rbx+rax*4] -- replace new[1] (not new[$])
      @@:
        add rsp,32
        mov rdx,[rsi]           -- previous value (if any)
        cmp rcx,r15
        jl @f
            add qword[rbx+rcx*4-16],1
      @@:
        mov [rdi],rcx
        mov [rsi],rax           -- newly allocated ref
        cmp rdx,r15
        jle @f
            sub qword[rbx+rdx*4-16],1
            jz :%pDealloc
      @@:
        ret
    []

  ::ApndSeq
-----------
    [32]
        -- recap:
        --  addr p1 (result)    @ [esp]
        --  addr p2 in edi and  @ [esp+4]
        --  addr p3             @ [esp+8]
        --  prepend flag        @ [esp+12]
        --  era                 @ [esp+16]
        --  esi = raw(p2)
        --  edx = length(p2) (==[esi-12])
        --  al is 0x80      (==[esi-1])
        --  ecx is ref(p3)
        --  edi is addr p2
        --
        cmp ecx,h4
        jl @f
            add dword[ebx+ecx*4-8],1
      @@:
        push ecx                --[1] save new element
--      cmp edi,[esp]           -- must be pbr-optimise (x=append(x,..)) [BUG! (push ecx)]
        cmp edi,[esp+4]         -- must be pbr-optimise (x=append(x,..)) (cmp addr p2, addr p1)
--11/5/21:
--      jne :ApndNewSeq
        jne :ApndNewSeqClone
        cmp dword[esi-8],1      -- check ref count of 1
        jne :ApndNewSeqClone    -- (opAlloClone then pAllocSeq)
--      jne :ApndNewSeq
--      cmp edi,[esp+8]         -- and not circular (x=append(x,x)) [BUG! (push ecx)]
        cmp edi,[esp+12]        -- and not circular (x=append(x,x)) (cmp addr p2, addr p3)
        je :ApndNewSeq

        lea ecx,[ebx+edx*4+20]  -- required (old) size in bytes
        mov edi,[esi-20]        -- slack (at start/left)
        cmp dword[esp+16],0     -- prepend flag
        jne :PpndSeq
        add ecx,edi
        cmp ecx,[esi-16]        -- check maxlen (allocated size in bytes)
        je :ApndSeqClone        -- (exactly full)

        pop eax                 --[1]
        lea ecx,[edx+1]
        mov [esi+edx*4],eax     -- in situ
        mov [esi-12],ecx        -- increase length
        add esp,16
        ret
    [64]
        -- recap:
        --  addr p1 (result)    @ [rsp]
        --  addr p2 in rdi and  @ [rsp+8]
        --  addr p3             @ [rsp+16]
        --  prepend flag        @ [rsp+24]
        --  era                 @ [rsp+32]
        --  rsi = raw(p2)
        --  rdx = length(p2) (==[rsi-24])
        --  al is 0x80      (==[rsi-1])
        --  rcx is ref(p3)
        --  rdi is addr p2
        --
        cmp rcx,r15
        jl @f
            add qword[rbx+rcx*4-16],1
      @@:
        push rcx                --[1] save new element
--      cmp edi,[esp]           -- must be pbr-optimise (x=append(x,..)) [BUG! (push ecx)]
        cmp rdi,[rsp+8]         -- must be pbr-optimise (x=append(x,..)) (cmp addr p2, addr p1)
--11/5/21:
--      jne :ApndNewSeq
        jne :ApndNewSeqClone
        cmp qword[rsi-16],1     -- check ref count of 1
        jne :ApndNewSeqClone    -- (pAlloClone then pAllocSeq)
--      jne :ApndNewSeq
--      cmp edi,[esp+8]         -- and not circular (x=append(x,x)) [BUG! (push ecx)]
        cmp rdi,[rsp+24]        -- and not circular (x=append(x,x)) (cmp addr p2, addr p3)
        je :ApndNewSeq

--      lea rcx,[rbx+rdx*4+40]  -- required (old) size in bytes
        lea rcx,[rbx+rdx*4+20]  -- half required (old) size in bytes
        mov rdi,[rsi-40]        -- slack (at start/left)
        shl rcx,1               -- required (old) size in bytes
        cmp qword[rsp+32],0     -- prepend flag
        jne :PpndSeq
        add rcx,rdi
        cmp rcx,[rsi-32]        -- check maxlen (allocated size in bytes)
        je :ApndSeqClone        -- (exactly full)

        pop rax                 --[1]
        lea rcx,[rdx+1]
        mov [rsi+rdx*8],rax     -- in situ
        mov [rsi-24],rcx        -- increase length
        add rsp,32
        ret
    []

  ::PpndSeq
-----------
    [32]
        test edi,edi            -- is slack 0?
        jz :PpndSeqClone
        -- shift header left and calc/store updated ref
        sub edi,4               -- new slack
        add edx,1               -- new length
        mov ecx,[esi-16]        -- maxlen
        mov [esi-24],edi        -- new slack
        mov [esi-20],ecx        -- copy maxlen
        mov ecx,[esi-4]         -- type/delete routine
        mov edi,[esp+4]         -- addr p1
        mov [esi-16],edx        -- set new length
        mov eax,esi
        mov dword[esi-12],1     -- refcount (we know it is 1)
        sub eax,3
        mov [esi-8],ecx         -- type/delete routine
        ror eax,2
        pop dword[esi-4]        -- new element
        mov [edi],eax           -- new ref of shifted header -> p1
        add esp,16
        ret
    [64]
        test rdi,rdi            -- is slack 0?
        jz :PpndSeqClone
        -- shift header left and calc/store updated ref
        sub rdi,8               -- new slack
        add rdx,1               -- new length
        mov rcx,[rsi-32]        -- maxlen
        mov [rsi-48],rdi        -- new slack
        mov [rsi-40],rcx        -- copy maxlen
        mov rcx,[rsi-8]         -- type/delete routine
        mov rdi,[rsp+8]         -- addr p1
--      mov [rsi-24],rdx        -- set new length
        mov [rsi-32],rdx        -- set new length
        mov rax,rsi
        mov qword[rsi-24],1     -- refcount (we know it is 1)
        sub rax,7
        mov [rsi-16],rcx        -- type/delete routine
        ror rax,2
--DEV fix this in pilasm.e
        pop qword[rsi-8]        -- new element
--      pop rcx                 -- new element
        mov [rdi],rax           -- new ref of shifted header -> p1
--      mov [rsi-8],rcx         -- new element
        add rsp,32
        ret
    []  

  ::ApndSeqClone
----------------
        -- Fairly obviously, "clone" differs from "new" in that we just
        --  copy the contents rather than incref/decref each one, and 
        --  pFreePool the old empty container rather than pDealloc it.
        -- An "append clone" always creates a "hard left" sequence; any
        --  subsequent prepend will preserve (some) free space on the 
        --  end, leaving space for both append and prepend to utilise.
    [32]
        --
        -- recap:
        --  ref(p3)             @ [esp]
        --  addr p1 (result)    @ [esp+4]
        --  addr p2             @ [esp+8] (==[esp+4],!=[esp+12])
        --  addr p3             @ [esp+12]
        --  prepend flag        @ [esp+16] (==0)
        --  era                 @ [esp+20]
        --  esi = raw(p2)    ([esi-8]==1)
        --  edx = length(p2) (==[esi-12])
        --  al is 0x80       (==[esi-1])
        --  edi = slack(p2) (==[esi-20])
        --  ecx is edi+edx*4+20 (==[esi-16] aka maxlen)
        --
        push esi                --[2] save p1=p2 raw address
        lea ecx,[edx+1]         -- length+1
        mov edx,[esp+24]        -- era
        call :%pAllocSeq        -- damages eax only
        mov edx,[esi-4]         -- copy type/delete_index
--DEV have I reintroduced this anywhere? (only applies to append, not prepend, not concat)
-- 15/06/13 (multithreading issues, content must be in place before ref updated, see pfileioN.e):
--  mov [edi],eax           -- replace p1 now
        mov [ebx+eax*4-4],edx
        sub ecx,1
        lea edi,[ebx+eax*4]     -- convert to real address (new[1])
--      mov edx,[esp+4]         -- target address
        mov edx,[esp+8]         -- target address
        rep movsd
--15/06/13:
        mov [edx],eax
        pop edx                 --[2] raw address of original p1 (=p2)
        pop eax                 --[1] (as pushed by ApndSeq)
        stosd                   -- mov [edi],eax; edi+=4 (not that we need edi again)
        mov ecx,[edx-16]        -- maxlen (allocated size in bytes)
        lea eax,[edx-20]
        add esp,16
        sub eax,[edx-20]        -- account for any slack
        mov edx,[esp]
        jmp :%pFreePool         -- free ecx bytes at eax (ignores any delete_routine)
    [64]
        push rsi                --[2] save p1=p2 raw address
        lea rcx,[rdx+1]         -- length+1
        mov rdx,[rsp+48]        -- era
        call :%pAllocSeq        -- damages eax only
        mov rdx,[rsi-8]         -- copy type/delete_index
        mov [rbx+rax*4-8],rdx
        sub rcx,1
        lea rdi,[rbx+rax*4]     -- convert to real address (new[1])
--      mov rdx,[rsp+8]         -- target address
        mov rdx,[rsp+16]        -- target address
        rep movsq
        mov [rdx],rax
        pop rdx                 --[2] raw address of original p1 (=p2)
        pop rax                 --[1] (as pushed by ApndSeq)
        stosq                   -- mov [rdi],rax; rdi+=8 (not that we need rdi again)
        mov rcx,[rdx-32]        -- maxlen (allocated size in bytes)
        lea rax,[rdx-40]
        add rsp,32
        sub rax,[rdx-40]        -- account for any slack
        mov rdx,[rsp]
        jmp :%pFreePool         -- free rcx bytes at rax (ignores any delete_routine)
    []

  ::PpndSeqClone
----------------
        --
        -- Request (existing length)*2+1, then plant the header slap in the middle.
        -- This leaves space for both append and prepend to utilise.
        --
        -- Technical/design note: [DEV this isn't helpful...]
        -- Analysis of the worst case scenarios yielded worst case points of:
        --      {0,0,2,7,17,37...} at [0,1,6,16,36,76] (32-bit) and 
        --      {0,2,6,15,33,69,...} at [0,5,14,32,68,140] (64-bit)
        -- ie there are a couple of (frankly rather unlikely) painful/pointless/could 
        -- be done in situ cases, but only on such small sizes no-one really cares, 
        -- and as the sizes start to get larger the low points gradually get less and 
        -- less painful (as well as less and less likely), exactly what we want. What
        -- I mean by worst case scenario, I should perhaps have mentioned, is various
        -- combinations of append/prepend that cause early clones when technically
        -- there is still space, but at the wrong end, compared that is to all-appends 
        -- and all-prepends, which perform about as good as we might hope them to.
        -- I only mention this to avoid anyone else noticing odd/unhelpful behaviour 
        -- (at small sizes) and not seeing the bigger picture, as I first did (before
        -- attempting a much more convoluted strategy, that I never got working, and,
        -- to be frank, was never going to do any better). Just in case you can find
        -- any use for it, the wcs analysis, focusing on a single prepend followed by
        -- enough appends to discard slack and clone to a naively chosen new size:
        --
        --  --
        --  -- wscditty.exw (worst case scenario analysis)
        --  --
        --  constant MWORD = 4, SBLOCK = 20 -- 32 bit
        --  --constant MWORD = 8, SBLOCK = 36 -- 64 bit
        --  -- (see "Revised Sizes" in pHeap.e for an explanation of SBLOCK being 20/34)
        --  constant SHDR = MWORD*5
        --
        --  integer reqlen, maxlen, allocated, newlen, slack, spare, full, fulllen, afull
        --  puts(1,"--      l       new length    slack(lhs)  spare(rhs)  appfull\n")
        --  for l=1 to MWORD*20 do -- (new length)
        --      reqlen = (l*2-1) -- (=== existinglength*2 /+/ 1)
        --      maxlen = reqlen*MWORD+SHDR
        --      allocated = SBLOCK
        --      while maxlen>allocated-MWORD do allocated*=2 end while
        --      newlen = floor((allocated-(SHDR+MWORD))/MWORD)
        --      spare = newlen-l
        --      slack = floor(spare/2)
        --      spare -= slack
        --      full = spare+l+1
        --      fulllen = full*MWORD+SHDR
        --      allocated = SBLOCK
        --      while fulllen>allocated-MWORD do allocated*=2 end while
        --      afull = (allocated-(SHDR+MWORD))/MWORD
        --      printf(1,"-- %4d(->%2d) %5d->%2d %10d %11d %10d->%2d    (%d)\n",
        --               {l-1,l,reqlen,newlen,slack,spare,full,afull,afull-full})
        --  end for
        --  if getc(0) then end if

--  [1]                               20[#00000014]=4+16            n/a                 n/a
--  [2]                               40[#00000028]=4+36            x(19)               s[4]
--  [3]                               80[#00000050]=4+76            x(59)               s[14]
--  [4]                              160[#000000A0]=4+156           x(139)              s[34]
--  [5]                              320[#00000140]=4+316           x(299)              s[74]

--      l       new length    slack(lhs)  spare(rhs)  appfull                   ecx     new maxlen      -   slack
--    0(-> 1)     1-> 4          1           2          4-> 4    (0)            24      36              12    4
--    1(-> 2)     3-> 4          1           1          4-> 4    (0)            28                      8     4
--    2(-> 3)     5->14          5           6         10->14    (4)            32      76              44    20
--    3(-> 4)     7->14          5           5         10->14    (4)            36                      40    20
--    4(-> 5)     9->14          4           5         11->14    (3)            40                      36    16
--    5(-> 6)    11->14          4           4         11->14    (3)            44                      32    16
--    6(-> 7)    13->14          3           4         12->14    (2)            48                      28    12
--    7(-> 8)    15->34         13          13         22->34    (12)           52      156             104   52

--DEV/DOC
-- Technical note:
--  Referring to the "Multithreading issues" as documented in VM/pfileioN.e, prepend
--  does NOT currently honor any promise of contents in place before ref is updated;
--  that only applies to append. Of course there is no issue if your table(s) etc are 
--  properly locked, or if your application is not multi-threaded.
--
-- Performance note:
--  Please don't read this in an alarmist fashion, or be put off from using prepend.
--  There /is/ a difference, and that difference deserves to be properly documented,
--  but in reality it is nothing to get all excited or bothered about.
--
--  Benchmark speaking, append is a better choice than prepend for top performance.
--  While prepend on dword-sequences delivers about the same performance as using
--  append, string = prepend(string,char) exhibits exponentially worse performance 
--  when compared to string = append(string,char), because strings must always be 
--  aligned to start on a dword/qword boundary. Also, when append needs to enlarge 
--  a dword-sequence it places all the spare space at the end, whereas in the same 
--  situation prepend splits any newly allocated extra space between the front and 
--  back, so that it is available for both append and prepend to utilise, which
--  avoids any "shunting" effects should they be mixed. This means that prepend has
--  "twice the clone overhead" of append, occurring every *1.5 instead of every *2, 
--  and uses about 25% more memory, which translates to a few percent points slower 
--  overall, but in most cases that is less than can actually be measured.
--
--  Hence, by all means use prepend when it makes your program easier or clearer,
--  and is unlikely to be invoked more than several million times. You can find
--  dozens of uses of prepend in the Phix sources, despite the fact that they are
--  rather aggressively optimised[1], and the same can be said for Edita. Likewise
--  printf() uses string=prepend(string,char), because "exponentially slower" just
--  doesn't matter one jot when building ~20 character long strings[2].
--
--  [1] Almost no effort has gone into optimising Phix /after/ a fatal error has
--  occurred, hence prepend tends to be freely used in the error reporting side,
--  but admittedly not so much in the pre-error-so-must-be-uber-fast bits.
--  [2] 20 chars takes some N+210 clocks instead of N+20, where N is at the very
--  least ~20,000: calculating each digit totally swamps any prepend overheads,
--  and even a report containing 100,000 so-generated numbers won't make that
--  190 clocks tally up to anywhere near 0.1s, so you and I would have to live
--  for a very long time indeed to regret such a miniscule overhead. Of course
--  if your length(string)>20K, then that's a very different matter altogether,
--  and you would probably be better off building it as a dword-sequence before
--  packing it back to a string once done, or doing it line-by-line. Also, if I
--  really wanted to, I could have removed the "offending" code in printf() in
--  far less time than it just took me to write this paragraph.
--

    [32]
        mov [esp+16],esi            -- save raw address (overwrite prepend flag[==1])
        -- recap:
        --  [p3] (increfed)     @ [esp]
        --  addr p1 (result)    @ [esp+4]   (with a refcount of 1)
        --  addr p2 in edi and  @ [esp+8]   (== addr p1) [pbr-optimise]
        --  addr p3             @ [esp+12]  (!= addr p1) [non-circular]
        --  esi = raw(p2)       = [esp+16]  (was prepend flag of 1)
        --  era                 @ [esp+20]
        --  edi is slack    (==[esi-20]==0)
        --  edx = length(p2) (==[esi-12])
        --  ecx is (edx*4+20), required (old) size in bytes (which might prove useful!)
        --
        mov edi,ecx                 -- save old size in bytes
        lea ecx,[ebx+edx*2+1]       -- length*2+1
        mov edx,[esp+20]            -- era
        call :%pAllocSeq            -- damages eax only
        mov edx,[ebx+eax*4-16]      -- new maxlen
        mov ecx,[esi-12]            -- old length
        push edx                    --[1] copy new maxlen
        sub edx,edi                 -- less old (used) size in bytes
        add ecx,1                   -- new length
        shr edx,1                   -- /2
        push dword[esi-4]           --[2] copy type/delete_rtn
        and edx,#FFFFFFFC           -- new slack, truncated to whole number of dwords
--DEV sopping great AGI here...
        lea edi,[edx+eax*4+4]       -- new raw address[2]
--...so, tryme:
--      lea edi,[ebx+eax*4+4]
--      add edi,edx
        mov [edx+eax*4-12],ecx      -- set new length
        lea eax,[edx+eax*4+1]       -- new ref part 1 (slack+raw+1)
        mov ecx,[esi-12]            -- original length (for rep movsd)
        -- plant new header and replace ref now...
        mov [edi-24],edx            -- new slack
        ror eax,2                   -- new ref part 2
        mov edx,[esp+12]            -- target addr (p1) [@[esp+4] on entry to PpndSeqClone]
        pop dword[edi-8]            --[2] type/delete_rtn
        pop dword[edi-20]           --[1] copy maxlen
        mov dword[edi-12],1         -- refcount
        mov [edx],eax
        mov edx,esi                 -- (save for pFreePool)
        pop dword[edi-4]            -- new[1]
        rep movsd
        mov ecx,[edx-16]        -- maxlen (allocated size in bytes)
        lea eax,[edx-20]
--      sub eax,[edx-20]        -- account for any slack    [known to be 0]
        add esp,16
        mov edx,[esp]
        jmp :%pFreePool         -- free ecx bytes at eax (ignoring any delete_routine)
    [64]
        mov [rsp+32],rsi            -- save raw address (overwrite prepend flag[==1])
        -- recap:
        --  [p3] (increfed)     @ [rsp]
        --  addr p1 (result)    @ [rsp+8]   (with a refcount of 1)
        --  addr p2 in rdi and  @ [rsp+16]  (== addr p1) [pbr-optimise]
        --  addr p3             @ [rsp+24]  (!= addr p1) [non-circular]
        --  rsi = raw(p2)       = [rsp+32]  (was prepend flag of 1)
        --  era                 @ [rsp+40]
        --  rdi is slack    (==[rsi-40]==0)
        --  rdx = length(p2) (==[rsi-24])
--      --  rcx is (rdx*4+40), required (old) size in bytes (which might prove useful!)
        --  rcx is (rdx*8+40), required (old) size in bytes (which might prove useful!)
        --
        mov rdi,rcx                 -- save old size in bytes
        lea rcx,[rbx+rdx*2+1]       -- length*2+1
        mov rdx,[rsp+40]            -- era
        call :%pAllocSeq            -- damages eax only
        mov rdx,[rbx+rax*4-32]      -- new maxlen
        mov rcx,[rsi-24]            -- old length
        push rdx                    --[1] copy new maxlen
        sub rdx,rdi                 -- less old (used) size in bytes
        add rcx,1                   -- new length
        shr rdx,1                   -- /2
        push qword[rsi-8]           --[2] copy type/delete_rtn
        and rdx,#FFFFFFF8           -- new slack, truncated to whole number of qwords
--DEV sopping great AGI here...
        lea rdi,[rdx+rax*4+8]       -- new raw address[2]
--...so, tryme:
--      lea rdi,[rbx+rax*4+8]
--      add rdi,rdx
        mov [rdx+rax*4-24],rcx      -- set new length
        lea rax,[rdx+rax*4+1]       -- new ref part 1 (slack+raw+1)
        mov rcx,[rsi-24]            -- original length (for rep movsd)
        -- plant new header and replace ref now...
--      mov [rdi-48],edx            -- new slack (rdi=raw[2], remember)
        mov [rdi-48],rdx            -- new slack (rdi=raw[2], remember)
        ror rax,2                   -- new ref part 2
        mov rdx,[rsp+24]            -- target addr (p1) [@[esp+8] on entry to PpndSeqClone]
--DEV fix this in pilasm.e
        pop qword[rdi-16]           --[2] type/delete_rtn
--pop rdx
--mov [rdi-16],rdx
        pop qword[rdi-40]           --[1] copy maxlen
        mov qword[rdi-24],1         -- refcount
        mov [rdx],rax
        mov rdx,rsi                 -- (save for pFreePool)
        pop qword[rdi-8]            -- new[1]
        rep movsq
        mov rcx,[rdx-32]        -- maxlen (allocated size in bytes)
        lea rax,[rdx-40]
--      sub rax,[rdx-40]        -- account for any slack    [known to be 0]
        add rsp,32
        mov rdx,[rsp]
        jmp :%pFreePool         -- free rcx bytes at rax (ignoring any delete_routine)
    []

  ::ApndNewSeqClone
    [32]
        mov ecx,edx
        mov edx,[esp+20]        -- era
        call :%pAlloClone       -- damages eax only
        mov edx,ecx
    [64]
        mov rcx,rdx
        mov rdx,[rsp+40]        -- era
        call :%pAlloClone       -- damages eax only
        mov rdx,rcx
    []
  ::ApndNewSeq
--------------
    [32]
        -- recap:
        --  ref(p3) in ecx and  @ [esp] (already incref;d) ([1] from ApndSeq)
        --  addr p1 (result)    @ [esp+4]
        --  addr p2 in edi and  @ [esp+8]
        --  addr p3             @ [esp+12]
        --  prepend flag        @ [esp+16]
        --  era                 @ [esp+20]
        --  esi = raw(p2)
        --  edx = length(p2) (==[esi-12])
        --  al is 0x80      (==[esi-1])
        --
        mov edi,[esp+16]        -- prepend flag ...
        mov ecx,edx             -- original length (for loop)
        shl edi,2               --              ...->dword
        add ecx,1               -- increase length by 1
        mov edx,[esp+20]        -- era
--X     call :%pAlloClone       -- (done above)
        call :%pAllocSeq        -- damages eax only
        push eax                --[2] save new ref
        lea edi,[edi+eax*4]     -- edi -> new[1] for append, or new[2] for prepend
      ::ApndNewSeqLoop
        sub ecx,1
        jz :ANSNewSeqLoopEnd
            lodsd                       -- mov eax,[esi], esi+=4
            stosd                       -- mov [edi],eax; edi+=4
--          mov eax,[esi]
--          add esi,4
--          mov [edi],eax
--          add edi,4
            cmp eax,h4
            jl :ApndNewSeqLoop
            add dword[ebx+eax*4-8],1
            jmp ApndNewSeqLoop
      ::ANSNewSeqLoopEnd
        mov esi,[esp+8]         -- target address
        pop eax                 --[2] restore new ref
        pop ecx                 --[1] p3 (from ::ApndSeq)
        mov edx,[esi]           -- prev a
        cmp dword[esp+12],0     -- prepend flag
        je :notprepend2
            mov [ebx+eax*4],ecx -- new[1]:=c
            jmp @f
      ::notprepend2
            mov [edi],ecx       -- new[$]:=c
      @@:
        add esp,16
        mov [esi],eax
        cmp edx,h4
        jle @f
            sub dword[ebx+edx*4-8],1
            jz :%pDealloc
      @@:
        ret
    [64]
        -- recap:
        --  ref(p3) in rcx and  @ [rsp] (already incref;d) ([1] from ApndSeq)
        --  addr p1 (result)    @ [rsp+8]
        --  addr p2 in rdi and  @ [rsp+16]
        --  addr p3             @ [rsp+24]
        --  prepend flag        @ [rsp+32]
        --  era                 @ [rsp+40]
        --  rsi = raw(p2)
        --  rdx = length(p2) (==[rsi-24])
        --  al is 0x80      (==[rsi-1])
        --
        mov rdi,[rsp+32]        -- prepend flag ...
        mov rcx,rdx             -- original length (for loop)
--      shl rdi,2               --              ...->dword
        shl rdi,3               --              ...->qword
        add rcx,1               -- increase length by 1
        mov rdx,[rsp+40]        -- era
--X     call :%pAlloClone       -- (done above)
        call :%pAllocSeq        -- damages eax only
        push rax                --[2] save new ref
        lea rdi,[rdi+rax*4]     -- rdi -> new[1] for append, or new[2] for prepend
      ::ApndNewSeqLoop
        sub rcx,1
        jz :ANSNewSeqLoopEnd
            lodsq                       -- mov rax,[rsi], rsi+=8
            stosq                       -- mov [rdi],rax; rdi+=8
--          mov rax,[rsi]
--          add rsi,8
--          mov [rdi],rax
--          add rdi,8
            cmp rax,r15
            jl :ApndNewSeqLoop
            add qword[rbx+rax*4-16],1
            jmp ApndNewSeqLoop
      ::ANSNewSeqLoopEnd
        mov rsi,[rsp+16]        -- target address
        pop rax                 --[2] restore new ref
        pop rcx                 --[1] p3 (from ::ApndSeq)
        mov rdx,[rsi]           -- prev p1
        cmp qword[rsp+24],0     -- prepend flag
        je :notprepend2
            mov [rbx+rax*4],rcx -- new[1]:=p3
            jmp @f
      ::notprepend2
            mov [rdi],rcx       -- new[$]:=p3
      @@:
        add rsp,32
        mov [rsi],rax
        cmp rdx,r15
        jle @f
            sub qword[rbx+rdx*4-16],1
            jz :%pDealloc
      @@:
        ret
    []


    [32]
  ::ppMap
        -- Maps p1=p2&p3 to p1=prepend(p3,p2) when p2 atom [and p3 is a sequence/string].
        -- When invoked from from opConcat (p1=p2&p3):
        --  addr p1 @ [esp]
        --  addr p2 @ [esp+4]
        --  addr p3 @ [esp+8]
        --  flag(0) @ [esp+12]
        --  esi is [p2] (an atom)
        --  ecx is [p3] (a sequence/string)
        -- Whereas :%opPpndSA (in opApnd) needs:
        --  addr p2 @ [esp+4] to be (opConcat's) addr p3,
        --  addr p3 @ [esp+8] to be (opConcat's) addr p2,
        -- then (switching to opApnd's p2/p3 order)
        --  edi to be addr p2 (as by now at [esp+4])
        --  esi to be [p2] (a sequence/string)
        --  ecx to be [p3] (an atom)
        -- So we just need to swap a few things around:
--DEV re-check that esi==[edi] when this is all done, for e93vhnbaav:
        mov edi,[esp+4]
        mov eax,[esp+8]
        mov [esp+4],eax
        mov [esp+8],edi
        xchg esi,ecx
        mov dword[esp+12],1 -- set prepend flag
        jmp :%opPpndSA  -- (Note that opApndA is invoked directly, when p3 is an atom.)
    [64]
  ::ppMap64
        -- Maps p1=p2&p3 to p1=prepend(p3,p2) when p2 atom [and p3 is a sequence/string].
        -- When invoked from from opConcat (p1=p2&p3):
        --  addr p1 @ [rsp]
        --  addr p2 @ [rsp+8]
        --  addr p3 @ [rsp+16]
        --  flag(0) @ [rsp+24]
        --  rsi is [p2] (an atom)
        --  rcx is [p3] (a sequence/string)
        -- Whereas :%opPpndSA (in opApnd) needs:
        --  addr p2 @ [rsp+8] to be (opConcat's) addr p3,
        --  addr p3 @ [rsp+16] to be (opConcat's) addr p2,
        -- then (switching to opApnd's p2/p3 order)
        --  rdi to be addr p2 (as by now at [rsp+8])
        --  rsi to be [p2] (a sequence/string)
        --  rcx to be [p3] (an atom)
        -- So we just need to swap a few things around:
--DEV re-check that rsi==[rdi] when this is all donw, for e93vhnbaav:
        mov rdi,[rsp+8]
        mov rax,[rsp+16]
        mov [rsp+8],rax
        mov [rsp+16],rdi
        xchg rsi,rcx
        mov qword[rsp+24],1 -- set prepend flag
        jmp :%opPpndSA  -- (Note that opApndA is invoked directly, when p3 is an atom.)
    []

--/*
procedure :%opConcat(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opConcat
--------------
        --
        --  p1 = p2&p3
        --
        --  Map some cases to opApnd (which, obviously, saves duplicating 
        --  about 800 lines of code). Note that pmain.e often performs a 
        --  very similar mapping directly (since every clock counts!).
        --
        --  if atom(p3) -> p1 = append(p2,p3)
        --  elsif atom(p2) -> p1 = prepend(p3,p2) (==:%opApnd with swapped args & a flag of 1)
        --  elsif p1==p2 and refcount(p1)==1 and spare(p1)>=length(p3) and (not string(p2) or string(p3)) -> in situ
        --  elsif string(p2) and string(p3) -> new str
        --  else -> new seq
        --
    [32]
        -- calling convention (for p1:=p2&p3):
        --  lea edx,[p1]    
        --  lea edi,[p2]    (opUnassigned)
        --  lea ecx,[p3]    (opUnassigned)
        --  call :%opConcat             -- [edx]=[edi]&[ecx]
        push ebx    -- prepend flag (to match :%opApnd, if we map to it)
        push ecx    -- p3@[esp+8]
        push edi    -- p2@[esp+4]
        push edx    -- p1@[esp]

        mov esi,[edi]           -- ref of p2
        mov ecx,[ecx]           -- ref of p3

        -- is p3 an atom?
        cmp ecx,h4
        jl :opApndA
        test byte[ebx+ecx*4-1],#80
        jz :opApndA

        -- is p2 an atom?
        cmp esi,h4
        jl :ppMap
        test byte[ebx+esi*4-1],#80
        jz :ppMap

        -- so, p2 and p3 are both string/sequence:
        
        shl esi,2           -- convert p2 to raw addr
        shl ecx,2           -- convert p3 to raw addr

        mov eax,[esi-12]            -- length p2
        mov edi,[ecx-12]            -- length p3
        cmp eax,0
        jnz @f
            -- ({} or "") & p3 is just p3
            -- if p1==p3 then just return
            cmp edx,[esp+8]
            je :CCret
--p2js 25/4/21: (causes too many non-1 refcounts)
            cmp byte[ecx-1],0x80
            je @f
--</p2js>
            lea eax,[ecx+1]
            add dword[ecx-8],1      -- increment refcount of p3.
            ror eax,2
--DEV try (or just reload the ref!) [NO: that's an AGI stall, fair bit more than 3 clocks!]
--          mov edx,[ecx-8]
--          lea eax,[ecx+1]
--          inc edx
--          ror eax,2
--          mov [ecx-8],edx         -- refcount += 1
            jmp :CCstore
      @@:
        cmp edi,0
        jne @f
            -- p2 & ("" or {}) is just p2
            -- if p1==p2 then just return
            cmp edx,[esp+4]
            je :CCret
--DEV as above
            lea eax,[esi+1]
            add dword[esi-8],1      -- increment refcount of p2.
            ror eax,2
            jmp :CCstore
      @@:

        add edi,eax
--DEV I think this can go:
        mov eax,[edx]           -- target ref (for in situ checks)
        cmp byte[esi-1],0x82    -- p2 string?
        jne :CCtwoSeq
        cmp byte[ecx-1],0x82    -- p3 string?
        jne :CCstrSeq
        --
        -- is the target==p2 (a string), with a refcount of 1, and does it have space?
        -- (note: ensuring tgt == p2 also ensures that even if it is p3 we will not 
        --       damage it before we get round to copying it [I got clobbered on that first try])   --???
        --
        cmp edx,[esp+4]
        jne @f
--          cmp byte[ebx+eax*4-1],0x82  -- (next test covers this)
--          jne @f
--          cmp eax,h4
--          jne @f
--          cmp dword[ebx+eax*4-8],1
            cmp dword[esi-8],1
            jne @f
            -- for a string, [ref*4-16] is maxlen+sHdr+1
--          mov ecx,[ebx+eax*4-16]      -- maxlen
            mov eax,[esi-16]            -- maxlen
            sub eax,16+1
            cmp eax,edi
            jge :CCstringInSitu
      @@:
        mov edx,ecx                 -- save raw(p3)
        mov ecx,edi
        call :%pAllocStr            -- damages eax only
        push eax
        mov ecx,[esi-12]            -- length(p2)
        lea edi,[ebx+eax*4]
        rep movsb
        mov ecx,[edx-12]            -- length(p3)
        mov esi,edx
        add ecx,1                   -- include trailing null
        rep movsb
        pop eax

  ::CCstore
-----------
        mov edi,[esp]           -- target addr
        add esp,16
--DEV try some nop here
        mov edx,[edi]           -- prev ref
        mov [edi],eax           -- store new ref
        cmp edx,h4
        jle @f
            sub dword[ebx+edx*4-8],1
            jz :%pDealloc
      @@:
        ret

  ::CCstringInSitu
------------------
        -- we have proved that tgt=p2, string, refcount is 1, and there is enough space.
        -- we just need to copy p3 at end of target and update the length.
        -- recap:
        --  addr p1,p2,p3,flag @ [esp],[esp+4],[esp+8],[esp+12]
        --  edi is the new length
        --  esi is raw(p2)
        --  ecx is raw(p3)
        mov eax,[esi-12]            -- length(p2)
        mov edx,ecx
        mov ecx,[ecx-12]            -- length(p3)
        mov [esi-12],edi            -- set new length
--      add esi,eax
        mov edi,esi
        mov esi,edx
        add ecx,1
        add edi,eax
        rep movsb                   -- copy p3 content and terminating null
      ::CCret
        add esp,16
        ret
--/*
<DEV>god knows how I got to the above starting from this...:
CCstringInSitu:
    ; we have proved that tgt=p2, hard left, refcount is 1, and there is enough space.
    ; we just need to copy p3 at end of target.
    mov [ebx+eax*4-12],edx      ; set new length
    sub edx,ecx                 ; length original p3
if newBase
    mov esi,edi
end if
    lea edi,[eax*4+ecx]         ; dest is base p2(=tgt)+length p2
    lea ecx,[edx+1]
    rep movsb                   ; copy p3 content and terminating null
    ret
--*/

  ::CCtwoSeq
------------
--X     -- p2 is a dword-sequence, and p3 is a seq/str. Neither are length 0
        -- p2 is a dword-sequence, and p3 is a seq/str. NB p2 can be length 0
        -- recap:
        --  addr p1,p2,p3,flag @ [esp],[esp+4],[esp+8],[esp+12]
        --  edi is the new length
        --  esi is raw(p2)
        --  ecx is raw(p3)
        --  edx is addr p1 (==[esp])
        --  eax is [p1] (if that is any help)
        cmp edx,[esp+4]         -- p1==p2?
        jne :CCnewSeq
        cmp dword[esi-8],1          -- refcount 1?
        jne :CCnewSeqClone
--      jne :CCnewSeq
        mov edx,[esi-16]        -- maxlen
        sub edx,[esi-20]        -- slack (we make no attempt to use that here)
        sub edx,20
        shr edx,2               -- (bytes->dwords)
        cmp edi,edx
-- advice taken, 30/1/15:
--jg?
--      jl :CCnewSeq
--DEV not call :%pAlloClone...: (we are also doing a bunch of pointless increfs)
        jg :CCnewSeq
        mov edx,[esi-12]        -- length(p2)
        cmp byte[ecx-1],#82
--17/6/15:
        push dword[ecx-12]
        mov [esi-12],edi        -- replace length now
        lea edi,[esi+edx*4]
        mov esi,ecx
--NO!; x &= x will have just clobbered this!
-->!        mov ecx,[ecx-12]        -- length(p3)
        pop ecx
        jne :CCSeqSeqInSitu
        -- expand the string
        xor eax,eax
      @@:
        lodsb
        stosd
        sub ecx,1
        jnz @b
        add esp,16
        ret

  ::CCSeqSeqInSitu
------------------
        lodsd
        stosd
        cmp eax,h4
        jl @f
          add dword[ebx+eax*4-8],1
      @@:
        sub ecx,1
        jnz :CCSeqSeqInSitu
        add esp,16
        ret

  ::CCnewSeqClone
        mov edx,[esp+16]            -- era
        call :%pAlloClone
  ::CCnewSeq
------------
--X     -- p2 is a dword-sequence, and p3 is a seq/str. Neither are length 0
        -- p2 is a dword-sequence, and p3 is a seq/str. NB p2 can be length 0
        -- recap:
        --  addr p1,p2,p3,flag,era @ [esp],[esp+4],[esp+8],[esp+12],[esp+16]
        --  edi is the new length
        --  esi is raw(p2)
        --  ecx is raw(p3)
        --  edx is unknown
--      --  eax is [p1] (if that is any help) [may try removing that anyway]
--      mov edx,ecx
        mov edx,[esp+16]            -- era
        push ecx
        mov ecx,edi
--X     call :%pAlloClone       -- (done above)
        call :%pAllocSeq        -- damages eax only
        pop edx
        lea edi,[ebx+eax*4]
        push eax
        mov ecx,[esi-12]
        cmp ecx,0
        je :CCnsp2empty
      ::CCnsClonep2Loop 
        lodsd
        stosd
        cmp eax,h4
        jl @f
          add dword[ebx+eax*4-8],1
      @@:
        sub ecx,1
        jnz :CCnsClonep2Loop
      ::CCnsp2empty
        mov ecx,[edx-12]            -- length p3
        mov esi,edx
        cmp byte[edx-1],#82
        jne :CCnsClonep3Loop
        xor eax,eax
      ::CCnsExpandp3Loop 
        lodsb
        stosd
        sub ecx,1
        jnz :CCnsExpandp3Loop
        pop eax
        jmp :CCstore

      ::CCnsClonep3Loop     -- (also used by CCstrSeq)
        lodsd
        stosd
        cmp eax,h4
        jl @f
          add dword[ebx+eax*4-8],1
      @@:
        sub ecx,1
        jnz :CCnsClonep3Loop
        pop eax
        jmp :CCstore
    
  ::CCstrSeq
------------
--X     -- p2 is a string, p3 is a dword-sequence. Neither are length 0
        -- p2 is a string, p3 is a dword-sequence. NB p2 can be length 0
        -- recap:
        --  addr p1,p2,p3,flag,era @ [esp],[esp+4],[esp+8],[esp+12],[esp+16]
        --  edi is the new length
        --  esi is raw(p2)
        --  ecx is raw(p3)
        --  edx is addr p1 (==[esp])
        --  eax is [p1] (if that is any help)
--      mov edx,ecx
        mov edx,[esp+16]            -- era
        push ecx
        mov ecx,edi
        call :%pAllocSeq
        pop edx
        lea edi,[ebx+eax*4]
        push eax
        mov ecx,[esi-12]
        xor eax,eax
        cmp ecx,0
        je :CCnss2empty
      ::CCssExpandp2Loop 
        lodsb
        stosd
        sub ecx,1
        jnz :CCssExpandp2Loop
      ::CCnss2empty
        mov ecx,[edx-12]            -- length p3
        mov esi,edx
        jmp :CCnsClonep3Loop
    
    [64]
        -- calling convention (for p1:=p2&p3):
        --  lea rdx,[p1]    
        --  lea rdi,[p2]    (opUnassigned)
        --  lea rcx,[p3]    (opUnassigned)
        --  call :%opConcat             -- [rdx]=[rdi]&[rcx]
        push rbx    -- prepend flag (to match :%opApnd, if we map to it)
        push rcx    -- p3@[rsp+16]
        push rdi    -- p2@[rsp+8]
        push rdx    -- p1@[rsp]

        mov rsi,[rdi]           -- ref of p2
        mov rcx,[rcx]           -- ref of p3
        mov r15,h4

        -- is p3 an atom?
        cmp rcx,r15
        jl :opApndA
        test byte[rbx+rcx*4-1],#80
        jz :opApndA

        -- is p2 an atom?
        cmp rsi,r15
        jl :ppMap64
        test byte[rbx+rsi*4-1],#80
        jz :ppMap64

        -- so, p2 and p3 are both string/sequence:
        
        shl rsi,2           -- convert p2 to raw addr
        shl rcx,2           -- convert p3 to raw addr

        mov rax,[rsi-24]            -- length p2
        mov rdi,[rcx-24]            -- length p3
        cmp rax,0
        jnz @f
            -- ({} or "") & p3 is just p3
            -- if p1==p3 then just return
            cmp rdx,[rsp+16]        -- DEV might refs (rather that addrs) be better?
            je :CCret64
--p2js 25/4/21: (causes too many non-1 refcounts)
            cmp byte[rcx-1],#80
            je @f
--</p2js>
            lea rax,[rcx+1]
            add qword[rcx-16],1     -- increment refcount of p3.
            ror rax,2
            jmp :CCstore64
      @@:
        cmp rdi,0
        jne @f
            -- p2 & ("" or {}) is just p2
            -- if p1==p2 then just return
            cmp rdx,[rsp+8]         -- ditto
            je :CCret64
            lea rax,[rsi+1]
            add qword[rsi-16],1     -- increment refcount of p2.
            ror rax,2
            jmp :CCstore64
      @@:

        add rdi,rax
--DEV I think this can go:
--28/08/15 (put back in, just to match 32 bit version...)
        mov rax,[rdx]           -- target ref (for in situ checks)
        cmp byte[rsi-1],0x82    -- p2 string?
        jne :CCtwoSeq64
        cmp byte[rcx-1],0x82    -- p3 string?
        jne :CCstrSeq64
        --
        -- is the target==p2 (a string), with a refcount of 1, and does it have space?
        -- (note: ensuring tgt == p2 also ensures that even if it is p3 we will not 
        --       damage it before we get round to copying it [I got clobbered on that first try])   --???
        --
        cmp rdx,[rsp+8]
        jne @f
            cmp qword[rsi-16],1
            jne @f
            mov rax,[rsi-32]            -- maxlen
            sub rax,32+1
            cmp rax,rdi
            jge :CCstringInSitu64
      @@:
        mov rdx,rcx                 -- save raw(p3)
        mov rcx,rdi
        call :%pAllocStr            -- damages eax only
        push rax
        mov rcx,[rsi-24]            -- length(p2)
        lea rdi,[rbx+rax*4]
        rep movsb
        mov rcx,[rdx-24]            -- length(p3)
        mov rsi,rdx
        add rcx,1                   -- include trailing null
        rep movsb
        pop rax

  ::CCstore64
-------------
        mov rdi,[rsp]           -- target addr
        add rsp,32
--DEV try some nop here
        mov rdx,[rdi]           -- prev ref
        mov [rdi],rax           -- store new ref
        cmp rdx,r15
        jle @f
            sub qword[rbx+rdx*4-16],1
            jz :%pDealloc
      @@:
        ret

  ::CCstringInSitu64
--------------------
        -- we have proved that tgt=p2, string, refcount is 1, and there is enough space.
        -- we just need to copy p3 at end of target and update the length.
        -- recap:
        --  addr p1,p2,p3,flag @ [rsp],[rsp+8],[rsp+16],[rsp+24]
        --  rdi is the new length
        --  rsi is raw(p2)
        --  rcx is raw(p3)
        mov rax,[rsi-24]            -- length(p2)
        mov rdx,rcx
        mov rcx,[rcx-24]            -- length(p3)
        mov [rsi-24],rdi            -- set new length
        mov rdi,rsi
        mov rsi,rdx
        add rcx,1
        add rdi,rax
        rep movsb                   -- copy p3 content and terminating null
      ::CCret64
        add rsp,32
        ret

  ::CCtwoSeq64
--------------
--X     -- p2 is a dword-sequence, and p3 is a seq/str. Neither are length 0
        -- p2 is a dword-sequence, and p3 is a seq/str. NB p2 can be length 0
        -- recap:
        --  addr p1,p2,p3,flag @ [rsp],[rsp+8],[rsp+16],[rsp+24]
        --  rdi is the new length
        --  rsi is raw(p2)
        --  rcx is raw(p3)
        --  rdx is addr p1 (==[rsp])
--      --  rax is [p1] (if that is any help) [removed]
        cmp rdx,[rsp+8]         -- p1==p2?
        jne :CCnewSeq64
        cmp qword[rsi-16],1         -- refcount 1?
        jne :CCnewSeq64Clone
--      jne :CCnewSeq64
        mov rdx,[rsi-32]        -- maxlen
        sub rdx,[rsi-40]        -- slack (we make no attempt to use that here)
        sub rdx,40
        shr rdx,3               -- (bytes->qwords)
        cmp rdi,rdx
--advice belatedly taken (to match 32-bit) 28/8/15..
--jg?
--      jl :CCnewSeq64
--dev not :%pAlloClone      -- (we are also doing a bunch of pointless incref)
        jg :CCnewSeq64
        mov rdx,[rsi-24]        -- length(p2)
        cmp byte[rcx-1],#82
--17/6/15:
        push qword[rcx-24]
        mov [rsi-24],rdi        -- replace length now
--28/12/15:
--      lea rdi,[rsi+rdx*4]
        lea rdi,[rsi+rdx*8]
        mov rsi,rcx
--NO!; x &= x will have just clobbered this!
-->     mov rcx,[rcx-24]        -- length(p3)
        pop rcx
        jne :CCSeqSeqInSitu64
        -- expand the string
        xor rax,rax
      @@:
        lodsb
        stosq
        sub rcx,1
        jnz @b
        add rsp,32
        ret

  ::CCSeqSeqInSitu64
--------------------
        lodsq
        stosq
        cmp rax,r15
        jl @f
          add qword[rbx+rax*4-16],1
      @@:
        sub rcx,1
        jnz :CCSeqSeqInSitu64
        add rsp,32
        ret

  ::CCnewSeq64Clone
        mov rdx,[rsp+32]            -- era
        call :%pAlloClone
  ::CCnewSeq64
--------------
--X     -- p2 is a dword-sequence, and p3 is a seq/str. Neither are length 0
        -- p2 is a dword-sequence, and p3 is a seq/str. NB p2 can be length 0
        -- recap:
        --  addr p1,p2,p3,flag,era @ [rsp],[rsp+8],[rsp+16],[rsp+24],[rsp+32]
        --  rdi is the new length
        --  rsi is raw(p2)
        --  rcx is raw(p3)
        --  rdx is unknown
--      --  rax is [p1] (if that is any help) [may try removing that anyway][have]
--      mov rdx,rcx
        mov rdx,[rsp+32]            -- era
        push rcx
        mov rcx,rdi
--X     call :%pAlloClone           -- (done above)
        call :%pAllocSeq            -- damages eax only
        pop rdx
        lea rdi,[rbx+rax*4]
        push rax
        mov rcx,[rsi-24]
        cmp rcx,0
        je :CCnsp2empty64
      ::CCnsClonep2Loop64
        lodsq
        stosq
        cmp rax,r15
        jl @f
          add qword[rbx+rax*4-16],1
      @@:
        sub rcx,1
        jnz :CCnsClonep2Loop64
      ::CCnsp2empty64
--      mov rcx,[rdx-12]            -- length p3
        mov rcx,[rdx-24]            -- length p3
        mov rsi,rdx
        cmp byte[rdx-1],#82
        jne :CCnsClonep3Loop64
        xor rax,rax
      ::CCnsExpandp3Loop64
        lodsb
        stosq
        sub rcx,1
        jnz :CCnsExpandp3Loop64
        pop rax
        jmp :CCstore64

      ::CCnsClonep3Loop64   -- (also used by CCstrSeq)
        lodsq
        stosq
        cmp rax,r15
        jl @f
          add qword[rbx+rax*4-16],1
      @@:
        sub rcx,1
        jnz :CCnsClonep3Loop64
        pop rax
        jmp :CCstore64
    
  ::CCstrSeq64
--------------
--X     -- p2 is a string, p3 is a dword-sequence. Neither are length 0
        -- p2 is a string, p3 is a dword-sequence. NB p2 can be length 0
        -- recap:
        --  addr p1,p2,p3,flag,era @ [rsp],[rsp+8],[rsp+16],[rsp+24],[rsp+32]
        --  rdi is the new length
        --  rsi is raw(p2)
        --  rcx is raw(p3)
        --  rdx is addr p1 (==[rsp])
        --  rax is [p1] (if that is any help)
--      mov rdx,rcx
        mov rdx,[rsp+32]            -- era
        push rcx
        mov rcx,rdi
        call :%pAllocSeq
        pop rdx
        lea rdi,[rbx+rax*4]
        push rax
        mov rcx,[rsi-24]
        xor rax,rax
        cmp rcx,0
        je :CCnss2empty
      ::CCssExpandp2Loop64
        lodsb
        stosq
        sub rcx,1
        jnz :CCssExpandp2Loop64
      ::CCnss2empty
        mov rcx,[rdx-24]            -- length p3
        mov rsi,rdx
        jmp :CCnsClonep3Loop64
    []


--/*
procedure :%opCatsi(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opCatsi
-------------
        --
        --  Optimised form of opConcat for p1&=p2 (===p1:=p1&p2), when p2 
        --  is integer or sequence of integer (as proven in the gvar_scan) 
        --  and p1 is a dword-sequence, and p1!=p2 and both p1,p2 are init.
        --  (So obviously it uses rep movsd instead of a test/incref loop.)
        --  Maps to opConcat when refcount!=1, insufficient space, etc.
        --  Calling this with floats/strings/nested sequences in p2 will
        --  lead to disaster. Originally, p1 had to be sequence of integer,
        --  but implementation yielded that it can be any dword-sequence,
        --  including one containing floats/strings/subsequences, because
        --  any cloning etc is palmed off to opConcat anyway. It may in 
        --  fact be valid to call this when p1==p2, but that has never 
        --  been tested and I strongly doubt it would ever gain anything.
        --
    [32]
        --calling convention:
        --  mov edi,[p1]    -- (ref)
        --  lea edx,[p1]    -- (address)    -- (must be init sequence of integer)
        --  lea esi,[p2]    --   ""         -- (must be init [sequence of] integer)
        --  call :%opCatsi
        --
        cmp dword[ebx+edi*4-8],1    -- refcount of tgt (!=1)
        jne :UseopConcat
        push edx                    --[1] save addr p1
        push esi                    --[2] save addr p2
        mov eax,[esi]               -- src(p2) ref
        -- how many new dwords do we need?
        mov ecx,1                   -- default for p1 &= <int> case
        cmp eax,h4
        jl @f
            mov ecx,[ebx+eax*4-12]  -- length(p2)
            lea esi,[ebx+eax*4]
      @@:
        mov eax,[ebx+edi*4-12]      -- original length p1
        push esi                    --[3] save
--      push ecx                    --[4] save no of items to add
        -- check there is enough space.
        mov edx,[ebx+edi*4-20]      -- slack
        mov esi,[ebx+edi*4-16]      --      maxlen
        add edi,eax                 -- ref+idx(in dwords)
        add edx,20                  -- plus header
        shl edi,2                   -- store new here [ref+idx->raw==lea edi,[edi*4+eax*4]]
        sub esi,edx                 --      less header+slack
--      lea edi,[edi+eax*4]         -- store new here (AGI stall)
        add eax,ecx                 -- new length
        shr esi,2                   --      bytes->dwords
        cmp eax,esi                 -- check maxlen
        jg :UseopConcatPop

        mov [ebx+edx*4-12],eax  -- set new length
--      pop ecx                 --[4] no of items to add
        pop esi                 --[3] restore
        add esp,8               --[2,1] discard
        rep movsd
        ret

      ::UseopConcatPop
--      pop ecx                 --[4] discard
        pop esi                 --[3] discard
        pop esi                 --[2] restore addr p2
        pop edx                 --[1] restore addr p1
      ::UseopConcat
        mov edi,edx
        mov ecx,esi
        jmp :%opConcat
    [64]
        --calling convention:
        --  mov rdi,[p1]    -- (ref)
        --  lea rdx,[p1]    -- (address)    -- (must be init sequence of integer)
        --  lea rsi,[p2]    --   ""         -- (must be init [sequence of] integer)
        --  call :%opCatsi
        --
        mov r15,h4
        cmp dword[rbx+rdi*4-16],1   -- refcount of tgt (!=1)
        jne :UseopConcat64
        push rdx                    --[1] save addr p1
        push rsi                    --[2] save addr p2
        mov rax,[rsi]               -- src(p2) ref
        -- how many new dwords do we need?
        mov rcx,1                   -- default for p1 &= <int> case
        cmp rax,r15
        jl @f
            mov rcx,[rbx+rax*4-24]  -- length(p2)
            lea rsi,[rbx+rax*4]
      @@:
        mov rax,[rbx+rdi*4-24]      -- original length p1
        push rsi                    --[3] save
--      push rcx                    --[4] save no of items to add
        -- check there is enough space.
        mov rdx,[rbx+rdi*4-40]      -- slack
        mov rsi,[rbx+rdi*4-32]      --      maxlen
        lea rdi,[rdi+rax*2]         -- ref+idx*2
        add rdx,40                  -- plus header
        shl rdi,2                   -- store new here [ref+idx*2->raw==lea rdi,[rdi*4+rax*8]]
        sub rsi,rdx                 --      less header+slack
        add rax,rcx                 -- new length
        shr rsi,3                   --      bytes->qwords
        cmp rax,rsi                 -- check maxlen
        jg :UseopConcatPop64

        mov [rbx+rdx*4-24],rax  -- set new length
--      pop rcx                 --[4] no of items to add
        pop rsi                 --[3] restore
        add rsp,16              --[2,1] discard
        rep movsq
        ret

      ::UseopConcatPop64
--      pop rcx                 --[4] discard
        pop rsi                 --[3] discard
        pop rsi                 --[2] restore addr p2
        pop rdx                 --[1] restore addr p1
      ::UseopConcat64
        mov rdi,rdx
        mov rcx,rsi
        jmp :%opConcat
    []

--/*
procedure :%opConcatN(:%)
end procedure -- (for Edita/CtrlQ)
--*/
  :%opConcatN
-------------
        --calling convention
        -- mov ecx,N            -- (literal, >=3)
        -- push <return address>
        -- push [pn]..[p1]      -- (opUnassigned) [DEV]
        -- mov eax,tgt          -- addr tgt
        -- jmp :%opConcatN      -- [tgt] = p3&p4&..pn
        --<return address>
        --
        --  logic:
        --      if (sum lengths while string result) then
        --          create string
        --      else (continue summing lengths of dword_sequence)
        --          create dword_sequence
        --  i.e. there are two near-identical "sum" loops with the first
        --       jumping into the middle of the second in several places,
        --       followed by two near-identical "create" loops. Enjoy!
        --
        --  special care must be taken to preserve four things:
        --      edi = [tgt] or 0 if not insitu-able (first loop only),
        --      N (ecx, which as above counts down twice),
        --      addr tgt (edx, which can be 0 if we went insitu), 
        --      and the newly allocated ref (esi, if any)
        --  (obviously we can't just throw them on the stack once and 
        --   then forget about them as we're peeling things off that.)
        --
        -- Lemma:
        --  Technically, a=a&...&a&... /is/ possible to do "in situ",
        --  however the first thing an insitu does is clobber length(p1),
        --  and I doubt it would improve performance very often anyway,
        --  especially as big things are very rarely >50% spare space.
        --
    [32]
        xor edx,edx                     -- output length
        mov esi,esp                     -- scan thru p1..pn on the stack
        push eax                        --[1] save addr tgt
        push ecx                        --[2] save N
        mov edi,[eax]   -- (may validly be h4, or 0 at the get-go, btw)
        --
        -- sum lengths while string result, but check for x = x&... case first
        --
        lodsd                           -- mov eax,[esi]; esi+=4, aka load p1
        cmp eax,edi
        je :opConcatNStrSumMid          -- (nb opposite of subsequent checks)
        xor edi,edi                     -- signals first not lhs... (no in situ)
        jmp :opConcatNStrSumMid

      ::opConcatNStringSumLoop
        lodsd                           -- next ref (mov eax,[esi]; esi+=4)
        cmp eax,edi
        jne :opConcatNStrSumMid
        xor edi,edi                     -- ... signals >first (also) lhs  (no in situ)
      ::opConcatNStrSumMid
        cmp eax,h4
        jl @f
            cmp byte[ebx+eax*4-1],0x82
            jne :opConcatNSeqCont       -- (continue in dword-sequence loop)
            add edx,[ebx+eax*4-12]      -- sum lengths
            jmp :opConcatNContinueStringSumLoop
      @@:
        cmp eax,#FF
        ja :opConcatNSeqAdd1            -- (continue in dword-sequence loop)
        add edx,1
      ::opConcatNContinueStringSumLoop
        sub ecx,1
        jnz opConcatNStringSumLoop

        --
        -- if we got here everything (on rhs) was string or char
        --
        -- can we do things in situ?
        --  result must be = first and != any later, as signalled by edi!=0
        --  result must already be string with refcount 1 and enough space
        --
--      test edi,edi                -- ref result if first & not later, else 0
--      jz opConcatNnewStr          -- result not first/not single use
        cmp edi,h4                  -- check for char=char&... case (covers "")
        jle opConcatNnewStr         -- (or 0 or unassigned)
--      cmp byte[ebx+edi*4-1],#82   -- type byte (must be, as tgt==p1!)
--      jne opConcatNnewStr
        cmp dword[ebx+edi*4-8],1    -- refcount
        jne opConcatNnewStr
        mov ecx,[ebx+edi*4-16]      -- maxlen
        mov eax,[ebx+edi*4-12]      -- length   
        sub ecx,16
        cmp ecx,edx
        jle :opConcatNnewStr        -- insufficient space
        mov [ebx+edi*4-12],edx      -- set new length
        pop ecx                     --[2] restore N
        mov edx,ebx                 -- target address:=0 (insitu occured)
        add esp,4                   --[1] discard tgt addr
        lea edi,[eax+edi*4]         -- stick new content here
        pop eax                     -- discard first
        sub ecx,1                   -- process one less entry
        jmp :opConcatNStrLoop

      ::opConcatNnewStr
        mov ecx,edx
        call :%pAllocStr            -- damages eax only
        pop ecx                     --[2] re-load N
        pop edx                     --[1] target address (nb must remain undamaged for a long time)
        mov esi,eax                 -- save newly allocated ref ("")
        lea edi,[ebx+eax*4]         -- raw addr of newly allocated string
      ::opConcatNStrLoop
        pop eax                     -- next ref
        cmp eax,h4
        jl :opConcatNcopyChar
            push ecx                -- (items left on stack)
            push esi                -- (newly allocated ref)
            lea esi,[ebx+eax*4]
            mov ecx,[ebx+eax*4-12]  -- length
            rep movsb
            pop esi
            pop ecx
            jmp :opConcatNstrContinue
      ::opConcatNcopyChar
        stosb
      ::opConcatNstrContinue
        sub ecx,1
        jnz :opConcatNStrLoop

        xor eax,eax
        stosb
        jmp :opConcatNStore


      ::opConcatNSeqAdd1    -- eax is a short int, (called from above if signed or too big for char), or a float
        add edx,1
        sub ecx,1
        jz :opConcatNSeqSumLoopEnd
        --
        -- carry on summing lengths now we know result is a dword-sequence:
        --
      ::opConcatNSeqSumLoop
        lodsd               -- next ref (still from the stack) (mov eax,[esi], esi+=4)
        cmp eax,edi
        jne @f
            xor edi,edi         -- ... signals >first (also) lhs  (no in situ)
      @@:
        cmp eax,h4
        jl :opConcatNSeqAdd1
      ::opConcatNSeqCont
        test byte[ebx+eax*4-1],0x80
        jz opConcatNSeqAdd1
        add edx,[ebx+eax*4-12]  -- lengths
        sub ecx,1
        jnz :opConcatNSeqSumLoop

      ::opConcatNSeqSumLoopEnd
        --
        -- so result needs to be a dword-sequence of length edx
        --
        -- can we do things in situ?
        --  result must be = first and != any later (as signalled by edi!=0)
        --  result must already be sequence with refcount 1 and enough space
        --
--      test edi,edi                -- ref result if first & not later, else 0
--      jz :opConcatNnewSeq         -- result not first/not single use
        cmp edi,h4                  -- check for int=int&... case (covers "")
        jle :opConcatNnewSeq
        cmp dword[ebx+edi*4-8],1    -- refcount
--      jne :opConcatNnewSeq
        jne :opConcatNnewSeqClone
        cmp byte[ebx+edi*4-1],#80   -- type byte
        jne :opConcatNnewSeq
        mov ecx,[ebx+edi*4-16]      -- maxlen
        mov eax,[ebx+edi*4-12]      -- length   
        sub ecx,20
--8/2/15:
--      mov esi,[ebx+eax*4-20]      -- slack
        mov esi,[ebx+edi*4-20]      -- slack
        shl eax,2                   -- length -> dwords
        sub ecx,esi                 -- (maxlen-hdr-slack)
        shl edi,2                   -- ref -> raw
        shr ecx,2                   -- bytes -> dwords
        cmp ecx,edx
        jl :opConcatNnewSeq         -- insufficient space
        mov [edi-12],edx            -- set new length
        pop ecx                     -- restore N
        mov edx,ebx                 -- target address:=0 (insitu occurred)
        add esp,4                   -- discard addr target
        add edi,eax                 -- stick new content here
        pop eax                     -- discard first
        sub ecx,1                   -- process one less entry
        jmp :opConcatNSeqLoop

      ::opConcatNnewSeqClone
        mov ecx,edx
        mov edx,[esp]
        mov edx,[esp+edx*4+8]
        call :%pAlloClone
        jmp @f

      ::opConcatNnewSeq
        mov ecx,edx
--DEV wronG!! (2/8/15)
--      mov edx,[esp+edx*4]         -- era
        mov edx,[esp]
        mov edx,[esp+edx*4+8]
      @@:
        call :%pAllocSeq            -- damages eax only
        pop ecx                     -- re-load N
        pop edx                     -- target address (nb must remain undamaged for a long time)
        mov esi,eax                 -- save newly allocated ref ("")
        lea edi,[ebx+eax*4]         -- raw addr of newly allocated sequence

      ::opConcatNSeqLoop
        pop eax                     -- next ref
        cmp eax,h4
        jl :opConcatNCopyInt
            test byte[ebx+eax*4-1],0x80
            jnz @f
                -- it is a float then
                add dword[ebx+eax*4-8],1
                jmp :opConcatNCopyInt
          @@:
            push esi                -- newly allocated ref
            push ecx                -- remaining N
            lea esi,[ebx+eax*4]
            mov ecx,[ebx+eax*4-12]  -- length
            test ecx,ecx
            jz :opConcatNZeroLengthString
            cmp byte[esi-1],0x82
            je :opConcatNCopyStr
              ::opConcatNCopySeqLoop
                lodsd               -- mov eax,[esi], esi+=4
                stosd               -- mov [edi],eax; edi+=4
                cmp eax,h4
                jl @f
                    add dword[ebx+eax*4-8],1
              @@:
                sub ecx,1
                jnz :opConcatNCopySeqLoop 
                pop ecx
                pop esi
                jmp :opConcatNSeqContinue
              ::opConcatNCopyStr
                xor eax,eax
              ::opConcatNCharLoop
                lodsb               -- mov al,[esi], esi+=1
                stosd               -- mov [edi],eax; edi+=4
                sub ecx,1
                jnz :opConcatNCharLoop 
          ::opConcatNZeroLengthString
            pop ecx                 -- remaining N
            pop esi                 -- newly allocated ref
            jmp :opConcatNSeqContinue
      ::opConcatNCopyInt
        stosd                       -- mov [edi],eax; edi+=4
      ::opConcatNSeqContinue
        sub ecx,1
        jnz :opConcatNSeqLoop

      ::opConcatNStore
        mov edi,edx
        test edx,edx
        jz @f               -- in situ occurred
            mov edx,[edi]
            mov [edi],esi
            cmp edx,h4
            jle @f
                sub dword[ebx+edx*4-8],1
                jz :%pDealloc
      @@:
        ret

    [64]
        mov r15,h4
        xor rdx,rdx                     -- output length
        mov rsi,rsp                     -- scan thru p1..pn on the stack
        push rax                        --[1] save addr tgt
        push rcx                        --[2] save N
        mov rdi,[rax]   -- (may validly be h4, or 0 at the get-go, btw)
        --
        -- sum lengths while string result, but check for x = x&... case first
        --
        lodsq                           -- mov rax,[rsi]; rsi+=8, aka load p1
        cmp rax,rdi
        je :opConcatNStrSumMid64        -- (nb opposite of subsequent checks)
        xor rdi,rdi                     -- signals first not lhs... (no in situ)
        jmp :opConcatNStrSumMid64

      ::opConcatNStringSumLoop64
        lodsq                           -- next ref (mov rax,[rsi]; rsi+=8)
        cmp rax,rdi
        jne :opConcatNStrSumMid64
        xor rdi,rdi                     -- ... signals >first (also) lhs  (no in situ)
      ::opConcatNStrSumMid64
        cmp rax,r15
        jl @f
            cmp byte[rbx+rax*4-1],0x82
            jne :opConcatNSeqCont64     -- (continue in dword-sequence loop)
            add rdx,[rbx+rax*4-24]      -- sum lengths
            jmp :opConcatNContinueStringSumLoop64
      @@:
        cmp rax,#FF
        ja :opConcatNSeqAdd164          -- (continue in dword-sequence loop)
        add rdx,1
      ::opConcatNContinueStringSumLoop64
        sub rcx,1
        jnz opConcatNStringSumLoop64

        --
        -- if we got here everything (on rhs) was string or char
        --
        -- can we do things in situ?
        --  result must be = first and != any later, as signalled by edi!=0
        --  result must already be string with refcount 1 and enough space
        --
--      test rdi,rdi                -- ref result if first & not later, else 0
--      jz :opConcatNnewStr64       -- result not first/not single use
        cmp rdi,r15                 -- check for char=char&... case (covers "")
        jle :opConcatNnewStr64      -- (or 0 or unassigned)
--      cmp byte[rbx+rdi*4-1],#82   -- type byte (must be, as tgt==p1!)
--      jne :opConcatNnewStr64
        cmp qword[rbx+rdi*4-16],1   -- refcount
        jne :opConcatNnewStr64
        mov rcx,[rbx+rdi*4-32]      -- maxlen
        mov rax,[rbx+rdi*4-24]      -- length   
--24/1/16:
--      sub rcx,16
        sub rcx,32
        cmp rcx,rdx
        jle :opConcatNnewStr64      -- insufficient space
        mov [rbx+rdi*4-24],rdx      -- set new length
        pop rcx                     --[2] restore N
        mov rdx,rbx                 -- target address:=0 (insitu occured)
        add rsp,8                   --[1] discard tgt addr
        lea rdi,[rax+rdi*4]         -- stick new content here
        pop rax                     -- discard first
        sub rcx,1                   -- process one less entry
        jmp :opConcatNStrLoop64

      ::opConcatNnewStr64
        mov rcx,rdx
        call :%pAllocStr            -- damages eax only
        pop rcx                     --[2] re-load N
        pop rdx                     --[1] target address (nb must remain undamaged for a long time)
        mov rsi,rax                 -- save newly allocated ref ("")
        lea rdi,[rbx+rax*4]         -- raw addr of newly allocated string
      ::opConcatNStrLoop64
        pop rax                     -- next ref
        cmp rax,r15
        jl :opConcatNcopyChar64
            push rcx                -- (items left on stack)
            push rsi                -- (newly allocated ref)
            lea rsi,[rbx+rax*4]
            mov rcx,[rbx+rax*4-24]  -- length
            rep movsb
            pop rsi
            pop rcx
            jmp :opConcatNstrContinue64
      ::opConcatNcopyChar64
        stosb
      ::opConcatNstrContinue64
        sub rcx,1
        jnz :opConcatNStrLoop64

        xor rax,rax
        stosb
        jmp :opConcatNStore64


      ::opConcatNSeqAdd164  -- eax is a short int, (called from above if signed or too big for char), or a float
        add rdx,1
        sub rcx,1
        jz :opConcatNSeqSumLoopEnd64
        --
        -- carry on summing lengths now we know result is a dword-sequence:
        --
      ::opConcatNSeqSumLoop64
        lodsq               -- next ref (still from the stack) (mov rax,[rsi], rsi+=8)
        cmp rax,rdi
        jne @f
            xor rdi,rdi         -- ... signals >first (also) lhs  (no in situ)
      @@:
        cmp rax,r15
        jl :opConcatNSeqAdd164
      ::opConcatNSeqCont64
        test byte[rbx+rax*4-1],0x80
        jz :opConcatNSeqAdd164
        add rdx,[rbx+rax*4-24]  -- lengths
        sub rcx,1
        jnz :opConcatNSeqSumLoop64

      ::opConcatNSeqSumLoopEnd64
        --
        -- so result needs to be a dword-sequence of length rdx
        --
        -- can we do things in situ?
        --  result must be = first and != any later (as signalled by edi!=0)
        --  result must already be sequence with refcount 1 and enough space
        --
--      test rdi,rdi                -- ref result if first & not later, else 0
--      jz :opConcatNnewSeq64       -- result not first/not single use
        cmp rdi,r15                 -- check for int=int&... case (covers "")
        jle :opConcatNnewSeq64
        cmp qword[rbx+rdi*4-16],1   -- refcount
        jne :opConcatNnewSeqClone64
--      jne :opConcatNnewSeq64
        cmp byte[rbx+rdi*4-1],#80   -- type byte
        jne :opConcatNnewSeq64
        mov rcx,[rbx+rdi*4-32]      -- maxlen
        mov rax,[rbx+rdi*4-24]      -- length   
        sub rcx,40                  -- (maxlen-hdr)
--8/2/15:
--      mov rsi,[rbx+rax*4-40]      -- slack
        mov rsi,[rbx+rdi*4-40]      -- slack
--      shl rax,3                   -- length -> qwords
        shl rax,3                   -- length in qwords -> bytes
        sub rcx,rsi                 -- (maxlen-hdr-slack)
        shl rdi,2                   -- ref -> raw
        shr rcx,3                   -- bytes -> qwords
        cmp rcx,rdx
        jl :opConcatNnewSeq64       -- insufficient space
        mov [rdi-24],rdx            -- set new length
        pop rcx                     -- restore N
        mov rdx,rbx                 -- target address:=0 (insitu occurred)
        add rsp,8                   -- discard addr target
        add rdi,rax                 -- stick new content here
        pop rax                     -- discard first
        sub rcx,1                   -- process one less entry
        jmp :opConcatNSeqLoop64

      ::opConcatNnewSeqClone64
        mov rcx,rdx
        mov rdx,[rsp]
        mov rdx,[rsp+rdx*8+16]
        call :%pAlloClone
        jmp @f

      ::opConcatNnewSeq64
        mov rcx,rdx
--DEV wronG!! (2/8/15)
--      mov rdx,[rsp+rdx*8]         -- era
        mov rdx,[rsp]
        mov rdx,[rsp+rdx*8+16]
      @@:
        call :%pAllocSeq            -- damages eax only
        pop rcx                     -- re-load N
        pop rdx                     -- target address (nb must remain undamaged for a long time)
        mov rsi,rax                 -- save newly allocated ref ("")
        lea rdi,[rbx+rax*4]         -- raw addr of newly allocated sequence

      ::opConcatNSeqLoop64
        pop rax                     -- next ref
        cmp rax,r15
        jl :opConcatNCopyInt64
            test byte[rbx+rax*4-1],0x80
            jnz @f
                -- it is a float then
                add qword[rbx+rax*4-16],1
                jmp :opConcatNCopyInt64
          @@:
            push rsi                -- newly allocated ref
            push rcx                -- remaining N
            lea rsi,[rbx+rax*4]
            mov rcx,[rbx+rax*4-24]  -- length
            test rcx,rcx
            jz :opConcatNZeroLengthString64
            cmp byte[rsi-1],0x82
            je :opConcatNCopyStr64
              ::opConcatNCopySeqLoop64
                lodsq               -- mov rax,[rsi], rsi+=8
                stosq               -- mov [rdi],rax; rdi+=8
                cmp rax,r15
                jl @f
                    add qword[rbx+rax*4-16],1
              @@:
                sub rcx,1
                jnz :opConcatNCopySeqLoop64
                pop rcx
                pop rsi
                jmp :opConcatNSeqContinue64

              ::opConcatNCopyStr64
                xor rax,rax
              ::opConcatNCharLoop64
                lodsb               -- mov al,[rsi], rsi+=1
                stosq               -- mov [rdi],rax; rdi+=8
                sub rcx,1
                jnz :opConcatNCharLoop64
          ::opConcatNZeroLengthString64
            pop rcx                 -- remaining N
            pop rsi                 -- newly allocated ref
            jmp :opConcatNSeqContinue64

      ::opConcatNCopyInt64
        stosq                       -- mov [rdi],rax; rdi+=8
      ::opConcatNSeqContinue64
        sub rcx,1
        jnz :opConcatNSeqLoop64

      ::opConcatNStore64
        mov rdi,rdx
        test rdx,rdx
        jz @f               -- in situ occurred
            mov rdx,[rdi]
            mov [rdi],rsi
            cmp rdx,r15
            jle @f
                sub qword[rbx+rdx*4-16],1
                jz :%pDealloc
      @@:
        ret
    []
      }

