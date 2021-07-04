--
-- pRepeN.e
-- ========
--
--  Various subscripting routines:
--
--      :%pRepe             -- eax[esp]..[esp+n]:=[edi]
--      :%pRepe1            -- esi[edi]:=ecx, aka ref[idx]:=rep
--      :%pRepe1ip          -- esi[edi]:=ecx, as opRepe1 when esi is sequence of integer, as proved by gvar_scan.
--      :%pRepe1is          -- esi[edi]:=ecx, as opRepe1 when esi is string, as proved by gvar_scan.
--
--  The latter three are optimised forms of :%pRepe, with exactly 1 index and other strict rules.
--  If porting, you could just (temporarily) implement :%pRepe and use it for everything, but 
--  that would require matching (temp) changes to psym.e, pmain.e, and pilx86.e.
--

include builtins\VM\pHeap.e     -- :%pDealloc, :%pAllocStr, :%pAllocSeq

include builtins\VM\pFixup.e    -- negative and floating point index handling (:%fixupIndex)

bool ma_ip = false      -- Set true under with js during {a,b} = x operations, to catch any
                        -- attempts to part-modify strings, as in {s[i]} := {ch}, since the
                        -- otherwise excellent JavaScript desequencing will just not cope,
                        -- because strings are immutable in JavaScript. (For eg s[i] = ch
                        -- when not part of multiple assignment, it gets transpiled into
                        -- s=$repe(s,i,ch) which uses substring + fromCharCode + substring 
                        -- in order to get round the whole immutable strings thingymajig.)

#ilASM{ jmp :%opRetf

--DEV FIXME: (and the :!bang labels below)
--  ::e04atsaa08
--      int3
--  ::e04atsaa8
--      int3
    ::e04atsaa9
        int3
    ::e04atsaa4
        int3
    ::e110Repe1is
        int3

--Fine, tested:
    [32]
     ::e52jsdnssd8
        add esp,4
     ::e52jsdnssd4
        add esp,4
     ::e52jsdnssd
        pop edx
        xor eax,eax
        call :%pDeSeqip
        mov al,52           -- e52jsdnssd
        sub edx,1
--      mov [ma_ip],ebx
        jmp :!iDiag
        int3
    [64]
     ::e52jsdnssd16
        add rsp,8
     ::e52jsdnssd8
        add rsp,8
     ::e52jsdnssd
        pop rdx
        xor rax,rax
        call :%pDeSeqip
        mov al,52           -- e52jsdnssd
        sub rdx,1
--      mov [ma_ip],rbx
        jmp :!iDiag
        int3
    []

--/*
procedure :%pRepe(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pRepe         -- eax[esp]..[esp+n]:=[edi]
-----------
    [32]
        --calling convention:
        --  push <return addr>
        --  push [rep]              (opUnassigned)
        --  push [idxn]..[idx1]     (opUnassigned)
        --  mov ecx,n
        --  lea eax,[ref]
        --  jmp :%pRepe
        --<return addr>
      ::opRepeWhile
        --
        -- On entry, eax is address of ref, ecx is remaining indexes,
        --  next index to apply is on the stack
        --
        mov esi,[eax]           -- ref
        pop edi                 -- idx
        cmp esi,h4
--      jl :e04atsaa08          -- attempt to subscript an atom, era @ [esp+ecx*4]
        jge @f
            mov edx,[esp+ecx*4]
            mov al,4            -- e04atssaa
            sub edx,1
            jmp :!iDiag
            int3
      @@:
        shl esi,2               -- raw addr of ref
        push eax                --[1] ref addr, in case we need to clone...
        sub edi,1
        mov edx,[esi-12]        -- length
--16/3/17!!
--      cmp byte[esi-1],0x80
--      jne :RepeStr
        cmp edi,edx             -- cmp idx,length
        jb @f                   -- unsigned jump, lets 0..len-1 through
            add ecx,1
            mov al,8+4+0        -- [era] @ [esp+ecx*4+4], "assigning to"
            call :%fixupIndex   -- idx-1 in edi, len in edx, al set
            sub ecx,1
      @@:
        cmp byte[esi-1],0x80
        jne :RepeStr
        --
        -- edi now contains 0-based index to replace, and edx the length
        -- ecx is remaining indexes (including the one in edi)
        --   
        mov eax,[esi-8]         -- refcount
        sub eax,1
        jz :RepeSeqNoClone
        mov [esi-8],eax         -- non-1 so no need to dealloc
        push ecx                --[2] remaining idx
        push edi                --[3] idx
        mov edi,[esp+8]         --[1] (ref addr, leaving it on the stack)
--      mov ecx,edx
        push edx
--      mov edx,[esp+ecx*4+20]  -- era
        mov edx,[esp+ecx*4+16]  -- era
        mov ecx,[esp]
        call :%pAlloClone
        call :%pAllocSeq        -- damages eax only
        pop edx
        mov [edi],eax           -- Replace the ref at the original address
        lea edi,[ebx+eax*4]
        push edi                --[4] raw addr of newly allocated sequence
        mov ecx,edx
      @@:
            lodsd               -- mov eax,[esi], esi+=4
            stosd               -- mov [edi],eax, edi+=4
            cmp eax,h4
            jl :Repe_no_incref
                add dword[ebx+eax*4-8],1
          ::Repe_no_incref
            sub ecx,1
            jnz @b

        pop esi                 --[4] NB esi:=edi!
        pop edi                 --[3] idx
        pop ecx                 --[2] remainding idx

      ::RepeSeqNoClone
        add esp,4               --[1] discard
        lea eax,[esi+edi*4]
        sub ecx,1
        jnz :opRepeWhile

        pop ecx                 -- replacement
        mov edx,[eax]
        cmp ecx,h4
        jl @f
          add dword[ebx+ecx*4-8],1
      @@:
        mov [eax],ecx
        cmp edx,h4
        jle @f
          sub dword[ebx+edx*4-8],1
          jz :%pDealloc
      @@:
        ret

      ::RepeStr
        cmp byte[esi-1],0x82
--      jne :e04atsaa8          -- era @ [esp+ecx*4]
        je @f
--mov al,byte[esi-1]
            mov edx,[esp+ecx*4+4]
            mov al,4    -- e04atasaa
            sub edx,1
            jmp :!iDiag
            int3
      @@:
        sub ecx,1
        jnz :e04atsaa9          -- must be last index, era @ [esp+ecx*4+4]
        cmp [ma_ip],ebx
        jne :e52jsdnssd8
        mov ecx,[esp+4]         -- replacement
        xor ebx,ebx
        cmp ecx,255
        ja :RepeExpandString    -- also jumps for -ve & non-int
        cmp [esi+edi],cl        -- avoid clone if unchanged
        je @f
        cmp dword[esi-8],1      -- refcount 1?
        jnz :RepeDupString
        mov [esi+edi],cl        -- replace char in situ then
      @@:
        add esp,8               -- discard [1] and replacement
        ret

      ::RepeDupString
        sub dword[esi-8],1      -- non-1 so no need to dealloc
        mov ecx,edx
        pop edx                 --[1] this is why we saved it!
        call :%pAllocStr        -- damages eax only
        add ecx,1               -- include trailing null
        mov [edx],eax           -- replace ref at original address
        lea edx,[eax*4+edi]     -- replacement char location
        lea edi,[ebx+eax*4]
        rep movsb
        pop ecx
        mov [edx],cl
        ret

      ::RepeExpandString
        mov ecx,edx
        mov edx,[esp+8]         -- era
        call :%pAllocSeq        -- damages eax only
        push eax                --[2]
        shl eax,2
        lea edx,[eax+edi*4]     -- replacement location
        mov edi,eax
        xor eax,eax
      @@:
            lodsb               -- mov al,[esi], esi+=1
            stosd               -- mov [edi],eax, edi+=4
            sub ecx,1
            jnz @b

        mov eax,[esp+8]         -- replacement
        cmp eax,h4
        jl @f
          add dword[ebx+eax*4-8],1
      @@:
        pop ecx                 --[2] newly allocated sequence
        pop edi                 --[1] address of ref to replace
        mov [edx],eax           -- replace element (a char that became a dword)
        add esp,4               -- discard replacement
        mov edx,[edi]           -- get old
        mov [edi],ecx           -- replace expanded string
        cmp edx,h4
        jle @f
          sub dword[ebx+edx*4-8],1
          jz :%pDealloc
      @@:
        ret
    [64]
        --calling convention:
        --  push <return addr>
        --  push [rep]              (opUnassigned)
        --  push [idxn]..[idx1]     (opUnassigned)
        --  mov rcx,n
        --  lea rax,[ref]
        --  jmp :%pRepe
        --<return addr>
        mov r15,h4
      ::opRepeWhile
        --
        -- On entry, rax is address of ref, rcx is remaining indexes,
        --  next index to apply is on the stack
        --
        mov rsi,[rax]       -- ref
        pop rdi             -- idx
        cmp rsi,r15
--      jl :e04atsaa08      -- attempt to subscript an atom, era @ [esp+ecx*4]
        jge @f
            mov rdx,[rsp+rcx*8]
            mov al,4        -- e04atssaa
            sub rdx,1
            jmp :!iDiag
            int3
      @@:
        shl rsi,2           -- raw addr of ref
        push rax            --[1] ref addr, in case we need to clone...
        sub rdi,1
--      mov rdx,[rsi-12]    -- length
        mov rdx,[rsi-24]    -- length
--16/3/17!!
--      cmp byte[rsi-1],0x80
--      jne :RepeStr
        cmp rdi,rdx         -- cmp idx,length
        jb @f               -- unsigned jump, lets 0..len-1 through
            add rcx,1
            mov al,8+4+0        -- [era] @ [rsp+rcx*8+8], "assigning to"
            call :%fixupIndex   -- idx-1 in rdi, len in rdx, al set
            sub rcx,1
      @@:
        cmp byte[rsi-1],0x80
        jne :RepeStr
        --
        -- rdi now contains 0-based index to replace, and rdx the length
        -- rcx is remaining indexes (including the one in rdi)
        --   
        mov rax,[rsi-16]        -- refcount
        sub rax,1
        jz :RepeSeqNoClone
        mov [rsi-16],rax        -- non-1 so no need to dealloc
        push rcx                --[2] remaining idx
        push rdi                --[3] idx
        mov rdi,[rsp+16]        --[1] (ref addr, leaving it on the stack)
--      mov rcx,rdx
        push rdx
--      mov rdx,[rsp+rcx*8+40]  -- era
        mov rdx,[rsp+rcx*8+32]  -- era
        mov rcx,[rsp]
        call :%pAlloClone
        call :%pAllocSeq        -- damages rax only
        pop rdx
        mov [rdi],rax           -- Replace the ref at the original address
        lea rdi,[rbx+rax*4]
        push rdi                --[4] raw addr of newly allocated sequence
        mov rcx,rdx
--      mov r15,h4
      @@:
            lodsq               -- mov rax,[rsi], rsi+=8
            stosq               -- mov [rdi],rax, rdi+=8
            cmp rax,r15
            jl :Repe_no_incref
                add qword[rbx+rax*4-16],1
          ::Repe_no_incref
            sub rcx,1
            jnz @b

        pop rsi                 --[4] NB rsi:=rdi!
        pop rdi                 --[3] idx
        pop rcx                 --[2] remainding idx

      ::RepeSeqNoClone
        add rsp,8               --[1] discard
        lea rax,[rsi+rdi*8]
        sub rcx,1
        jnz :opRepeWhile

        pop rcx                 -- replacement
        mov rdx,[rax]
--      mov r15,h4
        cmp rcx,r15
        jl @f
          add qword[rbx+rcx*4-16],1
      @@:
        mov [rax],rcx
        cmp rdx,r15
        jle @f
          sub qword[rbx+rdx*4-16],1
          jz :%pDealloc
      @@:
        ret

      ::RepeStr
        cmp byte[rsi-1],0x82
--      jne :e04atsaa8          -- era @ [esp+ecx*4]
        je @f
--mov al,byte[rsi-1]
            mov rdx,[rsp+rcx*8+8]
            mov al,4    -- e04atsaa
            sub rdx,1
            jmp :!iDiag
            int3
      @@:
        sub rcx,1
        jnz :e04atsaa9          -- must be last index, era @ [esp+ecx*4+4] [??]
        cmp [ma_ip],rbx
        jne :e52jsdnssd16
        mov rcx,[rsp+8]         -- replacement (from calling convention)
        xor rbx,rbx
        cmp rcx,255
        ja :RepeExpandString    -- also jumps for -ve & non-int
        cmp [rsi+rdi],cl        -- avoid clone if unchanged
        je @f
        cmp qword[rsi-16],1     -- refcount 1?
        jnz :RepeDupString
        mov [rsi+rdi],cl        -- replace char in situ then
      @@:
        add rsp,16              -- discard [1] and replacement
        ret

      ::RepeDupString
        sub qword[rsi-16],1     -- non-1 so no need to dealloc
        mov rcx,rdx
        pop rdx                 --[1] this is why we saved it!
        call :%pAllocStr        -- damages rax only
        add rcx,1               -- include trailing null
        mov [rdx],rax           -- replace ref at original address
        lea rdx,[rax*4+rdi]     -- replacement char location
        lea rdi,[rbx+rax*4]
        rep movsb
        pop rcx
        mov [rdx],cl
        ret

      ::RepeExpandString
        mov rcx,rdx
        mov rdx,[rsp+16]        -- era
        call :%pAllocSeq        -- damages rax only
        push rax                --[2]
        shl rax,2
        lea rdx,[rax+rdi*8]     -- replacement location
        mov rdi,rax
        xor rax,rax
      @@:
            lodsb               -- mov al,[rsi], rsi+=1
            stosq               -- mov [rdi],rax, rdi+=8
            sub rcx,1
            jnz @b

        mov rax,[rsp+16]        -- replacement
--      mov r15,h4
        cmp rax,r15
        jl @f
          add qword[rbx+rax*4-16],1
      @@:
        pop rcx                 --[2] newly allocated sequence
        pop rdi                 --[1] address of ref to replace
        mov [rdx],rax           -- replace element (a char that became a dword)
        add rsp,8               -- discard replacement
        mov rdx,[rdi]           -- get old
        mov [rdi],rcx           -- replace expanded string
        cmp rdx,r15
        jle @f
          sub qword[rbx+rdx*4-16],1
          jz :%pDealloc
      @@:
        ret
    []

--/*
procedure :%pRepe1(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pRepe1            -- esi[edi]:=ecx, aka ref[idx]:=rep
------------
    [32]
        --calling convention:
        --  mov edi,[idx]       (opUnassigned)
        --  mov ecx,[rep]       (opUnassigned)
        --  mov esi,[ref]       (opUnassigned)
        --  lea eax,[ref]
        --  call :%pRepe1       -- ref[idx]:=rep
        sub edi,1                   -- idx -= 1
--  mov [rep1ra],eax            -- save addr rep, in case we clone [ref?!]
        push eax                    --[0] save addr ref
        mov edx,[ebx+esi*4-12]      -- get length
        cmp edi,edx                 -- if idx is -ve/float/oob then longhand
        jb @f
            mov al,4+0              -- [era] @ [esp+8] "assigning to"
            call :%fixupIndex       -- idx-1 in edi, len in edx, not idx addr in ebx, al set
      @@:
        cmp dword[ebx+esi*4-8],1    -- if refcount!=1 then clone
        jne :opRepe1Clone
        lea eax,[ebx+esi*4]
        cmp byte[ebx+esi*4-1],0x80  -- type byte
        jbe :opRepe1Sequence
        cmp [ma_ip],ebx
        jne :e52jsdnssd4
        cmp ecx,255
        ja :opRepe1ExpandString
        add esp,4
        mov [eax+edi],cl
        ret

      ::opRepe1ExpandString
        -- esi is ref of source string, of refcount 1, will need dealloc
        -- edi is idx-1, checked to be in bounds
        -- ecx is some non-char replacement
        -- edx contains the original length
        -- eax is src base (not used/refetched here)
        -- [esp] is addr ref
        cmp ecx,h4
        jl @f
            add dword[ebx+ecx*4-8],1
      @@:
        pop eax                     --[0]
        push esi                    --[1] save the string ref for final dealloc
        push ecx                    --[2] Replacement ref (rep)
--newEBP
--? mov ecx,[rep1ra]            -- ref addr
        mov ecx,edx
--      mov edx,eax
        push eax
        mov edx,[esp+8]             -- era
        call :%pAllocSeq            -- damages eax only
        pop edx
--  mov ecx,[ecx-9]             -- ref addr
        lea esi,[ebx+esi*4]
--DEV would    shl esi,2    be any better?
        mov [edx],eax               -- replace it now
        mov edx,ecx
        lea ecx,[ebx+edi*4]         -- idx -> dwords
        lea edi,[ebx+eax*4]         -- new base
        xor eax,eax
        add ecx,edi                 -- replacement location
      @@:
--DEV lodsb stosd??
--      mov al,[esi]
--      inc esi
        lodsb
--      mov [edi],eax
--      add edi,4
        stosd
--      dec edx
        sub edx,1
        jnz @b

        pop esi                     --[2] Replacement ref
        pop edx                     --[1] previous content (a string)
        mov [ecx],esi
        jmp :%pDealloc

      ::opRepe1Sequence
        -- esi is source ref, of refcount 1 so replace in situ, 
        --                    unless it is an atom (jl error)
        -- edi is idx-1, checked to be in bounds
        -- ecx is rep (may need incref)
        -- edx contains the original length (no longer needed)
        -- eax is src base
        -- [esp] is addr ref
        mov edx,[eax+edi*4]         -- get prev s[i]
        jl :e04atsaa4               -- attempt to subscript an atom (era @ [esp-4])
        cmp ecx,h4
        jl @f
            add dword[ebx+ecx*4-8],1
      @@:
        mov [eax+edi*4],ecx         -- rep
        pop eax                     --[0] discard
        cmp edx,h4
        jle @f
            sub dword[ebx+edx*4-8],1
            jz :%pDealloc
      @@:
        ret

      ::opRepe1Clone
        -- esi is src ref (unknown type), of refcount >1, (so just decref it)
        -- edi is idx-1,
        -- ecx is rep (may need incref)
        -- edx contains the original length
        -- eax is src base [BLUFF: addr ref]
        -- [esp] is addr ref
        sub dword[ebx+esi*4-8],1    -- reduce refcount (not 1, dealloc not rqd)
        push ecx                    --[1] save rep
        cmp ecx,h4                  -- incref if needed
        jl @f
            add dword[ebx+ecx*4-8],1
      @@:
        cmp byte[ebx+esi*4-1],0x80  -- type byte
        jbe :opRepe1CloneSequence
        cmp [ma_ip],ebx
--27/6/21:
--      jne :e52jsdnssd4
        jne :e52jsdnssd8
        cmp ecx,255
        ja :opRepe1CloneExpandStr
--  mov ecx,[esp+4]             -- return addr
--newEBP
--  mov ecx,[rep1ra]
        mov ecx,edx
        mov edx,[esp+4]             -- [0]
        call :%pAllocStr            -- damages eax only
--  mov ecx,[ecx-9]             -- ref addr
--  lea esi,[ebx+esi*4]
        shl esi,2                   -- src base
        mov [edx],eax               -- replace it now
        add ecx,1                   -- include trailing null in following rep movsb
        lea edx,[eax*4+edi]         -- replacement char location
        lea edi,[ebx+eax*4]         -- raw(new)
        pop eax                     --[1] Replacement char
        rep movsb
        mov [edx],al
        add esp,4                   --[0]
        ret

      ::opRepe1CloneSequence
        -- esi is src ref, already decrefed, <= #80 (ie jl error rqd)
        -- edi is idx-1,
        -- edx contains the original length (ready for AllocSeq)
        -- eax is src base [BLUFF: addr ref]
        -- [esp] == ecx is rep (already increfd)
        -- [esp+4] is addr ref
--newEBP::
--> mov ecx,[rep1ra]
        mov ecx,edx
--      jl :e04atsaa8               -- attempt to subscript an atom (era @ [esp+8])
        jge @f
mov al, byte[ebx+esi*4-1]   -- type byte
            int3
      @@:
        mov edx,[esp+8]             -- era
        call :%pAlloClone
        call :%pAllocSeq            -- damages eax only
        mov edx,[esp+4]             -- [0]
--  mov ecx,[ecx-9]             -- ref addr
        lea edi,[eax+edi]           -- after shl2 below will effectively be ...
--  lea esi,[ebx+esi*4]
        shl esi,2                   -- src base
        shl edi,2                   -- rep addr ... [eax*4(new base)+edi*4(idx->dwords)]
        mov [edx],eax               -- replace it now (WOW! no AGI!)
        shl eax,2                   -- new base
      @@:
        mov edx,[esi]
        add esi,4
        mov [eax],edx
        add eax,4
        cmp edx,h4
        jl :Repe1_no_incref
            -- exception here fatal (mid-clone/back-end error)
            add dword[ebx+edx*4-8],1
      ::Repe1_no_incref
        sub ecx,1
        jnz @b
        mov edx,[edi]               -- prev s[i]
        pop ecx                     -- [1] rep ref (already incref'd)
        pop eax                     -- [0] discard
        mov [edi],ecx
        cmp edx,h4
        jl @f
            sub dword[ebx+edx*4-8],1
            jz :%pDealloc
      @@:
        ret

      ::opRepe1CloneExpandStr
        -- esi is src ref (string), already decrefed
        -- edi is idx-1,
        -- edx contains the original length (ready for AllocSeq)
        -- eax is src base [bluff: addr ref]
        -- [esp] == ecx is non-char rep (already increfd)
        -- [esp+4] is addr ref
--  mov ecx,[esp+4]             -- return addr
--  opRepe1is110: ---- rejoin point for e110Repe1is [DEV]
--newEBP::
--> mov ecx,[rep1ra]
        mov ecx,edx
        mov edx,[esp+8]         -- era
        call :%pAllocSeq        -- damages eax only
        mov edx,[esp+4]         -- [0]
--  mov ecx,[ecx-9]             -- ref addr
--  lea edi,[eax+edi]           -- after shl2 below will effectively be ...
        add edi,eax                 -- after shl2 below will effectively be ...
--  lea esi,[ebx+esi*4]
        shl esi,2               -- src base
        shl edi,2               -- rep addr ... [eax*4(new base)+edi*4(idx->dwords)]
        mov [edx],eax           -- replace it now
        mov edx,ecx
        shl eax,2               -- new base
        xor ecx,ecx
      @@:
        mov cl,[esi]
        add esi,1
        mov [eax],ecx
        add eax,4
        sub edx,1
        jnz @b
--      pop ecx                 -- [1] rep
        pop dword[edi]          -- [1] rep
        pop eax                 -- [0] discard
--      mov [edi],ecx
        ret
    [64]
        --calling convention:
        --  mov rdi,[idx]       (opUnassigned)
        --  mov rcx,[rep]       (opUnassigned)
        --  mov rsi,[ref]       (opUnassigned)
        --  lea rax,[ref]
        --  call :%pRepe1       -- ref[idx]:=rep
        sub rdi,1                   -- idx -= 1
        push rax                    --[0] save addr ref
        mov rdx,[rbx+rsi*4-24]      -- get length
        cmp rdi,rdx                 -- if idx is -ve/float/oob then longhand
        jb @f
            mov al,4+0              -- [era] @ [rsp+16] "assigning to"
            call :%fixupIndex       -- idx-1 in rdi, len in rdx, not idx addr in rbx, al set
      @@:
        cmp qword[rbx+rsi*4-16],1   -- if refcount!=1 then clone
        jne :opRepe1Clone
        lea rax,[rbx+rsi*4]
        cmp byte[rbx+rsi*4-1],0x80  -- type byte
        jbe :opRepe1Sequence
        cmp [ma_ip],rbx
        jne :e52jsdnssd8
        cmp rcx,255
        ja :opRepe1ExpandString
        add rsp,8
        mov [rax+rdi],cl
        ret

      ::opRepe1ExpandString
        -- rsi is ref of source string, of refcount 1, will need dealloc
        -- rdi is idx-1, checked to be in bounds
        -- rcx is some non-char replacement
        -- rdx contains the original length
        -- rax is src base (not used/refetched here)
        -- [rsp] is addr ref
        mov r15,h4
        cmp rcx,r15
        jl @f
            add qword[rbx+rcx*4-16],1
      @@:
        pop rax                     --[0]
        push rsi                    --[1] save the string ref for final dealloc
        push rcx                    --[2] Replacement ref (rep)
        mov rcx,rdx
--      mov rdx,rax
        mov r9,rax
        mov rdx,[rsp+16]            -- era
        call :%pAllocSeq            -- damages rax only
        lea rsi,[rbx+rsi*4]
--      mov [rdx],rax               -- replace it now
        mov [r9],rax                -- replace it now
        mov rdx,rcx
        lea rcx,[rbx+rdi*8]         -- idx -> qwords
        lea rdi,[rbx+rax*4]         -- new base
        xor rax,rax
        add rcx,rdi                 -- replacement location
      @@:
            lodsb
            stosq
            sub rdx,1
            jnz @b
        pop rsi                     --[2] Replacement ref
        pop rdx                     --[1] previous content (a string)
        mov [rcx],rsi
        jmp :%pDealloc

      ::opRepe1Sequence
        -- rsi is source ref, of refcount 1 so replace in situ, 
        --                    unless it is an atom (jl error)
        -- rdi is idx-1, checked to be in bounds
        -- rcx is rep (may need incref)
        -- rdx contains the original length (no longer needed)
        -- rax is src base
        -- [rsp] is addr ref
        mov rdx,[rax+rdi*8]         -- get prev s[i]
        jl :e04atsaa4               -- attempt to subscript an atom (era @ [esp-4])
        mov r15,h4
        cmp rcx,r15
        jl @f
            add qword[rbx+rcx*4-16],1
      @@:
        mov [rax+rdi*8],rcx         -- rep
--21/8/15 (debug assist)
--      pop rax                     --[0] discard
        add rsp,8                   --[0] discard
        cmp rdx,r15
        jle @f
            sub qword[rbx+rdx*4-16],1
            jz :%pDealloc
      @@:
        ret

      ::opRepe1Clone
        -- rsi is src ref (unknown type), of refcount >1, (so just decref it)
        -- rdi is idx-1,
        -- rcx is rep (may need incref)
        -- rdx contains the original length
        -- rax is src base [BLUFF: addr ref]
        -- [rsp] is addr ref
        sub qword[rbx+rsi*4-16],1   -- reduce refcount (not 1, dealloc not rqd)
        push rcx                    --[1] save rep
        mov r15,h4
        cmp rcx,r15                 -- incref if needed
        jl @f
            add qword[rbx+rcx*4-16],1
      @@:
        cmp byte[rbx+rsi*4-1],0x80  -- type byte
        jbe :opRepe1CloneSequence
        cmp [ma_ip],rbx
--27/6/21:
--      jne :e52jsdnssd8
        jne :e52jsdnssd16
        cmp rcx,255
        ja :opRepe1CloneExpandStr
        mov rcx,rdx
        mov rdx,[rsp+8]             --[0] ref addr
        call :%pAllocStr            -- damages rax only
        shl rsi,2                   -- src base
        mov [rdx],rax               -- replace it now
        add rcx,1                   -- include trailing null in following rep movsb
        lea rdx,[rax*4+rdi]         -- replacement char location
        lea rdi,[rbx+rax*4]         -- raw(new)
        pop rax                     --[1] Replacement char
        rep movsb
        mov [rdx],al
        add rsp,8                   --[0]
        ret

      ::opRepe1CloneSequence
        -- rsi is src ref, already decrefed, <= #80 (ie jl error rqd)
        -- rdi is idx-1,
        -- rdx contains the original length (ready for AllocSeq)
        -- rax is src base [BLUFF: addr ref]
        -- [rsp] == rcx is rep (already increfd)
        -- [rsp+8] is addr ref
        mov rcx,rdx
--      jl :e04atsaa8               -- attempt to subscript an atom (era @ [ebp-8])
        jge @f
mov al,byte[rbx+rsi*4-1]    -- type byte
          int3
      @@:
        mov rdx,[rsp+16]            -- era
        call :%pAlloClone
        call :%pAllocSeq            -- damages rax only
--      mov rdx,[rsp+4]             -- [0]
        mov rdx,[rsp+8]             -- [0]
        lea rdi,[rax+rdi*2]         -- after shl2 below will effectively be ...
        shl rsi,2                   -- src base
        shl rdi,2                   -- rep addr ... [rax*4(new base)+rdi*8(idx->qwords)]
        mov [rdx],rax               -- replace it now (WOW! no AGI!)
        mov rdx,rcx
        shl rax,2                   -- new base
        mov r15,h4
      @@:
        mov rcx,[rsi]
        add rsi,8
        mov [rax],rcx
        add rax,8
        cmp rcx,r15
        jl :Repe1_no_incref
            -- exception here fatal (mid-clone/back-end error)
            add qword[rbx+rcx*4-16],1
      ::Repe1_no_incref
        sub rdx,1
        jnz @b
        mov rdx,[rdi]               -- prev s[i]
        pop rcx                     -- [1] rep ref (already incref'd)
        pop rax                     -- [0] discard
        mov [rdi],rcx
        cmp rdx,r15
        jl @f
            sub qword[rbx+rdx*4-16],1
            jz :%pDealloc
      @@:
        ret

      ::opRepe1CloneExpandStr
        -- rsi is src ref (string), already decrefed
        -- rdi is idx-1,
        -- rdx contains the original length (ready for AllocSeq)
        -- rax is src base [BLUFF: addr ref]
        -- [rsp] == rcx is non-char rep (already increfd)
        -- [rsp+8] is addr ref
--  opRepe1is110: ---- rejoin point for e110Repe1is [DEV]
        mov rcx,rdx
        mov rdx,[rsp+16]        -- era
        call :%pAllocSeq        -- damages rax only
        mov rdx,[rsp+8]         -- [0]
        lea rdi,[rdi*2+rax]     -- after shl2 below will effectively be ...
        shl rsi,2               -- src base
        shl rdi,2               -- rep addr ... [rax*4(new base)+rdi*8(idx->qwords)]
        mov [rdx],rax           -- replace it now
        mov rdx,rcx
        shl rax,2               -- new base
        xor rcx,rcx
      @@:
        mov cl,[rsi]
        add rsi,1
        mov [rax],rcx
        add rax,8
        sub rdx,1
        jnz @b
--      pop qword[rdi]          -- [1] rep
        pop rdx
        pop rax                 -- [0] discard
        mov [rdi],rdx
        ret
    []

--/*
procedure :%pRepe1ip(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pRepe1ip          -- esi[edi]:=ecx, as opRepe1 when esi is sequence of integer, as proved by gvar_scan.
--------------
    [32]
        --calling convention:
        --  mov edi,[idx]       (opUnassigned)
        --  mov ecx,[rep]       (opUnassigned)
        --  mov esi,[ref]       (opUnassigned)
        --  lea eax,[ref]
        --  call :%pRepe1ip     -- ref[idx]:=rep
        sub edi,1                   -- idx -= 1
--  mov [rep1ra],eax
        push eax                    --[0] save ref addr
        mov edx,[ebx+esi*4-12]      -- get length
        lea eax,[ebx+esi*4]
        cmp edi,edx                 -- if idx is -ve/float/oob then longhand
        jb @f
--20/6/16:
--          mov al,2+0              -- [era] @ [esp+4] "assigning to"
            mov al,4+0              -- [era] @ [esp+8] "assigning to"
            call :%fixupIndex       -- idx-1 in edi, len in edx, not idx addr in ebx, al set
            lea eax,[ebx+esi*4]     -- as we just trashed it
      @@:
        cmp dword[ebx+esi*4-8],1    -- if refcount!=1 then clone
        jne :opRepe1ipClone
        add esp,4                   --[0] discard
        mov [eax+edi*4],ecx         -- rep
        ret

      ::opRepe1ipClone
        -- esi is src ref (sequence of integer), of refcount >1, (so just decref it)
        -- edi is idx-1,
        -- ecx is rep (an integer)
        -- edx contains the original length (ready for AllocSeq)
        -- eax is the new (non-0) refcount
        -- [esp] is ref addr
        sub dword[ebx+esi*4-8],1    -- reduce refcount (was not 1, dealloc not rqd)
        push ecx                    --[1] save rep
        mov ecx,edx
        mov edx,[esp+8]             -- era
        call :%pAlloClone
        call :%pAllocSeq            -- damages eax only
        mov edx,[esp+4]             --[0]
        lea edi,[eax+edi]           -- after shl2 below will effectively be ...
        shl esi,2                   -- src base
        shl edi,2                   -- rep addr ... [eax*4(new base)+edi*4(idx->dwords)]
        mov [edx],eax               -- replace it now (WOW! no AGI!)
        shl eax,2                   -- new base
      @@:
        mov edx,[esi]
        add esi,4
        mov [eax],edx
        add eax,4
        sub ecx,1
        jnz @b
        pop dword[edi]              --[1] rep
        add esp,4                   --[0] discard
        ret
    [64]
        --calling convention:
        --  mov rdi,[idx]       (opUnassigned)
        --  mov rcx,[rep]       (opUnassigned)
        --  mov rsi,[ref]       (opUnassigned)
        --  lea rax,[ref]
        --  call :%pRepe1ip     -- ref[idx]:=rep
        sub rdi,1                   -- idx -= 1
        push rax                    --[0] save ref addr
        mov rdx,[rbx+rsi*4-24]      -- get length
        lea rax,[rbx+rsi*4]
        cmp rdi,rdx                 -- if idx is -ve/float/oob then longhand
        jb @f
--20/6/16:
--          mov al,2+0              -- [era] @ [esp+4] "assigning to"
            mov al,4+0              -- [era] @ [rsp+16] "assigning to"
            call :%fixupIndex       -- idx-1 in rdi, len in rdx, not idx addr in rbx, al set
            lea rax,[rbx+rsi*4]     -- as we just trashed it
      @@:
        cmp qword[rbx+rsi*4-16],1   -- if refcount!=1 then clone
        jne :opRepe1ipClone
        add rsp,8                   --[0] discard
--8/7/15:
--      mov [rax+rdi*8],ecx         -- rep
        mov [rax+rdi*8],rcx         -- rep
        ret

      ::opRepe1ipClone
        -- rsi is src ref (sequence of integer), of refcount >1, (so just decref it)
        -- rdi is idx-1,
        -- rcx is rep (an integer)
        -- rdx contains the original length (ready for AllocSeq)
        -- rax is the new (non-0) refcount
        -- [rsp] is ref addr
        sub qword[rbx+rsi*4-16],1   -- reduce refcount (was not 1, dealloc not rqd)
        push rcx                    --[1] save rep
        mov rcx,rdx
        mov rdx,[rsp+16]            -- era
        call :%pAlloClone
        call :%pAllocSeq            -- damages rax only
        mov rdx,[rsp+8]             --[0]
        lea rdi,[rax+rdi*2]         -- after shl2 below will effectively be ...
        shl rsi,2                   -- src base
        shl rdi,2                   -- rep addr ... [rax*4(new base)+rdi*8(idx->qwords)]
        mov [rdx],rax               -- replace it now (WOW! no AGI!)
        shl rax,2                   -- new base
      @@:
            mov rdx,[rsi]
            add rsi,8
            mov [rax],rdx
            add rax,8
            sub rcx,1
            jnz @b
--      pop qword[rdi]              --[1] rep
        pop rax
        add rsp,8                   --[0] discard
        mov [rdi],rax
        ret
    []

--/*
procedure :%pRepe1is(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pRepe1is          -- esi[edi]:=ecx, as opRepe1 when esi is string, as proved by gvar_scan.
--------------          --  (no expand, no dealloc, also has builtin typecheck)
    [32]
        --calling convention:
        --  mov edi,[idx]       (opUnassigned)
        --  mov ecx,[rep]       (opUnassigned) [must be char-sized]
        --  mov esi,[ref]       (opUnassigned) [must be string]
        --  lea eax,[ref]
        --  call :%pRepe1is     -- ref[idx]:=rep, aka esi[edi]:=cl
        sub edi,1                   -- idx -= 1
        mov edx,[ebx+esi*4-12]      -- get length
        cmp [ma_ip],ebx
        jne :e52jsdnssd
        cmp edi,edx                 -- if idx is -ve/float/oob then longhand
        jb @f
            push eax
--17/4/16 undone:
--20/6/16 (spotted in passing, opposite of several others that needed doing)
            mov al,4+0              -- [era] @ [esp+8] "assigning to"
--          mov al,2+0              -- [era] @ [esp+4] "assigning to"
            call :%fixupIndex       -- idx-1 in edi, len in edx, not idx addr in ebx, al set
            pop eax
      @@:
        cmp ecx,255
        ja :e110Repe1is             -- type check error
        cmp dword[ebx+esi*4-8],1    -- if refcount!=1 then clone
        jne :opRepe1isClone
        mov [esi*4+edi],cl
        ret

      ::opRepe1isClone
        -- esi is src ref (string), of refcount >1, (so just decref it)
        -- edi is idx-1,
        -- ecx is rep (an integer char)
        -- edx contains the original length
        -- eax is the new (non-0) refcount
        sub dword[ebx+esi*4-8],1    -- reduce refcount (non-0, dealloc not rqd)
        push ecx                    --[1] save rep
        mov ecx,edx
        mov edx,eax
        call :%pAllocStr            -- damages eax only
        lea esi,[ebx+esi*4]
--DEV would    shl esi,2    be any better?
        mov [edx],eax               -- replace it now
        add ecx,1                   -- include trailing null in following rep movsb
        lea edx,[eax*4+edi]         -- replacement char location
        lea edi,[ebx+eax*4]         -- raw(new)
        pop eax                     --[1] Replacement char
        rep movsb
        mov [edx],al
        ret

    [64]
        --calling convention:
        --  mov rdi,[idx]       (opUnassigned)
        --  mov rcx,[rep]       (opUnassigned) [must be char-sized]
        --  mov rsi,[ref]       (opUnassigned) [must be string]
        --  lea rax,[ref]
        --  call :%pRepe1is     -- ref[idx]:=rep, aka rsi[rdi]:=cl
        sub rdi,1                   -- idx -= 1
        mov rdx,[rbx+rsi*4-24]      -- get length
        cmp [ma_ip],rbx
        jne :e52jsdnssd
        cmp rdi,rdx                 -- if idx is -ve/float/oob then longhand
        jb @f
            push rax
--17/4/17 undone:
--20/6/16 (spotted in passing, opposite of several others that needed doing)
            mov al,4+0              -- [era] @ [rsp+16] "assigning to"
--          mov al,2+0              -- [era] @ [esp+4] "assigning to"
            call :%fixupIndex       -- idx-1 in rdi, len in rdx, not idx addr in rbx, al set
            pop rax
      @@:
        cmp rcx,255
        ja :e110Repe1is             -- type check error
        cmp qword[rbx+rsi*4-16],1   -- if refcount!=1 then clone
        jne :opRepe1isClone
        mov [rsi*4+rdi],cl
        ret

      ::opRepe1isClone
        -- rsi is src ref (string), of refcount >1, (so just decref it)
        -- rdi is idx-1,
        -- rcx is rep (an integer char)
        -- rdx contains the original length
        -- rax is the new (non-0) refcount
        sub qword[rbx+rsi*4-16],1   -- reduce refcount (non-0, dealloc not rqd)
        push rcx                    --[1] save rep
        mov rcx,rdx
        mov rdx,rax
        call :%pAllocStr            -- damages rax only
        lea rsi,[rbx+rsi*4]
        mov [rdx],rax               -- replace it now
        add rcx,1                   -- include trailing null in following rep movsb
        lea rdx,[rax*4+rdi]         -- replacement char location
        lea rdi,[rbx+rax*4]         -- raw(new)
        pop rax                     --[1] Replacement char
        rep movsb
        mov [rdx],al
        ret
    []

    :%pDeSeqip          -- [ma_ip]:=e/rax. A multiple assigment (aka desequence/destructure)
--------------          --                 operation is in progress under with js therefore
                        --                 string subscript (/replacements) are now illegal,
                        --                 that is at least when [ma_ip] is non-zero.
    [32]
        mov [ma_ip],eax
    [64]
        mov [ma_ip],rax
    []
        jmp :%pDeSeqip2 -- and mirror the copy in pSubseN.e

      }

