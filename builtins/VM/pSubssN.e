--
-- pSubssN.e
-- =========
--
--  Various subscripting routines:
--
--      :%pSubss            -- res := ref[idx1]~[idxN..sliceend]
--      :%pSubsss           -- res := string[slicestart..sliceend]
--

include builtins\VM\pHeap.e     -- :%pDealloc, :%pAllocStr, :%pAllocSeq

include builtins\VM\pFixup.e    -- negative and floating point index handling (:%fixupIndex)

#ilASM{ jmp :%opRetf

--DEV FIXME: (and the :!bang labels below)
    ::e04atsaa4
        int3
    ::e04atsaa9
        int3
    ::e04atsaap8
        int3
    ::e04atsaap12
        int3
    ::e09slinecx
        [32]
            mov edx,[esp+8]         -- era
            mov edi,[esp]
            add ecx,edi
            add edi,1
            mov al,9                -- e09slin(edi,ecx)
            sub edx,1
        [64]
            mov rdx,[rsp+16]        -- era
            mov rdi,[rsp]
            add rcx,rdi
            add rdi,1
            mov al,9                -- e09slin(rdi,rcx)
            sub rdx,1
        []
            jmp :!iDiag
            int3
--  ::e09slinecx2
--      int3

--/*
procedure :%pSubss(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pSubss        -- res := ref[idx1]~[idxN..sliceend]
------------
    [32]
        --calling convention:
        -- <perform any required opUnassigned's>
        --  mov ecx,N
        --  push <return address>
        --  push res            -- result var addr
        --  push [sliceend]     -- (opUnassigned)
        --  push [idxN]..[idx1] -- (opUnassigned)
        --  mov esi,[ref]       -- (opUnassigned)
        --  jmp :%pSubss        -- actually a call
        --<return address>

      ::pSubssWhile
            pop edi             -- next idx (ref)
--expect exception here for subscript errors...
          :!pSubsse94   -- exception here mapped to e94vhnbaav(edx)
            mov al,[ebx+esi*4-1]
            mov edx,[ebx+esi*4-12]  -- length
            shl esi,2
            sub edi,1               -- decrement edi (:-)
            test al,0x80
            jz :e04atsaa9       -- era @ [esp+ecx*4+4]

            sub ecx,1
            jz :pSubssEndWhile -- quit loop with startslice (aka idxN) ref in edi (may need fixupSliceStart)

            cmp edi,edx
            jb @f               -- unsigned jump, lets 0..len-1 through
                                --               (we just decremented edi)
--17/7/17:
                add ecx,2
                mov al,8+4+1        -- [era] is [esp+ecx*4+4], "reading from"
                call :%fixupIndex   -- idx-1 in edi, len in edx, al set
                mov al,byte[esi-1]  -- as we just trashed it
                sub ecx,2
          @@:
            cmp al,0x80
            jne @f
            mov esi,[esi+edi*4]
            jmp :pSubssWhile

          @@:
            cmp al,0x82
            jne :e04atsaap8         -- era @ [esp+ecx*4+8] [??+4??]
            lea esi,[esi+edi]       -- address of (first) char
            sub ecx,1
            jnz :e04atsaap12        -- strings must be last segment

      ::pSubssEndWhile
        cmp edi,edx
        jbe @f              -- unsigned jump, lets 0..len through (NB jbe here)
                            --               (we just decremented edi)
                            --               (slice start can be 1..length+1)
            mov cl,12               -- [era] @ [esp+12]
            call :%fixupSliceStart  -- idx in edi, len in edx
      @@:
-->     mov [ap1],edi           -- save slice start
--not: edx,edi,ecx,ebx,esi,eax...
--      mov ??,edi
--      pop edi                 -- ref slice end
        xchg [esp],edi          -- swap slice start/end
        cmp edi,edx
        jbe @f              -- unsigned jump, lets 0..len through (NB jbe here, and no dec edi)
                            --               (slice end can be 0..length)
--          mov cl,8            -- [era] @ [esp+8]
            mov cl,12           -- [era] @ [esp+12] (inc ret to @@:)
            call :%fixupSliceEnd    -- idx in edi, len in edx
      @@:

        -- stack contents:
        --  [esp] slice start (0-based)
        --  [esp+4] addr res
        --  [esp+8] return address
        mov ecx,edi             -- slice end
--      xor ebx,ebx
        mov edi,[esp+4]         -- address of res (re-fetched later)
-->     sub ecx,[ap1]           -- slice start
        sub ecx,[esp]           -- slice start
        jl :e09slinecx          -- slice length is negative
--      mov edi,[esp]           -- address of res (re-fetched later)
--      mov edi,[esp+4]         -- address of res (re-fetched later)
        mov edx,ecx             -- new object len = slice len
        mov edi,[edi]           -- ref of result (prev)
        cmp al,0x80
        jne :pSubssStr
        --
        -- can we do things in-situ?
        --
        cmp edi,h4
--      jle :pSubssp1leh4
--    ::pSubssp1leh4    -- ref p1 <= h4         (first test)
--killed p2js (25/5/21):
        jle :pSubssNewSeq
/*
        jg @f
            -- if new length = length(p2) then incref p2 -> p1, exit/all done
            cmp edx,[esi-12]
            jne :pSubssNewSeq
            -- (this is the [int/unassigned]:=x[1..$] case)
            add esp,4                   -- discard slice start (must be 0 anyway)
            pop edi                     -- addr tgt
            mov edx,[esi-8]             -- refcount
            lea eax,[esi+1]
            add edx,1
            ror eax,2                   -- == ref p2
            mov [esi-8],edx
            mov [edi],eax               -- (no need to dealloc)
            ret
      @@:
*/
        shl edi,2
        cmp dword[edi-8],1              -- refcount of 1
--      jne :pSubssp2rcn1
--    ::pSubssp2rcn1    -- refcount(esi/p2)!=1  (second test)
        jne :pSubssNewSeq
/*
        je @f
            cmp edi,esi
            jne :pSubssp2nep1
            cmp edx,[esi-12]
            jne :pSubssNewSeq
            -- (this must therefore be the x:=x[1..$] case, at least datawise, 
            --  although the hll could be say ti:=table[i][1..$], repeated)
--          pop edi                     -- [1] discard addr res
            add esp,8                   -- discard slice start and addr res
            ret
      @@:
*/
        cmp edi,esi
--      jne :pSubssp2nep1
        jne :pSubssNewSeq
/*
        je @f
          ::pSubssp2nep1    -- edi!=esi             (third test)
            cmp edx,[esi-12]                -- if lengths match
            jne :pSubssNewSeq
            add esp,4                       -- discard slice start (must be 0 anyway)
            pop edi                         -- addr tgt
            mov ecx,[esi-8]                 -- p2 refcount (or ref[idx1][idx2]..[idxn-1])
            lea eax,[esi+1]                 -- calc ref (for "") in eax
            add ecx,1
            mov edx,[edi]                   -- get prev ref
            mov [esi-8],ecx                 -- incref
            ror eax,2                       -- == ref p2
            mov ecx,[ebx+edx*4-8]           -- p1 refcount
            mov [edi],eax                   -- save new ref
            sub ecx,1
            jz :%pDealloc
            mov [ebx+edx*4-8],ecx           -- decref
            ret

      @@:
*/
        -- (this is the x:=x[i..j] case, with a refcount of 1)
        push ecx                        --[?] save new length
        xor ebx,ebx
-->     mov edx,[ap1]                   -- slice start (0-based)
        mov edx,[esp+4]                 -- slice start (0-based)
        mov ecx,[edi-12]                -- original length
        add edx,1
      ::pSubssDeallocPrevLoop
            sub edx,1
            jz :pSubssDeallocPrevDone
            mov eax,[esi]
            sub ecx,1
            add esi,4
            cmp eax,h4
            jle :pSubssDeallocPrevLoop
            sub dword[ebx+eax*4-8],1
            jnz :pSubssDeallocPrevLoop
            pushad
            mov edx,eax
--  if debugmem2
--  mov eax,[esp+40]    -- (DEV: untested!)
--  mov [dmFera],eax
--  end if
            push dword[esp+44]
            call :%pDealloc0
            popad
            jmp :pSubssDeallocPrevLoop

      ::pSubssDeallocPrevDone
        --
        -- re-site the header.
        --
        -- register contents:
        --  eax - unused
        --  ecx - length less slice start (needed below)
        --  edx - 0
        --  esi - location of new s[1]
        --  edi - raw addr of prev header
        --  [esp] = new length
        --  [esp+4] = slice start (0-based), aka discarded items
        --  [esp+8] = addr res
        --  [esp+12] = return address
        --
        cmp esi,edi
        jne :pSubssReSiteHeader
            -- must be x=x[1..??] case, no need to resite header:
            pop edx                     -- new length
            add esp,8                   -- discard slice start and addr res
            mov [esi-12],edx            -- set new length
            jmp :pSubssDeallocRest

      ::pSubssReSiteHeader
        mov eax,[edi-16]                -- prev maxlen
-->     mov edx,[ap1]                   -- discarded items
        mov edx,[esp+4]                 -- discarded items, aka slice start
        mov ebx,[edi-4]                 -- type (added 1/11/14)
        shl edx,2                       -- discarded items in bytes
        mov [esi-16],eax                -- maxlen (unaltered)
        mov eax,[edi-20]                -- previous slack
--      add edx,[edi-20]                -- previous slack
        mov [esi-4],ebx                 -- type (unaltered)
        add eax,edx                     -- new slack
        mov [esi-8],dword 1             -- refcount (unaltered)
        mov ebx,esi                     -- calculate new ref in ebx
--      pop dword[esi-12]               -- new length
        pop edx                         -- new length
        add esp,4                       -- discard slice start
        add ebx,1
        pop edi                         -- addr res
        mov [esi-20],eax                -- set new slack
        ror ebx,2
--      mov [esi-20],edx                -- set new slack
        mov [esi-12],edx                -- set new length
        mov [edi],ebx                   -- new ref of shifted header -> res
        xor ebx,ebx                 -- important!
      ::pSubssDeallocRest
        lea esi,[esi+edx*4]             -- esi := esi[newlength+1]

      ::pSubssDeallocRestLoop
        cmp ecx,edx
        je :pSubssDeallocRestDone
        mov eax,[esi]
        sub ecx,1
        add esi,4
        cmp eax,h4
        jle :pSubssDeallocRestLoop
        sub dword[ebx+eax*4-8],1
        jnz :pSubssDeallocRestLoop
        pushad
        mov edx,eax
--    if debugmem2
--      mov eax,[esp+32]
--      mov [dmFera],eax
--    end if
        push dword[esp+32]
        call :%pDealloc0
        popad
        jmp :pSubssDeallocRestLoop
        
--      nop?
      ::pSubssDeallocRestDone
        ret


      ::pSubssNewSeq
        -- register contents:
        --  al 0x80
        --  ecx = edx = new length
        --  esi = raw addr of p2
        --  edi = unknown (ref or raw tgt)
        --  [esp] = slice start (0-based)
        --  [esp+4] = addr res
--      xor ebx,ebx
--mov ecx,edx
--      mov edx,[esp]               -- slice start (0-based)
--      pop edx                     -- slice start (0-based)
        pop edi                     -- slice start (0-based)
        mov edx,[esp+8]             -- era
        call :%pAllocSeq            -- damages eax only
-->     mov edx,[ap1]               -- slice start
--      lea esi,[esi+edx*4]
        lea esi,[esi+edi*4]
        push eax                    --[?] save ref
        test ecx,ecx
        je :pSubssStore
        lea edi,[ebx+eax*4]
      @@:
            lodsd                   -- mov eax,[esi], esi+=4
            stosd                   -- mov [edi],eax, edi+=4
            cmp eax,h4
            jl :Subss2_no_incref
                add dword[ebx+eax*4-8],1
          ::Subss2_no_incref
            sub ecx,1
            jnz @b
        nop
        jmp :pSubssStore

      ::pSubssStr
        -- register contents:
        --  al 0x82
        --  ecx = edx = new length
        --  esi = raw addr of p2
        --  edi = ref p1
        --  [esp] = slice start (post-fixup, zero-based)
        --  [esp+4] = addr res
        --  [esp+8] = return address
        -- insitu if: slice starts at 1, and refcount p2 is 1.
        -- jumps to @f(AllocStr) must preserve/reset edx(=ecx),ecx,esi.
        cmp al,0x82                     -- sanity check
        jne :e04atsaa4
        mov eax,[esi-8]         -- refcount
--      mov edx,[ap1]           -- slice start (post-fixup/0-based)
        mov edx,[esp]           -- slice start (post-fixup/0-based)
        cmp eax,1
--      jne @f                  -- refcount!=1 (==>AllocStr)
        jne :pSubssNewStr       -- refcount!=1 (==>AllocStr)
        test edx,edx
--      jnz @f                  -- slice start is not 1 (==>AllocStr)
        jnz :pSubssNewStr       -- slice start is not 1 (==>AllocStr)
        cmp edi,h4
--      jg pSubssStrp1gh4
        jg @f
            -- p1 unassigned/integer.
            -- can still just copy ref iff edx==length(p2) (eg x=s[1..-1])
            cmp ecx,[esi-12]
--          jne @f              -- (==>AllocStr)
            jne :pSubssNewStr
            lea eax,[esi+1]
            add esp,4           -- discard slice start (which is 1)
            pop edi             -- addr tgt
            ror eax,2           -- == ref p2 (or ref[idx1][idx2]...[idxn-1])
            mov dword[esi-8],2  -- refcount is now 2
            mov [edi],eax
            ret
--    ::pSubssStrp1gh4
      @@:
        lea eax,[ebx+edi*4]
    --  mov edx,edi             -- in case dealloc needed
    --  cmp edi,esi
        cmp eax,esi
--      jne pSubssStrp1nep2
        jne @f
            -- s=s[1..?], no incref/decref needed.
            mov [esi-12],ecx    -- set new length
--          pop edi             -- discard addr tgt
            add esp,8           -- discard slice start and addr tgt
            mov byte[esi+ecx],0 -- plant new terminator
            ret
--    ::pSubssStrp1nep2
      @@:
            -- p1!=p2.
            -- again, we can still just copy ref p2 & deref p1, iff ecx==length(p2).
            --  ie/eg a=b[1..-1] can just incref b, but a=b[1..-2] needs to
            --       end up with two different length strings in a & b.
            cmp ecx,[esi-12]
--          jne @f              -- (==>AllocStr)
            jne :pSubssNewStr
            mov edx,edi         -- for dealloc
            mov dword[esi-8],2  -- set new refcount
            add esp,4           -- discard slice start
            pop edi             -- addr tgt
            add esi,1
            mov ecx,[eax-8]
            ror esi,2           -- recreated ref p2 (or ref[idx][idx2]...[idxn-1])
            sub ecx,1
            mov [edi],esi
            jz :%pDealloc
            mov [eax-8],ecx
            ret
--    @@:
      ::pSubssNewStr
--      mov edx,ecx
        pop edx                 -- slice start (post-fixup/0-based)
--      mov edx,[esp]           -- slice start (post-fixup/0-based)
        call :%pAllocStr        -- damages eax only
--      mov edx,[ap1]           -- slice start (post-fixup/0-based)
--      mov edx,[esp]           -- slice start (post-fixup/0-based)
--      xor ebx,ebx
--      lea esi,[esi+edx]
        add esi,edx
        push eax                --[2] save ref
        lea edi,[ebx+eax*4]
--      or ecx,ecx
--      je @f
        rep movsb
--    @@:
        xor eax,eax
        stosb                   -- trailing null

      ::pSubssStore
        pop eax                 --[2] newly created ref
--      add esp,4               -- discard slice start
        pop edi                 -- addr result
        mov edx,[edi]
        mov [edi],eax
        cmp edx,h4
        jle @f
            sub dword[ebx+edx*4-8],1
            jz :%pDealloc
      @@:
        ret
    [64]
        --calling convention:
        -- <perform any required opUnassigned's>
        --  mov rcx,N
        --  push <return address>
        --  push res            -- result var addr
        --  push [sliceend]     -- (opUnassigned)
        --  push [idxN]..[idx1] -- (opUnassigned)
        --  mov rsi,[ref]       -- (opUnassigned)
        --  jmp :%pSubss        -- actually a call
        --<return address>

      ::pSubssWhile
            pop rdi             -- next idx (ref)
--expect exception here for subscript errors...
          :!pSubsse94   -- exception here mapped to e94vhnbaav(rdx)
            mov al,[rbx+rsi*4-1]
            mov rdx,[rbx+rsi*4-24]  -- length
            shl rsi,2
            sub rdi,1               -- decrement rdi (:-)
            test al,0x80
            jz :e04atsaa9       -- era @ [rsp+rcx*8+8]

            sub rcx,1
            jz :pSubssEndWhile -- quit loop with startslice (aka idxN) ref in rdi (may need fixupSliceStart)

            cmp rdi,rdx
            jb @f               -- unsigned jump, lets 0..len-1 through
                                --               (we just decremented rdi)
--17/7/17:
                add rcx,2
                mov al,8+4+1        -- [era] is [rsp+rcx*8+8], "reading from"
                call :%fixupIndex   -- idx-1 in rdi, len in rdx, al set
                mov al,byte[rsi-1]  -- as we just trashed it
                sub rcx,2
          @@:
            cmp al,0x80
            jne @f
            mov rsi,[rsi+rdi*8]
            jmp :pSubssWhile

          @@:
            cmp al,0x82
            jne :e04atsaap8         -- era @ [rsp+rcx*8+8] [??]
            lea rsi,[rsi+rdi]       -- address of (first) char
            sub rcx,1
            jnz :e04atsaap12        -- strings must be last segment

      ::pSubssEndWhile
        cmp rdi,rdx
        jbe @f              -- unsigned jump, lets 0..len through (NB jbe here)
                            --               (we just decremented rdi)
                            --               (slice start can be 1..length+1)
--          mov cl,12               -- [era] @ [esp+12]
            mov cl,24               -- [era] @ [rsp+24]
            call :%fixupSliceStart  -- idx in rdi, len in rdx
      @@:
-->     mov [ap1],edi           -- save slice start
--not: edx,edi,ecx,ebx,esi,eax...
--      mov ??,edi
--      pop edi                 -- ref slice end
        xchg [rsp],rdi          -- swap slice start/end
        mov r15,h4
        cmp rdi,rdx
        jbe @f              -- unsigned jump, lets 0..len through (NB jbe here, and no dec rdi)
                            --               (slice end can be 0..length)
--          mov cl,8            -- [era] @ [esp+8]
            mov cl,24           -- [era] @ [rsp+24]  (inc ret to @@:)
            call :%fixupSliceEnd    -- idx in rdi, len in rdx
      @@:

        -- stack contents:
        --  [rsp] slice start (0-based)
        --  [rsp+8] addr res
        --  [rsp+16] return address
        mov rcx,rdi             -- slice end
        mov rdi,[rsp+8]         -- address of res (re-fetched later)
        sub rcx,[rsp]           -- slice start
        jl :e09slinecx          -- slice length is negative
        mov rdx,rcx             -- new object len = slice len
        mov rdi,[rdi]           -- ref of result (prev)
        cmp al,0x80
        jne :pSubssStr
        --
        -- can we do things in-situ?
        --
        cmp rdi,r15
--killed p2js (25/5/21):
        jle :pSubssNewSeq
--/*
        jg @f
            -- if new length = length(p2) then incref p2 -> p1, exit/all done
            cmp rdx,[rsi-24]
            jne :pSubssNewSeq
            -- (this is the [int/unassigned]:=x[1..$] case)
            add rsp,8                   -- discard slice start (must be 0 anyway)
            pop rdi                     -- addr tgt
            mov rdx,[rsi-16]            -- refcount
            lea rax,[rsi+1]             -- (recreate ref)
            add rdx,1
            ror rax,2                   -- == ref p2
            mov [rsi-16],rdx
            mov [rdi],rax               -- (no need to dealloc)
            ret
      @@:
--*/
        shl rdi,2
        cmp qword[rdi-16],1             -- refcount of 1
        jne :pSubssNewSeq
/*
        je @f
            cmp rdi,rsi
            jne :pSubssp2nep1
            cmp rdx,[rsi-24]
            jne :pSubssNewSeq
            -- (this must therefore be the x:=x[1..$] case, at least datawise, 
            --  although the hll could be say ti:=table[i][1..$], repeated)
            add rsp,16                  -- discard slice start and addr res
            ret
      @@:
*/
        cmp rdi,rsi
        jne :pSubssNewSeq
/*
        je @f
          ::pSubssp2nep1    -- rdi!=rsi             (third test)
            cmp rdx,[rsi-24]                -- if lengths match
            jne :pSubssNewSeq
            add rsp,8                       -- discard slice start (must be 0 anyway)
            pop rdi                         -- addr tgt
            mov rcx,[rsi-16]                -- p2 refcount (or ref[idx1][idx2]..[idxn-1])
            lea rax,[rsi+1]                 -- calc ref (for "") in eax
            add rcx,1
            mov rdx,[rdi]                   -- get prev ref
            mov [rsi-16],rcx                -- incref
            ror rax,2                       -- == ref p2
            mov rcx,[rbx+rdx*4-16]          -- p1 refcount
            mov [rdi],rax                   -- save new ref
            sub rcx,1
            jz :%pDealloc
            mov [rbx+rdx*4-16],rcx          -- decref
            ret

      @@:
*/
        -- (this is the x:=x[i..j] case, with a refcount of 1)
        push rcx                        --[?] save new length
        xor rbx,rbx
        mov rdx,[rsp+8]                 -- slice start (0-based)
        mov rcx,[rdi-24]                -- original length
        add rdx,1
      ::pSubssDeallocPrevLoop
            sub rdx,1
            jz :pSubssDeallocPrevDone
            mov rax,[rsi]
            sub rcx,1
            add rsi,8
            cmp rax,r15
            jle :pSubssDeallocPrevLoop
            sub qword[rbx+rax*4-16],1
            jnz :pSubssDeallocPrevLoop
--          pushad
            push rdi
            push rcx
            push rsi
            push rdx
            mov rdx,rax
--  if debugmem2
--  mov eax,[esp+40]    -- (DEV: untested!)
--  mov [dmFera],eax
--  end if
            push qword[rsp+56]
            call :%pDealloc0
--          popad
            pop rdx
            pop rsi
            pop rcx
            pop rdi
            jmp :pSubssDeallocPrevLoop

      ::pSubssDeallocPrevDone
        --
        -- re-site the header.
        --
        -- register contents:
        --  rax - unused
        --  rcx - length less slice start (needed below)
        --  rdx - 0
        --  rsi - location of new s[1]
        --  rdi - raw addr of prev header
        --  [rsp] = new length
        --  [rsp+8] = slice start (0-based), aka discarded items
        --  [rsp+16] = addr res
        --  [rsp+24] = return address
        --
        cmp rsi,rdi
        jne :pSubssReSiteHeader
            -- must be x=x[1..??] case, no need to resite header:
            pop rdx                     -- new length
            add rsp,16                  -- discard slice start and addr res
            mov [rsi-24],rdx            -- set new length
            jmp :pSubssDeallocRest

      ::pSubssReSiteHeader
        mov rax,[rdi-32]                -- prev maxlen
        mov rdx,[rsp+8]                 -- discarded items, aka slice start
        mov r9,[rdi-8]                  -- previous type
        shl rdx,3                       -- discarded items in bytes
        mov [rsi-32],rax                -- maxlen (unaltered)
        mov rax,[rdi-40]                -- previous slack
--      add rdx,[rdi-40]                -- previous slack
        mov qword[rsi-8],r9             -- type (unaltered)
        add rax,rdx                     -- new slack
        mov [rsi-16],qword 1            -- refcount (unaltered)
        mov r9,rsi                      -- calculate new ref in r9
--      pop qword[rsi-24]               -- new length
        pop rdx                         -- new length
        add rsp,8                       -- discard slice start
        add r9,1
        pop rdi                         -- addr res
        mov [rsi-40],rax                -- set new slack
        ror r9,2
--      mov [rsi-40],rdx                -- set new slack
        mov [rsi-24],rdx                -- set new length
        mov [rdi],r9                    -- new ref of shifted header -> res
      ::pSubssDeallocRest
        lea rsi,[rsi+rdx*8]             -- rsi := rsi[newlength+1]

      ::pSubssDeallocRestLoop
        cmp rcx,rdx
        je :pSubssDeallocRestDone
        mov rax,[rsi]
        sub rcx,1
--29/12/15:
--      add rsi,4
        add rsi,8
        cmp rax,r15
        jle :pSubssDeallocRestLoop
        sub qword[rbx+rax*4-16],1
        jnz :pSubssDeallocRestLoop
--      pushad
        push rcx
        push rdx
        push rsi
        mov rdx,rax
        push qword[rsp+24]
        call :%pDealloc0
--      popad
        pop rsi
        pop rdx
        pop rcx     
        jmp :pSubssDeallocRestLoop
        
--      nop?
      ::pSubssDeallocRestDone
        ret


      ::pSubssNewSeq
        -- register contents:
        --  al 0x80
        --  rcx = rdx = new length
        --  rsi = raw addr of p2
        --  rdi = unknown (ref or raw tgt)
        --  [rsp] = slice start (0-based)
        --  [rsp+8] = addr res
--      pop rdx                     -- slice start (0-based)
        pop rdi                     -- slice start (0-based)
        mov rdx,[rsp+16]            -- era
        call :%pAllocSeq            -- damages rax only
--      lea rsi,[rsi+rdx*4]
--      lea rsi,[rsi+rdx*8]
        lea rsi,[rsi+rdi*8]
        push rax                    --[?] save ref
        test rcx,rcx
        je :pSubssStore
        lea rdi,[rbx+rax*4]
      @@:
            lodsq                   -- mov rax,[rsi], rsi+=4
            stosq                   -- mov [rdi],rax, rdi+=4
            cmp rax,r15
            jl :Subss2_no_incref
                add qword[rbx+rax*4-16],1
          ::Subss2_no_incref
            sub rcx,1
            jnz @b
        nop
        jmp :pSubssStore

      ::pSubssStr
        -- register contents:
        --  al 0x82
        --  rcx = rdx = new length
        --  rsi = raw addr of p2
        --  rdi = ref p1
        --  [rsp] = slice start (post-fixup, zero-based)
        --  [rsp+8] = addr res
        --  [rsp+16] = return address
        -- insitu if: slice starts at 1, and refcount p2 is 1.
        -- jumps to @f(AllocStr) must preserve/reset rdx(=rcx),rcx,rsi.
        cmp al,0x82                     -- sanity check
        jne :e04atsaa4
        mov rax,[rsi-16]        -- refcount
        mov rdx,[rsp]           -- slice start (post-fixup/0-based)
        cmp rax,1
        jne :pSubssNewStr       -- refcount!=1 (==>AllocStr)
        test rdx,rdx
        jnz :pSubssNewStr       -- slice start is not 1 (==>AllocStr)
        cmp rdi,r15
        jg @f
            -- p1 unassigned/integer.
            -- can still just copy ref iff rdx==length(p2) (eg x=s[1..-1])
            cmp rcx,[rsi-24]
            jne :pSubssNewStr
            lea rax,[rsi+1]
            add rsp,8           -- discard slice start (which is 1)
            pop rdi             -- addr tgt
            ror rax,2           -- == ref p2 (or ref[idx1][idx2]...[idxn-1])
            mov qword[rsi-16],2 -- refcount is now 2
            mov [rdi],rax
            ret
      @@:
        lea rax,[rbx+rdi*4]
        cmp rax,rsi
        jne @f
            -- s=s[1..?], no incref/decref needed.
            mov [rsi-24],rcx    -- set new length
            add rsp,16          -- discard slice start and addr tgt
            mov byte[rsi+rcx],0 -- plant new terminator
            ret
      @@:
            -- p1!=p2.
            -- again, we can still just copy ref p2 & deref p1, iff ecx==length(p2).
            --  ie/eg a=b[1..-1] can just incref b, but a=b[1..-2] needs to
            --       end up with two different length strings in a & b.
            cmp rcx,[rsi-24]
            jne :pSubssNewStr
            mov rdx,rdi         -- for dealloc
            mov qword[rsi-16],2 -- set new refcount
            add rsp,8           -- discard slice start
            pop rdi             -- addr tgt
            add rsi,1
            mov rcx,[rax-16]
            ror rsi,2           -- recreated ref p2 (or ref[idx][idx2]...[idxn-1])
            sub rcx,1
            mov [rdi],rsi
            jz :%pDealloc
            mov [rax-16],rcx
            ret

      ::pSubssNewStr
        pop rdx                 -- slice start (post-fixup/0-based)
        call :%pAllocStr        -- damages rax only
        add rsi,rdx
        push rax                --[2] save ref
        lea rdi,[rbx+rax*4]
        rep movsb
        xor rax,rax
        stosb                   -- trailing null

      ::pSubssStore
        pop rax                 --[2] newly created ref
        pop rdi                 -- addr result
        mov rdx,[rdi]
        mov [rdi],rax
        cmp rdx,r15
        jle @f
            sub qword[rbx+rdx*4-16],1
            jz :%pDealloc
      @@:
        ret
    []

--/*
procedure :%pSubsss(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pSubsss       -- res := string[slicestart..sliceend]
-------------
    [32]
        --calling convention:
        --  lea eax,[res]
        --  mov edi,[p2]        -- slice start (opUnassigned)
        --  mov ecx,[p3]        -- slice end (opUnassigned)
        --  mov esi,[p1]        -- ref (opUnassigned, must be a string)
--X     --  mov edx,p1          -- var no of ref            (DEV deliberately commented out in pilx86.e, as we opUnassign)
        --  call pSubsss        -- [eax]=esi[edi..ecx]
--  ::pSubssse92a       -- exception here mapped to e94vhnbaavedxesifeh
        mov edx,[ebx+esi*4-12]  -- length   (exception here mapped to e94vhnbaav(edx))
        sub edi,1               -- decrement edi (:-)
        shl esi,2
        cmp edi,edx
        jbe @f              -- unsigned jump, lets 0..len through (NB jbe here)
                            --               (we just decremented edi)
                            --               (slice start can be 1..length+1)
--          mov cl,0        -- [era] @ [esp]
            push ecx
            mov cl,8        -- [era] @ [esp+8] (inc ret to @@:)
            call :%fixupSliceStart  -- idx in edi, len in edx
            pop ecx
      @@:
--      push edi            -- save slice start
--!     mov edx,edi             -- save slice start
        cmp ecx,edx
        jbe @f              -- unsigned jump, lets 0..len through (NB jbe here, and no dec ecx)
                            --               (slice end can be 0..length)
            push edi            -- save slice start
            mov edi,ecx
            mov cl,8            -- [era] @ [esp+8] (inc ret to @@:)
            call :%fixupSliceEnd    -- idx in edi, len in edx
            mov ecx,edi
--          mov edi,[esp]
            pop edi
      @@:
        sub ecx,edi             -- slice length
--      jl :e09slinecx2         -- slice length is negative
        jge @f
            add ecx,edi
            add edi,1
            pop edx
            mov al,9    -- e09slin(edi,ecx)
            sub edx,1
            jmp :!iDiag
            int3
      @@:
        push eax                -- save addr res
--?     mov edx,ecx             -- new object len = slice len
        mov edx,edi             -- save slice start
        mov edi,[eax]           -- ref of result
        -- register contents:
        --  ecx = new length
        --  edx = slice start (0-based)
        --  esi = raw addr of p2
        --  edi = ref p1 (res)
        --  [esp] = addr res
--X     --  [esp+4] = slice start (post-fixup, zero-based)
        -- insitu if: slice starts at 1, and refcount p2 is 1.
        -- jumps to @f(AllocStr) must preserve/reset ecx,esi.
--      mov eax,[esi-8]         -- refcount
--      mov edx,[esp+4]         -- slice start (post-fixup/0-based)
--      cmp eax,1
        cmp dword[esi-8],1
        jne :pSubsssNewStr      -- refcount!=1 (==>AllocStr)
--      test edx,edx
        cmp edx,0
        jnz :pSubsssNewStr      -- slice start is not 1 (==>AllocStr)
        cmp edi,h4
        jg @f
            -- p1 unassigned/integer.
            -- can still just copy ref iff ecx==length(p2) (eg x=s[1..-1])
            cmp ecx,[esi-12]
            jne :pSubsssNewStr
            lea eax,[esi+1]
            pop edi             -- addr tgt
            ror eax,2           -- == ref p2 (or ref[idx1][idx2]...[idxn-1])
            mov dword[esi-8],2  -- refcount is now 2
            mov [edi],eax
            ret
      @@:
        lea eax,[ebx+edi*4]     -- raw tgt
        cmp eax,esi
        jne @f
            -- s=s[1..?], no incref/decref needed.
            mov [esi-12],ecx    -- set new length
            add esp,4           -- discard addr tgt
            mov byte[esi+ecx],0 -- plant new terminator
            ret
      @@:
        -- p1!=p2.
        -- again, we can still just copy ref p2 & deref p1, iff ecx==length(p2).
        --  ie/eg a=b[1..-1] can just incref b, but a=b[1..-2] needs to
        --       end up with two different length strings in a & b.
        cmp ecx,[esi-12]
        jne :pSubsssNewStr
        mov edx,edi         -- for dealloc
        mov dword[esi-8],2  -- set new refcount (we know it was 1)
        pop edi             -- addr tgt
        add esi,1
        mov ecx,[eax-8]     -- p1 refcount
        ror esi,2           -- recreated ref p2
        sub ecx,1
        mov [edi],esi
        jz :%pDealloc
        mov [eax-8],ecx
        ret

      ::pSubsssNewStr
    --  mov edx,ecx
    --DEV: [1..-1] could just incref...
    --DEV 21/1/09 that code above belongs here...   [ 28/1/09 fixed/rewritten above ]
--      pop edx                 -- slice start (post-fixup/0-based)
        call :%pAllocStr        -- damages eax only
    --  mov edx,[esp+4]         -- slice start (post-fixup/0-based)
    --  xor ebx,ebx
    --  lea esi,[esi+edx]
        add esi,edx             -- raw(p2) + slice start (0-based)
        push eax                --[2] save ref
        lea edi,[ebx+eax*4]
    --DEV do we need this? [NO, I just tested]
    --  test ecx,ecx
    --  je @f
        rep movsb
    --  @@:
        xor eax,eax
        stosb                   -- trailing null

    --  ::pSubsssStore
        pop eax                 --[2] newly created ref
        pop edi                 -- addr result
        mov edx,[edi]
        mov [edi],eax
        cmp edx,h4
        jle @f
            sub dword[ebx+edx*4-8],1
            jz :%pDealloc
      @@:
        ret
    [64]
        --calling convention:
        --  lea rax,[res]
        --  mov rdi,[p2]        -- slice start (opUnassigned)
        --  mov rcx,[p3]        -- slice end (opUnassigned)
        --  mov rsi,[p1]        -- ref (opUnassigned)
        --  mov rdx,p1
        --  call :%pSubsss      -- [rax]=rsi[rdi..rcx] (where esi is a string)
        mov rdx,[rbx+rsi*4-24]  -- length (exceptiuon here mapped to e94vhnbaav(rdx))
        sub rdi,1               -- decrement rdi (:-)
        shl rsi,2
        mov r15,h4
        cmp rdi,rdx
        jbe @f              -- unsigned jump, lets 0..len through (NB jbe here)
                            --               (we just decremented rdi)
                            --               (slice start can be 1..length+1)
            push rcx
--          mov cl,0        -- [era] @ [esp]
            mov cl,16       -- [era] @ [esp+16] (inc ret to @@:)
            call :%fixupSliceStart  -- idx in rdi, len in rdx
            pop rcx
      @@:
        cmp rcx,rdx
        jbe @f              -- unsigned jump, lets 0..len through (NB jbe here, and no dec rcx)
                            --               (slice end can be 0..length)
            push rdi            -- save slice start
            mov rdi,rcx
--          mov cl,8            -- [era] @ [rsp+8]
            mov cl,16           -- [era] @ [rsp+16] (inc ret to @@:)
            call :%fixupSliceEnd    -- idx in rdi, len in rdx
            mov rcx,rdi
            pop rdi
      @@:
        sub rcx,rdi             -- slice length
--      jl :e09slinecx2         -- slice length is negative
        jge @f
            add rcx,rdi
            add rdi,1
            pop rdx
            mov al,9    -- e09slin(rdi,rcx)
            sub rdx,1
            jmp :!iDiag
            int3
      @@:
        push rax                -- save addr res
        mov rdx,rdi             -- save slice start
        mov rdi,[rax]           -- ref of result
        -- register contents:
        --  rcx = new length
        --  rdx = slice start (0-based)
        --  rsi = raw addr of p2 (a string)
        --  rdi = ref p1 (res)
        --  [rsp] = addr res
        -- insitu if: slice starts at 1, and refcount p2 is 1.
        -- jumps to @f(AllocStr) must preserve/reset rcx,rsi.
        cmp dword[rsi-16],1
        jne :pSubsssNewStr      -- refcount!=1 (==>AllocStr)
        cmp rdx,0
        jnz :pSubsssNewStr      -- slice start is not 1 (==>AllocStr)
        cmp rdi,r15
        jg @f
            -- p1 unassigned/integer.
            -- can still just copy ref iff ecx==length(p2) (eg x=s[1..-1])
            cmp rcx,[rsi-24]
            jne :pSubsssNewStr
            lea rax,[rsi+1]
            pop rdi             -- addr tgt
            ror rax,2           -- == ref p2 (or ref[idx1][idx2]...[idxn-1])
            mov dword[rsi-16],2 -- refcount is now 2
            mov [rdi],rax
            ret
      @@:
        lea rax,[rbx+rdi*4]     -- raw tgt
        cmp rax,rsi
        jne @f
            -- s=s[1..?], no incref/decref needed.
            mov [rsi-24],rcx    -- set new length
--26/1/15:
--          add rsp,4           -- discard addr tgt
            add rsp,8           -- discard addr tgt
            mov byte[rsi+rcx],0 -- plant new terminator
            ret
      @@:
        -- p1!=p2.
        -- again, we can still just copy ref p2 & deref p1, iff ecx==length(p2).
        --  ie/eg a=b[1..-1] can just incref b, but a=b[1..-2] needs to
        --       end up with two different length strings in a & b.
        cmp rcx,[rsi-24]
        jne :pSubsssNewStr
        mov rdx,rdi         -- for dealloc
        mov qword[rsi-16],2 -- set new refcount (we know it was 1)
        pop rdi             -- addr tgt
        add rsi,1
        mov rcx,[rax-16]    -- p1 refcount
        ror rsi,2           -- recreated ref p2
        sub rcx,1
        mov [rdi],rsi
        jz :%pDealloc
        mov [rax-16],rcx
        ret

      ::pSubsssNewStr
        call :%pAllocStr        -- damages rax only
        add rsi,rdx             -- raw(p2) + slice start (0-based)
        push rax                --[2] save ref
        lea rdi,[rbx+rax*4]
        rep movsb
        xor rax,rax
        stosb                   -- trailing null

    --  ::pSubsssStore
        pop rax                 --[2] newly created ref
        pop rdi                 -- addr result
        mov rdx,[rdi]
        mov [rdi],rax
        cmp rdx,r15
        jle @f
            sub qword[rbx+rdx*4-16],1
            jz :%pDealloc
      @@:
        ret
    []
      }

