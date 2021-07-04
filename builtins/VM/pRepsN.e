--
-- pRepsN.e
-- ========
--
--  The replace slice routine:
--
--      :%pReps             -- [eax][idx1]..[idxN..sliceend]:=rep
--
-- (There may be a case for some optimised forms, especially string[slicestart..sliceend]:=string and
--  possibly string[slicestart..sliceend]:=char, but I stongly doubt that pReps1 (similar to pRepe1)
--  would deliver any measurable gain, but /would/ be a significant size and maintenance overhead.
--  The first step would be to plant puts(1,"xxx") statements in pilx86.e to show when/if it can use
--  a new pRepss/pRepsch, before bothering to write them.)
--
-- Technical note:
--  This is perhaps the most complex/ctritical part of the VM (well, it is either this or pHeap.e).
--  It, alone, implements variable length slice assignment (eg "food"[2..3]:="e" -> "fed") as well
--  as over half (rest in pRepeN.e) of the auto-expansion of strings to dword-sequences. I should
--  note that it is extremely easy to study this for 10 minutes and then gleefully modify completely
--  the wrong block of code, or at least that has been my sad and sorry experience with it. ;-)
--  Throughout this source I have liberally scattered "current state of registers and stack" comments,
--  (5 registers and 7 stack entries) because without them this code would be absolutely impossible 
--  to write/maintain. However, they are extremely difficult to keep up to date, not least because it
--  is the same thing over and over again, but with ever-so-subtle differences, and should always be 
--  treated with deep suspicion. They are most useful not when reading the source, but when single-
--  stepping though the instructions in OllyDbg or similar. Please fix any slip-ups that you spot.
--
-- TODO:
--  check/add tests (t24slice) for eg 'x[1][2.2..5.9] = expr' giving the same results as 'x[1][2..5] = expr'.
--  lots of tests in terror

include builtins\VM\pHeap.e     -- :%pDealloc, :%pAllocStr, :%pAllocSeq

include builtins\VM\pFixup.e    -- negative and floating point index handling (:%fixupIndex)

#ilASM{ jmp :%opRetf

--DEV FIXME: (and the :!bang labels below)
--  ::e04atsaam4
----            lea esp,[esp+ecx*4-4]
--      [32]
--          mov esi,[esp+ecx*4-4]   -- era
--      [64]
----            mov rsi,[rsp+rcx*8-8]   -- era (DEV untested!)
--          mov rdx,[rsp+rcx*8+8]   -- era (DEV untested!)
--          sub rdx,1
--      []
--          mov al,4
--          jmp :!iDiag
--          int3
--  ::e04atsaa4
--          int3
    ::e04atsaa9
        [32]
            mov esi,[esp+ecx*4+4]   -- era
        [64]
            mov rsi,[rsp+rcx*8+8]   -- era (DEV untested!)
        []
            mov al,4
--          jmp :!iDiag
            int3
    ::e09slinespp4
        [32]
            mov edx,[esp+16]        -- era
            mov ecx,[esp]           -- (edi already set)
            mov al,9                -- e09slin(edi,ecx)
            sub edx,1
        [64]
            mov rdx,[rsp+32]        -- era
            mov rcx,[rsp]           -- (rdi already set)
            mov al,9                -- e09slin(rdi,rcx)
            sub rdx,1
        []
            jmp :!iDiag
            int3
    ::e04atsaa24
            int3
--/*
procedure :%pReps(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pReps         -- [eax][idx1]..[idxN..sliceend]:=rep
-----------
    [32]
        --calling convention:
        --  mov ecx,N
        --  push <return address>
        --  push [rep]          -- replacement (opUnassigned)
        --  push [sliceend]     -- sliceend (opUnassigned)
        --  push [idxN]..[idx1] -- (opUnassigned)
        --  lea eax,[ref]       -- ref addr
        --  jmp opReps          -- actually a call
        --<return address>

      ::opRepsRnxt
        mov esi,[eax]
        pop edi                 -- next idx (ref)
        cmp esi,h4
--      jl :e04atsaam4          -- attempt to subscript an atom, era @ [esp+ecx*4-4]?
        jg @f
            mov edx,[esp+ecx*4+4]   -- era
            mov al,4                -- e04atsaam4
            sub edx,1
            jmp :!iDiag
            int3
      @@:
        push eax                --[1] ref addr, in case we need to clone...
        mov al,[ebx+esi*4-1]
        sub edi,1               -- decrement edi (:-)
        mov edx,[ebx+esi*4-12]  -- length
        shl esi,2

        sub ecx,1
        je :opRepsMain          -- deal with final slice separately
        cmp al,0x80
        jne :e04atsaa9          -- must be seq since another idx (or the slice) follows, era * [esp+ecx*4+4]?

        cmp edi,edx
        jb @f                   -- unsigned jump, lets 0..len-1 through
                                --               (we just decremented edi)
--DEV +8..
            mov al,8+4+0        -- [era] @ [esp+ecx*4+4], "assigning to"
            call :%fixupIndex   -- idx-1 in edi, len in edx, al set
      @@:
        --
        -- edi now contains index to replace, and edx the length
        --   
        mov eax,[esi-8]         -- refcount
        sub eax,1
        jz :opRepsSeqNoClone
        mov [esi-8],eax         -- non-1 so no need to dealloc
        mov eax,[esp+ecx*4+4]   -- era
        push ecx                --[2] no of remainding indexes
        push edi                --[3] idx
        mov ecx,edx
        mov edi,[esp+8]         --[1] (ref addr, leaving it on the stack)
        mov edx,eax             -- era
        call :%pAlloClone
        call :%pAllocSeq        -- damages eax only
        mov [edi],eax           -- Replace ref at original address [no dealloc rqd]
        lea edi,[ebx+eax*4]
        push edi                --[4]
      @@:
            lodsd               -- mov eax,[esi], esi+=4
            stosd               -- mov [edi],eax, edi+=4
            cmp eax,h4
            jl :opReps_no_incref
                add dword[ebx+eax*4-8],1
          ::opReps_no_incref

            sub ecx,1
            jnz @b

        pop esi                 --[4] NB esi:=edi
        pop edi                 --[3] idx
        pop ecx                 --[2] no of remaining indexes
      ::opRepsSeqNoClone
        add esp,4               --[1] discard ref addr
        lea eax,[esi+edi*4]
        jmp :opRepsRnxt

      ::opRepsMain
------------------
        -- al type byte
        -- ecx 0
        -- edx length ref
        -- esi raw ref
        -- edi slice start (-1, pre fixup)
        -- stack contents:
        --  [esp]   ref addr (after several subcripts)
        --  [esp+4] slice end (pre fixup)
        --  [esp+8] rep ref
        --  [esp+12] <return address>
        test al,0x80
--      jz :e04atsaa4
        jnz @f
            mov edx,[esp+12]
            mov al,4    -- e04atasaa
            sub edx,1
            jmp :!iDiag
            int3
      @@:
        cmp edi,edx
        jbe @f                  -- unsigned jump, lets 0..len through (NB jbe here)
                                --               (we just decremented edi)
                                --               (slice start can be 1..length+1)
--DEV +12
--          mov cl,8                -- [era] @ [esp+8]
            mov cl,16               -- [era] @ [esp+8]
            call :%fixupSliceStart  -- idx in edi, len in edx
      @@:
        push edi                -- save slice start (0-based)
        mov edi,[esp+8]         -- slice end
        cmp edi,edx
        jbe @f                  -- unsigned jump, lets 0..len through (NB jbe here, and no dec edi)
                                --               (slice end can be 0..length)
--DEV 16
--          mov cl,12               -- [era] @ [esp+12]
            mov cl,20               -- [era] @ [esp+20]
            call :%fixupSliceEnd    -- idx in edi, len in edx, idx addr in ebx
            mov [esp+8],edi         -- save normalised slice end (1-based)
      @@:
        mov ecx,edi
        sub ecx,[esp]
        jl :e09slinespp4        -- slice length is negative (%d..%d)
        push ecx                -- save slice length
        push edx                -- save source (ref) length
        mov edi,[esp+20]        -- rep ref
        --
        --  al: type byte ([esi-1])
        --  ecx: slice length [also in esp+4]
        --  edx: ref length [also in esp]
        --  edi: rep ref
        --  esi: raw(ref)
        --  [esp] ref length
        --  [esp+4] slice length
        --  [esp+8] slice start (0-based)
        --  [esp+12] ref addr
        --  [esp+16] slice end (1-based)
        --  [esp+20] rep ref
        --  [esp+24] return addr
        --
        -- handle sequence and string slices separately...
        --
        cmp al,0x80
        jne :opRepsStr
            --
            -- check for insitu replacement
            --
            cmp dword[esi-8],1
            jne :opRepsSeqMultiRef
            --
            -- which is ok for atoms...
            --
            cmp edi,h4
            jl @f
                lea eax,[ebx+edi*4]
                test byte[ebx+edi*4-1],0x80
                jnz :opRepsSeqSeq
                add dword[ebx+edi*4-8],ecx  -- bulk ref update (float)
          @@:                       -- replacement is an atom
            mov edx,[esp+8]         -- slice start
            cmp ecx,0               -- check for zero length slice replacement
            je :opRepsPop6          -- do nothing case
            mov eax,edi
--          lea edi,[esi+edx*4-4]
            lea edi,[esi+edx*4]
          ::opRepsSeqAtomLoop
            mov edx,[edi]
            cmp edx,h4
            jle @f
                sub dword[ebx+edx*4-8],1
                jnz @f
                    pushad
--                if debugmem2
--                  mov eax,[esp+52]    -- (untested!)
--                  mov [dmFera],eax
--                end if
                    push dword[esp+56]
                    call :%pDealloc0
                    popad
          @@:
            stosd                   -- mov [edi],eax, edi+=4
            sub ecx,1
            jnz :opRepsSeqAtomLoop

      ::opRepsPop6
------------------
            add esp,24
            ret

      ::opRepsSeqSeq
--------------------
            --
            -- .. and sequences of the same length
            --
            cmp dword[eax-12],ecx
            jne :opRepsSeqMRSeqStr
            cmp ecx,0
            je :opRepsPop6          -- eg x[2..1]={}/"", do nothing case
            --
            --  eax: raw(edi)
            --  ecx: slice length [also in esp+4] (!=0)
            --  edx: ref length [also in esp]
            --  edi: rep ref (a sequence/string of length ecx)
            --  esi: raw(ref) (a dword-sequence)
            --  [esp] ref length
            --  [esp+4] slice length
            --  [esp+8] slice start (0-based)
            --  [esp+12] ref addr
            --  [esp+16] slice end (1-based)
            --  [esp+20] rep ref
            --  [esp+24] return addr
            --
            mov edx,[esp+8]             -- slice start
            mov edi,esi                 -- raw(ref)
            mov esi,eax                 -- raw(rep)
            cmp byte[eax-1],0x80
--5/2/15:
--          lea edi,[edi+edx*4-4]       -- addr ref[slice start]
            lea edi,[edi+edx*4]         -- addr ref[slice start]
            jne :opRepsSeqStr
          ::opRepsSeqSeqLoop
                lodsd                       -- mov eax,[esi], esi+=4
                cmp eax,h4
                jl @f
                    add dword[ebx+eax*4-8],1
              @@:
                mov edx,[edi]
                cmp edx,h4
                jle @f
                    sub dword[ebx+edx*4-8],1
                    jnz @f
                        pushad
--                    if debugmem2
--                      mov eax,[esp+52]    -- as above
--                      mov [dmFera],eax
--                    end if
                        push dword[esp+56]
                        call :%pDealloc0
                        popad
              @@:
                stosd                   -- mov [edi],eax, edi+=4
                sub ecx,1
                jnz :opRepsSeqSeqLoop
            jmp :opRepsPop6

      ::opRepsSeqStr
--------------------
            --
            --  eax: raw(edi)
            --  ecx: slice length [also in esp+4] (!=0)
            --  edx: slice start [also in esp+8]
            --  edi: addr ref[slice start]
            --  esi: raw(rep) (a dword-sequence)
            --  [esp] ref length
            --  [esp+4] slice length
            --  [esp+8] slice start (0-based)
            --  [esp+12] ref addr
            --  [esp+16] slice end (1-based)
            --  [esp+20] rep ref
            --  [esp+24] return addr
            --
            xor eax,eax
          ::opRepsSeqStrLoop
                mov edx,[edi]
                lodsb                   -- mov al,[esi], esi+=1
                cmp edx,h4
                jle @f
                    sub dword[ebx+edx*4-8],1
                    jnz @f
                        pushad
--                    if debugmem2
--                      mov eax,[esp+52]    -- as above*2
--                      mov [dmFera],eax
--                    end if
                        push dword[esp+56]
                        call :%pDealloc0
                        popad
              @@:
                stosd                   -- mov [edi],eax, edi+=4
                sub ecx,1
                jnz :opRepsSeqStrLoop

--          jmp :opRepsPop6
            add esp,24
            ret

      ::opRepsSeqMultiRef
-------------------------
            --
            --  al: type byte ([esi-1])
            --  ecx: slice length [also in esp+4]
            --  edx: ref length [also in esp]
            --  edi: rep ref [also in esp+20]
            --  esi: raw(ref) (dword_sequence, refcount>1)
            --  [esp] ref length
            --  [esp+4] slice length
            --  [esp+8] slice start (0-based)
            --  [esp+12] ref addr
            --  [esp+16] slice end (1-based)
            --  [esp+20] rep ref
            --  [esp+24] return addr
            --
            --
            -- ok, how big does this thing have to be?
            --
            -- In the case of an atom, same as source...
            --
            cmp edi,h4
            jl @f
                lea eax,[ebx+edi*4]
                test byte[ebx+edi*4-1],0x80
                jnz :opRepsSeqMRSeqStrClone
                add dword[ebx+edi*4-8],ecx    -- bulk ref update (float)
          @@:                       -- replacement is an atom
            cmp ecx,0               -- check for zero length slice replacement
            je :opRepsPop6          -- do nothing case
            mov ecx,edx
            mov edx,[esp+24]        -- era
            mov edi,[esp+12]        -- addr ref
            call :%pAlloClone
            call :%pAllocSeq        -- damages eax only
            mov [edi],eax           -- Replace the ref at the original address
            sub dword[esi-8],1      -- non-1 so no need to dealloc
            lea edi,[ebx+eax*4]
            --
            -- duplicate original items before the slice
            --
--          mov ecx,[esp+12]                -- slice start
            mov ecx,[esp+8]         -- slice start (0-based)
--          sub ecx,1
            test ecx,ecx
            jz :opRepsSeqMRAmid
          ::opRepsSeqMRAtomBeforeLoop
                lodsd                           -- mov eax,[esi], esi+=4
                stosd                           -- mov [edi],eax, edi+=4
                cmp eax,h4
                jl @f
                    add dword[ebx+eax*4-8],1    -- exception should never happen (we are cloning)
              @@:
                sub ecx,1
                jnz :opRepsSeqMRAtomBeforeLoop

            --
            -- blat slice area with atom
            --
          ::opRepsSeqMRAmid
---------------------------
            mov eax,[esp+20]        -- rep ref 
            mov ecx,[esp+4]         -- slice len
            rep stosd
            --
            -- duplicate remaining original items
            --
            mov ecx,[esp]           -- original source ref length
--erm 17/11/14:
--          mov eax,[esp+8]         -- slice end
            mov eax,[esp+16]        -- slice end
            sub ecx,eax
            jz :opRepsPop6
            mov eax,[esp+4]         -- slice length
--15/2/18:
--          lea esi,[esi+eax*4-4]
            lea esi,[esi+eax*4]
          ::opRepsSeqMRAtomAfterLoop
                lodsd                       -- mov eax,[esi], esi+=4
                stosd                       -- mov [edi],eax, edi+=4
                cmp eax,h4
                jl @f
                    add dword[ebx+eax*4-8],1            -- exception should never happen (we are cloning)
              @@:
                sub ecx,1
                jnz :opRepsSeqMRAtomAfterLoop
--          jmp :opRepsPop6
            add esp,24
            ret

      ::opRepsSeqMRSeqStrClone
            sub edx,ecx                 -- original length-slicelength
            mov ecx,dword[eax-12]       -- replacement length
            add ecx,edx
            mov edi,[esp+12]            -- addr ref
            mov edx,[esp+24]            -- era
            call :%pAlloClone
            jmp @f

      ::opRepsSeqMRSeqStr
-------------------------
            --
            --  eax: raw(edi)
            --  ecx: slice length [also in esp+4]
            --  edx: ref length [also in esp]
            --  edi: rep ref (a sequence/string) [also in esp+20]
            --  esi: raw(ref) (a dword-sequence)
            --  [esp] ref length
            --  [esp+4] slice length
            --  [esp+8] slice start (0-based)
            --  [esp+12] ref addr (becomes original ref rsn)
            --  [esp+16] slice end (1-based)
            --  [esp+20] rep ref (becomes ref[sliceend+1] rsn)
            --  [esp+24] return addr
            --
            --
            -- .. in the case of a string/sequence rep, the required length is:
            --
            sub edx,ecx                 -- original length-slicelength
            mov ecx,dword[eax-12]       -- replacement length
            add ecx,edx
--          mov edx,[esp+12]            -- addr ref
            mov edi,[esp+12]            -- addr ref
            mov edx,[esp+24]            -- era
          @@:
            call :%pAllocSeq            -- damages eax only
--          mov edi,[edx]               -- original ref
            mov edx,[edi]               -- original ref
            mov ecx,[esp+8]             -- slice start (0-based)
--          mov [esp+12],edi            -- decref/dealloc at end (opRepsPop6da)
            mov [esp+12],edx            -- decref/dealloc at end (opRepsPop6da)
--          mov [edx],eax               -- Replace the ref at the original address
            mov [edi],eax               -- Replace the ref at the original address

            lea edi,[ebx+eax*4]
            mov edx,[esp+4]             -- slice length (for opRepsSeqMRSmid)
            --
            -- duplicate original items before the slice
            --
--          sub ecx,1
            test ecx,ecx
            jz opRepsSeqMRSmid
          ::opRepsSeqMRSeqBeforeLoop
                lodsd                   -- mov eax,[esi], esi+=4
                stosd                   -- mov [edi],eax, edi+=4
                cmp eax,h4
                jl @f
                    add dword[ebx+eax*4-8],1    -- exception should never happen (we are cloning)
              @@:
                sub ecx,1
                jnz :opRepsSeqMRSeqBeforeLoop

            --
            -- slice area is either from sequence or string...
            --
          ::opRepsSeqMRSmid
---------------------------
            lea ecx,[esi+edx*4]         -- ref[end slice+1]
            mov esi,[esp+20]            -- rep ref
            mov [esp+20],ecx            -- ref[end slice+1]
            shl esi,2
            mov ecx,[esi-12]            -- rep length
            cmp ecx,0
            je :opRepsSeqMRmidDone
            cmp byte[esi-1],0x80
            jne :opRepsSeqMRmidStr
            -- copy seq elements one by one into middle of new sequence
          ::opRepsSeqMRSeqLoop
                lodsd                   -- mov eax,[esi], esi+=4
                stosd                   -- mov [edi],eax, edi+=4
                cmp eax,h4
                jl @f
                    add dword[ebx+eax*4-8],1    -- exception should never happen (we are cloning)
              @@:
                sub ecx,1
                jnz :opRepsSeqMRSeqLoop
            nop
            jmp :opRepsSeqMRmidDone

          ::opRepsSeqMRmidStr
-----------------------------
            -- copy string chars one by one into middle of new sequence
            xor eax,eax
          @@:
                lodsb                   -- mov al,[esi], esi+=1
                stosd                   -- mov [edi],eax, edi+=4
                sub ecx,1
                jnz @b

          ::opRepsSeqMRmidDone
------------------------------
            --
            -- duplicate remaining original items
            --
            mov ecx,[esp]               -- original source ref length
            mov eax,[esp+16]            -- slice end
            sub ecx,eax
            jz :opRepsPop6da
            mov esi,[esp+20]            -- ref[sliceend+1]
          ::opRepsSeqMRSeqAfterLoop
                lodsd                   -- mov eax,[esi], esi+=4
                stosd                   -- mov [edi],eax, edi+=4
                cmp eax,h4
                jl @f
                    add dword[ebx+eax*4-8],1    -- exception should never happen (we are cloning)
              @@:
                sub ecx,1
                jnz :opRepsSeqMRSeqAfterLoop

          ::opRepsPop6da
------------------------
            mov edx,[esp+12]        -- original ref
            add esp,24
            cmp edx,h4
            jle @f
                sub dword[ebx+edx*4-8],1
                jz :%pDealloc
          @@:
            ret
      
      ::opRepsStr
-----------------
        --
        --  al: type byte ([esi-1]) (#82 [actually: has bit #80 but !=#80])
        --  ecx: slice length [also in esp+4]
        --  edx: ref length [also in esp]
        --  edi: rep ref (unknown type)
        --  esi: raw(ref) (a string)
        --  [esp] ref length
        --  [esp+4] slice length
        --  [esp+8] slice start (0-based)
        --  [esp+12] ref addr (may become original ref rsn)
        --  [esp+16] slice end (1-based)
        --  [esp+20] rep ref
        --  [esp+24] return addr
        --
        cmp al,0x82
        jne :e04atsaa24 -- cannot happen? (unless type byte is corrupt)
        --
        -- check for insitu replacement
        --
        cmp dword[esi-8],1
        jne :opRepsStrMultiRef
        --
        -- which is ok for chars...
        --
        cmp edi,h4
        jge @f
            cmp ecx,0               -- check for zero length slice replacement
            je :opRepsPop6          -- do nothing case
            cmp edi,#FF
            ja :opRepsStrToSeqA     -- also jumps for -ve & non-int
--          mov edi,-1
--          add esi,[esp+8]         -- slice start (0-based)
            mov edi,[esp+8]         -- slice start (0-based)
--          mov eax,edi
            mov eax,[esp+20]
            add edi,esi
--          lea edi,[esi+edx-1]
            rep stosb
            jmp :opRepsPop6
      @@:
        --
        -- .. and strings of the same length
        --
        cmp byte[ebx+edi*4-1],0x82
        jne @f
            cmp dword[ebx+edi*4-12],ecx
            jne :opRepsStrSeqL
            shl edi,2
--          mov edx,[esp+12]        -- slice start
            mov edx,[esp+8]         -- slice start (0-based)
            xchg esi,edi
--DEV see above?
--          lea edi,[edi+edx-1]
            add edi,edx
            rep movsb
            jmp :opRepsPop6

      @@:
        cmp byte[ebx+edi*4-1],0x80
        je :opRepsStrSeqL
      ::opRepsStrToSeqAtom
        add dword[ebx+edi*4-8],ecx  -- bulk ref update (float)

      ::opRepsStrToSeqA
-----------------------
        --
        -- replacement (edi) is atom, but we must create a sequence (same length as original)
        --
        cmp ecx,0                   -- check for zero length slice replacement
        je :opRepsPop6              -- do nothing case
        mov ecx,edx                 -- new length == original length
--      mov edx,[esp+12]            -- ref addr
        mov edi,[esp+12]            -- ref addr
--      push edi                    --[7]
        mov edx,[esp+24]            -- era
        call :%pAllocSeq            -- damages eax only
--      mov edi,[edx]
        mov edx,[edi]
--      mov [edx],eax               -- Replace the ref at the original address
        mov [edi],eax               -- Replace the ref at the original address
-->     mov [esp+16],edi            -- decref/dealloc at end (opRepsPop6da)
--      mov [esp+12],edi            -- decref/dealloc at end (opRepsPop6da)
        mov [esp+12],edx            -- decref/dealloc at end (opRepsPop6da)
        mov edx,eax
        lea edi,[ebx+eax*4]
--      add edx,[esp+12]
        add edx,[esp+8]             -- slice start, 0-based
        mov ecx,[esp]               -- source ref length
        shl edx,2                   -- (lea edx,newseq[slice start], doing both ref->raw and idx->dwords at the same time)
        xor eax,eax
      @@:
            lodsb                   -- mov al,[esi], esi+=1
            stosd                   -- mov [edi],eax, edi+=4
            sub ecx,1
            jnz @b

        mov edi,edx                 -- newseq[slicestart]
--      pop eax                     --[7]
        mov eax,[esp+20]            -- rep ref (already incref'd)
        mov ecx,[esp+4]             -- slice len
        rep stosd
        jmp :opRepsPop6da

      ::opRepsStrMultiRef
-------------------------
        --
        --  al: type byte ([esi-1]) (#82)
        --  ecx: slice length [also in esp+4]
        --  edx: ref length [also in esp]
        --  edi: rep ref (unknown type)
        --  esi: raw(ref) (a string)
        --  [esp] ref length
        --  [esp+4] slice length
        --  [esp+8] slice start (0-based)
        --  [esp+12] ref addr (may become original ref rsn)
        --  [esp+16] slice end (1-based)
        --  [esp+20] rep ref
        --  [esp+24] return addr
        --
        --
        -- if the replacement is a char or a string or a 0-length dword-sequence, then result is string
        --
        cmp edi,h4
        jg @f
            cmp ecx,0                   -- check for zero length slice replacement
            je :opRepsPop6              -- do nothing case
            cmp edi,#FF                 -- also jumps for -ve & non-int
            ja :opRepsStrToSeqA
            mov ecx,edx                 -- same length as original
            mov edx,[esp+12]            -- ref addr
            call :%pAllocStr            -- damages eax only
            mov [edx],eax               -- Replace the ref at the original address
            sub dword[esi-8],1          -- non-1 so no need to dealloc
            xchg eax,edi
            mov edx,[esp+8]             -- slice start (0-based)
            shl edi,2
            mov ecx,[esp]               -- original source length (all of it)
            add edx,edi                 -- (lea edx,newstr[slicestart])
            add ecx,1                   -- and the trailing null
            rep movsb
            mov edi,edx
            mov ecx,[esp+4]             -- slice length (go over middle bit)
            rep stosb
            jmp :opRepsPop6
      @@:
        cmp byte[ebx+edi*4-1],0x12
        je :opRepsStrToSeqAtom
      ::opRepsStrSeqL
        -- calculate required size (whether it needs to be a string or dword-sequence)
        add edx,dword[ebx+edi*4-12] -- original plus replacement length..
--      lea eax,[ebx+edi*4]         -- may as well
-->     sub dword[esi-8],1          -- non-1 so no need to dealloc (no, use opRepsPop6da)
        sub edx,dword[esp+4]        -- ..minus slice length
        mov ecx,edx                 -- new length
        mov edx,[esp+12]            -- ref addr
        cmp dword[ebx+edi*4-12],0   -- replacement length 0?
        je :opRepsStrStr
        cmp byte[ebx+edi*4-1],0x80
        je :opRepsStrSeq

      ::opRepsStrStr
--------------------
        --
        -- string[i..j]:=string
        -- new string (size already calculated), built in chunks
        --
        call :%pAllocStr            -- damages eax only
        mov edi,[edx]
        mov [esp+12],edi            -- now becomes ref to decref at end
        mov [edx],eax               -- Replace the ref at the original address
        --
        -- duplicate original items before the slice
        -- (and set edx,eax for later use, to avoid AGI stalls)
        --
        lea edi,[ebx+eax*4]
        mov edx,[esp+16]            -- slice end (1-based)
        mov ecx,[esp+8]             -- slice start (0-based)
        mov eax,[esp+20]            -- rep ref (a string)
        add edx,esi                 -- (lea edx,[esi+sliceend+1])
        shl eax,2                   -- (lea eax,[rep[1]])
        rep movsb
        --
        -- duplicate the replacement string
        --
        mov esi,eax
        mov ecx,[eax-12]            -- replacement length
        rep movsb
        --
        -- duplicate remaining original items
        --
        mov ecx,[esp]           -- ref length
        mov esi,edx
        sub ecx,[esp+16]        -- minus slice end (1-based)
--added 16/2/15:
        add ecx,1               -- include terminating null
        rep movsb
        jmp :opRepsPop6da

      ::opRepsStrSeq
--------------------
        --
        --  al: type byte ([esi-1]) (#82)
        --  ecx: slice length [also in esp+4]
--X     --  edx: ref length [also in esp]
        --  edx: ref addr
        --  edi: rep ref (a dword-sequence, of non-0 length)
        --  esi: raw(ref) (a string)
        --  [esp] ref length
        --  [esp+4] slice length
        --  [esp+8] slice start (0-based)
        --  [esp+12] ref addr (becomes original ref rsn)
        --  [esp+16] slice end (1-based)
        --  [esp+20] rep ref
        --  [esp+24] return addr
        --
        --
        -- string[i..j]:=dword_sequence
        -- new sequence (size already calculated), built in chunks
        --
        mov edi,edx
        mov edx,[esp+24]        -- era
        call :%pAllocSeq        -- damages eax only
--      mov edi,[edx]
        mov edx,[edi]
--      mov [esp+12],edi        -- now becomes ref to dealloc at end
        mov [esp+12],edx        -- now becomes ref to dealloc at end
--      mov [edx],eax           -- Replace the ref at the original address
        mov [edi],eax           -- Replace the ref at the original address

        --
        -- duplicate original chars before the slice as dwords
        -- (and set edx for later use, to avoid an AGI stall)
        --
--      mov edx,[esp+16]        -- slice end, 1-based
        lea edi,[ebx+eax*4]
--      shl edx,2
        mov ecx,[esp+8]         -- slice start (0-based)
-->     mov eax,[esp+20]        -- rep ref (a sequence)
--      add edx,esi             -- (lea edx,[esi+sliceend+1])
-->     shl eax,2
--?     mov [esp+20],edx
        mov edx,[esp+20]        -- rep ref (a sequence)
        test ecx,ecx
        jz @f
            xor eax,eax
      ::opRepsStrSeqBeforeLoop
            lodsb               -- mov al,[esi], esi+=1
            stosd               -- mov [edi],eax, edi+=4
            sub ecx,1
            jnz :opRepsStrSeqBeforeLoop

      @@:
        --
        -- duplicate replacement sequence contents
        -- (and save esi in edx for later use)
        --
-->     mov ecx,[esp]           -- ref length
--!     mov esi,edx
-->?        sub ecx,[esp+16]        -- minus slice end (1-based)
-->?        mov esi,edx
        mov eax,esi
        lea esi,[ebx+edx*4]
        mov ecx,[ebx+edx*4-12]  -- rep length
        mov edx,eax
--      mov edx,[esp+16]        -- slice end, 1-based
--      shl edx,2
--      add edx,esi             -- (lea edx,[esi+sliceend+1])
--      test ecx,ecx            -- (we know it is not 0-length)
--      jz :opRepsStrSeqMidDone
      ::opRepsStrSeqMidLoop
            lodsd               -- mov eax,[esi], esi+=4
            stosd               -- mov [edi],eax, edi+=4
            cmp eax,h4
            jl @f
                add dword[ebx+eax*4-8],1    -- exception should never happen (we are cloning)
          @@:
            sub ecx,1
            jnz :opRepsStrSeqMidLoop

--    ::opRepsStrSeqMidDone
---------------------------
        --
        -- duplicate remaining original items
        --
        mov esi,edx
        mov ecx,[esp]           -- ref length
        add esi,[esp+4]         -- slice length
--      mov esi,[esp+20]
--      add esi,[esp+4]         -- slice length
        sub ecx,[esp+16]        -- minus slice end (1-based)
        jz :opRepsPop6da
        xor eax,eax
      @@:
            lodsb               -- mov al,[esi], esi+=1
            stosd               -- mov [edi],eax, edi+=4
            sub ecx,1
            jnz @b
        nop
        jmp :opRepsPop6da

    [64]
        --calling convention:
        --  mov rcx,N
        --  push <return address>
        --  push [rep]          -- replacement (opUnassigned)
        --  push [sliceend]     -- sliceend (opUnassigned)
        --  push [idxN]..[idx1] -- (opUnassigned)
        --  lea rax,[ref]       -- ref addr
        --  jmp opReps          -- actually a call
        --<return address>
        mov r15,h4
      ::opRepsRnxt64
        mov rsi,[rax]
        pop rdi                 -- next idx (ref)
        cmp rsi,r15
--      jl :e04atsaam4          -- attempt to subscript an atom, era @ [esp+ecx*4-4]?
        jg @f
            mov rdx,[rsp+rcx*8+8]   -- era
            mov al,4                -- e04atsaam4
            sub rdx,1
            jmp :!iDiag
            int3
      @@:
        push rax                --[1] ref addr, in case we need to clone...
        mov al,[rbx+rsi*4-1]
        sub rdi,1               -- decrement rdi (:-)
--      mov rdx,[rbx+rsi*4-12]  -- length
        mov rdx,[rbx+rsi*4-24]  -- length
        shl rsi,2

        sub rcx,1
        je :opRepsMain64        -- deal with final slice separately
        cmp al,0x80
        jne :e04atsaa9          -- must be seq since another idx (or the slice) follows, era * [esp+ecx*4+4]?

        cmp rdi,rdx
        jb @f                   -- unsigned jump, lets 0..len-1 through
                                --               (we just decremented rdi)
            mov al,8+4+0        -- [era] @ [rsp+rcx*8+8?], "assigning to"
            call :%fixupIndex   -- idx-1 in rdi, len in rdx, al set
      @@:
        --
        -- rdi now contains index to replace, and rdx the length
        --   
        mov rax,[rsi-16]        -- refcount
        sub rax,1
        jz :opRepsSeqNoClone64
        mov [rsi-16],rax        -- non-1 so no need to dealloc
        mov rax,[rsp+rcx*8+8]   -- era
        push rcx                --[2] no of remainding indexes
        push rdi                --[3] idx
        mov rcx,rdx
        mov rdi,[rsp+16]        --[1] (ref addr, leaving it on the stack)
        mov rdx,rax             -- era
        call :%pAlloClone
        call :%pAllocSeq        -- damages rax only
        mov [rdi],rax           -- Replace ref at original address [no dealloc rqd]
        lea rdi,[rbx+rax*4]
        push rdi                --[4]
      @@:
            lodsq               -- mov rax,[rsi], rsi+=8
            stosq               -- mov [rdi],rax, rdi+=8
            cmp rax,r15
            jl :opReps_no_incref64
                add qword[rbx+rax*4-16],1
          ::opReps_no_incref64

            sub rcx,1
            jnz @b

        pop rsi                 --[4] NB rsi:=rdi
        pop rdi                 --[3] idx
        pop rcx                 --[2] no of remaining indexes
      ::opRepsSeqNoClone64
        add rsp,8               --[1] discard ref addr
--29/12/15:
--      lea rax,[rsi+rdi*4]
        lea rax,[rsi+rdi*8]
        jmp :opRepsRnxt64

      ::opRepsMain64
--------------------
        -- al type byte
        -- rcx 0
        -- rdx length ref
        -- rsi raw ref
        -- rdi slice start (-1, pre fixup)
        -- stack contents:
        --  [rsp]   ref addr (after several subcripts)
        --  [rsp+8] slice end (pre fixup)
        --  [rsp+16] rep ref
        --  [rsp+24] <return address>
        test al,0x80
--      jz :e04atsaa4
        jnz @f
            mov rdx,[rsp+24]
            mov al,4    -- e04atsaa
            sub rdx,1
            jmp :!iDiag
            int3
      @@:
        cmp rdi,rdx
        jbe @f                  -- unsigned jump, lets 0..len through (NB jbe here)
                                --               (we just decremented rdi)
                                --               (slice start can be 1..length+1)
--DEV +12
--          mov cl,8                -- [era] @ [esp+8]
            mov cl,32               -- [era] @ [rsp+32]
            call :%fixupSliceStart  -- idx in rdi, len in rdx
      @@:
        push rdi                -- save slice start (0-based)
        mov rdi,[rsp+16]        -- slice end
        cmp rdi,rdx
        jbe @f                  -- unsigned jump, lets 0..len through (NB jbe here, and no dec rdi)
                                --               (slice end can be 0..length)
--DEV 16
--          mov cl,12               -- [era] @ [esp+12]
            mov cl,40               -- [era] @ [rsp+40]
            call :%fixupSliceEnd    -- idx in rdi, len in rdx, idx addr in rbx(?)
            mov [rsp+16],rdi        -- save normalised slice end (1-based)
      @@:
        mov rcx,rdi
        sub rcx,[rsp]
        jl :e09slinespp4        -- slice length is negative (%d..%d)
        push rcx                -- save slice length
        push rdx                -- save source (ref) length
        mov rdi,[rsp+40]        -- rep ref
        --
        --  al: type byte ([esi-1])
        --  rcx: slice length [also in rsp+8]
        --  rdx: ref length [also in rsp]
        --  rdi: rep ref
        --  rsi: raw(ref)
        --  [rsp] ref length
        --  [rsp+8] slice length
        --  [rsp+16] slice start (0-based)
        --  [rsp+24] ref addr
        --  [rsp+32] slice end (1-based)
        --  [rsp+40] rep ref
        --  [rsp+48] return addr
        --
        -- handle sequence and string slices separately...
        --
        cmp al,0x80
        jne :opRepsStr64
            --
            -- check for insitu replacement
            --
--          cmp dword[rsi-16],1
            cmp qword[rsi-16],1
            jne :opRepsSeqMultiRef64
            --
            -- which is ok for atoms...
            --
            cmp rdi,r15
            jl @f
                lea rax,[rbx+rdi*4]
                test byte[rbx+rdi*4-1],0x80
                jnz :opRepsSeqSeq64
                add qword[rbx+rdi*4-16],rcx -- bulk ref update (float)
          @@:                       -- replacement is an atom
--          mov rdx,[rsp+8]         -- slice start
            mov rdx,[rsp+16]        -- slice start
            cmp rcx,0               -- check for zero length slice replacement
            je :opRepsPop664        -- do nothing case
            mov rax,rdi
--          lea rdi,[rsi+rdx*4-8]
--          lea rdi,[rsi+rdx*4]
            lea rdi,[rsi+rdx*8]
          ::opRepsSeqAtomLoop64
            mov rdx,[rdi]
            cmp rdx,r15
            jle @f
                sub qword[rbx+rdx*4-16],1
                jnz @f
--                  pushad
                    push rax
                    push rcx
                    push rsi
                    push rdi
                    push qword[rsp+80]
                    call :%pDealloc0
--                  popad
                    pop rdi
                    pop rsi
                    pop rcx
                    pop rax
          @@:
            stosq                   -- mov [rdi],rax, rdi+=8
            sub rcx,1
            jnz :opRepsSeqAtomLoop64

      ::opRepsPop664
--------------------
            add rsp,48
            ret

      ::opRepsSeqSeq64
----------------------
            --
            -- .. and sequences of the same length
            --
            cmp qword[rax-24],rcx
            jne :opRepsSeqMRSeqStr64
            cmp rcx,0
            je :opRepsPop664            -- eg x[2..1]={}/"", do nothing case
            --
            --  rax: raw(rdi)
            --  rcx: slice length [also in rsp+8] (!=0)
            --  rdx: ref length [also in rsp]
            --  rdi: rep ref (a sequence/string of length ecx)
            --  rsi: raw(ref) (a dword-sequence)
            --  [rsp] ref length
            --  [rsp+8] slice length
            --  [rsp+16] slice start (0-based)
            --  [rsp+24] ref addr
            --  [rsp+32] slice end (1-based)
            --  [rsp+40] rep ref
            --  [rsp+48] return addr
            --
            mov rdx,[rsp+16]            -- slice start
            mov rdi,rsi                 -- raw(ref)
            mov rsi,rax                 -- raw(rep)
            cmp byte[rax-1],0x80
--5/2/15:
--          lea rdi,[rdi+rdx*4-8]       -- addr ref[slice start]
--29/12/15:
--          lea rdi,[rdi+rdx*4]         -- addr ref[slice start]
            lea rdi,[rdi+rdx*8]         -- addr ref[slice start]
            jne :opRepsSeqStr64
          ::opRepsSeqSeqLoop64
                lodsq                       -- mov rax,[rsi], rsi+=8
                cmp rax,r15
                jl @f
                    add qword[rbx+rax*4-16],1
              @@:
                mov rdx,[rdi]
                cmp rdx,r15
                jle @f
                    sub qword[rbx+rdx*4-16],1
                    jnz @f
--                      pushad
                        push rcx
                        push rsi
                        push rax
                        push rdi
                        push qword[rsp+80]
                        call :%pDealloc0
                        pop rdi
                        pop rax
                        pop rsi
                        pop rcx
--                      popad
              @@:
                stosq                   -- mov [rdi],rax, rdi+=8
                sub rcx,1
                jnz :opRepsSeqSeqLoop64
            jmp :opRepsPop664

      ::opRepsSeqStr64
----------------------
            --
            --  rax: raw(edi)
            --  rcx: slice length [also in rsp+8] (!=0)
            --  rdx: slice start [also in rsp+16]
            --  rdi: addr ref[slice start]
            --  rsi: raw(rep) (a dword-sequence)
            --  [rsp] ref length
            --  [rsp+8] slice length
            --  [rsp+16] slice start (0-based)
            --  [rsp+24] ref addr
            --  [rsp+32] slice end (1-based)
            --  [rsp+40] rep ref
            --  [rsp+48] return addr
            --
            xor rax,rax
          ::opRepsSeqStrLoop64
                mov rdx,[rdi]
                lodsb                   -- mov al,[rsi], rsi+=1
                cmp rdx,r15
                jle @f
                    sub qword[rbx+rdx*4-16],1
                    jnz @f
--                      pushad
                        push rcx
                        push rsi
                        push rax
                        push rdi
                        push qword[rsp+80]
                        call :%pDealloc0
                        pop rdi
                        pop rax
                        pop rsi
                        pop rcx
--                      popad
              @@:
                stosq                   -- mov [rdi],rax, rdi+=8
                sub rcx,1
                jnz :opRepsSeqStrLoop64

--          jmp :opRepsPop664
            add rsp,48
            ret

      ::opRepsSeqMultiRef64
---------------------------
            --
            --  al: type byte ([rsi-1])
            --  rcx: slice length [also in rsp+8]
            --  rdx: ref length [also in rsp]
            --  rdi: rep ref [also in rsp+40]
            --  rsi: raw(ref) (dword_sequence, refcount>1)
            --  [rsp] ref length
            --  [rsp+8] slice length
            --  [rsp+16] slice start (0-based)
            --  [rsp+24] ref addr
            --  [rsp+32] slice end (1-based)
            --  [rsp+40] rep ref
            --  [rsp+48] return addr
            --
            --
            -- ok, how big does this thing have to be?
            --
            -- In the case of an atom, same as source...
            --
            cmp rdi,r15
            jl @f
                lea rax,[rbx+rdi*4]
                test byte[rbx+rdi*4-1],0x80
                jnz :opRepsSeqMRSeqStrClone64
                add qword[rbx+rdi*4-16],rcx   -- bulk ref update (float)
          @@:                       -- replacement is an atom
            cmp rcx,0               -- check for zero length slice replacement
            je :opRepsPop664        -- do nothing case
            mov rcx,rdx
--          mov rdx,[rsp+24]        -- addr ref
            mov rdi,[rsp+24]        -- addr ref
            mov rdx,[rsp+48]        -- era
            call :%pAlloClone
            call :%pAllocSeq        -- damages rax only
--          mov [rdx],rax           -- Replace the ref at the original address
            mov [rdi],rax           -- Replace the ref at the original address
            sub qword[rsi-16],1     -- non-1 so no need to dealloc
            lea rdi,[rbx+rax*4]
            --
            -- duplicate original items before the slice
            --
--          mov rcx,[rsp+24]                -- slice start
            mov rcx,[rsp+16]        -- slice start (0-based)
--          sub rcx,1
            test rcx,rcx
            jz :opRepsSeqMRAmid64
          ::opRepsSeqMRAtomBeforeLoop64
                lodsq                           -- mov rax,[rsi], rsi+=8
                stosq                           -- mov [rdi],rax, rdi+=8
                cmp rax,r15
                jl @f
                    add qword[rbx+rax*4-16],1   -- exception should never happen (we are cloning)
              @@:
                sub rcx,1
                jnz :opRepsSeqMRAtomBeforeLoop64

            --
            -- blat slice area with atom
            --
          ::opRepsSeqMRAmid64
-----------------------------
            mov rax,[rsp+40]        -- rep ref 
            mov rcx,[rsp+8]         -- slice len
            rep stosq
            --
            -- duplicate remaining original items
            --
            mov rcx,[rsp]           -- original source ref length
--erm, 7/11/14:
--          mov rax,[rsp+16]        -- slice end
            mov rax,[rsp+32]        -- slice end
            sub rcx,rax
            jz :opRepsPop664
            mov rax,[rsp+8]         -- slice length
--29/12/15:
--          lea rsi,[rsi+rax*4-8]
--15/2/18:
--          lea rsi,[rsi+rax*8-8]
            lea rsi,[rsi+rax*8]
          ::opRepsSeqMRAtomAfterLoop64
                lodsq                       -- mov rax,[rsi], rsi+=8
                stosq                       -- mov [rdi],rax, rdi+=8
                cmp rax,r15
                jl @f
                    add qword[rbx+rax*4-16],1           -- exception should never happen (we are cloning)
              @@:
                sub rcx,1
                jnz :opRepsSeqMRAtomAfterLoop64
--          jmp :opRepsPop664
            add rsp,48
            ret

      ::opRepsSeqMRSeqStrClone64
            sub rdx,rcx                 -- original length-slicelength
            mov rcx,qword[rax-24]       -- replacement length
            add rcx,rdx
            mov rdi,[rsp+24]            -- addr ref
            mov rdx,[rsp+48]            -- era
            call :%pAlloClone
            jmp @f

      ::opRepsSeqMRSeqStr64
---------------------------
            --
            --  rax: raw(rdi)
            --  rcx: slice length [also in rsp+8]
            --  rdx: ref length [also in rsp]
            --  rdi: rep ref (a sequence/string) [also in rsp+40]
            --  rsi: raw(ref) (a dword-sequence)
            --  [rsp] ref length
            --  [rsp+8] slice length
            --  [rsp+16] slice start (0-based)
            --  [rsp+24] ref addr (becomes original ref rsn)
            --  [rsp+32] slice end (1-based)
            --  [rsp+40] rep ref (becomes ref[sliceend+1] rsn)
            --  [rsp+48] return addr
            --
            --
            -- .. in the case of a string/sequence rep, the required length is:
            --
            sub rdx,rcx                 -- original length-slicelength
            mov rcx,qword[rax-24]       -- replacement length
            add rcx,rdx
--          mov rdx,[rsp+24]            -- addr ref
            mov rdi,[rsp+24]            -- addr ref
            mov rdx,[rsp+48]            -- era
          @@:
            call :%pAllocSeq            -- damages rax only
--          mov rdi,[rdx]               -- original ref
            mov rdx,[rdi]               -- original ref
            mov rcx,[rsp+16]            -- slice start (0-based)
--          mov [rsp+24],rdi            -- decref/dealloc at end (opRepsPop6da64)
            mov [rsp+24],rdx            -- decref/dealloc at end (opRepsPop6da64)
--          mov [rdx],rax               -- Replace the ref at the original address
            mov [rdi],rax               -- Replace the ref at the original address

            lea rdi,[rbx+rax*4]
            mov rdx,[rsp+8]             -- slice length (for opRepsSeqMRSmid64)
            --
            -- duplicate original items before the slice
            --
--          sub rcx,1
            test rcx,rcx
            jz opRepsSeqMRSmid64
          ::opRepsSeqMRSeqBeforeLoop64
                lodsq                   -- mov rax,[rsi], rsi+=8
                stosq                   -- mov [rdi],rax, rdi+=8
                cmp rax,r15
                jl @f
                    add qword[rbx+rax*4-16],1   -- exception should never happen (we are cloning)
              @@:
                sub rcx,1
                jnz :opRepsSeqMRSeqBeforeLoop64

            --
            -- slice area is either from sequence or string...
            --
          ::opRepsSeqMRSmid64
-----------------------------
--          lea rcx,[rsi+rdx*4]         -- ref[end slice+1]
            lea rcx,[rsi+rdx*8]         -- ref[end slice+1]
            mov rsi,[rsp+40]            -- rep ref
            mov [rsp+40],rcx            -- ref[end slice+1]
            shl rsi,2
            mov rcx,[rsi-24]            -- rep length
            cmp rcx,0
            je :opRepsSeqMRmidDone64
            cmp byte[rsi-1],0x80
            jne :opRepsSeqMRmidStr64
            -- copy seq elements one by one into middle of new sequence
          ::opRepsSeqMRSeqLoop64
                lodsq                   -- mov rax,[rsi], rsi+=8
                stosq                   -- mov [rdi],rax, rdi+=8
                cmp rax,r15
                jl @f
                    add qword[rbx+rax*4-16],1   -- exception should never happen (we are cloning)
              @@:
                sub rcx,1
                jnz :opRepsSeqMRSeqLoop64
            nop
            jmp :opRepsSeqMRmidDone64

          ::opRepsSeqMRmidStr64
-------------------------------
            -- copy string chars one by one into middle of new sequence
            xor rax,rax
          @@:
                lodsb                   -- mov al,[rsi], rsi+=1
                stosq                   -- mov [rdi],rax, rdi+=8
                sub rcx,1
                jnz @b

          ::opRepsSeqMRmidDone64
--------------------------------
            --
            -- duplicate remaining original items
            --
            mov rcx,[rsp]               -- original source ref length
            mov rax,[rsp+32]            -- slice end
            sub rcx,rax
            jz :opRepsPop6da64
            mov rsi,[rsp+40]            -- ref[sliceend+1]
          ::opRepsSeqMRSeqAfterLoop64
                lodsq                   -- mov rax,[rsi], rsi+=8
                stosq                   -- mov [rdi],rax, rdi+=8
                cmp rax,r15
                jl @f
                    add qword[rbx+rax*4-16],1   -- exception should never happen (we are cloning)
              @@:
                sub rcx,1
                jnz :opRepsSeqMRSeqAfterLoop64

          ::opRepsPop6da64
--------------------------
            mov rdx,[rsp+24]        -- original ref
            add rsp,48
            cmp rdx,r15
            jle @f
                sub qword[rbx+rdx*4-16],1
                jz :%pDealloc
          @@:
            ret
      
      ::opRepsStr64
-------------------
        --
        --  al: type byte ([rsi-1]) (#82 [actually: has bit #80 but !=#80])
        --  rcx: slice length [also in rsp+8]
        --  rdx: ref length [also in rsp]
        --  rdi: rep ref (unknown type)
        --  rsi: raw(ref) (a string)
        --  [rsp] ref length
        --  [rsp+8] slice length
        --  [rsp+16] slice start (0-based)
        --  [rsp+24] ref addr (may become original ref rsn)
        --  [rsp+32] slice end (1-based)
        --  [rsp+40] rep ref
        --  [rsp+48] return addr
        --
        cmp al,0x82
        jne :e04atsaa24 -- cannot happen? (unless type byte is corrupt)
        --
        -- check for insitu replacement
        --
        cmp dword[rsi-16],1
        jne :opRepsStrMultiRef64
        --
        -- which is ok for chars...
        --
        cmp rdi,r15
        jge @f
            cmp rcx,0               -- check for zero length slice replacement
            je :opRepsPop664        -- do nothing case
            cmp rdi,#FF
            ja :opRepsStrToSeqA64   -- also jumps for -ve & non-int
--          mov rdi,-1
--          add rsi,[rsp+16]        -- slice start (0-based)
            mov rdi,[rsp+16]        -- slice start (0-based)
--          mov rax,rdi
            mov rax,[rsp+40]
            add rdi,rsi
--          lea rdi,[rsi+rdx-1]
            rep stosb
            jmp :opRepsPop664
      @@:
        --
        -- .. and strings of the same length
        --
        cmp byte[rbx+rdi*4-1],0x82
        jne @f
            cmp qword[rbx+rdi*4-24],rcx
            jne :opRepsStrSeqL64
            shl rdi,2
--          mov rdx,[rsp+24]        -- slice start
            mov rdx,[rsp+16]        -- slice start (0-based)
            xchg rsi,rdi
--DEV see above?
--          lea rdi,[rdi+rdx-1]
            add rdi,rdx
            rep movsb
            jmp :opRepsPop664

      @@:
        cmp byte[rbx+rdi*4-1],0x80
        je :opRepsStrSeqL64
      ::opRepsStrToSeqAtom64
        add qword[rbx+rdi*4-16],rcx -- bulk ref update (float)

      ::opRepsStrToSeqA64
-------------------------
        --
        -- replacement (rdi) is atom, but we must create a sequence (same length as original)
        --
        cmp rcx,0                   -- check for zero length slice replacement
        je :opRepsPop664            -- do nothing case
        mov rcx,rdx                 -- new length == original length
--      mov rdx,[rsp+24]            -- ref addr
        mov rdi,[rsp+24]            -- ref addr
--      push rdi                    --[7]
        mov rdx,[rsp+48]            -- era
        call :%pAllocSeq            -- damages rax only
--      mov rdi,[rdx]
        mov rdx,[rdi]
--      mov [rdx],rax               -- Replace the ref at the original address
        mov [rdi],rax               -- Replace the ref at the original address
-->     mov [rsp+32],rdi            -- decref/dealloc at end (opRepsPop6da64)
--      mov [rsp+24],rdi            -- decref/dealloc at end (opRepsPop6da64)
        mov [rsp+24],rdx            -- decref/dealloc at end (opRepsPop6da64)
--      mov rdx,rax
        mov rdx,[rsp+16]            -- slice start, 0-based
        lea rdi,[rbx+rax*4]
--      add rdx,[rsp+24]
--      add rdx,[rsp+16]            -- slice start, 0-based
        mov rcx,[rsp]               -- source ref length
        shl rdx,3                   -- idx->qwords
        shl rax,2                   -- ref->raw
        add rdx,rax                 -- (lea rdx,newseq[slice start])
        xor rax,rax
      @@:
            lodsb                   -- mov al,[rsi], rsi+=1
            stosq                   -- mov [rdi],rax, rdi+=8
            sub rcx,1
            jnz @b

        mov rdi,rdx                 -- newseq[slicestart]
--      pop eax                     --[7]
        mov rax,[rsp+40]            -- rep ref (already incref'd)
        mov rcx,[rsp+8]             -- slice len
        rep stosd
        jmp :opRepsPop6da64

      ::opRepsStrMultiRef64
---------------------------
        --
        --  al: type byte ([rsi-1]) (#82)
        --  rcx: slice length [also in rsp+8]
        --  rdx: ref length [also in rsp]
        --  rdi: rep ref (unknown type)
        --  rsi: raw(ref) (a string)
        --  [rsp] ref length
        --  [rsp+8] slice length
        --  [rsp+16] slice start (0-based)
        --  [rsp+24] ref addr (may become original ref rsn)
        --  [rsp+32] slice end (1-based)
        --  [rsp+40] rep ref
        --  [rsp+48] return addr
        --
        --
        -- if the replacement is a char or a string or a 0-length dword-sequence, then result is string
        --
        cmp rdi,r15
        jg @f
            cmp rcx,0                   -- check for zero length slice replacement
            je :opRepsPop664            -- do nothing case
            cmp rdi,#FF                 -- also jumps for -ve & non-int
            ja :opRepsStrToSeqA64
            mov rcx,rdx                 -- same length as original
            mov rdx,[rsp+24]            -- ref addr
            call :%pAllocStr            -- damages rax only
            mov [rdx],rax               -- Replace the ref at the original address
            sub qword[rsi-16],1         -- non-1 so no need to dealloc
            xchg rax,rdi
            mov rdx,[rsp+16]            -- slice start (0-based)
            shl rdi,2                   -- ref -> raw
            mov rcx,[rsp]               -- original source length (all of it)
            add rdx,rdi                 -- (lea rdx,newstr[slicestart])
            add rcx,1                   -- and the trailing null
            rep movsb
            mov rdi,rdx
            mov rcx,[rsp+8]             -- slice length (go over middle bit)
            rep stosb
            jmp :opRepsPop664
      @@:
        cmp byte[rbx+rdi*4-1],0x12
        je :opRepsStrToSeqAtom64
      ::opRepsStrSeqL64
        -- calculate required size (whether it needs to be a string or dword-sequence)
        add rdx,qword[rbx+rdi*4-24] -- original plus replacement length..
--      lea rax,[rbx+rdi*4]         -- may as well
-->     sub qword[rsi-16],1         -- non-1 so no need to dealloc (no, use opRepsPop6da)
        sub rdx,qword[rsp+8]        -- ..minus slice length
        mov rcx,rdx                 -- new length
        mov rdx,[rsp+24]            -- ref addr
        cmp qword[rbx+rdi*4-24],0   -- replacement length 0?
        je :opRepsStrStr64
        cmp byte[rbx+rdi*4-1],0x80
        je :opRepsStrSeq64

      ::opRepsStrStr64
----------------------
        --
        -- string[i..j]:=string
        -- new string (size already calculated), built in chunks
        --
        call :%pAllocStr            -- damages rax only
        mov rdi,[rdx]
        mov [rsp+24],rdi            -- now becomes ref to decref at end
        mov [rdx],rax               -- Replace the ref at the original address
        --
        -- duplicate original items before the slice
        -- (and set rdx,rax for later use, to avoid AGI stalls)
        --
        lea rdi,[rbx+rax*4]
        mov rdx,[rsp+32]            -- slice end (1-based)
        mov rcx,[rsp+16]            -- slice start (0-based)
        mov rax,[rsp+40]            -- rep ref (a string)
        add rdx,rsi                 -- (lea rdx,[rsi+sliceend+1])
        shl rax,2                   -- (lea rax,[rep[1]])
        rep movsb
        --
        -- duplicate the replacement string
        --
        mov rsi,rax
        mov rcx,[rax-24]            -- replacement length
        rep movsb
        --
        -- duplicate remaining original items
        --
        mov rcx,[rsp]           -- ref length
        mov rsi,rdx
        sub rcx,[rsp+32]        -- minus slice end (1-based)
--added 16/2/15:
        add rcx,1               -- include terminating null
        rep movsb
        jmp :opRepsPop6da64

      ::opRepsStrSeq64
----------------------
        --
        --  al: type byte ([rsi-1]) (#82)
        --  rcx: slice length [also in rsp+8]
--X     --  rdx: ref length [also in rsp]
        --  rdx: ref addr
        --  rdi: rep ref (a dword-sequence, of non-0 length)
        --  rsi: raw(ref) (a string)
        --  [rsp] ref length
        --  [rsp+8] slice length
        --  [rsp+16] slice start (0-based)
        --  [rsp+24] ref addr (becomes original ref rsn)
        --  [rsp+32] slice end (1-based)
        --  [rsp+40] rep ref
        --  [rsp+48] return addr
        --
        --
        -- string[i..j]:=dword_sequence
        -- new sequence (size already calculated), built in chunks
        --
        mov rdi,rdx
        mov rdx,[rsp+48]        -- era
        call :%pAllocSeq        -- damages rax only
--      mov rdi,[rdx]
        mov rdx,[rdi]
--      mov [rsp+24],rdi        -- now becomes ref to dealloc at end
        mov [rsp+24],rdx        -- now becomes ref to dealloc at end
--      mov [rdx],rax           -- Replace the ref at the original address
        mov [rdi],rax           -- Replace the ref at the original address

        --
        -- duplicate original chars before the slice as dwords
        -- (and set rdx for later use, to avoid an AGI stall)
        --
        lea rdi,[rbx+rax*4]
        mov rcx,[rsp+16]        -- slice start (0-based)
        mov rdx,[rsp+40]        -- rep ref (a sequence)
        test rcx,rcx
        jz @f
            xor rax,rax
      ::opRepsStrSeqBeforeLoop64
            lodsb               -- mov al,[rsi], rsi+=1
            stosq               -- mov [rdi],rax, rdi+=8
            sub rcx,1
            jnz :opRepsStrSeqBeforeLoop64

      @@:
        --
        -- duplicate replacement sequence contents
        -- (and save rsi in rdx for later use)
        --
        mov rax,rsi
        lea rsi,[rbx+rdx*4]
        mov rcx,[rbx+rdx*4-24]  -- rep length
        mov rdx,rax
      ::opRepsStrSeqMidLoop64
            lodsq               -- mov rax,[rsi], rsi+=8
            stosq               -- mov [rdi],rax, rdi+=8
            cmp rax,r15
            jl @f
                add qword[rbx+rax*4-16],1   -- exception should never happen (we are cloning)
          @@:
            sub rcx,1
            jnz :opRepsStrSeqMidLoop64

--    ::opRepsStrSeqMidDone64
-----------------------------
        --
        -- duplicate remaining original items
        --
        mov rsi,rdx
        mov rcx,[rsp]           -- ref length
        add rsi,[rsp+8]         -- slice length
        sub rcx,[rsp+32]        -- minus slice end (1-based)
        jz :opRepsPop6da64
        xor rax,rax
      @@:
            lodsb               -- mov al,[rsi], rsi+=1
            stosq               -- mov [rdi],rax, rdi+=8
            sub rcx,1
            jnz @b
        nop
        jmp :opRepsPop6da64
    []
      }

