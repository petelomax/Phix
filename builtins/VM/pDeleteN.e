--
-- pDeleteN.e
-- ==========
--
-- Implements delete_routine and delete (auto-include) [DEV make this part of pcfunc.e]
--
--  This file is loaded automatically by Phix as and when needed. There is no
--  need to manually include this file (unless you want a namespace on it).
--
--  First, assume this in all the following examples, and something similar
--   in all programs that make use of delete_routine():
--
--      procedure del(object o)  -- (a procedure taking one object parameter)
--          puts(1,"del\n")      --   (put whatever code you like in here,)
--          ?o                   --   (    eg free/close/log/debug/etc    )
--      end procedure
--      constant r_del = routine_id("del")  -- (whatever names you like)
--
-- Usage
-- =====
--
--      data = delete_routine(data,r_del)
--      ...
--  --  delete(data) -- (or let this occur automatically when refcount:=0)
--
--  A call to delete(data) does NOT destroy/alter data; instead it invokes 
--  the routine(s) that perform any "done with this" processing, and detaches
--  said routines so that they are not invoked again.
--
--  If an integer is passed to delete_routine, it is returned as a float. See
--  notes below for more details.
--
--  A search of the std/ directory should yield plenty of examples.
--
-- NB:
-- ===
--
--  The deletion routines are attached to the *data*, NOT the variable.
--
--      In essence, delete_routine "protects" some data that you pass to it,
--      but it cannot do that reliably if you subsequently "play" with it.
--
--  Data passed to delete_routine should be final, or at least pretty close.
--  ========================================================================
--  
--      sequence table
--          table = {}
--      --  table = delete_routine(table,r_del) -- NO! utterly pointless!
--          for i=1 to something do
--              table = append(table,something_else)
--          end for
--          table = delete_routine(table,r_del) -- much more like it.
--          
--  Better yet, call delete_routine on each something_else as you add it to 
--  table, especially if table is likely to be further extended later on.
--  
--  The example below, with {7} being extended by lots of {8,9}, explains why
--  a call on non-final data is not the right thing to do.
--
--DEV reword if poc below gets implemented, it becomes 16 million permutations
--      of delete_routines, instead of 16 million data items, more on 64-bit.
--  Each and every data value passed to delete_routine gets a private entry in 
--  delete_sets and is marked with the index of that entry. Up to 16 million 
--  separate data items may have attached delete routines at any one time, 
--  which hopefully should be far more than enough (and is probably both far 
--  more than any real program could fit in memory, not particularly difficult 
--  to extend if the need should ever arise, just messy and widespread).
--
--  Each entry in delete_sets can have as many routine_ids as it likes.
--  When delete() is called or the reference count drops to zero, the routines 
--  are called, in reverse order of attachment, and the corresponding entry in
--  delete_sets is put on a freelist for possible later re-use (or we could 
--  quite easily "shrink" the table if that ever became an issue).
--
--  As above, deletion routines are attached to *data*, not variables. Hence:
--
--  (The following all relate to behaviour when data is not "final".)
--
--      object a,b
--
--          a = delete_routine({4},r_del)
--          b = a
--          a[1] = 5
--          puts(1,"a=0\n")
--          a = 0       -- del not called!
--          puts(1,"b=0\n")
--          b = 0       -- del called, with {4}, not {5}
--
--  The above outputs:
--      a=0
--      b=0
--      del
--      {4}
--
-- We asked to "protect" that {4}, not a, and not that modified to {5}, so
--  in this case (by luck) it has done the best we could possibly want.
--
-- However, if you remove the b=a line, del gets called earlier, with {5}:
--
--      a=0
--      del
--      {5}
--      b=0
--
-- We asked to "protect" that {4}, not a, and not that modified to {5}, but
--  in this case we may (or may not) need extra manual protection around
--  that "a[1] = 5", and/or removal (using delete()) beforehand.
--
-- Another example, as mentioned above:
--
--      object a
--          a = delete_routine({7},r_del)
--          for i=1 to 100 do
--              a &= {8,9}
--          end for
--
--  This calls del on the second iteration, with {7,8,9}, since at that point
--  there is no space to extend it, a "clone" occurs and the old is discarded.
--  It would be unlikely if that was what you wanted and may well change - so
--  stick to the "data must be final" rule. Obviously, therefore, a call such
--  as a = delete_routine(a,r_del) after the end for would probably be better.
--  (It may be possible to transfer the delete_routine association as part of 
--   the "clone", but that could still cause unexpected behaviour.)
--  OpenEuphoria calls del with {7}, btw.
--
--  Yet another bad example might be:
--
--      a = delete_routine("a",r_del)
--      b = delete_routine("b",r_del)
--      c = delete_routine("c",r_del)
--
--      a = b & a & c
--
--  The precise behaviour of which is completely undefined - ie del may get 
--  called with "a", or may get called later with "bac". However, of course,
--
--      a = {b,a,c}
--
--  is a perfectly valid and predictable/sensible way of coding, because the 
--  data (that delete_routine saw) has not changed (reference counts aside),
--  it has instead just been moved to a different variable/element. (There 
--  is no warranty of any particular order in the above case, only that all
--  specified delete_routines will (barring abort) at some point be called.)
--  And yes, the fact that a is not "a" anymore is no reason for r_rel to be
--  invoked, as that "a" is still an existing element of the new a.
--
--  You can of course also do things like:
--
--    table = append(table,delete_routine(<expr>,routine_id("tabledel")))
--
--DEV chaining delete_routines.
--  It is technically possible to daisy-chain several delete_routines.
--  Note however the use of file-level variables and multiple references
--  can severely interfere with any such attempts:
--
--      a = delete_routine(a,r_del1)
--      b = a
--      a = delete_routine(a,r_del2)
--
--  Now, r_del1 will effectively be attached to the content of b, and the
--  content of a will have just the one delete_routine, r_del2.
--

-- Finally, some technical points:
--
-- The abort() routine stops program execution immediately, without calling     [DEV not so... at least when interpreting]
--  any delete_routines. A "table={}" statement or similar may be required to
--  force them to be called. Just setting a delete_routine does not guarantee
--  that it will be called, but instead that it will be called *when and if*
--  the value it is associated with is deleted/deallocated. You should still
--  plan for and code some manual cleanup; delete_routine operates more as a
--  guard against data being "lost", than guaranteeing a call occurs.
--  Typically I use/recommend an "Abort()" routine wrapper for abort.
--
-- Delete routines and/or builtins\VM\pHeap.e\:%opDealloc may be called for
--  items in any order. This means you should only rely on the data /within/
--  the parameter to your delete_routine, so in eg:
--      atom t0 = time()    -- (or set during object creation)
--      procedure mydel(object o)
--          printf(1,"deleted after %3.2fs\n",{time()-t0})
--          ...
--      end procedure
--  you should not be surprised if (sometimes) t0 is garbage/unassigned/0.
--  The obvious solution to a situation like the above is to store the time
--  when the object was created somewhere within the object itself. Sometimes
--  you may need to maintain a flag, for example set anywhere that abort() is
--  invoked and after the WinMain() exits, and avoid eg updates to the GUI if 
--  the call to the delete_routine was triggered by application shutdown. (A
--  plain file-level integer flag is not subject to any delete_routine() or
--  :%opDealloc processing.)
--
-- If you pass an integer as the first parameter to delete_routine, it is
--  "promoted" to atom. This may conflict with some fundamental optimisations
--  widely practised in the backend, specifically that no integer is ever the
--  same as any value held in a float. You should not later rely on find() or 
--  even =, but instead use a loop and/or a-N=0 instead of a=N. (ditto "!=")
--  Things should be fine if all you have done is taken a copy of something
--  (after the delete_routine call(s)), but if you are debugging, see a "5",
--  so add (temporarily) say "find(5,table)", then it will not work at all.
--  Simply "wrap" an integer in a sequence to sidestep this issue (obviously 
--  any integers contained therein are left as they were, there is no need 
--  to "promote" them). In truth, integer support is (only) required because 
--  allocate() may return an integer (<#3FFFFFFF); applications should avoid
--  relying on this, at least for the sanity of future maintainers.
--      ie rather than
--          integer thing
--          thing = 5
--          thing = delete_routine(thing,r_del) -- (typecheck error)
--          if thing=5 ...
--      or
--          atom thing
--          thing = 5
--          thing = delete_routine(thing,r_del)
--          if thing=5 ...                      -- (will not work)
--      use
--          sequence thing
--          thing = {5}
--          thing = delete_routine(thing,r_del)
--          if thing[1]=5 ...                   -- (this is fine)
--
-- WARNING
-- =======
-- The delete_routines are not designed or expected to cope across opInterp 
--  and dll boundaries. In "p test", aka "p.exe test.exw", there are two 
--  symtabs, one for p.exe and one for test.exw. Any routine_ids apply to 
--  one but not the other. Likewise for this.exe and that.dll. A similar 
--  situation exists for file handles; you cannot open a file in a dll, 
--  pass the handle(/ftable index) back, and expect the callee to further 
--  process/close that file, at least not via the routines in (two copies 
--  of) builtins\VM\pfileioN.e. Of course if you returned a file number 
--  with the sole intention of passing it back to the dll for further
--  processing, that would be fine, whereas it would be technically tricky
--  and utterly nonsensical to explicitly invoke that sort of call-back in
--  order to implicitly trigger a delete_routine in the right context.
--  If necessary, it may be possible to use call_backs to the main process 
--  to set delete_routines and/or perform file i/o, but such is untested.
--
--include pcfunc.e
--DEV?
include pcallfunc.e

--integer freelist = 0
--sequence delete_sets      -- {{rid,rid,rid,...}}
--integer dinit = 0

--global function reset_delete()
---- for use by p.exw; no discernable effect when used elsewhere
--end function

--global procedure delete(object o)
procedure fdelete(object o)
-- Note this code is replicated in free()
--integer delete_index
--sequence dsi
integer rid
    if not integer(o) then
--      -- get the current delete_index, if any
        -- get the current delete_routine, if any
        #ilASM{ [32]
                    mov edx,[o]
                    mov eax,[ebx+edx*4-4] -- (load index & type byte)
                    mov ecx,eax
                    and eax,0x00FFFFFF  -- (keep delete_index only)
                    and ecx,0xFF000000  -- (keep type byte only)
                    mov [ebx+edx*4-4],ecx -- (zeroise delete_index on data)
--                  mov [delete_index],eax
                    mov [rid],eax
                [64]
                    mov rdx,[o]
                    mov rax,[rbx+rdx*4-8] -- (load index & type byte)
                    mov rcx,rax
--                  and rax,0x00FFFFFFFFFFFFFF  -- (keep delete_index only)
--                  and rcx,0xFF00000000000000  -- (keep type byte only)
                    shl rax,8
                    rol rcx,8
                    shr rax,8
                    shl rcx,32+24   -- (56, aka 64-8)
                    mov [rbx+rdx*4-8],rcx -- (zeroise delete_index on data)
--                  mov [delete_index],rax
                    mov [rid],rax
              }

--      if delete_index then
        if rid!=0 then
--          for i=length(delete_sets[delete_index]) to 1 by -1 do
--              call_proc(delete_sets[delete_index][i],{o})
--          end for
--          enter_cs()
--          dsi = delete_sets[delete_index]
--          leave_cs()
--          for i=length(dsi) to 1 by -1 do
--              call_proc(dsi[i],{o})
--          end for
            call_proc(rid,{o})
--DEV locking required (possibly just inside the "if not integer" test)
--          enter_cs()
--          delete_sets[delete_index] = freelist
--          freelist = delete_index
--          leave_cs()
        end if
    end if
end procedure

--global function delete_routine(object o, integer rid)
--function fdelete_routine(object o, integer rid)
----
---- attach rid to the data in o
----
--  --
--  -- verify that rid is a procedure accepting one parameter?
--  --
----> DEV
--
----integer prev
----integer delete_index
----, refcount
----    if dinit=0 then setup() end if
----/*
--  -- the penny drops: p.exw has to invoke delete_routine immediately
--  --  after running "#!:%opInterp", before any o/s delete() kick in.
----DEV
----    #ilASM{ call @f
--  #ilASM{ call :f
--
--          -- (3 nops to allow [pDelRtn] to be stored/4, see pHeap.e)
--          nop
--          nop
--          nop
----        ::opDelete
--      [32]
--          -- calling convention (from pHeap.e/pDealloc only)
--          --  mov edx,ref         -- (refcount set to 2)
--          --  call [pDelRtn]      -- ~==delete(edx) [pDelRtn] is ::opDelete
--          push edx                            --[1] o
--          mov edx,routine_id(delete)          -- mov edx,imm32 (sets K_ridt)
--          mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[fdelete][S_Ltot])
--          call :%opFrame
--          mov edx,[esp+4]
--          pop dword[ebp]                      --[1] o
--X         mov dword[ebp+16],:delret           -- return address
--          mov dword[ebp+28],:delret           -- return address
--          mov dword[ebp+12],edx               -- called from address
--          jmp $_il                            -- jmp code:delete
--      [64]
--          -- calling convention (from pHeap.e/pDealloc only)
--          --  mov rdx,ref         -- (refcount set to 2)
--          --  call [pDelRtn]      -- ~==delete(rdx) [pDelRtn] is ::opDelete
--          push rdx                            --[1] o
--          mov rdx,routine_id(delete)          -- mov rdx,imm32 (sets K_ridt)
--          mov rcx,$_Ltot                      -- mov rcx,imm32 (=symtab[fdelete][S_Ltot])
--          call :%opFrame
--          mov rdx,[rsp+8]
--          pop qword[rbp]                      --[1] addr
--X         mov qword[rbp+32],:delret           -- return address
--          mov qword[rbp+56],:delret           -- return address
--          mov qword[ebp+24],rdx               -- called from address
--          jmp $_il                            -- jmp code:delete
--      []
--        ::delret
--          ret
----          @@:
--        ::f
--      [32]
--          pop eax
--          add eax,3
--      [64]
--          pop eax
--          add rax,3
--      []
--          call :%pSetDel }
----*/
--
--  if integer(o) then
--      -- promote to atom
--      #ilASM{ [32]
--                  lea edi,[o]
--                  fild dword[o]
--                  fldpi                   -- (any non-integer value would do)
--                  call :%pStoreFlt
--                  mov edi,[edi]
--                  mov ecx,[rid]
--                  fstp qword[ebx+edi*4]
--                  mov eax,[ebx+edi*4-4]
--                  or eax,ecx  -- (combine rid and type byte)
--                  mov [ebx+edi*4-4],eax
--              [64]
--                  lea rdi,[o]
--                  fild qword[o]
--                  fldpi                   -- (any non-integer value would do)
--                  call :%pStoreFlt
--                  mov rdi,[rdi]
--                  mov rcx,[rid]
--                  fstp tbyte[rbx+rdi*4]
--                  mov rax,[rbx+rdx*4-8]
--                  or rax,rcx  -- (combine rid and type byte)
--                  mov [rbx+rdx*4-8],rax
--            }
----        delete_index = 0
--  else
--      -- first, force a refcount of 1
--      -- (DOH, if x is a file-level/global variable, then in
--      --      x = delete_routine(x,rid)
--      --  we would get a reference count of 2 here anyway,
--      --  though PBR (on locals), and unnamed temps will
--      --  usually get us a refount of 1)
----DEV we may need to put this back, to daisy chain delete routines...
----        #ilASM{ mov edx,[o]
----                mov eax,[ebx+edx*4-8] -- (load refcount)
----                mov [refcount],eax }
----        if refcount!=1 then
----        if atom(o) then
----            --DEV ilasm{fld/StoreFlt} might be better
----            o += 1.1
----            o -= 1.1
------DEV tryme:
------          #ilASM{ [32]
------                      lea edi,[o]
------                      fld qword[ebx+edi*4]
------                      call :%pStoreFlt
------                  [64]
------                      lea rdi,[o]
------                      fld tbyte[rbx+rdi*4]
------                      call :%pStoreFlt
------                  []
------                }
----        else
----            o &= 'x'
----            o = o[1..-2]
----        end if
----        -- get the current delete_index, if any
--      -- get the current delete_routine, if any
--      #ilASM{ [32]
--                  mov edx,[o]
--                  mov ecx,[rid]
--                  mov eax,[ebx+edx*4-4]
--                  mov esi,[ebx+edx*4-4]
--                  and eax,0x00FFFFFF
----                    mov [delete_index],eax
--                  jz @f
----                        :e??dras
--                      int3
--                @@:
--                  or ecx,esi  -- (combine rid and type byte)
--                  mov [ebx+edx*4-4],ecx
--              [64]
--                  mov rdx,[o]
--                  mov rcx,[rid]
--                  mov rax,[rbx+rdx*4-8]
--                  mov rsi,[rbx+rdx*4-8]
----                    and rax,0x00FFFFFFFFFFFFFF
--                  shl rax,8
--                  or rcx,rsi  -- (combine rid and type byte)
--                  shr rax,8
----                    mov [delete_index],rax
--                  jz @f
----                        :e??dras
--                      int3
--                @@:
--                  mov [rbx+rdx*4-8],rax
--              []
--            }
--  end if
--
----DEV locking required... (enter_cs()/leave_cs())
----DEV/SUG: ditch freelist and keep them permanently. Allow multiple data values to "share" delete sets.
----            For performance, we may want to keep a chain of all other delete_sets[] that are one item
----            added from the current. (See the proof of concept I wrote for it, at the end.)
----    if delete_index=0 then
----        enter_cs()
----        if freelist then
----            delete_index = freelist
----            freelist = delete_sets[freelist]
----            delete_sets[delete_index] = {}
----        else
----            delete_sets = append(delete_sets,{})
----            delete_index = length(delete_sets)
------          if machine=32 then
----            if delete_index>#FFFFFF then ?9/0 end if    -- (limit of 16,777,215 blown)
------          else --machine=64
------          if delete_index>#FFFFFFFFFFFFFF then ?9/0 end if    -- (limit of <lots> blown)
------          end if
----        end if
----        leave_cs()
--      -- store the delete index
----        #ilASM{ [32]
----                    mov edx,[o]
------                  mov ecx,[delete_index]
----                    mov ecx,[rid]
----                    mov eax,[ebx+edx*4-4]
----                    or eax,ecx  -- (combine delete_index and type byte)
----                    mov [ebx+edx*4-4],eax
----                [64]
----                    mov rdx,[o]
------                  mov rcx,[delete_index]
----                    mov rcx,[rid]
----                    mov rax,[rbx+rdx*4-8]
----                    or rax,rcx  -- (combine delete_index and type byte)
----                    mov [rbx+rdx*4-8],rax
----              }
----    end if
----    enter_cs()
----    delete_sets[delete_index] &= rid
----    leave_cs()
--  return o
--end function


--function fdelete_routine(object o, integer rid)
--/*
procedure :%opDelRtn(:%)
end procedure -- (for Edita/CtrlQ)
--*/
--!/*
--DEV:
#ilASM{ jmp :!opCallOnceYeNot
--#ilASM{ jmp :fin

    :%opDelRtn
--------------
        [32]
            -- calling convention
            --  lea edi,[res]       -- result location
            --  mov esi,[o]         -- (opUnassigned)
            --  mov eax,[rid]       -- (opUnassigned)
            --  call :%opDelRtn     -- [edi]:=delete_routine(esi,eax)
            cmp esi,h4
            jge @f
                -- integer, promote to atom:
                fild dword[edi]
                fldpi                   -- (any non-integer value would do)
                call :%pStoreFlt        -- (preserves all registers)
                mov esi,[edi]
                fstp qword[ebx+esi*4]
                or [ebx+esi*4-4],eax    -- (combine rid and type byte)
                ret
          @@:
                mov ecx,[ebx+esi*4-4]   -- type/delete_routine
                cmp eax,-1              -- nb 0 is allowed
                jne @f
                    -- :e72iri(edi)     -- (invalid routine id)
                    pop edx
                    mov al,72           -- e72iri(edi)
                    mov edi,eax
                    sub edx,1
                    jmp :!iDiag
                    int3
              @@:
--              and ecx,0x00FFFFFF
                test ecx,0x00FFFFFF
                jz @f
                    and ecx,0xFF000000
                    cmp eax,0
                    je @f
                        -- e123dras     -- (delete routine already set)
                        pop edx
                        mov al,123      -- e123dras
                        sub edx,1
                        jmp :!iDiag
                        int3
              @@:
--              or [ebx+esi*4-4],eax    -- (combine rid and type byte)
                or ecx,eax              -- (combine rid and type byte)
                mov edx,[edi]
                mov [ebx+esi*4-4],ecx
                add dword[ebx+esi*4-8],1    -- incref
                mov [edi],esi
                cmp edx,h4
                jle @f
                    sub dword[ebx+edx*4-8],1
                    jz :%pDealloc
              @@:
                ret
        [64]
            -- calling convention
            --  lea rdi,[res]       -- result location
            --  mov rsi,[o]         -- (opUnassigned)
            --  mov rax,[rid]       -- (opUnassigned)
            --  call :%opDelRtn     -- [rdi]:=delete_routine(rsi,rax)
            mov r15,h4
            cmp rsi,r15
            jge @f
                -- integer, promote to atom:
                fild qword[rdi]
                fldpi                   -- (any non-integer value would do)
                call :%pStoreFlt        -- (preserves most registers)
                mov rsi,[rdi]
                fstp tbyte[rbx+rsi*4]
--              mov rax,[rbx+rsi*4-8]
--              or rax,rcx  -- (combine rid and type byte)
--              mov [rbx+rdx*4-8],rax
                or [rbx+rsi*4-8],rax
                ret
          @@:
                mov rcx,[rbx+rsi*4-8]   -- type/delete_routine
                cmp rax,-1              -- nb 0 is allowed
                jne @f
                    -- :e72iri(edi)     -- (invalid routine id)
                    pop rdx
                    mov al,72           -- e72iri(edi)
                    mov rdi,rax
                    sub rdx,1
                    jmp :!iDiag
                    int3
              @@:
--              shl rcx,8
                mov rdx,rcx
                shl rdx,8
                jz @f
--                  mov rcx,[rbx+rsi*4-8]   -- type/delete_routine
                    shr rcx,56
                    shl rcx,56
                    cmp rax,0
                    je @f
                        -- e123dras     -- (delete routine already set)
                        pop rdx
                        mov al,123      -- e123dras
                        sub rdx,1
                        jmp :!iDiag
                        int3
              @@:
--              or [rbx+rsi*4-8],rax    -- (combine rid and type byte)
                or rcx,rax
                mov rdx,[rdi]
                mov [rbx+rsi*4-8],rcx
                add qword[rbx+rsi*4-16],1   -- incref
                mov r15,h4
                mov [rdi],rsi
                cmp rdx,r15
                jle @f
                    sub qword[rbx+rdx*4-16],1
                    jz :%pDealloc
              @@:
                ret
--  return o
--end function
--<
        []

--DEV obviously this becomes the backend entry point!!
--procedure fdelete(object o)
--/*
procedure :%opDelete(:%)
end procedure -- (for Edita/CtrlQ)
--!*/
    :%opDelete
-------------
        [32]
            -- calling convention
            --  mov eax,[o]     -- (opUnassigned)
--?? mov [o],h4 (no)
            --  call :%opDelete     -- delete(eax)
            cmp eax,h4
            jl @f
                add dword[ebx+eax*4-8],1        -- incref
          @@:
            push eax                            --[1] o
            mov edx,routine_id(fdelete)         -- mov edx,imm32 (sets K_ridt)
--          mov edx,routine_id(delete)          -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[fdelete][S_Ltot])
            call :%opFrame
--          mov edx,[esp+4]
            pop dword[ebp]                      --[1] o
            pop edx
--EXCEPT
--          mov dword[ebp+16],:delret           -- return address
--          mov dword[ebp+28],:delret           -- return address
            mov dword[ebp+28],edx               -- return address
            mov dword[ebp+12],edx               -- called from address
            jmp $_il                            -- jmp code:fdelete
        [64]
            -- calling convention
            --  mov rax,[o]         -- (opUnassigned)
            --  call :%opDelete     -- delete(rax)
            mov r15,h4
            cmp rax,r15
            jl @f
                add qword[rbx+rax*4-16],1
          @@:
            push rax                            --[1] o
            mov rdx,routine_id(fdelete)         -- mov rdx,imm32 (sets K_ridt)
--          mov rdx,routine_id(delete)          -- mov rdx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                      -- mov rcx,imm32 (=symtab[fdelete][S_Ltot])
            call :%opFrame
--          mov rdx,[rsp+8]
            pop qword[rbp]                      --[1] addr
            pop rdx
--EXCEPT
--          mov qword[rbp+32],:delret           -- return address
--          mov qword[rbp+56],:delret           -- return address
            mov qword[rbp+56],rdx               -- return address
            mov qword[rbp+24],rdx               -- called from address
            jmp $_il                            -- jmp code:fdelete
        []
--        ::delret
--          ret
--    ::fin
    }
--!*/



--DEV replace with:
--/*
procedure deletep()
-- Backend entry point (see pHeap.e/%:pDealloc)
-- (the point of this, rather than calling delete() directly
--  from below, is to have the following thread-safe local o)
object o
    #ilASM{ pop [o] }       -- (I hope this works!)
    delete(o)
    #ilASM{ [32]            -- (zero, prevent dealloc)
                mov [o],ebx
            [64]
                mov [o],rbx
          } 
end procedure

--  #ilASM{ jmp:%delfin
----            align 16
--          :%pDelRtn   -- invoked from pHeap.e [DEV to go]
--          [32]
--              push edx
--          [64]
--              push rdx
--        }
--          deletep()
--  #ilASM{ :%delfin }
--*/

-- Backend entry point. This must be a function so we can call_back() it.
-- (this is itself a work around to call_back only accepting atoms)
--DEV tricks from pthread.e... (atom a contains a reference)
--DEV certainly not thread safe at this point (cos of opDelRef)
--function deletef()
--object o
--  #ilASM{ call %opDelRef  -- load the parameter
--          mov [o],eax }
--  delete(o)
--  #ilASM{ mov [o],ebx } -- (zero, prevent dealloc)
--  return 0
--end function

--procedure setup()
--
----/* --DEV sort this out: (update: I think :%pDelRtn does it...)
---- tell the backend where it (deletef) is.
---- NB Phix itself (and for that matter any builtins it uses) should not use this...
----    (just needs a stack of them, but that ain't implemented yet, fix when we do ebp)
----    (Problem is that ebp->symtab must be right for any routine-ids to work, though
----     provided we invoke the appropriate callback that should all be taken care of.
----     But one callback for p.exe and the test.exw it is running spells disaster.)
--integer delcb
--  delcb = call_back(routine_id("deletef"))
--  #ilASM{ lea edi,[delcb]                         -- mov edi,addr delete callback
--          call %opDelRtn}                         -- save delete callback
----    delete_sets = {}
----*/
--  enter_cs()
--  if dinit=0 then
--      delete_sets = {{0}} -- make 1 illegal (see pemit.e)
--      dinit = 1
--  end if
--  leave_cs()
--end procedure
--setup()

--/*
-- proof of concept, as mentioned above, for delete_routine:
--  find: 97.7s, plus/next: 0.19s, so ~500x faster!
sequence ds = {{}}  -- or maybe {{0},{}} with root=2? (or as-is and drop that 1 thing, never caught anything anyway)
sequence plus = {0} -- start of list of entries with one item added
sequence next = {0} -- next in list of entries with one item added

function add(integer idx, integer i)
sequence newkey = append(ds[idx],i)
integer res
    if 0 then
        res = find(newkey,ds)
        if res=0 then
            ds = append(ds,newkey)
            res = length(ds)
        end if
    else
        res = plus[idx]
        while res do
            if ds[res]=newkey then exit end if
            res = next[res]
        end while
        if res=0 then
            ds = append(ds,newkey)
            res = length(ds)
            plus = append(plus,0)
            next = append(next,plus[idx])
            plus[idx] = res
        end if
    end if
    return res
end function

integer root=1,ai,aj,ak

constant LIM=50
puts(1,"started\n")
atom t0 = time()
for times=1 to 2 do
    for i=1 to LIM do
?i
        ai = add(root,i)
        for j=1 to LIM do
            aj = add(ai,j)
            for k=1 to LIM do
                ak = add(aj,k)
            end for
        end for
    end for
end for
printf(1,"%d items added in %3.2f\n",{length(ds),time()-t0})
if getc(0) then end if
abort(0)
--*/
