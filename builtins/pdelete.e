DEAD?
--
-- pdelete.e
--
-- Implements delete_routine and delete (auto-include)
--
--  This file is loaded automatically by Phix as and when needed. There is no
--  need to manually include this file (unless you want a namespace on it).
--DEV should really be moved into builtins\VM
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
--  RDS Eu, or I should be saying OpenEuphoria now, calls del with {7}, btw.
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
-- Finally, some technical points:
--
-- The abort() routine stops program execution immediately, without calling
--  any delete_routines. A "table={}" statement or similar may be required to
--  force them to be called. Just setting a delete_routine does not guarantee
--  that it will be called, but instead that it will be called *when and if*
--  the value it is associated with is deleted/deallocated. You should still
--  plan for and code some manual cleanup; delete_routine operates more as a
--  guard against data being "lost", than guaranteeing a call occurs.
--  Typically I use/recommend an "Abort()" routine wrapper for abort.
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
--          thing = delete_routine(thing,rid)   -- (typecheck error)
--          if thing=5 ...
--      or
--          atom thing
--          thing = 5
--          thing = delete_routine(thing,rid)
--          if thing=5 ...                      -- (will not work)
--      use
--          sequence thing
--          thing = {5}
--          thing = delete_routine(thing,rid)
--          if thing[1]=5 ...                   -- (this is fine)
--

integer freelist = 0
sequence delete_sets        -- {{rid,rid,rid,...}}
integer dinit = 0

global function delete_routine(object o, integer rid)
--
-- attach rid to the data in o
--
integer delete_index
--, refcount
    if dinit=0 then setup() end if
    if integer(o) then
        -- promote to atom
        #ilASM{ lea edi,[o]
--DEV (temp, and see 6 lines below)
                lea edx,[o]
                fild dword[o]
                fldpi                   -- (any non-integer value would do)
                call %opMovbi           -- call StoreFlt
--DEV these should probably all be edi...
                mov edx,[edx]
                fstp qword[ebx+edx*4] }
        delete_index = 0
    else
        -- first, force a refcount of 1
        -- (DOH, if x is a file-level/global variable, then in
        --      x = delete_routine(x,rid)
        --  we would get a reference count of 2 here anyway)
--      #ilASM{ mov edx,[o]
--              mov eax,[ebx+edx*4-8] -- (load refcount)
--              mov [refcount],eax }
--      if refcount!=1 then
        if atom(o) then
            --DEV ilasm{fld/StoreFlt} might be better (see pthread)
            o += 1.1
            o -= 1.1
--          #ilASM{ mov eax,[o]
--                  lea edi,[o]
--                  lea edx,[o]
--                  fld qword[ebx+edx*4]
--                  call %opMovbi}          -- call StoreFlt
        else
            o &= 'x'
            o = o[1..-2]
        end if
        -- get the current delete_index, if any
        #ilASM{ mov edx,[o]
                mov eax,[ebx+edx*4-4]
                and eax,0x00FFFFFF
                mov [delete_index],eax }
    end if
--DEV locking required...
    if delete_index=0 then
        if freelist then
            delete_index = freelist
            freelist = delete_sets[freelist]
            delete_sets[delete_index] = {}
        else
            delete_sets = append(delete_sets,{})
            delete_index = length(delete_sets)
            if delete_index>#FFFFFF then ?9/0 end if    -- (limit of 16,777,215 blown)
        end if
        -- store the delete index
        #ilASM{ mov edx,[o]
                mov ecx,[delete_index]
                mov eax,[ebx+edx*4-4]
                or eax,ecx  -- (combine delete_index and type byte)
                mov [ebx+edx*4-4],eax }
    end if
    delete_sets[delete_index] &= rid
    return o
end function

global procedure delete(object o)
integer delete_index
    if not integer(o) then
        -- get the current delete_index, if any
        #ilASM{ mov edx,[o]
                mov eax,[ebx+edx*4-4] -- (load index & type byte)
                mov ecx,eax
                and eax,0x00FFFFFF  -- (keep delete_index only)
                and ecx,0xFF000000  -- (keep type byte only)
                mov [ebx+edx*4-4],ecx -- (zeroise delete_index on data)
                mov [delete_index],eax }

        if delete_index then
            for i=length(delete_sets[delete_index]) to 1 by -1 do
                call_proc(delete_sets[delete_index][i],{o})
            end for
--DEV locking required (possibly just inside the "if not integer" test)
            delete_sets[delete_index] = freelist
            freelist = delete_index
        end if
    end if
end procedure

--DEV replace with:
--/*
procedure deletep()
-- Backend entry point (see pHeap.e/%:pDealloc)
object o
    #ilASM{ pop [o] }       -- (I hope this works!)
    delete(o)
    #ilASM{ mov [o],ebx }   -- (zero, prevent dealloc)
end procedure

    #ilASM{     jmp:%delfin
            :%DelRtn
--              and edx,#3FFFFFFF       ; (raw mem/4)
                push edx }
            deletep()
    #ilASM{ :%delfin }
--*/

-- Backend entry point. This must be a function so we can call_back() it.
-- (this is itself a work around to call_back only accepting atoms)
--DEV tricks from pthread.e... (atom a contains a reference)
--DEV certainly not thread safe at this point (cos of opDelRef)
function deletef()
object o
    #ilASM{ call %opDelRef  -- load the parameter
            mov [o],eax }
    delete(o)
    #ilASM{ mov [o],ebx } -- (zero, prevent dealloc)
    return 0
end function

procedure setup()
-- tell the backend where it (deletef) is.
-- NB Phix itself (and for that matter any builtins it uses) should not use this...
--  (just needs a stack of them, but that ain't implemented yet, fix when we do ebp)
--  (Problem is that ebp->symtab must be right for any routine-ids to work, though
--   provided we invoke the appropriate callback that should all be taken care of.
--   But one callback for p.exe and the test.exw it is running spells disaster.)
integer delcb
    delcb = call_back(routine_id("deletef"))
    #ilASM{ lea edi,[delcb]                         -- mov edi,addr delete callback
            call %opDelRtn}                         -- save delete callback
--  delete_sets = {}
    delete_sets = {{0}} -- make 1 illegal (pemit.e puts a 1 on constants etc)
    dinit = 1
end procedure
setup()
