--
-- pAlloc.e
-- ========
--
--  Phix implementation of allocate() and free(). 
--  The low-level routines %pAlloc and %pFree in pHeap.e are responsible for allocating and freeing memory,
--  whereas these hll routines (not part of the VM) are responsible for dealing with delete_routine needs.
--
--DEV/DOCS: Sort out safe.e: add a constant to pAlloc.e of SAFETY=NORMAL/DYNAMIC/VALIDATE.
-- NORMAL: best performance, no checking
-- DYNAMIC: can be enabled per-application (via allocation_safety(bool active=false)
-- VALIDATE: always checked.
--
-- The argument to allocation_safety() can be true/false.
-- When SAFETY=NORMAL (as distributed) the compiler should not emit any extra code or runtime tests,
--  resulting in the best possible performance, and will ignore any calls to alloction_safety().
-- Note that Phix does not have the safe.e of Euphoria, to replace machine.e, largely because of 
--  the auto-include system, so this (allocation_safety()) is effectively the replacement.
--/*
constant NORMAL = #00,
         DYNAMIC = #01,
         VALIDATE = #02,
         SAFETY = NORMAL -- (edit to DYNAMIC/VALIDATE as rqd)

bool safety = false

global procedure allocation_safety(bool active=false)
    safety = active
end procedure
--*/

include builtins\VM\pHeap.e
--include builtins\VM\pDelete.e

--SUG: global type heap_ptr(object addrs). 
--You can think of this as a non-destructive form of free().
--NOTE: this cannot use any of the code in :%pFree, you are move likely to find
--      the sort of code needed in that MemChk thing, start from pGtcb...
--      *** OH - this might just help with the linux problem ***
--Returns false for string, sequence, and fractional atom, otherwise it performs 
--much of the same initial processing that free(o) would, scanning the internal 
--tcb (thread control block) and superblock chains.
--Note that NULL is essentially granted a free pass (yields true), and that after 
--atom a = allocate(16), heap_ptr(a) yields true but heap_ptr(a+8) yields false.
--Also note that heap() is /not/ an autoinclude, you need to include pAlloc.e   [testme]
--unless, as is quite likely, it happens to have been dragged in already.
--Also note that the status of heap_ptr() can change "under your feet", such that
--heap_ptr a = allocate(16); free(a) does /not/ trigger a type check even though
--heap_ptr(a) would now yield false, and further heap_ptr b = a is likely to have
--the normal typecheck you would expect on b optimised away; you may need to get
--the value from a into and back out of an atom, to ensure it actually checks.
--No locking whatsoever occurs, hence this routine is not deemed thread-safe.
--[When/if implemented, update allocate/free docs.]
--/*
--  global type heap_ptr(object addr)
--  integer res = atom(addr)
--  atom pGtcb
--      if res then
--          #ilASM{
--              [32]
--                  mov eax,[addr]
--                  mov edx,[ebp+12]
--                  lea edi,[res]
--              [64]
--                  mov rax,[addr]
--                  mov rdx,[rbp+24]
--                  lea rdi,[res]
--              []
--                  call :%pHeapPtr
--or...
--                  call :%pGetpGtcb
--              [32]
--                  move [pGtcb],eax
--              [64]
--                  move [pGtcb],rax
--              []
--                }
--          while pGtcb do
--              pGtcb = 
--          end while
--      end if
--      return res
--  end type
--*/

global procedure free(object addr)
    if sequence(addr) then
        if string(addr) then ?9/0 end if
        for i=1 to length(addr) do
            atom ai = addr[i]   -- (deliberate typecheck)
            free(ai)
        end for
--14/3/2020:
--  else
    elsif addr!=NULL then
--      if not heap_ptr(addr) then ?9/0 end if (maybe/not)
-- 6/8/19:
        if not integer(addr) then
            integer rid
            -- get the current delete_routine, if any
            #ilASM{ [32]
                        mov edx,[addr]
                        mov eax,[ebx+edx*4-4] -- (load index & type byte)
                        mov ecx,eax
                        and eax,0x00FFFFFF  -- (keep delete_index only)
                        and ecx,0xFF000000  -- (keep type byte only)
                        mov [ebx+edx*4-4],ecx -- (zeroise delete_rtn on data)
                        mov [rid],eax
                    [64]
                        mov rdx,[addr]
                        mov rax,[rbx+rdx*4-8] -- (load index & type byte)
                        mov rcx,rax
--                      and rax,0x00FFFFFFFFFFFFFF  -- (keep delete_index only)
--                      and rcx,0xFF00000000000000  -- (keep type byte only)
                        shl rax,8
                        rol rcx,8
                        shr rax,8
                        shl rcx,32+24   -- (56, aka 64-8)
                        mov [rbx+rdx*4-8],rcx -- (zeroise delete_rtn on data)
                        mov [rid],rax
                  }
            -- Invoking free() on a value with delete_routine() in force just
            -- maps the call to delete(). It is assumed that will re-invoke 
            -- free() with the rtn field zeroed.
            if rid!=0 then
--              delete(addr) -- (no, we already zeroed it!)
                call_proc(rid,{addr})
                return
            end if
        end if
        #ilASM{
            [32]
                mov eax,[addr]
                mov edx,[ebp+12]
            [64]
                mov rax,[addr]
                mov rdx,[rbp+24]
            []
                call :%pFree
              }
    end if
end procedure
constant r_free = routine_id("free")

global function ffree(object addr)
    if sequence(addr) then
        if string(addr) then ?9/0 end if
        for i=1 to length(addr) do
            atom ai = addr[i]   -- (deliberate typecheck)
            addr[i] = NULL
            free(ai)
        end for
    elsif addr!=NULL then
        free(addr)
        addr = NULL
    end if
    return addr -- NULL or repeat(NULL,length(addr))
end function

global function allocate(integer size, bool cleanup=false)
atom res
    if size<0 then
        #ilASM{
            [32]
                mov ecx,1           -- no of frames to pop to obtain an era
            [64]
                mov rcx,1           -- no of frames to pop to obtain an era
            []
                mov al,37           -- e37atambpi (argument to allocate must be positive integer)
                jmp :!fatalN        -- fatalN(level,errcode,ep1,ep2)
              }
    end if
    #ilASM{
        [32]
            mov ecx,[size]
            lea edi,[res]
            mov edx,[ebp+12]        -- called from
        [64]
            mov rcx,[size]
            lea rdi,[res]
            mov rdx,[rbp+24]        -- called from
        []
            call :%pAlloc
          }
    if cleanup and res!=0 then
        res = delete_routine(res,r_free)
    end if
    return res
end function

global function allocate_word(atom v=0, bool cleanup=false, integer size=machine_word())
    atom res = allocate(size,cleanup)
    pokeN(res,v,size)
    return res
end function

global function allocate_data(integer size, bool cleanup=false)
    return allocate(size,cleanup)
end function

global function allocate_string(sequence s, bool cleanup=false)
-- create a C-style null-terminated string in memory
    atom mem = allocate(length(s)+1, cleanup) -- Thanks to Igor
    if mem then
        poke(mem, s)
        poke(mem+length(s), 0)  -- Thanks to Aku
    end if
    return mem
end function

global function allocate_wstring(sequence s, bool cleanup=false)
-- create a WideString (16 bits per char) null-terminated string in memory.
-- note: calling utf8_to_utf16() or utf32_to_utf16() first may be helpful.
    atom mem = allocate(length(s)*2+2, cleanup)
    if mem then
        poke2(mem, s)
        poke2(mem+length(s)*2, 0)
    end if
    return mem
end function

