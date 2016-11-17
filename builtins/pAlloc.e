--
-- pAlloc.e
-- ========
--
--  Phix implementation of allocate() and free(). 
--  The low-level routines %pAlloc and %pFree in pHeap.e are responsible for allocating and freeing memory,
--  whereas these hll routines (not part of the VM) are responsible for dealing with delete_routine needs.
--

include builtins\VM\pHeap.e

global procedure free(object addr)
    if sequence(addr) then
        for i=1 to length(addr) do
            free(addr[i])
        end for
    else
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

global function allocate(atom size, integer cleanup = 0)
atom res
    if size<0 or size!=floor(size) then
        #ilASM{
            [32]
                mov ecx,1           -- no of frames to pop to obtain an era (>=2)
            [64]
                mov rcx,1           -- no of frames to pop to obtain an era (>=2)
            []
                mov al,37           -- e37atambpi (argument to allocate must be positive integer)
                jmp :!fatalN        -- fatalN(level,errcode,ep1,ep2)
              }
    end if
    #ilASM{
        [32]
            mov ecx,[size]
            lea edi,[res]
--          mov edx,[ebp+16]        -- return address
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

global function allocate_data(integer size, integer cleanup = 0)
    return allocate(size,cleanup)
end function

global function allocate_string(sequence s, integer cleanup = 0)
-- create a C-style null-terminated string in memory
atom mem

    mem = allocate(length(s)+1) -- Thanks to Igor
    if mem then
        poke(mem, s)
        poke(mem+length(s), 0)  -- Thanks to Aku
        if cleanup then
--          mem = delete_routine(mem, FREE_RID)
            mem = delete_routine(mem, r_free)
        end if
    end if
    return mem
end function

