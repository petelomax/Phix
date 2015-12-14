--
-- pAlloc.e
-- ========
--
--  NB: There are untested pure-inline versions of these routines in pHeap.e, see :%pAlloc and :%pFree [DEV]
--      (allocate_data() etc might be an issue)
--
include builtins\VM\pHeap.e

--DEV having these be hll likely messes up putting pHeap.e into the opcode table.. (inline versions have now been written above) [or just split these off into pAlloc.e...]

global procedure free(atom addr)
integer iaddr -- addr/4, to simplify handling
    if and_bits(addr,#03) then ?9/0 end if  --DEV ima (invalid memory address)
--DEV test these (carry on allocating until we get something above #7FFFFFFF) and pick one!
--    (I expect the first will be fine, but we should test, and if we do get sign issues, one of the others should cope)
--    (or use cmp h4 etc, as per eg builtins\VM\_readme.txt)
    iaddr = floor(addr/4) -- [same for 32 and 64 bit]
--  addr = and_bits(floor(addr/4),#3FFFFFFF)
--  addr = floor(and_bits(addr,#3FFFFFFF)/4)
    #ilASM{
        [32]
            mov eax,[iaddr]
            mov edx,[ebp+16]
            shl eax,2
            sub eax,4
            mov ecx,[eax]   -- retrieve size
        [64]
            mov rax,[iaddr]
            mov rdx,[rbp+32]
            shl rax,2
            sub rax,8
            mov rcx,[rax]   -- retrieve size
        []
            call :%pFreePool
          }
end procedure
constant r_free = routine_id("free")
    
global function allocate(integer size, integer cleanup = 0)
integer r4  -- addr/4 (to ensure it is an integer)
--atom res
    #ilASM{
        [32]
            mov ecx,[size]
            mov edx,[ebp+16]
            add ecx,4   -- for pRoot (I think I mean nSize)
            call :%pGetPool
            -- result is edx bytes at eax, but we use first 4 bytes to save the size, for free().
            test eax,eax
            jz @f
                mov [eax],edx -- so this is 16 when returning 12 bytes of useable space (at eax+4).
                add eax,4   -- for nSize (see also "Minor point" above)
-- it might be wiser to use StoreFlt: (untested)
--      @@:
--          push ebx
--          push eax
--          fild qword[esp]
--          add esp,8
--          lea edi,[res]
--          call :%pStoreFlt        -- [edi]:=st0
                shr eax,2
         @@:
            mov [r4],eax
        [64]
            mov rcx,[size]
            mov rdx,[rbp+32]
            add rcx,8   -- for pRoot (I think I mean nSize)
            call :%pGetPool
            -- result is rdx bytes at rax, but we use first 8 bytes to save the size, for free().
            test rax,rax
            jz @f
                mov [rax],rdx -- so this is 28 when returning 20 bytes of useable space (at rax+8).
                add rax,8   -- for nSize (see also "Minor point" above)
-- it might be wiser to use StoreFlt: (untested)
--      @@:
--          push rbx
--          push rax
--          fild tbyte[rsp]
--          add rsp,16
--          lea rdi,[res]
--          call :%pStoreFlt        -- [edi]:=st0
                shr rax,2
         @@:
            mov [r4],rax
        []
          }
    if cleanup and r4!=0 then
        return delete_routine(r4*4,r_free)
    end if
    return r4*4
--  return res
end function

global function allocate_data(integer size, integer cleanup = 0)
    return allocate(size,cleanup)
end function


