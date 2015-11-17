--
-- allocate_data.e
--

procedure deallocate(atom addr)
--  ifdef DATA_EXECUTE and WINDOWS then
--      if dep_works() then
--          if c_func( VirtualFree_rid, { addr, 1, MEM_RELEASE } ) then end if
--          return
--      end if
--  end ifdef
--  machine_proc(M_FREE, addr)
    free(addr)
end procedure
constant r_dealloc = routine_id("deallocate")

global function allocate_data(integer n, integer cleanup=0)
    if n<=0 then ?9/0 end if
    if cleanup then
        return delete_routine(allocate(n), r_dealloc)
    end if
    return allocate(n)
end function


