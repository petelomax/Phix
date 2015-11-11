--
-- allocate_data.e
--

--export procedure deallocate(atom addr)
--  ifdef DATA_EXECUTE and WINDOWS then
--      if dep_works() then
--          if c_func( VirtualFree_rid, { addr, 1, MEM_RELEASE } ) then end if
--          return
--      end if
--  end ifdef
--  machine_proc(M_FREE, addr)
--end procedure
--FREE_RID = routine_id("deallocate")

--global function allocate_data(positive_int n, integer cleanup = 0)
global function allocate_data(integer n, integer cleanup=0)
    if cleanup then
--DEV ...
--/**/  return 9/0
--/**/  return delete_routine(allocate(n), FREE_RID)
--/*
        return delete_routine( machine_func(M_ALLOC, n ), FREE_RID )
--*/
    else
--/**/  return allocate(n)
--/*
        return machine_func(M_ALLOC, n)
--*/
    end if
end function


