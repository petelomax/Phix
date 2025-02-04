-- (c) Copyright - See License.txt
--
--****
-- == Pseudo Memory
--
--
-- One use is to emulate PBR, such as Euphoria's map and stack types.
--
-- <<LEVELTOC depth=2>>

namespace eumem

--**
-- The (pseudo) RAM heap space. Use [[:malloc]] to gain ownership to a heap location
-- and [[:memfree]] to release it back to the system.
export sequence ram_space = {}

integer ram_free_list = 0
--integer free_rid

--**
-- Deallocate a block of (pseudo) memory
--
-- Parameters:
-- # ##mem_p## : The handle to a previously acquired [[:ram_space]] location.
--
-- Comments:
-- This allows the location to be used by other parts of your application. You 
-- should no longer access this location again because it could be acquired by
-- some other process in your application.  This routine should only be called
-- if you passed 0 as ##cleanup_p## to [[:malloc]].
--
-- Example 1:
-- <eucode>
--  my_spot = malloc(1,0)
--  ram_space[my_spot] = my_data
--      -- . . . do some processing  . . . 
--  memfree(my_spot)
-- </eucode>

--DEV PL renamed as memfree...
export procedure memfree(atom mem_p)
    if mem_p>=1 and mem_p<=length(ram_space) then
        ram_space[mem_p] = ram_free_list
        ram_free_list = floor(mem_p)
    end if
end procedure
constant free_rid = routine_id("memfree")

--**
-- Allocate a block of (pseudo) memory
--
-- Parameters:
-- # ##mem_struct_p## : The initial structure (sequence) to occupy the allocated
-- block. If this is an integer, a sequence of zero this long is used. The default
-- is the number 1, meaning that the default initial structure is {0}
-- # ##cleanup_p## : Identifies whether the memory should be released automatically
-- when the reference count for the handle for the allocated block drops to
-- zero, or when passed to ##delete()##.  If 0, then the block must be freed
-- using the [[:memfree]] procedure.
--
-- Returns:
-- A **handle**, to the acquired block. Once you acquire this, you can use it as you
-- need to.  Note that if ##cleanup_p## is 1, then the variable holding the 
-- handle must be capable of storing an atom as a double floating point value
-- (i.e., not an integer).
-- 
-- Example 1:
-- <eucode>
--  my_spot = malloc()
--  ram_space[my_spot] = my_data
-- </eucode>

export function malloc(object mem_struct_p = 1, integer cleanup_p = 1)
integer temp_

    if atom(mem_struct_p) then
        mem_struct_p = repeat(0, mem_struct_p)
    end if
    if ram_free_list=0 then
        ram_space = append(ram_space, mem_struct_p)
        if cleanup_p then
            return delete_routine(length(ram_space), free_rid)
        else
            return length(ram_space)
        end if
    end if

    temp_ = ram_free_list
    ram_free_list = ram_space[temp_]
    ram_space[temp_] = mem_struct_p

    if cleanup_p then
        return delete_routine(temp_, free_rid)
    else
        return temp_
    end if
end function

--**
-- Validates a block of (pseudo) memory
--
-- Parameters:
-- # ##mem_p## : The handle to a previously acquired [[:ram_space]] location.
-- # ##mem_struct_p## : If an integer, this is the length of the sequence that
-- should be occupying the ram_space location pointed to by ##mem_p##.
--
-- Returns:
-- An **integer**,\\
-- 0 if either the ##mem_p## is invalid or if the sequence at that location is
-- the wrong length.\\
-- 1 if the handle and contents is okay.
--
-- Comments:
-- This can only check the length of the contents at the location. Nothing else
-- is checked at that location.
--
-- Example 1:
-- <eucode>
--  my_spot = malloc()
--  ram_space[my_spot] = my_data
--  . . . do some processing  . . 
--  if valid(my_spot, length(my_data)) then
--      memfree(my_spot)
--  end if
-- </eucode>

export function valid(object mem_p, object mem_struct_p = 1)
--PL
    if not integer(mem_p)
    and atom(mem_p)
    and mem_p>=1
    and mem_p<=#3FFFFFFF then
        mem_p = floor(mem_p)
    end if
    if not integer(mem_p)
    or mem_p<1
    or mem_p>length(ram_space) then
        return 0
    end if

    if sequence(mem_struct_p) then return 1 end if

    if atom(ram_space[mem_p]) then
        if mem_struct_p>=0 then
            return 0
        end if

        return 1
    end if

    if length(ram_space[mem_p])!=mem_struct_p then
        return 0
    end if

    return 1
end function
