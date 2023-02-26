--
-- builtins/ptypes.e
--
--  NB not an autoinclude, but used/included by [x]pGUI.e, mpfr.e, pFilter.e, and structs.e.
--

-- used by IupSetAttribute, IupSetGlobal, cdCreateCanvas, mpz_import:
global type atom_string(object o)
    return string(o) 
        or (integer(o) and o>=NULL) 
        or (atom(o) and o>=NULL and o=floor(o))
end type

global type rid_string(object o)
    return string(o) 
        or (integer(o) and o>15)
end type

global type nullable_string(object o)
    return string(o) or o=NULL
end type

-- Note: psym.e aliases bool to integer, without the true|false aka 0|1 validation
global type boolean(object o)
    return integer(o) and (o=true or o=false)
end type

--DEV/erm, see if we can get by (in xpGUI.e) without this...
--global function get_raw_string_ptr(string s)
----
---- Returns a raw string pointer for s, somewhat like allocate_string(s), but using the existing memory.
---- NOTE: The return is only valid as long as the value passed as the parameter remains in scope.
----         In particular, callbacks must make a semi-permanent copy somewhere other than locals/temps.
----
--atom res
--  #ilASM{
--      [32]
--          mov eax,[s]
--          lea edi,[res]
--          shl eax,2
--      [64]
--          mov rax,[s]
--          lea rdi,[res]
--          shl rax,2
--      []
--          call :%pStoreMint
--        }
--  return res
--end function

