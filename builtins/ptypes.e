--
-- builtins/ptypes.e
--
--  NB not an autoinclude, but used/included by pGUI.e, mpfr.e, and structs.e.
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

-- Note: psym.e aliases bool to integer, without the 0|1 validation
--global type bool(object o)
global type boolean(object o)
    return integer(o) and (o=true or o=false)
end type

