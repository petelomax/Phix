--
-- peekns.e
-- ========
--
--  Implements peekns() and peeknu(), simple hll wrappers to peekNS() with defaulted parameters.
--  This is an auto-include, there should be no need to manually include this file.
--
--  In some future release, mapping to peekNS may occur in the compiler front end (in pmain.e), 
--  thereby eliminating any performance overhead, or perhaps directly in VM\pMap.e, if I ever 
--  decide that having yet 2 more opcodes is worth it (and some real program arises that could 
--  really benefit from it). For now at least, these are more convenient but less performant.
--

global function peekns(object addr, integer size=machine_word(), bool signed=true)
    return peekNS(addr,size,signed)
end function

global function peeknu(object addr, integer size=machine_word(), bool signed=false)
    return peekNS(addr,size,signed)
end function

global procedure poken(object addr, object v, integer size=machine_word())
    pokeN(addr,v,size)
end procedure
