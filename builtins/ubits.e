--
-- builtins/ubits.e
--
--  unsigned bit operations (autoinclude).
--
--  :%opAndBits, ie and_bits(), returns signed results, mainly because it is part of 
--  builtins/pMaths.e and uses the same logic as :%opAdd, etc. Ditto or/xor/not_bits().
--  However many (esp cryptographic) functions require unsigned results, which is 
--  where these routines can come in handy.
--
--  These routines are implemented directly in p2js.js, using ">>>0" rather than poke/peek.
--
constant W = machine_word()
atom pMem = NULL

function unsign(atom a)
    if pMem=NULL then
        pMem = allocate(W)
    end if
    pokeN(pMem,a,W)
    return peekNS(pMem,W,false)
end function

global function and_bitsu(atom x1, x2)
    return unsign(and_bits(x1,x2))
end function

global function or_bitsu(atom x1, x2)
    return unsign(or_bits(x1,x2))
end function

global function xor_bitsu(atom x1, x2)
    return unsign(xor_bits(x1,x2))
end function

global function not_bitsu(atom x1)
    return unsign(not_bits(x1))
end function

