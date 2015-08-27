--
-- porall.e
--
--  Phix implementation of or_all()
--  This is an auto-include file.
--

global function or_all(object s)
-- or together all elements of a sequence
atom result, si
    if atom(s) then
        return s
    end if
    result = 0
    for i=1 to length(s) do
        si = s[i] -- (type check is deliberate)
        result = or_bits(result, si)
    end for
    return result
end function

