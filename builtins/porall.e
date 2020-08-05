--
-- porall.e
--
--  Phix implementation of or_all()
--  This is an auto-include file.
--

global function or_all(object s)
-- or together all elements of a sequence
    atom result = 0
    if atom(s) then
        result = or_bits(result, s) -- (force 32bit)
    else
        for i=1 to length(s) do
            atom si = s[i] -- (type check is deliberate)
            result = or_bits(result, si)
        end for
    end if
    return result
end function

global function or_allu(object s)
-- or together all elements of a sequence
    atom result = 0
    if atom(s) then
        result = or_bitsu(result, s) -- (unsign if rqd)
    else
        for i=1 to length(s) do
            atom si = s[i] -- (type check is deliberate)
            result = or_bitsu(result, si)
        end for
    end if
    return result
end function

