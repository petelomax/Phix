--
-- psign.e
--
--  Phix implementation of sign()
--
-- This is an auto-include file; there is no need to manually include
--  it, unless you want a namespace.
--

global function sign(object o)
    if atom(o) then
        if o>0 then
            return +1
        elsif o<0 then
            return -1
        else
            return 0
        end if
    end if
    for i=1 to length(o) do
        o[i] = sign(o[i])
    end for
    return o
end function

