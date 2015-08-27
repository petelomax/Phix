--
-- pabs.e
--
--  Phix implementation of abs()
--
-- This is an auto-include file; there is no need to manually include
--  it, unless you want a namespace.
--

global function abs(object o)
object oi
    if atom(o) then
        if o>=0 then
            return o
        else
            return -o
        end if
    end if
    for i = 1 to length(o) do
        oi = o[i]
        if atom(oi) then
            if oi<0 then
                o[i] = -oi
            end if
        else
            o[i] = abs(oi)
        end if
    end for
    return o
end function


