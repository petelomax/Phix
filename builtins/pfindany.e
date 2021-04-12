--
-- pfindany.e
--
--  Phix implementation of find_any()
--
-- This is an auto-include file; there is no need to manually include
--  it, unless you want a namespace.
--

global function find_any(sequence needles, haystack, integer start=1)
    for i=start to length(haystack) do
        if find(haystack[i],needles) then
            return i
        end if
    end for
    return 0
end function

