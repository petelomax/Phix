--
-- pFind.e
-- =======
--
--  (Temporary) hll implementation of find()
--  There is an equivalent commented-out backend/asm version in pJcc.e, conversion of which is yet to be completed.
--
without trace
--without debug

global function find(object x, sequence s, integer start=1)
    for i=start to length(s) do
        if x=s[i] then return i end if
    end for
    return 0
end function

global function rfind(object x, sequence s, integer start=-1)
    if start=-1 then start = length(s) end if
    for i=start to 1 by -1 do
        if x=s[i] then return i end if
    end for
    return 0
end function

