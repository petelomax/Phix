--
-- builtins\find.e (an autoinclude)
-- ===============
--
--  hll implementation of find()  (replaces builtins/VM/pFind.e)
--
without trace
--without debug

global function find(object needle, sequence haystack, integer start=1)
    if start<0 then start += length(haystack)+1 end if
    for i=start to length(haystack) do
        if needle=haystack[i] then return i end if
    end for
    return 0
end function

global function rfind(object needle, sequence haystack, integer start=-1)
    if start<0 then start += length(haystack)+1 end if
    for i=start to 1 by -1 do
        if needle=haystack[i] then return i end if
    end for
    return 0
end function

