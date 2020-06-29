--
-- builtins/pfindall.e
--
global function find_all(object needle, sequence haystack, integer start=1)
    sequence res = {}
--  if start<0 then start += length(haystack)+1 end if -- (done by find() anyway)
    while true do
        start = find(needle, haystack, start)
        if start=0 then exit end if
        res &= start
        start += 1
    end while
    return res
end function

