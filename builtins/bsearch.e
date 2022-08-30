--
-- bsearch.e
-- =========
--
without debug

--global function binary_search(object needle, sequence haystack)
--global function binary_search(object needle, sequence haystack, integer lo=1, hi=length(haystack), fn=0)
global function binary_search(object needle, sequence haystack, integer lo=1, hi=-1, fn=0)
--  integer lo = 1,
--          hi = length(haystack),
--          mid = lo,
    if hi=-1 then hi = length(haystack) end if
    integer mid = lo,
            c = 0

    while lo<=hi do
        mid = floor((lo+hi)/2)
        if fn=0 then
            c = compare(needle, haystack[mid])
            if c<0 then
                hi = mid-1
            elsif c>0 then
                lo = mid+1
            else
                return mid  -- found!
            end if
        else
            c = fn(needle, haystack[mid])
            if c then
                lo = mid+1
            else
                hi = mid-1
            end if
            if lo>hi then exit end if
        end if      
    end while
--  if c>0 then
--      mid += 1
--  end if
    mid += c>0
    return -mid         -- where it would go, if inserted now
end function


