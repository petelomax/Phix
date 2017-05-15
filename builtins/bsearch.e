--
-- bsearch.e
-- =========

global function binary_search(object needle, sequence haystack)
integer lo = 1,
        hi = length(haystack),
        mid = lo,
        c = 0

    while lo<=hi do
        mid = floor((lo+hi)/2)
        c = compare(needle, haystack[mid])
        if c<0 then
            hi = mid-1
        elsif c>0 then
            lo = mid+1
        else
            return mid  -- found!
        end if
    end while
--  if c>0 then
--      mid += 1
--  end if
    mid += c>0
    return -mid         -- where it would go, if inserted now
end function


