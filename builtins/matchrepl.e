--
-- matchrepl.e
--
-- Phix implementation of match_replace (auto-include)
--

global function match_replace(object needle, sequence haystack, object replacement, integer maxr=0)
integer posn, needle_len, replacement_len
        
    if atom(needle) then
        needle = {needle}
    end if
    if atom(replacement) then
        replacement = {replacement}
    end if

    needle_len = length(needle)
    replacement_len = length(replacement)

    posn = match(needle, haystack)
    while posn do
        haystack[posn..posn+needle_len-1] = replacement
        posn = match(needle, haystack, posn + replacement_len)
        maxr -= 1
        if maxr=0 then exit end if
    end while

    return haystack
end function

