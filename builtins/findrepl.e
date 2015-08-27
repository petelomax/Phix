--
-- builtins/findrepl.e
--
global function find_replace(object needle, sequence haystack, object replacement, integer max_replacements=0)

integer posn = 0
    while 1 do
        posn = find(needle, haystack, posn+1)
        if posn=0 then exit end if
        haystack[posn] = replacement
        max_replacements -= 1
        if max_replacements=0 then exit end if
    end while
    return haystack
end function

