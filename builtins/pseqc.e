--
-- pseqc.e
--
-- Phix compatibility implementation of OpenEuphoria's builtin
--   insert(), splice(), head(), tail(), remove(), and replace().
--
--  This is an auto-include; there is no need to manually include 
--  this file, unless you need a namespace attached to it.
--
--  When and *if* it becomes clear the Eu builtins are much faster 
--  than this, and it is a measurable issue in anything other than a 
--  pointless benchmark, I will reconsider reimplementing them using 
--  low-level assembly tricks etc to speed things up, but NOT before.
--   (actually, insert/splice/remove/replace are more likely to be
--    front-end tweaks aka syntactic sugar thingies, maybe with a
--    trick or two to avoid requiring the x = {x}.)
--  Besides, if one of these is nearly, but not quite, what you want,
--  it is significantly easier to copy/edit them in this form.
--DEV did I create a proper test set for these?
--  NO: there is a single splice() call in t24slice.exw, nowt else.
--

global function insert(sequence src, object what, integer index)
-- insert 1 element
    src = src[1..index-1] & '0' & src[index..length(src)]
    src[index] = what
    return src
end function

global function splice(sequence src, object what, integer index)
    return src[1..index-1] & what & src[index..length(src)]
end function

global function head(sequence src, integer size=1)
-- if size not specified, returns target[1..1]
    if size>=length(src) then return src end if
    return src[1..size]
end function

global function tail(sequence src, integer size=-1)
-- if size not specified, returns src[2..$]
integer l = length(src)
    if size<0 then size += l end if
    if size>=l then return src end if
    return src[l-size+1..l]
end function

global function pad_head(string src, integer size, ch=' ')
-- <same as pad(src,size,"HEAD"[,ch])>
    size -= length(src)
    if size>0 then
        src = repeat(ch,size) & src
    end if
    return src
end function

global function pad_tail(string src, integer size, ch=' ')
-- <same as pad(src,size,"TAIL"[,ch])>
    size -= length(src)
    if size>0 then
        src = src & repeat(ch,size)
    end if
    return src
end function

global function pad(string src, integer size, string method="BOTH", integer ch=' ')
    size -= length(src)
    if size>0 then
        if method="BOTH" then
            src = repeat(ch,floor(size/2)) & src & repeat(ch,size-floor(size/2))
        elsif method="HEAD" then
            src = repeat(ch,size) & src
        elsif method="TAIL" then
            src = src & repeat(ch,size)
        else
            ?9/0 -- unknown method
        end if
    end if
    return src
end function

global function remove(sequence src, integer start, integer stop=start)
    src[start..stop] = ""
--  src = src[1..start-1] & src[stop+1..length(src)]
    return src
end function

global function replace(sequence src, object replacement, integer start, integer stop=start)
    if atom(replacement) and stop!=start then src[start+1..stop] = ""; stop = start end if
    src[start..stop] = replacement
    return src
end function

