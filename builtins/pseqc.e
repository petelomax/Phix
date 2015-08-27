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

global function insert(sequence target, object what, integer index)
-- insert 1 element
    target = target[1..index-1] & '0' & target[index..length(target)]
    target[index] = what
    return target
--or: (untested) [erm, would spanner strings]
--  target[index..index-1] = {what}
--  return target
end function

global function splice(sequence target, object what, integer index)
-- insert m elements
    target = target[1..index-1] & what & target[index..length(target)]
    return target
-- or: (untested)
--  if atom(what) then what = {what} end if --(??)
--  target[index..index-1] = what
--  return target
end function

global function head(sequence target, atom size=1)
-- if size not specified, returns target[1..1]
    if size>=length(target) then return target end if
    return target[1..size]
end function

global function tail(sequence target, atom size=-1)
-- if size not specified, returns target[2..$]
integer l = length(target)
    if size<0 then size += l end if
    if size>=l then return target end if
    return target[l-size+1..l]
end function

global function remove(sequence target, atom start, atom stop=start)
    target[start..stop] = ""
--  target = target[1..start-1] & target[stop+1..length(target)]
    return target
end function

global function replace(sequence target, object replacement, atom start, atom stop=start)
-- replace n elements with m elements
--  target = target[1..start-1] & replacement & target[stop+1..length(target)]
--  if atom(replacement) then replacement = {replacement} end if
    if atom(replacement) and stop!=start then target[start+1..stop] = ""; stop = start end if
    target[start..stop] = replacement
    return target
end function

