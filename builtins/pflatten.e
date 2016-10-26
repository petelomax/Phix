--
-- pflatten.e
-- ==========
--
--  flatten: Remove all nesting from a sequence.
--  join: Concatenate all elements of a sequence.
--  join_path: Concatenate all elements of a path.
--

global function flatten(sequence s)
sequence res = ""
object si

    for i=1 to length(s) do
        si = s[i]
        if atom(si) 
        or string(si) then
            res &= si
        else
            res &= flatten(si)
        end if
    end for
    return res
end function

global function join(sequence s, object delim=" ")
sequence res = ""

    for i=1 to length(s) do
        if i!=1 then
            res &= delim
        end if
        res &= s[i]
    end for
    return res
end function

global function join_path(sequence path_elements, integer trailsep=0)
integer SLASH = iff(platform()=WINDOWS?'\\':'/')
string elem, fname = ""

    for i=1 to length(path_elements) do

        elem = path_elements[i]

        if length(elem) and elem[$]=SLASH then
            elem = elem[1..$-1]
        end if

        if length(fname) and length(elem) and elem[1]!=SLASH then
            if platform()=WINDOWS then
                if elem[$]!=':' then
                    elem = SLASH & elem
                end if
            else
                elem = SLASH & elem
            end if
        end if

        fname &= elem

    end for

    if trailsep and length(fname) and fname[$]!=SLASH then
        fname &= SLASH
    end if

    return fname
end function

