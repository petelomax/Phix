--
-- pflatten.e
-- ==========
--
--  flatten: Remove all nesting from a sequence.
--  join: Concatenate all elements of a sequence.
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
