--
-- builtins\pextract.e
--

global function extract(sequence source, indexes, bool invert=false)
    integer l = length(indexes), ii
--  sequence res = repeat(0,l)
    sequence res = repeat(iff(string(source)?' ':0),l)
    if invert then
        sequence inverse = repeat(0,l)
        for i=1 to l do
            ii = indexes[i]
            inverse[ii] = i
        end for
        indexes = inverse
    end if
    for i=1 to l do
        ii = indexes[i]
        res[i] = source[ii]
    end for
    return res
end function

global function reinstate(sequence source, object indexes, replacements, bool invert=false)
    sequence res
    if source={} and sequence(indexes) then
        assert(not invert) -- (??)
        res = repeat(0,max(indexes))
        if replacements={} then
            replacements = tagset(length(indexes))
        end if
    else
        res = deep_copy(source)
    end if
    if integer(indexes) then
        assert(not invert)
        res[indexes] = replacements
    else
        integer l = length(indexes), ii
        if length(replacements)!=l then ?9/0 end if
        if invert then
            sequence inverse = repeat(0,l)
            for i=1 to l do
                ii = indexes[i]
                inverse[ii] = i
            end for
            indexes = inverse
        end if
        for i=1 to l do
            ii = indexes[i]
            res[ii] = replacements[i]
        end for
    end if
    return res
end function
