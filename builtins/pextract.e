--
-- builtins\pextract.e
--

global function extract(sequence source, indexes, integer invert=false)
    integer l = length(indexes), ii
--  sequence res = repeat(0,l)
    if odd(invert) then
        sequence inverse = repeat(0,l)  // (or tagset??)
        for i=1 to l do
            ii = indexes[i]
            inverse[ii] = i
        end for
        indexes = inverse
        invert -= 1
    end if
    sequence res = repeat(iff(invert=0 and string(source)?' ':0),l)
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
--2/5/24:
--      if length(replacements)!=l then ?9/0 end if
        if invert then
            sequence inverse = repeat(0,l)
            for i=1 to l do
                ii = indexes[i]
                inverse[ii] = i
            end for
            indexes = inverse
        end if
        if atom(replacements) then
            for i=1 to l do
                ii = indexes[i]
                res[ii] = replacements
            end for
        else
--21/5/24:
            bool one = length(replacements)=1 and length(indexes)!=1
            assert(one or length(replacements)=l)
            for i=1 to l do
                ii = indexes[i]
                res[ii] = replacements[iff(one?1:i)]
            end for
        end if
    end if
    return res
end function
