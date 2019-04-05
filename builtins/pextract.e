--
-- builtins\pextract.e
--

global function extract(sequence source, indexes, bool invert=false)
    integer l = length(indexes)
    if invert then
        sequence inverse = repeat(0,l)
        for i=1 to l do
            inverse[indexes[i]] = i
        end for
        indexes = inverse
    end if
    for i=1 to l do
        indexes[i] = source[indexes[i]]
    end for
    return indexes
end function

global function reinstate(sequence source, indexes, replacements, bool invert=false)
    integer l = length(indexes)
    if length(replacements)!=l then ?9/0 end if
    if invert then
        sequence inverse = repeat(0,l)
        for i=1 to l do
            inverse[indexes[i]] = i
        end for
        indexes = inverse
    end if
    for i=1 to l do
        source[indexes[i]] = replacements[i]
    end for
    return source
end function
