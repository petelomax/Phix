--
-- builtins\pextract.e
--

global function extract(sequence source, indexes, bool invert=false)
    integer l = length(indexes), ii
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
        indexes[i] = source[ii]
    end for
    return indexes
end function

global function reinstate(sequence source, indexes, replacements, bool invert=false)
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
        source[ii] = replacements[i]
    end for
    return source
end function
