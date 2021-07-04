--
-- builtins\pextract.e
--

global function extract(sequence source, indexes, bool invert=false)
    integer l = length(indexes), ii
    sequence res = repeat(0,l)
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

global function reinstate(sequence source, indexes, replacements, bool invert=false)
    integer l = length(indexes), ii
    if length(replacements)!=l then ?9/0 end if
--DEV deep_copy(ifNeeded) may be in order here...
--  sequence res = repeat(0,length(source))
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
--      res[ii] = replacements[i]
        source[ii] = replacements[i]
    end for
--  return res
    return source
end function
