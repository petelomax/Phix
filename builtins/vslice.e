--
-- builtins/vslice.e
--

global function vslice(sequence source, integer column)
    for i=1 to length(source) do
        source[i] = source[i][column]
    end for
    return source
end function

