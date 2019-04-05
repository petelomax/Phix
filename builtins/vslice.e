--
-- builtins/vslice.e
--

global function vslice(sequence source, integer column)
    for i=1 to length(source) do
        source[i] = source[i][column]
    end for
    return source
end function

--SUG: (untested/undocumented)
--/* global 
function vslice(sequence source, object column)
    if integer(column) then
        for i=1 to length(source) do
            source[i] = source[i][column]
        end for
    else
        if length(column)!=2 then ?9/0 end if
        integer {s,e} = column
        for i=1 to length(source) do
            source[i] = source[i][s..e]
        end for
    end if
    return source
end function
--*/
