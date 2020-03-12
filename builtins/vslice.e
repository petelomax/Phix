--
-- builtins/vslice.e
--

global function vslice(sequence source, integer column)
--1/10/19 (make res string if possible)
--  for i=1 to length(source) do
--      source[i] = source[i][column]
--  end for
--  return source
    sequence res = ""
    for i=1 to length(source) do
        res = append(res,source[i][column])
    end for
    return res
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
