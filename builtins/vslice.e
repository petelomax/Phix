--
-- builtins/vslice.e
--

--/*
--global 
function vslice(sequence source, integer column)
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
--*/

--SUG: (untested/undocumented)
global function vslice(sequence source, object column, error_control=0)
    integer current_sub = 0,
            substitutes = iff(atom(error_control)?-(not error_control)
                                                 :length(error_control))
    sequence res = ""
    if integer(column) then
        if column<1 then crash("vslice(%d)",column) end if
        for i=1 to length(source) do
            if atom(source[i]) or column>length(source[i]) then
                if substitutes<=0 then
                    if substitutes=0 then exit end if
                    crash("vslice[%d][%d] oob",{i,column},2)
                end if
                current_sub += 1
                if current_sub>length(error_control) then current_sub = 1 end if
                res = append(res,error_control[current_sub])
            else
                res = append(res,source[i][column])
            end if
        end for
    else
        integer {s,e} = iff(length(column)=2?column:{0,0})
        if s<1 or e<s-1 then crash("vslice(%v)",{column}) end if
        for i=1 to length(source) do
            if atom(source[i]) or e>length(source[i]) then
                if substitutes<=0 then
                    if substitutes=0 then exit end if
--              if atom(error_control) then
--                  if error_control!=0 then exit end if
                    crash("vslice[%d][%d..%d] oob",i&column,2)
                end if
                current_sub += 1
                if current_sub>length(error_control) then current_sub = 1 end if
                res = append(res,error_control[current_sub])
            else
                res = append(res,source[i][s..e])
            end if
        end for
    end if
    return res
end function


--/*
function vslice(sequence source, atom colno, object error_control=0)
    for i = 1 to length(source) do
        if colno > length(source[i]) then
            if substitutes = -1 then
                error:crash("sequence:vslice(): colno should be a valid index on the %d-th element, but was %d", {i, colno})
            elsif substitutes = 0 then
                return source[1..i-1]
            else
                current_sub += 1
                if current_sub > length(error_control) then
                    current_sub = 1
                end if
                source[i] = error_control[current_sub]
                
            end if
        else
            if sequence(source[i]) then
                source[i] = source[i][colno]
            end if
        end if
    end for

    return source
end function

function vslice(sequence source, atom colno, object error_control=0)
    for i=1 to length(source) do
        if colno>=1+length(source[i]) then
            if substitutes=-1 then
                crash("sequence:vslice(): colno should be a valid index on the %d-th element, but was %d", {i,colno})
            elsif substitutes=0 then
                return ret[1..i-1]
            else
                                current_sub += 1
                                if current_sub > length(error_control) then
                                        current_sub = 1
                                end if

                substitutes -= 1
                ret[i] = error_control[current_sub]
                current_sub += 1
            end if
        else
            ret[i] = source[i][colno]
        end if
    end for

    return ret
end function

--*/
