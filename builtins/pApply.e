--
-- builtins\pApply.e
-- =================
--
include builtins\get_routine_info.e

global function apply(sequence s, integer fn, object userdata = {})
    -- apply function fn to all elements of sequence s
    integer {maxp,minp} = get_routine_info(fn)
    if maxp=0 or minp>2 then
        crash("supplied function must accept 1..2 parameters")
    end if
    for i=1 to length(s) do
        if userdata={} and (maxp=1 or minp<=1) then
            s[i] = fn(s[i])
        else
            s[i] = fn(s[i],userdata)
        end if
    end for
    return s
end function

