--
-- builtins/to_int.e
-- =================
--
-- Phix implementation of to_integer (auto-include)
--
--  This is, quite deliberately, just about the simplest possible implementation.
--  See also to_number() [in scanf.e] for fractions,exponents,_,non-decimal, etc.
--

global function to_integer(string s, integer def_value = 0)
    if length(s)=0 then
        return def_value
    end if
    integer res = 0,
            sgn = 1
    for i=1 to length(s) do
        integer ch = s[i]
        if ch<'0' or ch>'9' then
            if i!=1 or length(s)=1 
            or (ch!='-' and ch!='+') then
                return def_value
            end if
            if ch='-' then sgn = -1 end if
        else
            atom rchk = res*10 + ch-'0'
            if not integer(rchk) then
                return def_value
            end if
            res = rchk
        end if
    end for
    res = sgn*res   -- (explicitly enforce a final typecheck)
    return res
end function

global function is_integer(string s)
    integer r = -power(2,machine_bits()-2)
    -- (r is -1073741824 or -4611686018427387904)
    return to_integer(s, r) != r
end function


