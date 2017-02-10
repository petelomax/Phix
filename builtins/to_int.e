--
-- to_int.e
--
-- Phix implementation of to_integer (auto-include)
--
--  This is, quite deliberately, the simplest possible implementation; see also to_number() [in scanf.e].
--

global function to_integer(object s, integer def_value = 0)
integer res = 0
    if not string(s) or length(s)=0 then return def_value end if
    for i=1 to length(s) do
        integer ch = s[i]
        if ch<'0' or ch>'9' then return def_value end if
        res = res*10 + ch-'0'
    end for
    return res
end function

