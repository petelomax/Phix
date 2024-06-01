--
-- builtins\to_int.e
-- =================
--
-- Phix implementation of to_integer (auto-include)
--
--  This is, quite deliberately, just about the simplest possible implementation.
--  See also to_number() [in scanf.e] for fractions,exponents,underscores, etc.
--
--  11/11/21: Made this handle atoms, for Own_digits_power_sum, before finding out it did not
--            really help (that specific task), so rather than update docs etc backed out all
--            the changes, but left them in as comments. A better idea might be to_atom()...

global function to_integer(string s, integer def_value = 0, base = 10)
--global function to_integer(string s, integer def_value = 0, base = 10, bool bForceInt=false)
--11/11/21:
    integer res = 0,
            sgn = 1,
--  atom res = 0
--  integer sgn = 1,
            ch, d
    if length(s)>0 then
        ch = s[1]
        if ch='-' or ch='+' then
            if ch='-' then sgn = -1 end if
            s = s[2..$]
        end if
    end if
    if base=16 then
        -- (note: 0b, 0o, 0t, 0(base) not handled here [yet])
        if length(s)>0 and s[1]='#' then
            s = s[2..$]
        elsif length(s)>1 and lower(s[1..2])="0x" then
            s = s[3..$]
        end if
    end if
    if length(s)=0 then
        return def_value
    end if
    for i=1 to length(s) do
        ch = s[i]
-- 8/4/24 treats ':' as '3'...
        d = ch-iff(ch>'9'?iff(ch>='a'?'a':'A')-10:'0')
--      if d<0 or d>base then
        if d<0 or d>base or (ch>'9' and d<10) then
            return def_value
        end if
        atom rchk = res*base + d
        if not integer(rchk) then
--      if (bForceInt and not integer(rchk))
--      or rchk>power(2,iff(machine_bits()=32?53:64)) then
            return def_value
        end if
        res = rchk
    end for
    res = sgn*res  -- (explicitly enforce a final typecheck)
--  res = sgn*res
--  if bForceInt then
--?     integer rint = res  -- (explicitly enforce a final typecheck)
--      assert(integer(res)) -- (explicitly enforce a final typecheck)
--  end if
    return res
end function

global function is_integer(string s, integer base = 10)
    integer r = -power(2,machine_bits()-2)
    -- (r is -1073741824 or -4611686018427387904)
    return to_integer(s, r, base) != r
--  return to_integer(s, r, base, true) != r
end function

