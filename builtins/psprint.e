--
-- psprint.e
-- =========
--
-- Phix: implementation of sprint() and print().
--
--!without debug -- (NB: do not run this on RDS Eu)
                -- Normally do NOT explicitly "include psprint.e"; 
                --  instead treat sprint()/print() as say length(),
                --  and just let compiler auto-include this file.
                -- Occasionally you may want an explicit include, 
                --  eg for namespace or routine_id purposes, which
                --  is fine except for RDS Eu incompatibility, so
                --  an "--/**/include builtins\psprint.e" may be better(?).

include pprntf.e

constant tnr = "tnre"
function allascii(string x)
-- Phix allows "strings" to hold binary data, so double check 
-- before printing it as a string.
integer c
    for i=length(x) to 1 by -1 do
        c = x[i]
        if c<' ' then
            c = find(c,"\t\n\r\e")
            if c then
                x[i..i] = '\\'&tnr[c]
            else
                return 0
            end if
        end if
    end for
    return '\"'&x&'\"'
end function

global function sprint(object x)
-- Return the string representation of any data object. 
-- This is the same as the output from print(1, x) or '?', 
--  but returned as a string sequence rather than printed.
-- Alternative: see ppp.e (ppf/ppOpt/ppExf).
object s

    if atom(x) then
        return sprintf("%.10g", x)
    end if
    if string(x) then
        s = allascii(x)
        if string(s) then return s end if
    end if
    s = "{"
    for i=1 to length(x) do
        s &= sprint(x[i])
        if i<length(x) then
            s &= ','
        end if
    end for
    s &= "}"
    return s
end function

global procedure print(integer fn, object x)
-- Print a string representation of any data object.
-- Alternative: see ppp.e (pp/ppOpt/ppEx).
    puts(fn,sprint(x))
end procedure
