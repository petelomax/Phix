--
-- psprintN.e
-- =========
--
-- Phix: implementation of sprint() and print().
--
--without debug -- (NB: do not use this on RDS Eu/OpenEuphoria)
                -- Normally do NOT explicitly "include psprint.e"; 
                --  instead treat sprint()/print() as say length(),
                --  and just let compiler auto-include this file.
                -- Occasionally you may want an explicit include, 
                --  eg for namespace or routine_id purposes, which
                --  is fine except for RDS Eu incompatibility, so
                --  an "--/**/include builtins\psprint.e" may be better(?).

include pprntfN.e

--constant tnr = "tnr"
--constant tnr = "tnr\\\"\'\0"
constant tnr = "tnr\\\"\'0e"

--function allascii(string x, bool withquotes)
function allascii(string x)
-- Phix allows "strings" to hold binary data, so double check 
-- before printing it as a string.
integer c
    for i=length(x) to 1 by -1 do
        c = x[i]
--31/1/15:
--      if c<' ' then
        if c<' ' or c>#FF or find(c,"\\\"\'") then
--          c = find(c,"\t\n\r")
            c = find(c,"\t\n\r\\\"\'\0\e")
            if c then
                x[i..i] = '\\'&tnr[c]   -- NB does not work on RDS Eu/OpenEuphoria
            else
                return 0
            end if
        end if
    end for
--  if withquotes then
        x = '\"'&x&'\"'
--  end if
    return x
end function

--global function sprint(object x)
global function sprint(object x, integer maxlen=-1, integer nest=0)
-- Return the string representation of any data object. 
-- This is the same as the output from print(1, x) or '?', 
--  but returned as a string sequence rather than printed.
-- Alternative: see ppp.e (ppf/ppOpt/ppExf).
object s, xi

    if atom(x) then
--      return sprintf("%.10g", x)
        s = sprintf("%.10g", x)
        if not integer(x)
--removed 3/11/15 (so that eg 2000000000 gets the ".0")
--      and integer(floor(x))
        and not find('.',s)
        and not find('e',s)         -- eg 1e308
        and not find('n',s) then    -- (inf/nan)
            -- make sure you can tell 5 and 5.00000000001 
            --  apart in ex.err, trace, ?x, and the like.
            s &= ".0"
        end if
        return s
    end if
--  if string(x) then

--      s = allascii(x)
--      if string(s) then return s end if
--  end if
--8/8/16:
--  if maxlen!=-1 and length(x)>maxlen then
    if maxlen>4 and length(x)>maxlen then
        x = x[1..maxlen]
--8/8/16: (change as above)
        if string(x) then
--      if string(x) and length(x)>4 then
--          s = allascii(x[1..maxlen-4],true)
            s = allascii(x[1..maxlen-4])
            if string(s) then return s&".." end if
        end if
    elsif string(x) then
--      s = allascii(x,nest!=0)
        s = allascii(x)
        if string(s) then return s end if
    end if
--  s = "{"
    s = repeat('{',1)
    for i=1 to length(x) do
--      s &= sprint(x[i])
        xi = x[i]
        if maxlen=-1 then
--          s &= sprint(xi)
            s &= sprint(xi,-1,nest+1)
        else
            if maxlen>length(s) then
                s &= sprint(xi,maxlen-length(s),nest+1)
            end if
            if length(s)>=maxlen then
                if nest=0 then
                    s = s[1..maxlen-2]
                    s &= ".."
                else
                    s = s[1..maxlen]
                end if
                return s
            end if
        end if
        if i<length(x) then
            s &= ','
        end if
    end for
--  s &= "}"
    s &= '}'
    return s
end function

--DEV move this to pfileioN.e:
global procedure print(integer fn, object x, integer maxlen=-1)
-- Print a string representation of any data object.
-- Alternative: see ppp.e (pp/ppOpt/ppEx).
    puts(fn,sprint(x,maxlen))
end procedure
