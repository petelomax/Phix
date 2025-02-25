--
-- ptrim.e
--
--  implements trim(), trim_head(), trim_tail()
--  This is automatically included when needed; there should be no need 
--  to manually include this file, unless you want/need a namespace.
--
-- Typically these are used to trim whitespace from strings, but you 
--  could also, perhaps, trim unused entries from a table, etc.
--
-- Examples
--  ? trim(" abc ")         -- prints "abc"
--  ? trim(`"abc"`)     `   -- prints `"abc"`
--  ? trim(`"abc"`,'"')     -- prints "abc" (not ""abc"" nor `"abc"`, but 3 chars)
--  ? trim(table,{{}})      -- removes s[i]={} entries from start and end (note the extra {})
--  ? trim(table,{{},0})    -- removes s[i]={} or 0 """
--  ? trim(gets(fn))        -- trim returns eof (-1) unaltered.
--
-- Note that when working with indexes (passing the optional third parameter something
--      non-zero), especially for trim_head() and trim_tail(), it is almost certainly
--      easier to test that source (first parameter) is a sequence and avoid the call,
--      rather than struggle to correctly interpret the results. Passing an eof (-1)
--      to trim_head() or trim_tail() returns it unaltered, and it is rather doubtful 
--      that subsequently treating that -1 as an index will work out at all well...
--
without debug

global function trim(object source, object what=" \t\r\n", bool return_index=false)
--global function trim(object source, object what={' ','\t','\r','\n'}, bool return_index=false)
    if sequence(source) then
        integer lpos = 1,
                rpos = length(source)
        if atom(what) then
            while lpos<=rpos and source[lpos]=what do lpos += 1 end while
            while rpos>lpos  and source[rpos]=what do rpos -= 1 end while
        else
            while lpos<=rpos and find(source[lpos], what) do lpos += 1 end while
            while rpos>lpos  and find(source[rpos], what) do rpos -= 1 end while
        end if

        if return_index then
            if lpos>rpos then rpos = 0 end if   -- 15/2/22
            return {lpos, rpos}

        elsif lpos!=1
           or rpos!=length(source) then
            source = source[lpos..rpos]
        end if
    end if
    return source
end function

global function trim_head(object source, object what=" \t\r\n", bool return_index=false)
--global function trim_head(object source, object what={' ','\t','\r','\n'}, bool return_index=false)
    if sequence(source) then
        sequence s = trim(source,what,true)
        integer lpos = s[1]
        if return_index then
            return lpos

        elsif lpos!=1 then
            source = source[lpos..$]

        end if
    end if
    return source
end function

global function trim_tail(object source, object what=" \t\r\n", bool return_index=false)
--global function trim_tail(object source, object what={' ','\t','\r','\n'}, bool return_index=false)
    if sequence(source) then
        sequence s = trim(source,what,true)
        integer {lpos,rpos} = s
        if return_index then
            return rpos

        elsif lpos>rpos then
            source = source[1..0]   -- "" or {}

        elsif rpos<length(source) then
            source = source[1..rpos]

        end if
    end if
    return source
end function

global function shorten(sequence s, string what="digits", integer ml=20, string fmt="")
    integer l = length(s),
--PL 17/2/22: could find no reason not to, so undone (p2js*2).
--            [maybe I've since fixed $repss() or sq_eq()?]
--pwa.p2js:
            c = iff(string(s) and what="digits"?sum(sq_eq(s,',')):0)
--          c = 0
    if what="digits" then
--      for i=1 to l do
--          c += s[i]==','
--      end for
--29/1/22: (subtract [another] 1 from l in the case of a leading +/-)
        if l and find(s[1],"+-") then c += 1 end if
    end if
    string ls = iff(length(what)?sprintf(" (%,d %s)",{l-c,what}):"")
    if l>ml*2+iff(string(s)?3+length(ls):2) then
        if string(s) then
            assert(fmt=="")
            s[ml+1..-ml-1] = "..."
            if length(ls) then s &= ls end if
        else
--p2js:
--          s[ml+1..-ml-1] = {"..."}
            s = s[1..ml] & {"..."} & s[-ml..-1]
            if fmt!="" then
                for i=1 to ml do
                    s[i] = sprintf(fmt,{s[i]})
                    s[-i] = sprintf(fmt,{s[-i]})
                end for
            end if
            if length(ls) then s = append(s,ls) end if
        end if
    elsif fmt!="" then
        assert(not string(s))
        s = deep_copy(s)
        for i=1 to length(s) do
            s[i] = sprintf(fmt,{s[i]})
        end for
    end if
    return s
end function

