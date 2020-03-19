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

global function trim(object source, object what=" \t\r\n", integer ret_index=0)
integer rpos
integer lpos

    if sequence(source) then
        lpos = 1
        rpos = length(source)
        if atom(what) then
            while lpos<=rpos
              and source[lpos]=what do
                lpos += 1
            end while
            while rpos>lpos
              and source[rpos]=what do
                rpos -= 1
            end while
        else
            while lpos<=rpos
              and find(source[lpos], what) do
                lpos += 1
            end while
            while rpos>lpos
              and find(source[rpos], what) do
                rpos -= 1
            end while
        end if

        if ret_index then
            return {lpos, rpos}

        elsif lpos!=1
           or rpos!=length(source) then
            source = source[lpos..rpos]
        end if
    end if
    return source
end function

global function trim_head(object source, object what=" \t\r\n", integer ret_index=0)
sequence s
integer lpos

    if sequence(source) then
        s = trim(source,what,1)
        lpos = s[1]
        if ret_index then
            return lpos

        elsif lpos!=1 then
            source = source[lpos..$]

        end if
    end if
    return source
end function

global function trim_tail(object source, object what=" \t\r\n", integer ret_index=0)
sequence s
integer lpos, rpos

    if sequence(source) then
        s = trim(source,what,1)
        lpos = s[1]
        rpos = s[2]
        if ret_index then
            return rpos

        elsif lpos>rpos then
            source = source[1..0]   -- "" or {}

        elsif rpos<length(source) then
            source = source[1..rpos]

        end if
    end if
    return source
end function

global function shorten(sequence s, string what="digits", integer ml=20)
    integer l = length(s)
    string ls = sprintf(" (%,d %s)",{l,what}) 
    if l>ml*2+iff(string(s)?3+length(ls):2) then
        if string(s) then
            s[ml+1..-ml-1] = "..."
            s &= ls
        else
            s[ml+1..-ml-1] = {"..."}
            s = append(s,ls)
        end if
    end if
    return s
end function

