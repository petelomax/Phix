--
-- eadadj.e
--
-- Simple date adjustment routine.
--
--  For use in current applications only.
--   Dates before 1980 (see constant startYear) are not handled.
--   See also builtins/timedate.e, which was written after this...
--
--  Written for Edita's "Number of days to retain backups for" option, 
--   could also be useful for settlement terms, overdue accounts, etc.
--

-- Technically modifying this to any year after 1752 should work, 
--  but I don't need it, at least not in Edita:
constant startYear = 1980   --DEV not //actually tested// pre-2000...
                            -- but my cmos battery == +/-0.0v so...

function isleapyear(integer y)
    if remainder(y,4)!=0 then return 0 end if
    return (remainder(y,100)!=0 or remainder(y,400)=0)
end function

sequence dom
         dom = {31,28,31,30,31,30,31,31,30,31,30,31}

function ymdfromd(integer dsv)
--
-- returns date eg {2003,9,15}, or -1 on error
--  dsv is typically a value from daysSinceStartYear()+/-n
--
integer y, yl, m    -- year, year length (in days), month

    if dsv<1 then
        void = messageBox("Day error",
                          sprintf("dsv=%d;\n\n"&
                                  "System time may need setting.\n\n"&
                                  "Aborting...",dsv),0)
--      abort(0)
        return -1
    end if
    y = startYear
    while 1 do
        yl = 365+isleapyear(y)
        if dsv<=yl then exit end if
        y += 1
        dsv -= yl
    end while
    dom[2] = 28+isleapyear(y)
    m = 1
    for i=1 to 12 do
        if dsv<=dom[m] then exit end if
        dsv -= dom[m]
        m += 1
    end for
    return {y,m,dsv}
end function

constant dot = {0,31,59,90,120,151,181,212,243,273,304,334}

function daysSinceStartYear(sequence ymd)
--
-- returns integer number of days since 31/12/startYear-1, or {} on error.
--  ymd is eg {2003,9,15}
--
integer y,m,d
    y = ymd[1]
    if y<startYear then
        void = messageBox("Date error",
                          sprintf("ymd={%d,%d,%d}; startYear=%d\n\n"&
                                  "System time may need setting.\n\n"&
                                  "Aborting...",ymd&startYear),0)
--      abort(0)
        return -1
    end if
    m = ymd[2]
    d = dot[m]+(m>2 and isleapyear(y))+ymd[3]
    while y>startYear do
        y -= 1
        d += 365+isleapyear(y)
    end while
    return d
end function

global function adjustDate(sequence ymd, integer adjust)
--
-- returns a {y,m,d} style date, or -1 on error.
--  ymd is eg {2003,12,31}
--  adjust is an integer, +/- n days to adjust ymd by.
--
integer d = daysSinceStartYear(ymd)
    if d=-1 then return d end if
    return ymdfromd(d+adjust)
end function

