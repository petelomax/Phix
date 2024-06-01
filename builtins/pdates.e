--
-- pdates.e
-- ========
--
--   Some support routines for date(), namely day_of_week(), days_in_month(), 
--                                     day_of_year(), and is_leap_year()
--
--   [moved out of pdate.e as that uses #ilASM{}, for pwa/p2js]
--
sequence dot, dim, t, days
integer dinit = 0

procedure initd()
    dot = {0,31,59,90,120,151,181,212,243,273,304,334}
    dim = {31,28,31,30,31,30,31,31,30,31,30,31}
    t = {-1, 2, 1, 4,-1, 2, 4, 0, 3, 5, 1, 3 }
    days = {"Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"}
    dinit = 1
end procedure

global function is_leap_year(object y)
    if sequence(y) then y = y[DT_YEAR] end if
    return remainder(y,4)=0 and (remainder(y,100)!=0 or remainder(y,400)=0)
end function

global function day_of_year(object y, integer m=0, d=0)
-- day of year function, returns 1..366
    if sequence(y) then {y,m,d} = y end if -- (extract DT_YEAR,DT_MONTH,DT_YEAR)
    if not dinit then initd() end if
    if m<1 or m>12 or (y!=0 and y<1752) then return 0 end if
    return d+dot[m]+(m>2 and is_leap_year(y))
end function

global function days_in_month(object y, integer m=0)
    if sequence(y) then {y,m} = y end if -- (extract DT_YEAR,DT_MONTH)
    if not dinit then initd() end if
    if m<1 or m>12 or (y!=0 and y<1752) then return 0 end if
    return dim[m]+(m=2 and is_leap_year(y))
end function

--/*
function julianDayOfYear(object ymd) -- returns an integer
integer year, month, day
integer d

    year = ymd[1]
    month = ymd[2]
    day = ymd[3]

    if month=1 then return day end if

    d = 0
    for i=1 to month-1 do
        d += daysInMonth(year, i)
    end for

    d += day

    if year=Gregorian_Reformation and month=9 then
        if day>13 then
            d -= 11
        elsif day>2 then
            return 0
        end if
    end if

    return d
end function

--*/

global function day_of_week(object y, integer m=0, d=0, bool bAsText=false)
-- day of week function (Sakamoto) returns 1..7 (Mon..Sun)
    if sequence(y) then {y,m,d} = y end if -- (extract DT_YEAR,DT_MONTH,DT_YEAR)
    integer l
    if not dinit then initd() end if
--23/5/24: (erm, still maybe.. [untested])
--  if m=13 then y += 1; m = 1 elsif m=0 then y -= 1 m = 12 end if
--  if d=0 then
--      if m=1 then y -=1; m = 12 else m -= 1 end if
--      d = days_in_month(y, m)
--  end if
    if d<1 or d>31 or m<0 or m>12 or (y!=0 and y<1752) then ?9/0 end if
    if y!=0 or m!=0 or not bAsText or d>7 then
        y -= m<3
        l = floor(y/4)-floor(y/100)+floor(y/400)
        d += y+l+t[m]
        d = remainder(d,7)+1
    end if
    if bAsText then
        return days[d]
    end if
    return d
end function

global function week_number(object y, integer m=0, d=0)
    -- note days prior to the first monday of the year are
    --      classed as the 52nd week of the preceding year.
    -- returns {y[-1],1..52}
    if sequence(y) then {y,m,d} = y end if -- (extract DT_YEAR,DT_MONTH,DT_YEAR)
    integer d1 = day_of_week(y,1,1),    -- (1..7)
           w52 = remainder(7-d1,7)+1,   -- (1..7)
            dy = day_of_year(y,m,d)-w52 -- (1..366) - (1..7) = -6..365
    if dy<=0 then return {y-1,52} end if
    return {y,floor((dy-1)/7)+1}
end function

