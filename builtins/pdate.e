--
-- pdate.e
-- =======
--
-- The Phix implementation of date()
--
sequence dot
--  --   dot={0,31,59,90,120,151,181,212,243,273,304,334}   -- now done in init (forward refs).
sequence t
--  --   t={ 0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4 }           -- now done in init (forward refs).

atom kernel32, xGetLocalTime
integer dinit = 0

procedure initd()
    dinit = 1
    enter_cs()
    kernel32 = open_dll("kernel32.dll")
    xGetLocalTime = define_c_proc(kernel32,"GetLocalTime",{C_PTR})
    dot = {0,31,59,90,120,151,181,212,243,273,304,334}
    t = { 0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4 }
    leave_cs()
end procedure

global function is_leap_year(integer y)
--  if remainder(y,4)!=0 then return 0 end if
--  return (remainder(y,100)!=0 or remainder(y,400)=0)
    return remainder(y,4)=0 and (remainder(y,100)!=0 or remainder(y,400)=0)
end function


global function day_of_year(integer y, integer m, integer d)
-- day of year function, returns 1..366
--  y = (m>2 and remainder(y,4)=0 and (remainder(y,100)!=0 or remainder(y,400)=0))
    -- y is now 1 if later than Feb (29th) in a leap year.
--  y = (m>2 and is_leap_year(y))
--  d += dot[m]+y   -- eg march 1st is 60th day normally, 61st in a leap year.
--  return d
    if not dinit then initd() end if
    return d+dot[m]+(m>2 and is_leap_year(y))
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

global function day_of_week(integer y, integer m, integer d)
-- day of week function (Sakamoto) returns 1..7 (Sun..Sat)
integer l
    if not dinit then initd() end if
    y -= m<3
    l = floor(y/4)-floor(y/100)+floor(y/400)
    d += y+l+t[m]
    return remainder(d,7)+1
end function

constant
    -- SYSTEMTIME structure: (same on 64-bit)
    STwYear             = 0,    --  WORD wYear
    STwMonth            = 2,    --  WORD wMonth
    STwDayOfWeek        = 4,    --  WORD wDayOfWeek
    STwDay              = 6,    --  WORD wDay
    STwHour             = 8,    --  WORD wHour
    STwMinute           = 10,   --  WORD wMinute
    STwSecond           = 12,   --  WORD wSecond
--  STwMillisecs        = 14,   --  WORD wMilliseconds
    STsize = 16

--/* (now defined in psym.e):
global constant 
    DT_YEAR   = 1,
    DT_MONTH  = 2,
    DT_DAY    = 3,
    DT_HOUR   = 4,
    DT_MINUTE = 5,
    DT_SECOND = 6,
    DT_DOW    = 7,
    DT_DOY    = 8
--*/

global function date()
--
--Return a sequence with the following information:  
--            {year,  -- 4 digit
--            month,  -- January = 1
--              day,  -- day of month, starting at 1
--             hour,  -- 0 to 23
--           minute,  -- 0 to 59
--           second,  -- 0 to 59
--  day of the week,  -- Sunday = 1
--  day of the year}  -- January 1st = 1
-- 
-- Use builtin constants DT_YEAR etc (NB not D_YEAR, that is for dir())
--

--integer y, m, d, ys1900, dow
integer y, m, d, dow
atom xSystemTime
sequence res

    if not dinit then initd() end if
    xSystemTime = allocate(STsize)
    c_proc(xGetLocalTime,{xSystemTime})
    y = peek2u(xSystemTime+STwYear)
--  ys1900 = y-1900
    m = peek2u(xSystemTime+STwMonth)
    d = peek2u(xSystemTime+STwDay)
    dow = peek2u(xSystemTime+STwDayOfWeek)+1
--  res = {ys1900,
    res = {y,
           m,
           d,
           peek2u(xSystemTime+STwHour),
           peek2u(xSystemTime+STwMinute),
           peek2u(xSystemTime+STwSecond),
           dow,
           day_of_year(y,m,d)}
    free(xSystemTime)
    return res
end function

