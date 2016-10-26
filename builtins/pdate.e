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
    if platform()=WINDOWS then
        kernel32 = open_dll("kernel32.dll")
        xGetLocalTime = define_c_proc(kernel32,"GetLocalTime",{C_PTR})
    end if
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
integer year, diy, month, day, hour, mins, secs, dow
atom xSystemTime
sequence res

    if not dinit then initd() end if
    if platform()=WINDOWS then
        xSystemTime = allocate(STsize)
        c_proc(xGetLocalTime,{xSystemTime})
        year = peek2u(xSystemTime+STwYear)
--      ys1900 = year-1900
        month = peek2u(xSystemTime+STwMonth)
        day = peek2u(xSystemTime+STwDay)
        hour = peek2u(xSystemTime+STwHour)
        mins = peek2u(xSystemTime+STwMinute)
        secs = peek2u(xSystemTime+STwSecond)
        dow = peek2u(xSystemTime+STwDayOfWeek)+1
        free(xSystemTime)
    elsif platform()=LINUX then
        #ilASM{
            [ELF32]
--#     Name                        Registers                                                                                                               Definition
--                                  eax     ebx                     ecx                     edx                     esi                     edi
-->13   sys_time                    0x0d    time_t *tloc            -                       -                       -                       -               kernel/posix-timers.c:855
                xor ebx,ebx
                mov eax,13      -- sys_time
                int 0x80
                xor ebx,ebx
                push ebx
                push eax            -- (treat as unsigned, /might/ work after 2038...)
                fild qword[esp]
                add esp,8
                lea edi,[xSystemTime]
                call :%pStoreFlt
            [ELF64]
--%rax  System call             %rdi                    %rsi                            %rdx                    %rcx                    %r8                     %r9
--201   sys_time                time_t *tloc
                xor rdi,rdi
                mov rax,201     -- sys_time
                syscall
                push rax
                fild qword[rsp]
                add rsp,8
                lea rdi,[xSystemTime]
                call :%pStoreFlt    -- (also sets r15 to h4)
            []
              }
        secs = remainder(xSystemTime,60)
        xSystemTime = floor(xSystemTime/60)
        mins = remainder(xSystemTime,60)
        xSystemTime = floor(xSystemTime/60)
        hour = remainder(xSystemTime,24)
        xSystemTime = floor(xSystemTime/24)
        year = 1970
        while 1 do
            diy = 365+is_leap_year(year)
            if xSystemTime<=diy then exit end if
            year += 1
            xSystemTime -= diy
        end while
        month = 1
        for i=2 to 12 do
            if xSystemTime<=dot[i]+(i>2 and is_leap_year(year)) then exit end if
            month = i
        end for
        day = xSystemTime - (dot[month]+(month>2 and is_leap_year(year)))
        dow = day_of_week(year, month, day)
    end if
--  res = {ys1900,
    res = {year,
           month,
           day,
           hour,
           mins,
           secs,
           dow,
           day_of_year(year,month,day)}
    return res
end function

--/* 
--DEV/SUG possible builtin? (I wrote this for Edix, but then decided on a "not visited" count.)
--        Alternative: timedate_delta(dt1,dt2) produces a timedelta (in seconds) which can be
--                      displayed using elapsed(). Maybe better, but I haven't written that.
global function days_between(sequence dt1, sequence dt2={})
--
-- if dt2 is omitted or of length zero the current date is used.
-- dt1 and dt2 need to start with y,m,d, but can be longer, eg from date(), parse_date_string(), or adjust_timedate().
-- if dt1 and dt2 are the same date (the first three elements match), the result will be 0.
-- if dt1 is a date before dt2 the result will be positive
-- if dt1 is a date after dt2 the result will be negative
-- for any non-defaulted dt2, days_between(dt1,dt2)==days_between(dt2,dt1)*-1
-- no rounding occurs: if dt1 is one second past midnight and dt2 is one second before midnight on the same day,
-- the result will still be 0, even though it is 99.998% of a day, and likewise if dt1 is one second before
-- midnight and dt2 is 1 second later, the result will still be 1, even though it is really 0.001% of a day.
--
    integer {y1,m1,d1} = dt1
    if length(dt2)=0 then dt2 = date() end if
    integer {y2,m2,d2} = dt2
    integer res = day_of_year(y2,m2,d2)-day_of_year(y1,m1,d1)
    while y2>y1 do
        y2 -= 1
        res += 365+is_leap_year(y2)
    end while
    while y2<y1 do
        res -= 365+is_leap_year(y2)
        y2 += 1
    end while
    return res
end function

--and some tests:
--  if days_between({2016,08,08},{2016,08,09})!=1 then ?9/0 end if
--  if days_between({2016,01,01},{2016,08,09})!=221 then ?9/0 end if
--  if days_between({2015,12,31},{2016,08,09})!=222 then ?9/0 end if
--  if days_between({2015,01,01},{2016,08,09})!=586 then ?9/0 end if
--  if days_between({2014,12,31},{2016,08,09})!=587 then ?9/0 end if
--  if days_between({2017,01,01},{2016,08,09})!=-145 then ?9/0 end if
--  if days_between({2016,12,31},{2016,08,09})!=-144 then ?9/0 end if
--  if days_between({2016,08,10},{2016,08,09})!=-1 then ?9/0 end if
--  if days_between({2016,12,31},{2016,08,09})!=-144 then ?9/0 end if
--  if days_between({2016,08,09},{2016,12,31})!=144 then ?9/0 end if
--  if days_between(date())!=0 then ?9/0 end if
--  if days_between({1900,01,01},{2016,08,09})!=42589 then ?9/0 end if
--*/
