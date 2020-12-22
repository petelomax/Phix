--
-- pdate.e
-- =======
--
-- The Phix implementation of date()
--
include builtins\pdates.e           -- (non-#ilASM{} bits)
include builtins\VM\pcfunc.e        -- (not strictly necessary)

atom kernel32, xGetLocalTime, xGetSystemTime
integer dinit = 0
sequence dot

procedure initd()
--  enter_cs()  -- (erm, cannot see that actually helping, refcounts are still not thread-safe anyway)
    if platform()=WINDOWS then
        kernel32 = open_dll("kernel32.dll")
        xGetLocalTime = define_c_proc(kernel32,"GetLocalTime",{C_PTR})
        xGetSystemTime = define_c_proc(kernel32,"GetSystemTime",{C_PTR})
    end if
    dot = {0,31,59,90,120,151,181,212,243,273,304,334}
--  leave_cs()
    dinit = 1
end procedure

constant
    -- SYSTEMTIME structure: (same on 64-bit)
    STwYear             = 0,    --  WORD wYear
    STwMonth            = 2,    --  WORD wMonth
    STwDayOfWeek        = 4,    --  WORD wDayOfWeek
    STwDay              = 6,    --  WORD wDay
    STwHour             = 8,    --  WORD wHour
    STwMinute           = 10,   --  WORD wMinute
    STwSecond           = 12,   --  WORD wSecond
    STwMillisecs        = 14,   --  WORD wMilliseconds
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
(   DT_MSEC   = 7, )
    DT_DOY    = 8
--*/
--DEV (temp, now in psym.e)
--constant DT_GMT=-1

global function date(bool bMsecs = false)
--
--Return a sequence with the following information:  
--            {year,  -- 4 digit
--            month,  -- January = 1
--              day,  -- day of month, starting at 1
--             hour,  -- 0 to 23
--           minute,  -- 0 to 59
--           second,  -- 0 to 59
--  day of the week,  -- Sunday = 1      (or milliseconds)
--  day of the year}  -- January 1st = 1
-- 
-- Use builtin constants DT_YEAR etc (NB not D_YEAR, that is for dir())
--

--integer y, m, d, ys1900, dow
integer year, diy, month, day, hour, mins, secs, dow, msecs
atom xSystemTime
sequence res

    if not dinit then initd() end if
    if platform()=WINDOWS then
        xSystemTime = allocate(STsize)
        if bMsecs=DT_GMT then
            c_proc(xGetSystemTime,{xSystemTime})
        else
            c_proc(xGetLocalTime,{xSystemTime})
        end if
        year = peek2u(xSystemTime+STwYear)
--      ys1900 = year-1900
        month = peek2u(xSystemTime+STwMonth)
        day = peek2u(xSystemTime+STwDay)
        hour = peek2u(xSystemTime+STwHour)
        mins = peek2u(xSystemTime+STwMinute)
        secs = peek2u(xSystemTime+STwSecond)
        msecs = peek2u(xSystemTime+STwMillisecs)
        dow = peek2u(xSystemTime+STwDayOfWeek)+1
        free(xSystemTime)
    elsif platform()=LINUX then
--      integer opcode = iff(machine_bits()=32?iff(bMsecs=DT_GMT?25:13)     -- 25=sys_stime, 13=sys_time
--                                            :iff(bMsecs=DT_GMT?25:201))   -- 25=sys_stime, 201=sys_time
        #ilASM{
            [ELF32]
--#     Name                        Registers                                                                                                               Definition
--                                  eax     ebx                     ecx                     edx                     esi                     edi
-->13   sys_time                    0x0d    time_t *tloc            -                       -                       -                       -               kernel/posix-timers.c:855
--no! this is set system time!
--25    sys_stime                   0x19    time_t *tptr            -                       -                       -                       -               kernel/time.c:81
                xor ebx,ebx
                mov eax,13          -- sys_time
--              mov eax,[opcode]    -- sys_time/sys_stime
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
        msecs = 0 --DEV (use sys_clock_gettime as per pTime.e) [or sys_gettimeofday?]
--/*
78      sys_gettimeofday            0x4e    struct timeval *tv      struct timezone *tz     -                       -                       -               kernel/time.c:101

Arguments

eax     78
ebx     Pointer to a timeval structure (this parameter can be 0): 
struc timeval
{
tv_sec  rd 1 ; seconds 
tv_usec rd 1 ; microseconds 
}
ecx     Pointer to a timezone structure (this parameter can be 0): 
struc timezone
{
tz_minuteswest rd 1 
tz_dsttime     rd 1 
}

timezone members:

tz_minuteswest
Number of minutes west of UTC.
tz_dsttime
Contains a symbolic constant (values are given below) that indicates in which part of the year Daylight Saving Time is in force. 
(Note: its value is constant throughout the year: it does not indicate that DST is in force, it just selects an algorithm.) 
The daylight saving time algorithms defined are as follows :
DST_NONE    - not on dst
DST_USA     - USA style dst 
DST_AUST    - Australian style dst 
DST_WET     - Western European dst 
DST_MET     - Middle European dst 
DST_EET     - Eastern European dst 
DST_CAN     - Canada 
DST_GB      - Great Britain and Eire 
DST_RUM     - Rumania 
DST_TUR     - Turkey 
DST_AUSTALT - Australian style with shift in 1986

Return values

If the system call succeeds the return value is 0.
If the system call fails the return value is one of the following errno values:

-EFAULT One of ecx or edx pointed outside the accessible address space.
-EINVAL Timezone (or something else) is invalid.
    
#define DST_NONE        0       /* not on dst */
#define DST_USA         1       /* USA style dst */
#define DST_AUST        2       /* Australian style dst */
#define DST_WET         3       /* Western European dst */
#define DST_MET         4       /* Middle European dst */
#define DST_EET         5       /* Eastern European dst */
#define DST_CAN         6       /* Canada */
#define DST_GB          7       /* Great Britain and Eire */
#define DST_RUM         8       /* Rumania */
#define DST_TUR         9       /* Turkey */
#define DST_AUSTALT     10      /* Australian style with shift in 1986 */
--*/
        secs = remainder(xSystemTime,60)
        xSystemTime = floor(xSystemTime/60)
        mins = remainder(xSystemTime,60)
        xSystemTime = floor(xSystemTime/60)
        hour = remainder(xSystemTime,24)
--27/11/16:
--      xSystemTime = floor(xSystemTime/24)
        xSystemTime = floor(xSystemTime/24)+1
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
           iff(bMsecs?msecs:dow),
           day_of_year(year,month,day)}
    return res
end function


