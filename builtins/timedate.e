--
-- timedate.e
-- ==========
--
--  Inspired by but with very little connection to the OpenEuphoiria std\datetime.e
--  Also influenced by Python, and (some of!) the C# DateTime object.
--
--  Incorporates additional timezone and daylight savings handling.
--
--DEV/SUG:
--> atom diff = timediff(timedate t1, timedate t2)
--      the result is a value in seconds, compatible/comparable with a result from timedelta.
--> string res = format_timedelta(atom td) -- ?[, string fmt=??, integer flags=??]?
--      returns a string representation of a timedelta, eg "127 days, 4 hours, 15 minutes and 10 seconds".
--? isleapyear [or link to], isdaylightsavingtime
--x> integer c = time_cmp(timedate t1, timedate t2)
--x     returns -1, 0, 1 in a similar fashion to compare [is compare not good enough?   ** OF COURSE IT IS! **, also <, <=, >, >=, =, !=...]
--/* worth testing:
--/ Define two dates.
--DateTime date1 = new DateTime(2016, 1, 10, 11, 20, 30);
--DateTime date2 = new DateTime(2016, 2, 20, 12, 25, 35);
--
--hxxps://msdn.microsoft.com/en-us/library/system.datetime.datetime(v=vs.110).aspx%3E
--// Calculate the interval between the two dates.
--TimeSpan interval = date2 - date1;
--
--// Display individual properties of the resulting TimeSpan object.
--Console.WriteLine("No of Days:", interval.Days); // 41
--Console.WriteLine("Total No of Days:", interval.TotalDays); // 41.0451
--Console.WriteLine("No of Hours:", interval.Hours); //1
--Console.WriteLine("Total No of Hours:", interval.TotalHours); // 985.084
--Console.WriteLine("No of Minutes:", interval.Minutes); // 5
--Console.WriteLine("Total No of Minutes:", interval.TotalMinutes); // 59105.833
--Console.WriteLine("No of Seconds:", interval.Seconds); // 5
--Console.WriteLine("Total No of Seconds:", interval.TotalSeconds); // 3546305.0
--Console.WriteLine("No of Milliseconds:", interval.Milliseconds); // 0
--Console.WriteLine("Total No of Milliseconds:", interval.TotalMilliseconds); // 3546305000
--Console.WriteLine("Ticks:", interval.Ticks); // 354630500000000
--*/
--</DEV/SUG>
--
--  type timedate (weak validation)
--      a sequence of between 8 and 10 integers, depending on whether it has a timezone.
--      The result from date() is a valid timedate, without a timezone, as is {0,0,0,0,0,0,0,0}.
--      This type speeds development by trapping errors (eg "number expected") faster, as opposed to
--      catching partial or unusable values. It will however holler if it gets a month -1 or 13, or
--      a day or -1 or 32. Where you see "td" below, assume it is a timedate
--
--  timedelta(atom weeks=0, days=0, hours=0, minutes=0, seconds=0, milliseconds=0, microseconds=0)
--      returns a value in seconds and fractions of a second (idea cribbed from python)
--      milli/microseconds are probably only useful in these routines in delta*bignumber cases.
--      eg: atom tendays = timedelta(days:=10)
--
--  adjust_timedate(timedate td, atom timedelta)
--      returns a timedate adjusted by the specified delta in seconds
--      eg: td = adjust_timedate(td, tendays)
--
--  set_timezone(timedate td, string newtz)
--      Set a specific timezone on an un-timezoned timedate.
--      eg: td = set_timezone(td, "BST")
--
--  change_timezone(timedate td, string newtz)
--      converts a timedate to a different timezone
--      eg: td = change_timezone(td, "EDT")
--
--  set_timedate_formats(sequence parse_fmts, object out_fmt=1, integer partial=0)
--      sets the parameter defaults for parse_date_string() and format_dateline().
--      parse_fmts is a collection of formats for parse_date_string(), and
--      out_fmt is a single string or index to parse_fmts for format_timedate().
--      the partial flag controls whether to ignore excess text when parsing,
--      and if set, then parse_fmts should be ordered most detailed first.
--      eg: set_timedate_formats({"Mmmm d yyyy h:mmam tz"})
--
--  parse_date_string(string s, sequence fmts=default_parse_fmts, integer partial=allow_partial)
--      convert a string into a timedate [using set_timedate_formats() defaults].
--      eg: td = parse_date_string("March 7 2009 7:30pm EST")
--
--  format_timedate(timedate td, string fmt=default_format)
--      convert a timedate into a string [using set_timedate_formats() defaults].
--      eg format_timedate(td) -- reconstitutes the preceding example
--
-- for advanced use/testing only:
-- =============================
--  override_timezone(string tz, string desc, atom adj, object dsr=0, string dst="")
--      dynamically modify/specify timezone details (for clash handling)
--
--  get_tzid(string tz)
--      get the internal integer id for a timezone (for testing purposes only)
--
--
-- Technical notes:
--  I completely replaced the julian date stuff as the OE one didn't seem to gel
--  with some timezone code I found, but I've since replaced the latter and realised
--  it was really an internal implementation detail rather than any real bug that I
--  could pin down: the upshot is that the julian dates this uses internally are 
--  wholly incompatible with std/datetime.e, not like that should bother anyone.
--
--  

--DEV until I've fixed it (earein.ew handling of switch statements, multi-assignment, etc.
--          I'll work on a copy of this file w/o the next line soon [he said on 17/9/15]):
--#without reformat

--include pdate.e -- days_in_month() [not pwa/p2js]

-- deliberately not global, to deter direct inspection:
constant DT_TZ = 9,
         DT_DSTZ = 10



--(Draft/Old)
--  Daylight Savings Time
--  =====================
--
--      These routines offer limited support for daylight savings time.
--      Only current US/EU/AU rules are implemented; I have created what
--      I think are all the needed DSTrule entries, but it's the testing
--      /local knowledge that is the hard part. I have no plans to ever 
--      implement all the rules dating back to 1916, but if that floats 
--      your boat... feel free. (See you the christmas after next.)
--      As implemented, daylight savings always start at 12am and end at 
--      1am, rather than the precise country-specific times.
--
--      If you define something in BST and want to know the equivalent
--      in GMT, or EDT, that's all fine and easy:
--
--          set_timedate_formats({"D/M/YYYY hhpm TZ"})
--          s = parse_date_string("1/8/2015 10am BST")
--          s = change_timezone(s,"GMT")
--          ?format_timedate(s)
--
--      If you add 4 months you want to get "1/12/2015 9am GMT", then add 
--      another 6 months and it should be "1/6/2015 10am BST". To do this,
--      every timedate has both [DT_TZ], which qualifies the value being
--      held, and [DT_DSTZ], which holds the "test/revert to" timezone.
--      Note that parsing say "25/12/2015 10am BST" will do the honorable
--      thing, and return the same kind of gibberish that you fed in.
--      Likewise, changetimezone("25/12/2015 10am GMT",BST) will yield a
--      value that a) still prints "10am GMT", and b) after adding 9 
--      months then prints "prints 9am BST". So, you convert it even when
--      it ain't got no immediate effect on display, capiche? It may also
--      be that you get "1/6/15 10am" but you know that means BST, so use
--      set_timezone(td,"BST"), which you can rest assured will do the 
--      right thing with 25/12 10am as well (ie leave it 10am GMT).
--
--      Finally, of course we don't really care about the geographical
--      locations these rules apply to, but it makes sense to keep two
--      entries separate if legislative changes could apply to one but
--      not the other, at some point in the future.

sequence DSTrule = {}       -- rules for start and end days of DST
function addDSTrule(object rule)
    DSTrule = append(DSTrule,rule)
    return length(DSTrule)
end function

constant EU = addDSTrule({3,25,10,25,1})                        -- Last Sunday in March to Last Sunday in October       (Europe)
constant US = addDSTrule({3,8,11,1,1})                          -- Second Sunday in March to first Sunday in November   (North America)
constant AU = addDSTrule({10,1,4,1,1})                          -- First Sunday in October to first sunday in April     (South Australia)

--  == Daylight Savings Time rules as of 2015 ==
-- Tried my best to double-check these (obviously many out-of-date websites exist with conflicting info),
--  but not tested any of them (a complete lack of any local knowledge is one very good reason why not).
--  I mainly used a "tzdata" thing that looked like it would be the best/most reliable, but it's huge...
--constant ?BRAZIL(SOUTH/CENT)? = addDSTrule({10,15,2,15,1})    -- Third Sunday in October to third Sunday in February  (also delays end during carnival week)
--constant ?FIJI? = addDSTrule({11,1,1,15,1})                   -- First Sunday in November to third Sunday in January
--constant ?FIJI2? = addDSTrule({11,1,1,18,1})                  -- First Sunday in November to Sunday>=18 in January
--constant ?MEXICO? = addDSTrule({4,1,10,25,1})                 -- First Sunday in April to last Sunday in October  (some exceptions, eg Sonora)
--constant ?CUBA? = addDSTrule({3,8,11,1,1})                    -- Second Sunday in March to first Sunday in November
--constant ?HAITI?==CUBA
--constant ?ISRAEL?    = addDSTrule({3,25,10,25,6,1})           -- Last Friday in March to Last Sunday in October
--constant ?JORDAN?    = addDSTrule({3,25,10,25,6,6})           -- Last Friday in March to Last Friday in October
--constant ?PALESTINE? = addDSTrule({3,25,10,22,6,6})           -- Last Friday in March to fourth Friday in October
--constant ?MONGOLIA? = addDSTrule({3,25,9,24,1})               -- Last Sunday in March to last Sunday in September (not 2007..2014)
--constant ?NAMIBIA? = addDSTrule({9,1,4,1,1})                  -- First Sunday in Septmber to First Sunday in April
--constant ?NEWZEALAND? = addDSTrule{(9,24,4,1,1})              -- Last Sunday in September to first Sunday in April
--constant ?SAMOA(?WESTERN?)? = NEWZEALAND
--constant ?PARAGUAY? - addDSTrule({10,1,3,22,1})               -- First Sunday in October to fourth Sunday in March
--constant ?SYRIA? = addDSTrule({3,25,10,25,6,6})               -- Last Friday in Match to last Friday in October (same as Jordan)
--constant ?ZION? = addDSTrule({3,23,10,25,6,1})                -- Friday on or after 23rd Match to last Sunday in October
-- Canada (excluding Saskatchewan and parts of Quebec, B.C., and Ontario), Bermuda, St. Johns, Bahamas, Turks and Caicos use the US rule.
-- Cayman Islands due to start DST in 2016
-- Chatham uses NZ rule but switches at 2:45am
-- Greenland switches over at 22/23:00 on a Saturday but otherwise same rule as EU.
-- Iran starts March 21-22 and ends September 21-22 (code for that one manually?) [for 1996 and on, use d = 21+(remainder(y,4)!=0)]
-- Lebanon, Kyrgyzstan, Moldova, and Turkey use the same rule as EU
-- Morocco and the parts of Western Sahara it controls uses the same rule as EU except that DST is stopped during Ramadan.
-- Some US bases in Antarctica use NZ DST, "Troll" whatver that is, uses the EU rule
-- 

--
-- Feel free to add more (non-clashing) entries, or set them up dynamically per
--  application using set_timezone() (see below). There are plenty of (out of date 
--  and conflicting websites out there with dirty great lists of timezone codes
--  (eg https://en.wikipedia.org/wiki/List_of_time_zone_abbreviations ), the trick 
--  is not typing them in but matching up with the correct daylight savings rule(s).
--
-- Britain is an obvious odd one out in terms of there being no "British Winter Time": 
--  GMT is not in any way tied to BST, it's the other way round. Other daylight savings
--  have a natural partner. For instance all areas that use ADT are part of AST, (eg/ie
--  Nova Scotia in Canada) but not all areas that use AST use ADT (eg Barbados). If in 
--  a region that uses daylight savings, and you want times to auto-toggle, obviously
--  when adjusted that is, then use the summer time with a non-zero tzDSR, 4th column.
--  It should be obviuous why the partner of {ADT,US,AST} is {AST,0,ADT} and is not
--  {AST,US,ADT} - the latter would toggle Barbados times to ADT. Strictly speaking,
--  it could equally be {AST,0,""}, as with no rule tzDSL could not be used anyway,
--  but somehow pairing them up feels better, even with no possible practical use.
--
-- In the following incomplete table I have commented out anything that might be even
--  just a little bit dubious, and not attempted anything with no suitable tzDSR value.
--  Feel free to add/uncomment/correct as you see fit.
--

constant tdaDD = {{"UTC", "Coordinated Universal Time"          ,+0,    0,  ""},        -- (must be first entry [0->1]'s)
--                {"BST", "Bangladesh Standard Time"            ,+6,    0,  ""},        -- Note: collision with British Summer Time
                  {"BST", "British Summer Time"                 ,+1,    EU, "GMT"},
                  {"GMT", "Greenwich Mean Time"                 ,+0,    0,  ""},
                  {"CEST","Central European Summer Time"        ,+2,    EU, "CET"},
                  {"CET", "Central European Time"               ,+1,    0,  "CEST"},
                  {"EEST","Eastern European Summer Time"        ,+3,    EU, "EET"},
                  {"EET", "Eastern European Time"               ,+2,    0,  "EEST"},
                  {"IST", "Irish Standard Time"                 ,+1,    EU, "GMT"},     -- or WET?
--                {"KUYT","Kuybyshev Time"                      ,+4,    0,  ""},
--                {"MSD", "Moscow Daylight Time"                ,+4,    0,  "MSK"},     -- dead, I think
--                {"MSK", "Moscow Standard Time"                ,+3,    0,  ""},
--                {"SAMT","Samara Time"                         ,+4,    0,  ""},
                  {"WEST","Western European Summer Time"        ,+1,    EU, "WET"},
                  {"WET", "Western European Time"               ,+0,    0,  "WEST"},
                  {"ADT", "Atlantic Daylight Time"              ,-3,    US, "AST"},
                  {"AST", "Atlantic Standard Time"              ,-4,    0,  "ADT"},
                  {"AKDT","Alaska Daylight Time"                ,-8,    US, "AKST"},
                  {"AKST","Alaska Standard Time"                ,-9,    US, "AKDT"},
                  {"CDT", "Central Daylight Time"               ,-5,    US, "CST"},
                  {"CST", "Central Standard Time"               ,-6,    0,  "CDT"},
                  {"EDT", "Eastern Daylight Time"               ,-4,    US, "EST"},
                  {"EST", "Eastern Standard Time"               ,-5,    0,  "EDT"},
                  {"EGST","Eastern Greenland Summer Time"       ,+0,    EU, "EGST"},
                  {"EGT", "Eastern Greenland Time"              ,-1,    0,  "EGT"},
--                {"EST", "Egypt Standard Time",                ,+2,    0,  ""},        -- Note: collision with Eastern Standard Time
                  {"HADT","Hawaii-Aleutian Daylight Time"       ,-9,    US, "HAST"},
                  {"HAST","Hawaii-Aleutian Standard Time"       ,-10,   0,  "HADT"},
                  {"MDT", "Mountain Daylight Time"              ,-6,    US, "MST"},
                  {"MST", "Mountain Standard Time"              ,-7,    0,  "MDT"},
                  {"NDT", "Newfoundland Daylight Time"          ,-2.5,  US, "NST"},
                  {"NST", "Newfoundland Standard Time"          ,-3.5,  0,  "NDT"},
                  {"PDT", "Pacific Daylight Time"               ,-7,    US, "PST"},
                  {"PST", "Pacific Standard Time"               ,-8,    0,  "PDT"},
--                {"PMDT","Pierre & Miquelon Daylight Time"     ,-2,    0,  "PMST"},
--                {"PMST","Pierre & Miquelon Standard Time"     ,-3,    0,  "PMDT"},    -- [[UTC-03:00]] (PMST) — [[French Guiana]], [[Saint Pierre and Miquelon]] 
                  {"WGST","Western Greenland Summer Time"       ,-2,    EU, "WGT"},
                  {"WGT", "West Greenland Time"                 ,-3,    0,  "WGST"},
--                {"IST", "India Standard Time"                 ,+5.5,  0,  ""},        -- Note: collision with Irish Standard Time
--                {"IST", "Israel Standard Time",               ,+2,    0,  ""},        -- Note: collision with Irish Standard Time
--                {"PKT", "Pakistan Standard Time"              ,+5,    0,  ""},
--                {"NPT", "Nepal Time"                          ,+5.75, 0,  ""},
--                {"BTT", "Bhutan Time"                         ,+6,    0,  ""},
--                {"BIOT","British Indian Ocean Territory Time" ,+6,    0,  ""},
--                {"MVT", "Maldives Time"                       ,+5,    0,  ""},
--                {"CCT", "Cocos Islands Time"                  ,+6.5,  0,  ""},
--                {"TFT", "French Southern and Antarctic Time"  ,+5,    0,  ""},
--                {"MST", "Myanmar Standard Time",              ,+6.5,  0,  ""},        -- Note: collision with Mountain Standard Time  (MMT?)
                  {"ACST","Australian Central Standard Time"    ,+9.5,  0,  "ACDT"},
                  {"ACDT","Australian Central Daylight Time"    ,+10.5, AU, "ACST"},
                  {"AEST","Australian Eastern Standard Time"    ,+10,   0,  "AEDT"},
                  {"AEDT","Australian Eastern Daylight Time"    ,+11,   AU, "AEST"},
                  {"AWST","Australian Western Standard Time"    ,+8,    0,  ""},
                  $}
sequence {timezones, tzdescs, tzadjs, tzDSR, tzDSL} = columnize(tdaDD)

--some other leftovers that may be helpful (or not):
-- [[UTC-09:00]] ([[Alaska Time Zone|AKT]]) — most of the state of [[Alaska]]
-- [[UTC-03:00]] ([[Time in Argentina|ART]])
-- [[UTC+02:00]] ([[Central Africa Time|CAT]])
-- [[UTC+10:00]] ([[Chamorro Time Zone|ChT]]) — [[Guam]] and the [[Northern Mariana Islands]]
-- [[UTC+07:00]] ([[Christmas Island Time|CXT]]) — [[Christmas Island]]
-- [[UTC+03:00]] ([[East Africa Time|EAT]])
-- [[UTC+03:00]] ([[Further-eastern European Time|FET]])
-- [[UTC-03:00]] ([[Falkland Islands Standard Time|FKST]]) — [[Falkland Islands]]
-- [[UTC-06:00]] ([[Galápagos Time|GALT]]) — [[Galápagos Province, Ecuador|Galápagos Province]]
-- [[UTC-10:00]] ([[Hawaii–Aleutian Time Zone|HAT]]) —  [[Hawaii]], most of the [[Aleutian Islands]], and [[Johnston Atoll]]
-- [[UTC+08:00]] ([[Hong Kong Time|HKT]])
-- [[UTC+03:30]] ([[Iran Standard Time|IRST]])
-- [[UTC+09:00]] ([[Japan Standard Time|JST]])
-- [[UTC+11:30]] ([[Norfolk (Island) Time|NFT]]) — [[Norfolk Island]]
-- [[UTC-05:00]] ([[Time in Peru|PET]])
-- [[UTC+08:00]] ([[Philippine Standard Time|PHT]])
-- [[UTC+05:30]] ([[Sri Lanka Standard Time|SLST]])
-- [[UTC+08:00]] ([[Singapore Standard Time|SST]])
-- [[UTC-11:00]] ([[Samoa Time Zone|ST]]) — [[American Samoa]], [[Jarvis Island]], [[Kingman Reef]], [[Midway Atoll]] and [[Palmyra Atoll]]
-- [[UTC+01:00]] ([[West Africa Time|WAT]])

--DEV unused as yet...
constant ktz = {{"ADT", "HAA"},
                {"AKDT","HAY"},
                {"AKST","HNY"},
                {"AST", "HNA"},
                {"CDT", "HAC"},
                {"CST", "HNC"},
                {"CET", "CRT"},
                {"EDT", "HAE"},
                {"EST", "HNE"},
                {"EST", "ET" },
                {"MDT", "HAR"},
                {"MST", "HNR"},
--              {"NDT", "HAT"},     -- clash with HAST/HADT?
                {"NST", "HNT"},
                {"PDT", "HAP"},
                {"PST", "HNP"},
                {"PST", "PT" }, 
--              {"BIOT","IOT"},
                {"CST", "CT" },
                {"MST", "MT" },
                $}
constant {alttzkeys,alttz} = columnize(ktz)
if sequence(alttzkeys) or sequence(alttz) then end if

sequence validtd = repeat(0,DT_DSTZ)    -- (say it out loud!)
    validtd[DT_YEAR] = {1,0}            -- do not test
    validtd[DT_MONTH] = {0,12}
    validtd[DT_DAY] = {0,31}
    validtd[DT_HOUR] = {0,23}
    validtd[DT_MINUTE] = {0,59}
    validtd[DT_SECOND] = {0,59}
--  validtd[DT_DOW] = {0,7}
    validtd[DT_DOW] = {0,999}   -- accomodate milliseconds
    validtd[DT_DOY] = {0,366}
    -- (the following two entries are updated in override_timezone)
    validtd[DT_TZ] = {0,length(timezones)}
    validtd[DT_DSTZ] = {0,length(timezones)}

without trace
global type timedate(object s)
-- result from date() should pass as well as those with timezome info
-- note this must permit the result of eg parse_date_time, which can
-- be {0,0,0,0,0,0,0,0}.
object si
integer vmin, vmax
    if sequence(s)
--25/4/19:
--  and length(s)>=DT_DOY then
    and length(s)>=DT_SECOND then
        for i=1 to length(s) do
            si = s[i]
            if not integer(si) then return 0 end if
            {vmin,vmax} = validtd[i]
            if vmin<=vmax then
                if si<vmin
                or si>vmax then
                    return 0
                end if
            end if
        end for
--added 20/11/19:
--7/5/20:
--      if s[DT_YEAR]!=0 and s[DT_MONTH]!=0 and s[DT_DAY]!=0
        if s[DT_YEAR]>=1752 and s[DT_MONTH]!=0 and s[DT_DAY]!=0
        and s[DT_DAY]>days_in_month(s[DT_YEAR],s[DT_MONTH]) then
            return 0
        end if
        return 1
    end if
    return 0
end type
--with trace

global function timedelta(atom weeks=0, atom days=0, atom hours=0, atom minutes=0, atom seconds=0, atom milliseconds=0, atom microseconds=0)
--
-- The parameters are expected to be named: while 7 hours and 30 minutes could legally be defined using timedelta(0,0,7,30),
--  it is entirely expected the far more readable timedelta(hours:=7, minutes:=30) or timedelta(hours:=7.5) be used instead.
--
-- The parameters are defined as atoms to allow huge and/or fractional values to be passed.
--
-- 1,000 milliseconds equal 1 second and 1,000 microseconds equal 1 millisecond (and 1,000,000 microseconds equal 1 second)
--
-- Returns an atom representing the timedelta in seconds and fractions of a second.
--
-- Fairly obviously, I should hope, invoking timedelta(seconds:=a) is a rather pointless exercise.
--
-- timedeltas up to one year should be accurate to within one microsecond or better,
-- timedeltas up to one thousand years should be accurate to within one millisecond or better,
-- timedeltas up to one million years should be accurate to within one second or better.
-- timedeltas up to one billion years should be accurate to within 15 minutes or better.
-- timedeltas up to the age of the universe should be accurate to within 4 hours or better.
--  all of the above accuracy claims are rendered completely moot by the following statements:
-- timedeltas have no notion of and will account for neither leap years nor leap seconds; an
--  application programmer wishing to alter dates by more than 4 weeks is advised to take care
--  of any whole months/years separately and manually, perhaps with a loop to adjust month by
--  12 and year by 1 until month is between 1 and 12, probably requiring standard/leap fiddles
--  for days near the end of the month, but days and below via a timedelta, and anything like 
--  first or last thursday of the month should probably not be using this routine at all.
-- timedeltas have no notion of and will account for neither general relativity nor worm holes;
--  passengers travelling near the speed of light or passing through time portals should avoid 
--  attempting to apply timedeltas to any correspondingly incompatible timeframes.
--
    return ((((weeks*7)+days)*24+hours)*60+minutes)*60+seconds+milliseconds*0.001+microseconds*0.000001
end function

--  Format Strings (modified)
--  Date and time format elements for parsing/printing are defined by the following groups of characters:
--
--  Element  Aliases     Description  
--  d           D       One- or two-digit day.  
--  dd          DD      Two-digit day. Single-digit day values are preceded by a zero.  
--  th       st/TH/ST   Two-character ordinal suffix. (lower/uppercase)  Must immediately follow d/D/dd/DD.
--  ddd      Ddd/DDD    Three-character weekday abbreviation.  (lowercase/capitalised/uppercase)
--  dddd    Dddd/DDDD   Full weekday name.  (lowercase/capitalised/uppercase)
--  h           H       One- or two-digit hour.                                    (See am/pm notes below.)
--  hh          HH      Two-digit hour. Single-digit values are preceded by a zero. ""
--  m           mm      Two-digit minute. Single-digit values are preceded by a zero.        (NB not M/MM)
--  s        ss/S/SS    Two-digit second. Single-digit values are preceded by a zero.
--  pm       am/PM/AM   Two-letter am/pm abbreviation, prior hour must exist. (lower/uppercase)
--  M                   One- or two-digit month number.                                      (NB not m)
--  MM                  Two-digit month number. Single-digit values are preceded by a zero.  (NB not mm)
--  mmm      Mmm/MMM    Three-character month abbreviation. (lowercase/capitalised/uppercase)
--  mmmm    Mmmm/MMMM   Full month name.  (lowercase/capitalised/uppercase)
--  y        yy/Y/YY    One- or two-digit year.
--  yyyy       YYYY     Four-digit year.
--  tz          TZ      Three- or four-character uppercase time zone
--  tzz      Tzz/TZZ    Full time zone name. (capitalised)
--
-- SUG:
--  n       nnn/N/NNN   The day of year. (nn/NN are also acceptable)
--
--  Notes: Special attention is required to avoid confusing m/mm (minute) and M/MM (month).
--         Spaces and punctuation (other than single quotes) in a format are treated as literals.
--         Any required literal alphanumerics must be enclosed in single quotes, with two adjacent
--         single quotes treated as one single quote, eg "'Today''''s date is 'DD/MM/YYYY.") which
--         yields/parses eg "Today's date is 14/09/2015."
--         Feel free to print fragments, eg "dst", "Mmmm", "tz", and post-process/stitch together.
--         At the moment td_ordinals (1st/2nd/3rd/4th/etc) are only supported in/for english.
--         Minutes and seconds <10 cannot be printed without a leading 0, but month/day/hours can.
--         DD/dd/mm/MM will refuse to parse single digit inputs, however d/D/h/H/m/M/s will.
--         lowercase/uppercase actually match any case input, whereas capitalised is more strict;
--          it is really for output that all those different distinctions are available anyway.
--         Obviously uppercase/lowercase format specifiers make no difference on numeric fields.
--         A timezone is always parsed and printed in uppercase. tzz/Tzz/TZZ as-is, see tzdesc.
--         When am/pm is parsed/printed, it adjusts a previous hour (error if none) appropriately.
--          There is no way to print a 12-hour time without also printing an am/pm indicator.
--         The times "12:15am" and "2:15pm" are internally stored as {0,15} and {14,15} respectively,
--          and reconstituted exactly they were as long as you use the same format string.
--         Some languages, eg de, use a two-character weekday abbreviation - but the format string 
--          used to obtain said is still three characters long (ddd/Ddd/DDD).
--         Great care has been taken to avoid any potential ambiguity, the only potential mishap
--          is "stz" which opts for "st" (aka "th") which will probably error out because it
--          does not immediately follow a d, and if it was "dstz" then it would error on the 
--          "z" anyway. Should you really want seconds hard pressed against a timezone with 
--          no separating space, use "sstz" instead.
--

--constant true = (1=1), false = not true

enum TD_LITERAL,
     YEAR,
     MONTH,
     DAY,
     DOW,
     DOY,
     HOUR,
     MINUTE,
     SECOND,
     MSEC,
     AM,
     TZ,
     TH

enum en,
     langmax = $

integer currlang = en

sequence months      = repeat(0,langmax),
         day_names   = repeat(0,langmax),
--       shortdaylen = repeat(3,langmax),
         td_ordinals = repeat({""},langmax),
         ampm        = repeat(0,langmax)

--DEV
--,     langcodes   = repeat(0,langmax)
--langcodes[en] = "EN"  -- English
--langcodes[de] = "DE"  -- German
--langcodes[es] = "ES"  -- Spanish
--langcodes[fi] = "FI"  -- Finnish
--langcodes[fr] = "FR"  -- French
--langcodes[it] = "IT"  -- Italian
--langcodes[nl] = "NL"  -- Dutch
--langcodes[pt] = "PT"  -- Portugese
--global procedure set_timedate_lang(string langcode)
--  currlang = find(upper(langcodes),langcode)
--  if currlang=0 then ?9/0 end if
--end procedure
--English:
--January February March April May June July August September October November December 
--Sunday Monday Tuesday Wednesday Thursday Friday Saturday

months[en] = {"January","February","March","April","May","June","July",
              "August","September","October","November","December"}
--12/1/2020:
--day_names[en] = {"Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"}
day_names[en] = {"Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"}
--Google Translate:
--months[de] = {"Januar","Februar","März","April","Mai","Juni","Juli",
--              "August","September","Oktober","November","Dezember"}
--day_names[de] = {"Sonntag","Montag","Dienstag","Mittwoch","Donnerstag","Freitag","Samstag"}
--months[es] = {"Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio",
--              "Agosto","Septiembre","Octubre","Noviembre","Diciembre"}
--day_names[es] = {"Domingo","Lunes","Martes","Miércoles","Jueves","Viernes","Sábado"}
--months[fi] = {"Tammikuu","Helmikuu","Maaliskuu","Huhtikuu","Toukokuu","Kesäkuu",
--              "Heinäkuu","Elokuu","Syyskuu","Lokakuu","Marraskuu","Joulukuu"}
--day_names[fi] = {"maanantai","tiistai","keskiviikko","torstai","perjantai","lauantai"}
--months[fr] = {"Janvier","Février","Mars","Avril","Mai","Juin","Juillet",
--              "Août","Septembre","Octobre","Novembre","Décembre"}
--day_names[fr] = {"Dimanche","Lundi","Mardi","Mercredi","Jeudi","Vendredi","Samedi"}
--months[it] = {"Gennaio","Febbraio","Marzo","Aprile","Maggio","Giugno","Luglio",
--              "Agosto","Settembre","Ottobre","Novembre","Dicembre"}
--day_names[it] = {"domenica","lunedì","martedì","mercoledì","giovedì","venerdì","sabato
--months[nl] = {"Januari","februari","maart","april","mei","juni","juli",
--              "augustus","september","oktober","november","december"}
--day_names[nl] = {"zondag","maandag","dinsdag","woensdag","donderdag","vrijdag","zaterdag"}
--months[pt] = {"Janeiro","Fevereiro","Março","Abril","Maio","Junho","Julho",
--              "Agosto","Setembro","Outubro","Novembro","Dezembro"}
--day_names[pt] = {"domingo","segunda","terça","quarta","quinta","sexta","sábado"}

td_ordinals[en] = {"st","nd","rd","th"}
--td_ordinals[fr] = {"er","e"} --??

--DEV (only non-3 are rqd)
--shortdaylen[de] = 2

ampm[en] = {"am","pm"}


-- after updates to check the hours - not entirely perfect, but 5*1hr fwd then back gets:
--          "2015/03/28 10pm GMT"
--          "2015/03/28 11pm GMT"
--          "2015/03/29 1am BST"    -- (really 12am GMT)
--          "2015/03/29 2am BST"
--          "2015/03/29 3am BST"
--          "2015/03/29 4am BST"
--          "2015/03/29 3am BST"
--          "2015/03/29 2am BST"
--          "2015/03/29 1am BST"    -- ("")
--          "2015/03/28 11pm GMT"
--          "2015/03/28 10pm GMT"
--          ==
--          "2015/10/24 10pm BST"
--          "2015/10/24 11pm BST"
--          "2015/10/25 12am BST"
--          "2015/10/25 1am BST"
--          "2015/10/25 1am GMT"
--          "2015/10/25 2am GMT"
--          "2015/10/25 1am GMT"
--          "2015/10/25 1am BST"
--          "2015/10/25 12am BST"
--          "2015/10/24 11pm BST"
--          "2015/10/24 10pm BST"

function isDSTr(timedate td, integer ruleidx)
integer {y,m,d,h} = td
sequence dsrule = DSTrule[ruleidx]
integer sm,sd,em,ed,wd  -- (start/end month/day weekday)
integer prevSun -- (can be -ve near the start of the month)
    if y<1916 then return false end if -- (not that this has any rules for 1916..2014)
    {sm,sd,em,ed,wd} = dsrule
    if sm<em then -- northern hemisphere
        if m<sm or  m>em then return false end if
        if m>sm and m<em then return true end if
    else -- down under
        if m>sm  or m<em then return true end if
        if m<sm and m>em then return false end if
    end if
    if wd!=1 then ?9/0 end if   -- different code needed for Friday handling, 
--12/01/2020 (See readme. Without testing, I am just going to map dow back to Sun..Sat)
--  prevSun = d - day_of_week(y,m,d) + 1
    integer dow = {2,3,4,5,6,7,1}[day_of_week(y,m,d)]
    prevSun = d - dow + 1
--  if m=sm then return prevSun>=sd end if
    if m=sm then
        if length(td)<DT_DSTZ
        or td[DT_TZ]=td[DT_DSTZ] then
            if d=prevSun then
                return h>=1
            end if
        else
            if d=prevSun+1 then
                return h>=1
            end if
        end if
        return prevSun>=sd
    end if
    if length(dsrule)!=5 then ?9/0 end if   -- .. plus wd=dsrule[6] for Israel etc.
--  if m=em then return prevSun<ed end if
    if m=em then -- (always true)
        if d=prevSun then
            if length(td)<DT_DSTZ
            or td[DT_TZ]=td[DT_DSTZ] then
                return h<2
            end if
            return h<1
        end if
        return prevSun<ed
    end if
    ?9/0 -- should never happen
end function

--DEV move to test routine?
--if isDSTr({2015, 8, 1},EU)=0 then ?9/0 end if
--if isDSTr({2015,12,25},EU)=1 then ?9/0 end if
--if isDSTr({2015, 3,28},EU)=1 then ?9/0 end if
--if isDSTr({2015, 3,29},EU)=0 then ?9/0 end if
--if isDSTr({2015,10,24},EU)=0 then ?9/0 end if
--if isDSTr({2015,10,25},EU)=1 then ?9/0 end if

function get_DST_rule(timedate td, integer DT_TZX)
integer tz = td[DT_TZX]
    if tz=0 then return 0 end if
    return tzDSR[tz]
end function

--DEV??
--function isDST(timedate td)
--integer dsrule = get_DST_rule(td,DT_TZ)
--  if dsrule=0 then return 0 end if
--  return isDSTr(td,dsrule)
--end function

global function get_tzid(string tz)
-- for testing purposes only
    return find(tz,timezones)
end function

--global function get_tzdesc(object tz)
---- tz should be a string, eg "GMT", or an integer from td[DT_TZ] or td[DT_DSTZ]
---- use format_timedate(td,"tz") to get timezones[tz] (ie "" or "" -> "GMT" etc)
--  if string(tz) then tz = find(tz,timezones) end if
--  return {tz,timezones[tz],tzdescs[tz],tzadjs[tz]} end if
--  --get_tzdesc(object tz, integer retadj=0) (deprecated/commented out)
--  --  get more internal details (for testing/diagnostics only)
--end function

global procedure override_timezone(string tz, string desc, atom adj, object dsr=0, string dst="")
--
-- Time zone abbreviations are not necessarily unique, eg BST may refer to 
--  British Summer Time or Bangladesh Standard Time. Other collisions include 
--  EST (Eastern/Egypt), IST (Irish/India/Iran/Israel), and MST (Mountain/Myanmar).
-- This routine allows you to replace any such clashes, or add new entries. The 
--  table cannot meaningfully hold two versions of a timezone code, so instead the 
--  application must swap any clashes in and out as required. Less importantly but 
--  still perhaps beneficial is the ability to extend the tables without modifying 
--  the (this) original source code.
-- Unfortunately there is no international standard that includes GMT/BST/etc, and 
--  *ALL* of eg "America/Indiana/Indianapolis" is not something end-users are very
--  likely to want or agree to type in, or for that matter select from a drop-down 
--  of well over 400 (!!) such beasts.
--
-- Example:
--  -- (this will revert things to their original settings)
--  override_timezone("BST","British Summer Time",+1,{3,25,10,25,1},"GMT")
--
integer k = find(tz,timezones), l, dsrdx
    if length(tz)=0 then ?9/0 end if
    if length(desc)=0 then ?9/0 end if
    if sequence(dsr) then
        l = length(dsr)
        if l<5 or l>6 then ?9/0 end if
        if dsr[5]!=1 and l!=6 then ?9/0 end if
        dsrdx = find(dsr,DSTrule)
        if dsrdx=0 then
            dsr = addDSTrule(dsr)
        end if
    else
        if dsr<0 or dsr>length(DSTrule) then ?9/0 end if
        dsrdx = dsr
    end if
    if k!=0 then
        tzdescs[k] = desc
        tzadjs[k] = adj
        tzDSR[k] = dsrdx
        tzDSL[k] = dst
    else
        timezones = append(timezones,tz)
        tzdescs = append(tzdescs,desc)
        tzadjs = append(tzadjs,adj)
        tzDSR = append(tzDSR,dsrdx)
        tzDSL = append(tzDSL,dst)
        validtd[DT_TZ][2] = length(timezones)
        validtd[DT_DSTZ][2] = length(timezones)
    end if
end procedure


function timedate_to_julian_day(timedate td)
--
-- A julian day is the number of days since Jan 1st 4713 BC in the julian calandar,
--  which is Nov 24th 4714 BC in the gregorian calendar, the one we use today.
--  Effectively a julian date is a meaningless number that, typically after some 
--  simple adjustment (eg +10 days), can be converted back to a meaningful date.
--
-- Values returned are for noon on the given day. The parameter is always treated 
--  as a gregorian date, hence results prior to Oct 15 1582 may differ from other 
--  methods/languages, that is if they treat such as julian dates rather than as
--  back-dated gregorian calendar dates.
--
--integer {y,m,d} = td
--integer m1 = -(m<=2)
--  return floor((1461*(y+4800+m1))/4)+
--         floor((367*(m-2-12*m1))/12)-
--         floor((3*((y+4900+m1)/100))/4)+
--         d-32075
--end function
--
---- The following routine (from wikipedia) gives the same results. -- 22/5/19 oh no it does not!
--function jd2(timedate td)
integer {y,m,d} = td
integer m1 = (m<=2)
    y += 4800-m1
    m += 12*m1-3
    return d+floor((153*m+2)/5)+365*y+floor(y/4)-floor(y/100)+floor(y/400)-32045
end function
--
--?timedate_to_julian_day({2000,1,1,0,0,0,0,0})
--?jd2({2000,1,1,0,0,0,0,0})
--?timedate_to_julian_day({1500,1,1,0,0,0,0,0})
--?jd2({1500,1,1,0,0,0,0,0})

function julian_day_to_timedate(integer jd, integer hour, integer mins, integer secs, integer ms)
-- convert an integer julian day back to {y,m,d} form, and throw in the passed h,m,s
integer l = jd+68569
integer i, j
integer d, m, y
integer n = floor((4*l)/146097)
        l = l-floor((146097*n+3)/4)
        i = floor((4000*(l+1))/1461001)
        l = l-floor((1461*i)/4)+31
        j = floor((80*l)/2447)
        d = l-floor((2447*j)/80)
        l = floor(j/11)
        m = j+2-(12*l)
        y = 100*(n-49)+i+l
    return {y,m,d,hour,mins,secs,ms}
end function

constant day_in_seconds = 24*60*60  -- (=86400)

function timedate_to_seconds(timedate td)
-- returns an atom
--22/3/18:
--  return timedate_to_julian_day(td)*day_in_seconds+(td[DT_HOUR]*60+td[DT_MINUTE])*60+td[DT_SECOND]+td[DT_MSEC]/1000
--25/4/19:
--  return (timedate_to_julian_day(td)-2440588)*day_in_seconds+(td[DT_HOUR]*60+td[DT_MINUTE])*60+td[DT_SECOND]+td[DT_MSEC]/1000
    atom s = (td[DT_HOUR]*60+td[DT_MINUTE])*60+td[DT_SECOND]
    if length(td)>=DT_MSEC then s+=td[DT_MSEC]/1000 end if
    return (timedate_to_julian_day(td)-2440588)*day_in_seconds+s
end function

function seconds_to_timedate(atom seconds)
integer days, minutes, hours, milliseconds

--22/3/18:
--  days = floor(seconds/day_in_seconds)
    days = floor(seconds/day_in_seconds)+2440588
    seconds = remainder(seconds, day_in_seconds)

    hours = floor(seconds/3600)
    seconds -= hours*3600

    minutes = floor(seconds/60)
    seconds -= minutes*60

    milliseconds = floor((seconds-floor(seconds))*1000)
    seconds = floor(seconds)

    return julian_day_to_timedate(days, hours, minutes, seconds, milliseconds)
end function


global function adjust_timedate(sequence td, atom delta)
integer y, m, d, dsrule, tz, stz
    atom secs = timedate_to_seconds(td)
    secs += delta
--p2js
    td = deep_copy(td)
    td[1..7] = seconds_to_timedate(secs)
    delta = 0
    if length(td)=DT_DSTZ then
        dsrule = get_DST_rule(td,DT_DSTZ)
        if dsrule!=0 then
            tz = td[DT_TZ]
            stz = td[DT_DSTZ]
            if isDSTr(td,dsrule) then
                if tz!=stz then
                    -- modify ..,GMT,BST} to ..,BST,BST}
                    delta = -tzadjs[tz]*3600
                    td[DT_TZ] = stz
                    delta += tzadjs[stz]*3600
                end if
            else
                if tz=stz then
                    -- modify ..,BST,BST} to ..,GMT,BST}
                    stz = find(tzDSL[tz],timezones)
                    delta = tzadjs[stz]*3600
                    td[DT_TZ] = stz
                    delta -= tzadjs[tz]*3600
                end if
            end if
        end if
    elsif length(td)=DT_TZ then
        dsrule = get_DST_rule(td,DT_TZ)
        if dsrule!=0 then
            if not isDSTr(td,dsrule) then
                -- modify ..,BST} to ..,GMT,BST}
                tz = td[DT_TZ]
                stz = find(tzDSL[tz],timezones)
                td = append(td,tz)
                delta = tzadjs[stz]*3600
                td[DT_TZ] = stz
                delta -= tzadjs[tz]*3600
            end if
        end if
    end if
    if delta!=0 then
        td[1..7] = seconds_to_timedate(timedate_to_seconds(td)+delta)
    end if
--4/3/18 don't clobber DT_MSECS!
--  if length(td)>=DT_DOW then
--      td[DT_DOW] = day_of_week(td)
--      {y,m,d} = td
--      td[DT_DOW] = day_of_week(y,m,d)
        if length(td)>=DT_DOY then
            {y,m,d} = td
--          td[DT_DOY] = day_of_year(td)
            td[DT_DOY] = day_of_year(y,m,d)
        end if
--  end if
    return td
end function



global function set_timezone(timedate td, string newtz)
--
-- Set a specific timezone on an un-timezoned timedate.
-- For example if you have processed/are processing a file with no timezones 
--  but you know, or just want to pretend, they are all in GMT. Or maybe you 
--  can augment a specific user's input because you know where they live, or 
--  their account has a default.
--
-- This routine allows you to treat "3am" (plus a date) as "3am EDT";
--  - see change_timezone() for "what is 3am GMT in EDT?".
--
-- Specifying an unknown timezone causes a fatal error.
-- If a timedate already has a timezone it is returned unmodified.
--
-- Note that setting a DST out-of-season returns a ..,base,dst} timedate,
--  eg set_timezone(parse_date_string("25/12/2015"),"BST") returns a
--      timedate of {2015,12,25,0,0,0,dow,doy,GMT,BST}, whereas
--     set_timezone(parse_date_string("25/07/2015"),"BST") returns a
--      timedate of {2015, 7,25,0,0,0,dow,doy,BST,BST}.
--
integer tz = find(newtz,timezones),
        dsrule = tzDSR[tz]
--  if tz=0 then ?9/0 end if    -- (above line will crash anyway!)
--p2js:
    td = deep_copy(td)
    if length(td)>=DT_TZ then
        if td[DT_TZ]!=0 then return td end if
    else
        td = append(td,0)
        if length(td)!=DT_TZ then ?9/0 end if
    end if
    if dsrule!=0 then
        if length(td)<DT_DSTZ then
            td = append(td,tz)
            if length(td)!=DT_DSTZ then ?9/0 end if
        else
            td[DT_DSTZ] = tz
        end if
        if not isDSTr(td,dsrule) then
            tz = find(tzDSL[tz],timezones)
        end if
    end if
    td[DT_TZ] = tz
    return td
end function


global function change_timezone(timedate td, string newtz)
--
-- If there is no existing timezone, GMT is assumed.
-- Use set_timezone before calling this to change that default behaviour.
--
-- technical details
-- =================
--
--  valid timezones are:
--
--      ..}         \
--      ..,0}        }=no timezone (assume UTC/GMT)
--      ..,0,?}     /
--      ..,tz},         \=single timezone, tzadjs[tz] applied
--      ..,tz,0}        /
--      ..,dstz,dstz}   - daylight savings in-season
--      ..,tz,dstz}     - daylight savings out-of-season
--
integer tz, prevtz
atom hourdelta = 0

    tz = find(newtz,timezones)
--  if tz=0 then ?9/0 end if    -- (next line will crash anyway!)
    hourdelta = tzadjs[tz]
--p2js:
    td = deep_copy(td)
    if length(td)>=DT_TZ then
        prevtz = td[DT_TZ]
        if prevtz!=0 then
            hourdelta -= tzadjs[prevtz]
            td[DT_TZ] = tz
            if length(td)>=DT_DSTZ then
-- removed 20/2/18:
--              if not find(td[DT_DSTZ],{0,prevtz}) then
--                  if tzadjs[prevtz]!=0 then ?9/0 end if   -- sanity check
--              end if
                td[DT_DSTZ] = tz
            end if
        end if
    else
        td = append(td,tz)
        if length(td)!=DT_TZ then ?9/0 end if
    end if
    if hourdelta!=0 then
        td = adjust_timedate(td,hourdelta*3600)
    end if
    return td
end function


constant edescs = {"literal character mismatch",    -- 1
                   "unrecognised",                  -- 2
                   "number expected",               -- 3
                   -- the following mean we cannot calculate day_of_week(y,m,d):
                   "year is zero",                  -- 4
                   "invalid month",                 -- 5
                   "invalid day",                   -- 6
                   "unrecognised/illegal literal",  -- 7
                   "invalid timedate",              -- 8    -- no longer used?
                   "format code expected",          -- 9
                   "attempt to parse null string",  -- 10
                   "invalid hour",                  -- 11
                   "invalid hour for am/pm",        -- 12
                   "invalid minutes",               -- 13
                   "invalid seconds",               -- 14
                   "no prior hour for am/pm",       -- 15
                   "no prior day for th",           -- 16
                   "no closing quote",              -- 17
                   "unparsed input",                -- 18
                   "incomplete input",              -- 19
                   "month next to hour/second",     -- 20
                   "minute next to year/day",       -- 21
                   "invalid milliseconds",          -- 22
                   $}

object ecxtra = 0

function nxtch(string s, integer idx, integer ch, integer uppercase, integer len)
--
-- internal, non-damaging test function
-- returns true if there are len [case insensitive] instances of ch at s[idx+1..]
--
    if uppercase then
        ch = upper(ch)
    end if
    for i=1 to len do
        if i+idx>length(s) then return false end if
        if uppercase then
            if upper(s[i+idx])!=ch then return false end if
        else
            if s[i+idx]!=ch then return false end if
        end if
    end for
    return true
end function

--Aside:
-- I've put quite a few break statements in the switch constructs, but because the latter are not
--  declared "with fallthrough", they achieve nothing and represent the default behaviour anyway.
--  In contrast the explicit fallthough statements in this code are entirely necessary.

function next_fmt(string fmt, integer fmtdx)
integer ch
integer fcase = 0   -- lower, 1=upper
integer fsize = 1
integer ftyp  = 0
integer ecode = 0
integer closequote

    fmtdx += 1
    ch = fmt[fmtdx]
    --
    -- parse a "token" character-by-character.
    -- note that eg "mmam" is {mm,am}, iyswim.
    --
    switch ch do
        case 'A','P':
                    fcase = 1
                    fallthrough
        case 'a','p':
                    if nxtch(fmt,fmtdx,'m',fcase,1) then
                        fmtdx += 1
                        ftyp = AM
                        fsize = 2
                    --(else error, as ftyp=0 below)
                    end if
                    break
        case 'D':   fcase = 1
                    fallthrough
        case 'd':
                    ftyp = DAY
                    for i=2 to 4 do
                        if not nxtch(fmt,fmtdx,ch,fcase,1) then exit end if
                        fmtdx += 1
                        fsize = i
                    end for
                    if fsize>=3 then
                        ftyp = DOW
                        if fcase=1 and fmt[fmtdx]='d' then
                            fcase = 2
                        end if
                    elsif fsize=1 and fcase=0 then
                        if nxtch(fmt,fmtdx,'o',0,1)
                        and nxtch(fmt,fmtdx+1,'y',0,1) then
                            ftyp = DOY
                            fmtdx += 2
                        end if
                    end if
                    break
        case 'h','H':
                    ftyp = HOUR
                    if nxtch(fmt,fmtdx,ch,fcase,1) then
                        fmtdx += 1
                        fsize = 2
                    end if
                    break
        case 'M':   fcase = 1
                    fallthrough
        case 'm':
                    for i=2 to 4 do
                        if not nxtch(fmt,fmtdx,ch,fcase,1) then exit end if
                        fmtdx += 1
                        fsize = i
                    end for
                    if not fcase 
                    and fsize=1
--                  and ch='s' then
                    and nxtch(fmt,fmtdx,'s',fcase,1) then
                        ftyp = MSEC
                        fmtdx += 1
                        fsize = 2
                    elsif not fcase 
                    and fsize<=2 then
                        ftyp = MINUTE
                    else
                        ftyp = MONTH
                        if fcase=1 and fmt[fmtdx]='m' then
                            fcase = 2
                        end if
                    end if
                    break
        case 's','S':
                    ftyp = SECOND
                    if nxtch(fmt,fmtdx,ch,0,1) then
                        fmtdx += 1
                    elsif nxtch(fmt,fmtdx,'T',1,1) then
                        fmtdx += 1
                        fcase = (ch='S')
                        ftyp = TH
                    end if
                    fsize = 2
                    break
        case 'y','Y':
                    ftyp = YEAR
                    if nxtch(fmt,fmtdx,ch,0,1) then
                        fmtdx += 1
                        fsize = 2
                        if nxtch(fmt,fmtdx,ch,0,1) then
                            if nxtch(fmt,fmtdx+1,ch,0,1) then
                                fmtdx += 2
                                fsize = 4
                            else
                                ecxtra = "yy or yyyy expected"
                                ecode = 7
                            end if
                        end if
--                  else
--                      ecxtra = "yy or yyyy expected"
                    end if
                    break
        case 't','T':
                    if nxtch(fmt,fmtdx,'Z',1,1) then
                        fmtdx += 1
                        ftyp = TZ
                        fsize = 4
                        if nxtch(fmt,fmtdx,'Z',1,1) then
                            fmtdx += 1
                            fcase = 1
                        end if
                    elsif nxtch(fmt,fmtdx,'H',1,1) then
                        fmtdx += 1
                        fcase = (ch='T')
                        ftyp = TH
                        fsize = 2
                    else
--DEV (spotted in passing) ain't this going to get clobbered, always?
                        ecxtra = "tz or th expected"
                    end if
                    break
        case '\'':
                    ftyp = TD_LITERAL
                    closequote = 0
                    for i=fmtdx+1 to length(fmt) do
                        if fmt[i]='\'' then
                            closequote = i
                            exit
                        end if
                    end for
                    if closequote=0 then
                        ecode = 17
                    end if
                    fsize = closequote-fmtdx
                    fmtdx += 1  
        default:
                    if (ch>='a' and ch<='z')
                    or (ch>='A' and ch<='Z')
                    or (ch>='0' and ch<='9') then
--DEV ditto
                        ecxtra = "ch is "&ch
                    else
                        ftyp = TD_LITERAL
                    end if
    end switch
    if ftyp=0 then
        ecode = 7
        ecxtra = "ch is "&ch
    end if
    return {ecode,fmtdx,ftyp,fcase,fsize,ch}
end function

function to_space(string s)
-- error messsage helper. shrink s to first space
    for i=2 to length(s) do
--      if find(s[i]," \t\r\n") then
        if find(s[i]," \r\n"&9) then
            s = s[1..i-1]
            exit
        end if
    end for
    return s
end function

function td_get_number(string s, integer idx, integer nsize)
-- an nsize of 1..3 means 1 or 2 or 3, above that must be exact
integer ch, n, asize = 1, sidx = idx+1
    ch = s[sidx]
    if ch<'0' 
    or ch>'9' then
        ecxtra = sprintf(`number expected "%s" at position %d, not "%s"`,
                         {s,sidx,to_space(s[sidx..$])})
        return {3,idx,0}    -- "number expected"
    end if
    n = ch-'0'
    while 1 do
        if sidx>=length(s) then exit end if
        ch = s[sidx+1]
        if ch<'0' then exit end if
        if ch>'9' then exit end if
        n = n*10+ch-'0'
        sidx += 1
        asize += 1
    end while
--  if nsize>1
--  and nsize!=asize then
    -- (comment above updated)
    if asize!=nsize
    and (asize>nsize or
         nsize>3) then
        ecxtra = sprintf(`wrong size (%d not %d) number in "%s" at position %d, ("%s")`,
                         {asize,nsize,s,idx+1,to_space(s[idx+1..$])})
        return {3,idx,0}    -- "number expected"
    end if
    return {0,sidx,n}
end function

function get_any(string s, integer idx, sequence stringset, integer ssize, integer scase, string desc)
-- ssize can be 2 for ampm, 3 for an abbreviation, and 4 means full length
    for i=1 to length(stringset) do
        string si = stringset[i]
        if ssize=3 then
            si = si[1..3]
        end if
        integer sidx = idx+1,
                ecode = 0
        for k=1 to length(si) do
            if sidx>length(s) then exit end if
            integer sch = s[sidx]
            sidx += 1
            if scase!=2 then
                if upper(sch)!=upper(si[k]) then ecode = -1 exit end if
            else
                if sch!=si[k] then ecode = -1 exit end if
            end if
        end for
        if ecode=0 then
            return {ecode,idx+length(si),i}
        end if
    end for
    ecxtra = sprintf(`%s expected in "%s" at position %d, not "%s"`,
                     {desc,s,idx+1,to_space(s[idx+1..$])})
    return {2,idx,0}    -- "unrecognised"
end function

function parse_one(string s, string fmt, integer partial)
-- see parse_date_string()
integer sdx = 0,    -- chars processed in s
        fmtdx = 0,  -- chars processed in fmt
        ftyp,       -- TD_LITERAL..AM
        pftyp = 0,  -- previous value of ftyp
        fcase,      -- 0=lower, 1=upper
        fsize,      -- 1 (1 or 2)
                    -- 2 (2)
                    -- 3 (abbreviation)
                    -- 4 (full length)
        ch,
        ecode = 0,  -- error code
        year = 0,
        month = 0,
        day = 0,
        hour = 0,
        minute = 0,
        second = 0,
        msecs = 0,  -- (returned in dayofweek)
        pm = 0,     -- 1=am, 2=pm
        dayofweek = 0,
        dayofyear = 0,
        tz = 0
    ecxtra = 0
    if length(fmt)=0 then
        ecode = 9
        ecxtra = ""
        return {ecode,edescs[ecode],ecxtra}
    end if
--  if length(default_format)=0 then
--      default_format = fmt
--  end if
    if length(s)=0 then
        ecode = 10
        ecxtra = ""
        return {ecode,edescs[ecode],ecxtra}
    end if
    while ecode=0
      and sdx<length(s)
      and fmtdx<length(fmt) do

        {ecode,fmtdx,ftyp,fcase,fsize,ch} = next_fmt(fmt,fmtdx)
        if ecode!=0 then exit end if

        switch ftyp do
            case TD_LITERAL:
                if fsize=1 then
                    if nxtch(s,sdx,ch,0,1)=0 then
                        ecode = 1
                        ecxtra = sprintf("'%c' expected in %s at position %d",{ch,s,sdx+1})
                        if sdx<length(s) then
                            ecxtra &= sprintf(", not '%c'",{s[sdx+1]})
                        end if
                    else
                        sdx += 1
                    end if
                else
                    for i=fmtdx to fmtdx+fsize-2 do
                        ch = fmt[i]
                        if nxtch(s,sdx,ch,0,1)=0 then
                            ecode = 1
                            ecxtra = sprintf("'%c' expected in %s at position %d",{ch,s,sdx+1})
                            if sdx<length(s) then
                                ecxtra &= sprintf(", not '%c'",{s[sdx+1]})
                            end if
                            exit
                        end if
                        sdx += 1
                    end for
                    if ecode=0 then
                        fmtdx += fsize-1
                    end if
                end if
            case YEAR:
--NO!
--              if pftyp=MINUTE then
--                  ecode = 21
--                  exit
--              end if
                switch fsize do
                    case 1,2:
--                      {ecode,sdx,year} = td_get_number(s,sdx,fsize)
                        {ecode,sdx,year} = td_get_number(s,sdx,2)
--                      if fsize=2 then
--                      if fsize=2 
--                      or (fsize=4 and year>=0 and year<=99) then
                            --                          
                            -- This split is quite arbitrary; assumes that
                            -- date of births for people up to 80 is a bit
                            -- more likely than futures >20 years away. By
                            -- now most things use 4-digit dates anyway.
                            --
--/*
                            if year<35 then
                                year += 2000    -- 00..44 ==> 2000..2034
                            else
                                year += 1900    -- 35..99 ==> 1935..1999
                            end if
--*/
                            integer current_year = date()[DT_YEAR],
                                    century = floor(current_year/100)*100

                            year += (century-100)
                            if year < (current_year-80) then
                                year += 100
                            end if
--                      end if
--                      year -= 1900            -- to match date()
                    case 4:
                        {ecode,sdx,year} = td_get_number(s,sdx,fsize)
                    default:
                        ?9/0    -- should never happen
                end switch
            case MONTH:
                if pftyp=HOUR then
                    ecode = 20
                    exit
                end if
                switch fsize do
                    case 1,2:
--                      {ecode,sdx,month} = td_get_number(s,sdx,fsize)
                        {ecode,sdx,month} = td_get_number(s,sdx,2)
                    case 3,4:
                        {ecode,sdx,month} = get_any(s,sdx,months[currlang],fsize,fcase,"month")
                    default:
                        ?9/0    -- should never happen
                end switch
                if ecode=0 then
                    if month<1 or month>12 then
                        ecode = 5
                        exit
                    end if
                end if
            case DAY:
                switch fsize do
                    case 1,2:
--                      {ecode,sdx,day} = td_get_number(s,sdx,fsize)
                        {ecode,sdx,day} = td_get_number(s,sdx,2)
                    default:
                        ?9/0    -- should never happen
                end switch
                if ecode=0 then
                    if day<1 or day>31 then -- (more tests below)
                        ecode = 6
                        exit
                    end if
                end if
            case DOW:
                switch fsize do
                    case 3,4:
                        {ecode,sdx,dayofweek} = get_any(s,sdx,day_names[currlang],fsize,fcase,"day")
                    default:
                        ?9/0    -- should never happen
                end switch
            case DOY:
                {ecode,sdx,dayofyear} = td_get_number(s,sdx,3)
            case HOUR:
--              {ecode,sdx,hour} = td_get_number(s,sdx,fsize)
                {ecode,sdx,hour} = td_get_number(s,sdx,2)
                if ecode=0 then
                    if hour<0 or hour>23 then
                        ecode = 11
                        exit
                    end if
                end if
            case MINUTE:
                if pftyp=DAY
                or pftyp=YEAR then
                    ecode = 21
                    ecxtra = ""
                    return {ecode,edescs[ecode],ecxtra}
                end if
--              {ecode,sdx,minute} = td_get_number(s,sdx,fsize)
                {ecode,sdx,minute} = td_get_number(s,sdx,2)
                if ecode=0 then
                    if minute<0 or minute>59 then
                        ecode = 13
                        exit
                    end if
                end if
            case SECOND:
                if pftyp=MONTH then
                    ecode = 20
                    exit
                end if
--              {ecode,sdx,second} = td_get_number(s,sdx,fsize)
                {ecode,sdx,second} = td_get_number(s,sdx,2)
                if ecode=0 then
                    if second<0 or second>59 then
                        ecode = 14
                        exit
                    end if
                end if
            case MSEC:
                {ecode,sdx,msecs} = td_get_number(s,sdx,3)
                if ecode=0 then
                    if msecs<0 or msecs>999 then
                        ecode = 22
                        exit
                    end if
                end if
                dayofweek = msecs
            case AM:
                if pftyp=MONTH then
                    ecode = 20
                    exit
                end if
                {ecode,sdx,pm} = get_any(s,sdx,ampm[currlang],2,1,"am/pm")
                -- note: "12:00:00pm" is noon, and "12:00:00am" is midnight
                --       on a 12 hour clock, 12:00am..12:59am and 12:00pm..12:59pm
                --       equate respectively to 0:00..00:59 and 12:00..12:59 on
                --       a 24 hour clock, and as stored. further, 0/13:00am/pm 
                --       have no meaning and should raise an error.
                if ecode!=0 then exit end if
                if hour<=0 or hour>12 then
                    ecode = 12
                    ecxtra = sprintf("(hour is %d)",{hour})
                    exit
                end if
                if pm=1 then    -- am
                    if hour=12 then
                        hour = 0
                    end if
                else -- pm=2    -- pm
                    if hour!=12 then
                        hour += 12
                    end if
                end if
                if hour<0 or hour>23 then
                    ?9/0        -- should never happen
                end if
            case TZ:
                if fcase then
                    {ecode,sdx,tz} = get_any(s,sdx,tzdescs,4,1,"timezone")
                else
                    {ecode,sdx,tz} = get_any(s,sdx,timezones,4,1,"timezone")
                end if
            case TH:
                if pftyp!=DAY then
                    ecode = 16
                    ecxtra = ""
                    exit
                end if
--              {ecode,sdx,{}} = get_any(s,sdx,td_ordinals[currlang],2,1,"ordinal suffix")
--p2js:
--              {ecode,sdx,{}} = get_any(s,sdx,td_ordinals[currlang],4,1,"ordinal suffix")
                {ecode,sdx} = get_any(s,sdx,td_ordinals[currlang],4,1,"ordinal suffix")
            default:
                ?9/0    -- should never happen...
        end switch
        if ftyp!=TD_LITERAL then
            pftyp = ftyp
        end if
    end while   
    if ecode=0
    and not partial
    and sdx!=length(s) then
        for i=sdx+1 to length(s) do
--          if not find(s[i]," \t\r\n") then
            if not find(s[i]," \r\n"&9) then
                ecxtra = "remaining text is:"&s[i..$]
                ecode = 18
                exit
            end if
        end for
    end if
    if ecode=0 
    and fmtdx!=length(fmt) then
        ecxtra = "remaining format is:"&fmt[fmtdx+1..$]
        ecode = 19
    end if
--added 20/11/19:
    if ecode=0 
    and year!=0 and month!=0 and day!=0
    and day>days_in_month(year,month) then
        ecode = 6
    end if
    if ecode!=0 then return {ecode,edescs[ecode],ecxtra} end if
--  if fmtdx!=length(fmt) then ?9/0 end if  --should never happen

    --set dayofweek, dayofyear -- no!
    --  this routine has to handle eg parse("3am","ham") and you
    --  ain't going to get a day of week/year out of that!

    -- return matches the result from date(), plus a timezone(/0)
    return {year,month,day,hour,minute,second,dayofweek,dayofyear,tz}
end function

--(local)
sequence default_parse_fmts
--sequence default_parse_fmts = {}
integer allow_partial = 0
--string default_format = ""
string default_format = "h:mpm Dddd Mmmm ddth, yyyy"

global procedure set_timedate_formats(sequence parse_fmts, object out_fmt=1, integer partial=0)
--
-- Sets the default formats for parsing strings to timedates, and formatting 
--  timedates into strings.
--
-- parse_fmts must be a sequence of format strings for parse_date_strings(), which
--  tries each in order.
--
-- out_fmt is a single string or index to parse_fmts for format_dateime().
--
-- the partial flag controls whether to ignore excess text when parsing, and if 
--  set then parse_fmts should be ordered with the most detailed first. (If not 
--  ignoring excess text, then the order of entries probably does not matter.)
--
-- Examples: set_timedate_formats({"Mmmm d yyyy h:mmam tz"})
--           set_timedate_formats({"DD/MM/YYYY h:mmam tz",
--                                 "DD/MM/YYYY h:mmam",
--                                 "DD/MM/YYYY"},1,1)                       
--
--  In the second case, with partial matching enabled, we would not want to try
--  the DD/MM/YYYY first, and risk ignoring a following time and timezone.
--
    default_parse_fmts = parse_fmts
    if string(out_fmt) then
        default_format = out_fmt
    elsif out_fmt!=0 then
        default_format = parse_fmts[out_fmt]
    end if
    allow_partial = partial
end procedure

global function parse_date_string(string s, sequence fmts=default_parse_fmts, integer partial=allow_partial)
--
-- parse a date/time string, eg
--
--  parse_date_string("March 7 2009 7:30pm EST",{"Mmmm d yyyy h:mmpm tz"})
--
-- returns {2009,3,7,19,30,0,0,0,21}, like date() plus one or two elements.
--
-- The extra elements are timezones, which are not meant to be examined 
--  directly, though you can use format_timedate(td,"tz") to decipher them.
--  DT_TZ and DT_DSTZ are deliberately /not/ global to deter direct access.
--
-- If an error occurs, instead the result has a length of 3, which consists 
--  of a numeric error code, a description of that code, and 0 or extended 
--  description of the actual problem encountered.
--  Said messages are intended for a developer/typecheck, not an end user.
--
-- Store the result in a timedate for fast-fail development.
--
-- See set_timedate_formats() for detals of the fmts and partial parameters.
--
-- Note that parsing say "Monday 1/01/2000" returns a DT_DOW element
--  set to 2, even though it was a Saturday, and any non-explicitly
--  mentioned elements get left as zero. It is in fact perfectly
--  legal to invoke parse_date_string("?","?"), which checks the input
--  for that exact literal and returns {0,0,0,0,0,0,0,0,0}, however it
--  is illegal to specify an empty string ("") as the format.
--
-- The dayofyear field (res[DT_DOY]) is usually left zero, but can easily
--  be obtained using day_of_year(y,m,d).
--
-- The partial parameter controls whether to ignore excess text; if set the most detailed 
--  format should be first. For example if the input is "03/09/15 6pm Customer was not happy", 
--  you want try "DD/MM/YY ham tz" and fail before "DD/MM/YY ham" and pass.
--
string fmti     -- (gives an earlier/better typecheck and thus simplifies debugging)
sequence res = {}
    for i=1 to length(fmts) do
        fmti = fmts[i]
        res = parse_one(s,fmti,partial)
        if length(res)!=3 then return res end if
--?res
    end for
    return res  -- fail the lot
end function


global function format_timedate(timedate td, string fmt=default_format)
--
-- td is normally a result originally from parse_date_string(), posibly adjusted.
--   You can also legally pass the result of date() to this routine, as long
--   as fmt does not contain a timezone.
--
-- fmt is eg "Dddd, Mmmm ddth, YYYY" for eg "Monday, September 3rd, 2015".
--
-- It is perfectly valid to invoke this routine to obtain various fragments
--  that you plan to check/stitch together later, with one small exception: 
--  * am (and pm/AM/PM) cannot be specified without a preceding hour.
--  
-- For example, performance considerations aside,
--
--      if format_timedate(td,"Dddd")="Monday" then
--
-- may be much clearer than then the equivalent
--
--      if td[DT_DOW]=1 then
--
-- Any required literals other than punctuation and whitespace should be
--  enclosed in single quotes (for a literal singlequote, pair two), eg:
--
--      format_timedate(td,"'Today''''s date is' Dddd, Mmmm ddth, YYYY")
--
-- You can also explicitly set s[DT_DOW] to zero if, after some kind of
--  other manipulation on s, you want it to be recalculated. Or just use
--  day_of_week(y,m,d) like this does.
--
string res = ""
integer fmtdx = 0,
        ftyp,       -- TD_LITERAL..AM
        fcase,      -- 0=lower/12-hour, 1=upper/24-hour, 2=capitalise
        fsize,      -- 1 (1 or 2)
                    -- 2 (2)
                    -- 3 (abbreviation)
                    -- 4 (full length)
        ch,
        ecode = 0,
        hidx = 0,
        hlen,
        hsize,
        hour,
        ispm,
        year,
        month,
        day,
        l,
        dayofweek,
        dayofyear,
        minute,
        second,
        msecs,
        tz,
        pftyp = -1
object x

    ecxtra = 0
    if length(fmt)=0 then
        ecode = 9
        ecxtra = ""
        return {ecode,edescs[ecode],ecxtra}
    end if
    if length(default_format)=0 then
        default_format = fmt
    end if
-- now covered by timedate:
--  if length(td)<DT_DOY then
--      ecxtra = sprintf("format_timedate(s): length(s) is %d",length(td))
--      ecode = 8
--  end if

    while ecode=0
      and fmtdx<length(fmt) do

        {ecode,fmtdx,ftyp,fcase,fsize,ch} = next_fmt(fmt,fmtdx)
        if ecode!=0 then exit end if

        switch ftyp do
            case TD_LITERAL:
                    if fsize=1 then
                        x = ch
                    else
                        x = fmt[fmtdx..fmtdx+fsize-2]
                        fmtdx += fsize-1
                    end if
            case YEAR:
--NO!
--              if pftyp=MINUTE then
--                  ecode = 21
--                  exit
--              end if
                year = td[DT_YEAR]
                switch fsize do
                    case 1,2:
                        x = sprintf("%02d",remainder(year,100))
                    case 4:
                        x = sprintf("%04d",year)
                    default:
                        ?9/0    -- shoule never happen
                end switch
            case MONTH:
                if pftyp=HOUR then
                    ecode = 20
                    exit
                end if
                month = td[DT_MONTH]
                if month<1 then
                    ecxtra = sprintf("month is %d",month)
                    ecode = 5
                    exit
                end if
                switch fsize do
                    case 1:
                        x = sprintf("%d",month)
                    case 2:
                        x = sprintf("%02d",month)
                    case 3,4:
                        x = months[currlang][month]
                        if fsize=3 then
                            x = x[1..3]
                        end if
                        if fcase=0 then
                            x = lower(x)
                        elsif fcase=1 then
                            x = upper(x)
--                      elsif fcase=2 then
--                          x should already be capitalised
                        end if
                    default:
                        ?9/0    -- should never happen
                end switch
            case DAY:
                day = td[DT_DAY]
--20/11/19:
--              if day<1 then
                year  = td[DT_YEAR]
                month = td[DT_MONTH]
--7/5/20:
if year>=1752 then
                if day<1
                or day>days_in_month(year,month) then
                    ecxtra = sprintf("day is %d",day)
                    ecode = 6
                    exit
                end if
end if
                switch fsize do
                    case 1:
                        x = sprintf("%d",day)
                    case 2:
                        x = sprintf("%02d",day)
                    default:
                        ?9/0    -- should never happen
                end switch
            case DOW:
--5/3/18 td[DT_DOW] may contain miliseconds...
--              dayofweek = td[DT_DOW]
--              if dayofweek=0 then
                    year  = td[DT_YEAR]
                    month = td[DT_MONTH]
                    if month=0 then ecode = 5 exit end if
                    day   = td[DT_DAY]
                    if day=0 then ecode = 6 exit end if
                    dayofweek = day_of_week(year,month,day)
--              end if
                switch fsize do
                    case 3,4:
                        x = day_names[currlang][dayofweek]
                        if fsize=3 then
                            x = x[1..3]
                        end if
                        if fcase=0 then
                            x = lower(x)
                        elsif fcase=1 then
                            x = upper(x)
--                      elsif fcase=2 then
--                          x should already be capitalised
                        end if
                    default:
                        ?9/0    -- should never happen
                end switch
            case DOY:
                dayofyear = td[DT_DOY]
                if dayofyear=0 then
                    year  = td[DT_YEAR]
                    month = td[DT_MONTH]
                    if month=0 then ecode = 5 exit end if
                    day   = td[DT_DAY]
                    if day=0 then ecode = 6 exit end if
                    dayofyear = day_of_year(year,month,day)
                end if
                x = sprintf("%d",dayofyear)
            case HOUR:
                hour = td[DT_HOUR]
                switch fsize do
                    case 1:
                        x = sprintf("%d",hour)
                    case 2:
                        x = sprintf("%02d",hour)
                    default:
                        ?9/0    -- should never happen
                end switch
                -- replaced if am/pm found later...
                hidx = length(res)+1
                hlen = length(x)
                hsize = fsize
            case MINUTE:
                if pftyp=DAY
                or pftyp=YEAR then
                    ecode = 21
                    exit
                end if
                minute = td[DT_MINUTE]
                switch fsize do
                    case 1,2:
                        x = sprintf("%02d",minute)
                    default:
                        ?9/0    -- should never happen
                end switch
            case SECOND:
                if pftyp=MONTH then
                    ecode = 20
                    exit
                end if
                second = td[DT_SECOND]
                switch fsize do
                    case 1,2:
                        x = sprintf("%02d",second)
                    default:
                        ?9/0    -- should never happen
                end switch
            case MSEC:
                msecs = td[DT_MSEC]
                x = sprintf("%03d",msecs)
            case AM:
                if pftyp=MONTH then
                    ecode = 20
                    exit
                end if
                -- note: "12:00:00pm" is noon, and "12:00:00am" is midnight
                --       (ideally use "noon" and "midnight", when practical)
                --       on a 24 hour clock, 0:00..00:59 and 12:00..12:59 equate 
                --       respectively to 12:00am..12:59am and 12:00pm..12:59pm
                --       on a 12 hour clock.
                if hidx=0 then
                    ecode = 15
                    exit
                end if
                hour = td[DT_HOUR]
                ispm = (hour>=12)
                if ispm then
                    if hour>12 then
                        hour -= 12
                    else
                        hidx = 0
                    end if
                elsif hour=0 then
                    hour = 12
                else
                    hidx = 0
                end if
                if hidx!=0 then
                    switch hsize do
                        case 1:
                            x = sprintf("%d",hour)
                        case 2:
                            x = sprintf("%02d",hour)
                        default:
                            ?9/0    -- should never happen
                    end switch
                    res[hidx..hidx+hlen-1] = x
                    hidx = 0
                end if
                -- (is am/pm language-dependent? do they all use latin?)
                x = ampm[currlang][ispm+1]
                if fcase then
                    x = upper(x)
                end if
            case TZ:
--              tz = td[DT_TZ]
                tz = iff(length(td)>=DT_TZ?td[DT_TZ]:0)
                if tz=0 then tz = 1 end if
                if fcase then
                    x = tzdescs[tz]
                else
                    x = timezones[tz]
                end if
            case TH:
                if pftyp!=DAY then
                    ecode = 16
                    exit
                end if
--DEV en (english) only?
                if day<5 or day>20 then
                    day = remainder(day,10)
                end if
                l = length(td_ordinals[currlang])
                if day=0 or day>l then
                    day = l
                end if
                x = td_ordinals[currlang][day]
                if fcase then
                    x = upper(x)
                end if
            default:
                ?9/0    -- should never happen
        end switch
        res &= x
        if ftyp!=TD_LITERAL then
            pftyp = ftyp
        end if
    end while   
    if ecode!=0 then return {ecode,edescs[ecode],ecxtra} end if
    if fmtdx!=length(fmt) then ?9/0 end if  -- should never happen
    if length(res)=0 then ?9/0 end if       -- should never happen
    return res
end function

-- maybe:
--global function new_timedate(integer year, integer month, integer day, integer hour=0, integer minute=0, integer seconds=0, integer dow=0, integer doy=0, object tz=0)
--timedate td
--  if dow=0 then
--      dow = day_of_week(year,month,day)
--  end if
--  if doy=0 then
--      doy = day_of_year(year,month,day)
--  end if
--  td = {year,month,day,hour,minute,seconds,dow,doy}
--  if string(tz) then
--      tz = find(tz,timezones)
--      if tz=0 then ?9/0 end if
--  end if
--  if tz!=0 then
--      td = append(td,tz)
--      >>
--  end if
--end function

--DEV: (put this in a test routine?)
--sequence s
--s = parse_date_string("1996-03-31",{"YYYY-MM-DD"})
--integer jd = timedate_to_julian_day(s)
--if jd!=2450174 then ?9/0 end if
--if julian_day_to_timedate(jd,0,0,0,0)!={1996,3,31,0,0,0,0} then ?9/0 end if
--if julian_day_to_timedate(0,0,0,0,0)!={-4713,11,24,0,0,0,0} then ?9/0 end if

--constant Gregorian_Reformation = 1752     (what is 1582?)

--function isLeap(integer year)
---- returns integer (0 or 1)
--integer ly = remainder(year,4)=0
--  if ly and year>Gregorian_Reformation then
--      ly -= (remainder(year,100)=0)
--      ly += (remainder(year,400)=0)
--      ly -= (remainder(year,3200)=0)
--      ly += (remainder(year,80000)=0)
--  end if
--  return ly
--end function

--?{1900,isLeap(1900)}
--?{2000,isLeap(2000)}
--?{2004,isLeap(2004)}
--?{2100,isLeap(2100)}
--if isLeap(1900)!=0 then ?9/0 end if
--if isLeap(2000)!=1 then ?9/0 end if
--if isLeap(2004)!=1 then ?9/0 end if
--if isLeap(2100)!=0 then ?9/0 end if

global function timedate_diff(timedate td1, timedate td2, integer term=0)
atom delta = timedate_to_seconds(td2)-timedate_to_seconds(td1)
    if term!=0 then
        atom tsec = {365.25*24*60*60,30*24*60*60,24*60*60,60*60,60,1}[term]
        delta = trunc(delta/tsec)*iff(term=DT_YEAR?365*24*60*60:tsec)
    end if
    return delta
end function
--/* test code:
include builtins/timedate.e

set_timedate_formats({"DD/MM/YYYY","Dddd, Mmmm dd yyyy"},2)

constant BIRTH_DATE = parse_date_string("28/6/1950")
constant DATE2 = date()
 
constant secs = timedate_diff(BIRTH_DATE, DATE2)
constant days = floor(secs/(24*60*60))
 
constant d3 = adjust_timedate(BIRTH_DATE, secs)
 
printf(1, "There are %d days between %s and %s. \n\n",  
          {days, 
            format_timedate(BIRTH_DATE),  
            format_timedate(DATE2) 
          }) 
           
printf(1, "%s plus %d days = %s. \n\n",  
          {format_timedate(BIRTH_DATE), days,  
           format_timedate(d3 ) 
          })           

-- output: 
 
-- There are 24268 days between  Wednesday, June 28 1950 and Tuesday, December 06 2016  
 
-- Wednesday, June 28 1950 plus 24268 days = Tuesday, December 06 2016  
{} = wait_key()
abort(0)

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
