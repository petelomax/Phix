--
-- pelapsed.e
--
--  Phix implementation of elapsed() and elapsed_short() (autoinclude)
--
--  This is probably best kept simple: if you want more elaborate or flexible
--  results/formatting, leave this be, crib what you like, and start afresh.
--

function elapzd(integer v, string d)
-- private helper function. formats v and pluralises d by adding an "s", or not.
    return sprintf("%d %s%s",{v,d,iff(v=1?"":"s")})
end function

global function elapsed(atom s)
-- convert s (in seconds) into an elapsed time string suitable for display.
-- limits: a type check error occurs if s exceeds approx 100 billion years.
atom m,h,d,y
string minus = "", secs, mins, hours, days
    if s<0 then
        minus = "minus "
        s = 0-s
    end if
    m = floor(s/60)
    s = remainder(s,60)
    secs = sprintf(iff(integer(s)?"%ds":"%3.1fs"),s)
    if m=0 then
        return sprintf("%s%s",{minus,secs})
    end if
    s = round(s)
    secs = iff(s=0?"":sprintf(" and %02ds",s))
    h = floor(m/60)
    m = remainder(m,60)
    if h=0 then
        return sprintf("%s%s%s",{minus,elapzd(m,"minute"),secs})
    end if
    mins = iff(m=0?"":", "&elapzd(m,"minute"))
    if h<24 then
        return sprintf("%s%s%s%s",{minus,elapzd(h,"hour"),mins,secs})
    end if
    d = floor(h/24)
    h = remainder(h,24)
    hours = iff(h=0?"":", "&elapzd(h,"hour"))
    if d<365 then
        return sprintf("%s%s%s%s%s",{minus,elapzd(d,"day"),hours,mins,secs})
    end if
    y = floor(d/365)
    d = remainder(d,365)
    days = iff(d=0?"":", "&elapzd(d,"day"))
    return sprintf("%s%s%s%s%s%s",{minus,elapzd(y,"year"),days,hours,mins,secs})
end function

global function elapsed_short(atom s)
--
-- as per elapsed(s), but the string returned is, erm, more compact and not quite as long.
--  returns a string in the format (([minus][N year[s], ][N day[s], ]|[-])[N:[0]]N:[0]N|[-]Ns)
--  to explain that return value more explicitly:
--      if the result contains no ':', it is seconds (and has a trailing s),            eg      "3s"
--      if the result contains one ':', it is minutes:seconds (':' at [-3]),            eg    "2:30"
--      if the result contains two ':', it is hours:minutes:seconds (':' at [-6,-3]),   eg "2:30:00"
--      (if you want two and a half hours to appear as "2:30", chop 3 chars off the result!)
--      if required, the result is prefixed with "y year[s]" and "d day[s]", ie in longhand.
--      in the latter case negative values are prefixed with "minus ", otherwise with "-".
--
atom m,h,d,y
string minus = ""
    if s<0 then
        minus = "-"
        s = 0-s
    end if
    if s<60 then
        return sprintf("%s%ds",{minus,s})
    end if
    m = floor(s/60)
    s = remainder(s,60)
    if m<60 then
        return sprintf("%s%d:%02d",{minus,m,s})
    end if
    h = floor(m/60)
    m = remainder(m,60)
    if h<24 then
        return sprintf("%s%d:%02d:%02d",{minus,h,m,s})
    end if
    d = floor(h/24)
    h = remainder(h,24)
    if length(minus) then
        minus = "minus "
    end if
    if d<365 then
        return sprintf("%s%s, %d:%02d:%02d",{minus,elapzd(d,"day"),h,m,s})
    end if
    y = floor(d/365)
    d = remainder(d,365)
    return sprintf("%s%s, %s, %d:%02d:%02d",{minus,elapzd(y,"year"),elapzd(d,"day"),h,m,s})
end function
