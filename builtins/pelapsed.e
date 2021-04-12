--
-- pelapsed.e
--
--  Phix implementation of elapsed() and elapsed_short() (autoinclude)
--
--  This is probably best kept simple: if you want more elaborate or flexible
--  results/formatting, leave this be, crib what you like, and start afresh.
--

function elaps(atom v, string d, r="")
-- private helper function.
-- formats v and pluralises d by adding an "s", or not,
-- finally if length>0 appends r with a ", " separator.
    string res = sprintf("%d %s%s",{v,d,iff(v=1?"":"s")})
    if length(r) then
        if match(" and ",r)!=1 then
            res &= ", "
        end if
        res &= r
    end if
    return res
end function

function elapsdwy(atom d, string res)
    atom y,w 
    y = floor(d/365)
    d = remainder(d,365)
    w = floor(d/7)
    d = remainder(d,7)
    if d then res = elaps(d,"day",res) end if
    if w then res = elaps(w,"week",res) end if
    if y then res = elaps(y,"year",res) end if
    return res
end function

global function elapsed(atom s, min_s=0, string fmt="")
-- convert s (in seconds) into an elapsed time string suitable for display.
-- limits: a type check error occurs if s exceeds approx 100 billion years.
atom m,h,d
string res = ""
    if min_s=0 or s>=min_s then
        string minus = ""
        if s<0 then
            minus = "minus "
            s = 0-s
        end if
        m = floor(s/60)
        s = remainder(s,60)
        res = sprintf(iff(integer(s)?"%ds":"%3.1fs"),s)
        if m then
            s = round(s)
--          res = iff(s=0?"":sprintf(" and %02ds",s))
            res = iff(s=0?"":sprintf(" and %ds",s))
            h = floor(m/60)
            m = remainder(m,60)
            if m then res = elaps(m,"minute",res) end if
            if h then
                d = floor(h/24)
                h = remainder(h,24)
                if h then res = elaps(h,"hour",res) end if
                if d then res = elapsdwy(d,res) end if  
            end if
        end if
        res = minus&res
        if length(fmt) then
            res = sprintf(fmt,{res})
        end if
    end if
    return res
end function

global function elapsed_short(atom s, min_s=0, string fmt="")
--
-- as per elapsed(s), but the string returned is, erm, more compact and not quite as long.
--  returns a string in the format 
--      (([minus][N year[s], ][N week[s], ][N day[s], ]|[-])[N:[0]]N:[0]N|[-]Ns)
--  to explain that return value more explicitly:
--      if the result contains no ':', it is seconds (and has a trailing s),            eg      "3s"
--      if the result contains one ':', it is minutes:seconds (':' at [-3]),            eg    "2:30"
--      if the result contains two ':', it is hours:minutes:seconds (':' at [-6,-3]),   eg "2:30:00"
--      (if you want two and a half hours to appear as "2:30", chop 3 chars off the result!)
--      if required, the result is prefixed with "y year[s]" and "d day[s]", ie in longhand.
--      in the latter case negative values are prefixed with "minus ", otherwise with "-".
--
atom m,h,d
string res = ""
    if min_s=0 or s>=min_s then
        string minus = ""
        if s<0 then
            minus = "-"
            s = 0-s
        end if
        m = floor(s/60)
        s = remainder(s,60)
        res = sprintf("%ds",{s})
        if m then
            h = floor(m/60)
            m = remainder(m,60)
            res = sprintf("%d:%02d",{m,s})
            if h then
                d = floor(h/24)
                h = remainder(h,24)
                if h=0 and m=0 and s=0 then
                    res = ""
                else
                    res = sprintf("%d:%02d:%02d",{h,m,s})
                end if
                if d then
                    if length(minus) then
                        minus = "minus "
                    end if
                    res = elapsdwy(d,res)
                end if
            end if
        end if
        res = minus & res
        if length(fmt) then
            res = sprintf(fmt,{res})
        end if
    end if
    return res
end function
