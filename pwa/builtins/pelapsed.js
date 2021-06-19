"use strict";
// auto-generated by pwa/p2js.
//
// pelapsed.e
//
//  Phix implementation of elapsed() and elapsed_short() (autoinclude)
//
//  This is probably best kept simple: if you want more elaborate or flexible
//  results/formatting, leave this be, crib what you like, and start afresh.
//
function elaps(/*atom*/ v, /*string*/ d, r="") {
// private helper function.
// formats v and pluralises d by adding an "s", or not,
// finally if length>0 appends r with a ", " separator.
    let /*string*/ res = sprintf("%d %s%s",["sequence",v, d, (v===1) ? "" : "s"]);
    if (length(r)) {
        if (match(" and ",r)!==1) {
            res = $conCat(res, ", ");
        }
        res = $conCat(res, r);
    }
    return res;
}
function elapsdwy(/*atom*/ d, /*string*/ res) {
    let /*atom*/ y, w;
    y = floor(d/365);
    d = remainder(d,365);
    w = floor(d/7);
    d = remainder(d,7);
    if (d) { res = elaps(d,"day",res); }
    if (w) { res = elaps(w,"week",res); }
    if (y) { res = elaps(y,"year",res); }
    return res;
}
/*global*/ function elapsed(/*atom*/ s, min_s=0, /*string*/ fmt="") {
// convert s (in seconds) into an elapsed time string suitable for display.
// limits: a type check error occurs if s exceeds approx 100 billion years.
    let /*atom*/ m, h, d;
    let /*string*/ res = "";
    if ((min_s===0) || compare(s,min_s)>=0) {
        let /*string*/ minus = "";
        if (compare(s,0)<0) {
            minus = "minus ";
            s = 0-s;
        }
        m = floor(s/60);
        s = remainder(s,60);
        res = sprintf((integer(s)) ? "%ds" : "%3.1fs",s);
        if (m) {
            s = round(s);
//          res = iff(s=0?"":sprintf(" and %02ds",s))
            res = (s===0) ? "" : sprintf(" and %ds",s);
            h = floor(m/60);
            m = remainder(m,60);
            if (m) { res = elaps(m,"minute",res); }
            if (h) {
                d = floor(h/24);
                h = remainder(h,24);
                if (h) { res = elaps(h,"hour",res); }
                if (d) { res = elapsdwy(d,res); }
            }
        }
        res = $conCat(minus, res);
        if (length(fmt)) {
            res = sprintf(fmt,["sequence",res]);
        }
    }
    return res;
}
/*global*/ function elapsed_short(/*atom*/ s, min_s=0, /*string*/ fmt="") {
//
// as per elapsed(s), but the string returned is, erm, more compact and not quite as long.
//  returns a string in the format 
//      (([minus][N year[s], ][N week[s], ][N day[s], ]|[-])[N:[0]]N:[0]N|[-]Ns)
//  to explain that return value more explicitly:
//      if the result contains no ':', it is seconds (and has a trailing s),            eg      "3s"
//      if the result contains one ':', it is minutes:seconds (':' at [-3]),            eg    "2:30"
//      if the result contains two ':', it is hours:minutes:seconds (':' at [-6,-3]),   eg "2:30:00"
//      (if you want two and a half hours to appear as "2:30", chop 3 chars off the result!)
//      if required, the result is prefixed with "y year[s]" and "d day[s]", ie in longhand.
//      in the latter case negative values are prefixed with "minus ", otherwise with "-".
//
    let /*atom*/ m, h, d;
    let /*string*/ res = "";
    if ((min_s===0) || compare(s,min_s)>=0) {
        let /*string*/ minus = "";
        if (compare(s,0)<0) {
            minus = "-";
            s = 0-s;
        }
        m = floor(s/60);
        s = remainder(s,60);
        res = sprintf("%ds",["sequence",s]);
        if (m) {
            h = floor(m/60);
            m = remainder(m,60);
            res = sprintf("%d:%02d",["sequence",m, s]);
            if (h) {
                d = floor(h/24);
                h = remainder(h,24);
                if ((h===0) && (m===0) && (s===0)) {
                    res = "";
                } else {
                    res = sprintf("%d:%02d:%02d",["sequence",h, m, s]);
                }
                if (d) {
                    if (length(minus)) {
                        minus = "minus ";
                    }
                    res = elapsdwy(d,res);
                }
            }
        }
        res = $conCat(minus, res);
        if (length(fmt)) {
            res = sprintf(fmt,["sequence",res]);
        }
    }
    return res;
}
