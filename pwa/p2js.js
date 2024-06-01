"use strict";
//
// p2js.js: core language
// ======================
//
//  Implements: standard constants, at least those that are useful,
//              utilities such as puts(), crash(), and [s]printf(),
//              types integer/atom/string/sequence/object (see docs),
//              all the usual builtins like length(), append(), etc,
//              but not "transpilables" such as tagset(), join_by().
//
//  Aside: js allows $ in identifiers; we use that fact to effectively
//         "hide" things from p2js.exw generated code. For instance,
//         statics like $docBody, $prefer_backtick, and $seed, as well 
//         as internal/private helpers such as $typeCheckError(), and 
//         general use routines such as $conCat().
//
// Style conventions
// =================
//  Use phix_style_underscores where appropriate (ie direct mapping)
//  Use javaScriptStyleCamelCase where that seems /more/ appropriate
//  Use a very light dusting of bHungarianNotation (eg 'b' for bool)
//  Use $ident for any globals that must never ever clash with phix.
//
//  Very much a judgment call, eg $prefer_backtick suggests there is
//  something relevant in phix.chm, whereas bAsV and zeroE are only
//  to be found in here. In practice, underscore_variables heavily
//  outnumber camelCase here, simply because the vast majority /are/
//  based on something in phix.
//
let $docBody = document.body;   // (override if required, eg like
                                //  the result panel on codepen.)

const NULL = 0,     // nb !=null, see docs
//    TRUE = 1,     // nb mapped to true, see docs
//    FALSE = 0,    // nb mapped to false, see docs
      WINDOWS = 2,
      LINUX = 3,
//    WEB = 4,
//    JS = WEB,
//    JAVASCRIPT = WEB,
      JS = 4,
      PI = Math.PI,
      EULER = Math.E,
      INVLN10 = 0.43429448190325182765,
//    INVLN10 = 1/Math.LN10,    // NO!!
      D_NAME = 1,
      D_ATTRIBUTES = 2,
      D_SIZE = 3,
      D_YEAR = 4,
      D_MONTH = 5,
      D_DAY = 6,
      D_HOUR = 7,
      D_MINUTE = 8,
      D_SECOND = 9,
      D_CREATION = 1,
      D_LASTACCESS = 2,
      D_MODIFICATION = 3,
      DT_YEAR = 1,
      DT_MONTH = 2,
      DT_DAY = 3,
      DT_HOUR = 4,
      DT_MINUTE = 5,
      DT_SECOND = 6,
      DT_DOW = 7,
      DT_MSEC = 7,
      DT_DOY = 8,
      DT_GMT = -1,
      E_CODE = 1,
      E_ADDR = 2,
      E_LINE = 3,
      E_RTN  = 4,
      E_NAME = 5,
      E_FILE = 6,
      E_PATH = 7,
      E_USER = 8,
      pp_File = 1,
      pp_Maxlen = 2,
      pp_Pause = 3,
      pp_StrFmt = 4,
      pp_IntFmt = 5,
      pp_FltFmt = 6,
      pp_Nest = 7,
      pp_Ascii =  8,
      pp_Date = 9,
      pp_Brkt = 10,
      pp_Indent = 11,
      pp_Q22 = 12,
      pp_IntCh = 13,
      ASCENDING = -1,
      NORMAL_ORDER = -1,
      DESCENDING = +1,
      REVERSE_ORDER = +1,
      MIN_HEAP = -1,
      MAX_HEAP = +1,
      HSIEH30 = -6,
      SLASH = 0x2f, // '/'
      TEST_QUIET = 0,
      TEST_SUMMARY = 1,
      TEST_SHOW_FAILED = 2,
      TEST_SHOW_ALL = 3,
      TEST_ABORT = 1,
      TEST_CRASH = -1,
      TEST_PAUSE = 1,
      TEST_PAUSE_FAIL = -1,
      VC_COLOR      = 1,    // was colour/monochrome flag, now always 1 
      VC_MODE       = 2,    // was current video mode, now always 3 
      VC_LINES      = 3,    // number of text rows in console buffer 
      VC_COLUMNS    = 4,    // number of text columns in console buffer 
      VC_XPIXELS    = 5,    // was screen width in pixels, now always 0 
      VC_YPIXELS    = 6,    // was screen height in pixels, now always 0 
      VC_NCOLORS    = 7,    // was number of colors, now always 32 
      VC_PAGES      = 8,    // was number of display pages, now always 1 
      VC_SCRNLINES  = 9,    // number of text rows for current screen size 
      VC_SCRNCOLS   = 10,   // number of text columns for current screen size 
      ANY_QUEUE = 0,
      FIFO_QUEUE = 1,
      LIFO_QUEUE = 2,
      BLACK          = 0,
      BLUE           = 1,  // [or  4]
      GREEN          = 2,
      CYAN           = 3,  // [or  6]
      RED            = 4,  // [or  1]
      MAGENTA        = 5,
      BROWN          = 6,  // [or  3]
      WHITE          = 7,
      GRAY           = 8,
      BRIGHT_BLUE    = 9,  // [or 12]
      BRIGHT_GREEN   = 10,
      BRIGHT_CYAN    = 11, // [or 14]
      BRIGHT_RED     = 12, // [or  9]
      BRIGHT_MAGENTA = 13,
      YELLOW         = 14, // [or 11]
      BRIGHT_WHITE   = 15,
      $console_colours = [`black`,              // BLACK          (0)
                          `mediumblue`,         // BLUE           (1) [or 4]
                          `green`,              // GREEN          (2)
                          `darkcyan`,           // CYAN           (3) [or 6]
                          `darkred`,            // RED            (4) [or 1]
                          `darkmagenta`,        // MAGENTA        (5)
                          `olive`,              // BROWN          (6) [or 3]
                          `lightgray`,          // WHITE          (7)
                          `gray`,               // GRAY           (8)
                          `blue`,               // BRIGHT_BLUE    (9) [or 12]
                          `lime`,               // BRIGHT_GREEN   (10)
                          `cyan`,               // BRIGHT_CYAN    (11) [or 14]
                          `red`,                // BRIGHT_RED     (12) [or 9]
                          `magenta`,            // BRIGHT_MAGENTA (13)
                          `yellow`,             // YELLOW         (14) [or 11]
                          `white`];             // BRIGHT_WHITE   (15)

let $tx_clr = -1,
    $bg_clr = -1;

function puts(fn, text, cleanup=true) {
    integer(fn,"fn");
//  string(text,"text");
    if ((fn !== 1) && (fn !== 2)) { crash("fn must be 1 or 2"); }
    if (typeof(text) === "number") { text = String.fromCodePoint(text); }
    if (cleanup) {
        const am = new RegExp("&","g"),
              lt = new RegExp("[<]","g"),
              gt = new RegExp("[>]","g"),
              sp = new RegExp("[ ]","g"),
              lf = new RegExp("\\n","g");
        text = text.replace(am,"&amp;")
                   .replace(lt,"&lt;")
                   .replace(gt,"&gt;")
                   .replace(sp,"&ensp;")
                   .replace(lf,"<br>");
    }
//  let where = (fn === 2) ? "afterbegin" : "beforeend";
    let where = "beforeend";
    if ($tx_clr !== -1 || $bg_clr !== -1) {
        text = `<span style="` + ($tx_clr !== -1?`color:`+$tx_clr+`;`:"")
                               + ($bg_clr !== -1?`background-color:`+$bg_clr+`;`:"")
                               + `">` + text + `</span>`;
    }
    $docBody.insertAdjacentHTML(where, text);
}

function crash(msg, args = []) {
//  string(msg,"msg");
//  object(args,"args");
    if (!Array.isArray(args) || (args.length !== 0)) {
        // ^ ie treat sprintf(fmt,5) as sprintf(fmt,{5})
        msg = sprintf(msg,args);
    }
//  msg += "\n";
    puts(2,msg + "\n");
    throw(new Error(msg));
//  debugger;
//  puts(2,"this should not occur");
}

function assert(condition, msg = "", args = []) {
    if (!condition) { crash(msg,args); }
}

function abort(i) {
    crash("abort(%d)",["sequence",i])
}

function $typeCheckError(name, x) {
    if (name.length !== 0) {
        if (x === 'undefined') {
            crash('variable %s has not been assigned a value.',["sequence",name]);
        } else {
            crash('typecheck error, %s is %v',["sequence",name,x]);
        }
    }
    return false;
}

function integer(i, name = "") {
//
// invoke as (eg) integer(fn) to test, integer(fn,"fn") to typecheck [from js only].
//
    let ti = typeof(i);
//  ti is one of "string","number","undefined","boolean","object", or "function".
//  Please note:
//      The data type of NaN is number
//      The data type of an array is object
//      The data type of a date is object
//      The data type of null is object
//      The data type of an undefined (/unassigned) variable is undefined
//      You cannot use typeof to determine if a JavaScript object is an array (or a date).
//30/10/22: (the javascript (bitwise) & operator is limited to 32 bits...)
//  if ((ti !== "boolean") && (ti !== "function") && !Number.isSafeInteger(i)) {
//5/11/22:
//  if ((ti !== "boolean") && (ti !== "function") && (ti != "undefined") && (i!="") && (i!=~~i)) {
//7/11/23:
//  if ((ti !== "object") && !(i instanceof HTMLElement)) {

    if ((ti === "undefined") || 
//29/5/24:
//      (ti === "object") || 
        ((ti === "object") && !(i instanceof HTMLElement)) || 
        (ti === "string") || 
        ((ti !== "boolean") && (ti !== "function") && (i!=~~i))) {
// ^^^ put back 7/11/23...
//      (ti === "function") ||
//      ((ti !== "boolean") && (i!=~~i))) {
        return $typeCheckError(name,i);
    }
//  }
    return true;
}
let int = integer;
let bool = integer;

function atom(a, name = "") {
//7/8/21...
//  if ((typeof(a) !== "boolean") && (typeof(a) !== "function") && (typeof(a) !== "number" || !isFinite(a))) {
    let ta = typeof(a);
//23/10/21 (IupMultiBox, atom(ih)...)
//  if ((ta !== "boolean") && (ta !== "function") && (ta !== "number")) {
//  if ((ta !== "boolean") && (ta !== "function") && (ta !== "number") && ta !== "object") {
    if ((ta === "string") || (Array.isArray(a) && a[0] === "sequence")) {
        return $typeCheckError(name,a);
    }
    return true;
}
let number = atom;

function string(s, name = "") {
    if (typeof(s) !== "string") {
        return $typeCheckError(name,s);
    }
    return true;
}

function sequence(p, name = "") {
    if (typeof(p) !== "string") {
        if (!Array.isArray(p) || p[0] !== "sequence") {
            return $typeCheckError(name,p);
        }
    }
    return true;
}
let seq = sequence;

function object(o, name = "") {
    if (typeof(o) === "undefined") {
        return $typeCheckError(name,"undefined");
    }
    return true;
}

function length(o) {
    if (string(o)) {
        return o.length;
    } else if (sequence(o)) {
        return o.length-1;
    }
    crash("length of an atom is not defined");
}

//function deep_copy(p, depth=-1) {
function deep_copy(p) {
    // (we got no refcount here, so must ignore bIfNeeded)
    if (Array.isArray(p)) {
        p = p.slice();
//      if (depth) {
//          depth -= 1;
            let pl = p.length;
            for (let i = 1; i < pl; i += 1) {
//              p[i] = deep_copy(p[i],depth);
                p[i] = deep_copy(p[i]);
            }
//      }
    }
    return p;
}

function $charArray(s) {
// Needed because Array.from(string) produces lots of diddy-strings...
//  whereas this produces the array of codePointAt I was expecting.
//SUG: (instead of next, which tmr has never triggered, not even once)
//  if (!string(s)) { return s; }
    string(s,"s");
    let l = s.length, i = 1;
    let res = ["sequence"];
// As per MDN:
//  for (let i = 0; i < l; i += 1) {
    for (const codePoint of s) {
//      let ch = s.codePointAt(i);
        let ch = codePoint.codePointAt(0);
//      res[i+1] = ch;
        res[i++] = ch;
    }
    return res;
}

let $prefer_backtick = false;

function sprintf(fmt, args = []) {
//function sprintf(fmt, args = ["sequence"]) {
    //
    // Should give reasonable approximation to the Phix sprintf() builtin,
    //        but is rather unlikely to ever be 100% absolutely perfect...
    //
    // Known omissions and differences:
    //      %v outputs [] rather than {}. (A simple substitution would, of
    //          course, also swap them within nested strings, etc...)
    //      "inf" and "nan" are output as "Infinity" and "NaN" (so what?!)
    //      rounding errors, eg: "%5.2f",0.005 ==> Phix: " 0.00", this: " 0.01"
    //                           "%5.2f",0.015 ==> Phix: " 0.02", this: " 0.01"
    //         (see builtins\VM\pFPU.e which is not something I can or
    //          would ever want to attempt in a cross-browser fashion.)
    //      "%g" has strange rules for picking "%e" over "%f" in Phix (===Eu),
    //          whereas this just uses a much simpler "is shorter or f=0" test.
    //      This may very well accept nonsense specifiers, producing gibberish.
    //         (I have no interest in testing anything VM/pprntfN.e rejects.)
    //      See also pwa\printf_tests.html
    //
    string(fmt,"fmt");
    let i = sequence(args) ? 0 : -1; // (treat ["sequence",1,2,3] and [1,2,3] the same)

    // First, some private helper routines to get i'th arg as string/int/float:
    function argi() {
        if (Array.isArray(args)) {
            if (i >= args.length) {
                crash('Insufficient arguments for sprintf');
            }
            return args[i];
        }
        return args;
    }
    function stri(bAsV = false) {
        let res = argi();
        if (typeof(res) !== "string") {
            if (bAsV) {
//              res = JSON.stringify(res);              // (ie/for "%v")
                res = sprint(res,bAsV);                 // (ie/for "%v")
            } else {
//              if (Array.isArray(res)) {
                if (sequence(res)) {
                    // compatibility shim: Phix/sprintf() treats eg "ABC",
                    // {'A','B','C'}, and {65[.1],66,67} exactly the same.
                    let allChar = true,
                        sres = "",
                        l = length(res);
//                  for (let i = 0; i < res.length; i += 1) {
                    for (let i = 1; i <= l; i += 1) {
                        let ch = res[i],
                            tch = typeof(ch);
                        if (tch === "string" && ch.length === 1) {
                            sres += ch;
                        } else {
                            if (!Number.isInteger(ch) && tch === "number") {
                                ch = Math.floor(ch);
                            }
//12/1/2022
//                          if (!Number.isInteger(ch) || ch<0 || ch>255) {
                            if (!Number.isInteger(ch) || ch<0 || ch>0x10FFFF) {
                                allChar = false;
                                break;
                            }
                            sres += String.fromCodePoint(ch);
                        }   
                    }
                    if (allChar) { return sres; }
                }
//              crash('Invalid (string expected)');
                res = String.fromCodePoint(res);
            }
        }
        return res;
    }
    function inti(unsigned = false) {
        let res = argi(),
            tr = typeof(res);
        if (tr == "boolean") {
            res = res ? 1 : 0;
        } else if (res instanceof HTMLElement) {
            res = res.ID;
        } else {
            if (!Number.isInteger(res)) {
                if (tr !== "number") {
                    crash('Invalid (integer expected)');
                }
                res = Math.floor(res);
//              res = Math.round(res - res % 1); // (maybe..)
            }
            if (unsigned) {
                res = res >>> 0; // (casts -ve to +ve)
//              res = Math.abs(res);
            }
        }
        return res;
    }
    function remove_scientific(num) {
        // JavaScript prints numbers > 1e21 using scientific notation:
        // since we have explicitly asked for %d, undo things.
//      let numStr = String(num);
        let numStr = num.toString(10);
        if (Math.abs(num) < 1.0) {
//          let e = parseInt(num.toString().split('e-')[1]);
            let e = parseInt(numStr.split('e-')[1]);
            if (e) {
                let negative = (num < 0);
                if (negative) num *= -1
                num *= Math.pow(10, e - 1);
                numStr = '0.' + (new Array(e)).join('0') + num.toString().substring(2);
                if (negative) numStr = "-" + numStr;
            }
        } else {
//          let e = parseInt(num.toString().split('+')[1]);
            let e = parseInt(numStr.split('+')[1]);
            if (e > 20) {
                e -= 20;
                num /= Math.pow(10, e);
                numStr = num.toString(10) + (new Array(e + 1)).join('0');
            }
        }
        return numStr;
    }
// possible alternative + 94 test cases, see 
// https://stackoverflow.com/questions/1685680/how-to-avoid-scientific-notation-for-large-numbers-in-javascript
//  function eToNumber(num) {
//    let sgn = "";
//    (num += "").charAt(0) == "-" && (num = num.substring(1), sgn = "-");
//    let arr = num.split(/[e]/ig);
//    if (arr.length < 2) return sgn + num;
//    let n = arr[0], exp = +arr[1];
//    let w = (n = n.replace(/^0+/, '')).replace('.', ''),
//      pos = n.split('.')[1] ? n.indexOf('.') + exp : w.length + exp,
//      L = pos - w.length, s = "" + BigInt(w);
//    function r() {return w.replace(new RegExp(`^(.{${pos}})(.)`), `$1.$2`)}
//    w = exp >= 0 ? (L >= 0 ? s + "0".repeat(L) : r()) : (pos <= 0 ? "0." + "0".repeat(Math.abs(pos)) + s : r());
//    if (!+w) w = 0;
//    return sgn + w;
//  }
    function flti(precision) {
        let res = argi(),
            tr = typeof(res);
        if (tr === "boolean" ) {
            res = res ? 1 : 0;
        } else if (tr !== "number") {
            crash('Invalid (number expected)');
        }
//function RoundNum(num, length) { 
//  let number = Math.round(num * Math.pow(10, length)) / Math.pow(10, length);
//  return number;
//}
//function round(num, precision) {
//  let base = 10 ** precision;
//  return (Math.round(num * base) / base).toFixed(precision);
//}
/*
        if (Math.abs(res)<1e300) {
//          res = Math.round(res * Math.pow(10, precision)) / Math.pow(10, precision);
            let base = 10 ** precision;
            res = (Math.round(res * base) / base);//.toFixed(precision);
        }
*/
        return res;
    }

    function eorf(/*integer*/ precision) {  // (ie "%g")

        let fp = flti(precision),
            eres = fp.toExponential(precision);
        const zeroE = new RegExp("([.][0]+e|[0]+e)");
        const Ezero = new RegExp("(e\\+0$)");
        eres = eres.replace(zeroE,"e");             // eg "1.000000e14" -> "1e14"
        eres = eres.replace(Ezero,"");              // eg        "1e+0" -> "1"

        let fres = fp.toFixed(precision),
            fdot = fres.indexOf('.');
        if (fdot != -1) {
            if (fdot<precision) {
                fres = fp.toFixed(precision-fdot);
            }
            const Fzero = new RegExp("([.]?[0]+$)");    // eg "10.2300" -> "10.23"
            fres = fres.replace(Fzero,"");              // or "1023.00" -> "1023"
        }

        // note this will often disagree with the Phix/Eu output/choice
        //  (they pick %e for "(exponent>=precision or exponent<-4)")
        let res = (eres.length<fres.length) || (parseFloat(fres) === 0) ? eres : fres;
        return res;
    }

    function allascii(x, enquote = 'q') {
        let backtick = (enquote === 'q');
        let bsi = [];
        for (let i = x.length-1; i >= 0; i -= 1) {
            let c = x[i];
            if (c === '\\' || c === '\"' || c === '\'') {
                if (backtick) {
//                  bsi &= i
                    bsi.push(i);
                } else {
//                  x[i..i] = '\\'&c
                    x = x.slice(0,i) + '\\' + x.slice(i);
                }
            } else if (c<' ' || c>'~') {
//              if (c === '\t') { c='t'; }
                if (c === 0X9) { c='t'; }
                else if (c === '\n') { c='n'; }
                else if (c === '\r') { c='r'; }
                else if (c === '\0') { c='0'; }
                else { return 0; }
                if (backtick) {
                    for (let j = 0; j < bsi.length; j += 1) {
                        let k = bsi[j];
//                      x[k..k] = '\\'&x[k]
                        x = x.slice(0,k) + '\\' + x.slice(k);
                    }
                    backtick = false;
                }
//              x[i..i] = '\\'&c
                x = x.slice(0,i) + '\\' + x.slice(i);
            }
        }
        if (backtick && ($prefer_backtick || bsi.length !== 0)) {
            x = '`' + x + '`';
        } else {
            x = '"' + x + '"';
        }
        return x;
    }

    function inbase(bn) {
//      if (!sequence(bn) || length(bn)!=2 ...
        let [,b,n] = bn;
        if (b > 36) {
            const charset = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'.split('');
            if (n === 0) { return "0"; }
            let s = [];
            while (n > 0) {
              s = [charset[n % b], ...s];
              n = Math.floor(n / b);
            }
            return s.join('');
        }
        return n.toString(b);
    }

    function blankTZ(r1) {
//      if (find(0X2E,r1)) {
//      let i = r1.indexOf(0X2E);
        let i = r1.indexOf('.');
        if (i != -1) {
            for (let r1dx=length(r1); r1dx>=1; r1dx-=1) {
                let /*integer*/ r1ch = $subse(r1,r1dx);
                if ((r1ch===0X30) || (r1ch===0X2E)) {
                    r1 = $repe(r1,r1dx,0X20);
                }
                if (r1ch!==0X30) { break; }
            }
        }
        return r1;
    }

    function toRoman(/*integer*/ n) {
        assert(n>=1 && n<=3999);
        let /*string*/ res = "";
        let /*integer*/ idx = 1,  // (..7, to "MDCLXVI")
                        rn = 1000,  // 500,100,50,10,5,1
                        tenth = 100; // 100, 10,10, 1,1,0
        while (n>0) {
            while (n>=rn) {
                res = $conCat(res, $subse("MDCLXVI",idx), false);
                n -= rn;
            }
            if (compare(n+tenth,rn)>=0) {
                res = $conCat(res, $subse("CXI",floor((idx+1)/2)), false);
                n += tenth; // above loop once more
            } else if (n) {
                idx += 1;
                rn /= ((odd(idx)) ? 5 : 2);
                if (odd(idx)) { tenth = floor(rn/10); }
            }
        }
        return res;
    }

//  function callback(expr, sgn, size, dot, precision, specifier) {
    function callback(expr, subscript, sgn, size, dot, precision, specifier) {
        //
        // Replaces a single formatting specification:
        // expr is eg "%4.2f" or "%-20s" (the whole thing)
        // subscript is eg "[2]" for positional args, or undefined
        // sgn is eg '-' or one of "+,=|", or undefined
        // size is eg "4" (leading 0 === zero-fill), or undefined
        // dot is eg ".2" or "." or undefined
        // precision is eg "2" or undefined
        // specifier is eg 's' or one of "qQtcvVdboxXeEfgG%"
        //
        if (expr === '%%') { return '%'; }  // (aka specifier === '%')
        i += 1;
        if (subscript) {
            let l = subscript.length-1, j = 1, ssign = +1;
            i = 0;
            // handle negative subscripts, eg [-1] (0X2D=='-')
            if (subscript.codePointAt(1) === 0X2D) { ssign = -1; j = 2; } 
            while (j < l) {
                let d = subscript.codePointAt(j) - 0X30; // (ie ch-'0')
// (untested, just a tad too fiddly with the j=2 methinks...)
// (alternatively, make to_integer() "native" and exclude from transpilation...)
//              if ((d > 9) || d < (j === 0)) { // (not leading 0)
                if ((d > 9) || d < 0) {
                    crash("invalid format specifier subscript");
                }
                i = i*10 + d;
                j += 1;
            }
//          i *= ssign;
            if (ssign === -1) { i = args.length - i; }
        }
        precision = precision ? parseInt(precision,10) : (dot ? 0 : 6);
//      if (precision>15) { precision = 15; }
        if (precision>16) { precision = 16; }
        let res = "";
        switch (specifier) {
            case 's': res = stri(); break;
            case 'q': res = allascii(stri(),specifier); break;
            case 'Q': res = allascii(stri(),specifier); break;
            case 't': res = argi()?"true":"false"; break;
            case 'n': res = argi()?"\n":""; break;
            case 'c': res = String.fromCodePoint(argi()); break;
//14/02/22 %v and %V flipped
            case 'v': res = stri(-1); break;
            case 'V': res = stri(true); break;
            case 'd': res = remove_scientific(inti()); break;
            case 'b': res = inti().toString(2); break;
            case 'o': res = inti().toString(8); break;
            case 'x': res = inti(true).toString(16).toUpperCase(); break;
            case 'X': res = inti(true).toString(16); break; //^yep===Phix
//          case 'a': res = inbase(argi()); break;
            case 'a': res = inbase(argi()).toLowerCase(); break;
//          case 'A': res = inbase(argi()).toUpperCase(); break;
            case 'A': res = inbase(argi()); break;
            case 'e': res = flti(precision).toExponential(precision); break;
            case 'E': res = flti(precision).toExponential(precision).toUpperCase(); break;
            case 'f': res = flti(precision).toFixed(precision); break;
            case 'F': res = blankTZ(flti(precision).toFixed(precision)); break;
            case 'g': res = eorf(precision); break;
            case 'G': res = eorf(precision).toUpperCase(); break;
            case 'R': res = toRoman(inti()); break;
            case 'r': res = toRoman(inti()).toLowerCase(); break;
            default: crash("unrecognised specifier");
        }
        if (sgn === ',') {
            if ("aAdf".indexOf(specifier) === -1) {
                crash('comma-fill only permitted on %a, %A, %d, and %f');
            }
            let showcommas = res.indexOf('.');
            if (showcommas === -1) { showcommas = res.length; }
            let lim = "-+".indexOf(res[0]) === -1 ? 3 : 4;
            while (showcommas>lim) {
                showcommas -= 3;
                res = res.slice(0,showcommas) + ',' + res.slice(showcommas);
            }
        } else if (sgn === '+' && res[0] !== '-') {
            res = '+' + res;
        } else if (sgn === '_' && res[0] !== '-') {
            res = ' ' + res;
        }
        if (size) {
            let pad = (size[0] === '0' && "-+_=|".indexOf(sgn) === -1 && res.indexOf('-') === -1) ? '0' : ' ';
            let padlen = parseInt(size,10)-res.length;
            if (padlen>0) {
                let half = Math.floor(padlen/2);
                switch (sgn) {
                    case '=': res = pad.repeat(half) + res + pad.repeat(padlen-half); break;
                    case '|': res = pad.repeat(padlen-half) + res + pad.repeat(half); break;
                    case '-': res = res + pad.repeat(padlen); break;
                    default : res = pad.repeat(padlen) + res; break;
                }
            }       
        }       
        return res;
    }
    //
    // Replace each %[subscript][sgn][[0]size][.[precision]]specifier (eg "%,7.2f") in turn
    //  (where sgn is one of "-+_=|," or undefined)
    //
    const regex = new RegExp(`%(\[-?[0-9]+\])?([-+_=|,])?(0?[0-9]+)?([.]([0-9]+)?)?([sqQtncvVdboxXaAeEfFgGRr%])`,'g');
    if (string(args)) {
        if (fmt.match(regex).length>1) { args = $charArray(args); }
    }
    return fmt.replace(regex, callback);
}

function printf(fn, fmt, args = []) {
//  integer(fn,"fn");
//  string(fmt,"fmt");
//  object(args,"args");
    if (fn === 0 && fmt === "") {
        for (let i = 1; i < args.length; i += 2) {
            if (args[i] === 'prefer_backtick') {
                $prefer_backtick = args[i+1];
            } else if (args[i] !== 'unicode_align') {
                // ( simply ignore 'unicode_align' )
                crash("unsupported printf option " + args[i]);
            }
        }
    } else {
        puts(fn,sprintf(fmt, args));
    }
}

function sprint(o,asCh) {
//  object(o,"o");
    let res;
    if (string(o)) {
//      let all_ascii = true;
        let l = o.length;
        res = "\"";
//As per MDN:
//      for (let i = 0; i < l; i += 1) {
        for (const codePoint of o) {
//          let ch = o.codePointAt(i);
            let ch = codePoint.codePointAt(0);
            if (ch === 0) {
                res += "\\0";
            } else if (ch === 0X5C) {
                res += "\\\\";
            } else if (ch === 0X22) {
                res += "\\\"";
            } else if (ch >= 32) {
//              res += o[i];
                res += codePoint;
            } else if (ch === 9) {
                res += "\\t";
            } else if (ch === 10) {
                res += "\\n";
            } else if (ch === 13) {
                res += "\\r";
            } else {
//              all_ascii = false;
                o = $charArray(o);
                return sprint(o,asCh);
            }
        }   
        res += "\"";
    } else if (sequence(o)) {
        let l = o.length-1;
        res = "{";
        if (asCh === false) { asCh = true; } // (nb -1 and true left as-is)
        for (let i = 1; i <= l; i += 1) {
            if (i>1) { res += ","; }
            res += sprint(o[i],asCh);
        }
        res += "}";
    } else if ((asCh === true) && integer(o) && (o>=32) && (o<127)) {
        res = "'" + String.fromCodePoint(o) + "'";
    } else if (integer(o)) {
        res = o.toString();
    } else if (o === null) {
        res = "null";
    } else if (o instanceof Element) {
        res = "<HtmlElement>";
    } else {
        function cut_tz(v) {
            // remove trailing zeroes
            if (v.indexOf('.') !== -1) {
                let cutFrom = v.length - 1;
                while (v[cutFrom] === '0') {
                    cutFrom -= 1;
                }
                if (v[cutFrom] === '.') {
                    cutFrom -= 1;
                }
                v = v.substr(0, cutFrom + 1);
            }
            return v;
        }
//      res = cut_tz(o.toFixed(10));
        res = cut_tz(o.toPrecision(10));
    }
    return res;
}

function print(fn, o) {
//  integer(fn,"fn");
//  object(o,"o");
    puts(fn,sprint(o) + "\n");
}

//DEV if msg[$]='\n' then just display it
function progress(/*string*/ msg, /*object*/ args={}) {
    // (it would never work: the browser will not update while JavaScript is running)
    //  (anything using progress() should be rewritten to gui/timer/idle processing)
//  crash("progress does not work under p2js");
// 15/4/22 changed to "do nothing".
//  printf(1,msg,args); [temp, sometimes useful]
//25/11/22:
    if (equal($subse(msg,-1),0XA)) {    // if msg[$]='\n' then
        printf(1,msg,args);
    }
}

function append(a, x) {
//  object(a,"a");
//  object(x,"x");
    if (string(a)) {
        if (integer(x) && x>=0 && x<=255) {
            a += String.fromCodePoint(x);
        } else {
            a = $charArray(a);
            a.push(x);
        }
    } else {
        if (atom(a)) {
            a = ["sequence",a,x];
        } else {
            a.push(x);
        }
    }
    return a;
}

function prepend(a, x) {
//  object(a,"a");
//  object(x,"x");
    if (string(a)) {
        if (integer(x) && x>=0 && x<=255) {
            a = String.fromCodePoint(x) + a;
        } else {
            a = $charArray(a);
            a[0] = x;
            a.unshift("sequence");
        }
    } else {
        if (atom(a)) {
            a = ["sequence",x,a];
        } else {
            a[0] = x;
            a.unshift("sequence");
        }
    }
    return a;
}

function wait_key() {
    // specifically for "{} = wait_key()", do nothing
    // (as per docs, you simply cannot "stop JavaScript"
    //  while waiting for keyboard input, no way Jose.)
    return [];
}

function compare(a, b) {
//  if (string(a) && string(b)) { return a.localeCompare(b); }
    if (string(a) && string(b)) { return a < b ? -1 : a === b ? 0 : +1; }
    if (typeof(a) === "string") { a = $charArray(a); }
    if (typeof(b) === "string") { b = $charArray(b); }
//  if (Array.isArray(a)) {
    if (sequence(a)) {
//      if (!Array.isArray(b)) { return 1; }
        if (!sequence(b)) { return 1; }
        let la = length(a),
            lb = length(b),
            ml = Math.min(la, lb);
        for (let i = 1; i <= ml; i += 1) {
            let c = compare(a[i], b[i]);
            if (c !== 0) { return c; }
        }
        return compare(la,lb);
//  } else if (Array.isArray(b)) {
    } else if (sequence(b)) {
        return -1;
    }
    if (typeof(a)==="boolean") { a = a ? 1 : 0; }
    if (typeof(b)==="boolean") { b = b ? 1 : 0; }
    if (a < b) {
        return -1;
    } else if (a === b) {
        return 0;
    } else {
        return 1;
    }
}

function equal(a, b) {
    if (sequence(a)) {
        let la = length(a);
        if (!sequence(b) || la !== length(b)) {
            return false;
        }
        if (typeof(a) === "string") {
            if (typeof(b) === "string") { return (a === b); }
            a = $charArray(a);
        }
        if (typeof(b) === "string") {
            b = $charArray(b);
        }
        for (let i = 1; i <= la; i += 1) {
            if (!equal(a[i], b[i])) { return false; }
        }
        return true;
    }
    // aside: a, b may be function, since routine_id/Icallback are dummy...
    //        they may also be mpq or mpz, with undefined consequences...
    if (typeof(a)==="boolean") { a = a ? 1 : 0; }
    if (typeof(b)==="boolean") { b = b ? 1 : 0; }
    return (a === b);
}

function $conCat(a, b, clone=true) {
    // equivalent to the Phix infix & operator (js overloads +)
    if (integer(a) && a>=0 && a<=255) {
        if (integer(b) && b>=0 && b<=255) {
            return String.fromCodePoint(a) + String.fromCodePoint(b);
        } else if (string(b)) {
            return String.fromCodePoint(a) + b;
        } else if (!sequence(b)) {
            return ["sequence",a,b];
        }
        b = deep_copy(b);
        b[0] = a;
        b.unshift("sequence");
        return b
    } else if (string(a)) {
        if (string(b)) {
            return a + b;
        }
        if (integer(b) && b>=0 && b<=255) {
            return a + String.fromCodePoint(b);
        }
        a = $charArray(a);
        clone = false
    } else if (!sequence(a)) {
        a = ["sequence",a];
        clone = false
    }
    if (string(b)) {
        b = $charArray(b);
    }
    if (sequence(b)) {
        if (clone) {
            // (creates a new array)
            a = a.concat(b.slice(1));
        } else {
            // for eg "res = res & x" cases
            a.push(...b.slice(1));
        }
    } else {
        if (clone) {
            a.push(b);
        } else {
            let idx = a.length;
            a[idx] = b;
        }
    }
    return a;
}

function repeat(item, count) {
//  if (typeof(item) === "string" && item.length === 1) {
//      let res = item.repeat(count);
//      return res;
//  }
    if (integer(item) && (item >= 7) && (item <= 255)) {
        return String.fromCodePoint(item).repeat(count);
    }
    let res = ["sequence"];
    if (Array.isArray(item)) {
        for (let i = 1; i <= count; i += 1) {
            res[i] = deep_copy(item);
        }
    } else {
        res.length = count+1;
        res.fill(item,1,count+1);
    }
    return res;
}

function repeatch(ch,count) {
    if (!integer(ch) || ch < 0 || ch > 255) {
        crash("repeatch not passed a character");
    }
    return String.fromCodePoint(ch).repeat(count);
}   

let $seed = Date.now();

function set_rand(new_seed) {
    $seed = new_seed;
}

function get_rand() {
    return $seed;
}

function rnd() {
//  $seed += 1;
//  $seed = ($seed * 185852 + 1) % 34359738337
//  let x = Math.sin($seed) * 10000;
//  let x = Math.sin($seed) * 100000;
//  return x - Math.floor(x);
//  return Math.random();
//30/1/22 - better results for https://rosettacode.org/wiki/Monte_Carlo_methods#Phix
    // Robert Jenkins' 32 bit integer hash function
    $seed = (($seed + 0x7ED55D16) + ($seed << 12))  & 0xFFFFFFFF;
    $seed = (($seed ^ 0xC761C23C) ^ ($seed >>> 19)) & 0xFFFFFFFF;
    $seed = (($seed + 0x165667B1) + ($seed << 5))   & 0xFFFFFFFF;
    $seed = (($seed + 0xD3A2646C) ^ ($seed << 9))   & 0xFFFFFFFF;
    $seed = (($seed + 0xFD7046C5) + ($seed << 3))   & 0xFFFFFFFF;
    $seed = (($seed ^ 0xB55A4F09) ^ ($seed >>> 16)) & 0xFFFFFFFF;
    return ($seed & 0xFFFFFFF) / 0x10000000;
}

function sq_rnd(/*object*/ shape=1) {
    if (atom(shape)) { return rnd(); }
/* // old
    let /!*sequence*!/ res = repeat(0,length(shape));
    for (let i = 1, i$lim = length(shape); i <= i$lim; i += 1) {
        res = $repe(res,i,sq_rnd($subse(shape,i)));
    }
//untried (erm, can shape be a string...?):
*/
//warning: not yet properly tested...
    if (string(shape)) { shape = $charArray(shape); }
    let l = length(shape),
        res = repeat(0,l);
    for (let i = 1; i <= l; i += 1) {
        res[i] = sq_rnd(shape[i]);
    }
    return res;
}

function rand(n) {
    if (n <= 0) {
        if (n !== -1) { crash("argument to rand() must be >= 1"); }
        n = 0xFFFFFFFF;
    }
    return ((n*rnd()) >>> 0)+1;
}

function rand_range(/*integer*/ lo, hi) {
    if (lo>hi) { [lo,hi] = [hi,lo]; }
    lo -= 1;
    return lo+rand(hi-lo);
}

//function floor(n) {
//  return Math.floor(n);
//}

//21/3/24 should be in pwa/builtins/misc.js...
//function reverse(src, from_to = ["sequence",1,-1]) {
//  let lo = from_to[1],
//      hi = from_to[2],
//      len = length(src)+1;
//  if (lo < 0) { lo += len; }
//  if (hi < 0) { hi += len; }
//  let res = [...src];
//  if (len > 2 && lo < hi) {
//      let mid = floor((lo+hi-1)/2);
//      for (let lx = lo; lx <= mid; lx += 1) {
//          res[hi] = src[lx];
//          res[lx] = src[hi];
//          hi -= 1;
//      }
//  }
//  return res;
//}

function routine_id(rtn_name) {
    if (typeof(rtn_name) === "function") { return rtn_name; }
    let rtn = window[rtn_name];
    if (typeof(rtn) !== "function") { return -1; }
    return rtn;
}

function call_func(rid,params) {
    if (typeof(rid) !== "function") { crash("invalid routine_id"); }
    return rid(...params.slice(1));
}

function call_proc(rid,params) {
    if (typeof(rid) !== "function") { crash("invalid routine_id"); }
    rid(...params.slice(1));
}

function xor(a,b) {
//  return ( a || b ) && !( a && b );
//  return ( a && !b ) || ( !a && b );
  return ((!a) != (!b)) ? 1 : 0;
//  return (!a != !b) ? true : false;   // maybe...
}

function time() {
    let d = new Date();
    return d.valueOf()/1000;
}

function remainder(a, b) {
    return a % b;
}
const rmdr = remainder;

function machine_bits() {
    return 32;
}

function machine_word() {
    return 4;
}

function version() {
    return "1.0.5";
}
//let IupVersion = version;

function platform() {
    return JS;
}

function requires(x) {  // (hand translated)
//
// x: should be eg "0.8.2" for a version() requirement, or a
//                  WINDOWS/LINUX/WEB platform() check, or
//                  32/64 for a machine_bits() check.
//
    if (string(x)) {
        let v = version(),
            reqs = x.split('.').map(Number),
            acts = v.split('.').map(Number);
        if (reqs.length !== 3 || 
            acts.length !== 3 || 
            reqs[0] > acts[0] ||
           (reqs[0]===acts[0] && reqs[1] > acts[1]) ||
           (reqs[0]===acts[0] && reqs[1]===acts[1] && reqs[2] > acts[2])) {
            crash("requires %s, this is %s",["sequence",x,v]);
        }
    } else if (x >= 0 && x < 31) {
        if (x !== platform()) {
            if (x<6 || x>8) {
                let platforms = ["DOS32","WINDOWS","LINUX","JAVASCRIPT",
                                 "WINDOWS or LINUX",                // 5
                                 "WINDOWS or JAVASCRIPT",           // 6
                                 "WINDOWS or LINUX or JAVASCRIPT",  // 7
                                 "LINUX or JAVASCRIPT"],            // 8
                    that = platforms[x-1];
                crash("requires %s, this is JS",["sequence",that]);
            }
        }
    } else if (x !== machine_bits()) {
        crash("requires %d bit (JS is inherently 32 bit)...",["sequence",x]);
    }
}
requires(JS);

function $sidii(s, idii, skip=0, t) {
    // note this is only ever dealing with dword-sequence subscripts,
    //      and should never ever be asked to subscript a string.
    if (idii) { 
//      let l = length(idii)-skip;
        let l = length(idii);
//      for (let i = 1; i <= l; i += 1) {
        for (let i = l; i > skip; i -= 1) {
            let idx = floor(idii[i]);
            if (idx<0) { idx += length(s)+1; }
            if (string(s) || atom(s)) { crash("attempt to subscript an atom"); }
            s = s[idx];
        }
//12/5/21 (!!)
//      if (skip) { s[idii[l+1]] = t; }
//20/4/22 (!!)
//      if (skip) { s[idii[1]] = t; }
        if (skip) { 
            let idx = idii[1];
            if (idx<0) { idx += length(s)+1; }
            s[idx] = t;
        }
    }
    if (!sequence(s)) { crash("attempt to subscript an atom"); }
    // note: sometimes (see "shudder" below) this is invoked to effect
    //       pass-by-sharing-semantics-side-effects, and the return 
    //       value is ignored / not the result you are looking for...
    return s;
}

function $subse(s, idx, idii) {
    // A Phix compatible res := s[[idii]][idx] for strings and sequences
    sequence(s,"s");
    atom(idx,"idx");
    idx = floor(idx);
    s = $sidii(s,idii);
    if (idx<0) { idx += length(s)+1; }
    if (typeof(s) === "string") {
        return s.codePointAt(idx-1);
    } //else {
    return s[idx];
}   

function $subss(s, ss, se, idii) {
    // A Phix compatible res := s[[idii]][ss..se] for strings and sequences
    sequence(s,"s");
    atom(ss,"ss");
    atom(se,"se");
    ss = floor(ss);
    se = floor(se);
    s = $sidii(s,idii);
    if (ss<=0) { ss += length(s)+1; }
    if (se<0) { se += length(s)+1; }
    if (typeof(s) === "string") {
        return s.slice(ss-1,se);
    }
    s = s.slice(ss-1,se+1);
    s[0] = "sequence";
    return s;
}

function $repe(s, idx, x, idii) {
    // A Phix compatible s[[idii]][idx] := x for strings and sequences
    // nb this relies on JavaScript pass-by-sharing semantics internally.
    sequence(s,"s");
    atom(idx,"idx");
    idx = floor(idx);
    let t = $sidii(s,idii);
    if (idx<0) { idx += length(t)+1; }
    if (string(t)) {
        if (integer(x) && x>=0 && x<=255) {
            t = t.substring(0,idx-1) + String.fromCodePoint(x) + t.substring(idx);
            if (!idii) { return t; }
            t = $sidii(s,idii,1,t);
            return s;
        }
        t = $charArray(t);
    }
    t[idx] = x;
    if (string(s) || !idii) { return t; }
    // ooh-uh-ooh-uh (/shudder), these pass-by-sharing semantics make me right queasy...
    // ('s ok, I just prefer code to say what it is doing, rather than do & make me guess.)
    /*t =*/ $sidii(s,idii,1,t);
    return s;
}

function $repss(s, ss, se, x, idii) {
    // A Phix compatible s[[idii]][ss..se] := x for strings and sequences
    //  (including variable length slice assignment)
    // nb this relies on JavaScript pass-by-sharing semantics internally.
    sequence(s,"s");
    atom(ss,"ss");
    atom(se,"se");  
    ss = floor(ss);
    se = floor(se);
    let t = $sidii(s,idii);
    if (ss<=0) { ss += length(t)+1; }
    if (se<0) { se += length(t)+1; }
    if (string(t)) {
        if (integer(x) && x>=0 && x<=255) {
//21/1/22!!
//          t = t.substring(0,ss-1) + repeat(x,se-ss+1) + t.substring(se);
            t = t.substring(0,ss-1) + String.fromCodePoint(x).repeat(se-ss+1) + t.substring(se);
            if (!idii) { return t; }
            t = $sidii(s,idii,1,t);
            return s;
        }
        if (string(x)) {
            t = t.slice(0,ss-1) + x + t.slice(se);
            if (string(s)) { return t; }
            /*t =*/ $sidii(s,idii,1,t);
            return s;
        }
        t = $charArray(t);
//      /*t =*/ $sidii(s,idii,1,t);
    }
    if (atom(x)) {
        for (let i = ss; i <= se; i += 1) {
            t[i] = x;
        }
    } else {
//17/2/22:
        if (string(x)) { x = $charArray(x); }
        let l = length(x),
            sl = se-ss+1;
        if (l === sl) {
            for (let i = 1; i <= sl; i += 1) {
                t[ss] = x[i];
                ss += 1;
            }
        } else {
//          let inner = repeat(x,sl);
//          if (string(inner)) { inner = $charArray(inner); }
//          t = $conCat($conCat(t.slice(0,ss),inner),t.slice(se+1));
            if (string(x)) { x = $charArray(x); }
//          t = $conCat($conCat(t.slice(0,ss),x),t.slice(se+1));
            let te = t.slice(se); te[0] = "sequence";
            t = $conCat($conCat(t.slice(0,ss),x),te);
        }
    }
    if (string(s) || !idii) { return t; }
    // ooh-uh-ooh-uh (/shudder), these pass-by-sharing semantics make me right queasy...
    // ('s ok, I just prefer code to say what it is doing, rather than do & make me guess.)
    /*t =*/ $sidii(s,idii,1,t);
    return s;
}

function and_bits(a, b) {
    return a & b;
}

function and_bitsu(a, b) {
    return (a & b) >>> 0;
}

function or_bits(a, b) {
    return a | b;
}

function or_bitsu(a, b) {
    return (a | b) >>> 0;
}

function xor_bits(a, b) {
    return a ^ b;
}

function xor_bitsu(a, b) {
    return (a ^ b) >>> 0;
}

function not_bits(a) {
    return ~a;
}

function not_bitsu(a) {
    return (~a) >>> 0;
}

const power = Math.pow,
      floor = Math.floor,
      sqrt = Math.sqrt,
      log = Math.log,
      ln = Math.log,
      cos = Math.cos,
      sin = Math.sin,
      tan = Math.tan,
      arctan = Math.atan;

function date(bMSecs = false) {
    let D = new Date(),
        y = D.getFullYear(),    // Get the year as a four digit number (yyyy)
        m = D.getMonth()+1,     // Get the month as a number (1-12),
        d = D.getDate(),        // Get the day as a number (1-31)
        dot = ["sequence",0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334],
        leap = (remainder(y,4)===0) && ((remainder(y,100)!==0) || (remainder(y,400)===0)),
        doy = d+dot[m]+(m>2 && leap);
    let s = ["sequence",
             y,
             m,
             d,
             D.getHours(),              // Get the hour (0-23)
             D.getMinutes(),            // Get the minute (0-59)
             D.getSeconds(),            // Get the second (0-59)
             bMSecs?D.getMilliseconds() // Get the millisecond (0-999)
                   :D.getDay()+1,       // Get the weekday as a number (1-7)
             doy
            ]
    return s;
}

function get_routine_info(fn) {
    let minp = 0, fname;
//  if (integer(fn)) {
    if (integer(fn) && typeof(fn) !== "function") {
        const e = new Error();
        const frame = e.stack.split("\n")[2-fn];
        fname = frame.split(" ")[5];
    } else {
        minp = fn.length;
        fname = fn.name;
    }
    return ["sequence",minp,0,"",fname];
}

// (transpiled directly from pfile.e)
function file_size_k(/*atom*/ size, /*integer*/ width=1) {
//
// Trivial routine to convert a size in bytes to a human-readable string, such as "2GB".
// The width setting is also "sticky", ie whatever is set becomes the new default.
//
    let /*integer*/ sw = max(width,1), 
                    s2 = ((sw>=3) ? sw-2 : sw), 
                    fdx = 0;
    let /*string*/ fmt = sprintf("%%%d.0f%%s",sw),  // eg "%11.0f%s" (the %s gets ""...)
                   sfmt = sprintf("%%%d.0f%%s",s2),  // eg "%9.0f%s" (this %s gets eg "KB")
                   dpsfmt = sprintf("%%%d.2f%%s",s2),  // eg "%9.2f%s" (        ""          )
                   res, suffix = "";
    while (fdx<=3) {
        let /*atom*/ rsize = round(size/1024,100); // (to 2 d.p.)
        if (rsize<1) { break; }
        size = rsize;
        fdx += 1;
        suffix = $conCat($subse("KMGT",fdx), 0X42);
        fmt = sfmt;
    }
    if (!equal(size,trunc(size))) { fmt = dpsfmt; }
    res = sprintf(fmt,["sequence",size,suffix]);
    return res;
}

// (transpiled directly from machine.e, *2)
function int_to_bits(/*atom*/ x, /*integer*/ nbits=0) {
// Returns the low-order nbits bits of x as a sequence of 1's and 0's. 
// Note that the least significant bits come first. You can use 
// sq_and/or/not operators on sequences of bits. You can also subscript, 
// slice, concatenate and so on to manipulate bits.
    let /*sequence*/ bits;
    let /*integer*/ mask = 1;
    if (nbits<=0) {
        // (not intended to be fast, or ever timed as slow, just more conventient)
        if (!integer(x) || x<0) { crash("int_to_bits(x,0): x must be integer >=0",2); }
        bits = ["sequence"];
        while (true) {
            bits = $conCat(bits, and_bits(x,1));
            x = floor(x/2);
            if (x === 0) { break; }
        }
    } else {
        bits = repeat(0,nbits);
        if (integer(x) && nbits<30) {
            // faster method
            for (let i = 1; i <= nbits; i += 1) {
                bits = $repe(bits,i,and_bits(x,mask) && 1);
                mask *= 2;
            }
        } else {
            // slower, but works for large x and large nbits
            if (x<0) {
                x += power(2,nbits); // for 2's complement bit pattern
            }
            for (let i = 1; i <= nbits; i += 1) {
                bits = $repe(bits,i,remainder(x,2));
                x = floor(x/2);
            }
        }
    }
    return bits;
}

function bits_to_int(/*sequence*/ bits) {
// get the (positive) value of a sequence of "bits"
    let /*atom*/ val = 0, p = 1, nbits = length(bits);
    for (let i = 1; i <= nbits; i += 1) {
        if ($subse(bits,i)) {
            val += p;
        }
        p += p;
    }
    return val;
}
    
function int_to_bytes(a, size=4) {
    let res = repeat(0,size);
    for (let i = 1; i <= size; i += 1) {
        let byte = a & 0xff;
        res[i] = byte
        a = (a-byte) / 0x100;
    }
    return res;
}

//DEV signed not used/tested...
function bytes_to_int(s, signed=true) {
    let res = 0;
    for (let i = length(s); i >= 1; i -= 1) {
        res = (res * 0x100) + s[i];
    }
    return res;
}

function $catch(e) {
    // In Javascript a catch(e) can receive a string, number, boolean, or object,
    // so this simply massages that into a slightly more compatible Phix sequence.
    if (string(e) || !sequence(e)) {
        if (typeof(e) === "object") { e = e.message; }
        e = ["sequence",0,0,0,0,"","","",e];
    }
    return e;
}

function utf8_to_utf32(/*string*/ s) {
//12/1/22:
    s = ["sequence",...s];
    let l = length(s);
    for (let i = 1; i <= l; i += 1) {
        s[i] = s[i].codePointAt(0);
    }
    return s;
}

function utf32_to_utf8(/*sequence*/ s) {
    if (Array.isArray(s) && s[0] === "sequence") {
        let res = "",
            l = length(s);
        for (let i = 1; i <= l; i += 1) {
            res += String.fromCodePoint(s[i]);
        }
        s = res;
    }
    return s;
}

function include_file() { return 1; } // see docs
function incl0de_file() { return 0; }
// DEV/SUG: (see T_include_file on line 408 of p2js_parse.e, but let's be testing summat before making any change.)
//          (erm, but obvs. T_incl0de_file **cannot** become T_incl$de_file... hmmm...... )
//function incl$de_file() { return 0; }

function command_line() {
    return ["sequence","p2js",document.location.href];
}

function get_proper_path(filepath) {
    return filepath;
}

function get_proper_dir(/*string*/ filepath, /*bool*/ remove_slash=false) {
    string(filepath,"filepath");
    let l = filepath.length, res = "";
    for (let i = l-1; i >= 0; i -= 1) {
        let ch = filepath.codePointAt(i);
        if (ch === 0X2F ||  // '/'
            ch === 0X5C) {  // '\\'
            res = filepath.slice(0,i+1-remove_slash);
            break;
        } 
    }
    return res;
}

function trace(/*integer*/ i) {
    // (ignored, in other words transpilation behaves much like compilation, as far as this routine is concerned)
//Maybe:
//  debugger;
}

function clear_screen() {
    if (typeof(IupOpen) === "function") { crash("clear_screen() when pGUI detected"); }
    if (typeof(gMainLoop) === "function") { crash("clear_screen() when xpGUI detected"); }
    $docBody.innerHTML = "";
//  position(0,0);
}

function text_color(/*integer*/ i) {
    $tx_clr = $console_colours[i];
}

function bk_color(/*integer*/ i) {
    $bg_clr = $console_colours[i];
}

function even(/*atom*/ a) {
    return !(a & 1);
}

function odd(/*atom*/ a) {
    return a & 1;
}

function free_console() {}

function instance() { return 0; }

function atom_to_float32(/*atom*/ a) {
    let buffer = new ArrayBuffer(4),
          view = new DataView(buffer),
     byteArray = new Uint8Array(buffer);
    view.setFloat32(0, a);
    let res = repeat(0,4);
    for (let i = 1; i <= 4; i += 1) {
        res[i] = byteArray[4-i];
    }
    return res;
}

function atom_to_float64(/*atom*/ a) {
    let buffer = new ArrayBuffer(8),
          view = new DataView(buffer),
     byteArray = new Uint8Array(buffer);
    view.setFloat64(0, a);
    let res = repeat(0,8);
    for (let i = 1; i <= 8; i += 1) {
        res[i] = byteArray[8-i];
    }
    return res;
}

function float32_to_atom(/*sequence*/ s) {
    let buffer = new ArrayBuffer(4),
          view = new DataView(buffer),
     byteArray = new Uint8Array(buffer);
    for (let i = 1; i <= 4; i += 1) {
        byteArray[4-i] = s[i];
    }
    let res = view.getFloat32(0);
    return res;
}

function float64_to_atom(/*sequence*/ s) {
    let buffer = new ArrayBuffer(8),
// (I doubt there is any measurable difference between view and floatArray...)
          view = new DataView(buffer),
//  floatArray = new Float32Array(buffer),
     byteArray = new Uint8Array(buffer);
    for (let i = 1; i <= 8; i += 1) {
        byteArray[8-i] = s[i];
    }
    let res = view.getFloat64(0);
//  let res = floatArray[0];
    return res;
}
// (sadly there is no get/setFloat80 in JavaScript...)

const is_nan = Number.isNaN;

function is_inf(x, sgn) {
    if (typeof(x)!="number" || Number.isFinite(x) || Number.isNaN(x)) { return false; }
    if (sgn==-1) { return x<0; }
//  if (sgn==-1) { return x==Number.NEGATITIVE_INFINITY; } // (untested)
    if (sgn==0) { return true; }
    if (sgn==+1) { return x>0; }
//  if (sgn==+1) { return x==Number.POSITIVE_INFINITY; } // (not tested)
}

function video_config() {
    return ["sequence",1,3,25,80,0,0,32,1,25,80];
}

//DEV experimental...
function speak(/*string*/ text, /*atom*/ rate=0, /*integer*/speech_cb=0) {
    let utterance = new SpeechSynthesisUtterance(text);
    if (speech_cb) {
        function eh(event) {
            if (event.type === 'boundary') {
//no help...
//if (event.charLength) {
                speech_cb(event.charIndex,event.charLength);
//}
            } else { // 'end'
                speech_cb(-1,0);
            }   
//          console.log('type:' + event.type + ' name:' + event.name + ' charIndex:' + event.charIndex + ' charLength:' + event.charLength);
        }
        utterance.addEventListener('boundary', eh);
        utterance.addEventListener('end', eh);
    }
    if (rate) { utterance.rate = rate; }
    window.speechSynthesis.speak(utterance);
}

//if you have another AudioContext class use that one, as some browsers have a limit
//const $audioCtx = new (window.AudioContext || window.webkitAudioContext || window.audioContext);
let $audioCtx;

//All arguments are optional:

//duration of the tone in milliseconds. Default is 500
//frequency of the tone in hertz. default is 440
//volume of the tone. Default is 1, off is 0.
//type of tone. Possible values are sine, square, sawtooth, triangle, and custom. Default is sine.
function beep(frequencies=440, durations=500, volume=0.1, type) {
    if (!$audioCtx) {
//      $audioCtx = new (window.AudioContext || window.webkitAudioContext || window.audioContext);
        $audioCtx = new window.AudioContext;
    }
    let oscillator = $audioCtx.createOscillator(),
          gainNode = $audioCtx.createGain(),
                 t = $audioCtx.currentTime;

    oscillator.connect(gainNode);
    gainNode.connect($audioCtx.destination);

    if (volume) { gainNode.gain.value = volume; }
    if (type) { oscillator.type = type; }
    let af = atom(frequencies),
        ad = atom(durations),
        l = af?(ad?1:length(durations)):length(frequencies);

//  if (!frequencies) {
//      frequencies = ["sequence",440];
//  } else if (atom(frequencies)) {
//      frequencies = ["sequence",frequencies];
//  } else {
//      l = length(frequencies);
//  }
//  if (!durations) {
//      durations = ["sequence",500];
//  } else if (atom(durations)) {
//      durations = ["sequence",durations];
//  }
//  if (frequency) { oscillator.frequency.value = frequency; }
    for (let i = 1; i <= l; i += 1) {
        let f = af?((ad || odd(i))?frequencies:0):frequencies[i],
            d = ad?durations:durations[i];
        oscillator.frequency.setValueAtTime(f, t);
        t += d / 1000;
    }
    oscillator.start();
    oscillator.stop(t);
}

/*
[DEV] maybe we /can/ have sleep()...
const delay = (n) => {
  return new Promise((resolve) => {
    setTimeout(()=> resolve(n), n)
  })
}
async function main() {
  const delays = [100, 200, 300].map(n => delay(n))
  print("waiting…")
  const res = await Promise.all(delays)
  print("done. result is " + res)
}
main()
*/    

/*
From Douglas Crockford (or just transpile builtins/sort.e)
let m = ['aa', 'bb', 'a', 4, 8, 15, 16, 23, 42];
m.sort(function (a, b) {
    if (a === b) {
        return 0;
    }
    if (typeof a === typeof b) {
        return a < b ? -1 : 1;
    }
    return typeof a < typeof b ? -1 : 1;
});
// m is [4, 8, 15, 16, 23, 42, 'a', 'aa', 'bb']
*/  

//printf(1,'123 != "123" :%t\n',123 != "123")
//printf(1,'123 !== "123" :%t\n',123 !== "123")
////printf(1,'123 !=== "123" :%t\n',123 !=== "123")

