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
      WEB = 4,
      JS = WEB,
      JAVASCRIPT = WEB,
      PI = Math.PI,
      E = Math.E,
      E_USER = 8,
      INVLN10 = 0.43429448190325182765,
//    INVLN10 = 1/Math.LN10,    // NO!!
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
      SLASH = 0x2f,
      TEST_QUIET = 0,
      TEST_SUMMARY = 1,
      TEST_SHOW_FAILED = 2,
      TEST_SHOW_ALL = 3,
      TEST_ABORT = 1,
      TEST_CRASH = -1,
      TEST_PAUSE = 1,
      TEST_PAUSE_FAIL = -1

function puts(fn, text) {
    integer(fn,"fn");
//  string(text,"text");
    if ((fn !== 1) && (fn !== 2)) { crash("fn must be 1 or 2"); }
    if (typeof(text) === "number") { text = String.fromCodePoint(text); }
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
    let where = fn === 2 ? "afterbegin" : "beforeend";
//  text = `<span style="font-family: monospace;">` + text + `<\span>`;
    $docBody.insertAdjacentHTML(where, text);
//  $docBody.insertAdjacentText(where, text);
}

function crash(msg, args = []) {
//  string(msg,"msg");
//  object(args,"args");
    if (!Array.isArray(args) || (args.length !== 0)) {
        // ^ ie treat sprintf(fmt,5) as sprintf(fmt,{5})
        msg = sprintf(msg,args);
    }
    msg += "\n";
    puts(2,msg);
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
    if ((ti !== "boolean") && (ti !== "function") && !Number.isSafeInteger(i)) {
        return $typeCheckError(name,i);
    }
    return true;
}

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

function deep_copy(p, depth=-1) {
    // (we got no refcount here, so must ignore bIfNeeded)
    if (Array.isArray(p)) {
        p = p.slice();
        if (depth) {
            depth -= 1;
            let pl = p.length;
            for (let i = 1; i < pl; i += 1) {
                p[i] = deep_copy(p[i],depth);
            }
        }
    }
    return p;
}

function $charArray(s) {
// Needed because Array.from(string) produces lots of diddy-strings...
//  whereas this produces the array of codePointAt I was expecting.
    string(s,"s");
    let l = s.length;
    let res = ["sequence"];
    for (let i=0; i<l; i+=1) {
        let ch = s.codePointAt(i);
        res[i+1] = ch;
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
    let i = sequence(args) ? 0 : -1;

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
                            if (!Number.isInteger(ch) || ch<0 || ch>255) {
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
                let negative = num < 0;
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
//    let sign = "";
//    (num += "").charAt(0) == "-" && (num = num.substring(1), sign = "-");
//    let arr = num.split(/[e]/ig);
//    if (arr.length < 2) return sign + num;
//    let n = arr[0], exp = +arr[1];
//    let w = (n = n.replace(/^0+/, '')).replace('.', ''),
//      pos = n.split('.')[1] ? n.indexOf('.') + exp : w.length + exp,
//      L = pos - w.length, s = "" + BigInt(w);
//    function r() {return w.replace(new RegExp(`^(.{${pos}})(.)`), `$1.$2`)}
//    w = exp >= 0 ? (L >= 0 ? s + "0".repeat(L) : r()) : (pos <= 0 ? "0." + "0".repeat(Math.abs(pos)) + s : r());
//    if (!+w) w = 0;
//    return sign + w;
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
        if (b > 36) { crash("p2js.js: printf(%A) does not support base > 36"); }
        return n.toString(b);
    }

//  function callback(expr, sign, size, dot, precision, specifier) {
    function callback(expr, subscript, sign, size, dot, precision, specifier) {
        //
        // Replaces a single formatting specification:
        // expr is eg "%4.2f" or "%-20s" (the whole thing)
        // sign is eg '-' or one of "+,=|", or undefined
        // size is eg "4" (leading 0 === zero-fill, "")
        // dot is eg ".2" or "." or undefined
        // precision is eg "2" or undefined
        // specifier is eg 's' or one of "qQtcvVdboxXeEfgG%"
        //
        if (expr === '%%') { return '%'; }  // (aka specifier === '%')
        i += 1;
        if (subscript) {
            let l = subscript.length-1, j = 1, ssign = +1;
            i = 0;
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
        precision = precision ? parseInt(precision,10) : dot ? 0 : 6;
//      if (precision>15) { precision = 15; }
        if (precision>16) { precision = 16; }
        let res = "";
        switch (specifier) {
            case 's': res = stri(); break;
            case 'q': res = allascii(stri(),specifier); break;
            case 'Q': res = allascii(stri(),specifier); break;
            case 't': res = argi()?"true":"false"; break;
            case 'c': res = String.fromCodePoint(argi()); break;
            case 'v': res = stri(true); break;
            case 'V': res = stri(-1); break;
//          case 'd': res = inti().toString(10); break;
            case 'd': res = remove_scientific(inti()); break;
            case 'b': res = inti().toString(2); break;
            case 'o': res = inti().toString(8); break;
            case 'x': res = inti(true).toString(16).toUpperCase(); break;
            case 'X': res = inti(true).toString(16); break; //^yep===Phix
            case 'a': res = inbase(argi()); break;
            case 'A': res = inbase(argi()).toUpperCase(); break;
            case 'e': res = flti(precision).toExponential(precision); break;
            case 'E': res = flti(precision).toExponential(precision).toUpperCase(); break;
            case 'f': res = flti(precision).toFixed(precision); break;
            case 'g': res = eorf(precision); break;
            case 'G': res = eorf(precision).toUpperCase(); break;
            default: crash("unrecognised specifier");
        }
        if (sign === ',') {
            if ("df".indexOf(specifier) === -1) {
                crash('comma only permitted on %d and %f');
            }
            let showcommas = res.indexOf('.');
            if (showcommas === -1) { showcommas = res.length; }
            let lim = "-+".indexOf(res[0]) === -1 ? 3 : 4;
            while (showcommas>lim) {
                showcommas -= 3;
                res = res.slice(0,showcommas) + ',' + res.slice(showcommas);
            }
        } else if (sign === '+' && res[0] !== '-') {
            res = '+' + res;
        }
        if (size) {
            let pad = (size[0] === '0' && "-+=|".indexOf(sign) === -1 && res.indexOf('-') === -1) ? '0' : ' ';
            let padlen = parseInt(size,10)-res.length;
            if (padlen>0) {
                let half = Math.floor(padlen/2);
                switch (sign) {
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
    // Replace each %[sign][[0]size][.[precision]]specifier (eg "%,7.2f") in turn
    //  (where sign is one of "-+=|," or undefined)
    //
//  const regex = new RegExp("%([-+=|,])?(0?[0-9]+)?([.]([0-9]+)?)?([sqQtcvVdboxXaAeEfgG%])",'g');
    const regex = new RegExp(`%(\[-?[0-9]+\])?([-+=|,])?(0?[0-9]+)?([.]([0-9]+)?)?([sqQtcvVdboxXaAeEfgG%])`,'g');
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
        for (let i=0; i<l; i+=1) {
            let ch = o.codePointAt(i);
            if (ch===0) {
                res += "\\0";
            } else if (ch===0X5C) {
                res += "\\\\";
            } else if (ch===0X22) {
                res += "\\\"";
            } else if (ch>=32) {
                res += o[i];
            } else if (ch===9) {
                res += "\\t";
            } else if (ch===10) {
                res += "\\n";
            } else if (ch===13) {
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
        if (asCh===false) { asCh = true; } // (nb -1 and true left as-is)
        for (let i=1; i<=l; i+=1) {
            if (i>1) { res += ","; }
            res += sprint(o[i],asCh);
        }
        res += "}";
    } else if (asCh===true && integer(o) && o>=32 && o<127) {
        res = "'" + String.fromCodePoint(o) + "'";
    } else if (integer(o)) {
        res = o.toString();
    } else if (o === null) {
        res = "null";
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

function progress() {
    // (it would never work: the browser will not update while JavaScript is running)
    //  (anything using progress() should be rewritten to gui/timer/idle processing)
    crash("progress does not work under p2js");
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
    //  while waiting for keyboard input, no way Hose.)
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

function $conCat(a, b) {
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
    } else if (!sequence(a)) {
        a = ["sequence",a];
    }
    if (string(b)) {
        b = $charArray(b);
    }
    if (sequence(b)) {
        // (creates a new array)
        a = a.concat(b.slice(1));
    } else {
        a.push(b);
    }
    return a;
}

function repeat(item, count) {
//  if (typeof(item) === "string" && item.length === 1) {
//      let res = item.repeat(count);
//      return res;
//  }
    if (integer(item) && item>=7 && item<=255) {
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

//5/8/21 [p]apply() is now auto=transpiled, maybe filter() c/should be too.

function filter(s, rs, userdata = ["sequence"], rangetype = "") {
//
// Select only those elements from a sequence that pass a specified test.
//
    let res = (string(s) ? "" : ["sequence"]),
        ls = length(s);
    if (string(s)) { s = $charArray(s); }
    if (string(userdata)) { userdata = $charArray(userdata); }
    if (typeof(rs) === "string") {
        // built-in handling
        let inout = find(rs,["sequence","in","out"]);
        if (inout !== 0) {
            inout = (inout === 1); // in: true, out: false
            if (rangetype === "") {
                // set handling
                for (let i = 1; i <= ls; i += 1) {
                    let si = s[i],
                        f = find(si,userdata);
                    if ((f !== 0) === inout) {
                        res = append(res,si);
                    }
                }
            } else {
                // inclusive/exclusive lo/hi range handling
                let rt = find(rangetype,["sequence","[]","(]","[)","()"])-1;
                if (rt === -1) { crash("invalid rangetype"); }
                // rt is now 0..3, aka 0b00..0b11:  // exclsve, inclsive
                let xl =  (rt & 1),                 // 0 for [,  1 for (
                    xh = -(rt & 2)/2;               // 0 for ], -1 for )
                if (!sequence(userdata) || length(userdata) !== 2) {
                    crash("userdata must be a sequence of length 2 for in/out handling");
                }
                let lo = userdata[1],
                    hi = userdata[2];
                for (let i = 1; i <= ls; i += 1) {
                    let si = s[i],
                        lc = compare(si,lo),
                        hc = compare(si,hi);
                    if (((lc >= xl) && (hc <= xh)) === inout) {
                        res = append(res,si);
                    }
                }
            }
        } else {
            if (rangetype !== "") { crash("invalid rangetype"); }
            let ct = find(rs,["sequence","<", "<=","=", "!=",">=",">"]);
            if (ct === 0) {
                ct = find(rs,["sequence","lt","le","eq","ne","gt","ge"]);
                if (ct === 0) { //    maybe "=="    
                    if (rs !== "==") { crash("unrecognised comparison operator"); }
                    ct = 3;
                }
            }
            let ne = (ct === 4);
            ct -= (ct >= 4);
            let ok = ["sequence",[-1],[-1,0],[0],[0,1],[1]][ct];
            for (let i = 1; i <= ls; i += 1) {
                let si = s[i], c = compare(si,userdata);
                if ((ok.indexOf(c) !== -1) !== ne) {
                    res = append(res,si);
                }
            }
        }
        return res;
    }

    // user-defined function handling
    if (rangetype !== "") { crash("invalid rangetype"); }
    let fn = rs,
        maxp = fn.length;
    if (maxp<1) { crash("filter routine must accept 1..3 parameters"); }
    if (string(s)) { s = $charArray(s); }
    let mt = equal(userdata,["sequence"]);
    if (maxp===1 && !mt) { crash("filter routine must accept 2..3 parameters"); }
    for (let i = 1; i <= ls; i += 1) {
        let si = s[i], bAdd;
        if (mt) {
            bAdd = fn(si,i,s);
        } else if (maxp === 3) {
//5/8/21 (for the rc "Text completion" task, do what the docs actually say)
//          bAdd = fn(si,s,userdata);
            bAdd = fn(si,i,userdata);
        } else {
            bAdd = fn(si,userdata);
        }
        if (bAdd) {
            res = append(res,si);
        }
    }
    return res;
}

let $seed = Date.now();

function rnd() {
    $seed += 1;
    let x = Math.sin($seed) * 10000;
    return x - Math.floor(x);
//? return Math.random();
}

function rand(n) {
    if (n<=0) {
        if (n === -1) {
            n = 0xFFFFFFFF;
        } else {
            crash("argument to rand() must be >= 1");
        }
    }
    return ((n*rnd()) >>> 0)+1;
}

function rand_range(/*integer*/ lo, hi) {
    if (lo>hi) { [lo,hi] = [hi,lo]; }
    lo -= 1;
    return lo+rand(hi-lo);
}

function get_rand() {
    return $seed;
}

function set_rand(new_seed) {
    $seed = new_seed;
}

function floor(n) {
    return Math.floor(n);
}

function reverse(src, from_to = ["sequence",1,-1]) {
    let lo = from_to[1],
        hi = from_to[2],
        len = length(src)+1;
    if (lo < 0) { lo += len; }
    if (hi < 0) { hi += len; }
    let res = [...src];
    if (len > 2 && lo < hi) {
        let mid = floor((lo+hi-1)/2);
        for (let lx = lo; lx <= mid; lx += 1) {
            res[hi] = src[lx];
            res[lx] = src[hi];
            hi -= 1;
        }
    }
    return res;
}

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
  return (!a != !b) ? 1 : 0;
//  return (!a != !b) ? true : false;   // maybe...
}

function time() {
    let d = new Date();
    return d.valueOf()/1000;
}

function remainder(a, b) {
    return a % b;
}

function machine_bits() {
    return 32;
}

function machine_word() {
    return 4;
}

function version() {
    return "1.0.1";
}

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

function $sidii(s,idii,skip=0,t) {
    // note this is only ever dealing with dword-sequence subscripts,
    //      and should never ever be asked to subscript a string.
    if (idii) { 
//      let l = length(idii)-skip;
        let l = length(idii);
//      for (let i=1; i<=l; i+=1) {
        for (let i=l; i>skip; i-=1) {
            let idx = floor(idii[i]);
            if (idx<0) { idx += length(s)+1; }
            if (string(s) || atom(s)) { crash("attempt to subscript an atom"); }
            s = s[idx];
        }
//12/5/21 (!!)
//      if (skip) { s[idii[l+1]] = t; }
        if (skip) { s[idii[1]] = t; }
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
            t = t.substring(0,ss-1) + repeat(x,se-ss+1) + t.substring(se);
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
        for (let i=ss; i<=se; i+=1) {
            t[i] = x;
        }
    } else {
        let l = length(x),
            sl = se-ss+1;
        if (l===sl) {
            for (let i=1; i<=sl; i+=1) {
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
      sqrt = Math.sqrt,
      log = Math.log,
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
    return ["sequence",fn.length,0,"",fn.name];
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
            if (x===0) { break; }
        }
    } else {
        bits = repeat(0,nbits);
        if (integer(x) && nbits<30) {
            // faster method
            for (let i=1, i$lim=nbits; i<=i$lim; i+=1) {
                bits = $repe(bits,i,and_bits(x,mask) && 1);
                mask *= 2;
            }
        } else {
            // slower, but works for large x and large nbits
            if (x<0) {
                x += power(2,nbits); // for 2's complement bit pattern
            }
            for (let i=1, i$lim=nbits; i<=i$lim; i+=1) {
                bits = $repe(bits,i,remainder(x,2));
                x = floor(x/2);
            }
        }
    }
    return bits;
}

function bits_to_int(/*sequence*/ bits) {
// get the (positive) value of a sequence of "bits"
    let /*atom*/ val, p;
    val = 0;
    p = 1;
    for (let i=1, i$lim=length(bits); i<=i$lim; i+=1) {
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
    return s;
}

function utf32_to_utf8(/*string*/ s) {
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

function speak(/*string*/ s) {
    let utterance = new SpeechSynthesisUtterance("This is an example of speech synthesis.");
    window.speechSynthesis.speak(utterance);
}

/*
maybe we /can/ have sleep()...
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

