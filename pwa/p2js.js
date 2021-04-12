"use strict";
//
// Part 1: utilities puts(), crash(), and [s]printf().
// ==================================================
//
// Aside: js allows $ in identifiers; we use that fact to effectively
//        "hide" things from p2js.exw generated code. For instance,
//        statics like $docBody, $prefer_backtick, and $seed, as well 
//        as internal/private helpers such as $typeCheckError(), and 
//        general use routines such as $conCat() and $deepCopy().
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
// Quickref:   
// =========
//[DEV see phix.chm\Other Libraries\pwa/p2js\JavaScript in pwa/p2js]
//  array.slice(start,end): [nb: shallow copy...] **DEV***
//      if a is ["sequence",1,2,3,4], a.slice(3) is [3,4],
//                                and a.slice(1,2) is [1].
//  array.splice(start,deleteCount,item...):
//      let a = ['a', 'b', 'c'],
//          r = a.splice(1, 1, 'ache', 'bug');
//      a is ['a', 'ache', 'bug', 'c']
//      r is ['b']
//
//  "ABC".charAt(0) is "A" (a length-1 string)
//      equiv to "ABC".slice(0, 1);
//  "ABC".charCodeAt(0) is 65
//  String.fromCharCode(65) is "A"
//  String.fromCharCode(65,66,67) is "ABC" [?spread ... operator ?]
//
let $docBody = document.body;   // (override if required, eg like
                                //  the result panel on codepen.)

function puts(fn, text) {

/*
    function cleanup(text) {
        if (typeof(text) === "number") { text = String.fromCharCode(text); }
        const am = new RegExp("&","g"),
              lt = new RegExp("[<]","g"),
              gt = new RegExp("[>]","g"),
//            sp = new RegExp("[ ][ ]","g"),
              sp = new RegExp("[ ]","g"),
              lf = new RegExp("\\n","g");
        text = text.replace(am,"&amp;")
                   .replace(lt,"&lt;")
                   .replace(gt,"&gt;")
//                 .replace(sp,"&nbsp;&nbsp;")
                   .replace(sp,"&ensp;")
                   .replace(lf,"<br>");
        return text;
    }
    if (fn==="cleanup") { return cleanup; } // for progress()
*/

    integer(fn,"fn");
//  string(text,"text");
    if (fn !== 1 && fn !== 2) { crash("fn must be 1 or 2"); }
//  text = cleanup(text);
    if (typeof(text) === "number") { text = String.fromCharCode(text); }
    const am = new RegExp("&","g"),
          lt = new RegExp("[<]","g"),
          gt = new RegExp("[>]","g"),
//        sp = new RegExp("[ ][ ]","g"),
          sp = new RegExp("[ ]","g"),
          lf = new RegExp("\\n","g");
    text = text.replace(am,"&amp;")
               .replace(lt,"&lt;")
               .replace(gt,"&gt;")
//             .replace(sp,"&nbsp;&nbsp;")
               .replace(sp,"&ensp;")
               .replace(lf,"<br>");
    let where = (fn === 2 ? "afterbegin" : "beforeend");
    $docBody.insertAdjacentHTML(where, text);
//  $docBody.insertAdjacentText(where, text);
}

//const lf = /(\n)/g;
//puts(2,lf.toString());

function crash(msg, args = []) {
//  string(msg,"msg");
//  object(args,"args");
    if (!Array.isArray(args) || args.length !== 0) {
        // ^ ie treat sprintf(fmt,5) as sprintf(fmt,{5})
        msg = sprintf(msg,args);
    }
//  let s = "***ERROR***: " + msg + "<br>"
//  let s = "crash: " + msg + "\n"
//  document.body.insertAdjacentHTML("beforeend", s);
//  document.body.insertAdjacentHTML("afterbegin", s);
//  $docBody.insertAdjacentHTML("afterbegin", s);
//  puts(2,s);
    msg += "\n";
    puts(2,msg);
    throw(new Error(msg));
//  debugger;
//  puts(2,"this should not occur");
}

function $typeCheckError(name, x) {
    if (name.length !== 0) {
        if (x === 'undefined') {
            crash('variable %s has not been assigned a value.',[name]);
        } else {
            crash('typecheck error, %s is %v',[name,x]);
        }
    }
    return false;
}

function integer(i, name = "") {
//
// invoke as (eg) integer(fn) to test, integer(fn,"fn") to typecheck.
//
    if (!Number.isSafeInteger(i)) {
        return $typeCheckError(name,i);
    }
    return true;
}

function atom(a, name = "") {
    if ((typeof(a) !== "boolean") && (typeof(a) !== "number" || !isFinite(a))) {
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
//$share()/refcount:
        if (!Array.isArray(p) || p[0] !== "sequence") {
//      if (!Array.isArray(p) || (p[0] !== "sequence" && p[0] !== "sequense")) {
//      if (!Array.isArray(p) || !(p[0]==="sequence"||(Array.isArray(p[0]) && p[0].length===2 && p[0][0]==="sequence"))) {
            return $typeCheckError(name,p);
        }
    }
    return true;
}

function object(o, name = "") {
    if (typeof(o) === "undefined") {
//      return $typeCheckError(name,o);
        return $typeCheckError(name,"undefined");
//      return false;
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

//function $share(p) {
//  if (Array.isArray(p)) { p[0] = "sequense"; }
//  return p;
//}


function deep_copy(p, depth/*, bIfNeeded*/) {
    // (we got no refcount here, so must ignore asks for bIfNeeded)
    if (Array.isArray(p)) {
        p = p.slice();
        depth -= 1;
        if (depth) {
            let l = p.length;
            for (let i = 1; i < l; i += 1) {
                p[i] = $deepCopy(p[i],depth);
            }
        }
    }
    return p;
}

function $deepCopy(p) {
    if (Array.isArray(p)) {
        p = p.slice();
        let l = p.length;
        for (let i = 1; i < l; i += 1) {
            p[i] = $deepCopy(p[i]);
        }
    }
    return p;
}

function $charArray(s) {
// Needed because Array.from(string) produces lots of diddy-strings....
//  whereas this produces the array of charCodeAt I was expecting.
    string(s,"s");
    let l = s.length;
    let res = ["sequence"];
    for (let i=0; i<l; i+=1) {
        let ch = s.charCodeAt(i);
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
//  if (typeof(fmt) !== "string") {
////        return "invalid fmt";
//      crash("invalid fmt");
//  }
//DEV/temp(?): (or, maybe, i=0)
//  if (Array.isArray(args) && args[0] === "sequence") { args = args.slice(1); }
    let i = -1;
//  string(fmt,"fmt");
//  object(args,"args");
//  let i = 0;
//  if (Array.isArray(args) && args.length>0 && args[0] === "sequence") { i = 0; }
    if (sequence(args)) { i = 0; }

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
                if (Array.isArray(res)) {
                    // compatibility shim: Phix/sprintf() treats eg "ABC",
                    // {'A','B','C'}, and {65[.1],66,67} exactly the same.
                    let allChar = true,
                        sres = "";
                    for (let i = 0; i < res.length; i += 1) {
                        let ch = res[i];
                        if (typeof(ch) === "string" && ch.length === 1) {
                            sres += ch;
                        } else {
                            if (!Number.isInteger(ch) && typeof(ch) === "number") {
                                ch = Math.floor(ch);
                            }
                            if (!Number.isInteger(ch) || ch<0 || ch>255) {
                                allChar = false;
                                break;
                            }
                            sres += String.fromCharCode(ch);
                        }   
                    }
                    if (allChar) { return sres; }
                }
//              crash('Invalid (string expected)');
                res = String.fromCharCode(res);
            }
        }
        return res;
    }
    function inti(unsigned = false) {
        let res = argi();
        if (!Number.isInteger(res)) {
            if (typeof(res) !== "number") {
                crash('Invalid (integer expected)');
            }
            res = Math.floor(res);
//          res = Math.round(res - res % 1); // (maybe..)
        }
        if (unsigned) {
            res = res >>> 0; // (casts -ve to +ve)
//          res = Math.abs(res);
        }
        return res;
    }
    function flti(precision) {
        let res = argi();
        if (typeof(res) !== "number") {
            crash('Invalid (number expected)');
        }
//function RoundNum(num, length) { 
//  var number = Math.round(num * Math.pow(10, length)) / Math.pow(10, length);
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
//  function eorf(precision: number): number {  // (ie "%g") // NO!
//  function eorf(precision/*: number*/)/*: string*/ {  // (ie "%g")
    function eorf(/* integer */ precision) {    // (ie "%g")
//  function eorf(precision) {  // (ie "%g")

        let eres = flti(precision).toExponential(precision);
        const zeroE = new RegExp("([.][0]+e|[0]+e)");
        const Ezero = new RegExp("(e\\+0$)");
        eres = eres.replace(zeroE,"e");             // eg "1.000000e14" -> "1e14"
        eres = eres.replace(Ezero,"");              // eg        "1e+0" -> "1"

        let fres = flti(precision).toFixed(precision);
        if (fres.indexOf('.') != -1) {
            const Fzero = new RegExp("([.]?[0]+$)");    // eg "10.2300" -> "10.23"
            fres = fres.replace(Fzero,"");              // or "1023.00" -> "1023"
        }

        // note this will often disagree with the Phix/Eu output/choice
        //  (they pick %e for "(exponent>=precision or exponent<-4)")
        let res = eres.length<fres.length || parseFloat(fres) === 0 ? eres : fres;
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
//                  x[i..i] = '\\'&c    -- NB does not work on RDS Eu/OpenEuphoria
                    x = x.slice(0,i) + '\\' + x.slice(i);
                }
            } else if (c<' ' || c>'~') {
                if (c === '\t') { c='t'; }
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
//              x[i..i] = '\\'&c    -- NB does not work on RDS Eu/OpenEuphoria
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

    function callback(expr, sign, size, dot, precision, specifier) {
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
        precision = precision ? parseInt(precision,10) : dot ? 0 : 6;
        if (precision>15) { precision = 15; }
        let res = "";
        switch (specifier) {
            case 's': res = stri(); break;
            case 'q': res = allascii(stri(),specifier); break;
            case 'Q': res = allascii(stri(),specifier); break;
            case 't': res = argi()?"true":"false"; break;
            case 'c': res = String.fromCharCode(argi()); break;
            case 'v': res = stri(true); break;
            case 'V': res = stri(-1); break;
            case 'd': res = inti().toString(10); break;
            case 'b': res = inti().toString(2); break;
            case 'o': res = inti().toString(8); break;
            case 'x': res = inti(true).toString(16).toUpperCase(); break;
            case 'X': res = inti(true).toString(16); break; //^yep===Phix
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
        }
        else if (sign === '+' && res[0] !== '-') {
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
//  const regex = /%([-+=|,])?(0?[0-9]+)?([.]([0-9]+)?)?([stcvdboxXeEfgG%])/g;
//puts(2,regex.toString());
//  const regex = new RegExp('/%([-+=|,])?(0?[0-9]+)?([.]([0-9]+)?)?([stcvdboxXeEfgG%])/g');
    const regex = new RegExp("%([-+=|,])?(0?[0-9]+)?([.]([0-9]+)?)?([sqQtcvVdboxXeEfgG%])",'g');
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
        for (let i = 0; i < args.length; i += 2) {
            if (args[i] === 'prefer_backtick') {
                $prefer_backtick = args[i+1];
            } else {
                // sug: simply ignore 'unicode_align'?
                crash("unsupported printf option " + args[i]);
            }
        }
    } else {
        puts(fn,sprintf(fmt, args));
    }
}

// DEV/DOC: this is flattened...
//print(1,[[[[1,3]]],"this",3.1425,[5,7],11]) // (flattened...!)
function sprint(o,asCh) {
//  object(o,"o");
    let res;
//  if (typeof(o) === "string") {
    if (string(o)) {
//      let all_ascii = true;
        let l = o.length;
        res = "\"";
        for (let i=0; i<l; i+=1) {
            let ch = o.charCodeAt(i);
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
//bollocks!             o = ["sequence",...Array.from(o)];
                o = $charArray(o);
                return sprint(o,asCh);
            }
        }   
        res += "\"";
//  } else if (Array.isArray(o)) {
    } else if (sequence(o)) {
        let l = o.length-1;
//      res = "{" + res + "}";
//      res = "{" + res.splice(1).toString + "}";
        res = "{";
        if (asCh===false) { asCh = true; } // (nb -1 and true left as-is)
        for (let i=1; i<=l; i+=1) {
            if (i>1) { res += ","; }
            res += sprint(o[i],asCh);
        }
        res += "}";
    } else if (asCh===true && integer(o) && o>=32 && o<127) {
        res += "'" + String.fromCharCode(o) + "'";
    } else if (integer(o)) {
        res = o.toString();
    } else if (o === null) {
        res = "null";
    } else {
        function cut_tz(v) {
            if (v.indexOf('.') !== -1) {
                let cutFrom = v.length - 1
                while (v[cutFrom] === '0') {
                    cutFrom--
                }
                if (v[cutFrom] === '.') {
                    cutFrom--
                }
                v = v.substr(0, cutFrom + 1)
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

/* it'll never work...
function progress(msg, args = []) {
    const cleanup = puts("cleanup");
/@*
    let progress_div = $docBody.querySelector(".progress");
    if (!progress_div) {
//      progress_div = $docBody.createElement("div");
        progress_div = document.createElement("div");
        progress_div.classList.add('progress');
        $docBody.appendChild(progress_div);
    }
*@/
    if (!Array.isArray(args) || args.length !== 0) {
        // ^ ie treat progress(fmt,5) as progress(fmt,{5})
        msg = sprintf(msg,args);
    }
    msg = cleanup(msg);
//  progress_div.innerHTML = "<nobr>" + msg + "</nobr>";
//  setInterval(function() { progress_div.innerHTML = "<nobr>" + msg + "</nobr>"; }, 1000);
    window.statusbar.innerHTML = "<nobr>" + msg + "</nobr>";
}
*/
function progress() {
    crash("progress does not work under p2js");
}

//DEV why not just use builtins/pfactors.e, transpiled?
function factors(num, include1 = 0) {
//  atom(num,"num");
//  integer(include1,"include1");
    let n_factors = [],
        h_factors = [],
        i0 = include1 === 0 ? 2 : 1;
 
    for (let i = i0; i <= Math.floor(Math.sqrt(num)); i += 1) {
        if (num % i === 0) {
            n_factors.push(i);
            let ni = num/i;
            if ((ni !== i) && (ni !== num || include1 === 1)) {
//              n_factors.push(ni);
                h_factors.push(ni);
            }
        }
    }
//  n_factors.sort(function(a, b){return a - b;});  // numeric sort
    n_factors = n_factors.concat(h_factors.reverse());
    return n_factors;
}
 
//print(1,factors(45,1));  // [1,3,5,9,15,45] 
//print(1,factors(53,1));  // [1,53] 
//print(1,factors(64,1));  // [1,2,4,8,16,32,64]

//DEV why not just use builtins\psum.e, transpiled?
function sum(x) {
//  object(x,"x");
    if (!Array.isArray(x)) {
        if (typeof(x) === "number") { return x; }
        crash("argument to sum() must be a number or array");
    }
    let total = 0;
    for (let i = 0; i < x.length; i += 1) {
        let xi = x[i];
        if (Array.isArray(xi)) {
            xi = sum(xi);
        }
        total += xi;
    }
    return total;
}

function product(x) {
//  object(x,"x");
    if (!Array.isArray(x)) {
        if (typeof(x) === "number") { return x; }
        crash("argument to product() must be a number or array");
    }
    let res = 1;
    for (let i = 0; i < x.length; i += 1) {
        let xi = x[i];
        if (Array.isArray(xi)) {
            xi = product(xi);
        }
        res *= xi;
    }
    return res;
}

//let s = [1,2,3,4,5];
//printf(1,"sum is %d\n",sum(s));
//printf(1,"prod is %d\n",product(s));

function append(a, x) {
//  object(a,"a");
//  object(x,"x");
//  if (typeof(a) === "string") {
    if (string(a)) {
        if (integer(x) && x>=0 && x<=255) {
            a += String.fromCharCode(x);
        } else {
//          a = $charArray(a).push(x);
            a = $charArray(a);
            a.push(x);
        }
    } else {
//      if (!Array.isArray(a)) {
        if (atom(a)) {
//          a = [a];
            a = ["sequence",a,x];
        } else {
            a.push(x);
        }
    }
//  a.push(newElement); === a = append(a,newElement);
    return a;
}

function prepend(a, x) {
//  object(a,"a");
//  object(x,"x");
//  if (typeof(a) === "string") {
    if (string(a)) {
        if (integer(x) && x>=0 && x<=255) {
            a = String.fromCharCode(x) + a;
        } else {
            a = $charArray(a);
            a[0] = x;
            a.unshift("sequence");
        }
    } else {
//      if (!Array.isArray(a)) {
        if (atom(a)) {
//          a = [a];
            a = ["sequence",x,a];
        } else {
//          a = [x].concat(a);
            a[0] = x;
            a.unshift("sequence");
        }
    }
    return a;
}

//let s = [1,2,3,4,5];
//print(1,append(s,6));
//print(1,prepend(s,0));

function wait_key() {
    // specifically for "{} = wait_key()", do nothing
    return [];
}

// undocumented below here...
// none of these have been tested either...

function compare(a, b) {
    if (string(a) && string(b)) { return a.localeCompare(b); }
    if (typeof(a) === "string") { a = $charArray(a); }
    if (typeof(b) === "string") { b = $charArray(b); }
    if (Array.isArray(a)) {
        if (!Array.isArray(b)) { return 1; }
        let la = length(a),
            lb = length(b),
            ml = Math.min(la, lb);
        for (let i = 1; i <= ml; i += 1) {
            let c = compare(a[i], b[i]);
            if (c !== 0) { return c; }
        }
        return compare(la,lb);
    } else if (Array.isArray(b)) {
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
    if (Array.isArray(a) || typeof(a) === "string") {
        let la = length(a);
        if (!(Array.isArray(b) || typeof(b) === "string") || la !== length(b)) {
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
    if (typeof(a)==="boolean") { a = a ? 1 : 0; }
    if (typeof(b)==="boolean") { b = b ? 1 : 0; }
    return (a === b);
}


//DEV would the hll versions be fine???
//DOC: not strings: (nah, just fix it!)
//***NB*** these all need thorough testing for eg start vs. start-1, etc... [not that we build sequences proper like yet...]
/*
function find(needle, haystack, start = 1) {
//  if (!Array.isArray(haystack) || haystack[0] !== "sequence") {
////        if (typeof(haystack) === "string") {
////            return haystack.indexOf(x); // 0-based...
////        }
//      crash("second argument to find() must be a sequence");
//  }
    sequence(haystack,"haystack");
    let idx = haystack.indexOf(needle, start);
    if (idx === -1) { idx = 0; }
    return idx; // 1-based
}

function rfind(needle, haystack, start = -1) {
//  if (!Array.isArray(haystack) || haystack[0] !== "sequence") {
//      crash("second argument to find() must be a sequence");
//  }
    sequence(haystack,"haystack");
    // aside: haystack.length includes [0] aka +1:
    if (start < 0) { start += haystack.length; }
    let idx = haystack.lastIndexOf(needle,start);
//  if (idx === 0) { idx = -1; }    // (jic)
    if (idx === -1) { idx = 0; }
    return idx; // 1-based
}

//DOC: strings only:
function match(needle, haystack, start = 1, case_sensitive = true) {
//  if (typeof(needle) !== "string" ||
//      typeof(haystack) !== "string" ) {
//      crash('argumenst to match() must be string');
//  }
    string(needle,"haystack");
    string(haystack,"haystack");
    if (!case_sensitive) {
        needle = needle.toLowerCase();
        haystack = haystack.toLowerCase();
    }
//  let idx = haystack.indexOf(needle,start)+1;
    let idx = haystack.indexOf(needle,start);
    if (idx === -1) { idx = 0; }
    return idx; // 1-based
}
*/

function $deepCopy(src) {
  return JSON.parse(JSON.stringify(src));
}

// (equivalent to phix's infix & operator [I hope!])
//$conCat()? <==> &
function $conCat(a, b) {
    if (integer(a) && a>=0 && a<=255) {
        if (integer(b) && b>=0 && b<=255) {
            return String.fromCharCode(a) + String.fromCharCode(b);
        } else if (string(b)) {
            return String.fromCharCode(a) + b;
        } else if (!sequence(b)) {
            return ["sequence",a,b];
        }
        b = $deepCopy(b);
        b[0] = a;
        b.unshift("sequence");
        return b
    } else if (string(a)) {
    //      if (typeof(b) === "string") {
        if (string(b)) {
            return a + b;
        }
        if (integer(b) && b>=0 && b<=255) {
            return a + String.fromCharCode(b);
        }
//      a = ["sequence",...Array.from(a)];
        a = $charArray(a);
//  } else if (!Array.isArray(a)) {
    } else if (!sequence(a)) {
        a = ["sequence",a];
    }
//  if (typeof(b) === "string") {
    if (string(b)) {
//      a = a.concat(Array.from(b));
        b = $charArray(b);
    }
//  if (Array.isArray(b)) {
    if (sequence(b)) {
        // (creates a new array)
        a = a.concat(b.slice(1));
    } else {
//      a = a.concat(b);
        a.push(b);
    }
    return a;
}

//DOC note that strings of length 1 are treated as chars (NO)
//      (unlike phix, in js there is no difference between "a" and 'a')
function repeat(item, count) {
//  if (typeof(item) === "string" && item.length === 1) {
//      let res = item.repeat(count);
//      return res;
//  }
    if (integer(item) && item>=7 && item<=255) {
        return String.fromCharCode(item).repeat(count);
    }
    let res = ["sequence"];
    if (Array.isArray(item)) {
        for (let i = 1; i <= count; i += 1) {
//          res[i] = [...item];
            res[i] = $deepCopy(item);
        }
    } else {
        res.length = count+1;
        res.fill(item,1,count+1);
    }
    return res;
}

// hopefully we can rely on builtins\psqop.e (/transpile it automatcially to psqop.js)

function assert(condition, msg = "") {
    if (!condition) { crash(msg); }
}

function apply(s, fn, userdata = ["sequence"], bFunc = true) {
    // apply fn as function (bFunc===true) or procedure (bFunc===false) to all elements of sequence s
//  if (Number.isInteger(s)) {
    if (typeof(s)==="boolean") {
        let l = length(userdata),
            n = (l>0 ? 1 : 0);
        if (s === true) {
            //
            // userdata specifies multiple arguments:
            //  if userdata[i] is an atom, it is passed to every call.
            //  if userdata[i] is length(1), then userdata[i][1] "".
            //  otherwise length(userdata[i]) must match all other 
            //            non-atom/length(1) elements of it (==n).
            //  fn is then invoked n times (can be 0), and always
            //                with exactly length(userdata) args.
            //
            let args = userdata.slice(1), // (sets atoms and length)
                multi = repeat(false,l);
            for (let i = 1; i <= l; i += 1) {
                let ui = userdata[i];
                if (string(ui)) { userdata[i] = ui = $charArray(ui); }
                if (Array.isArray(ui)) {
                    let m = length(ui);
                    if (m === 1) {
                        args[i-1] = ui[1];
                    } else if (n !== 1 && m !== n) {
                        crash("invalid lengths");
                    } else {
                        n = m;
                        multi[i] = true;
                    }
                }
            }

            let res = (bFunc ? repeat(0,n) : []);
            for (let i = 1; i <= n; i += 1) {
                for (let j = 1; j <= l; j += 1) {
                    if (multi[j]) {
                        args[j-1] = userdata[j][i];
                    }
                }
                if (bFunc) {
                    res[i] = fn(...args);
//                  res[i] = fn(args);
                } else {
//DEV untested/while I try to parse this thing...
// (decided it is [probably] better to put unhandled operators into the ast...)
                    fn(...args);
//                  fn(args);
                }
            }
            return res;
        } else if (s === false) {
            let res = (bFunc ? repeat(0,l) : ["sequence"]);
            for (let i = 1; i <= l; i += 1) {
                let ui = userdata[i];
                if (typeof(ui) === "number" ||
                    typeof(ui) === "string") {
                    ui = [ui];
                }

                if (bFunc) {
                    res[i] = fn(...ui);
//                  res[i] = fn(ui);
                } else {
                    fn(...ui);
//??                fn(...ui.slice(1));
//                  fn(ui);
                }
            }
            return res;
        } else {
            crash("first atom argument to crash() must be true or false");
        }
    }

    if (string(s)) { s = $charArray(s); }
    // s is a list of single args
    let ls = length(s);
    let res = bFunc?repeat(0,ls):0;
    for (let i = 1; i <= ls; i += 1) {
        if (bFunc) {
//          s[i] = fn(s[i],userdata);
            res[i] = fn(s[i],...userdata.slice(1));
        } else {
//          fn(s[i],userdata);
            fn(s[i],...userdata.slice(1));
        }
    }
    return res;
}

function papply(s, fn, userdata = ["sequence"]) {
    apply(s, fn, userdata, false);
}

function filter(s, rs, userdata = ["sequence"], rangetype = "") {
//
// Select only those elements from a sequence that pass a specified test.
//
    let idx = 0;
    let si;
    if (typeof(rs) === "string") {
        // built-in handling
        let inout = find(rs,["sequence","in","out"]);
        if (inout !== 0) {
            inout = (inout === 1); // in: true, out: false
            if (rangetype === "") {
                // set handling
                for (let i = 1; i <= length(s); i += 1) {
                    si = s[i];
                    let f = find(si,userdata);
                    if ((f !== 0) === inout) {
                        idx += 1;
                        s[idx] = si;
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
                for (let i = 1; i <= length(s); i += 1) {
                    si = s[i];
                    let lc = compare(si,lo),
                        hc = compare(si,hi);
                    if (((lc >= xl) && (hc <= xh)) === inout) {
                        idx += 1;
                        s[idx] = si;
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
//          let ok = ["sequence",["sequence",-1],["sequence",-1,0],["sequence",0],["sequence",0,1],["sequence",1]][ct];
            let ok = ["sequence",[-1],[-1,0],[0],[0,1],[1]][ct];
            for (let i = 1; i <= length(s); i += 1) {
                si = s[i];
                let c = compare(si,userdata);
//              if ((find(c,ok) !== 0) !== ne) {
//haystack.indexOf(needle,start);
                if ((ok.indexOf(c) !== -1) !== ne) {
                    idx += 1;
                    s[idx] = si;
                }
            }
        }
        s = s.slice(0,idx+1);
        return s;
    }

    // user-defined function handling
    if (rangetype !== "") { crash("invalid rangetype"); }
    let fn = rs;
    let r = (string(s) ? "" : ["sequence"]);
    if (string(s)) { s = $charArray(s); }
    let ls = length(s),
        lu = length(userdata);
    for (let i = 1; i <= ls; i += 1) {
        si = s[i];
//      if (fn(si, i, userdata === [] ? s : userdata)) {
//      if (fn(si, i, equal(userdata,["sequence"]) ? s : userdata)) {
        if (fn(si, i, lu===0 ? s : userdata)) {
            r = append(r,si);
        }
    }
    return r;
}

let $seed = Date.now();

function rnd() {
    $seed += 1;
    let x = Math.sin($seed) * 10000;
    return x - Math.floor(x);
//? return Math.random();
}

function rand(n) {
    return ((n*rnd()) >>> 0)+1;
}

function get_rand() {
    return $seed;
}

function set_rand(new_seed) {
    $seed = new_seed;
}

/*
Math.abs(x)                 Returns the absolute value of x.
Math.acos(x)                Returns the arccosine of x.
Math.acosh(x)               Returns the hyperbolic arccosine of x.
Math.asin(x)                Returns the arcsine of x.
Math.asinh(x)               Returns the hyperbolic arcsine of a number.
//Math.atan(x)              Returns the arctangent of x.
Math.atanh(x)               Returns the hyperbolic arctangent of x.
Math.atan2(y, x)            Returns the arctangent of the quotient of its arguments.
Math.ceil(x)                Returns the smallest integer greater than or equal to x.
//Math.cos(x)               Returns the cosine of x.
Math.cosh(x)                Returns the hyperbolic cosine of x.
Math.exp(x)                 Returns Ex, where x is the argument, and E is Euler's constant (2.718…, the base of the natural logarithm).
Math.expm1(x)               Returns subtracting 1 from exp(x).
//Math.floor(x)             Returns the largest integer less than or equal to x.
Math.fround(x)              Returns the nearest single precision float representation of x.
Math.hypot([x[, y[, ...]]]) Returns the square root of the sum of squares of its arguments.
Math.imul(x, y)             Returns the result of the 32-bit integer multiplication of x and y.
//Math.log(x)               Returns the natural logarithm (㏒e; also, ㏑) of x.
Math.log1p(x)               Returns the natural logarithm (㏒e; also ㏑) of 1 + x for the number x.
Math.log10(x)               Returns the base-10 logarithm of x.
Math.log2(x)                Returns the base-2 logarithm of x.
Math.max([x[, y[, ...]]])   Returns the largest of zero or more numbers.
Math.min([x[, y[, ...]]])   Returns the smallest of zero or more numbers.
//Math.pow(x, y)            Returns base x to the exponent power y (that is, xy).
//Math.random()             Returns a pseudo-random number between 0 and 1.
Math.round(x)               Returns the value of the number x rounded to the nearest integer.
Math.sign(x)                Returns the sign of the x, indicating whether x is positive, negative, or zero.
//Math.sin(x)               Returns the sine of x.
Math.sinh(x)                Returns the hyperbolic sine of x.
//Math.sqrt(x)              Returns the positive square root of x.
//Math.tan(x)               Returns the tangent of x.
Math.tanh(x)                Returns the hyperbolic tangent of x.
Math.trunc(x)               Returns the integer portion of x, removing any fractional digits.
*/
//seems fine:
//for (let i = 1; i <= 10; i += 1) {
//  printf(1,"%v\n",rnd());
//  printf(1,"%d:%v\n",[i,rand(i)]);
//}

//DEV auto-xlate?
/*
function tagset(n) {
    let tags = repeat(0,n);
    for (let i = 1; i <= n; i += 1) { 
      tags[i] = i;
    }
    return tags;
}
*/

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
    if (len > 1 && lo < hi) {
        let mid = floor((lo+hi-1)/2);
        for (; lo <= mid; lo += 1) {
//      for (lo=lo; lo <= mid; lo += 1) {
            res[hi] = src[lo];
            res[lo] = src[hi];
            hi -= 1;
        }
    }
    return res;
}

function routine_id(rtn_name) {
    if (typeof(rtn_name)=== "function") { return rtn_name; }
    let rtn = window[rtn_name];
    if (typeof(rtn)!=="function") { return -1; }
    return rtn;
}
//function routine_id(rtn) {
//  return rtn;
//}
function call_func(rid,params) {
    if (typeof(rid)!=="function") { crash("invalid routine_id"); }
    return rid(...params.slice(1));
}
function call_proc(rid,params) {
    if (typeof(rid)!=="function") { crash("invalid routine_id"); }
    rid(...params.slice(1));
}

function xor(a,b) {
//  return ( a || b ) && !( a && b );
//  return ( a && !b ) || ( !a && b );
  return (!a != !b) ? 1 : 0;
}

function time() {
    return new Date().valueOf()/1000;
}

function mod(a,b) {
//  return a % b;
    return ((a % b ) + b ) % b;
}

function remainder(a, b) {
    return a % b;
}

function machine_bits() {
    return 32;
}

function power(a, b) {
//  return a ** b;
    return Math.pow(a,b);
}

function sqrt(a) {
    return Math.sqrt(a);
}

const WINDOWS = 2,
      LINUX = 3,
      WEB = 4,
      JS = WEB,
      JAVASCRIPT = WEB,
      PI = Math.PI,
      E = Math.E,
//    INVLN10 = 0.43429448190325182765,
      INVLN10 = 1/Math.LN10,
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

function platform() {
    return JS;
}

function $sidii(s,idii,skip=0,t) {
    // note this is only ever dealing with dword-sequence subscripts,
    //      and should never ever be asked to subscript a string.
    if (idii) { 
        let l = length(idii)-skip;
        for (let i=1; i<=l; i+=1) {
            let idx = floor(idii[i]);
            if (idx<0) { idx += length(s)+1; }
            if (string(s) || atom(s)) { crash("attempt to subscript an atom"); }
            s = s[idx];
        }
        if (skip) { s[idii[l+1]] = t; }
    }
    if (!sequence(s)) { crash("attempt to subscript an atom"); }
    return s;
}

function $subse(s, idx, idii) {
    sequence(s,"s");
    atom(idx,"idx");
    idx = floor(idx);
    s = $sidii(s,idii);
    if (idx<0) { idx += length(s)+1; }
    if (typeof(s) === "string") {
        return s.charCodeAt(idx-1);
    } //else {
    return s[idx];
}   

function $subss(s, ss, se, idii) {
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
//  return s.slice(ss-1,se);
    s = s.slice(ss-1,se+1);
    s[0] = "sequence";
    return s;
//  return $deepCopy(s.slice(ss,se+1));
}

function $repe(s, idx, x, idii) {
    // nb relies on JavaScript pass-by-sharing semantics.
    // implements s[[idii]][idx] := x
    sequence(s,"s");
    atom(idx,"idx");
    idx = floor(idx);
    let t = $sidii(s,idii);
    if (idx<0) { idx += length(t)+1; }
//  if (typeof(s) === "string") {
    if (string(t)) {
        if (integer(x) && x>=0 && x<=255) {
//
//          return s.slice() + String.fromCharCode(x) + s.slice();
//this.substr(0, index) + replacement + this.substr(index + replacement.length);
//substring
//function setCharAt(str,index,chr) {
//  if(index > str.length-1) return str;
//          return s.substring(0,idx) + String.fromCharCode(x) + s.substring(idx+1);
            t = t.substring(0,idx-1) + String.fromCharCode(x) + t.substring(idx);
            if (!idii) { return t; }
            t = $sidii(s,idii,1,t);
            return s;
        }
        t = $charArray(t);
//      /*t =*/ $sidii(s,idii,1,t);
    }
    t[idx] = x;
    if (string(s) || !idii) { return t; }
    // ooh-uh-ooh-uh (/shudder), these pass-by-sharing semantics make me right queasy...
    /*t =*/ $sidii(s,idii,1,t);
//  return s[idx];
    return s;
}

function $repss(s, ss, se, x, idii) {
    // nb relies on JavaScript pass-by-sharing semantics.
    // implements s[[idii]][ss..se] := x 
    //  (including variable length slice assignment)
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
    /*t =*/ $sidii(s,idii,1,t);
//  s.splice()
    return s;
}

function and_bits(a, b) {
    return a & b;
}

function or_bits(a, b) {
    return a | b;
}

function xor_bits(a, b) {
    return a ^ b;
}

function not_bits(a) {
    return ~a;
}

function log(a) {
    return Math.log(a);
}

function cos(a) {
    return Math.cos(a);
}

function sin(a) {
    return Math.sin(a);
}

function tan(a) {
    return Math.tan(a);
}

function arctan(a) {
    return Math.atan(a);
}

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

//sin, cos, tan, arctan, log, floor, remainder, mod?, sqrt, power, PI, and/or/xor/not_bits, 
//time, date, version, requires?, clear_screen?, dict?, timedate? progress?
//everything else in builtins/pdate.e, or move them away from the #ilASM{}-using date().
