//
// Part 1: utilities puts(), crash(), and [s]printf().
// ==================================================
//
// Aside: js allows $ in identifiers; we use that fact to effectively
//        "hide" things from p2js.exw generated code. For instance,
//        statics like $docbody and $seed, as well as local constants
//        such as $PROC and $FUNC, and also internal/private helpers 
//        such as $tce() and $_apply().
//
// Style conventions
// =================
//  Use phix_style_underscores where appropriate (ie direct mapping)
//  Use javaScriptStyleCamelCase where that seems /more/ appropriate
//  Use a very light dusting of bHungarianNotation (eg 'b' for bool)
//  Use $ident for any globals that must never, ever clash with phix
//
//  Very much a judgment call, eg $prefer_backtick suggests there is
//  something relevant in Phix.chm, whereas bAsV and zeroE are only
//  to be found in here. In practice, underscore_variables heavily
//  outnumber camelCase here, simply because the vast majority /are/
//  based on something in phix.
//
// Quickref:
// =========
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
let $docbody = document.body;   // (override if required, eg like
                                //  the result panel on codepen.)

function puts(fn, text) {
    integer(fn,"fn");
//  string(text,"text");
    if (fn !== 1 && fn !== 2) { crash("fn must be 1 or 2"); }
    const am = new RegExp("&","g"),
          lt = new RegExp("[<]","g"),
          gt = new RegExp("[>]","g"),
          sp = new RegExp("[ ][ ]","g"),
          lf = new RegExp("\\n","g");
    text = text.replace(am,"&amp;")
               .replace(lt,"&lt;")
               .replace(gt,"&gt;")
               .replace(sp,"&nbsp;&nbsp;")
               .replace(lf,"<br>");
    let where = (fn === 2 ? "afterbegin" : "beforeend");
    $docbody.insertAdjacentHTML(where, text);
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
//  $docbody.insertAdjacentHTML("afterbegin", s);
//  puts(2,s);
    puts(2,msg);
    throw(new Error(msg));
}

function $tce(name, x) {
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
        return $tce(name,i);
    }
    return true;
}

function atom(a, name = "") {
//  if (typeof(a) !== "number") {
    if (!isFinite(a)) {
        return $tce(name,a);
    }
    return true;
}

function string(s, name = "") {
    if (typeof(s) !== "string") {
        return $tce(name,s);
    }
    return true;
}

function sequence(p, name = "") {
    if (!Array.isArray(p) || p[0] !== "sequence") {
        return $tce(name,p);
    }
    return true;
}

function object(o, name = "") {
    if (typeof(o) === "undefined") {
//      return $tce(name,o);
        return $tce(name,"undefined");
//      return false;
    }
    return true;
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
    if (Array.isArray(args) && args.length>0 && args[0] === "sequence") { i = 0; }

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
                res = JSON.stringify(res);                  // (ie/for "%v")
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
                crash('Invalid (string expected)');
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
    function flti() {
        let res = argi();
        if (typeof(res) !== "number") {
            crash('Invalid (number expected)');
        }
        return res;
    }
//  function eorf(precision: number): number {  // (ie "%g") // NO!
//  function eorf(precision/*: number*/)/*: string*/ {  // (ie "%g")
    function eorf(/* integer */ precision) {    // (ie "%g")
//  function eorf(precision) {  // (ie "%g")

        let eres = flti().toExponential(precision);
        const zeroE = new RegExp("([.][0]+e|[0]+e)");
        const Ezero = new RegExp("(e\\+0$)");
        eres = eres.replace(zeroE,"e");             // eg "1.000000e14" -> "1e14"
        eres = eres.replace(Ezero,"");              // eg        "1e+0" -> "1"

        let fres = flti().toFixed(precision);
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
        // specifier is eg 's' or one of "tcvdboxXeEfgG%"
        //
        if (expr === '%%') { return '%'; }  // (aka specifier === '%')
        i += 1;
        precision = precision ? parseInt(precision) : dot ? 0 : 6;
        let res = "";
        switch (specifier) {
            case 's': res = stri(); break;
            case 'q': res = allascii(stri(),specifier); break;
            case 'Q': res = allascii(stri(),specifier); break;
            case 't': res = argi()?"true":"false"; break;
            case 'c': res = String.fromCharCode(argi()); break;
            case 'v': res = stri(true); break;
            case 'd': res = inti().toString(10); break;
            case 'b': res = inti().toString(2); break;
            case 'o': res = inti().toString(8); break;
            case 'x': res = inti(true).toString(16).toUpperCase(); break;
            case 'X': res = inti(true).toString(16); break; //^yep===Phix
            case 'e': res = flti().toExponential(precision); break;
            case 'E': res = flti().toExponential(precision).toUpperCase(); break;
            case 'f': res = flti().toFixed(precision); break;
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
            let padlen = parseInt(size)-res.length;
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
    const regex = new RegExp("%([-+=|,])?(0?[0-9]+)?([.]([0-9]+)?)?([sqQtcvdboxXeEfgG%])",'g');
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
function sprint(o) {
//  object(o,"o");
    let res = o.toString();
    if (Array.isArray(o)) {
        res = "{" + res + "}";
//      res = "{" + res.splice(1).toString + "}";
    }
    return res;
}

function print(fn, o) {
//  integer(fn,"fn");
//  object(o,"o");
    puts(fn,sprint(o) + "\n");
}

//DEV why not just use builtins/pfactors.e, transpiled?
function factors(num, include1 = 0) {
//  sequence(num,"num");
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
    if (typeof(a) === "string") {
        a += x;
    } else {
        if (!Array.isArray(a)) {
            a = [a];
        }
        a.push(x);
    }
//  a.push(newElement); === a = append(a,newElement);
    return a;
}

function prepend(a, x) {
//  object(a,"a");
//  object(x,"x");
    if (typeof(a) !== "string") {
        a = x + a;
    } else {
        if (!Array.isArray(a)) {
            a = [a];
        }
        a = [x].concat(a);
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

function length(o) {
    if (sequence(o)) {
        return o.length-1;
    } else if (typeof(o) === "string") {
        return o.length;
    }
    crash("length of an atom is not defined");
}

function compare(a, b) {
// crikeybobs... (mind you, this is not unlike what the desktop #ilASM{} gets up to...)
    if (Array.isArray(a)) {
        if (typeof(b) === "string") {
//          b = Array.from(b);
            b = Array.from('x'+b);
            b[0] = "sequence";
        }
        if (Array.isArray(b)) {
            let la = length(a),
                lb = length(b);
            for (let i = 1; i <= Math.min(la, lb); i += 1) {
                let c = compare(a[i], b[i]);
                if (c !== 0) { return c; }
            }
            if (la < lb) {
                return -1;
            } else if (la === lb) {
                return 0;
            } else {
                return 1;
            }
        }
        return -1;
    }
    if (typeof(a) === "string") {
        if (Array.isArray(b)) {
//          return compare(Array.from(a), b);
            a = Array.from('x'+a);
            a[0] = "sequence";
            return compare(a, b);
        }
        if (typeof(b) === "string") {
            return a.localeCompare(b);
        }
        return 1;
    }
    if (typeof(a) !== "number" &&
        typeof(a) !== "boolean") {
        crash("compare() failure");
    }
    if (typeof(b) !== "number" &&
        typeof(b) !== "boolean") {
        return -1;
    }
    if (a < b) {
        return -1;
    } else if (a === b) {
        return 0;
    } else {
        return 1;
    }
//  crash("compare not implemented!");
//  return 0;
}

function equal(a, b) {
// crikeybobs... (mind you, this is not unlike what the desktop #ilASM{} gets up to...)
    if (Array.isArray(a)) {
        if (typeof(b) === "string") {
//          b = Array.from(b);
            b = Array.from('x'+b);
            b[0] = "sequence";
        }
        if (Array.isArray(b)) {
            let la = length(a);
            if (la !== length(b)) { return false; }
            for (let i = 1; i <= la; i += 1) {
                let c = equal(a[i], b[i]);
                if (!c) { return c; }
            }
            return true;
        }
        return false;
    }
    if (typeof(a) === "string") {
        if (Array.isArray(b)) {
//          return equal(Array.from(a), b);
            a = Array.from('a'+a);
            a[0] = "sequence";
            return equal(a, b);
        }
        if (typeof(b) === "string") {
            return (a === b);
        }
        return false;
    }
    if (typeof(a) !== "number" &&
        typeof(a) !== "boolean") {
        crash("equal() failure");
    }
    if (typeof(b) !== "number" &&
        typeof(b) !== "boolean") {
        return false;
    }
    return (a === b);
}


//DEV would the hll versions be fine???
//DOC: not strings: (nah, just fix it!)
//***NB*** these all need thorough testing for eg start vs. start-1, etc... [not that we build sequences proper like yet...]
function find(needle, haystack, start = 1) {
    if (!Array.isArray(haystack) || haystack[0] !== "sequence") {
//      if (typeof(haystack) === "string") {
//          return haystack.indexOf(x); // 0-based...
//      }
        crash("second argument to find() must be a sequence");
    }
    let idx = haystack.indexOf(needle, start);
    if (idx === -1) { idx = 0; }
    return idx; // 1-based
}

function rfind(needle, haystack, start = -1) {
    if (!Array.isArray(haystack) || haystack[0] !== "sequence") {
        crash("second argument to find() must be a sequence");
    }
    // aside: haystack.length includes [0] aka +1:
    if (start < 0) { start += haystack.length; }
    let idx = haystack.lastIndexOf(needle,start);
//  if (idx === 0) { idx = -1; }    // (jic)
    if (idx === -1) { idx = 0; }
    return idx; // 1-based
}

//DOC: strings only:
function match(needle, haystack, start = 1, case_sensitive = true) {
    if (typeof(needle) !== "string" ||
        typeof(haystack) !== "string" ) {
        crash('argumenst to match() must be string');
    }
    if (!case_sensitive) {
        needle = needle.toLowerCase();
        haystack = haystack.toLowerCase();
    }
//  let idx = haystack.indexOf(needle,start)+1;
    let idx = haystack.indexOf(needle,start);
    if (idx === -1) { idx = 0; }
    return idx; // 1-based
}

// (equivalent to phix's infix & operator [I hope!])
//$_concat()? <==> &
function $_concat(a, b) {
//DEV we may need to perform deep a copy here...
//Hmmm...
    if (typeof(a) === "string") {
        a += b;
    } else {
        if (!Array.isArray(a)) {
            a = ["sequence",a];
        }
//[DEV] this may be wrong if b is a string... [added Hmmm... above, untested]
//DEV wrong if b[0] is "sequence"... [fixed but not tested]
        if (Array.isArray(b)) {
            a = a.concat(b.slice(1));
        } else {
//?         a = [a].concat(b);
            a = a.concat(b);
        }
    }
    return a;
}

//DOC note that strings of length 1 are treated as chars
//      (unlike phix, in js there is no difference between "a" and 'a')
function repeat(item, count) {
    if (typeof(item) === "string" && item.length === 1) {
        let res = item.repeat(count);
        return res;
    }
    let res = ["sequence"];
    if (Array.isArray(item)) {
        for (let i = 1; i <= count; i += 1) {
            res[i] = [...item];
        }
    } else {
        res.length = count+1;
        res.fill(item,1,count);
    }
    return res;
}

// hopefully we can rely on builtins\psqop.e (/transpile it automatcially to psqop.js)

function assert(condition, msg = "") {
    if (!condition) { crash(msg); }
}

const $PROC = 1, $FUNC = 2;

function $_apply(s, fn, pf, userdata) {
    // apply fn as $PROC or $FUNC to all elements of sequence s
    if (Number.isInteger(s)) {
        let l = userdata.length,
            n = (l>0);
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
            let args = userdata, // (sets atoms and length)
                multi = repeat(false,l);
            for (let i = 0; i < l; i += 1) {
                let ui = userdata[i];
                if (Array.isArray(ui)) {
                    let m = ui.length;
                    if (m === 1) {
                        args[i] = ui[0];
                    } else if (n !== 1 && m !== n) {
                        crash("invalid lengths");
                    } else {
                        n = m;
                        multi[i] = true;
                    }
                }
            }

            let res = (pf === $FUNC ? repeat(0,n) : []);
            for (let i = 0; i < n; i += 1) {
                for (let j = 1; j < l; j += 1) {
                    if (multi[j]) {
                        args[j] = userdata[j][i];
                    }
                }
                if (pf === $PROC) {
//DEV untested/while I try to parse this thing...
// (decided it is [probably] better to put unhandled operators into the ast...)
                    fn(...args);
//                  fn(args);
                } else {
                    res[i] = fn(...args);
//                  res[i] = fn(args);
                }
            }
            return res;
        } else if (s === false) {
            let res = (pf === $FUNC ? repeat(0,l) : []);
            for (let i = 0; i < l; i += 1) {
                let ui = userdata[i];
                if (typeof(ui) === "number" ||
                    typeof(ui) === "string") {
                    ui = [ui];
                }

                if (pf === $PROC) {
                    fn(...ui);
//                  fn(ui);
                } else {
                    res[i] = fn(...ui);
//                  res[i] = fn(ui);
                }
            }
            return res;
        } else {
            crash("first atom argument to crash() must be true or false");
        }
    }

    // s is a list of single args
    for (let i = 1; i <= length(s); i += 1) {
        if (pf === $PROC) {
            fn(s[i],userdata);
        } else {
            s[i] = fn(s[i],userdata);
        }
    }
    return s;
}

function apply(s, fn, userdata = []) {
    return $_apply(s, fn, $FUNC, userdata);
}

function papply(s, fn, userdata = []) {
    $_apply(s, fn, $PROC, userdata);
}

function filter(s, rs, userdata = [], rangetype = "") {
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
                if (!sequence(userdata) ||
                    length(userdata) !== 2) {
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
    let r = (typeof(s) === "string" ? "" : ["sequence"]);
    for (let i = 0; i < s.length; i += 1) {
        si = s[i];
        if (fn(si, i, userdata === [] ? s : userdata)) {
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

//seems fine:
//for (let i = 1; i <= 10; i += 1) {
//  printf(1,"%v\n",rnd());
//  printf(1,"%d:%v\n",[i,rand(i)]);
//}

//DEV auto-xlate?
function tagset(n) {
    let tags = repeat(0,n);
    for (let i = 1; i <= n; i += 1) { 
      tags[i] = i;
    }
    return tags;
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

function $deepcopy(src) {
  return JSON.parse(JSON.stringify(src));
}

function routine_id(rtn_name) {
    let rtn = window[rtn_name];
    if (typeof(fn)!=="function") { return -1; }
    return rtn;
}
function call_func(rid,params) {
    if (typeof(rid)!=="function") { crash("invalid routine_id"); }
    return rid(...params.slice(1));
}
function call_proc(rid,params) {
    if (typeof(rid)!=="function") { crash("invalid routine_id"); }
    rid(...params.slice(1));
}


/*
From Douglas Crockford (or just transpile builtins/sort.e)
var m = ['aa', 'bb', 'a', 4, 8, 15, 16, 23, 42];
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
