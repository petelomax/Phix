//
// pwa\builtins\mpfr.js
// ====================
//
//  Implementation of / hand crafted replacement for mpfr.e for pwa/p2js
//
//  Uses JavaScript BigInts and keeps floats as rationals.
//  Credit to various libs and SO entries, far too many to mention, but especially
//  https://jrsinclair.com/articles/2020/sick-of-the-jokes-write-your-own-arbitrary-precision-javascript-math-library/
//
//  Note that in desktop/Phix mpz/mpfr/mpq are atom, however here they are
//  of JavaScript Array type, but not sequence(), with eg "mpz" where a
//  sequence would normally have "sequence" (aka be properly tagged).
//  Several standard builtins in p2js.js (eg printf) may/will probably 
//  simply crash if any of these are accidentally thrown at them, as per
//  the usual "get it working on desktop/Phix before trying p2js" mantra.
//
// MPFR$XXX - various "private" constants and routines in builtins\mpfr.js
// pwa/p2js: Work on an mpfr.js replacement for mpfr.e is in progress.
//           Some of the notes below may give the false impression it is
//           already finished and available, but it is not.
// note that bitcount is ignored under pwa/p2js.
//
const MPFR$T = 0,   // as in ["mpfr",bigint,bigint,int,int,int]
      MPFR$N = 1,   // numerator (a BigInt)
      MPFR$D = 2,   // denominator (a bigInt)
      MPFR$E = 3,   // exponent (integer), aka implied 10^e denominator [???]
      MPFR$R = 4,   // rounding mode (eg MPFR_RNDN) [???]
      MPFR$P = 5,   // precision (-ve integer in decimals)

      MPQ$N = 1,    // as in ["mpq",bigint,bigint]
      MPQ$D = 2,

      MPZ$B = 1;    // as in ["mpz",bigint]

const MPFR_RNDN = 0,    // round to nearest
      MPFR_RNDZ = 1,    // round toward zero
      MPFR_RNDU = 2,    // round toward +infinity
      MPFR_RNDD = 3,    // round toward -infinity
      MPFR_RNDA = 4;    // round away from zero
/*
global constant MPFR_RNDN = 0,  -- round to nearest
                MPFR_RNDZ = 1,  -- round toward zero
                MPFR_RNDU = 2,  -- round toward +infinity
                MPFR_RNDD = 3,  -- round toward -infinity
                MPFR_RNDA = 4,  -- round away from zero
--              MPFR_RNDF = 5?  -- faithful rounding. (experimental?)
                GMP_RNDN = MPFR_RNDN,
                GMP_RNDZ = MPFR_RNDZ,
                GMP_RNDU = MPFR_RNDU,
                GMP_RNDD = MPFR_RNDD,
                GMP_RNDA = MPFR_RNDA
*/

function mpfr(v) {
    return Array.isArray(v)
        && (v.length === 6)
        && (v[MPFR$T] === "mpfr")
        && (typeof(v[MPFR$N]) === "bigint")
        && (typeof(v[MPFR$D]) === "bigint")
        && integer(v[MPFR$E])
        && ([MPFR_RNDN].indexOf(v[MPFR$R]) !== -1) // (more to be added as supported)
        && integer(v[MPFR$P])
        && (v[MPFR$P] < 0);
} mpfr.$sig="TO";

function mpq(v) {
    return Array.isArray(v)
        && (v.length === 3)
        && (v[MPFR$T] === "mpq")
        && (typeof(v[MPQ$N]) === "bigint")
        && (typeof(v[MPQ$D]) === "bigint");
} mpq.$sig="TO";

function mpz(v) {
    return Array.isArray(v)
        && (v.length === 2)
        && (v[MPFR$T] === "mpz")
        && (typeof(v[MPZ$B]) === "bigint");
} mpz.$sig="TO";

function mpir_open_dll() {
    return "";
} mpir_open_dll.$sig="FSO,0";

function mpir_get_versions() {
    return ["sequence","JavaScript","mpfr.js","n/a"];
} mpir_get_versions.$sig="FO,0";

function MPFR$replace_e(/*string*/ s) {
// allow strings such as "1e200" or even "2.5e1" (as long as integer overall)
    s = s.toLowerCase();
    let /*integer*/ e = find(0X65,s);
    if (e) {
        e = parseInt($subss(s,e+1,-1));
        s = $subss(s,1,e-1);
        let /*integer*/ d = find(0X2E,s);
        if (d) {
            e -= length(s)-d;
            s = $repss(s,d,d,"");
        }
        s = $conCat(s, repeat(0X30,e), false); // (-ve e is expected to crash)
    }
    return s;
}

function mpz_init(v=0) {
    // v can be integer/atom/string/mpz
    if (string(v)) {
        const us = new RegExp("_","g");
        v = v.replace(us,"");
        v = MPFR$replace_e(v);
    } else if (mpz(v)) {
        v = v[MPZ$B];
    }
    return ["mpz",BigInt(v)];
} mpz_init.$sig="FOI,0";
let mpz_init_set = mpz_init;

function mpz_inits(n, v=0) {
    let res = repeat(0,n);
    if (sequence(v) && !string(v)) {
        if (length(v)!=n) { crash("?9/0"); }
        for (let i = 1; i <= n; i += 1) {
            res[i] = mpz_init(v[i]);
        }
    } else {
        for (let i = 1; i <= n; i += 1) {
            res[i] = mpz_init(v);
        }
    }
    return res;
} mpz_inits.$sig="FIO,1";

function mpz_free(x) {
    if (sequence(x)) {
        let l = length(x);
        for (let i = 1; i <= l; i += 1) {
            x[i] = NULL;
        }
    } else {
        x = NULL;
    }
    return x;
} mpz_free.$sig="FO";

function MPFR$COMMAFILL(res) {
    let showcommas = res.length,
        lim = "-+".indexOf(res[0]) === -1 ? 3 : 4,
        dot = res.indexOf('.');
//20/8/22!!
//  if (dot) { showcommas = dot-1; }
//09/10/22!!!!!
//  if (dot !== -1) { showcommas = dot-1; }
    if (dot !== -1) { showcommas = dot; }
    while (showcommas>lim) {
        showcommas -= 3;
        res = res.slice(0,showcommas) + ',' + res.slice(showcommas);
    }
    return res;
}

function mpz_get_str(x, base=10, comma_fill=false) {
    let res = x[MPZ$B].toString(base);
    if (comma_fill) { res = MPFR$COMMAFILL(res); }
    return res;
} mpz_get_str.$sig="FOIO,1";

// (transiled and copied in by hand)
function mpz_get_short_str(/*mpz*/ op, /*integer*/ ml=20, base=10, /*boolean*/ comma_fill=false, /*string*/ what="digits") {
// equivalent to shorten(mpz_get_str(op,base,comma_fill),ml:=ml) but much faster, since it does not
// construct potentially hundreds of thousands of the middle digits before just throwing them away.
    let /*bool*/ neg = compare(mpz_cmp_si(op,0),0)<0;
    if (neg) { mpz_abs(op,op); }      // (don't worry that's very fast)
    let /*integer*/ l = mpz_sizeinbase(op,base);
    let /*string*/ ls = sprintf(" (%,d %s)",["sequence",l,what]), res;
    if (compare(l,(ml*2+3)+length(ls))>0) {
        let /*mpz*/ [,tmp,p10] = mpz_inits(2);
        mpz_ui_pow_ui(p10,10,ml);
        mpz_fdiv_r(tmp,op,p10);
        // get rightmost ml digits [plus any commas]
        let /*string*/ rml = mpz_get_str(tmp,base,comma_fill);
        let /*integer*/ lr = length(rml);
        while (lr<ml) {
            rml = $conCat((((lr<3 || !comma_fill) || (lr>=4 && (!equal($subse(rml,4),0X2C)))) ? 0X30 : 0X2C), rml);
            lr += 1;
        }
        // for comma_fill, say ml=4, get the "123,456" head, 
        //  not "123,4", so commas end up logically correct.
        let /*integer*/ ll = (l-ml)-((comma_fill) ? remainder(l-ml,3) : 0);
        mpz_ui_pow_ui(p10,10,ll);
        mpz_fdiv_q(tmp,op,p10);
        // get leftmost ml [+0..2] digits [ditto]
        res = mpz_get_str(tmp,base,comma_fill);
        if (comma_fill) {
            res = $subss(res,1,ml-neg);
            rml = $subss(rml,-ml,-1);
        }
        res = $conCat(res, $conCat($conCat("...", rml), ls), false);
    } else {
        res = mpz_get_str(op,base,comma_fill);
    }
    if (neg) { res = $conCat("-", res); mpz_neg(op,op); } // (ditto)
    return res;
} mpz_get_short_str.$sig="FOIIOS,1";

function mpz_add(rop, op1, op2) {
    rop[MPZ$B] = op1[MPZ$B] + op2[MPZ$B];
} mpz_add.$sig="POOO";

function mpz_add_ui(rop, op1, op2) {
    rop[MPZ$B] = op1[MPZ$B] + BigInt(op2);
} mpz_add_ui.$sig="POOI";

let mpz_add_si = mpz_add_ui;

function mpz_sub(rop, op1, op2) {
    rop[MPZ$B] = op1[MPZ$B] - op2[MPZ$B];
} mpz_sub.$sig="POOO";

function mpz_sub_ui(rop, op1, op2) {
    rop[MPZ$B] = op1[MPZ$B] - BigInt(op2);
} mpz_sub_ui.$sig="POOI";

let mpz_sub_si = mpz_sub_ui

function mpz_si_sub(rop, op1, op2) {
    rop[MPZ$B] = BigInt(op1) - op2[MPZ$B];
} mpz_si_sub.$sig="PNIO";

function mpz_mul(rop, op1, op2) {
    rop[MPZ$B] = op1[MPZ$B] * op2[MPZ$B];
} mpz_mul.$sig="POOO";

function mpz_mul_si(rop, op1, op2) {
    rop[MPZ$B] = op1[MPZ$B] * BigInt(op2);
} mpz_mul_si.$sig="POOI";

let mpz_mul_d = mpz_mul_si;

function mpz_mul_2exp(/*mpz*/ rop, op1, /*integer*/ op2) {
    rop[MPZ$B] = op1[MPZ$B] << BigInt(op2);
} mpz_mul_2exp.$sig="POOI";

function mpz_addmul(/*mpz*/ rop, op1, op2) {
    rop[MPZ$B] = rop[MPZ$B] + op1[MPZ$B] * op2[MPZ$B];
} mpz_addmul.$sig="POOO";
    
function mpz_addmul_ui(/*mpz*/ rop, op1, /*integer*/ op2) {
    rop[MPZ$B] = rop[MPZ$B] + op1[MPZ$B] * BigInt(op2);
} mpz_addmul_ui.$sig="POOI";

let mpz_addmul_si = mpz_addmul_ui;

function mpz_cmp(op1, op2) {
    op1 = op1[MPZ$B];
    op2 = op2[MPZ$B];
    if (op1 < op2) {
        return -1;
    } else if (op1 === op2) {
        return 0;
    } else {
        return 1;
    }
} mpz_cmp.$sig="FOO";

function mpz_cmp_si(op1, op2) {
    op1 = op1[MPZ$B];
//  op2 = BigInt(op2);
    if (op1 < op2) {
        return -1;
//  } else if (op1 === op2) {
    } else if (op1 == op2) {
        return 0;
    } else {
        return 1;
    }
} mpz_cmp_si.$sig="FOI";

function mpz_set(op1, op2) {
    op1[MPZ$B] = op2[MPZ$B];
} mpz_set.$sig="POO";

function mpz_set_si(rop, op) {
    rop[MPZ$B] = BigInt(op);
} mpz_set_si.$sig="POI";

let mpz_set_d = mpz_set_si;

function mpz_set_q(rop, op) {
    rop[MPZ$B] = op[MPQ$N] / op[MPQ$D];
//  rop[MPZ$B] = op[MPQ$D] / op[MPQ$N];
} mpz_set_q.$sig="POO";

//function mpz_set_ui(op1, op2) {
//  op1[MPZ$B] = BigInt(op2);
//}

function MPFR$PREFIX(s, base) {
    // 12/3/22: allow strings such as "1e200" or even "2.5e1" (as long as integer overall)
    if (base <= 10) {
//      s = lower(substitute(substitute(s,",",""),"_",""));
        s = substitute(substitute(s,",",""),"_","");
        s = s.toLowerCase();
        let /*integer*/ e = find(0X65,s); // 'e'
//3/5/22:
//      if (e) {
        if (e && s[0]!='#' && (s[0]!='0' || "xX(".indexOf(s[1])===-1)) {
//          [,s,e] = ["sequence",$subss(s,1,e-1),to_number($subss(s,e+1,-1))];
            let n = to_number($subss(s,e+1,-1));
            s = $subss(s,1,e-1);
            let /*integer*/ d = find(0X2E,s); // '.'
            if (d) {
                n -= length(s)-d;
                s = $repss(s,d,d,"");
            }
            s = $conCat(s, repeat(0X30,n), false); // (-ve e is expected to crash)
        }
    }
    // put "0b"/"0o"/"0x" at the start of string s if needed
    let neg = (s[0] === '-');
    if (neg) { s = s.slice(1); }
    if (base !== 0 && base !== 10) {
        let bdx = [2,8,16].indexOf(base);
//      if (bdx == -1) { crash("unsupported base"); }
        if (bdx != -1) {
            let prefix = "box"[bdx];
            s = '0' + prefix + s;
        } else {
            let z = 0n, bn = BigInt(base?base:10);
            for (let i=1, i$lim=length(s); i<=i$lim; i+=1) {
                let /*integer*/ c = $subse(s,i);
                c -= ((c<=0X39) ? 0X30 : ((c<=0X5A) ? 0X41-10 : 0X61-36));
                z = z*bn + BigInt(c);
            }
            if (neg) { z = -z; }
            return z;
        }
    }
    let res = BigInt(s);
    if (neg) { res = -res; }
    return res;
}

function mpz_set_str(rop, s, base=0) {
//DEV spotted in passing 6/11/21 - what about replace_e()?
    rop[MPZ$B] = MPFR$PREFIX(s,base);
} mpz_set_str.$sig="POSI,2";

function mpz_set_v(rop, v) {
    if (string(v)) {
        mpz_set_str(rop,v);
    } else {
        mpz_set_si(rop, v);
    }
} mpz_set_v.$sig="POO";

function mpz_odd(op1) {
    let bit = BigInt.asUintN(1,op1[MPZ$B]);
//  let bit = BigInt.asIntN(1,op1[MPZ$B]);
    // note that bit is a BigInt, so == not ===: 
    return bit == 1;
} mpz_odd.$sig="FO";

function mpz_even(op1) {
    let bit = BigInt.asUintN(1,op1[MPZ$B]);
//  let bit = BigInt.asIntN(1,op1[MPZ$B]);
    // ditto
    return bit == 0;
} mpz_even.$sig="FO";

function mpz_neg(/*mpz*/ rop, op) {
    rop[MPZ$B] = -op[MPZ$B];
} mpz_neg.$sig="POO";

function mpz_abs(/*mpz*/ rop, op) {
    let n = op[MPZ$B];
    if (n < 0n) { n = -n; }
    rop[MPZ$B] = n;
} mpz_abs.$sig="POO";

function mpz_pow_ui(rop, base, exponent) {
    base = (typeof base === 'number')? BigInt(base) : base[MPZ$B];
    exponent = (typeof exponent === 'number')? BigInt(exponent) : exponent[MPZ$B];
    rop[MPZ$B] = base**exponent;
} mpz_pow_ui.$sig="POOI";

let mpz_ui_pow_ui = mpz_pow_ui;

function mpz_fdiv_q(q, n, d) {
//mpz_fdiv_q(mpz q, n, d) - q := floor(n/d)
    d = d[MPZ$B];
    n = n[MPZ$B];
    q[MPZ$B] = n / d;
} mpz_fdiv_q.$sig="POOO";

let mpz_divexact = mpz_fdiv_q;

function mpz_fdiv_q_ui(q, n, d) {
//integer res = mpz_fdiv_q_ui(mpz q, n, integer d) - {q,res} := {floor(n/d),remainder(n,d)} 
    d = BigInt(d);
    n = n[MPZ$B];
    q[MPZ$B] = n / d;
//7/11/21:
//  return Number(n % d);
    n = n % d;
    if (n < 0n) { n += d; }
    return Number(n);
} mpz_fdiv_q_ui.$sig="FOOI";

let mpz_divexact_ui = mpz_fdiv_q_ui;

function mpz_fdiv_r(r, n, d) {
//mpz_fdiv_r(mpz r, n, d) - r := remainder(n,d)
    d = d[MPZ$B];
    n = n[MPZ$B];
//7/11/21:
//  r[MPZ$B] = n % d;
    n = n % d;
    if (n < 0n) { n += d; }
    r[MPZ$B] = n;
} mpz_fdiv_r.$sig="POOO";

function mpz_fdiv_ui(n, d) {
//integer res = mpz_fdiv_ui(mpz n, integer d) - returns mod(n,d) - n and d remain unaltered, d is a phix integer, 0..1GB.  
//7/11/21:
//  return Number(n[MPZ$B] % BigInt(d));
    n = n[MPZ$B];
    d = BigInt(d)
    n = n % d;
    if (n < 0n) { n += d; }
    return Number(n);
} mpz_fdiv_ui.$sig="FOI";

function mpz_mod(r, n, d) {
// mpz_mod(mpz r, n, d) - r := mod(n,d)
    n = n[MPZ$B];
    d = d[MPZ$B];
//  let s = n < 0;
//  if (s) { n = -n; }
//  n = ((n % d) + n) % d;
    n = n % d;
    if (n < 0n) { n += d; }
/*
//Phix:
    if equal(sign(x), sign(y)) then
        return remainder(x,y)
    end if
    return x - y * floor(x / y)
    return ((a % b ) + b ) % b;
*/
//  if (s) { n = -n; }
    r[MPZ$B] = n;
} mpz_mod.$sig="POOO";

function mpz_mod_ui(r, n, d) {
// mpz_mod_ui(mpz r, n, integer d) - "" except op2 is a phix integer, 0..1GB
//7/11/21:
//  r[MPZ$B] = n[MPZ$B] % BigInt(d);
    n = n[MPZ$B];
    d = BigInt(d);
    n = n % d;
    if (n < 0n) { n += d; }
    r[MPZ$B] = n;
} mpz_mod_ui.$sig="FOOI";

function mpz_fdiv_qr(/*mpz*/ q, r, n, d) {
// {q,r} := {floor(n/d),remainder(n,d)}
// {q,r} := {floor(n/d),mod(n,d)}       // dev?? (might be more accurate, update docs and everywhere else...)
    mpz_fdiv_q(q, n, d);
    mpz_mod(r, n, d);
} mpz_fdiv_qr.$sig="POOOO";

function mpz_fdiv_q_2exp(q, n, b) {
//mpz_fdiv_q_2exp(mpz q, n, integer b) - bitwise right shift, arithmetic if n -ve.
    q[MPZ$B] = n[MPZ$B] >> BigInt(b);
} mpz_fdiv_q_2exp.$sig="POOI";

function mpz_tdiv_q_2exp(q, n, b) {
//mpz_tdiv_q_2exp(mpz q, n, integer b) - q := trunc(n/2^b), rounds q towards zero
    n = n[MPZ$B];
    let s = n < 0n;
    if (s) { n = -n; }
    n = n >> BigInt(b);
    if (s) { n = -n; }
    q[MPZ$B] = n;
} mpz_tdiv_q_2exp.$sig="POOI";

function mpz_tdiv_r_2exp(r, n, b) {
//mpz_tdiv_r_2exp(mpz r, n, integer b) - r := remainder(n,2^b), r will have the same sign as n
    n = n[MPZ$B];
    let s = n < 0n;
    if (s) { n = -n; }
//  n = BigInt.asIntN(b, n);
    n = BigInt.asUintN(b, n);
    if (s) { n = -n; }
    r[MPZ$B] = n;
} mpz_tdiv_r_2exp.$sig="POOI";

function mpz_cdiv_q(/*mpz*/ q, n, d) {
//mpz_cdiv_q(mpz q, n, d) - q := ceil(n/d)
    n = n[MPZ$B];
    d = d[MPZ$B];
//  let s = n > 0n;
//  if (s) { n = -n; }
    n += d - 1n;
    n /= d;
//  if (s) { n = -n; }
    q[MPZ$B] = n;
} mpz_cdiv_q.$sig="POOO";

function mpz_powm(/*mpz*/ rop, base, exponent, modulus) {
    // Set rop to mod(base^exponent,modulus)
//  if (rop[MPZ$B] === base[MPZ$B]) {
    if (rop === base) {
        // (ensure "rop:=x" does not clobber base)
        base = mpz_init_set(base);
    }
    if (rop === modulus) {
        // (ensure "rop:=x" does not clobber modulus)
        modulus = mpz_init_set(modulus);
    }
//  if (equal(mpz_cmp_si(exponent,1),0)) {
    if (exponent[MPZ$B] === 1n) {
        mpz_set(rop,base);
    } else {
        exponent = mpz_init_set(exponent); // (use a copy)
        let /*bool*/ odd = mpz_odd(exponent);
        if (odd) {
            mpz_sub_ui(exponent,exponent,1);
        }
        mpz_fdiv_q_2exp(exponent,exponent,1); // (exponent/=2)
        mpz_powm(rop,base,exponent,modulus);
        exponent = mpz_free(exponent);
        mpz_mul(rop,rop,rop);
        if (odd) {
            mpz_mul(rop,rop,base);
        }
    }
    mpz_mod(rop,rop,modulus);
} mpz_powm.$sig="POOOO";

function mpz_powm_ui(/*mpz*/ rop, base, /*integer*/ exponent, /*mpz*/ modulus) {
    // Set rop to mod(base^exponent,modulus)
//  if (rop[MPZ$B] === base[MPZ$B]) {
    if (rop === base) {
        // (ensure "rop:=x" does not clobber base)
        base = mpz_init_set(base);
    }
    if (rop === modulus) {
        // (ensure "rop:=x" does not clobber modulus)
        modulus = mpz_init_set(modulus);
    }
    if (exponent===1) {
        mpz_set(rop,base);
    } else {
        let /*bool*/ odd = exponent & 1;
        if (odd) {
            exponent -= 1;
        }
        mpz_powm_ui(rop,base,exponent/2,modulus);
        mpz_mul(rop,rop,rop);
        if (odd) {
            mpz_mul(rop,rop,base);
        }
    }
    mpz_mod(rop,rop,modulus);
} mpz_powm_ui.$sig="POOIO";

// mpz_abs(mpz rop, op) - rop := abs(op)    
//  mpz_neg(mpz rop, op) - rop := -op  
//  mpz_mul(mpz rop, op1, op2) - rop := op1 * op2
//   mpz_mul_si(mpz rop, op1, integer op2) - "" except op2 is a phix integer, -1GB..+1GB
//   mpz_mul_d(mpz rop, op1, atom op2) - (an extra) as mpz_mul_si() except op2 is a phix atom 

// DEV/SUG: Can we do a "binary chop" here?
//          Maintain table(s) of base^(2^k), binary_search/extend it, then repeated subtraction?
//          (experiment in Phix, checking performance, then transpile it, of course)
function mpz_sizeinbase(op, base=2) {
/*
    let digits = 1,
        a = op[MPZ$B],
        bn = BigInt(base),
        nein = BigInt(base-1);
    while (a > nein) {
        digits += 1;
        if (base === 2) {
            a >>= 1n; // (marginally faster)
        } else {
            a /= bn;
        }
    }
    return digits;
*/
//  let res = x[MPZ$B].toString(base).length;
//  return op[MPZ$B].toString(base).length;
    let n = op[MPZ$B];
    if (n < 0n) { n = -n; }
    return n.toString(base).length;
} mpz_sizeinbase.$sig="FOI";

function mpz_tstbit(/*mpz*/ op, /*integer*/ bit_index) {
//integer res = mpz_tstbit(mpz op, integer bit_index) - Test bit bit_index in op and return 0 or 1 accordingly.  
    let bit = 1n << BigInt(bit_index);
    op = op[MPZ$B] & bit;
    return (op === 0n) ? 0 : 1;
} mpz_tstbit.$sig="FOI";

function mpz_scan0(/*mpz*/ op, /*integer*/ starting_bit) {
//integer res = mpz_scan0(mpz op, integer starting_bit) - Find first 0 in op >= starting_bit.  
    let res = starting_bit,
        bit = 1n << BigInt(starting_bit);
    op = op[MPZ$B];
    while (op & bit) {
        bit = bit << 1n;
        res += 1;
    }
    return res;
} mpz_scan0.$sig="FOI";

function mpz_scan1(/*mpz*/ op, /*integer*/ starting_bit) {
//integer res = mpz_scan1(mpz op, integer starting_bit) - Find first 1 in op >= starting_bit.  
    let res = starting_bit,
        bit = 1n << BigInt(starting_bit);
    op = op[MPZ$B];
    while ((op & bit) === 0n) {
        bit = bit << 1n;
        res += 1;
    }
    return res;
} mpz_scan1.$sig="FOI";

function mpz_fits_integer(/*mpz*/ op) {
    return mpz_sizeinbase(op,2)<machine_bits()-1;
} mpz_fits_integer.$sig="FO";

//function mpz_fits_integer(/*mpz*/ op) {
//// Return true iff the value of op fits in a (signed) integer, otherwise, return false.
//// Note this actually returns false for -#40000000, which technically fits, but true
//// for -#3FFFFFFF..#3FFFFFFF, and false for (+)#40000000 (on 32-bit).
//  if (equal(op,NULL)) { crash("9/0"); }
//  return compare(mpz_sizeinbase(op,2),machine_bits()-1)<0;
//}

function mpz_fits_atom(/*mpz*/ op, /*bool*/ tztrim=false) {
// Return true iff the value of op fits in a phix atom, otherwise, return false.
// Note: this returns false for 9007199254740992 (on 32 bit), since that is
//       the first value that "accidentally" fits, by ending in a binary 0.
    let /*integer*/ n = mpz_sizeinbase(op,2), 
                    lim = ((equal(machine_bits(),32)) ? 53 : 64);
    if (tztrim && n>lim) {
        let /*mpz*/ temp = mpz_init_set(op);
        while (n>lim && mpz_even(temp)) {
            /*[,] =*/ mpz_fdiv_q_ui(temp,temp,2);
            n -= 1;
        }
        temp = mpz_free(temp);
    }
    return n<=lim;
} mpz_fits_atom.$sig="FOO,1";

function mpz_get_integer(/*mpz*/ op) {
//  return Number(BigInt.asIntN(32, op[MPZ$B]));
//  return Number(BigInt.asUintN(32, op[MPZ$B]));
    op = op[MPZ$B];
    op = (op < 0n) ? BigInt.asIntN(32, op)
                   : BigInt.asUintN(32, op);
    return Number(op);
} mpz_get_integer.$sig="FO";

function mpz_get_atom(/*mpz*/ op) {
//  return Number(BigInt.asIntN(53, op[MPZ$B]));
//  return Number(BigInt.asUintN(53, op[MPZ$B]));
    op = op[MPZ$B];
    op = (op < 0n) ? BigInt.asIntN(53, op)
                   : BigInt.asUintN(53, op);
    return Number(op);
} mpz_get_atom.$sig="FO";

function mpz_fib2_ui(/*mpz*/ fn, fnsub1, /*integer*/ n) {
    n -= 1;
    //
    // (manually) translated from the fastFibonacciDoubling() method off of
    // https://www.nayuki.io/res/fast-fibonacci-algorithms/FastFibonacci.java
    // original desktop version used for testing and that this was transpiled
    // from can be found in demo\rosetta\fibonacci.exw, as Mpz_fib2_ui()
    //
    if (n<0) {
        mpz_set_si(fnsub1,1);
        mpz_set_si(fn,0);
    } else {
        mpz_set_si(fnsub1,0);
        mpz_set_si(fn,1);
        let /*integer*/ bit = 0, m = n;
        while (m) {
            m = floor(m/2);
            bit = ((bit===0) ? 1 : 2*bit);
        }
        let /*mpz*/ d = mpz_init();
        while (bit) {
            // Loop invariant: fnsub1 = F(m), fn = F(m+1)
            //  assert fnsub1.equals(slowFibonacci(m));
            //  assert fn.equals(slowFibonacci(m+1));
            // Double it
            // d := fnsub1 * (fn*2 - fnsub1)
            mpz_mul_si(d,fn,2);
            mpz_sub(d,d,fnsub1);
            mpz_mul(d,fnsub1,d);
            // e := fnsub1*fnsub1 + fn*fn
            mpz_mul(fnsub1,fnsub1,fnsub1);
            mpz_mul(fn,fn,fn);
            mpz_add(fn,fn,fnsub1);
            // fnsub1 = d;
            // fn = e;
            mpz_set(fnsub1,d);
            m *= 2;
            //  assert fnsub1.equals(slowFibonacci(m));
            //  assert fn.equals(slowFibonacci(m+1));
            // Advance by one conditionally
            if (and_bits(n,bit)) {
                //  {fnsub1,fn} = {fn,fnsub1+fn}
                //  d := fnsub1 + fn
                mpz_add(d,fnsub1,fn);
                mpz_set(fnsub1,fn);
                mpz_set(fn,d);
                m += 1;
                //  assert fnsub1.equals(slowFibonacci(m));
                //  assert fn.equals(slowFibonacci(m+1));
            }
            bit = floor(bit/2);
        }
    }
} mpz_fib2_ui.$sig="POOI";

function mpz_fib_ui(/*mpz*/ fn, /*integer*/ n) {
    //
    // algorithm 3 from https://arxiv.org/pdf/2008.08822.pdf
    // see also https://gmplib.org/manual/Fibonacci-Numbers-Algorithm
    //
    if (n<2) {
        if (n<0) { crash("9/0"); }
        mpz_set_si(fn,n);
    } else {
        let /*mpz*/ c = mpz_init(3);
        let /*mpz*/ [,a,b] = mpz_inits(2);
        if (equal(remainder(n,2),0)) {
            mpz_set_si(a,0);
            mpz_set_si(b,1);
        } else {
            mpz_set_si(a,1);
            mpz_set_si(b,-1);
        }
        n = floor(n/2);
        while (n>1) {
            if (equal(remainder(n,2),0)) {
                mpz_mul(b,b,c);
                mpz_add(b,a,b);
            } else {
                mpz_mul(a,a,c);
                mpz_add(a,b,a);
            }
            mpz_mul(c,c,c);
            mpz_sub_si(c,c,2);
            n = floor(n/2);
        }
        mpz_mul(a,a,c);
        mpz_add(fn,b,a);
    }
} mpz_fib_ui.$sig="PON";

function mpz_rand(rop,range) {
    // returns BigInt 0 to range-1
    let r = range[MPZ$B],
        digits = r.toString().length / 9 + 2 | 0,
        rands = [];
    while (digits--) { 
        rands.push(("" + (Math.random() * 1000000000 | 0)).padStart(9, "0"));
    }
    rop[MPZ$B] = BigInt(rands.join("")) % r;  // Leading zeros are ignored
} mpz_rand.$sig="POO";

let mpz_rand_ui = mpz_rand;

//  // Generates BigInts between low (inclusive) and high (exclusive)
//  function generateRandomBigInt(lowBigInt, highBigInt) {
//    if (lowBigInt >= highBigInt) {
//      throw new Error('lowBigInt must be smaller than highBigInt');
//    }
//    const difference = highBigInt - lowBigInt;
//    const differenceLength = difference.toString().length;
//    let multiplier = '';
//    while (multiplier.length < differenceLength) {
//PL can we not take more digits??
//      multiplier += Math.random().toString().split('.')[1];
//    }
//    multiplier = multiplier.slice(0, differenceLength);
//    const divisor = '1' + '0'.repeat(differenceLength);
//    const randomDifference = (difference * BigInt(multiplier)) / BigInt(divisor);
//    return lowBigInt + randomDifference;
//  }

function MPZ$GCD(a,b) {
    if (a < 0n) { a = -a; }
    if (b < 0n) { b = -b; }
    if (b > a) { [a, b] = [b, a]; }
/*
    while (true) {
        if (b === 0n) { rop[MPZ$B] = a; break; }
        a %= b;
        if (a === 0n) { rop[MPZ$B] = b; break; }
        b %= a;
    }
*/
    while (b) {
        [a, b] = [b, a%b];
    }
    return a;
}

function mpz_gcd(/*mpz*/ rop, op1, op2) {
    let a = op1[MPZ$B],
        b = op2[MPZ$B];
    rop[MPZ$B] = MPZ$GCD(a,b);
} mpz_gcd.$sig="POOO";

function mpz_gcd_ui(/*mpz*/ rop, op1, /*integer*/ op2) {
    let a = op1[MPZ$B],
        b = BigInt(op2);
    a = MPZ$GCD(a,b);
    if (rop !== NULL) {
        rop[MPZ$B] = a;
    }
    if (!mpz_fits_atom(a)) { return 0; }
    a = Number(a);
    return a;
} mpz_gcd_ui.$sig="FOOI";


//function gcd(x, y) {
//  while (x) {
//      [x, y] = [y % x, x];
//  }
//  
//  return y;
//}

function mpz_lcm(/*mpz*/ rop, op1, op2) {
    let a = op1[MPZ$B],
        b = op2[MPZ$B],
        c = MPZ$GCD(a,b);
    rop[MPZ$B] = a*b/c;
} mpz_lcm.$sig="POOO";

function mpz_lcm_ui(/*mpz*/ rop, op1, /*integer*/ op2) {
    let a = op1[MPZ$B],
        b = BigInt(op2),
        c = MPZ$GCD(a,b);
    rop[MPZ$B] = a*b/c;
} mpz_lcm_ui.$sig="POOI";
//lcm = (x,y) => x*y/gcd(x,y);

function mpz_invert(rop, op1, op2) {
    op1 = op1[MPZ$B];
    op2 = op2[MPZ$B]
    let r = op2,
        t = 0n,
        newR = (op1 < 0n) ? -op1 : op1,
        newT = 1n;
    while (newR !== 0n) {
        let q = r / newR,
            lastT = t,
            lastR = r;
        t = newT;
        r = newR;
        newT = lastT - q*newT;
        newR = lastR - q*newR;
    }
//  if (!r.isUnit) Fiber.abort("%(op1) and %(op2) are not co-prime.")
    if (r !== 1n) { return false; }
    if (t < 0n) { t += op2; }
    if (op2 < 0n) { t = -t; }
    rop[MPZ$B] = t;
    return true;
} mpz_invert.$sig="FOOO";

/*
// a possible alternative (needing much work) for mpz_invert:
function modInverse(a, m) {
  // validate inputs
  [a, m] = [Number(a), Number(m)]
  if (Number.isNaN(a) || Number.isNaN(m)) {
    return NaN // invalid input
  }
  a = (a % m + m) % m
  if (!a || m < 2) {
    return NaN // invalid input
  }
  // find the gcd
  const s = []
  let b = m
  while(b) {
    [a, b] = [b, a % b]
    s.push({a, b})
  }
  if (a !== 1) {
    return NaN // inverse does not exists
  }
  // find the inverse
  let x = 1
  let y = 0
  for(let i = s.length - 2; i >= 0; --i) {
    [x, y] = [y,  x - y * Math.floor(s[i].a / s[i].b)]
  }
  return (y % m + m) % m
}

// Tests
//??
console.log(modInverse(2, 5))       // = 3
console.log(modInverse(3, 5))       // = 2
//??
console.log(modInverse(1, 2))       // = 1
console.log(modInverse(3, 6))       // = NaN
console.log(modInverse(25, 87))     // = 7
console.log(modInverse(7, 87))      // = 25
console.log(modInverse(19, 1212393831))     // = 701912218
console.log(modInverse(31, 73714876143))    // = 45180085378
console.log(modInverse(3, 73714876143))     // = NaN
console.log(modInverse(-7, 87))     // = 62
console.log(modInverse(-25, 87))    // = 80
console.log(modInverse(0, 3))       // = NaN
console.log(modInverse(0, 0))       // = NaN
*/

function mpz_divisible_p(/*mpz*/ n, d) {
    // returns non-zero if n is exactly divisible by d. n is divisible by d if there exists an integer q satisfying n = qd.
    // Unlike the other division functions, d = 0 is accepted and following the rule it can be seen that only 0 is considereddivisible by 0.
    n = n[MPZ$B];
    d = d[MPZ$B];
    if (d === 0n) { return b === 0n; }
    return (n % d) === 0n;
} mpz_divisible_p.$sig="FOO";

function mpz_divisible_ui_p(/*mpz*/ n, /*integer*/ d) {
    // returns non-zero if n is exactly divisible by d. n is divisible by d if there exists an integer q satisfying n = qd.
    // Unlike the other division functions, d = 0 is accepted and following the rule it can be seen that only 0 is considereddivisible by 0.
    n = n[MPZ$B];
    d = BigInt(d);
    if (d === 0n) { return b === 0n; }
    return (n % d) === 0n;
} mpz_divisible_ui_p.$sig="FOI";

//boolean res = mpz_divisible_2exp_p(mpz n, integer b) - "" except tests if n is divisible by 2 b  

function mpz_remove(/*mpz*/ rop, op, f) {
    let count = 0;
    op = op[MPZ$B];
    f = f[MPZ$B];
    while (op !== 0n && op%f === 0n) {
        op /= f;
        count += 1;
    }
    rop[MPZ$B] = op;
    return count;
} mpz_remove.$sig="FOOO";

// as transpiled from mp_prime_mr() in mpfr.e, with these three manually renamed:
let /*mpz*/ MPZ$modp47 = NULL, MPZ$w;
let /*sequence*/ MPZ$witness_ranges;
function mpz_prime(/*mpz*/ p, /*integer*/ k=10) {
    // deterministic to 3,317,044,064,679,887,385,961,981
    const primes = ["sequence",2n, 3n, 5n, 7n, 11n, 13n, 17n, 19n, 23n, 29n, 31n, 37n, 41n, 43n, 47n];
    let pn = p[MPZ$B];
    if (pn<=47n) {
        return find(pn,primes);
    }
    if (equal(MPZ$modp47,NULL)) {
        MPZ$modp47 = mpz_init("614_889_782_588_491_410"); // === product(primes), largest < 2^64
        MPZ$w = mpz_init();
        // Best known deterministic witnesses for given range and set of bases
        // https://miller-rabin.appspot.com/
        // https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test
        MPZ$witness_ranges = ["sequence",["sequence","341_531",["sequence","9345883071009581737"]],
                                         ["sequence","1_050_535_501",["sequence","336781006125","9639812373923155"]],
                                         ["sequence","350_269_456_337",["sequence","4230279247111683200",
                                                                    "14694767155120705706","16641139526367750375"]],
                                         ["sequence","55_245_642_489_451",["sequence","2","141889084524735",
                                                                    "1199124725622454117","11096072698276303650"]],
                                         ["sequence","7_999_252_175_582_851",["sequence","2","4130806001517",
                                                "149795463772692060","186635894390467037","3967304179347715805"]],
                                         ["sequence","585_226_005_592_931_977",["sequence","2","123635709730000",
                                                                    "9233062284813009","43835965440333360",
                                                                    "761179012939631437","1263739024124850375"]],
                                         ["sequence","18_446_744_073_709_551_615",["sequence","2","325","9375",
                                                                     "28178","450775","9780504","1795265022"]],
                                         ["sequence","318_665_857_834_031_151_167_461",["sequence","2","3","5",
                                                                "7","11","13","17","19","23","29","31","37"]],
                                         ["sequence","3_317_044_064_679_887_385_961_981",["sequence","2","3",
                                                    "5","7","11","13","17","19","23","29","31","37","41"]]];
        for (let i=1, i$lim=length(MPZ$witness_ranges); i<=i$lim; i+=1) {
            MPZ$witness_ranges = $repe(MPZ$witness_ranges,1,mpz_init($subse($subse(MPZ$witness_ranges,i),1)),["sequence",i]);
            for (let j=1, j$lim=length($subse($subse(MPZ$witness_ranges,i),2)); j<=j$lim; j+=1) {
                MPZ$witness_ranges = $repe(MPZ$witness_ranges,j,mpz_init($subse($subse($subse(MPZ$witness_ranges,i),2),j)),["sequence",2,i]);
            }
        }
    }
    mpz_gcd(MPZ$w,p,MPZ$modp47);
    if (!equal(mpz_cmp_si(MPZ$w,1),0)) {
        return false;   // eliminates 86.2% of all integers
    }
    //
    // Choose input witness bases:
    //
    let /*sequence*/ witnesses;
    if (compare(mpz_cmp(p,$subse($subse(MPZ$witness_ranges,-1),1)),0)>=0) {
        witnesses = repeat(0,k);
        for (let i=1, i$lim=k; i<=i$lim; i+=1) {
            let /*mpz*/ a = mpz_init();
            mpz_sub_ui(a,p,2);
            mpz_rand(a,a);      // a := 0..a-1 (cf rand(n) yields 1..n)
            mpz_add_ui(a,a,2);
            witnesses = $repe(witnesses,i,a);
        }
    } else {
        for (let i=1, i$lim=length(MPZ$witness_ranges); i<=i$lim; i+=1) {
            if (compare(mpz_cmp(p,$subse($subse(MPZ$witness_ranges,i),1)),0)<0) {
                witnesses = $subse($subse(MPZ$witness_ranges,i),2);
                break;
            }
        }
    }
    let /*mpz*/ d = mpz_init();
    mpz_sub_ui(d,p,1);
    let /*mpz*/ nm1 = mpz_init_set(d);
//    d >>= 4 while (d & 0xf) == 0                  # suck out factors of 2
//    (d >>= (d & 3)^2; d >>= (d & 1)^1) if d.even? # 4 bits at a time
    while (mpz_even(d)) {
        mpz_fdiv_q_2exp(d,d,1);
    }
    for (let i=1, i$lim=length(witnesses); i<=i$lim; i+=1) {
        let /*mpz*/ b = $subse(witnesses,i);
        if (!mpz_divisible_p(b,p)) {     // skip multiples of input
            let /*mpz*/ s = mpz_init_set(d), 
                        y = mpz_init();
            mpz_powm(y,b,d,p);          // y := b^d % p
            while (((!equal(mpz_cmp_si(y,1),0))
                 && (!equal(mpz_cmp(y,nm1),0)))
                 && (!equal(mpz_cmp(s,nm1),0))) {
                mpz_powm_ui(y,y,2,p);       // y := y^2 mod p
                mpz_mul_2exp(s,s,1);        // s << 1
            }
            if (!equal(mpz_cmp(y,nm1),0)) {
                if (mpz_even(s)) { return false; }
            }
        }
    }
    return true;
} mpz_prime.$sig="FOI,1";

//let mpz_prime_mr = mpz_prime;

function mpz_nextprime(/*mpz*/ rop, op) {
    if (compare(mpz_cmp_si(op,2),0)<0) {
        mpz_set_si(rop,2);
    } else {
        mpz_add_si(rop,op,((mpz_odd(op)) ? 2 : 1));
        while (!mpz_prime(rop)) {
            mpz_add_si(rop,rop,2);
        }
    }
} mpz_nextprime.$sig="POO";

function mpz_prime_factors(/*mpz*/ s, /*integer*/ maxprime=100) {
//
// Attempts to decompose the integer s into powers of small primes.
// returns eg 108 ==> {{2,2},{3,3}}  (ie 2^2*3^3==4*27==108)
//         or 10080 ==> {{2,5},{3,2},{5,1},{7,1}}
//         or 1196836 ==> {{2,2},{"299209"}}
//  Each element is a {prime,power} pair, except last may be a lone string.
//  See mpz_factorstring() for more clarification and handling of that.
//
// The default 100th prime is 541, so at that setting this is exact/complete 
//  for all inputs <= 541^2 == "292681", and you can easily raise (or lower) 
//  that limit, within reason.
// However, factors of even a 500-digit number is properly hard, so hard that 
//  in fact almost all internet security is based on it being a *really* hard 
//  problem. Hence this is designed to "give up" early/in a sensible fashion, 
//  eg: mpz_prime_factors(sprintf("%d",power(2*get_prime(101),2)),100) yields
//  {{2,2},{"299209"}}. Note that length(res[$]) is 1, which means it greater
//  than either get_prime(maxprime)^2 or power(2,machine_word()), hence it 
//  returns it as a (lone) final string. Also, while all other elements of
//  res are almost certainly phix integer, res[$][1] may be atom (1GB..4GB).
//  Increasing maxprime to 101 above would obviously yield {{2,2},{547,2}},
//  ie from mpz_prime_factors(sprintf("%d",power(2*get_prime(101),2)),101).
//  [[Technically on 32-bit (no such "hole" on 64-bit) it could yield atoms
//    in the range power(2,32..53) as "exact". However a) there are no mpz
//    routines matching mpz_fits_ulong_p/mpz_get_ui, not that a few divides
//    and multiplications should trouble us, along with some calls to say
//    mpz_sizeinbase() to determine when to attempt, but more importantly 
//    b) there does not seem to be very much point in bothering.]]
// Once again, see below for one suggested way to cope with all of that.
//  
    let /*sequence*/ res = ["sequence"];
    let /*mpz*/ n = ((string(s)) ? mpz_init(s) : mpz_init_set(s)), 
                f = mpz_init();
    let /*integer*/ c = mpz_cmp_si(n,1), p;
    if (c<=0) {
        // (special cases, equivalent to prime_factors(0)==>{}
        //                           and prime_factors(1)==>{1})
        res = ((c<0) ? ["sequence"] : ["sequence",["sequence",2,0]]);
    } else {
        let /*boolean*/ bPrime = mpz_prime(n,20);
        if (!bPrime) {
            for (let d=1, d$lim=maxprime; d<=d$lim; d+=1) {
                p = get_prime(d);
                if (mpz_divisible_ui_p(n,p)) {
                    mpz_set_si(f,p);
                    let /*integer*/ e = mpz_remove(n,n,f);
                    res = append(res,["sequence",p,e]);
                    if (equal(mpz_cmp_si(n,1),0)) { break; }
                    bPrime = mpz_prime(n,20);
                    if (bPrime) { break; }
                }
            }
        }
        if (!equal(mpz_cmp_si(n,1),0)) {
            let /*boolean*/ fits = mpz_fits_atom(n);
            if (fits && !bPrime) {
                p = get_prime(maxprime);
                mpz_ui_pow_ui(f,p,2);
                if (compare(mpz_cmp(n,f),0)>0) {
                    fits = false;
                }
            }
            if (fits) {
                res = append(res,["sequence",mpz_get_atom(n),1]);
            } else {
                res = append(res,["sequence",mpz_get_str(n)]);
            }
        }
    }
    [,n,f] = mpz_free(["sequence",n,f]);
    return res;
} mpz_prime_factors.$sig="FOI,1";

/*
function mpz_perfect_power_p(/!*mpz*!/ op) {
    
}
*/

function mpz_factorstring(/*sequence*/ s) {
// converts eg {{2,2},{3,3}} to "2^2*3^3"
// s is typically from mpz_prime_factors(), but does not have to be.
// s[$] may be {string} (ie unfactored/able).
    if (equal(s,["sequence"])) { return "0"; } // (rather than ""/crash)
    if (equal(s,["sequence",["sequence",2,0]])) { return "1"; } // (rather than "2^0")
    let /*string*/ res = "";
    for (let i=1, i$lim=length(s); i<=i$lim; i+=1) {
        if (length(res)) { res = $conCat(res, "*", false); }
        let /*object*/ si = $subse(s,i);
        if (string(si)) {
            res = $conCat(res, si, false);
        } else if (equal(length(si),1)) {
            res = $conCat(res, $subse(si,1), false);
        } else {
            let [,/*atom*/ p,/*integer*/ e] = $subse(s,i);
            res = $conCat(res, sprintf("%d",p), false);
            if (e!==1) {
                res = $conCat(res, sprintf("^%d",["sequence",e]), false);
            }
        }
    }
    return res;
} mpz_factorstring.$sig="FP";

function mpz_re_compose(/*mpz*/ rop, /*sequence*/ s) {
// takes eg {{2,2},{3,3}} and sets rop to 108,
// where 108 is 2^2*3^3 ie 4*27.
    if (length(s) === 0) {
        mpz_set_si(rop,1);
    } else {
        let /*mpz*/ pn = mpz_init();
        let /*boolean*/ inexact = (length($subse(s,-1)) === 1);
        if (inexact) {
            mpz_set_str(rop,$subse($subse(s,-1),1));
        } else {
            mpz_set_si(rop,1);
        }
        for (let i=1, i$lim=length(s)-inexact; i<=i$lim; i+=1) {
            let [,/*atom*/ p,/*integer*/ e] = $subse(s,i);
            mpz_ui_pow_ui(pn,p,e);
            mpz_mul(rop,rop,pn);
        }
        pn = mpz_free(pn);
    }
} mpz_re_compose.$sig="POP";

function mpz_pollard_rho(/*mpz_or_string*/ s, /*bool*/ bAsStrings=false) {
//
// Note that unlike mpz_prime_factors() the result is a list of strings, eg 
//  mpz_pollard_rho("151740406071813") ==> {"3","13","13","54833","5458223"}
// Update: that now only be true if bAsStrings is true, by default you will
//  now get matching integer {prime,pow} entries when bAsStrings is false,
//  at least for bits that pass mpz_fits_integer/atom() [I forget which].
//
    function merge(/*sequence*/ p, s, /*bool*/ bAsStrings) {
    //
    // private routine for mpz_pollard_rho()
    // s has {prime,pow} or {string} elements, as per mpz_prime_factors().
    // p the same unless bAsStrings is true, iwc it should be string-only.
    // result is slightly flattened with {prime,pow} or string elements
    //  (note I did not say {string} elements), in correct numeric order.
    //    (both p and s are expected to be in the right order on entry)
    //  (any strings we get are expected to have failed mpz_fits_integer)
    // eg merge({"123"},{{2,2},{"321"}},true) ==> {"2","2","123","321"}
    //    merge({"123"},{{2,2},{"321"}},false) ==> {{2,2},"123","321"}
    // Note we are coping with {"321"} in "s" because mpz_prime_factors()
    //  spits that out, but as it stands a plain "321" shd be ok too.
    //
        function compare_strings(/*string*/ p, s) {
        // private routine for merge(): return true to pick p over s
        //  (could be nested but I'm not quite confident enough yet)
            return compare(length(p),length(s))<0 || ((equal(length(p),length(s))) && p<s);
        }
        let /*sequence*/ res = ["sequence"];
        let /*atom*/ prime;
        let /*integer*/ pwr;
        let /*bool*/ which;
        if (bAsStrings) {
            // p should already be string-only (as a prior result of this routine)
            for (let i=1, i$lim=length(s); i<=i$lim; i+=1) {
                let /*sequence*/ si = $subse(s,i);
                if (equal(length(si),1)) {
                    res = append(res,$subse(si,1));
                } else {
                    [,prime,pwr] = si;
                    si = sprintf("%d",prime);
                    for (let j=1; j<=pwr; j+=1) {
                        res = append(res,si);
                    }
                }
            }
            if (length(p)) {
                s = res;
                res = ["sequence"];
                while (length(p) && length(s)) {
                    let /*string*/ ps = $subse(p,1), 
                                   ss = $subse(s,1);
                    which = compare_strings(ps,ss);
                    if (which) {
                        res = append(res,ps);
                        p = $subss(p,2,-1);
                    } else {
                        res = append(res,ss);
                        s = $subss(s,2,-1);
                    }
                }
                res = $conCat(res, p, false);
                res = $conCat(res, s, false);
            }
        } else {
            let /*object*/ p1, s1;
            while (length(p) && length(s)) {
                p1 = $subse(p,1);
                s1 = $subse(s,1);
                if (length(s1) === 1) { s1 = $subse(s1,1); }
                if (string(p1)) {
                    if (string(s1)) {
                        which = compare_strings(p1,s1);
                    } else {
                        [,prime,pwr] = s1;
                        which = compare_strings(p1,sprintf("%d",prime));
                    }
                } else if (string(s1)) {
                    [,prime,pwr] = p1;
                    which = compare_strings(sprintf("%d",prime),s1);
                } else {
                    [,prime,pwr] = s1;
                    if ($subse(p1,1) === prime) {
                        p1 = deep_copy(p1);
                        p1 = $repe(p1,2,$subse(p1,2)+pwr); // (merge)
                        s = $subss(s,2,-1);
                        which = true;
                    } else {
                        which = compare($subse(p1,1),prime)<0;
                    }
                }
                if (which) {
                    res = append(res,p1);
                    p = $subss(p,2,-1);
                } else {
                    res = append(res,s1);
                    s = $subss(s,2,-1);
                }
            }
            res = $conCat(res, p, false);
    //      res &= s
            for (let i=1, i$lim=length(s); i<=i$lim; i+=1) {
                s1 = $subse(s,i);
                if (length(s1) === 1) { s1 = $subse(s1,1); }
                res = append(res,s1);
            }
        }
        return res;
    }

    let /*mpz*/ n = ((string(s)) ? mpz_init(s) : mpz_init_set(s));
    let /*sequence*/ res = ["sequence"];
    while (mpz_cmp_si(n,100000000) > 0) {
        if (mpz_prime(n)) {
            if (!bAsStrings && mpz_fits_atom(n)) {
                res = append(res,["sequence",mpz_get_atom(n),1]);
            } else {
                res = append(res,mpz_get_str(n));
            }
            mpz_set_si(n,1);
            break;
        }
        let /*mpz*/ x = mpz_init(2), 
                    y = mpz_init(2), 
                    f = mpz_init(1); // factor
        let /*integer*/ size = 2;
        while (mpz_cmp_si(f,1) === 0) {
            for (let count=1; count<=size; count+=1) {
                mpz_mul(x,x,x);
                mpz_add_si(x,x,1);
                mpz_mod(x,x,n);
                mpz_sub(f,x,y);
                mpz_abs(f,f);
                mpz_gcd(f,f,n);
                if (mpz_cmp_si(f,1) !== 0) { break; }
            }
            size *= 2;
            mpz_set(y,x);
        }
        if (mpz_cmp(f,n) === 0) { break; }
        res = merge(res,mpz_prime_factors(f,1230),bAsStrings);
        // aside: 1230 makes sense below, as (only check) all factors
        //        less than 10_000, but not entirely sure about prev.
        mpz_fdiv_q(n,n,f); // n := floor(n/f)
    }
    if (mpz_cmp_si(n,1) > 0) {
        res = merge(res,mpz_prime_factors(n,1230),bAsStrings);
    }
    return res;
} mpz_pollard_rho.$sig="FOI,1";

//manually pasted transpilation of the one in mpfr.e:
/*global*/ 
function mpz_factors(/*object*/ s, /*object*/ include1=0, /*bool*/ bAsAtmStr=true, bSort=true) {
//, bCountOnly=false) -- [cannot think of a good use, tbh: re-add should one ever pop up]
//
//  returns a list of all integer factors of s
//  s can be a small integer, a string, a result from mpz_pollard_rho, or
//  a "tagged" mpz which looks like this: {"mpz",z}. Take special care to
//  wrap a plain mpz argument like that; there is a high risk of treating
//  the raw memory address of an unwrapped mpz as a small integer.
//  When I say integer, I mean an atom with no fractional part that passes
//  builtins/pfactors.e/check_limits() or it&rsquo;s equivalent, so maybe
//  "small" isn&rsquo;t the right word, but I think you get what I mean.<br>
//  As per factors():
//  if include1 is 1 the result contains 1 and n
//  if include1 is -1 the result contains 1 but not n
//  if include1 is 0 the result contains neither 1 nor n (the default)
//  if bAsAtmStr is false, the result is a sequence of mpz, otherwise (the
//  default) the result is a mixture of atoms (when they fit) and strings
//  (when they exceed the 53/64 bit precision limit of normal Phix atoms).<br>
//  Supplying a bAsAtmStr parameter of/>= 2 means "return all as strings".
//  Sorting probably adds very little overhead, but you never know...<br>
//  Just as mpz_pollard_rho() can return strings for prime factors that do
//  not fit in a phix atom, any strings in prime_factors are assumed to be
//  primes, and the result may be a mixture of integers and strings.<br>
//  Note this is not expected to handle duplicate strings correctly: if
//  you have (somehow) managed to factor a 32-digit+ number, good on you,
//  but you've long passed the bounds of what is realistically feasible,
//  unless, that is, all the prime factors are reasonably small integers.<br>
//  ?mpz_factors(12)                ==> {1,2,4,3,6,12}<br>
//  ?mpz_factors(12,bSort:=true)    ==> {1,2,3,4,6,12}<br>
//  ?mpz_factors("12345678999999977")  -- ,bSort:=true) don't help)
//           ==> {1,35604581,346744117,"12345678999999977"}<br>
//  ?mpz_factors("12345678999999977",bAsAtmStr:=2)
//          ==> {"1","35604581","346744117","12345678999999977"}<br>
//
    if (string(include1)) {
        include1 = $subse(["sequence",1,-1,0,-9],find(include1,["sequence","BOTH","JUST1","NEITHER","SET_LIM"]));
    }
    let /*sequence*/ res = ["sequence",mpz_init(1)];
    if (atom(s)) {
        s = prime_factors(s,2,-1); // (precision limit applies)
    } else if (string(s)) {
        s = mpz_pollard_rho(s);
    } else if (equal($subse(s,1),"mpz")) {
        s = mpz_pollard_rho($subse(s,2));
    }
//  if s={} then return s end if
    if ((equal(s,["sequence"])) || (equal(s,["sequence",["sequence",2,0]]))) {
        if (equal(include1,0)) { return ["sequence"];
        } else if (bAsAtmStr===false) {
            return ["sequence",mpz_init(1)];
        } else if (bAsAtmStr===1) {
            return ["sequence",1];
        } else {
            return ["sequence","1"];
        }
//      return {}
    }
//  if bCountOnly then
//      integer count = 1
//      for pn in s do count *= pn[2]+1 end for
//      return count
//  end if
    for (let si$idx = 1, si$lim = length(s); si$idx <= si$lim; si$idx += 1) { let si = $subse(s,si$idx);
        if (string(si)) { si = ["sequence",si,1]; }
        let /*sequence*/ r1, r2 = ["sequence"];
        let /*integer*/ l = length(res);
        let /*mpz*/ p = mpz_init($subse(si,1)), 
                    pj = mpz_init_set(p);
        for (let j=1, j$lim=$subse(si,2); j<=j$lim; j+=1) {
            for (let k=1, k$lim=l; k<=k$lim; k+=1) {
                let /*mpz*/ pk = mpz_init();
                mpz_mul(pk,$subse(res,k),pj);
                r2 = $conCat(r2, pk, false);
            }
            mpz_mul(pj,pj,p);
        }
        if (bSort) {
            r2 = custom_sort(mpz_cmp,r2);
            r1 = res;
            res = ["sequence"];
            let /*integer*/ r1dx = 1, lr1 = length(r1), 
                            r2dx = 1;
            let /*mpz*/ p1 = $subse(r1,1), 
                        q1 = $subse(r2,1);
            while (true) {
                if (compare(mpz_cmp(p1,q1),0)<0) {
                    res = $conCat(res, p1, false);
                    r1dx += 1;
                    if (r1dx>lr1) { break; }
                    p1 = $subse(r1,r1dx);
                } else {
                    res = $conCat(res, q1, false);
                    r2dx += 1;
                    q1 = $subse(r2,r2dx);
                }
            }
            res = $conCat(res, $subss(r2,r2dx,-1), false);
        } else {
            res = $conCat(res, r2, false);
        }
    }
    if (bAsAtmStr) {
        for (let i = 1, ri$lim = length(res); i <= ri$lim; i += 1) { let ri = $subse(res,i);
            res = $repe(res,i,(((bAsAtmStr===true) && mpz_fits_atom(ri)) ? mpz_get_atom(ri) : mpz_get_str(ri)));
        }
    }
    if (equal(include1,-1)) { res = $subss(res,1,-1-1);
    } else if (equal(include1,0)) { res = $subss(res,2,-1-1);
    }
    return res;
} mpz_factors.$sig="FOOII,1";

function mpz_min(/*sequence*/ s, /*boolean*/ return_index=false) {
    let /*mpz*/ res = $subse(s,1); // (ioob as per docs...)
    let /*integer*/ rdx = 1;
    for (let i=2, i$lim=length(s); i<=i$lim; i+=1) {
        if (equal(mpz_cmp($subse(s,i),res),-1)) {
            res = $subse(s,i);
            rdx = i;
        }
    }
    return ((return_index) ? rdx : res);
} mpz_min.$sig="FPO,1";

function mpz_max(/*sequence*/ s, /*boolean*/ return_index=false) {
    let /*mpz*/ res = $subse(s,1); // (ioob as per docs...)
    let /*integer*/ rdx = 1;
    for (let i=2, i$lim=length(s); i<=i$lim; i+=1) {
        if (equal(mpz_cmp($subse(s,i),res),+1)) {
            res = $subse(s,i);
            rdx = i;
        }
    }
    return ((return_index) ? rdx : res);
} mpz_max.$sig="FPO,1";

function mpz_sign(/*mpz*/ op1) {
    return compare(mpz_cmp_si(op1,0),0); // -1: op1 -ve, 0: op1=0, +1: op1+ve
} mpz_sign.$sig="FO";

//These may yet be made official...
function mpz_vecprod(/*mpz*/ rop, /*sequence*/ s, /*integer*/ zlr=1) {
//
// Fast vector multiplication.
// Multiplying the vector elements in pairs is much faster for essentially 
//  much the same reason that merge sort is faster than insertion sort.
//  Improved rosettacode/Primorial_numbers from 6 minutes to 6 seconds!!!!
// NB: Input sequence s (must all be mpz) is damaged. Returns an mpz.
// The zlr param allows you to specify the result when {} is passed,
// since I imagine there'll be cases where you'd rather get a zero.
//
    if (equal(s,["sequence"])) {
        mpz_set_si(rop,zlr);
    } else {
        while (compare(length(s),1)>0) {
            for (let i=1, i$lim=floor(length(s)/2); i<=i$lim; i+=1) {
                mpz_mul($subse(s,i),$subse(s,i),$subse(s,-i));
            }
            s = $subss(s,1,ceil(length(s)/2));
        }
        mpz_set(rop,$subse(s,1));
    }
}

function mpz_vecprod_si(/*mpz*/ rop, /*sequence*/ s, /*integer*/ zlr=1) {
//
// As above except input sequence s must all be integer. Unlike above, 
//  s is not damaged, and this entry point is expected to be used more 
//  often than the above, for such simple practical reasons, that is.
//
    if (and_bits(length(s),1)) {
        s = $conCat(s, 1, false);
    }
    let /*integer*/ j = 0;
    for (let i=1, i$lim=length(s); i<=i$lim; i+=2) {
        let /*mpz*/ two = mpz_init($subse(s,i));
        mpz_mul_si(two,two,$subse(s,i+1));
        j += 1;
        s = $repe(s,j,two);
    }
    s = $subss(s,1,j);
    mpz_vecprod(rop,s,zlr);
}

function mpz_fac_ui(/*mpz*/ rop, /*integer*/ n) {
//Set rop to the factorial of n.
    mpz_vecprod_si(rop,tagset(n),1);
} mpz_fac_ui.$sig="POI";

function mpz_bin_uiui(/*mpz*/ rop, /*integer*/ n, k) {
// equivalent, for small n and k, to builtins/factorial.e's choose()
    mpz_set_si(rop,1);
    for (let i=1, i$lim=k; i<=i$lim; i+=1) {
//      r := (r*(n-i+1))/i
        mpz_mul_si(rop,rop,(n-i)+1);
        if (!equal(mpz_fdiv_q_ui(rop,rop,i),0)) { crash("9/0"); }
    }
} mpz_bin_uiui.$sig="POII";

//function mpz_nthroot(/*mpz*/ rop, op, /*integer*/ n) {
//  let /*integer*/ n1 = n-1;
//  let /*mpz*/ n2 = mpz_init(n), 
//              n3 = mpz_init(n1), 
//              c = mpz_init(1), 
//              d = mpz_init_set(n3), 
//              e = mpz_init_set(n3), 
//              t = mpz_init();
//  mpz_add(d,d,op);
//  mpz_fdiv_q(d,d,n2);
//  mpz_set(e,d);
//  while (true) {
//      mpz_pow_ui(t,e,n1);
//      mpz_mul(e,e,n3);
//      mpz_fdiv_q(t,op,t);
//      mpz_add(e,e,t);
//      mpz_fdiv_q(e,e,n2);
//      if (equal(mpz_cmp(c,d),0)) { break; }
//      if (equal(mpz_cmp(c,e),0)) { break; }
//      mpz_set(c,d);
//      mpz_set(d,e);
//  }
//  mpz_set(rop,((compare(mpz_cmp(d,e),0)<0) ? d : e));
//}

function MPZ$nthroot(/*BigInt*/ op, /*integer*/ n) {
    n = BigInt(n);
    let s = op + 1n,
        k1 = n - 1n,
        u = op;
    while (u < s) {
        s = u;
        u = ((u*k1) + op / (u ** k1)) / n;
    }
    return s;
}

function mpz_nthroot(/*mpz*/ rop, op, /*integer*/ n) {
    op = op[MPZ$B];
    rop[MPZ$B] = MPZ$nthroot(op,n);
} mpz_nthroot.$sig="POOI";

function mpz_sqrt(/*mpz*/ rop, op) {
    mpz_nthroot(rop,op,2);
} mpz_sqrt.$sig="POO";

//from wren (tryme?):
// isqrt {
//      if (isNegative) Fiber.abort("Cannot take the square root of a negative number.")
//      if (isSmall) return BigInt.small_(toSmall.sqrt.floor)
//      var one = BigInt.one
//      var a = one << ((bitLength + one) >> one)
//      while (true) {
//          var b = (this/a + a) >> one
//          if (b >= a) return a
//          a = b
//      }
//  } 

function mpz_and(/*mpz*/ rop, op1, op2) {
    rop[MPZ$B] = op1[MPZ$B] & op2[MPZ$B];
} mpz_and.$sig="POOO";

function mpz_xor(/*mpz*/ rop, op1, op2) {
    rop[MPZ$B] = op1[MPZ$B] ^ op2[MPZ$B];
} mpz_xor.$sig="POOO";

function mpz_popcount(/*mpz*/ op) {
    let count = 0,
        n = op[MPZ$B];
    while (n > 0n) {
        n &= n - 1n;
        count += 1;
    }
    return count;
} mpz_popcount.$sig="FO";

function MPFR$GCD(a, b) {
/*
    while (b > 0n) {
//      const r1 = a - Math.floor(a / b) * b,
        const r1 = a - (a / b) * b,
              r2 = b - r1,
              r = r1 < r2 ? r1 : r2;
        a = b;
        b = r;
    }
    return a;
*/
// (no actual proof [yet] that this is any better/faster than the above)
    if (a < 0n) { a = -a; }
    if (b < 0n) { b = -b; }
    if (a === 0n) {
        return b;
    } else if (b === 0n) {
        return a;
    }
    let shift = 0n;
    while (((a | b) & 1n) === 0n) {
        a >>= 1n;
        b >>= 1n;
        shift += 1n;
    }
    while ((a & 1n) === 0n) { a >>= 1n; }
    do {
        while ((b & 1n) === 0n) { b >>= 1n; }
        if (a > b) {
          const x = a;
          a = b;
          b = x;
        }
        b -= a;
      } while (b !== 0n)
    // rescale
    return a << shift;
/*
boost does this:
    if (a < 0n) { a = -a; }
    if (b < 0n) { b = -b; }
    for(;;) {
      if (a == 0n) { return b; }
      a %= b;
      if (b == 0n) { return a; }
      b %= a;
    }
*/
}

function MPFR$LCM(a, b) {
    if (a < 0n) { a = -a; }
    if (b < 0n) { b = -b; }
    return a / MPFR$GCD(a, b) * b;
}

function mpq_canonicalize(/*mpq*/ op) {
// Remove any factors that are common to the numerator and denominator of op, 
// and make the denominator positive.
    let n = op[MPQ$N],
        d = op[MPQ$D],
        g = MPFR$GCD(n,d);
    
    if (g > 1n) {
        n = n/g;
        d = d/g;
    }
    if (d < 0n) {
        n = -n;
        d = -d;
    }
    op[MPQ$N] = n;
    op[MPQ$D] = d;
} mpq_canonicalize.$sig="PO";

function mpq_set(/*mpq*/ tgt, src) {
    tgt[MPQ$N] = src[MPQ$N];
    tgt[MPQ$D] = src[MPQ$D];
} mpq_set.$sig="POO";

function mpq_set_si(/*mpq*/ tgt, /*integer*/ n, d=1) {
// tgt := n/d
    tgt[MPQ$N] = BigInt(n);
    tgt[MPQ$D] = BigInt(d);
    mpq_canonicalize(tgt);
} mpq_set_si.$sig="POII,2";

function mpq_set_str(/*mpq*/ tgt, /*string*/ s, /*integer*/ base=0) {
    let /*integer*/ k = s.indexOf('/'),
                   dn = 1n; 
    if (k !== -1) {
        dn = MPFR$PREFIX(s.slice(k+1),base);
        s = s.substr(0,k);
    }
    tgt[MPQ$N] = MPFR$PREFIX(s,base);
    tgt[MPQ$D] = dn;
    mpq_canonicalize(tgt);
} mpq_set_str.$sig="POSI,2";

function mpq_set_z(/*mpq*/ tgt, /*mpz*/ n, d=NULL) {
//mpq_set_z(mpq tgt, mpz n, d=NULL) - Set tgt from an mpz (d=NULL results in an implied denominator of 1).
    tgt[MPQ$N] = n[MPZ$B];
    if (d === NULL) {
        tgt[MPQ$D] = 1n;
    } else {
        tgt[MPQ$D] = d[MPZ$B];
        mpq_canonicalize(tgt);
    } 
} mpq_set_z.$sig="POOO,2";

function mpq_init_set(op) {
    return ["mpq",op[MPQ$N],op[MPQ$D]];
} mpq_init_set.$sig="FO";

function mpq_init_set_si(/*integer*/ n, d=1) {
    let /*mpq*/ res = mpq_init();
    mpq_set_si(res, n, d);
    return res;
} mpq_init_set_si.$sig="FII,1";

function mpq_init_set_str(/*string*/ s, /*integer*/ base=0) {
    let /*mpq*/ res = mpq_init();
    mpq_set_str(res,s,base);
    return res;
} mpq_init_set_str.$sig="FSI,1";

function mpq_init_set_z(/*mpz*/ n, d=NULL) {
    let /*mpq*/ res = mpq_init();
    mpq_set_z(res,n,d);
    return res;
} mpq_init_set_z.$sig="FOO,1";

function mpq_init(/*object*/ v=0) {
    let /*object*/ res = ["mpq",0n,1n];
    if (!equal(v,0)) {
//      if mpq(v) then              -- NO!!
//          mpq_set(res, v)
//      elsif mpz(v) then           -- NO!!
//          mpq_set_z(res, v)
        if (integer(v)) {
            mpq_set_si(res,v);
        } else if (string(v)) {
            mpq_set_str(res,v);
        } else {
            crash("9/0"); // what's v then?? (non-integer atom is invalid)
        }
    }
    return res;
} mpq_init.$sig="FO,0";

//function mpq_init() {
//  return ["mpq",0n,1n];
//}

function mpq_inits(/*integer*/ n, /*object*/ v=0) {
    let /*object*/ res = repeat(0,n);
    if (sequence(v) && !string(v)) {
        if (!equal(length(v),n)) { crash("9/0"); }
        for (let i=1, i$lim=n; i<=i$lim; i+=1) {
            res = $repe(res,i,mpq_init($subse(v,i))); }
    } else {
        for (let i=1, i$lim=n; i<=i$lim; i+=1) {
            res = $repe(res,i,mpq_init(v)); }
    }
    return res;
} mpq_inits.$sig="FIO,1";

//function mpq_inits(n) {
//  let res = repeat(0,n);
//  for (let i = 1; i <= n; i += 1) {
//      res[i] = mpq_init();
//  }
//  return res;
//}

let mpq_free = mpz_free;

function mpq_get_d(/*mpq*/ op) {
// return (atom) floor(mpq_get_num(op)/mpq_get_den(op))
//Convert op to a double, truncating if necessary (ie. rounding towards zero).
//If the exponent from the conversion is too big or too small to fit a double then the result is
//system dependent. For too big an infinity is returned when available. For too small 0:0 is
//normally returned. Hardware overflow, underflow and denorm traps may or may not occur.
//DEV (as per mpfr_get_d()) there must be some way quite a bit a better than this...

    let dn = (op[MPQ$N]*(10n**20n))/op[MPQ$D],
        d = Number(dn)/1e20;
    return d;
} mpq_get_d.$sig="FO";

function mpq_get_num(/*mpz*/ numerator, /*mpq*/ rational) {
    numerator[MPZ$B] = rational[MPQ$N];
} mpq_get_num.$sig="POO";

function mpq_get_den(/*mpz*/ denominator, /*mpq*/ rational) {
    denominator[MPZ$B] = rational[MPQ$D];
} mpq_get_den.$sig="POO";

function mpq_get_str(/*mpq*/ op, /*integer*/ base=10, /*boolean*/ comma_fill=false) {
//Return op as a string in the specified base (2..62).
//The result will be of the form "num/den", or if the denominator is 1 then just "num".  
    let n = op[MPQ$N],
        d = op[MPQ$D],
        res = n.toString(base);
    if (comma_fill) { res = MPFR$COMMAFILL(res); }
    if (d !== 1n && n !== 0n) {
        let den = d.toString(base);
        if (comma_fill) { den = MPFR$COMMAFILL(den); }
        res += '/' + den;
    }
    return res;
} mpq_get_str.$sig="FOIO,1";

function mpq_cmp(/*mpq*/ op1, op2) {
    let n1 = op1[MPQ$N],
        d1 = op1[MPQ$D],
        n2 = op2[MPQ$N],
        d2 = op2[MPQ$D];
    if (d1 === d2) {
        return n1 === n2 ? 0 : n1 > n2 ? 1 : -1;
    }
    let c = ((d1 < 0n) === (d2 < 0)) ? 1 : -1;
    n1 *= d2;
    n2 *= d1;
    return n1 === n2 ? 0 : n1 > n2 ? c : -c;
/*
alt, untested (adapted from from boost):
    // If the two values have different signs, we don't need to do the
    // expensive calculations below. We take advantage here of the fact
    // that the denominators are always positive.
    if (((n1<0n) !== (n2<0)) || (d1 === d2)) {
        return n1 === n2 ? 0 : n1 > n2 ? 1 : -1;
    }
    // Avoid overflow
    let gcd1 = MPFR$GCD(n1, n2),
        gcd2 = MPFR$GCD(d2, d1),
        a = (n1/gcd1) * (d2/gcd2),
        b = (n2/gcd1) * (d1/gcd2);
    // likewise this assumes neither denominator is negative.
    return a === b ? 0 : a > b ? 1 : -1;
// or from bjarne:
//  x1.num*x2.den==x1.den*x2.num;
*/
} mpq_cmp.$sig="FOO";


function mpq_cmp_si(/*mpq*/ op1, /*integer*/ n, d=1) {
    let op2 = mpq_init_set_si(n, d);
    return mpq_cmp(op1,op2);
} mpq_cmp_si.$sig="FOII,2";

function mpq_abs(/*mpq*/ rop, op) {
    let n = op[MPQ$N],
        d = op[MPQ$D];
    if (n < 0n) { n = -n; }
    if (d < 0n) { d = -d; }
    rop[MPQ$N] = n;
    rop[MPQ$D] = d;
} mpq_abs.$sig="POO";

function mpq_neg(/*mpq*/ rop, op) {
    let n = op[MPQ$N],
        d = op[MPQ$D];
    rop[MPQ$N] = -n;
    rop[MPQ$D] = d;
} mpq_neg.$sig="POO";

function mpq_inv(/*mpq*/ rop, op) {
    let n = op[MPQ$N],
        d = op[MPQ$D];
    if (d === 0n) { crash("divide by zero"); }
    if (n<0) {
        n = -n;
        d = -d;
    }
    rop[MPQ$N] = d;
    rop[MPQ$D] = n;
} mpq_inv.$sig="POO";

function mpq_add(/*mpq*/ rop, op1, op2) {
//-- set rop to op1 + op2.
    let n1 = op1[MPQ$N],
        d1 = op1[MPQ$D],
        n2 = op2[MPQ$N],
        d2 = op2[MPQ$D],
        lm = MPFR$LCM(d1,d2),
        a = lm / d1,
        b = lm / d2;
    a *= n1;
    b *= n2;
    rop[MPQ$N] = a + b;
    rop[MPQ$D] = lm;
    mpq_canonicalize(rop);
/*
alt, untested (adapted from from boost):
    //
    // This calculation avoids overflow, and minimises the number of expensive
    // calculations. Thanks to Nickolay Mladenov for this algorithm.
    //
    // Proof:
    // We have to compute a/b + c/d, where gcd(a,b)=1 and gcd(b,c)=1.
    // Let g = gcd(b,d), and b = b1*g, d=d1*g. Then gcd(b1,d1)=1
    //
    // The result is (a*d1 + c*b1) / (b1*d1*g).
    // Now we have to normalize this ratio.
    // Let's assume h | gcd((a*d1 + c*b1), (b1*d1*g)), and h > 1
    // If h | b1 then gcd(h,d1)=1 and hence h|(a*d1+c*b1) => h|a.
    // But since gcd(a,b1)=1 we have h=1.
    // Similarly h|d1 leads to h=1.
    // So we have that h | gcd((a*d1 + c*b1) , (b1*d1*g)) => h|g
    // Finally we have gcd((a*d1 + c*b1), (b1*d1*g)) = gcd((a*d1 + c*b1), g)
    // Which proves that instead of normalizing the result, it is better to
    // divide num and den by gcd((a*d1 + c*b1), g)
    //
    let n1 = op1[MPQ$N],
        d1 = op1[MPQ$D],
        n2 = op2[MPQ$N],
        d2 = op2[MPQ$D]
        g = MPFR$GCD(d1, d2);
    d1 /= g;  // = b1 from the calculations above
    n1 = n1 * (d2 / g) + n2 * d1;
    g = MPFR$GCD(n1, g);
    n1 /= g;
    d1 *= d2/g;
    rop[MPQ$N] = n1;
    rop[MPQ$D] = d2;
*/
} mpq_add.$sig="POOO";

function mpq_add_si(/*mpq*/ rop, op1, /*integer*/ n, d=1) {
    let /*mpq*/ op2 = mpq_init_set_si(n,d);
    mpq_add(rop, op1, op2);
    op2 = mpq_free(op2);
} mpq_add_si.$sig="POOII,3";

function mpq_sub(/*mpq*/ rop, op1, op2) {
// set rop to op1 - op2.
//oops, problem when rop===op1:
//  mpq_neg(rop,op2);
//  mpq_add(rop,op1,rop);
    let n1 = op1[MPQ$N],
        d1 = op1[MPQ$D],
        n2 = op2[MPQ$N],
        d2 = op2[MPQ$D],
        lm = MPFR$LCM(d1,d2),
        a = lm / d1,
        b = lm / d2;
    a *= n1;
    b *= n2;
    rop[MPQ$N] = a - b;
    rop[MPQ$D] = lm;
    mpq_canonicalize(rop);
// (see mpq_add for an alt, identical bar a single +/-)
} mpq_sub.$sig="POOO";
    
function mpq_sub_si(/*mpq*/ rop, op1, /*integer*/ n, d=1) {
    let /*mpq*/ op2 = mpq_init_set_si(n,d);
    mpq_sub(rop, op1, op2);
    op2 = mpq_free(op2);
} mpq_sub_si.$sig="POOII,3";

function mpq_mul(/*mpq*/ rop, op1, op2) {
//-- set rop to op1 * op2.
    rop[MPQ$N] = op1[MPQ$N] * op2[MPQ$N];
    rop[MPQ$D] = op1[MPQ$D] * op2[MPQ$D];
    mpq_canonicalize(rop);
/*
alt, untested (adapted from from boost):
    let n1 = op1[MPQ$N],
        n2 = op2[MPQ$N],
        d1 = op1[MPQ$D],
        d2 = op2[MPQ$D],
        // Avoid overflow and preserve normalization
        g1 = MPFR$GCD(n1,d2),
        g2 = MPFR$GCD(n2,d1),
        n = (n1/g1) * (n2/g2),
        d = (d1/g2) * (d2/g1);
    rop[MPQ$N] = n;
    rop[MPQ$D] = d;
*/
} mpq_mul.$sig="POOO";

function mpq_mul_si(/*mpq*/ rop, op1, /*integer*/ n, d=1) {
    let /*mpq*/ op2 = mpq_init_set_si(n,d);
    mpq_mul(rop, op1, op2);
    op2 = mpq_free(op2);
} mpq_mul_si.$sig="POOII,3";

function mpq_div(/*mpq*/ rop, op1, op2) {
// set rop to op1 / op2.
// (as mpq_mul() but with op2's num/den swapped)
    rop[MPQ$N] = op1[MPQ$N] * op2[MPQ$D];
    rop[MPQ$D] = op1[MPQ$D] * op2[MPQ$N];
    mpq_canonicalize(rop);
/*
alt, untested (adapted from from boost):
    let n1 = op1[MPQ$N],
        n2 = op2[MPQ$N],
        d1 = op1[MPQ$D],
        d2 = op2[MPQ$D];
    if (n2===0n) { crash("bad rational"); }
    if (n1!=0) {
        let g1 = MPFR$GCD(n1,n2),
            g1 = MPFR$GCD(d1,d2),
            n = (n1/g1) * (d2/g2),
            d = (d1/g2) * (n2/g1);
        if (d<0) {
            n = -n;
            d = -d;
        }
        rop[MPQ$N] = n;
        rop[MPQ$D] = d;
    }
*/
} mpq_div.$sig="POOO";

function mpq_div_si(/*mpq*/ rop, op1, /*integer*/ n, d=1) {
    let /*mpq*/ op2 = mpq_init_set_si(d,n);
    mpq_mul(rop, op1, op2);
    op2 = mpq_free(op2);
} mpq_div_si.$sig="POOII,3";

function mpq_div_2exp(/*mpq*/ rop, op, /*integer*/ bits) {
// set rop to op / 2^bits
    rop[MPQ$N] = op[MPQ$N];
    rop[MPQ$D] = op[MPQ$D] << BigInt(bits);
    mpq_canonicalize(rop);
} mpq_div_2exp.$sig="POOI";

//mpq_mul_2exp(mpq rop, op, integer bits) - set rop to op * 2^bits.
//mpq_div_2exp(mpq rop, op, integer bits) - set rop to op / 2^bits.
// dodgy below here!

/*


global procedure mpq_mul_2exp(mpq rop, op, integer bits)
-- set rop to op * 2^bits
    if rop=NULL then ?9/0 end if
    if op=NULL then ?9/0 end if
    if bits<0 then ?9/0 end if  -- (not sure about that...)
    if x_mpq_mul_2exp=NULL then
        x_mpq_mul_2exp = define_c_proc(mpir_dll, "+__gmpq_mul_2exp", {P,P,I})
    end if
    c_proc(x_mpq_mul_2exp,{rop,op,bits})
end procedure

*/

//let MPFR$default_precision = 53;
let MPFR$default_precision = -17,
    MPFR$default_rounding = MPFR_RNDN;

function MPFR$precision_in_binary(/*integer*/ precision) {
    if (precision < 0n) {
        let /*mpz*/ nines = mpz_init(repeat(0X39,-precision));
        precision = mpz_sizeinbase(nines,2);
        nines = mpz_free(nines);
    }
    return precision;
}

function MPFR$precision_in_dp(/*integer*/ precision) {
//<<<
// (internal) convert a (-ve) precision specified in decimal places to binary bits
//  if (precision>=0) { crash("9/0"); }
//  let /*mpz*/ nines = mpz_init(repeat(0X39,-precision));
//  precision = mpz_sizeinbase(nines,2)+2;  // (+2 as documented in phix.chm/mpfr_set_default_prec)
//  nines = mpz_free(nines);
//>>>
// (internal) convert a (+ve) precision specified in binary bits to (-ve) decimal places
    if (precision >= 0) {
        if (precision<=0) { crash("9/0"); }
//DEV and the other one...
//      let /*mpz*/ twos = mpz_init("0b" + repeat(0X32,precision)); // nope...
        let /*mpz*/ twos = mpz_init();
        twos[MPZ$B] = 2n**BigInt(precision+1) - 1n;
        precision = -(mpz_sizeinbase(twos,10)+2);
        twos = mpz_free(twos);
    } else {
        precision -= 2;  // (+2 (yes,*+*2) as documented in phix.chm/mpfr_set_default_prec)
    }
    return precision;
}

function mpfr_set_default_precision(/*integer*/ precision) {
// set the default precision in binary bits or (if -ve) decimal places. 
//  if (precision >= 0) { precision = MPFR$precision_in_dp(precision); } else { precision -= 2; }
//  MPFR$default_precision = MPFR$precision_in_dp(precision);
    MPFR$default_precision = MPFR$precision_in_binary(precision);
} mpfr_set_default_precision.$sig="PI";

function mpfr_get_default_precision(/*boolean*/ decimal=false) {
//  let precision = -MPFR$default_precision;
    let precision = MPFR$default_precision;
//  if (!decimal) { precision = MPFR$precision_in_binary(precision); }
    if (decimal) { precision = MPFR$precision_in_dp(precision); }
    return precision;
} mpfr_get_default_precision.$sig="FO,0";

function mpfr_set_default_rounding_mode(/*integer*/ rounding) {
// Set the default rounding mode. The initial default rounding mode is to nearest (MPFR_RNDN).
    if ([MPFR_RNDN].indexOf(rounding) === -1) { crash("?9/0"); } // (placeholder)
    MPFR$default_rounding = rounding;
} mpfr_set_default_rounding_mode.$sig="PI";

function mpfr_get_default_rounding_mode() {
    return MPFR$default_rounding;
} mpfr_get_default_rounding_mode.$sig="F";

//DEV completely untested *2:
function mpfr_get_precision(/*mpfr*/ x, /*boolean*/ decimal=false) {
    let /*integer*/ precision = x[MPFR$P];
    if (decimal) { precision = MPFR$precision_in_dp(precision); }
    return precision;
} mpfr_get_precision.$sig="FOO,1";

function mpfr_set_precision(/*mpfr*/ x, /*integer*/ precision) {
//
// Reset the precision of x to be exactly prec bits, and set its value to NaN. 
// The previous value stored in x is lost. It is equivalent to a call to mpfr_clear(x) 
//  followed by a call to mpfr_init2(x, prec), but more efficient as no allocation is 
//  done in case the current allocated space for the significand of x is enough. 
// The precision prec can be any integer between MPFR_PREC_MIN and MPFR_PREC_MAX. 
// To keep the previous value stored in x, use mpfr_prec_round instead.
//
// precision is the number of bits required for the mantissa
//
//  if (precision > 0) { precision = MPFR$precision_in_dp(precision); }
    precision = MPFR$precision_in_binary(precision);
    x[MPFR$N] = 0n;
    x[MPFR$D] = 1n;
    x[MPFR$P] = precision;
} mpfr_set_precision.$sig="POI";

function mpfr_set(/*mpfr*/ tgt, src, rounding=MPFR$default_rounding) {
// set tgt from src
    tgt[MPFR$N] = src[MPFR$N];
    tgt[MPFR$D] = src[MPFR$D];
    tgt[MPFR$E] = src[MPFR$E];
    tgt[MPFR$R] = rounding;
//  tgt[MPFR$P] = MPFR$default_precision;
} mpfr_set.$sig="POOI,2";

function mpfr_set_si(/*mpfr*/ tgt, /*integer*/ i, rounding=MPFR$default_rounding) {
// set tgt from a phix integer
    tgt[MPFR$N] = BigInt(i);
    tgt[MPFR$D] = 1n;
    tgt[MPFR$E] = 0;
    tgt[MPFR$R] = rounding;
//  tgt[MPFR$P] = MPFR$default_precision;
} mpfr_set_si.$sig="POII,2";

function mpfr_set_str(/*mpfr*/ tgt, /*string*/ s, /*integer*/ base=0, rounding=MPFR$default_rounding) {
    let d = 1n,
        dx = s.indexOf('.'),
//      ex = (base>10) ? -1 : s.toLowerCase().indexOf('e'),
        ex = (base>10) ? s.indexOf('@') : s.toLowerCase().indexOf('e'),
        sx = s.indexOf('/');
//  if (ex === -1 && base > 10) { ex = s.indexOf('@'); }
    if (sx !== -1) {
        if (dx !== -1 || ex !== -1 || (base!==0 && base !== 10) ) { crash("uh?"); } // trap eg "31.4/1e1" (jic)
        d = BigInt(s.slice(sx+1));
        s = s.slice(0,sx);
    } else {    
        let dn = 0n;
        if (ex !== -1) {
            dn = MPFR$PREFIX(s.slice(ex+1),base);
            s = s.slice(0,ex);
        }
        if (dx !== -1) {
            s = s.slice(0,dx) + s.slice(dx+1);
//          if (s[0] === '-') { dx -= 1; } // NO!!
            dn -= BigInt(s.length - dx);
        }
        if (dn !== 0n) {
            let bn = base ? BigInt(base) : 10n;
            if (dn < 0n) {
                d = bn**(-dn);
            } else {
                s = MPFR$PREFIX(s,base)*bn**dn;
            }
        }
    }
//20/8/22:
//  tgt[MPFR$N] = MPFR$PREFIX(s,base);
    if (typeof(s) === "string") { s = MPFR$PREFIX(s,base); }
    tgt[MPFR$N] = s
    tgt[MPFR$D] = d;
    tgt[MPFR$E] = 0;
    tgt[MPFR$R] = rounding;
//  tgt[MPFR$P] = MPFR$default_precision;
} mpfr_set_str.$sig="POSII,2";

function mpfr_set_d(/*mpfr*/ tgt, /*atom*/ a, /*integer*/ rounding=MPFR$default_rounding) {
    let s = a.toString();
    mpfr_set_str(tgt,s,rounding);
} mpfr_set_d.$sig="PONI,2";

function mpfr_init(/*object*/ v=0, /*integer*/ precision=MPFR$default_precision, rounding=MPFR$default_rounding) {
//
// example: mpfr x = mpfr_init()
// Invoke x = mpfr_free(x) when the variable is no longer needed, see below.
//
//  precision = MPFR$precision_in_dp(precision);
    precision = MPFR$precision_in_binary(precision);
    let res = ["mpfr",0n,1n,0,rounding,precision];
    if (integer(v)) {
        // (aside: note, as per docs, the default here /is/ 0 rather than the nan of the raw C api)
        mpfr_set_si(res,v,rounding);
    } else if (atom(v)) {
        mpfr_set_d(res,v,rounding);
    } else if (string(v)) {
        mpfr_set_str(res,v,rounding);
    } else {
        crash("?9/0"); // what's v??
    }
    return res;
} mpfr_init.$sig="FOII,0";

function mpfr_inits(/*integer*/ n, /*object*/ v=0, precision=MPFR$default_precision, rounding=MPFR$default_rounding) {
//
// Eg: mpfr {x,y,z} = mpfr_init(3)
//
//  Initialise n variables to v. Obviously the result my be stored in a sequence.
//
// Invoke {x,y,z} = mpfr_free({x,y,z}) when the variables are no longer needed, see below (will occur automatically).
//
    let /*sequence*/ res = repeat(0,n);
    precision = MPFR$precision_in_binary(precision);    // (may as well convert it just the once)
    if (sequence(v) && !string(v)) {
        if (length(v)!=n) { crash("?9/0"); }
        for (let i = 1; i <= n; i += 1) {
            res[i] = mpfr_init(v[i],precision,rounding);
        }
    } else {
        for (let i = 1; i <= n; i += 1) {
            res[i] = mpfr_init(v,precision,rounding);
        }
    }
    return res;
} mpfr_inits.$sig="FIOOO,1";

function mpfr_set_q(/*mpfr*/ tgt, /*mpq*/ q, /*integer*/ rounding=MPFR$default_rounding) {
// set the mpfr rop from an mpq
    tgt[MPFR$N] = q[MPQ$N];
    tgt[MPFR$D] = q[MPQ$D];
    tgt[MPFR$E] = 0;
    tgt[MPFR$R] = rounding;
} mpfr_set_q.$sig="POOI,2";

function mpfr_set_z(/*mpfr*/ tgt, /*mpz*/ z, /*integer*/ rounding=MPFR$default_rounding) {
// set the mpfr rop from an mpz
    tgt[MPFR$N] = z[MPZ$B];
    tgt[MPFR$D] = 1n;
    tgt[MPFR$E] = 0;
    tgt[MPFR$R] = rounding;
} mpfr_set_z.$sig="POOI,2";

function mpfr_init_set(/*mpfr*/ f, /*integer*/ rounding=MPFR$default_rounding) {
    let /*mpfr*/ res = mpfr_init();
    mpfr_set(res,f,rounding);
    return res;
} mpfr_init_set.$sig="FOI,1";

function mpfr_init_set_q(/*mpq*/ q, /*integer*/ rounding=MPFR$default_rounding) {
    let /*mpfr*/ res = mpfr_init();
    mpfr_set_q(res,q,rounding);
    return res;
} mpfr_init_set_q.$sig="FOI,1";

function mpfr_init_set_z(/*mpz*/ z, /*integer*/ rounding=MPFR$default_rounding) {
    let /*mpfr*/ res = mpfr_init();
    mpfr_set_z(res,z,rounding);
    return res;
} mpfr_init_set_z.$sig="FOI,1";

let mpfr_free = mpz_free;

function MPFR$normalise(/*mpfr*/ rop, /*BigInt*/ n,d) {
    // DEV/SUG what I'd really like here is to set d to 1n, honouring MPFR$P/R, and adjust MPFR$E...
    //      (also, while d is a power of 10, just increase MPFR$E)
    //      (as is, this is just a copy of mpq_canonicalize)
// put back 9/10/22, and the n=0 check added: (this matches what boost does...)
///!*
    if (n === 0n) {
        d = 1n;
    } else if (d !== 1n) {
        let g = MPFR$GCD(n,d);
        if (g > 1n) {
            n = n/g;
            d = d/g;
        }
        if (d < 0n) {
            n = -n;
            d = -d;
        }
    }
//*!/
/*
//DEV might be worth cacheing/making this a function...
    let precision = rop[MPFR$P]
    if (precision <= 0) { crash("uh?"); }
//DEV MPFR$P should always be +ve (/binary)
//  let limn = precision<0 ? 10n**BigInt(-precision) : 1n << BigInt(precision);
    let limn = 1n << BigInt(precision);

    while (true) {
        if (d !== 1n) {
            let g = MPFR$GCD(n,d);
            if (g > 1n) {
                n = n/g;
                d = d/g;
            }
            if (d < 0n) {
                n = -n;
                d = -d;
            }
        }

        let un = n;
        if (un < 0n) { un = -un; }
        if (un === 0n) { d = 1n; }
        if (d <= limn && un <= limn) { break; }

        if (d !== 1n) {
            n >>= 1n;
            d >>= 1n;
        } else {
//DEV MPFR$E is not properly used, and there should probably be a {d/=10 -1} for un<=limn
            n /= 10n
            rop[MPFR$E] += 1;
        }
    }
*/
    rop[MPFR$N] = n;
    rop[MPFR$D] = d;
}

function mpfr_floor(/*mpfr*/ rop, op) {
// rop := floor(op)
    rop[MPFR$N] = op[MPFR$N]/op[MPFR$D];
    rop[MPFR$D] = 1n;
} mpfr_floor.$sig="POO";

function mpfr_ceil(/*mpfr*/ rop, op) {
// rop := ceil(op)
    let n = op[MPFR$N],
        d = op[MPFR$D];
//let mc = BigInt(Math.ceil(Number(n)/Number(d))),
//  nc = n/d + (n%d>0n?1n:0n);
//if (mc!=nc) {
//  printf(1," mpfr_ceil(n:%s,d:%s):mc:%s, nc:%s\n",["sequence",n.toString(10),d.toString(10),mc.toString(10),nc.toString(10)])
//}
//  rop[MPFR$N] = BigInt(Math.ceil(Number(n)/Number(d)));
    rop[MPFR$N] = n/d + (n%d>0n?1n:0n);
    rop[MPFR$D] = 1n;
} mpfr_ceil.$sig="POO";

function mpfr_neg(/*mpz*/ rop, op) {
    let n = op[MPFR$N],
        d = op[MPFR$D];
    rop[MPFR$N] = -n;
    rop[MPFR$D] = d;
} mpfr_neg.$sig="POOI,2";

function mpfr_abs(/*mpz*/ rop, op) {
    let n = op[MPFR$N],
        d = op[MPFR$D];
    if (n < 0n) { n = -n; }
    if (d < 0n) { d = -d; }
    rop[MPFR$N] = n;
    rop[MPFR$D] = d;
} mpfr_abs.$sig="POOI,2";

function mpfr_cmpabs(/*mpq*/ op1, op2) {
    let n1 = op1[MPQ$N],
        d1 = op1[MPQ$D],
        n2 = op2[MPQ$N],
        d2 = op2[MPQ$D];
    if (n1 < 0n) { n1 = -n1; }
    if (d1 < 0n) { d1 = -d1; }
    if (n2 < 0n) { n2 = -n2; }
    if (d2 < 0n) { d2 = -d2; }
    if (d1 !== d2) {
        n1 *= d2;
        n2 *= d1;
    }
    return n1 === n2 ? 0 : n1 > n2 ? 1 : -1;
} mpfr_cmpabs.$sig="FOO";

function mpfr_add(/*mpfr*/ rop, op1, op2, /*integer*/ rounding=MPFR$default_rounding) {
    // rop := op1+op2 with specified rounding 
    let n1 = op1[MPFR$N],
        d1 = op1[MPFR$D],
        n2 = op2[MPFR$N],
        d2 = op2[MPFR$D],
        lm = MPFR$LCM(d1,d2),
        a = lm / d1,
        b = lm / d2;
    a *= n1;
    b *= n2;
    a += b;
    MPFR$normalise(rop,a,lm);
} mpfr_add.$sig="POOOI,3";

function mpfr_add_si(/*mpfr*/ rop, op1, /*integer*/ op2, /*integer*/ rounding=MPFR$default_rounding) {
    let n1 = op1[MPFR$N],
        d1 = op1[MPFR$D],
        n2 = BigInt(op2),
        a = n1 + d1 * n2;
    MPFR$normalise(rop,a,d1);
} mpfr_add_si.$sig="POOII,3";

//let mpfr_add_d = mpfr_add_si;

function mpfr_add_d(/*mpfr*/ rop, op1, /*atom*/ op2, /*integer*/ rounding=MPFR$default_rounding) {
    let s = op2.toString();
    op2 = ["mpfr",0n,1n,0,rounding,MPFR$default_precision];
    mpfr_set_str(op2,s,rounding);
    mpfr_add(rop, op1, op2, rounding);
} mpfr_add_d.$sig="POONI,3";

function mpfr_sub(/*mpfr*/ rop, op1, op2, /*integer*/ rounding=MPFR$default_rounding) {
    // rop := op1-op2 with specified rounding 
    let n1 = op1[MPFR$N],
        d1 = op1[MPFR$D],
        n2 = op2[MPFR$N],
        d2 = op2[MPFR$D],
        lm = MPFR$LCM(d1,d2),
        a = lm / d1,
        b = lm / d2;
    a *= n1;
    b *= n2;
    a -= b;
    MPFR$normalise(rop,a,lm);
} mpfr_sub.$sig="POOOI,3";

function mpfr_sub_si(/*mpfr*/ rop, op1, /*integer*/ op2, /*integer*/ rounding=MPFR$default_rounding) {
    let n1 = op1[MPFR$N],
        d1 = op1[MPFR$D],
        n2 = BigInt(op2),
        a = n1 - d1 * n2;
    MPFR$normalise(rop,a,d1);
} mpfr_sub_si.$sig="POOII,3";

//let mpfr_sub_d = mpfr_sub_si;

function mpfr_sub_d(/*mpfr*/ rop, op1, /*atom*/ op2, /*integer*/ rounding=MPFR$default_rounding) {
    let s = op2.toString();
    op2 = ["mpfr",0n,1n,0,rounding,MPFR$default_precision];
    mpfr_set_str(op2,s,rounding);
    mpfr_sub(rop, op1, op2, rounding);
} mpfr_sub_d.$sig="POONI,3";

function mpfr_si_sub(/*mpfr*/ rop, /*integer*/ op1, /*mpfr*/ op2, /*integer*/ rounding=MPFR$default_rounding) {
    let n1 = BigInt(op1),
        n2 = op2[MPFR$N],
        d2 = op2[MPFR$D],
        a = d2 * n1 - n2;
    MPFR$normalise(rop,a,d2);
} mpfr_si_sub.$sig="POIOI,3";

function mpfr_mul(/*mpfr*/ rop, op1, op2, /*integer*/ rounding=MPFR$default_rounding) {
// rop := op1*op2 with specified rounding 
    let n = op1[MPQ$N] * op2[MPQ$N],
        d = op1[MPQ$D] * op2[MPQ$D];
    MPFR$normalise(rop,n,d);
} mpfr_mul.$sig="POOOI,3";

function mpfr_mul_si(/*mpfr*/ rop, op1, /*integer*/ op2, rounding=MPFR$default_rounding) {
// rop := op1*op2 with specified rounding 
    let n = op1[MPQ$N] * BigInt(op2),
        d = op1[MPQ$D];
    MPFR$normalise(rop,n,d);
} mpfr_mul_si.$sig="POOII,3";

//let mpfr_mul_d = mpfr_mul_si;

function mpfr_mul_d(/*mpfr*/ rop, op1, /*atom*/ op2, /*integer*/ rounding=MPFR$default_rounding) {
    let s = op2.toString();
    op2 = ["mpfr",0n,1n,0,rounding,MPFR$default_precision];
    mpfr_set_str(op2,s,rounding);
    mpfr_mul(rop, op1, op2, rounding);
} mpfr_mul_d.$sig="POONN,3";

function mpfr_mul_z(/*mpfr*/ rop, op1, /*mpz*/ op2, rounding=MPFR$default_rounding) {
// rop := op1*op2 with specified rounding 
    let n = op1[MPQ$N] * op2[MPZ$B],
        d = op1[MPQ$D];
    MPFR$normalise(rop,n,d);
} mpfr_mul_z.$sig="POOOO,3";

function mpfr_addmul_si(/*mpfr*/ rop, op1, /*integer*/ op2, rounding=MPFR$default_rounding) {
    let n = op1[MPQ$N] * BigInt(op2),
        d = op1[MPQ$D],
        p = op1[MPFR$P],
        tmp = ["mpfr",n,d,0,rounding,p];
    mpfr_add(rop, rop, tmp, rounding);
} mpfr_addmul_si.$sig="POOII,3";

function mpfr_div(/*mpfr*/ rop, op1, op2, /*integer*/ rounding=MPFR$default_rounding) {
// rop := op1/op2 with specified rounding 
    let n = op1[MPQ$N] * op2[MPQ$D],
        d = op1[MPQ$D] * op2[MPQ$N];
    MPFR$normalise(rop,n,d);
// (see also the alt in mpq_div)
} mpfr_div.$sig="POOOI,3";

function mpfr_si_div(/*mpfr*/ rop, /*integer*/ op1, /*mpfr*/ op2, /*integer*/ rounding=MPFR$default_rounding) {
// rop := op1/op2
    let n = op2[MPFR$D] * BigInt(op1),
        d = op2[MPFR$N];
    MPFR$normalise(rop,n,d);
} mpfr_si_div.$sig="POIOI,3";

function mpfr_div_2si(/*mpfr*/ rop, op1, /*integer*/ op2, rounding=MPFR$default_rounding) {
// rop := op1/2^op2
    let n = op1[MPFR$N],
        d = op1[MPFR$D] * 2n**BigInt(op2);
    MPFR$normalise(rop,n,d);
} mpfr_div_2si.$sig="POOII,3";

function mpfr_div_si(/*mpfr*/ rop, op1, /*integer*/ op2, rounding=MPFR$default_rounding) {
// rop := op1/op2
    let n = op1[MPFR$N],
        d = op1[MPFR$D] * BigInt(op2);
    MPFR$normalise(rop,n,d);
} mpfr_div_si.$sig="POOII,3";

//let mpfr_div_d = mpfr_div_si;

function mpfr_div_d(/*mpfr*/ rop, op1, /*atom*/ op2, /*integer*/ rounding=MPFR$default_rounding) {
    let s = op2.toString();
    op2 = ["mpfr",0n,1n,0,rounding,MPFR$default_precision];
    mpfr_set_str(op2,s,rounding);
    mpfr_div(rop, op1, op2, rounding);
} mpfr_div_d.$sig="POONN,3";

function mpfr_div_z(/*mpfr*/ rop, op1, /*mpz*/ op2, /*integer*/ rounding=MPFR$default_rounding) {
    let n = op1[MPFR$N],
        d = op1[MPFR$D] * op2[MPZ$B];
    MPFR$normalise(rop,n,d);
} mpfr_div_z.$sig="POOOI,3";

function mpfr_fmod(/*mpfr*/ r, x, y, /*integer*/ rounding=MPFR$default_rounding) {
// Set r to the value of x - ny, rounded according to the direction rnd, 
// where n is the integer quotient of x divided by y, rounded toward zero
    let ny = y[MPFR$N],
        dy = y[MPFR$D],
        n = x[MPFR$N] * dy,
        d = x[MPFR$D] * ny,
        p = x[MPFR$P];
    n = n/d;
//  r[MPFR$N] = n*ny
//  r[MPFR$D] = dy
    tmp = ["mpfr",n*ny,dy,0,rounding,p];
    mpfr_sub(r, x, tmp, rounding)
} mpfr_fmod.$sig="POOOI,3";

function mpfr_pow_si(/*mpfr*/ rop, op1, /*integer*/ op2, rounding=MPFR$default_rounding) {
    let p = BigInt(op2),
        n = op1[MPFR$N]**p,
        d = op1[MPFR$D]**p;
    MPFR$normalise(rop,n,d);
} mpfr_pow_si.$sig="POOII,3";

//function mpfr_ui_pow(/*mpfr*/ rop, /*integer*/ op1, /*mpfr*/ op2, /*integer*/ rounding=MPFR$default_rounding) {
//  let pn = op2[MPFR$N],
//      pd = op2[MPFR$D],
//      n = BigInt(op1)**pn,
//      d = BigInt(1);
//  MPFR$normalise(rop,n,d);
//}

function mpfr_ui_pow_ui(/*mpfr*/ rop, /*integer*/ op1, op2, rounding=MPFR$default_rounding) {
    let p = BigInt(op2),
        n = BigInt(op1)**p,
        d = BigInt(1);
    MPFR$normalise(rop,n,d);
} mpfr_ui_pow_ui.$sig="POIII,3";

let MPFR$ONE = NULL;

function mpfr_si_pow_si(/*mpfr*/ rop, /*integer*/ op1, op2, rounding=MPFR$default_rounding) {
    mpfr_ui_pow_ui(rop,abs(op1),abs(op2),rounding);
    if (op1<0 && odd(op2)) {
        mpfr_neg(rop,rop,rounding);
    }
    if (op2<0) {
        if (MPFR$ONE === NULL) { MPFR$ONE = mpfr_init(1); }
        mpfr_div(rop,MPFR$ONE,rop,rounding);
    }
} mpfr_si_pow_si.$sig="POIII,3";

function mpfr_get_si(/*mpfr*/ op, /*integer*/ rounding=MPFR$default_rounding) {
// res := op as an integer.
    let dn = op[MPFR$N]/op[MPFR$D],
        d = Number(dn);
    return d;
} mpfr_get_si.$sig="FOI,1";

//DEV (spotted in passing) there must be some way quite a bit a better than this...
function mpfr_get_d(/*mpfr*/ op, /*integer*/ rounding=MPFR$default_rounding) {
// res := op as a double.     v---[parenth on 10**20 is for clarity only]
    let dn = (op[MPFR$N]*(10n**20n))/op[MPFR$D],
        d = Number(dn)/1e20;
    return d;
// maybe: [need to find some test cases...]
//  let n = op[MPFR$N],
//      d = op[MPFR$D],
//      s = 1;
//  if (n < 0n) { n = -n; s = -1; }
//  if (d < 0n) { d = -d; s *= -1; }
//  while (n>1e308n || d>1e308n) {
//      n /= 2n;
//      d /= 2n;
//  }
//  d = s*Number(n)/Number(d);
//  return d;
} mpfr_get_d.$sig="FOI,1";

let mpfr_cmp = mpq_cmp;
let mpfr_cmp_si = mpq_cmp_si;


//function mpfr_pow(/*mpfr*/ rop, op1, op2, /*integer*/ rounding=MPFR$default_rounding)
// rop := op1**op2 with specified rounding 

//function mpfr_rootn_ui(/*mpfr*/ rop, op, /*integer*/ k, rounding=MPFR$default_rounding) {
/*
//  rop[MPFR$N] = 1n;
//  rop[MPFR$D] = BigInt(k);
//  mpfr_pow(rop,op,rop,rounding);
//}
    let n = op[MPFR$N],
        d = op[MPFR$D];
    rop[MPFR$R] = rounding;
    if (n < 0n) { crash("cannot get the root of a negative number!"); }
    if ((n === 1n && d === 1n) || (n === 0n)) {
        // for all k, 1n**k is 1n and 0n**k is 0n:
        rop[MPFR$N] = n;
        rop[MPFR$D] = 1n;
        rop[MPFR$E] = 0;
    } else {
*/
//  static nthRoot(x, k) {
//  if (x === BigInt(1)) return new Ratio(BigInt(1), BigInt(1));
//  if (x === BigInt(0)) return new Ratio(BigInt(0), BigInt(1));
//  if (x < 0) return new Ratio(BigInt(1), BigInt(0));
/*
  static fromNumber(x) {
    const expParse = /(-?\d)\.(\d+)e([-+])(\d+)/;
    const [, n, decimals, sgn, pow] =
      x.toExponential(PRECISION).match(expParse) || [];
    const exp = PRECISION - pm(sgn) * +pow;
    return exp < 0
      ? simplify(BigInt(`${n}${decimals}`) * exp10(-1 * exp), BigInt(1))
      : simplify(BigInt(`${n}${decimals}`), exp10(exp));
  }
*/
/*
        // Get an initial estimate using floating point math
//      let res = Ratio.fromNumber(Math.pow(Number(x), 1 / k));
        let precision = rop[MPFR$P],
            s = Math.pow(Number(x), 1 / k).toExponential(precision),
            dx = s.indexOf('.'),
            ex = s.indexOf('e'),
            e = Number(s.slice(ex+1));
        s = s.slice(0,ex);
        s = s.slice(0,dx) + s.slice(dx+1);
        n = BigInt(s);
        e = precision-e;
        if (e < 0) {
            n *= 10n**-e;
            d = 1n;
        } else {
            d = 10n**e;
        }
        k = BigInt(k);
        let km1 = k-1n;
        const NUM_ITERATIONS = 3;
//      return [...new Array(NUM_ITERATIONS)].reduce((r) => {
          // x = ((k-1)*x + (num / Math.pow(x, k-1))) * (1/k);
        for (let iter = 1; iter<=NUM_ITERATIONS; iter += 1) {
            [n,d] = [km1 * n**k + x * d**k, k * d * n**km1];
        }
        MPFR$normalise(rop,n,d);
//      return res;
//  }
*/
/*
  P.sqrt = function () {
    var r, c, t,
      x = this,
      Big = x.constructor,
      s = x.s,
      e = x.e,
      half = new Big('0.5');
    // Zero?
    if (!x.c[0]) return new Big(x);
    // Negative?
    if (s < 0) {
      throw Error(NAME + 'No square root');
    }
    // Estimate.
    s = Math.sqrt(x + '');
    // Math.sqrt underflow/overflow?
    // Re-estimate: pass x coefficient to Math.sqrt as integer, then adjust the result exponent.
    if (s === 0 || s === 1 / 0) {
      c = x.c.join('');
      if (!(c.length + e & 1)) c += '0';
      s = Math.sqrt(c);
      e = ((e + 1) / 2 | 0) - (e < 0 || e & 1);
      r = new Big((s == 1 / 0 ? '5e' : (s = s.toExponential()).slice(0, s.indexOf('e') + 1)) + e);
    } else {
      r = new Big(s + '');
    }
    e = r.e + (Big.DP += 4);
    // Newton-Raphson iteration.
    do {
      t = r;
      r = half.times(t.plus(x.div(t)));
    } while (t.c.slice(0, e).join('') !== r.c.slice(0, e).join(''));
    return round(r, (Big.DP -= 4) + r.e + 1, Big.RM);
  };
*/
//  }
//}

function mpfr_sqrt(/*mpfr*/ rop, op, /*integer*/ rounding=MPFR$default_rounding) {
// rop:=sqrt(op)
//  mpfr_rootn_ui(rop,op,2,rounding);
    // The idea here is to calculate some BigInt n such that n/(10^precision) is the right answer...
    // I suspect I may be mixing up the notion of precision with decimal places, quite wrongly...
    // If by any chance this seems to work, I'll just add a note in the docs [DEV]:
//Note: pwa/p2js assumes precision means the number of decimal places after the decimal point.
//      The mechanisms for precision and rounding are quite different in gmp and pwa/p2js, so
//      you should expect a few little glitches, especially in the first few releases.
//Alt, possibly: https://rosettacode.org/wiki/Square_root_by_hand#Phix
//  let precision = -rop[MPFR$P],
    let precision = -MPFR$precision_in_dp(rop[MPFR$P]),
        p10 = 10n**BigInt(precision),
        p102 = 10n**BigInt(precision*2),
        n = op[MPFR$N],
        d = op[MPFR$D];
    n = n * p102 / d;
    n = MPZ$nthroot(n,2);
    MPFR$normalise(rop,n,p10);
} mpfr_sqrt.$sig="POOI,2";

function mpfr_const_pi(/*mpfr*/ x, /*integer*/ rounding=MPFR$default_rounding) {
// pi is the sum of (1/16**k)(4/(8k+1)-2/(8k+4)-1/(8k+5)-1/(8k+6)) for k=0 to the no of hex digits required.
    let /*integer*/ precision = mpfr_get_precision(x), k = 0;
    let /*mpq*/ [,final_pi,pi_term] = mpq_inits(2);
    while (precision>=0) {
        mpq_set_si(pi_term,4,8*k+1);
        mpq_add_si(pi_term,pi_term,-2,8*k+4);
        mpq_add_si(pi_term,pi_term,-1,8*k+5);
        mpq_add_si(pi_term,pi_term,-1,8*k+6);
        mpq_div_2exp(pi_term,pi_term,4*k);
        mpq_add(final_pi,final_pi,pi_term);
        precision -= 4; // (one digit/nibble)
        k += 1;
    }
    mpfr_set_q(x,final_pi,rounding);
} mpfr_const_pi.$sig="POI,1";
// original (w/o rounding):
//  procedure MPFR_const_pi(mpfr x)
//  -- pi is the sum of (1/16**k)(4/(8k+1)-2/(8k+4)-1/(8k+5)-1/(8k+6)) for k=0 to the no of hex digits required.
//      integer precision = mpfr_get_precision(x), k = 0
//      mpq {final_pi,pi_term} = mpq_inits(2)
//      while precision>=0 do
//          mpq_set_si(pi_term, 4, 8*k+1)
//          mpq_add_si(pi_term, pi_term, -2, 8*k+4)
//          mpq_add_si(pi_term, pi_term, -1, 8*k+5)
//          mpq_add_si(pi_term, pi_term, -1, 8*k+6)
//          mpq_div_2exp(pi_term, pi_term, 4*k)
//          mpq_add(final_pi,final_pi,pi_term)
//          precision -= 4  -- (one digit/nibble)
//          k += 1
//      end while
//      mpfr_set_q(x,final_pi)
//  end procedure

//BPP-like formula for transcendental functions on rationals...
// I am getting close to finishing my drop-in replacement for gmp using JavaScript BigInt.
// I am emulating mpfr (float) using mpq (rational): [performance quite probably sucks but] it fits my needs.
// I have written a trival BPP estimation of PI that manages 12,000 digits in 4.5s: rubbish by (m)any standards:
// (Should anyone care to see it, there is also a surprisingly trivial solution for sqrt: httpxxx.
// The final missing bits are log/sin, etc.
//exp(x) = sum for i=0 to ~(x^i/i!)
//ln(x) = sum for i=0 to ~((x-1)^i/i) [-1<(x-1)<=1]...
//sin(x) = sum for n=1 to ~(((-1)^(n-1)*x^(2n-1))/(2n-1)!
//cos(x) = sum for n=1 to ~(((-1)^n*x^(2n-1))/(2n)!
//arctan(x) = sum for n=1 to ~(((-1)^n*x^(2n+1))/(2n+1) [covergence extremely slow as x approaches 1]

/*
function * generateDigitsOfPi() {
    let q = 1n;
    let r = 180n;
    let t = 60n;
    let i = 2n;
    while (true) {
        let digit = ((i * 27n - 12n) * q + r * 5n) / (t * 5n);
        yield Number(digit);
        let u = i * 3n;
        u = (u + 1n) * 3n * (u + 2n);
        r = u * 10n * (q * (i * 5n - 2n) + r - t * digit);
        q *= 10n * i * (i++ * 2n - 1n);
        t *= u;
    }
}

// Demo
let iter = generateDigitsOfPi();

let output = document.querySelector("div");
(function displayTenNextDigits() {
    let digits = "";
    for (let i = 0; i < 10; i++) digits += iter.next().value;
    output.insertAdjacentHTML("beforeend", digits);
    scrollTo(0, document.body.scrollHeight);
    requestAnimationFrame(displayTenNextDigits);
})();
*/

/* // chudnovsky algorithm: (needs converting to native bigint...)
const BigNumber = require('bignumber.js')

const A = new BigNumber('13591409')
const B = new BigNumber('545140134')
const C = new BigNumber('640320')
const D = new BigNumber('426880')
const E = new BigNumber('10005')
const DIGITS_PER_TERM = new BigNumber('14.1816474627254776555') // log(53360^3) / log(10)

const C3_24 = C.multipliedBy(C).multipliedBy(C).dividedToIntegerBy(24)

async function computePI(digits) {
  if (digits <= 0) {
    return '0'
  }

  const DIGITS = new BigNumber(digits)
  const N = DIGITS.dividedToIntegerBy(DIGITS_PER_TERM).plus(1)
  const PREC = DIGITS.multipliedBy(Math.log2(10))

  BigNumber.config({
    DECIMAL_PLACES: Math.ceil(PREC.toNumber()),
    POW_PRECISION: Math.ceil(PREC.toNumber()),
  })

  const PQT = await computePQT(new BigNumber(0), N)
  let PI = D.multipliedBy(E.sqrt()).multipliedBy(PQT.Q)
  PI = PI.dividedBy(A.multipliedBy(PQT.Q).plus(PQT.T))

  return PI.toFixed(digits)
}

async function computePQT(n1, n2) {
  let m = new BigNumber(0)
  let PQT = {
    P: new BigNumber(0),
    Q: new BigNumber(0),
    T: new BigNumber(0),
  }

  if (n1.plus(1).isEqualTo(n2)) {
    PQT.P = n2.multipliedBy(2).minus(1)
    PQT.P = PQT.P.multipliedBy(n2.multipliedBy(6).minus(1))
    PQT.P = PQT.P.multipliedBy(n2.multipliedBy(6).minus(5))
    PQT.Q = C3_24.multipliedBy(n2).multipliedBy(n2).multipliedBy(n2)
    PQT.T = A.plus(B.multipliedBy(n2)).multipliedBy(PQT.P)
    if (n2.modulo(2).isEqualTo(1)) {
      PQT.T = PQT.T.negated()
    }
  } else {
    m = n1.plus(n2).dividedToIntegerBy(2)
    let res1 = await new Promise((resolve) =>
      process.nextTick(async () => resolve(await computePQT(n1, m)))
    )
    let res2 = await new Promise((resolve) =>
      process.nextTick(async () => resolve(await computePQT(m, n2)))
    )
    PQT.P = res1.P.multipliedBy(res2.P)
    PQT.Q = res1.Q.multipliedBy(res2.Q)
    PQT.T = res1.T.multipliedBy(res2.Q).plus(res1.P.multipliedBy(res2.T))
  }

  return PQT
}

module.exports = computePI
*/

//mpfr_get_str(
//sequence res =     mpfr_get_str(mpfr x, integer base=10, n=0, rounding=default_rounding) --  
  /**
   * Convert to a string with a fixed number of decimal places.
   * @param {number | bigint} n The number of decimal places to return.
   */
//    toFixed(n) {
//      // TODO: Fix this so that it rounds the last digit rather than truncating
//      if (simplify(this.numerator, this.denominator).numerator < 0) {
//        return "-" + this.abs().toFixed(n);
//      }
//      if (this.isInfinity()) {
//        return this.toValue().toString();
//      }
//  //PL can we not do this as a single division, then insert the decimal point?
//      const intPart = this.numerator / this.denominator;
//      if (n === 0) {
//        return `${intPart}`;
//      }
//      const en = exp10(Number(n)),
//            decimalPart = (this.numerator * en) / this.denominator - intPart * en;
//      return `${intPart}.${leftPad(Number(n), "" + decimalPart)}`;
//    }

//Grr: mpfr_sprint does not support base. (it //does// however support [dp and ] rounding, via the fmt)
//     mpfr_get_str does not support dp, but uses precision instead...
//      DOH: multiply by 10**dp and print to zero precision! (fucketty fuck fuck: zero means "full"!!!)
//       AH try -1, check for trailing 0, then strip/shift the decimal point into place.
//      DOH we actually need the log, as per ratio.js!!!

function mpfr_get_fixed(/*mpfr*/ x, /*integer*/ dp=6, base=10, /*bool*/ comma_fill=false, /*integer*/ maxlen=0) {
//global function mpfr_get_fixed(mpfr x, integer dp=6, base=10, boolean comma_fill=false, integer maxlen=0)
    let b = BigInt(base),
//      p = BigInt(dp),
//      bp = b**p,
//      n = x[MPFR$N] * bp,
//      n = x[MPFR$N] * b**p,
        n = x[MPFR$N] * b**BigInt(dp),
        d = x[MPFR$D],
        s = 1n,
        rounding = x[MPFR$R];
    if (dp === 0) {
        while (n%d && dp<16) {
            n *= b;
            dp += 1;
        }
    }
    if (n < 0n) { s = -1n; n = -n; }
    switch (rounding) {
        // DEV nb not particularly well tested...
        case MPFR_RNDN: n += d/2n; break;
        case MPFR_RNDZ: break;
        case MPFR_RNDU: n += d-1n; break;
        case MPFR_RNDD: n = s*n; break;
        case MPFR_RNDA: n = s*n + d-1n; break;
        default: crash("unrecognised rounding mode");
    }
    n = s*(n/d);
    let res = n.toString(base);
    if (dp > 0) {
        let sgn = "";
        if (res[0] === '-') {
            sgn = "-";
            res = res.slice(1);
        }
        let l = res.length;
        if (dp >= l) {
            res = sgn + '0.' + '0'.repeat(dp-l) + res;
        } else {
            l -= dp;
            res = sgn + res.slice(0,l) + '.' + res.slice(l);
        }
        l = res.length;
        while (l>1) {
            l -= 1;
            let ch = res.charAt(l);
            if (ch === '.') { break; }
            if (ch !== '0') { l += 1; break; }
        }
        res = res.slice(0,l);
    } else if (dp < 0) {
        res += '0'.repeat(-dp);
    }
    if (comma_fill) { res = MPFR$COMMAFILL(res); }
//6/10/22:
    let /*integer*/ lr = length(res);
    if (maxlen && lr>maxlen) {
        if (comma_fill) {
            res = mpfr_get_fixed(x,dp,base,false,0);
            lr = length(res);
            if (lr<maxlen) {
                res = $conCat(res, repeat(0X3F,maxlen-lr), false); // '?'
            }
        }
        let /*bool*/ neg = equal($subse(res,1),0X2D);   // '-'
        if (neg) { res = $subss(res,2,-1); maxlen -= 1; lr -= 1; }
        let /*integer*/ dot = find(0X2E,res), e = 0;    // '.'
        // eg "0.0013" -> "1.3",e=-3
        //    "0.0003" -> "3", e=-4 
        //    "-0.000" -> return "0"
        while ((dot===2) && (equal($subse(res,1),0X30))) { // '0'
            e -= 1;
            if (lr===3) { res = $subss(res,3,3); break; }
            if (lr<=2) { return "0"; }
            res = $repss(res,1,3,$conCat($subse(res,3), ".")); // "0.xYZZZ" -> "x.YZZZ"
            lr -= 1;
        }
        dot = find(0X2E,res);
        if (dot>2) {
            // eg "123.4" -> "1.234", e+=2
            e += dot-2;
            res = $repss(res,3,dot,$subss(res,2,dot-1));
            res = $repe(res,2,0X2E);
        } else if ((dot===0) && lr>1) {
            // eg "1234" -> "1.234", e+=3
            e += lr-1;
            res = $repss(res,2,1,".");
        }
        let /*string*/ estr = sprintf("e%d",e);
        res = $subss(res,1,maxlen-neg);
        res = $repss(res,-length(estr),-1,estr);
        if (neg) { res = $conCat("-", res); }
    }
    return res;
} mpfr_get_fixed.$sig="FOIIOI,1";

/*
//sign(x) {
//  if (x === 0n) return 0n
//  return x < 0n ? -1n : 1n
//}

//const big0 = BigInt(0)
//const big1 = BigInt(1)
//const big8 = BigInt(8)    
//function bigToUint8Array(big: bigint) {
//  if (big < big0) {
//  // work out how long is the big int in bits and add 1
//  const bits: bigint = (BigInt(big.toString(2).length) / big8 + big1) * big8  
//  // create a BigInt that's 100000... of length of big + 1
//  const prefix1: bigint = big1 << bits
//  big += prefix1
//  }
//  let hex = big.toString(16)
//  if (hex.length % 2) {
//  hex = '0' + hex
//  }
//  const len = hex.length / 2
//  const u8 = new Uint8Array(len)
//  var i = 0
//  var j = 0
//  while (i < len) {
//  u8[i] = parseInt(hex.slice(j, j + 2), 16)
//  i += 1
//  j += 2
//  }
//  return u8
//}

*/

/*
// ok, ...
function primeFactors(n) {
  const factors = ["sequence"];
  let divisor = 2;
  while (n >= 2) {
    if (n % divisor == 0) {
      factors.push(divisor);
      n = n / divisor;
    } else {
      divisor++;
    }
  }
  return factors;
}
print(1, primeFactors(100));
*/

/*
function fromBuffer(buf) {
    let ret = BigInt(0);
    for (let i of buf.values()) {
        let bi = BigInt(i);
        ret = (ret << BigInt(8)) + bi;
    }
    return ret;
}
function bitLength(a) {
    let bits = 1;
    do {
        bits++;
    } while ((a >>= BigInt(1)) > BigInt(1));
    return bits;
}
*/
/*
//function toZn (a: number|bigint, n: number|bigint): bigint {
function toZn(a, n) {
    // returns a bigint with the smallest positive representation of a modulo n
    if (typeof a === 'number') a = BigInt(a);
    if (typeof n === 'number') n = BigInt(n);
    if (n <= 0n) { crash('n must be > 0'); }
    let aZn = a % n;
    if (aZn < 0n) { aZn += n; }
    return aZn;
}

//  function gcd (a: number|bigint, b: number|bigint): bigint {
//    let aAbs = (typeof a === 'number') ? BigInt(abs(a)) : abs(a) as bigint
//    let bAbs = (typeof b === 'number') ? BigInt(abs(b)) : abs(b) as bigint
//    if (aAbs === 0n) {
//      return bAbs
//    } else if (bAbs === 0n) {
//      return aAbs
//    }
//    let shift = 0n
//    while (((aAbs | bAbs) & 1n) === 0n) {
//      aAbs >>= 1n
//      bAbs >>= 1n
//      shift++
//    }
//    while ((aAbs & 1n) === 0n) aAbs >>= 1n
//    do {
//      while ((bAbs & 1n) === 0n) bAbs >>= 1n
//      if (aAbs > bAbs) {
//        const x = aAbs
//        aAbs = bAbs
//        bAbs = x
//      }
//      bAbs -= aAbs
//    } while (bAbs !== 0n)
//    // rescale
//    return aAbs << shift
//  }
//function eGcd (a: number|bigint, b: number|bigint): Egcd {
function eGcd (a, b) {
    if (typeof a === 'number') { a = BigInt(a); }
    if (typeof b === 'number') { b = BigInt(b); }
    if (a <= 0n || b <= 0n) { crash('a and b MUST be > 0'); } // a and b MUST be positive
    let x = 0n, y = 1n, u = 1n, v = 0n;
//  let [x, y, u, v] = [0n, 1n, 1n, 0n];
    while (a !== 0n) {
        const q = b / a;
//      const r: bigint = b % a;
        const r = b % a;
        const m = x - (u * q);
        const n = y - (v * q);
        b = a;
        a = r;
        x = u;
        y = v;
        u = m;
        v = n;
//      [b,a,x,y,u,v] = [a,r,u,v,m,n];
//      let q = b/a;
//      [b,a,x,y,u,v] = [a,b%a,u,v,x-u*q,y-v*q];
    }
    return { g: b, x: x, y: y };
}

//function modInv (a: number|bigint, n: number|bigint): bigint {
function modInv(a, n) {
    const egcd = eGcd(toZn(a, n), n);
    if (egcd.g !== 1n) {
        // modular inverse does not exist
        crash(`%s does not have inverse modulo %s`,["sequence",a.toString(),n.toString()])
    }
    return toZn(egcd.x, n);
}

//function modPow (b: number|bigint, e: number|bigint, n: number|bigint): bigint {
function modPow(b, e, n) {
    if (typeof b === 'number') { b = BigInt(b); }
    if (typeof e === 'number') { e = BigInt(e); }
    if (typeof n === 'number') { n = BigInt(n); }
    if (n <= 0n) { crash('n must be > 0'); }
    if (n === 1n) { return 0n; }
    b = toZn(b, n);
    if (e < 0n) {
        return modInv(modPow(b, abs(e), n), n);
    }
    let r = 1n;
    while (e > 0) {
        if ((e % 2n) === 1n) {
            r = r * b % n;
        }
        e = e / 2n;
        b = b ** 2n % n;
    }
    return r;
}
*/


//SO: JavaScript BigInt with a gmp-style API, specifically mpfr
//I am writing a transpiler from my desktop programming language to JavaScript.
//I use gmp on the desktop, so am writing a thin wrapper to mimic the same entry points but use BigInt under the hood.
//(NB Emscripten etc NOT involved) So far mpz are working pretty well, ~30 entry points done, but I have a few questions.

//Is there any (efficient) way to implement things like mpz_import() and mpz_export()?
//X (or are the internals opaque, browser-dependent, and deliberately non-accessible?)

//X... think I got this one:
//XI would not mind a few suggestions for efficiently implementing mpz_probable_prime_p() in JavaScript as well.

//mpq does not seem terribly difficult, but has anything like that already been done (using BigInt num/den internally)?
// (in truth that is a lesser question, but one I feel I have to ask just so you understand the next one.)

//mpfr could perhaps (effectively) be done as mpq with implied/capped denominator of 10^k (where k can be negative), and 
// accordingly truncated/BigInt numerator? (I expect a bit of a struggle with mpfr_const_pi(), mpfr_sin/log/exp(), etc.)

//I have studied https://github.com/MikeMcl/big.js and friends but no offence meant all that seems to pre-date BigInts.

// In short, what code needs to be in mpfr.js so that the following will work (unaltered), obviously any partial ideas,
// hints, or tips are just as welcome as a full-blown working example. You can assume (eg) mpz_get_str() is available,
// or of course you can go with using (say) BigInt.toString() etc directly. Just something to get the ball rolling.
//  <script src="mpfr.js"></script>
//  <script>
//  mpfr_set_default_prec(75);
//  let one_third = mpfr_init(1);
//  mpfr_div_ui(one_third,one_third,3);
//  console.log(mpfr_sprintf("%.75Rf",one_third);
//  </script>

//DOH: pretty sure this is mpfr_init()...!!
//ERM: do mpq first, then maybe mpfr is (effectively) mpq with implied/capped denominator of 10^k (where k can be negative), and accordingly truncated numerator?
/*
function mpfrz_init(v=0) {
    // v can be integer/atom/string/mpz
    if (mpz(v)) {
        return deep_copy(v);
    }
    if (typeof(v) !== 'string') {
//      // Minus zero?
//      n = n === 0 && 1 / n < 0 ? '-0' : String(n);        
        v = String(v);
    }
    if (!NUMERIC.test(v)) { crash("invalid number"); }
    // Determine sign.
    let sgn = +1;
    if (v.charAt(0) == '-') {
        v = v.slice(1);
        sgn = -1;
    }
    // Decimal point?
    let exponent = v.indexOf('.');
    if (exponent > -1) { v = v.replace('.', ''); }
    // Exponential form?
//DEV/SUG use find()?
    let i = v.search(/e/i);
    if (i > 0) {
        // Determine exponent.
        if (exponent < 0) exponent = i;
        exponent += +v.slice(i + 1);
        v = v.substring(0, i);
    } else if (exponent < 0) {
        // Integer.
        exponent = v.length;
    }
    let vl = v.length;
    // Determine leading zeros.
    for (i = 0; i < vl && v.charAt(i) == '0';) ++i;
    let digits;
    if (i == vl) {
        // Zero.
        digits = [exponent = 0];
    } else {
        // Determine trailing zeros.
        for (; vl > 0 && v.charAt(--vl) == '0';);
        exponent = exponent - i - 1;
        digits = [];
        // Convert string to array of digits without leading/trailing zeros.
        for (let e = 0; i <= nl;) { digits[e++] = +v.charAt(i++); }
    }
    return ["mpz",sgn,exponent,digits];
}
*/

/*
--from https://rosettacode.org/wiki/Home_primes#Phix
-- needs /mpz_prime_factors()/ [potentially transpilable], /mpz_prime()/ [maybe ditto, as per miller-rabin...].
include mpfr.e
procedure test(integer n)
    string s = sprintf("%d",n), lastp = ""
    sequence res = {s}
    while true do
        s = substitute(s,"_","")
        sequence rr = mpz_prime_factors(s,20000)
        if length(rr[$])=1 then
            lastp = rr[$][1]
            mpz p = mpz_init(lastp)
            if not mpz_prime(p)
            or length(rr)=1 then
                exit
            end if
            rr = rr[1..$-1]
            lastp = "_"&lastp
        elsif length(rr)=1 
          and rr[1][2]=1 then
            exit
        end if
        s = ""
        for i=1 to length(rr) do
            atom {prime,pow} = rr[i]
            string si = sprintf("_%d",prime)
            for p=1 to pow do
                s &= si
            end for
        end for
        if length(lastp) then
            s &= lastp
            lastp = ""
        end if
        res = append(res,s[2..$])
    end while
    integer niter = length(res)-1
    string iter = iff(niter>1?sprintf("(%d)",niter):"")
    s = sprintf("HP%d%s = ",{n,iter})
    if niter=0 then
        printf(1,"%s%d\n",{s,n})
    else
        for i=2 to niter do
            niter -= 1
            printf(1,"%sHP%s(%d)\n",{s,res[i],niter})
            if i=2 then
                s = repeat(' ',length(s))
                s[-2] = '='
            end if
        end for
        printf(1,"%s%s\n",{s,res[$]})
    end if
end procedure
papply(tagset(20,2),test)
test(65)
Output:
Using underscores to show the individual factors that were concatenated together
HP2 = 2
HP3 = 3
HP4(2) = HP2_2(1)
       = 2_11
HP5 = 5
HP6 = 2_3
HP7 = 7
HP8(13) = HP2_2_2(12)
        = HP2_3_37(11)
        = HP3_19_41(10)
        = HP3_3_3_7_13_13(9)
        = HP3_11123771(8)
        = HP7_149_317_941(7)
        = HP229_31219729(6)
        = HP11_2084656339(5)
        = HP3_347_911_118189(4)
        = HP11_613_496501723(3)
        = HP97_130517_917327(2)
        = HP53_1832651281459(1)
        = 3_3_3_11_139_653_3863_5107
HP9(2) = HP3_3(1)
       = 3_11
HP10(4) = HP2_5(3)
        = HP5_5(2)
        = HP5_11(1)
        = 7_73
HP11 = 11
HP12 = 2_2_3
HP13 = 13
HP14(5) = HP2_7(4)
        = HP3_3_3(3)
        = HP3_3_37(2)
        = HP47_71(1)
        = 13_367
HP15(4) = HP3_5(3)
        = HP5_7(2)
        = HP3_19(1)
        = 11_29
HP16(4) = HP2_2_2_2(3)
        = HP2_11_101(2)
        = HP3_11_6397(1)
        = 3_163_6373
HP17 = 17
HP18 = 2_3_3
HP19 = 19
HP20(15) = HP2_2_5(14)
         = HP3_3_5_5(13)
         = HP5_11_61(12)
         = HP11_4651(11)
         = HP3_3_12739(10)
         = HP17_194867(9)
         = HP19_41_22073(8)
         = HP709_273797(7)
         = HP3_97_137_17791(6)
         = HP11_3610337981(5)
         = HP7_3391_4786213(4)
         = HP3_3_3_3_7_23_31_1815403(3)
         = HP13_17_23_655857429041(2)
         = HP7_7_2688237874641409(1)
         = 3_31_8308475676071413
*/

/*
>> (good *2)
from https://rosettacode.org/wiki/Knuth%27s_power_tree#Phix
needs /mpfr_set_default_prec()/, mpfr_init(), mpfr_mul(), mpfr_sprintf(), mpfr_free()

from https://rosettacode.org/mw/index.php?title=Sylvester%27s_sequence#Phix
needs /mpfr_set_default_prec()/, mpfr_inits(), mpfr_set_z(), mpfr_si_div(), mpfr_si_div(), mpfr_sprintf()
include mpfr.e
mpz n = mpz_init(2), nm1 = mpz_init()
mpfr_set_default_prec(698)
mpfr {rn, tmp} = mpfr_inits(2)
for i=1 to 10 do
    if i>1 then
        mpz_sub_ui(nm1,n,1)
        mpz_mul(n,nm1,n)
        mpz_add_ui(n,n,1)
    end if
    printf(1,"%d: %s\n",{i,mpz_get_str(n)})
    mpfr_set_z(tmp,n)
    mpfr_si_div(tmp,1,tmp)
    mpfr_add(rn,rn,tmp)
end for
printf(1,"sum of reciprocals: %s\n",{shorten(mpfr_sprintf("%.211Rf",rn))})

>
from https://rosettacode.org/wiki/Almkvist-Giullera_formula_for_pi#Phix
needs /mpfr_set_default_prec()/, /mpz_fac_ui()/, /mpq_init_set_z()/, mpfr_sprintf(Rf), mpfr_init_set_q(), 
      /mpq_inits()/, /mpq_init_set_str()/, mpq_add(), /mpq_sub()/, /mpq_abs()/, /mpq_cmp()/, /mpq_set()/,
      /mpq_inv()/, mpfr_sqrt(), mpfr_const_pi(***!!!***)
requires("0.8.2") 
include mpfr.e
mpfr_set_default_prec(-70)
 
function almkvistGiullera(integer n, bool bPrint)
    mpz {t1,t2,ip} = mpz_inits(3)
    mpz_fac_ui(t1,6*n) 
    mpz_mul_si(t1,t1,32)                -- t1:=2^5*(6n)!
    mpz_fac_ui(t2,n)
    mpz_pow_ui(t2,t2,6)
    mpz_mul_si(t2,t2,3)                 -- t2:=3*(n!)^6
    mpz_mul_si(ip,t1,532*n*n+126*n+9)   -- ip:=t1*(532n^2+126n+9)
    mpz_fdiv_q(ip,ip,t2)                -- ip:=ip/t2
    integer pw := 6*n+3
    mpz_ui_pow_ui(t1,10,pw)             -- t1 := 10^(6n+3)
    mpq tm = mpq_init_set_z(ip,t1)      -- tm := rat(ip/t1)
    if bPrint then
        string ips = mpz_get_str(ip),
               tms = mpfr_sprintf("%.50Rf",mpfr_init_set_q(tm))
        tms = trim_tail(tms,"0")
        printf(1,"%d  %44s  %3d  %s\n", {n, ips, -pw, tms})
    end if
    return tm
end function
 
constant hdr = "N --------------- Integer portion -------------  Pow  ----------------- Nth term (50 dp) -----------------"
printf(1,"%s\n%s\n",{hdr,repeat('-',length(hdr))})
for n=0 to 9 do
    {} = almkvistGiullera(n, true)
end for
 
mpq {res,prev,z} = mpq_inits(3),
    prec = mpq_init_set_str(sprintf("1/1%s",repeat('0',70)))
integer n = 0
while true do
    mpq term := almkvistGiullera(n, false)
    mpq_add(res,res,term)
    mpq_sub(z,res,prev)
    mpq_abs(z,z)
    if mpq_cmp(z,prec) < 0 then
        exit
    end if
    mpq_set(prev,res)
    n += 1
end while
mpq_inv(res,res)
mpfr pi = mpfr_init_set_q(res)
mpfr_sqrt(pi,pi)
printf(1,"\nCalculation of pi took %d iterations using the Almkvist-Giullera formula.\n\n",n)
printf(1,"Pi to 70 d.p.: %s\n",mpfr_sprintf("%.70Rf",pi))
mpfr_const_pi(pi)
printf(1,"Pi (builtin) : %s\n",mpfr_sprintf("%.70Rf",pi))
Output:
N --------------- Integer portion -------------  Pow  ----------------- Nth term (50 dp) -----------------
----------------------------------------------------------------------------------------------------------
0                                            96   -3  0.096
1                                       5122560   -9  0.00512256
2                                  190722470400  -15  0.0001907224704
3                              7574824857600000  -21  0.0000075748248576
4                         312546150372456000000  -27  0.000000312546150372456
5                    13207874703225491420651520  -33  0.00000001320787470322549142065152
6                567273919793089083292259942400  -39  0.0000000005672739197930890832922599424
7           24650600248172987140112763715584000  -45  0.000000000024650600248172987140112763715584
8      1080657854354639453670407474439566400000  -51  0.0000000000010806578543546394536704074744395664
9  47701779391594966287470570490839978880000000  -57  0.00000000000004770177939159496628747057049083997888
Calculation of pi took 52 iterations using the Almkvist-Giullera formula.
Pi to 70 d.p.: 3.1415926535897932384626433832795028841971693993751058209749445923078164
Pi (builtin) : 3.1415926535897932384626433832795028841971693993751058209749445923078164
mpq
*/

/*
function integerLogarithm(value, base) {
    if (base.compareTo(value) <= 0) {
        var tmp = integerLogarithm(value, base.square(base));
        var p = tmp.p;
        var e = tmp.e;
        var t = p.multiply(base);
        return t.compareTo(value) <= 0 ? { p: t, e: e * 2 + 1 } : { p: p, e: e * 2 };
    }
    return { p: bigInt(1), e: 0 };
}

BigInteger.prototype.bitLength = function () {
    var n = this;
    if (n.compareTo(bigInt(0)) < 0) {
        n = n.negate().subtract(bigInt(1));
    }
    if (n.compareTo(bigInt(0)) === 0) {
        return bigInt(0);
    }
    return bigInt(integerLogarithm(n, bigInt(2)).e).add(bigInt(1));
}
https://stackoverflow.com/questions/70382306/logarithm-of-a-bigint
*/
