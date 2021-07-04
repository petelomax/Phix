--
-- psqop.e
-- =======
--
-- The functions in this file implement explicit sequence ops.
--  This file is automatically included when needed, you should only
--  need to manually include it for compatibility with RDS Eu, or you
--  can write code in the following manner:
--      --/**/ sq_add() --/* -- Phix
--              +       --*/ -- RDS Eu
--
--  (Yes, '+' is far easier to type than 'sq_add()', but...
--        '=' is far easier to type than 'equal()', and...
--      the latter is several orders of magnitude more frequent.)
--
-- As an example, the legacy sequence-op-based function lower() was:
--
--  global function lower(object x)
--  -- convert atom or sequence to lower case
--      return x + (x >= 'A' and x <= 'Z') * TO_LOWER
--  end function
--
-- (Look in pcase.e to see the official Phix version of lower(), btw.)
--
-- In Phix you could, if you really wanted, recode the above as:
--
--  global function lower(object x)
--  -- convert atom or sequence to lower case
--  sequence mask
--      -- create 1/0 mask for chars in A..Z/not in A..Z
--      mask = sq_and(sq_ge(x,'A'),sq_le(x,'Z'))
--      -- convert to TO_LOWER/0 array
--      mask = sq_mul(mask,TO_LOWER)
--      return sq_add(x,mask)
--  end function
--
-- Agreed, it is visually uglier, but probably easier to understand 
-- (rather than looking like integer add etc, you know it might be 
--  doing loads of work) and also //much easier to debug//.
--
-- Interestingly, on RDS Eu the following is not significantly any
--  slower (ok, maybe 5%) than the legacy version from wildcard.e:
--
--  global function lower(object x)
--  -- convert atom or sequence to lower case
--  sequence tmp1, tmp2
--      tmp1 = (x >= 'A')
--      tmp2 = (x <= 'Z')
--      tmp1 = (tmp1 and tmp2)
--      tmp1 = (tmp1 * TO_LOWER)
--      tmp1 = (x + tmp1)
--      return tmp1
--  end function
--
-- Admittedly, the Phix equivalent:
--
--  global function lower(object x)
--  -- convert atom or sequence to lower case
--  sequence tmp1, tmp2
--      tmp1 = sq_ge(x,'A')
--      tmp2 = sq_le(x,'Z')
--      tmp1 = sq_and(tmp1,tmp2)
--      tmp1 = sq_mul(tmp1,TO_LOWER)
--      tmp1 = sq_add(x,tmp1)
--      return tmp1
--  end function
--
--
--DEV the following timings probably pre-date pbr optimisation and should be re-done...
-- is about 3.6x slower, however the pcase.e version is 8.4x
-- faster than the legacy wildcard.e version (on Phix, that
-- is, and about 2.7x faster on RDS Eu). Comparing the routine
-- immediately above and builtins/pcase.e, when both are run on
-- Phix, the latter is an astonishing 30.5x faster.
--
-- The bottom line is, I guess, that Phix supports (explicit)
-- sequence ops at a bearable speed loss, but there is usually
-- a much faster way in Phix. If, and not before, performance
-- of these routines causes real problems in real programs, I
-- will reconsider recoding them in assembly.
--
-- 2/5/21: completely rewritten for p2js (and vastly simplified)
--         note it is the programmer's responsibility to assign
--         unique/non-clashing private/internal character codes, 
--         eg/ie '+', '-', '*', ... 'f', 'g', 'l', ... etc.
--
include builtins\pmaths.e   -- (not strictly necessary)

procedure sq_fatal(sequence a, sequence b)
    printf(1,"sequence lengths not the same (%d!=%d)!\n",{length(a),length(b)})
    ?9/0
end procedure

function sq_general(object a,b, integer fn, bool recursive=true)
    if atom(a) or not recursive then
        if atom(b) or not recursive then
            switch fn do
                case '+': a = a+b
                case '-': a = a-b
                case '*': a = a*b
                case '/': a = a/b
                case '<': a = a<b
                case '=': a = a=b
                case '>': a = a>b
                case 'a': a = a and b
                case 'c': a = compare(a,b)
                case 'f': a = floor(a/b)
                case 'g': a = a>=b
                case 'l': a = a<=b
                case 'm': a = mod(a,b)
                case 'n': a = not equal(a,b)
                case 'o': a = a or b
                case 'p': a = power(a,b)
                case 'r': a = remainder(a,b)
                case 'x': a = a xor b
                case 'A': a = and_bits(a,b)
                case 'O': a = or_bits(a,b)
                case 'M': a = max(a,b)
                case 'N': a = min(a,b)
                case 'X': a = xor_bits(a,b)
                default: ?9/0
            end switch
            return a
        end if
        integer lb = length(b)
        sequence res = repeat(iff(string(b)?' ':0),lb)
        for i=1 to lb do
            res[i] = sq_general(a,b[i],fn)
        end for
        return res
    end if
    integer la = length(a)
    sequence res = repeat(iff(string(a)?' ':0),la)
    if atom(b) then
        for i=1 to la do
            res[i] = sq_general(a[i],b,fn)
        end for
    else
        if la!=length(b) then sq_fatal(a,b) end if
        for i=1 to la do
            res[i] = sq_general(a[i],b[i],fn)
        end for
    end if
    return res
end function

function sq_unary(object a, integer fn, bool recursive=true)
    if atom(a) or not recursive then
        switch fn do
            case 'a': a = abs(a)
            case 'c': a = ceil(a)
            case 'e': a = even(a)
            case 'f': a = floor(a)
            case 'l': a = log(a)
            case '1': a = log10(a)
            case '2': a = log2(a)
            case 'n': a = not a
            case 'o': a = odd(a)
            case 'q': a = sqrt(a)
            case 'r': a = rand(a)
            case 's': a = compare(a,0)
            case 't': a = trunc(a)
            case 'u': a = -a
            case 'C': a = cos(a)
            case 'D': a = arccos(a)
            case 'I': a = int(a)
            case 'A': a = atom(a)
            case 'G': a = string(a)
            case 'Q': a = sequence(a)
            case 'R': a = arcsin(a)
            case 'S': a = sin(a)
            case 'T': a = tan(a)
            case 'U': a = arctan(a)
            case 'N': a = not_bits(a)
            default: ?9/0
        end switch
        return a
    end if
    integer la = length(a)
    sequence res = repeat(0,la)
    for i=1 to la do
        res[i] = sq_unary(a[i],fn)
    end for
    return res
end function

global function sq_cmp(object a, b) return sq_general(a,b,'c') end function
global function sq_eq(object a, b) return sq_general(a,b,'=') end function
global function sq_ne(object a, b) return sq_general(a,b,'n') end function
global function sq_lt(object a, b) return sq_general(a,b,'<') end function
global function sq_le(object a, b) return sq_general(a,b,'l') end function
global function sq_gt(object a, b) return sq_general(a,b,'>') end function
global function sq_ge(object a, b) return sq_general(a,b,'g') end function
global function sq_add(object a, b) return sq_general(a,b,'+') end function
global function sq_sub(object a, b) return sq_general(a,b,'-') end function
global function sq_mul(object a, b) return sq_general(a,b,'*') end function
global function sq_div(object a, b) return sq_general(a,b,'/') end function
global function sq_floor_div(object a, b) return sq_general(a,b,'f') end function
global function sq_rmdr(object a, b) return sq_general(a,b,'r') end function
global function sq_mod(object a, b) return sq_general(a,b,'m') end function
global function sq_and(object a, b) return sq_general(a,b,'a') end function
global function sq_or(object a, b) return sq_general(a,b,'o') end function
global function sq_xor(object a, b) return sq_general(a,b,'x') end function
global function sq_and_bits(object a, b) return sq_general(a,b,'A') end function
global function sq_or_bits(object a, b) return sq_general(a,b,'O') end function
global function sq_xor_bits(object a, b) return sq_general(a,b,'X') end function
global function sq_power(object a, b) return sq_general(a,b,'p') end function
-- 24/6/21 false removed*2, for Hourglass puzzle... (all tests pass...)
--global function sq_max(object a, b) return sq_general(a,b,'M',false) end function
global function sq_max(object a, b) return sq_general(a,b,'M') end function
--global function sq_min(object a, b) return sq_general(a,b,'N',false) end function
global function sq_min(object a, b) return sq_general(a,b,'N') end function

global function sq_abs(object a) return sq_unary(a,'a') end function
global function sq_floor(object a) return sq_unary(a,'f') end function
global function sq_ceil(object a) return sq_unary(a,'c') end function
global function sq_even(object a) return sq_unary(a,'e') end function
global function sq_sign(object a) return sq_unary(a,'s') end function
global function sq_trunc(object a) return sq_unary(a,'t') end function
global function sq_rand(object a) return sq_unary(a,'r') end function
global function sq_uminus(object a) return sq_unary(a,'u') end function
global function sq_not(object a) return sq_unary(a,'n') end function
global function sq_not_bits(object a) return sq_unary(a,'N') end function
global function sq_odd(object a) return sq_unary(a,'o') end function
global function sq_cos(object a) return sq_unary(a,'C') end function
global function sq_sin(object a) return sq_unary(a,'S') end function
global function sq_tan(object a) return sq_unary(a,'T') end function
global function sq_arccos(object a) return sq_unary(a,'D') end function
global function sq_arcsin(object a) return sq_unary(a,'R') end function
global function sq_arctan(object a) return sq_unary(a,'U') end function
global function sq_log(object a) return sq_unary(a,'l') end function
global function sq_log10(object a)  return sq_unary(a,'1') end function
global function sq_log2(object a)  return sq_unary(a,'2') end function
global function sq_sqrt(object a)  return sq_unary(a,'q') end function
global function sq_int(object a)  return sq_unary(a,'I',false) end function
global function sq_atom(object a)  return sq_unary(a,'A',false) end function
global function sq_str(object a)  return sq_unary(a,'G',false) end function
global function sq_seq(object a)  return sq_unary(a,'Q',false) end function

-- If sq_atom()/sq_seq() used recursion, you would just get a 
-- "tree of 1's"/"tree of 0's" of the exact same size/shape.
-- [ie sum(sq_atom(x)) would be === length(flatten(x)), and
--  sum(sq_seq(x)) would always be 0, for any and all x]
-- It may be that these routines deserve a "nest" parameter
-- (possibly defaulted to 1), ditto for sq_int/sq_str.
-- OTOH, there are NO known uses of the above routines, anywhere!
-- update: recursion removed for sq_int/sq_str, as per docs.

-- removed 31/3/21 (p2js):
--/*
--DEV not even sure these return anything different from upper/lower...
-- (ie: this might just be a sad hangover from trying to cope with the one in wildcard.e)
--include builtins\pcase.e as pcase
include builtins\pcase.e

global function sq_upper(object a)
--  if integer(a) then return pcase:upper(a) end if
    if integer(a) then return upper(a) end if
    if atom(a) then return a end if
--  if string(a) then return pcase:upper(a) end if
    if string(a) then return upper(a) end if
    for i=1 to length(a) do
        a[i] = sq_upper(a[i])
    end for
    return a
--DEV tryme:
--  return pcase:upper(a)
end function

global function sq_lower(object a)
--  if integer(a) then return pcase:lower(a) end if
    if integer(a) then return lower(a) end if
    if atom(a) then return a end if
--  if string(a) then return pcase:lower(a) end if
    if string(a) then return lower(a) end if
    for i=1 to length(a) do
        a[i] = sq_lower(a[i])
    end for
    return a
--DEV tryme:
--  return pcase:lower(a)
end function
--*/


global function sq_round(object a, object inverted_precision=1)
integer len, lp
object res

    inverted_precision = sq_abs(inverted_precision)
    if atom(a) then
        if atom(inverted_precision) then
--25/5/20
--          res = floor(0.5 + (a * inverted_precision )) / inverted_precision
            res = round(a,inverted_precision)
        else
            len = length(inverted_precision)
            res = repeat(0, len)
            for i=1 to len do
                res[i] = sq_round(a, inverted_precision[i])
            end for
        end if
    else
        len = length(a)
        res = repeat(0, len)
        if atom(inverted_precision) then
            for i=1 to len do
                res[i] = sq_round(a[i], inverted_precision)
            end for
        else
            lp = length(inverted_precision)
            if len!=lp then crash("sequence lengths not the same (%d!=%d)!\n",{len,lp}) end if
            for i=1 to len do
                res[i] = sq_round(a[i], inverted_precision[i])
            end for
        end if
    end if
    return res
end function

