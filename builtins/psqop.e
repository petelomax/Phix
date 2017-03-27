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
-- is about 3.6x slower, however the pcase.e version is 8.4x
-- faster than the legacy wildcard.e version (on Phix, that
-- is, and about 2.7x faster on RDS Eu). Comparing the routine
-- immediately above and builtins/pcase.e, when both are run on
-- Phix, the latter is an astonishing 30.5x faster.
--
-- The bottom line is, I guess, that Phix supports (implicit)
-- sequence ops at a bearable speed loss, but there is usually
-- a much faster way in Phix. If, and not before, performance
-- of these routines causes real problems in real programs, I
-- will reconsider recoding them in assembly.
--

procedure fatal(sequence a, sequence b)
    printf(1,"sequence lengths not the same (%d!=%d)!\n",{length(a),length(b)})
    ?9/0
end procedure

--/* Not required for Phix:
--DEV: why is this global?
--global 
type string(object s)
    if not sequence(s) then return 0 end if
    for i = 1 to length(s) do
        object si = s[i]
        if not integer(si) then return 0 end if
        if si<0 or si>255 then return 0 end if
    end for
    return 1
end type
--*/

global function sq_eq(object a, object b)
    if atom(a) then
        if atom(b) then return a=b end if
        for i=1 to length(b) do
            b[i] = sq_eq(a,b[i])
        end for
        return b
    elsif atom(b) then
        for i=1 to length(a) do
            a[i] = sq_eq(a[i],b)
        end for
        return a
    end if
    if length(a)!=length(b) then fatal(a,b) end if
    for i=1 to length(a) do
        a[i] = sq_eq(a[i],b[i])
    end for
    return a
end function

global function sq_ne(object a, object b)
    if atom(a) then
        if atom(b) then return a!=b end if
        for i=1 to length(b) do
            b[i] = sq_ne(a,b[i])
        end for
        return b
    elsif atom(b) then
        for i=1 to length(a) do
            a[i] = sq_ne(a[i],b)
        end for
        return a
    end if
    if length(a)!=length(b) then fatal(a,b) end if
    for i=1 to length(a) do
        a[i] = sq_ne(a[i],b[i])
    end for
    return a
end function

global function sq_lt(object a, object b)
    if atom(a) then
        if atom(b) then return a<b end if
        for i=1 to length(b) do
            b[i] = sq_lt(a,b[i])
        end for
        return b
    elsif atom(b) then
        for i=1 to length(a) do
            a[i] = sq_lt(a[i],b)
        end for
        return a
    end if
    if length(a)!=length(b) then fatal(a,b) end if
    for i=1 to length(a) do
        a[i] = sq_lt(a[i],b[i])
    end for
    return a
end function

global function sq_le(object a, object b)
    if atom(a) then
        if atom(b) then return a<=b end if
        for i=1 to length(b) do
            b[i] = sq_le(a,b[i])
        end for
        return b
    elsif atom(b) then
        for i=1 to length(a) do
            a[i] = sq_le(a[i],b)
        end for
        return a
    end if
    if length(a)!=length(b) then fatal(a,b) end if
    for i=1 to length(a) do
        a[i] = sq_le(a[i],b[i])
    end for
    return a
end function

global function sq_gt(object a, object b)
    if atom(a) then
        if atom(b) then return a>b end if
        for i=1 to length(b) do
            b[i] = sq_gt(a,b[i])
        end for
        return b
    elsif atom(b) then
        for i=1 to length(a) do
            a[i] = sq_gt(a[i],b)
        end for
        return a
    end if
    if length(a)!=length(b) then fatal(a,b) end if
    for i=1 to length(a) do
        a[i] = sq_gt(a[i],b[i])
    end for
    return a
end function

global function sq_ge(object a, object b)
    if atom(a) then
        if atom(b) then return a>=b end if
        for i=1 to length(b) do
            b[i] = sq_ge(a,b[i])
        end for
        return b
    elsif atom(b) then
        for i=1 to length(a) do
            a[i] = sq_ge(a[i],b)
        end for
        return a
    end if
    if length(a)!=length(b) then fatal(a,b) end if
    for i=1 to length(a) do
        a[i] = sq_ge(a[i],b[i])
    end for
    return a
end function

global function sq_int(object a)
    if atom(a) then return integer(a) end if
    if string(a) then return 0 end if
    for i=1 to length(a) do
        a[i] = sq_int(a[i])
    end for
    return a
end function

global function sq_atom(object a)
    if atom(a) then return 1 end if
--DEV (added 7/8/2010, but commented out before saving/testing anything)
--  if string(a) then return 0 end if
    for i=1 to length(a) do
        a[i] = atom(a[i])   -- NB no recursion! (see note below)
    end for
    return a
end function

global function sq_str(object a)
    if atom(a) then return 0 end if
    if string(a) then return 1 end if
    for i=1 to length(a) do
        a[i] = sq_str(a[i])
    end for
    return a
end function

global function sq_seq(object a)
    if atom(a) then return 0 end if
    for i=1 to length(a) do
        a[i] = sequence(a[i])   -- NB no recursion! (see note below)
    end for
    return a
end function

-- If sq_atom()/sq_seq() used recursion, you would just get a 
-- "tree of 1's"/"tree of 0's" of the exact same size/shape.
-- It may be that these routines deserve a "nest" parameter
-- (possibly defaulted to 1), ditto for sq_int/sq_str.
-- OTOH, there are NO known uses of the above routines, anywhere!

global function sq_abs(object o)
    if atom(o) then
        return abs(o)
    end if
    for i=1 to length(o) do
        o[i] = sq_abs(o[i])
    end for
    return o
end function

global function sq_add(object a, object b)
    if atom(a) then
        if atom(b) then return a+b end if
        for i=1 to length(b) do
            b[i] = sq_add(a,b[i])
        end for
        return b
    elsif atom(b) then
        for i=1 to length(a) do
            a[i] = sq_add(a[i],b)
        end for
        return a
    end if
    if length(a)!=length(b) then fatal(a,b) end if
    for i=1 to length(a) do
        a[i] = sq_add(a[i],b[i])
    end for
    return a
end function

global function sq_sub(object a, object b)
    if atom(a) then
        if atom(b) then return a-b end if
        for i=1 to length(b) do
            b[i] = sq_sub(a,b[i])
        end for
        return b
    elsif atom(b) then
        for i=1 to length(a) do
            a[i] = sq_sub(a[i],b)
        end for
        return a
    end if
    if length(a)!=length(b) then fatal(a,b) end if
    for i=1 to length(a) do
        a[i] = sq_sub(a[i],b[i])
    end for
    return a
end function

global function sq_mul(object a, object b)
    if atom(a) then
        if atom(b) then return a*b end if
        for i=1 to length(b) do
            b[i] = sq_mul(a,b[i])
        end for
        return b
    elsif atom(b) then
        for i=1 to length(a) do
            a[i] = sq_mul(a[i],b)
        end for
        return a
    end if
    if length(a)!=length(b) then fatal(a,b) end if
    for i=1 to length(a) do
        a[i] = sq_mul(a[i],b[i])
    end for
    return a
end function

global function sq_div(object a, object b)
    if atom(a) then
        if atom(b) then return a/b end if
        for i=1 to length(b) do
            b[i] = sq_div(a,b[i])
        end for
        return b
    elsif atom(b) then
        for i=1 to length(a) do
            a[i] = sq_div(a[i],b)
        end for
        return a
    end if
    if length(a)!=length(b) then fatal(a,b) end if
    for i=1 to length(a) do
        a[i] = sq_div(a[i],b[i])
    end for
    return a
end function

global function sq_floor_div(object a, object b)
-- (equivalent to floor(sq_div(a,b)) but much faster.)
    if atom(a) then
        if atom(b) then return floor(a/b) end if
        for i=1 to length(b) do
            b[i] = sq_floor_div(a,b[i])
        end for
        return b
    elsif atom(b) then
        for i=1 to length(a) do
            a[i] = sq_floor_div(a[i],b)
        end for
        return a
    end if
    if length(a)!=length(b) then fatal(a,b) end if
    for i=1 to length(a) do
        a[i] = sq_floor_div(a[i],b[i])
    end for
    return a
end function

global function sq_rmdr(object a, object b)
    if atom(a) then
        if atom(b) then return remainder(a,b) end if
        for i=1 to length(b) do
            b[i] = sq_rmdr(a,b[i])
        end for
        return b
    elsif atom(b) then
        for i=1 to length(a) do
            a[i] = sq_rmdr(a[i],b)
        end for
        return a
    end if
    if length(a)!=length(b) then fatal(a,b) end if
    for i=1 to length(a) do
        a[i] = sq_rmdr(a[i],b[i])
    end for
    return a
end function

global function sq_floor(object a)
    if atom(a) then return floor(a) end if
    for i=1 to length(a) do
        a[i] = sq_floor(a[i])
    end for
    return a
end function

global function sq_round(object a, object inverted_precision=1)
integer len, lp
object res

    inverted_precision = sq_abs(inverted_precision)
    if atom(a) then
        if atom(inverted_precision) then
            res = floor(0.5 + (a * inverted_precision )) / inverted_precision
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

global function sq_ceil(object o)
    if atom(o) then
        o = -floor(-o)
    else
        for i=1 to length(o) do
            o[i] = sq_ceil(o[i])
        end for
    end if
    return o
end function

global function sq_sign(object o)
    if atom(o) then
        if o>0 then
            o = +1
        elsif o<0 then
            o = -1
        end if
    else
        for i=1 to length(o) do
            o[i] = sq_sign(o[i])
        end for
    end if
    return o
end function

global function sq_mod(object x, object y)
integer len, ly
object res

    if atom(x) then
        if atom(y) then
            if sign(x)=sign(y) then
                res = remainder(x,y)
            else
                res = x - y * floor(x / y)
            end if
        else
            len = length(y)
            res = repeat(0, len)
            for i=1 to len do
                res[i] = sq_mod(x, y[i])
            end for
        end if
    else
        len = length(x)
        res = repeat(0, len)
        if atom(y) then
            for i=1 to len do
                res[i] = sq_mod(x[i], y)
            end for
        else
            ly = length(y)
            if len!=ly then crash("sequence lengths not the same (%d!=%d)!\n",{len,ly}) end if
            for i=1 to len do
                res[i] = sq_mod(x[i], y[i])
            end for
        end if
    end if
    return res
end function

global function sq_trunc(atom x)
    return sq_mul(sign(x),sq_floor(abs(x)))
end function

global function sq_and(object a, object b)
    if atom(a) then
        if atom(b) then return (a and b) end if
        for i=1 to length(b) do
            b[i] = sq_and(a,b[i])
        end for
        return b
    elsif atom(b) then
        for i=1 to length(a) do
            a[i] = sq_and(a[i],b)
        end for
        return a
    end if
    if length(a)!=length(b) then fatal(a,b) end if
    for i=1 to length(a) do
        a[i] = sq_and(a[i],b[i])
    end for
    return a
end function

global function sq_or(object a, object b)
    if atom(a) then
        if atom(b) then return (a or b) end if
        for i=1 to length(b) do
            b[i] = sq_or(a,b[i])
        end for
        return b
    elsif atom(b) then
        for i=1 to length(a) do
            a[i] = sq_or(a[i],b)
        end for
        return a
    end if
    if length(a)!=length(b) then fatal(a,b) end if
    for i=1 to length(a) do
        a[i] = sq_or(a[i],b[i])
    end for
    return a
end function

global function sq_xor(object a, object b)
    if atom(a) then
        if atom(b) then return (a xor b) end if
        for i=1 to length(b) do
            b[i] = sq_xor(a,b[i])
        end for
        return b
    elsif atom(b) then
        for i=1 to length(a) do
            a[i] = sq_xor(a[i],b)
        end for
        return a
    end if
    if length(a)!=length(b) then fatal(a,b) end if
    for i=1 to length(a) do
        a[i] = sq_xor(a[i],b[i])
    end for
    return a
end function

global function sq_rand(object a)
    if atom(a) then return rand(a) end if
    for i=1 to length(a) do
        a[i] = sq_rand(a[i])
    end for
    return a
end function

global function sq_uminus(object a)
    if atom(a) then return -a end if
    for i=1 to length(a) do
        a[i] = sq_uminus(a[i])
    end for
    return a
end function

global function sq_not(object a)
    if atom(a) then return not a end if
    for i=1 to length(a) do
        a[i] = sq_not(a[i])
    end for
    return a
end function

global function sq_and_bits(object a, object b)
    if atom(a) then
        if atom(b) then return and_bits(a,b) end if
        for i=1 to length(b) do
            b[i] = sq_and_bits(a,b[i])
        end for
        return b
    elsif atom(b) then
        for i=1 to length(a) do
            a[i] = sq_and_bits(a[i],b)
        end for
        return a
    end if
    if length(a)!=length(b) then fatal(a,b) end if
    for i=1 to length(a) do
        a[i] = sq_and_bits(a[i],b[i])
    end for
    return a
end function

global function sq_or_bits(object a, object b)
    if atom(a) then
        if atom(b) then return or_bits(a,b) end if
        for i=1 to length(b) do
            b[i] = sq_or_bits(a,b[i])
        end for
        return b
    elsif atom(b) then
        for i=1 to length(a) do
            a[i] = sq_or_bits(a[i],b)
        end for
        return a
    end if
    if length(a)!=length(b) then fatal(a,b) end if
    for i=1 to length(a) do
        a[i] = sq_or_bits(a[i],b[i])
    end for
    return a
end function

global function sq_xor_bits(object a, object b)
    if atom(a) then
        if atom(b) then return xor_bits(a,b) end if
        for i=1 to length(b) do
            b[i] = sq_xor_bits(a,b[i])
        end for
        return b
    elsif atom(b) then
        for i=1 to length(a) do
            a[i] = sq_xor_bits(a[i],b)
        end for
        return a
    end if
    if length(a)!=length(b) then fatal(a,b) end if
    for i=1 to length(a) do
        a[i] = sq_xor_bits(a[i],b[i])
    end for
    return a
end function

global function sq_not_bits(object a)
    if atom(a) then return not_bits(a) end if
    for i=1 to length(a) do
        a[i] = sq_not_bits(a[i])
    end for
    return a
end function

global function sq_cos(object a)
    if atom(a) then return cos(a) end if
    for i=1 to length(a) do
        a[i] = sq_cos(a[i])
    end for
    return a
end function

global function sq_sin(object a)
    if atom(a) then return sin(a) end if
    for i=1 to length(a) do
        a[i] = sq_sin(a[i])
    end for
    return a
end function

global function sq_tan(object a)
    if atom(a) then return tan(a) end if
    for i=1 to length(a) do
        a[i] = sq_tan(a[i])
    end for
    return a
end function

include builtins\misc.e as misc
global function sq_arccos(object a)
    if atom(a) then return misc:arccos(a) end if
    for i=1 to length(a) do
        a[i] = sq_arccos(a[i])
    end for
    return a
end function

global function sq_arcsin(object a)
    if atom(a) then return misc:arcsin(a) end if
    for i=1 to length(a) do
        a[i] = sq_arcsin(a[i])
    end for
    return a
end function

global function sq_arctan(object a)
    if atom(a) then return arctan(a) end if
    for i=1 to length(a) do
        a[i] = sq_arctan(a[i])
    end for
    return a
end function

global function sq_log(object a)
    if atom(a) then return log(a) end if
    for i=1 to length(a) do
        a[i] = sq_log(a[i])
    end for
    return a
end function

global function sq_log10(object a)
    if atom(a) then return log10(a) end if
    for i=1 to length(a) do
        a[i] = sq_log10(a[i])
    end for
    return a
end function

global function sq_power(object a, object b)
    if atom(a) then
        if atom(b) then return power(a,b) end if
        for i=1 to length(b) do
            b[i] = sq_power(a,b[i])
        end for
        return b
    elsif atom(b) then
        for i=1 to length(a) do
            a[i] = sq_power(a[i],b)
        end for
        return a
    end if
    if length(a)!=length(b) then fatal(a,b) end if
    for i=1 to length(a) do
        a[i] = sq_power(a[i],b[i])
    end for
    return a
end function

global function sq_sqrt(object a)
    if atom(a) then return sqrt(a) end if
    for i=1 to length(a) do
        a[i] = sq_sqrt(a[i])
    end for
    return a
end function

--DEV not even sure these return anything different from upper/lower...
-- (ie: this might just be a sad hangover from trying to cope with the one in wildcard.e)
include builtins\pcase.e as pcase

global function sq_upper(object a)
    if integer(a) then return pcase:upper(a) end if
    if atom(a) then return a end if
    if string(a) then return pcase:upper(a) end if
    for i=1 to length(a) do
        a[i] = sq_upper(a[i])
    end for
    return a
--DEV tryme:
--  return pcase:upper(a)
end function

global function sq_lower(object a)
    if integer(a) then return pcase:lower(a) end if
    if atom(a) then return a end if
    if string(a) then return pcase:lower(a) end if
    for i=1 to length(a) do
        a[i] = sq_lower(a[i])
    end for
    return a
--DEV tryme:
--  return pcase:lower(a)
end function

