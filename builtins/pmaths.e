--
-- pmaths.e
--
--  Phix implementation of abs(), round(), ceil(), sign(), min(), max()
--
-- Note: There is no automatic-substitution-with-warning, as yet, of 
--       sq_abs, sq_round, sq_ceil, sq_sign, sq_mod, or sq_trunc.
--       Not that I have any particular objection to such, or even
--       just deleting the ones in psqop.e in favour of enhancing
--       the routines in here, to cope with sequence parameters.
--
--       There are no sq_xx versions of exp, min, max or atan2;
--       while sq_exp could be made, the other 3 c/should not.
--
-- This is an auto-include file; there is no need to manually include
--  it, unless you want a namespace.
--
without trace
include VM\pTrig.e  -- (not strictly necessary)

global function abs(atom o)
    if o<0 then
        o = -o
    end if
    return o
end function

global function exp(atom x)
    return power(E,x)
end function

global function round(atom a, atom inverted_precision=1)
    inverted_precision = abs(inverted_precision)
    return floor(0.5 + (a * inverted_precision )) / inverted_precision
end function

global function ceil(atom o)
    o = -floor(-o)
    return o
end function

global function sign(atom o)
    if o>0 then
        o = +1
    elsif o<0 then
        o = -1
    end if
    return o
end function

global function min(object a, object b)
    if a<b then return a else return b end if
end function

global function minsq(sequence s)
object res = s[1]
    for i=2 to length(s) do
        if s[i]<res then
            res = s[i]
        end if
    end for
    return res
end function

global function max(object a, object b)
    if a>b then return a else return b end if
end function

global function maxsq(sequence s)
object res = s[1]
    for i=2 to length(s) do
        if s[i]>res then
            res = s[i]
        end if
    end for
    return res
end function

global function mod(atom x, atom y)
    if equal(sign(x), sign(y)) then
        return remainder(x,y)
    end if
    return x - y * floor(x / y)
end function

global function trunc(atom x)
--  return sign(x)*floor(abs(x))
    if x<0 then
        return -floor(-x)
    end if
    return floor(x)
end function

global function atan2(atom y, atom x)
    if x>0 then
        return arctan(y/x)
    elsif x<0 then
        if y<0 then
            return arctan(y/x) - PI
        else
            return arctan(y/x) + PI
        end if
    elsif y>0 then
        return PI/2
    elsif y<0 then
        return -(PI/2)
    else
        return 0
    end if
end function

