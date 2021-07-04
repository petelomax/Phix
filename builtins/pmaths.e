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
--include VM\pTrig.e    -- (not strictly necessary)

global function abs(atom a)
    if a<0 then
        a = -a
    end if
    return a
end function

global function sign(atom a)
    if a>0 then
        a = +1
    elsif a<0 then
        a = -1
    end if
    return a
end function

global function even(atom a)
    return and_bits(a,1)=0
end function

global function odd(atom a)
    return and_bits(a,1)=1
end function

global function exp(atom a)
    return power(E,a)
end function

--bool bUseBankersRounding = false

global function round(atom a, atom inverted_precision=1)
--  if inverted_precision=0 then
--      if a!=true and a!=false then ?9/0 end if
--      bUseBankersRounding = a
--  elsif bUseBankersRounding then
--      --25/5/20 (round to nearest even)
--      integer s = sign(a)
--      a = abs(a)*inverted_precision
--      atom t = floor(a), f = a-t
--      if f=0.5 then
--          a = t+and_bits(t,1)
--      else
--          a = floor(0.5+a)
--      end if
--      a = s*(a/inverted_precision)
--  else
        -- (compatible with Euphoria)
        inverted_precision = abs(inverted_precision)
        a = floor(0.5 + (a * inverted_precision )) / inverted_precision
--  end if
    return a
end function

global function bankers_rounding(atom pence, integer precision=1)
    integer pennies, -- (or nearest 100, etc, but never nearest < 1 )
            s = sign(pence), whole
    pence = abs(pence)/precision
    whole = floor(pence)
    atom fract = pence-whole
    if fract=0.5 then
        pennies = whole+and_bits(whole,1)
    else
        pennies = floor(0.5+pence)
    end if
    pennies *= s*precision
    return pennies
end function


global function ceil(atom o)
    o = -floor(-o)
    return o
end function

global function min(object a, object b)
    if a<b then return a else return b end if
end function

global function minsq(sequence s, bool return_index=false)
object res = s[1]
integer rdx = 1
    for i=2 to length(s) do
        if s[i]<res then
            res = s[i]
            rdx = i
        end if
    end for
    return iff(return_index?rdx:res)
end function

global function max(object a, object b)
    if a>b then return a else return b end if
end function

global function maxsq(sequence s, bool return_index=false)
object res = s[1]
integer rdx = 1
    for i=2 to length(s) do
        if s[i]>res then
            res = s[i]
            rdx = i
        end if
    end for
    return iff(return_index?rdx:res)
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
--    return 2*arctan((sqrt(power(x,2)+power(y,2))-x)/y)
end function

