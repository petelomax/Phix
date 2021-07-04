--
-- builtins\complex.e
--
--  Simple complex number handling.
--
-- Note: some uses on http://rosettacode.org/wiki/Arithmetic/Complex suggest that
--       norm==magnitude (aka abs), whereas complex_norm() is abs^2 in this lib.
--
--namespace complex

--global constant i = {0,1} -- i

constant REAL = 1,
         IMAG = 2

--DEV forward types needed before this can be made an autoinclude...
global type complex(object c)
    bool res = sequence(c) and length(c)=2 and atom(c[REAL]) and atom(c[IMAG])
    return res
end type

--
-- The following type allows complex or atom arguments, the latter are treated as
--  and usually immediately converted to {a,0} [ie an imaginary part of 0]. 
-- All functions return a complex, except a few which always return atom[1], and 
--  obviously complex_sprint returns a string: no routine here ever returns one
--  of these, by which I mean the type complexn of "either", so theoretically
--  you shouldn't ever need this, unless you are writing mini-shim "wrappers".
--  [1]: complex_real, complex_imag, complex_norm, complex_abs, complex_arg
--       complex_theta (~=complex_arg), and complex_rho (==complex_abs).
--
--global 
type complexn(object c)
    bool res = atom(c) or complex(c)
    return res
end type

global function complex_new(atom real, imag)
    complex res = {real,imag}
    return res
end function

global function complex_real(complexn a)
    atom res = iff(atom(a)?a:a[REAL])
    return res
end function

global function complex_imag(complexn a)
    atom res = iff(atom(a)?0:a[IMAG])
    return res
end function

global function complex_norm(complexn a)    -- (aka abs2)
--  if atom(a) then a = {a,0} end if
--  atom {r,i} = a
    atom {ar,ai} = iff(atom(a)?{a,0}:a)
    atom res = ar*ar+ai*ai
    return res
end function

global function complex_abs(complexn a)     -- (aka magnitude, modulus)
    atom res = sqrt(complex_norm(a))
    return res
end function

global function complex_add(complexn a, b)
    if atom(a) then a = {a,0} end if
    if atom(b) then b = {b,0} end if
    complex res = sq_add(a,b)
    return res
end function
 
global function complex_neg(complexn a)     -- (aka unary minus)
    if atom(a) then a = {a,0} end if
    complex res = sq_uminus(a)
    return res
end function

global function complex_sub(complexn a, b)
--  return complex_add(a,complex_neg(b))
    if atom(a) then a = {a,0} end if
    if atom(b) then b = {b,0} end if
    complex res = sq_sub(a,b)
    return res
end function

global function complex_conjugate(complexn a)
    if atom(a) then a = {a,0} end if
    complex res = {a[REAL], -1 * a[IMAG]}
    return res
end function

global function complex_mul(complexn a, b)
--  if atom(a) or atom(b) then return sq_mul(a,b) end if
--  if atom(a) then a = {a,0} end if
--  if atom(b) then b = {b,0} end if
--  return {a[REAL] * b[REAL] - a[IMAG] * b[IMAG],
--          a[REAL] * b[IMAG] + a[IMAG] * b[REAL]}
    atom {ar,ai} = iff(atom(a)?{a,0}:a)
    atom {br,bi} = iff(atom(b)?{b,0}:b)
    complex res = {ar*br - ai*bi, ar*bi + ai*br}
    return res
end function

global function complex_inv(complexn a)     -- (aka reciprocal)
    if atom(a) then a = {a,0} end if
--  atom denom = a[REAL] * a[REAL] + a[IMAG] * a[IMAG]
    atom denom = complex_norm(a)
    complex res = {a[REAL] / denom, -a[IMAG] / denom}
    return res
end function
 
global function complex_div(complexn a, b)
--  return complex_mul(a,complex_inv(b))
--  if atom(a) then a = {a,0} end if
--  if atom(b) then b = {b,0} end if
--  if atom(b) then
--      return {a[REAL]/b, a[IMAG]/b}
--  end if
--  atom bn = complex_norm(b)
--  return {(a[REAL]*b[REAL] + a[IMAG]*b[IMAG])/bn,
--          (a[IMAG]*b[REAL] - a[REAL]*b[IMAG])/bn}
    atom {ar,ai} = iff(atom(a)?{a,0}:a)
    atom {br,bi} = iff(atom(b)?{b,0}:b)
    if bi=0 then
        return {ar/br, ai/br}
    end if
--  atom bn = complex_norm(b)
    atom bn = br*br+bi*bi
    complex res = {(ar*br + ai*bi)/bn,
                   (ai*br - ar*bi)/bn}
    return res
end function

--/* -- same results (may be faster?)
global function complex_div2(complexn a, b)
    complex rc = complex_conjugate(b),
            num = complex_mul(a,rc),
            den = complex_mul(b,rc),
--          res = {num[REAL]/den[REAL],num[IMAG]/den[REAL]}
            res = sq_div(num,den[REAL])
    return res
end function
--*/

global function complex_arg(complexn a)     -- (aka angle)
    if atom(a) then a = {a,0} end if
    atom res = atan2(a[IMAG], a[REAL])
    return res
end function

global function complex_theta(complex a)    -- (aka angle, arg)
-- derived polar angle theta for polar form. Normalized to 0 <= theta < 2*PI.
    atom theta = atan2(a[IMAG],a[REAL])
    if theta<0 then theta += 2*PI end if
    return theta
end function

global function complex_rho(complex a)
-- derived polar magnitude rho for polar form.
    atom res = complex_abs(a)
    return res
end function

global function from_polar(atom rho, theta)
-- Runs Euler's polar-to-Cartesian complex conversion,
-- converting [rh0,theta] inputs into a [real, imag]-based complex number
    complex res = {rho*cos(theta),rho*sin(theta)}
    return res
end function

global function with_theta(complex c, atom theta)
-- creates new complex with same magnitude but different angle
    complex res = from_polar(complex_rho(c),theta)
    return res
end function

global function with_rho(complex c, atom rho)
-- creates new complex with same angle but different magnitude
    complex res = from_polar(rho,complex_theta(c))
    return res
end function

global function complex_log(complexn a)
    if atom(a) then a = {a,0} end if
    complex res = {0.5 * log(complex_norm(a)), complex_arg(a)}
--  return {log(complex_rho(a)),complex_theta(a)}
    return res
end function

global function complex_exp(complexn a)
    if atom(a) then a = {a,0} end if
--NO!:
--  atom e = exp(a[REAL])
--  return {e*cos(a[IMAG]), e*sin(a[REAL])}
    complex res = from_polar(exp(a[REAL]), a[IMAG])
    return res
end function

--same results as complex_exp (might be faster?):
--/*
global function complex_exp2(complexn a)
complex r
    if atom(a) then
        r = {exp(a),0}
    else
        atom {ar,ai} = a
        r = {cos(ai),sin(ai)}
        if ar!=0 then
            r = complex_mul(exp(ar),r)
        end if
    end if
    return r
end function
--*/

global function complex_power(complexn a, p)
    complex result
    if atom(a) then a = {a,0} end if
    if sequence(p) and p[2]=0 then p = p[1] end if
    if integer(p) then
        bool inverse = false
        if p<0 then
            inverse = true
            p = -p
        end if
 
        result = {1,0}
        complex multiplier = a
        while p>0 do
            if and_bits(p,1)!=0 then
                result = complex_mul(result,multiplier)
            end if
            multiplier = complex_mul(multiplier,multiplier)
            p = floor(p/2)
        end while
 
        if inverse then
            result = complex_inv(result)
        end if
    else
        result = complex_exp(complex_mul(p,complex_log(a)))
    end if
    return result
end function

--/* -- same results (probably trash?)
global function complex_power2(complexn a, p)
    if atom(a) then a = {a,0} end if
    if atom(p) then p = {p,0} end if
    complex r
    if a={0,0} and p!={0,0} then
        r = {0,0}
    elsif p={1,0} then
        r = a
    else
        r = complex_exp(complex_mul(complex_log(a),p))
    end if
    return r
end function
--*/

global function complex_sqrt(complexn a)
--  return complex_exp(complex_mul(0.5,complex_log(a)))
    complex res = complex_power(a,0.5)
    return res
end function

global function complex_sinh(complexn a)
    complex res = complex_mul(0.5,complex_sub(complex_exp(a),complex_exp(complex_neg(a))))
    return res
end function
 
global function complex_cosh(complexn a)
    complex res = complex_mul(0.5,complex_add(complex_exp(a),complex_exp(complex_neg(a))))
    return res
end function

global function complex_sin(complexn a)
    complex res = complex_div(complex_sinh(complex_mul({0,1},a)),{0,1})
    return res
end function

--/* -- same results (both return statements)
--sin(x) =  i*(-1/2*e^(i*x)+1/2*e^(-i*x))
global function complex_sin2(complexn a)
--  complex res = complex_div2(complex_sinh(complex_mul({0,1},a)),{0,1})
    complex res = complex_mul({0,1},complex_add(complex_mul(-1/2,complex_exp(complex_mul({0,+1},a))),
                                                complex_mul(+1/2,complex_exp(complex_mul({0,-1},a)))))
    return res
end function
--*/

global function complex_cos(complexn a)
    complex res = complex_cosh(complex_mul({0,1},a))
    return res
end function

global function complex_round(complex a, atom inverted_precision=1)
    if atom(a) then a = {a,0} end if
    a[REAL] = round(a[REAL],inverted_precision)
    a[IMAG] = round(a[IMAG],inverted_precision)
    return a
end function

global function complex_sprint(complexn a, bool both=false)
--
-- if both is true then 0 -> "0+0i"
--                 else 0 -> "0", and 
-- likewise {1,0} -> "1+0i" vs. "1",
--          {0,1} -> "0+i" vs. "i"
--
sequence s = ""
atom ar, ai
    {ar, ai} = iff(atom(a)?{a,0}:a)
    if ar!=0 or both then
        s = sprintf("%g",ar)
    end if
 
    if ai!=0 or both then
        if ai=1 then
            if length(s) then
                s &= "+i"
            else
                s = "i"
            end if
        elsif ai=-1 then
            s &= "-i"
        else
            if length(s) then
                s &= sprintf("%+gi",ai)
            else    
                s = sprintf("%gi",ai)
            end if
        end if
    end if
 
    if length(s)=0 then
        s = "0"
    end if
    return s
end function
 
--/* tests
complex a, b
a = { 1.0,     1.0 }
b = { 3.14159, 1.2 }
printf(1,"a = %s\n",{complex_sprint(a)})                                    -- a = 1+i
printf(1,"b = %s\n",{complex_sprint(b)})                                    -- b = 3.14159+1.2i
printf(1,"a+b = %s\n",{complex_sprint(complex_add(a,b))})                   -- a+b = 4.14159+2.2i
printf(1,"a*b = %s\n",{complex_sprint(complex_mul(a,b))})                   -- a*b = 1.94159+4.34159i
printf(1,"1/a = %s\n",{complex_sprint(complex_inv(a))})                     -- 1/a = 0.5-0.5i
printf(1," -a = %s\n",{complex_sprint(complex_neg(a))})                     --  -a = -1-i
printf(1,"conjugate(a) = %s\n",{complex_sprint(complex_conjugate(a))})      --  conjugate(a) = 1-i

a = complex_neg(a)
a = complex_add(a,a)
printf(1,"a = %s\n",{complex_sprint(a)})                                    -- a = -2-2i
b = complex_inv(a)
printf(1,"b = %s\n",{complex_sprint(b)})                                    -- b = -0.25+0.25i
b = complex_div({1,0},a)
printf(1,"b = %s\n",{complex_sprint(b)})                                    -- b = -0.25+0.25i
a = complex_mul(a,b)
printf(1,"a = %s\n",{complex_sprint(a)})                                    -- a = 1

complex x = {1,1},
        y = {2,2},
        z = complex_add(x,y)
printf(1,"z = %s\n",{complex_sprint(z)})                                    -- z = 3+3i
z=complex_mul(x,y)
printf(1,"z = %s\n",{complex_sprint(z)})                                    -- z = 4i
z=complex_inv(x)
printf(1,"z = %s\n",{complex_sprint(z)})                                    -- z = 0.5+0.5i
z=complex_neg(x)
printf(1,"z = %s\n",{complex_sprint(z)})                                    -- z = -1-1i

a = complex_new(5, 3)
b = complex_new(4, -3)
string sa = complex_sprint(a),
       sb = complex_sprint(b),
       sx = complex_sprint(complex_add(a,b)), sy, sz
printf(1,"(%s) + (%s) = %s\n",{sa,sb,sx})                               -- (5+3i) + (4-3i) = 9
        x = complex_mul(a,b)
       sx = complex_sprint(x)
printf(1,"(%s) * (%s) = %s\n",{sa,sb,sx})                               -- (5+3i) * (4-3i) = 29-3i
        z = complex_div(x,b)
--      z = complex_div2(x,b)
       sz = complex_sprint(z)
printf(1,"(%s) / (%s) = %s\n",{sx,sb,sz})                               -- (29-3i) / (4-3i) = 5+3i
       sx = complex_sprint(complex_neg(b))
printf(1,"-1 * (%s) = %s\n",{sb,sx})                                    -- -1 * (4-3i) = -4+3i
       sx = complex_sprint(complex_sub(a,b))
printf(1,"(%s) - (%s) = %s\n",{sa,sb,sx})                               -- (5+3i) - (4-3i) = 1+6i
       sx = complex_sprint(complex_inv(b))
printf(1," 1 / (%s) = %s\n",{sb,sx})                                    --   1 / (4-3i) = 0.16+0.12i
        a = complex_new(0, 1)
        sa = complex_sprint(a)
        x = complex_mul(a,a)
       sx = complex_sprint(x)
printf(1,"(%s) * (%s) = %s\n",{sa,sa,sx})                               -- (+i) * (+i) = -1
        a = complex_new(1, 1)
        b = complex_new(0, 2)
        sa = complex_sprint(a)
        sb = complex_sprint(b)
        x = complex_mul(a,b)
       sx = complex_sprint(x)
printf(1,"(%s) * (%s) = %s\n",{sa,sb,sx})                               -- (1+i) * (+2i) = -2+2i

x = complex_new(1,3)
y = complex_new(5,2)
z = complex_conjugate(x)
sx = complex_sprint(x)
sy = complex_sprint(y)
printf(1,"x = %s\n",{sx})                                           -- 1+3i
printf(1,"y = %s\n",{sy})                                           -- 5+2i
printf(1,"x + y = %s\n",{complex_sprint(complex_add(x,y))})         -- 6+5i
printf(1,"x - y = %s\n",{complex_sprint(complex_sub(x,y))})         -- -4+i
printf(1,"x * y = %s\n",{complex_sprint(complex_mul(x,y))})         -- -1+17i
printf(1,"x / y = %s\n",{complex_sprint(complex_div(x,y))})         -- 0.37931+0.448276i
--printf(1,"x / y = %s\n",{complex_sprint(complex_div2(x,y))})      -- 0.37931+0.448276i
printf(1,"  - x = %s\n",{complex_sprint(complex_neg(x))})           -- -1-3i
printf(1,"1 / x = %s\n",{complex_sprint(complex_inv(x))})           -- 0.1-0.3i
printf(1,"conjugate(x) = %s\n",{complex_sprint(z)})                 -- 1-3i
z = complex_sin(x)
printf(1,"sin(x) = %s\n",{complex_sprint(z)})                       -- 8.47165+5.41268i
--z = complex_sin2(x)
--printf(1,"sin2(x) = %s\n",{complex_sprint(z)})                        -- 8.47165+5.41268i


?complex_rho({5,3})                         -- 5.830951895
?complex_theta({5,3})                       -- 0.5404195003
?from_polar(10,PI/3)                        -- {5.0,8.660254038}
?complex_mul(complex_exp({0,PI/3}),10)      -- {5.0,8.660254038}
--?complex_mul(complex_exp2({0,PI/3}),10)   -- {5.0,8.660254038}
?complex_abs({1,2})                         -- 2.236067977
?complex_exp({0,PI})                        -- {-1,1.224606353e-16}
?complex_abs({1.5,3})                       -- 3.354101966
?complex_power({1.5,3},{1.5,1.5})           -- {-1.102482955,-0.3830641512}
?complex_power({1,1},{3,7})                 -- {0.0008081971129,-0.01155651633}
?complex_norm({1,1})                        -- 2 (see note)
?complex_abs({1,1})                         -- 1.414213562
?complex_theta({1,1})                       -- 0.7853981634
?complex_rho({1,1})                         -- 1.414213562
?complex_abs({1,1})                         -- 1.414213562
?complex_exp({1.5,3})                       -- {-4.436838552,0.6324559977}
?complex_sqrt({1,1})                        -- {1.098684113,0.4550898606}
?complex_abs({2,3})                         -- 3.605551275
?complex_arg({2,3})                         -- 0.9827937232
?complex_exp({2,3})                         -- {-7.315110095,1.042743656}
?complex_log({2,3})                         -- {1.282474679,0.9827937232}
?complex_power({1,1},{-1,0})                -- {0.5,-0.5}
--*/

--/* from the archive (dated Feb 2000), may be of some use...
-- Complex numbers
-- by: Spocky Productions
-- http://dynamicinfinity.cjb.net
-- spocky@befree.co.uk

--global type complex(object z)
---- the variable type of a complex number  
--  if atom(z) then
--      return 1
--  elsif sequence(z) and length(z)=2 and atom(z[1]) and atom(z[2]) then
--      return 1
--  else
--      return 0
--  end if
--end type

--global function cTrueComplex(complex z)
---- returns the true complex format of z: turns atoms into sequences
--if atom(z) then
--  return {z,0}
--else
--  return z
--end if
--end function

global function cGon(complex z)
-- returns the modulus and argument of z as the sequence {modulus,argument}
    atom r,theta,a,b,pi
    z=cTrueComplex(z)
    a=z[1]  b=z[2]  pi=3.141592654
    r=sqrt(a*a+b*b)
    if a>0 then
        theta=arctan(b/a)
    elsif a<0 then
        theta=arctan(b/a)+pi
    elsif b>0 then
        theta=pi/2
    elsif b<0 then
        theta=-pi/2
    else
        theta=0
    end if
    return {r,theta}
end function

global function cBin(sequence z)
-- returns the complex number of {modulus,argument}
    atom a,b,r,theta
    r=z[1]  theta=z[2]
    a=r*cos(theta)
    b=r*sin(theta)
    if (a<0 and a>-1e-8) or (a>=0 and a<1e-8) then
        a=0
    end if
    if (b<0 and b>-1e-8) or (b>=0 and b<1e-8) then
        b=0
    end if
    return {a,b}
end function

global function cFormatXY(complex z)
-- returns a string containing the visual format "(x,y)" of a complex number
    return sprintf("(%g,%g)",cTrueComplex(z))
end function

global function cFormatI(complex z)
-- returns a string containing the visual format "a+b*i" of a complex number
    sequence s
    z=cTrueComplex(z)
    if z[1]>=-0.00001 and z[1]<=0.00001 then
        if z[2]>=-0.00001 and z[2]<=0.00001 then
            s="0"
        elsif z[2]>=0.99999 and z[2]<=1.00001 then
            s="i"
        elsif z[2]>=-1.00001 and z[2]<=-0.99999 then
            s="-i"
        else
            s=sprintf("%g*i",z[2])
        end if  
    elsif z[2]<0 then
        if z[2]>=-1.00001 and z[2]<=-0.99999 then
            s=sprintf("%g-i",z[1])
        else
            s=sprintf("%g%g*i",z)
        end if  
    elsif z[2]>0 then
        if z[2]>=0.99999 and z[2]<=1.00001 then
            s=sprintf("%g+i",z[1])
        else
            s=sprintf("%g+%g*i",z)
        end if
    else
        s=sprintf("%g",z[1])
    end if
    return s
end function

global function cFormatGon(complex z)
-- returns a string containing the visual format "r*(cos(theta)+i*sin(theta))" of a complex number
    sequence g
    g=cGon(cTrueComplex(z))
    return sprintf("%g*(cos(%g)+i*sin(%g))",{g[1],g[2],g[2]})
end function

global function cFormatSGon(complex z)
-- returns a string containing the visual format "r*cis(theta)" of a complex number
    return sprintf("%g*cis(%g)",cGon(cTrueComplex(z)))
end function

global function cMul(complex x,complex y)
-- returns x*y (x and y are complex numbers)    
    atom a,b,c,d
    x=cTrueComplex(x)
    y=cTrueComplex(y)
    a=x[1]  b=x[2]  c=y[1]  d=y[2]
    return {a*c-b*d,a*d+c*b}
end function

global function cDiv(complex x,complex y)
-- returns x/y (x and y are complex numbers)    
    atom a,b,c,d
    x=cTrueComplex(x)
    y=cTrueComplex(y)
    a=x[1]  b=x[2]  c=y[1]  d=y[2]
    return {(a*c+b*d)/(c*c+d*d),(b*c-a*d)/(c*c+d*d)}
end function

global function rSqrt(atom x)
-- returns the complex number that is the POSITIVE square root of
-- the real number (positive or NEGATIVE) x
    if x<0 then
        return {0,sqrt(-x)}
    else
        return {sqrt(x),0}
    end if
end function

global function cPow(complex z,atom n)
-- returns z^n (z is a complex number)
    sequence g
    g=cGon(cTrueComplex(z))
    return cBin({power(g[1],n),n*g[2]})
end function

global function cRoot(complex z,atom n)
-- returns the complex n-th roots of the complex number z {r1,r2,...} (r1,r2,... are complex numbers)
    sequence s,g
    atom r,theta,r2,pi
    g=cGon(cTrueComplex(z))  r=g[1]  theta=g[2]  pi=3.141592654
    r2=power(r,(1/n))
    s=repeat({r2,0},n)
    for i=1 to n do
        s[i][2]=theta/n+(i-1)*2*pi/n
        s[i]=cBin(s[i])
    end for
    return s
end function

global function cSqrComp(complex a,complex b,complex c)
-- returns the 2 solutions of a*z^2 + b*z + c = 0 as {z1,z2} (z1 and z2 are complex numbers)
    complex d,z1,z2
    sequence z
    a=cTrueComplex(a)
    b=cTrueComplex(b)
    c=cTrueComplex(c)
    d=cMul(b,b)-cMul(cMul(a,c),4)
    z=cRoot(d,2)
    z1=cDiv(-b+z[1],cMul(a,2))
    z2=cDiv(-b+z[2],cMul(a,2))
    return {z1,z2}
end function
--*/

