--
-- builtins/pfrac.e
--
--  DEV incomplete/in progress/undocumented
--  rational fraction handling
--
--without warning  -- (several unused routines in this code)
 
enum NUMER = 1, DENOM = 2, FRACLEN=$

--constant USE_BIGATOM = true
bool USE_BIGATOM = true
--bool USE_BIGINT = false

global procedure use_bigatom(bool bUseBigNum)
-- use a (slower) big num library (currently bigatom,
-- but will be switched to bigint when that is ready)
    USE_BIGATOM = bUseBigNum
end procedure
 
include bigatom.e   --*******DEV replace with bigint when/if ever ready... *******

global type frac(object r)
--  return sequence(r) and integer(r[NUMER]) and integer(r[DENOM]) and length(r)=2
    return sequence(r) 
       and length(r)=FRACLEN
       and (integer(r[NUMER]) or bigatom(r[NUMER]))
       and (integer(r[DENOM]) or bigatom(r[DENOM]))
       and ba_compare(r[DENOM],BA_ZERO)!=0
end type

global constant frac_zero = {0,1},
                frac_one  = {1,1}
 
--function normalise(object n, d=0)
function normalise(object n, d)
object g
--  if sequence(n) then
--      {n,d} = n
--  end if
if not USE_BIGATOM then
    if n=0 then return {0,1} end if
    if d<0 then
        n = -n
        d = -d
    end if
    g = gcd(n,d)
    return {n/g,d/g}
else
--if ba_compare(n,0)=0 then return {0,1} end if
--if ba_compare(n,BA_ZERO)=0 then return {BA_ZERO,BA_ONE} end if
    if ba_compare(d,0)<0 then
        n = ba_uminus(n)
        d = ba_uminus(d)
    end if
    g = ba_gcd(n,d)
    return {ba_idivide(n,g),ba_idivide(d,g)}
end if
end function

global function frac_new(object n,d=1)
    return normalise(n,d)
--X return normalise(ba_new(n),ba_new(d))
end function
 
global function frac_abs(frac r)
if not USE_BIGATOM then
    return {abs(r[NUMER]),r[DENOM]}
else
    return {ba_abs(r[NUMER]),r[DENOM]}
end if
end function

global function frac_uminus(frac r)
if not USE_BIGATOM then
    return {-r[NUMER],r[DENOM]}
else
    return {ba_uminus(r[NUMER]),r[DENOM]}
end if
end function
 
global function frac_inv(frac r)
    return reverse(r)
end function
 
global function frac_add(frac a, frac b)
object {an,ad} = a,
       {bn,bd} = b
if not USE_BIGATOM then
    return normalise(an*bd+bn*ad,ad*bd)
else
    return normalise(ba_add(ba_mul(an,bd),ba_mul(bn,ad)),ba_mul(ad,bd))
end if
end function
 
global function frac_sub(frac a, frac b)
object {an,ad} = a,
       {bn,bd} = b
if not USE_BIGATOM then
    return normalise(an*bd-bn*ad,ad*bd)
else
    return normalise(ba_sub(ba_mul(an,bd),ba_mul(bn,ad)),ba_mul(ad,bd))
end if
end function

--function ba_frac_sub(ba_frac a, ba_frac b)
--bigatom {an,ad} = a,
--      {bn,bd} = b
--  return ba_frac_normalise(ba_sub(ba_mul(an,bd),ba_mul(bn,ad)),ba_mul(ad,bd))
--end function

 
global function frac_mul(frac a, frac b)
object {an,ad} = a,
       {bn,bd} = b
if not USE_BIGATOM then
    return normalise(an*bn,ad*bd)
else
    return normalise(ba_mul(an,bn),ba_mul(ad,bd))
end if
end function
 
global function frac_div(frac a, frac b)
object {an,ad} = a,
       {bn,bd} = b
if not USE_BIGATOM then
    return normalise(an*bd,ad*bn)
else
    return normalise(ba_mul(an,bd),ba_mul(ad,bn))
end if
end function
 
global function frac_eq(frac a, frac b)
if not USE_BIGATOM then
    return a==b
else
    return ba_compare(a[NUMER],b[NUMER])==0
       and ba_compare(a[DENOM],b[DENOM])==0
end if
end function
 
global function frac_ne(frac a, frac b)
if not USE_BIGATOM then
    return a!=b
else
    return ba_compare(a[NUMER],b[NUMER])!=0
        or ba_compare(a[DENOM],b[DENOM])!=0
end if
end function
 
global function frac_lt(frac a, frac b)
if not USE_BIGATOM then
    return frac_sub(a,b)[NUMER]<0
else
    return ba_compare(frac_sub(a,b)[NUMER],0)<0
end if
end function
 
global function frac_gt(frac a, frac b)
if not USE_BIGATOM then
    return frac_sub(a,b)[NUMER]>0
else
    return ba_compare(frac_sub(a,b)[NUMER],0)>0
end if
end function
 
global function frac_le(frac a, frac b)
if not USE_BIGATOM then
    return frac_sub(a,b)[NUMER]<=0
else
    return ba_compare(frac_sub(a,b)[NUMER],0)<=0
end if
end function
 
global function frac_ge(frac a, frac b)
if not USE_BIGATOM then
    return frac_sub(a,b)[NUMER]>=0
else
    return ba_compare(frac_sub(a,b)[NUMER],0)>=0
end if
end function

global function frac_sprint(frac a, bool asPair=false)
-- if asPair is true, returns {num_str,denom_str} to allow
-- long fractions to have embedded \n etc.
sequence res
    if not asPair then
        if frac_eq(a,frac_one) then return "1" end if
        if frac_eq(a,frac_zero) then return "0" end if
    end if
if not USE_BIGATOM then
    if asPair then
        res = {sprintf("%d",a[NUMER]),sprintf("%d",a[DENOM])}
    else
        res = sprintf("%d",a[NUMER])
        if a[DENOM]!=1 then
            res &= sprintf("%d",a[DENOM])
        end if
    end if
else
    if asPair then
        res = {ba_sprint(a[NUMER]),ba_sprint(a[DENOM])}
    else
        res = ba_sprint(a[NUMER])
        if a[DENOM]!=BA_ONE then
            res &= "/"&ba_sprint(a[DENOM]) 
        end if
    end if
end if
    return res
end function

--test case (from http://rosettacode.org/wiki/Arithmetic/Rational#Phix)
-- (from prior to incorporating bigatom) [still works, but takes (est) 1 hour instead of 15s, when USE_BIGATOM is true]
--with a 2 second t1 diag:
--bigatom=false:
--"started"
--524288 (limit)
--perfect: 6
--perfect: 28
--perfect: 496
--perfect: 8128
--94460
--170550
--239897
--302763
--361687
--421080
--478589
--elapsed: 15.70 seconds
--perfect: 33550336
--Done!
--bigatom=false, for about the same running time, so it is about 260x slower:
--"started"
--524288 (limit)
--perfect: 6
--perfect: 28
--480
--perfect: 496
--804
--1075
--1320
--1562
--1798
--2028

--/*
function is_perfect(integer num)
frac tot = frac_new(0)
sequence f = factors(num,1)
    for i=1 to length(f) do
        tot = frac_add(tot,frac_new(1,f[i]))
    end for
    return frac_eq(tot,frac_new(2))
end function
 
procedure get_perfect_numbers()
atom t0 = time()
    for i=2 to power(2,19) do
        if is_perfect(i) then
            printf(1,"perfect: %d\n",i)
        end if
    end for
    printf(1,"elapsed: %3.2f seconds\n",time()-t0)
 
    integer pn5 = power(2,12)*(power(2,13)-1) -- 5th perfect number
    if is_perfect(pn5) then
        printf(1,"perfect: %d\n",pn5)
    end if
end procedure
 
get_perfect_numbers()
Output:
perfect: 6
perfect: 28
perfect: 496
perfect: 8128
elapsed: 13.56 seconds
perfect: 33550336
--*/

--/*
from http://rosettacode.org/wiki/Bernoulli_numbers#Phix
--include builtins\bigatom.e
-- 
--constant NUMER = 1, DENOM = 2
-- 
--type ba_frac(object r)
--  return sequence(r) and length(r)=2 and bigatom(r[NUMER]) and bigatom(r[DENOM])
--end type
 
--(slightly less, copy made...)
--function ba_gcd(bigatom u, bigatom v)
--bigatom t
--  u = ba_floor(ba_abs(u))
--  v = ba_floor(ba_abs(v))
--  while v!=BA_ZERO do
--      t = u
--      u = v
--      v = ba_remainder(t, v)
--  end while
--  return u
--end function
 
--function ba_frac_normalise(bigatom n, bigatom d)
--bigatom g
--  if ba_compare(d,BA_ZERO)<0 then
--      n = ba_sub(0,n)
--      d = ba_sub(0,d)
--  end if
--  g = ba_gcd(n,d)
--  return {ba_idivide(n,g),ba_idivide(d,g)}
--end function
 
--function ba_frac_sub(ba_frac a, ba_frac b)
--bigatom {an,ad} = a,
--      {bn,bd} = b
--  return ba_frac_normalise(ba_sub(ba_multiply(an,bd),ba_multiply(bn,ad)),ba_multiply(ad,bd))
--end function
 
--function ba_frac_mul(ba_frac a, ba_frac b)
--bigatom {an,ad} = a,
--      {bn,bd} = b
--  return ba_frac_normalise(ba_multiply(an,bn),ba_multiply(ad,bd))
--end function

--- and ---

sequence a = {}
for m=0 to 60 do
    a = append(a,{ba_new(1),ba_new(m+1)})
    for j=m to 1 by -1 do
        a[j] = ba_frac_mul({ba_new(j),ba_new(1)},ba_frac_sub(a[j+1],a[j]))
    end for
    if a[1][1]!=BA_ZERO then
        printf(1,"B(%2d) = %44s / %s\n",{m,ba_sprint(a[1][1]),ba_sprint(a[1][2])})
    end if
end for
--*/
