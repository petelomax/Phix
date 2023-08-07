--
-- builtins\gcd.e
-- ==============
--
--  Implements gcd(), lcm() [aka hcf(), aliased in psym.e], and phi().
--

global function gcd(object u, atom v=0)
--
-- returns the greatest common divisor of two numbers or a sequence of numbers.
-- result is always a positive integer (that might still need storing in an atom), except for gcd(0,0) which is 0
-- atom parameters allow greater precision, but any fractional parts are immediately and deliberately discarded.
-- if the first parameter is a sequence then the second paramneter is completely ignored.
-- if the sequence contains only one element, then (floored and unsigned) it is the result, ie gcd({N}) is N.
--
    atom t
    if sequence(u) then
        v = u[1]                        -- (for the typecheck)
        t = floor(abs(v))
        for i=2 to length(u) do
            v = u[i]                    -- (for the typecheck)
            t = gcd(t,v)
        end for
        return t
    end if
    u = floor(abs(u))
    v = floor(abs(v))
    while v do
        t = u
        u = v
        v = remainder(t, v)
    end while
    return u
end function

global function lcm(object m, atom n=0)
--
-- returns the least common multiple of two numbers or a sequence of numbers
--
atom g
    if sequence(m) then
--DEV...
--      sequence s = m
        object s = m
        if not atom(m[1]) then ?9/0 end if
        m = floor(m[1])
        for i=2 to length(s) do
            n = floor(s[i])
--DEV
--          m = m/gcd(m,n)*n
            g = gcd(m,n)
            m = m/g*n
        end for
        s = {}
    else
        m = floor(m)
        n = floor(n)
--DEV
--      m = m/gcd(m,n)*n
        g = gcd(m,n)
        m = m/g*n
    end if
    return m
end function

--
--moved from:
-- builtins\phi.e (an autoinclude)
-- ==============
--
--  Implements Euler's totient function
--  double-memoised, which makes this ~16x faster.
--
include pmaths.e -- max()
integer pinit = 0   -- 0 = uninitialised, else kept as length(fact)
sequence fact,      -- smallest prime factor of n (1 if n is prime)
         totient    -- cached phi(n) values

global function phi(integer n)
    if n<=1 then return 1 end if
    if n>pinit then
        if pinit=0 then
            fact = {1}
            totient = {1}
        end if
        integer l = length(fact), adnl = max(n-l,8192-odd(l))
        fact &= repeat(1,adnl)
        totient &= repeat(0,adnl)
        integer nl = length(fact)
        for i=2 to nl do
            if fact[i]==1 then
                integer kmin = (floor((max(l,i))/i)+1)*i
--              assert(kmin>l)
                for k=kmin to nl by i do
                    if fact[k]==1 then fact[k] = i end if
                end for
            end if
        end for
--/!*   -- even though it gets rid of recursion, this made no improvement 
        -- on the 1..1e6 time compared to res=0 below, so I backed it out,
        -- since doing so ought to make make some "1 in 10" cases faster.
        for i=l+1 to nl do
            integer f = fact[i], r
            if f=1 then
                r = i-1
            else
                integer p = 1, fp = 1, j = i/f;
                while remainder(j,f)=0 do
                    p += 1
                    fp *= f
                    j /= f
                end while
                r = totient[j]*(f-1)*fp
            end if
            totient[i] = r
        end for
--*!/
        pinit = nl
    end if
    integer res = totient[n]
--/*
    if res=0 then
        integer f = fact[n]
        if f=1 then
            res = n-1
        else
            integer p = 1, fp = 1, m = n/f;
            while remainder(m,f)=0 do
                p += 1
                fp *= f
                m /= f
            end while
            res = phi(m)*(f-1)*fp
        end if
        totient[n] = res
    end if
--*/
    return res
end function

-- Just in case it helps, here's an earlier non-cached version:
--  function totient(integer n)
--      integer n0 = n, tot = n, i = 2
--      while i*i<=n do
--          if mod(n,i)=0 then
--              while true do
--                  n /= i
--                  if mod(n,i)!=0 then exit end if
--              end while
--              tot -= tot/i
--          end if
--          i += iff(i=2?1:2)
--      end while
--      if n>1 then
--          tot -= tot/n
--      end if
--      return tot
--  end function

