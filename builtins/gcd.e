--
-- gcd.e
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

