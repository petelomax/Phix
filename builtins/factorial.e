--
-- factorial.e
--

integer finit = 0
sequence fcache

global function factorial(integer n)
--
-- Standard recursive factorial function, with memoisation.
-- eg         n : 0 1 2 3 4  5   6   7    8
--  factorial(n): 1 1 2 6 24 120 720 5040 40320 
--
atom res = 1
    if n>0 then
        if not finit then
            fcache = {}
            finit = 1
        end if
        if n<=length(fcache) then
            res = fcache[n]
            if res!=0 then return res end if
        end if
        if n>length(fcache) then
            fcache &= repeat(0,n-length(fcache))
        end if
        res = n*factorial(n-1)
        fcache[n] = res
    end if 
    return res
end function

