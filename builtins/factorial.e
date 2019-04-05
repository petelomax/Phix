--
-- factorial.e
--

integer finit = 0
sequence fcache

global function factorial(integer n)
--
--3/10/18:
--X Standard recursive factorial function, with memoisation.
-- Standard iterative factorial function, with memoisation.
-- eg         n : 0 1 2 3 4  5   6   7    8
--  factorial(n): 1 1 2 6 24 120 720 5040 40320 
--
atom res = 1
    if n>0 then
        if not finit then
--          fcache = {}
            fcache = {1}
            finit = 1
        end if
--      if n<=length(fcache) then
--          res = fcache[n]
--          if res!=0 then return res end if
--      end if
--      if n>length(fcache) then
--          fcache &= repeat(0,n-length(fcache))
--      end if
--      res = n*factorial(n-1)
--      fcache[n] = res
        for i=length(fcache)+1 to n do
            fcache &= fcache[$]*i
        end for
        res = fcache[n]
    end if 
    return res
end function

global function k_perm(integer n, k)
-- standard partial permutations calculation (sequences without repetition)
atom res = n
    for i=n-1 to n-k+1 by -1 do
        res *= i
    end for
    return res
end function

global function choose(integer n, k)
-- standard combinations calculation - choose k from n aka "n choose k"
atom res = k_perm(n,k)/factorial(k)
    return res
end function

