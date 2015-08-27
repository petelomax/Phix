--
-- factorial.e
--

global function factorial(integer n)
--
-- Bog standard factorial function.
-- eg         n : 0 1 2 3 4  5   6   7    8
--  factorial(n): 1 1 2 6 24 120 720 5040 40320 
--
atom res
    res = 1
    while n>1 do
        res *= n
        n -= 1
    end while
    return res
end function

