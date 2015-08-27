--
-- permute.e
-- Copyright Pete Lomax 2004
-- see demo\permutes.exw
--

global function permute(integer n, sequence set)
--
-- return the nth permute of the given set.
-- n should be an integer in the range 1 to factorial(length(set))
--
--sequence res,rem  -- (now reuses set instead of rem)
sequence res
integer w
    n -= 1
--  res = repeat(0,length(set))
    res = set   -- so permute(n,string/seq) yields string/seq
--  rem = set -- remaining set is initially full set
    for i=length(set) to 1 by -1 do 
        w = remainder(n,i)+1
--      w = (i+1)-w         -- (slightly better ordering)
--      res[i] = rem[w]
        res[i] = set[w]
--      rem[w] = rem[i]
        set[w] = set[i]
--      set[w..i-1] = set[w+1..i]   -- (ever so "")
        n = floor(n/i)
    end for
    return res
end function
