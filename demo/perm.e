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
sequence res,rem
integer w
    n -= 1
    res = repeat(0,length(set))
    rem = set -- remaining set is initially full set
    for i=length(set) to 1 by -1 do 
        w = remainder(n,i)+1
        res[i] = rem[w]
        rem[w] = rem[i]
        n = floor(n/i)
    end for
    return res
end function
