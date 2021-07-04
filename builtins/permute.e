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
    integer l = length(set)
    sequence res = repeat(iff(string(set)?' ':0),l)
    n -= 1
    set = deep_copy(set,1)  -- (top level only needed)
    for i=l to 1 by -1 do 
        integer w = remainder(n,i)+1
--      w = (i+1)-w         -- (slightly better ordering)
        res[i] = set[w]
        set[w] = set[i]
--      set[w..i-1] = set[w+1..i]   -- (ever so "")
        n = floor(n/i)
    end for
    return res
end function

--DEV these are non-unique/"impossible" combinations... more thought required<br>
--/*
global function combination(integer k, n, sequence set)
--
-- return the kth combination of length n items from the given set.
-- k should be an integer in the range 1 to power(length(set),n)
--
    integer l = length(set)
    if k<1 or k>power(l,n) then ?9/0 end if
    k -= 1
    sequence res = repeat(' ',n)
    for i=n to 1 by -1 do
        integer m = remainder(k,l)+1
        res[i] = set[m]
        k = floor(k/l)
    end for
    return res
end function

--/!*
sequence set = "123",
         res = repeat(0,power(length(set),3))
for k=1 to length(res) do
    res[k] = combination(k,3,set)
end for
?length(res)
puts(1,join_by(res,1,9))
puts(1,"===\n")
puts(1,join_by(res,3,9))
--*!/

--/*
27
111   112   113   121   122   123   131   132   133
211   212   213   221   222   223   231   232   233
311   312   313   321   322   323   331   332   333
===
111   121   131   211   221   231   311   321   331
112   122   132   212   222   232   312   322   332
113   123   133   213   223   233   313   323   333
"done"

sequence set = "1234",
         res = repeat(0,power(length(set),3))
for k=1 to length(res) do
    res[k] = combination(k,3,set)
end for
?length(res)
puts(1,join_by(res,1,16))
puts(1,"===\n")
puts(1,join_by(res,4,16))
--*/

--/*
sequence set = "1234",
         res = repeat(0,power(length(set),4))
for k=1 to length(res) do
    res[k] = combination(k,4,set)
end for
?length(res)
puts(1,join_by(res,1,16))
puts(1,"===\n")
puts(1,join_by(res,16,16))
--*/

--*/
