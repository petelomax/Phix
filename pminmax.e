--
-- pminmax.e
--
without trace

global function min(object a, object b)
    if a<b then return a else return b end if
end function

global function minsq(sequence s)
object res = s[1]
    for i=2 to length(s) do
        if s[i]<res then
            res = s[i]
        end if
    end for
    return res
end function

global function max(object a, object b)
    if a>b then return a else return b end if
end function

global function maxsq(sequence s)
object res = s[1]
    for i=2 to length(s) do
        if s[i]>res then
            res = s[i]
        end if
    end for
    return res
end function

