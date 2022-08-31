--
-- ptagset.e
--
--  Phix implementation of tagset (auto include)
--
global function tagset(integer lim, start=1, step=1)
--
-- When passed a single parameter, returns a sequence of integers: {1,..,lim}.
-- See the manual for other uses/examples.
--
    bool bString = (start>=' ' and start<='~' and lim>=' ' and lim<='~')
    integer l = max(0,floor((lim-start+step)/step))
    sequence res = repeat(iff(bString?' ':0),l)
    for i=1 to l do
        res[i] = start
        start += step
    end for
    return res
end function

global function tagstart(integer start, len, step=1)
--
-- Sometimes you don't know or really care where it ends, 
-- for instance tagstart('A',5) is "ABCDE", and sometimes
-- tagstart(1,26) is just more natural than tagset(26,1).
--
    return tagset(start+(len-1)*step,start,step)
end function
    

