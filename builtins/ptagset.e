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
    integer z = iff(bString?' ':0),
            l = max(0,floor((lim-start+step)/step))
    sequence res = repeat(z,l)
    for i=1 to l do
        res[i] = start
        start += step
    end for
    return res
end function

