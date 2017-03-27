--
-- ptagset.e
--
--  Phix implementation of tagset (auto include)
--

--global function tagset(integer len)
global function tagset(integer lim, start=1, step=1)
--
-- When passed a single parameter, returns a sequence of integers: {1,..,lim}.
-- See the manual for other uses/examples.
--
--sequence res = repeat(0,len)
sequence res = repeat(0,max(0,ceil((lim-start+step)/step)))
--  for i=1 to len do
--      res[i] = i
--  end for
    for i=1 to length(res) do
        res[i] = start
        start += step
    end for
    return res
end function

