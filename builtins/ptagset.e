--
-- ptagset.e
--
--  Phix implementation of tagset (auto include)
--

--global function tagset(integer len)
global function tagset(integer lim, start=1, step=1, bool bString=false)
--
-- When passed a single parameter, returns a sequence of integers: {1,..,lim}.
-- See the manual for other uses/examples.
--
--sequence res = repeat(0,len)
integer z = 0
    if bString then
        z = ' '
        if lim<#20
        or lim>#7E
        or start<#20
        or start>#7E then
            ?9/0
        end if
    end if
    sequence res = repeat(z,max(0,ceil((lim-start+step)/step)))
    for i=1 to length(res) do
        res[i] = start
        start += step
    end for
    return res
end function

