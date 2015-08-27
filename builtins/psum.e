--
-- psum.e
--
--  Phix implementation of sum()
--
-- This is an auto-include file; there is no need to manually include
--  it, unless you want a namespace.
--

global function sum(object a)
atom res = 0
    if atom(a) then
        res = a
    else
        for i=1 to length(a) do
            res += sum(a[i])
        end for
    end if
    return res
end function

