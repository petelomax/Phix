--
-- psum.e
--
--  Phix implementation of sum(), and now product()
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

global function product(object a)
    atom res = 1
    if atom(a) then
        res = a
    elsif length(a) then
        for i=1 to length(a) do
            res *= product(a[i])
        end for
    end if
    return res
end function

