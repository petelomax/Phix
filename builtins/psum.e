--
-- psum.e
--
--  Phix implementation of sum() and product()
--
--  This is an auto-include file; there is no need to manually include
--  it, unless you want a namespace.
--

function zl(object zlr)
    atom res
    if atom(zlr) then
        res = zlr
    elsif length(zlr)!=1 then
        crash("zlr not length 1")
    elsif not atom(zlr[1]) then
        crash("zlr[1] not atom")
    else
        res = zlr[1]
    end if
    return res
end function

global function sum(object a, zlr=0)
    atom res
    if atom(a) then
        res = a
    else
        integer l = length(a)
        if l=0 then
            res = zl(zlr)
        else
            res = 0
            for i=1 to l do
                res += sum(a[i],iff(atom(zlr)?0:zlr))
            end for
        end if
    end if
    return res
end function

global function product(object a, zlr=1)
    atom res
    if atom(a) then
        res = a
    else
        integer l = length(a)
        if l=0 then
            res = zl(zlr)
        else
            res = 1
            for i=1 to l do
                res *= product(a[i],iff(atom(zlr)?1:zlr))
            end for
        end if
    end if
    return res
end function

global function average(object a, zlr=0)
    atom res
    if atom(a) then
        res = a
    else
        integer l = length(a)
        if l=0 then
            res = zl(zlr)
        else
--          res = sum(s)/l
            res = 0
            for i=1 to l do
                res += average(a[i],iff(atom(zlr)?0:zlr))
            end for
            res /= l
        end if
    end if
    return res
end function

-- see also demo\rosetta\Standard_deviation.exw
-- no attempt is made to apply Bessel's correction.
global function std_dev(object a, zlr=0)
    atom res
    if atom(a) then
        res = a
    else
        integer l = length(a)
        if l=0 then
            res = zl(zlr)
        else
            atom mean = 0
            res = 0
            for x in a do
                if not atom(x) then x = average(x,zlr) end if
                mean += x
                res += x*x
            end for
--          mean /= l
--          res -= mean*mean*l
            res -= mean*mean/l
            res /= l
            res = sqrt(res)
        end if
    end if
    return res
end function

--/*
function digital_root(integer n)
    assert(n>=0)
    while n>9 do
        integer tot = 0
        while n>0 do
            tot += remainder(n,10)
            n = floor(n/10)
        end while
        n = tot
    end while
    return n
end function
--*/
