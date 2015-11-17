--
-- prnd.e (auto-include)
--

global function rnd()
-- Returns a random floating point number in the range 0.0 to 1.0 inclusive.
atom a,b,r
     a = rand(#3FFFFFFF)
     if a = 1 then return 0 end if
     b = rand(#3FFFFFFF)
     if b = 1 then return 0 end if
     if a > b then
        r = b / a
     else
        r = a / b
     end if
     return r
end function

global function sq_rnd(object a)
    if atom(a) then return rnd() end if
    for i=1 to length(a) do
        a[i] = sq_rnd(a[i])
    end for
    return a
end function

