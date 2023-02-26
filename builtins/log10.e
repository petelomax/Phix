--
-- log10.e
--

global function log10(atom n)
--Added 26/9/22:
    if integer(n) and n>0 then
        -- (sadly not quite as efficient as log2)   
        atom t = 1, r = 0
        while t<=n do
            if t=n then return r end if
            t *= 10
            r += 1
        end while
    end if
    return log(n) * INVLN10
end function

global function log2(atom n)
--Added 26/9/22:
    if integer(n) and n>0 
    and and_bits(n,n-1)=0 then -- "if Kernigans bit counter would yield 1"
        -- KBC says and_bits(xxx1{0},xxx0{1}) is xxx0{0}, ie
        -- least significant set bit is cleared - clever, eh?
        integer t = 1, r = 0 -- 2^0==1
        while true do
            if t=n then return r end if
            t *= 2
            r += 1           -- 2^r==t
        end while
    end if
--</26/9/22>
--  return log(n) * INVLN2
    return log(n) * 1.44269504088896340739
end function

