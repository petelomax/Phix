--
-- pfactors.e
--
--  implementation of factors(n), which returns a list of all integer factors of n,
--                and prime_factors(n), which returns a list of prime factors of n,
--               also prime_powers(), factor_count(), factor_sum() and square_free().
--
-- eg factors(6) is {2,3}
--    factors(6,1) is {1,2,3,6}
--    factors(6,-1) is {1,2,3}
--    factors(720,1) is {1,2,3,4,5,6,8,9,10,12,15,16,18,20,24,30,36,
--                       40,45,48,60,72,80,90,120,144,180,240,360,720}
--    factors(12345) is {3 5 15 823 2469 4115}
--
-- whereas:
--    prime_factors(6) is {2,3}
--    prime_factors(720) is {2,3,5}
--    prime_factors(720,1) is {2,2,2,2,3,3,5}
--    prime_factors(12345) is {3,5,823}
--
without debug -- keep ex.err clean

integer lim_chk = 0     -- 0: fail maxint, 1: pass it

procedure check_limits(atom n, string called_from)
--28/9/22:
--  if n<1 or n!=floor(n) then
    if n<0 or not integer(n-floor(n)) then
--atom dbg = n-floor(n)
        string g = sprintf("%g",n)
        if not find('.',g) then g &= sprintf("%+g",n-round(n)) end if
        crash("%s(%s): argument must be a non-negative integer",{called_from,g},nFrames:=3)
--29/9/22:
--  elsif n>=power(2,iff(machine_bits()=32?53:64)) then
    elsif compare(n,power(2,iff(machine_bits()=32?53:64)))>=lim_chk then
        -- n above power(2,53|64) is pointless, since IEE 754 floats drop 
        -- low-order bits. As per the manual, on 32-bit atoms can store 
        -- exact integers up to 9,007,199,254,740,992, however between
        -- that and 18,014,398,509,481,984 you can only store even nos,
        -- then upto 36,028,797,018,963,968, you can only store numbers 
        -- divisible by 4, etc. Hence were you to ask for the prime
        -- factors of 9,007,199,254,740,995 you might be surprised when
        -- it does not list 5, since this gets 9,007,199,254,740,994.
        -- (yes, this routine actually gets a number ending in 4 not 5)
        -- (also, it is >= since eg 90007..93 ends up in here as ..92,
        --  though factors(0|1,-9) can be used to change that behaviour)
        crash("argument to %s() exceeds maximum precision",{called_from},nFrames:=3)
    end if
end procedure

--include primes.e

--global function prime_factors(atom n, bool duplicates=false, integer maxprime=100)
global function prime_factors(atom n, integer duplicates=false, maxprime=100)
-- returns a list of all prime factors of n that are <= get_prime(maxprime)
--  if duplicates is true returns a true decomposition of n (eg 8 --> {2,2,2})
--  if duplicates is 2 returns {prime,power} pairs as per mpz_prime_factors()
--  if duplicates is 3 returns product(power+1) as the total factor_count()
--  if maxprime is -1 it is set to get_maxprime(n), and obviously some programs
--  will benefit from not performing an unnecessary sqrt() on every single call.
    if n=0 then return iff(duplicates=3?0:{}) end if
    check_limits(n,"prime_factors")
    if maxprime=-1 then maxprime = get_maxprime(n) end if
    sequence pfactors = {}
    integer pn = 1,
            p = get_prime(pn), 
            mp = get_prime(maxprime),
            lim = min(floor(sqrt(n)),mp),
            fc = 1, f

    while p<=lim do
        if remainder(n,p)=0 then
--          pfactors = append(pfactors,p)
            if duplicates=3 then
                f = 1
            else
                pfactors = append(pfactors,iff(duplicates=2?{p,1}:p))
            end if
            while true do
                n = n/p
                if remainder(n,p)!=0 then exit end if
                if duplicates then
                    if duplicates=3 then
                        f += 1
                    elsif duplicates=2 then
                        pfactors[$][2] += 1
                    else
                        pfactors = append(pfactors,p)
                    end if
                end if
            end while
            if duplicates=3 then fc *= f+1 end if
            if n<=p then exit end if
            lim = min(floor(sqrt(n)),mp)
        end if
        pn += 1
        p = get_prime(pn)
    end while 
    if n>1 and (length(pfactors)!=0 or duplicates) then
        if duplicates=3 then return fc*2 end if
        pfactors = append(pfactors,iff(duplicates=2?{n,1}:n))
--added 12/6/19:
    elsif duplicates and pfactors={} then
        if n!=1 then ?9/0 end if    -- sanity check
--      if duplicates=3 then return 1 end if
        pfactors = iff(duplicates=2?{{2,0}}:{1})
    end if
    if duplicates=3 then return fc end if
    return pfactors
end function

global function prime_powers(atom n)
--
-- Decompose n into powers of small primes.
-- returns eg 108 ==> {{2,2},{3,3}}  (ie 2^2*3^3==4*27==108)
--         or 10080 ==> {{2,5},{3,2},{5,1},{7,1}}
--         or 1196836 ==> {{2,2},{547,2}}
--         or 1196837 ==> {{1196837,1}} (for it be prime)
-- Each element of the result is a {prime,power} pair.
-- Note this will persevere all the way on up to 16/20 digit numbers,
--  and may get rather slow towards the upper end, of power(2,53|64).
-- This is closer to mpz_prime_factors() than prime_factors() is,
--  maybe I should rename the former as mpz_prime_powers()...
--
--29/9/22:
    check_limits(n,"prime_powers")
    return prime_factors(n,2,-1)
--/*
    if n=0 then return {} end if
    if n=1 then return {1,0} end if
    check_limits(n,"prime_powers")
    sequence res = {}
    if not is_prime(n) then
        integer maxprime = get_maxprime(n),
                mp = get_prime(maxprime),
                pn = 1,
                p = get_prime(pn), 
                lim = min(floor(sqrt(n)),mp)

        while p<=lim do
            integer e = 0
            while remainder(n,p)=0 do
                n = floor(n/p)
                e += 1
            end while
            if e then
                res = append(res,{p,e})
                if n<=p or is_prime(n) then exit end if
                lim = min(floor(sqrt(n)),mp)
            end if
            pn += 1
            p = get_prime(pn)
        end while 
    end if
    if n!=1 then
        res = append(res,{n,1})
    end if
    return res
--*/
end function

global function factors(atom n, object include1=0)
--
-- returns a list of all integer factors of n
--  if include1 is 1 the result contains 1 and n
--  if include1 is -1 the result contains 1 but not n
--  if include1 is 0 the result contains neither 1 nor n (the default)
--  if include1 is -9 then n is a lim_chk setting (0=fail maxint, 1=pass it)
--  if include1 is -8 then just perform a check_limits on n (internal use only)
--  
    if string(include1) then
        include1 = {1,-1,0,-9}[find(include1,{"BOTH","JUST1","NEITHER","SET_LIM"})]
    end if
    if include1=-9 then
        integer res = lim_chk
        lim_chk = n -- (nb/aside: -1 [or >1] would be "fail everything")
        return res
    end if
    if n=0 then return {} end if
    check_limits(n,"factors")
    if include1=-8 then return 0 end if
    if n>1e7 then   -- added 30/9/22, quite a bit faster at the high end,
                    --                but "" slower for smaller n (<1e7)
                    -- (discovered while writing mpz_factors() function)
--      sequence res = {1}, pn = prime_powers(n)
        sequence res = {1}, pn = prime_factors(n,2,-1) -- (same)
        for i=1 to length(pn) do
            sequence r1 = res, r2 = {}; res = {}
            integer l = length(r1), r1dx = 1, r2dx = 1
            atom p = pn[i][1], q = p
            for j=1 to pn[i][2] do
                for k=1 to l do
--                  r2 &= r1[k]*p
                    r2 &= r1[k]*q
                end for
                q *= p
            end for
            r2 = sort(r2)
            -- and perform a merge_sort of the two sorted lists:
            p = r1[1]
            q = r2[1]
            while true do
                if p<=q then
                    res = append(res, p)
                    r1dx += 1
                    if r1dx>l then exit end if
                    p = r1[r1dx]
                else
                    res = append(res, q)
                    r2dx += 1
                    q = r2[r2dx] -- should never overrun, o/c
                end if
            end while
            res &= r2[r2dx..$] -- shd never be empty, either
        end for
        if include1=-1 then res = res[1..$-1]
        elsif include1=0 then res = res[2..$-1] end if
        return res
    end if
    sequence lfactors = {}, hfactors = {}
    integer p = 2,
            lim = floor(sqrt(n))

    if include1!=0 then
        lfactors = {1}
        if n!=1 and include1=1 then
            hfactors = {n}
        end if
    end if
    while p<=lim do
        if remainder(n,p)=0 then
            lfactors = append(lfactors,p)
            atom hfactor = n/p
            if hfactor=p then exit end if
            hfactors = prepend(hfactors,hfactor)
        end if
        p += 1
    end while 
    lfactors &= hfactors
    return lfactors
end function

global function factor_count(atom n)
    return prime_factors(n,3,-1)
end function    

global function factor_sum(atom n)
    -- credit: https://mathschallenge.net/index.php?section=faq&ref=number/sum_of_divisors
    if n<=1 then return n end if
    atom res = 1
--  for pq in prime_powers(n) do   -- (I'd probably still call this where clarity matters)
--  for pq in prime_factors(n,2,-1) do  -- (same, no point calling via the thin shim here)
    sequence pf = prime_factors(n,2,-1) -- (same, no point calling via the thin shim here)
    for pq in pf do
        integer {p,q} = pq
        res *= (power(p,q+1)-1)/(p-1)
    end for
    return res
end function    

global function square_free(atom n, integer maxprime=100)
-- returns true if prime_factors(n,duplicates:=true,maxprime) would contain no duplicates
--  (but terminating early and without building any unnecessary internal lists)
    if n=0 then return true end if
    check_limits(n,"square_free")
    if maxprime=-1 then maxprime = get_maxprime(n) end if
    integer pn = 1,
            p = get_prime(pn), 
            lim = min(floor(sqrt(n)),get_prime(maxprime))

    while p<=lim do
        if remainder(n,p)=0 then
            n = n/p
            if remainder(n,p)=0 then return false end if
            if n<=p then exit end if
            lim = min(floor(sqrt(n)),get_prime(maxprime))
        end if
        pn += 1
        p = get_prime(pn)
    end while 
    return true
end function

--/* 29/10/22 merged with is_prime2(), for best of both
--global function is_prime(atom n)
--  if n<2 then return false end if
--  check_limits(n,"is_prime")
--  integer pn = 1,
--          p = get_prime(pn), 
--          lim = floor(sqrt(n))
--
--  while p<=lim do
--      if remainder(n,p)=0 then return false end if
--      pn += 1
--      p = get_prime(pn)
--  end while 
--  return n>1
--end function
--*/

