--
-- pfactors.e
--
--  implementation of factors(n), which returns a list of all integer factors of n,
--                and prime_factors(n), which returns a list of prime factors of n
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

procedure check_limits(atom n, string rtn)
    if n<1 or n!=floor(n) then
        crash("argument to %s() must be a positive integer",{rtn})
    elsif n>=power(2,iff(machine_bits()=32?53:64)) then
        -- n above power(2,53|64) is pointless, since IEE 754 floats drop 
        -- low-order bits. As per the manual, on 32-bit atoms can store 
        -- exact integers up to 9,007,199,254,740,992, however between
        -- that and 18,014,398,509,481,984 you can only store even nos,
        -- then upto 36,028,797,018,963,968, you can only store numbers 
        -- divisible by 4, etc. Hence were you to ask for the prime
        -- factors of 9,007,199,254,740,995 you might be surprised when
        -- it does not list 5, since this gets 9,007,199,254,740,994.
        -- (yes, the routine actually gets a number ending in 4 not 5)
        -- (and it's >= since eg 90...93 will end up in here as ..92)
        crash("argument to %s() exceeds maximum precision",{rtn})
    end if
end procedure

global function factors(atom n, integer include1=0)
--
-- returns a list of all integer factors of n
--  if include1 is 0 (the default), result does not contain either 1 or n
--  if include1 is 1 the result contains 1 and n
--  if include1 is -1 the result contains 1 but not n
--  
    if n=0 then return {} end if
    check_limits(n,"factors")
    sequence lfactors = {}, hfactors = {}
    atom hfactor
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
            hfactor = n/p
            if hfactor=p then exit end if
            hfactors = prepend(hfactors,hfactor)
        end if
        p += 1
    end while 
    return lfactors & hfactors
end function

--include primes.e

--global function prime_factors(atom n, bool duplicates=false, integer maxprime=100)
global function prime_factors(atom n, integer duplicates=false, maxprime=100)
-- returns a list of all prime factors of n that are <= get_prime(maxprime)
--  if duplicates is true returns a true decomposition of n (eg 8 --> {2,2,2})
--  if duplicates is 2 returns {prime,power} pairs as per mpz_prime_factors()
--  if maxprime is -1 it is set to get_maxprime(n), and obviously some programs
--  will benefit from not performing an unnecessary sqrt() on every single call.
    if n=0 then return {} end if
    check_limits(n,"prime_factors")
    if maxprime=-1 then maxprime = get_maxprime(n) end if
    sequence pfactors = {}
    integer pn = 1,
            p = get_prime(pn), 
            lim = min(floor(sqrt(n)),get_prime(maxprime))

    while p<=lim do
        if remainder(n,p)=0 then
--          pfactors = append(pfactors,p)
            pfactors = append(pfactors,iff(duplicates=2?{p,1}:p))
            while true do
                n = n/p
                if remainder(n,p)!=0 then exit end if
                if duplicates then
                    if duplicates=2 then
                        pfactors[$][2] += 1
                    else
                        pfactors = append(pfactors,p)
                    end if
                end if
            end while
            if n<=p then exit end if
            lim = min(floor(sqrt(n)),get_prime(maxprime))
        end if
        pn += 1
        p = get_prime(pn)
    end while 
    if n>1 and (length(pfactors)!=0 or duplicates) then
        pfactors = append(pfactors,iff(duplicates=2?{n,1}:n))
--added 12/6/19:
    elsif duplicates and pfactors={} then
        if n!=1 then ?9/0 end if    -- sanity check
        pfactors = iff(duplicates=2?{{2,0}}:{1})
    end if
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

global function is_prime(atom n)
    if n=0 then return false end if
    check_limits(n,"is_prime")
    integer pn = 1,
            p = get_prime(pn), 
            lim = floor(sqrt(n))

    while p<=lim do
        if remainder(n,p)=0 then return false end if
        pn += 1
        p = get_prime(pn)
    end while 
    return n>1
end function

