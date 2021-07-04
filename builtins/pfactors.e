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
    elsif n>power(2,iff(machine_bits()=32?53:64)) then
        -- n above power(2,53|64) is pointless, since IEE 754 floats drop 
        -- low-order bits. As per the manual, on 32-bit atoms can store 
        -- exact integers up to 9,007,199,254,740,992, however between
        -- that and 18,014,398,509,481,984 you can only store even nos,
        -- then upto 36,028,797,018,963,968, you can only store numbers 
        -- divisible by 4, etc. Hence were you to ask for the prime
        -- factors of 9,007,199,254,740,995 you might be surprised when
        -- it does not list 5, since this gets 9,007,199,254,740,994.
        -- (yes, the routine actually gets a number ending in 4 not 5)
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

global function prime_factors(atom n, bool duplicates=false, integer maxprime=100)
-- returns a list of all prime factors <=get_prine(maxprime) of n
--  if duplicates is true returns a true decomposition of n (eg 8 --> {2,2,2})
    if n=0 then return {} end if
    check_limits(n,"prime_factors")
    if maxprime=-1 then maxprime = get_maxprime(n) end if
    sequence pfactors = {}
    integer pn = 1,
            p = get_prime(pn), 
            lim = min(floor(sqrt(n)),get_prime(maxprime))

    while p<=lim do
        if remainder(n,p)=0 then
            pfactors = append(pfactors,p)
            while 1 do
                n = n/p
                if remainder(n,p)!=0 then exit end if
                if duplicates then
                    pfactors = append(pfactors,p)
                end if
            end while
            if n<=p then exit end if
            lim = min(floor(sqrt(n)),get_prime(maxprime))
        end if
        pn += 1
        p = get_prime(pn)
    end while 
    if n>1 and (length(pfactors)!=0 or duplicates) then
        pfactors = append(pfactors,n)
--added 12/6/19:
    elsif duplicates and pfactors={} then
        if n!=1 then ?9/0 end if    -- sanity check
        pfactors = {1}
    end if
    return pfactors
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

