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
--    prime_factors(12345) is {3,5,823}
--
-- Note that prime_factors() always returns {} when passed a prime number.
--

global function factors(atom n, integer include1=0)
-- returns a list of all integer factors of n
--  if include1 is 0 (the default), result does not contain either 1 or n
--  if include1 is 1, and n>1, the result contains 1 and n
--  if include1 is -1, and n>1, the result contains 1 but not n
sequence lfactors = {}, hfactors = {}
atom hfactor
integer p = 2,
        lim = floor(sqrt(n))

    if n<1 or n!=floor(n) then ?9/0 end if  --DEV crash("first argument to factors() must be a positive integer",{},2)
    if n!=1 and include1!=0 then
        lfactors = {1}
        if include1=1 then
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

global function prime_factors(atom n)
sequence pfactors = {}
integer p = 2, 
        lim = floor(sqrt(n))

    if n<1 or n!=floor(n) then ?9/0 end if      --DEV crash("argument to prime_factors() must be a positive integer",{},2)
    while p<=lim do
        if remainder(n,p)=0 then
            pfactors = append(pfactors,p)
            while 1 do
                n = n/p
                if remainder(n,p)!=0 then exit end if
            end while
            if n<=p then exit end if
            lim = floor(sqrt(n))
        end if
        p += 1
    end while 
    if n>1 and length(pfactors)!=0 then
        pfactors = append(pfactors,n)
    end if
    return pfactors
end function

