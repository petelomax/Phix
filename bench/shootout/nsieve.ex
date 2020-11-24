 -- Computer Language Shootout
 -- http://shootout.alioth.debian.org/
 --
 --  by Jason Gade
 --  run: exu nsieve.ex [N=2]

without warning
without type_check


constant TRUE  = ( 1 = 1 )
constant FALSE = ( 1 = 0 )



function NSieve(integer m)

    integer count
    sequence isPrime
    count = 0

    isPrime = repeat(TRUE, m)
    
    for i = 2 to m do
        if isPrime[i] then
            count += 1
            for j = 2*i to m by i do
                isPrime[j] = FALSE
            end for
        end if
    end for

    return count

end function



constant k = 10000


for i=1 to 10 do
    if NSieve(10000)!=1229 then ?9/0 end if
    if NSieve(20000)!=2262 then ?9/0 end if
    if NSieve(40000)!=4203 then ?9/0 end if
    if NSieve(80000)!=7837 then ?9/0 end if
    if NSieve(160000)!=14683 then ?9/0 end if
    if NSieve(320000)!=27608 then ?9/0 end if
end for
--Primes up to  10000     1229
--Primes up to  20000     2262
--Primes up to  40000     4203
--Primes up to  80000     7837
--Primes up to  160000   14683
--Primes up to  320000   27608
--
--  object n
--  integer m
--
--  n = 5
--
--  m = k * power(2, n)
--  printf(1, "Primes up to %8d %8d\n", {m, NSieve(m)})
--  
--  m = k * power(2, n - 1)
--  printf(1, "Primes up to %8d %8d\n", {m, NSieve(m)})
--  
--  m = k * power(2, n - 2)
--  printf(1, "Primes up to %8d %8d\n", {m, NSieve(m)})
--
--if getc(0) then end if
