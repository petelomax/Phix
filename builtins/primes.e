--
-- builtins/primes.e
-- =================
--
sequence primes
atom sieved = 0

procedure init_sieve()
    primes = {2,3,5,7}
    sieved = 10 
end procedure

procedure add_block()
    integer N = min((sieved-1)*sieved,400000)
    sequence sieve = repeat(1,N)    -- sieve[i] is really i+sieved
    for i=2 to length(primes) do -- (evens filtered on output)
        atom p = primes[i], p2 = p*p
        if p2>sieved+N then exit end if
        if p2<sieved+1 then
            p2 += ceil((sieved+1-p2)/p)*p
        end if
        p2 -= sieved
        if and_bits(p2,1)=0 then p2 += p end if
--      if sieve[p2] then           -- dang!
        for k=p2 to N by p*2 do
            sieve[k] = 0
        end for
--      end if
    end for
    for i=1 to N by 2 do
        if sieve[i] then
            primes &= i+sieved
        end if
    end for
    sieved += N
end procedure

-- replaced with much faster version in pfactors.e:
--global function is_prime(atom p)
--  if sieved=0 then init_sieve() end if
--  while sieved<p do
--      add_block()
--  end while
--  return binary_search(p,primes)>0
--end function

global function get_prime(integer n)
    if sieved=0 or n=-1 then init_sieve() end if
    while length(primes)<n do
        add_block()
    end while
    return primes[n]
end function

global function get_maxprime(atom p)
-- returns a suitable maxprime for prime_factors()
    p += 1
    p = floor(sqrt(p))
    if sieved=0 then init_sieve() end if
    while sieved<p do
        add_block()
    end while
    return abs(binary_search(p,primes))
end function

global function get_primes(integer count=0)
    if sieved=0 then init_sieve() end if
    while length(primes)<abs(count) do
        add_block()
    end while
    sequence res = primes
    if count<0 then
        res = res[1..abs(count)]
    end if
    return res
end function
