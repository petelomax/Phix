--
-- builtins\primes.e
-- =================
--
without debug
--DEV/SUG:
--/*
for n=1 to 10 do
    sequence p = get_primes(-n)
    printf(1,"%d: product(%v) = %d\n",{n,p,product(p)})
end for
1: product({2}) = 2
2: product({2,3}) = 6
3: product({2,3,5}) = 30
4: product({2,3,5,7}) = 210
5: product({2,3,5,7,11}) = 2310
6: product({2,3,5,7,11,13}) = 30030 -- (== 15015*2)
7: product({2,3,5,7,11,13,17}) = 510510
8: product({2,3,5,7,11,13,17,19}) = 9699690
9: product({2,3,5,7,11,13,17,19,23}) = 223092870
10: product({2,3,5,7,11,13,17,19,23,29}) = 6469693230

That 30030 looks pretty good for me a as "wheel", ie create/deep_copy in add_block()
and continue from primes[7] (aka 17), should make things a bit faster. Would like to
knock/improve deep_copy() into respectable/best possible performance first though.
-- abandoned, for now (more perhaps when register alloc is working):
-- 1e7th prime in 2.4s on old, 3.5s with a 30030 wheel, 2.0s with a 510510 wheel.
--  (so yes, it's an improvement, but not one to shout about, or really risk.)
--*/
include pfactors.e -- uses factors(n,-8) for check_limits()
include bsearch.e -- binary_search()

--with trace
sequence primes
--       wheel,
--       sieve
atom sieved = 0

procedure init_sieve()
    primes = {2,3,5,7}
--  primes = {2,3,5,7,11,13}
--  primes = {2}
    sieved = 10 
--/*
--  wheel = repeat(1,30030)
--  sieve = repeat(1,30030)
    wheel = repeat(1,510510)
    sieve = repeat(1,510510)
--  for i=3 to 30030 by 2 do
    for i=3 to 510510 by 2 do
        if sieve[i] then
            primes &= i
--          for j=i to 30030 by i*2 do
            for j=i to 510510 by i*2 do
                sieve[j] = 0
--              if i<=13 then
                if i<=17 then
                    wheel[j] = 0
                end if
            end for
        end if
    end for
--  sieved = 30030
    sieved = 510510
--pp(shorten(wheel,"digits",40))
--pp(shorten(primes,"primes",40))
--*/
end procedure

procedure add_block()
    integer N = min((sieved-1)*sieved,400000)
--  integer N = min((sieved-1)*sieved,30030)
    sequence sieve = repeat(1,N)    -- sieve[i] is really i+sieved
--X sequence sieve = deep_copy(wheel)
--/*
--trace(1)
--  integer N = 30030
--  integer N = 510510
--  #ilASM{
--      [32]
--          mov esi,[wheel]
--          mov edi,[sieve]
--          shl esi,2
--          shl edi,2
--          mov ecx,[N]
--          rep movsd
--      [64]
--          mov rsi,[wheel]
--          mov rdi,[sieve]
--          shl rsi,2
--          shl rdi,2
--          mov rcx,[N]
--          rep movsq
--      []
--        } 
--  for i=7 to length(primes) do -- (evens filtered on output)
--*/
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
--pp(shorten(sieve))
--X procedure add_primes(sieve)
    for i=1 to N by 2 do
        if sieve[i] then
            primes &= i+sieved
        end if
    end for
    sieved += N
--pp(shorten(primes))
--?{sieved,length(primes)}
end procedure

global function get_prime(integer k)
    if k=0 then return 0 end if
    if sieved=0 or k=-1 then init_sieve() end if
    while length(primes)<k do
        add_block()
    end while
    return primes[k]
end function

-- (moved here from pfactors.e 29/10/22, then merged)
--/*
--global function is_prime(atom n)
--  if n<2 then return false end if
--  if sieved=0 then init_sieve() end if
--  if n>sieved then {} = factors(n,-8) end if
----    check_limits(n,"is_prime") -- (at least that bit's ok)
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
--
---- replaced with much faster version in pfactors.e:
--global function is_prime2(atom p, bool bIndex=false)
--  if sieved=0 then init_sieve() end if
--  while sieved<p do
--      add_block()
--  end while
--  integer k = binary_search(p,primes)
----    return iff(bIndex?max(k,0):k>0)
--  return iff(bIndex?k:k>0)
--end function
--*/

global function is_prime(atom n, integer bIndex=-1)
    if n<2 then return false end if
    if sieved=0 then init_sieve() end if
    -- perform a check_limits(n) when needed:
    if n>sieved then {} = factors(n,-8) end if
    if bIndex!=-1 then -- (nb explicitly true or false)
        while sieved<n do
            add_block()
        end while
        integer k = binary_search(n,primes)
        return iff(bIndex?k:k>0)
    end if
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

global function get_maxprime(atom m)
-- returns a suitable maxprime for prime_factors()
    m = floor(sqrt(m+1))
    if sieved=0 then init_sieve() end if
    while sieved<m do
        add_block()
    end while
    integer res = binary_search(m,primes)
    if res<0 then res = abs(res)-1 end if
    return res
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

--/* no no no, don't do that...
--function segmented_primes(atom limit, integer show_progress)
    -- translation of https://gist.github.com/kimwalisch/3dc39786fab8d5b34fee
    // Generate primes using the segmented sieve of Eratosthenes.
    // Note this is slower than get_primes_le(), for 1e9 33s vs 19s, hence
    // only invoked for > 1e9, which would otherwise hit allocation issues.
    // Further we count then alloc, potentially doubling time taken, so that
    // we do not start fragmenting the heap, as building res with &= would.
--  if limit<2 then return {} end if -- (only ever called with limit>1e9)
    integer sqrtlim = floor(sqrt(limit)),
       segment_size = max(sqrtlim, 32768), -- (assumed L1 cache size)
              count = 1, i = 3, s = 3
    atom n = 3,  t1 = time()+1

    sequence res,
             isprime = repeat(true,sqrtlim+1),
             primes = {},
             multiples = {},
             multiplez = {}

    assert(limit<=5e9 or machine_bits()=64)
    -- ^1e10 yields 455,052,511 primes, whereas sequences are
    --   limited to 402,653,177 entries on 32bit, as all
    --   explained in gory detail in builtins\VM\pHeap.e.

    for bCounting in {true, false} do
        atom low = 0
        while low<=limit do
            sequence sieve = repeat(true,segment_size+1)
            if show_progress and time()>t1 then
                string cb = iff(bCounting?"Count":"Build"),
                        f = iff(bCounting?sprintf(", found %,d",count):"")
                atom lo = iff(bCounting?low:count),
                     hi = iff(bCounting?limit:length(res))
                if show_progress=true then
                    progress("%sing primes %,d/%,d (%3.2f%%)%s\r",{cb, lo,hi,(lo/hi)*100,f})
                else
                    show_progress(bCounting,lo,hi)
                end if
                t1 = time()+1
            end if

            // current segment = [low, high]
            atom high = min(low+segment_size,limit)
            if bCounting then

                // generate sieving primes using simple sieve of Eratosthenes
                while i*i<=min(high,sqrtlim) do
                    if isprime[i+1] then
                        for j=i*i to sqrtlim by i do
                            isprime[j+1] = false
                        end for
                    end if
                    i += 2
                end while
    
                // initialize sieving primes for segmented sieve
                while s*s<=high do
                    if isprime[s+1] then
                           primes &= s
                        multiples &= s*s-low
                        multiplez &= s*s-low
                    end if
                    s += 2
                end while
            end if

            // sieve the current segment
            for mi,j in multiples do
                integer k = primes[mi]
                if not bCounting and k*k>high then exit end if
                k *= 2
                while j<segment_size do
                    sieve[j+1] = false
                    j += k
                end while
                multiples[mi] = j - segment_size
            end for

            while n<=high do
                if sieve[n-low+1] then // n is a prime
                    count += 1
                    if not bCounting then
                        res[count] = n
                    end if
                end if
                n += 2
            end while
            low += segment_size
        end while
        if bCounting then
            res = repeat(2,count)
            count = 1
            i = 3
            s = 3
            n = 3
            multiples = multiplez
            multiplez = {}
        end if
    end for
    if show_progress=true then
        progress("")
    elsif show_progress then
        show_progress(false,count,count)
    end if
    return res
--end function

--global function get_primes_le(atom hi, integer show_progress=true)
    if hi>1e9 then return segmented_primes(hi,show_progress) end if
--*/
global function get_primes_le(integer hi, overrun=0)
    assert(hi>=0 and overrun>=0)
    if sieved=0 then init_sieve() end if
    while primes[$]<hi do
        add_block()
    end while
    hi = binary_search(hi,primes)
    if hi<0 then hi = abs(hi)-1 end if
    while hi+overrun>length(primes) do
        add_block()
    end while
    sequence res = primes[1..hi+overrun]
    return res
end function

-- my go at the prime sieve shootout: (see also demo/prime_drag_race.exw, 2 years earlier that I'd forgotten about)
--/*
enum SIEVE_SIZE, NUM_BITS, SIEVE 
constant STDOUT = 1 
 
without type_check 
constant BIT8_TABLE = x"0102040810204080",  -- (or use power(2,xx) as commented out below) 
     NOT_BIT8_TABLE = x"FEFDFBF7EFDFBF7F"
 
function set_bits(integer num_bits) 
    -- create a binary string of precisely num-bits 1s 
    integer num_bytes = floor((num_bits + 7) / 8), 
        leftover_bits = 1 + and_bits(num_bits - 1, 0x7), 
        leftover_byte = BIT8_TABLE[leftover_bits] * 2 - 1 
--      leftover_byte = power(2,leftover_bits)-1 
    return repeatch('\xFF',num_bytes-1) & leftover_byte 
end function 
 
function clear_bits(sequence this, integer num_bits, integer start, integer step) 
    integer byte_num = 1 + floor((start - 1) / 8), 
             bit_pos = 1 + and_bits(start - 1, 0x7), 
            byte_inc = floor(step / 8), 
             bit_inc = and_bits(step, 0x7) 
    for k = start to num_bits by step do 
        this[byte_num] = and_bits(this[byte_num], NOT_BIT8_TABLE[bit_pos]) 
--      this[byte_num] = and_bits(this[byte_num], not_bits(power(2,bit_pos-1))) 
        byte_num += byte_inc 
         bit_pos +=  bit_inc 
        if bit_pos > 8 then 
            bit_pos -= 8 
            byte_num += 1 
        end if 
    end for 
    return this 
end function 
 
function get_bit(sequence sieve, integer bit) 
    integer byte_pos = 1 + floor((bit - 1) / 8), 
             bit_pos = 1 + and_bits(bit - 1, 0x7) 
    return and_bits(sieve[byte_pos], BIT8_TABLE[bit_pos]) 
--  return and_bits(sieve[byte_pos], power(2,bit_pos-1)) 
end function 
 
function run_sieve(integer sieve_size) 
    integer num_bits = floor((sieve_size - 1) / 2) 
    sequence sieve = set_bits(num_bits) 
    integer q = floor(sqrt(sieve_size) / 2) 
    -- 
    -- we start off with a sieve of all 1s, and (should) end up with 
    --    < byte2>< byte1> 
    --  0b0110010110110111  -- (little-endian, 33 first, 3 last) 
    --     19  3 97 31 753  -- ie 3,5,7,11,13,17,19,23,29,31 prime 
    --    3  75 1  5  9     --      and 9,15,21,25,27,33 not prime 
    --   30>< 20>< 10> 
    -- (as shown the first byte of sieve contains the flags for 3..17) 
    -- 
    -- Of course this is complete madness, no-one would ever want to 
    -- code like this on a day-to-day basis, but performance-critical 
    -- things can be done the once and then forgotten about. 
    -- 
    -- I already knew the algorithm, though not used an odd-only bitmap.
    -- I figured out which byte/bit needed testing and let my brain stew
    -- on it for a couple of days before drumming up this little monster.
    --
    integer step -- (needed due to v.high reg pressure, 64-bit uses r9) 
    #ilASM{ 
            [32] 
                xor edi,edi     -- for bit=1 to q do (using 0-based idx) 
                mov esi,[sieve] 
                shl esi,2       -- esi := raw_address(sieve) 
              ::bitloop 
                    -- if sieve[bit] then 
                    -- edi (p) ax/dl  test  
                    --  0   3   0/01  true 
                    --  1   5   0/02  true 
                    --  2   7   0/04  true 
                    --  3   9   0/08  false 
                    --  4  11   0/10  true 
                    --  5  13   0/20  true 
                    --  6  15   0/40  false 
                    --  7  17   0/80  true 
                    --  8  19   1/01  true 
                    --  9  21   1/02  false 
                    mov eax,edi 
                    mov dl,1 
                    mov cl,al 
                    shr eax,3 
                    and cl,7    -- edi -> byte, bit 
                    shl dl,cl   -- bit -> mask 
                    test byte[esi+eax],dl 
                    jz :notset 
                        -- clear_bits: 
                        push edi 
                        -- for edx=p*p to num_bits by p do (0/odd-only)
                        --  sieve[edx] = 0 
                        -- edx step (ea/dx) al/cl (p) bl
                        --  0   3     9/3    0/3   9  08
                        --              6    0/6  15  40
                        --              9    1/1  21  02
                        --              12   1/4  27  10
                        --              15   1/7  33  80
                        --              18   2/2  39  04
                        --  1   5    25/11   1/3  25  08
                        --              16   2/0  35  01
                        --              21   2/5  45  20
                        --              26   3/2  55  04
                        --  2   7    49/23   2/7  49  80
                        --  3   n/a 
                        --  4  11   121/60  ... 
                        lea ecx,[ebx+edi*2+3]   -- step 
                        mov eax,ecx 
                        mul ecx 
                        mov [step],ecx          -- (regs v scarce here!) 
                        shr eax,1 
                        sub eax,1               -- (0-based) 
                        mov edx,eax             -- start 
 
                      ::innerloop 
                        mov cl,al 
                        shr eax,3 
                        and cl,7                -- -> byte,bit 
                        mov bl,1 
                        shl bl,cl 
                        not bl                  -- bit -> not mask 
                        mov ecx,[step] 
                        add edx,ecx 
                        and byte[esi+eax],bl 
                        mov eax,edx 
                        cmp edx,[num_bits] 
                        jle :innerloop 
 
                        pop edi 
                        xor ebx,ebx -- important!! 
              ::notset           
                add edi,1 
                cmp edi,[q] 
                jle :bitloop 
            [64] 
                xor rdi,rdi     -- for bit=1 to q do (using 0-based idx) 
                mov rsi,[sieve] 
                shl rsi,2       -- rsi := raw_address(sieve) 
              ::bitloop 
                    -- if sieve[bit] then (as above) 
                    mov rax,rdi 
                    mov dl,1 
                    mov cl,al 
                    shr rax,3 
                    and cl,7 
                    shl dl,cl 
                    test byte[rsi+rax],dl 
                    jz :notset 
                        -- clear_bits: (as above) 
                        push rdi 
                        lea rcx,[rbx+rdi*2+3]   -- step 
                        mov rax,rcx 
                        mul rcx 
                        mov r9,rcx              -- (save step) 
                        shr rax,1               -- start 
                        sub rax,1               -- (0-based) 
                        mov rdx,rax             -- start/k 
                        mov r10,[num_bits]      -- (may as well) 
 
                      ::innerloop 
                        mov cl,al 
                        shr rax,3 
                        and cl,7                -- -> byte,bit 
                        mov bl,1 
                        shl bl,cl 
                        not bl 
                        add rdx,r9              -- + step 
                        and byte[rsi+rax],bl 
                        mov rax,rdx 
                        cmp rdx,r10 
                        jle :innerloop 
 
                        pop rdi 
                        xor rbx,rbx -- important!! 
              ::notset           
                add rdi,1 
                cmp rdi,[q] 
                jle :bitloop 
            [] 
          } 
    return {sieve_size, num_bits, sieve} 
end function 
 
function count_primes(sequence this, bool show_results=FALSE) 
    integer count = 1, count2 = 1 
    if show_results then 
        printf(STDOUT, "2, ") 
    end if 
 
    for bit=1 to this[NUM_BITS] do 
        if get_bit(this[SIEVE], bit) then 
            count += 1 
            if show_results then 
                printf(STDOUT, "%d, ", 2 * bit + 1) 
            end if 
        end if 
    end for 
    -- or, maybe: 
    for b in this[SIEVE] do count2 += count_bits(b) end for 
    assert(count=count2) 
    -- or even: (just showing off now, ain't I?) 
    assert(count == 1+sum(apply(this[SIEVE],count_bits))) 
 
    if show_results then 
        printf(STDOUT, "\n") 
    end if 
 
    return count 
end function 
 
-- Erm, this might deserve re-testing?? 
-- Cannot support sieve size greater than 1 billion -- 
constant {sizes,expected} = columnize({{10,4}, 
                                       {100,25}, 
                                       {1_000,168}, 
                                       {10_000,1229}, 
                                       {100_000,9592}, 
                                       {1_000_000,78498}, 
                                       {10_000_000,664579}, 
                                       {100_000_000,5761455}, 
                                       {1_000_000_000,50847534}}) 
 
function validate_results(sequence this) 
    integer k = find(this[SIEVE_SIZE],sizes) 
    assert(k!=0,"Invalid sieve size") 
    integer expected_count = expected[k] 
    return count_primes(this) = expected_count 
end function 
 
procedure print_results(sequence this, integer show_results, atom duration, integer passes) 
    printf( 
        STDOUT, 
        "Passes: %d, Time: %.8f, Avg: %.8f, Limit: %d, Count: %d, Valid: %s\n", 
        { 
            passes, 
            duration, 
            duration / passes, 
            this[SIEVE_SIZE], 
            count_primes(this, show_results), 
            iff(validate_results(this), "true", "false") 
        } 
    ) 
    printf( 
        STDOUT, 
        "\nrzuckerm;%d;%.8f;1;algorithm=base,faithful=yes,bits=1\n", 
        {passes, duration} 
    ) 
end procedure 
 
procedure main() 
    -- should these be obtained from command_line()?? 
    integer n = 1_000_000, 
            show_results = false 
    atom start = time(), passes = 0 
    sequence sieve 
    while 1 do 
        passes += 1 
        sieve = run_sieve(n) 
        atom duration = time() - start 
        if duration >= 5 then 
            print_results(sieve, show_results, duration, passes) 
            exit 
        end if 
    end while 
end procedure 
 
main() 
--*/
