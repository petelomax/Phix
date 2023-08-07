--
-- builtins/primesieve.e
-- =====================
-- I found a suitable prebuilt 64-bit windows dll in the pascal bindings of primesieve (version 7.7, dated 31/12/21), 
-- so gave it a try. Note that you'll have to grab the following dll yourself, and of course update the path to it.
-- DEV: Not yet documented.
--
constant psdll = `F:\misc\rpi\primesieve-pas-master\libprimesieve\MSVC\primesieve.dll`
without js
requires(WINDOWS)   -- (a suitable Linux .so is probably reasonably straightforward to build...)
requires(64,true)
include cffi.e

--NB this is version 7.7...
constant tIterator = """
typedef struct
{
  size_t i;
  size_t last_idx;
  uint64_t start;
  uint64_t stop;
  uint64_t stop_hint;
  uint64_t dist;
  uint64_t* primes;
  void* vector;
  void* primeGenerator;
  int is_error;
} primesieve_iterator;""",
idIterator = define_struct(tIterator),
pIterator = allocate_struct(idIterator),
--size_of_Iterator = get_struct_size(idIterator),
pslib = open_dll(psdll),
x_primesieve_init = define_c_proc(pslib,"primesieve_init",{C_PTR}),
x_primesieve_free_iterator = define_c_proc(pslib,"primesieve_free_iterator",{C_PTR}),
--x_primesieve_jump_to = define_c_proc(pslib,"primesieve_jump_to",{C_PTR,C_INT64,C_INT64}),
x_primesieve_skipto = define_c_proc(pslib,"primesieve_skipto",{C_PTR,C_INT64,C_INT64}),
x_primesieve_generate_next_primes = define_c_proc(pslib,"primesieve_generate_next_primes",{C_PTR}),
x_primesieve_generate_prev_primes = define_c_proc(pslib,"primesieve_generate_prev_primes",{C_PTR}),
UINT64_MAX = power(2,64)-1

--?define_struct(tIterator,bAdd:=false)
--{"primesieve_iterator",80,8,{{"i","last_idx","start","stop","stop_hint","dist",
--                              "primes","vector","primeGenerator","is_error"},
--   {{"size_t",8,0,1},{"size_t",8,8,1},{"uint64_t",8,16,0},{"uint64_t",8,24,0},{"uint64_t",8,32,0},{"uint64_t",8,40,0},
--                  {"ptr",8,48,1},{"ptr",8,56,1},{"ptr",8,64,1},{"int",4,72,1}}},{}}

c_proc(x_primesieve_init,{pIterator})
--?peek({pIterator,size_of_Iterator})&-1

-- not yet meaningful {0,0}:
--atom p_last_idx = get_struct_field(idIterator,pIterator,"last_idx"),
--   p_primes = get_struct_field(idIterator,pIterator,"primes")
atom p_last_idx = 0,
     p_primes = -1

/* Free all memory */
-- Warning: completely untested
global procedure primesieve_free_iterator()
--  primesieve_iterator* it);
    c_proc(x_primesieve_free_iterator,{pIterator})
end procedure

-- not in 7.7:
/**
 * Reset the primesieve iterator to start.
 * @param start      Generate primes >= start (or <= start).
 * @param stop_hint  Stop number optimization hint. E.g. if you want
 *                   to generate the primes <= 1000 use
 *                   stop_hint = 1000, if you don't know use
 *                   UINT64_MAX.
 */
--void primesieve_jump_to(primesieve_iterator* it, uint64_t start, uint64_t stop_hint);
--global procedure primesieve_jump_to(atom start, stop_hint=UINT64_MAX)
--  c_proc(x_primesieve_jump_to,{pIterator,start,stop_hint})
--end procedure

/**
 * Reset the primesieve iterator to start.
 * @param start      Generate primes > start (or < start).
 * @param stop_hint  Stop number optimization hint. E.g. if you want
 *                   to generate the primes <= 1000 use
 *                   stop_hint = 1000, if you don't know use
 *                   UINT64_MAX.
 */
-- (deprecated, apparently purely on the basis that most calls would naturally want a -1)
global procedure primesieve_skipto(atom start, stop_hint=UINT64_MAX)
    c_proc(x_primesieve_skipto,{pIterator,start,stop_hint})
end procedure

/**
 * Get the next prime.
 * Returns PRIMESIEVE_ERROR (UINT64_MAX) if any error occrus.
 */
global function primesieve_next_prime()
--  integer i = get_struct_field(idIterator,pIterator,"i")+1
    integer i = peek8u(pIterator)+1
--  set_struct_field(idIterator,pIterator,"i",i)
    poke8(pIterator,i)
    if i>p_last_idx then
        c_proc(x_primesieve_generate_next_primes,{pIterator})
--      integer e = get_struct_field(idIterator,pIterator,"is_error")
--      assert(e=0)
--      assert(get_struct_field(idIterator,pIterator,"last_idx")=p_last_idx)
--      assert(get_struct_field(idIterator,pIterator,"primes")=p_primes)
        p_last_idx = get_struct_field(idIterator,pIterator,"last_idx")
        p_primes = get_struct_field(idIterator,pIterator,"primes")
--      i = get_struct_field(idIterator,pIterator,"i")
        i = peek8u(pIterator)
    end if  
    return peek8u(p_primes+i*8)
end function

/**
 * Get the previous prime.
 * primesieve_prev_prime(n) returns 0 for n <= 2.
 * Note that primesieve_next_prime() runs up to 2x faster than
 * primesieve_prev_prime(). Hence if the same algorithm can be written
 * using either primesieve_prev_prime() or primesieve_next_prime()
 * it is preferable to use primesieve_next_prime().
 */
-- Warning: completely untested
global function primesieve_prev_prime()
--  integer i = get_struct_field(idIterator,pIterator,"i")
    integer i = peek8u(pIterator)
    if i=0 then
        c_proc(x_primesieve_generate_prev_primes,{pIterator})
        p_last_idx = get_struct_field(idIterator,pIterator,"last_idx")
        p_primes = get_struct_field(idIterator,pIterator,"primes")
--      i = get_struct_field(idIterator,pIterator,"i")
        i = peek8u(pIterator)
    end if  
    i -= 1
--  set_struct_field(idIterator,pIterator,"i",i)
    poke8(pIterator,i)
    return peek8u(p_primes+i*8)
end function


--/* a test program, took 1hr 53 mins to run - "next" was atom not integer, but shd be ok.
without js
include builtins/primesieve.e -- (can provide if desired, ask me on my talk page)
requires(WINDOWS)
requires(64,true)
atom t0 = time(), t1 = time()+1
integer next = 1, count = 1, p
while next!=36 do
    if even(next) then
        next /= 2
    else
        p = primesieve_next_prime()
        next += p
    end if
    count += 1
    if time()>t1 then
        atom cpc = (count/77_534_485_877)*100,
             ppc = (p/677_121_348_413)*100
        progress("count: %,d (%2.2f%%)  p: %,d (%2.2f%%)\r",{count,cpc,p,ppc})
        t1 = time()+1
    end if
end while
progress("")
printf(1,"%,d%s member is: %,d and highest prime needed: %,d\n",
             {count, ord(count), next, p})
?elapsed(time()-t0)
--*/
