--
-- factorial.e (an autoinclude)
--
integer finit = 0
sequence fcache

global function gamma(atom z)

    sequence p = { 676.5203681218851,   
                 -1259.1392167224028, 
                   771.32342877765313,  
                  -176.61502916214059,  
                    12.507343278686905, 
                    -0.13857109526572012,
                     9.9843695780195716e-6,
                     1.5056327351493116e-7 }
--maybe:
--forward function factorial(atom n) -- probably not needed/wanted, since it's in psym.e anyway
--  if integer(z) and z>=1 then return factorial(z-1) end if
    if z<0.5 then
        return PI / (sin(PI*z)*gamma(1-z))
    end if
    -- use a lanczos approximation:
    z -= 1
    atom x = 0.99999999999980993;
    for i=1 to length(p) do
        x += p[i] / (z + i);
    end for
    atom t = z + 7.5;
    return sqrt(2*PI) * power(t,z+0.5) * exp(-t) * x
end function

--4/10/22:
--global function factorial(integer n)
global function factorial(atom n)
--
-- Standard iterative factorial function, with memoisation.
-- eg         n : 0 1 2 3 4  5   6   7    8
--  factorial(n): 1 1 2 6 24 120 720 5040 40320 
-- see also mpz_fac_ui()
--
    if not integer(n) or n<0 then
        return gamma(n+1)
    end if
    atom res = 1
    if n>0 then
        if not finit then
            fcache = {1}
            finit = 1
        end if
        for i=length(fcache)+1 to n do
            fcache &= fcache[$]*i
        end for
        res = fcache[n]
    end if 
    return res
end function

global function k_perm(integer n, k)
--
-- Standard partial permutations calculation (sequences without repetition)
--
    atom res = n
    for i=n-1 to n-k+1 by -1 do
        res *= i
    end for
    return res
end function

global function choose(integer n, k)
--
-- Standard combinations calculation - choose k from n aka "n choose k"
-- see also mpz_binom() [probably wiser to use mpz over atom anyway]
--
--24/10/22 (from below)
--  if k>n-k then k = n-k end if
--  k = min(k,n-k)
    atom res = 1
--  for i=1 to k do
    for i=1 to min(k,n-k) do
        res = (res*(n-i+1))/i
    end for
    return res
end function

--/* alt, from pe 53 overview paper
function combinations(integer n, r)
    if r>n-r then r = n-r end if
    atom res = 1
    for i=1 to r do
        res = res*(n-i+1)/i
    end for
    return res
end function
--*/

--  atom res = k_perm(n,k)/factorial(k)
--/* alt:
    mpz r = mpz_init(1)
    for i=1 to k do
--      r := (r * (n-i+1)) / i, but using mpz routines:
        mpz_mul_si(r, r, n-i+1)
        if mpz_fdiv_q_ui(r, r, i)!=0 then ?9/0 end if
    end for
    return mpz_get_str(r)
--or (more likely to overflow)
--function binom(integer n, k)
--  return factorial(n)/(factorial(k)*factorial(n-k))
--end function
--or (getting complicated here) (for numeric limits not speed)
--      if n<=k then --(overlap)
--          numerators = k+1..n-k-1, denominators = 2..n-1
--      else
--          numerators = n..n-k-1, denominators = 2..k
--      while there exists a whole multiple z of den[k=$..1] in num
--          numerator[z]/=den[$], denom[k..k] = {} (or 1)
--      where mul_tt() is that "multiple table thing",
--      return mul_tt(num)/mul_tt(den)
--*/

