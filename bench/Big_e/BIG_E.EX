-----------------------------------------------------------------------
-- Very fast calculation of number e, with thousands of decimal digits
-----------------------------------------------------------------------

--  Author:  "Jaime Marcos" <jmarcos2@xtec.net>        Oct'06   ver 1.2
--  Language:  Euphoria ver 2.X 
--  Credits:  S. Wozniak (Byte, June 1981, pp 392-407)

--  Output is dumped on the file  num_e.txt

--  Expression used:
--  e-2 := [0] +1/n+1)/ ... +1)/5+1)/4+1)/3+1)/2
--  Starting with n := nmax , to be determined
--  Calculations are performed using the basis 10^7

--  with profile --_time
--  with trace  trace( 1)
    without type_check
--puts(1,"started...\n")
--  include get.e
constant pow10a7 = power(10, 7) --          10_000000
constant pow2a53 = power(2, 53) -- 9007_199254_740992
constant sal     = "e = 2."

function abs(atom x) if x<0 then x=-x end if return x end function --abs

type positive_int(integer x)
    return 1 <= x
end type --positive_int

type super_digit(integer sd)
    return abs(sd) < pow10a7
end type --super_digit

type long_int(atom x)
    return -pow2a53 <= x and x < pow2a53 and floor(x) = x
end type --long_int

-----------------------------------------------------------------------
sequence        sq, sw, swloc, exc
super_digit     mod
long_int        divn
positive_int    nsal, numsdx, nmax, fn
atom            t_ini

-----------------------------------------------------------------------
-- Set-up of the values nsal & nmax:

-- nsal will be (by convenience) a multiple of 42.
-- nmax will be the biggest integer verifying  nmax ! < 2ú10^(nsal+7)
--                                  Then,  log(nmax !) < nsal+7+log(2)

-- Table with some pair of values:
constant
nsalX={210,420,840,2100,4200,8400,21000,42000,84000,210000,420000,840000},
nmaxX={128,222,391, 844,1529,2791, 6248,11574,21544, 49312, 92662,174708}

-- Let's use, for instance:
constant k=4
--                              nsal = 210 --2100 --21000
--                              nmax = 128 --844 --6248
                    nsal=nsalX[k]
                    nmax=nmaxX[k]

-----------------------------------------------------------------------
-- Formula used in the calculation of nmax (Stirling series
-- development; numerators are the Bernoulli numbers)
-- see Francis Scheid, Numerical Analysis, pp  157, 165

-- ln(n!) = ln[sqrt(2*Pi*n)(n/e)^(n+.5)]+
--    +1/6/(2n)-1/30/(12n^3)+1/42/(30n^5)-1/30/(56n^7)+5/66/(90n^9)-...

-- ln(n!) = .5*(1+ln(2*Pi))+(n+.5)(ln(n)-1)+
--    +((...(...+1/1188)/n^2-1/1680)/n^2+1/1260)/n^2-1/360)/n^2+1/12)/n
-----------------------------------------------------------------------

t_ini = time()

numsdx = nsal / 7 + 2
sq = repeat(0, numsdx)

-- Main calculation (only two nested loops!!):

for n = nmax to 2 by -1 do
    sq[1] = 1   -- = sq[1] + 1
    mod = 0
    for j = 1 to numsdx do
        divn = mod * pow10a7 + sq[j]
        mod = remainder( divn, n)
        sq[j] = floor( divn / n)        -- Quotient is placed 'in situ'
    end for --j
end for --n

-- Pre-output:

if sq[numsdx] < 5000000 then             -- Testing 'guard' super_digit
    exc = " ..."                         -- last digit given by default
elsif sq[numsdx - 1] < 9999999 and sq[numsdx] > 5000000 then
    sq[numsdx - 1] = sq[numsdx - 1] + 1
    exc = " (-)"                         -- last digit given by excess.
else
    exc = "    "
end if

-- Output:

fn = open( "num_e.txt", "w")
sw = ""
--sw={}
for n1 = 1 to nsal/42 do
    swloc = ""
    for n2 = 6*n1 - 4 to 6*n1 + 1 do
        swloc = swloc & sprintf("%07d", sq[n2])
    end for --n2
    --  At this moment, swloc (always having 42 digits) could be
    --  handled to give a certain format to the output.
    sw = sw & swloc
--  sw = append(sw,swloc)
end for --n1
puts(fn, sal)
puts(fn, sw)
--for i=1 to length(sw) do
--  puts(fn,sw[i])
--end for
puts(fn, exc)

puts(fn, "\n\nNo. of decimal digits obtained: ")  print(fn, nsal)
puts(fn, "\nTime elapsed in seconds: ")  print(fn, time()-t_ini)
--if getc(0) then end if
