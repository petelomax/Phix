-- The Great Computer Language Shootout
-- http://shootout.alioth.debian.org/
--
-- By András Szabó 2007 
-- Modified by Matt Lewis and Jason Gade
-- run: exu partial-sums.ex [N=0]

without warning
without type_check

atom res1, res2, res3, res4, res5, res6, res7, res8, res9

atom k2, k3, ksin, kcos, alt
constant tt = 2/3

procedure calc(integer N)
res1 = 0.0
res2 = 0.0
res3 = 0.0
res4 = 0.0
res5 = 0.0
res6 = 0.0
res7 = 0.0
res8 = 0.0
res9 = 0.0
alt = 1.0

for k=1 to N by 1 do
    k2 = k * k
    k3 = k2 * k
    ksin = sin(k)
    kcos = cos(k)

    res1 += power(tt, k-1)
    res2 += power(k, -0.5)
    res3 += 1 / (k * (k + 1))
    res4 += 1 / (k3 * ksin * ksin)
    res5 += 1 / (k3 * kcos * kcos)
    res6 += 1 / k
    res7 += 1 / k2
    res8 += alt / k
    res9 += alt / (2 * k - 1)
    alt *= -1
    
end for

--printf(1,"%.8f\t(2/3)^k",res1)
--printf(1,"\n%.8f\tk^-0.5",res2)
--printf(1,"\n%.8f\t1/k(k+1)",res3)
--printf(1,"\n%.8f\tFlint Hills",res4)
--printf(1,"\n%.8f\tCookson Hills",res5)
--printf(1,"\n%.8f\tHarmonic",res6)
--printf(1,"\n%.8f\tRiemann Zeta",res7)
--printf(1,"\n%.8f\tAlternating Harmonic",res8)
--printf(1,"\n%.8f\tGregory",res9)
end procedure
sequence testset
testset={
-- N, (2/3)^k,     k^-0.5,     1/k(k+1),    Flint Hills, Cookson,    Harmonic,   Riem.Zeta,  AltHarm,    Gregory
{  1, 1,           1,          0.5,         1.41228293,  3.42551882, 1,          1,          1,          1},
{  2, 5/3,         1.70710678, 2/3,         1.56346423,  4.14731872, 1.5,        1.25,       0.5,        2/3},
{  3, 2.11111111,  2.28445705, 0.75,        3.42323343,  4.18510833, 1.83333333, 1.36111111, 0.83333333, 0.86666667},
{  4, 2.40740741,  2.78445705, 0.8,         3.45051409,  4.22167943, 2.08333333, 1.42361111, 0.58333333, 0.72380952},
{ 50, 3,          12.75237394, 0.98039216,  4.79862182, 42.97406330, 4.49920534, 1.62513273, 0.68324716, 0.78039866},
{500, 3,          43.28336199, 0.99800399, 29.40596416, 42.99431856, 6.79282343, 1.64293607, 0.69214818, 0.78489816}}

atom N

for j=1 to 2000 do
    for i=1 to length(testset) do
        N=testset[i][1]
        calc(N)
        if j=1 then
            if not equal(sprintf("%.8f",res1),sprintf("%.8f",testset[i][2])) then ?9/0 end if
            if not equal(sprintf("%.8f",res2),sprintf("%.8f",testset[i][3])) then ?9/0 end if
            if not equal(sprintf("%.8f",res3),sprintf("%.8f",testset[i][4])) then ?9/0 end if
            if not equal(sprintf("%.8f",res4),sprintf("%.8f",testset[i][5])) then ?9/0 end if
            if not equal(sprintf("%.8f",res5),sprintf("%.8f",testset[i][6])) then ?9/0 end if
            if not equal(sprintf("%.8f",res6),sprintf("%.8f",testset[i][7])) then ?9/0 end if
            if not equal(sprintf("%.8f",res7),sprintf("%.8f",testset[i][8])) then ?9/0 end if
            if not equal(sprintf("%.8f",res8),sprintf("%.8f",testset[i][9])) then ?9/0 end if
            if not equal(sprintf("%.8f",res9),sprintf("%.8f",testset[i][10])) then ?9/0 end if
            testset[i]={N,res1,res2,res3,res4,res5,res6,res7,res8,res9}
        else
            if not equal(testset[i],{N,res1,res2,res3,res4,res5,res6,res7,res8,res9}) then ?9/0 end if
        end if
    end for
end for
--if getc(0) then end if

