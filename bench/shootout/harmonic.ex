 -- Computer Language Shootout
 -- http://shootout.alioth.debian.org/
 --
 --  by Jason Gade
 --  run: exu harmonic.ex [N=10000000]

without warning
without type_check

    object n

    atom partialSum

    partialSum = 0.0
    n = 10000000

    for i = 1 to n do
        partialSum += 1.0/i
    end for

    if not equal(sprintf("%0.9f",partialSum),"16.695311366") then ?9/0 end if

    partialSum = 0.0
    n = 5000000

    for i = 1 to n do
        partialSum += 1.0/i
    end for
    if not equal(sprintf("%0.9f",partialSum),"16.002164235") then ?9/0 end if

    partialSum = 0.0
    n = 1000000

    for i = 1 to n do
        partialSum += 1.0/i
    end for
    if not equal(sprintf("%0.9f",partialSum),"14.392726723") then ?9/0 end if

--  printf(1, "%0.9f\n", {partialSum})
--  if getc(0) then end if
