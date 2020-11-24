-- The Computer Language Shootout Benchmarks
--   http://shootout.alioth.debian.org/
--
--  Converted to Euphoria by Jason Gade
--  run: exu fannkuch.ex [N=1]

without warning
without type_check

include misc.e
--include get.e



function fannkuch(integer n)

    sequence start, perm, perm1, count
    integer maxFlipsCount, r
    integer perm0, flipsCount, k, k2, j, j2

    maxFlipsCount = 0

    start = repeat(1, n)
    for i = 1 to n do start[i] = i end for
    perm1 = start
    count = start
    r = n + 1

    while 1 do

        while r != 1 do count[r - 1] = r r -= 1 end while

        if not (perm1[1] = 1 or perm1[n] = n) then

            perm = perm1
            flipsCount = 0
        
            k = perm[1]
            while k != 1 do

                k2 = floor((k + 1) / 2)
                perm = reverse(perm[1..k]) & perm[k+1..n]
                flipsCount += 1
                k = perm[1]

            end while

            if flipsCount > maxFlipsCount then

                maxFlipsCount = flipsCount

            end if

        end if

        -- Use incremental change to generate another permutation
        while 1 do

            if r > n then return maxFlipsCount end if
            perm0 = perm1[1]
            j2 = 1
            while j2 < r do
            
                j = j2 + 1
                perm1[j2] = perm1[j]
                j2 = j

            end while
            perm1[r] = perm0
            
            count[r] = count[r] - 1
            if count[r] > 1 then exit else r+=1 end if

         end while

    end while

end function -- fannkuch




constant r={0,1,2,4,7,10,16,22}
atom N

for k=1 to 10 do
    for i=1 to 8 do
        N = fannkuch(i)
        if N!=r[i] then ?9/0 end if
--      printf(1, "Pfannkuchen(%d) = %d\n", {i, N})
    end for
end for

--if getc(0) then end if
