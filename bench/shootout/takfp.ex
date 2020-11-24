-- Great Computer Language Shootout
-- http://shootout.alioth.debian.org/
--
-- By Jason Gade
--
-- run: exu takfp.ex [N=1]

without warning
without type_check

include get.e



function TAK(atom x, atom y, atom z)

    if y < x then
        return TAK(TAK(x-1.0, y, z), TAK(y-1.0, z, x), TAK(z-1.0, x, y))
    else
        return z
    end if

end function -- TAK



procedure main(sequence argv)
    object N

    if length(argv) >= 3 then
        N = value(argv[3])
        N = N[2]
    else
        N = 1
    end if

    printf(1, "%.1f\n", TAK(N*3.0, N*2.0, N))
    
end procedure -- main

--atom start
--start = time()
main(command_line())
--print(2, time() - start)

