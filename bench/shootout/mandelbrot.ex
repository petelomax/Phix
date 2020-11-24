-- The Computer Language Shootout Benchmarks
--  http://shootout.alioth.debian.org/
-- 
--  Converted to Euphoria by Jason Gade
--  run: exu mandelbrot.ex [N=200] [output file]
--  Note: Windows STDOUT is broken

without warning
without type_check

include get.e

procedure main(sequence argv)
    integer fn
    integer flag
    integer maxbits, padcol
    integer w, h, x, y, bit_num, byte_acc, iter
    atom limit
    atom Zr, Zi, Cr, Ci, Tr, Ti
    atom Zr2, Zi2

    object n
    
    fn = 1 -- STDOUT
    if length(argv) > 2 then
        n = value(argv[3])
        n = n[2]
        if length(argv) > 3 then
            fn = open(argv[4], "wb")
        end if
    else
        n = 200
    end if

    bit_num = 0
    byte_acc = 0
    iter = 50
    limit = 2.0 * 2.0
    
    w = n
    h = n
    
    printf(fn, "P4\n%d %d\n", {w, h})

    maxbits = 8
    if w / maxbits != 0 then
        padcol = power(2, 8 - remainder(w, 8))
    else
        padcol = 0
    end if
    
    flag = 0
    y = 0
    while y < h do
        Ci = (2 * y/h - 1)
        x = 0
        while x < w do
            Zr = 0.0
            Zr2 = 0.0
            Zi = 0.0
            Zi2 = 0.0
            Cr = (2 * x/w - 1.5)

            for i = 1 to iter do
                Tr = Zr2 - Zi2 + Cr
                Ti = 2 * Zr*Zi + Ci
                Zr = Tr
                Zi = Ti
                Zr2 = Zr * Zr
                Zi2 = Zi * Zi
                if (Zr2 + Zi2) > limit then
                    flag = 1
                    exit
                end if
            end for

            byte_acc *= 2
            byte_acc = and_bits(byte_acc, #FF) -- prevent overflow
            if flag then
                flag = 0
            else
                byte_acc += 1
            end if
            
            bit_num += 1
            if bit_num = maxbits then
                puts(fn, byte_acc)
                byte_acc = 0
                bit_num = 0
            end if

            x += 1
        end while
        
        if padcol then
            byte_acc *= padcol
            byte_acc = and_bits(byte_acc, #FF) -- prevent overflow
            puts(fn, byte_acc)
            byte_acc = 0
            bit_num = 0
        end if
            
        y += 1
    end while
    
    close(fn)
end procedure -- main



main(command_line())

