 -- Computer Language Shootout
 -- http://shootout.alioth.debian.org/
 --
 --  by Jason Gade
 --  run: exu random.ex [N=0]

without warning
without type_check

constant IM = 139968
constant IA = 3877
constant IC = 29573



integer last last = 42
function gen_random(atom max)

    last = remainder(last * IA + IC, IM)
    return( max * last / IM )

end function -- gen_random



integer N
atom v
    
    N = 2000000

    for i = N to 1 by -1 do
        v = gen_random(100.0)
    end for
    

