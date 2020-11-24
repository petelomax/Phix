-- The Computer Language Shootout Benchmarks
--   http://shootout.alioth.debian.org/
--
--   contributed by Derek Parnell
--
--   run: exu ackermann.ex [N=1]
--/**/with console

without warning
without type_check

function Ack(integer M, integer N) 
    if M = 0 then
        return N+1
    elsif N = 0 then
        return Ack(M-1,1)
    end if
    return Ack(M-1, Ack(M, N-1))
end function

for i=1 to 1500 do
    if Ack(3,1)!=13 then ?9/0 end if
    if Ack(3,2)!=29 then ?9/0 end if
    if Ack(3,3)!=61 then ?9/0 end if
    if Ack(3,4)!=125 then ?9/0 end if
end for
--  printf(1, "Ack(3,%d): %d\n", {n, Ack(3, n)})

