-- The Computer Language Shootout Benchmarks
--  http://shootout.alioth.debian.org/
-- 
--  Converted to Euphoria by Jason Gade
--  run: exu fasta.ex

without warning
without type_check


--include get.e
include ../builtins/get.e
include file.e

object line
integer sum

integer fn

procedure Sum()
-- No error checking!
sum = 0
line = get(fn)
while line[1] != GET_EOF do
    sum += line[2]
    line = get(fn)
end while
end procedure
--fn = open("sumfile.txt","r")
fn = open("shootout\\sumfile.txt","r")
for i=1 to 1000 do
    Sum()
    if sum!=500 then ?9/0 end if
    if seek(fn,0)!=0 then ?9/0 end if
end for
close(fn)
--print(1, sum)
--if getc(0) then end if
