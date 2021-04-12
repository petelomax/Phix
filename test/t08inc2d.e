global integer l
l=4
include t08inc2a.e as a
include t08inc2b.e as b
include t08inc2c.e as c
include t08inc2d.e as d
--if sprintf("d.e: %d %d %d %d\n",{a:i,b:j,c:k,d:l})!="d.e: 1 2 3 4\n" then crash("d:fail\n") end if
--if {a:i,b:j,c:k,d:l}!={1,2,3,4} then crash("d:fail\n") end if
if a:i!=1 then crash("fail[d1]\n") end if
if b:j!=2 then crash("fail[d2]\n") end if
if c:k!=3 then crash("fail[d3]\n") end if
if d:l!=4 then crash("fail[d4]\n") end if
