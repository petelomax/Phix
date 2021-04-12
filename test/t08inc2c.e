global integer k
k=3
include t08inc2a.e as a
include t08inc2b.e as b
include t08inc2c.e as c
include t08inc2d.e as d
--if sprintf("c.e: %d %d %d %d\n",{a:i,b:j,c:k,d:l})!="c.e: 1 2 3 4\n" then crash("c:fail\n") end if
--if {a:i,b:j,c:k,d:l}!={1,2,3,4} then crash("c:fail\n") end if
if a:i!=1 then crash("fail[c1]\n") end if
if b:j!=2 then crash("fail[c2]\n") end if
if c:k!=3 then crash("fail[c3]\n") end if
if d:l!=4 then crash("fail[c4]\n") end if
