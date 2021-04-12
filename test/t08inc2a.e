global integer i
i=1
include t08inc2a.e as a
include t08inc2b.e as b
include t08inc2c.e as c
include t08inc2d.e as d
--if sprintf("a.e: %d %d %d %d\n",{a:i,b:j,c:k,d:l})!="a.e: 1 2 3 4\n" then crash("a:fail\n") end if
--if {a:i,b:j,c:k,d:l}!={1,2,3,4} then crash("a:fail\n") end if
if a:i!=1 then crash("fail[a1]\n") end if
if b:j!=2 then crash("fail[a2]\n") end if
if c:k!=3 then crash("fail[a3]\n") end if
if d:l!=4 then crash("fail[a4]\n") end if
