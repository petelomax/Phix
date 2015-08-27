global integer k
k=3
include t08inc2a.e as a
include t08inc2b.e as b
include t08inc2c.e as c
include t08inc2d.e as d
--if sprintf("c.e: %d %d %d %d\n",{a:i,b:j,c:k,d:l})!="c.e: 1 2 3 4\n" then puts(1,"c:fail\n") abort(1) end if
--if {a:i,b:j,c:k,d:l}!={1,2,3,4} then puts(1,"c:fail\n") abort(1) end if
if a:i!=1 then puts(1,"fail[c1]\n") abort(1) end if
if b:j!=2 then puts(1,"fail[c2]\n") abort(1) end if
if c:k!=3 then puts(1,"fail[c3]\n") abort(1) end if
if d:l!=4 then puts(1,"fail[c4]\n") abort(1) end if
