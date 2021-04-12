global integer z
z=4
include t07inc2a.e as a
include t07inc2b.e as b
include t07inc2c.e as c
include t07inc2d.e as d
--if sprintf("d.e: %d %d %d %d\n",{a:z,b:z,c:z,d:z})!="d.e: 1 2 3 4\n" then crash("d:fail\n") end if
--if {a:z,b:z,c:z,d:z}!={1,2,3,4} then crash("d:fail\n") end if
if a:z!=1 then crash("d:fail\n") end if
if b:z!=2 then crash("d:fail\n") end if
if c:z!=3 then crash("d:fail\n") end if
if d:z!=4 then crash("d:fail\n") end if
