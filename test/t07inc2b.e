global integer z
z=2
include t07inc2a.e as a
include t07inc2b.e as b
include t07inc2c.e as c
include t07inc2d.e as d
--if sprintf("b.e: %d %d %d %d\n",{a:z,b:z,c:z,d:z})!="b.e: 1 2 3 4\n" then puts(1,"b:fail\n") abort(1) end if
--if {a:z,b:z,c:z,d:z}!={1,2,3,4} then puts(1,"b:fail\n") abort(1) end if
if a:z!=1 then puts(1,"d:fail\n") abort(1) end if
if b:z!=2 then puts(1,"d:fail\n") abort(1) end if
if c:z!=3 then puts(1,"d:fail\n") abort(1) end if
if d:z!=4 then puts(1,"d:fail\n") abort(1) end if
