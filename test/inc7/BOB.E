-- bob.e
include alice.e

global integer x
x = 1
global procedure bob_name()
    if name!="Alice" then puts(1,"bob_name(): name!=Alice") abort(1) end if
--	printf(1,"Bob says name = \"%s\"\n",{name})
end procedure