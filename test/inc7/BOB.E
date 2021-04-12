-- bob.e
include alice.e

global integer x
x = 1
global procedure bob_name()
    if name!="Alice" then crash("bob_name(): name!=Alice") end if
--  printf(1,"Bob says name = \"%s\"\n",{name})
end procedure
