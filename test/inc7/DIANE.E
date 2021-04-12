-- diane.e
include chris.e
global procedure diane_name()
    if name!="Chris" then crash("diane_name(): name!=Chris") end if
--  printf(1,"Diane says name = \"%s\"\n",{name})
end procedure
