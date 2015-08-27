-- diane.e
include chris.e
global procedure diane_name()
    if name!="Chris" then puts(1,"diane_name(): name!=Chris") abort(1) end if
--	printf(1,"Diane says name = \"%s\"\n",{name})
end procedure