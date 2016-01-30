--
--  pIUP.e
--  ======
--

--global type Ihandle(object o)
--  return atom(o) and o>=NULL and o=floor(o)
global type Ihandle(integer /*i*/)
    return 1
end type

global constant
    IUP_ERROR       = 1,
    IUP_NOERROR     = 0,
    IUP_OPENED      = -1,
    IUP_IGNORE      = -1,
    IUP_DEFAULT     = -2,
    IUP_CLOSE       = -3,
    IUP_CONTINUE    = -4,
    $

constant string curr_dir = current_dir()
constant integer libidx = iff(platform()=WINDOWS ? 1:
                          iff(platform()=LINUX   ? 2:
                                                   9/0))
constant sequence dirs = {"win","lnx"}
constant string dll_path = curr_dir&sprintf("\\%s%d\\",{dirs[libidx],machine_bits()})

function iup_open_dll(sequence libs)
string fullpath = dll_path&libs[libidx]
atom res
    if chdir(dll_path)=0 then ?9/0 end if
    res = open_dll(fullpath)
    if res=0 then ?9/0 end if
    if chdir(curr_dir)=0 then ?9/0 end if
    return res
end function

atom iup = iup_open_dll({"iup.dll", "libiup.so"})
atom iupimglib = iup_open_dll({"iupimglib.dll", "libiupimglib.so"})

constant
--       D  = C_DOUBLE, 
--       F  = C_FLOAT, 
         I  = C_INT,
--       L  = C_LONG,
         P  = C_POINTER, 
--       UC = C_UCHAR,
--       UL = C_ULONG,
         $

constant 
    xIupOpen = define_c_func(iup, "+IupOpen", {P,P}, I),
    xIupClose = define_c_proc(iup, "+IupClose", {}),
    xIupDialog = define_c_func(iup, "+IupDialog", {P}, P),
    xIupShow = define_c_func(iup, "+IupShow", {P}, I),
    xIupAlarm = define_c_func(iup, "+IupAlarm", {P,P,P,P,P}, I),
    xIupMessage = define_c_proc(iup, "+IupMessage", {P,P}),
    $

global procedure IupOpen()
    if c_func(xIupOpen, {NULL,NULL})=IUP_ERROR then
        ?9/0
    end if
end procedure

global procedure IupClose()
    c_proc(xIupClose, {})
end procedure

global function IupDialog(Ihandle child = NULL)
Ihandle res = c_func(xIupDialog, {child})
    return res
end function

global procedure IupShow(Ihandle ih)
    if c_func(xIupShow, {ih})=IUP_ERROR then ?9/0 end if
end procedure

global function IupAlarm(string title, string msg, string b1, object b2 = NULL, object b3 = NULL)
    return c_func(xIupAlarm, {title,msg,b1,b2,b3})
end function

global procedure IupMessage(object title = NULL, object msg = NULL)
    c_proc(xIupMessage, {title,msg})
end procedure

