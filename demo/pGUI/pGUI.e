--
 -- pUI.e
---------
-- =====
--

global type Ihandle(integer i)
    return i>0
end type

global type Ihandln(integer i)
    return i>=0
end type

sequence callbacks = {}

global type cbfunc(atom cb)
    return cb=NULL or rfind(cb,callbacks)!=0
end type

global function Icallback(sequence name, atom rid = routine_id(name))
    atom cb = call_back({'+', rid})
    callbacks = append(callbacks,cb)
    return cb
end function

--type non_null_atom(object o)
--  return atom(o) and o>NULL and o=floor(o)
--end type

--global type nullable_atom(object o)
--  return atom(o) and o>=NULL and o=floor(o)
--end type

global type nullable_string(object o)
    return string(o) or o=NULL
end type

-- only used by IupSetAttribute and cdCreateCanvas:
type atom_string(object o)
    return string(o) 
        or (integer(o) and o>=NULL) 
        or (atom(o) and o>=NULL and o=floor(o))
end type

type bool(object o)
    return integer(o) and (o=0 or o=1)
end type

--DEV...
global function rand_range(integer lo, integer hi)
    if lo>hi then {lo,hi} = {hi,lo} end if
    lo -= 1
    return lo+rand(hi-lo)
end function

-- (This is an example of an application-specific variation of OpenEuphoria's allocate_pointer_array)
function iup_ptr_array(sequence pointers)
atom pList
    pointers &= 0
    pList = allocate(length(pointers)*machine_word())
    pokeN(pList, pointers, machine_word())
    return pList
end function

function iup_peek_double(object pDouble)
sequence doubles

    if atom(pDouble) then
        return float64_to_atom(peek({pDouble,8}))
    else
        doubles = {}
        for i=1 to pDouble[2] do
            doubles &= float64_to_atom(peek({pDouble[1]+8*(i-1),8}))
        end for
        return doubles
    end if
end function

procedure iup_poke_double(atom ptr, object data)
    if atom(data) then
        poke(ptr,atom_to_float64(data))
    else
        for i=1 to length(data) do
            poke(ptr+8*(i-1),atom_to_float64(data[i]))
        end for
    end if
end procedure

procedure iup_poke_float(atom ptr, object data)
    if atom(data) then
        poke(ptr,atom_to_float32(data))
    else
        for i=1 to length(data) do
            poke(ptr+4*(i-1),atom_to_float32(data[i]))
        end for
    end if
end procedure

function iup_peek_string_pointer_array(atom ptr, integer n)
sequence strings = {}
atom pstr
    while 1 do
        pstr = peekNS(ptr,machine_word(),0)
        if pstr=NULL then exit end if
        strings = append(strings, peek_string(pstr))
        if length(strings)=n then exit end if
        ptr += machine_word()
    end while
    return strings
end function


global constant
    -- Common Flags and Return Values
    IUP_ERROR       = 1,
    IUP_NOERROR     = 0,
    IUP_OPENED      = -1,
    -- Callback Return Values
    IUP_IGNORE      = -1,
    IUP_DEFAULT     = -2,
    IUP_CLOSE       = -3,
    IUP_CONTINUE    = -4,
    -- IupGetParam Callback situations
    IUP_GETPARAM_BUTTON1 = -1,
    IUP_GETPARAM_INIT    = -2,
    IUP_GETPARAM_BUTTON2 = -3,
    IUP_GETPARAM_BUTTON3 = -4,
    IUP_GETPARAM_CLOSE   = -5,
    IUP_GETPARAM_OK      = IUP_GETPARAM_BUTTON1,
    IUP_GETPARAM_CANCEL  = IUP_GETPARAM_BUTTON2,
    IUP_GETPARAM_HELP    = IUP_GETPARAM_BUTTON3,
    -- IupPopup and IupShowXY Parameter Values (Smart positioning values)
    IUP_CENTER       = 0xFFFF, /* 65535 */
    IUP_LEFT         = 0xFFFE, /* 65534 */
    IUP_RIGHT        = 0xFFFD, /* 65533 */
    IUP_MOUSEPOS     = 0xFFFC, /* 65532 */
    IUP_CURRENT      = 0xFFFB, /* 65531 */
    IUP_CENTERPARENT = 0xFFFA, /* 65530 */
    IUP_TOP          = IUP_LEFT,
    IUP_BOTTOM       = IUP_RIGHT,
    IUP_ANYWHERE     = IUP_CURRENT,

    IUP_MASK_UINT = "/d+",

    K_BS = 8,
    K_TAB = '\t',       -- 9
    K_LF = '\n',        -- 10 (0x0A)
    K_CR = '\r',        -- 13 (0x0D)
    K_SP = ' ',         -- 32 (0x20)
    K_asterisk = '*',   -- 42 (0x2A)
    K_PAUSE  = 0xFF13,
    K_ESC    = 0xFF1B,
    K_HOME   = 0xFF50,
    K_LEFT   = 0xFF51,
    K_UP     = 0xFF52,
    K_RIGHT  = 0xFF53,
    K_DOWN   = 0xFF54,
    K_PGUP   = 0xFF55,
    K_PGDN   = 0xFF56,
    K_END    = 0xFF57,
    K_MIDDLE = 0xFF0B,
    K_Print  = 0xFF61,
    K_INS    = 0xFF63,
    K_Menu   = 0xFF67,
    K_DEL    = 0xFFFF,
    K_F1     = 0xFFBE,
    K_F2     = 0xFFBF,
    K_F3     = 0xFFC0,
    K_F4     = 0xFFC1,
    K_F5     = 0xFFC2,
    K_F6     = 0xFFC3,
    K_F7     = 0xFFC4,
    K_F8     = 0xFFC5,
    K_F9     = 0xFFC6,
    K_F10    = 0xFFC7,
    K_F11    = 0xFFC8,
    K_F12    = 0xFFC9,

    ACTION = "ACTION",
    ACTION_CB = "ACTION_CB",
    $

-- Record Input Modes
global enum
    IUP_RECBINARY = 0,
    IUP_RECTEXT,
    $

include builtins\VM\\pcmdlnN.e      -- command_line()
include builtins\pgetpath.e         -- get_proper_path()

constant string root_dir = get_proper_dir(command_line()[2])
constant string curr_dir = current_dir()
constant integer libidx = iff(platform()=WINDOWS ? 1:
                          iff(platform()=LINUX   ? 2:
                                                   9/0))
constant sequence dirs = {"win","lnx"}
constant string dll_path = root_dir&sprintf("\\%s%d\\",{dirs[libidx],machine_bits()})

function iup_open_dll(sequence libs)
string fullpath = dll_path&libs[libidx]
atom res
    if chdir(dll_path)=0 then ?9/0 end if
    res = open_dll(fullpath)
    if chdir(curr_dir)=0 then ?9/0 end if
    if res=0 then ?9/0 end if
    return res
end function

--DEV:
if platform()=WINDOWS then
    -- Aside: normally I'd expect msvcr120.dll to be loaded from system32/syswow64, 
    --        but if someone puts copies in pGUI\win32/64, it should be alright.
    if chdir(dll_path)=0 then ?9/0 end if
    if open_dll("msvcr120.dll")=0 then
        puts(1,"fatal error: msvcr120.dll could not be loaded\n")
        puts(1," try installing Visual C++ Redistributable Packages for Visual Studio 2013\n")
        puts(1," from https://www.microsoft.com/en-us/download/details.aspx?id=40784 \n")
        -- ( https://www.microsoft.com/en-us/download/details.aspx?id=40784 )
        {} = wait_key()
        ?9/0
    end if
    if chdir(curr_dir)=0 then ?9/0 end if
end if

constant
         D  = C_DOUBLE, 
         F  = C_FLOAT,      -- NB: VM/pcfuncN.e may not be up to this..
         I  = C_INT,
         L  = C_LONG,
         P  = C_POINTER, 
         U  = C_UINT,
         UC = C_UCHAR,
         UL = C_ULONG,
         $

constant atom iup = iup_open_dll({"iup.dll",
                                  "libiup.so",
                                  "libiup.dylib"})

-- Control
constant
    xIupOpen            = define_c_func(iup, "IupOpen", {I,P}, I),
    xIupClose           = define_c_proc(iup, "IupClose", {}),
--  xIupLoad            = define_c_func(iup, "IupLoad", {P},P),
--  xIupLoadBuffer      = define_c_func(iup, "IupLoadBuffer", {P}, P),
--  xIupSetLanguage     = define_c_proc(iup, "IupSetLanguage", {P}),
--  xIupGetLanguage     = define_c_func(iup, "IupGetLanguage", {}, P),
--DEV these are documented in elements...
    xIupCreate          = define_c_func(iup, "IupCreate", {P},P),
--  xIupCreatev         = define_c_func(iup, "IupCreatev", {P,P},P), -- (deliberately omitted)
    xIupDestroy         = define_c_proc(iup, "IupDestroy", {P}),
    xIupMap             = define_c_func(iup, "IupMap", {P},I),
    xIupUnmap           = define_c_proc(iup, "IupUnmap", {P}),
--DEV attributes? (nah)
    xIupVersion         = define_c_func(iup, "IupVersion", {}, P),
    xIupVersionDate     = define_c_func(iup, "IupVersionDate", {}, P),
    xIupVersionNumber   = define_c_func(iup, "IupVersionNumber", {}, I),
    $

--DEV/SUG?
--sequence cmd = command_line()
--sequence argv = cmd[3..$]
--integer argc = length(argv)
--
--int IupOpen(int *argc, char ***argv);
global procedure IupOpen()
    if c_func(xIupOpen, {NULL,NULL})=IUP_ERROR then ?9/0 end if
end procedure

global procedure IupClose()
    c_proc(xIupClose, {})
end procedure

--deprecated? (it is an LED file)
--global function IupLoad(string filename)
--atom pError = c_func(xIupLoad, {filename})
--  if pError!=0 then
--      return peek_string(pError)
--  end if
--  return 1
--end function
--
--global function IupLoadBuffer(string buffer)
--atom pError = c_func(xIupLoadBuffer, {buffer})
--  if pError!=0 then
--      return peek_string(pError)
--  end if
--  return 1
--end function

--global procedure IupSetLanguage(string language)
--  c_proc(xIupSetLanguage, {language})
--end procedure
--
--global function IupGetLanguage()
--  return peek_string(c_func(xIupGetLanguage, {}))
--end function

global function IupCreate(string name)
    Ihandle ih = c_func(xIupCreate, {name})
    return ih
end function

global procedure IupDestroy(Ihandle ih)
    c_proc(xIupDestroy, {ih})
end procedure

global procedure IupMap(Ihandle ih)
    if c_func(xIupMap, {ih})!=IUP_NOERROR then ?9/0 end if
end procedure

global procedure IupUnmap(Ihandle ih)
    c_proc(xIupUnmap, {ih})
end procedure

global function IupVersion()
    string version = peek_string(c_func(xIupVersion))
    return version
end function

global function IupVersionDate()
    string version_date = peek_string(c_func(xIupVersionDate))
    return version_date
end function

global function IupVersionNumber()
    integer version_number = c_func(xIupVersionNumber, {})
    return version_number
end function


constant
    xIupMainLoop        = define_c_proc(iup, "IupMainLoop", {}),
    xIupMainLoopLevel   = define_c_func(iup, "IupMainLoopLevel", {}, I),
    xIupLoopStep        = define_c_func(iup, "IupLoopStep", {}, I),
    xIupLoopStepWait    = define_c_func(iup, "IupLoopStepWait", {}, I),
    xIupExitLoop        = define_c_proc(iup, "IupExitLoop", {}),
    xIupFlush           = define_c_proc(iup, "IupFlush", {}),
    xIupRecordInput     = define_c_func(iup, "IupRecordInput", {P,I}, I),
    xIupPlayInput       = define_c_proc(iup, "IupPlayInput", {P}),
    xIupGetActionName   = define_c_func(iup, "IupGetActionName", {}, P)

global procedure IupMainLoop()
    c_proc(xIupMainLoop, {})
end procedure

global function IupMainLoopLevel()
    integer level = c_func(xIupMainLoopLevel, {})
    return level
end function

global function IupLoopStep()
integer res = c_func(xIupLoopStep, {})
    return res -- IUP_CLOSE or IUP_DEFAULT
end function

global function IupLoopStepWait()
integer res = c_func(xIupLoopStepWait, {})
    return res -- IUP_CLOSE or IUP_DEFAULT
end function

global procedure IupExitLoop()
    c_proc(xIupExitLoop, {})
end procedure

global procedure IupFlush()
    c_proc(xIupFlush, {})
end procedure

global procedure IupRecordInput(nullable_string filename, integer mode)
    if c_func(xIupRecordInput, {filename,mode})!=IUP_NOERROR then ?9/0 end if
end procedure

global procedure IupPlayInput(nullable_string filename = NULL)
    c_proc(xIupPlayInput, {filename})
end procedure

/* DEPRECATED callback management. It will be removed in a future version. */
-- (works only if application used IupSetFunction)
global function IupGetActionName()
    atom pName = c_func(xIupGetActionName, {})
    string name = peek_string(pName)
    return name
end function

 -- Attributes
--------------
constant
    xIupSetAttribute            = define_c_proc(iup, "IupSetAttribute", {P,P,P}),
     xIupSetAttributeId         = define_c_proc(iup, "IupSetAttributeId", {P,P,I,P}),
     xIupSetAttributeId2        = define_c_proc(iup, "IupSetAttributeId2", {P,P,I,I,P}),
     xIupSetStrAttribute        = define_c_proc(iup, "IupSetStrAttribute", {P,P,P}),
      xIupSetStrAttributeId     = define_c_proc(iup, "IupSetStrAttributeId", {P,P,I,P}),
      xIupSetStrAttributeId2    = define_c_proc(iup, "IupSetStrAttributeId2", {P,P,I,I,P}),
      xIupSetInt                = define_c_proc(iup, "IupSetInt", {P,P,I}),
      xIupSetIntId              = define_c_proc(iup, "IupSetIntId", {P,P,I,I}),
      xIupSetIntId2             = define_c_proc(iup, "IupSetIntId2", {P,P,I,I,I}),
--DEV (pcfuncN.e not coping with floats)
--    xIupSetFloat              = define_c_proc(iup, "IupSetFloat", {P,P,F}),
--    xIupSetFloatId            = define_c_proc(iup, "IupSetFloatId", {P,P,I,F}),
--    xIupSetFloatId2           = define_c_proc(iup, "IupSetFloatId2", {P,P,I,I,F}),
      xIupSetDouble             = define_c_proc(iup, "IupSetDouble", {P,P,D}),
      xIupSetDoubleId           = define_c_proc(iup, "IupSetDoubleId", {P,P,I,D}),
      xIupSetDoubleId2          = define_c_proc(iup, "IupSetDoubleId2", {P,P,I,I,D}),
      xIupSetRGB                = define_c_proc(iup, "IupSetRGB", {P,P,UC,UC,UC}),
      xIupSetRGBId              = define_c_proc(iup, "IupSetRGBId", {P,P,I,UC,UC,UC}),
      xIupSetRGBId2             = define_c_proc(iup, "IupSetRGBId2", {P,P,I,I,UC,UC,UC}),
      xIupStoreAttribute        = define_c_proc(iup, "IupStoreAttribute", {P,P,P}),
     xIupSetAttributeHandle     = define_c_proc(iup, "IupSetAttributeHandle", {P,P,P}),
      xIupSetHandle             = define_c_proc(iup, "IupSetHandle", {P,P}),
    xIupSetAttributes           = define_c_proc(iup, "IupSetAttributes", {P,P}),
    xIupResetAttribute          = define_c_proc(iup, "IupResetAttribute", {P,P}),
    xIupGetAttribute            = define_c_func(iup, "IupGetAttribute", {P,P}, P),
     xIupGetAttributeId         = define_c_func(iup, "IupGetAttributeId", {P,P,I}, P),
     xIupGetAttributeId2        = define_c_func(iup, "IupGetAttributeId2", {P,P,I,I}, P),
--   xIupGetAttributes          = define_c_func(iup, "IupGetAttributes", {P}, P), (deprecated)
     xIupGetAllAttributes       = define_c_func(iup, "IupGetAllAttributes", {P,P,I}, I),
     xIupGetAttributeHandle     = define_c_func(iup, "IupGetAttributeHandle", {P,P}, P),
      xIupGetHandle             = define_c_func(iup, "IupGetHandle", {P}, P),
     xIupGetInt                 = define_c_func(iup, "IupGetInt", {P,P}, I),
     xIupGetInt2                = define_c_func(iup, "IupGetInt2", {P,P}, I),
     xIupGetIntInt              = define_c_func(iup, "IupGetIntInt", {P,P,P,P}, I),
     xIupGetIntId               = define_c_func(iup, "IupGetIntId", {P,P,I}, I),
     xIupGetIntId2              = define_c_func(iup, "IupGetIntId2", {P,P,I,I}, I),
--   xIupGetFloat               = define_c_func(iup, "IupGetFloat", {P,P}, F),
--   xIupGetFloatId             = define_c_func(iup, "IupGetFloatId", {P,P,I}, F),
--   xIupGetFloatId2            = define_c_func(iup, "IupGetFloatId2", {P,P,I,I}, F),
     xIupGetDouble              = define_c_func(iup, "IupGetDouble", {P,P}, D),
     xIupGetDoubleId            = define_c_func(iup, "IupGetDoubleId", {P,P,I}, D),
     xIupGetDoubleId2           = define_c_func(iup, "IupGetDoubleId2", {P,P,I,I}, D),
     xIupGetRGB                 = define_c_proc(iup, "IupGetRGB", {P,P,P,P,P}),
     xIupGetRGBId               = define_c_proc(iup, "IupGetRGBId", {P,P,I,P,P,P}),
     xIupGetRGBId2              = define_c_proc(iup, "IupGetRGBId2", {P,P,I,I,P,P,P}),
    xIupSetGlobal               = define_c_proc(iup, "IupSetGlobal", {P,P}),
     xIupSetStrGlobal           = define_c_proc(iup, "IupSetStrGlobal", {P,P}),
     xIupStoreGlobal            = define_c_proc(iup, "IupStoreGlobal", {P,P}),
    xIupGetGlobal               = define_c_func(iup, "IupGetGlobal", {P}, P),
    xIupSetCallback             = define_c_func(iup, "IupSetCallback", {P,P,P}, P),
     xIupGetCallback            = define_c_func(iup, "IupGetCallback", {P,P}, P),
    xIupGetClassName            = define_c_func(iup, "IupGetClassName", {P},P),
     xIupGetAllClasses          = define_c_func(iup, "IupGetAllClasses", {P,I}, I),
     xIupGetClassType           = define_c_func(iup, "IupGetClassType", {P}, P),
     xIupGetClassAttributes     = define_c_func(iup, "IupGetClassAttributes", {P,P,I}, I),
     xIupGetClassCallbacks      = define_c_func(iup, "IupGetClassCallbacks", {P,P,I}, I),
     xIupSaveClassAttributes    = define_c_proc(iup, "IupSaveClassAttributes", {P}),
     xIupCopyClassAttributes    = define_c_proc(iup, "IupCopyClassAttributes", {P,P}),
     xIupSetClassDfltAttribute  = define_c_proc(iup, "IupSetClassDefaultAttribute", {P,P,P})

global procedure IupSetAttribute(Ihandln ih, string name, atom_string v)
    if name!=upper(name) then ?9/0 end if
    c_proc(xIupSetAttribute, {ih, name, v})
end procedure

global procedure IupSetAttributeId(Ihandle ih, string name, integer id, atom_string v)
    if name!=upper(name) then ?9/0 end if
    c_proc(xIupSetAttributeId, {ih,name,id,v})
end procedure

global procedure IupSetAttributeId2(Ihandle ih, string name, integer lin, integer col, atom_string v)
    if name!=upper(name) then ?9/0 end if
    c_proc(xIupSetAttributeId2, {ih,name,lin,col,v})
end procedure

global procedure IupSetStrAttribute(Ihandle ih, string name, nullable_string val, sequence data = {})
    if name!=upper(name) then ?9/0 end if
    if length(data) then
        val = sprintf(val, data)
    end if
    c_proc(xIupSetStrAttribute, {ih, name, val})
end procedure

global procedure IupSetStrAttributeId(Ihandle ih, string name, integer id, nullable_string v = NULL)
    c_proc(xIupSetStrAttributeId, {ih,name,id,v})
end procedure

global procedure IupSetStrAttributeId2(Ihandle ih, string name, integer lin, integer col, nullable_string v = NULL)
    c_proc(xIupSetStrAttributeId2, {ih,name,lin,col,v})
end procedure

global procedure IupSetInt(Ihandle ih, string name, integer v)
    c_proc(xIupSetInt, {ih,name,v})
end procedure

global procedure IupSetIntId(Ihandle ih, string name, integer id, integer v)
    c_proc(xIupSetIntId, {ih,name,id,v})
end procedure

global procedure IupSetIntId2(Ihandle ih, string name, integer lin, integer col, integer v)
    c_proc(xIupSetIntId2, {ih,name,lin,col,v})
end procedure

--DEV (as pcfuncN.e not coping with floats)
global procedure IupSetFloat(Ihandle ih, string name, atom v)
--  c_proc(xIupSetFloat, {ih,name,v})
    c_proc(xIupSetDouble, {ih,name,v})
end procedure

global procedure IupSetFloatId(Ihandle ih, string name, integer id, atom v)
--  c_proc(xIupSetFloatId, {ih,name,id,v})
    c_proc(xIupSetDoubleId, {ih,name,id,v})
end procedure

global procedure IupSetFloatId2(Ihandle ih, string name, integer lin, integer col, atom v)
--  c_proc(xIupSetFloatId2, {ih,name,lin,col,v})
    c_proc(xIupSetDoubleId2, {ih,name,lin,col,v})
end procedure

global procedure IupSetDouble(Ihandle ih, string name, atom v)
    c_proc(xIupSetDouble, {ih,name,v})
end procedure

global procedure IupSetDoubleId(Ihandle ih, string name, integer id, atom v)
    c_proc(xIupSetDoubleId, {ih,name,id,v})
end procedure

global procedure IupSetDoubleId2(Ihandle ih, string name, integer lin, integer col, atom v)
    c_proc(xIupSetDoubleId2, {ih,name,lin,col,v})
end procedure

global procedure IupSetRGB(Ihandle ih, string name, integer r, integer g, integer b)
    c_proc(xIupSetRGB, {ih,name,r,g,b})
end procedure

global procedure IupSetRGBId(Ihandle ih, string name, integer id, integer r, integer g, integer b)
    c_proc(xIupSetRGBId, {ih,name,id,r,g,b})
end procedure

global procedure IupSetRGBId2(Ihandle ih, string name, integer lin, integer col, integer r, integer g, integer b)
    c_proc(xIupSetRGBId2, {ih,name,lin,col,r,g,b})
end procedure

global procedure IupStoreAttribute(Ihandle ih, string name, nullable_string val, sequence data = {})
    if name!=upper(name) then ?9/0 end if
    if length(data) then
        val = sprintf(val, data)
    end if
    c_proc(xIupStoreAttribute, {ih, name, val})
end procedure

global procedure IupSetAttributes(Ihandle ih, string attributes, sequence data = {})
    if length(data) then
        attributes = sprintf(attributes, data)
    end if
    c_proc(xIupSetAttributes, {ih, attributes})
end procedure

global function IupSetAttributesf(Ihandle ih, string attributes, sequence data = {})
    IupSetAttributes(ih, attributes, data)
    return ih
end function

global procedure IupSetAttributeHandle(Ihandln ih, string name, Ihandle ih_named)
    if name!=upper(name) then ?9/0 end if
    c_proc(xIupSetAttributeHandle, {ih, name, ih_named})
end procedure

global function IupSetAttributeHandlef(Ihandln ih, string name, Ihandle ih_named)
    IupSetAttributeHandle(ih, name, ih_named)
    return ih
end function

-- (deprecated, use IupSetAttributeHandle instead)
global procedure IupSetHandle(string name, Ihandle ih)
    c_proc(xIupSetHandle, {name, ih})
end procedure

global procedure IupResetAttribute(Ihandle ih, string name)
    c_proc(xIupResetAttribute, {ih,name})
end procedure

--DEV/doc
--global function IupGetAttribute(Ihandle ih, string name)
global function IupGetAttribute(Ihandln ih, string name)
    atom ptr = c_func(xIupGetAttribute, {ih, name})
    if ptr=NULL then return "" end if
    return peek_string(ptr)
end function

global function IupGetAttributeId(Ihandle ih, string name, integer id)
    atom ptr = c_func(xIupGetAttributeId, {ih,name,id})
    if ptr=NULL then return "" end if
    return peek_string(ptr)
end function

global function IupGetAttributeId2(Ihandle ih, string name, integer lin, integer col)
    atom ptr = c_func(xIupGetAttributeId2, {ih,name,lin,col})
    if ptr=NULL then return "" end if
    return peek_string(ptr)
end function

-- Avoid. Use IupGetAllAttributes instead.
--global function IupGetAttributes(Ihandle ih)
--atom pAttributes = c_func(xIupGetAttributes, {ih})
--  return peek_string(pAttributes)
--end function

global function IupGetAllAttributes(Ihandle ih)
integer n = c_func(xIupGetAllAttributes, {ih,NULL,0})
    atom ptr = allocate(machine_word()*n, 1)
    n = c_func(xIupGetAllAttributes, {ih,ptr,n})
    return iup_peek_string_pointer_array(ptr, n)
end function

--DEV
global function IupGetAttributeHandle(Ihandle ih, string name)
    Ihandln ih_named = c_func(xIupGetAttributeHandle, {ih, name})
    return ih_named
end function

-- (deprecated, use IupGetAttributeHandle instead)
global function IupGetHandle(string name)
    Ihandln ih = c_func(xIupGetHandle, {name})
    return ih
end function

--DEV doc:
--global function IupGetInt(Ihandle ih, string name)
global function IupGetInt(Ihandln ih, string name)
    atom res = c_func(xIupGetInt, {ih, name})
    return res
end function

global function IupGetInt2(Ihandle ih, string name)
    atom res = c_func(xIupGetInt2, {ih, name})
    return res
end function

global function IupGetIntInt(Ihandle ih, string name)
sequence res
atom pTwoInts = allocate(8)
    if c_func(xIupGetIntInt, {ih,name,pTwoInts,pTwoInts+4})!=2 then ?9/0 end if
    res = peek4s({pTwoInts,2})
    free(pTwoInts)
    return res
end function

global function IupGetIntId(Ihandle ih, string name, integer id)
    atom result = c_func(xIupGetIntId, {ih,name,id})
    return result
end function

global function IupGetIntId2(Ihandle ih, string name, integer lin, integer col)
    atom result = c_func(xIupGetIntId2, {ih,name,lin,col})
    return result
end function

--DEV (ditto)
global function IupGetFloat(Ihandle ih, string name)
--  atom result = c_func(xIupGetFloat, {ih,name})
    atom result = c_func(xIupGetDouble, {ih,name})
    return result
end function

global function IupGetFloatId(Ihandle ih, string name, integer id)
--  atom result = c_func(xIupGetFloatId, {ih,name,id})
    atom result = c_func(xIupGetDoubleId, {ih,name,id})
    return result
end function

global function IupGetFloatId2(Ihandle ih, string name, integer lin, integer col)
--  atom result = c_func(xIupGetFloatId2, {ih,name,lin,col})
    atom result = c_func(xIupGetDoubleId2, {ih,name,lin,col})
    return result
end function

--DEV/doc:
--global function IupGetDouble(Ihandle ih, string name)
global function IupGetDouble(Ihandln ih, string name)
    atom result = c_func(xIupGetDouble, {ih,name})
    return result
end function

global function IupGetDoubleId(Ihandle ih, string name, integer id)
    atom result = c_func(xIupGetDoubleId, {ih,name,id})
    return result
end function

global function IupGetDoubleId2(Ihandle ih, string name, integer lin, integer col)
    atom result = c_func(xIupGetDoubleId2, {ih,name,lin,col})
    return result
end function

global function IupGetRGB(Ihandle ih, string name)
atom rgb = allocate(3, 1)
atom r = rgb+0
atom g = rgb+1
atom b = rgb+2
    c_proc(xIupGetRGB, {ih,name,r,g,b})
    return peek({rgb, 3})
end function

global function IupGetRGBId(Ihandle ih, string name, integer id)
atom rgb = allocate(3, 1)
atom r = rgb+0
atom g = rgb+1
atom b = rgb+2
    c_proc(xIupGetRGBId, {ih,name,id,r,g,b})
    return peek({rgb, 3})
end function

global function IupGetRGBId2(Ihandle ih, string name, integer lin, integer col)
atom rgb = allocate(3, 1)
atom r = rgb+0
atom g = rgb+1
atom b = rgb+2
    c_proc(xIupGetRGBId2, {ih,name,lin,col,r,g,b})
    return peek({rgb, 3})
end function

global procedure IupSetGlobal(string name, nullable_string v)
    c_proc(xIupSetGlobal, {name, v})
end procedure

global procedure IupSetStrGlobal(string name, nullable_string v)
    c_proc(xIupSetStrGlobal, {name,v})
end procedure

global procedure IupStoreGlobal(string name, nullable_string v)
    c_proc(xIupStoreGlobal, {name, v})
end procedure

global function IupGetGlobal(string name)
    atom ptr = c_func(xIupGetGlobal, {name})
    if ptr=NULL then return "" end if
    return peek_string(ptr)
end function

global procedure IupSetCallback(Ihandle ih, string name, cbfunc func)
    atom prev = c_func(xIupSetCallback, {ih, name, func})
end procedure

global function IupSetCallbackf(Ihandle ih, string name, cbfunc func)
    IupSetCallback(ih, name, func)
    return ih
end function

global function IupGetCallback(Ihandle ih, string name)
atom func = c_func(xIupGetCallback, {ih, name})
    return func
end function

global function IupGetAllClasses()
atom n = c_func(xIupGetAllClasses, {NULL,0})
atom ptr = allocate_data(sizeof(P)*n, 1)
    n = c_func(xIupGetAllClasses, {ptr,n})
    return iup_peek_string_pointer_array(ptr, n)
end function

global function IupGetClassName(Ihandle ih)
    atom pClassName = c_func(xIupGetClassName, {ih})
    return peek_string(pClassName)
end function

global function IupGetClassType(Ihandle ih)
    atom pClassType = c_func(xIupGetClassType, {ih})
    return peek_string(pClassType)
end function

global function IupGetClassAttributes(string classname)
atom n = c_func(xIupGetClassAttributes, {classname,NULL,0})
atom ptr = allocate_data(sizeof(P)*n, 1)
    n = c_func(xIupGetClassAttributes, {classname,ptr,n})
    return iup_peek_string_pointer_array(ptr, n)
end function

global function IupGetClassCallbacks(string classname)
atom n = c_func(xIupGetClassCallbacks, {classname,NULL,0})
atom ptr = allocate_data(sizeof(P)*n, 1)
    n = c_func(xIupGetClassCallbacks, {classname,ptr,n})
    return iup_peek_string_pointer_array(ptr, n)
end function

global procedure IupSaveClassAttributes(Ihandle ih)
    c_proc(xIupSaveClassAttributes, {ih})
end procedure

global procedure IupCopyClassAttributes(Ihandle src_ih, Ihandle dst_ih)
    c_proc(xIupCopyClassAttributes, {src_ih,dst_ih})
end procedure

type m1_string(object o)
--  return string(o) or o=NULL or o=-1  -- maybe?
    return string(o) or o=-1
end type

global procedure IupSetClassDefaultAttribute(string classname, string name, m1_string val)
    c_proc(xIupSetClassDfltAttribute, {classname, name, val})
end procedure


constant atom iupControls = iup_open_dll({"iupcontrols.dll",
                                          "libiupcontrols.so",
                                          "libiupcontrols.dylib"})

constant 
    xIupControlsOpen = define_c_proc(iupControls, "IupControlsOpen", {})

integer did_iup_controls_open = 0

procedure controls_open()
    if not did_iup_controls_open then
        c_proc(xIupControlsOpen, {})
    end if
    did_iup_controls_open = 1
end procedure


 -- done to here --
--------------------


global procedure set_events_callbacks(Ihandle ih, string name, sequence funcs)
    for i=1 to length(funcs) do
        IupSetCallback(ih, name, funcs[i])
    end for
end procedure


--- === ----
--DEV?
constant atom iupimglib = iup_open_dll({"iupimglib.dll",
                                        "libiupimglib.so",
                                        "libiupimglib.dylib"})
constant 
    xIupImageLibOpen = define_c_proc(iupimglib, "IupImageLibOpen", {})

global procedure IupImageLibOpen()
    c_proc(xIupImageLibOpen, {})
end procedure

--/*

constant atom iupIm = iup_open_dll({"iupim.dll",
                                    "libiupim.so",
                                    "libiupim.dylib"})


(put this with IupImage)

--*/

--/?**?/    include builtins\read_file.e





 -- pIUP\iupX.e
---------------
--
-- Copyright (C) 2008-2010 by Jeremy Cowgar <jeremy@cowgar.com>
--
-- This file is part of EuIup.
--
-- EuIup is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as
-- published by the Free Software Foundation, either version 3 of
-- the License, or (at your option) any later version.
--
-- EuIup is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public
-- License along with EuIup.  If not, see <http://www.gnu.org/licenses/>.
--

global function iup_isprint(integer c)
    return c>31 and c<127
end function

--****
-- == Iup Constants
--

--****
-- === Common return values
--

global constant
--      ERROR = 1,
--      NOERROR = 0,
--      OPENED = -1,
        INVALID = -1

--****
-- === Callback return values
--

--global constant
--      IUP_IGNORE = -1,
--      IUP_DEFAULT = -2,
--      IUP_CLOSE = -3,
--      IUP_CONTINUE = -4

--****
-- === Mouse button values
--

global constant
        BUTTON1 = '1',
        BUTTON2 = '2',
        BUTTON3 = '3',
        BUTTON4 = '4',
        BUTTON5 = '5'

--****
-- === Smart positioning values
--

--global constant
--      IUP_CENTER = 65535,
--      LEFT = 65534,
--      RIGHT = 65533,
--      MOUSEPOS = 65532,
--      CURRENT = 66531,
--      IUP_CENTERPARENT = 65530,
----public constant IUP_CENTERPARENT = 0xFFFA /* 65530 */
--      TOP = IUP_CENTER,
--      BOTTOM = RIGHT,
--      ANYWHERE = CURRENT

--****
-- === Generic
--

--/*
global constant
        RUN = "RUN",
        ENGLISH = "ENGLISH",
        PORTUGUESE = "PORTUGUESE",
        SBH = "SBH",
        SBV = "SBV",
        --
        -- Acoes
        --
        DEFAULT_ACTION = "DEFAULT_ACTION",
        IDLE_ACTION = "IDLE_ACTION",
--      ACTION = "ACTION",
--      ACTION_CB = "ACTION_CB",
        GETFOCUS_CB = "GETFOCUS_CB",
        KILLFOCUS_CB = "KILLFOCUS_CB",
        K_ANY = "K_ANY",
        KEYPRESS_CB = "KEYPRESS_CB",
        HELP_CB = "HELP_CB",
        SCROLL_CB = "SCROLL_CB",
        RESIZE_CB = "RESIZE_CB",
        MOTION_CB = "MOTION_CB",
        BUTTON_CB = "BUTTON_CB",
        ENTERWINDOW_CB = "ENTERWINDOW_CB",
        LEAVEWINDOW_CB = "LEAVEWINDOW_CB",
        WHEEL_CB = "WHEEL_CB",
        MASK_CB = "MASK_CB",
        OPEN_CB = "OPEN_CB",
        HIGHLIGHT_CB = "HIGHLIGHT_CB",
        MENUCLOSE_CB = "MENUCLOSE_CB",
        MAP_CB = "MAP_CB",
        CLOSE_CB = "CLOSE_CB",
        SHOW_CB = "SHOW_CB",
        DROPFILES_CB = "DROPFILES_CB",
        WOM_CB = "WOM_CB",
        --
        -- Atributos
        --
        DIRECTION = "DIRECTION",
        ACTIVE = "ACTIVE",
        BGCOLOR = "BGCOLOR",
        FRAMECOLOR = "FRAMECOLOR",
        FGCOLOR = "FGCOLOR",
        COLOR = "COLOR",
        WID = "WID",
        SIZE = "SIZE",
        RASTERSIZE = "RASTERSIZE",
        TITLE = "TITLE",
        IUP_VALUE = "VALUE",
        VISIBLE = "VISIBLE",
        FONT = "FONT",
        TIP = "TIP",
        EXPAND = "EXPAND",
        SEPARATOR = "SEPARATOR",
        HOTSPOT = "HOTSPOT",
        HEIGHT = "HEIGHT",
        WIDTH = "WIDTH",
        KEY = "KEY",
        MULTIPLE = "MULTIPLE",
        DROPDOWN = "DROPDOWN",
        VISIBLE_ITEMS = "VISIBLE_ITEMS",
        MARGIN = "MARGIN",
        GAP = "GAP",
        ALIGNMENT = "ALIGNMENT",
        IMAGE = "IMAGE",
        IMINACTIVE = "IMINACTIVE",
        IMPRESS = "IMPRESS",
        WIN_SAVEBITS = "WIN_SAVEBITS",
        NC = "NC",
        MASK = "MASK",
        APPEND = "APPEND",
        BORDER = "BORDER",
        CARET = "CARET",
        SELECTION = "SELECTION",
        SELECTEDTEXT = "SELECTEDTEXT",
        INSERT = "INSERT",
        CONID = "CONID",
        CURSOR = "CURSOR",
        ICON = "ICON",
        MENUBOX = "MENUBOX",
        MINBOX = "MINBOX",
        MAXBOX = "MAXBOX",
        RESIZE = "RESIZE",
        IUP_MENU = "MENU",
        STARTFOCUS = "STARTFOCUS",
        PARENTDIALOG = "PARENTDIALOG",
        SHRINK = "SHRINK",
        DEFAULTENTER = "DEFAULTENTER",
        DEFAULTESC = "DEFAULTESC",
        X = "X",
        Y = "Y",
        TOOLBOX = "TOOLBOX",
        CONTROL = "CONTROL",
        READONLY = "READONLY",
        SCROLLBAR = "SCROLLBAR",
        POSY = "POSY",
        POSX = "POSX",
        DX = "DX",
        DY = "DY",
        XMAX = "XMAX",
        XMIN = "XMIN",
        YMAX = "YMAX",
        YMIN = "YMIN",
--PL (possible conflict with graphics.e)
--      RED = "255 0 0",
--      GREEN = "0 255 0",
--      BLUE = "0 0 255",
        MIN = "MIN",
        MAX = "MAX",
        TIME = "TIME",
        DRAG = "DRAG",
        DROP = "DROP",
        REPAINT = "REPAINT",
        TOPMOST = "TOPMOST",
        CLIPCHILDREN = "CLIPCHILDREN",
        DIALOGTYPE = "DIALOGTYPE",
        FILE = "FILE",
        MULTIPLEFILES = "MULTIPLEFILES",
        FILTER = "FILTER",
        FILTERUSED = "FILTERUSED",
        FILTERINFO = "FILTERINFO",
        EXTFILTER = "EXTFILTER",
        DIRECTORY = "DIRECTORY",
        ALLOWNEW = "ALLOWNEW",
        NOOVERWRITEPROMPT = "NOOVERWRITEPROMPT",
        NOCHANGEDIR = "NOCHANGEDIR",
        FILEEXIST = "FILEEXIST",
        STATUS = "STATUS",
        LOCKLOOP = "LOCKLOOP",
        SYSTEM = "SYSTEM",
        DRIVER = "DRIVER",
        SCREENSIZE = "SCREENSIZE",
        SYSTEMLANGUAGE = "SYSTEMLANGUAGE",
        COMPUTERNAME = "COMPUTERNAME",
        USERNAME = "USERNAME",
        --
        --                   Valores para o filedlg
        --
        OPEN = "OPEN",
        SAVE = "SAVE",
        DIR = "DIR",
        --
        -- Valores para scrollbar
        --
        HORIZONTAL = "HORIZONTAL",
        VERTICAL = "VERTICAL",
        --
        -- Valores dos Atributos
        --
        YES = "YES",
        NO = "NO",
        ON = "ON",
        OFF = "OFF",
        ACENTER = "ACENTER",
        ALEFT = "ALEFT",
        ARIGHT = "ARIGHT",
        ATOP = "ATOP",
        ABOTTOM = "ABOTTOM",
        NORTH = "NORTH",
        SOUTH = "SOUTH",
        WEST = "WEST",
        EAST = "EAST",
        NE = "NE",
        SE = "SE",
        NW = "NW",
        SW = "SW",
        FULLSCREEN = "FULLSCREEN",
        FULL = "FULL",
        HALF = "HALF",
        THIRD = "THIRD",
        QUARTER = "QUARTER",
        EIGHTH = "EIGHTH",
        ARROW = "ARROW",
        BUSY = "BUSY",
        RESIZE_N = "RESIZE_N",
        RESIZE_S = "RESIZE_S",
        RESIZE_E = "RESIZE_E",
        RESIZE_W = "RESIZE_W",
        RESIZE_NE = "RESIZE_NE",
        RESIZE_NW = "RESIZE_NW",
        RESIZE_SE = "RESIZE_SE",
        RESIZE_SW = "RESIZE_SW",
        MOVE = "MOVE",
        HAND = "HAND",
        NONE = "NONE",
        IUP = "IUP",
        CROSS = "CROSS",
        PEN = "PEN",
        TEXT = "TEXT",
        RESIZE_C = "RESIZE_C",
        OPENHAND = "OPENHAND",
        HELVETICA_NORMAL_8 = "HELVETICA_NORMAL_8",
        HELVETICA_ITALIC_8 = "HELVETICA_ITALIC_8",
        HELVETICA_BOLD_8 = "HELVETICA_BOLD_8",
        HELVETICA_NORMAL_10 = "HELVETICA_NORMAL_10",
        HELVETICA_ITALIC_10 = "HELVETICA_ITALIC_10",
        HELVETICA_BOLD_10 = "HELVETICA_BOLD_10",
        HELVETICA_NORMAL_12 = "HELVETICA_NORMAL_12",
        HELVETICA_ITALIC_12 = "HELVETICA_ITALIC_12",
        HELVETICA_BOLD_12 = "HELVETICA_BOLD_12",
        HELVETICA_NORMAL_14 = "HELVETICA_NORMAL_14",
        HELVETICA_ITALIC_14 = "HELVETICA_ITALIC_14",
        HELVETICA_BOLD_14 = "HELVETICA_BOLD_14",
        COURIER_NORMAL_8 = "COURIER_NORMAL_8",
        COURIER_ITALIC_8 = "COURIER_ITALIC_8",
        COURIER_BOLD_8 = "COURIER_BOLD_8",
        COURIER_NORMAL_10 = "COURIER_NORMAL_10",
        COURIER_ITALIC_10 = "COURIER_ITALIC_10",
        COURIER_BOLD_10 = "COURIER_BOLD_10",
        COURIER_NORMAL_12 = "COURIER_NORMAL_12",
        COURIER_ITALIC_12 = "COURIER_ITALIC_12",
        COURIER_BOLD_12 = "COURIER_BOLD_12",
        COURIER_NORMAL_14 = "COURIER_NORMAL_14",
        COURIER_ITALIC_14 = "COURIER_ITALIC_14",
        COURIER_BOLD_14 = "COURIER_BOLD_14",
        TIMES_NORMAL_8 = "TIMES_NORMAL_8",
        TIMES_ITALIC_8 = "TIMES_ITALIC_8",
        TIMES_BOLD_8 = "TIMES_BOLD_8",
        TIMES_NORMAL_10 = "TIMES_NORMAL_10",
        TIMES_ITALIC_10 = "TIMES_ITALIC_10",
        TIMES_BOLD_10 = "TIMES_BOLD_10",
        TIMES_NORMAL_12 = "TIMES_NORMAL_12",
        TIMES_ITALIC_12 = "TIMES_ITALIC_12",
        TIMES_BOLD_12 = "TIMES_BOLD_12",
        TIMES_NORMAL_14 = "TIMES_NORMAL_14",
        TIMES_ITALIC_14 = "TIMES_ITALIC_14",
        TIMES_BOLD_14 = "TIMES_BOLD_14",
        --
        -- Dial
        --
        ICTL_DENSITY = "DENSITY",
        ICTL_HORIZONTAL = "HORIZONTAL",
        ICTL_VERTICAL = "VERTICAL",
        ICTL_CIRCULAR = "CIRCULAR",
        ICTL_UNIT = "UNIT",
        --
        -- Matrix
        --
        ENTERITEM_CB = "ENTERITEM_CB",
        LEAVEITEM_CB = "LEAVEITEM_CB",
        EDITION_CB = "EDITION_CB",
        CLICK_CB = "CLICK_CB",
        DROP_CB = "DROP_CB",
        DROPSELECT_CB = "DROPSELECT_CB",
        DROPCHECK_CB = "DROPCHECK_CB",
        VALUE_CB = "VALUE_CB",
        VALUE_EDIT_CB = "VALUE_EDIT_CB",
        FIELD_CB = "FIELD_CB",
        RESIZEMATRIX = "RESIZEMATRIX",
        ADDLIN = "ADDLIN",
        ADDCOL = "ADDCOL",
        DELLIN = "DELLIN",
        DELCOL = "DELCOL",
        NUMLIN = "NUMLIN",
        NUMCOL = "NUMCOL",
        NUMLIN_VISIBLE = "NUMLIN_VISIBLE",
        NUMCOL_VISIBLE = "NUMCOL_VISIBLE",
        MARKED = "MARKED",
        WIDTHDEF = "WIDTHDEF",
        HEIGHTDEF = "HEIGHTDEF",
        AREA = "AREA",
        MARK_MODE = "MARK_MODE",
        LIN = "LIN",
        COL = "COL",
        LINCOL = "LINCOL",
        CELL = "CELL",
        EDIT_MODE = "EDIT_MODE",
        FOCUS_CELL = "FOCUS_CELL",
        ORIGIN = "ORIGIN",
        REDRAW = "REDRAW",
        PREVIOUSVALUE = "PREVIOUSVALUE",
        MOUSEMOVE_CB = "MOUSEMOVE_CB",
        --
        -- Tree
        --
        ADDLEAF = "ADDLEAF",
        ADDBRANCH = "ADDBRANCH",
        DELNODE = "DELNODE",
        IMAGELEAF = "IMAGELEAF",
        IMAGEBRANCHCOLLAPSED = "IMAGEBRANCHCOLLAPSED",
        IMAGEBRANCHEXPANDED = "IMAGEBRANCHEXPANDED",
        IMAGEEXPANDED = "IMAGEEXPANDED",
        KIND = "KIND",
        PARENT = "PARENT",
        DEPTH = "DEPTH",
        ADDEXPANDED = "ADDEXPANDED",
        CTRL = "CTRL",
        SHIFT = "SHIFT",
        STATE = "STATE",
        STARTING = "STARTING",
        LEAF = "LEAF",
        BRANCH = "BRANCH",
        SELECTED = "SELECTED",
        CHILDREN = "CHILDREN",
        ROOT = "ROOT",
        LAST = "LAST",
        PGUP = "PGUP",
        PGDN = "PGDN",
        NEXT = "NEXT",
        PREVIOUS = "PREVIOUS",
        INVERT = "INVERT",
        BLOCK = "BLOCK",
        CLEARALL = "CLEARALL",
        MARKALL = "MARKALL",
        INVERTALL = "INVERTALL",
        COLLAPSED = "COLLAPSED",
        EXPANDED = "EXPANDED",
        SELECTION_CB = "SELECTION_CB",
        BRANCHOPEN_CB = "BRANCHOPEN_CB",
        BRANCHCLOSE_CB = "BRANCHCLOSE_CB",
        RIGHTCLICK_CB = "RIGHTCLICK_CB",
        EXECUTELEAF_CB = "EXECUTELEAF_CB",
        RENAMENODE_CB = "RENAMENODE_CB",
        IMGLEAF = "IMGLEAF",
        IMGCOLLAPSED = "IMGCOLLAPSED",
        IMGEXPANDED = "IMGEXPANDED",
        IMGBLANK = "IMGBLANK",
        IMGPAPER = "IMGPAPER",
        FORMATTING = "FORMATTING",
        SCROLLTO = "SCROLLTO",
        SCROLLTOPOS = "SCROLLTOPOS",
        CARETPOS = "CARETPOS"
--*/

--global enum
--      IUP_RECBINARY = 0,
--      IUP_RECTEXT,
--$

--****
-- === Keyboard
--

--/*
global constant
        K_exclam = "K_exclam",
        K_quotedbl = "K_quotedbl",
        K_numbersign = "K_numbersign",
        K_dollar = "K_dollar",
        K_percent = "K_percent",
        K_ampersand = "K_ampersand",
        K_quoteright = "K_quoteright",
        K_parentleft = "K_parentleft",
        K_parentright = "K_parentright",
        K_asterisk = "K_asterisk",
        K_plus = "K_plus",
        K_comma = "K_comma",
        K_minus = "K_minus",
        K_period = "K_period",
        K_slash = "K_slash",
        K_0 = "K_0",
        K_1 = "K_1",
        K_2 = "K_2",
        K_3 = "K_3",
        K_4 = "K_4",
        K_5 = "K_5",
        K_6 = "K_6",
        K_7 = "K_7",
        K_8 = "K_8",
        K_9 = "K_9",
        K_colon = "K_colon",
        K_semicolon = "K_semicolon = ",
        K_less = "K_less",
        K_equal = "K_equal",
        K_greater = "K_greater",
        K_question = "K_question",
        K_at = "K_at",
        K_A = "K_A",
        K_B = "K_B",
        K_C = "K_C",
        K_D = "K_D",
        K_E = "K_E",
        K_F = "K_F",
        K_G = "K_G",
        K_H = "K_H",
        K_I = "K_I",
        K_J = "K_J",
        K_K = "K_K",
        K_L = "K_L",
        K_M = "K_M",
        K_N = "K_N",
        K_O = "K_O",
        K_P = "K_P",
        K_Q = "K_Q",
        K_R = "K_R",
        K_S = "K_S",
        K_T = "K_T",
        K_U = "K_U",
        K_V = "K_V",
        K_W = "K_W",
        K_X = "K_X",
        K_Y = "K_Y",
        K_Z = "K_Z",
        K_bracketleft = "K_bracketleft",
        K_backslash = "K_backslash",
        K_bracketright = "K_bracketright",
        K_circum = "K_circum",
        K_underscore = "K_underscore",
        K_quoteleft = "K_quoteleft",
        K_a = "K_a",
        K_b = "K_b",
        K_c = "K_c",
        K_d = "K_d",
        K_e = "K_e",
        K_f = "K_f",
        K_g = "K_g",
        K_h = "K_h",
        K_i = "K_i",
        K_j = "K_j",
        K_k = "K_k",
        K_l = "K_l",
        K_m = "K_m",
        K_n = "K_n",
        K_o = "K_o",
        K_p = "K_p",
        K_q = "K_q",
        K_r = "K_r",
        K_s = "K_s",
        K_t = "K_t",
        K_u = "K_u",
        K_v = "K_v",
        K_w = "K_w",
        K_x = "K_x",
        K_y = "K_y",
        K_z = "K_z",
        K_braceleft = "K_braceleft",
        K_bar = "K_bar",
        K_braceright = "K_braceright",
        K_tilde = "K_tilde",
        K_cA = "K_cA",
        K_cB = "K_cB",
        K_cC = "K_cC",
        K_cD = "K_cD",
        K_cE = "K_cE",
        K_cF = "K_cF",
        K_cG = "K_cG",
        K_cJ = "K_cJ",
        K_cK = "K_cK",
        K_cL = "K_cL",
        K_cN = "K_cN",
        K_cO = "K_cO",
        K_cP = "K_cP",
        K_cQ = "K_cQ",
        K_cR = "K_cR",
        K_cS = "K_cS",
        K_cT = "K_cT",
        K_cU = "K_cU",
        K_cV = "K_cV",
        K_cW = "K_cW",
        K_cX = "K_cX",
        K_cY = "K_cY",
        K_cZ = "K_cZ",
        K_mA = "K_mA",
        K_mB = "K_mB",
        K_mC = "K_mC",
        K_mD = "K_mD",
        K_mE = "K_mE",
        K_mF = "K_mF",
        K_mG = "K_mG",
        K_mH = "K_mH",
        K_mI = "K_mI",
        K_mJ = "K_mJ",
        K_mK = "K_mK",
        K_mL = "K_mL",
        K_mM = "K_mM",
        K_mN = "K_mN",
        K_mO = "K_mO",
        K_mP = "K_mP",
        K_mQ = "K_mQ",
        K_mR = "K_mR",
        K_mS = "K_mS",
        K_mT = "K_mT",
        K_mU = "K_mU",
        K_mV = "K_mV",
        K_mW = "K_mW",
        K_mX = "K_mX",
        K_mY = "K_mY",
        K_mZ = "K_mZ",
        K_BS = "K_BS",
        K_TAB = "K_TAB",
        K_CR = "K_CR",
        K_SP = "K_SP",
        K_ESC = "K_ESC",
        K_sCR = "K_sCR",
        K_sTAB = "K_sTAB",
        K_cTAB = "K_cTAB",
        K_mTAB = "K_mTAB",
        K_HOME = "K_HOME",
        K_UP = "K_UP",
        K_PGUP = "K_PGUP",
        K_LEFT = "K_LEFT",
        K_RIGHT = "K_RIGHT",
        K_END = "K_END",
        K_DOWN = "K_DOWN",
        K_PGDN = "K_PGDN",
        K_MIDDLE = "K_MIDDLE",
        K_INS = "K_INS",
        K_DEL = "K_DEL",
        K_sHOME = "K_sHOME",
        K_sUP = "K_sUP",
        K_sPGUP = "K_sPGUP",
        K_sLEFT = "K_sLEFT",
        K_sRIGHT = "K_sRIGHT",
        K_sEND = "K_sEND",
        K_sDOWN = "K_sDOWN",
        K_sPGDN = "K_sPGDN",
        K_cHOME = "K_cHOME",
        K_cPGUP = "K_cPGUP",
        K_cLEFT = "K_cLEFT",
        K_cRIGHT = "K_cRIGHT",
        K_cEND = "K_cEND",
        K_cPGDN = "K_cPGDN",
        K_cUP = "K_cUP",
        K_cDOWN = "K_cDOWN",
        K_cMIDDLE = "K_cMIDDLE",
        K_cINS = "K_cINS",
        K_cDEL = "K_cDEL",
        K_mHOME = "K_mHOME",
        K_mPGUP = "K_mPGUP",
        K_mLEFT = "K_mLEFT",
        K_mRIGHT = "K_mRIGHT",
        K_mEND = "K_mEND",
        K_mPGDN = "K_mPGDN",
        K_mUP = "K_mUP",
        K_mDOWN = "K_mDOWN",
        K_mINS = "K_mINS",
        K_mDEL = "K_mDEL",
        K_F1 = "K_F1",
        K_F2 = "K_F2",
        K_F3 = "K_F3",
        K_F4 = "K_F4",
        K_F5 = "K_F5",
        K_F6 = "K_F6",
        K_F7 = "K_F7",
        K_F8 = "K_F8",
        K_F9 = "K_F9",
        K_F10 = "K_F10",
        K_F11 = "K_F11",
        K_F12 = "K_F12",
        K_sF1 = "K_sF1",
        K_sF2 = "K_sF2",
        K_sF3 = "K_sF3",
        K_sF4 = "K_sF4",
        K_sF5 = "K_sF5",
        K_sF6 = "K_sF6",
        K_sF7 = "K_sF7",
        K_sF8 = "K_sF8",
        K_sF9 = "K_sF9",
        K_sF10 = "K_sF10",
        K_sF11 = "K_sF11",
        K_sF12 = "K_sF12",
        K_cF1 = "K_cF1",
        K_cF2 = "K_cF2",
        K_cF3 = "K_cF3",
        K_cF4 = "K_cF4",
        K_cF5 = "K_cF5",
        K_cF6 = "K_cF6",
        K_cF7 = "K_cF7",
        K_cF8 = "K_cF8",
        K_cF9 = "K_cF9",
        K_cF10 = "K_cF10",
        K_cF11 = "K_cF11",
        K_cF12 = "K_cF12",
        K_mF1 = "K_mF1",
        K_mF2 = "K_mF2",
        K_mF3 = "K_mF3",
        K_mF4 = "K_mF4",
        K_mF5 = "K_mF5",
        K_mF6 = "K_mF6",
        K_mF7 = "K_mF7",
        K_mF8 = "K_mF8",
        K_mF9 = "K_mF9",
        K_mF10 = "K_mF10",
        K_m1 = "K_m1",
        K_m2 = "K_m2",
        K_m3 = "K_m3",
        K_m4 = "K_m4",
        K_m5 = "K_m5",
        K_m6 = "K_m6",
        K_m7 = "K_m7",
        K_m8 = "K_m8",
        K_m9 = "K_m9",
        K_m0 = "K_m0"
--*/
--public include iupkey.e
/* from 32 to 126, all character sets are equal, the key code is the same as the ASCii character code. */
--/*
global constant K_SP = ' '   /* 32 (0x20) */
global constant K_exclam = '!' /* 33 */
global constant K_quotedbl = '\"' /* 34 */
global constant K_numbersign = '#' /* 35 */
global constant K_dollar = '$' /* 36 */
global constant K_percent = '%' /* 37 */
global constant K_ampersand = '&' /* 38 */
global constant K_apostrophe = '\'' /* 39 */
global constant K_parentleft = '(' /* 40 */
global constant K_parentright = ')' /* 41 */
global constant K_asterisk = '*' /* 42 */
global constant K_plus = '+' /* 43 */
global constant K_comma = ',' /* 44 */
global constant K_minus = '-' /* 45 */
global constant K_period = '.' /* 46 */
global constant K_slash = '/' /* 47 */
global constant K_0 = '0' /* 48 (0x30) */
global constant K_1 = '1' /* 49 */
global constant K_2 = '2' /* 50 */
global constant K_3 = '3' /* 51 */
global constant K_4 = '4' /* 52 */
global constant K_5 = '5' /* 53 */
global constant K_6 = '6' /* 54 */
global constant K_7 = '7' /* 55 */
global constant K_8 = '8' /* 56 */
global constant K_9 = '9' /* 57 */
global constant K_colon = ':' /* 58 */
global constant K_semicolon = ';' /* 59 */
global constant K_less = '<' /* 60 */
global constant K_equal = '=' /* 61 */
global constant K_greater = '>' /* 62 */
global constant K_question = '?' /* 63 */
global constant K_at = '@' /* 64 */
global constant K_A = 'A' /* 65 (0x41) */
global constant K_B = 'B' /* 66 */
global constant K_C = 'C' /* 67 */
global constant K_D = 'D' /* 68 */
global constant K_E = 'E' /* 69 */
global constant K_F = 'F' /* 70 */
global constant K_G = 'G' /* 71 */
global constant K_H = 'H' /* 72 */
global constant K_I = 'I' /* 73 */
global constant K_J = 'J' /* 74 */
global constant K_K = 'K' /* 75 */
global constant K_L = 'L' /* 76 */
global constant K_M = 'M' /* 77 */
global constant K_N = 'N' /* 78 */
global constant K_O = 'O' /* 79 */
global constant K_P = 'P' /* 80 */
global constant K_Q = 'Q' /* 81 */
global constant K_R = 'R' /* 82 */
global constant K_S = 'S' /* 83 */
global constant K_T = 'T' /* 84 */
global constant K_U = 'U' /* 85 */
global constant K_V = 'V' /* 86 */
global constant K_W = 'W' /* 87 */
global constant K_X = 'X' /* 88 */
global constant K_Y = 'Y' /* 89 */
global constant K_Z = 'Z' /* 90 */
global constant K_bracketleft = '[' /* 91 */
global constant K_backslash = '\\' /* 92 */
global constant K_bracketright = ']' /* 93 */
global constant K_circum = '^' /* 94 */
global constant K_underscore = '_' /* 95 */
global constant K_grave = '`' /* 96 */
global constant K_a = 'a' /* 97 (0x61) */
global constant K_b = 'b' /* 98 */
global constant K_c = 'c' /* 99 */
global constant K_d = 'd' /* 100 */
global constant K_e = 'e' /* 101 */
global constant K_f = 'f' /* 102 */
global constant K_g = 'g' /* 103 */
global constant K_h = 'h' /* 104 */
global constant K_i = 'i' /* 105 */
global constant K_j = 'j' /* 106 */
global constant K_k = 'k' /* 107 */
global constant K_l = 'l' /* 108 */
global constant K_m = 'm' /* 109 */
global constant K_n = 'n' /* 110 */
global constant K_o = 'o' /* 111 */
global constant K_p = 'p' /* 112 */
global constant K_q = 'q' /* 113 */
global constant K_r = 'r' /* 114 */
global constant K_s = 's' /* 115 */
global constant K_t = 't' /* 116 */
global constant K_u = 'u' /* 117 */
global constant K_v = 'v' /* 118 */
global constant K_w = 'w' /* 119 */
global constant K_x = 'x' /* 120 */
global constant K_y = 'y' /* 121 */
global constant K_z = 'z' /* 122 */
global constant K_braceleft = '{' /* 123 */
global constant K_bar = '|' /* 124 */
global constant K_braceright = '}' /* 125 */
global constant K_tilde = '~' /* 126 (0x7E) */
--*/
/* Printable ASCii keys */
/* also define the escape sequences that have keys associated */
--/*
global constant K_BS = 8 /* 8 */
global constant K_TAB = '\t' /* 9 */
global constant K_LF = '\n' /* 10 (0x0A) not a real key, is a combination of CR with a modifier, just to document */
global constant K_CR = '\r' /* 13 (0x0D) */
/* backward compatible definitions */
global constant K_quoteleft = K_grave
global constant K_quoteright = K_apostrophe
/* IUP Extended Key Codes, range start at 128      */
global constant K_PAUSE = 0xFF13
global constant K_ESC = 0xFF1B
global constant K_HOME = 0xFF50
global constant K_LEFT = 0xFF51
global constant K_UP = 0xFF52
global constant K_RIGHT = 0xFF53
global constant K_DOWN = 0xFF54
global constant K_PGUP = 0xFF55
global constant K_PGDN = 0xFF56
global constant K_END = 0xFF57
global constant K_MIDDLE = 0xFF0B
global constant K_Print = 0xFF61
global constant K_INS = 0xFF63
global constant K_Menu = 0xFF67
global constant K_DEL = 0xFFFF
global constant K_F1 = 0xFFBE
global constant K_F2 = 0xFFBF
global constant K_F3 = 0xFFC0
global constant K_F4 = 0xFFC1
global constant K_F5 = 0xFFC2
global constant K_F6 = 0xFFC3
global constant K_F7 = 0xFFC4
global constant K_F8 = 0xFFC5
global constant K_F9 = 0xFFC6
global constant K_F10 = 0xFFC7
global constant K_F11 = 0xFFC8
global constant K_F12 = 0xFFC9
/* no Shift/Ctrl/Alt */
global constant K_LSHIFT = 0xFFE1
global constant K_RSHIFT = 0xFFE2
global constant K_LCTRL = 0xFFE3
global constant K_RCTRL = 0xFFE4
global constant K_LALT = 0xFFE9
global constant K_RALT = 0xFFEA
global constant K_NUM = 0xFF7F
global constant K_SCROLL = 0xFF14
global constant K_CAPS = 0xFFE5
/* Also, these are the same as the Latin-1 definition */
global constant K_ccedilla = 0x00E7
global constant K_Ccedilla = 0x00C7
global constant K_acute = 0x00B4 /* no Shift/Ctrl/Alt */
global constant K_diaeresis = 0x00A8
/******************************************************/
/* Modifiers use last 4 bits. Since IUP 3.9           */
/* These modifiers definitions are specific to IUP    */
/******************************************************/
global function iup_isShiftXkey( atom _c )
        return and_bits( _c, 0x10000000 )
end function

global function iup_isCtrlXkey( atom _c )
        return and_bits( _c, 0x20000000 )
end function

global function iup_isAltXkey( atom _c )
        return and_bits( _c, 0x40000000 )
end function

global function iup_isSysXkey( atom _c )
        return and_bits( _c, 0x80000000 )
end function

global function iup_XkeyBase( atom _c )
        return and_bits( _c, 0x0FFFFFFF )
end function

global function iup_XkeyShift( atom _c )
        return or_bits( _c, 0x10000000 )
end function

global function iup_XkeyCtrl( atom _c )
        return or_bits( _c, 0x20000000 )
end function

global function iup_XkeyAlt( atom _c )
        return or_bits( _c, 0x40000000 )
end function

global function iup_XkeySys( atom _c )
        return or_bits( _c, 0x80000000 )
end function

--*/
--/*
global constant K_sHOME = iup_XkeyShift(K_HOME )
global constant K_sUP = iup_XkeyShift(K_UP )
global constant K_sPGUP = iup_XkeyShift(K_PGUP )
global constant K_sLEFT = iup_XkeyShift(K_LEFT )
global constant K_sMIDDLE = iup_XkeyShift(K_MIDDLE )
global constant K_sRIGHT = iup_XkeyShift(K_RIGHT )
global constant K_sEND = iup_XkeyShift(K_END )
global constant K_sDOWN = iup_XkeyShift(K_DOWN )
global constant K_sPGDN = iup_XkeyShift(K_PGDN )
global constant K_sINS = iup_XkeyShift(K_INS )
global constant K_sDEL = iup_XkeyShift(K_DEL )
global constant K_sSP = iup_XkeyShift(K_SP )
global constant K_sTAB = iup_XkeyShift(K_TAB )
global constant K_sCR = iup_XkeyShift(K_CR )
global constant K_sBS = iup_XkeyShift(K_BS )
global constant K_sPAUSE = iup_XkeyShift(K_PAUSE )
global constant K_sESC = iup_XkeyShift(K_ESC )
global constant K_sF1 = iup_XkeyShift(K_F1 )
global constant K_sF2 = iup_XkeyShift(K_F2 )
global constant K_sF3 = iup_XkeyShift(K_F3 )
global constant K_sF4 = iup_XkeyShift(K_F4 )
global constant K_sF5 = iup_XkeyShift(K_F5 )
global constant K_sF6 = iup_XkeyShift(K_F6 )
global constant K_sF7 = iup_XkeyShift(K_F7 )
global constant K_sF8 = iup_XkeyShift(K_F8 )
global constant K_sF9 = iup_XkeyShift(K_F9 )
global constant K_sF10 = iup_XkeyShift(K_F10 )
global constant K_sF11 = iup_XkeyShift(K_F11 )
global constant K_sF12 = iup_XkeyShift(K_F12 )
global constant K_sPrint = iup_XkeyShift(K_Print )
global constant K_sMenu = iup_XkeyShift(K_Menu )
global constant K_cHOME = iup_XkeyCtrl(K_HOME )
global constant K_cUP = iup_XkeyCtrl(K_UP )
global constant K_cPGUP = iup_XkeyCtrl(K_PGUP )
global constant K_cLEFT = iup_XkeyCtrl(K_LEFT )
global constant K_cMIDDLE = iup_XkeyCtrl(K_MIDDLE )
global constant K_cRIGHT = iup_XkeyCtrl(K_RIGHT )
global constant K_cEND = iup_XkeyCtrl(K_END )
global constant K_cDOWN = iup_XkeyCtrl(K_DOWN )
global constant K_cPGDN = iup_XkeyCtrl(K_PGDN )
global constant K_cINS = iup_XkeyCtrl(K_INS )
global constant K_cDEL = iup_XkeyCtrl(K_DEL )
global constant K_cSP = iup_XkeyCtrl(K_SP )
global constant K_cTAB = iup_XkeyCtrl(K_TAB )
global constant K_cCR = iup_XkeyCtrl(K_CR )
global constant K_cBS = iup_XkeyCtrl(K_BS )
global constant K_cPAUSE = iup_XkeyCtrl(K_PAUSE )
global constant K_cESC = iup_XkeyCtrl(K_ESC )
global constant K_cCcedilla = iup_XkeyCtrl(K_Ccedilla)
global constant K_cF1 = iup_XkeyCtrl(K_F1 )
global constant K_cF2 = iup_XkeyCtrl(K_F2 )
global constant K_cF3 = iup_XkeyCtrl(K_F3 )
global constant K_cF4 = iup_XkeyCtrl(K_F4 )
global constant K_cF5 = iup_XkeyCtrl(K_F5 )
global constant K_cF6 = iup_XkeyCtrl(K_F6 )
global constant K_cF7 = iup_XkeyCtrl(K_F7 )
global constant K_cF8 = iup_XkeyCtrl(K_F8 )
global constant K_cF9 = iup_XkeyCtrl(K_F9 )
global constant K_cF10 = iup_XkeyCtrl(K_F10 )
global constant K_cF11 = iup_XkeyCtrl(K_F11 )
global constant K_cF12 = iup_XkeyCtrl(K_F12 )
global constant K_cPrint = iup_XkeyCtrl(K_Print )
global constant K_cMenu = iup_XkeyCtrl(K_Menu )
global constant K_mHOME = iup_XkeyAlt(K_HOME )
global constant K_mUP = iup_XkeyAlt(K_UP )
global constant K_mPGUP = iup_XkeyAlt(K_PGUP )
global constant K_mLEFT = iup_XkeyAlt(K_LEFT )
global constant K_mMIDDLE = iup_XkeyAlt(K_MIDDLE )
global constant K_mRIGHT = iup_XkeyAlt(K_RIGHT )
global constant K_mEND = iup_XkeyAlt(K_END )
global constant K_mDOWN = iup_XkeyAlt(K_DOWN )
global constant K_mPGDN = iup_XkeyAlt(K_PGDN )
global constant K_mINS = iup_XkeyAlt(K_INS )
global constant K_mDEL = iup_XkeyAlt(K_DEL )
global constant K_mSP = iup_XkeyAlt(K_SP )
global constant K_mTAB = iup_XkeyAlt(K_TAB )
global constant K_mCR = iup_XkeyAlt(K_CR )
global constant K_mBS = iup_XkeyAlt(K_BS )
global constant K_mPAUSE = iup_XkeyAlt(K_PAUSE )
global constant K_mESC = iup_XkeyAlt(K_ESC )
global constant K_mCcedilla = iup_XkeyAlt(K_Ccedilla)
global constant K_mF1 = iup_XkeyAlt(K_F1 )
global constant K_mF2 = iup_XkeyAlt(K_F2 )
global constant K_mF3 = iup_XkeyAlt(K_F3 )
global constant K_mF4 = iup_XkeyAlt(K_F4 )
global constant K_mF5 = iup_XkeyAlt(K_F5 )
global constant K_mF6 = iup_XkeyAlt(K_F6 )
global constant K_mF7 = iup_XkeyAlt(K_F7 )
global constant K_mF8 = iup_XkeyAlt(K_F8 )
global constant K_mF9 = iup_XkeyAlt(K_F9 )
global constant K_mF10 = iup_XkeyAlt(K_F10 )
global constant K_mF11 = iup_XkeyAlt(K_F11 )
global constant K_mF12 = iup_XkeyAlt(K_F12 )
global constant K_mPrint = iup_XkeyAlt(K_Print )
global constant K_mMenu = iup_XkeyAlt(K_Menu )
global constant K_yHOME = iup_XkeySys(K_HOME )
global constant K_yUP = iup_XkeySys(K_UP )
global constant K_yPGUP = iup_XkeySys(K_PGUP )
global constant K_yLEFT = iup_XkeySys(K_LEFT )
global constant K_yMIDDLE = iup_XkeySys(K_MIDDLE )
global constant K_yRIGHT = iup_XkeySys(K_RIGHT )
global constant K_yEND = iup_XkeySys(K_END )
global constant K_yDOWN = iup_XkeySys(K_DOWN )
global constant K_yPGDN = iup_XkeySys(K_PGDN )
global constant K_yINS = iup_XkeySys(K_INS )
global constant K_yDEL = iup_XkeySys(K_DEL )
global constant K_ySP = iup_XkeySys(K_SP )
global constant K_yTAB = iup_XkeySys(K_TAB )
global constant K_yCR = iup_XkeySys(K_CR )
global constant K_yBS = iup_XkeySys(K_BS )
global constant K_yPAUSE = iup_XkeySys(K_PAUSE )
global constant K_yESC = iup_XkeySys(K_ESC )
global constant K_yCcedilla = iup_XkeySys(K_Ccedilla)
global constant K_yF1 = iup_XkeySys(K_F1 )
global constant K_yF2 = iup_XkeySys(K_F2 )
global constant K_yF3 = iup_XkeySys(K_F3 )
global constant K_yF4 = iup_XkeySys(K_F4 )
global constant K_yF5 = iup_XkeySys(K_F5 )
global constant K_yF6 = iup_XkeySys(K_F6 )
global constant K_yF7 = iup_XkeySys(K_F7 )
global constant K_yF8 = iup_XkeySys(K_F8 )
global constant K_yF9 = iup_XkeySys(K_F9 )
global constant K_yF10 = iup_XkeySys(K_F10 )
global constant K_yF11 = iup_XkeySys(K_F11 )
global constant K_yF12 = iup_XkeySys(K_F12 )
global constant K_yPrint = iup_XkeySys(K_Print )
global constant K_yMenu = iup_XkeySys(K_Menu )
global constant K_sPlus = iup_XkeyShift(K_plus )
global constant K_sComma = iup_XkeyShift(K_comma )
global constant K_sMinus = iup_XkeyShift(K_minus )
global constant K_sPeriod = iup_XkeyShift(K_period )
global constant K_sSlash = iup_XkeyShift(K_slash )
global constant K_sAsterisk = iup_XkeyShift(K_asterisk)
global constant K_cA = iup_XkeyCtrl(K_A)
global constant K_cB = iup_XkeyCtrl(K_B)
global constant K_cC = iup_XkeyCtrl(K_C)
global constant K_cD = iup_XkeyCtrl(K_D)
global constant K_cE = iup_XkeyCtrl(K_E)
global constant K_cF = iup_XkeyCtrl(K_F)
global constant K_cG = iup_XkeyCtrl(K_G)
global constant K_cH = iup_XkeyCtrl(K_H)
global constant K_cI = iup_XkeyCtrl(K_I)
global constant K_cJ = iup_XkeyCtrl(K_J)
global constant K_cK = iup_XkeyCtrl(K_K)
global constant K_cL = iup_XkeyCtrl(K_L)
global constant K_cM = iup_XkeyCtrl(K_M)
global constant K_cN = iup_XkeyCtrl(K_N)
global constant K_cO = iup_XkeyCtrl(K_O)
global constant K_cP = iup_XkeyCtrl(K_P)
global constant K_cQ = iup_XkeyCtrl(K_Q)
global constant K_cR = iup_XkeyCtrl(K_R)
global constant K_cS = iup_XkeyCtrl(K_S)
global constant K_cT = iup_XkeyCtrl(K_T)
global constant K_cU = iup_XkeyCtrl(K_U)
global constant K_cV = iup_XkeyCtrl(K_V)
global constant K_cW = iup_XkeyCtrl(K_W)
global constant K_cX = iup_XkeyCtrl(K_X)
global constant K_cY = iup_XkeyCtrl(K_Y)
global constant K_cZ = iup_XkeyCtrl(K_Z)
global constant K_c1 = iup_XkeyCtrl(K_1)
global constant K_c2 = iup_XkeyCtrl(K_2)
global constant K_c3 = iup_XkeyCtrl(K_3)
global constant K_c4 = iup_XkeyCtrl(K_4)
global constant K_c5 = iup_XkeyCtrl(K_5)
global constant K_c6 = iup_XkeyCtrl(K_6)
global constant K_c7 = iup_XkeyCtrl(K_7)
global constant K_c8 = iup_XkeyCtrl(K_8)
global constant K_c9 = iup_XkeyCtrl(K_9)
global constant K_c0 = iup_XkeyCtrl(K_0)
global constant K_cPlus = iup_XkeyCtrl(K_plus )
global constant K_cComma = iup_XkeyCtrl(K_comma )
global constant K_cMinus = iup_XkeyCtrl(K_minus )
global constant K_cPeriod = iup_XkeyCtrl(K_period )
global constant K_cSlash = iup_XkeyCtrl(K_slash )
global constant K_cSemicolon = iup_XkeyCtrl(K_semicolon )
global constant K_cEqual = iup_XkeyCtrl(K_equal )
global constant K_cBracketleft = iup_XkeyCtrl(K_bracketleft )
global constant K_cBracketright = iup_XkeyCtrl(K_bracketright)
global constant K_cBackslash = iup_XkeyCtrl(K_backslash )
global constant K_cAsterisk = iup_XkeyCtrl(K_asterisk )
global constant K_mA = iup_XkeyAlt(K_A)
global constant K_mB = iup_XkeyAlt(K_B)
global constant K_mC = iup_XkeyAlt(K_C)
global constant K_mD = iup_XkeyAlt(K_D)
global constant K_mE = iup_XkeyAlt(K_E)
global constant K_mF = iup_XkeyAlt(K_F)
global constant K_mG = iup_XkeyAlt(K_G)
global constant K_mH = iup_XkeyAlt(K_H)
global constant K_mI = iup_XkeyAlt(K_I)
global constant K_mJ = iup_XkeyAlt(K_J)
global constant K_mK = iup_XkeyAlt(K_K)
global constant K_mL = iup_XkeyAlt(K_L)
global constant K_mM = iup_XkeyAlt(K_M)
global constant K_mN = iup_XkeyAlt(K_N)
global constant K_mO = iup_XkeyAlt(K_O)
global constant K_mP = iup_XkeyAlt(K_P)
global constant K_mQ = iup_XkeyAlt(K_Q)
global constant K_mR = iup_XkeyAlt(K_R)
global constant K_mS = iup_XkeyAlt(K_S)
global constant K_mT = iup_XkeyAlt(K_T)
global constant K_mU = iup_XkeyAlt(K_U)
global constant K_mV = iup_XkeyAlt(K_V)
global constant K_mW = iup_XkeyAlt(K_W)
global constant K_mX = iup_XkeyAlt(K_X)
global constant K_mY = iup_XkeyAlt(K_Y)
global constant K_mZ = iup_XkeyAlt(K_Z)
global constant K_m1 = iup_XkeyAlt(K_1)
global constant K_m2 = iup_XkeyAlt(K_2)
global constant K_m3 = iup_XkeyAlt(K_3)
global constant K_m4 = iup_XkeyAlt(K_4)
global constant K_m5 = iup_XkeyAlt(K_5)
global constant K_m6 = iup_XkeyAlt(K_6)
global constant K_m7 = iup_XkeyAlt(K_7)
global constant K_m8 = iup_XkeyAlt(K_8)
global constant K_m9 = iup_XkeyAlt(K_9)
global constant K_m0 = iup_XkeyAlt(K_0)
global constant K_mPlus = iup_XkeyAlt(K_plus )
global constant K_mComma = iup_XkeyAlt(K_comma )
global constant K_mMinus = iup_XkeyAlt(K_minus )
global constant K_mPeriod = iup_XkeyAlt(K_period )
global constant K_mSlash = iup_XkeyAlt(K_slash )
global constant K_mSemicolon = iup_XkeyAlt(K_semicolon )
global constant K_mEqual = iup_XkeyAlt(K_equal )
global constant K_mBracketleft = iup_XkeyAlt(K_bracketleft )
global constant K_mBracketright = iup_XkeyAlt(K_bracketright)
global constant K_mBackslash = iup_XkeyAlt(K_backslash )
global constant K_mAsterisk = iup_XkeyAlt(K_asterisk )
global constant K_yA = iup_XkeySys(K_A)
global constant K_yB = iup_XkeySys(K_B)
global constant K_yC = iup_XkeySys(K_C)
global constant K_yD = iup_XkeySys(K_D)
global constant K_yE = iup_XkeySys(K_E)
global constant K_yF = iup_XkeySys(K_F)
global constant K_yG = iup_XkeySys(K_G)
global constant K_yH = iup_XkeySys(K_H)
global constant K_yI = iup_XkeySys(K_I)
global constant K_yJ = iup_XkeySys(K_J)
global constant K_yK = iup_XkeySys(K_K)
global constant K_yL = iup_XkeySys(K_L)
global constant K_yM = iup_XkeySys(K_M)
global constant K_yN = iup_XkeySys(K_N)
global constant K_yO = iup_XkeySys(K_O)
global constant K_yP = iup_XkeySys(K_P)
global constant K_yQ = iup_XkeySys(K_Q)
global constant K_yR = iup_XkeySys(K_R)
global constant K_yS = iup_XkeySys(K_S)
global constant K_yT = iup_XkeySys(K_T)
global constant K_yU = iup_XkeySys(K_U)
global constant K_yV = iup_XkeySys(K_V)
global constant K_yW = iup_XkeySys(K_W)
global constant K_yX = iup_XkeySys(K_X)
global constant K_yY = iup_XkeySys(K_Y)
global constant K_yZ = iup_XkeySys(K_Z)
global constant K_y1 = iup_XkeySys(K_1)
global constant K_y2 = iup_XkeySys(K_2)
global constant K_y3 = iup_XkeySys(K_3)
global constant K_y4 = iup_XkeySys(K_4)
global constant K_y5 = iup_XkeySys(K_5)
global constant K_y6 = iup_XkeySys(K_6)
global constant K_y7 = iup_XkeySys(K_7)
global constant K_y8 = iup_XkeySys(K_8)
global constant K_y9 = iup_XkeySys(K_9)
global constant K_y0 = iup_XkeySys(K_0)
global constant K_yPlus = iup_XkeySys(K_plus )
global constant K_yComma = iup_XkeySys(K_comma )
global constant K_yMinus = iup_XkeySys(K_minus )
global constant K_yPeriod = iup_XkeySys(K_period )
global constant K_ySlash = iup_XkeySys(K_slash )
global constant K_ySemicolon = iup_XkeySys(K_semicolon )
global constant K_yEqual = iup_XkeySys(K_equal )
global constant K_yBracketleft = iup_XkeySys(K_bracketleft )
global constant K_yBracketright = iup_XkeySys(K_bracketright)
global constant K_yBackslash = iup_XkeySys(K_backslash )
global constant K_yAsterisk = iup_XkeySys(K_asterisk )
--*/

--include defs.e


--SUG:
--integer libidx = -1
----global 
--function iup_open_dll(sequence libs)
--atom res
--  if libidx=-1 then
--      for i=1 to length(libs) do
--          res = open_dll(libs[i])
--          if res!=0 then
--              libidx = i
--              exit
--          end if
--      end for
--  else
--      res = open_dll(libs[libidx])
--  end if
---- erm: this is probably better off being in iup_open()...
----    (that way you can call has_iup() and use something else?)
--  if res=0 then
--      puts(1,"error opening ")
--      if libidx then
--          puts(1,libs[libidx])
--      else
--          ?libs
--      end if
--      {} = wait_key()
--      ?9/0
--  end if
--  return res
--end function
--

--include common.e
--DEV** compiler error when this was "version", probably because there are/were several global version() knocking about...
--global constant common_version = "3.3.0p1"

--global 
--atom
--      --**
--      -- iup.dll handle
--      iup,
--
--      --**
--      -- Pointer to the current language (if changed)
--      pLanguage=0
--
--include core.e
--include wrap.e
--global 
--constant
--  UC = C_UCHAR,
--  P  = C_POINTER, 
--  F  = C_FLOAT, 
--  D  = C_DOUBLE, 
--  I  = C_INT,
--  L  = C_LONG,
--  UL = C_ULONG
--$

--/*
--global 
function get_string_array(integer hFunc, integer max_n, object classname = {})
sequence name_list
sequence plist          -- list of pointers to above
atom array_pointer      -- pointer to above array
integer name_count      -- the number of names loaded to the table
--integer inc
--atom temp

    name_list = {}
    array_pointer = 0

    -- create an area of memory for the pointer to the array to be reurned to
    array_pointer = allocate(4*max_n)

    if atom(classname) then
        name_count = c_func(hFunc, {classname, array_pointer, max_n})
--  elsif sequence(classname) and length(classname) then
    elsif length(classname) then
        atom hClassname = allocate_string(classname)
        name_count = c_func(hFunc, {hClassname, array_pointer, max_n})
        free(hClassname)
    else
        name_count = c_func(hFunc, {array_pointer, max_n})
    end if

    -- get the list of pointers for the strings
    plist = {}
    plist = peek4u({array_pointer, name_count})

    -- get the list of strings
    name_list = repeat({}, name_count)
    for i=1 to name_count do
        name_list[i] = peek_string(plist[i])
    end for

    free(array_pointer)

    return name_list
end function
--*/
--include common.e

--constant iupdlls = {
--                  "iup.dll",
--                  "libiup.so",
--                  "libiup.dylib"
--                 }
--iup = iup_open_dll(iupdlls)

--****
-- === Routines
--

--constant
--  xIupOpen = define_c_func(iup, "IupOpen", {I,P},I),
--  xIupClose = define_c_proc(iup, "IupClose", {}),
--  xIupVersion = define_c_func(iup, "IupVersion", {},P),
--  xIupLoad = define_c_func(iup, "IupLoad", {P},P),
--  xIupLoadBuffer = define_c_func(iup, "IupLoadBuffer", {P},P),
--  xIupSetLanguage = define_c_proc(iup, "IupSetLanguage", {P}),
--  xIupGetLanguage = define_c_func(iup, "IupGetLanguage", {},P)
--
--**
-- Does this system have access to the Iup DLL files?
--
-- Returns:
--   TRUE or FALSE
--

global function has_iup()
    return iup!=0
end function

----include attributes.e
--constant
--  xIupStoreAttribute = define_c_proc(iup, "IupStoreAttribute", {P,P,P}),
--  xIupSetAttribute = define_c_proc(iup, "IupSetAttribute", {P,P,P}),
--  xIupSetAttributes = define_c_proc(iup, "IupSetAttributes", {P,P}),
----    xIupSetInt = define_c_proc(iup, "IupSetInt", {P,P,I}),
--  xIupResetAttribute = define_c_proc(iup, "IupResetAttribute", {P,P}),
--  xIupGetAttribute = define_c_func(iup, "IupGetAttribute", {P,P},P),
--  xIupGetAllAttributes = define_c_func(iup, "IupGetAllAttributes", {P,P,I},I),
--  xIupSetAttributeHandle = define_c_proc(iup, "IupSetAttributeHandle", {P,P,P}),
--  xIupGetAttributeHandle = define_c_func(iup, "IupGetAttributeHandle", {P,P},P),
--  xIupGetAttributes = define_c_func(iup, "IupGetAttributes", {P},P),
--  xIupGetFloat = define_c_func(iup, "IupGetFloat", {P,P},F),
--  xIupGetInt = define_c_func(iup, "IupGetInt", {P,P},I),
--  xIupGetInt2 = define_c_func(iup, "IupGetInt", {P,P},I),
--  xIupStoreGlobal = define_c_proc(iup, "IupStoreGlobal", {P,P}),
--  xIupSetGlobal = define_c_proc(iup, "IupSetGlobal", {P,P}),
--  xIupGetGlobal = define_c_func(iup, "IupGetGlobal", {P},P)
--
--global procedure IupStoreAttribute(Ihandle ih, sequence name, object value, sequence data = {})
--atom pName = allocate_string(name), pValue = 0
--
--  if length(data) then
--      value = sprintf(value, data)
--  end if
--
--  if sequence(value) then
--      pValue = allocate_string(value)
--  else
--      pValue = value
--  end if
--
--  c_proc(xIupStoreAttribute, {ih, pName, pValue})
--
--  free(pName)
--
--  if sequence(value) then
--      free(pValue)
--  end if
--end procedure
--

 -- pIUP\iupX.e (done to here)
------------------------------

--include events.e

--include layout.e

--****
-- === Class Information
--

--****
-- === Layout/Composition
--

constant
    xIupFill        = define_c_func(iup, "IupFill", {}, P),
    xIupHboxv       = define_c_func(iup, "IupHboxv", {P}, P),
    xIupVboxv       = define_c_func(iup, "IupVboxv", {P}, P),
    xIupZboxv       = define_c_func(iup, "IupZboxv", {P}, P),
    xIupRadio       = define_c_func(iup, "IupRadio", {P}, P),
    xIupNormalizerv = define_c_func(iup, "IupNormalizerv", {P}, P),
    xIupCboxv       = define_c_func(iup, "IupCboxv", {P}, P),
    xIupSbox        = define_c_func(iup, "IupSbox", {P}, P),
    xIupSplit       = define_c_func(iup, "IupSplit", {P,P}, P),
    xIupGridBoxv    = define_c_func(iup, "IupGridBoxv", {P}, P),
    $

global function IupFill()
    Ihandle ih = c_func(xIupFill, {})
    return ih
end function

global function IupHbox(sequence children, string attributes = "", sequence data = {})
atom pChildren = iup_ptr_array(children)
    Ihandle ih = c_func(xIupHboxv, {pChildren})
    free(pChildren)
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupVbox(sequence children, string attributes = "", sequence data = {})
atom pChildren = iup_ptr_array(children)
    Ihandle ih = c_func(xIupVboxv, {pChildren})
    free(pChildren)
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupZbox(sequence children, string attributes = "", sequence data = {})
atom pChildren = iup_ptr_array(children)
    Ihandle ih = c_func(xIupZboxv, {pChildren})
    free(pChildren)
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupRadio(Ihandle pChild)
    Ihandle ih = c_func(xIupRadio, {pChild})
    return ih
end function

global function IupNormalizer(sequence ih_list)
    atom p_ih_list = iup_ptr_array(ih_list)
    Ihandle ih = c_func(xIupNormalizerv, {p_ih_list})
    free(p_ih_list)
    return ih
end function

global function IupCbox(sequence children, string attributes = "", sequence data = {})
atom pChildren = iup_ptr_array(children)
    Ihandle ih = c_func(xIupCboxv, {pChildren})
    free(pChildren)
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupSbox(Ihandle child)
    Ihandle ih = c_func(xIupSbox, {child})
    return ih
end function

global function IupSplit(Ihandle child1, Ihandle child2)
    Ihandle ih = c_func(xIupSplit, {child1,child2})
    return ih
end function

global function IupGridBox(sequence children={})
atom pChildren = iup_ptr_array(children)
    Ihandle ih = c_func(xIupGridBoxv, {pChildren})
    free(pChildren)
    return ih
end function


--****
-- === Layout/Hierarchy
--

constant
    xIupAppend          = define_c_func(iup, "IupAppend", {P,P}, P),
    xIupDetach          = define_c_proc(iup, "IupDetach", {P}),
    xIupInsert          = define_c_func(iup, "IupInsert", {P,P,P}, P),
    xIupReparent        = define_c_func(iup, "IupReparent", {P,P,P}, P),
    xIupGetParent       = define_c_func(iup, "IupGetParent", {P}, P),
    xIupGetChild        = define_c_func(iup, "IupGetChild", {P,I}, P),
    xIupGetChildPos     = define_c_func(iup, "IupGetChildPos", {P,P}, I),
    xIupGetChildCount   = define_c_func(iup, "IupGetChildCount", {P}, I),
    xIupGetNextChild    = define_c_func(iup, "IupGetNextChild", {P,P}, P),
    xIupGetBrother      = define_c_func(iup, "IupGetBrother", {P},P),
    xIupGetDialog       = define_c_func(iup, "IupGetDialog", {P},P),
    xIupGetDialogChild  = define_c_func(iup, "IupGetDialogChild", {P,P},P)

global procedure IupAppend(Ihandle ih, Ihandle child)
    Ihandle parent = c_func(xIupAppend, {ih,child}) -- (error if NULL/fail)
end procedure

global procedure IupAppends(Ihandle ih, sequence children)
    for i=1 to length(children) do
        IupAppend(ih, children[i])
    end for
end procedure

global procedure IupDetach(Ihandle ih)
    c_proc(xIupDetach, {ih})
end procedure

global procedure IupInsert(Ihandle ih, Ihandln ref_child, Ihandle new_child)
    Ihandle parent = c_func(xIupInsert, {ih,ref_child,new_child})   -- (error if NULL/fail)
end procedure

global procedure IupReparent(Ihandle child, Ihandle new_parent, Ihandle ref_child)
    if c_func(xIupReparent, {child, new_parent, ref_child})!=IUP_NOERROR then ?9/0 end if
end procedure

global function IupGetParent(Ihandle ih)
    Ihandln parent = c_func(xIupGetParent, {ih})
    return parent
end function

global function IupGetChild(Ihandle ih, integer pos)
    Ihandln child = c_func(xIupGetChild, {ih, pos})
    return child
end function

global function IupGetChildPos(Ihandle ih, Ihandle child)
    integer pos = c_func(xIupGetChildPos, {ih, child})
    return pos
end function

global function IupGetChildCount(Ihandle ih)
    integer n = c_func(xIupGetChildCount, {ih})
    return n
end function

global function IupGetNextChild(Ihandle ih, Ihandle child)
    Ihandln nextchild = c_func(xIupGetNextChild, {ih,child})
    return nextchild
end function

global function IupGetBrother(Ihandle ih)
    Ihandln brother = c_func(xIupGetBrother, {ih})
    return brother
end function

global function IupGetDialog(Ihandle ih)
    Ihandln dlg = c_func(xIupGetDialog, {ih})
    return dlg
end function

global function IupGetDialogChild(Ihandle ih, string name)
    Ihandln child = c_func(xIupGetDialogChild, {ih, name})
    return child
end function


--****
-- === Layout Utilities
--

constant
    xIupRefresh         = define_c_proc(iup, "IupRefresh", {P}),
    xIupRefreshChildren = define_c_proc(iup, "IupRefreshChildren", {P}),
    xIupUpdate          = define_c_proc(iup, "IupUpdate", {P}),
    xIupUpdateChildren  = define_c_proc(iup, "IupUpdate", {P}),
    xIupRedraw          = define_c_proc(iup, "IupRedraw", {P,I}),
    xIupConvertXYToPos  = define_c_func(iup, "IupConvertXYToPos", {P,I,I}, I)

global procedure IupRefresh(Ihandle ih)
    c_proc(xIupRefresh, {ih})
end procedure

global procedure IupRefreshChildren(Ihandle ih)
    c_proc(xIupRefreshChildren, {ih})
end procedure

global procedure IupUpdate(Ihandle ih)
    c_proc(xIupUpdate, {ih})
end procedure

global procedure IupUpdateChildren(Ihandle ih)
    c_proc(xIupUpdateChildren, {ih})
end procedure

global procedure IupRedraw(Ihandle ih, bool children)
    c_proc(xIupRedraw, {ih,children})
end procedure

global function IupConvertXYToPos(Ihandle ih, integer x, integer y)
    integer pos = c_func(xIupConvertXYToPos, {ih,x,y})
    return pos
end function

--include dialog.e
constant
    xIupDialog = define_c_func(iup, "IupDialog", {P},P),
    xIupPopup = define_c_func(iup, "IupPopup", {P,I,I},I),
    xIupShow = define_c_func(iup, "IupShow", {P},I),
    xIupShowXY = define_c_func(iup, "IupShowXY", {P,I,I},I),
    xIupHide = define_c_proc(iup, "IupHide", {P})

global function IupDialog(Ihandln child = NULL, string attributes = "", sequence data = {})
    Ihandle dlg = c_func(xIupDialog, {child})
    if length(attributes) then
        IupSetAttributes(dlg, attributes, data)
    end if
    return dlg
end function

global procedure IupPopup(Ihandle ih, integer x, integer y)
    if c_func(xIupPopup, {ih,x,y})!=IUP_NOERROR then ?9/0 end if
end procedure

global procedure IupShow(Ihandle ih)
    if c_func(xIupShow, {ih})!=IUP_NOERROR then ?9/0 end if
end procedure

global procedure IupShowXY(Ihandle ih, integer x, integer y)
    if c_func(xIupShowXY, {ih, x, y})!=IUP_NOERROR then ?9/0 end if
end procedure

global procedure IupHide(Ihandle ih)
    c_proc(xIupHide, {ih})
end procedure

--****
-- === Predefined
--

constant
    xIupAlarm           = define_c_func(iup, "IupAlarm", {P,P,P,P,P}, I),
    xIupMessage         = define_c_proc(iup, "IupMessage", {P,P}),
    xIupMessageDlg      = define_c_func(iup, "IupMessageDlg", {}, P),
    xIupColorDlg        = define_c_func(iup, "IupColorDlg", {}, P),
    xIupFileDlg         = define_c_func(iup, "IupFileDlg", {}, P),
    xIupFontDlg         = define_c_func(iup, "IupFontDlg", {}, P),
    xIupGetColor        = define_c_func(iup, "IupGetColor", {I,I,P,P,P}, I),
    xIupGetFile         = define_c_func(iup, "IupGetFile", {P}, I),
    xIupGetParamv       = define_c_func(iup, "IupGetParamv", {P,P,P,P,I,I,P}, I),
    xIupGetText         = define_c_func(iup, "IupGetText", {P,P}, I),
    xIupListDialog      = define_c_func(iup, "IupListDialog", {I,P,I,P,I,I,I,P}, I),
    xIupLayoutDialog    = define_c_func(iup, "IupLayoutDialog", {P}, P),
    xIupProgressDlg     = define_c_func(iup, "+IupProgressDlg", {}, P)


global function IupAlarm(string title, string msg, string b1, nullable_string b2 = NULL, nullable_string b3 = NULL)
    return c_func(xIupAlarm, {title,msg,b1,b2,b3})
end function

global procedure IupMessage(nullable_string title = NULL, nullable_string msg = NULL)
    c_proc(xIupMessage, {title,msg})
end procedure

global function IupMessageDlg()
    Ihandle ih = c_func(xIupMessageDlg, {})
    return ih
end function

global function IupColorDlg()
    Ihandle ih = c_func(xIupColorDlg, {})
    return ih
end function

global function IupFileDlg()
    Ihandle ih = c_func(xIupFileDlg, {})
    return ih
end function

global function IupFontDlg()
    Ihandle ih = c_func(xIupFontDlg, {})
    return ih
end function

global function IupGetColor(integer x=IUP_CENTERPARENT, integer y=IUP_CENTERPARENT,integer r=255, integer g=255, integer b=255)
atom pRGB=allocate(3,1), pR=pRGB, pG=pRGB+1, pB=pRGB+2
    poke(pR, {r,g,b})
    bool result = c_func(xIupGetColor, {x, y, pR, pG, pB})
    return result&peek({pRGB,3})
end function

global function IupGetFile(string filefilter)
-- filefilter is eg "../docs/*.txt".
    atom pFilename = allocate(4096,1)   -- (NB: xIupGetFile param is InOut)
    poke(pFilename, filefilter & 0)
    if c_func(xIupGetFile, {pFilename})=-1 then
        return NULL
    end if
    string filename = peek_string(pFilename)
    return filename
end function

global function IupGetParam(string title, cbfunc action, atom user_data, string fmt, sequence param_data = {})
integer fskip = 0
integer param_count = 0
integer param_extra = 0
string fmts = ""
    controls_open()
    for i=1 to length(fmt) do
        if fskip then
            fskip -= 1
        elsif fmt[i]='%' then
            integer fi = fmt[i+1]
            if fi='%' then
                fskip = 1
            elsif find(fi,"uth") then   -- (dunno what to do with a handle...)
                param_extra += 1
            else
                fmts = append(fmts,fi)
                param_count += 1
            end if
        end if
    end for

    if length(fmts)!=param_count then ?9/0 end if
    if length(param_data)!=param_count then ?9/0 end if

    sequence pN = repeat(0,param_count)

    for i=param_count to 1 by -1 do
        object param = param_data[i]
        integer fi = fmts[i]
        atom p
        if find(fi,"bilo") then                     -- bool/int/list/radio
            if not integer(param) then ?9/0 end if
            p = allocate(4)
            poke4(p, param)
        elsif find(fi,"ra") then                    -- real/angle (as 32-bit float)
            if not atom(param) then ?9/0 end if
            p = allocate(4)
            poke(p,atom_to_float32(param))
        elsif find(fi,"RA") then                    -- "" but double
            if not atom(param) then ?9/0 end if
            p = allocate(8)
            poke(p,atom_to_float64(param))
        elsif find(fi,"smfcn") then                 -- string/multiline/file/color/font
            if not string(param) then ?9/0 end if
            -- feel free to increase these if needed (and update the docs)
            integer size = iff(fi='m'?10240:    -- multiline
                           iff(fi='f'?4096:     -- filenames
                                      512))     -- other strings
            p = allocate(size)
            mem_set(p, 0, size)
            poke(p, param)
        else
            -- unknown parameter type
            ?9/0
        end if
        pN[i] = p
    end for

    atom data = iup_ptr_array(pN)

    integer result = c_func(xIupGetParamv, {title, action, user_data, fmt, param_count, param_extra, data})

    free(data)

    sequence vals = repeat(0,param_count)&result

    for i=param_count to 1 by -1 do
        object param
        integer fi = fmts[i]
        atom p = pN[i]
        if find(fi,"bilo") then
            param = peek4s(p)
        elsif find(fi,"ra") then
            param = float32_to_atom(peek({p,4}))
        elsif find(fi,"RA") then
            param = float64_to_atom(peek({p,8}))
        elsif find(fi,"smfcn") then
            param = peek_string(p)
        else
            -- unknown parameter type
            ?9/0
        end if
        vals[i] = param
        free(p)
    end for
    return vals
end function

global function IupGetText(string title, string text, integer max_size = 10240)
atom pText = allocate(max_size,1)
    mem_set(pText, 0, max_size)
    poke(pText, text)
    if c_func(xIupGetText, {title, pText})!=0 then
        text = peek_string(pText)
    end if
    return text
end function

global function IupListDialog(integer seltype, string title, sequence options, integer isel, integer maxCols, integer maxLines)
integer size = length(options), result
atom pOptions = allocate(machine_word()*size),
     pMark = NULL
sequence pOptAry = {}, marked, selected = {}

    for i=1 to size do
        pOptAry &= allocate_string(options[i],1)
    end for
    poke4(pOptions, pOptAry)
    if seltype=2 then
        -- multiple selection
        pMark = allocate(4*size)
        mem_set(pMark,0,4*size)
    end if
    result = c_func(xIupListDialog, {seltype, title, size, pOptions, isel, maxCols, maxLines, pMark})
    free(pOptions)
    if seltype=1 then
        -- single selection
        return result+1 -- (1-based result)
    end if
    marked = peek4u({pMark,size})
    for i=1 to size do
        if marked[i] then
            selected &= i
        end if
    end for
    free(pMark)
    return selected
end function

global function IupLayoutDialog(Ihandle dialog)
    Ihandle ih = c_func(xIupLayoutDialog, {dialog})
    return ih
end function

--include controls.e
constant
    xIupButton = define_c_func(iup, "IupButton", {P,P}, P),
    xIupCanvas = define_c_func(iup, "IupCanvas", {P}, P),
    xIupFrame = define_c_func(iup, "IupFrame", {P}, P),
    xIupLabel = define_c_func(iup, "IupLabel", {P}, P),
    xIupList = define_c_func(iup, "IupList", {P}, P),
    xIupProgressBar = define_c_func(iup, "IupProgressBar", {}, P),
    xIupSpin = define_c_func(iup, "IupSpin", {}, P),
    xIupSpinbox = define_c_func(iup, "IupSpinbox", {P}, P),
    xIupTabsv = define_c_func(iup, "IupTabsv", {P}, P),
    xIupText = define_c_func(iup, "IupText", {P}, P),
    xIupMultiLine = define_c_func(iup, "IupMultiLine", {P},P),
    xIupTextConvertLinColToPos = define_c_proc(iup, "IupTextConvertLinColToPos", {P,I,I,P}),
    xIupTextConvertPosToLinCol = define_c_proc(iup, "IupTextConvertPosToLinCol", {P,I,P,P}),
    xIupToggle = define_c_func(iup, "IupToggle", {P,P}, P),
    xIupTree = define_c_func(iup, "IupTree", {}, P),
    xIupVal = define_c_func(iup, "IupVal", {P}, P),
    $
--/*
function paranormalise(object action = NULL, object func = NULL, sequence attributes = "", sequence data = {})
--
--  This routine implements a little trickery to make certain routines easier to use, for example the documentation contains
--
--      IupButton(string title, [[nullable_string action = NULL,] atom func = NULL,] string attributes = "", sequence data = {})
--
--  with the idea being that [action, func] are doubly-optional. In short you can assume
--
--      IupButton(title) and
--      IupButton(title,action,func) [NB func is /not/ optional in this case] and
--      IupButton(title,func) and
--      IupButton(title,attributes[,data]) and
--      IupButton(title,func,attributes[,data]) and
--      IupButton(title,action,func,attributes[,data])
--
--  are all valid. 
--
--  The action and func parameters are actually declared as an object, and attributes as a sequence.
--  There is an underlying assumption that if func is NULL then action is meaningless; what that specifically
--  means is IupButton(title,action,func) is ok but IupButton(title,action) is not supported/would be misread.
--
--  The comments below assume the calling routine has another/hidden first parameter - not the case for IupText,
--  but true for IupButton, Iup[Menu]Item, )
--
    if not nullable_string(action) then
        -- assume action omitted (and at least one other parameter passed)
        if atom(action) then
            -- assume p2 is func
            if func!=NULL then
                
            end if
        end if
    elsif func=NULL then
        if length(attributes) then
            -- assume 4 or 5 parameters were passed
        elsif length(data)!=0 then
            ?9/0    -- something odd passed (attributes="", data!={})
        elsif action!=NULL then
            -- assume IupButton(title,attributes)
            attributes = action -- p4:=p2
            action = NULL
        end if
    elsif sequence(func) then
        -- assume IupButton(title,attributes,data), ie func (p3) is actually data
        -- [it would be odd for data to be {} here in a static call, but things might be table-driven]
        if length(action)=0 and length(func)!=0 then ?9/0 end if -- (odd, as above)
        attributes = action     -- p4:=p2
        data = func             -- p5:=p3
        action = NULL
        func = NULL
    else
        -- assume 4 or 5 parameters were passed
    end if
    return {action,func,attributes,data}
end function
--*/

--DEV can we cope with say IupButton("clickme","SIZE=80x20")? Do we want to say action is always NULL?
--      (ie if func=NULL or sequence(func) then action is meaningless/must be attributes, and non-NULL func must be data)
-- document as:  IupButton(string title, [[nullable_string action = NULL,] atom func = NULL,] string attributes = "", sequence data = {})
--  This routine uses a little trickery to make it easier to use, such that [action, func] are doubly-optional. In short you can assume
--      IupButton(title) and
--      IupButton(title,action,func) [NB func is /not/ optional in this case] and
--      IupButton(title,func) and
--      IupButton(title,attributes[,data]) and
--      IupButton(title,func,attributes[,data]) and
--      IupButton(title,action,func,attributes[,data])
--  are all valid. Note however that the following (action=NULL, func!=NULL cases) cannot be shortened any further:
--      IupButton(title,NULL,Icallback("btn_cb"))
--      IupButton(title,NULL,Icallback("btn_cb"),"SIZE=80x20")
--  The func parameter is actually declared as an object. See pGUI.e/paranormalise() for the precise details of this handling.
--? This is a <a href=??">paranormalisable</a> function.
--? See the Technicalia dropdown [on IupButton] for more details and some pseudo code.
--  The func parameter is actually declared as an object. There is an underlying assumption that if func is NULL then action is meaningless. 
--  What that specifically means is IupButton(title,action,func) is ok but IupButton(title,action) is not supported/would be misread.
--  The following pseudo code ("paranormalise" in pGUI.e) shows how it decides that action/func (p2/p3) are actually p4/p5:
--      if func=NULL then
--          if length(attributes) then
--              -- assume 4 or 5 parameters were passed
--          elsif length(data)!=0 then
--              ?9/0    -- something odd passed (attributes="", data!={})
--          elsif action!=NULL then
--              -- assume IupButton(title,attributes)
--              attributes = action -- p4:=p2
--              action = NULL
--          end if
--      elsif sequence(func) then
--          -- assume IupButton(title,attributes,data), ie func (p3) is actually data
--          -- [it would be odd for data to be {} here in a static call, but things might be table-driven]
--          if length(action)=0 and length(func)!=0 then ?9/0 end if -- (odd, as above)
--          attributes = action     -- p4:=p2
--          data = func             -- p5:=p3
--          action = NULL
--          func = NULL
--      else
--          -- assume 4 or 5 parameters were passed
--      end if
global function IupButton(nullable_string title=NULL, nullable_string action=NULL, atom func=NULL, string attributes="", sequence data={})
--  {action,func,attributes,data} = paranormalise(action,func,attributes,data)
    Ihandle ih = c_func(xIupButton, {title, action})
    if func!=NULL then
        IupSetCallback(ih, ACTION, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupCanvas(nullable_string action = NULL)
    Ihandle ih = c_func(xIupCanvas, {action})
    return ih
end function

global function IupFrame(Ihandle child, string attributes = "", sequence data = {})
    Ihandle ih = c_func(xIupFrame, {child})
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupLabel(nullable_string title=NULL, string attributes="", sequence data={})
    Ihandle ih = c_func(xIupLabel, {title})
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupList(nullable_string action = NULL)
    Ihandle ih = c_func(xIupList, {action})
    return ih
end function

global function IupProgressBar()
    Ihandle ih = c_func(xIupProgressBar, {})
    return ih
end function

/* Old controls, use SPIN attribute of IupText */ -- erm??
global function IupSpin()
    Ihandle ih = c_func(xIupSpin, {})
    return ih
end function

global function IupSpinbox(Ihandle child)
    Ihandle ih = c_func(xIupSpinbox, {child})
    return ih
end function

global function IupTabs(sequence children={})
atom pChildren = iup_ptr_array(children)
    Ihandle ih = c_func(xIupTabsv, {pChildren})
    free(pChildren)
    return ih
end function

global function IupText(nullable_string action = NULL, string attributes = "", sequence data = {})
    Ihandle ih = c_func(xIupText, {action})
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupMultiLine(nullable_string action = NULL, string attributes = "", sequence data = {})
    Ihandle ih = c_func(xIupMultiLine, {action})
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupTextConvertLinColToPos(Ihandle ih, integer lin, integer col)
atom pPos = allocate(4,1)
    c_proc(xIupTextConvertLinColToPos, {ih,lin,col,pPos})
    integer pos = peek4s(pPos)
    return pos
end function

global function IupTextConvertPosToLinCol(atom ih, atom pos)
atom pLineCol = allocate(16,1)
    c_proc(xIupTextConvertPosToLinCol, {ih,pos,pLineCol,pLineCol+4})
    return peek4s({pLineCol,2}) -- integer {lin, col}
end function

global function IupToggle(string title, nullable_string action = NULL)
    Ihandle ih = c_func(xIupToggle, {title, action})
    return ih
end function

global function IupTree()
    Ihandle ih = c_func(xIupTree, {})
    return ih
end function

global function IupVal(nullable_string orientation=NULL)
    Ihandle ih = c_func(xIupVal, {orientation})
    return ih
end function

global function IupValuator(nullable_string orientation=NULL)
    Ihandle ih = c_func(xIupVal, {orientation})
    return ih
end function

global constant -- function delcarations
    xIupConfig                        = define_c_func(iup, "IupConfig", {}, P),
    xIupConfigLoad                    = define_c_func(iup, "IupConfigLoad", {P}, I),
    xIupConfigSave                    = define_c_func(iup, "IupConfigSave", {P}, I),
    xIupConfigSetVariableInt          = define_c_proc(iup, "IupConfigSetVariableInt", {P,P,P,I}),
    xIupConfigSetVariableIntId        = define_c_proc(iup, "IupConfigSetVariableIntId", {P,P,P,I,I}),
    xIupConfigSetVariableDouble       = define_c_proc(iup, "IupConfigSetVariableDouble", {P,P,P,D}),
    xIupConfigSetVariableDoubleId     = define_c_proc(iup, "IupConfigSetVariableDoubleId", {P,P,P,I,D}),
    xIupConfigSetVariableStr          = define_c_proc(iup, "IupConfigSetVariableStr", {P,P,P,P}),
    xIupConfigSetVariableStrId        = define_c_proc(iup, "IupConfigSetVariableStrId", {P,P,P,I,P}),
    xIupConfigGetVariableInt          = define_c_func(iup, "IupConfigGetVariableInt", {P,P,P}, I),
    xIupConfigGetVariableIntDef       = define_c_func(iup, "IupConfigGetVariableIntDef", {P,P,P,I}, I),
    xIupConfigGetVariableIntId        = define_c_func(iup, "IupConfigGetVariableIntId", {P,P,P,I}, I),
    xIupConfigGetVariableIntIdDef     = define_c_func(iup, "IupConfigGetVariableIntIdDef", {P,P,P,I,I}, I),
    xIupConfigGetVariableDouble       = define_c_func(iup, "IupConfigGetVariableDouble", {P,P,P}, D),
    xIupConfigGetVariableDoubleDef    = define_c_func(iup, "IupConfigGetVariableDoubleDef", {P,P,P,D}, D),
    xIupConfigGetVariableDoubleId     = define_c_func(iup, "IupConfigGetVariableDoubleId", {P,P,P,I}, D),
    xIupConfigGetVariableDoubleIdDef  = define_c_func(iup, "IupConfigGetVariableDoubleIdDef", {P,P,P,I,D}, D),
    xIupConfigGetVariableStr          = define_c_func(iup, "IupConfigGetVariableStr", {P,P,P}, P),
    xIupConfigGetVariableStrDef       = define_c_func(iup, "IupConfigGetVariableStrDef", {P,P,P,P}, P),
    xIupConfigGetVariableStrId        = define_c_func(iup, "IupConfigGetVariableStrId", {P,P,P,I}, P),
    xIupConfigGetVariableStrIdDef     = define_c_func(iup, "IupConfigGetVariableStrIdDef", {P,P,P,I,P}, P),
    xIupConfigRecentInit              = define_c_proc(iup, "IupConfigRecentInit", {P,P,P,I}),
    xIupConfigRecentUpdate            = define_c_proc(iup, "IupConfigRecentUpdate", {P,P}),
    xIupConfigDialogShow              = define_c_proc(iup, "IupConfigDialogShow", {P,P,P}),
    xIupConfigDialogClosed            = define_c_proc(iup, "IupConfigDialogClosed", {P,P,P}),
    $

global function IupConfig()
    Ihandle config = c_func(xIupConfig, {})
    return config
end function

global function IupConfigLoad(Ihandle ih)
    integer errcode = c_func(xIupConfigLoad, {ih})
    return errcode  -- 0=no error; -1=error opening the file; -2=error accessing the file; -3=error during filename construction
end function

global function IupConfigSave(Ihandle ih)
    integer errcode = c_func(xIupConfigSave, {ih})
    return errcode  -- 0=no error; -1=error opening the file; -2=error accessing the file; -3=error during filename construction
end function

global procedure IupConfigSetVariableInt(Ihandle ih, string group, string key, integer v)
    c_proc(xIupConfigSetVariableInt, {ih,group,key,v})
end procedure

global procedure IupConfigSetVariableIntId(Ihandle ih, string group, string key, integer id, integer v)
    c_proc(xIupConfigSetVariableIntId, {ih,group,key,id,v})
end procedure

global procedure IupConfigSetVariableDouble(Ihandle ih, string group, string key, atom v)
    c_proc(xIupConfigSetVariableDouble, {ih,group,key,v})
end procedure

global procedure IupConfigSetVariableDoubleId(Ihandle ih, string group, string key, integer id, atom v)
    c_proc(xIupConfigSetVariableDoubleId, {ih,group,key,id,v})
end procedure

global procedure IupConfigSetVariableStr(Ihandle ih, string group, string key, string v)
    c_proc(xIupConfigSetVariableStr, {ih,group,key,v})
end procedure

global procedure IupConfigSetVariableStrId(Ihandle ih, string group, string key, integer id, string v)
    c_proc(xIupConfigSetVariableStrId, {ih,group,key,id,v})
end procedure

global function IupConfigGetVariableInt(Ihandle ih, string group, string key)
    integer res = c_func(xIupConfigGetVariableInt, {ih,group,key})
    return res
end function

global function IupConfigGetVariableIntDef(Ihandle ih, string group, string key, integer def)
    integer res = c_func(xIupConfigGetVariableIntDef, {ih,group,key,def})
    return res
end function

global function IupConfigGetVariableIntId(Ihandle ih, string group, string key, integer id)
    integer res = c_func(xIupConfigGetVariableIntId, {ih,group,key,id})
    return res
end function

global function IupConfigGetVariableIntIdDef(Ihandle ih, string group, string key, integer id, integer def)
    integer res = c_func(xIupConfigGetVariableIntIdDef, {ih,group,key,id,def})
    return res
end function

global function IupConfigGetVariableDouble(Ihandle ih, string group, string key)
    atom res = c_func(xIupConfigGetVariableDouble, {ih,group,key})
    return res
end function

global function IupConfigGetVariableDoubleDef(Ihandle ih, string group, string key, atom def)
    atom res = c_func(xIupConfigGetVariableDoubleDef, {ih,group,key,def})
    return res
end function

global function IupConfigGetVariableDoubleId(Ihandle ih, string group, string key, integer id)
    atom res = c_func(xIupConfigGetVariableDoubleId, {ih,group,key,id})
    return res
end function

global function IupConfigGetVariableDoubleIdDef(Ihandle ih, string group, string key, integer id, atom def)
    atom res = c_func(xIupConfigGetVariableDoubleIdDef, {ih,group,key,id,def})
    return res
end function

global function IupConfigGetVariableStr(Ihandle ih, string group, string key)
    atom pString = c_func(xIupConfigGetVariableStr, {ih,group,key})
    string res = iff(pString=NULL?"":peek_string(pString))
    return res
end function

global function IupConfigGetVariableStrDef(Ihandle ih, string group, string key, string def)
    atom pString = c_func(xIupConfigGetVariableStrDef, {ih,group,key,def})
    string res = iff(pString=NULL?"":peek_string(pString))
    return res
end function

global function IupConfigGetVariableStrId(Ihandle ih, string group, string key, integer id)
    atom pString = c_func(xIupConfigGetVariableStrId, {ih,group,key,id})
    string res = iff(pString=NULL?"":peek_string(pString))
    return res
end function

global function IupConfigGetVariableStrIdDef(Ihandle ih, string group, string key, integer id, string def)
    atom pString = c_func(xIupConfigGetVariableStrIdDef, {ih,group,key,id,def})
    string res = iff(pString=NULL?"":peek_string(pString))
    return res
end function

global procedure IupConfigRecentInit(Ihandle ih, Ihandle menu, cbfunc recent_cb, integer max_recent)
    c_proc(xIupConfigRecentInit, {ih,menu,recent_cb,max_recent})
end procedure

global procedure IupConfigRecentUpdate(Ihandle ih, string filename)
    c_proc(xIupConfigRecentUpdate, {ih,filename})
end procedure

global procedure IupConfigDialogShow(Ihandle ih, Ihandle dialog, string name)
    c_proc(xIupConfigDialogShow, {ih,dialog,name})
end procedure

global procedure IupConfigDialogClosed(Ihandle ih, Ihandle dialog, string name)
    c_proc(xIupConfigDialogClosed, {ih,dialog,name})
end procedure


--****
-- === Additional
--

--constant hIupControls = iup_open_dll({
--                                    "iupcontrols.dll",
--                                    "libiupcontrols.so",
--                                    "libiupcontrols.dylib"
--                                   })
--
constant
--  xIupControlsOpen = define_c_proc(iupControls, "IupControlsOpen", {}),
    xIupCells = define_c_func(iupControls, "IupCells", {},P),
    xIupColorbar = define_c_func(iupControls, "IupColorbar", {},P),
    xIupColorBrowser = define_c_func(iupControls, "IupColorBrowser", {},P),
    xIupDial = define_c_func(iupControls, "IupDial", {P},P),
    xIupMatrix = define_c_func(iupControls, "IupMatrix", {P},P),
    xIupMatSetAttribute = define_c_proc(iupControls, "IupMatSetAttribute",{P,P,I,I,P}),
    xIupMatStoreAttribute = define_c_proc(iupControls, "IupMatStoreAttribute",{P,P,I,I,P}),
    xIupMatGetAttribute = define_c_func(iupControls, "IupMatrixGetAttribute",{P,P,I,I},P),
    xIupMatGetInt = define_c_func(iupControls, "IupMatrixGetInt",{P,P,I,I},I),
    xIupMatGetFloat = define_c_func(iupControls, "IupMatrixGetFloat",{P,P,I,I},F)

global function cells()
    controls_open()
    return c_func(xIupCells, {})
end function

global function colorbar()
    controls_open()
    return c_func(xIupColorbar, {})
end function

global function color_browser()
    controls_open()
    return c_func(xIupColorBrowser, {})
end function

global function dial(sequence orientation)
    controls_open()
    atom pOrientation = allocate_string(orientation)
    Ihandle ih = c_func(xIupDial, {pOrientation})
    free(pOrientation)
    return ih
end function

global function matrix(object action = 0, sequence attributes = {}, sequence data = {})
    controls_open()

    atom pAction = 0
    if sequence(action) then
        pAction = allocate_string(action)
    end if

    Ihandle ih = c_func(xIupMatrix, {pAction})

    free(pAction)

    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if

    return ih
end function

global procedure mat_set_attribute(Ihandle ih, sequence name, integer lin, integer col, atom pValue)
atom pName = allocate_string(name)
    c_proc(xIupMatSetAttribute, {ih, pName, lin, col, pValue})
    free(pName)
end procedure

global procedure mat_store_attribute(Ihandle ih, sequence name, integer lin, integer col, sequence val, sequence data = {})
    if length(data) then
        val = sprintf(val, data)
    end if

    atom pName = allocate_string(name)
    atom pValue = allocate_string(val)

    c_proc(xIupMatStoreAttribute, {ih, pName, lin, col, pValue})

    free(pName)
    free(pValue)
end procedure

global function mat_get_attribute(Ihandle ih, sequence name, integer lin, integer col)
atom pName = allocate_string(name)
atom pValue = c_func(xIupMatGetAttribute, {ih, pName, lin, col})

    free(pName)

    return peek_string(pValue)
end function

global function mat_get_int(Ihandle ih, sequence name, integer lin, integer col)
atom pName = allocate_string(name)

integer val = c_func(xIupMatGetInt, {ih, pName, lin, col})

    free(pName)

    return val
end function

global function mat_get_float(Ihandle ih, sequence name, integer lin, integer col)
atom pName = allocate_string(name)

atom val = c_func(xIupMatGetFloat, {ih, pName, lin, col})

    free(pName)

    return val
end function

--****
-- === Additional Web
--
atom hIupWeb = 0, xIupWebBrowserOpen, xIupWebBrowser

-- (you could make this global if you wanted, but it gets called automatically anyway)
procedure IupWebBrowserOpen()
    if hIupWeb=0 then
        hIupWeb = iup_open_dll({"iupweb.dll",
                                "libiupweb.so",
                                "libiupweb.dylib"})
        xIupWebBrowserOpen = define_c_proc(hIupWeb, "IupWebBrowserOpen", {})
        xIupWebBrowser = define_c_func(hIupWeb, "IupWebBrowser", {},P)
        c_proc(xIupWebBrowserOpen, {})
    end if
end procedure

global function IupWebBrowser()
    IupWebBrowserOpen()
    Ihandle ih = c_func(xIupWebBrowser, {})
    return ih
end function

--include resources.e

--****
-- === Images
--

global enum --   imErrorCodes { 
  IM_ERR_NONE=0, IM_ERR_OPEN, IM_ERR_ACCESS, IM_ERR_FORMAT, 
  IM_ERR_DATA, IM_ERR_COMPRESS, IM_ERR_MEM, IM_ERR_COUNTER 
--} 

global constant IM_GAMUT_MINMAX = 0x0100

global enum --  imToneGamut { 
  IM_GAMUT_NORMALIZE=0, IM_GAMUT_POW, IM_GAMUT_LOG, IM_GAMUT_EXP, 
  IM_GAMUT_INVERT, IM_GAMUT_ZEROSTART, IM_GAMUT_SOLARIZE, IM_GAMUT_SLICE, 
  IM_GAMUT_EXPAND, IM_GAMUT_CROP, IM_GAMUT_BRIGHTCONT 
--} 

--global type imImage(object o)
--  return atom(o) and o>=NULL and o=floor(o)
global type imImage(integer i)
--  return i>0
    return i>=0
end type

--global type imImage(object o)
--  return atom(o) and o>=NULL and o=floor(o)
--end type

constant iupIm = iup_open_dll({"iupim.dll",
                               "libiupim.so",
                               "libiupim.dylib"})

constant atom hIm = iup_open_dll({"im.dll",
                                  "?libiupcd.so",
                                  "?libiupcd.dylib"})

constant hImProcess = iup_open_dll({"im_process.dll",
                                    "?libiupweb.so",
                                    "?libiupweb.dylib"})

constant
    xIupImage                   = define_c_func(iup, "IupImage", {I,I,P}, P),
    xIupImageRGB                = define_c_func(iup, "IupImageRGB", {I,I,P}, P),
    xIupImageRGBA               = define_c_func(iup, "IupImageRGBA", {I,I,P}, P),
--DEV??
    xIupLoadImage               = define_c_func(iupIm, "IupLoadImage", {P}, P),
    xIupSaveImage               = define_c_func(iupIm, "IupSaveImage", {P,P,P}, I),
    xIupSaveImageAsText         = define_c_func(iup, "IupSaveImageAsText", {P,P,P,P}, I),
    xIupGetNativeHandleImage    = define_c_func(iupIm, "IupGetNativeHandleImage", {P}, P),
    xIupGetImageNativeHandle    = define_c_func(iupIm, "IupGetImageNativeHandle", {P}, P),
    xIupImageFromImImage        = define_c_func(iupIm, "IupImageFromImImage", {P}, P),
    ximFileImageLoadBitmap      = define_c_func(hIm, "imFileImageLoadBitmap", {P,I,P}, P),
    ximImageGetAttribString     = define_c_func(hIm, "imImageGetAttribString", {P,P}, P),
    ximImageSetAttribString     = define_c_proc(hIm, "imImageSetAttribString", {P,P,P}),
--(wrong one)
--  ximFileSaveImage            = define_c_func(iupIm, "imFileSaveImage", {P,P}, I),
    ximFileImageSave            = define_c_func(iupIm, "imFileImageSave", {P,P,P}, I),
    ximProcessRenderFloodFill   = define_c_proc(hImProcess, "imProcessRenderFloodFill", {P,I,I,P,F}),
    ximProcessRenderConstant    = define_c_proc(hImProcess, "imProcessRenderConstant", {P,P}),
    ximProcessToneGamut         = define_c_proc(hImProcess, "imProcessToneGamut", {}),
    ximProcessResize            = define_c_proc(hImProcess, "imProcessResize", {P,P,I}),
    ximProcessMirror            = define_c_proc(hImProcess, "imProcessMirror", {P,P}),
    ximProcessFlip              = define_c_proc(hImProcess, "imProcessFlip", {P,P}),
    ximProcessRotate180         = define_c_proc(hImProcess, "imProcessRotate180", {P,P}),
    ximProcessRotate90          = define_c_proc(hImProcess, "imProcessRotate90", {P,P,I}),
    ximProcessNegative          = define_c_proc(hImProcess, "imProcessNegative", {P,P}),
    ximImageRemoveAlpha         = define_c_proc(hIm, "imImageRemoveAlpha", {P}),
    ximImageCreate              = define_c_func(hIm, "imImageCreate", {I,I,I,I}, P),
    ximImageCreateBased         = define_c_func(hIm, "imImageCreateBased", {P,I,I,I,I}, P),
    ximConvertColorSpace        = define_c_func(hIm, "imConvertColorSpace", {P,P}, I),
    ximImageClone               = define_c_func(hIm, "imImageClone", {P}, P),
    ximImageDestroy             = define_c_proc(hIm, "imImageDestroy", {P}),
    $

global function IupImage(integer width, integer height, sequence pixels)
atom pPixels = allocate(length(pixels))
    poke(pPixels, pixels)
    Ihandle ih = c_func(xIupImage, {width, height, pPixels})
    free(pPixels)
    return ih
end function

global function IupImageA(integer width, integer height, atom pPixels)
    Ihandle ih = c_func(xIupImage, {width, height, pPixels})
    return ih
end function

global function IupImageRGB(integer width, integer height, sequence pixels)
atom pPixels = allocate(length(pixels))
    poke(pPixels, pixels)
    Ihandle ih = c_func(xIupImageRGB, {width, height, pPixels})
    free(pPixels)
    return ih
end function

global function IupImageRGBA(integer width, integer height, sequence pixels)
atom pPixels = allocate(length(pixels))
    poke(pPixels, pixels)
    Ihandle ih = c_func(xIupImageRGBA, {width, height, pPixels})
    free(pPixels)
    return ih
end function

global function IupLoadImage(string filename)
    Ihandln ih = c_func(xIupLoadImage, {filename})
    return ih
end function

global function IupSaveImage(Ihandle ih, string file_name, string fmt)
--  integer result = c_func(xIupSaveImage, {ih,file_name,fmt})
--  return result
    if c_func(xIupSaveImage, {ih,file_name,fmt})=0 then
        return IupGetGlobal("IUPIM_LASTERROR")
    end if
    return 1
end function

global function IupSaveImageAsText(Ihandle ih, string filename, string fmt, nullable_string name=NULL)
    integer result = c_func(xIupSaveImageAsText, {ih, filename, fmt, name})
    return result   -- 0=failure
end function

--imImage* IupGetNativeHandleImage(void* handle);
global function IupGetNativeHandleImage(atom handle)
    imImage result = c_func(xIupGetNativeHandleImage, {handle})
    return result
end function

--void* IupGetImageNativeHandle(const imImage* image);
global function IupGetImageNativeHandle(imImage image)
    atom result = c_func(xIupGetImageNativeHandle, {image})
    return result
end function

--Ihandle* IupImageFromImImage(const imImage* image);
global function IupImageFromImImage(imImage image)
    Ihandle ih = c_func(xIupImageFromImImage, {image})
    return ih
end function


--/*
("Use IupLoadImage" - scuri)
--DEV/BLUFF: (needs undoing!)
The C function imFileImageLoadBitmap is not wrapped, but IupLoadImage invokes it internally (when appropriate),
and the latter returns an Ihandle which <i>can</i> be used on a control, while the former returns an imImage, 
which cannot so readily be used. Hence IupLoadImage should be used instead of imFileImageLoadBitmap.
imImage* imFileImageLoadBitmap  (       const char *    file_name,
int     index,
int *   error    
)                       
Loads an image from file, but forces the image to be a bitmap. Open, loads and closes the file. 
index specifies the image number between 0 and image_count-1. 
Returns NULL if failed. Attributes from the file will be stored at the image. See also imErrorCodes.
--*/
global function imFileImageLoadBitmap(string filename, integer index, atom pError)
    imImage image = c_func(ximFileImageLoadBitmap,{filename,index,pError})
    return image
end function

--const char *  imImageGetAttribString (const imImage *image, const char *attrib) 
global function imImageGetAttribString(imImage image, string attrib)
atom pString = c_func(ximImageGetAttribString,{image,attrib})
    if pString=NULL then return "" end if
    return peek_string(pString)
end function

--/*
void imImageSetAttribString(const imImage* image, const char* attrib, const char* value)
Changes an extended attribute as a string.
--*/
global procedure imImageSetAttribString(imImage image, string attrib, nullable_string v)
    c_proc(ximImageSetAttribString,{image,attrib,v})
end procedure

--/* (wrong one)
int imFileSaveImage(imFile* ifile, const imImage* image)      
Saves the image to an already open file. 
This will call imFileWriteImageInfo and imFileWriteImageData. 
Attributes from the image will be stored at the file. Returns error code.
--*/

--/*
int imFileImageSave(const char* file_name, const char* format, const imImage* image)      
Saves the image to file. Open, saves and closes the file. 
Returns error code. 
Attributes from the image will be stored at the file.
--*/

global function imFileImageSave(string filename, string fmt, imImage image)
    integer error = c_func(ximFileImageSave,{filename,fmt,image})
    return error
end function

--/*
void imProcessRenderFloodFill(imImage *image, int start_x, int start_y, float* replace_color, float tolerance)    
Render a color flood fill. 
Image must the IM_RGB color space. replace_color must have 3 components
--*/
global procedure imProcessRenderFloodFill(imImage image, integer start_x, integer start_y, sequence replace_color, atom tolerance)
atom pColour = allocate(12,1)
    iup_poke_float(pColour,replace_color)
    c_proc(ximProcessRenderFloodFill,{image,start_x,start_y,pColour,tolerance})
end procedure

--/*
int imProcessRenderConstant  ( imImage *  image,  
  float *  value   
 )    
Render a constant. The number of values must match the depth of the image.
--*/
global procedure imProcessRenderConstant(imImage image, sequence v)
atom pColour = allocate(12,1)
    iup_poke_float(pColour,v)
    c_proc(ximProcessRenderConstant,{image,pColour})
end procedure

--/*
void imProcessToneGamut(const imImage* src_image, imImage* dst_image, int op, float* params)
Apply a gamut operation with arguments. 
Supports all data types except complex. 
For IM_GAMUT_NORMALIZE when min > 0 and max < 1, it will just do a copy. 
IM_BYTE images have min=0 and max=255 always. 
To control min and max values use the IM_GAMUT_MINMAX flag. Can be done in-place. When there is no extra parameters, params can use NULL.
--*/
global procedure imProcessToneGamut(imImage src_image, imImage dst_image, integer op, sequence params)
atom pParams = allocate(4*length(params),1)
    iup_poke_float(pParams,params)
    c_proc(ximProcessToneGamut,{src_image, dst_image, op, pParams})
end procedure

--/*
int imProcessResize(const imImage* src_image, imImage* dst_image, int order)

Change the image size using the given interpolation order. 
Supported interpolation orders: 

0 - zero order (near neighborhood) [default in Lua for MAP and BINARY] 
1 - first order (bilinear interpolation) [default in Lua] 
3 - third order (bicubic interpolation) Images must be of the same type. If image type is IM_MAP or IM_BINARY, must use order=0. 
Returns zero if the counter aborted. 
--*/
global procedure imProcessResize(imImage src_image, imImage dst_image, integer order)
    c_proc(ximProcessResize,{src_image, dst_image, order})
end procedure

--/*
void imProcessMirror(const imImage* src_image, imImage* dst_image)
Mirror the image in a horizontal flip. Swap columns. 
Images must be of the same type and size. Can be done in-place.
--*/
global procedure imProcessMirror(imImage src_image, imImage dst_image)
    c_proc(ximProcessMirror,{src_image, dst_image})
end procedure

--/*
void imProcessFlip(const imImage* src_image, imImage* dst_image)
Apply a vertical flip. Swap lines. 
Images must be of the same type and size. Can be done in-place.
--*/
global procedure imProcessFlip(imImage src_image, imImage dst_image)
    c_proc(ximProcessFlip,{src_image, dst_image})
end procedure

--/*
void imProcessRotate180(const imImage* src_image, imImage* dst_image)
Rotates the image in 180 degrees. Swap columns and swap lines. 
Images must be of the same type and size.
--*/
global procedure imProcessRotate180(imImage src_image, imImage dst_image)
    c_proc(ximProcessRotate180,{src_image, dst_image})
end procedure

--/*
void imProcessRotate90(const imImage* src_image, imImage* dst_image, int dir_clockwise)
Rotates the image in 90 degrees counterclockwise or clockwise. Swap columns by lines. 
Images must be of the same type. Target width and height must be source height and width. 
Direction can be clockwise (1) or counter clockwise (-1).
--*/
global procedure imProcessRotate90(imImage src_image, imImage dst_image, integer dir_clockwise)
    c_proc(ximProcessRotate90,{src_image, dst_image,dir_clockwise})
end procedure

--/*
void imProcessNegative(const imImage* src_image, imImage* dst_image)
A negative effect. Uses imProcessToneGamut with IM_GAMUT_INVERT for non MAP images. 
Supports all color spaces and all data types except complex. 
Can be done in-place.
--*/
global procedure imProcessNegative(imImage src_image, imImage dst_image)
    c_proc(ximProcessNegative,{src_image, dst_image})
end procedure


--/*
void imImageRemoveAlpha(imImage* image)
Removes the alpha channel plane if any.
--*/
global procedure imImageRemoveAlpha(imImage image)
    c_proc(ximImageRemoveAlpha,{image})
end procedure

--/*
imImage* imImageCreate(int width, int height, int color_space, int data_type)
Creates a new image. See also imDataType and imColorSpace. Image data is cleared as imImageClear. 
In Lua the IM image metatable name is "imImage". When converted to a string will return "imImage(%p) [width=%d,height=%d,color_space=%s,data_type=%s,depth=%d]" where p is replaced by the userdata address, and other values are replaced by the respective attributes. If the image is already destroyed by im.ImageDestroy, then it will return also the suffix "-destroyed".
--*/
global function imImageCreate(integer width, integer height, integer color_space, integer data_type)
    imImage image = c_func(ximImageCreate,{width,height,color_space,data_type})
    return image
end function

--/*
imImage* imImageCreateBased(const imImage * image, int width, int height, int color_space, int data_type)
Creates a new image based on an existing one. 
If the addicional parameters are -1, the given image parameters are used. 
The image atributes always are copied. HasAlpha is copied. See also imDataType and imColorSpace.
--*/
global function imImageCreateBased(imImage image, integer width, integer height, integer color_space, integer data_type)
    imImage res = c_func(ximImageCreateBased,{image,width,height,color_space,data_type})
    return res
end function

--/*
int imConvertColorSpace(const imImage* src_image, imImage* dst_image)
Converts one color space to another. 
Images must be of the same size and data type. If color mode is the same nothing is done. 
CMYK can be converted to RGB only, and it is a very simple conversion. 
All colors can be converted to Binary, the non zero gray values are converted to 1. 
RGB to Map uses the median cut implementation from the free IJG JPEG software, copyright Thomas G. Lane. 
Alpha channel is considered and Transparency* attributes are converted to alpha channel. 
All other color space conversions assume sRGB and CIE definitions, see Color Manipulation. 
Returns IM_ERR_NONE, IM_ERR_DATA or IM_ERR_COUNTER, see also imErrorCodes. 
See also imColorSpace, imColorModeConfig and Color Mode Utilities.
--*/
global procedure imConvertColorSpace(imImage src_image, imImage dst_image)
integer err = c_func(ximConvertColorSpace,{src_image,dst_image})
    if err!=IM_ERR_NONE then ?9/0 end if
end procedure

--/*
imImage* imImageClone(const imImage* image)  
Creates a clone of the image. i.e. same attributes but ignore contents.
--*/
global function imImageClone(imImage image)
    imImage clone = c_func(ximImageClone,{image})
    return clone
end function

--/*
void imImageDestroy(imImage* image)
Destroys the image and frees the memory used. image data is destroyed only if its data[0] is not NULL
--*/
global procedure imImageDestroy(imImage image)
    c_proc(ximImageDestroy,{image})
end procedure

global enum --  imDataType { 
  IM_BYTE=0, IM_SHORT, IM_USHORT, IM_INT, 
  IM_FLOAT, IM_DOUBLE, IM_CFLOAT, IM_CDOUBLE 
--} 
global enum --  imColorSpace { 
  IM_RGB=0, IM_MAP, IM_GRAY, IM_BINARY, 
  IM_CMYK, IM_YCBCR, IM_LAB, IM_LUV, 
  IM_XYZ 
--} 

--/*
Data Fields 
int  width 
int  height 
int  color_space 
int  data_type 
int  has_alpha 
int  depth 
int  line_size 
int  plane_size 
int  size 
int  count 
void **  data 
long *  palette 
int  palette_count 
void *  attrib_table 
--*/
global function im_width(imImage image)
    return peek4s(image)
end function

global function im_height(imImage image)
    return peek4s(image+4)
end function

global function im_color_space(imImage image)
    return peek4s(image+8)
end function

global function im_data(imImage image)
    return peekNS(image+44,machine_word(),0)
end function


--****
-- === Keyboard
--

constant
    xIupPreviousField   = define_c_func(iup, "IupPreviousField", {P},P),
    xIupNextField       = define_c_func(iup, "IupNextField", {P},P),
    xIupSetFocus        = define_c_proc(iup, "IupSetFocus", {P}),
    xIupGetFocus        = define_c_func(iup, "IupGetFocus", {},P)

global function IupPreviousField(Ihandle ih)
    Ihandln prev = c_func(xIupPreviousField, {ih})
    return prev
end function

global function IupNextField(Ihandle ih)
    Ihandln next = c_func(xIupNextField, {ih})
    return next
end function

global procedure IupSetFocus(Ihandle ih)
    c_proc(xIupSetFocus, {ih})
end procedure

global function IupGetFocus()
    Ihandln ih = c_func(xIupGetFocus, {})
    return ih
end function


--****
-- === Menus
--

constant
    xIupMenuv       = define_c_func(iup, "IupMenuv", {P}, P),
    xIupItem        = define_c_func(iup, "IupItem",  {P,P}, P),
    xIupSeparator   = define_c_func(iup, "IupSeparator", {}, P),
    xIupSubmenu     = define_c_func(iup, "IupSubmenu", {P,P}, P)

global function IupMenu(sequence children)
atom pChildren = iup_ptr_array(children)
Ihandle ih = c_func(xIupMenuv, {pChildren})
    free(pChildren)
    return ih
end function

global function IupMenuItem(string title, nullable_string action = NULL, atom func = NULL, string attributes = "", sequence data = {})
    Ihandle ih = c_func(xIupItem, {title, action})
    if func!=NULL then
        IupSetCallback(ih, ACTION, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupItem(string title, nullable_string action = NULL, atom func = NULL, string attributes = "", sequence data = {})
    return IupMenuItem(title, action, func, attributes, data)
end function

global function IupSeparator()
    Ihandle ih = c_func(xIupSeparator, {})
    return ih
end function

--Ihandle* IupSubmenu(const char* title, Ihandle* child);
global function IupSubmenu(nullable_string title, Ihandln menu)
    Ihandle ih = c_func(xIupSubmenu, {title, menu})
    return ih
end function

--****
-- === Names
--

constant
--  xIupSetHandle = define_c_proc(iup, "IupSetHandle", {P,P}),
--  xIupGetHandle = define_c_func(iup, "IupGetHandle", {P},P),
    xIupGetName = define_c_func(iup, "IupGetName", {P},P),
    xIupGetAllNames = define_c_func(iup, "IupGetAllNames", {P,I},I),
    xIupGetAllDialogs = define_c_func(iup, "IupGetAllDialogs", {P,I},I)

global function IupGetName(Ihandle ih)
atom pName = c_func(xIupGetName, {ih})
    if pName=NULL then return "" end if
    return peek_string(pName)
end function

--**
-- Returns the names of all interface elements that have an associated name using IupSetHandle.
-- 
global function IupGetAllNames()
integer n = c_func(xIupGetAllNames, {NULL,0})
atom ptr = allocate_data(machine_word()*n, 1)
    n = c_func(xIupGetAllNames, {ptr,n})
    return iup_peek_string_pointer_array(ptr, n)
end function

global function IupGetAllDialogs()
integer n = c_func(xIupGetAllDialogs, {NULL,0})
atom ptr = allocate_data(machine_word()*n, 1)
    n = c_func(xIupGetAllDialogs, {ptr,n})
    return iup_peek_string_pointer_array(ptr, n)
end function

--include mouse.e
global function isshift(atom pchar)
    return peek(pchar)='S'
end function

global function iscontrol(atom pchar)
    return peek(pchar+1)='C'
end function

global function isbutton1(atom pchar)
    return peek(pchar+2)='1'
end function

global function isbutton2(atom pchar)
    return peek(pchar+3)='2'
end function

global function isbutton3(atom pchar)
    return peek(pchar+4)='3'
end function

global function isbutton4(atom pchar)
    return peek(pchar+8)='4'
end function

global function isbutton5(atom pchar)
    return peek(pchar+9)='5'
end function

global function isdouble(atom pchar)
    return peek(pchar+5)='D'
end function

global function isalt(atom pchar)
    return peek(pchar+6)='A'
end function

global function issys(atom pchar)
    return peek(pchar+7)='Y'
end function

--include iupmisc.e
constant
    xIupClipboard = define_c_func(iup, "IupClipboard", {}, P),
    xIupTimer = define_c_func(iup, "IupTimer", {}, P),
    xIupUser = define_c_func(iup, "IupUser", {}, P),
    xIupHelp = define_c_func(iup, "IupHelp", {P}, I)

global function IupClipboard()
    Ihandle ih = c_func(xIupClipboard, {})
    return ih
end function

global function IupTimer(atom func = NULL, integer msecs = 0, integer active = 1)
    Ihandle ih = c_func(xIupTimer, {})
    if func!=NULL and msecs!=0 then
        IupSetCallback(ih, ACTION_CB, func)
        IupSetAttributes(ih, "TIME=%d,RUN=%s", {msecs, iff(active, "YES", "NO")})
    end if
    return ih
end function

--global function IupTimer(integer rid = -2, integer msecs = 0, integer active = 1)
--Ihandle ih = c_func(xIupTimer, {})
--  if rid>=0 and msecs!=0 then
--      IupSetCallback(ih, ACTION_CB, rid)
--      IupSetAttributes(ih, "TIME=%d,RUN=%s", {msecs, iff(active, "YES", "NO")})
--  end if
--  return ih
--end function

global function IupUser()
    Ihandle ih = c_func(xIupUser, {})
    return ih
end function

global function IupHelp(string url)
-- returns 1 if successful, -1 if failed, -2 if file not found
    return c_func(xIupHelp, {url})
end function


--cd.e:
constant atom hCd = iup_open_dll({"cd.dll",
                                  "libcd.so",
                                  "libcd.dylib"})

constant atom hCdIup = iup_open_dll({"iupcd.dll",
                                     "libiupcd.so",
                                     "libiupcd.dylib"})

constant atom hCdGL = iup_open_dll({"cdgl.dll",
                                     "?libiupcd.so",
                                     "?libiupcd.dylib"})

--global type cdCanvas(object o)
--  return atom(o) and o>=NULL and o=floor(o)
global type cdCanvas(integer i)
    return i>0
end type

--global type cdCanvas(object o)
--  return atom(o) and o>=NULL and o=floor(o)
--end type

--****
-- === Canvas Draw Constants

global constant
    CD_QUERY = -1,
    CD_ERROR = -1,
    CD_OK = 0

--****
-- === Polygon Mode Constants

global constant
    CD_FILL = 0,
    CD_OPEN_LINES = 1,
    CD_CLOSED_LINES = 2,
    CD_CLIP = 3,
    CD_BEZIER = 4,
    CD_REGION = 5,
    CD_POLYCUSTOM = 10

--****
-- === Bitmap Type Constants
--
--  These definitions are compatible with the IM library

global constant
    CD_RGB = 0,
    CD_MAP = 1,
    CD_RGBA = #100

--****
-- === Bitmap Data Constants

global constant
    CD_IRED = 0,
    CD_IGREEN = 1,
    CD_IBLUE = 2,
    CD_IALPHA = 3,
    CD_INDEX = 4,
    CD_COLORS = 5

--****
-- === Clip Mode Constants
--

global constant
    CD_CLIPOFF = 0,
    CD_CLIPAREA = 1,
    CD_CLIPPOLYGON = 2,
    CD_CLIPREGION = 3

--****
-- === Region Combine Mode Constants
--

global constant
    CD_UNION = 0,
    CD_INTERSECT = 1,
    CD_DIFFERENCE = 2,
    CD_NOTINTERSECT = 3

--****
-- === Fill Mode Constants
--

global constant
    CD_EVENODD = 0,
    CD_WINDING = 1

--****
-- === Line Join Constants
--

global constant
    CD_MITER = 0,
    CD_BEVEL = 1,
    CD_ROUND = 2

--****
-- === Line Cap Constants
--

global constant
    CD_CAPFLAT = 0,
    CD_CAPSQUARE = 1,
    CD_CAPROUND = 2

--****
-- === Background Opacity Mode Constants
--

global constant
    CD_OPAQUE = 0,
    CD_TRANSPARENT = 1

--****
-- === Write Mode Constants
--

global constant
    CD_REPLACE = 0,
    CD_XOR = 1,
    CD_NOT_XOR = 2

--****
-- === Color Allocation Mode Constants
--
-- Pallette
--

global constant
    CD_POLITE = 0,
    CD_FORCE = 1

--****
-- === Line Style Constants
--

global constant
    CD_CONTINUOUS = 0,
    CD_DASHED = 1,
    CD_DOTTED = 2,
    CD_DASH_DOT = 3,
    CD_DASH_DOT_DOT = 4,
    CD_CUSTOM = 5

--****
-- === Marker Type Constants
--

global constant
    CD_PLUS = 0,
    CD_STAR = 1,
    CD_CIRCLE = 2,
    CD_X = 3,
    CD_BOX = 4,
    CD_DIAMOND = 5,
    CD_HOLLOW_CIRCLE = 6,
    CD_HOLLOW_BOX = 7,
    CD_HOLLOW_DIAMOND = 8

--****
-- === Hatch Type Constants
--

global constant
    CD_HORIZONTAL = 0,
    CD_VERTICAL = 1,
    CD_FDIAGONAL = 2,
    CD_BDIAGONAL = 3,
    CD_CROSS = 4,
    CD_DIAGCROSS = 5

--****
-- === Interior Style Constants
--

global constant
    CD_SOLID = 0,
    CD_HATCH = 1,
    CD_STIPPLE = 2,
    CD_PATTERN = 3,
    CD_HOLLOW = 4

--****
-- === Text Alignment Constants
--

global constant
    CD_NORTH = 0,
    CD_SOUTH = 1,
    CD_EAST = 2,
    CD_WEST = 3,
    CD_NORTH_EAST = 4,
    CD_NORTH_WEST = 5,
    CD_SOUTH_EAST = 6,
    CD_SOUTH_WEST = 7,
    CD_CENTER = 8,
    CD_BASE_LEFT = 9,
    CD_BASE_CENTER = 10,
    CD_BASE_RIGHT = 11

--****
-- === Style Constants
--

global constant
    CD_PLAIN = 0,
    CD_BOLD = 1,
    CD_ITALIC = 2,
    CD_UNDERLINE = 4,
    CD_STRIKEOUT = 8,
    CD_BOLD_ITALIC = or_bits(CD_BOLD, CD_ITALIC)

--****
-- === Font Size Constants
--

global constant
    CD_SMALL = 8,
    CD_STANDARD = 12,
    CD_LARGE = 18

--****
-- === Canvas Capabilities Constants
--

global constant
    CD_CAP_NONE             = 0x00000000,
    CD_CAP_FLUSH            = 0x00000001,
    CD_CAP_CLEAR            = 0x00000002,
    CD_CAP_PLAY             = 0x00000004,
    CD_CAP_YAXIS            = 0x00000008,
    CD_CAP_CLIPAREA         = 0x00000010,
    CD_CAP_CLIPPOLY         = 0x00000020,
    CD_CAP_REGION           = 0x00000040,
    CD_CAP_RECT             = 0x00000080,
    CD_CAP_CHORD            = 0x00000100,
    CD_CAP_IMAGERGB         = 0x00000200,
    CD_CAP_IMAGERGBA        = 0x00000400,
    CD_CAP_IMAGEMAP         = 0x00000800,
    CD_CAP_GETIMAGERGB      = 0x00001000,
    CD_CAP_IMAGESRV         = 0x00002000,
    CD_CAP_BACKGROUND       = 0x00004000,
    CD_CAP_BACKOPACITY      = 0x00008000,
    CD_CAP_WRITEMODE        = 0x00010000,
    CD_CAP_LINESTYLE        = 0x00020000,
    CD_CAP_LINEWITH         = 0x00040000,
    CD_CAP_FPRIMTIVES       = 0x00080000,
    CD_CAP_HATCH            = 0x00100000,
    CD_CAP_STIPPLE          = 0x00200000,
    CD_CAP_PATTERN          = 0x00400000,
    CD_CAP_FONT             = 0x00800000,
    CD_CAP_FONTDIM          = 0x01000000,
    CD_CAP_TEXTSIZE         = 0x02000000,
    CD_CAP_TEXTORIENTATION  = 0x04000000,
    CD_CAP_PALETTE          = 0x08000000,
    CD_CAP_LINECAP          = 0x10000000,
    CD_CAP_LINEJOIN         = 0x20000000,
    CD_CAP_PATH             = 0x40000000,
    CD_CAP_BEZIER           = 0x80000000,
    CD_CAP_ALL              = 0xFFFFFFFF

--
--****
-- === Canvas Draw Play Constants
--

global constant
    CD_SIZECB = 0,
    CD_ABORT = 1,
    CD_CONTINUE = 0

--****
-- === Simulation Flag Constants
--

global constant
    CD_SIM_NONE = #0,
    CD_SIM_LINE = #1,
    CD_SIM_RECT = #2,
    CD_SIM_BOX = #4,
    CD_SIM_ARC = #8,
    CD_SIM_SECTOR = #10,
    CD_SIM_CHORD = #20,
    CD_SIM_POLYLINE = #40,
    CD_SIM_POLYGON = #80,
    CD_SIM_TEXT = #100,
    CD_SIM_ALL = #FFFF,
    CD_SIM_LINES = or_bits(CD_SIM_LINE, or_bits(CD_SIM_RECT, or_bits(CD_SIM_ARC, CD_SIM_POLYLINE))),
    CD_SIM_FILLS = or_bits(CD_SIM_BOX, or_bits(CD_SIM_SECTOR, or_bits(CD_SIM_CHORD, CD_SIM_POLYGON)))

--****
-- === Predefined Color Constants
--
-- These are simply for convenience
--

global constant
    CD_RED = #FF0000,
    CD_DARK_RED = #800000,
    CD_GREEN = #FF00,
    CD_DARK_GREEN = #8000,
    CD_BLUE = #FF,
    CD_DARK_BLUE = #80,
    CD_YELLOW = #FFFF00,
    CD_DARK_YELLOW = #808000,
    CD_MAGENTA = #FF00FF,
    CD_DARK_MAGENTA = #800080,
    CD_CYAN = #FFFF,
    CD_DARK_CYAN = #8080,
    CD_WHITE = #FFFFFF,
    CD_BLACK = #0,
    CD_DARK_GRAY = #808080,
    CD_GRAY = #C0C0C0

--****
-- === Conversion Factor Constants
--
-- These are simply for convenience
--

global constant
    --** 
-- Milimeters to Points (pt = CD_MM2PT * mm)
CD_MM2PT = 2.83465,

--**
-- Radians to Degrees (deg = CD_RAD2DEG * rad)
CD_RAD2DEG = 57.2958,

--**
-- Degrees to Radians (rad = CD_DEG2RAD * deg)
CD_DEG2RAD = 0.0174533

--****
-- === Version Information Routines

constant
    xcdVersion       = define_c_func(hCd, "cdVersion", {}, P),
    xcdVersionDate   = define_c_func(hCd, "cdVersionDate", {}, P),
    xcdVersionNumber = define_c_func(hCd, "cdVersionNumber", {}, I)

global function cdVersion()
    return peek_string(c_func(xcdVersion, {}))
end function

global function cdVersionDate()
    return peek_string(c_func(xcdVersionDate, {}))
end function

global function cdVersionNumber()
    return c_func(xcdVersionNumber, {})
end function

--****
-- === Canvas Initialization Routines
--

constant
    xcdContextIup           = define_c_func(hCdIup, "cdContextIup", {}, P),
    xcdContextPrinter       = define_c_func(hCd, "cdContextPrinter", {}, P),
    xcdContextPS            = define_c_func(hCd, "cdContextPS", {}, P),
    xcdContextPicture       = define_c_func(hCd, "cdContextPicture", {}, P),
    xcdContextGL            = define_c_func(hCdGL,"cdContextGL", {}, P),
    xcdContextIupDBuffer    = define_c_func(hCdIup,"cdContextIupDBuffer", {}, P)

global constant
    CD_IUP          = c_func(xcdContextIup, {}),
    CD_PRINTER      = c_func(xcdContextPrinter, {}),
    CD_PS           = c_func(xcdContextPS, {}),
    CD_PICTURE      = c_func(xcdContextPicture, {}),
    CD_GL           = c_func(xcdContextGL, {}),
    CD_IUPDBUFFER   = c_func(xcdContextIupDBuffer, {})


ifdef WINDOWS then
constant
    xcdContextNativeWindow = define_c_func(hCd, "cdContextNativeWindow", {},P),
    xcdGetScreenSize = define_c_proc(hCd, "cdGetScreenSize", {P,P,P,P}),
    xcdGetScreenColorPlanes = define_c_func(hCd, "cdGetScreenColorPlanes", {},I)

global constant
    CD_NATIVEWINDOW = c_func(xcdContextNativeWindow, {})
end ifdef

global function cdGetScreenSize()
ifdef WINDOWS then
--DEV machine_bits?
atom w = allocate(24)
atom h = w+4
atom w_mm = h+4
atom h_mm = w_mm+8

    c_proc(xcdGetScreenSize, {w, h, w_mm, h_mm})

    sequence data = peek4s({w, 2}) & iup_peek_double({w_mm, 2})

    free(w)

    return data
elsedef
    return 0
end ifdef
end function

global function cdGetScreenColorPlanes()
ifdef WINDOWS then
    return c_func(xcdGetScreenColorPlanes, {})
elsedef
    return 0
end ifdef
end function

constant
    xcdCreateCanvas     = define_c_func(hCd, "cdCreateCanvas", {P,P}, P),
    xcdKillCanvas       = define_c_proc(hCd, "cdKillCanvas", {P}),
    xcdCanvasGetContext = define_c_func(hCd, "cdCanvasGetContext", {P}, P),
    xcdCanvasActivate   = define_c_func(hCd, "cdCanvasActivate", {P}, I),
    xcdCanvasDeactivate = define_c_proc(hCd, "cdCanvasDeactivate", {P}),
    xcdUseContextPlus   = define_c_func(hCd, "cdUseContextPlus", {I}, I),
    xcdInitContextPlus  = define_c_proc(hCd, "cdInitContextPlus", {})

global function cdCreateCanvas(atom hCdContext, atom_string data, sequence params={})
    if length(params) then
        data = sprintf(data,params)
    end if
    cdCanvas hCdCanvas = c_func(xcdCreateCanvas,{hCdContext,data})
    return hCdCanvas
end function

global procedure cdKillCanvas(cdCanvas hCdCanvas)
    c_proc(xcdKillCanvas, {hCdCanvas})
end procedure

global function cdCanvasGetContext(cdCanvas hCdCanvas)
    atom hCdContext = c_func(xcdCanvasGetContext, {hCdCanvas})
    return hCdContext
end function

global function cdCanvasActivate(cdCanvas hCdCanvas)
    return c_func(xcdCanvasActivate, {hCdCanvas})
end function

global procedure cavas_deactivate(cdCanvas hCdCanvas)
    c_proc(xcdCanvasDeactivate, {hCdCanvas})
end procedure

global function cdUseContextPlus(integer use)
    return c_func(xcdUseContextPlus, {use})
end function

global procedure cdInitContextPlus()
    c_proc(xcdInitContextPlus, {})
end procedure

--****
-- === Context Routines
--

constant
    xcdContextRegisterCallback  = define_c_func(hCd, "cdContextRegisterCallback", {P,I,P}, I),
    xcdContextCaps              = define_c_func(hCd, "cdContextCaps", {P}, UL),
    xcdCanvasPlay               = define_c_func(hCd, "cdCanvasPlay", {P,P,I,I,I,I,P}, I)

global function context_register_callback(atom hCdContext, integer cb, integer cbFunc)
    return c_func(xcdContextRegisterCallback, {hCdContext, cb, cbFunc})
end function

global function cdContextCaps(atom hCdContext)
    return c_func(xcdContextCaps, {hCdContext})
end function

-- ??????
global function canvas_play(cdCanvas hCdCanvas, atom hCdContext, integer xmin, integer xmax, integer ymin, integer ymax, sequence data)
atom fnVal, pData

    pData = allocate_string(data)
    fnVal = c_func(xcdCanvasPlay, {hCdCanvas, hCdContext, xmin, xmax, ymin, ymax, pData})
    free(pData)
    return fnVal
end function

--DEV optional??
constant atom hCdIm = iup_open_dll({"cdim.dll",
                                    "?libiupcd.so",
                                    "?libiupcd.dylib"})

constant
    xcdContextImImage           = define_c_func(hCdIm, "cdContextImImage", {}, P),
    xcdCanvasPutImImage         = define_c_proc(hCdIm, "cdCanvasPutImImage", {P,P,I,I,I,I}),
    $

--cdContext* cdContextImImage(void)
global function cdContextImImage()
    return c_func(xcdContextImImage,{})
end function

global constant CD_IMIMAGE = cdContextImImage()

--void cdCanvasPutImImage(cdCanvas* canvas, const imImage* image, int x, int y, int w, int h); [in C]
--void cdfCanvasPutImImage(cdCanvas* canvas, const imImage* image, double x, double y, double w, double h); [in C]
--void wdCanvasPutImImage(cdCanvas* canvas, const imImage* image, double x, double y, double w, double h); (WC) [in C]
global procedure cdCanvasPutImImage(cdCanvas canvas, imImage image, integer x, integer y, integer w, integer h)
    c_proc(xcdCanvasPutImImage,{canvas,image,x,y,w,h})
end procedure



-----------------------------------------------------------------------------------------
--
-- control
--
-----------------------------------------------------------------------------------------
constant
    xcdCanvasSimulate = define_c_func(hCd, "cdCanvasSimulate", {P,I},I),
    xcdCanvasFlush = define_c_proc(hCd, "cdCanvasFlush", {P}),
    xcdCanvasClear = define_c_proc(hCd, "cdCanvasClear", {P}),
    xcdCanvasSaveState = define_c_func(hCd, "cdCanvasSaveState", {P},P),
    xcdCanvasRestoreState = define_c_proc(hCd, "cdCanvasRestoreState", {P,P}),
    xcdCanvasReleaseState = define_c_proc(hCd, "cdReleaseState", {P}),
    xcdCanvasSetAttribute = define_c_proc(hCd, "cdCanvasSetAttribute", {P,P,P}),
    --void     cdCanvasSetfAttribute(cdCanvas* canvas, const char* name, const char* format, ...);
    xcdCanvasGetAttribute = define_c_func(hCd, "cdCanvasGetAttribute", {P,P},P)

global function canvas_simulate(cdCanvas hCdCanvas, integer mode)
    return c_func(xcdCanvasSimulate, {hCdCanvas, mode})
end function

global procedure cdCanvasFlush(cdCanvas hCdCanvas)
    c_proc(xcdCanvasFlush, {hCdCanvas})
end procedure

global procedure cdCanvasClear(cdCanvas hCdCanvas)
    c_proc(xcdCanvasClear, {hCdCanvas})
end procedure

global function canvas_save_state(cdCanvas hCdCanvas)
    return c_func(xcdCanvasSaveState, {hCdCanvas})
end function

global procedure canvas_restore_state(cdCanvas hCdCanvas, atom hCdState)
    c_proc(xcdCanvasRestoreState, {hCdCanvas, hCdState})
end procedure

global procedure canvas_release_state(cdCanvas hCdState)
    c_proc(xcdCanvasReleaseState, {hCdState})
end procedure

--DEV (doc) cdCanvasSetfAttribute (same)
global procedure cdCanvasSetAttribute(cdCanvas hCdCanvas, string name, string val, sequence data={})
    if length(data) then
        val = sprintf(val, data)
    end if
    c_proc(xcdCanvasSetAttribute, {hCdCanvas, name, val})
end procedure

global function cdCanvasGetAttribute(cdCanvas hCdCanvas, string name)
atom pRes
    pRes = c_func(xcdCanvasGetAttribute, {hCdCanvas, name})
    if pRes=NULL then return NULL end if
    return peek_string(pRes)
end function


------------------------------------------------------------------------------------------
--
--  coordinate transformation 
--
------------------------------------------------------------------------------------------
constant
    xcdCanvasGetSize = define_c_proc(hCd, "cdCanvasGetSize", {P,I,I,P,P}),
--  xcdCanvasUpdateYAxis = define_c_func(hCd, "cdCanvasUpdateYAxis", {P,P},I),
    xcdfCanvasUpdateYAxis = define_c_func(hCd, "cdfCanvasUpdateYAxis", {P,P},D),
--  xcdCanvasInvertYAxis = define_c_func(hCd, "cdCanvasInvertYAxis", {P,I},I),
    xcdfCanvasInvertYAxis = define_c_func(hCd, "cdfCanvasInvertYAxis", {P,D},D),
    xcdCanvasMM2Pixel = define_c_proc(hCd, "cdCanvasMM2Pixel", {P,D,D,P,P}),
--  xcdfCanvasMM2Pixel = define_c_proc(hCd, "cdfCanvasMM2Pixel", {P,D,D,P,P}),
--  xcdCanvasPixel2MM = define_c_proc(hCd, "cdCanvasPixel2MM", {P,I,I,P,P}),
    xcdfCanvasPixel2MM = define_c_proc(hCd, "cdfCanvasPixel2MM", {P,D,D,P,P}),
--  xcdCanvasOrigin = define_c_proc(hCd, "cdCanvasOrigin", {P,I,I}),
    xcdfCanvasOrigin = define_c_proc(hCd, "cdfCanvasOrigin", {P,D,D}),
--  xcdCanvasGetOrigin = define_c_proc(hCd, "cdCanvasGetOrigin", {P,P,P}),
    xcdfCanvasGetOrigin = define_c_proc(hCd, "cdfCanvasGetOrigin", {P,P,P}),
    xcdCanvasTransform = define_c_proc(hCd, "cdCanvasTransform", {P,P}),
    xcdCanvasGetTransform = define_c_func(hCd, "cdCanvasGetTransform", {P},P),
    xcdCanvasTransformMultiply = define_c_proc(hCd, "cdCanvasTransformMultiply", {P,P}),
    xcdCanvasTransformRotate = define_c_proc(hCd, "cdCanvasTransformRotate", {P,D}),
    xcdCanvasTransformScale = define_c_proc(hCd, "cdCanvasTransformScale", {P,D,D}),
    xcdCanvasTransformTranslate = define_c_proc(hCd, "cdCanvasTransformTranslate", {P,D,D}),
--  xcdCanvasTransformPoint = define_c_proc(hCd, "cdCanvasTransformPoint", {P,I,I,P,P}),
    xcdfCanvasTransformPoint = define_c_proc(hCd, "cdfCanvasTransformPoint", {P,D,D,P,P})

global function cdCanvasGetSize(cdCanvas hCdCanvas)
atom pWidth, pHeight, pWidth_mm, pHeight_mm
sequence size
--DEV machine_bits()?
    pWidth = allocate(2*4+2*8)
    pHeight = pWidth+4
    pWidth_mm = pHeight+4
    pHeight_mm = pWidth_mm+8
    c_proc(xcdCanvasGetSize, {hCdCanvas, pWidth, pHeight, pWidth_mm, pHeight_mm})
    size = peek4s({pWidth, 2}) & iup_peek_double({pWidth_mm, 2})
    free(pWidth)
    return size     -- {width,height,width_mm,height_mm}
end function

--global function cdCanvasUpdateYAxis(cdCanvas hCdCanvas, integer y)
--atom pY
--integer newy
--  pY = allocate(4)
--  poke4(pY, y)
--  newy = c_func(xcdCanvasUpdateYAxis, {hCdCanvas, pY})
--  free(pY) --peek4s(pY)==newy
--  return newy
--end function

--global function cdCanvasUpdateYAxis(cdCanvas hCdCanvas, integer y)
--atom pY
--integer newy
--  pY = allocate(machine_word())
--  pokeN(pY, y, machine_word())
--  newy = c_func(xcdCanvasUpdateYAxis, {hCdCanvas, pY})
--  if newy!=peekNS(pY,machine_word(),1) then ?9/0 end if
--  free(pY)
--  return newy
--end function

global function cdCanvasUpdateYAxis(cdCanvas hCdCanvas, atom y)
atom newy, pY
    pY = allocate(8)
    iup_poke_double(pY, y)
    newy = c_func(xcdfCanvasUpdateYAxis, {hCdCanvas, pY})
    -- if this triggers we may need to resurrect the int C function
--atom dbg = iup_peek_double(pY)
    if newy!=iup_peek_double(pY) then ?9/0 end if
    free(pY)
    return newy
end function

--global function cdCanvasInvertYAxis(cdCanvas hCdCanvas, integer y)
--  integer newy c_func(xcdCanvasInvertYAxis, {hCdCanvas, y})
--  return newy
--end function

global function cdCanvasInvertYAxis(cdCanvas hCdCanvas, atom y)
    atom newy = c_func(xcdfCanvasInvertYAxis, {hCdCanvas, y})
    return newy
end function

global function cdCanvasMM2Pixel(cdCanvas hCdCanvas, atom mm_dx, atom mm_dy)
atom pDx, pDy
sequence dx_dy
    pDx = allocate(machine_word()*2)
    pDy = pDx+machine_word()
    c_proc(xcdCanvasMM2Pixel, {hCdCanvas, mm_dx, mm_dy, pDx, pDy})
    dx_dy = peekNS({pDx, 2},machine_word(),1)
    free(pDx)
    return dx_dy
end function

--global function cdfCanvasMM2Pixel(cdCanvas hCdCanvas, atom mm_dx, atom mm_dy)
--atom pDx, pDy
--sequence dx_dy
--  pDx = allocate(16)
--  pDy = pDx+8
--  c_proc(xcdfCanvasMM2Pixel, {hCdCanvas, mm_dx, mm_dy, pDx, pDy})
--  dx_dy = iup_peek_double({pDx, 2})
--  free(pDx)
--  return dx_dy
--end function

--global function cdCanvasPixel2MM(cdCanvas hCdCanvas, atom dx, atom dy)
--atom pmm_dx, pmm_dy
--sequence mmdx_mmdy
--
--  pmm_dx = allocate(16)
--  pmm_dy = pmm_dx+8
--  c_proc(xcdCanvasPixel2MM, {hCdCanvas, dx, dy, pmm_dx, pmm_dy})
--  mmdx_mmdy = iup_peek_double({pmm_dx, 2})
--  free(pmm_dx)
--  return mmdx_mmdy
--end function

global function cdCanvasPixel2MM(cdCanvas hCdCanvas, atom dx, atom dy)
atom pmm_dx, pmm_dy
sequence mmdx_mmdy

    pmm_dx = allocate(16)
    pmm_dy = pmm_dx+8
    c_proc(xcdfCanvasPixel2MM, {hCdCanvas, dx, dy, pmm_dx, pmm_dy})
    mmdx_mmdy = iup_peek_double({pmm_dx, 2})
    free(pmm_dx)
    return mmdx_mmdy
end function

--global procedure cdCanvasOrigin(cdCanvas hCdCanvas, atom x, atom y)
--  c_proc(xcdCanvasOrigin, {hCdCanvas, x, y})
--end procedure

global procedure cdCanvasOrigin(cdCanvas hCdCanvas, atom x, atom y)
    c_proc(xcdfCanvasOrigin, {hCdCanvas, x, y})
end procedure

--global function cdCanvasGetOrigin(cdCanvas hCdCanvas)
--atom pX, pY
--sequence origin
--
--  pX = allocate(8)    -- machine_word()?
--  pY = pX+4
--  c_proc(xcdCanvasGetOrigin, {hCdCanvas, pX, pY})
--  origin = peek4s({pX, 2})
--  free(pX)
--  return origin
--end function

global function cdCanvasGetOrigin(cdCanvas hCdCanvas)
atom pX, pY
sequence origin

    pX = allocate(16)
    pY = pX+8
    c_proc(xcdfCanvasGetOrigin, {hCdCanvas, pX, pY})
    origin = iup_peek_double({pX, 2})
    free(pX)
    return origin
end function

global procedure cdCanvasTransform(cdCanvas hCdCanvas, object matrix)
atom pMatrix = NULL
    if sequence(matrix) then
        if length(matrix)!=6 then ?9/0 end if
        pMatrix = allocate(6*8,1)
        iup_poke_double(pMatrix, matrix)
    end if
    c_proc(xcdCanvasTransform, {hCdCanvas, pMatrix})
end procedure

global function cdCanvasGetTransform(cdCanvas hCdCanvas)
atom pMatrix
    pMatrix = c_func(xcdCanvasGetTransform, {hCdCanvas})
    return iup_peek_double({pMatrix, 6})
end function

global procedure canvas_transform_multiply(cdCanvas hCdCanvas, sequence matrix)
atom pMatrix
    pMatrix = allocate(6*8)
    iup_poke_double(pMatrix, matrix)
    c_proc(xcdCanvasTransformMultiply, {hCdCanvas, pMatrix})
    free(pMatrix)
end procedure

global procedure canvas_transform_rotate(cdCanvas hCdCanvas, atom angle)
    c_proc(xcdCanvasTransformRotate, {hCdCanvas, angle})
end procedure

global procedure cdCanvasTransformScale(cdCanvas hCdCanvas, atom sx, atom sy)
    c_proc(xcdCanvasTransformScale, {hCdCanvas, sx, sy})
end procedure

global procedure cdCanvasTransformTranslate(cdCanvas hCdCanvas, atom dx, atom dy)
    c_proc(xcdCanvasTransformTranslate, {hCdCanvas, dx, dy})
end procedure

--global function cdCanvasTransformPoint(cdCanvas hCdCanvas, atom x, atom y)
--atom pX, pY
--sequence tx_ty
--  pX = allocate(8)    -- machine_word()?
--  pY = pX+4
--  c_proc(xcdCanvasTransformPoint, {hCdCanvas, x, y, pX, pY})
--  tx_ty = peek4s({pX, 2})
--  free(pX)
--  return tx_ty
--end function

global function cdCanvasTransformPoint(cdCanvas hCdCanvas, atom x, atom y)
atom pX, pY
sequence tx_ty
    pX = allocate(16)
    pY = pX+8
    c_proc(xcdfCanvasTransformPoint, {hCdCanvas, x, y, pX, pY})
    tx_ty = iup_peek_double({pX, 2})
    free(pX)
    return tx_ty
end function

------------------------------------------------------------------------------------------
--
--   clipping 
--
------------------------------------------------------------------------------------------
constant
    xcdCanvasClip = define_c_func(hCd, "cdCanvasClip", {P,I},I),
    xcdCanvasClipArea = define_c_proc(hCd, "cdCanvasClipArea", {P,I,I,I,I}),
    xcdfCanvasClipArea = define_c_proc(hCd, "cdfCanvasClipArea", {P,D,D,D,D}),
    xcdCanvasGetClipArea = define_c_func(hCd, "cdCanvasGetClipArea", {P,P,P,P,P},I),
    xcdfCanvasGetClipArea = define_c_func(hCd, "cdfCanvasGetClipArea", {P,P,P,P,P},I)

global function canvas_clip(cdCanvas hCdCanvas, integer mode)
    return c_func(xcdCanvasClip, {hCdCanvas, mode})
end function --cdCanvasClip

global procedure canvas_clip_area(cdCanvas hCdCanvas, atom xmin, atom xmax, atom ymin, atom ymax)
    c_proc(xcdCanvasClipArea, {hCdCanvas, xmin, xmax, ymin, ymax})
end procedure

global procedure f_canvas_clip_area(cdCanvas hCdCanvas, atom xmin, atom xmax, atom ymin, atom ymax)
    c_proc(xcdfCanvasClipArea, {hCdCanvas, xmin, xmax, ymin, ymax})
end procedure

global function canvas_get_clip_area(cdCanvas hCdCanvas)
atom pXmin, pXmax, pYmin, pYmax
sequence area
    pXmin = allocate(16)
    pXmax = pXmin+4
    pYmin = pXmax+4
    pYmax = pYmin+4
    c_proc(xcdCanvasGetClipArea, {hCdCanvas, pXmin, pXmax, pYmin, pYmax})
    area = peek4s({pXmin, 4})
    free(pXmin)
    return area
end function

global function f_canvas_get_clip_area(cdCanvas hCdCanvas)
atom pXmin, pXmax, pYmin, pYmax
sequence area
    pXmin = allocate(32)
    pXmax = pXmin+8
    pYmin = pXmax+8
    pYmax = pYmin+8
    c_proc(xcdfCanvasGetClipArea, {hCdCanvas, pXmin, pXmax, pYmin, pYmax})
    area = iup_peek_double({pXmin, 4})
    free(pXmin)
    return area
end function

------------------------------------------------------------------------------------------
--
--  clipping region 
--
------------------------------------------------------------------------------------------
constant
    xcdCanvasIsPointInRegion = define_c_func(hCd, "cdCanvasIsPointInRegion", {P,I,I},I),
    xcdCanvasOffsetRegion = define_c_proc(hCd, "cdCanvasOffsetRegion", {P,I,I}),
    xcdCanvasGetRegionBox = define_c_proc(hCd, "cdCanvasGetRegionBox", {P,P,P,P,P,P}),
    xcdCanvasRegionCombineMode = define_c_func(hCd, "cdCanvasRegionCombineMode", {P,I},I)

global function canvas_is_point_in_region(cdCanvas hCdCanvas, atom x, atom y)
    return c_func(xcdCanvasIsPointInRegion, {hCdCanvas, x, y})
end function

global procedure canvas_offset_region(cdCanvas hCdCanvas, atom x, atom y)
    c_proc(xcdCanvasOffsetRegion, {hCdCanvas, x, y})
end procedure

global function canvas_get_region_box(cdCanvas hCdCanvas)
atom pXmin, pXmax, pYmin, pYmax
sequence box

    pXmin = allocate(16)
    pXmax = pXmin+4
    pYmin = pXmax+4
    pYmax = pYmin+4
    c_proc(xcdCanvasGetRegionBox, {hCdCanvas, pXmin, pXmax, pYmin, pYmax})
    box = peek4s({pXmin, 4})
    free(pXmin)
    return box
end function

global procedure canvas_region_combine_mode(cdCanvas hCdCanvas, integer mode)
    c_proc(xcdCanvasRegionCombineMode, {hCdCanvas, mode})
end procedure

------------------------------------------------------------------------------------------
--
-- drawing primitives
--
------------------------------------------------------------------------------------------
constant
    xcdCanvasPixel = define_c_proc(hCd, "cdCanvasPixel", {P,I,I}),
    xcdCanvasMark = define_c_proc(hCd, "cdCanvasMark", {P,I,I}),
    xcdCanvasLine = define_c_proc(hCd, "cdCanvasLine", {P,I,I,I,I}),
    xcdCanvasBegin = define_c_proc(hCd, "cdCanvasBegin", {P,I}),
    xcdCanvasVertex = define_c_proc(hCd, "cdCanvasVertex", {P,I,I}),
    xcdCanvasEnd = define_c_proc(hCd, "cdCanvasEnd", {P}),
    xcdCanvasRect = define_c_proc(hCd, "cdCanvasRect", {P,I,I,I,I}),
    xcdCanvasBox = define_c_proc(hCd, "cdCanvasBox", {P,I,I,I,I}),
    xcdCanvasArc = define_c_proc(hCd, "cdCanvasArc", {P,I,I,I,I,D,D}),
    xcdCanvasSector = define_c_proc(hCd, "cdCanvasSector", {P,I,I,I,I,D,D}),
    xcdCanvasChord = define_c_proc(hCd, "cdCanvasChord", {P,I,I,I,I,D,D}),
    xcdCanvasText = define_c_proc(hCd, "cdCanvasText", {P,I,I,P})

global procedure canvas_pixel(cdCanvas hCdCanvas, atom x, atom y)
    c_proc(xcdCanvasPixel, {hCdCanvas, x, y})
end procedure

global procedure canvas_mark(cdCanvas hCdCanvas, atom x, atom y)
    c_proc(xcdCanvasMark, {hCdCanvas, x, y})
end procedure

global procedure cdCanvasLine(cdCanvas hCdCanvas, atom x1, atom y1, atom x2, atom y2)
    c_proc(xcdCanvasLine, {hCdCanvas, x1, y1, x2, y2})
end procedure

global procedure cdCanvasBegin(cdCanvas hCdCanvas, integer mode)
    c_proc(xcdCanvasBegin, {hCdCanvas, mode})
end procedure

global procedure cdCanvasVertex(cdCanvas hCdCanvas, atom x, atom y)
    c_proc(xcdCanvasVertex, {hCdCanvas, x, y})
end procedure

global procedure cdCanvasEnd(cdCanvas hCdCanvas)
    c_proc(xcdCanvasEnd, {hCdCanvas})
end procedure

global procedure cdCanvasRect(cdCanvas hCdCanvas, atom minX, atom minY, atom maxX, atom maxY)
    c_proc(xcdCanvasRect, {hCdCanvas, minX, minY, maxX, maxY})
end procedure

global procedure cdCanvasBox(cdCanvas hCdCanvas, atom minX, atom minY, atom maxX, atom maxY)
    c_proc(xcdCanvasBox, {hCdCanvas, minX, minY, maxX, maxY})
end procedure

global procedure cdCanvasArc(cdCanvas hCdCanvas, atom xc, atom yc, atom w, atom h, atom a1, atom a2)
    c_proc(xcdCanvasArc, {hCdCanvas, xc, yc, w, h, a1, a2})
end procedure

global procedure cdCanvasSector(cdCanvas hCdCanvas, atom xc, atom yc, atom w, atom h, atom a1, atom a2)
    c_proc(xcdCanvasSector, {hCdCanvas, xc, yc, w, h, a1, a2})
end procedure

global procedure canvas_chord(cdCanvas hCdCanvas, atom xc, atom yc, atom w, atom h, atom a1, atom a2)
    c_proc(xcdCanvasChord, {hCdCanvas, xc, yc, w, h, a1, a2})
end procedure

global procedure cdCanvasText(cdCanvas hCdCanvas, atom x, atom y, string text)
    c_proc(xcdCanvasText, {hCdCanvas, x, y, text})
end procedure

-----------------------------------------------------------------------------------------
--
-- primitives with double as arguments instead of integer
--
-----------------------------------------------------------------------------------------
constant
    xcdfCanvasLine = define_c_proc(hCd, "cdfCanvasLine", {P,D,D,D,D}),
    xcdfCanvasVertex = define_c_proc(hCd, "cdfCanvasVertex", {P,D,D}),
    xcdfCanvasRect = define_c_proc(hCd, "cdfCanvasRect", {P,D,D,D,D}),
    xcdfCanvasBox = define_c_proc(hCd, "cdfCanvasBox", {P,D,D,D,D}),
    xcdfCanvasArc = define_c_proc(hCd, "cdfCanvasArc", {P,D,D,D,D,D,D}),
    xcdfCanvasSector = define_c_proc(hCd, "cdfCanvasSector", {P,D,D,D,D,D,D}),
    xcdfCanvasChord = define_c_proc(hCd, "cdfCanvasChord", {P,D,D,D,D,D,D}),
    xcdfCanvasText = define_c_proc(hCd, "cdfCanvasText", {P,D,D,P})

global procedure f_canvas_line(cdCanvas hCdCanvas, atom x1, atom y1, atom x2, atom y2)
    c_proc(xcdfCanvasLine, {hCdCanvas, x1, y1, x2, y2})
end procedure

global procedure f_canvas_vertex(cdCanvas hCdCanvas, atom x1, atom y1)
    c_proc(xcdfCanvasVertex, {hCdCanvas, x1, y1})
end procedure

global procedure f_canvas_rect(cdCanvas hCdCanvas, atom xmin, atom ymin, atom xmax, atom ymax)
    c_proc(xcdfCanvasRect, {hCdCanvas, xmin, ymin, xmax, ymax})
end procedure

global procedure f_canvas_box(cdCanvas hCdCanvas, atom xmin, atom ymin, atom xmax, atom ymax)
    c_proc(xcdfCanvasBox, {hCdCanvas, xmin, ymin, xmax, ymax})
end procedure

global procedure f_canvas_arc(cdCanvas hCdCanvas, atom xc, atom yc, atom w, atom h, atom angle1, atom angle2)
    c_proc(xcdfCanvasArc, {hCdCanvas, xc, yc, w, h, angle1, angle2})
end procedure

global procedure f_canvas_sector(cdCanvas hCdCanvas, atom xc, atom yc, atom w, atom h, atom angle1, atom angle2)
    c_proc(xcdfCanvasSector, {hCdCanvas, xc, yc, w, h, angle1, angle2})
end procedure

global procedure f_canvas_chord(cdCanvas hCdCanvas, atom xc, atom yc, atom w, atom h, atom angle1, atom angle2)
    c_proc(xcdfCanvasChord, {hCdCanvas, xc, yc, w, h, angle1, angle2})
end procedure

global procedure f_canvas_text(cdCanvas hCdCanvas, atom x1, atom y1, sequence text)
atom pText
    pText = allocate_string(text)
    c_proc(xcdfCanvasText, {hCdCanvas, x1, y1, pText})
    free(pText)
end procedure

------------------------------------------------------------------------------------------
--
-- attributes
--
------------------------------------------------------------------------------------------
constant
    xcdCanvasSetForeground   = define_c_proc(hCd, "cdCanvasSetForeground", {P,I}),
    xcdCanvasSetBackground   = define_c_proc(hCd, "cdCanvasSetBackground", {P,I}),
    xcdCanvasForeground      = define_c_func(hCd, "cdCanvasForeground", {P,L}, L),
    xcdCanvasBackground      = define_c_func(hCd, "cdCanvasBackground", {P,L}, L),
    xcdCanvasBackOpacity     = define_c_func(hCd, "cdCanvasBackOpacity", {P,I}, I),
    xcdCanvasWriteMode       = define_c_func(hCd, "cdCanvasWriteMode", {P,I}, I),
    xcdCanvasLineStyle       = define_c_func(hCd, "cdCanvasLineStyle", {P,I}, I),
    xcdCanvasLineStyleDashes = define_c_proc(hCd, "cdCanvasLineStyleDashes", {P,P, I}),
    xcdCanvasLineWidth       = define_c_func(hCd, "cdCanvasLineWidth", {P,I}, I),
    xcdCanvasLineJoin        = define_c_func(hCd, "cdCanvasLineJoin", {P,I}, I),
    xcdCanvasLineCap         = define_c_func(hCd, "cdCanvasLineCap", {P,I}, I),
    xcdCanvasInteriorStyle   = define_c_func(hCd, "cdCanvasInteriorStyle", {P,I}, I),
    xcdCanvasHatch           = define_c_func(hCd, "cdCanvasHatch", {P,I}, I),
    xcdCanvasStipple         = define_c_proc(hCd, "cdCanvasStipple", {P,I,I,P}),
    xcdCanvasGetStipple      = define_c_func(hCd, "cdCanvasGetStipple", {P,P,P}, P),
    xcdCanvasPattern         = define_c_proc(hCd, "cdCanvasPattern", {P,I,I,P}),
    xcdCanvasGetPattern      = define_c_func(hCd, "cdCanvasGetPattern", {P,P,P}, P),
    xcdCanvasFillMode        = define_c_func(hCd, "cdCanvasFillMode", {P,I}, I),
    xcdCanvasFont            = define_c_proc(hCd, "cdCanvasFont", {P,P,I,I}),
    xcdCanvasGetFont         = define_c_proc(hCd, "cdCanvasGetFont", {P,P,P,P}),
    xcdCanvasNativeFont      = define_c_func(hCd, "cdCanvasNativeFont", {P,P}, P),
    xcdCanvasTextAlignment   = define_c_func(hCd, "cdCanvasTextAlignment", {P,I}, I),
    xcdCanvasTextOrientation = define_c_func(hCd, "cdCanvasTextOrientation", {P,D}, D),
    xcdCanvasMarkType        = define_c_func(hCd, "cdCanvasMarkType", {P,I}, I),
    xcdCanvasMarkSize        = define_c_func(hCd, "cdCanvasMarkSize", {P,I}, I)

global procedure cdCanvasSetForeground(cdCanvas hCdCanvas, atom color)
    c_proc(xcdCanvasSetForeground, {hCdCanvas, color})
end procedure

global function cdCanvasGetForeground(cdCanvas hCdCanvas)
    integer color = c_func(xcdCanvasForeground, {hCdCanvas, CD_QUERY})
    return color
end function

global procedure cdCanvasSetBackground(cdCanvas hCdCanvas, atom color)
    c_proc(xcdCanvasSetBackground, {hCdCanvas, color})
end procedure

global function cdCanvasGetBackground(cdCanvas hCdCanvas)
    return c_func(xcdCanvasBackground, {hCdCanvas, CD_QUERY})
end function

global function canvas_back_opacity(cdCanvas hCdCanvas, atom opacity)
    return c_func(xcdCanvasBackOpacity, {hCdCanvas, opacity})
end function

global function canvas_write_mode(cdCanvas hCdCanvas, atom mode)
    return c_func(xcdCanvasWriteMode, {hCdCanvas, mode})
end function

global procedure cdCanvasSetLineWidth(cdCanvas hCdCanvas, integer width)
    if width<1 then ?9/0 end if
    width = c_func(xcdCanvasLineWidth, {hCdCanvas, width})
end procedure

global procedure cdCanvasLineWidth(cdCanvas hCdCanvas, integer width)
    cdCanvasSetLineWidth(hCdCanvas, width)
end procedure

global function cdCanvasGetLineWidth(cdCanvas hCdCanvas)
    return c_func(xcdCanvasLineWidth, {hCdCanvas, CD_QUERY})
end function

global procedure cdCanvasSetLineStyle(cdCanvas hCdCanvas, integer style)
    style = c_func(xcdCanvasLineStyle, {hCdCanvas, style})
end procedure

global procedure cdCanvasLineStyle(cdCanvas hCdCanvas, integer style)
    cdCanvasSetLineStyle(hCdCanvas, style)
end procedure

global function cdCanvasGetLineStyle(cdCanvas hCdCanvas)
    return c_func(xcdCanvasLineStyle, {hCdCanvas, CD_QUERY})
end function

global procedure cdCanvasLineStyleDashes(cdCanvas hCdCanvas, sequence dashes, integer count)
atom pDashes = allocate(4*length(dashes))
    poke4(pDashes, dashes)
    c_proc(xcdCanvasLineStyleDashes, {hCdCanvas, pDashes, count})
    free(pDashes)
end procedure

global function canvas_line_join(cdCanvas hCdCanvas, atom join)
    return c_func(xcdCanvasLineJoin, {hCdCanvas, join})
end function

global function canvas_line_cap(cdCanvas hCdCanvas, atom cap)
    return c_func(xcdCanvasLineCap, {hCdCanvas, cap})
end function

global function canvas_interior_style(cdCanvas hCdCanvas, atom style)
    return c_func(xcdCanvasInteriorStyle, {hCdCanvas, style})
end function

global function canvas_hatch(cdCanvas hCdCanvas, atom style)
    return c_func(xcdCanvasHatch, {hCdCanvas, style})
end function

global procedure canvas_stipple(cdCanvas hCdCanvas, atom width, atom height, sequence stipple)
atom pStipple

    pStipple = allocate(length(stipple))
    poke(pStipple, stipple)
    c_proc(xcdCanvasStipple, {hCdCanvas, width, height, pStipple})
    free(pStipple)
end procedure

global function canvas_get_stipple(cdCanvas hCdCanvas)
atom fnVal, pW, pH
sequence w_h, stipple
    pW = allocate(8)
    pH = pW+4
    fnVal = c_func(xcdCanvasGetStipple, {hCdCanvas, pW, pH})
    w_h = peek4s({pW, 2})
    free(pW)
    stipple = peek({fnVal, w_h[1]*w_h[2]})
    return {{w_h}, stipple}
end function

global procedure canvas_pattern(cdCanvas hCdCanvas, atom width, atom height, sequence pattern)
atom pPattern

    pPattern = allocate(4*length(pattern))
    poke4(pPattern, pattern)
    c_proc(xcdCanvasPattern, {hCdCanvas, width, height, pPattern})
    free(pPattern)
end procedure

global function canvas_get_pattern(cdCanvas hCdCanvas)
atom fnVal, pW, pH
sequence w_h, pattern
    pW = allocate(8)
    pH = pW+4
    fnVal = c_func(xcdCanvasGetPattern, {hCdCanvas, pW, pH})
    w_h = peek4s({pW, 2})
    free(pW)
    pattern = peek4s({fnVal, w_h[1]*w_h[2]})
    return {{w_h}, pattern}
end function

global function canvas_fill_mode(cdCanvas hCdCanvas, atom mode)
    return c_func(xcdCanvasFillMode, {hCdCanvas, mode})
end function

global procedure cdCanvasFont(cdCanvas hCdCanvas, nullable_string font, integer style, integer size)
    c_proc(xcdCanvasFont, {hCdCanvas, font, style, size})
end procedure

global function cdCanvasGetFont(cdCanvas hCdCanvas)
atom pFont, pSize, pStyle
sequence font
    pStyle = allocate(1024)
    pSize = pStyle+4
    pFont = pSize+4
    c_proc(xcdCanvasGetFont, {hCdCanvas, pFont, pStyle, pSize})
    font = {peek_string(pFont)} & peek4s({pStyle, 2})
    free(pStyle)
    return font
end function

--DEV doc: cdCanvasNativeFont/CD_QUERY
global procedure cdCanvasSetNativeFont(cdCanvas hCdCanvas, string font)
    atom pFont = c_func(xcdCanvasNativeFont, {hCdCanvas, font})
end procedure

global function cdCanvasGetNativeFont(cdCanvas hCdCanvas)
    atom pFont = c_func(xcdCanvasNativeFont, {hCdCanvas, CD_QUERY})
    string font = peek_string(pFont)
    return font
end function

global function cdCanvasTextAlignment(cdCanvas hCdCanvas, atom alignment)
    return c_func(xcdCanvasTextAlignment, {hCdCanvas, alignment})
end function

global function canvas_text_orientation(cdCanvas hCdCanvas, atom angle)
    return c_func(xcdCanvasTextOrientation, {hCdCanvas, angle})
end function

global function canvas_mark_type(cdCanvas hCdCanvas, atom mtype)
    return c_func(xcdCanvasMarkType, {hCdCanvas, mtype})
end function

global function canvas_mark_size(cdCanvas hCdCanvas, atom msize)
    return c_func(xcdCanvasMarkSize, {hCdCanvas, msize})
end function

-----------------------------------------------------------------------------------------
--
--  vector text
--
-----------------------------------------------------------------------------------------
constant
    xcdCanvasVectorText = define_c_proc(hCd, "cdCanvasVectorText", {P,I,I,P}),
    xcdCanvasMultiLineVectorText = define_c_proc(hCd, "cdCanvasMultiLineVectorText", {P,I,I,P})

global procedure canvas_vector_text(cdCanvas hCdCanvas, atom x, atom y, sequence text)
atom pText
    pText = allocate_string(text)
    c_proc(xcdCanvasVectorText, {hCdCanvas, x, y, pText})
    free(pText)
end procedure

global procedure cdCanvasMultiLineVectorText(cdCanvas hCdCanvas, atom x, atom y, string text)
    c_proc(xcdCanvasMultiLineVectorText, {hCdCanvas, x, y, text})
end procedure

-----------------------------------------------------------------------------------------
--
-- vector text attributes
--
-----------------------------------------------------------------------------------------
constant
    xcdCanvasVectorFont = define_c_func(hCd, "cdCanvasVectorFont", {P,P},P),
    xcdCanvasVectorTextDirection = define_c_proc(hCd, "cdCanvasVectorTextDirection", {P,I,I,I,I}),
    xcdCanvasVectorTextTransform = define_c_func(hCd, "cdCanvasVectorTextTransform", {P,P},P),
    xcdCanvasVectorTextSize = define_c_proc(hCd, "cdCanvasVectorTextSize", {P,I,I,P}),
    xcdCanvasVectorCharSize = define_c_func(hCd, "cdCanvasVectorCharSize", {P,I},I)

global function cdCanvasVectorFont(cdCanvas hCdCanvas, nullable_string font)
    atom pFont = c_func(xcdCanvasVectorFont, {hCdCanvas, font})
    font = peek_string(pFont)
    return font
end function

global procedure cdCanvasVectorTextDirection(cdCanvas hCdCanvas, integer x1, integer y1, integer x2, integer y2)
    c_proc(xcdCanvasVectorTextDirection, {hCdCanvas, x1, y1, x2, y2})
end procedure

global function canvas_vector_text_transform(cdCanvas hCdCanvas, sequence matrix)
atom fnVal, pMatrix

    pMatrix = allocate(8*6)
    iup_poke_double(pMatrix, matrix)
    fnVal = c_func(xcdCanvasVectorTextTransform, {hCdCanvas, pMatrix})
    matrix = iup_peek_double({fnVal, 6})
    free(pMatrix)
    return matrix
end function

--global function canvas_vector_text_transform(cdCanvas hCdCanvas, object matrix=NULL)
--atom pPrevMatrix, pMatrix = NULL
--
--  if matrix!=NULL then
--      if length(matrix)!=6 then ?9/0 end if
--      pMatrix = allocate(8*6)
--      iup_poke_double(pMatrix, matrix)
--  end if
--  pPrevMatrix = c_func(xcdCanvasVectorTextTransform, {hCdCanvas, pMatrix})
--  matrix = iup_peek_double({pPrevMatrix, 6})
--  if pMatrix!=NULL then
--      free(pMatrix)
--  end if
--  return matrix
--end function

global procedure canvas_vector_text_size(cdCanvas hCdCanvas, atom w, atom h, string text)
    c_proc(xcdCanvasVectorTextSize, {hCdCanvas, w, h, text})
end procedure

global function canvas_vector_char_size(cdCanvas hCdCanvas, atom size)
    return c_func(xcdCanvasVectorCharSize, {hCdCanvas, size})
end function

-----------------------------------------------------------------------------------------
--
--  vector text properties 
--
-----------------------------------------------------------------------------------------
constant
    xcdCanvasGetVectorTextSize = define_c_proc(hCd, "cdCanvasGetVectorTextSize", {P,P,P,P}),
    xcdCanvasGetVectorTextBounds = define_c_proc(hCd, "cdCanvasGetVectorTextBounds", {P,P,I,I,P})

global function cdCanvasGetVectorTextSize(cdCanvas hCdCanvas, sequence text)
atom pX, pY
sequence x_y
    pX = allocate(8)
    pY = pX+4
    c_proc(xcdCanvasGetVectorTextSize, {hCdCanvas, text, pX, pY})
    x_y = peek4s({pX, 2})
    free(pX)
    return x_y
end function

global function cdCanvasGetVectorTextBounds(cdCanvas hCdCanvas, string text, integer px, integer py)
atom pRect
sequence rect
    pRect = allocate(8*4)
    c_proc(xcdCanvasGetVectorTextBounds, {hCdCanvas, text, px, py, pRect})
    rect = peek4s({pRect, 8})
    free(pRect)
    return rect
end function

-----------------------------------------------------------------------------------------
--
-- properties --
--
-----------------------------------------------------------------------------------------
constant
    xcdCanvasGetFontDim = define_c_proc(hCd, "cdCanvasGetFontDim", {P,P,P,P,P}),
    xcdCanvasGetTextSize = define_c_proc(hCd, "cdCanvasGetTextSize", {P,P,P,P}),
--  xcdCanvasGetTextBox = define_c_proc(hCd, "cdCanvasGetTextBox", {P,P,I,I,P,P,P,P}),
    xcdfCanvasGetTextBox = define_c_proc(hCd, "cdfCanvasGetTextBox", {P,D,D,P,P,P,P,P}),
    xcdCanvasGetTextBounds = define_c_proc(hCd, "cdCanvasGetTextBounds", {P,I,I,P,P}),
    xcdCanvasGetColorPlanes = define_c_func(hCd, "cdCanvasGetColorPlanes", {P},I)

global function cdCanvasGetFontDim(cdCanvas hCdCanvas)
atom pWidth, pHeight, pAscent, pDescent
sequence font_metrics
    pWidth = allocate(16)
    pHeight = pWidth+4
    pAscent = pHeight+4
    pDescent = pAscent+4
    c_proc(xcdCanvasGetFontDim, {hCdCanvas, pWidth, pHeight, pAscent, pDescent})
    font_metrics = peek4s({pWidth, 4})
    free(pWidth)
    return font_metrics -- {width, height, ascent, descent}
end function

global function cdCanvasGetTextSize(cdCanvas hCdCanvas, string text)
atom pW, pH
sequence text_size
    pW = allocate(8)
    pH = pW+4
    c_proc(xcdCanvasGetTextSize, {hCdCanvas, text, pW, pH})
    text_size = peek4s({pW, 2})
    free(pW)
    return text_size    -- {width, height}
end function

global function cdCanvasGetTextBox(cdCanvas hCdCanvas, atom x, atom y, string text)
atom pXmin, pXmax, pYmin, pYmax
sequence box
    pXmin = allocate(32)
    pXmax = pXmin+8
    pYmin = pXmax+8
    pYmax = pYmin+8
    c_proc(xcdfCanvasGetTextBox, {hCdCanvas, x, y, text, pXmin, pXmax, pYmin, pYmax})
    box = iup_peek_double({pXmin, 4})
    free(pXmin)
    return box
end function

global function cdCanvasGetTextBounds(cdCanvas hCdCanvas, atom x, atom y, string text)
atom pRect
sequence bounds
    pRect = allocate(32)
    c_proc(xcdCanvasGetTextBounds, {hCdCanvas, x, y, text, pRect})
    bounds = peek4s({pRect, 8})
    free(pRect)
    return bounds
end function

--DEV crashes...
--global function cdCanvasGetTextBounds(cdCanvas hCdCanvas, atom x, atom y, string text)
--atom pRect
--sequence bounds
--  pRect = allocate(64)
--  c_proc(xcdfCanvasGetTextBounds, {hCdCanvas, x, y, text, pRect})
--  bounds = iup_peek_double({pRect, 8})
--  free(pRect)
--  return bounds
--end function

global function cdCanvasGetColorPlanes(cdCanvas hCdCanvas)
    integer p = c_func(xcdCanvasGetColorPlanes, {hCdCanvas})
    return p
end function

-----------------------------------------------------------------------------------------
--
-- color 
--
-----------------------------------------------------------------------------------------
constant
    xcdCanvasPalette = define_c_proc(hCd, "cdCanvasPalette", {P,I,P,I})

global procedure canvas_palette(cdCanvas hCdCanvas, sequence palette, integer mode)
atom pPalette

    pPalette = allocate(4*length(palette))
    poke4(pPalette, palette)
    c_proc(xcdCanvasPalette, {hCdCanvas, length(palette), pPalette, mode})
end procedure

-----------------------------------------------------------------------------------------
--
-- client images 
--
-----------------------------------------------------------------------------------------
constant
    xcdCanvasGetImageRGB = define_c_proc(hCd, "cdCanvasGetImageRGB", {P,P,P,P,I,I,I,I}),
    xcdCanvasPutImageRectRGB = define_c_proc(hCd, "cdCanvasPutImageRectRGB", {P,I,I,P,P,P,I,I,I,I,I,I,I,I}),
    xcdCanvasPutImageRectRGBA = define_c_proc(hCd, "cdCanvasPutImageRectRGBA", {P,I,I,P,P,P,P,I,I,I,I,I,I,I,I}),
    xcdCanvasPutImageRectMap = define_c_proc(hCd, "cdCanvasPutImageRectMap", {P,I,I,P,P,I,I,I,I,I,I,I,I})

global function cdCanvasGetImageRGB(cdCanvas hCdCanvas, atom x, atom y, atom w, atom h)
atom pR, pG, pB
sequence r,g,b

    pR = allocate(3*w*h)
    pG = pR+w*h
    pB = pG+w*h
    c_proc(xcdCanvasGetImageRGB, {hCdCanvas, pR, pG, pB, x, y, w, h})
    r = peek({pR, w*h})
    g = peek({pG, w*h})
    b = peek({pB, w*h})
    free(pR)
    return {r,g,b}
end function

global procedure cdCanvasPutImageRectRGB(cdCanvas hCdCanvas, atom iw, atom ih, sequence rgb, atom x, atom y,
                                         atom w, atom h, atom xmin, atom xmax, atom ymin, atom ymax)
atom pR, pG, pB

    pR = allocate(3*length(rgb[1]))
    pG = pR+length(rgb[1])
    pB = pG+length(rgb[1])
    poke(pR, rgb[1])
    poke(pG, rgb[2])
    poke(pB, rgb[3])
    c_proc(xcdCanvasPutImageRectRGB, {hCdCanvas, iw, ih, pR, pG, pB, x, y, w, h, xmin, xmax, ymin, ymax})
    free(pR)
end procedure

global procedure cdCanvasPutImageRectRGBA(cdCanvas hCdCanvas, atom iw, atom ih, sequence rgba, atom x, atom y,
                                          atom w, atom h, atom xmin, atom xmax, atom ymin, atom ymax)
atom pR, pG, pB, pA

    pR = allocate(4*length(rgba[1]))
    pG = pR+length(rgba[1])
    pB = pG+length(rgba[1])
    pA = pB+length(rgba[1])
    poke(pR, rgba[1])
    poke(pG, rgba[2])
    poke(pB, rgba[3])
    poke(pA, rgba[4])
    c_proc(xcdCanvasPutImageRectRGBA, {hCdCanvas, iw, ih, pR, pG, pB, pA, x, y, w, h, xmin, xmax, ymin, ymax})
    free(pR)
end procedure


global procedure canvas_put_image_rect_map(cdCanvas hCdCanvas, atom iw, atom ih, sequence map, sequence colors,
                                           atom x, atom y, atom w, atom h, 
                                           atom xmin, atom xmax, atom ymin, atom ymax)
atom pIndex, pColors
    pColors = allocate(4*256+length(map))
    pIndex = pColors+4*256
    poke4(pColors, colors)
    poke(pIndex, map)
    c_proc(xcdCanvasPutImageRectMap, {hCdCanvas, iw, ih, pIndex, pColors, x, y, w, h, xmin, xmax, ymin, ymax})
    free(pColors)
end procedure

-----------------------------------------------------------------------------------------
--
-- server images 
--
-----------------------------------------------------------------------------------------
constant
    xcdCanvasCreateImage = define_c_func(hCd, "cdCanvasCreateImage", {P,I,I},P),
    xcdKillImage = define_c_proc(hCd, "cdKillImage", {P}),
    xcdCanvasGetImage = define_c_proc(hCd, "cdCanvasGetImage", {P,P,I,I}),
    xcdCanvasPutImageRect = define_c_proc(hCd, "CdCanvasPutImageRect", {P,P,I,I,I,I,I,I}),
    xcdCanvasScrollArea = define_c_proc(hCd, "CdCanvasScrollArea", {P,I,I,I,I,I,I})

global function canvas_create_image(cdCanvas hCdCanvas, atom w, atom h)
    return c_func(xcdCanvasCreateImage, {hCdCanvas, w, h})
end function

global procedure kill_image(atom hCdImage)
    c_proc(xcdKillImage, {hCdImage})
end procedure

global procedure canvas_get_image(cdCanvas hCdCanvas, atom hCdImage, atom x, atom y)
    c_proc(xcdCanvasGetImage, {hCdCanvas, hCdImage, x, y})
end procedure

global procedure canvas_put_image_rect(cdCanvas hCdCanvas, atom hCdImage, atom x, atom y,
        atom xmin, atom xmax, atom ymin, atom ymax)
    c_proc(xcdCanvasPutImageRect, {hCdCanvas, hCdImage, x, y, xmin, xmax, ymin, ymax})
end procedure

global procedure canvas_scroll_area(cdCanvas hCdCanvas, atom xmin, atom xmax,
        atom ymin, atom ymax, atom dx, atom dy)
    c_proc(xcdCanvasScrollArea, {hCdCanvas, xmin, xmax, ymin, ymax, dx, dy})
end procedure

-----------------------------------------------------------------------------------------
--
-- bitmap 
--
-----------------------------------------------------------------------------------------
constant
    xcdCreateBitmap = define_c_func(hCd, "cdCreateBitmap", {I,I,I},P),
    xcdInitBitmapRGB = define_c_func(hCd, "cdInitBitmap", {I,I,I,P,P,P},P), -- type CD_RGB
    xcdInitBitmapRGBA = define_c_func(hCd, "cdInitBitmap", {I,I,I,P,P,P,P},P), -- type CD_RGBA
--PL unused
--  xcdInitBitmapMAP = define_c_func(hCd, "cdInitBitmap", {I,I,I,P,P},P), -- type CD_MAP
    xcdKillBitmap = define_c_proc(hCd, "cdKillBitmap", {P}),
    xcdBitmapGetData = define_c_func(hCd, "cdBitmapGetData", {P,I},P),
    xcdBitmapSetRect = define_c_proc(hCd, "CdBitmapSetRect", {P,I,I,I,I}),
    xcdCanvasPutBitmap = define_c_proc(hCd, "cdCanvasPutBitmap", {P,P,I,I,I,I}),
    xcdCanvasGetBitmap = define_c_proc(hCd, "cdCanvasGetBitmap", {P,P,I,I}),
    xcdBitmapRGB2Map = define_c_proc(hCd, "cdBitmapRGB2Map", {P,P})

global function canvas_create_bitmap(atom w, atom h, integer btype)
    return c_func(xcdCreateBitmap, {w, h, btype})
end function

global function init_bitmap(atom w, atom h, integer btype, sequence data)
atom hCdBitmap, pR, pG, pB, pA, pIndex, pColors

    hCdBitmap = 0
    if btype=CD_RGB then
        pR = allocate(3*length(data[1]))
        pG = pR+length(data[1])
        pB = pG+length(data[1])
        poke(pR, data[1])
        poke(pG, data[2])
        poke(pB, data[3])
        hCdBitmap = c_func(xcdInitBitmapRGB, {w, h, btype, pR, pG, pB})
        free(pR)
    elsif btype=CD_RGBA then
        pR = allocate(4*length(data[1]))
        pG = pR+length(data[1])
        pB = pG+length(data[1])
        pA = pB+length(data[1])
        poke(pR, data[1])
        poke(pG, data[2])
        poke(pB, data[3])
        poke(pA, data[4])
        hCdBitmap = c_func(xcdInitBitmapRGBA, {w, h, btype, pR, pG, pB, pA})
        free(pR)
    elsif btype=CD_MAP then
        pColors = allocate(4*256+length(data[1]))
        pIndex = pColors+4*256
        poke4(pColors, data[2])
        poke(pIndex, data[1])
        ?9/0    -- DEV: clearly there is a missing c_func(xcdInitBitmapMAP... here
        free(pColors)
    end if
    return hCdBitmap
end function

global procedure kill_bitmap(atom hCdBitmap)
    c_proc(xcdKillBitmap, {hCdBitmap})
end procedure

global function bitmap_get_data(atom hCdBitmap, integer dataptr)
    return c_func(xcdBitmapGetData, {hCdBitmap, dataptr})
end function

global procedure bitmap_set_rect(atom hCdBitmap, atom xmin, atom xmax, atom ymin, atom ymax)
    c_proc(xcdBitmapSetRect, {hCdBitmap, xmin, xmax, ymin, ymax})
end procedure

global procedure canvas_put_bitmap(cdCanvas hCdCanvas, atom hCdBitmap, atom x, atom y, atom w, atom h)
    c_proc(xcdCanvasPutBitmap, {hCdCanvas, hCdBitmap, x, y, w, h})
end procedure

global procedure canvas_get_bitmap(cdCanvas hCdCanvas, atom hCdBitmap, atom x, atom y)
    c_proc(xcdCanvasGetBitmap, {hCdCanvas, hCdBitmap, x, y})
end procedure

global procedure bitmap_rgb_2_map(atom hCdBitmapRGB, atom hCdBitmapMAP)
    c_proc(xcdBitmapRGB2Map, {hCdBitmapRGB, hCdBitmapMAP})
end procedure

-----------------------------------------------------------------------------------------
--
-- color 
--
-----------------------------------------------------------------------------------------
constant
    xcdEncodeColor = define_c_func(hCd, "cdEncodeColor", {UC,UC,UC},L),
    xcdDecodeColor = define_c_proc(hCd, "cdDecodeColor", {L,P,P,P}),
    xcdDecodeAlpha = define_c_func(hCd, "cdDecodeAlpha", {L},UC),
    xcdEncodeAlpha = define_c_func(hCd, "CdEncodeAlpha", {L,UC},L),
    xcdRGB2Map = define_c_proc(hCd, "cdRGB2Map", {I,I,P,P,P,P,I,P})

global function cdEncodeColor(integer red, integer green, integer blue)
    return c_func(xcdEncodeColor, {red, green, blue})
end function

global function decode_color(atom color)
atom pR, pG, pB
sequence rgb_tuple
    pR = allocate(12)
    pG = pR+4
    pB = pG+4
    c_proc(xcdDecodeColor, {color, pR, pG, pB})
    rgb_tuple = peek({pR, 3})
    free(pR)
    return rgb_tuple
end function

global function decode_alpha(atom color)
    return c_func(xcdDecodeAlpha, {color})
end function

global function encode_alpha(atom color, integer alpha)
    return c_func(xcdEncodeAlpha, {color, alpha})
end function

global function alpha(atom color)
    color = and_bits(color, #FF000000)
    color = floor(color/#1000000)
    return 255-color
end function

global function red(atom color)
    color = and_bits(color, #FF0000)
    return color/power(2, 16)
end function

global function green(atom color)
    color = and_bits(color, #FF00)
    return color/256
end function

global function blue(atom color)
    return remainder(color, 256)
end function

global function rgb_2_map(atom w, atom h, sequence rgb, integer pal_size)
atom pR, pG, pB, pI, pC
sequence smap

    if sequence(rgb) then end if    --DEV PL unused
    pR = allocate(4*w*h+1024)
    pG = pR+w*h
    pB = pG+w*h
    pI = pB+w*h
    pC = pI+w*h
    c_proc(xcdRGB2Map, {w, h, pR, pG, pB, pI, pal_size, pC})
    smap = {peek({pI, w*h}), peek4s({pC, pal_size})}
    free(pR)
    return smap
end function

-----------------------------------------------------------------------------------------
--
--  world coordinates
--
-----------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------
--
--  coordinate transformation
--
-----------------------------------------------------------------------------------------
constant
    xwdCanvasWindow = define_c_proc(hCd, "wdCanvasWindow", {P,D,D,D,D}),
    xwdCanvasGetWindow = define_c_proc(hCd, "wdCanvasGetWindow", {P,P,P,P,P}),
    xwdCanvasViewport = define_c_proc(hCd, "wdCanvasViewport", {P,I,I,I,I}),
    xwdCanvasGetViewport = define_c_proc(hCd, "wdCanvasGetViewport", {P,P,P,P,P}),
    xwdCanvasWorld2Canvas = define_c_proc(hCd, "wdCanvasWorld2Canvas", {P,D,D,P,P}),
    xwdCanvasWorld2CanvasSize = define_c_proc(hCd, "wdCanvasWorld2CanvasSize", {P,D,D,P,P}),
    xwdCanvasCanvas2World = define_c_proc(hCd, "wdCanvasCanvas2World", {P,I,I,D,D})

global procedure wdCanvasWindow(cdCanvas hCdCanvas, atom xmin, atom xmax, atom ymin, atom ymax)
    c_proc(xwdCanvasWindow, {hCdCanvas, xmin, xmax, ymin, ymax})
end procedure

global function wdCanvasGetWindow(cdCanvas hCdCanvas)
atom pXmin, pXmax, pYmin, pYmax
sequence wdWindow

--DEV machine_bits?
    pXmin = allocate(4*8)
    pXmax = pXmin+8
    pYmin = pXmax+8
    pYmax = pYmin+8
    c_proc(xwdCanvasGetWindow, {hCdCanvas, pXmin, pXmax, pYmin, pYmax})
    wdWindow = iup_peek_double({pXmin, 4})
    free(pXmin)
    return wdWindow
end function

global procedure wdCanvasViewport(cdCanvas hCdCanvas, atom xmin, atom xmax, atom ymin, atom ymax)
    c_proc(xwdCanvasViewport, {hCdCanvas, xmin, xmax, ymin, ymax})
end procedure

global function wdCanvasGetViewport(cdCanvas hCdCanvas)
atom pXmin, pXmax, pYmin, pYmax
sequence wdViewport

    pXmin = allocate(4*4)
    pXmax = pXmin+4
    pYmin = pXmax+4
    pYmax = pYmin+4
    c_proc(xwdCanvasGetViewport, {hCdCanvas, pXmin, pXmax, pYmin, pYmax})
    wdViewport = peek4s({pXmin, 4})
    free(pXmin)
    return wdViewport
end function

global function wd_canvas_world2_canvas(cdCanvas hCdCanvas, atom xw, atom yw)
atom pX, pY
sequence xy

--DEV machine_bits?
    pX = allocate(2*4)
    pY = pX+4
    c_proc(xwdCanvasWorld2Canvas, {hCdCanvas, xw, yw, pX, pY})
    xy = peek4s({pX, 2})
    free(pX)
    return xy
end function

global function wd_canvas_world2_canvas_size(cdCanvas hCdCanvas, atom ww, atom hw)
atom pW, pH
sequence wh

--DEV machine_bits?
    pW = allocate(8)
    pH = pW+4
    c_proc(xwdCanvasWorld2CanvasSize, {hCdCanvas, ww, hw, pW, pH})
    wh = peek4s({pW, 2})
    free(pW)
    return wh
end function

global function wd_canvas_canvas2_world(cdCanvas hCdCanvas, atom xv, atom yv)
atom pWx, pWy
sequence xy

--DEV machine_bits?
    pWx = allocate(2*8)
    pWy = pWx+8
    c_proc(xwdCanvasCanvas2World, {hCdCanvas, xv, yv, pWx, pWy})
    xy = iup_peek_double({pWx, 2})
    free(pWx)
    return xy
end function

-----------------------------------------------------------------------------------------
--
-- clipping region
--
-----------------------------------------------------------------------------------------
constant
    xwdCanvasClipArea = define_c_proc(hCd, "wdCanvasClipArea", {P,D,D,D,D}),
    xwdCanvasGetClipArea = define_c_func(hCd, "wdCanvasGetclipArea", {P,P,P,P,P},I),
    xwdCanvasIsPointInRegion = define_c_func(hCd, "wdCanvasIsPointInRegion", {P,D,D},I),
    xwdCanvasOffsetRegion = define_c_proc(hCd, "wdCanvasOffsetRegion", {P,D,D}),
    xwdCanvasGetRegionBox = define_c_proc(hCd, "wdCanvasGetRegionBox", {P,P,P,P,P}),
    xwdCanvasHardcopy = define_c_proc(hCd, "wdCanvasHardCopy", {P,P,P,P})

global procedure wd_canvas_clip_area(cdCanvas hCdCanvas, atom xmin, atom xmax, atom ymin, atom ymax)
    c_proc(xwdCanvasClipArea, {hCdCanvas, xmin, xmax, ymin, ymax})
end procedure

global function wd_canvas_get_clip_area(cdCanvas hCdCanvas)
atom pXmin, pXmax, pYmin, pYmax
integer clipping_status
sequence area

    pXmin = allocate(32)
    pXmax = pXmin+8
    pYmin = pXmax+8
    pYmax = pYmin+8
    clipping_status = c_func(xwdCanvasGetClipArea, {hCdCanvas, pXmin, pXmax, pYmin, pYmax})
    area = iup_peek_double({pXmin, 4})
    free(pXmin)
    return clipping_status & area
end function

global function wd_canvas_is_point_in_region(cdCanvas hCdCanvas, atom x, atom y)
    return c_func(xwdCanvasIsPointInRegion, {hCdCanvas, x, y})
end function

global procedure wd_canvas_offset_region(cdCanvas hCdCanvas, atom x, atom y)
    c_proc(xwdCanvasOffsetRegion, {hCdCanvas, x, y})
end procedure

global function wd_canvas_get_region_box(cdCanvas hCdCanvas)
atom pXmin, pXmax, pYmin, pYmax
sequence box

    pXmin = allocate(32)
    pXmax = pXmin+8
    pYmin = pXmax+8
    pYmax = pYmin+8
    c_proc(xwdCanvasGetRegionBox, {hCdCanvas, pXmin, pXmax, pYmin, pYmax})
    box = iup_peek_double({pXmin, 4})
    free(pXmin)
    return box
end function

global procedure wd_canvas_hardcopy(cdCanvas hCdCanvas, atom hCdContext, atom pData, atom cbFct)
    c_proc(xwdCanvasHardcopy, {hCdCanvas, hCdContext, pData, cbFct})
end procedure

-----------------------------------------------------------------------------------------
--
--  world draw primitives
--
-----------------------------------------------------------------------------------------
constant
    xwdCanvasPixel = define_c_proc(hCd, "wdCanvasPixel", {P,D,D,L}),
    xwdCanvasMark = define_c_proc(hCd, "wdCanvasMark", {P,D,D}),
    xwdCanvasLine = define_c_proc(hCd, "wdCanvasLine", {P,D,D,D,D}),
    xwdCanvasVertex = define_c_proc(hCd, "wdCanvasVertex", {P,D,D}),
    xwdCanvasRect = define_c_proc(hCd, "wdCanvasRect", {P,D,D,D,D}),
    xwdCanvasBox = define_c_proc(hCd, "wdCanvasBox", {P,D,D,D,D}),
    xwdCanvasArc = define_c_proc(hCd, "wdCanvasArc", {P,D,D,D,D,D,D}),
    xwdCanvasSector = define_c_proc(hCd, "wdCanvasSector", {P,D,D,D,D,D,D}),
    xwdCanvasChord = define_c_proc(hCd, "wdCanvasChord", {P,D,D,D,D,D,D}),
    xwdCanvasText = define_c_proc(hCd, "wdCanvasText", {P,D,D,P})

global procedure wd_canvas_pixel(cdCanvas hCdCanvas, atom x, atom y)
    c_proc(xwdCanvasPixel, {hCdCanvas, x, y})
end procedure

global procedure wd_canvas_mark(cdCanvas hCdCanvas, atom x, atom y)
    c_proc(xwdCanvasMark, {hCdCanvas, x, y})
end procedure

global procedure wd_canvas_line(cdCanvas hCdCanvas, atom minX, atom minY, atom maxX, atom maxY)
    c_proc(xwdCanvasLine, {hCdCanvas, minX, minY, maxX, maxY})
end procedure

global procedure wd_canvas_vertex(cdCanvas hCdCanvas, atom x, atom y)
    c_proc(xwdCanvasVertex, {hCdCanvas, x, y})
end procedure

global procedure wd_canvas_rect(cdCanvas hCdCanvas, atom minX, atom minY, atom maxX, atom maxY)
    c_proc(xwdCanvasRect, {hCdCanvas, minX, minY, maxX, maxY})
end procedure

global procedure wd_canvas_box(cdCanvas hCdCanvas, atom minX, atom minY, atom maxX, atom maxY)
    c_proc(xwdCanvasBox, {hCdCanvas, minX, minY, maxX, maxY})
end procedure

global procedure wd_canvas_arc(cdCanvas hCdCanvas, atom xc, atom yc, atom w, atom h, atom a1, atom a2)
    c_proc(xwdCanvasArc, {hCdCanvas, xc, yc, w, h, a1, a2})
end procedure

global procedure wd_canvas_sector(cdCanvas hCdCanvas, atom xc, atom yc, atom w, atom h, atom a1, atom a2)
    c_proc(xwdCanvasSector, {hCdCanvas, xc, yc, w, h, a1, a2})
end procedure

global procedure wd_canvas_chord(cdCanvas hCdCanvas, atom xc, atom yc, atom w, atom h, atom a1, atom a2)
    c_proc(xwdCanvasChord, {hCdCanvas, xc, yc, w, h, a1, a2})
end procedure

global procedure wd_canvas_text(cdCanvas hCdCanvas, atom x, atom y, sequence text)
atom pText
    pText = allocate_string(text)
    c_proc(xwdCanvasText, {hCdCanvas, x, y, pText})
    free(pText)
end procedure

-----------------------------------------------------------------------------------------
--
-- world draw images
--
-----------------------------------------------------------------------------------------
constant
    xwdCanvasPutImageRect = define_c_proc(hCd, "wdCanvasImageRect", {P,P,D,D,D,D,D,D}),
    xwdCanvasPutImageRectRGB = define_c_proc(hCd, "wdCanvasImageRectRGB", {P,I,I,P,P,P,D,D,D,D,D,D,D,D,D,D}),
    xwdCanvasPutImageRectRGBA = define_c_proc(hCd, "wdCanvasImageRectRGBA", {P,I,I,P,P,P,P,D,D,D,D,D,D,D,D}),
    xwdCanvasPutImageRectMap = define_c_proc(hCd, "wdCanvasImageRectMap", {P,I,I,P,P,D,D,D,D,D,D,D,D}),
    xwdCanvasPutBitmap = define_c_proc(hCd, "wdCanvasPutBitmap", {P,P,D,D,D,D})

global procedure wd_canvas_put_image_rect(cdCanvas hCdCanvas, atom hCdImage, atom x, atom y,
        atom xmin, atom xmax, atom ymin, atom ymax)
    c_proc(xwdCanvasPutImageRect, {hCdCanvas, hCdImage, x, y, xmin, xmax, ymin, ymax})
end procedure

global procedure wd_canvas_put_image_rect_rgb(cdCanvas hCdCanvas, atom iw, atom ih,
        sequence rgb, atom x, atom y,
        atom w, atom h, atom xmin, atom xmax,
        atom ymin, atom ymax)
atom pR, pG, pB

    pR = allocate(3*length(rgb[1]))
    pG = pR+length(rgb[1])
    pB = pG+length(rgb[1])
    poke(pR, rgb[1])
    poke(pG, rgb[2])
    poke(pB, rgb[3])
    c_proc(xwdCanvasPutImageRectRGB, {hCdCanvas, iw, ih, pR, pG, pB, x, y, w, h, xmin, xmax, ymin, ymax})
    free(pR)
end procedure

global procedure wd_canvas_put_image_rect_rgba(cdCanvas hCdCanvas, atom iw, atom ih,
        sequence rgba, atom x, atom y,
        atom w, atom h, atom xmin, atom xmax,
        atom ymin, atom ymax)

atom pR, pG, pB, pA

    pR = allocate(4*length(rgba[1]))
    pG = pR+length(rgba[1])
    pB = pG+length(rgba[1])
    pA = pB+length(rgba[1])
    poke(pR, rgba[1])
    poke(pG, rgba[2])
    poke(pB, rgba[3])
    poke(pA, rgba[4])
    c_proc(xwdCanvasPutImageRectRGBA, {hCdCanvas, iw, ih, pR, pG, pB, pA, x, y, w, h, xmin, xmax, ymin, ymax})
    free(pR)
end procedure

global procedure wd_canvas_put_image_rect_map(cdCanvas hCdCanvas, atom iw, atom ih,
        sequence map, sequence colors,
        atom x, atom y, atom w, atom h,
        atom xmin, atom xmax,
        atom ymin, atom ymax)

atom pIndex, pColors
    pColors = allocate(4*256+length(map))
    pIndex = pColors+4*256
    poke4(pColors, colors)
    poke(pIndex, map)
    c_proc(xwdCanvasPutImageRectMap, {hCdCanvas, iw, ih, pIndex, pColors, x, y, w, h, xmin, xmax, ymin, ymax})
    free(pColors)
end procedure

global procedure wd_canvas_put_bitmap(cdCanvas hCdCanvas, atom hCdBitmap, atom x, atom y, atom w, atom h)
    c_proc(xwdCanvasPutBitmap, {hCdCanvas, hCdBitmap, x, y, w, h})
end procedure

-----------------------------------------------------------------------------------------
--
-- world draw attributes
--
-----------------------------------------------------------------------------------------
constant
    xwdCanvasLineWidth      = define_c_func(hCd, "wdCanvasLineWidth", {P,D},D),
    xwdCanvasFont           = define_c_proc(hCd, "wdCanvasFont", {P,P,I,D}),
    xwdCanvasGetFont        = define_c_proc(hCd, "wdCanvasGetFont", {P,P,P,P}),
    xwdCanvasGetFontDim     = define_c_proc(hCd, "wdCanvasGetFontDim", {P,P,P,P,P}),
    xwdCanvasMarkSize       = define_c_func(hCd, "wdCanvasMarkSize", {P,D},D),
    xwdCanvasGetTextSize    = define_c_proc(hCd, "wdCanvasGetTextSize", {P,P,P,P}),
    xwdCanvasGetTextBox     = define_c_proc(hCd, "wdCanvasGetTextBox", {P,D,D,P,P,P,P,P}),
    xwdCanvasGetTextBounds  = define_c_proc(hCd, "wdCanvasGetTextbounds", {P,D,D,P,P}),
    xwdCanvasStipple        = define_c_proc(hCd, "wdCanvasStipple", {P,I,I,P,D,D}),
    xwdCanvasPattern        = define_c_proc(hCd, "wdCanvasPattern", {P,I,I,P,D,D})

global function wd_canvas_line_width(cdCanvas hCdCanvas, atom width)
    return c_func(xwdCanvasLineWidth, {hCdCanvas, width})
end function

global procedure wdCanvasFont(cdCanvas hCdCanvas, nullable_string font, integer style, atom size)
    c_proc(xwdCanvasFont, {hCdCanvas, font, style, size})
end procedure

global function wd_canvas_get_font(cdCanvas hCdCanvas)
atom pFont, pSize, pStyle
sequence font
    pStyle = allocate(1024)
    pSize = pStyle+4
    pFont = pSize+8
    c_proc(xwdCanvasGetFont, {hCdCanvas, pFont, pStyle, pSize})
    font = {peek_string(pFont)} & peek4s(pStyle) & iup_peek_double(pSize)
    free(pStyle)
    return font
end function

global function wdCanvasGetFontDim(cdCanvas hCdCanvas)
atom pWidth, pHeight, pAscent, pDescent
sequence font_metrics
    pWidth = allocate(32)
    pHeight = pWidth+8
    pAscent = pHeight+8
    pDescent = pAscent+8
    c_proc(xwdCanvasGetFontDim, {hCdCanvas, pWidth, pHeight, pAscent, pDescent})
    font_metrics = iup_peek_double({pWidth, 4})
    free(pWidth)
    return font_metrics -- {width, height, ascent, descent}
end function

global function wd_canvas_mark_size(cdCanvas hCdCanvas, atom msize)
    return c_func(xwdCanvasMarkSize, {hCdCanvas, msize})
end function

global function wdCanvasGetTextSize(cdCanvas hCdCanvas, sequence text)
atom pW, pH
sequence text_size

    pW = allocate(16)
    pH = pW+8
    c_proc(xwdCanvasGetTextSize, {hCdCanvas, text, pW, pH})
    text_size = iup_peek_double({pW, 2})
    free(pW)
    return text_size    -- {width,height}
end function

global function wdCanvasGetTextBox(cdCanvas hCdCanvas, atom x, atom y, string text)
atom pXmin, pXmax, pYmin, pYmax
sequence box
    pXmin = allocate(32)
    pXmax = pXmin+8
    pYmin = pXmax+8
    pYmax = pYmin+8
    c_proc(xwdCanvasGetTextBox, {hCdCanvas, x, y, text, pXmin, pXmax, pYmin, pYmax})
    box = iup_peek_double({pXmin, 4})
    free(pXmin)
    return box
end function

global function wdCanvasGetTextBounds(cdCanvas hCdCanvas, atom x, atom y, string text)
atom pRect
sequence bounds
    pRect = allocate(64)
    c_proc(xwdCanvasGetTextBounds, {hCdCanvas, x, y, text, pRect})
    bounds = iup_peek_double({pRect, 8})
    free(pRect)
    return bounds
end function

global procedure wd_canvas_stipple(cdCanvas hCdCanvas, atom width, atom height, sequence stipple)
atom pStipple

    pStipple = allocate(length(stipple))
    poke(pStipple, stipple)
    c_proc(xwdCanvasStipple, {hCdCanvas, width, height, pStipple})
    free(pStipple)
end procedure

global procedure wd_canvas_pattern(cdCanvas hCdCanvas, atom width, atom height, sequence pattern, atom width_mm, atom height_mm)
atom pPattern

    pPattern = allocate(4*length(pattern))
    poke4(pPattern, pattern)
    c_proc(xwdCanvasPattern, {hCdCanvas, width, height, pPattern, width_mm, height_mm})
    free(pPattern)
end procedure

-----------------------------------------------------------------------------------------
--
-- world draw vector text
--
-----------------------------------------------------------------------------------------
constant
    xwdCanvasVectorTextDirection = define_c_proc(hCd, "wdCanvasVectorTextDirection", {P,D,D,D,D}),
    xwdCanvasVectorTextSize = define_c_proc(hCd, "wdCanvasVectorTextSize", {P,D,D,P}),
    xwdCanvasGetVectorTextSize = define_c_proc(hCd, "wdCanvasGetVectorTextSize", {P,P,P,P}),
    xwdCanvasVectorCharSize = define_c_func(hCd, "wdCanvasVectorCharSize", {P,D},D),
    xwdCanvasVectorText = define_c_proc(hCd, "wdCanvasVectorText", {P,D,D,P}),
    xwdCanvasMultiLineVectorText = define_c_proc(hCd, "wdCanvasMultiLineVectorText", {P,D,D,P}),
    xwdCanvasGetVectorTextBounds = define_c_proc(hCd, "wdCanvasGetVectorTextBounds", {P,P,D,D,P})

global procedure wd_canvas_vector_text_direction(cdCanvas hCdCanvas, atom x1, atom y1, atom x2, atom y2)
    c_proc(xwdCanvasVectorTextDirection, {hCdCanvas, x1, y1, x2, y2})
end procedure

global procedure wd_canvas_vector_text_size(cdCanvas hCdCanvas, atom w, atom h, sequence text)
atom pText

    pText = allocate_string(text)
    c_proc(xwdCanvasVectorTextSize, {hCdCanvas, w, h, pText})
    free(pText)
end procedure

global function wd_canvas_vector_char_size(cdCanvas hCdCanvas, atom size)
    return c_func(xwdCanvasVectorCharSize, {hCdCanvas, size})
end function

global function wd_canvas_get_vector_text_size(cdCanvas hCdCanvas, sequence text)
atom pText, pX, pY
sequence x_y

    pText = allocate_string(text)
    pX = allocate(16)
    pY = pX+8
    c_proc(xwdCanvasGetVectorTextSize, {hCdCanvas, pText, pX, pY})
    x_y = iup_peek_double({pX, 2})
    free(pText)
    free(pX)
    return x_y
end function

global function wdCanvasGetVectorTextBounds(cdCanvas hCdCanvas, string text, atom px, atom py)
atom pRect
sequence rect
    pRect = allocate(8*8)
    c_proc(xwdCanvasGetVectorTextBounds, {hCdCanvas, text, px, py, pRect})
    rect = iup_peek_double({pRect, 8})
    free(pRect)
    return rect
end function


global procedure wd_canvas_vector_text(cdCanvas hCdCanvas, atom x, atom y, sequence text)
atom pText
    pText = allocate_string(text)
    c_proc(xwdCanvasVectorText, {hCdCanvas, x, y, pText})
    free(pText)
end procedure

global procedure wd_canvas_multi_line_vector_text(cdCanvas hCdCanvas, atom x, atom y, sequence text)
atom pText
    pText = allocate_string(text)
    c_proc(xwdCanvasMultiLineVectorText, {hCdCanvas, x, y, pText})
    free(pText)
end procedure

-- pplot.e:
constant hIupPPlot = iup_open_dll({
                                   "iup_pplot.dll",
                                   "libiup_pplot.so",
                                   "libiup_pplot.dylib"
                                  })

--****
-- === Routines

constant
    xIupPPlotOpen = define_c_proc(hIupPPlot, "IupPPlotOpen", {}),
    xIupPPlot = define_c_func(hIupPPlot, "IupPPlot", {},P),
    xIupPPlotBegin = define_c_proc(hIupPPlot, "IupPPlotBegin", {P,I}),
    xIupPPlotAdd = define_c_proc(hIupPPlot, "IupPPlotAdd", {P,F,F}),
    xIupPPlotAddStr = define_c_proc(hIupPPlot, "IupPPlotAddStr", {P,P,F}),
    xIupPPlotEnd = define_c_proc(hIupPPlot, "IupPPlotEnd", {P}),
    xIupPPlotInsert = define_c_proc(hIupPPlot, "IupPPlotInsert", {P,I,I,F,F}),
    xIupPPlotInsertStr = define_c_proc(hIupPPlot, "IupPPlotInsertStr", {P,I,I,P,F}),
    xIupPPlotTransform = define_c_proc(hIupPPlot, "IupPPlotTransform", {P,F,F,P,P}),
    xIupPPlotPaintTo = define_c_proc(hIupPPlot, "IupPPlotPaintTo", {P,P})

integer did_pplot_open = 0

procedure pplot_open()
    did_pplot_open = 1

    c_proc(xIupPPlotOpen, {})
end procedure

global function IupPPlot(sequence attributes = {}, sequence data = {})
    if not did_pplot_open then
        pplot_open()
    end if

    Ihandle ih = c_func(xIupPPlot, {})

    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if

    return ih
end function

--**
-- Begin the PPlot
--
-- Note:
--   The name is changed from the Iup name due to harmonization
--   with the "end" routine. End is a key word in Euphoria, thus
--   IupPPlotEnd cannot be shortened to pplot:end as desired. Thus
--   both begin and end have been changed to IupPPlotBegin and end_plot.
--

global procedure IupPPlotBegin(Ihandle ih, integer str_xdata)
    c_proc(xIupPPlotBegin, {ih, str_xdata})
end procedure

global procedure IupPPlotAdd(Ihandle ih, atom x, atom y)
    c_proc(xIupPPlotAdd, {ih, x, y})
end procedure

global procedure add_str(Ihandle ih, sequence x, atom y)
atom pX = allocate_string(x)
    c_proc(xIupPPlotAddStr, {ih, pX, y})
    free(pX)
end procedure

global procedure IupPPlotEnd(Ihandle ih)
    c_proc(xIupPPlotEnd, {ih})
end procedure

global procedure insert_plot(Ihandle ih, integer index, integer sample_index, atom x, atom y)
    c_proc(xIupPPlotInsert, {ih, index, sample_index, x, y})
end procedure

global procedure insert_str(Ihandle ih, integer index, integer sample_index, sequence x, atom y)
atom pX = allocate_string(x)
    c_proc(xIupPPlotInsertStr, {ih, index, sample_index, pX, y})
    free(pX)
end procedure

global function transform_plot(Ihandle ih, atom x, atom y)
atom pX = allocate(4), pY = allocate(4)

    c_proc(xIupPPlotTransform, {ih, x, y, pX, pY})
    x = iup_peek_double(pX)
    y = iup_peek_double(pY)

    free(pX)
    free(pY)

    return {x, y}
end function

global procedure paint_to(Ihandle ih, atom cnv)
    c_proc(xIupPPlotPaintTo, {ih, cnv})
end procedure

--****
-- == OpenGL Canvas
--

constant hIupGL = iup_open_dll({"iupgl.dll", 
                                "libiupgl.so",
                                "libiupgl.dylib"})

--****
-- === Routines

constant 
    xIupGLCanvasOpen  = define_c_proc(hIupGL, "IupGLCanvasOpen", {}),
    xIupGLCanvas      = define_c_func(hIupGL, "IupGLCanvas", {P},P),
    xIupGLMakeCurrent = define_c_proc(hIupGL, "IupGLMakeCurrent", {P}),
    xIupGLIsCurrent   = define_c_func(hIupGL, "IupGLIsCurrent", {P},I),
    xIupGLSwapBuffers = define_c_proc(hIupGL, "IupGLSwapBuffers", {P}),
    xIupGLPalette     = define_c_proc(hIupGL, "IupGLPalette", {P,I,F,F,F}),
    xIupGLUseFont     = define_c_proc(hIupGL, "IupGLUseFont", {P,I,I,I}),
    xIupGLWait        = define_c_proc(hIupGL, "IupGLWait", {I})

integer did_gl_open = 0

global procedure IupGLCanvasOpen()
    did_gl_open = 1
    c_proc(xIupGLCanvasOpen, {})
end procedure

global function IupGLCanvas(nullable_string action, string attributes = "", sequence data = {})
    if not did_gl_open then
        IupGLCanvasOpen()
    end if
    Ihandle ih = c_func(xIupGLCanvas, {action})
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global procedure IupGLMakeCurrent(Ihandle ih)
    c_proc(xIupGLMakeCurrent, {ih})
end procedure

global function is_current(Ihandle ih)
    return c_func(xIupGLIsCurrent, { ih })
end function

global procedure IupGLSwapBuffers(Ihandle ih)
    c_proc(xIupGLSwapBuffers, {ih})
end procedure

global procedure palette(Ihandle ih, integer index, atom r, atom g, atom b)
    c_proc(xIupGLPalette, { ih, index, r, g, b })
end procedure

global procedure use_font(Ihandle ih, integer first, integer count, integer list_base)
    c_proc(xIupGLUseFont, { ih, first, count, list_base })
end procedure

global procedure wait(integer gl)
    c_proc(xIupGLWait, { gl })
end procedure

--DEV
--/*
-- imagelib.e:
--****
-- == Image Lib
--

constant hImageLib = iup_open_dll({ 
        "iupimagelib.dll", 
        "libiupimagelib.so", 
        "libiupimagelib.dylib"
    }) 

constant 
--  xIupImageLibOpen = define_c_func(hImageLib, "IupImageLibOpen", {},I)
    xIupImageLibOpen = define_c_proc(hImageLib, "IupImageLibOpen", {})

--****
-- === Routines

global procedure IupImageLibOpen()
    c_proc(xIupImageLibOpen, {})
end procedure
--*/

--ole.e:

--****
-- == OLE Control
--
constant hOle = iup_open_dll({"iupole.dll",
                              "libiupole.so",
                              "libiupole.dylib"})

--****
-- === Routines

constant
        xIupOleControlOpen = define_c_proc(hOle, "IupOleControlOpen", {}),
        xIupOleControl = define_c_func(hOle, "IupOleControl", {P},P)

integer did_ole_open = 0

procedure ole_open()
    did_ole_open = 1
    c_proc(xIupOleControlOpen, {})
end procedure

global function control(sequence prog_id = {}, sequence attributes = {}, sequence data = {})
    if not did_ole_open then
        ole_open()
    end if

    atom pProgId = allocate_string(prog_id)
    Ihandle ih = c_func(xIupOleControl, {pProgId})
    free(pProgId)

    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if

    return ih
end function

--/* erm no dll:
--tuio.e:
--****
-- == Tuio Client
--
constant hTuio = iup_open_dll({ 
        "iuptuio.dll", 
        "libiuptuio.so",
        "libiuptuio.dylib"
    })

--****
-- === Routines

constant
    xIupTuioOpen = define_c_proc(hTuio, "IupTuioOpen", {}),
    xIupTuioClient = define_c_func(hTuio, "IupTuioClient", {I},P)

integer did_tuio_open = 0

procedure tuio_open()
    did_tuio_open = 1

    c_proc(xIupTuioOpen, {})
end procedure

global function client(integer port = 3333, sequence attributes = {}, sequence data = {})
    if not did_tuio_open then
        tuio_open()
    end if

    Ihandle ih = c_func(xIupTuioClient, { port })

    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if

    return ih
end function
--*/

 -- tee\iup.e
-------------
--atom iup = open_dll({"win32\\iup.dll", "libiup.so"})
--if iup=0 then ?9/0 end if
--atom iupimglib = open_dll({"win32\\iupimglib.dll", "libiupimglib.so"})
--if iupimglib=0 then ?9/0 end if

public constant EXIT_SUCCESS = 0
public constant EXIT_FAILURE = 1

ifdef not EU4_1 then
public function sizeof(atom ctype)
    return and_bits(ctype, #FF)
end function
end ifdef

-- allocate an image to memory
public function allocate_image(sequence data, integer cleanup = 0)
atom buff = allocate_data(length(data), cleanup)
    poke(buff, data) return buff
end function


--public include iupkey.e
/* from 32 to 126, all character sets are equal, the key code is the same as the ASCii character code. */
--/*
public constant K_SP = ' '   /* 32 (0x20) */
public constant K_exclam = '!' /* 33 */
public constant K_quotedbl = '\"' /* 34 */
public constant K_numbersign = '#' /* 35 */
public constant K_dollar = '$' /* 36 */
public constant K_percent = '%' /* 37 */
public constant K_ampersand = '&' /* 38 */
public constant K_apostrophe = '\'' /* 39 */
public constant K_parentleft = '(' /* 40 */
public constant K_parentright = ')' /* 41 */
public constant K_asterisk = '*' /* 42 */
public constant K_plus = '+' /* 43 */
public constant K_comma = ',' /* 44 */
public constant K_minus = '-' /* 45 */
public constant K_period = '.' /* 46 */
public constant K_slash = '/' /* 47 */
public constant K_0 = '0' /* 48 (0x30) */
public constant K_1 = '1' /* 49 */
public constant K_2 = '2' /* 50 */
public constant K_3 = '3' /* 51 */
public constant K_4 = '4' /* 52 */
public constant K_5 = '5' /* 53 */
public constant K_6 = '6' /* 54 */
public constant K_7 = '7' /* 55 */
public constant K_8 = '8' /* 56 */
public constant K_9 = '9' /* 57 */
public constant K_colon = ':' /* 58 */
public constant K_semicolon = ';' /* 59 */
public constant K_less = '<' /* 60 */
public constant K_equal = '=' /* 61 */
public constant K_greater = '>' /* 62 */
public constant K_question = '?' /* 63 */
public constant K_at = '@' /* 64 */
public constant K_A = 'A' /* 65 (0x41) */
public constant K_B = 'B' /* 66 */
public constant K_C = 'C' /* 67 */
public constant K_D = 'D' /* 68 */
public constant K_E = 'E' /* 69 */
public constant K_F = 'F' /* 70 */
public constant K_G = 'G' /* 71 */
public constant K_H = 'H' /* 72 */
public constant K_I = 'I' /* 73 */
public constant K_J = 'J' /* 74 */
public constant K_K = 'K' /* 75 */
public constant K_L = 'L' /* 76 */
public constant K_M = 'M' /* 77 */
public constant K_N = 'N' /* 78 */
public constant K_O = 'O' /* 79 */
public constant K_P = 'P' /* 80 */
public constant K_Q = 'Q' /* 81 */
public constant K_R = 'R' /* 82 */
public constant K_S = 'S' /* 83 */
public constant K_T = 'T' /* 84 */
public constant K_U = 'U' /* 85 */
public constant K_V = 'V' /* 86 */
public constant K_W = 'W' /* 87 */
public constant K_X = 'X' /* 88 */
public constant K_Y = 'Y' /* 89 */
public constant K_Z = 'Z' /* 90 */
public constant K_bracketleft = '[' /* 91 */
public constant K_backslash = '\\' /* 92 */
public constant K_bracketright = ']' /* 93 */
public constant K_circum = '^' /* 94 */
public constant K_underscore = '_' /* 95 */
public constant K_grave = '`' /* 96 */
public constant K_a = 'a' /* 97 (0x61) */
public constant K_b = 'b' /* 98 */
public constant K_c = 'c' /* 99 */
public constant K_d = 'd' /* 100 */
public constant K_e = 'e' /* 101 */
public constant K_f = 'f' /* 102 */
public constant K_g = 'g' /* 103 */
public constant K_h = 'h' /* 104 */
public constant K_i = 'i' /* 105 */
public constant K_j = 'j' /* 106 */
public constant K_k = 'k' /* 107 */
public constant K_l = 'l' /* 108 */
public constant K_m = 'm' /* 109 */
public constant K_n = 'n' /* 110 */
public constant K_o = 'o' /* 111 */
public constant K_p = 'p' /* 112 */
public constant K_q = 'q' /* 113 */
public constant K_r = 'r' /* 114 */
public constant K_s = 's' /* 115 */
public constant K_t = 't' /* 116 */
public constant K_u = 'u' /* 117 */
public constant K_v = 'v' /* 118 */
public constant K_w = 'w' /* 119 */
public constant K_x = 'x' /* 120 */
public constant K_y = 'y' /* 121 */
public constant K_z = 'z' /* 122 */
public constant K_braceleft = '{' /* 123 */
public constant K_bar = '|' /* 124 */
public constant K_braceright = '}' /* 125 */
public constant K_tilde = '~' /* 126 (0x7E) */
--*/
/* Printable ASCii keys */
/* also define the escape sequences that have keys associated */
--/*
public constant K_BS = 8 /* 8 */
public constant K_TAB = '\t' /* 9 */
public constant K_LF = '\n' /* 10 (0x0A) not a real key, is a combination of CR with a modifier, just to document */
public constant K_CR = '\r' /* 13 (0x0D) */
/* backward compatible definitions */
public constant K_quoteleft = K_grave
public constant K_quoteright = K_apostrophe
/* IUP Extended Key Codes, range start at 128      */
public constant K_PAUSE = 0xFF13
public constant K_ESC = 0xFF1B
public constant K_HOME = 0xFF50
public constant K_LEFT = 0xFF51
public constant K_UP = 0xFF52
public constant K_RIGHT = 0xFF53
public constant K_DOWN = 0xFF54
public constant K_PGUP = 0xFF55
public constant K_PGDN = 0xFF56
public constant K_END = 0xFF57
public constant K_MIDDLE = 0xFF0B
public constant K_Print = 0xFF61
public constant K_INS = 0xFF63
public constant K_Menu = 0xFF67
public constant K_DEL = 0xFFFF
public constant K_F1 = 0xFFBE
public constant K_F2 = 0xFFBF
public constant K_F3 = 0xFFC0
public constant K_F4 = 0xFFC1
public constant K_F5 = 0xFFC2
public constant K_F6 = 0xFFC3
public constant K_F7 = 0xFFC4
public constant K_F8 = 0xFFC5
public constant K_F9 = 0xFFC6
public constant K_F10 = 0xFFC7
public constant K_F11 = 0xFFC8
public constant K_F12 = 0xFFC9
/* no Shift/Ctrl/Alt */
public constant K_LSHIFT = 0xFFE1
public constant K_RSHIFT = 0xFFE2
public constant K_LCTRL = 0xFFE3
public constant K_RCTRL = 0xFFE4
public constant K_LALT = 0xFFE9
public constant K_RALT = 0xFFEA
public constant K_NUM = 0xFF7F
public constant K_SCROLL = 0xFF14
public constant K_CAPS = 0xFFE5
/* Also, these are the same as the Latin-1 definition */
public constant K_ccedilla = 0x00E7
public constant K_Ccedilla = 0x00C7
public constant K_acute = 0x00B4 /* no Shift/Ctrl/Alt */
public constant K_diaeresis = 0x00A8
/******************************************************/
/* Modifiers use last 4 bits. Since IUP 3.9           */
/* These modifiers definitions are specific to IUP    */
/******************************************************/
public function iup_isShiftXkey( atom _c )
        return and_bits( _c, 0x10000000 )
end function

public function iup_isCtrlXkey( atom _c )
        return and_bits( _c, 0x20000000 )
end function

public function iup_isAltXkey( atom _c )
        return and_bits( _c, 0x40000000 )
end function

public function iup_isSysXkey( atom _c )
        return and_bits( _c, 0x80000000 )
end function

public function iup_XkeyBase( atom _c )
        return and_bits( _c, 0x0FFFFFFF )
end function

public function iup_XkeyShift( atom _c )
        return or_bits( _c, 0x10000000 )
end function

public function iup_XkeyCtrl( atom _c )
        return or_bits( _c, 0x20000000 )
end function

public function iup_XkeyAlt( atom _c )
        return or_bits( _c, 0x40000000 )
end function

public function iup_XkeySys( atom _c )
        return or_bits( _c, 0x80000000 )
end function

--*/
--/*
public constant K_sHOME = iup_XkeyShift(K_HOME )
public constant K_sUP = iup_XkeyShift(K_UP )
public constant K_sPGUP = iup_XkeyShift(K_PGUP )
public constant K_sLEFT = iup_XkeyShift(K_LEFT )
public constant K_sMIDDLE = iup_XkeyShift(K_MIDDLE )
public constant K_sRIGHT = iup_XkeyShift(K_RIGHT )
public constant K_sEND = iup_XkeyShift(K_END )
public constant K_sDOWN = iup_XkeyShift(K_DOWN )
public constant K_sPGDN = iup_XkeyShift(K_PGDN )
public constant K_sINS = iup_XkeyShift(K_INS )
public constant K_sDEL = iup_XkeyShift(K_DEL )
public constant K_sSP = iup_XkeyShift(K_SP )
public constant K_sTAB = iup_XkeyShift(K_TAB )
public constant K_sCR = iup_XkeyShift(K_CR )
public constant K_sBS = iup_XkeyShift(K_BS )
public constant K_sPAUSE = iup_XkeyShift(K_PAUSE )
public constant K_sESC = iup_XkeyShift(K_ESC )
public constant K_sF1 = iup_XkeyShift(K_F1 )
public constant K_sF2 = iup_XkeyShift(K_F2 )
public constant K_sF3 = iup_XkeyShift(K_F3 )
public constant K_sF4 = iup_XkeyShift(K_F4 )
public constant K_sF5 = iup_XkeyShift(K_F5 )
public constant K_sF6 = iup_XkeyShift(K_F6 )
public constant K_sF7 = iup_XkeyShift(K_F7 )
public constant K_sF8 = iup_XkeyShift(K_F8 )
public constant K_sF9 = iup_XkeyShift(K_F9 )
public constant K_sF10 = iup_XkeyShift(K_F10 )
public constant K_sF11 = iup_XkeyShift(K_F11 )
public constant K_sF12 = iup_XkeyShift(K_F12 )
public constant K_sPrint = iup_XkeyShift(K_Print )
public constant K_sMenu = iup_XkeyShift(K_Menu )
public constant K_cHOME = iup_XkeyCtrl(K_HOME )
public constant K_cUP = iup_XkeyCtrl(K_UP )
public constant K_cPGUP = iup_XkeyCtrl(K_PGUP )
public constant K_cLEFT = iup_XkeyCtrl(K_LEFT )
public constant K_cMIDDLE = iup_XkeyCtrl(K_MIDDLE )
public constant K_cRIGHT = iup_XkeyCtrl(K_RIGHT )
public constant K_cEND = iup_XkeyCtrl(K_END )
public constant K_cDOWN = iup_XkeyCtrl(K_DOWN )
public constant K_cPGDN = iup_XkeyCtrl(K_PGDN )
public constant K_cINS = iup_XkeyCtrl(K_INS )
public constant K_cDEL = iup_XkeyCtrl(K_DEL )
public constant K_cSP = iup_XkeyCtrl(K_SP )
public constant K_cTAB = iup_XkeyCtrl(K_TAB )
public constant K_cCR = iup_XkeyCtrl(K_CR )
public constant K_cBS = iup_XkeyCtrl(K_BS )
public constant K_cPAUSE = iup_XkeyCtrl(K_PAUSE )
public constant K_cESC = iup_XkeyCtrl(K_ESC )
public constant K_cCcedilla = iup_XkeyCtrl(K_Ccedilla)
public constant K_cF1 = iup_XkeyCtrl(K_F1 )
public constant K_cF2 = iup_XkeyCtrl(K_F2 )
public constant K_cF3 = iup_XkeyCtrl(K_F3 )
public constant K_cF4 = iup_XkeyCtrl(K_F4 )
public constant K_cF5 = iup_XkeyCtrl(K_F5 )
public constant K_cF6 = iup_XkeyCtrl(K_F6 )
public constant K_cF7 = iup_XkeyCtrl(K_F7 )
public constant K_cF8 = iup_XkeyCtrl(K_F8 )
public constant K_cF9 = iup_XkeyCtrl(K_F9 )
public constant K_cF10 = iup_XkeyCtrl(K_F10 )
public constant K_cF11 = iup_XkeyCtrl(K_F11 )
public constant K_cF12 = iup_XkeyCtrl(K_F12 )
public constant K_cPrint = iup_XkeyCtrl(K_Print )
public constant K_cMenu = iup_XkeyCtrl(K_Menu )
public constant K_mHOME = iup_XkeyAlt(K_HOME )
public constant K_mUP = iup_XkeyAlt(K_UP )
public constant K_mPGUP = iup_XkeyAlt(K_PGUP )
public constant K_mLEFT = iup_XkeyAlt(K_LEFT )
public constant K_mMIDDLE = iup_XkeyAlt(K_MIDDLE )
public constant K_mRIGHT = iup_XkeyAlt(K_RIGHT )
public constant K_mEND = iup_XkeyAlt(K_END )
public constant K_mDOWN = iup_XkeyAlt(K_DOWN )
public constant K_mPGDN = iup_XkeyAlt(K_PGDN )
public constant K_mINS = iup_XkeyAlt(K_INS )
public constant K_mDEL = iup_XkeyAlt(K_DEL )
public constant K_mSP = iup_XkeyAlt(K_SP )
public constant K_mTAB = iup_XkeyAlt(K_TAB )
public constant K_mCR = iup_XkeyAlt(K_CR )
public constant K_mBS = iup_XkeyAlt(K_BS )
public constant K_mPAUSE = iup_XkeyAlt(K_PAUSE )
public constant K_mESC = iup_XkeyAlt(K_ESC )
public constant K_mCcedilla = iup_XkeyAlt(K_Ccedilla)
public constant K_mF1 = iup_XkeyAlt(K_F1 )
public constant K_mF2 = iup_XkeyAlt(K_F2 )
public constant K_mF3 = iup_XkeyAlt(K_F3 )
public constant K_mF4 = iup_XkeyAlt(K_F4 )
public constant K_mF5 = iup_XkeyAlt(K_F5 )
public constant K_mF6 = iup_XkeyAlt(K_F6 )
public constant K_mF7 = iup_XkeyAlt(K_F7 )
public constant K_mF8 = iup_XkeyAlt(K_F8 )
public constant K_mF9 = iup_XkeyAlt(K_F9 )
public constant K_mF10 = iup_XkeyAlt(K_F10 )
public constant K_mF11 = iup_XkeyAlt(K_F11 )
public constant K_mF12 = iup_XkeyAlt(K_F12 )
public constant K_mPrint = iup_XkeyAlt(K_Print )
public constant K_mMenu = iup_XkeyAlt(K_Menu )
public constant K_yHOME = iup_XkeySys(K_HOME )
public constant K_yUP = iup_XkeySys(K_UP )
public constant K_yPGUP = iup_XkeySys(K_PGUP )
public constant K_yLEFT = iup_XkeySys(K_LEFT )
public constant K_yMIDDLE = iup_XkeySys(K_MIDDLE )
public constant K_yRIGHT = iup_XkeySys(K_RIGHT )
public constant K_yEND = iup_XkeySys(K_END )
public constant K_yDOWN = iup_XkeySys(K_DOWN )
public constant K_yPGDN = iup_XkeySys(K_PGDN )
public constant K_yINS = iup_XkeySys(K_INS )
public constant K_yDEL = iup_XkeySys(K_DEL )
public constant K_ySP = iup_XkeySys(K_SP )
public constant K_yTAB = iup_XkeySys(K_TAB )
public constant K_yCR = iup_XkeySys(K_CR )
public constant K_yBS = iup_XkeySys(K_BS )
public constant K_yPAUSE = iup_XkeySys(K_PAUSE )
public constant K_yESC = iup_XkeySys(K_ESC )
public constant K_yCcedilla = iup_XkeySys(K_Ccedilla)
public constant K_yF1 = iup_XkeySys(K_F1 )
public constant K_yF2 = iup_XkeySys(K_F2 )
public constant K_yF3 = iup_XkeySys(K_F3 )
public constant K_yF4 = iup_XkeySys(K_F4 )
public constant K_yF5 = iup_XkeySys(K_F5 )
public constant K_yF6 = iup_XkeySys(K_F6 )
public constant K_yF7 = iup_XkeySys(K_F7 )
public constant K_yF8 = iup_XkeySys(K_F8 )
public constant K_yF9 = iup_XkeySys(K_F9 )
public constant K_yF10 = iup_XkeySys(K_F10 )
public constant K_yF11 = iup_XkeySys(K_F11 )
public constant K_yF12 = iup_XkeySys(K_F12 )
public constant K_yPrint = iup_XkeySys(K_Print )
public constant K_yMenu = iup_XkeySys(K_Menu )
public constant K_sPlus = iup_XkeyShift(K_plus )
public constant K_sComma = iup_XkeyShift(K_comma )
public constant K_sMinus = iup_XkeyShift(K_minus )
public constant K_sPeriod = iup_XkeyShift(K_period )
public constant K_sSlash = iup_XkeyShift(K_slash )
public constant K_sAsterisk = iup_XkeyShift(K_asterisk)
public constant K_cA = iup_XkeyCtrl(K_A)
public constant K_cB = iup_XkeyCtrl(K_B)
public constant K_cC = iup_XkeyCtrl(K_C)
public constant K_cD = iup_XkeyCtrl(K_D)
public constant K_cE = iup_XkeyCtrl(K_E)
public constant K_cF = iup_XkeyCtrl(K_F)
public constant K_cG = iup_XkeyCtrl(K_G)
public constant K_cH = iup_XkeyCtrl(K_H)
public constant K_cI = iup_XkeyCtrl(K_I)
public constant K_cJ = iup_XkeyCtrl(K_J)
public constant K_cK = iup_XkeyCtrl(K_K)
public constant K_cL = iup_XkeyCtrl(K_L)
public constant K_cM = iup_XkeyCtrl(K_M)
public constant K_cN = iup_XkeyCtrl(K_N)
public constant K_cO = iup_XkeyCtrl(K_O)
public constant K_cP = iup_XkeyCtrl(K_P)
public constant K_cQ = iup_XkeyCtrl(K_Q)
public constant K_cR = iup_XkeyCtrl(K_R)
public constant K_cS = iup_XkeyCtrl(K_S)
public constant K_cT = iup_XkeyCtrl(K_T)
public constant K_cU = iup_XkeyCtrl(K_U)
public constant K_cV = iup_XkeyCtrl(K_V)
public constant K_cW = iup_XkeyCtrl(K_W)
public constant K_cX = iup_XkeyCtrl(K_X)
public constant K_cY = iup_XkeyCtrl(K_Y)
public constant K_cZ = iup_XkeyCtrl(K_Z)
public constant K_c1 = iup_XkeyCtrl(K_1)
public constant K_c2 = iup_XkeyCtrl(K_2)
public constant K_c3 = iup_XkeyCtrl(K_3)
public constant K_c4 = iup_XkeyCtrl(K_4)
public constant K_c5 = iup_XkeyCtrl(K_5)
public constant K_c6 = iup_XkeyCtrl(K_6)
public constant K_c7 = iup_XkeyCtrl(K_7)
public constant K_c8 = iup_XkeyCtrl(K_8)
public constant K_c9 = iup_XkeyCtrl(K_9)
public constant K_c0 = iup_XkeyCtrl(K_0)
public constant K_cPlus = iup_XkeyCtrl(K_plus )
public constant K_cComma = iup_XkeyCtrl(K_comma )
public constant K_cMinus = iup_XkeyCtrl(K_minus )
public constant K_cPeriod = iup_XkeyCtrl(K_period )
public constant K_cSlash = iup_XkeyCtrl(K_slash )
public constant K_cSemicolon = iup_XkeyCtrl(K_semicolon )
public constant K_cEqual = iup_XkeyCtrl(K_equal )
public constant K_cBracketleft = iup_XkeyCtrl(K_bracketleft )
public constant K_cBracketright = iup_XkeyCtrl(K_bracketright)
public constant K_cBackslash = iup_XkeyCtrl(K_backslash )
public constant K_cAsterisk = iup_XkeyCtrl(K_asterisk )
public constant K_mA = iup_XkeyAlt(K_A)
public constant K_mB = iup_XkeyAlt(K_B)
public constant K_mC = iup_XkeyAlt(K_C)
public constant K_mD = iup_XkeyAlt(K_D)
public constant K_mE = iup_XkeyAlt(K_E)
public constant K_mF = iup_XkeyAlt(K_F)
public constant K_mG = iup_XkeyAlt(K_G)
public constant K_mH = iup_XkeyAlt(K_H)
public constant K_mI = iup_XkeyAlt(K_I)
public constant K_mJ = iup_XkeyAlt(K_J)
public constant K_mK = iup_XkeyAlt(K_K)
public constant K_mL = iup_XkeyAlt(K_L)
public constant K_mM = iup_XkeyAlt(K_M)
public constant K_mN = iup_XkeyAlt(K_N)
public constant K_mO = iup_XkeyAlt(K_O)
public constant K_mP = iup_XkeyAlt(K_P)
public constant K_mQ = iup_XkeyAlt(K_Q)
public constant K_mR = iup_XkeyAlt(K_R)
public constant K_mS = iup_XkeyAlt(K_S)
public constant K_mT = iup_XkeyAlt(K_T)
public constant K_mU = iup_XkeyAlt(K_U)
public constant K_mV = iup_XkeyAlt(K_V)
public constant K_mW = iup_XkeyAlt(K_W)
public constant K_mX = iup_XkeyAlt(K_X)
public constant K_mY = iup_XkeyAlt(K_Y)
public constant K_mZ = iup_XkeyAlt(K_Z)
public constant K_m1 = iup_XkeyAlt(K_1)
public constant K_m2 = iup_XkeyAlt(K_2)
public constant K_m3 = iup_XkeyAlt(K_3)
public constant K_m4 = iup_XkeyAlt(K_4)
public constant K_m5 = iup_XkeyAlt(K_5)
public constant K_m6 = iup_XkeyAlt(K_6)
public constant K_m7 = iup_XkeyAlt(K_7)
public constant K_m8 = iup_XkeyAlt(K_8)
public constant K_m9 = iup_XkeyAlt(K_9)
public constant K_m0 = iup_XkeyAlt(K_0)
public constant K_mPlus = iup_XkeyAlt(K_plus )
public constant K_mComma = iup_XkeyAlt(K_comma )
public constant K_mMinus = iup_XkeyAlt(K_minus )
public constant K_mPeriod = iup_XkeyAlt(K_period )
public constant K_mSlash = iup_XkeyAlt(K_slash )
public constant K_mSemicolon = iup_XkeyAlt(K_semicolon )
public constant K_mEqual = iup_XkeyAlt(K_equal )
public constant K_mBracketleft = iup_XkeyAlt(K_bracketleft )
public constant K_mBracketright = iup_XkeyAlt(K_bracketright)
public constant K_mBackslash = iup_XkeyAlt(K_backslash )
public constant K_mAsterisk = iup_XkeyAlt(K_asterisk )
public constant K_yA = iup_XkeySys(K_A)
public constant K_yB = iup_XkeySys(K_B)
public constant K_yC = iup_XkeySys(K_C)
public constant K_yD = iup_XkeySys(K_D)
public constant K_yE = iup_XkeySys(K_E)
public constant K_yF = iup_XkeySys(K_F)
public constant K_yG = iup_XkeySys(K_G)
public constant K_yH = iup_XkeySys(K_H)
public constant K_yI = iup_XkeySys(K_I)
public constant K_yJ = iup_XkeySys(K_J)
public constant K_yK = iup_XkeySys(K_K)
public constant K_yL = iup_XkeySys(K_L)
public constant K_yM = iup_XkeySys(K_M)
public constant K_yN = iup_XkeySys(K_N)
public constant K_yO = iup_XkeySys(K_O)
public constant K_yP = iup_XkeySys(K_P)
public constant K_yQ = iup_XkeySys(K_Q)
public constant K_yR = iup_XkeySys(K_R)
public constant K_yS = iup_XkeySys(K_S)
public constant K_yT = iup_XkeySys(K_T)
public constant K_yU = iup_XkeySys(K_U)
public constant K_yV = iup_XkeySys(K_V)
public constant K_yW = iup_XkeySys(K_W)
public constant K_yX = iup_XkeySys(K_X)
public constant K_yY = iup_XkeySys(K_Y)
public constant K_yZ = iup_XkeySys(K_Z)
public constant K_y1 = iup_XkeySys(K_1)
public constant K_y2 = iup_XkeySys(K_2)
public constant K_y3 = iup_XkeySys(K_3)
public constant K_y4 = iup_XkeySys(K_4)
public constant K_y5 = iup_XkeySys(K_5)
public constant K_y6 = iup_XkeySys(K_6)
public constant K_y7 = iup_XkeySys(K_7)
public constant K_y8 = iup_XkeySys(K_8)
public constant K_y9 = iup_XkeySys(K_9)
public constant K_y0 = iup_XkeySys(K_0)
public constant K_yPlus = iup_XkeySys(K_plus )
public constant K_yComma = iup_XkeySys(K_comma )
public constant K_yMinus = iup_XkeySys(K_minus )
public constant K_yPeriod = iup_XkeySys(K_period )
public constant K_ySlash = iup_XkeySys(K_slash )
public constant K_ySemicolon = iup_XkeySys(K_semicolon )
public constant K_yEqual = iup_XkeySys(K_equal )
public constant K_yBracketleft = iup_XkeySys(K_bracketleft )
public constant K_yBracketright = iup_XkeySys(K_bracketright)
public constant K_yBackslash = iup_XkeySys(K_backslash )
public constant K_yAsterisk = iup_XkeySys(K_asterisk )
--*/

--public include iupdef.e
/* Deprecated definitions */
/* Avoid using these definitions. Use the strings instead. */
/* Define __IUPDEF_H to avoid the inclusion of this header */
--/*
public constant IUP_RUN = "RUN"
public constant IUP_ENGLISH = "ENGLISH"
public constant IUP_PORTUGUESE = "PORTUGUESE"
public constant IUP_SBH = "SBH"
public constant IUP_SBV = "SBV"
/************************************************************************/
/*                            Callbacks                                 */
/************************************************************************/
public constant IUP_DEFAULT_ACTION = "DEFAULT_ACTION"
public constant IUP_IDLE_ACTION = "IDLE_ACTION"
public constant IUP_ACTION = "ACTION"
public constant IUP_GETFOCUS_CB = "GETFOCUS_CB"
public constant IUP_KILLFOCUS_CB = "KILLFOCUS_CB"
public constant IUP_K_ANY = "K_ANY"
public constant IUP_KEYPRESS_CB = "KEYPRESS_CB"
public constant IUP_HELP_CB = "HELP_CB"
public constant IUP_SCROLL_CB = "SCROLL_CB"
public constant IUP_RESIZE_CB = "RESIZE_CB"
public constant IUP_MOTION_CB = "MOTION_CB"
public constant IUP_BUTTON_CB = "BUTTON_CB"
public constant IUP_ENTERWINDOW_CB = "ENTERWINDOW_CB"
public constant IUP_LEAVEWINDOW_CB = "LEAVEWINDOW_CB"
public constant IUP_WHEEL_CB = "WHEEL_CB"
public constant IUP_MASK_CB = "MASK_CB"
public constant IUP_OPEN_CB = "OPEN_CB"
public constant IUP_HIGHLIGHT_CB = "HIGHLIGHT_CB"
public constant IUP_MENUCLOSE_CB = "MENUCLOSE_CB"
public constant IUP_MAP_CB = "MAP_CB"
public constant IUP_CLOSE_CB = "CLOSE_CB"
public constant IUP_SHOW_CB = "SHOW_CB"
public constant IUP_DROPFILES_CB = "DROPFILES_CB"
public constant IUP_WOM_CB = "WOM_CB"
/************************************************************************/
/*                            Attributes                                */
/************************************************************************/
public constant IUP_DIRECTION = "DIRECTION"
public constant IUP_ACTIVE = "ACTIVE"
public constant IUP_BGCOLOR = "BGCOLOR"
public constant IUP_FRAMECOLOR = "FRAMECOLOR"
public constant IUP_FGCOLOR = "FGCOLOR"
public constant IUP_COLOR = "COLOR"
public constant IUP_WID = "WID"
public constant IUP_SIZE = "SIZE"
public constant IUP_RASTERSIZE = "RASTERSIZE"
public constant IUP_TITLE = "TITLE"
public constant IUP_VALUE = "VALUE"
public constant IUP_VISIBLE = "VISIBLE"
public constant IUP_FONT = "FONT"
public constant IUP_TIP = "TIP"
public constant IUP_EXPAND = "EXPAND"
public constant IUP_SEPARATOR = "SEPARATOR"
public constant IUP_HOTSPOT = "HOTSPOT"
public constant IUP_HEIGHT = "HEIGHT"
public constant IUP_WIDTH = "WIDTH"
public constant IUP_KEY = "KEY"
public constant IUP_MULTIPLE = "MULTIPLE"
public constant IUP_DROPDOWN = "DROPDOWN"
public constant IUP_VISIBLE_ITEMS = "VISIBLE_ITEMS"
public constant IUP_MARGIN = "MARGIN"
public constant IUP_GAP = "GAP"
public constant IUP_ALIGNMENT = "ALIGNMENT"
public constant IUP_IMAGE = "IMAGE"
public constant IUP_IMINACTIVE = "IMINACTIVE"
public constant IUP_IMPRESS = "IMPRESS"
public constant IUP_WIN_SAVEBITS = "WIN_SAVEBITS"
public constant IUP_NC = "NC"
public constant IUP_MASK = "MASK"
public constant IUP_APPEND = "APPEND"
public constant IUP_BORDER = "BORDER"
public constant IUP_CARET = "CARET"
public constant IUP_SELECTION = "SELECTION"
public constant IUP_SELECTEDTEXT = "SELECTEDTEXT"
public constant IUP_INSERT = "INSERT"
public constant IUP_CONID = "CONID"
public constant IUP_CURSOR = "CURSOR"
public constant IUP_ICON = "ICON"
public constant IUP_MENUBOX = "MENUBOX"
public constant IUP_MINBOX = "MINBOX"
public constant IUP_MAXBOX = "MAXBOX"
public constant IUP_RESIZE = "RESIZE"
public constant IUP_MENU = "MENU"
public constant IUP_STARTFOCUS = "STARTFOCUS"
public constant IUP_PARENTDIALOG = "PARENTDIALOG"
public constant IUP_SHRINK = "SHRINK"
public constant IUP_DEFAULTENTER = "DEFAULTENTER"
public constant IUP_DEFAULTESC = "DEFAULTESC"
public constant IUP_X = "X"
public constant IUP_Y = "Y"
public constant IUP_TOOLBOX = "TOOLBOX"
public constant IUP_CONTROL = "CONTROL"
public constant IUP_READONLY = "READONLY"
public constant IUP_SCROLLBAR = "SCROLLBAR"
public constant IUP_POSY = "POSY"
public constant IUP_POSX = "POSX"
public constant IUP_DX = "DX"
public constant IUP_DY = "DY"
public constant IUP_XMAX = "XMAX"
public constant IUP_XMIN = "XMIN"
public constant IUP_YMAX = "YMAX"
public constant IUP_YMIN = "YMIN"
public constant IUP_RED = "255 0 0"
public constant IUP_GREEN = "0 255 0"
public constant IUP_BLUE = "0 0 255"
public constant IUP_MIN = "MIN"
public constant IUP_MAX = "MAX"
public constant IUP_TIME = "TIME"
public constant IUP_DRAG = "DRAG"
public constant IUP_DROP = "DROP"
public constant IUP_REPAINT = "REPAINT"
public constant IUP_TOPMOST = "TOPMOST"
public constant IUP_CLIPCHILDREN = "CLIPCHILDREN"
public constant IUP_DIALOGTYPE = "DIALOGTYPE"
public constant IUP_FILE = "FILE"
public constant IUP_MULTIPLEFILES = "MULTIPLEFILES"
public constant IUP_FILTER = "FILTER"
public constant IUP_FILTERUSED = "FILTERUSED"
public constant IUP_FILTERINFO = "FILTERINFO"
public constant IUP_EXTFILTER = "EXTFILTER"
public constant IUP_DIRECTORY = "DIRECTORY"
public constant IUP_ALLOWNEW = "ALLOWNEW"
public constant IUP_NOOVERWRITEPROMPT = "NOOVERWRITEPROMPT"
public constant IUP_NOCHANGEDIR = "NOCHANGEDIR"
public constant IUP_FILEEXIST = "FILEEXIST"
public constant IUP_STATUS = "STATUS"
public constant IUP_LOCKLOOP = "LOCKLOOP"
public constant IUP_SYSTEM = "SYSTEM"
public constant IUP_DRIVER = "DRIVER"
public constant IUP_SCREENSIZE = "SCREENSIZE"
public constant IUP_SYSTEMLANGUAGE = "SYSTEMLANGUAGE"
public constant IUP_COMPUTERNAME = "COMPUTERNAME"
public constant IUP_USERNAME = "USERNAME"
public constant IUP_OPEN = "OPEN"
public constant IUP_SAVE = "SAVE"
public constant IUP_DIR = "DIR"
public constant IUP_HORIZONTAL = "HORIZONTAL"
public constant IUP_VERTICAL = "VERTICAL"
/************************************************************************/
/*                       Attribute Values                               */
/************************************************************************/
public constant IUP_YES = "YES"
public constant IUP_NO = "NO"
public constant IUP_ON = "ON"
public constant IUP_OFF = "OFF"
public constant IUP_ACENTER = "ACENTER"
public constant IUP_ALEFT = "ALEFT"
public constant IUP_ARIGHT = "ARIGHT"
public constant IUP_ATOP = "ATOP"
public constant IUP_ABOTTOM = "ABOTTOM"
public constant IUP_NORTH = "NORTH"
public constant IUP_SOUTH = "SOUTH"
public constant IUP_WEST = "WEST"
public constant IUP_EAST = "EAST"
public constant IUP_NE = "NE"
public constant IUP_SE = "SE"
public constant IUP_NW = "NW"
public constant IUP_SW = "SW"
public constant IUP_FULLSCREEN = "FULLSCREEN"
public constant IUP_FULL = "FULL"
public constant IUP_HALF = "HALF"
public constant IUP_THIRD = "THIRD"
public constant IUP_QUARTER = "QUARTER"
public constant IUP_EIGHTH = "EIGHTH"
public constant IUP_ARROW = "ARROW"
public constant IUP_BUSY = "BUSY"
public constant IUP_RESIZE_N = "RESIZE_N"
public constant IUP_RESIZE_S = "RESIZE_S"
public constant IUP_RESIZE_E = "RESIZE_E"
public constant IUP_RESIZE_W = "RESIZE_W"
public constant IUP_RESIZE_NE = "RESIZE_NE"
public constant IUP_RESIZE_NW = "RESIZE_NW"
public constant IUP_RESIZE_SE = "RESIZE_SE"
public constant IUP_RESIZE_SW = "RESIZE_SW"
public constant IUP_MOVE = "MOVE"
public constant IUP_HAND = "HAND"
public constant IUP_NONE = "NONE"
public constant IUP_IUP = "IUP"
public constant IUP_CROSS = "CROSS"
public constant IUP_PEN = "PEN"
public constant IUP_TEXT = "TEXT"
public constant IUP_RESIZE_C = "RESIZE_C"
public constant IUP_OPENHAND = "OPENHAND"
/*****************/
/* Fonts        */
/*****************/
public constant IUP_HELVETICA_NORMAL_8 = "HELVETICA_NORMAL_8"
public constant IUP_HELVETICA_ITALIC_8 = "HELVETICA_ITALIC_8"
public constant IUP_HELVETICA_BOLD_8 = "HELVETICA_BOLD_8"
public constant IUP_HELVETICA_NORMAL_10 = "HELVETICA_NORMAL_10"
public constant IUP_HELVETICA_ITALIC_10 = "HELVETICA_ITALIC_10"
public constant IUP_HELVETICA_BOLD_10 = "HELVETICA_BOLD_10"
public constant IUP_HELVETICA_NORMAL_12 = "HELVETICA_NORMAL_12"
public constant IUP_HELVETICA_ITALIC_12 = "HELVETICA_ITALIC_12"
public constant IUP_HELVETICA_BOLD_12 = "HELVETICA_BOLD_12"
public constant IUP_HELVETICA_NORMAL_14 = "HELVETICA_NORMAL_14"
public constant IUP_HELVETICA_ITALIC_14 = "HELVETICA_ITALIC_14"
public constant IUP_HELVETICA_BOLD_14 = "HELVETICA_BOLD_14"
public constant IUP_COURIER_NORMAL_8 = "COURIER_NORMAL_8"
public constant IUP_COURIER_ITALIC_8 = "COURIER_ITALIC_8"
public constant IUP_COURIER_BOLD_8 = "COURIER_BOLD_8"
public constant IUP_COURIER_NORMAL_10 = "COURIER_NORMAL_10"
public constant IUP_COURIER_ITALIC_10 = "COURIER_ITALIC_10"
public constant IUP_COURIER_BOLD_10 = "COURIER_BOLD_10"
public constant IUP_COURIER_NORMAL_12 = "COURIER_NORMAL_12"
public constant IUP_COURIER_ITALIC_12 = "COURIER_ITALIC_12"
public constant IUP_COURIER_BOLD_12 = "COURIER_BOLD_12"
public constant IUP_COURIER_NORMAL_14 = "COURIER_NORMAL_14"
public constant IUP_COURIER_ITALIC_14 = "COURIER_ITALIC_14"
public constant IUP_COURIER_BOLD_14 = "COURIER_BOLD_14"
public constant IUP_TIMES_NORMAL_8 = "TIMES_NORMAL_8"
public constant IUP_TIMES_ITALIC_8 = "TIMES_ITALIC_8"
public constant IUP_TIMES_BOLD_8 = "TIMES_BOLD_8"
public constant IUP_TIMES_NORMAL_10 = "TIMES_NORMAL_10"
public constant IUP_TIMES_ITALIC_10 = "TIMES_ITALIC_10"
public constant IUP_TIMES_BOLD_10 = "TIMES_BOLD_10"
public constant IUP_TIMES_NORMAL_12 = "TIMES_NORMAL_12"
public constant IUP_TIMES_ITALIC_12 = "TIMES_ITALIC_12"
public constant IUP_TIMES_BOLD_12 = "TIMES_BOLD_12"
public constant IUP_TIMES_NORMAL_14 = "TIMES_NORMAL_14"
public constant IUP_TIMES_ITALIC_14 = "TIMES_ITALIC_14"
public constant IUP_TIMES_BOLD_14 = "TIMES_BOLD_14"
/************************************************************************/
/*                           Keys                                       */
/************************************************************************/
public constant IUP_K_exclam = "K_exclam"
public constant IUP_K_quotedbl = "K_quotedbl"
public constant IUP_K_numbersign = "K_numbersign"
public constant IUP_K_dollar = "K_dollar"
public constant IUP_K_percent = "K_percent"
public constant IUP_K_ampersand = "K_ampersand"
public constant IUP_K_quoteright = "K_quoteright"
public constant IUP_K_parentleft = "K_parentleft"
public constant IUP_K_parentright = "K_parentright"
public constant IUP_K_asterisk = "K_asterisk"
public constant IUP_K_plus = "K_plus"
public constant IUP_K_comma = "K_comma"
public constant IUP_K_minus = "K_minus"
public constant IUP_K_period = "K_period"
public constant IUP_K_slash = "K_slash"
public constant IUP_K_0 = "K_0"
public constant IUP_K_1 = "K_1"
public constant IUP_K_2 = "K_2"
public constant IUP_K_3 = "K_3"
public constant IUP_K_4 = "K_4"
public constant IUP_K_5 = "K_5"
public constant IUP_K_6 = "K_6"
public constant IUP_K_7 = "K_7"
public constant IUP_K_8 = "K_8"
public constant IUP_K_9 = "K_9"
public constant IUP_K_colon = "K_colon"
public constant IUP_K_semicolon = "K_semicolon "
public constant IUP_K_less = "K_less"
public constant IUP_K_equal = "K_equal"
public constant IUP_K_greater = "K_greater"
public constant IUP_K_question = "K_question"
public constant IUP_K_at = "K_at"
public constant IUP_K_A = "K_A"
public constant IUP_K_B = "K_B"
public constant IUP_K_C = "K_C"
public constant IUP_K_D = "K_D"
public constant IUP_K_E = "K_E"
public constant IUP_K_F = "K_F"
public constant IUP_K_G = "K_G"
public constant IUP_K_H = "K_H"
public constant IUP_K_I = "K_I"
public constant IUP_K_J = "K_J"
public constant IUP_K_K = "K_K"
public constant IUP_K_L = "K_L"
public constant IUP_K_M = "K_M"
public constant IUP_K_N = "K_N"
public constant IUP_K_O = "K_O"
public constant IUP_K_P = "K_P"
public constant IUP_K_Q = "K_Q"
public constant IUP_K_R = "K_R"
public constant IUP_K_S = "K_S"
public constant IUP_K_T = "K_T"
public constant IUP_K_U = "K_U"
public constant IUP_K_V = "K_V"
public constant IUP_K_W = "K_W"
public constant IUP_K_X = "K_X"
public constant IUP_K_Y = "K_Y"
public constant IUP_K_Z = "K_Z"
public constant IUP_K_bracketleft = "K_bracketleft"
public constant IUP_K_backslash = "K_backslash"
public constant IUP_K_bracketright = "K_bracketright"
public constant IUP_K_circum = "K_circum"
public constant IUP_K_underscore = "K_underscore"
public constant IUP_K_quoteleft = "K_quoteleft"
public constant IUP_K_a = "K_a"
public constant IUP_K_b = "K_b"
public constant IUP_K_c = "K_c"
public constant IUP_K_d = "K_d"
public constant IUP_K_e = "K_e"
public constant IUP_K_f = "K_f"
public constant IUP_K_g = "K_g"
public constant IUP_K_h = "K_h"
public constant IUP_K_i = "K_i"
public constant IUP_K_j = "K_j"
public constant IUP_K_k = "K_k"
public constant IUP_K_l = "K_l"
public constant IUP_K_m = "K_m"
public constant IUP_K_n = "K_n"
public constant IUP_K_o = "K_o"
public constant IUP_K_p = "K_p"
public constant IUP_K_q = "K_q"
public constant IUP_K_r = "K_r"
public constant IUP_K_s = "K_s"
public constant IUP_K_t = "K_t"
public constant IUP_K_u = "K_u"
public constant IUP_K_v = "K_v"
public constant IUP_K_w = "K_w"
public constant IUP_K_x = "K_x"
public constant IUP_K_y = "K_y"
public constant IUP_K_z = "K_z"
public constant IUP_K_braceleft = "K_braceleft"
public constant IUP_K_bar = "K_bar"
public constant IUP_K_braceright = "K_braceright"
public constant IUP_K_tilde = "K_tilde"
public constant IUP_K_cA = "K_cA"
public constant IUP_K_cB = "K_cB"
public constant IUP_K_cC = "K_cC"
public constant IUP_K_cD = "K_cD"
public constant IUP_K_cE = "K_cE"
public constant IUP_K_cF = "K_cF"
public constant IUP_K_cG = "K_cG"
public constant IUP_K_cJ = "K_cJ"
public constant IUP_K_cK = "K_cK"
public constant IUP_K_cL = "K_cL"
public constant IUP_K_cN = "K_cN"
public constant IUP_K_cO = "K_cO"
public constant IUP_K_cP = "K_cP"
public constant IUP_K_cQ = "K_cQ"
public constant IUP_K_cR = "K_cR"
public constant IUP_K_cS = "K_cS"
public constant IUP_K_cT = "K_cT"
public constant IUP_K_cU = "K_cU"
public constant IUP_K_cV = "K_cV"
public constant IUP_K_cW = "K_cW"
public constant IUP_K_cX = "K_cX"
public constant IUP_K_cY = "K_cY"
public constant IUP_K_cZ = "K_cZ"
public constant IUP_K_mA = "K_mA"
public constant IUP_K_mB = "K_mB"
public constant IUP_K_mC = "K_mC"
public constant IUP_K_mD = "K_mD"
public constant IUP_K_mE = "K_mE"
public constant IUP_K_mF = "K_mF"
public constant IUP_K_mG = "K_mG"
public constant IUP_K_mH = "K_mH"
public constant IUP_K_mI = "K_mI"
public constant IUP_K_mJ = "K_mJ"
public constant IUP_K_mK = "K_mK"
public constant IUP_K_mL = "K_mL"
public constant IUP_K_mM = "K_mM"
public constant IUP_K_mN = "K_mN"
public constant IUP_K_mO = "K_mO"
public constant IUP_K_mP = "K_mP"
public constant IUP_K_mQ = "K_mQ"
public constant IUP_K_mR = "K_mR"
public constant IUP_K_mS = "K_mS"
public constant IUP_K_mT = "K_mT"
public constant IUP_K_mU = "K_mU"
public constant IUP_K_mV = "K_mV"
public constant IUP_K_mW = "K_mW"
public constant IUP_K_mX = "K_mX"
public constant IUP_K_mY = "K_mY"
public constant IUP_K_mZ = "K_mZ"
public constant IUP_K_BS = "K_BS"
public constant IUP_K_TAB = "K_TAB"
public constant IUP_K_CR = "K_CR"
public constant IUP_K_SP = "K_SP"
public constant IUP_K_ESC = "K_ESC"
public constant IUP_K_sCR = "K_sCR"
public constant IUP_K_sTAB = "K_sTAB"
public constant IUP_K_cTAB = "K_cTAB"
public constant IUP_K_mTAB = "K_mTAB"
public constant IUP_K_HOME = "K_HOME"
public constant IUP_K_UP = "K_UP"
public constant IUP_K_PGUP = "K_PGUP"
public constant IUP_K_LEFT = "K_LEFT"
public constant IUP_K_RIGHT = "K_RIGHT"
public constant IUP_K_END = "K_END"
public constant IUP_K_DOWN = "K_DOWN"
public constant IUP_K_PGDN = "K_PGDN"
public constant IUP_K_MIDDLE = "K_MIDDLE"
public constant IUP_K_INS = "K_INS"
public constant IUP_K_DEL = "K_DEL"
public constant IUP_K_sHOME = "K_sHOME"
public constant IUP_K_sUP = "K_sUP"
public constant IUP_K_sPGUP = "K_sPGUP"
public constant IUP_K_sLEFT = "K_sLEFT"
public constant IUP_K_sRIGHT = "K_sRIGHT"
public constant IUP_K_sEND = "K_sEND"
public constant IUP_K_sDOWN = "K_sDOWN"
public constant IUP_K_sPGDN = "K_sPGDN"
public constant IUP_K_cHOME = "K_cHOME"
public constant IUP_K_cPGUP = "K_cPGUP"
public constant IUP_K_cLEFT = "K_cLEFT"
public constant IUP_K_cRIGHT = "K_cRIGHT"
public constant IUP_K_cEND = "K_cEND"
public constant IUP_K_cPGDN = "K_cPGDN"
public constant IUP_K_cUP = "K_cUP"
public constant IUP_K_cDOWN = "K_cDOWN"
public constant IUP_K_cMIDDLE = "K_cMIDDLE"
public constant IUP_K_cINS = "K_cINS"
public constant IUP_K_cDEL = "K_cDEL"
public constant IUP_K_mHOME = "K_mHOME"
public constant IUP_K_mPGUP = "K_mPGUP"
public constant IUP_K_mLEFT = "K_mLEFT"
public constant IUP_K_mRIGHT = "K_mRIGHT"
public constant IUP_K_mEND = "K_mEND"
public constant IUP_K_mPGDN = "K_mPGDN"
public constant IUP_K_mUP = "K_mUP"
public constant IUP_K_mDOWN = "K_mDOWN"
public constant IUP_K_mINS = "K_mINS"
public constant IUP_K_mDEL = "K_mDEL"
public constant IUP_K_F1 = "K_F1"
public constant IUP_K_F2 = "K_F2"
public constant IUP_K_F3 = "K_F3"
public constant IUP_K_F4 = "K_F4"
public constant IUP_K_F5 = "K_F5"
public constant IUP_K_F6 = "K_F6"
public constant IUP_K_F7 = "K_F7"
public constant IUP_K_F8 = "K_F8"
public constant IUP_K_F9 = "K_F9"
public constant IUP_K_F10 = "K_F10"
public constant IUP_K_F11 = "K_F11"
public constant IUP_K_F12 = "K_F12"
public constant IUP_K_sF1 = "K_sF1"
public constant IUP_K_sF2 = "K_sF2"
public constant IUP_K_sF3 = "K_sF3"
public constant IUP_K_sF4 = "K_sF4"
public constant IUP_K_sF5 = "K_sF5"
public constant IUP_K_sF6 = "K_sF6"
public constant IUP_K_sF7 = "K_sF7"
public constant IUP_K_sF8 = "K_sF8"
public constant IUP_K_sF9 = "K_sF9"
public constant IUP_K_sF10 = "K_sF10"
public constant IUP_K_sF11 = "K_sF11"
public constant IUP_K_sF12 = "K_sF12"
public constant IUP_K_cF1 = "K_cF1"
public constant IUP_K_cF2 = "K_cF2"
public constant IUP_K_cF3 = "K_cF3"
public constant IUP_K_cF4 = "K_cF4"
public constant IUP_K_cF5 = "K_cF5"
public constant IUP_K_cF6 = "K_cF6"
public constant IUP_K_cF7 = "K_cF7"
public constant IUP_K_cF8 = "K_cF8"
public constant IUP_K_cF9 = "K_cF9"
public constant IUP_K_cF10 = "K_cF10"
public constant IUP_K_cF11 = "K_cF11"
public constant IUP_K_cF12 = "K_cF12"
public constant IUP_K_mF1 = "K_mF1"
public constant IUP_K_mF2 = "K_mF2"
public constant IUP_K_mF3 = "K_mF3"
public constant IUP_K_mF4 = "K_mF4"
public constant IUP_K_mF5 = "K_mF5"
public constant IUP_K_mF6 = "K_mF6"
public constant IUP_K_mF7 = "K_mF7"
public constant IUP_K_mF8 = "K_mF8"
public constant IUP_K_mF9 = "K_mF9"
public constant IUP_K_mF10 = "K_mF10"
public constant IUP_K_m1 = "K_m1"
public constant IUP_K_m2 = "K_m2"
public constant IUP_K_m3 = "K_m3"
public constant IUP_K_m4 = "K_m4"
public constant IUP_K_m5 = "K_m5"
public constant IUP_K_m6 = "K_m6"
public constant IUP_K_m7 = "K_m7"
public constant IUP_K_m8 = "K_m8"
public constant IUP_K_m9 = "K_m9"
public constant IUP_K_m0 = "K_m0"
/************/
/* Colorbar */
/************/
public constant IUP_NUM_PARTS = "NUM_PARTS"
public constant IUP_NUM_CELLS = "NUM_CELLS"
public constant IUP_CELL = "CELL"
public constant IUP_PREVIEW_SIZE = "PREVIEW_SIZE"
public constant IUP_SHOW_PREVIEW = "SHOW_PREVIEW"
public constant IUP_SHOW_SECONDARY = "SHOW_SECONDARY"
public constant IUP_PRIMARY_CELL = "PRIMARY_CELL"
public constant IUP_SECONDARY_CELL = "SECONDARY_CELL"
public constant IUP_ORIENTATION = "ORIENTATION"
public constant IUP_SQUARED = "SQUARED"
public constant IUP_SHADOWED = "SHADOWED"
public constant IUP_BUFFERIZE = "BUFFERIZE"
public constant IUP_TRANSPARENCY = "TRANSPARENCY"
public constant IUP_CELL_CB = "CELL_CB"
public constant IUP_EXTENDED_CB = "EXTENDED_CB"
public constant IUP_SELECT_CB = "SELECT_CB"
public constant IUP_SWITCH_CB = "SWITCH_CB"
--public constant IUP_VERTICAL = "VERTICAL"
--public constant IUP_HORIZONTAL = "HORIZONTAL"
/************/
/* Cells    */
/************/
public constant IUP_ALL = "ALL"
public constant IUP_BOXED = "BOXED"
public constant IUP_CLIPPED = "CLIPPED"
public constant IUP_TRANSPARENT = "TRANSPARENT"
public constant IUP_NON_SCROLLABLE_LINES = "NON_SCROLLABLE_LINES"
public constant IUP_NON_SCROLLABLE_COLS = "NON_SCROLLABLE_COLS"
public constant IUP_ORIGIN = "ORIGIN"
public constant IUP_NO_COLOR = "NO_COLOR"
public constant IUP_FIRST_LINE = "FIRST_LINE"
public constant IUP_FIRST_COL = "FIRST_COL"
public constant IUP_DOUBLE_BUFFER = "DOUBLE_BUFFER"
public constant IUP_LIMITS = "LIMITS"
public constant IUP_CANVAS = "CANVAS"
public constant IUP_IMAGE_CANVAS = "IMAGE_CANVAS"
public constant IUP_FULL_VISIBLE = "FULL_VISIBLE"
public constant IUP_MOUSECLICK_CB = "MOUSECLICK_CB"
public constant IUP_MOUSEMOTION_CB = "MOUSEMOTION_CB"
public constant IUP_DRAW_CB = "DRAW_CB"
public constant IUP_WIDTH_CB = "WIDTH_CB"
public constant IUP_HEIGHT_CB = "HEIGHT_CB"
public constant IUP_NLINES_CB = "NLINES_CB"
public constant IUP_NCOLS_CB = "NCOLS_CB"
public constant IUP_HSPAN_CB = "HSPAN_CB"
public constant IUP_VSPAN_CB = "VSPAN_CB"
public constant IUP_SCROLLING_CB = "SCROLLING_CB"
/*****************/
/* ColorBrowser  */
/*****************/
public constant IUP_RGB = "RGB"
public constant IUP_CHANGE_CB = "CHANGE_CB"
public constant IUP_DRAG_CB = "DRAG_CB"
/*****************/
/* Val           */
/*****************/
public constant ICTL_MOUSEMOVE_CB = "MOUSEMOVE_CB"
public constant ICTL_BUTTON_PRESS_CB = "BUTTON_PRESS_CB"
public constant ICTL_BUTTON_RELEASE_CB = "BUTTON_RELEASE_CB"
public constant ICTL_HORIZONTAL = "HORIZONTAL"
public constant ICTL_VERTICAL = "VERTICAL"
public constant ICTL_SHOWTICKS = "SHOWTICKS"
/*****************/
/* Tabs          */
/*****************/
public constant ICTL_TOP = "TOP"
public constant ICTL_BOTTOM = "BOTTOM"
public constant ICTL_LEFT = "LEFT"
public constant ICTL_RIGHT = "RIGHT"
public constant ICTL_TABTYPE = "TABTYPE"
public constant ICTL_TABTITLE = "TABTITLE"
public constant ICTL_TABSIZE = "TABSIZE"
public constant ICTL_TABCHANGE_CB = "TABCHANGE_CB"
public constant ICTL_FONT = "FONT"
public constant ICTL_FONT_ACTIVE = "FONT_ACTIVE"
public constant ICTL_FONT_INACTIVE = "FONT_INACTIVE"
/*****************/
/* Gauge         */
/*****************/
public constant ICTL_SHOW_TEXT = "SHOW_TEXT"
public constant ICTL_DASHED = "DASHED"
public constant ICTL_MARGIN = "MARGIN"
public constant ICTL_TEXT = "TEXT"
/*****************/
/* Dial          */
/*****************/
public constant ICTL_DENSITY = "DENSITY"
--public constant ICTL_HORIZONTAL = "HORIZONTAL"
--public constant ICTL_VERTICAL = "VERTICAL"
public constant ICTL_CIRCULAR = "CIRCULAR"
public constant ICTL_UNIT = "UNIT"
/*****************/
/* Matrix        */
/*****************/
public constant IUP_ENTERITEM_CB = "ENTERITEM_CB"
public constant IUP_LEAVEITEM_CB = "LEAVEITEM_CB"
public constant IUP_EDITION_CB = "EDITION_CB"
public constant IUP_CLICK_CB = "CLICK_CB"
public constant IUP_DROP_CB = "DROP_CB"
public constant IUP_DROPSELECT_CB = "DROPSELECT_CB"
public constant IUP_DROPCHECK_CB = "DROPCHECK_CB"
--public constant IUP_SCROLL_CB = "SCROLL_CB"
public constant IUP_VALUE_CB = "VALUE_CB"
public constant IUP_VALUE_EDIT_CB = "VALUE_EDIT_CB"
public constant IUP_FIELD_CB = "FIELD_CB"
public constant IUP_RESIZEMATRIX = "RESIZEMATRIX"
public constant IUP_ADDLIN = "ADDLIN"
public constant IUP_ADDCOL = "ADDCOL"
public constant IUP_DELLIN = "DELLIN"
public constant IUP_DELCOL = "DELCOL"
public constant IUP_NUMLIN = "NUMLIN"
public constant IUP_NUMCOL = "NUMCOL"
public constant IUP_NUMLIN_VISIBLE = "NUMLIN_VISIBLE"
public constant IUP_NUMCOL_VISIBLE = "NUMCOL_VISIBLE"
public constant IUP_MARKED = "MARKED"
public constant IUP_WIDTHDEF = "WIDTHDEF"
public constant IUP_HEIGHTDEF = "HEIGHTDEF"
public constant IUP_AREA = "AREA"
public constant IUP_MARK_MODE = "MARK_MODE"
public constant IUP_LIN = "LIN"
public constant IUP_COL = "COL"
public constant IUP_LINCOL = "LINCOL"
--public constant IUP_CELL = "CELL"
public constant IUP_EDIT_MODE = "EDIT_MODE"
public constant IUP_FOCUS_CELL = "FOCUS_CELL"
--public constant IUP_ORIGIN = "ORIGIN"
public constant IUP_REDRAW = "REDRAW"
public constant IUP_PREVIOUSVALUE = "PREVIOUSVALUE"
public constant IUP_MOUSEMOVE_CB = "MOUSEMOVE_CB"
/*****************/
/* Tree          */
/*****************/
public constant IUP_ADDLEAF = "ADDLEAF"
public constant IUP_ADDBRANCH = "ADDBRANCH"
public constant IUP_DELNODE = "DELNODE"
public constant IUP_IMAGELEAF = "IMAGELEAF"
public constant IUP_IMAGEBRANCHCOLLAPSED = "IMAGEBRANCHCOLLAPSED"
public constant IUP_IMAGEBRANCHEXPANDED = "IMAGEBRANCHEXPANDED"
public constant IUP_IMAGEEXPANDED = "IMAGEEXPANDED"
public constant IUP_KIND = "KIND"
public constant IUP_PARENT = "PARENT"
public constant IUP_DEPTH = "DEPTH"
--public constant IUP_MARKED = "MARKED"
public constant IUP_ADDEXPANDED = "ADDEXPANDED"
public constant IUP_CTRL = "CTRL"
public constant IUP_SHIFT = "SHIFT"
public constant IUP_STATE = "STATE"
public constant IUP_STARTING = "STARTING"
public constant IUP_LEAF = "LEAF"
public constant IUP_BRANCH = "BRANCH"
public constant IUP_SELECTED = "SELECTED"
public constant IUP_CHILDREN = "CHILDREN"
--public constant IUP_MARKED = "MARKED"
public constant IUP_ROOT = "ROOT"
public constant IUP_LAST = "LAST"
public constant IUP_PGUP = "PGUP"
public constant IUP_PGDN = "PGDN"
public constant IUP_NEXT = "NEXT"
public constant IUP_PREVIOUS = "PREVIOUS"
public constant IUP_INVERT = "INVERT"
public constant IUP_BLOCK = "BLOCK"
public constant IUP_CLEARALL = "CLEARALL"
public constant IUP_MARKALL = "MARKALL"
public constant IUP_INVERTALL = "INVERTALL"
--public constant IUP_REDRAW = "REDRAW"
public constant IUP_COLLAPSED = "COLLAPSED"
public constant IUP_EXPANDED = "EXPANDED"
public constant IUP_SELECTION_CB = "SELECTION_CB"
public constant IUP_BRANCHOPEN_CB = "BRANCHOPEN_CB"
public constant IUP_BRANCHCLOSE_CB = "BRANCHCLOSE_CB"
public constant IUP_RIGHTCLICK_CB = "RIGHTCLICK_CB"
public constant IUP_EXECUTELEAF_CB = "EXECUTELEAF_CB"
public constant IUP_RENAMENODE_CB = "RENAMENODE_CB"
public constant IUP_IMGLEAF = "IMGLEAF"
public constant IUP_IMGCOLLAPSED = "IMGCOLLAPSED"
public constant IUP_IMGEXPANDED = "IMGEXPANDED"
public constant IUP_IMGBLANK = "IMGBLANK"
public constant IUP_IMGPAPER = "IMGPAPER"
--*/

public constant -- function delcarations
--      xIupOpen                          = define_c_func(iup, "+IupOpen", {P,P}, I),
--      xIupClose                         = define_c_proc(iup, "+IupClose", {}),
--      xIupImageLibOpen                  = define_c_proc(iupimglib, "+IupImageLibOpen", {}),
--      xIupMainLoop                      = define_c_func(iup, "+IupMainLoop", {}, I),
--      xIupLoopStep                      = define_c_func(iup, "+IupLoopStep", {}, I),
--      xIupLoopStepWait                  = define_c_func(iup, "+IupLoopStepWait", {}, I),
--      xIupMainLoopLevel                 = define_c_func(iup, "+IupMainLoopLevel", {}, I),
--      xIupFlush                         = define_c_proc(iup, "+IupFlush", {}),
--      xIupExitLoop                      = define_c_proc(iup, "+IupExitLoop", {}),
--      xIupRecordInput                   = define_c_func(iup, "+IupRecordInput", {P,I}, I),
--      xIupPlayInput                     = define_c_func(iup, "+IupPlayInput", {P}, I),
--      xIupUpdate                        = define_c_proc(iup, "+IupUpdate", {P}),
--      xIupUpdateChildren                = define_c_proc(iup, "+IupUpdateChildren", {P}),
--      xIupRedraw                        = define_c_proc(iup, "+IupRedraw", {P,I}),
--      xIupRefresh                       = define_c_proc(iup, "+IupRefresh", {P}),
--      xIupRefreshChildren               = define_c_proc(iup, "+IupRefreshChildren", {P}),
--      xIupHelp                          = define_c_func(iup, "+IupHelp", {P}, I),
        xIupLoad                          = define_c_func(iup, "+IupLoad", {P}, P),
        xIupLoadBuffer                    = define_c_func(iup, "+IupLoadBuffer", {P}, P),
--      xIupVersion                       = define_c_func(iup, "+IupVersion", {}, P),
--      xIupVersionDate                   = define_c_func(iup, "+IupVersionDate", {}, P),
--      xIupVersionNumber                 = define_c_func(iup, "+IupVersionNumber", {}, I),
        xIupSetLanguage                   = define_c_proc(iup, "+IupSetLanguage", {P}),
        xIupGetLanguage                   = define_c_func(iup, "+IupGetLanguage", {}, P),
        xIupSetLanguageString             = define_c_proc(iup, "+IupSetLanguageString", {P,P}),
        xIupStoreLanguageString           = define_c_proc(iup, "+IupStoreLanguageString", {P,P}),
        xIupGetLanguageString             = define_c_func(iup, "+IupGetLanguageString", {P}, P),
        xIupSetLanguagePack               = define_c_proc(iup, "+IupSetLanguagePack", {P}),
--      xIupDestroy                       = define_c_proc(iup, "+IupDestroy", {P}),
--      xIupDetach                        = define_c_proc(iup, "+IupDetach", {P}),
--      xIupAppend                        = define_c_func(iup, "+IupAppend", {P,P}, P),
--      xIupInsert                        = define_c_func(iup, "+IupInsert", {P,P,P}, P),
--      xIupGetChild                      = define_c_func(iup, "+IupGetChild", {P,I}, P),
--      xIupGetChildPos                   = define_c_func(iup, "+IupGetChildPos", {P,P}, I),
--      xIupGetChildCount                 = define_c_func(iup, "+IupGetChildCount", {P}, I),
--      xIupGetNextChild                  = define_c_func(iup, "+IupGetNextChild", {P,P}, P),
--      xIupGetBrother                    = define_c_func(iup, "+IupGetBrother", {P}, P),
--      xIupGetParent                     = define_c_func(iup, "+IupGetParent", {P}, P),
--      xIupGetDialog                     = define_c_func(iup, "+IupGetDialog", {P}, P),
--      xIupGetDialogChild                = define_c_func(iup, "+IupGetDialogChild", {P,P}, P),
--      xIupReparent                      = define_c_func(iup, "+IupReparent", {P,P,P}, I),
--      xIupPopup                         = define_c_func(iup, "+IupPopup", {P,I,I}, I),
--      xIupShow                          = define_c_func(iup, "+IupShow", {P}, I),
--      xIupShowXY                        = define_c_func(iup, "+IupShowXY", {P,I,I}, I),
--      xIupHide                          = define_c_func(iup, "+IupHide", {P}, I),
--      xIupMap                           = define_c_func(iup, "+IupMap", {P}, I),
--      xIupUnmap                         = define_c_proc(iup, "+IupUnmap", {P}),
--      xIupResetAttribute                = define_c_proc(iup, "+IupResetAttribute", {P,P}),
--      xIupGetAllAttributes              = define_c_func(iup, "+IupGetAllAttributes", {P,P,I}, I),
--      xIupSetAttributes                 = define_c_func(iup, "+IupSetAttributes", {P,P}, P),
--      xIupGetAttributes                 = define_c_func(iup, "+IupGetAttributes", {P}, P),
--      xIupSetAttribute                  = define_c_proc(iup, "+IupSetAttribute", {P,P,P}),
--      xIupSetStrAttribute               = define_c_proc(iup, "+IupSetStrAttribute", {P,P,P}),
--      xIupSetInt                        = define_c_proc(iup, "+IupSetInt", {P,P,I}),
--      xIupSetFloat                      = define_c_proc(iup, "+IupSetFloat", {P,P,F}),
--      xIupSetDouble                     = define_c_proc(iup, "+IupSetDouble", {P,P,D}),
--      xIupSetRGB                        = define_c_proc(iup, "+IupSetRGB", {P,P,UC,UC,UC}),
--      xIupGetAttribute                  = define_c_func(iup, "+IupGetAttribute", {P,P}, P),
--      xIupGetInt                        = define_c_func(iup, "+IupGetInt", {P,P}, I),
--      xIupGetInt2                       = define_c_func(iup, "+IupGetInt2", {P,P}, I),
--      xIupGetIntInt                     = define_c_func(iup, "+IupGetIntInt", {P,P,P,P}, I),
--      xIupGetFloat                      = define_c_func(iup, "+IupGetFloat", {P,P}, F),
--      xIupGetDouble                     = define_c_func(iup, "+IupGetDouble", {P,P}, D),
--      xIupGetRGB                        = define_c_proc(iup, "+IupGetRGB", {P,P,P,P,P}),
--      xIupSetAttributeId                = define_c_proc(iup, "+IupSetAttributeId", {P,P,I,P}),
--      xIupSetStrAttributeId             = define_c_proc(iup, "+IupSetStrAttributeId", {P,P,I,P}),
--      xIupSetIntId                      = define_c_proc(iup, "+IupSetIntId", {P,P,I,I}),
--      xIupSetFloatId                    = define_c_proc(iup, "+IupSetFloatId", {P,P,I,F}),
--      xIupSetDoubleId                   = define_c_proc(iup, "+IupSetDoubleId", {P,P,I,D}),
--      xIupSetRGBId                      = define_c_proc(iup, "+IupSetRGBId", {P,P,I,UC,UC,UC}),
--      xIupGetAttributeId                = define_c_func(iup, "+IupGetAttributeId", {P,P,I}, P),
--      xIupGetIntId                      = define_c_func(iup, "+IupGetIntId", {P,P,I}, I),
--      xIupGetFloatId                    = define_c_func(iup, "+IupGetFloatId", {P,P,I}, F),
--      xIupGetDoubleId                   = define_c_func(iup, "+IupGetDoubleId", {P,P,I}, D),
--      xIupGetRGBId                      = define_c_proc(iup, "+IupGetRGBId", {P,P,I,P,P,P}),
--      xIupSetAttributeId2               = define_c_proc(iup, "+IupSetAttributeId2", {P,P,I,I,P}),
--      xIupSetStrAttributeId2            = define_c_proc(iup, "+IupSetStrAttributeId2", {P,P,I,I,P}),
--      xIupSetIntId2                     = define_c_proc(iup, "+IupSetIntId2", {P,P,I,I,I}),
--      xIupSetFloatId2                   = define_c_proc(iup, "+IupSetFloatId2", {P,P,I,I,F}),
--      xIupSetDoubleId2                  = define_c_proc(iup, "+IupSetDoubleId2", {P,P,I,I,D}),
--      xIupSetRGBId2                     = define_c_proc(iup, "+IupSetRGBId2", {P,P,I,I,UC,UC,UC}),
--      xIupGetAttributeId2               = define_c_func(iup, "+IupGetAttributeId2", {P,P,I,I}, P),
--      xIupGetIntId2                     = define_c_func(iup, "+IupGetIntId2", {P,P,I,I}, I),
--      xIupGetFloatId2                   = define_c_func(iup, "+IupGetFloatId2", {P,P,I,I}, F),
--      xIupGetDoubleId2                  = define_c_func(iup, "+IupGetDoubleId2", {P,P,I,I}, D),
--      xIupGetRGBId2                     = define_c_proc(iup, "+IupGetRGBId2", {P,P,I,I,P,P,P}),
--      xIupSetGlobal                     = define_c_proc(iup, "+IupSetGlobal", {P,P}),
--      xIupSetStrGlobal                  = define_c_proc(iup, "+IupSetStrGlobal", {P,P}),
--      xIupGetGlobal                     = define_c_func(iup, "+IupGetGlobal", {P}, P),
--      xIupSetFocus                      = define_c_func(iup, "+IupSetFocus", {P}, P),
--      xIupGetFocus                      = define_c_func(iup, "+IupGetFocus", {}, P),
--      xIupPreviousField                 = define_c_func(iup, "+IupPreviousField", {P}, P),
--      xIupNextField                     = define_c_func(iup, "+IupNextField", {P}, P),
--      xIupGetCallback                   = define_c_func(iup, "+IupGetCallback", {P,P}, P),
--      xIupSetCallback                   = define_c_func(iup, "+IupSetCallback", {P,P,P}, P),
        xIupGetFunction                   = define_c_func(iup, "+IupGetFunction", {P}, P),
        xIupSetFunction                   = define_c_func(iup, "+IupSetFunction", {P,P}, P),
--      xIupGetHandle                     = define_c_func(iup, "+IupGetHandle", {P}, P),
--      xIupSetHandle                     = define_c_proc(iup, "+IupSetHandle", {P,P}),
--      xIupGetAllNames                   = define_c_func(iup, "+IupGetAllNames", {P,I}, I),
--      xIupGetAllDialogs                 = define_c_func(iup, "+IupGetAllDialogs", {P,I}, I),
--      xIupGetName                       = define_c_func(iup, "+IupGetName", {P}, P),
--      xIupSetAttributeHandle            = define_c_proc(iup, "+IupSetAttributeHandle", {P,P,P}),
--      xIupGetAttributeHandle            = define_c_func(iup, "+IupGetAttributeHandle", {P,P}, P),
--      xIupGetClassName                  = define_c_func(iup, "+IupGetClassName", {P}, P),
--      xIupGetClassType                  = define_c_func(iup, "+IupGetClassType", {P}, P),
--      xIupGetAllClasses                 = define_c_func(iup, "+IupGetAllClasses", {P,I}, I),
--      xIupGetClassAttributes            = define_c_func(iup, "+IupGetClassAttributes", {P,P,I}, I),
--      xIupGetClassCallbacks             = define_c_func(iup, "+IupGetClassCallbacks", {P,P,I}, I),
--      xIupSaveClassAttributes           = define_c_proc(iup, "+IupSaveClassAttributes", {P}),
--      xIupCopyClassAttributes           = define_c_proc(iup, "+IupCopyClassAttributes", {P,P}),
--      xIupSetClassDfltAttribute         = define_c_proc(iup, "+IupSetClassDefaultAttribute", {P,P,P}),
        xIupClassMatch                    = define_c_func(iup, "+IupClassMatch", {P,P}, I),
--      xIupCreate                        = define_c_func(iup, "+IupCreatev", {P,P}, P),
--      xIupFill                          = define_c_func(iup, "+IupFill", {}, P),
--      xIupRadio                         = define_c_func(iup, "+IupRadio", {P}, P),
--      xIupVbox                          = define_c_func(iup, "+IupVboxv", {P}, P),
--      xIupZbox                          = define_c_func(iup, "+IupZboxv", {P}, P),
--      xIupHbox                          = define_c_func(iup, "+IupHboxv", {P}, P),
--      xIupNormalizer                    = define_c_func(iup, "+IupNormalizerv", {P}, P),
--      xIupCboxv                         = define_c_func(iup, "+IupCboxv", {P}, P),
--      xIupSbox                          = define_c_func(iup, "+IupSbox", {P}, P),
--      xIupSplit                         = define_c_func(iup, "+IupSplit", {P,P}, P),
        xIupScrollBox                     = define_c_func(iup, "+IupScrollBox", {P}, P),
--      xIupGridBox                       = define_c_func(iup, "+IupGridBoxv", {P}, P),
        xIupExpander                      = define_c_func(iup, "+IupExpander", {P}, P),
        xIupDetachBox                     = define_c_func(iup, "+IupDetachBox", {P}, P),
        xIupBackgroundBox                 = define_c_func(iup, "+IupBackgroundBox", {P}, P),
--      xIupFrame                         = define_c_func(iup, "+IupFrame", {P}, P),
--      xIupImage                         = define_c_func(iup, "+IupImage", {I,I,P}, P),
--      xIupImageRGB                      = define_c_func(iup, "+IupImageRGB", {I,I,P}, P),
--      xIupImageRGBA                     = define_c_func(iup, "+IupImageRGBA", {I,I,P}, P),
--      xIupItem                          = define_c_func(iup, "+IupItem", {P,P}, P),
--      xIupSubmenu                       = define_c_func(iup, "+IupSubmenu", {P,P}, P),
--      xIupSeparator                     = define_c_func(iup, "+IupSeparator", {}, P),
--      xIupMenu                          = define_c_func(iup, "+IupMenuv", {P}, P),
--      xIupButton                        = define_c_func(iup, "+IupButton", {P,P}, P),
--      xIupCanvas                        = define_c_func(iup, "+IupCanvas", {P}, P),
--      xIupDialog                        = define_c_func(iup, "+IupDialog", {P}, P),
--      xIupUser                          = define_c_func(iup, "+IupUser", {}, P),
--      xIupLabel                         = define_c_func(iup, "+IupLabel", {P}, P),
--      xIupList                          = define_c_func(iup, "+IupList", {P}, P),
--      xIupText                          = define_c_func(iup, "+IupText", {P}, P),
--      xIupMultiLine                     = define_c_func(iup, "+IupMultiLine", {P}, P),
--      xIupToggle                        = define_c_func(iup, "+IupToggle", {P,P}, P),
--      xIupTimer                         = define_c_func(iup, "+IupTimer", {}, P),
--      xIupClipboard                     = define_c_func(iup, "+IupClipboard", {}, P),
--      xIupProgressBar                   = define_c_func(iup, "+IupProgressBar", {}, P),
--      xIupVal                           = define_c_func(iup, "+IupVal", {P}, P),
--      xIupTabs                          = define_c_func(iup, "+IupTabsv", {P}, P),
--      xIupTree                          = define_c_func(iup, "+IupTree", {}, P),
        xIupLink                          = define_c_func(iup, "+IupLink", {P,P}, P),
        xIupFlatButton                    = define_c_func(iup, "+IupFlatButton", {P}, P),
--      xIupSpin                          = define_c_func(iup, "+IupSpin", {}, P),
--      xIupSpinbox                       = define_c_func(iup, "+IupSpinbox", {P}, P),
--      xIupSaveImageAsText               = define_c_func(iup, "+IupSaveImageAsText", {P,P,P,P}, I),
--      xIupTextConvertLinColToPos        = define_c_proc(iup, "+IupTextConvertLinColToPos", {P,I,I,P}),
--      xIupTextConvertPosToLinCol        = define_c_proc(iup, "+IupTextConvertPosToLinCol", {P,I,P,P}),
--      xIupConvertXYToPos                = define_c_func(iup, "+IupConvertXYToPos", {P,I,I}, I),
--      xIupStoreGlobal                   = define_c_proc(iup, "+IupStoreGlobal", {P,P}),
--      xIupStoreAttribute                = define_c_proc(iup, "+IupStoreAttribute", {P,P,P}),
        xIupStoreAttributeId              = define_c_proc(iup, "+IupStoreAttributeId", {P,P,I,P}),
        xIupStoreAttributeId2             = define_c_proc(iup, "+IupStoreAttributeId2", {P,P,I,I,P}),
        xIupTreeSetUserId                 = define_c_func(iup, "+IupTreeSetUserId", {P,I,P}, I),
        xIupTreeGetUserId                 = define_c_func(iup, "+IupTreeGetUserId", {P,I}, P),
        xIupTreeGetId                     = define_c_func(iup, "+IupTreeGetId", {P,P}, I),
        xIupTreeSetAttributeHandle        = define_c_proc(iup, "+IupTreeSetAttributeHandle", {P,P,I,P}),
        xIupTreeSetAttribute              = define_c_proc(iup, "+IupTreeSetAttribute", {P,P,I,P}),
        xIupTreeStoreAttribute            = define_c_proc(iup, "+IupTreeStoreAttribute", {P,P,I,P}),
        xIupTreeGetAttribute              = define_c_func(iup, "+IupTreeGetAttribute", {P,P,I}, P),
        xIupTreeGetInt                    = define_c_func(iup, "+IupTreeGetInt", {P,P,I}, I),
        xIupTreeGetFloat                  = define_c_func(iup, "+IupTreeGetFloat", {P,P,I}, F),
--      xIupGetActionName                 = define_c_func(iup, "+IupGetActionName", {}, P),
        xIupMapFont                       = define_c_func(iup, "+IupMapFont", {P}, P),
        xIupUnMapFont                     = define_c_func(iup, "+IupUnMapFont", {P}, P),
--      xIupFileDlg                       = define_c_func(iup, "+IupFileDlg", {}, P),
--      xIupMessageDlg                    = define_c_func(iup, "+IupMessageDlg", {}, P),
--      xIupColorDlg                      = define_c_func(iup, "+IupColorDlg", {}, P),
--      xIupFontDlg                       = define_c_func(iup, "+IupFontDlg", {}, P),
--      xIupProgressDlg                   = define_c_func(iup, "+IupProgressDlg", {}, P),
--      xIupGetFile                       = define_c_func(iup, "+IupGetFile", {P}, I),
--      xIupMessage                       = define_c_proc(iup, "+IupMessage", {P,P}),
--      xIupAlarm                         = define_c_func(iup, "+IupAlarm", {P,P,P,P,P}, I),
--      xIupListDialog                    = define_c_func(iup, "+IupListDialog", {I,P,I,P,I,I,I,P}, I),
--      xIupGetText                       = define_c_func(iup, "+IupGetText", {P,P}, I),
--      xIupGetColor                      = define_c_func(iup, "+IupGetColor", {I,I,P,P,P}, I),
--      xIupGetParam                      = define_c_func(iup, "+IupGetParamv", {P,P,P,P,I,I,P}, I),
        xIupParamf                        = define_c_func(iup, "+IupParamf", {P}, P),
        xIupParamBox                      = define_c_func(iup, "+IupParamBox", {P,P,I}, P),
--      xIupLayoutDialog                  = define_c_func(iup, "+IupLayoutDialog", {P}, P),
        xIupElementPropertiesDialog       = define_c_func(iup, "+IupElementPropertiesDialog", {P}, P),
$

--if xIupSetCallback=0 then ?9/0 end if

public constant IUP_NAME = "IUP - Portable User Interface"
public constant IUP_DESCRIPTION = "Multi-platform Toolkit for Building Graphical User Interfaces"
public constant IUP_COPYRIGHT = "Copyright (C) 1994-2015 Tecgraf/PUC-Rio"
public constant IUP_VERSION = "3.16" /* bug fixes are reported only by IupVersion functions */
public constant IUP_VERSION_NUMBER = 316000
public constant IUP_VERSION_DATE = "2015/09/15" /* does not include bug fix releases */

/************************************************************************/
/*                   Common Flags and Return Values                     */
/************************************************************************/
--public constant IUP_ERROR = 1
--public constant IUP_NOERROR = 0
--public constant IUP_OPENED = -1
public constant IUP_INVALID = -1
public constant IUP_INVALID_ID = -10

/************************************************************************/
/*                        Main API                                      */
/************************************************************************/
----int IupOpen(int *argc, char ***argv);
--global procedure IupOpen()
--  if c_func(xIupOpen, {NULL,NULL})=IUP_ERROR then
--      ?9/0
--  end if
--end procedure
--
--public function IupOpen(atom argc, sequence argv = {})
--atom p_argv = allocate_string_pointer_array(argv)
--atom result = c_func(xIupOpen, {argc,p_argv})
--  free(p_argv)
--  return result
--end function
--
--void IupClose(void);
--public procedure IupClose()
--  c_proc(xIupClose, {})
--end procedure

--void IupImageLibOpen(void);
--public procedure IupImageLibOpen()
--  c_proc(xIupImageLibOpen, {})
--end procedure

--int IupLoopStep(void);
--public function IupLoopStep()
--atom result = c_func(xIupLoopStep, {})
--  return result
--end function

----int IupLoopStepWait(void);
--public function IupLoopStepWait()
--atom result = c_func(xIupLoopStepWait, {})
--  return result
--end function
--
----void IupFlush(void);
--public procedure IupFlush()
--  c_proc(xIupFlush, {})
--end procedure
--
----void IupExitLoop(void);
--public procedure IupExitLoop()
--  c_proc(xIupExitLoop, {})
--end procedure
--
----char* IupLoad(const char *filename);
--public function IupLoad(object filename = NULL)
--  if sequence(filename) then filename = allocate_string(filename) end if
--  atom ptr = c_func(xIupLoad, {filename})
--  sequence str = ""
--  if ptr!=NULL then str = peek_string(ptr) end if
--  return str
--end function
--
----char* IupLoadBuffer(const char *buffer);
--public function IupLoadBuffer(object buffer = NULL)
--  if sequence(buffer) then buffer = allocate_string(buffer) end if
--  atom ptr = c_func(xIupLoadBuffer, {buffer})
--  sequence str = ""
--  if ptr!=NULL then str = peek_string(ptr) end if
--  return str
--end function
--
----void IupSetLanguage(const char *lng);
--public procedure IupSetLanguage(object lng = NULL)
--  if sequence(lng) then lng = allocate_string(lng) end if
--  c_proc(xIupSetLanguage, {lng})
--end procedure
--
----char* IupGetLanguage(void);
--public function IupGetLanguage()
--atom ptr = c_func(xIupGetLanguage, {})
--sequence str = ""
--  if ptr!=NULL then str = peek_string(ptr) end if
--  return str
--end function
--
----void IupSetLanguageString(const char* name, const char* str);
--public procedure IupSetLanguageString(object name = NULL, object str = NULL)
--  if sequence(name) then name = allocate_string(name,1) end if
--  if sequence(str) then str = allocate_string(str) end if
--  c_proc(xIupSetLanguageString, {name,str})
--end procedure
--
----void IupStoreLanguageString(const char* name, const char* str);
--public procedure IupStoreLanguageString(object name = NULL, object str = NULL)
--  if sequence(name) then name = allocate_string(name,1) end if
--  if sequence(str) then str = allocate_string(str) end if
--  c_proc(xIupStoreLanguageString, {name,str})
--end procedure
--
----char* IupGetLanguageString(const char* name);
--public function IupGetLanguageString(object name = NULL)
--  if sequence(name) then name = allocate_string(name,1) end if
--  atom ptr = c_func(xIupGetLanguageString, {name})
--  sequence str = ""
--  if ptr!=NULL then str = peek_string(ptr) end if
--  return str
--end function
--
----void IupSetLanguagePack(Ihandle* ih);
--public procedure IupSetLanguagePack(atom ih)
--  c_proc(xIupSetLanguagePack, {ih})
--end procedure

----Icallback IupGetFunction(const char *name);
--public function IupGetFunction(object name = NULL)
--  if sequence(name) then name = allocate_string(name,1) end if
--  atom result = c_func(xIupGetFunction, {name})
--  return result
--end function
--
----Icallback IupSetFunction(const char *name, Icallback func);
--public function IupSetFunction(object name, atom func)
--  if sequence(name) then name = allocate_string(name,1) end if
--  atom result = c_func(xIupSetFunction, {name,func})
--  return result
--end function

----int IupClassMatch(Ihandle* ih, const char* classname);
--public function IupClassMatch(atom ih, string classname)
--  integer result = c_func(xIupClassMatch, {ih,classname})
--  return result   -- true(1) or false(0)
----boolean?
--end function

/************************************************************************/
/*                        Elements                                      */
/************************************************************************/

--Ihandle* IupScrollBox(Ihandle* child);
public function IupScrollBox(atom child)
atom result = c_func(xIupScrollBox, {child})
    return result
end function

--Ihandle* IupExpander(Ihandle *child);
public function IupExpander(atom child)
atom result = c_func(xIupExpander, {child})
    return result
end function

--Ihandle* IupDetachBox(Ihandle *child);
public function IupDetachBox(atom child)
atom result = c_func(xIupDetachBox, {child})
    return result
end function

--Ihandle* IupBackgroundBox(Ihandle *child);
public function IupBackgroundBox(atom child)
atom result = c_func(xIupBackgroundBox, {child})
    return result
end function

--Ihandle* IupLink(const char* url, const char* title);
public function IupLink(object url = NULL, object title = NULL)
    if sequence(url) then url = allocate_string(url) end if
    if sequence(title) then title = allocate_string(title) end if
    atom result = c_func(xIupLink, {url,title})
    return result
end function

--Ihandle* IupFlatButton(const char* title);
public function IupFlatButton(object title = NULL)
    if sequence(title) then title = allocate_string(title) end if
    atom result = c_func(xIupFlatButton, {title})
    return result
end function

/************************************************************************/
/*                      Utilities                                       */
/************************************************************************/

--void IupStoreAttributeId(Ihandle *ih, const char* name, int id, const char *v);
public procedure IupStoreAttributeId(atom ih, object name, atom id, object v = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    if sequence(v) then v = allocate_string(v) end if
    c_proc(xIupStoreAttributeId, {ih,name,id,v})
end procedure

--void IupStoreAttributeId2(Ihandle* ih, const char* name, int lin, int col, const char* v);
public procedure IupStoreAttributeId2(atom ih, object name, atom lin, atom col, object v = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    if sequence(v) then v = allocate_string(v) end if
    c_proc(xIupStoreAttributeId2, {ih,name,lin,col,v})
end procedure

/* IupTree utilities */
--int IupTreeSetUserId(Ihandle* ih, int id, void* userid);
public function IupTreeSetUserId(atom ih, atom id, atom userid)
atom result = c_func(xIupTreeSetUserId, {ih,id,userid})
    return result
end function

--void* IupTreeGetUserId(Ihandle* ih, int id);
public function IupTreeGetUserId(atom ih, atom id)
atom result = c_func(xIupTreeGetUserId, {ih,id})
    return result
end function

--int IupTreeGetId(Ihandle* ih, void *userid);
public function IupTreeGetId(atom ih, atom userid)
atom result = c_func(xIupTreeGetId, {ih,userid})
    return result
end function

--void IupTreeSetAttributeHandle(Ihandle* ih, const char* name, int id, Ihandle* ih_named);
public procedure IupTreeSetAttributeHandle(atom ih, object name, atom id, atom ih_named)
    if sequence(name) then name = allocate_string(name,1) end if
    c_proc(xIupTreeSetAttributeHandle, {ih,name,id,ih_named})
end procedure

/* DEPRECATED IupTree utilities, use Iup*AttributeId functions. It will be removed in a future version.  */
--void IupTreeSetAttribute(Ihandle* ih, const char* name, int id, const char* v);
public procedure IupTreeSetAttribute(atom ih, object name, atom id, object v = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    if sequence(v) then v = allocate_string(v) end if
    c_proc(xIupTreeSetAttribute, {ih,name,id,v})
end procedure

--void IupTreeStoreAttribute(Ihandle* ih, const char* name, int id, const char* v);
public procedure IupTreeStoreAttribute(atom ih, object name, atom id, object v = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    if sequence(v) then v = allocate_string(v) end if
    c_proc(xIupTreeStoreAttribute, {ih,name,id,v})
end procedure

--char* IupTreeGetAttribute(Ihandle* ih, const char* name, int id);
public function IupTreeGetAttribute(atom ih, object name, atom id)
    if sequence(name) then name = allocate_string(name,1) end if
    atom ptr = c_func(xIupTreeGetAttribute, {ih,name,id})
    sequence str = ""
    if ptr!=NULL then str = peek_string(ptr) end if
    return str
end function

--int IupTreeGetInt(Ihandle* ih, const char* name, int id);
public function IupTreeGetInt(atom ih, object name, atom id)
    if sequence(name) then name = allocate_string(name,1) end if
    atom result = c_func(xIupTreeGetInt, {ih,name,id})
    return result
end function

--float IupTreeGetFloat(Ihandle* ih, const char* name, int id);
public function IupTreeGetFloat(atom ih, object name, atom id)
    if sequence(name) then name = allocate_string(name,1) end if
    atom result = c_func(xIupTreeGetFloat, {ih,name,id})
    return result
end function

/* DEPRECATED font names. It will be removed in a future version.  */
--char* IupMapFont(const char *iupfont);
public function IupMapFont(object iupfont = NULL)
    if sequence(iupfont) then iupfont = allocate_string(iupfont) end if
    atom ptr = c_func(xIupMapFont, {iupfont})
    sequence str = ""
    if ptr!=NULL then str = peek_string(ptr) end if
    return str
end function

--char* IupUnMapFont(const char *driverfont);
public function IupUnMapFont(object driverfont = NULL)
    if sequence(driverfont) then driverfont = allocate_string(driverfont) end if
    atom ptr = c_func(xIupUnMapFont, {driverfont})
    sequence str = ""
    if ptr!=NULL then str = peek_string(ptr) end if
    return str
end function

/************************************************************************/
/*                      Pre-defined dialogs                           */
/************************************************************************/
--Ihandle* IupProgressDlg(void);
public function IupProgressDlg()
atom result = c_func(xIupProgressDlg, {})
    return result
end function

--Ihandle* IupParamf(const char* format);
public function IupParamf(object format = NULL)
    if sequence(format) then format = allocate_string(format) end if
    atom result = c_func(xIupParamf, {format})
    return result
end function

--Ihandle* IupParamBox(Ihandle* parent, Ihandle** params, int count);
public function IupParamBox(atom parent, atom params, atom count)
atom result = c_func(xIupParamBox, {parent,params,count})
    return result
end function

--Ihandle* IupElementPropertiesDialog(Ihandle* elem);
public function IupElementPropertiesDialog(atom elem)
atom result = c_func(xIupElementPropertiesDialog, {elem})
    return result
end function

/************************************************************************/
/*                   Callback Return Values                             */
/************************************************************************/
--public constant IUP_IGNORE = -1
--public constant IUP_DEFAULT = -2
--public constant IUP_CLOSE = -3
--public constant IUP_CONTINUE = -4
/************************************************************************/
/*           IupPopup and IupShowXY Parameter Values                    */
/************************************************************************/
--public constant IUP_CENTER = 0xFFFF /* 65535 */
--public constant IUP_LEFT = 0xFFFE /* 65534 */
--public constant IUP_RIGHT = 0xFFFD /* 65533 */
--public constant IUP_MOUSEPOS = 0xFFFC /* 65532 */
--public constant IUP_CURRENT = 0xFFFB /* 65531 */
--public constant IUP_CENTERPARENT = 0xFFFA /* 65530 */
--public constant IUP_TOP = IUP_LEFT
--public constant IUP_BOTTOM = IUP_RIGHT
/************************************************************************/
/*               SHOW_CB Callback Values                                */
/************************************************************************/
public enum
        IUP_SHOW = 0,
        IUP_RESTORE,
        IUP_MINIMIZE,
        IUP_MAXIMIZE,
        IUP_HIDE,
$
/************************************************************************/
/*               SCROLL_CB Callback Values                              */
/************************************************************************/
public enum
        IUP_SBUP = 0,
        IUP_SBDN,
        IUP_SBPGUP,
        IUP_SBPGDN,
        IUP_SBPOSV,
        IUP_SBDRAGV,
        IUP_SBLEFT,
        IUP_SBRIGHT,
        IUP_SBPGLEFT,
        IUP_SBPGRIGHT,
        IUP_SBPOSH,
        IUP_SBDRAGH,
$
/************************************************************************/
/*               Mouse Button Values and Macros                         */
/************************************************************************/
public constant IUP_BUTTON1 = '1'
public constant IUP_BUTTON2 = '2'
public constant IUP_BUTTON3 = '3'
public constant IUP_BUTTON4 = '4'
public constant IUP_BUTTON5 = '5'
/************************************************************************/
/*                      Pre-Defined Masks                               */
/************************************************************************/
public constant IUP_MASK_FLOAT = "[+/-]?(/d+/.?/d*|/./d+)"
public constant IUP_MASK_UFLOAT = "(/d+/.?/d*|/./d+)"
public constant IUP_MASK_EFLOAT = "[+/-]?(/d+/.?/d*|/./d+)([eE][+/-]?/d+)?"
public constant IUP_MASK_FLOATCOMMA = "[+/-]?(/d+/,?/d*|/,/d+)"
public constant IUP_MASK_UFLOATCOMMA = "(/d+/,?/d*|/,/d+)"
public constant IUP_MASK_INT = "[+/-]?/d+"
--public constant IUP_MASK_UINT = "/d+"
/* Old definitions for backward compatibility */
public constant IUPMASK_FLOAT = IUP_MASK_FLOAT
public constant IUPMASK_UFLOAT = IUP_MASK_UFLOAT
public constant IUPMASK_EFLOAT = IUP_MASK_EFLOAT
public constant IUPMASK_INT = IUP_MASK_INT
public constant IUPMASK_UINT = IUP_MASK_UINT
/************************************************************************/
--/*                     IupGetParam Callback situations                    */
--/************************************************************************/
--public constant IUP_GETPARAM_BUTTON1 = -1
--public constant IUP_GETPARAM_INIT = -2
--public constant IUP_GETPARAM_BUTTON2 = -3
--public constant IUP_GETPARAM_BUTTON3 = -4
--public constant IUP_GETPARAM_CLOSE = -5
--public constant IUP_GETPARAM_OK = IUP_GETPARAM_BUTTON1
--public constant IUP_GETPARAM_CANCEL = IUP_GETPARAM_BUTTON2
--public constant IUP_GETPARAM_HELP = IUP_GETPARAM_BUTTON3
/************************************************************************/
/*                   Record Input Modes                                 */
/************************************************************************/
--public enum
--      IUP_RECBINARY = 0,
--      IUP_RECTEXT,
--$
/************************************************************************/
/*              Replacement for the WinMain in Windows,                 */
/*        this allows the application to start from "main".             */
/*        Used only for Watcom.                                         */
/************************************************************************/

--atom zlib1 = open_dll({ "win32\\zlib1.dll", "??libiupim.so", "??libiupim.dylib" })
--if zlib1=0 then ?9/0 end if
--atom im = open_dll({ "win32\\im.dll", "??libiupim.so", "??libiupim.dylib" })
--if im=0 then ?9/0 end if
--atom iupim = open_dll({ "win32\\iupim.dll", "libiupim.so" })
--if iupim=0 then ?9/0 end if

--atom iup = open_dll({"bin\\iup.dll", "libiup.so"})
--if iup=0 then ?9/0 end if

--public constant -- function delcarations
--  xIupConfig                        = define_c_func(iup, "+IupConfig", {}, P),
--  xIupConfigLoad                    = define_c_func(iup, "+IupConfigLoad", {P}, I),
--  xIupConfigSave                    = define_c_func(iup, "+IupConfigSave", {P}, I),
--  xIupConfigSetVariableStr          = define_c_proc(iup, "+IupConfigSetVariableStr", {P,P,P,P}),
--  xIupConfigSetVariableStrId        = define_c_proc(iup, "+IupConfigSetVariableStrId", {P,P,P,I,P}),
--  xIupConfigSetVariableInt          = define_c_proc(iup, "+IupConfigSetVariableInt", {P,P,P,I}),
--  xIupConfigSetVariableIntId        = define_c_proc(iup, "+IupConfigSetVariableIntId", {P,P,P,I,I}),
--  xIupConfigSetVariableDouble       = define_c_proc(iup, "+IupConfigSetVariableDouble", {P,P,P,D}),
--  xIupConfigSetVariableDoubleId     = define_c_proc(iup, "+IupConfigSetVariableDoubleId", {P,P,P,I,D}),
--  xIupConfigGetVariableStr          = define_c_func(iup, "+IupConfigGetVariableStr", {P,P,P}, P),
--  xIupConfigGetVariableStrId        = define_c_func(iup, "+IupConfigGetVariableStrId", {P,P,P,I}, P),
--  xIupConfigGetVariableInt          = define_c_func(iup, "+IupConfigGetVariableInt", {P,P,P}, I),
--  xIupConfigGetVariableIntId        = define_c_func(iup, "+IupConfigGetVariableIntId", {P,P,P,I}, I),
--  xIupConfigGetVariableDouble       = define_c_func(iup, "+IupConfigGetVariableDouble", {P,P,P}, D),
--  xIupConfigGetVariableDoubleId     = define_c_func(iup, "+IupConfigGetVariableDoubleId", {P,P,P,I}, D),
--  xIupConfigGetVariableStrDef       = define_c_func(iup, "+IupConfigGetVariableStrDef", {P,P,P,P}, P),
--  xIupConfigGetVariableStrIdDef     = define_c_func(iup, "+IupConfigGetVariableStrIdDef", {P,P,P,I,P}, P),
--  xIupConfigGetVariableIntDef       = define_c_func(iup, "+IupConfigGetVariableIntDef", {P,P,P,I}, I),
--  xIupConfigGetVariableIntIdDef     = define_c_func(iup, "+IupConfigGetVariableIntIdDef", {P,P,P,I,I}, I),
--  xIupConfigGetVariableDoubleDef    = define_c_func(iup, "+IupConfigGetVariableDoubleDef", {P,P,P,D}, D),
--  xIupConfigGetVariableDoubleIdDef  = define_c_func(iup, "+IupConfigGetVariableDoubleIdDef", {P,P,P,I,D}, D),
--  xIupConfigRecentInit              = define_c_proc(iup, "+IupConfigRecentInit", {P,P,P,I}),
--  xIupConfigRecentUpdate            = define_c_proc(iup, "+IupConfigRecentUpdate", {P,P}),
--  xIupConfigDialogShow              = define_c_proc(iup, "+IupConfigDialogShow", {P,P,P}),
--  xIupConfigDialogClosed            = define_c_proc(iup, "+IupConfigDialogClosed", {P,P,P}),
--  $


atom iup_scintilla = 0, xIupScintillaOpen, xIupScintilla, xIupScintillaSendMessage

-- (gets called automatically anyway)
global procedure IupScintillaOpen()
    if iup_scintilla=0 then
        iup_scintilla = iup_open_dll({"iup_scintilla.dll",
                                      "libiup_scintilla.so",
                                      "libiup_scintilla.dylib"})
        xIupScintillaOpen        = define_c_proc(iup_scintilla, "IupScintillaOpen", {})
        xIupScintilla            = define_c_func(iup_scintilla, "IupScintilla", {}, P)
        xIupScintillaSendMessage = define_c_func(iup_scintilla, "IupScintillaSendMessage", {P,U,P,P}, P)
        c_proc(xIupScintillaOpen, {})
    end if
end procedure

global function IupScintilla()
    IupScintillaOpen()
    Ihandle ih = c_func(xIupScintilla, {})
    return ih
end function

--sptr_t IupScintillaSendMessage(Ihandle* ih, unsigned int iMessage, uptr_t wParam, sptr_t lParam);
global function IupScintillaSendMessage(Ihandle ih, atom iMessage, atom wParam, atom lParam)
--  IupScintillaOpen() -- nah, lets have xIupScintillaSendMessage unassigned (in which case ih=garbage).
    atom res = c_func(xIupScintillaSendMessage, {ih,iMessage,wParam,lParam})
    return res
end function

--
 -- pIUP.e (\pIUP)
------------------
-- ======
--


constant
--  xIupOpen = define_c_func(iup, "IupOpen", {P,P},I),
--  xIupControlsOpen = define_c_proc(iupControls, "IupControlsOpen", {}),
--  xIupClose = define_c_proc(iup, "IupClose", {}),
--  xIupVersion = define_c_func(iup, "IupVersion", {},P),
--  xIupLoad = define_c_func(iup, "IupLoad", {P},P),
--  xIupLoadBuffer = define_c_func(iup, "IupLoadBuffer", {P},P),
--  xIupSetLanguage = define_c_proc(iup, "IupSetLanguage", {P}),
--  xIupGetLanguage = define_c_func(iup, "IupGetLanguage", {},P),
--  xIupGetText = define_c_func(iup, "IupGetText", {P,P},P),
--  xIupListDialog = define_c_func(iup, "IupListDialog", {I,P,I,P,I,I,I,P},I),
--  xIupLayoutDialog = define_c_func(iup, "IupLayoutDialog", {P},P),
--  xIupDialog = define_c_func(iup, "IupDialog", {P},P),
--  xIupPopup = define_c_func(iup, "IupPopup", {P,I,I},I),
--  xIupShow = define_c_func(iup, "IupShow", {P},I),
--  xIupShowXY = define_c_func(iup, "IupShowXY", {P,I,I},I),
--  xIupHide = define_c_proc(iup, "IupHide", {P}),
--  xIupRefresh = define_c_proc(iup, "IupRefresh", {P}),
--  xIupRefreshChildren = define_c_proc(iup, "IupRefreshChildren", {P}),
--  xIupUpdate = define_c_proc(iup, "IupUpdate", {P}),
--  xIupUpdateChildren = define_c_proc(iup, "IupUpdate", {P}),
--  xIupRedraw = define_c_proc(iup, "IupRedraw", {P,I}),
--  xIupConvertXYToPos = define_c_func(iup, "IupConvertXYToPos", {P,I,I},I),
--  xIupNextField = define_c_func(iup, "IupNextField", {P},P),
--  xIupPreviousField = define_c_func(iup, "IupPreviousField", {P},P),
--  xIupGetFocus = define_c_func(iup, "IupGetFocus", {},P),
--  xIupSetFocus = define_c_proc(iup, "IupSetFocus", {P}),
--  xIupGetCallback = define_c_func(iup, "IupGetCallback", {P,P},P),
--  xIupSetCallback = define_c_func(iup, "IupSetCallback", {P,P,P},P),

--  xIupSetAttribute = define_c_proc(iup, "IupSetAttribute", {P,P,P}),
--  xIupSetAttributes = define_c_proc(iup, "IupSetAttributes", {P,P}),
--  xIupSetAttributeHandle = define_c_proc(iup, "IupSetAttributeHandle", {P,P,P}),
--  xIupSetStrAttribute = define_c_proc(iup, "IupSetStrAttribute", {P,P,P}),
--  xIupSetInt = define_c_proc(iup, "IupSetInt", {P,P,I}),
--  xIupStoreAttribute = define_c_proc(iup, "IupStoreAttribute", {P,P,P}),
--  xIupResetAttribute = define_c_proc(iup, "IupResetAttribute", {P,P}),
--  xIupGetAttribute = define_c_func(iup, "IupGetAttribute", {P,P},P),
--  xIupGetAttributes = define_c_func(iup, "IupGetAttributes", {P},P),
--  xIupGetAttributeHandle = define_c_func(iup, "IupGetAttributeHandle", {P,P},P),
--  xIupGetAllAttributes = define_c_func(iup, "IupGetAllAttributes", {P,P,I},I),
--  xIupGetFloat = define_c_func(iup, "IupGetFloat", {P,P},F),
--  xIupGetInt = define_c_func(iup, "IupGetInt", {P,P},I),
--  xIupGetInt2 = define_c_func(iup, "IupGetInt", {P,P},I),
--  xIupGetIntInt = define_c_func(iup, "IupGetIntInt", {P,P,P,P},I),
--  xIupGetGlobal = define_c_func(iup, "IupGetGlobal", {P},P),
--  xIupStoreGlobal = define_c_proc(iup, "IupStoreGlobal", {P,P}),

--  xIupAlarm = define_c_func(iup, "IupAlarm", {P,P,P,P,P},I),
--  xIupMessage  = define_c_proc(iup, "IupMessage", {P,P}),
--  xIupButton = define_c_func(iup, "IupButton", {P,P},P),
--  xIupCanvas = define_c_func(iup, "IupCanvas", {P},P),
--  xIupFill  = define_c_func(iup, "IupFill", {},P),
--  xIupFrame = define_c_func(iup, "IupFrame", {P},P),
--  xIupHboxv = define_c_func(iup, "IupHboxv", {P},P),
--  xIupImage = define_c_func(iup, "IupImage", {I,I,P},P),
--  xIupImageRGB = define_c_func(iup, "IupImageRGB", {I,I,P},P),
--  xIupImageRGBA = define_c_func(iup, "IupImageRGBA", {I,I,P},P),
--  xIupLoadImage = define_c_func(iupIm, "IupLoadImage", {P},P),
--  xIupSaveImage = define_c_func(iupIm, "IupSaveImage", {P,P,P},I),
--  xIupGetNativeHandleImage = define_c_func(iupIm, "IupGetNativeHandleImage", {P},P),
--  xIupGetImageNativeHandle = define_c_func(iupIm, "IupGetImageNativeHandle", {P},P),
--  xIupSaveImageAsText = define_c_func(iup, "IupSaveImageAsText", {P,P,P,P},I)
--  xIupLabel = define_c_func(iup, "IupLabel", {P},P),
--  xIupList = define_c_func(iup, "IupList", {P},P),
--  xIupMenuv = define_c_func(iup, "IupMenuv", {P},P),
--  xIupItem = define_c_func(iup, "IupItem",  {P,P},P),
--  xIupSeparator = define_c_func(iup, "IupSeparator", {},P),
--  xIupSubmenu = define_c_func(iup, "IupSubmenu", {P,P},P),
--  xIupProgressBar = define_c_func(iup, "IupProgressBar", {},P),
--  xIupRadio = define_c_func(iup, "IupRadio", {P},P),
--  xIupSpin = define_c_func(iup, "IupSpin", {},P),
--  xIupTabsv = define_c_func(iup, "IupTabsv", {P},P),
--  xIupText = define_c_func(iup, "IupText", {P},P),
--  xIupMultiLine = define_c_func(iup, "IupMultiLine", {P},P),
--  xIupTextConvertLinColToPos = define_c_proc(iup, "IupTextConvertLinColToPos", {P,I,I,P}),
--  xIupTextConvertPosToLinCol = define_c_proc(iup, "IupTextConvertPosToLinCol", {P,I,P,P}),
--  xIupTimer = define_c_func(iup, "IupTimer", {},P),
--  xIupToggle = define_c_func(iup, "IupToggle", {P,P},P),
--  xIupTree = define_c_func(iup, "IupTree", {},P),
--  xIupVal = define_c_func(iup, "IupVal", {P},P),
--  xIupVboxv = define_c_func(iup, "IupVboxv", {P},P),
--  xIupZboxv = define_c_func(iup, "IupZboxv", {P},P),

--  xIupUser = define_c_func(iup, "IupUser", {},P),
--  xIupMap = define_c_func(iup, "IupMap", {P},I),
--  xIupUnmap = define_c_proc(iup, "IupUnmap", {P}),
--  xIupMainLoop = define_c_proc(iup, "IupMainLoop", {}),
--  xIupMainLoopLevel = define_c_func(iup, "IupMainLoopLevel", {},I),
--  xIupLoopStep = define_c_proc(iup, "IupLoopStep", {}),
--  xIupExitLoop = define_c_proc(iup, "IupExitLoop", {}),
--  xIupFlush = define_c_proc(iup, "IupFlush", {}),
--  xIupGetActionName = define_c_func(iup, "IupGetActionName", {},P),
--  xIupNormalizerv = define_c_func(iup, "IupNormalizer", {P},P),
--  xIupCboxv = define_c_func(iup, "IupCboxv", {P},P),
--  xIupSbox  = define_c_func(iup, "IupSbox", {P},P),
--  xIupSplit = define_c_func(iup, "IupSplit", {P,P},P),
--  xIupCreate = define_c_func(iup, "IupCreate", {P},P),
--  xIupDestroy = define_c_proc(iup, "IupDestroy", {P}),
--  xIupFileDlg = define_c_func(iup, "IupFileDlg", {},P),
--  xIupMessageDlg = define_c_func(iup, "IupMessageDlg", {},P),
--  xIupColorDlg = define_c_func(iup, "IupColorDlg", {},P),
--  xIupFontDlg = define_c_func(iup, "IupFontDlg", {},P),
--  xIupGetColor = define_c_func(iup, "IupGetColor", {I,I,P,P,P},I),
--  xIupGetFile = define_c_func(iup, "IupGetFile", {P},I),
--  xIupGetParam = define_c_func(iup, "IupGetParam", {P,P,P,P,P,P,P,P,P,P,P,P,P,P,P},I),
--  xIupGetParam = define_c_func(iup, "IupGetParam", {P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P},I),
--  xIupRecordInput = define_c_func(iup, "IupRecordInput", {P,I},I),
--  xIupPlayInput = define_c_func(iup, "IupPlayInput", {P},I),

--  xIupSetHandle = define_c_proc(iup, "IupSetHandle", {P,P}),
--  xIupGetHandle = define_c_func(iup, "IupGetHandle", {P},P),
--  xIupGetName = define_c_func(iup, "IupGetName", {P},P),
--  xIupGetAllNames = define_c_func(iup, "IupGetAllNames", {P,I},I),
--  xIupGetAllDialogs = define_c_func(iup, "IupGetAllDialogs", {P,I},I),
--  xIupGetAllClasses = define_c_func(iup, "IupGetAllClasses", {P,I},I),
--  xIupGetClassName = define_c_func(iup, "IupGetClassName", {P},P),
--  xIupGetClassType = define_c_func(iup, "IupGetClassType", {P},P),
--  xIupGetClassAttributes = define_c_func(iup, "IupGetClassAttributes", {P,P,I},I),
--  xIupGetClassCallbacks = define_c_func(iup, "IupGetClassCallbacks", {P,P,I},I),
--  xIupSaveClassAttributes = define_c_proc(iup, "IupSaveClassAttributes", {P}),
--  xIupCopyClassAttributes = define_c_proc(iup, "IupCopyClassAttributes", {P,P}),
--  xIupSetClassDfltAttribute = define_c_proc(iup, "IupSetClassDefaultAttribute", {P,P,P}),
--  xIupSetGlobal = define_c_proc(iup, "IupSetGlobal", {P,P}),
--  xIupGetGlobal = define_c_func(iup, "IupGetGlobal", {P},P),
    xiupKeyCodeToName = define_c_func(iup, "iupKeyCodeToName", {I},P),
--  xIupAppend = define_c_func(iup, "IupAppend", {P,P},P),
--  xIupDetach = define_c_proc(iup, "IupDetach", {P}),
--  xIupInsert = define_c_func(iup, "IupInsert", {P,P,P},P),
--  xIupReparent = define_c_func(iup, "IupReparent", {P,P,P},P),
--  xIupGetParent = define_c_func(iup, "IupGetParent", {P},P),
--  xIupGetChild = define_c_func(iup, "IupGetChild", {P,I},P),
--  xIupGetChildPos = define_c_func(iup, "IupGetChildPos", {P,P},I),
--  xIupGetChildCount = define_c_func(iup, "IupGetChildCount", {P},I),
--  xIupGetNextChild = define_c_func(iup, "IupGetNextChild", {P,P},P),
--  xIupGetBrother = define_c_func(iup, "IupGetBrother", {P},P),
--  xIupGetDialog = define_c_func(iup, "IupGetDialog", {P},P),
--  xIupGetDialogChild = define_c_func(iup, "IupGetDialogChild", {P,P},P),
--  xIupClipboard = define_c_func(iup, "IupClipboard", {},P),

--  xIupHelp = define_c_func(iup, "IupHelp", {P},I),
    $


global function iupKeyCodeToName(atom ch)
    atom pKeyName = c_func(xiupKeyCodeToName,{ch})
    return peek_string(pKeyName)
end function

--DEV not working, not documented:
--/*
constant iupPPlot = iup_open_dll({
                                   "iup_pplot.dll",
                                   "libiup_pplot.so",
                                   "libiup_pplot.dylib"
                                  })

constant
    xIupPPlotOpen = define_c_proc(iupPPlot, "IupPPlotOpen", {}),
    xIupPPlot = define_c_func(iupPPlot, "IupPPlot", {},P),
    xIupPPlotBegin = define_c_proc(iupPPlot, "IupPPlotBegin", {P,I}),
    xIupPPlotAdd = define_c_proc(iupPPlot, "IupPPlotAdd", {P,F,F}),
--  xIupPPlotAddStr = define_c_proc(iupPPlot, "IupPPlotAddStr", {P,P,F}),
    xIupPPlotEnd = define_c_proc(iupPPlot, "IupPPlotEnd", {P}),
--  xIupPPlotInsert = define_c_proc(iupPPlot, "IupPPlotInsert", {P,I,I,F,F}),
--  xIupPPlotInsertStr = define_c_proc(iupPPlot, "IupPPlotInsertStr", {P,I,I,P,F}),
--  xIupPPlotTransform = define_c_proc(iupPPlot, "IupPPlotTransform", {P,F,F,P,P}),
--  xIupPPlotPaintTo = define_c_proc(iupPPlot, "IupPPlotPaintTo", {P,P}),
    $

integer did_pplot_open = 0

global function IupPPlot(string attributes = "", sequence data = {})
    if did_pplot_open=0 then
        c_proc(xIupPPlotOpen, {})
        did_pplot_open = 1
    end if
    Ihandle ih = c_func(xIupPPlot, {})
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global procedure IupPPlotBegin(Ihandle ih, integer str_xdata)
    c_proc(xIupPPlotBegin, {ih, str_xdata})
end procedure

global procedure IupPPlotAdd(Ihandle ih, atom x, atom y)
    c_proc(xIupPPlotAdd, {ih, x, y})
--atom pX = allocate(4), pY = allocate(4)
--  iup_poke_double(pX,x)
--  iup_poke_double(pY,y)
--  c_proc(xIupPPlotAdd, {ih, pX, pY})
end procedure

--global procedure add_str(Ihandle ih, string x, atom y)
--  c_proc(xIupPPlotAddStr, {ih, x, y})
--end procedure

global procedure IupPPlotEnd(Ihandle ih)
    c_proc(xIupPPlotEnd, {ih})
end procedure

--global procedure insert_plot(Ihandle ih, integer index, integer sample_index, atom x, atom y)
--  c_proc(xIupPPlotInsert, {ih, index, sample_index, x, y})
--end procedure

--global procedure insert_str(Ihandle ih, integer index, integer sample_index, string x, atom y)
--  c_proc(xIupPPlotInsertStr, {ih, index, sample_index, x, y})
--end procedure

--global function transform_plot(Ihandle ih, atom x, atom y)
--atom pX = allocate(4), 
--   pY = allocate(4)
--  c_proc(xIupPPlotTransform, {ih, x, y, pX, pY})
--  x = iup_peek_double(pX)
--  y = iup_peek_double(pY)
--  free(pX)
--  free(pY)
--  return {x, y}
--end function

--global procedure paint_to(Ihandle ih, atom cnv)
--  c_proc(xIupPPlotPaintTo, {ih, cnv})
--end procedure
--*/


--dev WIERD ERROR...
--constant atom hCd = iup_open_dll({"cd.dll","libcd.so","libcd.dylib"})
--constant atom hCdIup = iup_open_dll({"iupcd.dll","libiupcd.so","libiupcd.dylib"})

constant
--  xcdVersion = define_c_func(hCd, "cdVersion", {},P),
--  xcdVersionDate = define_c_func(hCd, "cdVersionDate", {},P),
--  xcdVersionNumber = define_c_func(hCd, "cdVersionNumber", {},I),
--  xcdCanvasSetBackground = define_c_proc(hCd, "cdCanvasSetBackground", {P,I}),
--  xcdCanvasSetForeground = define_c_proc(hCd, "cdCanvasSetForeground", {P,I}),
--  xcdCanvasBackground = define_c_func(hCd, "cdCanvasBackground", {P,L},L),
--  xcdCanvasForeground = define_c_func(hCd, "cdCanvasForeground", {P,L},L),
--  xcdCanvasBackOpacity = define_c_func(hCd, "cdCanvasBackOpacity", {P,I},I),
--  xcdCanvasWriteMode = define_c_func(hCd, "cdCanvasWriteMode", {P,I},I),
--  xcdCanvasLineStyle = define_c_func(hCd, "cdCanvasLineStyle", {P,I},I),
--  xcdCanvasLineStyleDashes = define_c_proc(hCd, "cdCanvasLineStyleDashes", {P,P,I}),
--  xcdCanvasLineWidth = define_c_func(hCd, "cdCanvasLineWidth", {P,I},I),
--  xcdCanvasLineJoin = define_c_func(hCd, "cdCanvasLineJoin", {P,I},I),
--  xcdCanvasLineCap = define_c_func(hCd, "cdCanvasLineCap", {P,I},I),
--  xcdCanvasInteriorStyle = define_c_func(hCd, "cdCanvasInteriorStyle", {P,I},I),
--  xcdCanvasHatch = define_c_func(hCd, "cdCanvasHatch", {P,I},I),
--  xcdCanvasStipple = define_c_proc(hCd, "cdCanvasStipple", {P,I,I,P}),
--  xcdCanvasGetStipple = define_c_func(hCd, "cdCanvasGetStipple", {P,P,P},P),
--  xcdCanvasPattern = define_c_proc(hCd, "cdCanvasPattern", {P,I,I,P}),
--  xcdCanvasGetPattern = define_c_func(hCd, "cdCanvasGetPattern", {P,P,P},P),
--  xcdCanvasFillMode = define_c_func(hCd, "cdCanvasFillMode", {P,I},I),
--  xcdCanvasFont = define_c_proc(hCd, "cdCanvasFont", {P,P,I,I}),
--  xcdCanvasGetFont = define_c_proc(hCd, "cdCanvasGetFont", {P,P,P,P}),
--  xcdCanvasNativeFont = define_c_func(hCd, "cdCanvasNativeFont", {P,P},P),
--  xcdCanvasTextAlignment = define_c_func(hCd, "cdCanvasTextAlignment", {P,I},I),
--  xcdCanvasTextOrientation = define_c_func(hCd, "cdCanvasTextOrientation", {P,D},D),
--  xcdCanvasMarkType = define_c_func(hCd, "cdCanvasMarkType", {P,I},I),
--  xcdCanvasMarkSize = define_c_func(hCd, "cdCanvasMarkSize", {P,I},I),
--  xcdCanvasPixel = define_c_proc(hCd, "cdCanvasPixel", {P,I,I}),
--  xcdCanvasMark = define_c_proc(hCd, "cdCanvasMark", {P,I,I}),
--  xcdCanvasLine = define_c_proc(hCd, "cdCanvasLine", {P,I,I,I,I}),
--  xcdCanvasBegin = define_c_proc(hCd, "cdCanvasBegin", {P,I}),
--  xcdCanvasVertex = define_c_proc(hCd, "cdCanvasVertex", {P,I,I}),
--  xcdCanvasEnd = define_c_proc(hCd, "cdCanvasEnd", {P}),
--  xcdCanvasRect = define_c_proc(hCd, "cdCanvasRect", {P,I,I,I,I}),
--  xcdCanvasBox = define_c_proc(hCd, "cdCanvasBox", {P,I,I,I,I}),
--  xcdCanvasArc = define_c_proc(hCd, "cdCanvasArc", {P,I,I,I,I,D,D}),
--  xcdCanvasSector = define_c_proc(hCd, "cdCanvasSector", {P,I,I,I,I,D,D}),
--  xcdCanvasChord = define_c_proc(hCd, "cdCanvasChord", {P,I,I,I,I,D,D}),
--  xcdCanvasText = define_c_proc(hCd, "cdCanvasText", {P,I,I,P}),
--  xcdContextRegisterCallback = define_c_func(hCd, "cdContextRegisterCallback", {P,I,P},I),
--  xcdCanvasPlay = define_c_func(hCd, "cdCanvasPlay", {P,P,I,I,I,I,P},I),
--  xcdCanvasActivate = define_c_func(hCd, "cdCanvasActivate", {P},I),
--  xcdCanvasDeactivate = define_c_proc(hCd, "cdCanvasDeactivate", {P}),
--  xcdUseContextPlus = define_c_func(hCd, "cdUseContextPlus", {I},I),
--  xcdInitContextPlus = define_c_proc(hCd, "cdInitContextPlus", {}),
--  xcdCanvasSimulate = define_c_func(hCd, "cdCanvasSimulate", {P,I},I),
--  xcdCanvasFlush = define_c_proc(hCd, "cdCanvasFlush", {P}),
--  xcdCanvasClear = define_c_proc(hCd, "cdCanvasClear", {P}),
--  xcdCanvasSaveState = define_c_func(hCd, "cdCanvasSaveState", {P},P),
--  xcdCanvasRestoreState = define_c_proc(hCd, "cdCanvasRestoreState", {P,P}),
--  xcdCanvasReleaseState = define_c_proc(hCd, "cdReleaseState", {P}),
--  xcdCanvasSetAttribute = define_c_proc(hCd, "cdCanvasSetAttribute", {P,P,P}),
    --void     cdCanvasSetfAttribute(cdCanvas* canvas, const char* name, const char* format, ...);
--  xcdCanvasGetAttribute = define_c_func(hCd, "cdCanvasGetAttribute", {P,P},P),
--  xcdCanvasGetColorPlanes = define_c_func(hCd, "cdCanvasGetColorPlanes", {P},I),
--  xcdCanvasGetContext = define_c_func(hCd, "cdCanvasGetContext", {P},P),
--  xcdCanvasGetSize = define_c_proc(hCd, "cdCanvasGetSize", {P,I,I,P,P}),
--  xcdCanvasUpdateYAxis = define_c_func(hCd, "cdCanvasUpdateYAxis", {P,P},I),
--  xcdfCanvasUpdateYAxis = define_c_func(hCd, "cdfCanvasUpdateYAxis", {P,P},D),
--  xcdCanvasInvertYAxis = define_c_func(hCd, "cdCanvasInvertYAxis", {P,I},I),
--  xcdfCanvasInvertYAxis = define_c_func(hCd, "cdfCanvasInvertYAxis", {P,D},D),
--  xcdCanvasMM2Pixel = define_c_proc(hCd, "cdCanvasMM2Pixel", {P,D,D,P,P}),
--  xcdCanvasPixel2MM = define_c_proc(hCd, "cdCanvasPixel2MM", {P,I,I,P,P}),
--  xcdfCanvasMM2Pixel = define_c_proc(hCd, "cdfCanvasMM2Pixel", {P,D,D,P,P}),
--  xcdfCanvasPixel2MM = define_c_proc(hCd, "cdfCanvasPixel2MM", {P,D,D,P,P}),
--  xcdCanvasOrigin = define_c_proc(hCd, "cdCanvasOrigin", {P,I,I}),
--  xcdfCanvasOrigin = define_c_proc(hCd, "cdfCanvasOrigin", {P,D,D}),
--  xcdCanvasGetOrigin = define_c_proc(hCd, "cdCanvasGetOrigin", {P,P,P}),
--  xcdfCanvasGetOrigin = define_c_proc(hCd, "cdfCanvasGetOrigin", {P,P,P}),
--  xcdCanvasTransform = define_c_proc(hCd, "cdCanvasTransform", {P,P}),
--  xcdCanvasGetTransform = define_c_func(hCd, "cdCanvasGetTransform", {P},P),
--  xcdCanvasTransformMultiply = define_c_proc(hCd, "cdCanvasTransformMultiply", {P,P}),
--  xcdCanvasTransformRotate = define_c_proc(hCd, "cdCanvasTransformRotate", {P,D}),
--  xcdCanvasTransformScale = define_c_proc(hCd, "cdCanvasTransformScale", {P,D,D}),
--  xcdCanvasTransformTranslate = define_c_proc(hCd, "cdCanvasTransformTranslate", {P,D,D}),
--  xcdCanvasTransformPoint = define_c_proc(hCd, "cdCanvasTransformPoint", {P,I,I,P,P}),
--  xcdfCanvasTransformPoint = define_c_proc(hCd, "cdfCanvasTransformPoint", {P,D,D,P,P}),
--  xcdCanvasVectorText = define_c_proc(hCd, "cdCanvasVectorText", {P,I,I,P}),
--  xcdCanvasMultiLineVectorText = define_c_proc(hCd, "cdCanvasMultiLineVectorText", {P,I,I,P}),
--  xcdCanvasGetVectorTextSize = define_c_proc(hCd, "cdCanvasGetVectorTextSize", {P,P,P,P}),
--  xcdCanvasGetVectorTextBounds = define_c_proc(hCd, "cdCanvasGetVectorTextBounds", {P,P,I,I,P}),
--  xwdCanvasGetVectorTextBounds = define_c_proc(hCd, "wdCanvasGetVectorTextBounds", {P,P,D,D,P}),
--  xcdCanvasGetFontDim = define_c_proc(hCd, "cdCanvasGetFontDim", {P,P,P,P,P}),
--  xcdCanvasGetTextSize = define_c_proc(hCd, "cdCanvasGetTextSize", {P,P,P,P}),
--  xcdCanvasGetTextBox = define_c_proc(hCd, "cdCanvasGetTextBox", {P,P,I,I,P,P,P,P}),
--  xcdCanvasGetTextBox = define_c_proc(hCd, "cdCanvasGetTextBox", {P,I,I,P,P,P,P,P}),
--  xcdfCanvasGetTextBox = define_c_proc(hCd, "cdfCanvasGetTextBox", {P,D,D,P,P,P,P,P}),
--  xwdCanvasGetTextBox = define_c_proc(hCd, "wdCanvasGetTextBox", {P,D,D,P,P,P,P,P}),
--  xcdCanvasGetTextBounds = define_c_proc(hCd, "cdCanvasGetTextBounds", {P,I,I,P,P}),
--  xcdfCanvasGetTextBounds = define_c_proc(hCd, "cdCanvasGetTextBounds", {P,D,D,P,P}),
--  xwdCanvasGetTextBounds = define_c_proc(hCd, "wdCanvasGetTextBounds", {P,D,D,P,P}),
--  xcdCanvasGetImageRGB = define_c_proc(hCd, "cdCanvasGetImageRGB", {P,P,P,P,I,I,I,I}),
    xwdCanvasGetImageRGB = define_c_proc(hCd, "wdCanvasGetImageRGB", {P,P,P,P,D,D,I,I})
--  xcdCanvasPutImageRectRGB = define_c_proc(hCd, "cdCanvasPutImageRectRGB", {P,I,I,P,P,P,I,I,I,I,I,I,I,I}),
--  xcdCanvasPutImageRectRGBA = define_c_proc(hCd, "cdCanvasPutImageRectRGBA", {P,I,I,P,P,P,P,I,I,I,I,I,I,I,I}),
--  xcdCanvasPutImageRectMap = define_c_proc(hCd, "cdCanvasPutImageRectMap", {P,I,I,P,P,I,I,I,I,I,I,I,I}),
--  xwdCanvasWindow = define_c_proc(hCd, "wdCanvasWindow", {P,D,D,D,D}),
--  xwdCanvasGetWindow = define_c_proc(hCd, "wdCanvasGetWindow", {P,P,P,P,P}),
--  xwdCanvasViewport = define_c_proc(hCd, "wdCanvasViewport", {P,I,I,I,I}),
--  xwdCanvasGetViewport = define_c_proc(hCd, "wdCanvasGetViewport", {P,P,P,P,P}),
--  xwdCanvasWorld2Canvas = define_c_proc(hCd, "wdCanvasWorld2Canvas", {P,D,D,P,P}),
--  xwdCanvasWorld2CanvasSize = define_c_proc(hCd, "wdCanvasWorld2CanvasSize", {P,D,D,P,P}),
--  xwdCanvasCanvas2World = define_c_proc(hCd, "wdCanvasCanvas2World", {P,I,I,D,D}),
--  xcdContextCaps = define_c_func(hCd, "cdContextCaps", {P},UL),
--  xcdCreateCanvas = define_c_func(hCd, "cdCreateCanvas", {P,P},P),
--  xcdKillCanvas = define_c_proc(hCd, "cdKillCanvas", {P}),
--  xcdContextIup = define_c_func(hCdIup, "cdContextIup", {},P),
--  xcdContextPrinter = define_c_func(hCd, "cdContextPrinter", {},P),
--  xcdContextPS = define_c_func(hCd, "cdContextPS", {},P),
--  xcdContextPicture = define_c_func(hCd, "cdContextPicture", {},P)
--
--global constant
--  CD_IUP = c_func(xcdContextIup, {}),
--  CD_PRINTER = c_func(xcdContextPrinter, {}),
--  CD_PS = c_func(xcdContextPS, {}),
--  CD_PICTURE = c_func(xcdContextPicture, {})
--/*
todo:
C:\Program Files (x86)\Phix\demo\tee\simple_paint.exw:898   rgb_canvas = cdCreateCanvas(CD_IMIMAGE, image);
C:\Program Files (x86)\Phix\demo\tee\simple_paint.exw:1044   cd_canvas = cdCreateCanvasf(CD_GL, "10x10 %g", res);
C:\Program Files (x86)\Phix\demo\tee\simple_paint.exw:1049   cd_canvas = cdCreateCanvas(CD_IUPDBUFFER, canvas);
C:\Program Files (x86)\Phix\demo\tee\simple_paint.exw:1222               cdCanvas* rgb_canvas = cdCreateCanvas(CD_IMIMAGE, image);
--*/

--global procedure cdCanvasTransform(cdCanvas hCdCanvas, object matrix=NULL)
--atom pMatrix = NULL
--  if matrix!=NULL then
--      if length(matrix)!=6 then ?9/0 end if
--      pMatrix = allocate(6*8)
--      iup_poke_double(pMatrix, matrix)
--  end if
--  c_proc(xcdCanvasTransform, {hCdCanvas, pMatrix})
--  if matrix!=NULL then
--      free(pMatrix)
--  end if
--end procedure

--global procedure cdCanvasTransformMultiply(cdCanvas hCdCanvas, sequence matrix)
--atom pMatrix
--  if length(matrix)!=6 then ?9/0 end if
--  pMatrix = allocate(6*8)
--  iup_poke_double(pMatrix, matrix)
--  c_proc(xcdCanvasTransformMultiply, {hCdCanvas, pMatrix})
--  free(pMatrix)
--end procedure
--
--global procedure canvas_transform_rotate(cdCanvas hCdCanvas, atom angle)
--  c_proc(xcdCanvasTransformRotate, {hCdCanvas, angle})
--end procedure
--
--global procedure canvas_transform_scale(cdCanvas hCdCanvas, atom sx, atom sy)
--  c_proc(xcdCanvasTransformScale, {hCdCanvas, sx, sy})
--end procedure
--
--global procedure canvas_transform_translate(cdCanvas hCdCanvas, atom dx, atom dy)
--  c_proc(xcdCanvasTransformTranslate, {hCdCanvas, dx, dy})
--end procedure

--global procedure canvas_vector_text(cdCanvas hCdCanvas, atom x, atom y, sequence text)
--atom pText
--  pText = allocate_string(text)
--  c_proc(xcdCanvasVectorText, {hCdCanvas, x, y, pText})
--  free(pText)
--end procedure

global function wdCanvasGetImageRGB(cdCanvas hCdCanvas, atom x, atom y, atom w, atom h)
atom pR, pG, pB
sequence r,g,b

    pR = allocate(3*w*h)
    pG = pR+w*h
    pB = pG+w*h
    c_proc(xwdCanvasGetImageRGB, {hCdCanvas, pR, pG, pB, x, y, w, h})
    r = peek({pR, w*h})
    g = peek({pG, w*h})
    b = peek({pB, w*h})
    free(pR)
    return {r,g,b}
end function

 -- vector text attributes
--------------------------
--constant
--  xcdCanvasVectorFont = define_c_func(hCd, "cdCanvasVectorFont", {P,P},P),
--  xcdCanvasVectorTextDirection = define_c_proc(hCd, "cdCanvasVectorTextDirection", {P,I,I,I,I}),
--  xcdCanvasVectorTextTransform = define_c_func(hCd, "cdCanvasVectorTextTransform", {P,P},P),
--  xcdCanvasVectorTextSize = define_c_proc(hCd, "cdCanvasVectorTextSize", {P,I,I,P}),
--  xcdCanvasVectorCharSize = define_c_func(hCd, "cdCanvasVectorCharSize", {P,I},I),
--  $

 -- world draw attributes
-------------------------
--constant 
--  xwdCanvasLineWidth = define_c_func(hCd, "wdCanvasLineWidth", {P,D},D),
--  xwdCanvasFont = define_c_proc(hCd, "wdCanvasFont", {P,P,I,D}),
--  xwdCanvasGetFont = define_c_proc(hCd, "wdCanvasGetFont", {P,P,P,P}),
--  xwdCanvasMarkSize = define_c_func(hCd, "wdCanvasMarkSize", {P,D},D),
--  xwdCanvasGetFontDim = define_c_proc(hCd, "wdCanvasGetFontDim", {P,P,P,P,P}),
--  xwdCanvasGetTextSize = define_c_proc(hCd, "wdCanvasGetTextSize", {P,P,P,P}),
--  xwdCanvasGetTextBox = define_c_proc(hCd, "wdCanvasGetTextBox", {P,D,D,P,P,P,P,P}),
--  xwdCanvasGetTextBounds = define_c_proc(hCd, "wdCanvasGetTextbounds", {P,D,D,P,P}),
--  xwdCanvasStipple = define_c_proc(hCd, "wdCanvasStipple", {P,I,I,P,D,D}),
--  xwdCanvasPattern = define_c_proc(hCd, "wdCanvasPattern", {P,I,I,P,D,D})
--  $

--
 -- pIUP.e (\tee)   [all done]
-----------------
--  ======
--
--constant
--  xIupOpen = define_c_func(iup, "IupOpen", {P,P},I),
--  xIupControlsOpen = define_c_proc(iupControls, "IupControlsOpen", {}),
--  xIupImageLibOpen = define_c_proc(iupimglib, "IupImageLibOpen", {}),
--  xIupClose = define_c_proc(iup, "IupClose", {}),
--  xIupDialog = define_c_func(iup, "IupDialog", {P},P),
--  xIupPopup = define_c_func(iup, "IupPopup", {P,I,I},I),
--  xIupShow = define_c_func(iup, "IupShow", {P},I),
--  xIupShowXY = define_c_func(iup, "IupShowXY", {P,I,I},I),
--  xIupHide = define_c_proc(iup, "IupHide", {P}),
--  xIupRefresh = define_c_proc(iup, "IupRefresh", {P}),
--  xIupSetFocus = define_c_proc(iup, "IupSetFocus", {P}),
--  xIupSetCallback = define_c_func(iup, "IupSetCallback", {P,P,P},P),

--  xIupSetAttribute = define_c_proc(iup, "IupSetAttribute", {P,P,P}),
--  xIupSetAttributes = define_c_proc(iup, "IupSetAttributes", {P,P}),
--  xIupSetAttributeHandle = define_c_proc(iup, "IupSetAttributeHandle", {P,P,P}),
--  xIupSetHandle = define_c_proc(iup, "IupSetHandle", {P,P}),
--  xIupSetInt = define_c_proc(iup, "IupSetInt", {P,P,I}),
--  xIupSetStrAttribute = define_c_proc(iup, "IupSetStrAttribute", {P,P,P}),
--  xIupStoreAttribute = define_c_proc(iup, "IupStoreAttribute", {P,P,P}),
--  xIupGetAttribute = define_c_func(iup, "IupGetAttribute", {P,P},P),
--  xIupGetInt = define_c_func(iup, "IupGetInt", {P,P},I),
--  xIupGetInt2 = define_c_func(iup, "IupGetInt", {P,P},I),
--  xIupGetIntInt = define_c_func(iup, "IupGetIntInt", {P,P,P,P},I),
--  xIupGetAllClasses = define_c_func(hIup, "IupGetAllClasses", {P,I},I),
--  xIupGetClassName = define_c_func(iup, "IupGetClassName", {P},P),
--  xIupGetClassType = define_c_func(hIup, "IupGetClassType", {P},P),
--  xIupGetClassAttributes = define_c_func(hIup, "IupGetClassAttributes", {P,P,I},I),
--  xIupGetClassCallbacks = define_c_func(hIup, "IupGetClassCallbacks", {P,P,I},I),
--  xIupSaveClassAttributes = define_c_proc(hIup, "IupSaveClassAttributes", {P}),
--  xIupCopyClassAttributes = define_c_proc(hIup, "IupCopyClassAttributes", {P,P}),
--  xIupSetClassDfltAttribute = define_c_proc(hIup, "IupSetClassDefaultAttribute", {P,P,P}),

--  xIupAlarm = define_c_func(iup, "IupAlarm", {P,P,P,P,P},I),
--  xIupMessage = define_c_proc(iup, "IupMessage", {P,P}),
--  xIupButton = define_c_func(iup, "IupButton", {P,P},P),
--  xIupCanvas = define_c_func(iup, "IupCanvas", {P},P),
--  xIupCboxv = define_c_func(iup, "IupCboxv", {P},P),
--  xIupFill  = define_c_func(iup, "IupFill", {},P),
--  xIupFrame = define_c_func(iup, "IupFrame", {P},P),
--  xIupHboxv = define_c_func(iup, "IupHboxv", {P},P),
--  xIupImage = define_c_func(iup, "IupImage", {I,I,P},P),
--  xIupImageRGB = define_c_func(iup, "IupImageRGB", {I,I,P},P),
--  xIupImageRGBA = define_c_func(iup, "IupImageRGBA", {I,I,P},P),
--  xIupLabel = define_c_func(iup, "IupLabel", {P},P),
--  xIupList = define_c_func(iup, "IupList", {P},P),
--  xIupMenuv = define_c_func(iup, "IupMenuv", {P},P),
--  xIupItem = define_c_func(iup, "IupItem",  {P,P},P),
--  xIupSeparator = define_c_func(iup, "IupSeparator", {},P),
--  xIupSubmenu = define_c_func(iup, "IupSubmenu", {P,P},P),
--  xIupProgressBar = define_c_func(iup, "IupProgressBar", {},P),
--  xIupRadio = define_c_func(iup, "IupRadio", {P},P),
--  xIupTabsv = define_c_func(iup, "IupTabsv", {P},P),
--  xIupText = define_c_func(iup, "IupText", {P},P),
--  xIupMultiLine = define_c_func(iup, "IupMultiLine", {P},P),
--  xIupTextConvertLinColToPos = define_c_proc(iup, "IupTextConvertLinColToPos", {P,I,I,P}),
--  xIupTextConvertPosToLinCol = define_c_proc(iup, "IupTextConvertPosToLinCol", {P,I,P,P}),
--  xIupTimer = define_c_func(iup, "IupTimer", {},P),
--  xIupToggle = define_c_func(iup, "IupToggle", {P,P},P),
--  xIupTree = define_c_func(iup, "IupTree", {},P),
--  xIupVal = define_c_func(iup, "IupVal", {P},P),
--  xIupVboxv = define_c_func(iup, "IupVboxv", {P},P),
--  xIupZboxv = define_c_func(iup, "IupZboxv", {P},P),

--  xIupMap = define_c_func(iup, "IupMap", {P},I),
--  xIupUnmap = define_c_proc(iup, "IupUnmap", {P}),
--  xIupMainLoop = define_c_proc(iup, "IupMainLoop", {}),
--  xIupDestroy = define_c_proc(iup, "IupDestroy", {P}),
--  xIupFileDlg = define_c_func(iup, "IupFileDlg", {},P),
--  xIupFontDlg = define_c_func(iup, "IupFontDlg", {},P),
--  xIupGetColor = define_c_func(iup, "IupGetColor", {I,I,P,P,P},I),
--- xIupGetParam = define_c_func(iup, "IupGetParam", {P,P,P,P,P,P,P,P,P,P,P,P,P,P,P},I),
--  xIupGetParam = define_c_func(iup, "IupGetParam", {P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P},I),
--  xIupRecordInput = define_c_func(iup, "IupRecordInput", {P,I},I),
--  xIupPlayInput = define_c_func(iup, "IupPlayInput", {P},I),
--/*
--  xIupSetHandle = define_c_proc(iup, "IupSetHandle", {P,P}),
--  xIupGetHandle = define_c_func(iup, "IupGetHandle", {P},P),
--  xIupGetName = define_c_func(iup, "IupGetName", {P},P),
--  xIupGetAllNames = define_c_func(iup, "IupGetAllNames", {P,I},I),
--  xIupGetAllDialogs = define_c_func(iup, "IupGetAllDialogs", {P,I},I),
--  xIupGetAllClasses = define_c_func(iup, "IupGetAllClasses", {P,I},I),
--  xIupGetClassName = define_c_func(iup, "IupGetClassName", {P},P),
--  xIupGetClassType = define_c_func(iup, "IupGetClassType", {P},P),
--  xIupGetClassAttributes = define_c_func(iup, "IupGetClassAttributes", {P,P,I},I),
--  xIupGetClassCallbacks = define_c_func(iup, "IupGetClassCallbacks", {P,P,I},I),
--  xIupSaveClassAttributes = define_c_proc(iup, "IupSaveClassAttributes", {P}),
--  xIupCopyClassAttributes = define_c_proc(iup, "IupCopyClassAttributes", {P,P}),
--  xIupSetClassDfltAttribute = define_c_proc(iup, "IupSetClassDefaultAttribute", {P,P,P}),
--*/
--  xIupSetGlobal = define_c_proc(iup, "IupSetGlobal", {P,P}),
--  xIupGetGlobal = define_c_func(iup, "IupGetGlobal", {P},P),
--  xiupKeyCodeToName = define_c_func(iup, "iupKeyCodeToName", {I},P),
--  xIupAppend = define_c_func(iup, "IupAppend", {P,P},P),
--  xIupDetach = define_c_proc(iup, "IupDetach", {P}),
--  xIupInsert = define_c_func(iup, "IupInsert", {P,P,P},P),
--  xIupReparent = define_c_func(iup, "IupReparent", {P,P,P},P),
--  xIupGetParent = define_c_func(iup, "IupGetParent", {P},P),
--  xIupGetChild = define_c_func(iup, "IupGetChild", {P,I},P),
--  xIupGetChildPos = define_c_func(iup, "IupGetChildPos", {P,P},I),
--  xIupGetChildCount = define_c_func(iup, "IupGetChildCount", {P},I),
--  xIupGetNextChild = define_c_func(iup, "IupGetNextChild", {P,P},P),
--  xIupGetBrother = define_c_func(iup, "IupGetBrother", {P},P),
--  xIupGetDialog = define_c_func(iup, "IupGetDialog", {P},P),
--  xIupGetDialogChild = define_c_func(iup, "IupGetDialogChild", {P,P},P),
--  xIupClipboard = define_c_func(iup, "IupClipboard", {},P),

--  xIupConfig                        = define_c_func(iup, "IupConfig", {},P),
--  xIupConfigLoad                    = define_c_func(iup, "IupConfigLoad", {P},I),
--  xIupConfigSave                    = define_c_func(iup, "IupConfigSave", {P},I),
--  xIupConfigSetVariableStr          = define_c_proc(iup, "IupConfigSetVariableStr", {P,P,P,P}),
--  xIupConfigSetVariableStrId        = define_c_proc(iup, "IupConfigSetVariableStrId", {P,P,P,I,P}),
--  xIupConfigSetVariableInt          = define_c_proc(iup, "IupConfigSetVariableInt", {P,P,P,I}),
--  xIupConfigSetVariableIntId        = define_c_proc(iup, "IupConfigSetVariableIntId", {P,P,P,I,I}),
--  xIupConfigSetVariableDouble       = define_c_proc(iup, "IupConfigSetVariableDouble", {P,P,P,D}),
--  xIupConfigSetVariableDoubleId     = define_c_proc(iup, "IupConfigSetVariableDoubleId", {P,P,P,I,D}),
--  xIupConfigGetVariableStr          = define_c_func(iup, "IupConfigGetVariableStr", {P,P,P},P),
--  xIupConfigGetVariableStrId        = define_c_func(iup, "IupConfigGetVariableStrId", {P,P,P,I},P),
--  xIupConfigGetVariableInt          = define_c_func(iup, "IupConfigGetVariableInt", {P,P,P},I),
--  xIupConfigGetVariableIntId        = define_c_func(iup, "IupConfigGetVariableIntId", {P,P,P,I},I),
--  xIupConfigGetVariableDouble       = define_c_func(iup, "IupConfigGetVariableDouble", {P,P,P},D),
--  xIupConfigGetVariableDoubleId     = define_c_func(iup, "IupConfigGetVariableDoubleId", {P,P,P,I},D),
--  xIupConfigGetVariableStrDef       = define_c_func(iup, "IupConfigGetVariableStrDef", {P,P,P,P},P),
--  xIupConfigGetVariableStrIdDef     = define_c_func(iup, "IupConfigGetVariableStrIdDef", {P,P,P,I,P},P),
--  xIupConfigGetVariableIntDef       = define_c_func(iup, "IupConfigGetVariableIntDef", {P,P,P,I},I),
--  xIupConfigGetVariableIntIdDef     = define_c_func(iup, "IupConfigGetVariableIntIdDef", {P,P,P,I,I},I),
--  xIupConfigGetVariableDoubleDef    = define_c_func(iup, "IupConfigGetVariableDoubleDef", {P,P,P,D},D),
--  xIupConfigGetVariableDoubleIdDef  = define_c_func(iup, "IupConfigGetVariableDoubleIdDef", {P,P,P,I,D},D),
--  xIupConfigRecentInit              = define_c_proc(iup, "IupConfigRecentInit", {P,P,P,I}),
--  xIupConfigRecentUpdate            = define_c_proc(iup, "IupConfigRecentUpdate", {P,P}),
--  xIupConfigDialogShow              = define_c_proc(iup, "IupConfigDialogShow", {P,P,P}),
--  xIupConfigDialogClosed            = define_c_proc(iup, "IupConfigDialogClosed", {P,P,P}),

--  xIupHelp = define_c_func(iup, "IupHelp", {P},I),
--  $

--constant atom hCd = iup_open_dll({"cd.dll", "libcd.so","libcd.dylib"})
--constant atom hCdIup = iup_open_dll({"iupcd.dll", "libiupcd.so","libiupcd.dylib"})

--constant
--  xcdVersion = define_c_func(hCd, "cdVersion", {},P),
--  xcdVersionDate = define_c_func(hCd, "cdVersionDate", {},P),
--  xcdCanvasGetAttribute = define_c_func(hCd, "cdCanvasGetAttribute", {P,P},P),
--  xcdCanvasGetColorPlanes = define_c_func(hCd, "cdCanvasGetColorPlanes", {P},I),
--  xcdCanvasGetContext = define_c_func(hCd, "cdCanvasGetContext", {P},P),
--  xcdCanvasFont = define_c_proc(hCd, "cdCanvasFont", {P,P,I,I}),
--  xcdCanvasGetFont = define_c_proc(hCd, "cdCanvasGetFont", {P,P,P,P}),
--  xcdCanvasOrigin = define_c_proc(hCd, "cdCanvasOrigin", {P,I,I}),
--  xcdfCanvasOrigin = define_c_proc(hCd, "cdfCanvasOrigin", {P,D,D}),
--  xcdCanvasGetOrigin = define_c_proc(hCd, "cdfCanvasGetOrigin", {P,P,P}),
--  xcdfCanvasGetOrigin = define_c_proc(hCd, "cdfCanvasGetOrigin", {P,P,P}),
--  xcdCanvasGetSize = define_c_proc(hCd, "cdCanvasGetSize", {P,I,I,P,P}),
--  xcdCanvasGetTransform = define_c_func(hCd, "cdCanvasGetTransform", {P},P),
--  xcdCanvasUpdateYAxis = define_c_func(hCd, "cdCanvasUpdateYAxis", {P,P},I),
--  xcdfCanvasUpdateYAxis = define_c_func(hCd, "cdfCanvasUpdateYAxis", {P,P},D),
--  xcdCanvasInvertYAxis = define_c_func(hCd, "cdCanvasInvertYAxis", {P,I},I),
--  xcdfCanvasInvertYAxis = define_c_func(hCd, "cdfCanvasInvertYAxis", {P,D},D),
--  xcdCanvasMM2Pixel = define_c_proc(hCd, "cdCanvasMM2Pixel", {P,D,D,P,P}),
--  xcdCanvasPixel2MM = define_c_proc(hCd, "cdCanvasPixel2MM", {P,I,I,P,P}),
--  xcdfCanvasMM2Pixel = define_c_proc(hCd, "cdfCanvasMM2Pixel", {P,D,D,P,P}),
--  xcdfCanvasPixel2MM = define_c_proc(hCd, "cdfCanvasPixel2MM", {P,D,D,P,P}),
--  xcdCanvasSetForeground = define_c_proc(hCd, "cdCanvasSetForeground", {P,I}),
--  xcdCanvasForeground = define_c_func(hCd, "cdCanvasForeground", {P,L},L),
--  xcdCanvasLineWidth = define_c_func(hCd, "cdCanvasLineWidth", {P,I},I),
--  xcdCanvasLineStyle = define_c_func(hCd, "cdCanvasLineStyle", {P,I},I),
--  xcdCanvasLine = define_c_proc(hCd, "cdCanvasLine", {P,I,I,I,I}),
--  xcdCanvasBegin = define_c_proc(hCd, "cdCanvasBegin", {P,I}),
--  xcdCanvasVertex = define_c_proc(hCd, "cdCanvasVertex", {P,I,I}),
--  xcdCanvasEnd = define_c_proc(hCd, "cdCanvasEnd", {P}),
--  xcdCanvasRect = define_c_proc(hCd, "cdCanvasRect", {P,I,I,I,I}),
--  xcdCanvasArc = define_c_proc(hCd, "cdCanvasArc", {P,I,I,I,I,D,D}),
--  xcdCanvasVectorTextDirection = define_c_proc(hCd, "cdCanvasVectorTextDirection", {P,I,I,I,I}),
--  xcdCanvasMultiLineVectorText = define_c_proc(hCd, "cdCanvasMultiLineVectorText", {P,I,I,P}),
--  xcdCanvasGetVectorTextSize = define_c_proc(hCd, "cdCanvasGetVectorTextSize", {P,P,P,P}),
--  xcdCanvasGetVectorTextBounds = define_c_proc(hCd, "cdCanvasGetVectorTextBounds", {P,P,I,I,P}),
--  xwdCanvasGetVectorTextBounds = define_c_proc(hCd, "wdCanvasGetVectorTextBounds", {P,P,D,D,P}),
--  xcdCanvasGetFontDim = define_c_proc(hCd, "cdCanvasGetFontDim", {P,P,P,P,P}),
--  xcdCanvasGetTextSize = define_c_proc(hCd, "cdCanvasGetTextSize", {P,P,P,P}),
--  xcdCanvasGetTextBox = define_c_proc(hCd, "cdCanvasGetTextBox", {P,P,I,I,P,P,P,P}),
--  xcdCanvasGetTextBox = define_c_proc(hCd, "cdCanvasGetTextBox", {P,I,I,P,P,P,P,P}),
--  xcdfCanvasGetTextBox = define_c_proc(hCd, "cdfCanvasGetTextBox", {P,D,D,P,P,P,P,P}),
--  xwdCanvasGetTextBox = define_c_proc(hCd, "wdCanvasGetTextBox", {P,D,D,P,P,P,P,P}),
--  xcdCanvasGetTextBounds = define_c_proc(hCd, "cdCanvasGetTextBounds", {P,I,I,P,P}),
--  xcdfCanvasGetTextBounds = define_c_proc(hCd, "cdCanvasGetTextBounds", {P,D,D,P,P}),
--  xwdCanvasGetTextBounds = define_c_proc(hCd, "wdCanvasGetTextBounds", {P,D,D,P,P}),
--  xcdCanvasGetImageRGB = define_c_proc(hCd, "cdCanvasGetImageRGB", {P,P,P,P,I,I,I,I}),
--  xwdCanvasGetImageRGB = define_c_proc(hCd, "wdCanvasGetImageRGB", {P,P,P,P,D,D,I,I}),
--  xcdCanvasPutImageRectRGB = define_c_proc(hCd, "cdCanvasPutImageRectRGB", {P,I,I,P,P,P,I,I,I,I,I,I,I,I}),
--  xcdCanvasTransformPoint = define_c_proc(hCd, "cdCanvasTransformPoint", {P,I,I,P,P}),
--  xcdfCanvasTransformPoint = define_c_proc(hCd, "cdfCanvasTransformPoint", {P,D,D,P,P}),
--  xwdCanvasWindow = define_c_proc(hCd, "wdCanvasWindow", {P,D,D,D,D}),
--  xwdCanvasGetWindow = define_c_proc(hCd, "wdCanvasGetWindow", {P,P,P,P,P}),
--  xwdCanvasViewport = define_c_proc(hCd, "wdCanvasViewport", {P,I,I,I,I}),
--  xwdCanvasGetViewport = define_c_proc(hCd, "wdCanvasGetViewport", {P,P,P,P,P}),
--  xcdContextCaps = define_c_func(hCd, "cdContextCaps", {P},UL),
--  xcdCreateCanvas = define_c_func(hCd, "cdCreateCanvas", {P,P},P),
--  xcdKillCanvas = define_c_proc(hCd, "cdKillCanvas", {P}),
--/*
todo:
C:\Program Files (x86)\Phix\demo\tee\simple_paint.exw:898   rgb_canvas = cdCreateCanvas(CD_IMIMAGE, image);
C:\Program Files (x86)\Phix\demo\tee\simple_paint.exw:1044   cd_canvas = cdCreateCanvasf(CD_GL, "10x10 %g", res);
C:\Program Files (x86)\Phix\demo\tee\simple_paint.exw:1049   cd_canvas = cdCreateCanvas(CD_IUPDBUFFER, canvas);
C:\Program Files (x86)\Phix\demo\tee\simple_paint.exw:1222               cdCanvas* rgb_canvas = cdCreateCanvas(CD_IMIMAGE, image);
CD_IMIMAGE = cdContextImImage()
--*/

 -- vector text attributes
--------------------------
--constant
--  xcdCanvasVectorFont = define_c_func(hCd, "cdCanvasVectorFont", {P,P},P),
--  xcdCanvasVectorTextDirection = define_c_proc(hCd, "cdCanvasVectorTextDirection", {P,I,I,I,I}),
--  xcdCanvasVectorTextTransform = define_c_func(hCd, "cdCanvasVectorTextTransform", {P,P},P),
--  xcdCanvasVectorTextSize = define_c_proc(hCd, "cdCanvasVectorTextSize", {P,I,I,P}),
--  xcdCanvasVectorCharSize = define_c_func(hCd, "cdCanvasVectorCharSize", {P,I},I),
--  $

 -- world draw attributes
-------------------------
--constant 
--  xwdCanvasLineWidth = define_c_func(hCd, "wdCanvasLineWidth", {P,D},D),
--  xwdCanvasFont = define_c_proc(hCd, "wdCanvasFont", {P,P,I,D}),
--  xwdCanvasGetFont = define_c_proc(hCd, "wdCanvasGetFont", {P,P,P,P}),
--  xwdCanvasMarkSize = define_c_func(hCd, "wdCanvasMarkSize", {P,D},D),
--  xwdCanvasGetFontDim = define_c_proc(hCd, "wdCanvasGetFontDim", {P,P,P,P,P}),
--  xwdCanvasGetTextSize = define_c_proc(hCd, "wdCanvasGetTextSize", {P,P,P,P}),
--  xwdCanvasGetTextBox = define_c_proc(hCd, "wdCanvasGetTextBox", {P,D,D,P,P,P,P,P}),
--  xwdCanvasGetTextBounds = define_c_proc(hCd, "wdCanvasGetTextbounds", {P,D,D,P,P}),
--  xwdCanvasStipple = define_c_proc(hCd, "wdCanvasStipple", {P,I,I,P,D,D}),
--  xwdCanvasPattern = define_c_proc(hCd, "wdCanvasPattern", {P,I,I,P,D,D})
--  $

 -- iup.dll, according to dependency walker:
--------------------------------------------
--/*
IupAlarm
IupAnimatedLabel
IupAppend
iupArrayAdd
iupArrayCount
iupArrayCreate
iupArrayDestroy
iupArrayGetData
iupArrayInc
iupArrayInsert
iupArrayRemove
iupAssert
iupAttribGet
iupAttribGetBoolean
iupAttribGetBooleanId
iupAttribGetBooleanId2
iupAttribGetClassObject
iupAttribGetClassObjectId
iupAttribGetClassObjectId2
iupAttribGetDouble
iupAttribGetDoubleId
iupAttribGetDoubleId2
iupAttribGetFloat
iupAttribGetFloatId
iupAttribGetFloatId2
iupAttribGetHandleName
iupAttribGetId
iupAttribGetId2
iupAttribGetInherit
iupAttribGetInheritNativeParent
iupAttribGetInt
iupAttribGetIntId
iupAttribGetIntId2
iupAttribGetLocal
iupAttribGetStr
iupAttribIsIhandle
iupAttribIsNotString
iupAttribSet
iupAttribSetClassObject
iupAttribSetClassObjectId
iupAttribSetClassObjectId2
iupAttribSetDouble
iupAttribSetDoubleId
iupAttribSetDoubleId2
iupAttribSetFloat
iupAttribSetFloatId
iupAttribSetFloatId2
iupAttribSetHandleName
iupAttribSetId
iupAttribSetId2
iupAttribSetInt
iupAttribSetIntId
iupAttribSetIntId2
iupAttribSetStr
iupAttribSetStrf
iupAttribSetStrId
iupAttribSetStrId2
IupBackgroundBox
iupBaseCallValueChangedCb
iupBaseComputeNaturalSize
iupBaseContainerGetExpandAttrib
iupBaseContainerUpdateExpand
iupBaseGetActiveAttrib
iupBaseGetClientOffsetAttrib
iupBaseGetRasterSizeAttrib
iupBaseGetScrollbar
iupBaseGetSizeAttrib
iupBaseGetVisibleAttrib
iupBaseGetWidAttrib
iupBaseRegisterCommonAttrib
iupBaseRegisterCommonCallbacks
iupBaseRegisterVisualAttrib
iupBaseSetActiveAttrib
iupBaseSetCurrentSize
iupBaseSetPosition
iupBaseSetRasterSizeAttrib
iupBaseSetSizeAttrib
iupBaseSetVisibleAttrib
iupBaseTypeVoidMapMethod
IupButton
IupCalendar
iupCallGetFocusCb
iupCallKillFocusCb
IupCanvas
IupCbox
IupCboxv
iupChildTreeAppend
iupChildTreeGetNativeParent
iupChildTreeGetNativeParentHandle
iupClassCallbackGetFormat
IupClassMatch
iupClassMatch
iupClassNew
iupClassObjectAttribIsNotString
iupClassObjectChildAdded
iupClassObjectChildRemoved
iupClassObjectComputeNaturalSize
iupClassObjectCreate
iupClassObjectDestroy
iupClassObjectDlgPopup
iupClassObjectGetAttribute
iupClassObjectGetAttributeInfo
iupClassObjectGetInnerContainer
iupClassObjectLayoutUpdate
iupClassObjectMap
iupClassObjectSetAttribute
iupClassObjectSetChildrenCurrentSize
iupClassObjectSetChildrenPosition
iupClassObjectUnMap
iupClassRegisterAttribute
iupClassRegisterAttributeId
iupClassRegisterAttributeId2
iupClassRegisterCallback
iupClassRegisterGetAttribute
iupClassRegisterReplaceAttribDef
iupClassRegisterReplaceAttribFlags
iupClassRegisterReplaceAttribFunc
iupClassRelease
IupClipboard
IupClose
IupColorDlg
IupConfig
IupConfigDialogClosed
IupConfigDialogShow
IupConfigGetVariableDouble
IupConfigGetVariableDoubleDef
IupConfigGetVariableDoubleId
IupConfigGetVariableDoubleIdDef
IupConfigGetVariableInt
IupConfigGetVariableIntDef
IupConfigGetVariableIntId
IupConfigGetVariableIntIdDef
IupConfigGetVariableStr
IupConfigGetVariableStrDef
IupConfigGetVariableStrId
IupConfigGetVariableStrIdDef
IupConfigLoad
IupConfigRecentInit
IupConfigRecentUpdate
IupConfigSave
IupConfigSetVariableDouble
IupConfigSetVariableDoubleId
IupConfigSetVariableInt
IupConfigSetVariableIntId
IupConfigSetVariableStr
IupConfigSetVariableStrId
IupConvertXYToPos
IupCopyClassAttributes
IupCreate
IupCreatep
IupCreatev
iupDataEntry
IupDatePick
IupDestroy
IupDetach
IupDetachBox
IupDialog
iupDlgListAdd
iupDlgListCount
iupDlgListFirst
iupDlgListNext
iupDlgListRemove
iupDlgListVisibleCount
iupDlgListVisibleDec
iupDlgListVisibleInc
iupDrawArc
iupDrawCreateCanvas
iupDrawFlush
iupDrawFocusRect
iupDrawGetSize
iupDrawImage
iupDrawKillCanvas
iupDrawLine
iupDrawParentBackground
iupDrawPolygon
iupDrawRectangle
iupDrawResetClip
iupDrawSelectRect
iupDrawSetClipRect
iupDrawText
iupDrawUpdateSize
iupdrvActivate
iupdrvAddScreenOffset
iupdrvBaseLayoutUpdateMethod
iupdrvBaseSetCursorAttrib
iupdrvBaseSetTipAttrib
iupdrvBaseSetTipVisibleAttrib
iupdrvBaseSetZorderAttrib
iupdrvBaseUnMapMethod
iupdrvClientToScreen
iupdrvDrawFocusRect
iupdrvFontGetCharSize
iupdrvFontGetMultiLineStringSize
iupdrvFontGetStringWidth
iupdrvGetComputerName
iupdrvGetCursorPos
iupdrvGetDisplay
iupdrvGetFullSize
iupdrvGetKeyState
iupdrvGetScreenDepth
iupdrvGetScreenDpi
iupdrvGetScreenSize
iupdrvGetScrollbarSize
iupdrvGetSystemFont
iupdrvGetSystemName
iupdrvGetSystemVersion
iupdrvGetUserName
iupdrvImageCreateImageRaw
iupdrvImageDestroy
iupdrvImageGetRawData
iupdrvImageGetRawInfo
iupdrvIsActive
iupdrvIsVisible
iupdrvKeyEncode
iupdrvLocaleInfo
iupdrvPostRedraw
iupdrvRedrawNow
iupdrvRegisterDragDropAttrib
iupdrvReparent
iupdrvScreenToClient
iupdrvSendKey
iupdrvSendMouse
iupdrvSetActive
iupdrvSetStandardFontAttrib
iupdrvSetVisible
iupdrvWarpPointer
IupElementPropertiesDialog
iupError
IupExecute
IupExitLoop
IupExpander
IupFileDlg
IupFill
IupFlatButton
IupFlush
iupFocusCanAccept
IupFontDlg
iupFontParsePango
iupFontParseWin
iupFontParseX
IupFrame
IupGetActionName
IupGetAllAttributes
IupGetAllClasses
IupGetAllDialogs
IupGetAllNames
IupGetAttribute
IupGetAttributeHandle
IupGetAttributeId
IupGetAttributeId2
IupGetAttributes
IupGetBrother
IupGetCallback
IupGetChild
IupGetChildCount
IupGetChildPos
IupGetClassAttributes
IupGetClassCallbacks
IupGetClassName
IupGetClassType
IupGetColor
IupGetDialog
IupGetDialogChild
IupGetDouble
IupGetDoubleId
IupGetDoubleId2
IupGetFile
IupGetFloat
IupGetFloatId
IupGetFloatId2
IupGetFocus
iupGetFontAttrib
iupGetFontFaceAttrib
iupGetFontInfo
iupGetFontSizeAttrib
iupGetFontStyleAttrib
IupGetFunction
IupGetGlobal
IupGetHandle
IupGetInt
IupGetInt2
IupGetIntId
IupGetIntId2
IupGetIntInt
IupGetLanguage
IupGetLanguageString
IupGetName
IupGetNextChild
IupGetParam
iupGetParamCount
iupGetParamType
IupGetParamv
IupGetParent
IupGetRGB
IupGetRGBId
IupGetRGBId2
IupGetText
iupGlobalIsPointer
IupGridBox
IupGridBoxv
IupHbox
IupHboxv
IupHelp
IupHide
IupImage
iupImageColorMakeInactive
iupImageInitColorTable
IupImageRGB
IupImageRGBA
iupImageStockLoadAll
iupImageStockSet
IupInsert
IupItem
iupKeyCallKeyCb
iupKeyCallKeyPressCb
iupKeyCodeToName
iupKeyForEach
iupKeyProcessMnemonic
iupKeyProcessNavigation
iupKeySetMnemonic
IupLabel
iupLayoutApplyMinMaxSize
IupLayoutDialog
iupLayoutUpdate
iupLineFileClose
iupLineFileEOF
iupLineFileGetBuffer
iupLineFileOpen
iupLineFileReadLine
IupLink
IupList
IupListDialog
IupLoad
IupLoadBuffer
IupLoopStep
IupLoopStepWait
IupMainLoop
IupMainLoopLevel
IupMap
IupMapFont
iupMaskCheck
iupMaskCreate
iupMaskCreateFloat
iupMaskCreateInt
iupMaskCreateReal
iupMaskDestroy
iupMaskGetStr
IupMenu
IupMenuv
IupMessage
IupMessageDlg
IupMessagef
IupMultiLine
IupNextField
IupNormalizer
IupNormalizerv
iupObjectCheck
iupObjectGetParamList
IupOpen
IupParamBox
IupParamf
IupPlayInput
IupPopup
IupPreviousField
IupProgressBar
IupProgressDlg
IupRadio
iupRadioFindToggleParent
IupRecordInput
IupRedraw
IupRefresh
IupRefreshChildren
iupRegisterClass
iupRegisterFindClass
IupReparent
IupResetAttribute
iupRound
IupSaveClassAttributes
IupSaveImageAsText
iupSaveImageAsText
IupSbox
IupScanf
IupScrollBox
IupSeparator
IupSetAtt
IupSetAttribute
IupSetAttributeHandle
IupSetAttributeId
IupSetAttributeId2
IupSetAttributes
IupSetCallback
IupSetCallbacks
IupSetClassDefaultAttribute
IupSetDouble
IupSetDoubleId
IupSetDoubleId2
IupSetfAttribute
IupSetfAttributeId
IupSetfAttributeId2
IupSetFloat
IupSetFloatId
IupSetFloatId2
IupSetFocus
iupSetFontAttrib
iupSetFontFaceAttrib
iupSetFontSizeAttrib
iupSetFontStyleAttrib
IupSetFunction
IupSetGlobal
IupSetHandle
IupSetInt
IupSetIntId
IupSetIntId2
IupSetLanguage
IupSetLanguagePack
IupSetLanguageString
IupSetRGB
IupSetRGBId
IupSetRGBId2
IupSetStrAttribute
IupSetStrAttributeId
IupSetStrAttributeId2
IupSetStrf
IupSetStrfId
IupSetStrfId2
IupSetStrGlobal
IupShow
iupShowError
iupShowVersion
IupShowXY
IupSpin
IupSpinbox
IupSplit
IupStoreAttribute
IupStoreAttributeId
IupStoreAttributeId2
IupStoreGlobal
IupStoreLanguageString
iupStrBoolean
iupStrCompare
iupStrCompareEqual
iupStrCompareFind
iupStrCopyN
iupStrCountChar
iupStrDup
iupStrDupUntil
iupStrEqual
iupStrEqualNoCase
iupStrEqualNoCaseNoSpace
iupStrEqualNoCasePartial
iupStrEqualPartial
iupStrFalse
iupStrFileGetExt
iupStrFileGetPath
iupStrFileGetTitle
iupStrFileMakeFileName
iupStrFindMnemonic
iupStrGetFormatPrecision
iupStrGetLargeMem
iupStrGetMemory
iupStrHasSpace
IupStringCompare
iupStrInsert
iupStrIsAscii
iupStrLineCount
iupStrLower
iupStrNextLine
iupStrNextValue
iupStrPrintfDoubleLocale
iupStrProcessMnemonic
iupStrRemove
iupStrReplace
iupStrReturnBoolean
iupStrReturnChecked
iupStrReturnDouble
iupStrReturnFloat
iupStrReturnInt
iupStrReturnIntInt
iupStrReturnRGB
iupStrReturnRGBA
iupStrReturnStr
iupStrReturnStrf
iupStrReturnStrStr
iupStrToDos
iupStrToDouble
iupStrToDoubleDef
iupStrToDoubleDouble
iupStrToDoubleLocale
iupStrToFloat
iupStrToFloatDef
iupStrToFloatFloat
iupStrToInt
iupStrToIntInt
iupStrToMac
iupStrToRGB
iupStrToRGBA
iupStrToStrStr
iupStrToUnix
iupStrUpper
IupSubmenu
iupTableClear
iupTableCount
iupTableCreate
iupTableCreateSized
iupTableDestroy
iupTableFirst
iupTableGet
iupTableGetCurr
iupTableGetCurrType
iupTableGetFunc
iupTableGetTyped
iupTableNext
iupTableRemove
iupTableRemoveCurr
iupTableSet
iupTableSetCurr
iupTableSetFunc
IupTabs
IupTabsv
IupText
IupTextConvertLinColToPos
IupTextConvertPosToLinCol
IupTimer
IupToggle
IupTree
IupTreeGetAttribute
IupTreeGetFloat
IupTreeGetId
IupTreeGetInt
IupTreeGetUserId
IupTreeSetAttribute
IupTreeSetAttributeHandle
IupTreeSetfAttribute
IupTreeSetUserId
IupTreeStoreAttribute
IupUnmap
IupUnMapFont
IupUpdate
IupUpdateChildren
IupUser
IupVal
IupVbox
IupVboxv
IupVersion
IupVersionDate
IupVersionNumber
iupwinBaseMsgProc
iupwinButtonDown
iupwinButtonUp
iupwinCreateWindow
iupwinMouseMove
iupwinStrChar2Wide
iupwinStrFromSystem
iupwinStrToSystem
iupwinStrToSystemLen
iupwinStrWide2Char
IupZbox
IupZboxv
--*/

 -- cd.dll, according to dependency walker:
-------------------------------------------
--/*
cdActivate
cdActiveCanvas
cdAlphaImage
cdArc
cdBackground
cdBackOpacity
cdBaseDriver
cdBegin
cdBitmapGetData
cdBitmapRGB2Map
cdBitmapSetRect
cdBlueImage
cdBox
cdCanvasActivate
cdCanvasArc
cdCanvasBackground
cdCanvasBackOpacity
cdCanvasBegin
cdCanvasBox
cdCanvasChord
cdCanvasClear
cdCanvasClip
cdCanvasClipArea
cdCanvasCreateImage
cdCanvasDeactivate
cdCanvasEnd
cdCanvasFillMode
cdCanvasFlush
cdCanvasFont
cdCanvasForeground
cdCanvasGetAttribute
cdCanvasGetBitmap
cdCanvasGetClipArea
cdCanvasGetColorPlanes
cdCanvasGetContext
cdCanvasGetFont
cdCanvasGetFontDim
cdCanvasGetImage
cdCanvasGetImageRGB
cdCanvasGetOrigin
cdCanvasGetPattern
cdCanvasGetRegionBox
cdCanvasGetSize
cdCanvasGetStipple
cdCanvasGetTextBounds
cdCanvasGetTextBox
cdCanvasGetTextSize
cdCanvasGetTransform
cdCanvasGetVectorFontSize
cdCanvasGetVectorTextBounds
cdCanvasGetVectorTextBox
cdCanvasGetVectorTextSize
cdCanvasHatch
cdCanvasInteriorStyle
cdCanvasInvertYAxis
cdCanvasIsPointInRegion
cdCanvasLine
cdCanvasLineCap
cdCanvasLineJoin
cdCanvasLineStyle
cdCanvasLineStyleDashes
cdCanvasLineWidth
cdCanvasMark
cdCanvasMarkSize
cdCanvasMarkType
cdCanvasMM2Pixel
cdCanvasMultiLineVectorText
cdCanvasNativeFont
cdCanvasOffsetRegion
cdCanvasOrigin
cdCanvasPalette
cdCanvasPathSet
cdCanvasPattern
cdCanvasPixel
cdCanvasPixel2MM
cdCanvasPlay
cdCanvasPutBitmap
cdCanvasPutImageRect
cdCanvasPutImageRectMap
cdCanvasPutImageRectRGB
cdCanvasPutImageRectRGBA
cdCanvasRect
cdCanvasRegionCombineMode
cdCanvasRestoreState
cdCanvasSaveState
cdCanvasScrollArea
cdCanvasSector
cdCanvasSetAttribute
cdCanvasSetBackground
cdCanvasSetfAttribute
cdCanvasSetForeground
cdCanvasSimulate
cdCanvasStipple
cdCanvasText
cdCanvasTextAlignment
cdCanvasTextOrientation
cdCanvasTransform
cdCanvasTransformMultiply
cdCanvasTransformPoint
cdCanvasTransformRotate
cdCanvasTransformScale
cdCanvasTransformTranslate
cdCanvasUpdateYAxis
cdCanvasVectorCharSize
cdCanvasVectorFont
cdCanvasVectorFontSize
cdCanvasVectorText
cdCanvasVectorTextDirection
cdCanvasVectorTextSize
cdCanvasVectorTextTransform
cdCanvasVertex
cdCanvasWriteMode
cdCanvasYAxisMode
cdChord
cdClear
cdClip
cdClipArea
cdContextCaps
cdContextCGM
cdContextClipboard
cdContextDBuffer
cdContextDBufferRGB
cdContextDebug
cdContextDGN
cdContextDXF
cdContextEMF
cdContextImage
cdContextImageRGB
cdContextIsPlus
cdContextMetafile
cdContextNativeWindow
cdContextPicture
cdContextPrinter
cdContextPS
cdContextRegisterCallback
cdContextSVG
cdContextType
cdContextWMF
cdCreateBitmap
cdCreateCanvas
cdCreateCanvasf
cdcreatecanvasMF
cdCreateImage
cdDecodeAlpha
cdDecodeColor
cdDecodeColorAlpha
cdEncodeAlpha
cdEncodeColor
cdEncodeColorAlpha
cdEnd
cdfCanvasArc
cdfCanvasBox
cdfCanvasChord
cdfCanvasClipArea
cdfCanvasGetClipArea
cdfCanvasGetOrigin
cdfCanvasGetTextBounds
cdfCanvasGetTextBox
cdfCanvasGetVectorTextBounds
cdfCanvasGetVectorTextBox
cdfCanvasGetVectorTextSize
cdfCanvasInvertYAxis
cdfCanvasLine
cdfCanvasMark
cdfCanvasMM2Pixel
cdfCanvasMultiLineVectorText
cdfCanvasOrigin
cdfCanvasPixel
cdfCanvasPixel2MM
cdfCanvasPutImageRectMap
cdfCanvasPutImageRectRGB
cdfCanvasPutImageRectRGBA
cdfCanvasRect
cdfCanvasSector
cdfCanvasText
cdfCanvasTransformPoint
cdfCanvasUpdateYAxis
cdfCanvasVectorCharSize
cdfCanvasVectorText
cdfCanvasVectorTextDirection
cdfCanvasVectorTextSize
cdfCanvasVertex
cdfGetArcPath
cdfGetArcStartEnd
cdFillMode
cdFlush
cdFont
cdFontDim
cdForeground
cdfRotatePoint
cdfRotatePointY
cdfSimArc
cdfSimBox
cdfSimChord
cdfSimPolyPath
cdfSimSector
cdfTextTranslatePoint
cdGetArcBox
cdGetArcPath
cdGetArcPathF
cdGetArcStartEnd
cdGetAttribute
cdGetBitmap
cdGetCanvasSize
cdGetClipArea
cdGetClipPoly
cdGetColorPlanes
cdGetContext
cdGetContextPlus
cdGetFileName
cdGetFont
cdGetFontFileName
cdGetFontFileNameDefault
cdGetFontFileNameSystem
cdGetFontSizePixels
cdGetFontSizePoints
cdGetImage
cdGetImageRGB
cdGetPattern
cdGetScreenColorPlanes
cdGetScreenSize
cdGetStipple
cdGetVectorTextBounds
cdGetVectorTextSize
cdGreenImage
cdHatch
cdInitBitmap
cdInitContextPlusList
cdinittableMF
cdInteriorStyle
cdKillBitmap
cdKillCanvas
cdkillcanvasMF
cdKillImage
cdLine
cdLineCap
cdLineJoin
cdLineStyle
cdLineStyleDashes
cdLineWidth
cdMark
cdMarkSize
cdMarkType
cdMM2Pixel
cdMultiLineVectorText
cdNativeFont
cdOffsetRegion
cdOrigin
cdPalette
cdParseIupWinFont
cdParsePangoFont
cdParseXWinFont
cdPattern
cdPixel
cdPixel2MM
cdPlay
cdPointInRegion
cdPutBitmap
cdPutImageRect
cdPutImageRectMap
cdPutImageRectRGB
cdPutImageRectRGBA
cdRect
cdRedImage
cdRegionBox
cdRegionCombineMode
cdRegisterAttribute
cdRegisterCallback
cdReleaseState
cdRestoreState
cdRGB2Map
cdRotatePointY
cdRound
cdSaveState
cdScrollArea
cdSector
cdSetAttribute
cdSetfAttribute
cdSetPaperSize
cdSimArc
cdSimBox
cdSimChord
cdSimPolyPath
cdSimSector
cdSimulate
cdStipple
cdStrDup
cdStrEqualNoCase
cdStrEqualNoCasePartial
cdStrIsAscii
cdStrTmpFileName
cdText
cdTextAlignment
cdTextBounds
cdTextBox
cdTextOrientation
cdTextSize
cdTextTranslatePoint
cdUpdateAttributes
cdUpdateYAxis
cdUseContextPlus
cdVectorCharSize
cdVectorFont
cdVectorText
cdVectorTextDirection
cdVectorTextSize
cdVectorTextTransform
cdVersion
cdVersionDate
cdVersionNumber
cdVertex
cdwCreateCanvas
cdwInitTable
cdwKillCanvas
cdwRestoreDC
cdWriteMode
wdArc
wdBox
wdCanvas2World
wdCanvasArc
wdCanvasBox
wdCanvasCanvas2World
wdCanvasChord
wdCanvasClipArea
wdCanvasFont
wdCanvasGetClipArea
wdCanvasGetFont
wdCanvasGetFontDim
wdCanvasGetImageRGB
wdCanvasGetRegionBox
wdCanvasGetTextBounds
wdCanvasGetTextBox
wdCanvasGetTextSize
wdCanvasGetTransform
wdCanvasGetVectorTextBounds
wdCanvasGetVectorTextBox
wdCanvasGetVectorTextSize
wdCanvasGetViewport
wdCanvasGetWindow
wdCanvasHardcopy
wdCanvasIsPointInRegion
wdCanvasLine
wdCanvasLineWidth
wdCanvasMark
wdCanvasMarkSize
wdCanvasMultiLineVectorText
wdCanvasOffsetRegion
wdCanvasPattern
wdCanvasPixel
wdCanvasPlay
wdCanvasPutBitmap
wdCanvasPutImageRect
wdCanvasPutImageRectMap
wdCanvasPutImageRectRGB
wdCanvasPutImageRectRGBA
wdCanvasRect
wdCanvasScale
wdCanvasSector
wdCanvasSetTransform
wdCanvasStipple
wdCanvasText
wdCanvasTranslate
wdCanvasVectorCharSize
wdCanvasVectorText
wdCanvasVectorTextDirection
wdCanvasVectorTextSize
wdCanvasVertex
wdCanvasViewport
wdCanvasWindow
wdCanvasWorld2Canvas
wdCanvasWorld2CanvasSize
wdChord
wdClipArea
wdFont
wdFontDim
wdGetClipArea
wdGetClipPoly
wdGetFont
wdGetVectorTextBounds
wdGetVectorTextSize
wdGetViewport
wdGetWindow
wdHardcopy
wdLine
wdLineWidth
wdMark
wdMarkSize
wdMultiLineVectorText
wdOffsetRegion
wdPattern
wdPixel
wdPointInRegion
wdPutBitmap
wdPutImageRect
wdPutImageRectMap
wdPutImageRectRGB
wdPutImageRectRGBA
wdRect
wdRegionBox
wdSector
wdStipple
wdText
wdTextBounds
wdTextBox
wdTextSize
wdVectorCharSize
wdVectorText
wdVectorTextDirection
wdVectorTextSize
wdVertex
wdViewport
wdWindow
wdWorld2Canvas
wdWorld2CanvasSize
--*/

 -- cdim.dll, according to dependency walker:
-------------------------------------------
--/*
cdCanvasGetImImage
cdCanvasPatternImImage
cdCanvasPutImImage
cdCanvasStippleImImage
cdContextImImage
cdfCanvasPutImImage
wdCanvasGetImImage
wdCanvasPutImImage
--*/

 -- im.dll, according to dependency walker:
-------------------------------------------
--/*
imAttribArrayCopyFrom
imAttribArrayCreate
imAttribArrayGet
imAttribArraySet
imAttribTableCopyFrom
imAttribTableCount
imAttribTableCreate
imAttribTableDestroy
imAttribTableForEach
imAttribTableGet
imAttribTableGetInteger
imAttribTableGetReal
imAttribTableGetString
imAttribTableRemoveAll
imAttribTableSet
imAttribTableSetInteger
imAttribTableSetReal
imAttribTableSetString
imAttribTableUnSet
imBinCPUByteOrder
imBinFileByteOrder
imBinFileClose
imBinFileEndOfFile
imBinFileError
imBinFileNew
imBinFileOpen
imBinFilePrintf
imBinFileRead
imBinFileReadInteger
imBinFileReadLine
imBinFileReadReal
imBinFileRegisterModule
imBinFileSeekFrom
imBinFileSeekOffset
imBinFileSeekTo
imBinFileSetCurrentModule
imBinFileSize
imBinFileSkipLine
imBinFileTell
imBinFileWrite
imBinMemoryRelease
imBinSwapBytes2
imBinSwapBytes4
imBinSwapBytes8
imColorDecode
imColorEncode
imColorHSI2RGB
imColorHSI2RGBbyte
imColorHSI_ImaxS
imColorModeDepth
imColorModeIsBitmap
imColorModeSpaceName
imColorModeToBitmap
imColorRGB2HSI
imColorRGB2HSIbyte
imCompressDataLZF
imCompressDataUnLZF
imCompressDataUnZ
imCompressDataZ
imConvertColorSpace
imConvertDataType
imConvertMapToRGB
imConvertPacking
imConvertRGB2Map
imConvertToBitmap
imCounterBegin
imCounterEnd
imCounterGetUserData
imCounterHasCallback
imCounterInc
imCounterIncTo
imCounterSetCallback
imCounterSetUserData
imCounterTotal
imDataTypeIntMax
imDataTypeIntMin
imDataTypeName
imDataTypeSize
imDecodeColor
imDibCaptureScreen
imDibCopyClipboard
imDibCreate
imDibCreateCopy
imDibCreateReference
imDibCreateSection
imDibDecodeToBitmap
imDibDecodeToMap
imDibDecodeToRGBA
imDibDestroy
imDibEncodeFromBitmap
imDibEncodeFromMap
imDibEncodeFromRGBA
imDibFromHBitmap
imDibFromImage
imDibIsClipboardAvailable
imDibLineGetPixelFunc
imDibLineSetPixelFunc
imDibLoadFile
imDibLogicalPalette
imDibPasteClipboard
imDibSaveFile
imDibToHBitmap
imDibToImage
imEncodeColor
imFileClose
imFileFormat
imFileGetAttribInteger
imFileGetAttribReal
imFileGetAttribString
imFileGetAttribute
imFileGetAttributeList
imFileGetInfo
imFileGetPalette
imFileHandle
imFileImageLoad
imFileImageLoadBitmap
imFileImageLoadRegion
imFileImageSave
imFileLineBufferCount
imFileLineBufferInc
imFileLineBufferRead
imFileLineBufferWrite
imFileLineSizeAligned
imFileLoadBitmap
imFileLoadBitmapFrame
imFileLoadImage
imFileLoadImageFrame
imFileLoadImageRegion
imFileNew
imFileNewRaw
imFileOpen
imFileOpenAs
imFileOpenRaw
imFileReadImageData
imFileReadImageInfo
imFileSaveImage
imFileSetAttribInteger
imFileSetAttribReal
imFileSetAttribString
imFileSetAttribute
imFileSetBaseAttributes
imFileSetInfo
imFileSetPalette
imFileWriteImageData
imFileWriteImageInfo
imFormatCanWriteImage
imFormatCompressions
imFormatInfo
imFormatInfoExtra
imFormatList
imFormatRegister
imFormatRegisterInternal
imFormatRemoveAll
imImageAddAlpha
imImageCheckFormat
imImageClear
imImageClone
imImageCopy
imImageCopyAttributes
imImageCopyData
imImageCopyPlane
imImageCreate
imImageCreateBased
imImageCreateFromOpenGLData
imImageDataSize
imImageDestroy
imImageDuplicate
imImageGetAttribInteger
imImageGetAttribReal
imImageGetAttribString
imImageGetAttribute
imImageGetAttributeList
imImageGetOpenGLData
imImageInfo
imImageInit
imImageIsBitmap
imImageLineCount
imImageLineSize
imImageLoadFromResource
imImageMakeBinary
imImageMakeGray
imImageMatch
imImageMatchColor
imImageMatchColorSpace
imImageMatchDataType
imImageMatchSize
imImageMergeAttributes
imImageRemoveAlpha
imImageReshape
imImageSetAlpha
imImageSetAttribInteger
imImageSetAttribReal
imImageSetAttribString
imImageSetAttribute
imImageSetBinary
imImageSetGray
imImageSetMap
imImageSetPalette
imLoadMap
imLoadRGB
imMap2Gray
imMap2RGB
imPaletteBlackBody
imPaletteBlue
imPaletteBlueIce
imPaletteCian
imPaletteDuplicate
imPaletteFindColor
imPaletteFindNearest
imPaletteGray
imPaletteGreen
imPaletteHighContrast
imPaletteHotIron
imPaletteHues
imPaletteLinear
imPaletteMagenta
imPaletteNew
imPaletteRainbow
imPaletteRed
imPaletteRelease
imPaletteUniform
imPaletteUniformIndex
imPaletteUniformIndexHalftoned
imPaletteYellow
imRegisterCallback
imResize
imRGB2Gray
imRGB2Map
imSaveMap
imSaveRGB
imStrCheck
imStrEqual
imStretch
imStrNLen
imVersion
imVersionDate
imVersionNumber
--*/

 -- iupgl.dll, according to dependency walker:
-------------------------------------------
--/*
IupGLCanvas
IupGLCanvasOpen
IupGLIsCurrent
IupGLMakeCurrent
IupGLPalette
IupGLSwapBuffers
IupGLUseFont
IupGLWait
--*/

 -- im_process.dll, according to dependency walker:
--------------------------------------------------
--/*
imAnalyzeFindRegions
imAnalyzeMeasureArea
imAnalyzeMeasureCentroid
imAnalyzeMeasureHoles
imAnalyzeMeasurePerimArea
imAnalyzeMeasurePerimeter
imAnalyzeMeasurePrincipalAxis
imCalcByteHistogram
imCalcCountColors
imCalcGrayHistogram
imCalcHistogram
imCalcHistogramStatistics
imCalcHistoImageStatistics
imCalcImageStatistics
imCalcPercentMinMax
imCalcRMSError
imCalcShortHistogram
imCalcSNR
imCalcUShortHistogram
imGaussianKernelSize2StdDev
imGaussianStdDev2KernelSize
imHistogramNew
imHistogramRelease
imHistogramShift
imKernelBarlett5x5
imKernelCircularMean5x5
imKernelCircularMean7x7
imKernelEnhance
imKernelGaussian3x3
imKernelGaussian5x5
imKernelGradian3x3
imKernelGradian7x7
imKernelKirsh
imKernelLaplacian4
imKernelLaplacian5x5
imKernelLaplacian7x7
imKernelLaplacian8
imKernelMean3x3
imKernelMean5x5
imKernelMean7x7
imKernelPrewitt
imKernelSculpt
imKernelSobel
imKernelTopHat5x5
imKernelTopHat7x7
imProcessAbnormalHyperionCorrection
imProcessAddMargins
imProcessArithmeticConstOp
imProcessArithmeticOp
imProcessAutoCovariance
imProcessBarlettConvolve
imProcessBinMorphClose
imProcessBinMorphConvolve
imProcessBinMorphDilate
imProcessBinMorphErode
imProcessBinMorphOpen
imProcessBinMorphOutline
imProcessBinMorphThin
imProcessBitMask
imProcessBitPlane
imProcessBitwiseNot
imProcessBitwiseOp
imProcessBlend
imProcessBlendConst
imProcessCalcAutoGamma
imProcessCalcRotateSize
imProcessCanny
imProcessCompassConvolve
imProcessCompose
imProcessConvertColorSpace
imProcessConvertDataType
imProcessConvertToBitmap
imProcessConvolve
imProcessConvolveDual
imProcessConvolveRep
imProcessConvolveSep
imProcessCrop
imProcessDiffOfGaussianConvolve
imProcessDifusionErrThreshold
imProcessDirectConv
imProcessDistanceTransform
imProcessEqualizeHistogram
imProcessExpandHistogram
imProcessFillHoles
imProcessFlip
imProcessGaussianConvolve
imProcessGrayMorphClose
imProcessGrayMorphConvolve
imProcessGrayMorphDilate
imProcessGrayMorphErode
imProcessGrayMorphGradient
imProcessGrayMorphOpen
imProcessGrayMorphTopHat
imProcessGrayMorphWell
imProcessHoughLines
imProcessHoughLinesDraw
imProcessHysteresisThresEstimate
imProcessHysteresisThreshold
imProcessInsert
imProcessInterlaceSplit
imProcessLapOfGaussianConvolve
imProcessLocalMaxThresEstimate
imProcessLocalMaxThreshold
imProcessMeanConvolve
imProcessMedianConvolve
imProcessMergeComplex
imProcessMergeComponents
imProcessMergeHSI
imProcessMinMaxThreshold
imProcessMirror
imProcessMultipleMean
imProcessMultipleMedian
imProcessMultipleStdDev
imProcessMultiplyConj
imProcessMultiPointColorOp
imProcessMultiPointOp
imProcessNegative
imProcessNormalizeComponents
imProcessNormDiffRatio
imProcessOpenMPSetMinCount
imProcessOpenMPSetNumThreads
imProcessOtsuThreshold
imProcessPercentThreshold
imProcessPerimeterLine
imProcessPixelate
imProcessPosterize
imProcessPrewittConvolve
imProcessQuantizeGrayUniform
imProcessQuantizeRGBUniform
imProcessRadial
imProcessRangeContrastThreshold
imProcessRangeConvolve
imProcessRankClosestConvolve
imProcessRankMaxConvolve
imProcessRankMinConvolve
imProcessReduce
imProcessReduceBy4
imProcessRegionalMaximum
imProcessRemoveByArea
imProcessRenderAddGaussianNoise
imProcessRenderAddSpeckleNoise
imProcessRenderAddUniformNoise
imProcessRenderBox
imProcessRenderChessboard
imProcessRenderCondOp
imProcessRenderCone
imProcessRenderConstant
imProcessRenderCosine
imProcessRenderFloodFill
imProcessRenderGaussian
imProcessRenderGrid
imProcessRenderLapOfGaussian
imProcessRenderOp
imProcessRenderRamp
imProcessRenderRandomNoise
imProcessRenderSinc
imProcessRenderTent
imProcessRenderWheel
imProcessReplaceColor
imProcessResize
imProcessRotate
imProcessRotate180
imProcessRotate90
imProcessRotateKernel
imProcessRotateRef
imProcessSetAlphaColor
imProcessSharp
imProcessSharpKernel
imProcessShiftHSI
imProcessSliceThreshold
imProcessSobelConvolve
imProcessSplineEdgeConvolve
imProcessSplitComplex
imProcessSplitComponents
imProcessSplitHSI
imProcessSplitYChroma
imProcessSwirl
imProcessThreshold
imProcessThresholdByDiff
imProcessToneGamut
imProcessUnArithmeticOp
imProcessUnaryPointColorOp
imProcessUnaryPointOp
imProcessUniformErrThreshold
imProcessUnNormalize
imProcessUnsharp
imProcessZeroCrossing
--*/

 -- x.dll, according to dependency walker:
-------------------------------------------
--/*
--*/

