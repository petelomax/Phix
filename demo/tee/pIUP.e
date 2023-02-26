--
--  pIUP.e
--  ======
--

--global type Ihandle(object o)
--  return atom(o) and o>=NULL and o=floor(o)
global type Ihandle(integer i)
    return i>0
end type

global type Ihandln(integer i)
    return i>=0
end type

sequence callbacks = {}

type cbfunc(atom cb)
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

global function rand_range(integer lo, integer hi)
    if lo>hi then {lo,hi} = {hi,lo} end if
    lo -= 1
    return lo+rand(hi-lo)
end function

global function ptr_array(sequence pointers)
atom pList

    pointers &= 0
    pList = allocate(length(pointers)*machine_word())
    pokeN(pList, pointers, machine_word())
    return pList
end function

--global 
function peek_double(object pDouble)
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

--global 
procedure poke_double(atom ptr, object data)
    if atom(data) then
        poke(ptr,atom_to_float64(data))
    else
        for i=1 to length(data) do
            poke(ptr+8*(i-1),atom_to_float64(data[i]))
        end for
    end if
end procedure

global constant
    IUP_ERROR       = 1,
    IUP_NOERROR     = 0,
    IUP_OPENED      = -1,
    IUP_IGNORE      = -1,
    IUP_DEFAULT     = -2,
    IUP_CLOSE       = -3,
    IUP_CONTINUE    = -4,
    -- IupPopup and IupShowXY Parameter Values
    IUP_CENTER       = 0xFFFF, /* 65535 */
    IUP_LEFT         = 0xFFFE, /* 65534 */
    IUP_RIGHT        = 0xFFFD, /* 65533 */
    IUP_MOUSEPOS     = 0xFFFC, /* 65532 */
    IUP_CURRENT      = 0xFFFB, /* 65531 */
    IUP_CENTERPARENT = 0xFFFA, /* 65530 */
    IUP_TOP          = IUP_LEFT,
    IUP_BOTTOM       = IUP_RIGHT,

    IUP_MASK_UINT = "/d+",

    K_ESC = 0xFF1B,

    ACTION = "ACTION",
    ACTION_CB = "ACTION_CB",
    $

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
    if chdir(dll_path)=0 then ?9/0 end if
    atom res = open_dll(fullpath)
    if chdir(curr_dir)=0 then ?9/0 end if
    return res
end function

constant
         D  = C_DOUBLE, 
--       F  = C_FLOAT,      -- NB: VM/pcfunc.e may not be up to this..
         I  = C_INT,
         L  = C_LONG,
         P  = C_PTR, 
         UC = C_UCHAR,
         UL = C_ULONG,
         $

constant atom iup = iup_open_dll({"iup.dll",
                                  "libiup.so",
                                  "libiup.dylib"})

constant atom iupControls = iup_open_dll({"iupcontrols.dll",
                                          "libiupcontrols.so",
                                          "libiupcontrols.dylib"})

constant atom iupimglib = iup_open_dll({"iupimglib.dll",
                                          "libiupimglib.so"})

constant
    xIupOpen = define_c_func(iup, "IupOpen", {P,P},I),
    xIupControlsOpen = define_c_proc(iupControls, "IupControlsOpen", {}),
    xIupImageLibOpen = define_c_proc(iupimglib, "IupImageLibOpen", {}),
    xIupClose = define_c_proc(iup, "IupClose", {}),
    xIupDialog = define_c_func(iup, "IupDialog", {P},P),
    xIupPopup = define_c_func(iup, "IupPopup", {P,I,I},I),
    xIupShow = define_c_func(iup, "IupShow", {P},I),
    xIupShowXY = define_c_func(iup, "IupShowXY", {P,I,I},I),
    xIupHide = define_c_proc(iup, "IupHide", {P}),
    xIupRefresh = define_c_proc(iup, "IupRefresh", {P}),
    xIupSetFocus = define_c_proc(iup, "IupSetFocus", {P}),
    xIupSetCallback = define_c_func(iup, "IupSetCallback", {P,P,P},P),
    xIupVersion = define_c_func(iup, "IupVersion", {},P),
    xIupVersionDate = define_c_func(iup, "IupVersionDate", {}, P),
    xIupVersionNumber = define_c_func(iup, "IupVersionNumber", {}, I),

    xIupSetAttribute = define_c_proc(iup, "IupSetAttribute", {P,P,P}),
    xIupSetAttributes = define_c_proc(iup, "IupSetAttributes", {P,P}),
    xIupSetAttributeHandle = define_c_proc(iup, "IupSetAttributeHandle", {P,P,P}),
    xIupSetHandle = define_c_proc(iup, "IupSetHandle", {P,P}),
    xIupSetInt = define_c_proc(iup, "IupSetInt", {P,P,I}),
    xIupSetStrAttribute = define_c_proc(iup, "IupSetStrAttribute", {P,P,P}),
    xIupStoreAttribute = define_c_proc(iup, "IupStoreAttribute", {P,P,P}),
    xIupGetAttribute = define_c_func(iup, "IupGetAttribute", {P,P},P),
    xIupGetInt = define_c_func(iup, "IupGetInt", {P,P},I),
    xIupGetInt2 = define_c_func(iup, "IupGetInt", {P,P},I),
    xIupGetIntInt = define_c_func(iup, "IupGetIntInt", {P,P,P,P},I),
--  xIupGetAllClasses = define_c_func(hIup, "IupGetAllClasses", {P,I},I),
    xIupGetClassName = define_c_func(iup, "IupGetClassName", {P},P),
--  xIupGetClassType = define_c_func(hIup, "IupGetClassType", {P},P),
--  xIupGetClassAttributes = define_c_func(hIup, "IupGetClassAttributes", {P,P,I},I),
--  xIupGetClassCallbacks = define_c_func(hIup, "IupGetClassCallbacks", {P,P,I},I),
--  xIupSaveClassAttributes = define_c_proc(hIup, "IupSaveClassAttributes", {P}),
--  xIupCopyClassAttributes = define_c_proc(hIup, "IupCopyClassAttributes", {P,P}),
--  xIupSetClassDefaultAttribute = define_c_proc(hIup, "IupSetClassDefaultAttribute", {P,P,P}),

    xIupAlarm = define_c_func(iup, "IupAlarm", {P,P,P,P,P},I),
    xIupMessage = define_c_proc(iup, "IupMessage", {P,P}),
    xIupButton = define_c_func(iup, "IupButton", {P,P},P),
    xIupCanvas = define_c_func(iup, "IupCanvas", {P},P),
    xIupCboxv = define_c_func(iup, "IupCboxv", {P},P),
    xIupFill  = define_c_func(iup, "IupFill", {},P),
    xIupFrame = define_c_func(iup, "IupFrame", {P},P),
    xIupHboxv = define_c_func(iup, "IupHboxv", {P},P),
    xIupImage = define_c_func(iup, "IupImage", {I,I,P},P),
    xIupImageRGB = define_c_func(iup, "IupImageRGB", {I,I,P},P),
    xIupImageRGBA = define_c_func(iup, "IupImageRGBA", {I,I,P},P),
    xIupLabel = define_c_func(iup, "IupLabel", {P},P),
    xIupList = define_c_func(iup, "IupList", {P},P),
    xIupMenuv = define_c_func(iup, "IupMenuv", {P},P),
    xIupItem = define_c_func(iup, "IupItem",  {P,P},P),
    xIupSeparator = define_c_func(iup, "IupSeparator", {},P),
    xIupSubmenu = define_c_func(iup, "IupSubmenu", {P,P},P),
    xIupProgressBar = define_c_func(iup, "IupProgressBar", {},P),
    xIupRadio = define_c_func(iup, "IupRadio", {P},P),
    xIupTabsv = define_c_func(iup, "IupTabsv", {P},P),
    xIupText = define_c_func(iup, "IupText", {P},P),
    xIupMultiLine = define_c_func(iup, "IupMultiLine", {P},P),
    xIupTextConvertLinColToPos = define_c_proc(iup, "IupTextConvertLinColToPos", {P,I,I,P}),
    xIupTextConvertPosToLinCol = define_c_proc(iup, "IupTextConvertPosToLinCol", {P,I,P,P}),
    xIupTimer = define_c_func(iup, "IupTimer", {},P),
    xIupToggle = define_c_func(iup, "IupToggle", {P,P},P),
    xIupTree = define_c_func(iup, "IupTree", {},P),
    xIupVal = define_c_func(iup, "IupVal", {P},P),
    xIupVboxv = define_c_func(iup, "IupVboxv", {P},P),
    xIupZboxv = define_c_func(iup, "IupZboxv", {P},P),

    xIupMap = define_c_func(iup, "IupMap", {P},I),
--  xIupUnmap = define_c_proc(iup, "IupUnmap", {P}),
    xIupMainLoop = define_c_proc(iup, "IupMainLoop", {}),
    xIupDestroy = define_c_proc(iup, "IupDestroy", {P}),
    xIupFileDlg = define_c_func(iup, "IupFileDlg", {},P),
    xIupFontDlg = define_c_func(iup, "IupFontDlg", {},P),
    xIupGetColor = define_c_func(iup, "IupGetColor", {I,I,P,P,P},I),
--  xIupGetParam = define_c_func(iup, "IupGetParam", {P,P,P,P,P,P,P,P,P,P,P,P,P,P,P},I),
    xIupGetParam = define_c_func(iup, "IupGetParam", {P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P},I),
    xIupGetParamv = define_c_func(iup, "IupGetParamv", {P,P,P,P,I,I,P}, I),
    xIupRecordInput = define_c_func(iup, "IupRecordInput", {P,I},I),
    xIupPlayInput = define_c_func(iup, "IupPlayInput", {P},I),
--/*
    xIupSetHandle = define_c_proc(iup, "IupSetHandle", {P,P}),
--  xIupGetHandle = define_c_func(iup, "IupGetHandle", {P},P),
--  xIupGetName = define_c_func(iup, "IupGetName", {P},P),
--  xIupGetAllNames = define_c_func(iup, "IupGetAllNames", {P,I},I),
--  xIupGetAllDialogs = define_c_func(iup, "IupGetAllDialogs", {P,I},I),
--  xIupGetAllClasses = define_c_func(iup, "IupGetAllClasses", {P,I},I),
    xIupGetClassName = define_c_func(iup, "IupGetClassName", {P},P),
--  xIupGetClassType = define_c_func(iup, "IupGetClassType", {P},P),
--  xIupGetClassAttributes = define_c_func(iup, "IupGetClassAttributes", {P,P,I},I),
--  xIupGetClassCallbacks = define_c_func(iup, "IupGetClassCallbacks", {P,P,I},I),
--  xIupSaveClassAttributes = define_c_proc(iup, "IupSaveClassAttributes", {P}),
--  xIupCopyClassAttributes = define_c_proc(iup, "IupCopyClassAttributes", {P,P}),
--  xIupSetClassDefaultAttribute = define_c_proc(iup, "IupSetClassDefaultAttribute", {P,P,P}),
--*/
    xIupSetGlobal = define_c_proc(iup, "IupSetGlobal", {P,P}),
    xIupGetGlobal = define_c_func(iup, "IupGetGlobal", {P},P),
    xiupKeyCodeToName = define_c_func(iup, "iupKeyCodeToName", {I},P),
--  xIupAppend = define_c_func(iup, "IupAppend", {P,P},P),
--  xIupDetach = define_c_proc(iup, "IupDetach", {P}),
--  xIupInsert = define_c_func(iup, "IupInsert", {P,P,P},P),
--  xIupReparent = define_c_func(iup, "IupReparent", {P,P,P},P),
    xIupGetParent = define_c_func(iup, "IupGetParent", {P},P),
    xIupGetChild = define_c_func(iup, "IupGetChild", {P,I},P),
--  xIupGetChildPos = define_c_func(iup, "IupGetChildPos", {P,P},I),
--  xIupGetChildCount = define_c_func(iup, "IupGetChildCount", {P},I),
--  xIupGetNextChild = define_c_func(iup, "IupGetNextChild", {P,P},P),
    xIupGetBrother = define_c_func(iup, "IupGetBrother", {P},P),
    xIupGetDialog = define_c_func(iup, "IupGetDialog", {P},P),
    xIupGetDialogChild = define_c_func(iup, "IupGetDialogChild", {P,P},P),
    xIupClipboard = define_c_func(iup, "IupClipboard", {},P),

    xIupConfig                        = define_c_func(iup, "IupConfig", {},P),
    xIupConfigLoad                    = define_c_func(iup, "IupConfigLoad", {P},I),
    xIupConfigSave                    = define_c_func(iup, "IupConfigSave", {P},I),
    xIupConfigSetVariableStr          = define_c_proc(iup, "IupConfigSetVariableStr", {P,P,P,P}),
    xIupConfigSetVariableStrId        = define_c_proc(iup, "IupConfigSetVariableStrId", {P,P,P,I,P}),
    xIupConfigSetVariableInt          = define_c_proc(iup, "IupConfigSetVariableInt", {P,P,P,I}),
    xIupConfigSetVariableIntId        = define_c_proc(iup, "IupConfigSetVariableIntId", {P,P,P,I,I}),
    xIupConfigSetVariableDouble       = define_c_proc(iup, "IupConfigSetVariableDouble", {P,P,P,D}),
    xIupConfigSetVariableDoubleId     = define_c_proc(iup, "IupConfigSetVariableDoubleId", {P,P,P,I,D}),
    xIupConfigGetVariableStr          = define_c_func(iup, "IupConfigGetVariableStr", {P,P,P},P),
    xIupConfigGetVariableStrId        = define_c_func(iup, "IupConfigGetVariableStrId", {P,P,P,I},P),
    xIupConfigGetVariableInt          = define_c_func(iup, "IupConfigGetVariableInt", {P,P,P},I),
    xIupConfigGetVariableIntId        = define_c_func(iup, "IupConfigGetVariableIntId", {P,P,P,I},I),
    xIupConfigGetVariableDouble       = define_c_func(iup, "IupConfigGetVariableDouble", {P,P,P},D),
    xIupConfigGetVariableDoubleId     = define_c_func(iup, "IupConfigGetVariableDoubleId", {P,P,P,I},D),
    xIupConfigGetVariableStrDef       = define_c_func(iup, "IupConfigGetVariableStrDef", {P,P,P,P},P),
    xIupConfigGetVariableStrIdDef     = define_c_func(iup, "IupConfigGetVariableStrIdDef", {P,P,P,I,P},P),
    xIupConfigGetVariableIntDef       = define_c_func(iup, "IupConfigGetVariableIntDef", {P,P,P,I},I),
    xIupConfigGetVariableIntIdDef     = define_c_func(iup, "IupConfigGetVariableIntIdDef", {P,P,P,I,I},I),
    xIupConfigGetVariableDoubleDef    = define_c_func(iup, "IupConfigGetVariableDoubleDef", {P,P,P,D},D),
    xIupConfigGetVariableDoubleIdDef  = define_c_func(iup, "IupConfigGetVariableDoubleIdDef", {P,P,P,I,D},D),
    xIupConfigRecentInit              = define_c_proc(iup, "IupConfigRecentInit", {P,P,P,I}),
    xIupConfigRecentUpdate            = define_c_proc(iup, "IupConfigRecentUpdate", {P,P}),
    xIupConfigDialogShow              = define_c_proc(iup, "IupConfigDialogShow", {P,P,P}),
    xIupConfigDialogClosed            = define_c_proc(iup, "IupConfigDialogClosed", {P,P,P}),

    xIupHelp = define_c_func(iup, "IupHelp", {P},I),
    $

--DEV
--sequence cmd = command_line()
--sequence argv = cmd[3..$]
--integer argc = length(argv)
--
--  {} = IupOpen(argc, argv);
global procedure IupOpen()
    if c_func(xIupOpen, {NULL,NULL})=IUP_ERROR then ?9/0 end if
end procedure

integer did_iup_controls_open = 0

procedure controls_open()
    if not did_iup_controls_open then
        c_proc(xIupControlsOpen, {})
    end if
    did_iup_controls_open = 1
end procedure

global procedure IupImageLibOpen()
    c_proc(xIupImageLibOpen, {})
end procedure

global procedure IupClose()
    c_proc(xIupClose, {})
end procedure

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

global procedure IupRefresh(Ihandle ih)
    c_proc(xIupRefresh, {ih})
end procedure

global procedure IupSetFocus(Ihandle ih)
    c_proc(xIupSetFocus, {ih})
end procedure

global function iup_version()
    -- static value internal to iup, do not need to free
    return peek_string(c_func(xIupVersion))
end function

--char* IupVersion(void);
public function IupVersion()
atom ptr = c_func(xIupVersion, {})
sequence str = ""
    if ptr!=NULL then str = peek_string(ptr) end if
    return str
end function

--char* IupVersionDate(void);
public function IupVersionDate()
atom ptr = c_func(xIupVersionDate, {})
sequence str = ""
    if ptr!=NULL then str = peek_string(ptr) end if
    return str
end function

--int IupVersionNumber(void);
public function IupVersionNumber()
atom result = c_func(xIupVersionNumber, {})
    return result
end function


global procedure IupSetAttribute(Ihandln ih, string name, atom_string val)
    if name!=upper(name) then ?9/0 end if
    c_proc(xIupSetAttribute, {ih, name, val})
end procedure

global procedure IupSetStrAttribute(Ihandle ih, string name, nullable_string val, sequence data = {})
    if name!=upper(name) then ?9/0 end if
    if length(data) then
        val = sprintf(val, data)
    end if
    c_proc(xIupSetStrAttribute, {ih, name, val})
end procedure

global procedure IupSetInt(Ihandle ih, string name, integer v)
    c_proc(xIupSetInt, {ih,name,v})
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

global procedure IupSetHandle(string name, Ihandle ih)
    c_proc(xIupSetHandle, {name, ih})
end procedure

global function IupGetAttribute(Ihandle ih, string name)
    atom pValue = c_func(xIupGetAttribute, {ih, name})
    if pValue=NULL then return NULL end if
    return peek_string(pValue)
end function

global function IupGetInt(Ihandle ih, string name)
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
    res = {peek4s(pTwoInts),peek4s(pTwoInts+4)}
    free(pTwoInts)
    return res
end function

global procedure IupSetGlobal(string name, nullable_string v)
    c_proc(xIupSetGlobal, {name, v})
end procedure

global function IupGetGlobal(string name)
    return peek_string(c_func(xIupGetGlobal, {name}))
end function

global function IupGetGlobalInt(string name)
    return c_func(xIupGetGlobal, {name})
end function

global function IupGetClassName(Ihandle ih)
    return peek_string(c_func(xIupGetClassName, {ih}))
end function

global procedure IupSetCallback(Ihandle ih, string name, cbfunc func)
--  func = c_func(xIupSetCallback, {ih, name, func})
    atom prev = c_func(xIupSetCallback, {ih, name, func})
end procedure

global function IupSetCallbackf(Ihandle ih, string name, cbfunc func)
    IupSetCallback(ih, name, func)
    return ih
end function

global function IupAlarm(string title, string msg, string b1, nullable_string b2 = NULL, nullable_string b3 = NULL)
    return c_func(xIupAlarm, {title,msg,b1,b2,b3})
end function

global procedure IupMessage(nullable_string title = NULL, nullable_string msg = NULL)
    c_proc(xIupMessage, {title,msg})
end procedure

global function IupButton(string title, nullable_string action = NULL, atom func = NULL, string attributes = "", sequence data = {})
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

global function IupFill()
    Ihandle ih = c_func(xIupFill, {})
    return ih
end function

global function IupFrame(Ihandle child, string attributes = "", sequence data = {})
    Ihandle ih = c_func(xIupFrame, {child})
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupImage(integer width, integer height, sequence pixels)
atom pPixels = allocate(length(pixels))
    poke(pPixels, pixels)
    Ihandle ih = c_func(xIupImage, {width, height, pPixels})
    free(pPixels)
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

global function IupLabel(string title)
    Ihandle ih = c_func(xIupLabel, {title})
    return ih
end function

global function IupList(nullable_string action = NULL)
    Ihandle ih = c_func(xIupList, {action})
    return ih
end function

global function IupMenu(sequence children)
atom pChildren = ptr_array(children)
Ihandle ih = c_func(xIupMenuv, {pChildren})
    free(pChildren)
    return ih
end function

global function IupMenuItem(string title, nullable_string action = NULL, atom func = NULL, string attributes = "", sequence data = {})
    Ihandle ih = c_func(xIupItem, {title, action})
    if func!=0 then
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

global function IupSubmenu(nullable_string title, Ihandle menu)
    Ihandle ih = c_func(xIupSubmenu, {title, menu})
    return ih
end function

global function IupProgressBar()
    Ihandle ih = c_func(xIupProgressBar, {})
    return ih
end function

global function IupRadio(Ihandle pChild)
    Ihandle ih = c_func(xIupRadio, {pChild})
    return ih
end function

global function IupTabs(sequence children)
atom pChildren = ptr_array(children)
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
    return peek4s(pPos)
end function

global function IupTextConvertPosToLinCol(Ihandle ih, integer pos)
atom pLineCol = allocate(16,1)
    c_proc(xIupTextConvertPosToLinCol, {ih,pos,pLineCol,pLineCol+4})
    return peek4s({pLineCol,2})
end function

global function IupTimer(atom func = NULL, integer msecs = 0, integer active = 1)
    Ihandle ih = c_func(xIupTimer, {})
    if func!=NULL and msecs!=0 then
        IupSetCallback(ih, ACTION_CB, func)
        IupSetAttributes(ih, "TIME=%d,RUN=%s", {msecs, iff(active, "YES", "NO")})
    end if
    return ih
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

global function IupCbox(sequence children, string attributes = "", sequence data = {})
atom pChildren = ptr_array(children)
    Ihandle ih = c_func(xIupCboxv, {pChildren})
    free(pChildren)
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupHbox(sequence children, string attributes = "", sequence data = {})
atom pChildren = ptr_array(children)
    Ihandle ih = c_func(xIupHboxv, {pChildren})
    free(pChildren)
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupVbox(sequence children, string attributes = "", sequence data = {})
atom pChildren = ptr_array(children)
    Ihandle ih = c_func(xIupVboxv, {pChildren})
    free(pChildren)
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupZbox(sequence children, string attributes = "", sequence data = {})
atom pChildren = ptr_array(children)
    Ihandle ih = c_func(xIupZboxv, {pChildren})
    free(pChildren)
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global procedure IupMap(Ihandle ih)
    if c_func(xIupMap, {ih})!=IUP_NOERROR then ?9/0 end if
end procedure

global procedure IupMainLoop()
    c_proc(xIupMainLoop, {})
end procedure

global function IupFileDlg()
    Ihandle ih = c_func(xIupFileDlg, {})
    return ih
end function

global function IupFontDlg()
    Ihandle ih = c_func(xIupFontDlg, {})
    return ih
end function

global function IupGetColor(integer x=IUP_CENTERPARENT, integer y=IUP_CENTERPARENT, integer r=255, integer g=255, integer b=255)
atom pR, pG, pB

    pR = allocate(1)
    poke(pR, r)

    pG = allocate(1)
    poke(pG, g)

    pB = allocate(1)
    poke(pB, b)

    integer result = c_func(xIupGetColor, {x, y, pR, pG, pB})

    r = peek(pR)
    g = peek(pG)
    b = peek(pB)

    free(pR)
    free(pG)
    free(pB)

    return {result, r, g, b}
end function

function iup_ptr_array(sequence pointers)
atom pList
    pointers &= 0
    pList = allocate(length(pointers)*machine_word())
    pokeN(pList, pointers, machine_word())
    return pList
end function

global function IupGetParamv(string title, cbfunc action, atom user_data, string fmt, sequence param_data = {})
--?                                         cbfunc?                                  ?                    ?
--  atom p_param_data = iup_ptr_array(param_data)
--  atom result = c_func(xIupGetParamv, {title,action,user_data,fmt,param_count,param_extra,p_param_data})
--? atom result = c_func(xIupGetParamv, {title,action,user_data,fmt,?length(param_data),param_extra,p_param_data})
--  free(p_param_data)
--  return result

--?atom pRid = 0
integer param_count = 0-- length(param_data)
--sequence pN = repeat(0,21)

    controls_open()

--  if la>20 then
--      -- if this triggers, because you genuinely want >20 args, just
--      --  increase the define_c_func and the 20/21 in here to match.
--      ?9/0
--  end if
--
--DEV??
--  if rid>0 then
--      pRid = call_back({'+', rid})
--  end if

    integer fskip = 0
    integer param_extra = 0
    string fmts = ""
    for i=1 to length(fmt) do
        if fskip then
            fskip -= 1
        elsif fmt[i]='%' then
            integer fi = fmt[i+1]
            if fi='%' then
                fskip = 1
--          elsif not find(fi,"ut") then
            elsif find(fi,"uth") then   -- dunno what to do with a handle...
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

global function IupGetParam(string title, integer rid, object data, string fmt, sequence args)
atom pRid = 0
integer la = length(args)
sequence pN = repeat(0,21)

    controls_open()

    if la>20 then
        -- if this triggers, because you genuinely want >20 args, just
        --  increase the define_c_func and the 20/21 in here to match.
        ?9/0
    end if

    if rid>0 then
        pRid = call_back({'+', rid})
    end if

    integer fskip = 0
    string fmts = ""
    for i=1 to length(fmt) do
        if fskip then
            fskip -= 1
        elsif fmt[i]='%' then
            integer fi = fmt[i+1]
            if fi='%' then
                fskip = 1
--          elsif not find(fi,"ut") then
            elsif not find(fi,"uth") then   -- dunno what to do with a handle...
                fmts = append(fmts,fi)
            end if
        end if
    end for
    if length(fmts)!=la then ?9/0 end if

    for i=la to 1 by -1 do
        object param = args[i]
        integer fi = fmts[i]
        atom p
        if find(fi,"bilo") then
            if not integer(param) then ?9/0 end if
            p = allocate(4)
            poke4(p, param)
        elsif find(fi,"ra") then
            if not atom(param) then ?9/0 end if
            p = allocate(4)
            poke(p,atom_to_float32(param))
        elsif find(fi,"RA") then
            if not atom(param) then ?9/0 end if
            p = allocate(8)
            poke(p,atom_to_float64(param))
        elsif find(fi,"smfcn") then
            if not string(param) then ?9/0 end if
            -- (feel free to increase this if needed)
            -- DEV: Its default value is 10240 for multiline strings, 4096 for file names, and 512 for other strings.<br>
            p = allocate(1024)
            mem_set(p, 0, 1024)
            poke(p, param)
        else
            -- unknown parameter type
            ?9/0
        end if
        pN[i] = p
    end for

    integer result = c_func(xIupGetParam, {title, pRid, data, fmt}&pN)

    sequence vals = repeat(0,la)&result

    for i=la to 1 by -1 do
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

global procedure IupRecordInput(nullable_string filename, integer mode)
    if c_func(xIupRecordInput, {filename,mode})!=IUP_NOERROR then ?9/0 end if
end procedure

global procedure IupPlayInput(nullable_string filename = NULL)
    if c_func(xIupPlayInput, {filename})!=IUP_NOERROR then ?9/0 end if
end procedure

global function iupKeyCodeToName(atom ch)
    atom pKeyName = c_func(xiupKeyCodeToName,{ch})
    return peek_string(pKeyName)
end function

--/*
global procedure IupAppend(Ihandle ih, Ihandle child)
    Ihandle parent = c_func(xIupAppend, {ih,child}) -- (error if NULL/fail)
end procedure

global function IupInsert(Ihandle ih, Ihandln ref_child, Ihandle child)
    Ihandle parent = c_func(xIupInsert, {ih,ref_child,child})   -- (error if NULL/fail)
end procedure
--*/

global function IupGetParent(Ihandle ih)
    Ihandln parent = c_func(xIupGetParent, {ih})
    return parent
end function

global function IupGetChild(Ihandle ih, integer pos)
    Ihandln child = c_func(xIupGetChild, {ih, pos})
    return child
end function

--global function IupGetChildPos(Ihandle ih, Ihandle child)
--  integer pos = c_func(xIupGetChildPos, {ih, child})
--  return pos
--end function
--
--global function IupGetChildCount(Ihandle ih)
--  integer n = c_func(xIupGetChildCount, {ih})
--  return n
--end function
--
--global function IupGetNextChild(Ihandle ih, Ihandle child)
--  Ihandln child = c_func(xIupGetNextChild, {ih,child})
--  return child
--end function

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

global function IupClipboard()
    Ihandle ih = c_func(xIupClipboard, {})
    return ih
end function

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

global procedure IupConfigSetVariableStr(Ihandle ih, string group, string key, string v)
    c_proc(xIupConfigSetVariableStr, {ih,group,key,v})
end procedure

--void IupConfigSetVariableStrId(Ihandle* ih, const char* group, const char* key, int id, const char* v);
public procedure IupConfigSetVariableStrId(atom ih, object group, object key, atom id, object v = NULL)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    if sequence(v) then v = allocate_string(v,1) end if
    c_proc(xIupConfigSetVariableStrId, {ih,group,key,id,v})
end procedure

global procedure IupConfigSetVariableInt(Ihandle ih, string group, string key, integer v)
    c_proc(xIupConfigSetVariableInt, {ih,group,key,v})
end procedure

--void IupConfigSetVariableIntId(Ihandle* ih, const char* group, const char* key, int id, int v);
public procedure IupConfigSetVariableIntId(atom ih, object group, object key, atom id, atom v)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    c_proc(xIupConfigSetVariableIntId, {ih,group,key,id,v})
end procedure

--void IupConfigSetVariableDouble(Ihandle* ih, const char* group, const char* key, double v);
public procedure IupConfigSetVariableDouble(atom ih, object group, object key, atom v)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    c_proc(xIupConfigSetVariableDouble, {ih,group,key,v})
end procedure

--void IupConfigSetVariableDoubleId(Ihandle* ih, const char* group, const char* key, int id, double v);
public procedure IupConfigSetVariableDoubleId(atom ih, object group, object key, atom id, atom v)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    c_proc(xIupConfigSetVariableDoubleId, {ih,group,key,id,v})
end procedure

global function IupConfigGetVariableStr(Ihandle ih, string group, string key)
    atom pString = c_func(xIupConfigGetVariableStr, {ih,group,key})
    string res = iff(pString=NULL?"":peek_string(pString))
    return res
end function

--const char* IupConfigGetVariableStrId(Ihandle* ih, const char* group, const char* key, int id);
public function IupConfigGetVariableStrId(atom ih, object group, object key, atom id)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    atom result = c_func(xIupConfigGetVariableStrId, {ih,group,key,id})
    return peek_string(result)
end function

--int IupConfigGetVariableInt(Ihandle* ih, const char* group, const char* key);
public function IupConfigGetVariableInt(atom ih, object group = NULL, object key = NULL)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    atom result = c_func(xIupConfigGetVariableInt, {ih,group,key})
    return result
end function

--int IupConfigGetVariableIntId(Ihandle* ih, const char* group, const char* key, int id);
public function IupConfigGetVariableIntId(atom ih, object group, object key, atom id)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    atom result = c_func(xIupConfigGetVariableIntId, {ih,group,key,id})
    return result
end function

--double IupConfigGetVariableDouble(Ihandle* ih, const char* group, const char* key);
public function IupConfigGetVariableDouble(atom ih, object group = NULL, object key = NULL)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    atom result = c_func(xIupConfigGetVariableDouble, {ih,group,key})
    return result
end function

--double IupConfigGetVariableDoubleId(Ihandle* ih, const char* group, const char* key, int id);
public function IupConfigGetVariableDoubleId(atom ih, object group, object key, atom id)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    atom result = c_func(xIupConfigGetVariableDoubleId, {ih,group,key,id})
    return result
end function

--const char* IupConfigGetVariableStrDef(Ihandle* ih, const char* group, const char* key, const char* def);
public function IupConfigGetVariableStrDef(atom ih, object group = NULL, object key = NULL, object def = NULL)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    if sequence(def) then def = allocate_string(def,1) end if
    atom result = c_func(xIupConfigGetVariableStrDef, {ih,group,key,def})
    return peek_string(result)
end function

--const char* IupConfigGetVariableStrIdDef(Ihandle* ih, const char* group, const char* key, int id, const char* def);
public function IupConfigGetVariableStrIdDef(atom ih, object group, object key, atom id, object def = NULL)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    if sequence(def) then def = allocate_string(def,1) end if
    atom result = c_func(xIupConfigGetVariableStrIdDef, {ih,group,key,id,def})
    return peek_string(result)
end function

global function IupConfigGetVariableIntDef(Ihandle ih, string group, string key, integer def)
    integer res = c_func(xIupConfigGetVariableIntDef, {ih,group,key,def})
    return res
end function

--int IupConfigGetVariableIntIdDef(Ihandle* ih, const char* group, const char* key, int id, int def);
public function IupConfigGetVariableIntIdDef(atom ih, object group, object key, atom id, atom def)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    atom result = c_func(xIupConfigGetVariableIntIdDef, {ih,group,key,id,def})
    return result
end function

--double IupConfigGetVariableDoubleDef(Ihandle* ih, const char* group, const char* key, double def);
public function IupConfigGetVariableDoubleDef(atom ih, object group, object key, atom def)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    atom result = c_func(xIupConfigGetVariableDoubleDef, {ih,group,key,def})
    return result
end function

--double IupConfigGetVariableDoubleIdDef(Ihandle* ih, const char* group, const char* key, int id, double def);
public function IupConfigGetVariableDoubleIdDef(atom ih, object group, object key, atom id, atom def)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    atom result = c_func(xIupConfigGetVariableDoubleIdDef, {ih,group,key,id,def})
    return result
end function

/****************************************************************/
--void IupConfigRecentInit(Ihandle* ih, Ihandle* menu, Icallback recent_cb, int max_recent);
public procedure IupConfigRecentInit(atom ih, atom menu, atom recent_cb, atom max_recent)
    c_proc(xIupConfigRecentInit, {ih,menu,recent_cb,max_recent})
end procedure

--void IupConfigRecentUpdate(Ihandle* ih, const char* filename);
public procedure IupConfigRecentUpdate(atom ih, object filename = NULL)
    if sequence(filename) then filename = allocate_string(filename,1) end if
    c_proc(xIupConfigRecentUpdate, {ih,filename})
end procedure

global procedure IupConfigDialogShow(Ihandle ih, Ihandle dialog, string name)
    c_proc(xIupConfigDialogShow, {ih,dialog,name})
end procedure

global procedure IupConfigDialogClosed(Ihandle ih, Ihandle dialog, string name)
    c_proc(xIupConfigDialogClosed, {ih,dialog,name})
end procedure

global function IupHelp(string url)
-- returns 1 if successful, -1 if failed, -2 if file not found
    return c_func(xIupHelp, {url})
end function

global procedure IupDestroy(Ihandle ih)
    c_proc(xIupDestroy, {ih})
end procedure


--global type cdCanvas(object o)
--  return atom(o) and o>=NULL and o=floor(o)
global type cdCanvas(integer i)
    return i>0
end type


global constant
    CD_QUERY = -1,
    CD_ERROR = -1,
    CD_OK = 0,
    
    /* Context Capabilities */
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
    CD_CAP_ALL              = 0xFFFFFFFF,

    CD_PLAIN = 0,
    CD_BOLD = 1,
    CD_ITALIC = 2,
    CD_UNDERLINE = 4,
    CD_STRIKEOUT = 8,
    CD_BOLD_ITALIC = or_bits(CD_BOLD, CD_ITALIC),

    $

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
-- === Polygon Mode Constants

global constant
    CD_FILL = 0,
    CD_OPEN_LINES = 1,
    CD_CLOSED_LINES = 2,
    CD_CLIP = 3,
    CD_BEZIER = 4,
    CD_REGION = 5,
    CD_POLYCUSTOM = 10

constant atom hCd = iup_open_dll({"cd.dll", "libcd.so","libcd.dylib"})
constant atom hCdIup = iup_open_dll({"iupcd.dll", "libiupcd.so","libiupcd.dylib"})

constant
    xcdVersion = define_c_func(hCd, "cdVersion", {},P),
    xcdVersionDate = define_c_func(hCd, "cdVersionDate", {},P),
    xcdCanvasGetAttribute = define_c_func(hCd, "cdCanvasGetAttribute", {P,P},P),
    xcdCanvasGetColorPlanes = define_c_func(hCd, "cdCanvasGetColorPlanes", {P},I),
    xcdCanvasGetContext = define_c_func(hCd, "cdCanvasGetContext", {P},P),
    xcdCanvasFont = define_c_proc(hCd, "cdCanvasFont", {P,P,I,I}),
    xcdCanvasGetFont = define_c_proc(hCd, "cdCanvasGetFont", {P,P,P,P}),
--  xcdCanvasOrigin = define_c_proc(hCd, "cdCanvasOrigin", {P,I,I}),
    xcdfCanvasOrigin = define_c_proc(hCd, "cdfCanvasOrigin", {P,D,D}),
--  xcdCanvasGetOrigin = define_c_proc(hCd, "cdfCanvasGetOrigin", {P,P,P}),
    xcdfCanvasGetOrigin = define_c_proc(hCd, "cdfCanvasGetOrigin", {P,P,P}),
    xcdCanvasGetSize = define_c_proc(hCd, "cdCanvasGetSize", {P,I,I,P,P}),
    xcdCanvasGetTransform = define_c_func(hCd, "cdCanvasGetTransform", {P},P),
--  xcdCanvasUpdateYAxis = define_c_func(hCd, "cdCanvasUpdateYAxis", {P,P},I),
    xcdfCanvasUpdateYAxis = define_c_func(hCd, "cdfCanvasUpdateYAxis", {P,P},D),
--  xcdCanvasInvertYAxis = define_c_func(hCd, "cdCanvasInvertYAxis", {P,I},I),
    xcdfCanvasInvertYAxis = define_c_func(hCd, "cdfCanvasInvertYAxis", {P,D},D),
    xcdCanvasMM2Pixel = define_c_proc(hCd, "cdCanvasMM2Pixel", {P,D,D,P,P}),
--  xcdCanvasPixel2MM = define_c_proc(hCd, "cdCanvasPixel2MM", {P,I,I,P,P}),
--  xcdfCanvasMM2Pixel = define_c_proc(hCd, "cdfCanvasMM2Pixel", {P,D,D,P,P}),
    xcdfCanvasPixel2MM = define_c_proc(hCd, "cdfCanvasPixel2MM", {P,D,D,P,P}),
    xcdCanvasSetForeground = define_c_proc(hCd, "cdCanvasSetForeground", {P,I}),
    xcdCanvasForeground = define_c_func(hCd, "cdCanvasForeground", {P,L},L),
    xcdCanvasLineWidth = define_c_func(hCd, "cdCanvasLineWidth", {P,I},I),
    xcdCanvasLineStyle = define_c_func(hCd, "cdCanvasLineStyle", {P,I},I),
    xcdCanvasLine = define_c_proc(hCd, "cdCanvasLine", {P,I,I,I,I}),
    xcdCanvasBegin = define_c_proc(hCd, "cdCanvasBegin", {P,I}),
    xcdCanvasVertex = define_c_proc(hCd, "cdCanvasVertex", {P,I,I}),
    xcdCanvasEnd = define_c_proc(hCd, "cdCanvasEnd", {P}),
    xcdCanvasRect = define_c_proc(hCd, "cdCanvasRect", {P,I,I,I,I}),
    xcdCanvasArc = define_c_proc(hCd, "cdCanvasArc", {P,I,I,I,I,D,D}),
--  xcdCanvasVectorTextDirection = define_c_proc(hCd, "cdCanvasVectorTextDirection", {P,I,I,I,I}),
    xcdCanvasMultiLineVectorText = define_c_proc(hCd, "cdCanvasMultiLineVectorText", {P,I,I,P}),
    xcdCanvasGetVectorTextSize = define_c_proc(hCd, "cdCanvasGetVectorTextSize", {P,P,P,P}),
    xcdCanvasGetVectorTextBounds = define_c_proc(hCd, "cdCanvasGetVectorTextBounds", {P,P,I,I,P}),
    xwdCanvasGetVectorTextBounds = define_c_proc(hCd, "wdCanvasGetVectorTextBounds", {P,P,D,D,P}),
    xcdCanvasGetFontDim = define_c_proc(hCd, "cdCanvasGetFontDim", {P,P,P,P,P}),
    xcdCanvasGetTextSize = define_c_proc(hCd, "cdCanvasGetTextSize", {P,P,P,P}),
--  xcdCanvasGetTextBox = define_c_proc(hCd, "cdCanvasGetTextBox", {P,P,I,I,P,P,P,P}),
--  xcdCanvasGetTextBox = define_c_proc(hCd, "cdCanvasGetTextBox", {P,I,I,P,P,P,P,P}),
    xcdfCanvasGetTextBox = define_c_proc(hCd, "cdfCanvasGetTextBox", {P,D,D,P,P,P,P,P}),
    xwdCanvasGetTextBox = define_c_proc(hCd, "wdCanvasGetTextBox", {P,D,D,P,P,P,P,P}),
    xcdCanvasGetTextBounds = define_c_proc(hCd, "cdCanvasGetTextBounds", {P,I,I,P,P}),
--  xcdfCanvasGetTextBounds = define_c_proc(hCd, "cdCanvasGetTextBounds", {P,D,D,P,P}),
    xwdCanvasGetTextBounds = define_c_proc(hCd, "wdCanvasGetTextBounds", {P,D,D,P,P}),
    xcdCanvasGetImageRGB = define_c_proc(hCd, "cdCanvasGetImageRGB", {P,P,P,P,I,I,I,I}),
    xwdCanvasGetImageRGB = define_c_proc(hCd, "wdCanvasGetImageRGB", {P,P,P,P,D,D,I,I}),
    xcdCanvasPutImageRectRGB = define_c_proc(hCd, "cdCanvasPutImageRectRGB", {P,I,I,P,P,P,I,I,I,I,I,I,I,I}),
--  xcdCanvasTransformPoint = define_c_proc(hCd, "cdCanvasTransformPoint", {P,I,I,P,P}),
    xcdfCanvasTransformPoint = define_c_proc(hCd, "cdfCanvasTransformPoint", {P,D,D,P,P}),
    xwdCanvasWindow = define_c_proc(hCd, "wdCanvasWindow", {P,D,D,D,D}),
    xwdCanvasGetWindow = define_c_proc(hCd, "wdCanvasGetWindow", {P,P,P,P,P}),
    xwdCanvasViewport = define_c_proc(hCd, "wdCanvasViewport", {P,I,I,I,I}),
    xwdCanvasGetViewport = define_c_proc(hCd, "wdCanvasGetViewport", {P,P,P,P,P}),
    xcdContextCaps = define_c_func(hCd, "cdContextCaps", {P},UL),
    xcdCreateCanvas = define_c_func(hCd, "cdCreateCanvas", {P,P},P),
    xcdKillCanvas = define_c_proc(hCd, "cdKillCanvas", {P}),
    xcdContextIup = define_c_func(hCdIup, "cdContextIup", {},P),
    xcdContextPrinter = define_c_func(hCd, "cdContextPrinter", {},P),
    xcdContextPS = define_c_func(hCd, "cdContextPS", {},P),
    xcdContextPicture = define_c_func(hCd, "cdContextPicture", {},P),
    xcdEncodeColor = define_c_func(hCd, "cdEncodeColor", {UC,UC,UC},L),
    xcdCanvasPixel = define_c_proc(hCd, "cdCanvasPixel", {P,I,I,I})

global function cdEncodeColor(integer red, integer green, integer blue)
    integer color = c_func(xcdEncodeColor, {red, green, blue})
    return color
end function

global constant
    CD_IUP = c_func(xcdContextIup, {}),
    CD_PRINTER = c_func(xcdContextPrinter, {}),
    CD_PS = c_func(xcdContextPS, {}),
    CD_PICTURE = c_func(xcdContextPicture, {})
--/*
todo:
C:\Program Files (x86)\Phix\demo\tee\simple_paint.exw:898   rgb_canvas = cdCreateCanvas(CD_IMIMAGE, image);
C:\Program Files (x86)\Phix\demo\tee\simple_paint.exw:1044   cd_canvas = cdCreateCanvasf(CD_GL, "10x10 %g", res);
C:\Program Files (x86)\Phix\demo\tee\simple_paint.exw:1049   cd_canvas = cdCreateCanvas(CD_IUPDBUFFER, canvas);
C:\Program Files (x86)\Phix\demo\tee\simple_paint.exw:1222               cdCanvas* rgb_canvas = cdCreateCanvas(CD_IMIMAGE, image);
--*/

ifdef WINDOWS then
constant
    xcdContextNativeWindow = define_c_func(hCd, "cdContextNativeWindow", {},P),
    xcdGetScreenSize = define_c_proc(hCd, "cdGetScreenSize", {P,P,P,P}),
    xcdGetScreenColorPlanes = define_c_func(hCd, "cdGetScreenColorPlanes", {},I),
    $

global constant
    CD_NATIVEWINDOW = c_func(xcdContextNativeWindow, {})
end ifdef

global function cdCreateCanvas(atom hCdContext, atom_string data, sequence params={})
    if length(params) then
        data = sprintf(data,params)
    end if
    cdCanvas hCdCanvas = c_func(xcdCreateCanvas,{hCdContext,data})
    return hCdCanvas
end function

global function cdCanvasGetAttribute(cdCanvas hCdCanvas, string name)
atom fnVal
    fnVal = c_func(xcdCanvasGetAttribute, {hCdCanvas, name})
    if fnVal=NULL then return NULL end if
    return peek_string(fnVal)
end function

global function cdCanvasGetColorPlanes(atom hCdCanvas)
    integer p = c_func(xcdCanvasGetColorPlanes, {hCdCanvas})
    return p
end function

global function cdCanvasGetContext(atom hCdCanvas)
    atom hCdContext = c_func(xcdCanvasGetContext, {hCdCanvas})
    return hCdContext
end function

global procedure cdCanvasFont(atom hCdCanvas, nullable_string font, integer style, integer size)
    c_proc(xcdCanvasFont, {hCdCanvas, font, style, size})
end procedure

global function cdCanvasGetFont(atom hCdCanvas)
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

--global procedure cdCanvasOrigin(atom hCdCanvas, atom x, atom y)
--  c_proc(xcdCanvasOrigin, {hCdCanvas, x, y})
--end procedure

global procedure cdCanvasOrigin(atom hCdCanvas, atom x, atom y)
    c_proc(xcdfCanvasOrigin, {hCdCanvas, x, y})
end procedure

global function cdCanvasGetOrigin(atom hCdCanvas)
atom pX, pY
sequence origin

    pX = allocate(16)
    pY = pX+8
    c_proc(xcdfCanvasGetOrigin, {hCdCanvas, pX, pY})
    origin = peek_double({pX, 2})
    free(pX)
    return origin
end function

global function cdCanvasGetSize(cdCanvas hCdCanvas)
atom pWidth, pHeight, pWidth_mm, pHeight_mm
sequence size
--DEV machine_bits()?
    pWidth = allocate(2*4+2*8)
    pHeight = pWidth+4
    pWidth_mm = pHeight+4
    pHeight_mm = pWidth_mm+8
    c_proc(xcdCanvasGetSize, {hCdCanvas, pWidth, pHeight, pWidth_mm, pHeight_mm})
    size = peek4s({pWidth, 2}) & peek_double({pWidth_mm, 2})
    free(pWidth)
    return size
end function

global function cdCanvasGetTransform(atom hCdCanvas)
atom pMatrix
    pMatrix = c_func(xcdCanvasGetTransform, {hCdCanvas})
    return peek_double({pMatrix, 6})
end function

global procedure cdCanvasSetForeground(cdCanvas hCdCanvas, atom color)
    c_proc(xcdCanvasSetForeground, {hCdCanvas, color})
end procedure

global function cdCanvasGetForeground(cdCanvas hCdCanvas)
    integer color = c_func(xcdCanvasForeground, {hCdCanvas, CD_QUERY})
    return color
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
    integer style = c_func(xcdCanvasLineStyle, {hCdCanvas, CD_QUERY})
    return style
end function

global procedure cdCanvasArc(cdCanvas hCdCanvas, atom xc, atom yc, atom w, atom h, atom a1, atom a2)
    c_proc(xcdCanvasArc, {hCdCanvas, xc, yc, w, h, a1, a2})
end procedure

global procedure cdCanvasLine(cdCanvas hCdCanvas, atom x1, atom y1, atom x2, atom y2)
    c_proc(xcdCanvasLine, {hCdCanvas, x1, y1, x2, y2})
end procedure

global procedure cdCanvasRect(cdCanvas hCdCanvas, atom minX, atom minY, atom maxX, atom maxY)
    c_proc(xcdCanvasRect, {hCdCanvas, minX, minY, maxX, maxY})
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

global procedure cdCanvasMultiLineVectorText(cdCanvas hCdCanvas, atom x, atom y, string text)
    c_proc(xcdCanvasMultiLineVectorText, {hCdCanvas, x, y, text})
end procedure

global function cdCanvasGetVectorTextSize(atom hCdCanvas, sequence text)
atom pX, pY
sequence x_y
    pX = allocate(8)
    pY = pX+4
    c_proc(xcdCanvasGetVectorTextSize, {hCdCanvas, text, pX, pY})
    x_y = peek4s({pX, 2})
    free(pX)
    return x_y
end function

global function cdCanvasGetVectorTextBounds(atom hCdCanvas, string text, integer px, integer py)
atom pRect
sequence rect
    pRect = allocate(8*4)
    c_proc(xcdCanvasGetVectorTextBounds, {hCdCanvas, text, px, py, pRect})
    rect = peek4s({pRect, 8})
    free(pRect)
    return rect
end function

global function wdCanvasGetVectorTextBounds(atom hCdCanvas, sequence text, atom px, atom py)
atom pRect
sequence rect
    pRect = allocate(8*8)
    c_proc(xwdCanvasGetVectorTextBounds, {hCdCanvas, text, px, py, pRect})
    rect = peek_double({pRect, 8})
    free(pRect)
    return rect
end function

global function cdCanvasGetFontDim(atom hCdCanvas)
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

global function cdCanvasGetTextSize(atom hCdCanvas, string text)
atom pW, pH
sequence text_size
    pW = allocate(8)
    pH = pW+4
    c_proc(xcdCanvasGetTextSize, {hCdCanvas, text, pW, pH})
    text_size = peek4s({pW, 2})
    free(pW)
    return text_size
end function


global function cdCanvasGetTextBox(atom hCdCanvas, atom x, atom y, string text)
atom pXmin, pXmax, pYmin, pYmax
sequence box
    pXmin = allocate(32)
    pXmax = pXmin+8
    pYmin = pXmax+8
    pYmax = pYmin+8
    c_proc(xcdfCanvasGetTextBox, {hCdCanvas, x, y, text, pXmin, pXmax, pYmin, pYmax})
    box = peek_double({pXmin, 4})
    free(pXmin)
    return box
end function

global function wdCanvasGetTextBox(atom hCdCanvas, atom x, atom y, sequence text)
atom pXmin, pXmax, pYmin, pYmax
sequence box
    pXmin = allocate(32)
    pXmax = pXmin+8
    pYmin = pXmax+8
    pYmax = pYmin+8
    c_proc(xwdCanvasGetTextBox, {hCdCanvas, x, y, text, pXmin, pXmax, pYmin, pYmax})
    box = peek_double({pXmin, 4})
    free(pXmin)
    return box
end function

global function cdCanvasGetTextBounds(atom hCdCanvas, atom x, atom y, string text)
atom pRect
sequence bounds
    pRect = allocate(32)
    c_proc(xcdCanvasGetTextBounds, {hCdCanvas, x, y, text, pRect})
    bounds = peek4s({pRect, 8})
    free(pRect)
    return bounds
end function

--DEV crashes...
--global function cdCanvasGetTextBounds(atom hCdCanvas, atom x, atom y, string text)
--atom pRect
--sequence bounds
--  pRect = allocate(64)
--  c_proc(xcdfCanvasGetTextBounds, {hCdCanvas, x, y, text, pRect})
--  bounds = peek_double({pRect, 8})
--  free(pRect)
--  return bounds
--end function

global function wdCanvasGetTextBounds(atom hCdCanvas, atom x, atom y, string text)
atom pRect
sequence bounds
    pRect = allocate(64)
    c_proc(xwdCanvasGetTextBounds, {hCdCanvas, x, y, text, pRect})
    bounds = peek_double({pRect, 8})
    free(pRect)
    return bounds
end function

global function cdCanvasGetImageRGB(atom hCdCanvas, atom x, atom y, atom w, atom h)
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

global function wdCanvasGetImageRGB(atom hCdCanvas, atom x, atom y, atom w, atom h)
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

--global function cdCanvasUpdateYAxis(atom hCdCanvas, atom y)
--atom newy, pY
--  pY = allocate(machine_word())
--  pokeN(pY, y, machine_word())
--  newy = c_func(xcdCanvasUpdateYAxis, {hCdCanvas, pY})
--  if newy!=peekNS(pY,machine_word(),1) then ?9/0 end if
--  free(pY)
--  return newy
--end function

--global function cdCanvasTransformPoint(atom hCdCanvas, atom x, atom y)
--atom pX, pY
--sequence tx_ty
--  pX = allocate(8)
--  pY = pX+4
--  c_proc(xcdCanvasTransformPoint, {hCdCanvas, x, y, pX, pY})
--  tx_ty = peek4s({pX, 2})
--  free(pX)
--  return tx_ty
--end function

global function cdCanvasTransformPoint(atom hCdCanvas, atom x, atom y)
atom pX, pY
sequence tx_ty
    pX = allocate(16)
    pY = pX+8
    c_proc(xcdfCanvasTransformPoint, {hCdCanvas, x, y, pX, pY})
    tx_ty = peek_double({pX, 2})
    free(pX)
    return tx_ty
end function

global function cdCanvasUpdateYAxis(atom hCdCanvas, atom y)
atom newy, pY
    pY = allocate(8)
    poke_double(pY, y)
    newy = c_func(xcdfCanvasUpdateYAxis, {hCdCanvas, pY})
    -- if this triggers we may need to resurrect the int C function
atom dbg = peek_double(pY)
    if newy!=peek_double(pY) then ?9/0 end if
    free(pY)
    return newy
end function

--global function cdCanvasInvertYAxis(atom hCdCanvas, atom y)
--  return c_func(xcdCanvasInvertYAxis, {hCdCanvas, y})
--end function

global function cdCanvasInvertYAxis(atom hCdCanvas, atom y)
    atom newy = c_func(xcdfCanvasInvertYAxis, {hCdCanvas, y})
    return newy
end function

global function cdCanvasMM2Pixel(atom hCdCanvas, atom mm_dx, atom mm_dy)
atom pDx, pDy
sequence dx_dy
    pDx = allocate(machine_word()*2)
    pDy = pDx+machine_word()
    c_proc(xcdCanvasMM2Pixel, {hCdCanvas, mm_dx, mm_dy, pDx, pDy})
    dx_dy = peekNS({pDx, 2},machine_word(),1)
    free(pDx)
    return dx_dy
end function
--
--global function cdCanvasPixel2MM(atom hCdCanvas, atom dx, atom dy)
--atom pmm_dx, pmm_dy
--sequence mmdx_mmdy
--
--  pmm_dx = allocate(16)
--  pmm_dy = pmm_dx+8
--  c_proc(xcdCanvasPixel2MM, {hCdCanvas, dx, dy, pmm_dx, pmm_dy})
--  mmdx_mmdy = peek_double({pmm_dx, 2})
--  free(pmm_dx)
--  return mmdx_mmdy
--end function

--global function cdCanvasMM2Pixel(atom hCdCanvas, atom mm_dx, atom mm_dy)
--atom pDx, pDy
--sequence dx_dy
--  pDx = allocate(16)
--  pDy = pDx+8
--  c_proc(xcdfCanvasMM2Pixel, {hCdCanvas, mm_dx, mm_dy, pDx, pDy})
--  dx_dy = peek_double({pDx, 2})
--  free(pDx)
--  return dx_dy
--end function

global function cdCanvasPixel2MM(atom hCdCanvas, atom dx, atom dy)
atom pmm_dx, pmm_dy
sequence mmdx_mmdy

    pmm_dx = allocate(16)
    pmm_dy = pmm_dx+8
    c_proc(xcdfCanvasPixel2MM, {hCdCanvas, dx, dy, pmm_dx, pmm_dy})
    mmdx_mmdy = peek_double({pmm_dx, 2})
    free(pmm_dx)
    return mmdx_mmdy
end function

global procedure cdCanvasPixel(cdCanvas hCdCanvas, atom x, atom y, atom color)
    c_proc(xcdCanvasPixel, {hCdCanvas, x, y, color})
end procedure

global function cdGetScreenSize()
ifdef WINDOWS then
--DEV machine_bits?
atom w = allocate(24)
atom h = w+4
atom w_mm = h+4
atom h_mm = w_mm+8

    c_proc(xcdGetScreenSize, {w, h, w_mm, h_mm})

    sequence data = peek4s({w, 2}) & peek_double({w_mm, 2})

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

global function cdContextCaps(atom hCdContext)
    return c_func(xcdContextCaps, {hCdContext})
end function

global function cdVersion()
    return peek_string(c_func(xcdVersion, {}))
end function

global function cdVersionDate()
    return peek_string(c_func(xcdVersionDate, {}))
end function

 -- vector text attributes
--------------------------
constant
--  xcdCanvasVectorFont = define_c_func(hCd, "cdCanvasVectorFont", {P,P},P),
    xcdCanvasVectorTextDirection = define_c_proc(hCd, "cdCanvasVectorTextDirection", {P,I,I,I,I}),
--  xcdCanvasVectorTextTransform = define_c_func(hCd, "cdCanvasVectorTextTransform", {P,P},P),
--  xcdCanvasVectorTextSize = define_c_proc(hCd, "cdCanvasVectorTextSize", {P,I,I,P}),
--  xcdCanvasVectorCharSize = define_c_func(hCd, "cdCanvasVectorCharSize", {P,I},I),
    $

--global function cdCanvasVectorFont(atom hCdCanvas, string font)
--  atom pFont = c_func(xcdCanvasVectorFont, {hCdCanvas, font})
--  font = peek_string(pFont)
--  return font
--end function

global procedure cdCanvasVectorTextDirection(cdCanvas hCdCanvas, integer x1, integer y1, integer x2, integer y2)
    c_proc(xcdCanvasVectorTextDirection, {hCdCanvas, x1, y1, x2, y2})
end procedure

--global function canvas_vector_text_transform(atom hCdCanvas, object matrix=NULL)
--atom pPrevMatrix, pMatrix = NULL
--
--  if matrix!=NULL then
--      if length(matrix)!=6 then ?9/0 end if
--      pMatrix = allocate(8*6)
--      poke_double(pMatrix, matrix)
--  end if
--  pPrevMatrix = c_func(xcdCanvasVectorTextTransform, {hCdCanvas, pMatrix})
--  matrix = peek_double({pPrevMatrix, 6})
--  if pMatrix!=NULL then
--      free(pMatrix)
--  end if
--  return res
--end function

--global procedure canvas_vector_text_size(atom hCdCanvas, atom w, atom h, string text)
--  c_proc(xcdCanvasVectorTextSize, {hCdCanvas, w, h, text})
--end procedure
--
--global function canvas_vector_char_size(atom hCdCanvas, atom size)
--  return c_func(xcdCanvasVectorCharSize, {hCdCanvas, size})
--end function

 -- world draw attributes
-------------------------
constant 
--  xwdCanvasLineWidth = define_c_func(hCd, "wdCanvasLineWidth", {P,D},D),
    xwdCanvasFont = define_c_proc(hCd, "wdCanvasFont", {P,P,I,D}),
--  xwdCanvasGetFont = define_c_proc(hCd, "wdCanvasGetFont", {P,P,P,P}),
--  xwdCanvasMarkSize = define_c_func(hCd, "wdCanvasMarkSize", {P,D},D),
    xwdCanvasGetFontDim = define_c_proc(hCd, "wdCanvasGetFontDim", {P,P,P,P,P}),
    xwdCanvasGetTextSize = define_c_proc(hCd, "wdCanvasGetTextSize", {P,P,P,P}),
--  xwdCanvasGetTextBox = define_c_proc(hCd, "wdCanvasGetTextBox", {P,D,D,P,P,P,P,P}),
--  xwdCanvasGetTextBounds = define_c_proc(hCd, "wdCanvasGetTextbounds", {P,D,D,P,P}),
--  xwdCanvasStipple = define_c_proc(hCd, "wdCanvasStipple", {P,I,I,P,D,D}),
--  xwdCanvasPattern = define_c_proc(hCd, "wdCanvasPattern", {P,I,I,P,D,D})
    $

global procedure wdCanvasFont(atom hCdCanvas, nullable_string font, integer style, atom size)
    c_proc(xwdCanvasFont, {hCdCanvas, font, style, size})
end procedure

global function wdCanvasGetFontDim(atom hCdCanvas)
atom pWidth, pHeight, pAscent, pDescent
sequence font_metrics
    pWidth = allocate(32)
    pHeight = pWidth+8
    pAscent = pHeight+8
    pDescent = pAscent+8
    c_proc(xwdCanvasGetFontDim, {hCdCanvas, pWidth, pHeight, pAscent, pDescent})
    font_metrics = peek_double({pWidth, 4})
    free(pWidth)
    return font_metrics -- {width, height, ascent, descent}
end function

global function wdCanvasGetTextSize(atom hCdCanvas, sequence text)
atom pW, pH
sequence text_size

    pW = allocate(16)
    pH = pW+8
    c_proc(xwdCanvasGetTextSize, {hCdCanvas, text, pW, pH})
    text_size = peek_double({pW, 2})
    free(pW)
    return text_size
end function

--

global procedure wdCanvasWindow(cdCanvas hCdCanvas, atom xmin, atom xmax, atom ymin, atom ymax)
    c_proc(xwdCanvasWindow, {hCdCanvas, xmin, xmax, ymin, ymax})
end procedure

global function wdCanvasGetWindow(cdCanvas hCdCanvas)
atom pXmin, pXmax, pYmin, pYmax
sequence wdWindow

    pXmin = allocate(4*8)
    pXmax = pXmin+8
    pYmin = pXmax+8
    pYmax = pYmin+8
    c_proc(xwdCanvasGetWindow, {hCdCanvas, pXmin, pXmax, pYmin, pYmax})
    wdWindow = peek_double({pXmin, 4})
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

global procedure cdKillCanvas(atom hCdCanvas)
    c_proc(xcdKillCanvas, {hCdCanvas})
end procedure

