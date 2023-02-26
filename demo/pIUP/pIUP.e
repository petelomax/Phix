--
-- pIUP.e
-- ======
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
constant string dll_path = root_dir&sprintf("%s%d\\",{dirs[libidx],machine_bits()})

function iup_open_dll(sequence libs)
    string fullpath = dll_path&libs[libidx]
    if chdir(dll_path)=0 then ?9/0 end if
    atom res = open_dll(fullpath)
    if chdir(curr_dir)=0 then ?9/0 end if
    return res
end function

constant
         D  = C_DOUBLE, 
         F  = C_FLOAT,      -- NB: VM/pcfunc.e may not be up to this..
         I  = C_INT,
         L  = C_LONG,
         P  = C_PTR, 
--       UC = C_UCHAR,
         UL = C_ULONG,
         $


constant atom iup = iup_open_dll({"iup.dll",
                                  "libiup.so",
                                  "libiup.dylib"})

constant atom iupControls = iup_open_dll({"iupcontrols.dll",
                                           "libiupcontrols.so",
                                           "libiupcontrols.dylib"})

constant atom iupIm = iup_open_dll({"iupim.dll",
                                    "libiupim.so",
                                    "libiupim.dylib"})

constant
    xIupOpen = define_c_func(iup, "IupOpen", {P,P},I),
    xIupControlsOpen = define_c_proc(iupControls, "IupControlsOpen", {}),
    xIupClose = define_c_proc(iup, "IupClose", {}),
--  xIupVersion = define_c_func(iup, "IupVersion", {},P),
--  xIupLoad = define_c_func(iup, "IupLoad", {P},P),
--  xIupLoadBuffer = define_c_func(iup, "IupLoadBuffer", {P},P),
--  xIupSetLanguage = define_c_proc(iup, "IupSetLanguage", {P}),
--  xIupGetLanguage = define_c_func(iup, "IupGetLanguage", {},P),
--  xIupGetText = define_c_func(iup, "IupGetText", {P,P},P),
--  xIupListDialog = define_c_func(iup, "IupListDialog", {I,P,I,P,I,I,I,P},I),
--  xIupLayoutDialog = define_c_func(iup, "IupLayoutDialog", {P},P),
    xIupDialog = define_c_func(iup, "IupDialog", {P},P),
    xIupPopup = define_c_func(iup, "IupPopup", {P,I,I},I),
    xIupShow = define_c_func(iup, "IupShow", {P},I),
    xIupShowXY = define_c_func(iup, "IupShowXY", {P,I,I},I),
    xIupHide = define_c_proc(iup, "IupHide", {P}),
    xIupRefresh = define_c_proc(iup, "IupRefresh", {P}),
--  xIupRefreshChildren = define_c_proc(iup, "IupRefreshChildren", {P}),
--  xIupUpdate = define_c_proc(iup, "IupUpdate", {P}),
--  xIupUpdateChildren = define_c_proc(iup, "IupUpdate", {P}),
--  xIupRedraw = define_c_proc(iup, "IupRedraw", {P,I}),
--  xIupConvertXYToPos = define_c_func(iup, "IupConvertXYToPos", {P,I,I},I),
--  xIupNextField = define_c_func(iup, "IupNextField", {P},P),
--  xIupPreviousField = define_c_func(iup, "IupPreviousField", {P},P),
--  xIupGetFocus = define_c_func(iup, "IupGetFocus", {},P),
    xIupSetFocus = define_c_proc(iup, "IupSetFocus", {P}),
--  xIupGetCallback = define_c_func(iup, "IupGetCallback", {P,P},P),
    xIupSetCallback = define_c_func(iup, "IupSetCallback", {P,P,P},P),

    xIupSetAttribute = define_c_proc(iup, "IupSetAttribute", {P,P,P}),
    xIupSetAttributes = define_c_proc(iup, "IupSetAttributes", {P,P}),
    xIupSetAttributeHandle = define_c_proc(iup, "IupSetAttributeHandle", {P,P,P}),
    xIupSetStrAttribute = define_c_proc(iup, "IupSetStrAttribute", {P,P,P}),
    xIupSetInt = define_c_proc(iup, "IupSetInt", {P,P,I}),
    xIupStoreAttribute = define_c_proc(iup, "IupStoreAttribute", {P,P,P}),
--  xIupResetAttribute = define_c_proc(iup, "IupResetAttribute", {P,P}),
    xIupGetAttribute = define_c_func(iup, "IupGetAttribute", {P,P},P),
--  xIupGetAttributes = define_c_func(iup, "IupGetAttributes", {P},P),
--  xIupGetAttributeHandle = define_c_func(iup, "IupGetAttributeHandle", {P,P},P),
--  xIupGetAllAttributes = define_c_func(iup, "IupGetAllAttributes", {P,P,I},I),
--  xIupGetFloat = define_c_func(iup, "IupGetFloat", {P,P},F),
    xIupGetInt = define_c_func(iup, "IupGetInt", {P,P},I),
    xIupGetInt2 = define_c_func(iup, "IupGetInt", {P,P},I),
    xIupGetIntInt = define_c_func(iup, "IupGetIntInt", {P,P,P,P},I),
--  xIupGetGlobal = define_c_func(iup, "IupGetGlobal", {P},P),
--  xIupStoreGlobal = define_c_proc(iup, "IupStoreGlobal", {P,P}),

    xIupAlarm = define_c_func(iup, "IupAlarm", {P,P,P,P,P},I),
    xIupMessage  = define_c_proc(iup, "IupMessage", {P,P}),
    xIupButton = define_c_func(iup, "IupButton", {P,P},P),
    xIupCanvas = define_c_func(iup, "IupCanvas", {P},P),
    xIupFill  = define_c_func(iup, "IupFill", {},P),
    xIupFrame = define_c_func(iup, "IupFrame", {P},P),
    xIupHboxv = define_c_func(iup, "IupHboxv", {P},P),
    xIupImage = define_c_func(iup, "IupImage", {I,I,P},P),
    xIupImageRGB = define_c_func(iup, "IupImageRGB", {I,I,P},P),
    xIupImageRGBA = define_c_func(iup, "IupImageRGBA", {I,I,P},P),
    xIupLoadImage = define_c_func(iupIm, "IupLoadImage", {P},P),
--  xIupSaveImage = define_c_func(iupIm, "IupSaveImage", {P,P,P},I),
--  xIupGetNativeHandleImage = define_c_func(iupIm, "IupGetNativeHandleImage", {P},P),
--  xIupGetImageNativeHandle = define_c_func(iupIm, "IupGetImageNativeHandle", {P},P),
--  xIupSaveImageAsText = define_c_func(iup, "IupSaveImageAsText", {P,P,P,P},I)
    xIupLabel = define_c_func(iup, "IupLabel", {P},P),
    xIupList = define_c_func(iup, "IupList", {P},P),
    xIupMenuv = define_c_func(iup, "IupMenuv", {P},P),
    xIupItem = define_c_func(iup, "IupItem",  {P,P},P),
    xIupSeparator = define_c_func(iup, "IupSeparator", {},P),
    xIupSubmenu = define_c_func(iup, "IupSubmenu", {P,P},P),
    xIupProgressBar = define_c_func(iup, "IupProgressBar", {},P),
    xIupRadio = define_c_func(iup, "IupRadio", {P},P),
--  xIupSpin = define_c_func(iup, "IupSpin", {},P),
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

--  xIupUser = define_c_func(iup, "IupUser", {},P),
    xIupMap = define_c_func(iup, "IupMap", {P},I),
--  xIupUnmap = define_c_proc(iup, "IupUnmap", {P}),
    xIupMainLoop = define_c_proc(iup, "IupMainLoop", {}),
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
    xIupDestroy = define_c_proc(iup, "IupDestroy", {P}),
    xIupFileDlg = define_c_func(iup, "IupFileDlg", {},P),
--  xIupMessageDlg = define_c_func(iup, "IupMessageDlg", {},P),
--  xIupColorDlg = define_c_func(iup, "IupColorDlg", {},P),
    xIupFontDlg = define_c_func(iup, "IupFontDlg", {},P),
    xIupGetColor = define_c_func(iup, "IupGetColor", {I,I,P,P,P},I),
--  xIupGetFile = define_c_func(iup, "IupGetFile", {P},I),
--  xIupGetParam = define_c_func(iup, "IupGetParam", {P,P,P,P,P,P,P,P,P,P,P,P,P,P,P},I),
    xIupGetParam = define_c_func(iup, "IupGetParam", {P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P},I),
    xIupRecordInput = define_c_func(iup, "IupRecordInput", {P,I},I),
    xIupPlayInput = define_c_func(iup, "IupPlayInput", {P},I),

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

    xIupHelp = define_c_func(iup, "IupHelp", {P},I),
    $

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

--global procedure refresh_children(Ihandle ih)
--  c_proc(xIupRefreshChildren, {ih})
--end procedure
--
--global procedure update(Ihandle ih)
--  c_proc(xIupUpdate, {ih})
--end procedure
--
--global procedure update_children(Ihandle ih)
--  c_proc(xIupUpdateChildren, {ih})
--end procedure
--
--global procedure redraw(Ihandle ih, integer children)
--  c_proc(xIupRedraw, {ih, children})
--end procedure
--
--global function convert_xy_to_pos(Ihandle ih, integer x, integer y)
--  return c_func(xIupConvertXYToPos, {ih, x, y})
--end function

--global function IupNextField(atom h)
--  return c_func(xIupNextField, {h})
--end function
--
--global function IupPreviousField(atom h)
--  return c_func(xIupPreviousField, {h})
--end function

--global function IupGetFocus()
--  return c_func(xIupGetFocus, {})
--end function

global procedure IupSetFocus(Ihandle ih)
    c_proc(xIupSetFocus, {ih})
end procedure

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

global procedure IupSetCallback(Ihandle ih, string name, cbfunc func)
    atom prev = c_func(xIupSetCallback, {ih, name, func})
end procedure

global function IupSetCallbackf(Ihandle ih, string name, cbfunc func)
    IupSetCallback(ih, name, func)
    return ih
end function

global function IupAlarm(string title, string msg, string b1, nullable_string b2 = NULL, nullable_string b3 = NULL)
    return c_func(xIupAlarm, {title, msg, b1, b2, b3})
end function

global procedure IupMessage(nullable_string title = NULL, nullable_string msg = NULL)
    c_proc(xIupMessage, {title, msg})
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

--global function IupImageA(integer width, integer height, atom pPixels)
--  Ihandle ih = c_func(xIupImage, {width, height, pPixels})
--  return ih
--end function

global function IupLoadImage(string filename)
    Ihandln ih = c_func(xIupLoadImage, {filename})
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

--global function user()
--  return c_func(xIupUser, {})
--end function

global function IupClipboard()
    Ihandle ih = c_func(xIupClipboard, {})
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
--  if c_func(xIupPlayInput, {filename})!=IUP_NOERROR then ?9/0 end if
    integer r = c_func(xIupPlayInput, {filename})
--  if r!=IUP_NOERROR then ?9/0 end if
end procedure

--global function get_all_classes(integer max_n = 128)
--  return get_string_array(xIupGetAllClasses, max_n)
--end function
--
global function IupGetClassName(Ihandle ih)
    atom pClassName = c_func(xIupGetClassName, {ih})
    return peek_string(pClassName)
end function

global function iupKeyCodeToName(atom ch)
    atom pKeyName = c_func(xiupKeyCodeToName,{ch})
    return peek_string(pKeyName)
end function

--global function get_class_type(Ihandle ih)
--  atom pClassType = c_func(xIupGetClassType, {ih})
--  return peek_string(pClassType)
--end function
--
--global function get_class_attributes(sequence classname, integer max_n = 128)
--  return get_string_array(xIupGetClassAttributes, max_n, classname)
--end function
--
--global function get_class_callbacks(sequence classname, integer max_n = 128)
--  return get_string_array(xIupGetClassCallbacks, max_n, classname)
--end function
--
--global procedure save_class_attributes(Ihandle ih)
--  c_proc(xIupSaveClassAttributes, {ih})
--end procedure
--
--global procedure copy_class_attributes(Ihandle src_ih, Ihandle dst_ih)
--  c_proc(xIupCopyClassAttributes, {src_ih, dst_ih})
--end procedure
--
--global procedure set_class_default_attribute(string classname, string name, string val)
--  c_proc(xIupSetClassDefaultAttribute, {classname, name, val})
--end procedure

--global function iup_append(Ihandle ih, object child)
--  if atom(child) then
--      if child!=NULL then
--          return c_func(xIupAppend, {ih, child})
--      end if
----    elsif sequence(child) then
--  else
--      for i=1 to length(child) do
--          return c_func(xIupAppend, {ih, child[i]})
--      end for
--  end if
--end function
--
--global procedure iup_detach(Ihandle ih)
--  c_proc(xIupDetach, {ih})
--end procedure
--
--global function iup_insert(Ihandle ih, Ihandle ref_child, Ihandle new_child)
--  return c_func(xIupInsert, {ih, ref_child, new_child})
--end function
--
--global function reparent(Ihandle child, Ihandle new_parent, Ihandle ref_child)
--  return c_func(xIupReparent, {child, new_parent, ref_child})
--end function
--
global function IupGetParent(Ihandle ih)
    Ihandln parent = c_func(xIupGetParent, {ih})
    return parent
end function

global function IupGetChild(Ihandle ih, integer pos)
    Ihandln child = c_func(xIupGetChild, {ih, pos})
    return child
end function

--global function IupGetChildPos(Ihandle ih, Ihandle child)
--  return c_func(xIupGetChildPos, {ih, child})
--end function
--
--global function IupGetChildCount(Ihandle ih)
--  integer n = c_func(xIupGetChildCount, {ih})
--  return n
--end function
--global function get_child_count(Ihandle ih)
--  return c_func(xIupGetChildCount, {ih})
--end function
--
--global function IupGetNextChild(Ihandle ih, Ihandle child)
--  Ihandln child = c_func(xIupGetNextChild, {ih,child})
--  return child
--end function
--global function get_next_child(Ihandle ih, Ihandle child)
--  return c_func(xIupGetNextChild, {ih, child})
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


--DEV not working, not documented:
--!/*
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
--  poke_double(pX,x)
--  poke_double(pY,y)
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
--  x = peek_double(pX)
--  y = peek_double(pY)
--  free(pX)
--  free(pY)
--  return {x, y}
--end function

--global procedure paint_to(Ihandle ih, atom cnv)
--  c_proc(xIupPPlotPaintTo, {ih, cnv})
--end procedure
--!*/

global function IupHelp(string url)
-- returns 1 if successful, -1 if failed, -2 if file not found
    return c_func(xIupHelp, {url})
end function

global procedure IupDestroy(Ihandle ih)
    c_proc(xIupDestroy, {ih})
end procedure


global type cdCanvas(object o)
    return atom(o) and o>=NULL and o=floor(o)
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


constant atom hCd = iup_open_dll({"cd.dll","libcd.so","libcd.dylib"})
constant atom hCdIup = iup_open_dll({"iupcd.dll","libiupcd.so","libiupcd.dylib"})

constant
    xcdVersion = define_c_func(hCd, "cdVersion", {},P),
    xcdVersionDate = define_c_func(hCd, "cdVersionDate", {},P),
--  xcdVersionNumber = define_c_func(hCd, "cdVersionNumber", {},I),
--  xcdCanvasSetBackground = define_c_proc(hCd, "cdCanvasSetBackground", {P,I}),
    xcdCanvasSetForeground = define_c_proc(hCd, "cdCanvasSetForeground", {P,I}),
--  xcdCanvasBackground = define_c_func(hCd, "cdCanvasBackground", {P,L},L),
    xcdCanvasForeground = define_c_func(hCd, "cdCanvasForeground", {P,L},L),
--  xcdCanvasBackOpacity = define_c_func(hCd, "cdCanvasBackOpacity", {P,I},I),
--  xcdCanvasWriteMode = define_c_func(hCd, "cdCanvasWriteMode", {P,I},I),
    xcdCanvasLineStyle = define_c_func(hCd, "cdCanvasLineStyle", {P,I},I),
--  xcdCanvasLineStyleDashes = define_c_proc(hCd, "cdCanvasLineStyleDashes", {P,P,I}),
    xcdCanvasLineWidth = define_c_func(hCd, "cdCanvasLineWidth", {P,I},I),
--  xcdCanvasLineJoin = define_c_func(hCd, "cdCanvasLineJoin", {P,I},I),
--  xcdCanvasLineCap = define_c_func(hCd, "cdCanvasLineCap", {P,I},I),
--  xcdCanvasInteriorStyle = define_c_func(hCd, "cdCanvasInteriorStyle", {P,I},I),
--  xcdCanvasHatch = define_c_func(hCd, "cdCanvasHatch", {P,I},I),
--  xcdCanvasStipple = define_c_proc(hCd, "cdCanvasStipple", {P,I,I,P}),
--  xcdCanvasGetStipple = define_c_func(hCd, "cdCanvasGetStipple", {P,P,P},P),
--  xcdCanvasPattern = define_c_proc(hCd, "cdCanvasPattern", {P,I,I,P}),
--  xcdCanvasGetPattern = define_c_func(hCd, "cdCanvasGetPattern", {P,P,P},P),
--  xcdCanvasFillMode = define_c_func(hCd, "cdCanvasFillMode", {P,I},I),
    xcdCanvasFont = define_c_proc(hCd, "cdCanvasFont", {P,P,I,I}),
    xcdCanvasGetFont = define_c_proc(hCd, "cdCanvasGetFont", {P,P,P,P}),
--  xcdCanvasNativeFont = define_c_func(hCd, "cdCanvasNativeFont", {P,P},P),
--  xcdCanvasTextAlignment = define_c_func(hCd, "cdCanvasTextAlignment", {P,I},I),
--  xcdCanvasTextOrientation = define_c_func(hCd, "cdCanvasTextOrientation", {P,D},D),
--  xcdCanvasMarkType = define_c_func(hCd, "cdCanvasMarkType", {P,I},I),
--  xcdCanvasMarkSize = define_c_func(hCd, "cdCanvasMarkSize", {P,I},I),
--  xcdCanvasPixel = define_c_proc(hCd, "cdCanvasPixel", {P,I,I}),
--  xcdCanvasMark = define_c_proc(hCd, "cdCanvasMark", {P,I,I}),
    xcdCanvasLine = define_c_proc(hCd, "cdCanvasLine", {P,I,I,I,I}),
    xcdCanvasBegin = define_c_proc(hCd, "cdCanvasBegin", {P,I}),
    xcdCanvasVertex = define_c_proc(hCd, "cdCanvasVertex", {P,I,I}),
    xcdCanvasEnd = define_c_proc(hCd, "cdCanvasEnd", {P}),
    xcdCanvasRect = define_c_proc(hCd, "cdCanvasRect", {P,I,I,I,I}),
--  xcdCanvasBox = define_c_proc(hCd, "cdCanvasBox", {P,I,I,I,I}),
    xcdCanvasArc = define_c_proc(hCd, "cdCanvasArc", {P,I,I,I,I,D,D}),
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
    xcdCanvasGetAttribute = define_c_func(hCd, "cdCanvasGetAttribute", {P,P},P),
    xcdCanvasGetColorPlanes = define_c_func(hCd, "cdCanvasGetColorPlanes", {P},I),
    xcdCanvasGetContext = define_c_func(hCd, "cdCanvasGetContext", {P},P),
    xcdCanvasGetSize = define_c_proc(hCd, "cdCanvasGetSize", {P,I,I,P,P}),
--  xcdCanvasUpdateYAxis = define_c_func(hCd, "cdCanvasUpdateYAxis", {P,P},I),
    xcdfCanvasUpdateYAxis = define_c_func(hCd, "cdfCanvasUpdateYAxis", {P,P},D),
--  xcdCanvasInvertYAxis = define_c_func(hCd, "cdCanvasInvertYAxis", {P,I},I),
    xcdfCanvasInvertYAxis = define_c_func(hCd, "cdfCanvasInvertYAxis", {P,D},D),
    xcdCanvasMM2Pixel = define_c_proc(hCd, "cdCanvasMM2Pixel", {P,D,D,P,P}),
--  xcdCanvasPixel2MM = define_c_proc(hCd, "cdCanvasPixel2MM", {P,I,I,P,P}),
--  xcdfCanvasMM2Pixel = define_c_proc(hCd, "cdfCanvasMM2Pixel", {P,D,D,P,P}),
    xcdfCanvasPixel2MM = define_c_proc(hCd, "cdfCanvasPixel2MM", {P,D,D,P,P}),
--  xcdCanvasOrigin = define_c_proc(hCd, "cdCanvasOrigin", {P,I,I}),
    xcdfCanvasOrigin = define_c_proc(hCd, "cdfCanvasOrigin", {P,D,D}),
--  xcdCanvasGetOrigin = define_c_proc(hCd, "cdCanvasGetOrigin", {P,P,P}),
    xcdfCanvasGetOrigin = define_c_proc(hCd, "cdfCanvasGetOrigin", {P,P,P}),
--  xcdCanvasTransform = define_c_proc(hCd, "cdCanvasTransform", {P,P}),
    xcdCanvasGetTransform = define_c_func(hCd, "cdCanvasGetTransform", {P},P),
--  xcdCanvasTransformMultiply = define_c_proc(hCd, "cdCanvasTransformMultiply", {P,P}),
--  xcdCanvasTransformRotate = define_c_proc(hCd, "cdCanvasTransformRotate", {P,D}),
--  xcdCanvasTransformScale = define_c_proc(hCd, "cdCanvasTransformScale", {P,D,D}),
--  xcdCanvasTransformTranslate = define_c_proc(hCd, "cdCanvasTransformTranslate", {P,D,D}),
--  xcdCanvasTransformPoint = define_c_proc(hCd, "cdCanvasTransformPoint", {P,I,I,P,P}),
    xcdfCanvasTransformPoint = define_c_proc(hCd, "cdfCanvasTransformPoint", {P,D,D,P,P}),
--  xcdCanvasVectorText = define_c_proc(hCd, "cdCanvasVectorText", {P,I,I,P}),
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
--  xcdCanvasPutImageRectRGBA = define_c_proc(hCd, "cdCanvasPutImageRectRGBA", {P,I,I,P,P,P,P,I,I,I,I,I,I,I,I}),
--  xcdCanvasPutImageRectMap = define_c_proc(hCd, "cdCanvasPutImageRectMap", {P,I,I,P,P,I,I,I,I,I,I,I,I}),
    xwdCanvasWindow = define_c_proc(hCd, "wdCanvasWindow", {P,D,D,D,D}),
    xwdCanvasGetWindow = define_c_proc(hCd, "wdCanvasGetWindow", {P,P,P,P,P}),
    xwdCanvasViewport = define_c_proc(hCd, "wdCanvasViewport", {P,I,I,I,I}),
    xwdCanvasGetViewport = define_c_proc(hCd, "wdCanvasGetViewport", {P,P,P,P,P}),
--  xwdCanvasWorld2Canvas = define_c_proc(hCd, "wdCanvasWorld2Canvas", {P,D,D,P,P}),
--  xwdCanvasWorld2CanvasSize = define_c_proc(hCd, "wdCanvasWorld2CanvasSize", {P,D,D,P,P}),
--  xwdCanvasCanvas2World = define_c_proc(hCd, "wdCanvasCanvas2World", {P,I,I,D,D}),
    xcdContextCaps = define_c_func(hCd, "cdContextCaps", {P},UL),
    xcdCreateCanvas = define_c_func(hCd, "cdCreateCanvas", {P,P},P),
    xcdKillCanvas = define_c_proc(hCd, "cdKillCanvas", {P}),
    xcdContextIup = define_c_func(hCdIup, "cdContextIup", {},P),
    xcdContextPrinter = define_c_func(hCd, "cdContextPrinter", {},P),
    xcdContextPS = define_c_func(hCd, "cdContextPS", {},P),
    xcdContextPicture = define_c_func(hCd, "cdContextPicture", {},P)

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
    cdCanvas hCdCanvas = c_func(xcdCreateCanvas, {hCdContext, data})
    return hCdCanvas
end function

global procedure cdCanvasSetForeground(cdCanvas hCdCanvas, atom color)
    c_proc(xcdCanvasSetForeground, {hCdCanvas, color})
end procedure

global function cdCanvasGetForeground(cdCanvas hCdCanvas)
    integer color = c_func(xcdCanvasForeground, {hCdCanvas, CD_QUERY})
    return color
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

global function cdCanvasGetSize(atom hCdCanvas)
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

--global function cdCanvasUpdateYAxis(atom hCdCanvas, atom y)
--atom newy, pY
--  pY = allocate(machine_word())
--  pokeN(pY, y, machine_word())
--  newy = c_func(xcdCanvasUpdateYAxis, {hCdCanvas, pY})
--  if newy!=peekNS(pY,machine_word(),1) then ?9/0 end if
--  free(pY)
--  return newy
--end function

global function cdCanvasUpdateYAxis(atom hCdCanvas, atom y)
atom newy, pY
    pY = allocate(8)
    poke_double(pY, y)
    newy = c_func(xcdfCanvasUpdateYAxis, {hCdCanvas, pY})
    -- if this triggers we may need to resurrect the int C function
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

--global procedure cdCanvasOrigin(atom hCdCanvas, atom x, atom y)
--  c_proc(xcdCanvasOrigin, {hCdCanvas, x, y})
--end procedure

global procedure cdCanvasOrigin(atom hCdCanvas, atom x, atom y)
    c_proc(xcdfCanvasOrigin, {hCdCanvas, x, y})
end procedure

--global function cdCanvasGetOrigin(atom hCdCanvas)
--atom pX, pY
--sequence origin
--
--  pX = allocate(8)
--  pY = pX+4
--  c_proc(xcdCanvasGetOrigin, {hCdCanvas, pX, pY})
--  origin = peek4s({pX, 2})
--  free(pX)
--  return origin
--end function

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

--global procedure cdCanvasTransform(atom hCdCanvas, object matrix=NULL)
--atom pMatrix = NULL
--  if matrix!=NULL then
--      if length(matrix)!=6 then ?9/0 end if
--      pMatrix = allocate(6*8)
--      poke_double(pMatrix, matrix)
--  end if
--  c_proc(xcdCanvasTransform, {hCdCanvas, pMatrix})
--  if matrix!=NULL then
--      free(pMatrix)
--  end if
--end procedure

global function cdCanvasGetTransform(atom hCdCanvas)
atom pMatrix
    pMatrix = c_func(xcdCanvasGetTransform, {hCdCanvas})
    return peek_double({pMatrix, 6})
end function

--global procedure cdCanvasTransformMultiply(atom hCdCanvas, sequence matrix)
--atom pMatrix
--  if length(matrix)!=6 then ?9/0 end if
--  pMatrix = allocate(6*8)
--  poke_double(pMatrix, matrix)
--  c_proc(xcdCanvasTransformMultiply, {hCdCanvas, pMatrix})
--  free(pMatrix)
--end procedure
--
--global procedure canvas_transform_rotate(atom hCdCanvas, atom angle)
--  c_proc(xcdCanvasTransformRotate, {hCdCanvas, angle})
--end procedure
--
--global procedure canvas_transform_scale(atom hCdCanvas, atom sx, atom sy)
--  c_proc(xcdCanvasTransformScale, {hCdCanvas, sx, sy})
--end procedure
--
--global procedure canvas_transform_translate(atom hCdCanvas, atom dx, atom dy)
--  c_proc(xcdCanvasTransformTranslate, {hCdCanvas, dx, dy})
--end procedure

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

--global procedure canvas_vector_text(atom hCdCanvas, atom x, atom y, sequence text)
--atom pText
--  pText = allocate_string(text)
--  c_proc(xcdCanvasVectorText, {hCdCanvas, x, y, pText})
--  free(pText)
--end procedure

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
atom pW
sequence text_size
--DEV machine_bits?
    pW = allocate(8)
    c_proc(xcdCanvasGetTextSize, {hCdCanvas, text, pW, pW+4})
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

--global procedure canvas_put_image_rect_rgba(atom hCdCanvas, atom iw, atom ih, sequence rgba, atom x, atom y,
--                                          atom w, atom h, atom xmin, atom xmax, atom ymin, atom ymax)
--atom pR, pG, pB, pA
--
--  pR = allocate(4*length(rgba[1]))
--  pG = pR+length(rgba[1])
--  pB = pG+length(rgba[1])
--  pA = pB+length(rgba[1])
--  poke(pR, rgba[1])
--  poke(pG, rgba[2])
--  poke(pB, rgba[3])
--  poke(pA, rgba[4])
--  c_proc(xcdCanvasPutImageRectRGBA, {hCdCanvas, iw, ih, pR, pG, pB, pA, x, y, w, h, xmin, xmax, ymin, ymax})
--  free(pR)
--end procedure
--
--global procedure canvas_put_image_rect_map(atom hCdCanvas, atom iw, atom ih, sequence map, sequence colors,
--                                         atom x, atom y, atom w, atom h, 
--                                         atom xmin, atom xmax, atom ymin, atom ymax)
--
--atom pIndex, pColors
--  pColors = allocate(4*256+length(map))
--  pIndex = pColors+4*256
--  poke4(pColors, colors)
--  poke(pIndex, map)
--  c_proc(xcdCanvasPutImageRectMap, {hCdCanvas, iw, ih, pIndex, pColors, x, y, w, h, xmin, xmax, ymin, ymax})
--  free(pColors)
--end procedure
--

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
    xcdCanvasVectorFont = define_c_func(hCd, "cdCanvasVectorFont", {P,P},P),
    xcdCanvasVectorTextDirection = define_c_proc(hCd, "cdCanvasVectorTextDirection", {P,I,I,I,I}),
--  xcdCanvasVectorTextTransform = define_c_func(hCd, "cdCanvasVectorTextTransform", {P,P},P),
--  xcdCanvasVectorTextSize = define_c_proc(hCd, "cdCanvasVectorTextSize", {P,I,I,P}),
--  xcdCanvasVectorCharSize = define_c_func(hCd, "cdCanvasVectorCharSize", {P,I},I),
    $

global function cdCanvasVectorFont(atom hCdCanvas, nullable_string font)
    atom pFont = c_func(xcdCanvasVectorFont, {hCdCanvas, font})
    font = peek_string(pFont)
    return font
end function

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

--DEV machine_bits?
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

--global function wd_canvas_world2_canvas(atom hCdCanvas, atom xw, atom yw)
--atom pX, pY
--sequence xy
--
--DEV machine_bits?
--  pX = allocate(2*4)
--  pY = pX+4
--  c_proc(xwdCanvasWorld2Canvas, {hCdCanvas, xw, yw, pX, pY})
--  xy = peek4s({pX, 2})
--  free(pX)
--  return xy
--end function
--
--global function wd_canvas_world2_canvas_size(atom hCdCanvas, atom ww, atom hw)
--atom pW, pH
--sequence wh
--
--DEV machine_bits?
--  pW = allocate(8)
--  pH = pW+4
--  c_proc(xwdCanvasWorld2CanvasSize, {hCdCanvas, ww, hw, pW, pH})
--  wh = peek4s({pW, 2})
--  free(pW)
--  return wh
--end function
--
--global function wd_canvas_canvas2_world(atom hCdCanvas, atom xv, atom yv)
--atom pWx, pWy
--sequence xy
--
--DEV machine_bits?
--  pWx = allocate(2*8)
--  pWy = pWx+8
--  c_proc(xwdCanvasCanvas2World, {hCdCanvas, xv, yv, pWx, pWy})
--  xy = peek_double({pWx, 2})
--  free(pWx)
--  return xy
--end function

global procedure cdKillCanvas(atom hCdCanvas)
    c_proc(xcdKillCanvas, {hCdCanvas})
end procedure

