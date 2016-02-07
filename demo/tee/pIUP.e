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

global function Icallback(sequence name, atom rid = routine_id(name))
    return call_back({'+', rid})
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
    return string(o) or (integer(o) and o>=NULL) or (atom(o) and o>=NULL and o=floor(o))
end type

global function rand_range(integer lo, integer hi)
    if lo>hi then {lo,hi} = {hi,lo} end if
    lo -= 1
    return lo+rand(hi-lo)
end function

global procedure free_pointer_array(atom pointers_array)
atom next_ptr = pointers_array, ptr
    while 1 do
        ptr = peekNS(next_ptr,machine_word(),0)
        if ptr=0 then exit end if
        free(ptr)
        next_ptr += machine_word()
    end while
    free(pointers_array)
end procedure
constant FREE_ARRAY_RID = routine_id("free_pointer_array")

global function allocate_pointer_array(sequence pointers, integer cleanup = 0)
atom pList

    pointers &= 0
    pList = allocate(length(pointers)*machine_word())
    pokeN(pList, pointers, machine_word())
--  if machine_bits()=32 then
--      pList = allocate(length(pointers)*4)
--      poke4(pList, pointers)
--  else
--      pList = allocate(length(pointers)*8)
--      poke8(pList, pointers)
--  end if
    if cleanup then
        return delete_routine(pList, FREE_ARRAY_RID)
    end if
    return pList
end function


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
    if res=0 then ?9/0 end if
    if chdir(curr_dir)=0 then ?9/0 end if
    return res
end function

constant
         D  = C_DOUBLE, 
--       F  = C_FLOAT, 
         I  = C_INT,
         L  = C_LONG,
         P  = C_POINTER, 
--       UC = C_UCHAR,
--       UL = C_ULONG,
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
    xIupSetAttribute = define_c_proc(iup, "IupSetAttribute", {P,P,P}),
    xIupSetStrAttribute = define_c_proc(iup, "IupSetStrAttribute", {P,P,P}),
    xIupSetInt = define_c_proc(iup, "IupSetInt", {P,P,I}),
    xIupStoreAttribute = define_c_proc(iup, "IupStoreAttribute", {P,P,P}),
    xIupSetAttributes = define_c_proc(iup, "IupSetAttributes", {P,P}),
    xIupSetAttributeHandle = define_c_proc(iup, "IupSetAttributeHandle", {P,P,P}),
    xIupSetHandle = define_c_proc(iup, "IupSetHandle", {P,P}),
    xIupGetAttribute = define_c_func(iup, "IupGetAttribute", {P,P},P),
    xIupGetInt = define_c_func(iup, "IupGetInt", {P,P},I),
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
    xIupFill  = define_c_func(iup, "IupFill", {},P),
    xIupFrame = define_c_func(iup, "IupFrame", {P},P),
    xIupHboxv = define_c_func(iup, "IupHboxv", {P},P),
    xIupImage = define_c_func(iup, "IupImage", {I,I,P},P),
    xIupLabel = define_c_func(iup, "IupLabel", {P},P),
    xIupMenuv = define_c_func(iup, "IupMenuv", {P},P),
    xIupItem = define_c_func(iup, "IupItem",  {P,P},P),
    xIupSeparator = define_c_func(iup, "IupSeparator", {},P),
    xIupSubmenu = define_c_func(iup, "IupSubmenu", {P,P},P),
    xIupRadio = define_c_func(iup, "IupRadio", {P},P),
    xIupText = define_c_func(iup, "IupText", {P},P),
    xIupMultiLine = define_c_func(iup, "IupMultiLine", {P},P),
    xIupTextConvertLinColToPos = define_c_proc(iup, "IupTextConvertLinColToPos", {P,I,I,P}),
    xIupTextConvertPosToLinCol = define_c_proc(iup, "IupTextConvertPosToLinCol", {P,I,P,P}),
    xIupTimer = define_c_func(iup, "IupTimer", {},P),
    xIupToggle = define_c_func(iup, "IupToggle", {P,P},P),
    xIupVboxv = define_c_func(iup, "IupVboxv", {P},P),
    xIupZboxv = define_c_func(iup, "IupZboxv", {P},P),
    xIupMainLoop = define_c_proc(iup, "IupMainLoop", {}),
    xIupDestroy = define_c_proc(iup, "IupDestroy", {P}),
    xIupFileDlg = define_c_func(iup, "IupFileDlg", {},P),
    xIupFontDlg = define_c_func(iup, "IupFontDlg", {},P),
    xIupGetColor = define_c_func(iup, "IupGetColor", {I,I,P,P,P},I),
--  xIupGetParam = define_c_func(iup, "IupGetParam", {P,P,P,P,P,P,P,P,P,P,P,P,P,P,P},I),
    xIupGetParam = define_c_func(iup, "IupGetParam", {P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P,P},I),
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
    xIupSetGlobal = define_c_proc(iup, "IupSetGlobal", {P,P}),
    xIupGetGlobal = define_c_func(iup, "IupGetGlobal", {P},P),
    xiupKeyCodeToName = define_c_func(iup, "iupKeyCodeToName", {I},P),
--  xIupAppend = define_c_func(iup, "IupAppend", {P,P},P),
--  xIupDetach = define_c_proc(iup, "IupDetach", {P}),
--  xIupInsert = define_c_func(iup, "IupInsert", {P,P,P},P),
--  xIupReparent = define_c_func(iup, "IupReparent", {P,P,P},P),
--*/
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
integer res = c_func(xIupGetInt, {ih, name})
    return res
end function

global function IupGetClassName(Ihandle ih)
    return peek_string(c_func(xIupGetClassName, {ih}))
end function

global procedure IupSetCallback(Ihandle ih, string name, atom func)
    func = c_func(xIupSetCallback, {ih, name, func})
end procedure

global function IupSetCallbackf(Ihandle ih, string name, atom func)
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

global function IupLabel(string title)
    Ihandle ih = c_func(xIupLabel, {title})
    return ih
end function

global function IupMenu(sequence children)
atom pChildren = allocate_pointer_array(children)
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

global function IupRadio(Ihandle pChild)
    Ihandle ih = c_func(xIupRadio, {pChild})
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

global function IupHbox(sequence children, string attributes = "", sequence data = {})
atom pChildren = allocate_pointer_array(children)
    Ihandle ih = c_func(xIupHboxv, {pChildren})
    free(pChildren)
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupVbox(sequence children, string attributes = "", sequence data = {})
atom pChildren = allocate_pointer_array(children)
    Ihandle ih = c_func(xIupVboxv, {pChildren})
    free(pChildren)
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupZbox(sequence children, string attributes = "", sequence data = {})
atom pChildren = allocate_pointer_array(children)
    Ihandle ih = c_func(xIupZboxv, {pChildren})
    free(pChildren)
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

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

--int IupConfigLoad(Ihandle* ih);
public function IupConfigLoad(atom ih)
atom result = c_func(xIupConfigLoad, {ih})
    return result
end function

--int IupConfigSave(Ihandle* ih);
public function IupConfigSave(atom ih)
atom result = c_func(xIupConfigSave, {ih})
    return result
end function

/****************************************************************/
--void IupConfigSetVariableStr(Ihandle* ih, const char* group, const char* key, const char* v);
public procedure IupConfigSetVariableStr(atom ih, object group = NULL, object key = NULL, object v = NULL)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    if sequence(v) then v = allocate_string(v,1) end if
    c_proc(xIupConfigSetVariableStr, {ih,group,key,v})
end procedure

--void IupConfigSetVariableStrId(Ihandle* ih, const char* group, const char* key, int id, const char* v);
public procedure IupConfigSetVariableStrId(atom ih, object group, object key, atom id, object v = NULL)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    if sequence(v) then v = allocate_string(v,1) end if
    c_proc(xIupConfigSetVariableStrId, {ih,group,key,id,v})
end procedure

--void IupConfigSetVariableInt(Ihandle* ih, const char* group, const char* key, int v);
public procedure IupConfigSetVariableInt(atom ih, object group, object key, atom v)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
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

--const char* IupConfigGetVariableStr(Ihandle* ih, const char* group, const char* key);
public function IupConfigGetVariableStr(atom ih, object group = NULL, object key = NULL)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    atom result = c_func(xIupConfigGetVariableStr, {ih,group,key})
    if result=NULL then return "" end if
    return peek_string(result)
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

--int IupConfigGetVariableIntDef(Ihandle* ih, const char* group, const char* key, int def);
public function IupConfigGetVariableIntDef(atom ih, object group, object key, atom def)
    if sequence(group) then group = allocate_string(group,1) end if
    if sequence(key) then key = allocate_string(key,1) end if
    atom result = c_func(xIupConfigGetVariableIntDef, {ih,group,key,def})
    return result
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

--void IupConfigDialogShow(Ihandle* ih, Ihandle* dialog, const char* name);
public procedure IupConfigDialogShow(atom ih, atom dialog, object name = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
    c_proc(xIupConfigDialogShow, {ih,dialog,name})
end procedure

--void IupConfigDialogClosed(Ihandle* ih, Ihandle* dialog, const char* name);
public procedure IupConfigDialogClosed(atom ih, atom dialog, object name = NULL)
    if sequence(name) then name = allocate_string(name,1) end if
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
    CD_OK = 0

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
    xcdCanvasVectorTextDirection = define_c_proc(hCd, "cdCanvasVectorTextDirection", {P,I,I,I,I}),
    xcdCanvasMultiLineVectorText = define_c_proc(hCd, "cdCanvasMultiLineVectorText", {P,I,I,P}),
    xcdCanvasPutImageRectRGB = define_c_proc(hCd, "cdCanvasPutImageRectRGB", {P,I,I,P,P,P,I,I,I,I,I,I,I,I}),
    xwdCanvasWindow = define_c_proc(hCd, "wdCanvasWindow", {P,D,D,D,D}),
    xwdCanvasViewport = define_c_proc(hCd, "wdCanvasViewport", {P,I,I,I,I}),
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
--  xcdGetScreenSize = define_c_proc(hCd, "cdGetScreenSize", {P,P,P,P}),
--  xcdGetScreenColorPlanes = define_c_func(hCd, "cdGetScreenColorPlanes", {},I),
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

global procedure cdCanvasVectorTextDirection(cdCanvas hCdCanvas, integer x1, integer y1, integer x2, integer y2)
    c_proc(xcdCanvasVectorTextDirection, {hCdCanvas, x1, y1, x2, y2})
end procedure

global procedure cdCanvasMultiLineVectorText(cdCanvas hCdCanvas, atom x, atom y, string text)
    c_proc(xcdCanvasMultiLineVectorText, {hCdCanvas, x, y, text})
end procedure

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

global procedure wdCanvasWindow(cdCanvas hCdCanvas, atom xmin, atom xmax, atom ymin, atom ymax)
    c_proc(xwdCanvasWindow, {hCdCanvas, xmin, xmax, ymin, ymax})
end procedure

global procedure wdCanvasViewport(cdCanvas hCdCanvas, atom xmin, atom xmax, atom ymin, atom ymax)
    c_proc(xwdCanvasViewport, {hCdCanvas, xmin, xmax, ymin, ymax})
end procedure

global procedure cdKillCanvas(atom hCdCanvas)
    c_proc(xcdKillCanvas, {hCdCanvas})
end procedure

