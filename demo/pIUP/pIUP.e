--
-- pIUP.e
--

global type Ihandle(object o)
    return atom(o) and o>=NULL and o=floor(o)
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

global constant
    IUP_CENTER = 65535,
--  LEFT = 65534,
--  RIGHT = 65533,
--  MOUSEPOS = 65532,
--  CURRENT = 66531,
    IUP_CENTERPARENT = 65530,
--  TOP = IUP_CENTER,
--  BOTTOM = RIGHT,
--  ANYWHERE = CURRENT,

    ACTION = "ACTION",
    ACTION_CB = "ACTION_CB",
    $

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

global function rand_range(integer lo, integer hi)
    if lo>hi then {lo,hi} = {hi,lo} end if
    lo -= 1
    return lo+rand(hi-lo)
end function

global procedure free_pointer_array(atom pointers_array)
atom next_ptr = pointers_array, ptr
    while 1 do
        ptr = peek4u(next_ptr)
        if ptr=0 then exit end if
        free(ptr)
        next_ptr += 4
    end while
    free(pointers_array)
end procedure
constant FREE_ARRAY_RID = routine_id("free_pointer_array")

global function allocate_pointer_array(sequence pointers, integer cleanup = 0)
atom pList

    pointers &= 0
    if machine_bits()=32 then
        pList = allocate(length(pointers)*4)
        poke4(pList, pointers)
    else
        pList = allocate(length(pointers)*8)
        poke8(pList, pointers)
    end if
    if cleanup then
        return delete_routine(pList, FREE_ARRAY_RID)
    end if
    return pList
end function

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

constant
--  UC = C_UCHAR,
    P  = C_POINTER, 
--  F  = C_FLOAT, 
    D  = C_DOUBLE, 
    I  = C_INT,
--  L  = C_LONG,
    UL = C_ULONG,
    $

atom hIup

constant iupdlls = {
                    "iup.dll",
                    "libiup.so",
                    "libiup.dylib"
                   }

hIup = iup_open_dll(iupdlls)

constant
    xIupOpen = define_c_func(hIup, "IupOpen", {P,P},I),
    xIupClose = define_c_proc(hIup, "IupClose", {}),
--  xIupVersion = define_c_func(hIup, "IupVersion", {},P),
--  xIupLoad = define_c_func(hIup, "IupLoad", {P},P),
--  xIupLoadBuffer = define_c_func(hIup, "IupLoadBuffer", {P},P),
--  xIupSetLanguage = define_c_proc(hIup, "IupSetLanguage", {P}),
--  xIupGetLanguage = define_c_func(hIup, "IupGetLanguage", {},P),
--  xIupFileDlg = define_c_func(hIup, "IupFileDlg", {},P),
--  xIupMessageDlg = define_c_func(hIup, "IupMessageDlg", {},P),
--  xIupColorDlg = define_c_func(hIup, "IupColorDlg", {},P),
--  xIupFontDlg = define_c_func(hIup, "IupFontDlg", {},P),
    xIupAlarm = define_c_func(hIup, "IupAlarm", {P,P,P,P,P},I),
--  xIupGetFile = define_c_func(hIup, "IupGetFile", {P},I),
--  xIupGetColor = define_c_func(hIup, "IupGetColor", {I,I,P,P,P},I),
--  xIupGetParam = define_c_func(hIup, "IupGetParam", {P,P,P,P,P,P,P,P,P,P,P,P,P,P,P},I),
--  xIupGetText = define_c_func(hIup, "IupGetText", {P,P},P),
--  xIupListDialog = define_c_func(hIup, "IupListDialog", {I,P,I,P,I,I,I,P},I),
    xIupMessage  = define_c_proc(hIup, "IupMessage", {P,P}),
--  xIupLayoutDialog = define_c_func(hIup, "IupLayoutDialog", {P},P),
    xIupDialog = define_c_func(hIup, "IupDialog", {P},P),
--  xIupPopup = define_c_func(hIup, "IupPopup", {P,I,I},I),
    xIupShow = define_c_func(hIup, "IupShow", {P},I),
    xIupShowXY = define_c_func(hIup, "IupShowXY", {P,I,I},I),
--  xIupHide = define_c_func(hIup, "IupHide", {P},P),
    xIupStoreAttribute = define_c_proc(hIup, "IupStoreAttribute", {P,P,P}),
    xIupSetAttribute = define_c_proc(hIup, "IupSetAttribute", {P,P,P}),
    xIupSetAttributes = define_c_proc(hIup, "IupSetAttributes", {P,P}),
--  xIupSetInt = define_c_proc(hIup, "IupSetInt", {P,P,I}),
--  xIupResetAttribute = define_c_proc(hIup, "IupResetAttribute", {P,P}),
--  xIupGetAttribute = define_c_func(hIup, "IupGetAttribute", {P,P},P),
--  xIupGetAllAttributes = define_c_func(hIup, "IupGetAllAttributes", {P,P,I},I),
    xIupSetAttributeHandle = define_c_proc(hIup, "IupSetAttributeHandle", {P,P,P}),
--  xIupGetAttributeHandle = define_c_func(hIup, "IupGetAttributeHandle", {P,P},P),
--  xIupGetAttributes = define_c_func(hIup, "IupGetAttributes", {P},P),
--  xIupGetFloat = define_c_func(hIup, "IupGetFloat", {P,P},F),
--  xIupGetInt = define_c_func(hIup, "IupGetInt", {P,P},I),
--  xIupGetInt2 = define_c_func(hIup, "IupGetInt", {P,P},I),
--  xIupStoreGlobal = define_c_proc(hIup, "IupStoreGlobal", {P,P}),
--  xIupSetGlobal = define_c_proc(hIup, "IupSetGlobal", {P,P}),
--  xIupGetGlobal = define_c_func(hIup, "IupGetGlobal", {P},P),
    xIupButton = define_c_func(hIup, "IupButton", {P,P},P),
    xIupCanvas = define_c_func(hIup, "IupCanvas", {P},P),
--  xIupFrame = define_c_func(hIup, "IupFrame", {P},P),
    xIupLabel = define_c_func(hIup, "IupLabel", {P},P),
--  xIupList = define_c_func(hIup, "IupList", {P},P),
--  xIupProgressBar = define_c_func(hIup, "IupProgressBar", {},P),
--  xIupSpin = define_c_func(hIup, "IupSpin", {},P),
--  xIupTabs = define_c_func(hIup, "IupTabsv", {P},P),
--  xIupText = define_c_func(hIup, "IupText", {P},P),
--  xIupToggle = define_c_func(hIup, "IupToggle", {P,P},P),
--  xIupTree = define_c_func(hIup, "IupTree", {},P),
--  xIupVal = define_c_func(hIup, "IupVal", {P},P),
--  xIupTextConvertPosToLinCol = define_c_proc(hIup, "IupTextConvertPosToLinCol", {P,I,P,P}),
--  xIupTextConvertLinColToPos = define_c_proc(hIup, "IupTextConvertLinColToPos", {P,I,I,P}),
    xIupClipboard = define_c_func(hIup, "IupClipboard", {},P),
    xIupTimer = define_c_func(hIup, "IupTimer", {},P),
--  xIupUser = define_c_func(hIup, "IupUser", {},P),
    xIupHelp = define_c_func(hIup, "IupHelp", {P},I),
    xIupMainLoop = define_c_proc(hIup, "IupMainLoop", {}),
--  xIupMainLoopLevel = define_c_func(hIup, "IupMainLoopLevel", {},I),
--  xIupLoopStep = define_c_proc(hIup, "IupLoopStep", {}),
--  xIupExitLoop = define_c_proc(hIup, "IupExitLoop", {}),
--  xIupFlush = define_c_proc(hIup, "IupFlush", {}),
--  xIupRecordInput = define_c_func(hIup, "IupRecordInput", {P,I},I),
--  xIupPlayInput = define_c_func(hIup, "IupPlayInput", {P},I),
--  xIupGetCallback = define_c_func(hIup, "IupGetCallback", {P,P},P),
    xIupSetCallback = define_c_func(hIup, "IupSetCallback", {P,P,P},P),
--  xIupGetActionName = define_c_func(hIup, "IupGetActionName", {},P),
--  xIupFill  = define_c_func(hIup, "IupFill", {},P),
--  xIupHboxv = define_c_func(hIup, "IupHboxv", {P},P),
    xIupVboxv = define_c_func(hIup, "IupVboxv", {P},P),
--  xIupZboxv = define_c_func(hIup, "IupZboxv", {P},P),
--  xIupRadio = define_c_func(hIup, "IupRadio", {P},P),
--  xIupNormalizerv = define_c_func(hIup, "IupNormalizer", {P},P),
--  xIupCboxv = define_c_func(hIup, "IupCboxv", {P},P),
--  xIupSbox  = define_c_func(hIup, "IupSbox", {P},P),
--  xIupSplit = define_c_func(hIup, "IupSplit", {P,P},P),
--  xIupCreate = define_c_func(hIup, "IupCreate", {P},P),
    xIupDestroy = define_c_proc(hIup, "IupDestroy", {P}),
--  xIupMap = define_c_func(hIup, "IupMap", {P},I),
--  xIupUnmap = define_c_proc(hIup, "IupUnmap", {P}),
    $

global procedure IupOpen()
    if hIup=0 then
        printf(1,"error opening %s%s",{dll_path,iupdlls[libidx]})
        {} = wait_key()
        ?9/0
    end if
    if c_func(xIupOpen,{NULL,NULL})=IUP_ERROR then ?9/0 end if
end procedure

global procedure IupClose()
    c_proc(xIupClose)
end procedure

global function IupAlarm(string title, string message, string b1, object b2 = NULL, object b3 = NULL)
    return c_func(xIupAlarm, {title, message, b1, b2, b3})
end function

global procedure IupShow(Ihandle ih)
    if c_func(xIupShow, {ih})=IUP_ERROR then ?9/0 end if
end procedure

global function IupShowXY(Ihandle ih, integer x, integer y)
    return c_func(xIupShowXY, {ih, x, y})
end function

global procedure IupSetAttribute(Ihandle ih, string name, object pValue)
    if name!=upper(name) then ?9/0 end if
    c_proc(xIupSetAttribute, {ih, name, pValue})
end procedure

global procedure IupStoreAttribute(Ihandle ih, string name, object val, sequence data = {})
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

global procedure IupSetAttributeHandle(Ihandle ih, string name, Ihandle ih_named)
    if name!=upper(name) then ?9/0 end if
    c_proc(xIupSetAttributeHandle, {ih, name, ih_named})
end procedure

global function IupDialog(Ihandle ih, string attributes = "", sequence data = {})
    Ihandle dlg = c_func(xIupDialog, {ih})
    if length(attributes) then
        IupSetAttributes(dlg, attributes, data)
    end if
    return dlg
end function

global procedure IupMessage(sequence title, sequence message)
    c_proc(xIupMessage, {title, message})
end procedure

global function IupButton(sequence title, object action = NULL, integer rid = -1, sequence attributes = {}, sequence data = {})
    Ihandle ih = c_func(xIupButton, {title, action})
    if rid>=0 then
        IupSetCallback(ih, ACTION, rid)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupCanvas(string action = "")
    Ihandle ih = c_func(xIupCanvas, {action})
    return ih
end function

global function IupLabel(string title)
    Ihandle ih = c_func(xIupLabel, {title})
    return ih
end function

global function IupClipboard()
    Ihandle ih = c_func(xIupClipboard, {})
    return ih
end function

global function IupTimer(integer rid = -2, integer msecs = 0, integer active = 1)
    Ihandle ih = c_func(xIupTimer, {})
    if rid>=0 and msecs!=0 then
        IupSetCallback(ih, ACTION_CB, rid)
        IupSetAttributes(ih, "TIME=%d,RUN=%s", {msecs, iff(active, "YES", "NO")})
    end if
    return ih
end function

--global function user()
--  return c_func(xIupUser, {})
--end function

global function IupHelp(string url)
-- returns 1 if successful, -1 if failed, -2 if file not found
    return c_func(xIupHelp, {url})
end function

global procedure IupMainLoop()
    c_proc(xIupMainLoop, {})
end procedure

global procedure IupSetCallback(Ihandle ih, object name, integer rid)
    ih = c_func(xIupSetCallback, {ih, name, call_back({'+', rid})})
end procedure

global function IupSetCallbackf(Ihandle ih, object name, integer rid)
    IupSetCallback(ih, name, rid)
    return ih
end function

global function IupVbox(object children, sequence attributes = {})
atom pChildren = 0
    if sequence(children) then
        pChildren = allocate_pointer_array(children)
    end if
    Ihandle ih = c_func(xIupVboxv, {pChildren})
    if sequence(children) then
        free(pChildren)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes)
    end if
    return ih
end function

global procedure IupDestroy(Ihandle ih)
    c_proc(xIupDestroy, {ih})
end procedure


atom
    hCd = iup_open_dll({
                        "cd.dll",
                        "libcd.so",
                        "libcd.dylib"
                       }),
    hCdIup = iup_open_dll({
                           "iupcd.dll",
                           "libiupcd.so",
                           "libiupcd.dylib"
                          })

constant
    xcdVersion = define_c_func(hCd, "cdVersion", {},P),
    xcdVersionDate = define_c_func(hCd, "cdVersionDate", {},P),
--  xcdVersionNumber = define_c_func(hCd, "cdVersionNumber", {},I),
--  xcdCanvasSetBackground = define_c_proc(hCd, "cdCanvasSetBackground", {P,I}),
    xcdCanvasSetForeground = define_c_proc(hCd, "cdCanvasSetForeground", {P,I}),
--  xcdCanvasBackground = define_c_func(hCd, "cdCanvasBackground", {P,L},L),
--  xcdCanvasForeground = define_c_func(hCd, "cdCanvasForeground", {P,L},L),
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
    xcdCanvasFont = define_c_func(hCd, "cdCanvasFont", {P,P,I,I},I),
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
    xcdContextCaps = define_c_func(hCd, "cdContextCaps", {P},UL),
--  xcdCanvasPlay = define_c_func(hCd, "cdCanvasPlay", {P,P,I,I,I,I,P},I),
    xcdCreateCanvas = define_c_func(hCd, "cdCreateCanvas", {P,P},P),
    xcdKillCanvas = define_c_proc(hCd, "cdKillCanvas", {P}),
    xcdCanvasGetContext = define_c_func(hCd, "cdCanvasGetContext", {P},P),
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
    xcdCanvasGetSize = define_c_proc(hCd, "cdCanvasGetSize", {P,I,I,P,P}),
    xcdCanvasUpdateYAxis = define_c_func(hCd, "cdCanvasUpdateYAxis", {P,P},I),
    xcdfCanvasUpdateYAxis = define_c_func(hCd, "cdfCanvasUpdateYAxis", {P,P},D),
    xcdCanvasInvertYAxis = define_c_func(hCd, "cdCanvasInvertYAxis", {P,I},I),
    xcdfCanvasInvertYAxis = define_c_func(hCd, "cdfCanvasInvertYAxis", {P,D},D),
    xcdCanvasMM2Pixel = define_c_proc(hCd, "cdCanvasMM2Pixel", {P,D,D,P,P}),
    xcdCanvasPixel2MM = define_c_proc(hCd, "cdCanvasPixel2MM", {P,I,I,P,P}),
    xcdfCanvasMM2Pixel = define_c_proc(hCd, "cdfCanvasMM2Pixel", {P,D,D,P,P}),
    xcdfCanvasPixel2MM = define_c_proc(hCd, "cdfCanvasPixel2MM", {P,D,D,P,P}),
    xcdCanvasOrigin = define_c_proc(hCd, "cdCanvasOrigin", {P,I,I}),
--  xcdfCanvasOrigin = define_c_proc(hCd, "cdfCanvasOrigin", {P,D,D}),
    xcdCanvasGetOrigin = define_c_proc(hCd, "cdCanvasGetOrigin", {P,P,P}),
--  xcdfCanvasGetOrigin = define_c_proc(hCd, "cdfCanvasGetOrigin", {P,P,P}),
--  xcdCanvasTransform = define_c_proc(hCd, "cdCanvasTransform", {P,P}),
    xcdCanvasGetTransform = define_c_func(hCd, "cdCanvasGetTransform", {P},P),
--  xcdCanvasTransformMultiply = define_c_proc(hCd, "cdCanvasTransformMultiply", {P,P}),
--  xcdCanvasTransformRotate = define_c_proc(hCd, "cdCanvasTransformRotate", {P,D}),
--  xcdCanvasTransformScale = define_c_proc(hCd, "cdCanvasTransformScale", {P,D,D}),
--  xcdCanvasTransformTranslate = define_c_proc(hCd, "cdCanvasTransformTranslate", {P,D,D}),
    xcdCanvasTransformPoint = define_c_proc(hCd, "cdCanvasTransformPoint", {P,I,I,P,P}),
--  xcdfCanvasTransformPoint = define_c_proc(hCd, "cdfCanvasTransformPoint", {P,D,D,P,P}),
    xcdCanvasVectorFont = define_c_func(hCd, "cdCanvasVectorFont", {P,P},P),
    xcdCanvasVectorTextDirection = define_c_proc(hCd, "cdCanvasVectorTextDirection", {P,I,I,I,I}),
--  xcdCanvasVectorTextTransform = define_c_func(hCd, "cdCanvasVectorTextTransform", {P,P},P),
--  xcdCanvasVectorTextSize = define_c_proc(hCd, "cdCanvasVectorTextSize", {P,I,I,P}),
--  xcdCanvasVectorCharSize = define_c_func(hCd, "cdCanvasVectorCharSize", {P,I},I),
--  xcdCanvasVectorText = define_c_proc(hCd, "cdCanvasVectorText", {P,I,I,P}),
    xcdCanvasMultiLineVectorText = define_c_proc(hCd, "cdCanvasMultiLineVectorText", {P,I,I,P}),
--  xcdCanvasGetVectorTextSize = define_c_proc(hCd, "cdCanvasGetVectorTextSize", {P,P,P,P}),
    xcdCanvasGetVectorTextBounds = define_c_proc(hCd, "cdCanvasGetVectorTextBounds", {P,P,I,I,P}),
    xcdCanvasGetFontDim = define_c_proc(hCd, "cdCanvasGetFontDim", {P,P,P,P,P}),
    xcdCanvasGetTextSize = define_c_proc(hCd, "cdCanvasGetTextSize", {P,P,P,P}),
    xcdCanvasGetTextBox = define_c_proc(hCd, "cdCanvasGetTextBox", {P,P,I,I,P,P,P,P}),
    xcdCanvasGetTextBounds = define_c_proc(hCd, "cdCanvasGetTextBounds", {P,I,I,P,P}),
    xcdCanvasGetColorPlanes = define_c_func(hCd, "cdCanvasGetColorPlanes", {P},I),
    xcdCanvasGetImageRGB = define_c_proc(hCd, "cdCanvasGetImageRGB", {P,P,P,P,I,I,I,I}),
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
    $

global function cdVersion()
    return peek_string(c_func(xcdVersion, {}))
end function

global function cdVersionDate()
    return peek_string(c_func(xcdVersionDate, {}))
end function

global procedure cdCanvasSetForeground(atom hCdCanvas, atom color)
    c_proc(xcdCanvasSetForeground, {hCdCanvas, color})
end procedure

global function cdCreateCanvas(atom hCdContext, object data)
--atom fnVal, pData
--
--  if sequence(data) then
--      pData = allocate_string(data)
--  else
--      pData = data
--  end if
--
--  fnVal = c_func(xcdCreateCanvas, {hCdContext, pData})
--
--  if sequence(data) then
--      free(pData)
--  end if
--
--  return fnVal
    return c_func(xcdCreateCanvas, {hCdContext, data})
end function

global procedure cdKillCanvas(atom hCdCanvas)
    c_proc(xcdKillCanvas, {hCdCanvas})
end procedure

global function cdCanvasGetContext(atom hCdCanvas)
    return c_func(xcdCanvasGetContext, {hCdCanvas})
end function

global function cdCanvasGetAttribute(atom hCdCanvas, string name)
atom fnVal
--, pName
--  pName = allocate_string(name)
--  fnVal = c_func(xcdCanvasGetAttribute, {hCdCanvas, pName})
    fnVal = c_func(xcdCanvasGetAttribute, {hCdCanvas, name})
--  free(pName)
    if fnVal=NULL then return NULL end if
    return peek_string(fnVal)
end function

global function cdCanvasFont(atom hCdCanvas, string font, atom style, atom size)
--atom fnVal, pFont
--
--  pFont = allocate_string(font)
--  fnVal = c_func(xcdCanvasFont, {hCdCanvas, pFont, style, size})
--  free(pFont)
--  return fnVal
    return c_func(xcdCanvasFont, {hCdCanvas, font, style, size})
end function

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

global function cdCanvasUpdateYAxis(atom hCdCanvas, atom y)
atom fnVal, pY
--DEV machine_bits()?
    pY = allocate(4)
    poke4(pY, y)
    fnVal = c_func(xcdCanvasUpdateYAxis, {hCdCanvas, pY})
    free(pY) --peek4s(pY)==fnVal
    return fnVal
end function

global function cdCanvasInvertYAxis(atom hCdCanvas, atom y)
    return c_func(xcdCanvasInvertYAxis, {hCdCanvas, y})
end function

global function cdfCanvasUpdateYAxis(atom hCdCanvas, atom y)
atom fnVal, pY
--DEV machine_bits?
    pY = allocate(8)
    poke_double(pY, y)
    fnVal = c_func(xcdfCanvasUpdateYAxis, {hCdCanvas, pY})
    free(pY) --peek_double(pY)=fnVal
    return fnVal
end function

global function cdfCanvasInvertYAxis(atom hCdCanvas, atom y)
    return c_func(xcdfCanvasInvertYAxis, {hCdCanvas, y})
end function

global function cdCanvasMM2Pixel(atom hCdCanvas, atom mm_dx, atom mm_dy)
atom pDx, pDy
sequence dx_dy
    pDx = allocate(8)
    pDy = pDx+4
    c_proc(xcdCanvasMM2Pixel, {hCdCanvas, mm_dx, mm_dy, pDx, pDy})
    dx_dy = peek4s({pDx, 2})
    free(pDx)
    return dx_dy
end function

global function cdCanvasPixel2MM(atom hCdCanvas, atom dx, atom dy)
atom pmm_dx, pmm_dy
sequence mmdx_mmdy

    pmm_dx = allocate(16)
    pmm_dy = pmm_dx+8
    c_proc(xcdCanvasPixel2MM, {hCdCanvas, dx, dy, pmm_dx, pmm_dy})
    mmdx_mmdy = peek_double({pmm_dx, 2})
    free(pmm_dx)
    return mmdx_mmdy
end function

global function cdfCanvasMM2Pixel(atom hCdCanvas, atom mm_dx, atom mm_dy)
atom pDx, pDy
sequence dx_dy
    pDx = allocate(16)
    pDy = pDx+8
    c_proc(xcdfCanvasMM2Pixel, {hCdCanvas, mm_dx, mm_dy, pDx, pDy})
    dx_dy = peek_double({pDx, 2})
    free(pDx)
    return dx_dy
end function

global function cdfCanvasPixel2MM(atom hCdCanvas, atom dx, atom dy)
atom pmm_dx, pmm_dy
sequence mmdx_mmdy

    pmm_dx = allocate(16)
    pmm_dy = pmm_dx+8
    c_proc(xcdfCanvasPixel2MM, {hCdCanvas, dx, dy, pmm_dx, pmm_dy})
    mmdx_mmdy = peek_double({pmm_dx, 2})
    free(pmm_dx)
    return mmdx_mmdy
end function

global procedure cdCanvasOrigin(atom hCdCanvas, atom x, atom y)
    c_proc(xcdCanvasOrigin, {hCdCanvas, x, y})
end procedure

--global procedure f_canvas_origin(atom hCdCanvas, atom x, atom y)
--  c_proc(xcdfCanvasOrigin, {hCdCanvas, x, y})
--end procedure

global function cdCanvasGetOrigin(atom hCdCanvas)
atom pX, pY
sequence origin

    pX = allocate(8)
    pY = pX+4
    c_proc(xcdCanvasGetOrigin, {hCdCanvas, pX, pY})
    origin = peek4s({pX, 2})
    free(pX)
    return origin
end function

--global function f_canvas_get_origin(atom hCdCanvas)
--atom pX, pY
--sequence origin
--
--  pX = allocate(16)
--  pY = pX+8
--  c_proc(xcdfCanvasGetOrigin, {hCdCanvas, pX, pY})
--  origin = peek_double({pX, 2})
--  free(pX)
--  return origin
--end function

--global procedure canvas_transform(atom hCdCanvas, sequence matrix)
--atom pMatrix
--  pMatrix = allocate(6*8)
--  poke_double(pMatrix, matrix)
--  c_proc(xcdCanvasTransform, {hCdCanvas, pMatrix})
--  free(pMatrix)
--end procedure

global function cdCanvasGetTransform(atom hCdCanvas)
atom fnVal
    fnVal = c_func(xcdCanvasGetTransform, {hCdCanvas})
    return peek_double({fnVal, 6})
end function

--global procedure canvas_transform_multiply(atom hCdCanvas, sequence matrix)
--atom pMatrix
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

global function cdCanvasTransformPoint(atom hCdCanvas, atom x, atom y)
atom pX, pY
sequence tx_ty
    pX = allocate(8)
    pY = pX+4
    c_proc(xcdCanvasTransformPoint, {hCdCanvas, x, y, pX, pY})
    tx_ty = peek4s({pX, 2})
    free(pX)
    return tx_ty
end function

--global function f_canvas_transform_point(atom hCdCanvas, atom x, atom y)
--atom pX, pY
--sequence tx_ty
--  pX = allocate(16)
--  pY = pX+8
--  c_proc(xcdfCanvasTransformPoint, {hCdCanvas, x, y, pX, pY})
--  tx_ty = peek_double({pX, 2})
--  free(pX)
--  return tx_ty
--end function

constant
    xcdContextIup = define_c_func(hCdIup, "cdContextIup", {},P),
    xcdContextPrinter = define_c_func(hCd, "cdContextPrinter", {},P),
    xcdContextPS = define_c_func(hCd, "cdContextPS", {},P),
    xcdContextPicture = define_c_func(hCd, "cdContextPicture", {},P)

global constant
    CD_IUP = c_func(xcdContextIup, {}),
    CD_PRINTER = c_func(xcdContextPrinter, {}),
    CD_PS = c_func(xcdContextPS, {}),
    CD_PICTURE = c_func(xcdContextPicture, {})

ifdef WINDOWS then
constant
        xcdContextNativeWindow = define_c_func(hCd, "cdContextNativeWindow", {},P),
        xcdGetScreenSize = define_c_proc(hCd, "cdGetScreenSize", {P,P,P,P}),
        xcdGetScreenColorPlanes = define_c_func(hCd, "cdGetScreenColorPlanes", {},I)

global constant
        CD_NATIVEWINDOW = c_func(xcdContextNativeWindow, {})
end ifdef

global constant
    CD_QUERY = -1,
    CD_ERROR = -1,
    CD_OK = 0

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

global procedure cdCanvasBegin(atom hCdCanvas, integer mode)
    c_proc(xcdCanvasBegin, {hCdCanvas, mode})
end procedure

global procedure cdCanvasEnd(atom hCdCanvas)
    c_proc(xcdCanvasEnd, {hCdCanvas})
end procedure

--DEV/doc: split into cdCanvasLineWidth and cdCanvasGetLineWidth
global procedure cdCanvasLineWidth(atom hCdCanvas, atom width)
    {} = c_func(xcdCanvasLineWidth, {hCdCanvas, width})
end procedure

global function cdCanvasGetLineWidth(atom hCdCanvas)
    return c_func(xcdCanvasLineWidth, {hCdCanvas, CD_QUERY})
end function

global procedure cdCanvasLineStyle(atom hCdCanvas, atom style)
    {} = c_func(xcdCanvasLineStyle, {hCdCanvas, style})
end procedure

global function cdCanvasGetLineStyle(atom hCdCanvas)
    return c_func(xcdCanvasLineStyle, {hCdCanvas, CD_QUERY})
end function

global procedure cdCanvasLine(atom hCdCanvas, atom minX, atom minY, atom maxX, atom maxY)
    c_proc(xcdCanvasLine, {hCdCanvas, minX, minY, maxX, maxY})
end procedure

global procedure cdCanvasRect(atom hCdCanvas, atom minX, atom minY, atom maxX, atom maxY)
    c_proc(xcdCanvasRect, {hCdCanvas, minX, minY, maxX, maxY})
end procedure

global procedure cdCanvasArc(atom hCdCanvas, atom xc, atom yc, atom w, atom h, atom a1, atom a2)
    c_proc(xcdCanvasArc, {hCdCanvas, xc, yc, w, h, a1, a2})
end procedure

global procedure cdCanvasVertex(atom hCdCanvas, atom x, atom y)
    c_proc(xcdCanvasVertex, {hCdCanvas, x, y})
end procedure

global function cdContextCaps(atom hCdContext)
    return c_func(xcdContextCaps, {hCdContext})
end function

global function cdCanvasVectorFont(atom hCdCanvas, object font)
--atom pFont = 0
--
--  if length(font) then
--      pFont = allocate_string(font)
--  end if

    atom fnVal = c_func(xcdCanvasVectorFont, {hCdCanvas, font})

    font = peek_string(fnVal)
--  if pFont then
--      free(pFont)
--  end if

    return font
end function

global procedure cdCanvasVectorTextDirection(atom hCdCanvas, integer x1, integer y1, integer x2, integer y2)
    c_proc(xcdCanvasVectorTextDirection, {hCdCanvas, x1, y1, x2, y2})
end procedure

--global function canvas_vector_text_transform(atom hCdCanvas, sequence matrix)
--atom fnVal, pMatrix
--
----DEV machine_bits?
--  pMatrix = allocate(8*6)
--  poke_double(pMatrix, matrix)
--  fnVal = c_func(xcdCanvasVectorTextTransform, {hCdCanvas, pMatrix})
--  matrix = peek_double({fnVal, 6})
--  free(pMatrix)
--  return matrix
--end function

--global procedure canvas_vector_text_size(atom hCdCanvas, atom w, atom h, string text)
--  c_proc(xcdCanvasVectorTextSize, {hCdCanvas, w, h, text})
--end procedure
--
--global function canvas_vector_char_size(atom hCdCanvas, atom size)
--  return c_func(xcdCanvasVectorCharSize, {hCdCanvas, size})
--end function

--global procedure canvas_vector_text(atom hCdCanvas, atom x, atom y, sequence text)
--atom pText
--  pText = allocate_string(text)
--  c_proc(xcdCanvasVectorText, {hCdCanvas, x, y, pText})
--  free(pText)
--end procedure

global procedure cdCanvasMultiLineVectorText(atom hCdCanvas, atom x, atom y, string text)
    c_proc(xcdCanvasMultiLineVectorText, {hCdCanvas, x, y, text})
end procedure

--global function canvas_get_vector_text_size(atom hCdCanvas, sequence text)
--atom pText, pX, pY
--sequence x_y
--  pText = allocate_string(text)
--  pX = allocate(8)
--  pY = pX+4
--  c_proc(xcdCanvasGetVectorTextSize, {hCdCanvas, pText, pX, pY})
--  x_y = peek4s({pX, 2})
--  free(pText)
--  free(pX)
--  return x_y
--end function

global function cdCanvasGetVectorTextBounds(atom hCdCanvas, string text, atom px, atom py)
atom pRect
sequence rect
--DEV machine_bits?
    pRect = allocate(8*4)
    c_proc(xcdCanvasGetVectorTextBounds, {hCdCanvas, text, px, py, pRect})
    rect = peek4s({pRect, 8})
    free(pRect)
    return rect
end function

global function cdCanvasGetFontDim(atom hCdCanvas)
atom pW, pH, pA, pD
sequence font_metrics
--DEV machine_bits?
    pW = allocate(16)
    pH = pW+4
    pA = pH+4
    pD = pA+4
    c_proc(xcdCanvasGetFontDim, {hCdCanvas, pW, pH, pA, pD})
    font_metrics = peek4s({pW, 4})
    free(pW)
    return font_metrics
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
--DEV machine_bits?
    pXmin = allocate(16)
    pXmax = pXmin+4
    pYmin = pXmax+4
    pYmax = pYmin+4
    c_proc(xcdCanvasGetTextBox, {hCdCanvas, x, y, text, pXmin, pXmax, pYmin, pYmax})
    box = peek4s({pXmin, 4})
    free(pXmin)
    return box
end function

global function cdCanvasGetTextBounds(atom hCdCanvas, atom x, atom y, string text)
atom pRect
sequence bounds
--DEV machine_bits?
    pRect = allocate(32)
    c_proc(xcdCanvasGetTextBounds, {hCdCanvas, x, y, text, pRect})
    bounds = peek4s({pRect, 8})
    free(pRect)
    return bounds
end function

global function cdCanvasGetColorPlanes(atom hCdCanvas)
    return c_func(xcdCanvasGetColorPlanes, {hCdCanvas})
end function

global function cdCanvasGetImageRGB(atom hCdCanvas, atom x, atom y, atom w, atom h)
atom pR, pG, pB
sequence rgb

    pR = allocate(3*w*h)
    pG = pR+w*h
    pB = pG+w*h
    c_proc(xcdCanvasGetImageRGB, {hCdCanvas, pR, pG, pB, x, y, w, h})
    rgb = repeat({}, 3)
    rgb[1] = peek({pR, w*h})
    rgb[2] = peek({pG, w*h})
    rgb[3] = peek({pB, w*h})
    free(pR)
    return rgb
end function

global procedure cdCanvasPutImageRectRGB(atom hCdCanvas, atom iw, atom ih, sequence rgb, atom x, atom y,
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

global procedure wdCanvasWindow(atom hCdCanvas, atom xmin, atom xmax, atom ymin, atom ymax)
    c_proc(xwdCanvasWindow, {hCdCanvas, xmin, xmax, ymin, ymax})
end procedure

global function wdCanvasGetWindow(atom hCdCanvas)
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

global procedure wdCanvasViewport(atom hCdCanvas, atom xmin, atom xmax, atom ymin, atom ymax)
    c_proc(xwdCanvasViewport, {hCdCanvas, xmin, xmax, ymin, ymax})
end procedure

global function wdCanvasGetViewport(atom hCdCanvas)
atom pXmin, pXmax, pYmin, pYmax
sequence wdViewport

--DEV machine_bits?
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

