--
-- demo\gui\xpGUI.e
-- ================
--
--  A new (Dec 2022) cross-platform GUI for Phix
--
--  A cross platform GUI:
--   * works on Windows, Linux (including Raspberry OS), and under pwa/p2js.
--   * Uses winAPI directly on Windows, essentially arwen-based but more IUP-like.
--   * Uses GTK on Linux, likewise Raspberry OS, and can be induced to on Windows.
--   * Uses xpGUI.js in a browser, a manually hand-crafted drop-in replacement.
--  Uses an IUP-inspired attribute system, although IUP itself is not used at all.
--  Routines are named after Iup, eg gSetAttribute() is like IupSetAttribute().
--
--  Motivation:
--   * Installing IUP on Linux was nowhere near as easy as I hoped it would be,
--     plus there are no prebuilt binaries(/suport) of IUP for an ARM machine.
--   * Low-level hiccups, in particular the inability to prevent <Alt x> bleeping,
--     and timing problems triggering unexpected/unstoppable program shutdowns.
--   * The IUP layout is not the best fit for the browser box model, and it is
--     far easier to modify desktop/Phix than Chrome, FireFox, and all the rest.
--   * Much of the work already done in pGUI.js should be easily transferrable.
--   * Likewise much of the IUP documentation should need only minor adjustments,
--     that is eg copy IupButton.htm to gButton.htm, and rip out a few things.
--   * It should also be reasonable easy to transfer opengl.e by removing both of
--     iup_c_func/proc() and making define_c_func/proc() crash by default. [DONE]
--  Using GTK on Windows almost certainly won't be officially supported, but it
--  should obviously prove rather useful for testing and development purposes.
--
--  Progress is expected to be rather slow and cautious: while existing libs 
--   such as Arwen and EuGTK are a great source of reference, they contain a
--   pile of heavily refactored code, into which it is simply impractical to
--   inject consistent cross-platform behaviour...
--
--  Some bits may end up in separate file(s), so they can be auto-transpiled.
--
--  Demo/test   Description/details:
--   win01.exw  Two overlapping windows
--                  Escape quits one at a time, can be disabled.
--                  Window placement [IN PROGRESS...]
--
--  Translation from pGUI: (see also xpGUI.htm)
--  =====================
--    Obviously, include pGUI.e ==> include xpGUI.e, and IupOpen() is no more.
--    IupDialog(child,) ==> gDialog(child,parent,) [parent must be given at creation time]
--    cbfunc res = Icallback("name") ==> gfunc res = gCallback(name) [no ""]
--    IupXxxx() ==> gXxxx(), and IUP_XXX ==> XPG_XXX, in general.
--
-- Use rem in preference to px, since rem honour user perferences. 
--      px/16=rem, eg 24px===1.5rem (on a standard system)
-- Users can modify the browsers default font size in their browser settings,
--  however I am unaware of an equivalent setting for the desktop.
--
-- Make a list of existing pGUI demos and try them against xpGUI.js, while
-- editing xpGUI.js to be as close as possible to pGUI.js w/o sizing issues.
-- Maybe xpgmap.js which is just 100s of "const gHandle = IupHandle;", etc.
-- Or maybe pGUIM.js which is just 100s of "const IupHandle = gHandle;"...
--
-- Partial: https://en.wikibooks.org/wiki/GTK%2B_By_Example

--constant bUseMapping = true -- **DEV**
without debug
include pprntfN.e
include cffi.e
include pcfunc.e
include scanf.e
include ubits.e
with debug

local constant integer W = machine_word(),
                       M = machine_bits()

local bool bInit = false, -- (xpg_Init() not yet called)
         bUseGTK = platform()!=WINDOWS

global procedure gUseGTK()
    assert(not bInit)
    bUseGTK = true
end procedure

--global function gUsingGTK()
--  return bUseGTK
--end function

local constant UNDEFINED = 0

local integer PrimaryWindowID = UNDEFINED -- id of the main application window
local integer handlers --= new_dict()   -- key is {gdx,name}, data is integer routine_id

--DEV/SUG: forward function gDialog(...), then DIALOG:=gDialog, or use gDialog directly,
--  and have the debug [not constant?] aid {crl_ids,ctrl_names} = columnize({}) instead...
-- DEV/SUG with debug [ctrl_ids, ctrl_names] - ensure they *do* appear in an ex.err file [or perhaps just show them anyway].
--XXX(BLUFF) NB: Windows only, GTK uses GdkWindow etc directly instead. [erm, pardon, [DEV]?!!!]
local constant control_set = {{  DIALOG:=1,"Dialog"}, --, CE_DLGMAX},
--                            {     BOX:=2,"Box"},
--                            {    MENU:=3,"Menu"},
--X                           {   SPLIT:=4,"Split"},
--                            {    TABS:=4,"Tabs"},
                             -- no children past here --
                              {  BUTTON:=5,"Button"},
                              {TREEVIEW:=6,"TreeView"},
                              {  CANVAS:=7,"Canvas"}},
--                            {   TIMER:=8,"Timer"}},
                ctrl_names = vslice(control_set,2), -- (for error messages only)
--                ctrl_rdx = reinstate({},vslice(control_set,1),{}), -- (reverse lookup)
            CF_NEVER_SHOWN = 0x0001,    -- (only meaningful on dialogs)
           CF_CLOSE_ON_ESC = 0x0002,    -- (only meaningful on dialogs)
                 CF_MAPPED = 0x0004,    -- (if 0, ctrl_extra contains CE_CRID, CE_ARGS)
--             CF_VERTICAL = 0x0008,    -- (only meaningful on BOX controls)
--                CF_SPLIT = 0x0010,    -- (only meaningful on BOX controls)
--                CF_MULTI = 0x0020,    -- (only meaningful on BOX controls)
--                CF_FRAME = 0x0040,    -- (only meaningful on BOX controls)
--                CF_RADIO = 0x0080,    -- (only meaningful on BOX controls)
--              CF_EXPANDH = 0x0100,    -- (only meaningful on child elements?)
--              CF_EXPANDV = 0x0200     -- (only meaningful on child elements?)
$

--indexed by gdx:
local sequence ctrl_handles = {},   -- (native handles) (nee ObjectHwnd)
                 ctrl_types = {},   -- (also ctrl_free_list) (nee ObjectType)
                 ctrl_flags = {},   -- (CF_XXX settings, see above)
                 parent_ids = {},   -- (as gdx) (nee ObjectParent) -- [DEV, but deferred?]
               children_ids = {},   -- (as gdx) (nee ObjectChildren) (also cairo context?)
                create_rids = {},   -- eg xpg_Dialog
                create_args = {},   --      (""-specific)
--DEV...
--               attributes = {},
--              attr_values = {},
              natural_width = {},   -- 
             natural_height = {},   -- 
              last_focus_id = {},   -- (WinAPI dialogs only) (nee ObjectExtra[id][LASTFOCUS])
              wnd_proc_addr = {},   --       ""
              sub_proc_addr = {}    --       ""
--            deferred_attr = {}    -- (per-id chains, starting from [CE_ACHAIN])
--SUG:
-- (deferred info, clobbered on actual creation, eg xpg_Dialog)
--local enum CE_CRID, CE_ARGS, CE_ACHAIN, CE_SIZE=$
-- WinAPI dialogs:
--local enum CE_MAINDC, CE_LAST_FOCUS, CE_WNDPROC, CE_SUBPROC, CE_DLGMAX=$
--               ctrl_extra = {}    -- see CE_XXX constants above

-- general routine naming conventions:
--  gXxxx: global routines, cross platform
--  xpg_gtk_xxx: local, gtk only
--  xpg_WinAPI_xxx: local, WinAPI only
--  xpg_xxx: local, cross_platform
--  a_xxx: cribs from arwen, to go. (none left at the mo...)

-- types:
--  xpg_handle: (local) a handle in ctrl_handles[], as returned by eg gtk_window_new or CreateWindowEx
--  gdx: an index to the internal tables, such as ctrl_handles[] [nee ObjectHwnd], and not xpg_handle.
--  rtn: NULL or the routine-id (aka integer index into symtab) of a suitable handler routine.

local type xpg_handle(atom h) -- (esp: not a gdx by mistake)
    -- aside: falls into the "technically unsafe" category, as in many things that
    --        are "obviously" not may pass (eg #2000000), but hopefully worthwhile.
    return h=NULL or h>length(ctrl_handles) or find(h,ctrl_handles)
end type

global type gdx(object hdx)
    integer l = length(ctrl_handles)
    if sequence(hdx) then
        for h in hdx do
            if not (integer(h) and h>=0 and h<=l) then return false end if
        end for
        return true
    end if
    return integer(hdx) and hdx>=0 and hdx<=l
end type

integer last_xpgui_rid = gdx -- (overwritten with gMainLoopf[or whatever] later on in this file)

global type rtn(object rid)
    return integer(rid) and (rid=NULL or rid>last_xpgui_rid)
end type

integer ctrl_free_list = 0

--DEV might not have parent_id here/yet...
--local function xpg_add_control(integer ctrl_type, gdx parent_id, atom handle=NULL, bool bEsc=false) -- (nee addControl)
--local function xpg_add_control(integer ctrl_type, gdx parent_id=NULL, bool bEsc=false) -- (nee addControl)
local function xpg_add_control(integer ctrl_type, parent_id=0, flags=0) -- (nee addControl)
--local function xpg_add_control(integer ctrl_type) -- (nee addControl)
--
--  returns: a gdx
--  control_type: DIALOG, TREEVIEW, etc, stored in ctrl_types[id]
--  parent_id: a gdx and not a native handle, stored in parent_ids[id]
--  handle: a native handle for the new control, stored in ctrl_handles[id]
--  bEsc: true/false (meaningful only on dialogs)
--
--  integer id, flags = CF_NEVER_SHOWN+iff(bEsc?CF_CLOSE_ON_ESC:0)
    flags = or_bits(flags,CF_NEVER_SHOWN)
    integer id
--,ex_len = control_set[ctrl_type]
    if ctrl_free_list then
        id = ctrl_free_list
        ctrl_free_list = ctrl_types[ctrl_free_list]
--      ctrl_handles[id] = handle
        ctrl_handles[id] = NULL
        ctrl_types[id] = ctrl_type
        ctrl_flags[id] = flags
        parent_ids[id] = parent_id
        children_ids[id] = UNDEFINED
        create_rids[id] = UNDEFINED
        create_args[id] = UNDEFINED
        natural_width[id] = UNDEFINED
        natural_height[id] = UNDEFINED
--      attributes[id] = {}
--      attr_values[id] = {}
        last_focus_id[id] = UNDEFINED
        wnd_proc_addr[id] = UNDEFINED
        sub_proc_addr[id] = UNDEFINED
    else
--      ctrl_handles &= handle
        ctrl_handles &= NULL
        ctrl_types &= ctrl_type
        ctrl_flags &= flags
        parent_ids &= parent_id
        children_ids &= UNDEFINED
        create_rids &= UNDEFINED
        create_args &= UNDEFINED
        natural_width &= UNDEFINED
        natural_height &= UNDEFINED
--      attributes &= {{}}
--      attr_values &= {{}}
        last_focus_id &= UNDEFINED
        wnd_proc_addr &= UNDEFINED
        sub_proc_addr &= UNDEFINED
        id = length(ctrl_handles)
    end if
--DEV possibly insufficient: gDialog has 1, gSplit 2, g(H|V|Multi)Box|gRadio|gTabs have many.
--  if ctrl_type=DIALOG then
    if ctrl_type<BUTTON then
        children_ids[id] = {}
    end if
    return id
end function

--local procedure free_ctrl(integer tdx)
--  ctrl_handles[tdx] = -999 // (since UNDEFINED[=0] or -1 might trap fewer errors)
--  ctrl_types[tdx] = ctrl_free_list
--  ctrl_free_list = tdx
--end procedure

-- NB: WinAPI only, GTK uses a GtkTreeStore instead.
sequence tree_items = {},
         extensions = {':'} -- for imagelist lookup
                            -- 0 is folder, 1 is drive.
-- indexes to tree_items:
local constant tHandle   = 1,
         tText     = 2,
         tUserData = 3
--       tLoaded   = 4,
--       tPidx     = 5

local integer tree_free_list = 0

local function xpg_WinAPI_new_tree_item(string text, bool bLoaded=true, integer pidx=0) -- (NB WinAPI only)
    integer res
    if tree_free_list then
        res = tree_free_list
        tree_free_list = tree_items[tree_free_list]
        tree_items[res] = {0,text,0,bLoaded,pidx}
    else
        tree_items = append(tree_items,{0,text,0,bLoaded,pidx})
        res = length(tree_items)
    end if
    return res
end function

local procedure xpg_WinAPI_free_tree_item(integer tdx)
    tree_items[tdx] = tree_free_list
    tree_free_list = tdx
end procedure

--/*
----**NB** we may yet want these for private use within this file itself... then again probably not.
--local sequence gallbacks = {},
--       gnames = {},
--       grids = {}
--
--global type gfunc(atom cb)
--  --
--  -- (aside: rfind() is used (instead of find) on the assumption that 
--  --         nine cases out of ten will use the last one just created
--  --         at least in the gButton("ok",gCallback(ok_cb)) and 
--  --         [hence] gSetHandler() etc common use cases anyway.)
--  --
--  return cb=NULL or rfind(cb,gallbacks)!=0
--end type
--
--global function gCallback(integer rid)
--  string name = get_routine_info(rid)[4]
--  integer k = find(rid,grids)
--  atom cb
--  if k=0 then
--      cb = call_back({'+', rid})
--      gallbacks = append(gallbacks,cb)
--      gnames = append(gnames,name)
--      grids = append(grids,rid)
--  else
--      assert(gnames[k]==name)
--      cb = gallbacks[k]
--  end if
--  return cb
--end function
--
--global function g_name_from_cb(atom/*gfunc*/ cb)
--  string name = gnames[find(cb,gallbacks)]
--  return name
--end function
--
--global function g_cb_from_name(string name)
--  integer idx = find(name,gnames)
--  if idx=0 then return NULL end if
--  assert(find(name,gnames,idx+1)==0,"multiple definitions")
--  atom/*gfunc*/ cb = gallbacks[idx]
--  return cb
--end function
--*/

-- (used internally...)
-- /*global*/ function g_rid_from_cb(atom cb)
--  integer rid = grids[find(cb,gallbacks)]
--  return rid
--end function

local function xpg_raw_string_ptr(string s)
--
-- Returns a raw string pointer for s, somewhat like allocate_string(s) but using the existing memory.
-- NOTE: The return is only valid as long as the value passed as the parameter remains in existence.
--       In particular, callbacks must make a semi-permanent copy somewhere other than locals/temps.
--       (one example in xpGUI where that /still/ applies would be in setting say tvItem.pszText)
--
    atom res
    #ilASM{
        [32]
            mov eax,[s]
            lea edi,[res]
            shl eax,2
        [64]
            mov rax,[s]
            lea rdi,[res]
            shl rax,2
        []
            call :%pStoreMint
          }
    return res
end function

local function xpg_word_array(sequence v)
    integer l = length(v)
    sequence v0 = repeat(0,l+1)
    for i=1 to l do
        object vi = v[i]
        if string(vi) then
            vi = xpg_raw_string_ptr(vi)
        end if
        v0[i] = vi
    end for
    atom pList = allocate(length(v0)*W,true)
    pokeN(pList, v0, W)
    return pList
end function

global constant VK_ESC          = 27,   -- = #1B
                VK_TAB          =  9,   -- = '\t'
                XPG_IGNORE      = -1,
                XPG_DEFAULT     = -2,
                XPG_CLOSE       = -3,
                XPG_CONTINUE    = -4

include builtins\ptypes.e   -- (types atom_string, rid_string, nullable_string, and boolean)

include builtins\cffi.e

local constant  --G_TYPE_CHAR = 3*4,
                --G_TYPE_UCHAR = 4*4,
                --G_TYPE_BOOLEAN = 5*4,
                G_TYPE_INT = 6*4,
                --G_TYPE_UINT = 7*4,
                --G_TYPE_LONG = 8*4,
                --G_TYPE_ULONG = 9*4,
                --G_TYPE_INT64 = 10*14,
                --G_TYPE_UINT64 = 11*4,
                --G_TYPE_ENUM = 12*4,
                --G_TYPE_FLAGS = 13*4,
                --G_TYPE_FLOAT = 14*4,
                --G_TYPE_DOUBLE = 15*4,
                G_TYPE_STRING = 16*4,
                --G_TYPE_POINTER = 17*4),   
--              GDK_BUTTON_PRESS_MASK  = 1 << 8
--              GDK_KEY_PRESS_MASK  = 1 << 10,
--              GDK_EXPOSURE_MASK  = 1 << 1,
                GDK_BKE_MASK = 0b0101_0000_0010,
                GDK_KEY_ESCAPE = #FF1B,
                GTK_SELECTION_NONE = 0,
--              GTK_SELECTION_SINGLE = 1
                GTK_WINDOW_TOPLEVEL = 0,
--              GTK_WINDOW_POPUP = 1,               -- menu/tooltip (not a proper sub-window)
                GTK_POLICY_AUTOMATIC = 1,

--              BI_RGB = 0,
                BS_PUSHBUTTON = 0,
                CBM_INIT = #4,   --  initialize bitmap
                COLOR_BTNFACE = 15,
                CS_VREDRAW = 1,
                CS_HREDRAW = 2,
                CS_DBLCLKS = 8,
--              CS_OWNDC = #20,
                CW_USEDEFAULT = #80000000,
                DIB_RGB_COLORS = 0,
--              DT_SINGLELINE = #0020,
--              DT_CENTER      = #0001,
--              DT_VCENTER   = #0004,
--              DT_SINGLECENTER = or_all({DT_SINGLELINE,DT_CENTER,DT_VCENTER}),
--              ERROR_CLASS_ALREADY_EXISTS = 1410,
--              GWL_EXSTYLE = -20,
--              GWL_STYLE = -16,
                GWL_USERDATA = -21,
                GWL_WNDPROC = -4,
                I_IMAGECALLBACK = -1,
                IDC_ARROW = 32512,
                ILC_MASK        = 1,
--              ILC_COLOR       = 0,
--              ILC_COLORDDB    = #FE,
--              ILC_COLOR4      = #00,
                ILC_COLOR8      = #08,
--              ILC_COLOR16     = #10,
--              ILC_COLOR24     = #18,
--              ILC_COLOR32     = #20,
                LPSTR_TEXTCALLBACK = -1,
                SM_CXSCREEN = 0,
                SM_CYSCREEN = 1,
                SM_CXSMICON = 49,
                SM_CYSMICON = 50,
--              SND_FILENAME    = #00020000,
--              SND_ASYNC       = #00000001,
--              SND_FILEASYNC   = or_bits(SND_FILENAME,SND_ASYNC),
                SW_HIDE         = 0,
                SW_SHOWNORMAL       = 1,
--              SW_SHOWNOACTIVATE   = 4,
--              SW_SHOWNA           = 8,
                SWP_NOSIZE          = #0001,
                SWP_NOMOVE          = #0002,
                SWP_NOZORDER        = #0004,
--              SWP_NOREDRAW        = #0008,
                SWP_NOACTIVATE      = #0010,
--              SWP_FRAMECHANGED    = #0020,
--              SWP_SHOWWINDOW      = #0040,
--              SWP_HIDEWINDOW      = #0080,
--              SWP_NOCOPYBITS      = #0100,
--              SWP_NOOWNERZORDER   = #0200,
--              SWP_NOSENDCHANGING  = #0400,
--              SWP_DRAWFRAME       = SWP_FRAMECHANGED,
--              SWP_NOREPOSITION    = SWP_NOOWNERZORDER,
--              SWP_DEFERERASE      = #2000,
--              SWP_ASYNCWINDOWPOS  = #4000,
--              SWP_UPDATECACHE     = SWP_NOSIZE+SWP_NOMOVE+SWP_NOZORDER+SWP_FRAMECHANGED+SWP_NOCOPYBITS,
                TV_FIRST            = #1100,
                TVE_COLLAPSE        = 1,
                TVE_EXPAND          = 2,
                TVGN_CHILD          = 4,
                TVGN_ROOT           = 0,
                TVI_LAST            = -0x0FFFE,
                TVIF_TEXT           = #01,
                TVIF_IMAGE          = #02,
                TVIF_PARAM          = #04,
--              TVIF_STATE          = #08,
                TVIF_HANDLE         = #10,
                TVIF_SELECTEDIMAGE  = #20,
                TVIF_CHILDREN       = #40,
--              TVIF_INTEGRAL       = #80,
                TVIF_EXPANDEDIMAGE  = #200,
                TVIS_EXPANDED       = #0020,
                TVM_DELETEITEM      = (TV_FIRST + 1),
                TVM_EXPAND          = (TV_FIRST + 2),
--              TVM_INSERTITEMA     = (TV_FIRST + 0),
--              TVM_INSERTITEM      = TVM_INSERTITEMA,
                TVM_INSERTITEM      = (TV_FIRST + 0),
                TVM_SETIMAGELIST    = (TV_FIRST + 9),
                TVM_GETNEXTITEM     = (TV_FIRST + 10),
                TVM_GETITEM         = (TV_FIRST + 12),
                TVM_SETITEM         = (TV_FIRST + 13),
--              TVN_SELCHANGED      = -402,
--              TVN_SELCHANGEDW     = -451,
                TVN_GETDISPINFO     = -403,
--              TVN_GETDISPINFOW    = -452,
                TVN_ITEMEXPANDED    = -406,
                TVS_HASBUTTONS      = 1,
                TVS_HASLINES        = 2,
                TVS_LINESATROOT     = 4,
                TVSIL_NORMAL        = 0,
--              WHITE_BRUSH = 0,
--              WM_CREATE = 1,
                WM_DESTROY = 2,
--              WM_MOVE = 3,
                WM_SIZE = 5,
--              WM_ACTIVATE = 6,
                WM_SETFOCUS = 7,
--              WM_KILLFOCUS = 8,
--              WM_SETTEXT = 12,
--              WM_PAINT = 15,
                WM_CLOSE = 16,
--              WM_ERASEBKGND = 20,
--              WM_SHOWWINDOW = 24,
--              WM_ACTIVATEAPP = 28,
--              WM_GETMINMAXINFO = 36,
--              WM_WINDOWPOSCHANGING = 70,
--              WM_WINDOWPOSCHANGED = 71,
                WM_NOTIFY = 78,
--              WM_GETICON = 127,
--              WM_NCCREATE = 129,
--              WM_NCDESTROY = 130,
--              WM_NCCALCSIZE = 131,
--              WM_NCPAINT = 133,
--              WM_NCACTIVATE = 134,
                WM_KEYDOWN = 256,
                WM_CHAR = 258,
--              WM_SYSKEYDOWN = 260,
                WM_COMMAND = 273,
--              WM_IME_SETCONTEXT = 641,
--              WM_IME_NOTIFY = 642,
                WS_CHILD            = #40000000,
--              WS_POPUP            = #80000000,
--              WS_POPUPWINDOW      = #80880000,    -- = WS_POPUP+WS+BORDER+WS_SYSMENU
--              WS_CLIPSIBLINGS     = #04000000,
                WS_EX_ACCEPTFILES   = #00000010,
                WS_EX_CLIENTEDGE    = #00000200,
--              WS_EX_NOACTIVATE    = #08000000,
--              WS_EX_TOPMOST       = #00000008,
--              WS_CAPTION          = #00C00000,    -- (==WS_DLGFRAME+WS_BORDER)
--              WS_MINIMIZE         = #20000000,
--              WS_MINIMIZEBOX      = #00020000,
--              WS_MAXIMIZEBOX      = #00010000,
--              WS_OVERLAPPED       = #00000000,
--              WS_SYSMENU          = #00080000,
                WS_TABSTOP          = #00010000,
--              WS_THICKFRAME       = #00040000,    -- (==WS_SIZEBOX)
                WS_VISIBLE          = #10000000,
--              WS_OVERLAPPEDWINDOW = or_all({WS_OVERLAPPED,WS_CAPTION,WS_SYSMENU,
--                                            WS_THICKFRAME,WS_MINIMIZEBOX,WS_MAXIMIZEBOX})
                WS_OVERLAPPEDWINDOW = #00CF0000

local integer   gdk_display_get_default,
                gdk_display_get_monitor,
                gdk_monitor_get_geometry,
                gdk_pixbuf_get_type,
                gdk_pixbuf_new_from_xpm_data,
                gdk_screen_get_default,
                gdk_screen_get_height,
                gdk_screen_get_width,
--              gdk_screen_get_root_window,
--              gdk_window_get_geometry,
--              gdk_window_get_height,
--              gdk_window_get_width,
--              gdk_window_get_position,
--              gdk_window_move_resize,
                gtk_box_pack_start,
                gtk_button_new_with_mnemonic,
                gtk_cell_renderer_pixbuf_new,
                gtk_cell_renderer_text_new,
                gtk_container_add,
                gtk_css_provider_new,
                gtk_css_provider_load_from_data,
                gtk_drawing_area_new,
                gtk_hbox_new,
--              gtk_list_store_newv,
                gtk_list_store_append,
--              gtk_list_store_set_valuesv, -- (borken)
                gtk_list_store_set,
                gtk_main,
                gtk_main_quit,
                gtk_scrolled_window_new,
                gtk_scrolled_window_set_policy,
                gtk_style_context_add_provider_for_screen,
--              gtk_tree_iter_copy,
--              gtk_tree_iter_free,
                gtk_tree_model_get,
                gtk_tree_model_get_iter,
                gtk_tree_model_get_path,
                gtk_tree_model_get_string_from_iter,
--              gtk_tree_model_get_value,
                gtk_tree_model_iter_children,
                gtk_tree_path_free,
--              gtk_tree_path_new_from_string,
--              gtk_tree_selection_get_selected,
                gtk_tree_selection_set_mode,
--              gtk_tree_selection_set_select_function,
                gtk_tree_store_newv,
                gtk_tree_store_append,
                gtk_tree_store_clear,
                gtk_tree_store_remove,
                gtk_tree_store_set,
--              gtk_tree_store_set_valuesv, -- (borken)
                gtk_tree_view_column_new,
                gtk_tree_view_column_pack_start,
                gtk_tree_view_column_add_attribute,
                gtk_tree_view_collapse_row,
                gtk_tree_view_expand_row,
                gtk_tree_view_expand_to_path,
                gtk_tree_view_insert_column,
--              gtk_tree_view_insert_column_with_attributes, -- (decided against)
                gtk_tree_view_get_cursor,
                gtk_tree_view_get_model,
                gtk_tree_view_get_selection,
                gtk_tree_view_new,
                gtk_tree_view_set_enable_search,
                gtk_tree_view_set_enable_tree_lines,
--              gtk_tree_view_set_grid_lines,
                gtk_tree_view_set_headers_visible,
                gtk_tree_view_set_model,
--              gtk_tree_view_set_search_column,
                gtk_vbox_new,
--              gtk_widget_get_root_window,
                gtk_widget_hide,
                gtk_widget_set_events,
                gtk_window_move,
                gtk_window_new,
--              gtk_widget_show,
                gtk_widget_show_all,
                gtk_window_set_default_size,
                gtk_window_set_title,
                gtk_window_set_transient_for,
                gtk_window_get_transient_for,
                gtk_window_get_position,
                gtk_window_get_size,
                g_signal_connect_data,
                g_object_get_property,
                g_object_set_property,
--              g_value_get_int,
--              g_value_init,
--              g_value_unset,
                xg_object_unref,
                idGdkEventKey,
                idGdkRectangle,
                GTK_ID_LOOKUP,
                GDK_TYPE_PIXBUF,

                xBeginPaint,
                xCallWindowProc,
                xCreateDIBitmap,
                xCreateWindowEx,
                xDefWindowProc,
                xDeleteObject,
                xDestroyWindow,
                xDispatchMessage,
                xDrawText,
                xEndPaint,
                xGetClientRect,
                xGetConsoleWindow,
                xGetDC,
                xGetFocus,
                xGetForegroundWindow,
                xGetLastError,
                xGetMessage, 
                xGetStockObject,
                xGetSystemMetrics,
                xGetTextExtentPoint32,
                xGetWindowLong,
                xGetWindowRect,
                xImageList_Add,
                xImageList_Create,
                xLoadCursor,
                xLoadIcon,
                xMoveWindow,
--              xPlaySound,
                xPostQuitMessage,
                xRegisterClassEx,
                xReleaseDC,
                xSendMessage,
                xSetFocus,
                xSetParent,
                xSetWindowLong,
                xSetWindowPos,
                xSetWindowText,
                xShowWindow,
                xTranslateMessage,
                xUpdateWindow,
                idPOINT,
                idMESSAGE,
                idRECT,
                idSIZE,
                idPAINTSTRUCT,
                idNMHDR,
                idTVITEM,
                idTVITEMEX,
                idNMTREEVIEW,
                idTVINSERTSTRUCT

local atom szAppName, pData, pPAINTSTRUCT, pRECT, pSIZE, pTVINSERTSTRUCT, pTVITEMEX, pMSG

local function xpg_intint(string val)
    -- convert eg "225x75" to {225,75}
    sequence res = apply(split(val,'x'),to_number)
    return res
end function

local procedure xpg_setID(atom handle, integer id) -- (nee setID)
--DEV/SUG: (seems good!)
--/!*
    ctrl_handles[id] = handle
--  assert(and_bits(ctrl_flags[id],CF_MAPPED)=0)
--  ctrl_flags[id] += CF_MAPPED
--  if not and_bits(ctrl_flags[id],CF_MAPPED) then
--      ctrl_flags[id] += CF_MAPPED
--  end if
    ctrl_flags[id] = or_bits(ctrl_flags[id],CF_MAPPED)
--*!/
    integer ct = ctrl_types[id]
    if ct!=CANVAS then
        if bUseGTK then
----[DEV] doesn't work... (WHO CARES?...)
----        poken(pData,id)
----        c_proc(g_object_set_property,{handle,"id",pData})
            setd(handle,id,GTK_ID_LOOKUP)
        else
            {} = c_func(xSetWindowLong,{handle,GWL_USERDATA,id})
            if ct=DIALOG then
--DEV dunno if this has or has not been fixed, let's test it... (seems good!)
--          atom tmp = c_func(xGetWindowLong, {handle, GWL_WNDPROC})
--          wnd_proc_addr[id] = tmp
                wnd_proc_addr[id] = c_func(xGetWindowLong, {handle, GWL_WNDPROC})
            end if
        end if
    end if
end procedure 

local function xpg_getID(xpg_handle handle) -- (nee getID)
    -- returns: a gdx
    integer id = 0
    if handle!=NULL then
        if bUseGTK then
--poken(pData,0) -- kills it... (DITTO)
--          c_proc(g_object_get_property,{handle,"id",pData})
--          id = peekns(pData)
--          id = find(handle,ctrl_handles)
            id = getd(handle,GTK_ID_LOOKUP)
--          if id then
--              assert(ctrl_handles[id]=handle)
--          else
--              assert(find(handle,ctrl_handles)=0)
--          end if
        else
            id = c_func(xGetWindowLong,{handle,GWL_USERDATA})
--DEV this may still thwack... (can we do something with assume_id??) [seems ok]
--          assert(iff(id?ctrl_handles[id]=handle
--                       :find(handle,ctrl_handles)=0))
        end if
        assert(iff(id?ctrl_handles[id]=handle
                     :find(handle,ctrl_handles)=0))
--      assert(id!=0) -- no, see a_WndProc!
--else -- temp... (never triggered)
--  ?"handle==NULL (line 739)"
    end if
    return id
end function

--DEV/SUG drop the "everything must be a string" restriction...
-- DOCS: 
--  You can of course replace eg [Iup/]gSetAttribute(id,"SIZE","%dx%d",{w,h}) with
--                                     gSetAttribute(id,"SIZE",{w,h}) which is not
--  only neater but also (to be fair, insignificantly) faster.
--  Note that, unlike say <a href="gSetHandler.htm">gSetHandler[s]</a>(), the "with-s" version, 
--  ie <a href="gSetAttributes.htm">gSetAttributes</a>(), has a different purpose/api, being 
--  multiple attributes in one string, whereas this "without-s" version sets a single attribute, 
--  with both being able to apply said to one or more elements.
--global procedure gSetAttribute(gdx id, string name, nullable_string val, sequence args={})
global procedure gSetAttribute(gdx id, string name, object v, sequence args={})
    if length(args) then v = sprintf(v,args) end if
    if sequence(id) then
        for i in id do
            assert(integer(i))
            gSetAttribute(i,name,v)
        end for
    else
        atom handle = ctrl_handles[id]
        integer ct = ctrl_types[id]
--      assert(and_bits(ctrl_flags[id],CF_MAPPED)!=0)
--      bool mapped = and_bits(ctrl_flags[id],CF_MAPPED)!=0
--      bool mapped = (not bUseMapping) or and_bits(ctrl_flags[h],CF_MAPPED)!=0
--      if not mapped then
        if and_bits(ctrl_flags[id],CF_MAPPED)=0 then
----dev, no, slap then in ctrl_handles[id] instead...
-- we're going to need a couple of {}, or probably better: a single table with per-id chains, eg (untested!)
--          deferred_attr &= {{gSetAttribute,name,val,ctrl_extra[id][CE_ACHAIN]}}
--          ctrl_extra[id][CE_ACHAIN] = length(deferred_attr)
?9/0
--  (note chain is backwards, so [evenbtually] do this:)
--procedure apply_deferred_attributes(integer id, chain)
--  if chain then
--      {string name, object v, integer next} = deferred_attr[chain]
--      apply_deferred_attributes(next)
--      gSetAttributes(id,name,v)
--  end if
--end procedure
----            attributes[id] = append(attributes[id],name)
----            attr_values[id] = append(attr_values[id],val)
--          return
        end if

        if ct=DIALOG then
            if name="TITLE" then
--              if mapped then
                    if bUseGTK then
                        c_proc(gtk_window_set_title,{handle,v}) 
                    else
                        c_proc(xSetWindowText,{handle,v})
                    end if
--              end if
                return
            elsif name="SIZE" then
                if string(v) then v = xpg_intint(v) end if
--              integer {width,height} = xpg_intint(v)
--              if mapped then
--DEV NULL should be allowed too??
                    integer {width,height} = v
                    if bUseGTK then
                        c_proc(gtk_window_set_default_size,{handle,width,height}) 
                    else
                        integer flags = SWP_NOMOVE+SWP_NOZORDER+SWP_NOACTIVATE
                        bool ok = c_func(xSetWindowPos,{handle,NULL,0,0,width,height,flags})
                        assert(ok)
                    end if
--              end if
                return
            end if
        end if
        -- (placeholder for more code and/or typo-catcher:)
        crash("gSetAttribute(%s,%s,%s)",{ctrl_names[ct],name,iff(v=NULL?"NULL":v)})
    end if
end procedure

--local procedure xpg_set_any_deferred_attrs(gdx id)
--  for i=1 to length(attributes[id]) do
--      gSetAttribute(id, attributes[id][i], attr_values[id][i])
--  end for
--end procedure

global procedure gSetAttributes(gdx id, string attributes, sequence args={})
--procedure gSetAttributes(gdx id, string attributes, sequence args={}, integer nFrames=1)
    // (manually translated from iup_attrib.c)
--DEV foobars the switch toktype (try it!)
--  static [XPGLEX_TK_END,XPGLEX_TK_SET,XPGLEX_TK_COMMA,XPGLEX_TK_NAME]
--  constant XPGLEX_TK_END   = 0,
    integer XPGLEX_TK_END    = 0,
             XPGLEX_TK_SET   = 1,
             XPGLEX_TK_COMMA = 2,
             XPGLEX_TK_NAME  = 3,
             XPGLEX_TK_HEX   = 4;
    if length(attributes) then
        if length(args) then
            attributes = sprintf(attributes,args)
        end if
        integer i = 1, 
                l = length(attributes)
        string token = ""
        object name = null,
                val = null
        bool get_name = true, // (else get_value)
                 bEnd = false
        while true do
            integer toktype = XPGLEX_TK_END
            while i<=l do
                integer ch = attributes[i]
                i += 1
                string delims = ""
                switch ch do
                    case '%':   // Skip comment
                        while i<=l do
                            ch = attributes[i]
                            i += 1
                            if ch=='\n' then exit end if
                        end while
                    case '#':   // Hex constant
                        toktype = XPGLEX_TK_HEX
                        i -= 1;                     // unget first character
                        delims = ", \n\r\t"         // get until delimiter
                    case ' ', '\n', '\r':
                        break
                    case '=':
                        toktype = XPGLEX_TK_SET
                    case ',':
                        toktype = XPGLEX_TK_COMMA
                    case '"':                       // string
                        delims = "\""
                    default:
                        if ch>' ' then              // identifier
                            i -= 1;                 // unget first character
                            delims = "=, \n\r\t"    // get until delimiter
                        end if
                end switch
                if length(delims) then
                    token = ""
                    while i<=l do
                        ch = attributes[i]
                        i += 1
                        if find(ch,delims) then exit end if
                        if toktype=XPGLEX_TK_HEX then
                            --DEV/SUG "source line with ^"-style error, if we can
                            -- maybe crash_hat("bad hex char",attributes,i,1,nFrames)
                            --   ==>    gSetAttributes(dlg,"BGCOLOR=#1234J678")
                            --                                           ^ bad hex char
                            -- or       bad hex char ("...234(>>J<<)678...") if no match
                            -- crash_hat() attempts to deliver a source-level error with
                            -- a caret indicating the exact point of failure, however and
                            -- of course a compiled program has no access to source code 
                            -- to point at, so that will be more like a normal crash().
                            -- You provide a code fragment, offset and length, which it
                            -- tries to match against the source code line if it can.
                            -- To keep things simple, there is no auto-sprintf(msg,args)
                            -- option in crash_hat(). For an example see xpGUI.e
                            assert(find(ch,"#01234567890ABCDEFabcdef")!=0,"bad hex char")
                            assert(ch!='#' or token="","multiple # chars?")
                        end if
                        token &= ch
                    end while
                    if length(delims)>1 and find(ch,delims) then 
                        i -= 1  // unget delimiter
                    end if
                    if toktype!=XPGLEX_TK_HEX then
                        toktype = XPGLEX_TK_NAME
                    end if
                end if
                if toktype!=XPGLEX_TK_END then exit end if
            end while
            switch toktype do
                case XPGLEX_TK_END:
                    bEnd = true
                    fallthrough
                case XPGLEX_TK_COMMA:
                    if name!=NULL then
                        gSetAttribute(id,name,val)
                    end if
                    if bEnd then return end if
                    name = null
                    val = null
                    get_name = true
                case XPGLEX_TK_SET:
                    get_name = false
                case XPGLEX_TK_NAME:
                    if get_name then
                        name = token
                    else
                        val = token
                    end if
                case XPGLEX_TK_HEX:
                    assert(not get_name)
                    val = token
            end switch
        end while
    end if
end procedure

global procedure gSetHandler(gdx id, sequence name, rtn handler=NULL)
    if sequence(id) then
        for i in id do
            assert(integer(i))
            gSetHandler(i,name,handler)
        end for
    elsif not string(name) then
        assert(handler==NULL)
        assert(even(length(name)))
        for i=1 to length(name) by 2 do
            string ni = name[i]
            integer f = name[i+1]
            gSetHandler(id,ni,f)
        end for
    else
        assert(integer(id) and id>=1 and id<=length(ctrl_handles))
        integer ct = ctrl_types[id]
        --DEV/SUG validate name against ctrl_types[id], and perhaps also the signature, 
        --        including time-bomb KEY_CB thing from pGUI.e (unless we change that)
        -- aside: using deld() for callback==NULL should make no real difference
        if handler!=NULL then
            assert(handler>15) -- (not T_integer..T_object!)
            sequence hinfo = get_routine_info(handler,false)
            string sig = hinfo[3]
            assert(find(sig[1],"FP"))
--untried, might not even need this anyway:
--          if name="KEY_CB" then assert(sig='FIN') end if
        end if
        if name="ACTION" then
            if ct=BUTTON then
                crash(`gSetHandler(BUTTON,"ACTION",rtn) should be changed to "CLICK"`)
            elsif ct=CANVAS then
                crash(`gSetHandler(CANVAS,"ACTION",rtn) should be changed to "REDRAW"`)
            end if
        end if
        setd({id,name},handler,handlers)
    end if
end procedure

global procedure gSetHandlers(gdx id, sequence name, rtn handler=NULL)
    -- (simple alias that implies but does not enforce that h|name|both are non-unitary)
    gSetHandler(id,name,handler)
end procedure

--atom XPG_PARENTDIALOG = NULL

--global procedure gSetAttributeHandle(atom handle, string name, atom ih_named)
--global procedure gSetAttributeHandle(gdx handle, string name, atom ih_named)
--  if name="PARENTDIALOG" then
--      if handle=NULL then
--          XPG_PARENTDIALOG = ih_named
----DEV/SUG
----            crash("Use gSetGlobal("PARENTDIALOG",ih) instead",nFrames:=2)
--      else
--          crash("Use gDialog(child,parent,...) instead",nFrames:=2)
--      end if
--  else
--      printf(1,"gSetAttributeHandle(%d,%s,%d)\n",{handle,name,ih_named})
--  end if
--end procedure

--gdk_window_get_geometry() gdk_window_get_position() or gdk_window_get_origin().
--gdk_window_get_position(), gdk_window_get_width() and gdk_window_get_height() instead, 
--because it avoids the global 
local function xpg_get_window_rect(gdx id)
    -- returns {left,top,width,height}
    xpg_handle handle = ctrl_handles[id]
    if bUseGTK then
        -- [DEV] seems fine on windows 64-bit, might not be on Linux 64 bit...
        atom pX = pRECT, pY = pX+4, pW = pY+4, pH = pW+4
        c_proc(gtk_window_get_position,{handle,pX,pY})
        c_proc(gtk_window_get_size,{handle,pW,pH})
    else
        integer r = c_func(xGetWindowRect, {handle, pRECT})
        assert(r!=0)
    end if
    sequence res = peek4s({pRECT,4})
    if not bUseGTK then
        res[3] -= res[1]    -- right ==> width
        res[4] -= res[2]    -- btm ==> height
    end if
    return res
end function

local function xpg_get_client_rect(gdx id) -- (nee getClientRect)
    -- returns {0,0,width,height}
    xpg_handle handle = ctrl_handles[id]
    if bUseGTK then
        -- [DEV] seems fine on windows 64-bit, might not be on Linux 64 bit...
        atom pX = pRECT, pY = pX+4, pW = pY+4, pH = pW+4
        poke4(pRECT,{0,0})
        c_proc(gtk_window_get_size,{handle,pW,pH})
    else
        integer r = c_func(xGetClientRect, {handle, pRECT})
        assert(r!=0)
    end if
    sequence res = peek4u({pRECT,4})
    return res
end function

--DEV we might want atoms here, to hide any rounding issues... (oh, just do it anyway)
--local procedure xpg_move_window(gdx id, integer x, y)
local procedure xpg_move_window(gdx id, atom x, y)
    xpg_handle handle = ctrl_handles[id]
    if bUseGTK then
        c_proc(gtk_window_move,{handle,x,y})
    else
        integer flags = SWP_NOZORDER+SWP_NOSIZE
        bool ok = c_func(xSetWindowPos,{handle,NULL,x,y,0,0,flags})
        assert(ok)
    end if
end procedure

global procedure gHide(gdx id)
    assert(ctrl_types[id]=DIALOG)
    atom handle = ctrl_handles[id]
    if bUseGTK then
        c_proc(gtk_widget_hide,{handle})
        if c_func(gtk_window_get_transient_for,{handle})=NULL then
            c_proc(gtk_main_quit)
        end if
    else
        {} = c_func(xSendMessage,{handle,WM_CLOSE,0,0}) -- this line has the better way of closing!!!
--      {} = c_func(xCloseWindow,{handle})  -- this function will minimize a child window or minimize & 
--                                          --  half close a main window. very poor name & behaviour!
    end if
end procedure

--local function xpg_gtk_quit() -- (GTK only)
--local function xpg_gtk_quit((GtkObject *object, gpointer user_data))  -- (GTK only)
local function xpg_gtk_quit(atom handle, /*user_data*/) -- (GTK only)
--  c_proc(gtk_main_quit) 
    integer id = xpg_getID(handle)
    gHide(id)
    return 0 
end function 
local constant gtk_main_quit_cb = call_back({'+',xpg_gtk_quit})

local function xpg_gtk_check_escape(atom handle, event, data) -- (GTK only)
--DEV have we any "KEY"
    -- connected to the dialog, not any of the child elements of one,
    -- but (I assume that) gtk bubbles up any messages appropriately.
    -- (aside: not really worth checking handle is an xpg_handle)
    integer key = get_struct_field(idGdkEventKey,event,"keyval")
--DEV check/map VK_ESC:
    if key=GDK_KEY_ESCAPE then
        integer id = xpg_getID(handle)
--      if id then
        bool bClose = and_bits(CF_CLOSE_ON_ESC,ctrl_flags[id])!=0
        if bClose then
            gHide(id)
            return true
        end if
    end if
    return false
end function
local constant gkt_check_esc_cb = call_back({'+',xpg_gtk_check_escape})

local function xpg_get_parent_window(gdx id)    -- (nee getParentWindow)
    while id and ctrl_types[id]!=DIALOG do
        id = parent_ids[id]
    end while
    return id
end function

local procedure xpg_set_focus(gdx id)
    if bUseGTK then ?9/0 end if --DEV
    if ctrl_types[id]=DIALOG then
        -- Attempts to programmatically set the focus onto a window
        -- (like Edita does with main) must get rid of any auto-focus info.
        last_focus_id[id] = UNDEFINED
    end if
    c_proc(xSetFocus, {ctrl_handles[id]})
end procedure

local function xpg_get_focus()
    if bUseGTK then ?9/0 end if --DEV
    atom hFocus = c_func(xGetFocus, {})
    if not hFocus then
        return 0
    end if
    integer id = xpg_getID(hFocus)
    if id<1 or id>length(ctrl_handles) then
        return 0
    end if
    return id
end function

local integer assume_id -- for betwixt xCreateWindowEx() and xpg_setID()?... (windows only)
local atom assumed_hwnd -- ""
-- In other words, gWndProc is likely to get WM_CREATE etc before it has managed to properly log the new control.

local function xpg_WinAPI_sub_proc(atom hwnd, msg, wParam, lParam) -- (nee SubProc)
    integer id = xpg_getID(hwnd),
           pid = xpg_get_parent_window(id),
            ct = ctrl_types[id] 
--if ct=BUTTON then
--  ?{id,msg}
--end if
    if msg=WM_SETFOCUS then
        -- save the item being focussed on in the parent window
        last_focus_id[pid] = id
    elsif msg=WM_CHAR and wParam=VK_ESC 
      and and_bits(CF_CLOSE_ON_ESC,ctrl_flags[pid]) then
--      xpg_close_window(pid)
        gHide(pid)
    end if
    return c_func(xCallWindowProc, {wnd_proc_addr[id], hwnd, msg, wParam, lParam})
end function
local constant WINAPI_SUBPROC_CB = call_back(xpg_WinAPI_sub_proc)

local procedure xpg_WinAPI_sub_class_control(gdx id, xpg_handle hwnd) -- (nee subClassControl)
    integer ctrl_type = ctrl_types[id] -- certain control types do not get subclassed:
--  if ctrl_type!=StaticBitmap and ctrl_type!=DIALOG 
--  and ctrl_type!=ReBarBand then--and ctrl_type!=HyperText then
    if ctrl_type!=DIALOG then
        assert(sub_proc_addr[id]==UNDEFINED)
        atom prev_wnd_proc = c_func(xSetWindowLong,{hwnd,GWL_WNDPROC,WINAPI_SUBPROC_CB})
        wnd_proc_addr[id] = prev_wnd_proc
        sub_proc_addr[id] = WINAPI_SUBPROC_CB
    end if
end procedure

local procedure xpg_WinAPI_unsub_class_control(integer id) -- (nee unsubClassControl)
    if sub_proc_addr[id]!=UNDEFINED then
        atom PrevWndProc = wnd_proc_addr[id]
        {} = c_func(xSetWindowLong,{ctrl_handles[id],GWL_WNDPROC,PrevWndProc})
        sub_proc_addr[id] = UNDEFINED
    end if
end procedure

local function xpg_WinAPI_WndProc(atom hwnd, msg, wParam, lParam)
    bool bAssumed = false
    integer id = xpg_getID(hwnd)
    if id=0 then
        -- assume betwixt xCreateWindowEx() and xpg_setID()...
        --  (whereas any subclassing occurs after """ )
        bAssumed = true
        id = assume_id
        if assumed_hwnd=NULL then
            assumed_hwnd = hwnd
        else
            assert(assumed_hwnd=hwnd)
        end if
    end if

--      case WM_ACTIVATE:
--          if bAssumed then return 0 end if
--          return c_func(xDefWindowProc,{hwnd,iMsg,wParam,lParam})

--      case WM_CREATE:
--          atom wav_file = allocate_string(`\Windows\Media\tada.wav`)
--          c_proc(xPlaySound,{wav_file,NULL,SND_FILEASYNC})
--          free(wav_file)

--  if msg=WM_CHAR and wParam=VK_ESC then
--      xpg_close_window(PrimaryWindowID)
--      gHide(PrimaryWindowID)
--/*
        case WM_KEYDOWN:
            if wParam=VK_ESC and and_bits(CF_CLOSE_ON_ESC,ctrl_flags[id]) then
                {} = c_func(xSendMessage,{hwnd,WM_CLOSE,0,0})
            end if
--*/

    if msg=WM_COMMAND then
        if lParam then -- (see win00inglod.exw for more...)
            id = xpg_getID(lParam)
        end if
        if id then
            integer ct = ctrl_types[id]
--?{"WM_COMMAND",id,ct,ctrl_names[ct]}
            if ct=BUTTON then
--?"BUTTON"
                integer click = getd({id,"CLICK"},handlers)
--?action
                if click then
                    integer res = click(id)
                    if res=XPG_IGNORE then
                        return 0
                    elsif res=XPG_CLOSE then
--                      xpg_close_window(xpg_get_parent_window(id))
                        gHide(xpg_get_parent_window(id))
                        return 0
                    end if
                end if
            end if
        end if
    elsif msg=WM_KEYDOWN then
--DEV have we any "KEY" handler??
        if wParam=VK_TAB then
            integer fid = xpg_get_focus()
            if fid=0 then fid = xpg_get_parent_window(id) end if
            if fid then
                integer window = xpg_get_parent_window(fid),
                         child = children_ids[window][1]
                xpg_set_focus(child)
                return 0
            end if
        elsif wParam=VK_ESC then
            if not bAssumed
            and id=xpg_get_parent_window(id)
            and and_bits(CF_CLOSE_ON_ESC,ctrl_flags[id]) then
--              xpg_close_window(PrimaryWindowID)
--              xpg_close_window(id)
                gHide(id)
--              {} = c_func(xSendMessage,{hwnd,WM_CLOSE,0,0})
            end if
            
        end if

    elsif msg=WM_NOTIFY then

        -- get handle & id of the control
        hwnd = get_struct_field(idNMTREEVIEW,lParam,"hdr.hwndFrom")
        id = xpg_getID(hwnd)

        if id and ctrl_types[id]=TREEVIEW then
            --NB: code had to be made INT, not UNIT for this to work:
            wParam = get_struct_field(idNMHDR,lParam,"code")
            if wParam=TVN_GETDISPINFO then
                atom tvItem = lParam + get_struct_size(idNMHDR),
                     tvmask = get_struct_field(idTVITEM,tvItem,"mask")
                integer treeIdx = get_struct_field(idTVITEM,tvItem,"lParam")
                string tvtext = tree_items[treeIdx][tText]
                if and_bits(tvmask,TVIF_TEXT) then
                    set_struct_field(idTVITEM,tvItem,"pszText",xpg_raw_string_ptr(tvtext))
                end if
                if and_bits(tvmask,TVIF_SELECTEDIMAGE) then
                    integer tvstate = get_struct_field(idTVITEM,tvItem,"state"),
                              tvimg = iff(and_bits(tvstate,TVIS_EXPANDED)?1:0)
                    set_struct_field(idTVITEM,tvItem,"iSelectedImage",tvimg)
                end if
            elsif wParam=TVN_ITEMEXPANDED then
                integer state = get_struct_field(idNMTREEVIEW,lParam,"itemNew.state")
                bool expanded = and_bits(state,TVIS_EXPANDED)
                if expanded then
                    integer branchopen = getd({id,"BRANCHOPEN"},handlers)
                    if branchopen then
                        -- aside: the argument to branchopen() is whatever the Windows branches of 
                        -- gTreeGetUserId()/gTreeDeleteChildren()/gTreeAddNodes(), as defined in 
                        -- this very source file, actually need. (backend-specific)
                        integer treeIdx = get_struct_field(idNMTREEVIEW,lParam,"itemNew.lParam")
                        branchopen({hwnd,treeIdx})
                    end if
                end if
            end if
        end if

--DEV tryme:
--  elsif id=TREE1 and (msg=WM_LBUTTONDBLCLK or
--                     (msg=WM_CHAR and wParam=VK_RETURN)) then
--/*
    elsif (msg=WM_CHAR and id=TREE1 and wParam=VK_RETURN)
       or (msg=WM_LBUTTONDBLCLK and id=TREE1) then
--DEV SELECTED_CB??
        treeIdx = getIndex(TREE1)
        if treeIdx and tree_items[treeIdx][tLoaded]=-1 then -- a leaf node
--          void = messageBox("Selected",getTreeText(treeIdx,1),0)
--          ?{"Selected",getTreeText(treeIdx,1)}
--  --      xpg_close_window(DEMO)
--          gHide(DEMO)
--          xpg_set_focus(TREE1)
        else
            void = sendMessage(TREE1,TVM_EXPAND,TVE_TOGGLE,tree_items[treeIdx][tHandle])
        end if
        return {0}
--*/

--/*
    elsif msg=WM_PAINT then
            atom hdc = c_func(xBeginPaint,{hwnd,pPAINTSTRUCT})
--               Phix = allocate_string("A Plain Vanilla Window using Phix!")
            string Phix = "A Plain Vanilla Window using Phix!"
--DEV... (need to check what arwen does...)
            bool res = c_func(xGetClientRect,{hwnd,pRECT})
            assert(res!=0,"GetClientRect failure")
            integer l = length(Phix),
            h = c_func(xDrawText,{hdc,Phix,l,pRECT,DT_SINGLECENTER})
            c_proc(xEndPaint,{hwnd,pPAINTSTRUCT})
--*/

    elsif msg=WM_SIZE then

        --DEV if expand... and all children, obvs.
        if length(children_ids[id]) then
            integer {x,y,w,h} = xpg_get_client_rect(id),
                         tree = children_ids[id][1]
            if ctrl_types[tree]=TREEVIEW then
                c_proc(xMoveWindow, {ctrl_handles[tree], x+10, y+10, w-20, h-20, 1})
            end if
        end if

    elsif msg=WM_CLOSE then

        if id=PrimaryWindowID then
            c_proc(xDestroyWindow,{hwnd})
        else
            c_proc(xShowWindow,{hwnd,SW_HIDE})
            assert(ctrl_handles[id]=hwnd)
            id = parent_ids[id]
            if id then
                c_proc(xSetFocus,{ctrl_handles[id]})
            end if              
--ARWEN:
--          if ctrl_types[id]=Window
--          and parent_ids[id] then
--              hwnd = getWindowHwnd(parent_ids[id])
--          else
--              hwnd = getWindowHwnd(id)
--          end if
--          if hwnd!=NULL then c_proc(xSetFocus,{hwnd}) end if
--          setVisible(id,false)
        end if
        return 0

    -- MUST handle this message
    elsif msg=WM_DESTROY and id=PrimaryWindowID then
        c_proc(xPostQuitMessage, {0})
        -- unsubclass all subclassed controls
        for obj_id=1 to length(ctrl_types) do
            xpg_WinAPI_unsub_class_control(obj_id)
        end for
        return  0

    elsif msg=WM_SETFOCUS then
        -- focus on the saved item rather than the window
        -- (for task switching, eg Alt-Tab)
        if ctrl_types[id]=DIALOG then
            integer last_focus = last_focus_id[id]
            if last_focus!=UNDEFINED
            and ctrl_types[last_focus]!=DIALOG then
                c_proc(xSetFocus, {ctrl_handles[last_focus]})
                return 0
            end if
        end if
    end if
    return c_func(xDefWindowProc, {hwnd, msg, wParam, lParam})
end function

local integer xpg_create_image_list -- see xpg_xpm.e
local atom closed_folder, open_folder, dot  -- for GTK
local atom tree_himl                        -- for WinAPI

local function xpg_xpm_callback(object xpm)
    -- low-level operations for xpg_xpm.e (avoids making anything global)
    if bUseGTK then
        return c_func(gdk_pixbuf_new_from_xpm_data,{xpg_word_array(xpm)})
    else
        string what = xpm[1]
        if what="NEWLIST" then
            -- create imagelist using the recommended sizes for small icons:
            return c_func(xImageList_Create,{c_func(xGetSystemMetrics,{SM_CXSMICON}),
                                             c_func(xGetSystemMetrics,{SM_CYSMICON}),
                                             ILC_COLOR8+ILC_MASK,1,32})
        elsif what="NEWDIB" then    
            atom {mem, hdrSize} = xpm[2],
                 hdc = c_func(xGetDC,{0}),      -- Get the screen's device context.
                hDIB = c_func(xCreateDIBitmap, {hdc,                -- create DDB for/compatible with this
                                                mem,                -- info about the DIB to create, eg h*w
                                                CBM_INIT,           -- int it please with...
                                                mem+hdrSize,        --      this bitmap,
                                                mem,                --      which has this structure
                                                DIB_RGB_COLORS})    --      and has explicit RGB values
            assert(hDIB!=NULL)
            bool bOK = c_func(xReleaseDC, {0, hdc})
            assert(bOK)
            return hDIB
        elsif what="ADDICON" then
            atom tree = xpm[2], icon = xpm[3]
            c_proc(xImageList_Add,{tree,icon,NULL})
            c_proc(xDeleteObject,{icon})
            return true -- (ignored)
        else
            ?9/0
        end if
    end if
end function

local procedure xpg_Init()
    bInit = true
    handlers = new_dict()   -- key is {gdx,name}, data is integer routine_id
    bool L = platform()=LINUX
    if bUseGTK then
        string gdk = iff(L?"libgdk-3.so.0"
                          :"libgdk-3-0.dll"), -- (win32bit replaced below)
               gtk = iff(L?"libgtk-3.so.0"
                          :"libgtk-3-0.dll"), -- (win32bit replaced below)
               gto = iff(L?"libgobject-2.0.so.0"
                          :"libgobject-2.0-0.dll"),
               gtp = iff(L?"libgdk_pixbuf-2.0.so.0"
                          :"libgdk_pixbuf-2.0-0.dll"),
        initial_dir = current_dir()
        if platform()=WINDOWS then
            if M=32 then
                gdk = "libgdk-win32-2.0-0.dll"
                gtk = "libgtk-win32-2.0-0.dll"
            end if
            -- NB not formally supported [aka gUseGTK() *has been* invoked].
            string dll_dir = sprintf(`%s\win_gtk%d`,{include_path("xpGUI"),M})
            assert(chdir(dll_dir),"gUseGTK()???")   
        end if
        atom GTKLIB = open_dll(gtk),
             GDKLIB = open_dll(gdk),
             GTKGDO = open_dll(gto),
             GDKGTP = open_dll(gtp)
        if platform()=WINDOWS then
            assert(chdir(initial_dir))
        end if
        integer gtk_init_check = define_c_func(GTKLIB,"gtk_init_check",
            {C_PTR,     --  int* argc
             C_PTR},    --  char*** argv
            C_INT)      -- gboolean
        if c_func(gtk_init_check,{0,0})=0 then  
            crash("Failed to initialize GTK library!")  
        end if
        if M=32 then -- (and/or <= gtk 2.0)
            gdk_screen_get_width = define_c_func(GDKLIB,"gdk_screen_get_width",
                {},         --  void
                C_INT)      -- gint
            gdk_screen_get_height = define_c_func(GDKLIB,"gdk_screen_get_height",
                {},         --  void
                C_INT)      -- gint
        else
            gdk_display_get_default = define_c_func(GDKLIB,"gdk_display_get_default",
                {},         --  void
                C_PTR)      -- GdkDisplay*
            gdk_display_get_monitor = define_c_func(GDKLIB,"gdk_display_get_monitor",
                {C_PTR,     --  GdkDisplay*
                 C_INT},    --  int monitor_num
                C_PTR)      -- GdkMonitor*
            gdk_monitor_get_geometry = define_c_proc(GDKLIB,"gdk_monitor_get_geometry",
                {C_PTR,     --  GdkMonitor* monitor,
                 C_PTR})    --  GdkRectangle* geometry
        end if
        gdk_pixbuf_get_type = define_c_func(GDKGTP,"gdk_pixbuf_get_type",
            {},         --  void
            C_INT)      -- GType
        gdk_pixbuf_new_from_xpm_data = define_c_func(GDKGTP,"gdk_pixbuf_new_from_xpm_data",
            {C_PTR},    --  const char** data
            C_PTR)      -- GdkPixbuf*
        gdk_screen_get_default = define_c_func(GDKLIB,"gdk_screen_get_default",
            {},         --  void
            C_PTR)      -- GdkScreen*
--GdkWindow*
--          gdk_screen_get_root_window = define_c_func(GDKLIB,"gdk_screen_get_root_window",
--  GdkScreen* screen
--)
--      gdk_window_get_geometry = define_c_proc(GDKLIB,"gdk_window_get_geometry",
--          {C_PTR,     --  GdkWindow* window
--           C_PTR,     --  gint* x
--           C_PTR,     --  gint* y
--           C_PTR,     --  gint* width
--           C_PTR,     --  gint* height
--           C_PTR})    --  gint* depth
--      gdk_window_get_height = define_c_func(GDKLIB,"gdk_window_get_height",
--          {C_PTR},    --  GdkWindow *window
--          C_INT)      -- int
--      gdk_window_get_width = define_c_func(GDKLIB,"gdk_window_get_width",
--          {C_PTR},    --  GdkWindow *window
--          C_INT)      -- int
--      gdk_window_get_position = define_c_proc(GDKLIB,"gdk_window_get_position",
--          {C_PTR,     --  GdkWindow *window
--           C_PTR,     --  gint *x
--           C_PTR})    --  gint *y
--      gdk_window_move_resize = define_c_proc(GDKLIB,"gdk_window_move_resize",
--          {C_PTR,     --  GdkWindow* window
--           C_INT,     --  gint x
--           C_INT,     --  gint y
--           C_INT,     --  gint width
--           C_INT})    --  gint height
        gtk_box_pack_start = define_c_proc(GTKLIB,"gtk_box_pack_start",
            {C_PTR,     --  GtkBox* box
             C_PTR,     --  GtkWidget* child
             C_INT,     --  gboolean expand
             C_INT,     --  gboolean fill
             C_INT})    --  guint padding
        gtk_button_new_with_mnemonic = define_c_func(GTKLIB,"gtk_button_new_with_mnemonic",
            {C_PTR},    --  const gchar* label
            C_PTR)      -- GtkWidget*
        gtk_cell_renderer_pixbuf_new = define_c_func(GTKLIB,"gtk_cell_renderer_pixbuf_new",
            {},         --  void
            C_PTR)      -- GtkCellRenderer*
        gtk_cell_renderer_text_new = define_c_func(GTKLIB,"gtk_cell_renderer_text_new",
            {},         --  void
            C_PTR)      -- GtkCellRenderer*
        gtk_container_add = define_c_proc(GTKLIB,"gtk_container_add",
            {C_PTR,     --  GtkContainer* container
             C_PTR})    --  GtkTreeIter* parent
        gtk_css_provider_new = define_c_func(GTKLIB,"gtk_css_provider_new",
            {},         --  void
            C_PTR,      -- GtkCssProvider*
            false)      -- bCrash (3.0+ only)
--?gtk_css_provider_new (-1 on 2.0)
        gtk_css_provider_load_from_data = define_c_proc(GTKLIB,"gtk_css_provider_load_from_data",
            {C_PTR,     --  GtkCssProvider* css_provider
             C_PTR,     --  const gchar* data
             C_INT,     --  gssize length (-1)
             C_INT},    --  GError** error (NULL)
--          C_INT)      -- gboolean (unreliable anyway, it will either work or not)
            false)      -- bCrash (3.0+ only)
        gtk_drawing_area_new = define_c_func(GTKLIB,"gtk_drawing_area_new",
            {},         --  void
            C_PTR)      -- GtkWidget*
        gtk_hbox_new = define_c_func(GTKLIB,"gtk_hbox_new",
            {C_INT,     --  gboolean homogeneous
             C_INT},    --  gint spacing
            C_PTR)      -- GtkWidget*
--?gtk_css_provider_load_from_data ("")
--      gtk_list_store_newv = define_c_func(GTKLIB,"gtk_list_store_newv",
--          {C_INT,     --  gint n_columns
--           C_PTR},    --  GType* types
--          C_PTR)      -- GtkListStore*
        gtk_list_store_append = define_c_proc(GTKLIB,"gtk_list_store_append",
            {C_PTR,     --  GtkListStore* list_store
             C_PTR})    --  GtkTreeIter* iter
--DEV suspect, I think...
--      gtk_list_store_set_valuesv = define_c_proc(GTKLIB,"gtk_list_store_set_valuesv",
--          {C_PTR,     --  GtkListStore* list_store
--           C_PTR,     --  GtkTreeIter* iter
--           C_PTR,     --  gint* columns
--           C_PTR,     --  GValue* values
--           C_INT})    --  gint n_values
        -- nb limited version, "..." replaced with column,value,-1
        gtk_list_store_set = define_c_proc(GTKLIB,"gtk_list_store_set",
            {C_PTR,     --  GtkListStore* list_store
             C_PTR,     --  GtkTreeIter* iter
        -- ...
             C_INT,     --  gint column
             C_PTR,     --  GValue* value
             C_INT})    --  -1 terminator
        gtk_main = define_c_proc(GTKLIB,"gtk_main",
            {})
        gtk_main_quit = define_c_proc(GTKLIB,"gtk_main_quit",
            {})
        gtk_scrolled_window_new = define_c_func(GTKLIB,"gtk_scrolled_window_new",
            {C_PTR,     --  GtkAdjustment* hadjustment (NULL)
             C_PTR},    --  GtkAdjustment* vadjustment (NULL)
            C_PTR)      -- GtkWidget*
        gtk_scrolled_window_set_policy = define_c_proc(GTKLIB,"gtk_scrolled_window_set_policy",
            {C_PTR,     --  GtkScrolledWindow* scrolled_window
             C_PTR,     --  GtkPolicyType hscrollbar_policy
             C_PTR})    --  GtkPolicyType vscrollbar_policy
        gtk_style_context_add_provider_for_screen = define_c_proc(GTKLIB,"gtk_style_context_add_provider_for_screen",
            {C_PTR,     --  GdkScreen* screen
             C_PTR,     --  GtkCssProvider* css_provider
             C_INT},    --  guint priority
            false)      -- bCrash (3.0+ only)
--      gtk_tree_iter_copy = define_c_func(GTKLIB,"gtk_tree_iter_copy",
--          {C_PTR},    --  GtkTreeIter* iter
--          C_PTR)      -- GtkTreeIter*
--      gtk_tree_iter_free = define_c_proc(GTKLIB,"gtk_tree_iter_free",
--          {C_PTR})    --  GValue* value
        -- nb limited version, "..." replaced with column,ptr,-1
        gtk_tree_model_get = define_c_proc(GTKLIB,"gtk_tree_model_get",
            {C_PTR,     --  GtkTreeModel* tree_model
             C_PTR,     --  GtkTreeIter* iter
            -- ...
             C_INT,     --  int column
             C_PTR,     --  void* data
             C_INT})    --  -1 terminator
        gtk_tree_model_get_iter = define_c_func(GTKLIB,"gtk_tree_model_get_iter",
            {C_PTR,     --  GtkTreeModel* tree_model
             C_PTR,     --  GtkTreeIter* iter
             C_PTR},    --  GtkTreePath* path
            C_INT)      -- gboolean
        gtk_tree_model_get_path = define_c_func(GTKLIB,"gtk_tree_model_get_path",
            {C_PTR,     --  GtkTreeModel *tree_model
             C_PTR},    --  GtkTreeIter *iter
            C_PTR)      -- GtkTreePath*
        gtk_tree_model_get_string_from_iter = define_c_func(GTKLIB,"gtk_tree_model_get_string_from_iter",
            {C_PTR,     --  GtkTreeModel *tree_model
             C_PTR},    --  GtkTreeIter *iter
            C_PTR)      -- GtkTreePath*
--      gtk_tree_model_get_value = define_c_proc(GTKLIB,"gtk_tree_model_get_value",
--          {C_PTR,     --  GtkTreeModel* tree_model
--           C_PTR,     --  GtkTreeIter* iter
--           C_INT,     --  gint column
--           C_PTR})    --  GValue* value
        gtk_tree_model_iter_children = define_c_func(GTKLIB,"gtk_tree_model_iter_children",
            {C_PTR,     --  GtkTreeModel* tree_model
             C_PTR,     --  GtkTreeIter* iter
             C_PTR},    --  GtkTreeIter* parent
            C_INT)      -- gboolean
        gtk_tree_path_free = define_c_proc(GTKLIB,"gtk_tree_path_free",
            {C_PTR})    --  GtkTreePath** path
--(fine, should you actually need it)
--      gtk_tree_path_new_from_string = define_c_func(GTKLIB,"gtk_tree_path_new_from_string",
--          {C_PTR},    --  const gchar* path
--          C_PTR)      -- GtkTreePath*
--(not sure, not needed/used anyway)
--      gtk_tree_selection_get_selected = define_c_func(GTKLIB,"gtk_tree_selection_get_selected",
--          {C_PTR,     --  GtkTreeSelection* selection
--           C_PTR,     --  GtkTreeModel** model
--           C_PTR},    --  GtkTreeIter* iter
--          C_INT)      -- gboolean
        gtk_tree_selection_set_mode = define_c_proc(GTKLIB,"gtk_tree_selection_set_mode",
            {C_PTR,     --  GtkTreeSelection* selection
             C_INT})    --  GtkSelectionMode type
--(fine, but didn't do what I needed)
--      gtk_tree_selection_set_select_function = define_c_proc(GTKLIB,"gtk_tree_selection_set_select_function",
--          {C_PTR,     --  GtkTreeSelection* selection
--           C_PTR,     --  GtkTreeSelectionFunc func
--           C_PTR,     --  gpointer data (NULL)
--           C_PTR})    --  GDestroyNotify destroy (NULL)
        gtk_tree_store_append = define_c_proc(GTKLIB,"gtk_tree_store_append",
            {C_PTR,     --  GtkTreeStore* tree_store
             C_PTR,     --  GtkTreeIter* iter
             C_PTR})    --  GtkTreeIter* parent
        gtk_tree_store_clear = define_c_proc(GTKLIB,"gtk_tree_store_append",
            {C_PTR})    --  GtkTreeStore* tree_store
        gtk_tree_store_newv = define_c_func(GTKLIB,"gtk_tree_store_newv",
            {C_INT,     --  gint n_columns
             C_PTR},    --  GType* types
            C_PTR)      -- GtkTreeStore*
        gtk_tree_store_remove = define_c_func(GTKLIB,"gtk_tree_store_remove",
            {C_PTR,     --  GtkTreeStore* tree_store
             C_PTR},    --  GtkTreeIter* iter
            C_INT)      -- gboolean
        -- nb limited version, "..." replaced with column,value,-1
        gtk_tree_store_set = define_c_proc(GTKLIB,"gtk_tree_store_set",
            {C_PTR,     --  GtkTreeStore* tree_store
             C_PTR,     --  GtkTreeIter* iter
        -- ...
             C_INT,     --  gint column
             C_PTR,     --  GValue* value
             C_INT})    --  -1 terminator
--      gtk_tree_store_set_valuesv = define_c_proc(GTKLIB,"gtk_tree_store_set_valuesv",
--          {C_PTR,     --  GtkTreeStore* tree_store
--           C_PTR,     --  GtkTreeIter* iter
--           C_PTR,     --  gint* columns
--           C_PTR,     --  GValue* values
--           C_INT})    --  gint n_values
        gtk_tree_view_column_new = define_c_func(GTKLIB,"gtk_tree_view_column_new",
            {},         --  void
            C_PTR)      -- GtkTreeViewColumn*
        gtk_tree_view_column_pack_start = define_c_proc(GTKLIB,"gtk_tree_view_column_pack_start",
            {C_PTR,     --  GtkTreeViewColumn* tree_column
             C_PTR,     --  GtkCellRenderer* cell
             C_INT})    --  gboolean expand
        gtk_tree_view_column_add_attribute = define_c_proc(GTKLIB,"gtk_tree_view_column_add_attribute",
            {C_PTR,     --  GtkTreeViewColumn* tree_column
             C_PTR,     --  GtkCellRenderer* cell_renderer
             C_PTR,     --  const gchar* attribute
             C_INT})    --  gint column
        gtk_tree_view_collapse_row = define_c_proc(GTKLIB,"gtk_tree_view_collapse_row",
            {C_PTR,     --  GtkTreeView* tree_view
             C_PTR})    --  GtkTreePath* path
--          C_INT)      -- gboolean
        gtk_tree_view_expand_row = define_c_proc(GTKLIB,"gtk_tree_view_expand_row",
            {C_PTR,     --  GtkTreeView* tree_view
             C_PTR,     --  GtkTreePath* path
             C_INT})    --  gboolean open_all (children, recursively: false==just immediate children)
--          C_INT)      -- gboolean
        gtk_tree_view_expand_to_path = define_c_proc(GTKLIB,"gtk_tree_view_expand_to_path",
            {C_PTR,     --  GtkTreeView* tree_view
             C_PTR})    --  GtkTreePath* path
        gtk_tree_view_insert_column = define_c_proc(GTKLIB,"gtk_tree_view_insert_column",
            {C_PTR,     --  GtkTreeView* tree_view
             C_PTR,     --  GtkTreeViewColumn* column
             C_INT})    --  gint position
--          C_PTR)      -- gint (the number of columns after insertion)
        -- nb limited version, "..." replaced with title,column,NULL (decided against)
--      gtk_tree_view_insert_column_with_attributes = define_c_func(GTKLIB,"gtk_tree_view_insert_column_with_attributes",
--          {C_PTR,     --  GtkTreeView* tree_view
--           C_INT,     --  gint position
--           C_PTR,     --  const gchar* title
--           C_PTR,     --  GtkCellRenderer* cell
--          -- ...
--           C_PTR,     --  char* title
--           C_INT,     --  int column
--           C_PTR},    --  NULL
--          C_INT)      -- gint
        gtk_tree_view_get_cursor = define_c_proc(GTKLIB,"gtk_tree_view_get_cursor",
            {C_PTR,     --  GtkTreeView* tree_view
             C_PTR,     --  GtkTreePath** path
             C_PTR})    --  GtkTreeViewColumn** focus_column (NULL)
        gtk_tree_view_get_model = define_c_func(GTKLIB,"gtk_tree_view_get_model",
            {C_PTR},    --  GtkTreeView* tree_view
            C_PTR)      -- GtkTreeModel*
        gtk_tree_view_get_selection = define_c_func(GTKLIB,"gtk_tree_view_get_selection",
            {C_PTR},    --  GtkTreeView* tree_view
            C_PTR)      -- GtkTreeSelection*
        gtk_tree_view_new = define_c_func(GTKLIB,"gtk_tree_view_new",
            {},         --  void
            C_PTR)      -- GtkWidget*
        gtk_tree_view_set_enable_search = define_c_proc(GTKLIB,"gtk_tree_view_set_enable_search",
            {C_PTR,     --  GtkTreeView* tree_view
             C_INT})    --  gboolean enable_search
        gtk_tree_view_set_enable_tree_lines = define_c_proc(GTKLIB,"gtk_tree_view_set_enable_tree_lines",
            {C_PTR,     --  GtkTreeView* tree_view
             C_INT})    --  gboolean enabled
--      gtk_tree_view_set_grid_lines = define_c_proc(GTKLIB,"gtk_tree_view_set_grid_lines",
--          {C_PTR,     --  GtkTreeView* tree_view
--           C_INT})    --  GtkTreeViewGridLines grid_lines
        gtk_tree_view_set_headers_visible = define_c_proc(GTKLIB,"gtk_tree_view_set_headers_visible",
            {C_PTR,     --  GtkTreeView* tree_view
             C_INT})    --  gboolean headers_visible
        gtk_tree_view_set_model = define_c_proc(GTKLIB,"gtk_tree_view_set_model",
            {C_PTR,     --  GtkTreeView* tree_view
             C_PTR})    --  GtkTreeModel* model
--      gtk_tree_view_set_search_column = define_c_proc(GTKLIB,"gtk_tree_view_set_search_column",
--          {C_PTR,     --  GtkTreeView* tree_view
--           C_INT})    --  gint column
        gtk_vbox_new = define_c_func(GTKLIB,"gtk_vbox_new",
            {C_INT,     --  gboolean homogeneous
             C_INT},    --  gint spacing
            C_PTR)      -- GtkWidget*
--      gtk_widget_get_root_window = define_c_func(GTKLIB,"gtk_widget_get_root_window",
--          {C_PTR},    --  GtkWidget* widget
--          C_PTR)      -- GdkWindow*
        gtk_widget_hide = define_c_proc(GTKLIB,"gtk_widget_hide",
            {C_PTR})    --  GtkWindow* window,  // aka handle
        gtk_widget_set_events = define_c_proc(GTKLIB,"gtk_widget_set_events",
            {C_PTR,     --  GtkWidget* widget
             C_INT})    --  gint events
        gtk_window_move = define_c_proc(GTKLIB,"gtk_window_move",
            {C_PTR,     --  GtkWindow* window
             C_INT,     --  gint x
             C_INT})    --  gint y
        gtk_window_new = define_c_func(GTKLIB,"gtk_window_new",
            {C_INT},    --  GtkWindowType type // usually GTK_WINDOW_TOPLEVEL (nb gone in GTK4)
            C_PTR)      -- GtkWidget* // handle
--      gtk_widget_show = define_c_proc(GTKLIB,"gtk_widget_show",
--          {C_PTR})    --  GtkWindow* window,  // aka handle
        gtk_widget_show_all = define_c_proc(GTKLIB,"gtk_widget_show_all",
            {C_PTR})    --  GtkWindow* window,  // aka handle
        gtk_window_set_default_size = define_c_proc(GTKLIB,"gtk_window_set_default_size",
            {C_PTR,     --  GtkWindow* window,  // aka handle
             C_INT,     --  gint width,
             C_INT})    --  gint height
        gtk_window_set_title = define_c_proc(GTKLIB,"gtk_window_set_title",
            {C_PTR,     --  GtkWindow* window,  // aka handle
             C_PTR})    --  const gchar* title  // a string
        gtk_window_set_transient_for = define_c_proc(GTKLIB,"gtk_window_set_transient_for",
            {C_PTR,     --  GtkWindow* window,  // aka handle
             C_PTR})    --  GtkWindow* parent   // ""
        gtk_window_get_transient_for = define_c_func(GTKLIB,"gtk_window_get_transient_for",
            {C_PTR},    --  GtkWindow* window,  // aka handle
             C_PTR)     --  GtkWindow* parent
        gtk_window_get_position = define_c_proc(GTKLIB,"gtk_window_get_position",
            {C_PTR,     --  GtkWindow *window
             C_PTR,     --  gint* root_x
             C_PTR})    --  gint* root_y
        gtk_window_get_size = define_c_proc(GTKLIB,"gtk_window_get_size",
            {C_PTR,     --  GtkWindow* window
             C_PTR,     --  gint* width
             C_PTR})    --  gint* height
--      g_signal_connect_data = define_c_func(GTKGDO,"g_signal_connect_data",
        g_signal_connect_data = define_c_proc(GTKGDO,"g_signal_connect_data",
--      g_signal_connect_data = define_c_func(GTKGDO,"+g_signal_connect_data", -- no help...
            {C_PTR,     --  GObject* instance,              // aka handle
             C_PTR,     --  const gchar* detailed_signal,   // a string
             C_PTR,     --  GCallback c_handler,            // a callback
             C_PTR,     --  gpointer data,                  // data for ""
             C_PTR,     --  GClosureNotify destroy_data,    // (NULL here)
             C_INT})    --  GConnectFlags connect_flags     //     ""
--          C_INT)      -- gulong // handler id (>0 for success)
        g_object_set_property = define_c_proc(GTKGDO,"g_object_set_property",
            {C_PTR,     --  GObject* object
             C_PTR,     --  const gchar* property_name
             C_PTR})    --  const GValue* value
        g_object_get_property = define_c_proc(GTKGDO,"g_object_get_property",
            {C_PTR,     --  GObject* object
             C_PTR,     --  const gchar* property_name
             C_PTR})    --  GValue* value
--      g_value_get_int = define_c_func(GTKGDO,"g_value_get_int",
--          {C_PTR},    --  const GValue* value
--          C_INT)      -- gint
--      g_value_init = define_c_func(GTKGDO,"g_value_init",
--          {C_PTR,     --  GValue* value
--           C_INT},    --  GType g_type
--          C_PTR)      -- GValue*
--      g_value_unset = define_c_proc(GTKGDO,"g_value_unset",
--          {C_PTR})    --  GValue* value
        xg_object_unref = define_c_proc(GTKGDO,"g_object_unref",{C_PTR})
        idGdkRectangle = define_struct("""typedef struct GdkRectangle {
                                          int x;
                                          int y;
                                          int width;
                                          int height;
                                        }""")
-- NB: cffi.e updated to my best guess for these...
-- (this works on 64 bit, the one below didn't [some mods to cffi.e were commented back out])
-- (source: https://api.gtkd.org/gdk.c.types.GdkEventKey.html )
--      idGdkEventKey = define_struct("""typedef struct GdkEventKey {
        string tGdkEventKey = """typedef struct GdkEventKey {
                                          GdkEventType ty"""&"""pe;
                                          GdkWindow* window;
                                          byte sendEvent;
                                          uint time;
                                          ModifierType state;
                                          uint keyval;
                                          int length;
                                          char* string_;
                                          ushort hardwareKeycode;
                                          ubyte group;
                                         }"""
--                                       }""")
        idGdkEventKey = define_struct(tGdkEventKey)
--  tGdkEventKey = """
--  typedef struct GdkEventKey {
--    GdkEventType type;
--    GdkWindow* window;
--    gint8 send_event;
--    guint32 time;
--    GdkModifierType* state;
--    guint keyval;
--    gint length;
--    gchar* string;
--    guint16 hardware_keycode;
--    guint8 group;
--    guint is_modifier;
--  }
--  """
--  guint is_modifier : 1;
--64 bit:
--{"GdkEventKey",56,8,{{"type","window","send_event","time","state","keyval","length","string","hardware_keycode","group","is_modifier"},
--{{"GdkEventType",4,0,1},{"ptr",8,8,1},{"gint8",1,16,1},{"guint32",4,20,0},{"ptr",8,24,1},{"guint",4,32,0},{"gint",4,36,1},{"ptr",8,40,1},
-- {"guint16",2,48,0},{"guint8",1,50,0},{"guint",4,52,0}}},{}}
--32 bit:
--{"GdkEventKey",40,4,{{"type","window","send_event","time","state","keyval","length","string","hardware_keycode","group","is_modifier"},
--{{"GdkEventType",4,0,1},{"ptr",4,4,1},{"gint8",1,8,1},{"guint32",4,12,0},{"ptr",4,16,1},{"guint",4,20,0},{"gint",4,24,1},{"ptr",4,28,1},
-- {"guint16",2,32,0},{"guint8",1,34,0},{"guint",4,36,0}}},{}}
--working:
--{"GdkEventKey",36,4,{{"type","window","sendEvent","time","state","keyval","length","string_","hardwareKeycode","group"},
--{{"GdkEventType",4,0,1},{"ptr",4,4,1},{"byte",1,8,1},{"uint",4,12,0},{"ModifierType",4,16,1},{"uint",4,20,0},{"int",4,24,1},{"ptr",4,28,1},
-- {"ushort",2,32,0},{"ubyte",1,34,0}}},{}}

        ?define_struct(tGdkEventKey,bAdd:=0)
        pData = allocate(W)
        pRECT = allocate_struct(idGdkRectangle)
        GTK_ID_LOOKUP = new_dict() -- GTK only (key:handle, data:id)
        GDK_TYPE_PIXBUF = c_func(gdk_pixbuf_get_type,{})
        {closed_folder, open_folder, dot} = xpg_create_image_list(bUseGTK,xpg_xpm_callback)
    else
        atom COMCTL32 = open_dll("comctl32.dll"),
             GDI32 = open_dll("gdi32.dll"),
--           WINMM = open_dll("winmm.dll"),
             USER32 = open_dll("user32.dll"),
             KERNEL32 = open_dll("kernel32.dll")
        xBeginPaint = define_c_func(USER32,"BeginPaint",
            {C_PTR,     --  HWND  hwnd              // handle of window
             C_PTR},    --  LPPAINTSTRUCT  lpPaint  // address of structure for paint information
            C_PTR)      -- HDC
        xCallWindowProc = define_c_func(USER32,"CallWindowProcA",
            {C_PTR,     --  WNDPROC lpPrevWndFunc
             C_PTR,     --  HWND hWnd
             C_UINT,    --  UINT Msg
             C_UINT,    --  WPARAM wParam
             C_UINT},   --  LPARAM lParam
            C_PTR)      -- LRESULT
        xCreateDIBitmap = define_c_func(GDI32, "CreateDIBitmap",
            {C_PTR,     --  HDC hdc
             C_PTR,     --  const BITMAPINFOHEADER *lpbmih
             C_LONG,    --  DWORD fdwInit
             C_LONG,    --  const VOID *lpbInit
             C_PTR,     --  const BITMAPINFO *lpbmi
             C_LONG},   --  UINT fuUsage
            C_PTR)      -- HBITMAP
        xCreateWindowEx = define_c_func(USER32,"CreateWindowExA",
            {C_LONG,    --  DWORD  dwExStyle        // extended window style
             C_PTR,     --  LPCTSTR  lpClassName    // address of registered class name
             C_PTR,     --  LPCTSTR  lpWindowName   // address of window name
             C_LONG,    --  DWORD  dwStyle          // window style
             C_INT,     --  int  x              // horizontal position of window
             C_INT,     --  int  y              // vertical position of window
             C_INT,     --  int  nWidth         // window width
             C_INT,     --  int  nHeight        // window height
             C_PTR,     --  HWND  hWndParent    // handle of parent or owner window
             C_PTR,     --  HMENU  hMenu        // handle of menu or child-window identifier
             C_PTR,     --  HANDLE  hInstance   // handle of application instance
             C_PTR},    --  LPVOID  lpParam     // address of window-creation data
            C_PTR)      -- HWND
        xDefWindowProc = define_c_func(USER32,"DefWindowProcA",
            {C_PTR,     --  HWND  hWnd  // handle of window
             C_UINT,    --  UINT  Msg   // message identifier
             C_UINT,    --  WPARAM  wParam  // first message parameter
             C_UINT},   --  LPARAM  lParam  // second message parameter
            C_PTR)      -- LRESULT
        xDeleteObject = define_c_proc(GDI32, "DeleteObject",
            {C_PTR})    --  HGDIOBJ  hObject    // handle of graphic object
--          C_LONG)     -- BOOL
        xDestroyWindow = define_c_proc(USER32,"DestroyWindow",
            {C_PTR})    --  HWND  hWnd  // handle of window to destroy
--          C_LONG)     -- BOOL
        xDispatchMessage = define_c_proc(USER32,"DispatchMessageA",
            {C_PTR})    --  CONST MSG  * lpmsg  // address of structure with message
--          C_LONG)     -- LONG (generally ignored)
        xDrawText = define_c_func(USER32,"DrawTextA",
            {C_INT,     --  HDC  hDC            // handle of device context
             C_INT,     --  LPCTSTR  lpString   // address of string to draw
             C_INT,     --  int  nCount     // string length, in characters
             C_INT,     --  LPRECT  lpRect  // address of structure with formatting dimensions
             C_INT},    --  UINT  uFormat   // text-drawing flags
            C_INT)      -- int height of the text
        xEndPaint = define_c_proc(USER32,"EndPaint",
            {C_PTR,     --  HWND  hWnd                  // handle of window
             C_PTR})    -- CONST PAINTSTRUCT  *lpPaint  // address of structure for paint data
--          C_INT)      -- BOOL (function always returns true so linked as c_proc)
        xGetClientRect = define_c_func(USER32,"GetClientRect",
            {C_PTR,     --  HWND hWnd
             C_PTR},    --  LPRECT lpRect
            C_LONG)     -- BOOL
        xGetConsoleWindow = define_c_func(KERNEL32,"GetConsoleWindow",
            {},         --  (void)
            C_PTR)      -- HWND of the console window, or NULL.
        xGetDC = define_c_func(USER32,"GetDC",
            {C_PTR},    --  HWND  hWnd  // handle of window
            C_PTR)      -- HDC
        xGetFocus = define_c_func(USER32,"GetFocus",
            {},         --  (void)
            C_PTR)      -- HWND
        xGetForegroundWindow = define_c_func(USER32,"GetForegroundWindow",
            {},         --  (void)
            C_PTR)      -- HWND of the forground window.
        xGetLastError = define_c_func(KERNEL32,"GetLastError",
            {},         -- (void)
            C_INT)      -- DWORD
        xGetMessage = define_c_func(USER32,"GetMessageA",
            {C_PTR,     --  LPMSG  lpMsg    // address of structure with message
             C_PTR,     --  HWND  hWnd      // handle of window
             C_UINT,    --  UINT  wMsgFilterMin  // first message
             C_UINT},   --  UINT  wMsgFilterMax  // last message
             C_INT)     -- BOOL
        xGetStockObject = define_c_func(GDI32,"GetStockObject",
            {C_INT},    --  int  fnObject   // type of stock object
            C_PTR)      -- HGDIOBJ GetStockObject(
        xGetSystemMetrics = define_c_func(USER32,"GetSystemMetrics",
            {C_INT},    --  int nIndex
            C_INT)      -- int
        xGetTextExtentPoint32 = define_c_func(GDI32, "GetTextExtentPoint32A",
            {C_PTR,     --  HDC  hdc,   // handle of device context
             C_PTR,     --  LPCTSTR  lpString,  // address of text string
             C_INT,     --  int  cbString,  // number of characters in string
             C_PTR},    --  LPSIZE  lpSize  // address of structure for string size
            C_INT)      -- BOOL
        xGetWindowLong = define_c_func(USER32,iff(M=32?"GetWindowLongA"
                                                      :"GetWindowLongPtrA"),
            {C_PTR,     --  HWND  hWnd      // handle of window
             C_UINT},   --  int  nIndex     // offset of value to retrieve
            C_LONG)     -- LONG/LONG_PTR
        xGetWindowRect = define_c_func(USER32,"GetWindowRect",
            {C_PTR,     --  HWND hWnd
             C_PTR},    --  LPRECT lpRect
            C_INT)      -- BOOL
        xImageList_Add = define_c_proc(COMCTL32,"ImageList_Add",
            {C_PTR,     --  HIMAGELIST  himl,   // handle to the image list
             C_PTR,     --  HBITMAP  hbmImage,  // handle to the bitmap containing the image
             C_PTR})    --  HBITMAP  hbmMask    // handle to the bitmap containing the mask
--          C_INT)      -- int
        xImageList_Create = define_c_func(COMCTL32, "ImageList_Create",
            {C_INT,     --  int cx (Specifies the width, in pixels, of each image.)
             C_INT,     --  int cy (Specifies the height, in pixels, of each image.)
             C_UINT,    --  UINT  flags (ILC_xxx values, usually ILC_COLOR8)
             C_INT,     --  int  cInitial (Number of images that the image list initially contains.)
             C_INT},    --  int  cGrow (?Number of images to grow by when resized?)
            C_PTR)      -- HIMAGELIST (handle to the image list, NULL on failure)
        xLoadCursor = define_c_func(USER32,"LoadCursorA",
            {C_PTR,     --  HINSTANCE hInstance
             C_PTR},    --  LPCTSTR lpCursorName
            C_PTR)      -- HCURSOR
        xLoadIcon = define_c_func(USER32,"LoadIconA",
            {C_PTR,     --  HINSTANCE hInstance
             C_PTR},    --  LPCTSTR lpIconName
            C_PTR)      -- HICON
        xMoveWindow = define_c_proc(USER32,"MoveWindow",
            {C_PTR,     --  HWND hWnd
             C_INT,     --  int X
             C_INT,     --  int Y
             C_INT,     --  int nWidth
             C_INT,     --  int nHeight
             C_INT})    --  BOOL bRepaint
--          C_INT)      -- BOOL
--      xPlaySound = define_c_proc(WINMM,"PlaySound",
--  --  xPlaySound = define_c_func(WINMM,"PlaySoundA",
--          {C_PTR,     --  LPCTSTR pszSound
--           C_PTR,     --  HMODULE hmod
--           C_INT})    --  DWORD   fdwSound
--  --      C_INT)      -- BOOL
        xPostQuitMessage = define_c_proc(USER32,"PostQuitMessage",
            {C_INT})    --  int  nExitCode      // exit code
        xRegisterClassEx = define_c_func(USER32,"RegisterClassExA",
            {C_PTR},    --  CONST WNDCLASSEX FAR *lpwcx // address of structure with class data
            C_PTR)      -- ATOM
        xReleaseDC = define_c_func(USER32, "ReleaseDC",
            {C_PTR,     --  HWND  hwnd, // handle of window
             C_PTR},    --  HDC  hdc    // handle of device context
            C_INT)      -- BOOL
        xSendMessage = define_c_func(USER32,"SendMessageA",
            {C_PTR,     --  HWND  hwnd  // handle of destination window
             C_UINT,    --  UINT  uMsg  // message to send
             C_UINT,    --  WPARAM  wParam  // first message parameter
             C_UINT},   --  LPARAM  lParam  // second message parameter
            C_LONG)     -- LRESULT
        xSetFocus = define_c_proc(USER32,"SetFocus",
            {C_PTR})    --  HWND hWnd
--          C_PTR)      -- HWND (previous focus)
        xSetParent = define_c_func(USER32,"SetParent",
            {C_PTR,     --  HWND  hwndChild         // handle of window whose parent is changing
             C_PTR},    --  HWND  hwndNewParent     // handle of new parent window
            C_PTR)      -- HWND of the previous parent window.
        xSetWindowLong = define_c_func(USER32,iff(M=32?"SetWindowLongA"
                                                      :"SetWindowLongPtrA"),
            {C_PTR,     --  HWND  hWnd              // handle of window
             C_UINT,    --  int  nIndex             // offset of value to store
             C_LONG},   --  LONG/LONG_PTR dwNewLong // value to store
            C_LONG)     -- LONG/LONG_PTR            // previous value
        xSetWindowPos = define_c_func(USER32,"SetWindowPos",
            {C_PTR,     --  HWND hwnd               // handle of window
             C_PTR,     --  HWND hwndInsertAfter    // placement-order handle
             C_INT,     --  int x       // horizontal position
             C_INT,     --  int y       // vertical position
             C_INT,     --  int cx      // width
             C_INT,     --  int cy      // height
             C_UINT},   --  UINT uFlags // window-positioning flags (SWP_xxx)
            C_LONG)     -- BOOL
        xSetWindowText = define_c_proc(USER32,"SetWindowTextA",
            {C_PTR,     --  HWND hWnd
             C_PTR})    --  LPCTSTR lpString
        xShowWindow = define_c_proc(USER32,"ShowWindow",
            {C_PTR,     --  HWND  hwnd      // handle of window
             C_INT})    --  int  nCmdShow   // show state of window
        xTranslateMessage = define_c_proc(USER32,"TranslateMessage",
            {C_PTR})    --  CONST MSG  *lpmsg   // address of structure with message
--          C_INT)      -- BOOL (true if was translated...)
        xUpdateWindow = define_c_proc(USER32,"UpdateWindow",
            {C_PTR})    --  HWND hWnd
--          C_INT)      -- BOOL

        idPOINT = define_struct("""typedef struct tagPOINT {
                                     LONG x;
                                     LONG y;
                                   } POINT, *PPOINT;""")

        idMESSAGE = define_struct("""typedef struct tagMSG {
                                      HWND   hwnd;
                                      UINT   message;
                                      WPARAM wParam;
                                      LPARAM lParam;
                                      DWORD  time;
                                      POINT  pt;
                                     } MSG, *PMSG, *LPMSG;""")

        idRECT = define_struct("""typedef struct _RECT {
                                    LONG left;
                                    LONG top;
                                    LONG right;
                                    LONG bottom;
                                  } RECT, *PRECT;""")

        idSIZE = define_struct("""typedef struct tagSIZE {
                                    LONG cx;
                                    LONG cy;
                                  } SIZE, *PSIZE;""")

        idPAINTSTRUCT = define_struct("""typedef struct tagPAINTSTRUCT {
                                          HDC  hdc;
                                          BOOL fErase;
                                          RECT rcPaint;
                                          BOOL fRestore;
                                          BOOL fIncUpdate;
                                          BYTE rgbReserved[32];
                                         } PAINTSTRUCT, *PPAINTSTRUCT;""")

        -- aside: code is now INT, was UINT (specific case: TVN_SELCHANGED == -402...)
        idNMHDR = define_struct("""typedef struct tagNMHDR {
                                    HWND     hwndFrom;
                                    UINT_PTR idFrom;
                                    INT      code;
                                   } NMHDR;""")

        idTVITEM = define_struct("""typedef struct tagTVITEM {
                                     UINT       mask;
                                     HTREEITEM  hItem;
                                     UINT       state;
                                     UINT       stateMask;
                                     LPTSTR     pszText;
                                     int        cchTextMax;
                                     int        iImage;
                                     int        iSelectedImage;
                                     int        cChildren;
                                     LPARAM     lParam;
                                    } TVITEM, *LPTVITEM;""")

        idTVITEMEX = define_struct("""typedef struct tagTVITEMEX {
                                        UINT        mask;
                                        HTREEITEM   hItem;
                                        UINT        state;
                                        UINT        stateMask;
                                        LPTSTR      pszText;
                                        int         cchTextMax;
                                        int         iImage;
                                        int         iSelectedImage;
                                        int         cChildren;
                                        LPARAM      lParam;
                                        int         iIntegral;
                                        UINT        uStateEx;
                                        HWND        hwnd;
                                        int         iExpandedImage;
                                        int         iReserved;
                                      } TVITEMEX, *LPTVITEMEX;""")

        idNMTREEVIEW = define_struct("""typedef struct tagNMTREEVIEW {
                                         NMHDR  hdr;
                                         UINT   action;
                                         TVITEM itemOld;
                                         TVITEM itemNew;
                                         POINT  ptDrag;
                                        } NMTREEVIEW, *LPNMTREEVIEW;""")

        idTVINSERTSTRUCT = define_struct("""typedef struct {
                                             HTREEITEM hParent;
                                             HTREEITEM hInsertAfter;
                                             TVITEMEX itemex;
                                            } TVINSERTSTRUCT, *LPTVINSERTSTRUCT;""")
           
        pPAINTSTRUCT = allocate_struct(idPAINTSTRUCT)
        pRECT = allocate_struct(idRECT)
        pSIZE = allocate_struct(idSIZE)
        pTVINSERTSTRUCT = allocate_struct(idTVINSERTSTRUCT)
        pTVITEMEX = allocate_struct(idTVITEMEX)
        pMSG = allocate_struct(idMESSAGE)

        -- register class:
        szAppName = xpg_raw_string_ptr("xpGUI")
        integer idWNDCLASSEX = define_struct("""typedef struct tagWNDCLASSEX {
                                                  UINT      cbSize;
                                                  UINT      style;
                                                  WNDPROC   lpfnWndProc;
                                                  int       cbClsExtra;
                                                  int       cbWndExtra;
                                                  HINSTANCE hInstance;
                                                  HICON     hIcon;
                                                  HCURSOR   hCursor;
                                                  HBRUSH    hbrBackground;
                                                  LPCTSTR   lpszMenuName;
                                                  LPCTSTR   lpszClassName;
                                                  HICON     hIconSm;
                                                } WNDCLASSEX, *PWNDCLASSEX;""")
        atom pWNDCLASSEX = allocate_struct(idWNDCLASSEX),
                  hwIcon = c_func(xLoadIcon,{instance(),10}),
                 hCursor = c_func(xLoadCursor, {NULL, IDC_ARROW}),
             class_style = or_all({CS_DBLCLKS,CS_HREDRAW,CS_VREDRAW}),
                win_proc = call_back(xpg_WinAPI_WndProc)
        set_struct_field(idWNDCLASSEX,pWNDCLASSEX,"cbSize",get_struct_size(idWNDCLASSEX))
        set_struct_field(idWNDCLASSEX,pWNDCLASSEX,"style",class_style)
        set_struct_field(idWNDCLASSEX,pWNDCLASSEX,"lpfnWndProc",win_proc) -- default message handler
        set_struct_field(idWNDCLASSEX,pWNDCLASSEX,"cbClsExtra",0)   -- no more than 40 bytes for win95
        set_struct_field(idWNDCLASSEX,pWNDCLASSEX,"cbWndExtra",0)   -- "		"      "		"        "
        set_struct_field(idWNDCLASSEX,pWNDCLASSEX,"hInstance",instance())
        set_struct_field(idWNDCLASSEX,pWNDCLASSEX,"hIcon",hwIcon)   -- (32 x 32)
        set_struct_field(idWNDCLASSEX,pWNDCLASSEX,"hIconSm",hwIcon) -- (16 x 16)
        set_struct_field(idWNDCLASSEX,pWNDCLASSEX,"hCursor",hCursor)
        set_struct_field(idWNDCLASSEX,pWNDCLASSEX,"hbrBackground",COLOR_BTNFACE+1)
        set_struct_field(idWNDCLASSEX,pWNDCLASSEX,"lpszMenuName",NULL)
        set_struct_field(idWNDCLASSEX,pWNDCLASSEX,"lpszClassName",szAppName)
        if c_func(xRegisterClassEx,{pWNDCLASSEX})=0 then
            crash("RegisterClassEx error #%08x (%d)",c_func(xGetLastError,{}))
        end if
        tree_himl = xpg_create_image_list(bUseGTK,xpg_xpm_callback)
    end if
end procedure

--global function gGetIntInt(gdx h, string name)
--end function

global function gVersion()
--  if not bInit then xpg_Init() end if
    integer pdx = find(platform(),{WINDOWS,LINUX,ARM,JS})
    string bare = "xpGUI 0.1",
           plat = {"Windows","Linux","Arm","JavaScript"}[pdx],
        backing = iff(bUseGTK?"GTK":"WinAPI") -- (or "xpGUI.js" therein)
    return sprintf("%s on Phix version %s (%d bits) on %s, using %s",
                  {bare,version(),M,plat,backing})
end function

global function gGetGlobalIntInt(string name)
    if not bInit then xpg_Init() end if
    if name="SCREENSIZE" then 
        integer width, height
        if bUseGTK then
            if M=32 then
                width = c_func(gdk_screen_get_width)
                height = c_func(gdk_screen_get_height)
            else -- GTK/64 bit (and/or post-2.0)
                atom display = c_func(gdk_display_get_default),
                     monitor = c_func(gdk_display_get_monitor,{display,0})
                c_proc(gdk_monitor_get_geometry,{monitor,pRECT})
                width = get_struct_field(idGdkRectangle,pRECT,"width")
                height = get_struct_field(idGdkRectangle,pRECT,"height")
            end if
        else
            width = c_func(xGetSystemMetrics, {SM_CXSCREEN})
            height = c_func(xGetSystemMetrics, {SM_CYSCREEN})
        end if
        return {width, height}
--/*
    elsif name="MOUSEPOS" then
void
gdk_display_get_pointer (GdkDisplay *display,
                         GdkScreen **screen,
                         gint *x,
                         gint *y,
                         GdkModifierType *mask);
--*/
    end if
    crash("gGetGlobalIntInt(%s) not supported",{name})
end function

global function gGetGlobal(string name)
    if name="SCREENSIZE" then 
        return sprintf("%dx%x",gGetGlobalIntInt(name))
    elsif name="VERSION" then
        return gVersion()
    end if
    crash("gGetGlobal(%s) not supported",{name})
end function

global procedure gSetGlobal(string name, object v)
    if name="XPM_INIT" then 
        xpg_create_image_list = v
    else
        crash("gSetGlobal(%s,%v) not supported",{name,v})
    end if
end procedure

include xpg_xpm.e


--bUseMapping:
--sequence dm_create_rids = {},
--       dm_args = {}

--procedure xpg_defer_mapping(integer id, create_rid, sequence args)
----    dm_create_rids &= create_rid
--  create_rids[id] = create_rid
----    dm_args = append(dm_args,args)
--  create_args[id] = args
--end procedure

local function xpg_WinAPI_create(integer id, string class_name, lbl, atom pID, w, h, dwStyle, dwExStyle)
    atom pHwnd = iff(pID?ctrl_handles[pID]:NULL),
        lpszClassName = xpg_raw_string_ptr(class_name),
       lpszWindowName = xpg_raw_string_ptr(lbl)

    sequence cw_params = {dwExStyle,        -- extended style
                          lpszClassName,    -- window class name
                          lpszWindowName,   -- window caption or Button text etc..
                          dwStyle,          -- window style
                          CW_USEDEFAULT,    -- initial x position
                          CW_USEDEFAULT,    -- initial y position
                          w,                -- initial x size
                          h,                -- initial y size
                          pHwnd,            -- parent window handle
                          NULL,             -- window menu handle OR user id for child windows
                          NULL,             -- program instance handle - Legacy of Win16 apps. 0 will work too.
                          NULL}             -- creation parameters
--DEV
--if class_name="button" then
----    cw_params[5..8] = {10,10,120,30}
--  cw_params[7..8] = {120,30}
--end if

    assumed_hwnd = NULL
    assume_id = id
    atom hwnd = c_func(xCreateWindowEx, cw_params)
    if hwnd=0 then
        crash("CreateWindowEx error #%08x",{c_func(xGetLastError,{})})
    end if
    assert(assumed_hwnd=NULL or assumed_hwnd=hwnd)
    assume_id = 0

--DEV?? (nope, still needed before subclassing...) [which[DEV] we might be able to move...]
--  ctrl_handles[id] = hwnd
--
--  atom tmp = c_func(xGetWindowLong, {hwnd, GWL_WNDPROC})
--  wnd_proc_addr[id] = tmp

--DEV?? (now does above too)
    xpg_setID(hwnd,id)

    xpg_WinAPI_sub_class_control(id, hwnd)

--  if ctrl_type=DIALOG and PrimaryWindowID=UNDEFINED then
--      PrimaryWindowID = id
--  end if

    if pID then
--if bUseMapping then
        if ctrl_types[id]=DIALOG then
            assert(ctrl_types[pID]=DIALOG)
        else
            assert(find(id,children_ids[pID])!=0)
        end if
--  assert(pID=NULL or find(id,children_ids[pID])!=0 or (ctrl_types[id]=DIALOG and ctrl_types[pID]=DIALOG))
--else
--      children_ids[pID] &= id
--end if
    end if

    return hwnd  -- (nb handle not id!)

end function

--/* --DEV I suspect manual(/auto) translate needed...
function $paranormalise(func, attributes, args) {
// (see pGUI.e or the help docs for more details)
// ([DOC] named parameters are not [generally] permitted in pwa/p2js, except perhaps for eg timedelta()/as individually hand-coded in p2js.e)
?   if (action === NULL) { action = null; }
    if (func === NULL) { func = null; }
?   if ((action !== null) && (typeof(action) !== "string")) {
?       if (typeof(action) === "function") {
            if (attributes && Array.isArray(attributes) && attributes.length) {
                args = attributes;
                attributes = func;
            } else if (typeof(func) === "string") {
                attributes = func;
            }
?           func = action;
?           action = null;
        } else {
            if (Array.isArray(func)) {
                args = func;
            }
?           attributes = action;
            func = null;
            action = null;
        }
    } else if (func === null) {
        if ((typeof(attributes) !== "string" || attributes.length === 0) && (action !== null)) {
?           attributes = action;
            action = null;
        }
    } else if (Array.isArray(func)) {
?       attributes = action;
        args = func;
?       action = null;
        func = null;
    }
--  return [action,func,attributes,args];
    return [func,attributes,args];
}
--*/

include builtins/ptypes.e

local type dword_seq(object s)  -- (technically qword_seq on 64-bit)
    return sequence(s) and not string(s)
end type

local function paranormalise_traa(object title, click, sequence attributes, dword_seq args)
-- used by gButton([nullable_string title=NULL,] [rtn click=NULL,] string attributes="", sequence args={})
-- and gLink(), gMenuItem(), gToggle(), and gValuator().
-- (See the docs for the full details)
-- This routine is designed to crash on the slightest oddity. (some msg cleanup to be expected)
    integer nFrames = 3 -- (change this back to 1 to debug)
    if not nullable_string(title) then
        -- assume title omitted (and at least one other parameter passed)
        if atom(title) then -- (and, of course, title is not NULL here)
            -- assume gButton(click,[attributes[,args]])                -- (intended)
            --                ^title ^click      ^attributes            -- (actual)
            assert(args={},"original_default",nFrames:=nFrames)
            if length(attributes) then -- (args really?)
                args = attributes                                       -- (verified dword_seq now)
                attributes = click                                      -- (verified string below)
            elsif string(click) then
                attributes = click                                      -- (verified string below)
            else
                asserteq(click,NULL,nFrames:=nFrames)                   -- something odd passed?
            end if
            click = title                                               -- (verified rtn below)
            title = NULL
        else
            -- assume gButton(attributes[,args])                        -- (intended)
            --                ^title      ^click                        -- (actual)
            asserteq(attributes,"","original default",nFrames:=nFrames)
            asserteq(args,{},"original default",nFrames:=nFrames)
            if sequence(click) then -- (args really?)
                args = click                                            -- (verified dword_seq now)
            end if
            attributes = title                                          -- (verified string below)
            click = NULL
            title = NULL
        end if
    elsif string(click) then
        -- assume gButton(title,attributes[,args]))                     -- (intended)
        --                      ^click      ^attributes                 -- (actual)
        if not string(attributes) then -- (args really?)
            args = attributes                                           -- (verified dword_seq now)
        end if
        attributes = click                                              -- (verified string below)
        click = NULL
    elsif sequence(click) then -- (and not string)
        -- assume gButton(attributes,args)                              -- (intended)
        --                ^title     ^click                             -- (actual)
        asserteq(args,{},"original default",nFrames:=nFrames)
        asserteq(attributes,"","original default",nFrames:=nFrames)
        attributes = title                                              -- (verified string below)
        args = click                                                    -- (verified dword_seq now)
        title = NULL
        click = NULL
--  else assume 3 or 4 parameters were passed (title,click,attributes[,args])
    end if
    assert(nullable_string(title),"not string title",nFrames:=nFrames)
    assert(rtn(click),"not rtn click",nFrames:=nFrames)
    assert(string(attributes),"not string attributes",nFrames:=nFrames)
--  assert(dword_seq(args),nFrames:=nFrames)                            -- (ple/autoverified)
    if attributes="" then
        asserteq(args,{},"original default",nFrames:=nFrames)
    end if
    return {title,click,attributes,args}
end function

--tests:
--  1   gButton()
--  2   gButton(click)
--  3   gButton(click,attributes[,args])
--  4   gButton(attributes,args)
--  -   gButton(attributes)     -- invalid (treated as gButton(title)
--  5   gButton(title)
--  6   gButton(title,click)
--  7   gButton(title,attributes[,args])
--  8   gButton(title,click,attributes[,args])
--
-- (with [title=NULL, click=NULL, attr="", args={}] defaults after "  ":)
--/*
constant tra = paranormalise_traa  -- (nb still>last_xpgui_rid here)
assert(paranormalise_traa(     NULL,NULL,"",{})=={NULL,NULL,"",{}})         -- 1
assert(paranormalise_traa(tra      ,NULL,"",{})=={NULL,tra,"",{}})          -- 2
assert(paranormalise_traa(tra,"x"       ,"",{})=={NULL,tra,"x",{}})         -- 3
assert(paranormalise_traa(tra,"x",{1}      ,{})=={NULL,tra,"x",{1}})        -- 3
assert(paranormalise_traa("x",{1}       ,"",{})=={NULL,NULL,"x",{1}})       -- 4
assert(paranormalise_traa("title"  ,NULL,"",{})=={"title",NULL,"",{}})      -- 5
assert(paranormalise_traa("title",tra   ,"",{})=={"title",tra,"",{}})       -- 6
assert(paranormalise_traa(NULL,tra      ,"",{})=={NULL,tra,"",{}})          -- 6
assert(paranormalise_traa("title","x"   ,"",{})=={"title",NULL,"x",{}})     -- 7
assert(paranormalise_traa("title","x",{1}  ,{})=={"title",NULL,"x",{1}})    -- 7
assert(paranormalise_traa("title",tra,"x"  ,{})=={"title",tra,"x",{}})      -- 8
assert(paranormalise_traa("title",tra,"x",{1} )=={"title",tra,"x",{1}})     -- 8
assert(paranormalise_traa(NULL,tra,"x"     ,{})=={NULL,tra,"x",{}})         -- 8
assert(paranormalise_traa(NULL,tra,"x",{1}    )=={NULL,tra,"x",{1}})        -- 8
--assert(paranormalise_traa("x"    ,NULL,"",{})=={NULL,NULL,"x",{}})    -- (invalid)
--named parameters:
assert(paranormalise_traa(NULL,tra,"",{})=={NULL,tra,"",{}})    -- click:=tra
assert(paranormalise_traa(NULL,NULL,"x",{})=={NULL,NULL,"x",{}}) -- attributes:="x"
--*/

function paranormalise_qraa(object q, rid, sequence attributes, dword_seq args)
-- used by gTreeView([sequence q={},] [rtn rid=NULL,] string attributes="", sequence args={})
-- (where q is tree_nodes and rid is branchopen)  (See the docs for the full details)
-- This routine is designed to crash on the slightest oddity. (some msg cleanup to be expected)
    integer nFrames = 3 -- (change this back to 1 to debug)
    if not dword_seq(q) then
        -- assume q omitted (and at least one other parameter passed)
        if atom(q) then -- assume q is really rid
            -- assume gTreeView(rid,[attributes[,args]])                -- (intended)
            --                  ^q   ^rid        ^attributes            -- (actual)
            assert(args={},"original_default",nFrames:=nFrames)
            if length(attributes) then -- (args really?)
                args = attributes                                       -- (verified qword_seq now)
                attributes = rid                                        -- (verified string below)
            elsif string(rid) then -- (attributes really?)
                attributes = rid                                        -- (verified string below)
            else
                asserteq(rid,NULL,nFrames:=nFrames)                     -- something odd passed?
            end if
            rid = q                                                     -- (verified rtn below)
            q = {}
        else
            -- assume gTreeView(attributes[,args])                      -- (intended)
            --                  ^q         ^rid                         -- (actual)
            asserteq(attributes,"","original default",nFrames:=nFrames)
            asserteq(args,{},"original default",nFrames:=nFrames)
            if sequence(rid) then -- (args really?)
                args = rid                                              -- (verified dword_seq now)
            end if
            attributes = q                                              -- (verified string below)
            rid = NULL
            q = {}
        end if
    elsif string(rid) then
        -- assume gTreeView(q,attributes[,args]))                       -- (intended)
        --                    ^rid        ^attributes                   -- (actual)
        if not string(attributes) then -- (args really?)
            args = attributes                                           -- (verified dword_seq now)
        end if
        attributes = rid                                                -- (verified string below)
        rid = NULL
    elsif sequence(rid) then -- (and not string)
        -- assume gTreeView(attributes,args)                            -- (intended)
        --                  ^ in q     ^ in rid                         -- (actual)
        asserteq(args,{},"original default",nFrames:=nFrames)
        asserteq(attributes,"","original default",nFrames:=nFrames)
        attributes = q                                                  -- (verified string below)
        args = rid                                                      -- (verified dword_seq now)
        q = {}
        rid = NULL
--  else assume 3 or 4 parameters were passed (q,rid,attributes[,args])
    end if
    assert(dword_seq(q),"not dword_seq treenodes",nFrames:=nFrames)
    assert(rtn(rid),"not rtn rid",nFrames:=nFrames)
    assert(string(attributes),"not string attributes",nFrames:=nFrames)
--  assert(dword_seq(args),nFrames:=nFrames)        -- (ple/autoverified)
    if attributes="" then
        asserteq(args,{},"original default",nFrames:=nFrames)
    end if
    return {q,rid,attributes,args}
end function

--tests (eg):
--  1   gTreeView()
--  2   gTreeView(rid)
--  3   gTreeView(attributes[,args])
--  4   gTreeView(rid,attributes[,args])
--  5   gTreeView({})
--  6   gTreeView({},rid)
--  7   gTreeView({},attributes[,args])
--  8   gTreeView({},rid,attributes[,args])
--/*
--?"paranormalise_qraa"
constant qra = paranormalise_qraa  -- (nb still>last_xpgui_rid here)
-- (with [tn={}, rid=NULL, attr="", args={}] defaults after "  ":)
assert(paranormalise_qraa(      {},NULL,"",{})=={{},NULL,"",{}})    -- 1
assert(paranormalise_qraa(qra     ,NULL,"",{})=={{},qra,"",{}})     -- 2
assert(paranormalise_qraa("x"     ,NULL,"",{})=={{},NULL,"x",{}})   -- 3
assert(paranormalise_qraa("x",{1}      ,"",{})=={{},NULL,"x",{1}})  -- 3
assert(paranormalise_qraa(qra,"x"      ,"",{})=={{},qra,"x",{}})    -- 4
assert(paranormalise_qraa(qra,"x",{1}     ,{})=={{},qra,"x",{1}})   -- 4
assert(paranormalise_qraa({2}     ,NULL,"",{})=={{2},NULL,"",{}})   -- 5
assert(paranormalise_qraa({2},qra      ,"",{})=={{2},qra,"",{}})    -- 6
assert(paranormalise_qraa({2},"x"      ,"",{})=={{2},NULL,"x",{}})  -- 7
assert(paranormalise_qraa({2},"x",{1}     ,{})=={{2},NULL,"x",{1}}) -- 7
assert(paranormalise_qraa({2},qra,"x"     ,{})=={{2},qra,"x",{}})   -- 8
assert(paranormalise_qraa({2},qra,"x",{1}    )=={{2},qra,"x",{1}})  -- 8
--named parameters:
assert(paranormalise_qraa({},qra  ,"",{})=={{},qra,"",{}})   -- rid:=raa
assert(paranormalise_qraa({},NULL,"x",{})=={{},NULL,"x",{}}) -- attributes:="x"
--*/

function paranormalise_raa(object rid, sequence attributes, dword_seq args)
-- used by gText([rtn rid=NULL,] string attributes="", sequence args={})
-- and gCanvas(), gDatePick(), gDetachBox(), gList(), and gSpinBox().
-- (See the docs for the full details)
-- This routine is designed to crash on the slightest oddity. (some msg cleanup to be expected)
    integer nFrames = 3 -- (change this back to 1 to debug)
    if dword_seq(attributes) then
        -- assume gText(attributes,args)                                    -- (intended)
        --              ^ in rid   ^ in attributes                          -- (actual)
        asserteq(args,{},"original default",nFrames:=nFrames)
        args = attributes                                                   -- (verified dword_seq now)
        attributes = rid                                                    -- (verified string below)
        rid = NULL
    elsif string(rid) then
        -- assume gText(attributes)                                         -- (intended)
        --              ^ in rid                                            -- (actual)
        asserteq(args,{},"original default",nFrames:=nFrames)
        attributes = rid                                                    -- (verified string below)      
        rid = NULL
    end if
    assert(rtn(rid),"not rtn rid",nFrames:=nFrames)
    assert(string(attributes),"not string attributes",nFrames:=nFrames)
--  assert(dword_seq(args),nFrames:=nFrames)                                -- (ple/autoverified)
    if attributes="" then
        asserteq(args,{},"original default",nFrames:=nFrames)
    end if
    return {rid,attributes,args}
end function

--tests (eg):
--  1   gText()
--  2   gText(rid)
--  3   gText(attributes[,args])
--  4   gText(rid,attributes[,args])
--/*
--?"paranormalise_raa"
constant raa = paranormalise_raa  -- (nb still>last_xpgui_rid here)
-- (with [rid=NULL, attr="", args={}] defaults after "  ":)
assert(paranormalise_raa(      NULL,"",{})=={NULL,"",{}})   -- 1
assert(paranormalise_raa(raa       ,"",{})=={raa,"",{}})    -- 2
assert(paranormalise_raa("x"       ,"",{})=={NULL,"x",{}})  -- 3
assert(paranormalise_raa("x",{1}      ,{})=={NULL,"x",{1}}) -- 3
assert(paranormalise_raa(raa,"x"      ,{})=={raa,"x",{}})   -- 4
assert(paranormalise_raa(raa,"x",{1}     )=={raa,"x",{1}})  -- 4
--named parameters:
assert(paranormalise_raa(raa  ,"",{})=={raa,"",{}})   -- rid:=raa [===2]
assert(paranormalise_raa(NULL,"x",{})=={NULL,"x",{}}) -- attributes:="x"
--*/

function paranormalise_paab(object parent, attributes, args, bool bEsc)
-- used by gDialog(gdx child, [parent=NULL,] [string attributes=""[, sequence args={}]], bool bEsc=true)
-- (however the non-optional child is not actually passed here)  (See the docs for the full details)
-- This routine is designed to crash on the slightest oddity. (some msg cleanup to be expected)
    integer nFrames = 3 -- (change this back to 1 to debug)
    if not gdx(parent) then
        -- assume parent omitted, and at least one other parameter passed,
        -- and in fact the first **must** then be the (string) attributes.
        assert(string(parent),"not string attributes",nFrames:=nFrames)
        asserteq(bEsc,true,"original default",nFrames:=nFrames)
        if bool(attributes) then
            -- assume gDialog(child,"attr",bEsc)                -- (intended)
            --                    parent^  ^attributes          -- (actual)
            asserteq(args,{},"original default",nFrames:=nFrames)
            bEsc = attributes
        else
            -- assume(gDialog(child,"attr",args[,bEsc])         -- (intended)
            --                    parent^  ^attr ^args          -- (actual)
            if bool(args) then
                bEsc = args                                     -- (verified bool now)
            else -- (should be original default)
                asserteq(args,{},"and not bool bEsc",nFrames:=nFrames)
            end if
            if dword_seq(attributes) then
                args = attributes                               -- (verified dword_seq below)
            else -- (should be original default)
                asserteq(attributes,"","and not dword_seq args",nFrames:=nFrames)
            end if
        end if
        attributes = parent                                     -- (verified string below)
        parent = NULL
    elsif bool(attributes) then
        -- assume gDialog(child,parent,bEsc):                   -- (intended)
        --                             ^attributes              -- (actual)
        asserteq(args,{},"original default)",nFrames:=nFrames)
        asserteq(bEsc,true,"original default",nFrames:=nFrames)
        bEsc = attributes                                       -- (verified bool now)
        attributes = ""
    elsif bool(args) then
        -- assume gDialog(child,parent,attributes,bEsc):        -- (intended)
        --                                        ^args         -- (actual)
        asserteq(bEsc,true,"original default",nFrames:=nFrames)
        bEsc = args                                             -- (verified bool now)
        args = {}
    end if
    assert(gdx(parent),"not gdx parent",nFrames:=nFrames)
    assert(string(attributes),"not string attributes",nFrames:=nFrames)
    assert(dword_seq(args),"not dword_seq args",nFrames:=nFrames)
--  assert(bool(bEsc),nFrames:=nFrames)                         -- (ple/autoverified)
    if attributes="" then
        asserteq(args,{},"original default",nFrames:=nFrames)
    end if
    return {parent,attributes,args,bEsc}
end function

--tests (child omitted):
--  1   gDialog()
--  2   gDialog(attributes)
--  3   gDialog(attributes,args)
--  4   gDialog(attributes,args,bEsc)
--  5   gDialog(attributes,bEsc)
--  -   gDialog(bEsc)   -- (invalid)
--  6   gDialog(parent)
--  7   gDialog(parent,bEsc)
--  8   gDialog(parent,attributes)
--  9   gDialog(parent,attributes,bEsc)
--  10  gDialog(parent,attributes,args)
--  11  gDialog(parent,attributes,args,bEsc)
--/*
--?"paranormalise_paab"
gdx p = xpg_add_control(DIALOG) -- [otherwise all the gdx() tests will fail]
-- (with [parent=NULL, attr="", args={}, bEsc=true] defaults after "  ":)
assert(paranormalise_paab(     NULL,"",{},true)=={NULL,"",{},true})     -- 1
assert(paranormalise_paab("x"      ,"",{},true)=={NULL,"x",{},true})    -- 2
assert(paranormalise_paab("x",{1}     ,{},true)=={NULL,"x",{1},true})   -- 3
assert(paranormalise_paab("x",{1},false  ,true)=={NULL,"x",{1},false})  -- 4
assert(paranormalise_paab("x",false   ,{},true)=={NULL,"x",{},false})   -- 5
assert(paranormalise_paab(p        ,"",{},true)=={p,"",{},true})        -- 6
assert(paranormalise_paab(p,false     ,{},true)=={p,"",{},false})       -- 7
assert(paranormalise_paab(p,"x"       ,{},true)=={p,"x",{},true})       -- 8
assert(paranormalise_paab(p,"x",false    ,true)=={p,"x",{},false})      -- 9
assert(paranormalise_paab(p,"x",{1}      ,true)=={p,"x",{1},true})      -- 10
assert(paranormalise_paab(p,"x",{1},false     )=={p,"x",{1},false})     -- 11
--named parameters:
assert(paranormalise_paab(   p,"", {},true)=={p,"",{},true})     -- parent:=p
assert(paranormalise_paab(NULL,"x",{},true)=={NULL,"x",{},true}) -- attributes:="x"
--*/

procedure xpg_Dialog(integer id, parent, string attributes, sequence args)
    atom handle
    if bUseGTK then
        handle = c_func(gtk_window_new,{GTK_WINDOW_TOPLEVEL}) 
        assert(handle!=NULL)
        if parent!=NULL then
            c_proc(gtk_window_set_transient_for,{handle,ctrl_handles[parent]}) 
        end if
        c_proc(g_signal_connect_data,{handle,"key_press_event",gkt_check_esc_cb,NULL,NULL,NULL})
    else
        -- (aside: ctrl_types[id] is already DIALOG)
        atom d = CW_USEDEFAULT
--CS_OWNDC?
        handle = xpg_WinAPI_create(id,"xpGUI","",parent,d,d,WS_OVERLAPPEDWINDOW,WS_EX_ACCEPTFILES)
--      atom mainDC = c_func(xGetDC,{handle})
--      ctrl_extra[id][CE_MAINDC] = mainDC

    end if
    xpg_setID(handle,id)
    if length(attributes) then
        gSetAttributes(id,attributes,args)
    end if
--  xpg_set_any_deferred_attrs(id)
end procedure

global function gDialog(gdx child, object parent=NULL, object attributes="", args={}, bool bEsc=true)
without debug
    {parent,attributes,args,bEsc} = paranormalise_paab(parent,attributes,args,bEsc)
    if not bInit then xpg_Init() end if
    assert(parent=NULL or ctrl_types[parent]=DIALOG)
    integer flags = iff(bEsc?CF_CLOSE_ON_ESC:0),
               id = xpg_add_control(DIALOG, parent, flags)
    if child then
        parent_ids[child] = id
        children_ids[id] = {child}
    end if
    create_rids[id] = xpg_Dialog
    create_args[id] = {id,parent,attributes,args}
    -- DEV might be better to have this in xpg_Dialog()??
    if PrimaryWindowID=UNDEFINED then
        PrimaryWindowID = id
    end if
    return id
end function

local procedure gtk_store_set(atom store, iter, sequence columns, data, integer n, cfn)
    -- common code for gtk_tree_store_set() and gtk_list_store_set()
    assert(length(columns)=n)
    assert(length(data)=n)
    for i=1 to n do
        integer column = columns[i]
        object di = data[i]
        if string(di) then di = xpg_raw_string_ptr(di) end if
        c_proc(cfn,{store, iter, column, di, -1})
    end for
end procedure

local constant COL_IMG  = 0,    -- the node icon
               COL_NAME = 1,    -- node text
               F_COLOUR = 2,    -- foreground colour
               B_COLOUR = 3,    -- background colour
               COLSPACE = 4,    -- a spacer column (else name is full width)
               USERDATA = 5,    -- hidden data, not displayed (an integer)
               NUM_COLS = 6,
               STD_COLS = {COL_IMG, COL_NAME, F_COLOUR, B_COLOUR, COLSPACE, USERDATA}

local procedure xpg_TreeSetNodeAttributes(xpg_handle tree_view, object what, sequence attrs)
-- (internal routine)
    atom tree_store, path, iter,    -- (GTK only)
         hItem, tIdx                -- (WinAPI only)
    if bUseGTK then
        {tree_store, path, iter} = what
    else
        {hItem, tIdx} = what
    end if
    for i=1 to length(attrs) by 2 do
        string name = attrs[i]
--      if not find(name,{"COLOR",
--                        "STATE",
--                        "TITLE",
--                        "TITLEFONT",
--                        "TOGGLEVALUE",
--                        "TOGGLEVISIBLE",
--                        "USERDATA",
--                        "IMAGE",
--                        "IMAGEEXPANDED",
--                        "MARKED"}) then
--          ?9/0
--      end if
        if name="STATE" then
            string state = attrs[i+1]
            if state="COLLAPSED" then
                if bUseGTK then
                    c_proc(gtk_tree_view_collapse_row,{tree_view,path})
                else
                    {} = c_func(xSendMessage,{tree_view,TVM_EXPAND,TVE_COLLAPSE,hItem})
                end if
            elsif state="EXPANDED" then
                if bUseGTK then
                    c_proc(gtk_tree_view_expand_row,{tree_view,path,false})
                else
                    {} = c_func(xSendMessage,{tree_view,TVM_EXPAND,TVE_EXPAND,hItem})
                end if
            else crash("invalid STATE:"&state)
            end if
        elsif name="USERDATA" then
            integer user_data = attrs[i+1]
            if bUseGTK then
                gtk_store_set(tree_store,iter,{USERDATA},{user_data},1,gtk_tree_store_set)
            else
--NO! lParam is the tree_items index!
--              set_struct_field(idTVITEMEX,pTVITEMEX,"hItem",hItem)
--              set_struct_field(idTVITEMEX,pTVITEMEX,"mask",TVIF_HANDLE+TVIF_PARAM)
--              set_struct_field(idTVITEMEX,pTVITEMEX,"lParam",user_data)
--              bool bOK = c_func(xSendMessage,{tree_view,TVM_SETITEM,0,pTVITEMEX})
--              assert(bOK)
                tree_items[tIdx][tUserData] = user_data
            end if
        else crash("invalid attribute name:"&name)
        end if
    end for
end procedure

local function xpg_WinAPI_addTVItemEx(xpg_handle tree_view, atom hParen, integer tIdx, iImage, iExpanded, cChildren)    -- (WinAPI only)
--local function xpg_WinAPI_addTVItemEx(gdx id, atom hParen, integer tIdx, iImage, iExpanded, cChildren)
-- result is the handle (/"ATOM") of the inserted item
-- id is the handle (not gdx) of a TreeView Control
-- hParen is NULL or the result from a previous addTVItem call
-- iImage/iExpanded are (0-based) indices to the imagelist.
-- cChildren must be non-zero (but not necessarily correct) for an expandable node
--
    set_struct_field(idTVINSERTSTRUCT,pTVINSERTSTRUCT,"hParent",hParen)
    set_struct_field(idTVINSERTSTRUCT,pTVINSERTSTRUCT,"hInsertAfter",TVI_LAST)
    integer mask = or_all({TVIF_TEXT,TVIF_IMAGE,TVIF_SELECTEDIMAGE,TVIF_PARAM,TVIF_CHILDREN,TVIF_EXPANDEDIMAGE})
    set_struct_field(idTVINSERTSTRUCT,pTVINSERTSTRUCT,"itemex.mask",mask)
    set_struct_field(idTVINSERTSTRUCT,pTVINSERTSTRUCT,"itemex.pszText",LPSTR_TEXTCALLBACK)
    set_struct_field(idTVINSERTSTRUCT,pTVINSERTSTRUCT,"itemex.iImage",iImage)
    integer icon = iff(iImage=iExpanded?iImage:I_IMAGECALLBACK)
    set_struct_field(idTVINSERTSTRUCT,pTVINSERTSTRUCT,"itemex.iSelectedImage",icon)
    set_struct_field(idTVINSERTSTRUCT,pTVINSERTSTRUCT,"itemex.iExpandedImage",iExpanded)
    set_struct_field(idTVINSERTSTRUCT,pTVINSERTSTRUCT,"itemex.lParam",tIdx)
    set_struct_field(idTVINSERTSTRUCT,pTVINSERTSTRUCT,"itemex.cChildren",cChildren)
--  atom handle = sendMessage(id,TVM_INSERTITEM,0,pTVINSERTSTRUCT)
    atom handle = c_func(xSendMessage,{tree_view,TVM_INSERTITEM,0,pTVINSERTSTRUCT})
    return handle
end function

local procedure xpg_tree_add_nodes_rec(sequence tree_nodes, xpg_handle tree_view, object args)
    if tree_nodes={} then return end if
    -- GTK vars:
    atom tree_store, iter, piter, icon, path, chiter
    sequence coldata
    -- WinAPI vars:
    integer tIdx, pdx
    atom hItem
    if bUseGTK then
        {tree_store, iter, piter} = args
        c_proc(gtk_tree_store_append,{tree_store, iter, piter})
    else
        pdx = args
    end if
    if string(tree_nodes) then tree_nodes = {tree_nodes} end if
    string desc = tree_nodes[1]
    integer l = min(length(tree_nodes),3)
    bool bLeaf = l=1 or atom(tree_nodes[l])
    if bUseGTK then
        icon = iff(bLeaf?dot:closed_folder)
        integer userdata = 0 -- (set/overidden later via attributes)
        coldata = {icon,desc,"#000000","#FFFFFF","",userdata}
        gtk_store_set(tree_store,iter,STD_COLS,coldata,NUM_COLS,gtk_tree_store_set) 
    else
        integer {iImage, iExpanded} = iff(bLeaf?{2,2}:{0,1}),   -- dot or closed/open folder
                cChildren = iff(bLeaf?0:length(tree_nodes[l]))
        tIdx = xpg_WinAPI_new_tree_item(desc,bLeaf,pdx)
--      if pdx=null then
----        if ??[id]=UNDEFINED then ??[id] = tIdx end if
--      end if
        atom hParen = iff(pdx?tree_items[pdx][tHandle]:NULL)
        hItem = xpg_WinAPI_addTVItemEx(tree_view, hParen, tIdx, iImage, iExpanded, cChildren)
        tree_items[tIdx][tHandle] = hItem
    end if
    if not bLeaf or l=3 then
        if bUseGTK then
            path = c_func(gtk_tree_model_get_path,{tree_store,iter})
        end if
        object recargs
        if not bLeaf then
            if bUseGTK then
                chiter = allocate(32,true)
                recargs = {tree_store,chiter,iter}
            else
                recargs = tIdx
            end if
            sequence children = tree_nodes[l]
            for child in children do
                xpg_tree_add_nodes_rec(child, tree_view, recargs)
            end for
        end if
        if bUseGTK then
            c_proc(gtk_tree_view_expand_to_path,{tree_view,path})
        else
            {} = c_func(xSendMessage,{tree_view,TVM_EXPAND,TVE_EXPAND,hItem})
        end if
        if l=3 then
            recargs = iff(bUseGTK?{tree_store, path, iter}:{hItem,tIdx})
            xpg_TreeSetNodeAttributes(tree_view, recargs, tree_nodes[2])
        end if
        if bUseGTK then
            c_proc(gtk_tree_path_free,{path})
        end if
    end if
end procedure

global function gTreeGetUserId(object treenode)
    integer res
    if bUseGTK then
        atom {tree_view, iter, path} = treenode,
             tree_store = c_func(gtk_tree_view_get_model,{tree_view}),
             pWord = allocate(machine_word(),true)
        c_proc(gtk_tree_model_get,{tree_store,iter,USERDATA,pWord,-1})
        res = peekns(pWord)
        free(pWord)
    else
        integer tIdx = treenode[2]
        res = tree_items[tIdx][tUserData]
    end if
    return res
end function

bool bNodesAdded = false

global procedure gTreeDeleteChildren(object treenode)
    xpg_handle tree_view = treenode[1]
    if bUseGTK then
        bNodesAdded = true
        atom piter = treenode[2],
             tree_store = c_func(gtk_tree_view_get_model,{tree_view}),
             chiter = allocate(32,true)
        bool ok = c_func(gtk_tree_model_iter_children,{tree_store,chiter,piter})
--      assert(ok) -- child exists
--      ok = c_func(gtk_tree_store_remove,{tree_store,chiter})
--      assert(not ok) -- no next child
        while ok do
--recursive call here???
            ok = c_func(gtk_tree_store_remove,{tree_store,chiter})
        end while
    else
        integer treeIdx = treenode[2]
        string itemtext = tree_items[treeIdx][tText]
        atom hTreeItem = tree_items[treeIdx][tHandle]
        assert(hTreeItem!=NULL)
        -- first off, collapse the node we are about to empty:
        {} = c_func(xSendMessage,{tree_view,TVM_EXPAND,TVE_COLLAPSE,hTreeItem})
        bool bOK    
        while true do
            atom child = c_func(xSendMessage,{tree_view,TVM_GETNEXTITEM,TVGN_CHILD,hTreeItem})
            if child=NULL then exit end if
            set_struct_field(idTVITEMEX,pTVITEMEX,"mask",TVIF_HANDLE+TVIF_PARAM+TVIF_CHILDREN)
            set_struct_field(idTVITEMEX,pTVITEMEX,"hItem",child)
            bOK = c_func(xSendMessage,{tree_view,TVM_GETITEM,0,pTVITEMEX})
            assert(bOK)
            integer childIdx = get_struct_field(idTVITEMEX,pTVITEMEX,"lParam"),
                  childCount = get_struct_field(idTVITEMEX,pTVITEMEX,"cChildren")
            assert(tree_items[childIdx][tHandle]=child)
            if childCount then
                --?{"deleting (children of)",childIdx,tree_items[childIdx][tText]}
                gTreeDeleteChildren({tree_view,childIdx})
            end if
            --?{"deleting (child)",childIdx,tree_items[childIdx][tText]}
            bOK = c_func(xSendMessage,{tree_view,TVM_DELETEITEM,0,child})
            assert(bOK)
            xpg_WinAPI_free_tree_item(childIdx)
        end while
        -- and, for completeness, set the child count back to zero...
        set_struct_field(idTVITEMEX,pTVITEMEX,"mask",TVIF_HANDLE+TVIF_CHILDREN)
        set_struct_field(idTVITEMEX,pTVITEMEX,"hItem",hTreeItem)
        set_struct_field(idTVITEMEX,pTVITEMEX,"cChildren",0)
        bOK = c_func(xSendMessage,{tree_view,TVM_SETITEM,0,pTVITEMEX})
        assert(bOK)
    end if
end procedure

--DEV id treenode is a gdx, then splat the whole tree (but error if not mapped)
global procedure gTreeAddNodes(object treenode, sequence children)
    if gdx(treenode) then -- whole tree replacement
        if and_bits(ctrl_flags[treenode],CF_MAPPED)=0 then
            create_args[treenode][2] = children
        else
            xpg_handle tree_view = ctrl_handles[treenode]
            if bUseGTK then
                atom iter = allocate(32,true),
                     tree_store = c_func(gtk_tree_view_get_model,{tree_view})
                c_proc(gtk_tree_store_clear,{tree_store})
                xpg_tree_add_nodes_rec(children,tree_view,{tree_store,iter,null})
            else
                atom hwnd = c_func(xSendMessage,{tree_view,TVM_GETNEXTITEM,TVGN_ROOT,0})
                if hwnd!=NULL then -- (empty tree)
--              if hwnd=NULL then
--                  ?{"(no chidlren)",tree_view,treenode}
--              else
                    atom pHwnd = c_func(xSendMessage,{tree_view,TVM_GETNEXTITEM,TVGN_ROOT,0})
                    set_struct_field(idTVITEMEX,pTVITEMEX,"hItem",pHwnd)
                    set_struct_field(idTVITEMEX,pTVITEMEX,"mask",TVIF_HANDLE+TVIF_PARAM)
                    bool bOK = c_func(xSendMessage,{tree_view,TVM_GETITEM,0,pTVITEMEX})
                    assert(bOK)
                    integer rootIdx = get_struct_field(idTVITEMEX,pTVITEMEX,"lParam")
                    assert(tree_items[rootIdx][tHandle]=pHwnd)
                    --?{"root:",pHwnd,rootIdx,tree_items[rootIdx][tText]}
                    gTreeDeleteChildren({tree_view,rootIdx})
                    bOK = c_func(xSendMessage,{tree_view,TVM_DELETEITEM,0,pHwnd})
                    assert(bOK)
                    xpg_WinAPI_free_tree_item(rootIdx)
                end if
                xpg_tree_add_nodes_rec(children, tree_view, 0)
            end if
        end if -- </mapped>
    else -- from branchopen, a subnode update
        atom tree_view = treenode[1]
        object args
        if bUseGTK then
            atom tree_store = c_func(gtk_tree_view_get_model,{tree_view}),
                     chiter = allocate(32,true),
                      piter = treenode[2]
            args = {tree_store,chiter,piter}
        else
            integer treeIdx = treenode[2]
            args = treeIdx
        end if
        for child in children do
            xpg_tree_add_nodes_rec(child, tree_view, args)
        end for
    end if
end procedure

local function gtk_row_collapsed(xpg_handle tree_view, atom iter, path, user_data)  -- (GTK only)
    atom tree_store = c_func(gtk_tree_view_get_model,{tree_view})
    gtk_store_set(tree_store,iter,{COL_IMG},{closed_folder},1,gtk_tree_store_set) 
    return true
end function
local constant gtk_row_collapsed_cb = call_back({'+',gtk_row_collapsed})

local function gtk_row_expanded(xpg_handle tree_view, atom iter, path, user_data)   -- (GTK only)
    atom tree_store = c_func(gtk_tree_view_get_model,{tree_view})
    gtk_store_set(tree_store,iter,{COL_IMG},{open_folder},1,gtk_tree_store_set) 
    integer id = xpg_getID(tree_view),
            branchopen = getd({id,"BRANCHOPEN"},handlers)
    if branchopen!=NULL then
        -- aside: user_data is 4th arg of g_signal_connect_data, no use here.
        bNodesAdded = false
        branchopen({tree_view, iter, path})
        if bNodesAdded then
            c_proc(gtk_tree_view_expand_row,{tree_view,path,false})
        end if
    end if
    return 0 /* ignored */
end function
local constant gtk_row_expanded_cb = call_back({'+',gtk_row_expanded})

local bool bPrev = false
local atom sel_iter = allocate(32),
             ppPath = allocate(machine_word())

local function gtk_cursor_changed(xpg_handle tree_view, atom userdata)  -- (GTK only)
    atom tree_store = c_func(gtk_tree_view_get_model,{tree_view})
    if bPrev then
        gtk_store_set(tree_store,sel_iter,{F_COLOUR,B_COLOUR},{"#000000","#FFFFFF"},2,gtk_tree_store_set)
        bPrev = false
    end if
    c_proc(gtk_tree_view_get_cursor,{tree_view,ppPath,NULL})
    atom path = peekns(ppPath)
    if path!=NULL
    and c_func(gtk_tree_model_get_iter,{tree_store, sel_iter, path}) then
        gtk_store_set(tree_store,sel_iter,{F_COLOUR,B_COLOUR},{"#FFFFFF","#0000FF"},2,gtk_tree_store_set)
        bPrev = true
    end if
    return 0 /* ignored */
end function
local constant gtk_cursor_changed_cb = call_back({'+',gtk_cursor_changed})

--DOCS: note that treenodes do not have any attributes, you have to manage that sort of thing through a single
--      USERDATA integer (taking care not to cripple the deferred loading handling).

local procedure xpg_TreeView(gdx id, sequence tree_nodes, string attributes, sequence args)
    -- (invoked via xpg_map)
    gdx parent = parent_ids[id]
    atom tree_view
    if bUseGTK then
        tree_view = c_func(gtk_tree_view_new,{})

        atom selection = c_func(gtk_tree_view_get_selection,{tree_view})
        c_proc(gtk_tree_view_set_headers_visible,{tree_view,false})
        c_proc(gtk_tree_view_set_enable_search,{tree_view,false})
        c_proc(gtk_tree_view_set_enable_tree_lines,{tree_view,true})
        c_proc(gtk_tree_selection_set_mode,{selection,GTK_SELECTION_NONE})
        c_proc(g_signal_connect_data,{tree_view,"row-collapsed",gtk_row_collapsed_cb,0,0,0}) 
        c_proc(g_signal_connect_data,{tree_view,"row-expanded",gtk_row_expanded_cb,0,0,0}) 
        c_proc(g_signal_connect_data,{tree_view,"cursor-changed",gtk_cursor_changed_cb,0,0,0}) 

        atom column = c_func(gtk_tree_view_column_new,{}),
             img_renderer = c_func(gtk_cell_renderer_pixbuf_new,{}),
             txt_renderer = c_func(gtk_cell_renderer_text_new,{}),
             spc_renderer = c_func(gtk_cell_renderer_text_new,{})
        c_proc(gtk_tree_view_column_pack_start,{column, img_renderer, false})
        c_proc(gtk_tree_view_column_add_attribute,{column, img_renderer, "pixbuf", COL_IMG})
        c_proc(gtk_tree_view_column_pack_start,{column, txt_renderer, false})
        c_proc(gtk_tree_view_column_add_attribute,{column, txt_renderer, "text", COL_NAME})
        c_proc(gtk_tree_view_column_add_attribute,{column, txt_renderer, "foreground", F_COLOUR})
        c_proc(gtk_tree_view_column_add_attribute,{column, txt_renderer, "background", B_COLOUR})
        c_proc(gtk_tree_view_column_pack_start,{column, spc_renderer, true})
        c_proc(gtk_tree_view_column_add_attribute,{column, spc_renderer, "text", COLSPACE})
        c_proc(gtk_tree_view_insert_column,{tree_view, column, -1})

        sequence gtypes = {GDK_TYPE_PIXBUF,G_TYPE_STRING,G_TYPE_STRING,G_TYPE_STRING,G_TYPE_STRING,G_TYPE_INT}
        atom tree_store = c_func(gtk_tree_store_newv,{NUM_COLS,xpg_word_array(gtypes)})
        c_proc(gtk_tree_view_set_model,{tree_view,tree_store})
        c_proc(xg_object_unref,{tree_store})

        atom iter = allocate(32,true)
        xpg_tree_add_nodes_rec(tree_nodes,tree_view,{tree_store,iter,null})

        atom sandle = c_func(gtk_scrolled_window_new,{NULL,NULL}) 
        c_proc(gtk_scrolled_window_set_policy,{sandle,GTK_POLICY_AUTOMATIC,GTK_POLICY_AUTOMATIC})
        c_proc(gtk_container_add,{sandle,tree_view})
        c_proc(gtk_container_add,{ctrl_handles[parent],sandle})
    else -- winAPI
        atom dwStyle = or_all({WS_CHILD,WS_VISIBLE,WS_TABSTOP,TVS_HASLINES,TVS_LINESATROOT,TVS_HASBUTTONS}),
                   d = CW_USEDEFAULT
        tree_view = xpg_WinAPI_create(id,"SysTreeView32","",parent,d,d,dwStyle,WS_EX_CLIENTEDGE)
        {} = c_func(xSendMessage,{tree_view,TVM_SETIMAGELIST,TVSIL_NORMAL,tree_himl})
        xpg_tree_add_nodes_rec(tree_nodes, tree_view, 0)
    end if
    xpg_setID(tree_view,id)
    if length(attributes) then
        gSetAttributes(id,attributes,args)
    end if
end procedure

--[DOCS]function gTreeView([gdx parent,] sequence tree_nodes, [integer branchopen=NULL,] string attributes="", sequence args={})
--global function gTreeView(gdx parent, sequence tree_nodes, object branchopen=NULL, sequence attributes="", args={})
global function gTreeView(object tree_nodes={}, branchopen=NULL, sequence attributes="", args={})
--  {branchopen,attributes,args} = paranormalise_raa(branchopen,attributes,args)
--DEV:
    {tree_nodes,branchopen,attributes,args} = paranormalise_qraa(tree_nodes,branchopen,attributes,args)
    if not bInit then xpg_Init() end if
    integer id = xpg_add_control(TREEVIEW)
    create_rids[id] = xpg_TreeView
    create_args[id] = {id,tree_nodes,attributes,args}
    if branchopen!=NULL then
        gSetHandler(id,"BRANCHOPEN",branchopen)
    end if
    return id
end function 

local function xpg_gtk_button_clicked(atom handle, /*user_data*/) -- (GTK only)
    -- (aside: not really worth checking handle is an xpg_handle)
    integer id = xpg_getID(handle),
        click = getd({id,"CLICK"},handlers)
    if click then
        integer res = click(id)
        if res=XPG_CLOSE then
--          xpg_close_window(xpg_get_parent_window(id))
            gHide(xpg_get_parent_window(id))
        end if
    end if
    return 0 -- (ignored)
end function
local constant gtk_button_clicked_cb = call_back({'+',xpg_gtk_button_clicked})

local function xpg_WinAPI_GetTextExtentPoint32(string text, gdx parent, integer padx=0, pady=0)
    atom pHwnd = ctrl_handles[parent],
           hDC = c_func(xGetDC,{pHwnd})
    bool bOK = c_func(xGetTextExtentPoint32,{hDC,text,length(text),pSIZE})
    assert(bOK)
    integer w = get_struct_field(idSIZE,pSIZE,"cx")+padx,
            h = get_struct_field(idSIZE,pSIZE,"cy")+pady
    bOK = c_func(xReleaseDC, {pHwnd, hDC})
    assert(bOK)
    return {w,h}
end function

local procedure xpg_Button(gdx id, string title, attributes, sequence args)
    -- (invoked via xpg_map)
    gdx parent = parent_ids[id]
    atom button
    if bUseGTK then
--DOCS, maybe (I made this up) note the child of a dialog should normally be a vbox or hbox; should
--      you create, for instance, a dialog with a single button child, it will expand to fill the
--      entire dialog.
--As a (temporary?) workaround, I've stuck a v/hbox in:
        button = c_func(gtk_button_new_with_mnemonic,{title})
--      c_proc(gtk_container_add,{ctrl_handles[parent],button})
        c_proc(g_signal_connect_data,{button,"clicked",gtk_button_clicked_cb,0,0,0}) 
--(temp?:)
        atom vbox = c_func(gtk_vbox_new,{false,6})
        atom hbox = c_func(gtk_hbox_new,{false,6})
        c_proc(gtk_box_pack_start,{hbox,button,false,false,0})
        c_proc(gtk_box_pack_start,{vbox,hbox,false,false,0})
        c_proc(gtk_container_add,{ctrl_handles[parent],vbox})
    else -- winAPI
        atom dwStyle = or_all({WS_CHILD,WS_VISIBLE,BS_PUSHBUTTON,WS_TABSTOP}),
               {w,h} = xpg_WinAPI_GetTextExtentPoint32(title,parent,26,14)
        natural_width[id] = w
        natural_height[id] = h
        button = xpg_WinAPI_create(id,"button",title,parent,w,h,dwStyle,0)
    end if
    xpg_setID(button,id)
    if length(attributes) then
        gSetAttributes(id,attributes,args)
    end if
end procedure

--/*
    gButton()                                                                 -- [std defaults apply]
    gButton(title)
    gButton(title,click)                            -- [assuming there is a function click somewhere]
    gButton(title,attributes[,args])                                    -- [detected via string(rid)]
    gButton(title,click,attributes[,args])                                      -- [ie the full args]
    gButton(click)                                                   -- [detected via integer(title)]
    gButton(click,attributes[,args])                             -- [detected via "" and string(rid)]
    gButton(attributes,args)                                          -- [detected via sequence(rid)]
--  gButton(attributes)                                        -- [invalid/treated as gButton(title)]
    gButton(attributes,{})                                                   -- [but that's ok again]
--*/        

global function gButton(nullable_string title=NULL, object click=NULL, sequence attributes="", args={})
    {title,click,attributes,args} = paranormalise_traa(title,click,attributes,args)
    if not bInit then xpg_Init() end if
    integer id = xpg_add_control(BUTTON)
    create_rids[id] = xpg_Button
    create_args[id] = {id,title,attributes,args}
    if click!=NULL then
        gSetHandler(id,"CLICK",click)
    end if
--  gSetAttribute(id,"EXPAND",false)
--  gSetInt(id,"EXPAND",false)
    return id
end function 

--static gboolean gtk_draw(GtkWidget*widget,cairo_t*cr,gpointer data)
function xpg_gtk_draw(atom canvas, cairo, /*data*/)
    gdx id = xpg_getID(canvas)
--  ??[id] = cairo -- maybe...
--/*
 /* unselected fill in yellow */
  cairo_set_source_rgba(cr,0.8,0.8,0,1),
void
gdk_cairo_set_source_rgba (
  cairo_t* cr,
  const GdkRGBA* rgba
)
    cairo_move_to(cr,x,y);
    cairo_line_to(cr,x,y);
    cairo_close_path(cr);
  cairo_fill(cr);

--  cairo_fill_preserve(cr);

--  cairo_set_line_width (cr, 3.0);
--  cairo_stroke(cr);

--  cairo_move_to(cr,x,Cy)
--  cairo_show_text(cr,text)
--  cairo_stroke(cr); (??)
--*/
    return true
end function
local constant xpg_gtk_draw_cb = call_back({'+',xpg_gtk_draw})

local procedure xpg_Canvas(gdx id, string title, attributes, sequence args)
    -- (invoked via xpg_map)
    gdx parent = parent_ids[id]
    atom canvas
    if bUseGTK then
        canvas = c_func(gtk_drawing_area_new,{})
        c_proc(gtk_widget_set_events,{canvas,GDK_BKE_MASK})
        c_proc(gtk_container_add,{ctrl_handles[parent],canvas})
        c_proc(g_signal_connect_data,{canvas,"draw",xpg_gtk_draw_cb,0,0,0}) 
-- create_pango_layout
--gtk_widget_create_pango_layout
--get_size

    else -- winAPI
--      atom dwStyle = or_all({WS_CHILD,WS_VISIBLE,BS_PUSHBUTTON,WS_TABSTOP}),
--             {w,h} = xpg_WinAPI_GetTextExtentPoint32(title,parent,26,14)
        canvas = NULL   -- or maybe getDC?
--       backDC = c_func(xCreateCompatibleDC, {NULL}),  -- the background

        atom {w,h} = xpg_WinAPI_GetTextExtentPoint32("W",parent)
        natural_width[id] = w
        natural_height[id] = h
--      canvas = xpg_WinAPI_create(id,"canvas",title,parent,w,h,dwStyle,0)
    end if
    xpg_setID(canvas,id)
    if length(attributes) then
        gSetAttributes(id,attributes,args)
    end if
end procedure

global function gCanvas(object redraw=NULL, sequence attributes="", args={})
    {redraw,attributes,args} = paranormalise_raa(redraw,attributes,args)
    if not bInit then xpg_Init() end if
    integer id = xpg_add_control(CANVAS)
    create_rids[id] = xpg_Canvas
    create_args[id] = {id,attributes,args}
    if redraw!=NULL then
        gSetHandler(id,"REDRAW",redraw)
    end if
    return id
end function 

-- https://stackoverflow.com/questions/5561236/win32-splitter-control

--DEV oh, almost... we want to collect multiple groups here, in a way that lets us 
--  figure out/update the normal size (and I've yet got to store that somewhere!)
--  almost certainly in a post-traversal/creation second pass. So, "" first. [DONE, ish]

--integer next_ng = 0
--
--global procedure gNormalise(string hvorboth, sequence elements)
--  integer hvb = find(hvorboth,{"HORIZONTAL","VERTICAL","BOTH"})
--X next_ng += 1
--X for e in elements do
--X     if and_bits(hvb,0b01) then gSetAttribute(e,"NORMH",next_ng) end if
--X     if and_bits(hvb,0b10) then gSetAttribute(e,"NORMV",next_ng) end if
--X end for
--  ngflags &= hvb
--  ngsets = append(ngsets,elements)
--end procedure

global procedure gMap(gdx id, integer level=0)
    --
    -- What this really addresses is that WinAPI demands parents are created first
    -- and specified as the child elements are created, however it is more natural
    -- to define say text, checkbox, and hbox({text,checkbox}), then dialog(hbox),
    -- hence actual control creation is deferred until the gShow(dialog) at which
    -- point it can/shoulebe able to create things dialog-first.
    --
    assert(and_bits(ctrl_flags[id],CF_MAPPED)=0)
--bUseMapping...
--  ?9/0
    call_proc(create_rids[id],create_args[id])
    assert(and_bits(ctrl_flags[id],CF_MAPPED)!=0)
--DEV:
--  apply_deferred_attributes(id,ctrl_extra[id][CE_ACHAIN])
    object children = children_ids[id]
    if sequence(children) then
        for child in children do
            gMap(child,level+1)
        end for
    end if
--DEV rescan for sizes...
    if level=0 then
--?"calculate natural sizes..."
    end if
end procedure

-- gShow x and y less than #FFF0 (65520) are treated as actual pixel locations,
-- otherwise the low 4 bits determine the parent/mouse-relative positioning:
--                                                PBT (for y)
--                                                 RL (for x)
global constant XPG_CURRENT      = #FFF0,   -- 0b0000   -- 65520
                XPG_LEFT         = #FFF1,   -- 0b0001   -- 65521
                XPG_RIGHT        = #FFF2,   -- 0b0010   -- 65522
                XPG_CENTER       = #FFF3,   -- 0b0011   -- 65523
                XPG_MOUSEPOS     = #FFF4,   -- 0b0100   -- 65524
                XPG_LEFTPARENT   = #FFF5,   -- 0b0101   -- 65525
                XPG_RIGHTPARENT  = #FFF6,   -- 0b0110   -- 65526
                XPG_CENTERPARENT = #FFF7,   -- 0b0111   -- 65527
                XPG_TOP          = XPG_LEFT,            -- (=65521)
                XPG_TOPPARENT    = XPG_LEFTPARENT,      -- (=65525)
                XPG_BOTTOM       = XPG_RIGHT,           -- (=65522)
                XPG_BOTTOMPARENT = XPG_RIGHTPARENT      -- (=65526)

local constant XPG_PARENT        = #0004    -- 0b0100

local function xpg_placement(integer xy, os, ol, is, il)
    -- xy is the x or y of gShow(), which can be an absolute value such as 100 (returned as is)
    -- or one of the XPG_XXX - which obviously need the outer and inner starts and lengths.
    integer res = is
    if xy<XPG_CURRENT then
        res = xy
    elsif xy>XPG_CURRENT then
        switch xy && 0b0011 do
            case 0b11: res = os+floor((ol-il)/2)    -- centre
            case 0b10: res = os+ol-il               -- right/btm
            case 0b01: res = os                     -- left/top
--          case 0b00: res = 9/0                    -- mousepos [DEV]
            case 0b00: res = is-iff(is+il<=ol?0:il) -- mousepos [DEV untested]
        end switch
--      if xy && 0b001 then
--          if xy && 0b010 then
--              res = os+floor((ol-il)/2)   -- centre
--          else
--              res = os                    -- left/top
--          end if
--      elsif xy && 0b010 then
--          res = os+ol-il                  -- right/btm
--      end if
    end if
    return res
end function

global procedure gShow(gdx id, integer x=XPG_CURRENT, y=XPG_CURRENT)
    assert(ctrl_types[id]=DIALOG)
    if and_bits(ctrl_flags[id],CF_MAPPED)=0 then
        gMap(id)
    end if
    atom hwnd = ctrl_handles[id]
    integer pid = parent_ids[id],
        {sx,sy} = {0,0},
        {sw,sh} = gGetGlobalIntInt("SCREENSIZE")
    -- catch the handle of the first window displayed (nb arwen saves first created)
    if PrimaryWindowID=UNDEFINED then
        assert(pid=0,"first window displayed must be parent-less")
        PrimaryWindowID = id
    end if
    if pid!=0
    and (and_bits(x,XPG_PARENT) or
         and_bits(y,XPG_PARENT)) then
        integer {px,py,pw,ph} = xpg_get_window_rect(pid)
        if and_bits(x,XPG_PARENT) then {sx,sw} = {px,pw} end if
        if and_bits(y,XPG_PARENT) then {sy,sh} = {py,ph} end if
    end if  
    if and_bits(ctrl_flags[id],CF_NEVER_SHOWN) then
        if x=XPG_CURRENT then x=XPG_CENTER end if
        if y=XPG_CURRENT then y=XPG_CENTER end if
        ctrl_flags[id] -= CF_NEVER_SHOWN
    end if  
--?"gwr"
--  integer {wx,wy,ww,wh} = xpg_get_window_rect(id)
    sequence gwr = xpg_get_window_rect(id)
    integer {wx,wy,ww,wh} = gwr
    if x=XPG_MOUSEPOS
    or y=XPG_MOUSEPOS then
        assert(x==y,"gShow: XPG_MOUSEPOS is both-only")
        ?9/0 -- (placeholder)
--      {wx,wy} = gGetGlobalIntInt("MOUSEPOS") -- (nee CURSORPOS)
    end if
    wx = xpg_placement(x,sx,sw,wx,ww)
    wy = xpg_placement(y,sy,sh,wy,wh)
--?{"win/rect",{h,gwr},{x},wx,ww,{y},wy,wh}
    xpg_move_window(id,wx,wy)
--?"moved"
--DEV setAttribute(h,"VISIBLE",true)...
    if bUseGTK then
        if c_func(gtk_window_get_transient_for,{hwnd})=NULL then
--          if XPG_PARENTDIALOG!=NULL
--          and XPG_PARENTDIALOG!=h then
--              c_proc(gtk_window_set_transient_for,{h,XPG_PARENTDIALOG}) 
--          else
--              integer sigid = c_func(g_signal_connect_data,{h,"destroy",gtk_main_quit_cb,0,0,0}) 
                c_proc(g_signal_connect_data,{hwnd,"destroy",gtk_main_quit_cb,0,0,0}) 
--g_signal_connect_data "destroy"
--          end if
        end if
--      c_proc(gtk_widget_show,{id})
        c_proc(gtk_widget_show_all,{hwnd})
    else
        c_proc(xShowWindow,{hwnd,SW_SHOWNORMAL})
        c_proc(xUpdateWindow,{hwnd})
    end if
end procedure

--gtk3+: (from https://stackoverflow.com/questions/50076541/override-icons-used-in-the-tree-view )
local constant gtk_css_text = """
treeview.view.expander { -gtk-icon-source: -gtk-icontheme("zoom-in-symbolic"); color: #4d4d4d; }
treeview.view.expander:dir(rtl) { -gtk-icon-source: -gtk-icontheme("pan-end-symbolic-rtl"); }
treeview.view.expander:hover { color: black; }
treeview.view.expander:selected { color: #c9def4; }
treeview.view.expander:selected:hover { color: #ffffff; }
treeview.view.expander:selected:backdrop { color: #c7dcf2; }
treeview.view.expander:checked { -gtk-icon-source: -gtk-icontheme("zoom-out-symbolic"); }
treeview.view.expander:backdrop { color: #adafb0; }"""

local constant GTK_STYLE_PROVIDER_PRIORITY_APPLICATION = 600

global procedure gMainLoop()
    if bUseGTK then
--/!*
        if gtk_css_provider_new!=-1 then    -- (available on 3.0+ only)
            atom screen = c_func(gdk_screen_get_default,{}),
                 provider = c_func(gtk_css_provider_new,{})
            c_proc(gtk_css_provider_load_from_data,{provider,gtk_css_text,-1,NULL})
            c_proc(gtk_style_context_add_provider_for_screen,{screen,provider,GTK_STYLE_PROVIDER_PRIORITY_APPLICATION})
--Squared Plus  &#8862; &#x229E;
--Squared Minus   &#8863; &#x229F;
        end if
--*!/
        c_proc(gtk_main)
    else
        while c_func(xGetMessage,{pMSG,NULL,0,0}) do
            c_proc(xTranslateMessage,{pMSG})
            c_proc(xDispatchMessage,{pMSG})
        end while
    end if
end procedure

global function gMainLoopf()
    gMainLoop()
    return true
end function  

last_xpgui_rid = gMainLoopf
--/*
--one off the mailing list: (we definately need a natural size (and always have), at least for win32)
  Ihandle *dlg, *frame, *button;
   
  IupOpen(NULL, NULL);
   
  button = IupButton("Button", NULL);
   
  frame = IupFrame(button);
  IupSetAttribute(frame, "TITLE", "A title wider than the button");
 
  dlg = IupDialog(frame);
  IupSetAttribute(dlg, "TITLE", "Test bug length of frame when title is set");
  IupSetAttribute(dlg, "SIZE", "200x80");

  IupShowXY(dlg, IUP_CENTER, IUP_CENTER);
  IupMainLoop();
  IupClose();
-- one reply (which I think rather misses the point!), use a v/hbox: 
  Ihandle *dlg, *frame, *button;
  Ihandle *hbox;
  Ihandle *vbox;

  IupOpen(NULL, NULL);

  button = IupButton("Button", NULL);

  hbox = IupHbox(button, NULL);
  IupSetAttribute(hbox, "SIZE", "200x80");

  frame = IupFrame(hbox);
  IupSetAttribute(frame, "TITLE", "A title wider than the button");

  vbox = IupVbox(frame, NULL);
  dlg = IupDialog(vbox);
  IupSetAttribute(dlg, "TITLE", "Test bug length of frame when title is set");
  IupSetAttribute(dlg, "SIZE", "200x80");

  IupShowXY(dlg, IUP_CENTER, IUP_CENTER);
  IupMainLoop();
  IupClose();
--*/

--/*
--constant GTK_STATE_FLAG_NORMAL = 0,
--       GTK_STATE_FLAG_SELECTED = 4
--*/

