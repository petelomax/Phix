--
-- demo\gui\xpGUI.e
-- ================
--
constant COPYRIGHT = "Copyright (C) 2023 Pete Lomax / Open Software License version 3.0"
--                   "Copyright (C) 2023-2024 Pete Lomax / Open Software License version 3.0"
--
integer test_elem = 0
-- **** DEV **** try making this all work **without** c_func/c_proc, but that #ilASM{} thing...
-- (is that going to be a bucketload harder, cos this don't rely on #ilASM{[PE]|[ELF]} guards??)
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
-- Maybe: https://cboard.cprogramming.com/c-programming/153029-revised-gtkplus-text-editor-more-stable.html

without debug
include pprntfN.e
include cffi.e
include pcfunc.e
include scanf.e
include ubits.e
with debug -- (leaving ths commented out often makes asserts etc more helpful)

--with trace

local bool bInit = false -- (xpg_Init() not yet called)
local constant SPLAT = {LINUX,WINDOWS},
               backdesc = {"GTK","WinAPI"},
               GTK = 1, WinAPI = 2, -- must match ""
               UNDEFINED = 0
--local enum GTK, WinAPI
local integer PrimaryWindowID = UNDEFINED,  -- id of the main application window
              handlers,                     -- key is {gdx,name}, data is integer routine_id
              handler_sigs,                 -- key is {ct,name}, data is allowed sig[1..3]
              backend = find(platform(),SPLAT)
--            backend = iff(platform()= LINUX  ? GTK
--                     :iff(platform()=WINDOWS ? WinAPI
--                     :9/0))
global procedure gUseGTK()
    assert(not bInit)
    backend = GTK
end procedure

-- Aside: This source is littered with
--
--      if backend=GTK then
--          ...
--      elsif backend=WinAPI then
--          ...
--      else
--          ?9/0 -- (unknown backend)
--      end if
--
-- While only a few of those ?9/0 can actually trigger now, mid-way through adding say Qt
--  they should theoretically be helpful to locate "what next to get next demo working".
--


local constant control_set = {{       DIALOG:=$,1,"Dialog"      },
                              {          BOX:=$,2,"Box"         },
                              {       BUTTON:=$,0,"Button"      },
                              {       CANVAS:=$,0,"Canvas"      }, -- (also gGraph/gList/gTable)
                              {     CHECKBOX:=$,0,"Checkbox"    },
                              {    CLIPBOARD:=$,0,"Clipboard"   },
                              {     DATEPICK:=$,0,"DatePick"    },
                              {     DROPDOWN:=$,0,"DropDown"    },
                              {        FRAME:=$,1,"Frame"       },
                              {        GRAPH:=$,0,"Graph"       }, -- (in [CX_CANVAS_TYPE] only)
                              {        LABEL:=$,0,"Label"       },
                              {         LIST:=$,0,"list"        }, -- (in [CX_CANVAS_TYPE] only)
                              {         MENU:=$,0,"Menu"        },
--                            {      MENUBAR:=$,0,"MenuBar"     },
--                            {     MENUITEM:=$,0,"MenuItem"    },
--                            {      MENUSEP:=$,0,"MenuSep"     },
--                            {      SUBMENU:=$,0,"SubMenu"     },
                              {  PROGRESSBAR:=$,0,"ProgressBar" },
                              {       SLIDER:=$,0,"Slider"      },
                              {         TABS:=$,3,"Tabs"        },
                              {        TABLE:=$,0,"Table"       }, -- (in [CX_CANVAS_TYPE] only)
                              {         TEXT:=$,0,"Text"        },
                              {        TIMER:=$,0,"Timer"       },
--                            {       TOGGLE:=$,0,"Toggle"      },
                              {     TREEVIEW:=$,0,"TreeView"    }},
                       lcs = length(control_set),
                 ctrl_kids = vslice(control_set,2), -- (controls that have children, 0b01=has decorations)
                ctrl_names = vslice(control_set,3), -- (for error messages only)
--                ctrl_rdx = reinstate({},vslice(control_set,1),{}), -- (reverse lookup?)
                  cf_glags = {{     CF_EXPANDH:=0x00000001, "CF_EXPANDH"      },
                              {     CF_EXPANDV:=0x00000002, "CF_EXPANDV"      },
--SUG:
--                            {     CF_EXPANCH:=0x00000004, "CF_EXPANDH"      },    -- (accumulated from child elements)
--                            {     CF_EXPANCV:=0x00000008, "CF_EXPANDV"      },    -- (accumulated from child elements)
                              {      CF_MAPPED:=0x00000010, "CF_MAPPED"       },    -- (re-used for "Active" on timers)
                              {    CF_INACTIVE:=0x00000020, "CF_INACTIVE"     },
                              { CF_NEVER_SHOWN:=0x00000040, "CF_NEVER_SHOWN"  },    -- (only meaningful on dialogs)
                              {CF_CLOSE_ON_ESC:=0x00000080, "CF_CLOSE_ON_ESC" },    -- (only meaningful on dialogs)
--DEV (ctrl_kids prob. better)
--                            {        CF_LEAF:=0x00000100, "CF_LEAF"         },    -- (controls which have no children)
                              {    CF_VERTICAL:=0x00000100, "CF_VERTICAL"     },    -- (only meaningful on some controls*)
-- (NO!)                      {     CF_NATURAL:=0x00000200, "CF_NATURAL"      },    -- (only meaningful on some controls)
--                            {       CF_SPLIT:=0x00000200, "CF_SPLIT"        },    -- (only meaningful on BOX controls)
--                            {       CF_MULTI:=0x00000400, "CF_MULTI"        },    -- (only meaningful on BOX controls)
                              {      CF_RESIZE:=0x00000800, "CF_RESIZE"       },    -- (only meaningful on gDialog ctrls)

--DEV erm, NO: just put expand=NO on the container??!!
--                            {        CF_FREE:=0x00001000, "CF_FREE"         },
--                            {     CF_EX_FREE:=0x00001300, "CF_EX_FREE"      },    -- or_all({CF_EXPANDH,CF_EXPANDV,CF_FREE})
--                            {       CF_EX_HV:=0x00000300, "CF_EX_HV"        },    -- or_all({CF_EXPANDH,CF_EXPANDV})
                              {      CF_SHRINK:=0x00002000, "CF_SHRINK"       },
                              {      CF_NORMAL:=0x00010000, "CF_NORMAL"       },    -- ctrl is part of a normaliser group
                              {       CF_RADIO:=0x00020000, "CF_RADIO"        },    -- ctrl is part of a radio group
--                            {       CF_ACCEL:=0x00040000, "CF_ACCEL"        },    -- ctrl has an accelerator key (??) DEV/SUG
                              {  CF_IGNORESIZE:=0x04000000, "CF_IGNORESIZE"   },    -- (ignore [recursive] resize events)
                              {   CF_UNMAPATTR:=0x08000000, "CF_UNMAPATTR"    }}    -- (these handle unmapped attributes)
--                  CF_EHV = or_bits(CP_EXPANDH,CF_EXPANDV)

--*CF_VERTICAL distinguishes a gVbox from a gHbox, and is ORIENTATION=VERTICAL for gProgressBar and gSlider

--indexed by [DIALOG..TEEVIEW][map/set/get],
--the routine info (2nd column) is for diagnostics only:
local sequence ctrl_msg = repeat(0,lcs)
local enum CM_MAP, CM_MRI,              -- map/create
           CM_SET, CM_SRI,              -- set attributes
           CM_GET, CM_GRI,              -- get attributes   
           CM_LEN=$

local procedure set_ctrl_msg(integer ct, rmap, rset, rget)
    -- aside: get_routine_info(0,true) yields {0,0,"P",-1}
    sequence cmct = repeat(0,CM_LEN)
    cmct[CM_MAP] = rmap
    cmct[CM_SET] = rset
    cmct[CM_GET] = rget
    cmct[CM_MRI] = get_routine_info(rmap,true)
    cmct[CM_SRI] = get_routine_info(rset,true)
    cmct[CM_GRI] = get_routine_info(rget,true)
    ctrl_msg[ct] = cmct
end procedure

--indexed by gdx:
local sequence ctrl_handles = {},   -- (native handles)
                 ctrl_types = {},   -- (also ctrl_free_list)
                 ctrl_flags = {},   -- (CF_XXX settings, see above)
                 parent_ids = {},   -- (as gdx, nb see xpg_get_parent_handle)
               children_ids = {},   -- (as gdx) (also cairo context?)
                  ctrl_size = {},   -- (see SZ_XXX below)
--                  ctrl_mp = {},   -- (margin/padding, see MP_XXX below)
                  ctrl_font = {},   -- (UNDEFINED or index to fontcache)
                 ctrl_fonts = {},   -- (NULL or string)
--              ctrl_styles = {},   -- XPG_NORMAL/(NULL or string)
                 ctrl_fontd = {},   -- (NULL?, pango fontdesc, WinAPI hFont atom)
                  ctrl_xtra = {},   -- (control-specific, see CX_XXX below)
              wnd_proc_addr = {},   -- (WinAPI controls only)
--            sub_proc_addr = {},   --       ""
              deferred_attr = {},   -- ({name,val} pairs)
              def_menu_attr = {}    -- ({ids},{name,val} pair-sets)

--DEV/SUG:
--/!*
-- natural: includes decorations such as border and title, but not content.
--          A parent needs "sum/max" of nested offspring naturalsizes, btw.
-- user: as requested/set by [raster]size, or zero if not specifically set.
-- w, h: actual, as calculated, should match results from querying the API.
-- min/max may also be required/stored here.
local enum SZ_W,
           SZ_H,
           SZ_NATURAL_W,
           SZ_NATURAL_H,
--DEV normalised naturals...
--DEV just use SZ_W, SZ_H ???
           SZ_NORMAL_W,
           SZ_NORMAL_H,
--DEV shrinkage limits...
--         SZ_SHRINK_W,
--         SZ_SHRINK_H,
--DEVor maybe [SZ_MIN][SZ_W], [S_MIN][SW_H], etc?
--         SZ_MIN,  -- 0 or {[SZ_W],[SZ_H]}
--         SZ_MAX,  -- ""
           SZ_MIN_W,
           SZ_MIN_H,
           SZ_MAX_W,
           SZ_MAX_H,
           SZ_USER_W,
           SZ_USER_H,
--DEV x,y (or just calculate as we go, why not...)
           SZ_X,
           SZ_Y,
-- margins/padding (trbl), erm...
--         SZ_MARGIN_TOP,
--         SZ_MARGIN_RGT,
--         SZ_MARGIN_BTM,
--         SZ_MARGIN_LFT,
           SZ_MARGIN,   -- (see MP_XXX below)
           SZ_PADDING,  -- "", BUTTON/DATEPICK/DROPDOWN/TEXT only
           SZ_LENGTH = $

local enum CX_LAST_FOCUS,
--abandoned (for now)
--         CX_DEF_ENTER,
--         CX_DEF_ESCAPE,
           CX_DLG_LEN = $

--local enum MP_MARGIN,
--         MP_PADDING
local enum MP_TOP,
           MP_RGT,
           MP_BTM,
           MP_LFT

local enum --CX_BOX_MARGIN, -- trbl
           CX_BOX_GAP,
           CX_BOX_SPACE,
           CX_BOX_LEN = $
--local enum CX_BM_TOP,
--         CX_BM_RIGHT,
--         CX_BM_BOTTOM,
--         CX_BM_LEFT

local constant CXCF_REPEN = 0b01
--             CXCF_REFONT = 0b10 -- (Windows only)
--DEV:
--             CXCF_SCROLL = 0b100 -- (creation only)
local enum CX_CANVAS_TYPE,      -- CANVAS/GRAPH/LIST/TABLE...
           CX_CANVAS_FLAGS,     -- CXCF_REPEN (etc)
           CX_CANVAS_BACK,      -- eg XPG_PARCHMENT (default XPG_WHITE)
           CX_CANVAS_FORE,      -- default XPG_BLACK
           CX_CANVAS_HDC,       -- WinAPI:hDC, GTK:{cairo,layout}
           CX_PENSTYLE,         -- eg/default XPG_CONTINUOUS
           CX_PENWIDTH,         -- default 1
           CX_TXTANGLE,         -- default 0 (degrees, not radians)
           CX_GTL_ATTRS,        -- (gGraph/gTable/gList, see GX/LX/TX below)
           CX_CANVAS_LEN = $    -- (init ctrl_xtra[id] this size)

-- for gGraph():
local constant graph_attrs = {{      GX_GRID:=$,'B',"GRID"          },
                              { GX_GRIDCOLOR:=$,'I',"GRIDCOLOR"     },
                              { GX_LEGENDBOX:=$,'B',"LEGENDBOX"     },
                              {     GX_XTICK:=$,'N',"XTICK"         },
                              {     GX_YTICK:=$,'N',"YTICK"         },
                              {      GX_XMIN:=$,'N',"XMIN"          },
                              {      GX_XMAX:=$,'N',"XMAX"          },
                              {      GX_YMIN:=$,'N',"YMIN"          },
                              {      GX_YMAX:=$,'N',"YMAX"          },
                              {   GX_XMARGIN:=$,'N',"XMARGIN"       },
                              {   GX_YMARGIN:=$,'N',"YMARGIN"       },
                              {   GX_XYSHIFT:=$,'N',"XYSHIFT"       },
                              {   GX_YXSHIFT:=$,'N',"YXSHIFT"       },
                              {    GX_XANGLE:=$,'N',"XANGLE"        },
                              {    GX_YANGLE:=$,'N',"YANGLE"        },
                              {    GX_GTITLE:=$,'S',"GTITLE"        },
                              {     GX_XNAME:=$,'S',"XNAME"         },
                              {     GX_YNAME:=$,'S',"YNAME"         },
                              {  GX_XTICKFMT:=$,'S',"XTICKFMT"      },
                              {  GX_YTICKFMT:=$,'S',"YTICKFMT"      },
                              {      GX_MODE:=$,'S',"MODE"          },
                              {   GX_BARMODE:=$,'S',"BARMODE"       },
                              { GX_MARKSTYLE:=$,'S',"MARKSTYLE"     },
                              {  GX_MARKSIZE:=$,'I',"MARKSIZE"      },
                              {   GX_XACROSS:=$,'B',"XACROSS"       },
                              {   GX_YACROSS:=$,'B',"YACROSS"       },
--                            {      GX_XRID:=$,'I',"XRID"          },
--                            {      GX_YRID:=$,'I',"YRID"          },
                              { GX_LEGENDPOS:=$,'S',"LEGENDPOS"     },
                              {  GX_LEGENDXY:=$,'P',"LEGENDXY"      }},
                    GX_LEN = length(graph_attrs),
              grattr_types = vslice(graph_attrs,2),
              grattr_names = vslice(graph_attrs,3)
--assert(GX_GRID=1)
--assert(GX_LEGENDXY=GX_LEN)
if GX_LEGENDXY or GX_LEGENDPOS then end if -- DEV currently unused...

local enum CX_TABTITLES,
           CX_TABIMAGES,
           CX_TABLENGTH = $ -- (init ctrl_xtra[id] this size)

-- for gTable():
-- (first 5 & last 3 are creation-only or internal and cannot be set via gSetAttribute)
local constant table_attrs = {{   TX_COLUMNS:=$,'X',"?COLUMNS?"     },
--DEV might yet make this get_data-style...
                              {      TX_DATA:=$,'X',"?DATA?"        },
                              {   TX_ACTCOLS:=$,'X',"?ACTCOLS?"     },
                              {    TX_TAGSET:=$,'X',"?TAGSET?"      },
                              {  TX_SORTCOLS:=$,'X',"?SORTCOLS?"    },
--NO: scrollbar wants to ovelap titles, [x]pGUI.js arguably has it wrong.
--X                           {   TX_TCANVAS:=$,'X',"?TCANVAS?"     },
--X                           {   TX_BCANVAS:=$,'X',"?BCANVAS?"     },
--                            {       TX_XXX:=$,'?',"XXX"           },
                              {      TX_ROWS:=$,'X',"?ROWS?"        }, -- (visible lines)
--DEV why not just use SZ_NATURAL_W?
                              {  TX_NATHIGHT:=$,'X',"?NAT_HIGHT?"   },
                              {  TX_NATWIDTH:=$,'X',"?NAT_WIDTH?"   }},
                    TX_LEN = length(table_attrs),
              t_attr_types = vslice(table_attrs,2),
              t_attr_names = vslice(table_attrs,3)

-- for gList():
local constant list_attrs = {{      LX_DATA:=$,'O',"DATA"       },
                             {LX_AUTOSCROLL:=$,'B',"AUTOSCROLL" },
--DEV we'll need these, and probably more:
--                           {     LX_COUNT:=$,'I',"COUNT"      },
--                           {  LX_MULTIPLE:=$,'B',"MULTIPLE"   },
--                           {      LX_ROWS:=$,'I',"ROWS"       },
--                           {   LX_TOPITEM:=$,'O',"TOPITEM"    },
--                           {  LX_SELECTED:=$,'O',"SELECTED"   },
--DEV is this that? (next really prev)
                             {    LX_VALINT:=$,'I',"VALINT"     }},
                    LX_LEN = length(list_attrs),
              l_attr_types = vslice(list_attrs,2),
              l_attr_names = vslice(list_attrs,3)
if LX_AUTOSCROLL then end if -- DEV currently unused...

local constant gtl_sets = {{GRAPH,grattr_names,grattr_types},
                           { LIST,l_attr_names,l_attr_types},
                           {TABLE,t_attr_names,t_attr_types}},
              {gtl_types,
               gtl_names,
               gtl_sigs} = columnize(gtl_sets)

local sequence norm_groups = {},    -- gNormalise() groups
              radio_groups = {}     -- gRadio() groups

--*!/
-- general routine naming conventions:
--  gXxxx: global routines, cross platform
--  xpg_gtk_xxx: local, gtk only
--  xpg_WinAPI_xxx: local, WinAPI only
--  xpg_xxx: local, cross_platform
--  a_xxx: cribs from arwen, to go. (none left at the mo...)

-- types:
--  xpg_handle: (local) a handle in ctrl_handles[], as returned by eg gtk_window_new or CreateWindowEx
--  gdx: an index to the internal tables, such as ctrl_handles[], and certainly not any xpg_handles.
--  rtn: NULL or the routine identifier (aka integer index into symtab) of a suitable handler routine.

local type xpg_handle(atom h) -- (esp: not a gdx by mistake)
    -- aside: falls into the "technically unsafe" category, as in many things that
    --        are "obviously" not may pass (eg #2000000), but hopefully worthwhile.
    return h=NULL or h>length(ctrl_handles) or find(h,ctrl_handles)
end type

global type gdx(object hdx)
    integer l = length(ctrl_handles)
    if sequence(hdx) then
        if string(hdx) then return false end if
        for h in hdx do
            if not (integer(h) and h>=0 and h<=l) then return false end if
        end for
        return true
    end if
    return integer(hdx) and hdx>=0 and hdx<=l
end type

global type gdc(object h)
    -- a single gdx from gCanvas or a single gImage.
--  if atom(h) then return h!=NULL and gdx(h) and ctrl_types[h]=CANVAS and ctrl_xtra[h][CX_GTL_ATTRS]=0 end if
    if atom(h) then return h!=NULL and gdx(h) and ctrl_types[h]=CANVAS end if
--  return sequence(h) and length(h)=2 and h[1]="gImage" and atom(h[2])
    return sequence(h) and length(h)=2 and h[1]="gImage"
end type

local integer last_xpgui_rid = gdx, -- (overwritten with gMainLoop[or whatever] later on in this file)
              rtn_default_drop = 0,
              rtn_graph_redraw = 0,
              rtn_table_redraw = 0,
              rtn_list_redraw = 0

--DEV better off saying >rtn and get_routine_info() says ok??
global type rtn(object rid)
    return integer(rid) and (rid=NULL or rid>last_xpgui_rid or find(rid,{rtn_default_drop,rtn_graph_redraw,rtn_table_redraw,rtn_list_redraw}))
end type

local integer ctrl_free_list = 0

local function xpg_add_control(integer ctrl_type, parent_id=0, flags=0, bool bHasChildren=false)
--
--  returns: a gdx
--  control_type: DIALOG, TREEVIEW, etc, stored in ctrl_types[id]
--  parent_id: a gdx and not a native handle, stored in parent_ids[id] (can be NULL)
--  handle: a native handle for the new control, stored in ctrl_handles[id]
--  bEsc: true/false (meaningful only on dialogs)
--
    integer id
    if ctrl_free_list then
        id = ctrl_free_list
        ctrl_free_list = ctrl_types[ctrl_free_list]
        ctrl_handles[id] = NULL
        ctrl_types[id] = ctrl_type
        ctrl_flags[id] = flags
        parent_ids[id] = parent_id
        children_ids[id] = UNDEFINED
        ctrl_size[id] = UNDEFINED
--      ctrl_mp[id] = UNDEFINED
        ctrl_font[id] = UNDEFINED
        ctrl_fonts[id] = UNDEFINED
        ctrl_fontd[id] = UNDEFINED
        ctrl_xtra[id] = UNDEFINED
        wnd_proc_addr[id] = UNDEFINED
--      sub_proc_addr[id] = UNDEFINED
        deferred_attr[id] = {}
        def_menu_attr[id] = {}
    else
        ctrl_handles &= NULL
        ctrl_types &= ctrl_type
        ctrl_flags &= flags
        parent_ids &= parent_id
        children_ids &= UNDEFINED
        ctrl_size &= UNDEFINED
--      ctrl_mp &= UNDEFINED
        ctrl_font &= UNDEFINED
        ctrl_fonts &= UNDEFINED
        ctrl_fontd &= UNDEFINED
        ctrl_xtra &= UNDEFINED
        wnd_proc_addr &= UNDEFINED
--      sub_proc_addr &= UNDEFINED
        deferred_attr &= {{}}
        def_menu_attr &= UNDEFINED
        id = length(ctrl_handles)
    end if
    ctrl_size[id] = repeat(0,SZ_LENGTH)
--DEV possibly insufficient: gDialog has 1, gSplit 2, g(H|V|Multi)Box|gRadio|gTabs have many (-1?).
    if bHasChildren then
        children_ids[id] = {}
    end if
    return id
end function

--local procedure free_ctrl(integer tdx)
--  ctrl_handles[tdx] = -999 // (since UNDEFINED[=0] or -1 might trap fewer errors)
--  ctrl_types[tdx] = ctrl_free_list
--  ctrl_free_list = tdx
--end procedure

local procedure xpg_set_ctrl_flag(gdx id, integer cf_flag, bool v=true)
    -- (if v is false then clear the flag instead of setting it)
    integer f = ctrl_flags[id]
    if v then
        f = or_bits(f,cf_flag)
    else
        f -= and_bits(f,cf_flag)
    end if
    ctrl_flags[id] = f
end procedure

--local function xpg_get_ctrl_flag(gdx id, integer cf_flag)
--  return and_bits(ctrl_flags[id],cf_flag)!=0
--end function

-- NB: WinAPI only, GTK uses a GtkTreeStore instead.
local sequence tree_items = {},
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
--      cb = call_back({'+',rid})
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
    atom pList = allocate(length(v0)*machine_word(),true)
    poken(pList,v0)
    return pList
end function

local function xpg_double_array(sequence doubles)
    atom ptr = allocate(8*length(doubles))
    for i,d in doubles do
        poke(ptr+8*(i-1),atom_to_float64(d))
    end for
    return ptr
end function

local constant VK_SHIFT = #10, -- apps get/use VK_LSHIFT/VK_RSHIFT, not this
               VK_CTRL  = #11, -- apps get/use VK_LCTRL/VK_RCTRL, not this
               VK_ALT   = #12  -- apps get/use VK_LALT/VK_RALT, not this

global constant VK_SP   = #20   -- aka ' '

with nested_globals             -- Name         Value        xpGUI   WinAPI    GTK
local constant key_mappings = {{"VK_BS",        VK_BS       := #08,     #08, #FF08}, -- aka '\b'
                               {"VK_TAB",       VK_TAB      := #09,     #09, #FF09}, -- aka '\t'
                               {"VK_TAB",       VK_TAB,                 #09, #FF20}, -- (shift tab, GTK only)
                               {"VK_LF",        VK_LF       := #0A,     #0A, #FF0A}, -- aka '\n'
                               {"VK_CR",        VK_CR       := #0D,     #0D, #FF0D}, -- aka '\r'    
                               {"VK_PAUSE",     VK_PAUSE    := #13,     #13, #FF13}, -- (GTK3+ only, not GTK2)
                               {"VK_CAPSLOCK",  VK_CAPSLOCK := #14,     #14, #0000}, -- (not GTK)
                               {"VK_LSHIFT",    VK_LSHIFT   := #15,     #2A, #FFE1},
                               {"VK_RSHIFT",    VK_RSHIFT   := #16,     #36, #FFE2},
                               {"VK_LCTRL",     VK_LCTRL    := #17,     #1D, #FFE3},
                               {"VK_RCTRL",     VK_RCTRL    := #18,    #11D, #FFE4},
                               {"VK_LALT",      VK_LALT     := #19,   #2038, #FFE9},
                               {"VK_RALT",      VK_RALT     := #1A,   #2138, #FFEA},
                               {"VK_ESC",       VK_ESC      := #1B,     #1B, #FF1B},
                               {"VK_DEL",       VK_DEL      := #7F,     #2E, #FFFF},
                               {"VK_UP",        VK_UP       := #A1,     #26, #FF52},
                               {"VK_RIGHT",     VK_RIGHT    := #A2,     #27, #FF53},
                               {"VK_DOWN",      VK_DOWN     := #A3,     #28, #FF54}, -- (oops, should VK_POUND be #A3?)
                               {"VK_LEFT",      VK_LEFT     := #A4,     #25, #FF51},
                               {"VK_APPS",      VK_APPS     := #E0,     #5D, #FF67},
                               {"VK_INS",       VK_INS      := #E1,     #2D, #FF63},
                               {"VK_HOME",      VK_HOME     := #E2,     #24, #FF50},
                               {"VK_END",       VK_END      := #E3,     #23, #FF57},
                               {"VK_PGUP",      VK_PGUP     := #E4,     #21, #FF55},
                               {"VK_PGDN",      VK_PGDN     := #E5,     #22, #FF56},
                               {"VK_NUMLOCK",   VK_NUMLOCK  := #90,     #90, #FF7F},
                               {"VK_SCROLL",    VK_SCROLL   := #E6,     #91, #FF14},
                               {"VK_POUND",     VK_POUND  := #C2A3,     #A3, #00A3}, -- UTF8(ish) pound sign (or #A3?)
                               {"-",            '-',                    #00, #FFAD}, -- (Numpad -, GTK only)
                               {"+",            '+',                    #00, #FFAB}, -- (Numpad +, GTK only)
                               {"/",            '/',                    #00, #FFAF}, -- (Numpad /, GTK only)
                               {"*",            '*',                    #00, #FFAA}, -- (Numpad *, GTK only)
                               {"0",            '0',                    #00, #FFB0}, -- (Numpad 0, GTK only)
                               {"1",            '1',                    #00, #FFB1}, -- (Numpad 1, GTK only)
                               {"2",            '2',                    #00, #FFB2}, -- (Numpad 2, GTK only)
                               {"3",            '3',                    #00, #FFB3}, -- (Numpad 3, GTK only)
                               {"4",            '4',                    #00, #FFB4}, -- (Numpad 3, GTK only)
                               {"5",            '5',                    #00, #FFB5}, -- (Numpad 3, GTK only)
                               {"6",            '6',                    #00, #FFB6}, -- (Numpad 3, GTK only)
                               {"7",            '7',                    #00, #FFB7}, -- (Numpad 3, GTK only)
                               {"8",            '8',                    #00, #FFB8}, -- (Numpad 3, GTK only)
                               {"9",            '9',                    #00, #FFB9}, -- (Numpad 3, GTK only)
                               {"VK_F1",        VK_F1       := #F1,     #70, #FFBE},
                               {"VK_F2",        VK_F2       := #F2,     #71, #FFBF},
                               {"VK_F3",        VK_F3       := #F3,     #72, #FFC0},
                               {"VK_F4",        VK_F4       := #F4,     #73, #FFC1},
                               {"VK_F5",        VK_F5       := #F5,     #74, #FFC2},
                               {"VK_F6",        VK_F6       := #F6,     #75, #FFC3},
                               {"VK_F7",        VK_F7       := #F7,     #76, #FFC4},
                               {"VK_F8",        VK_F8       := #F8,     #77, #FFC5},
                               {"VK_F9",        VK_F9       := #F9,     #78, #FFC6},
                               {"VK_F10",       VK_F10      := #FA,     #79, #FFC7},
                               {"VK_F11",       VK_F11      := #FB,     #7A, #FFC8},
                               {"VK_F12",       VK_F12      := #FC,     #7B, #FFC9}}

local constant known_colours = {{"XPG_BLACK",           XPG_BLACK           := #000000},
                                {"XPG_NAVY",            XPG_NAVY            := #000080},
                                {"XPG_BLUE",            XPG_BLUE            := #0000FF},
                                {"XPG_LIGHT_BLUE",      XPG_LIGHT_BLUE      := #4363D8},
                                {"XPG_TEAL",            XPG_TEAL            := #008080},
                                {"XPG_CYAN",            XPG_CYAN            := #00FFFF},
                                {"XPG_DARK_GREEN",      XPG_DARK_GREEN      := #008000},
                                {"XPG_GREEN",           XPG_GREEN           := #3CB44B},
                                {"XPG_LIGHT_GREEN",     XPG_LIGHT_GREEN     := #00FF00},
                                {"XPG_OLIVE",           XPG_OLIVE           := #808000},
                                {"XPG_ORANGE",          XPG_ORANGE          := #FF8C00},
                                {"XPG_AMBER",           XPG_AMBER           := #FFBF00},
                                {"XPG_DARK_YELLOW",     XPG_DARK_YELLOW     := #EBEB00},
                                {"XPG_YELLOW",          XPG_YELLOW          := #FFFF00},
                                {"XPG_INDIGO",          XPG_INDIGO          := #4B0082},
                                {"XPG_PURPLE",          XPG_PURPLE          := #911EB4},
                                {"XPG_DARK_PURPLE",     XPG_DARK_PURPLE     := #800080},
                                {"XPG_MAGENTA",         XPG_MAGENTA         := #FF00FF},
                                {"XPG_VIOLET",          XPG_VIOLET          := #EE82EE},
                                {"XPG_DARK_RED",        XPG_DARK_RED        := #800000},
                                {"XPG_RED",             XPG_RED             := #FF0000},
                                {"XPG_SLATE",           XPG_SLATE           := #404040},
                                {"XPG_DARK_GREY",       XPG_DARK_GREY       := #808080},    
                                {"XPG_GREY",            XPG_GREY            := #C0C0C0},    
                                {"XPG_LIGHT_GREY",      XPG_LIGHT_GREY      := #E4E4E4},
                                {"XPG_PARCHMENT",       XPG_PARCHMENT       := #FFFFE0},
                                {"XPG_LIGHT_PARCHMENT", XPG_LIGHT_PARCHMENT := #FAF8EF},
                                {"XPG_WHITE",           XPG_WHITE           := #FFFFFF}}

local constant line_styles = {{"XPG_CONTINUOUS",    XPG_CONTINUOUS   := 0},
                              {"XPG_DASHED",        XPG_DASHED       := 1},
                              {"XPG_DOTTED",        XPG_DOTTED       := 2},
                              {"XPG_DASH_DOT",      XPG_DASH_DOT     := 3},
                              {"XPG_DASH_DOT_DOT",  XPG_DASH_DOT_DOT := 4}}
-- (local)                                          XPG_CUSTOM_DASH   = 5, -- defined below

-- stored in ctrl_xtra[id][CX_BOX_SPACE]:
local constant box_spacing = {{"NONE",      XPG_SPACE_NONE   := 0b000},
                              {"LEFT",      XPG_SPACE_LEFT   := 0b100},
                              {"TOP",       XPG_SPACE_TOP    := XPG_SPACE_LEFT},
                              {"RIGHT",     XPG_SPACE_RIGHT  := 0b001},
                              {"BOTTOM",    XPG_SPACE_BOTTOM := XPG_SPACE_RIGHT},
                              {"BETWEEN",   XPG_SPACE_BETWEEN:= 0b010},
                              {"AROUND",    XPG_SPACE_AROUND := 0b111},
                              {"CENTRE",    XPG_SPACE_CENTRE := 0b101}},
               box_spacing_descs = vslice(box_spacing,1),   
               box_spacing_masks = vslice(box_spacing,2)&-1

without nested_globals -- (always disable asap)

global constant XPG_DEFAULT     = -1,
                XPG_IGNORE      = -2,
                XPG_CLOSE       = -3,
                XPG_CONTINUE    = -4,

                XPG_DARK_GRAY = XPG_DARK_GREY,
                XPG_GRAY = XPG_GREY,
                XPG_SILVER = XPG_GREY,
                XPG_LIGHT_GRAY = XPG_LIGHT_GREY,
                
                XPG_NORMAL      = 0x0,
                XPG_BOLD        = 0x1,
                XPG_ITALIC      = 0x2,
                XPG_BOLDITALIC  = 0x3,
-- (local)      XPG_NORESET     = 0x4, -- for redraw_list(), defined below

                XPG_FILLED = 0b001, -- (nb same as true[/false])
                XPG_CHORD  = 0b010,
                XPG_SECTOR = 0b100,

                        -- WENS
                XPG_NW = 0b1010,    XPG_NORTHWEST = XPG_NW,
                XPG_W  = 0b1000,    XPG_WEST      = XPG_W,
                XPG_SW = 0b1001,    XPG_SOUTHWEST = XPG_SW,
                XPG_N  = 0b0010,    XPG_NORTH     = XPG_N,
                XPG_S  = 0b0001,    XPG_SOUTH     = XPG_S,
                XPG_NE = 0b0110,    XPG_NORTHEAST = XPG_NE,
                XPG_E  = 0b0100,    XPG_EAST      = XPG_E,
                XPG_SE = 0b0101,    XPG_SOUTHEAST = XPG_SE,
                XPG_C  = 0b0000,    XPG_CENTRE    = XPG_C,

                XPG_RAD2DEG = 180/PI,
                XPG_DEG2RAD = PI/180

local constant  XPG_CUSTOM_DASH = 5

global function gGetAlignName(integer d)
--  if d=-1 then return "-1" end if -- erm, no
    if d=XPG_C then return "XPG_C" end if
    string res = "XPG_"
    for bc in {{XPG_N,'N'},{XPG_S,'S'},{XPG_E,'E'},{XPG_W,'W'}} do
        if and_bits(d,bc[1]) then res &= bc[2] end if
    end for
    return res
end function

global function gGetLineStyleName(integer s)
    if s=-1 then return "-1" end if -- erm, no
    if s=XPG_CUSTOM_DASH then return "XPG_CUSTOM_DASH" end if
    for sn in line_styles do
        if s=sn[2] then return sn[1] end if
    end for
    return sprintf("** unknown style:%d **",s)
end function

global function gGetBoxSpacingName(integer s, gdx id=NULL)
    assert(id=NULL or ctrl_types[id]=BOX)
    integer k = find(s,box_spacing_masks)
    assert(k!=0,"unknown box spacing:0b%b",s)
    if id!=NULL
    and and_bits(ctrl_flags[id],CF_VERTICAL) 
    and box_spacing_masks[k+1]==s then
        k += 1
    end if
    return box_spacing_descs[k]
end function

sequence dashers = {{1},                                -- XPG_CONTINUOUS
                    {1,1,1,1,1,1,0,0},                  -- XPG_DASHED
                    {1,1,0,0},                          -- XPG_DOTTED
                    {1,1,1,1,1,1,0,0,1,1,0,0},          -- XPG_DASH_DOT
                    {1,1,1,1,1,1,0,0,1,1,0,0,1,1,0,0},  -- XPG_DASH_DOT_DOT
                    {1}}                                -- XPG_CUSTOM_DASH

include builtins\ptypes.e   -- (types atom_string, rid_string, nullable_string, and boolean)

include builtins\cffi.e

local constant  C_DBL = C_DOUBLE,
                --G_TYPE_CHAR = 3*4,
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
--              typedef enum {
--              GDK_EXPOSURE_MASK               = 0x000002, -- 1 << 1,
                GDK_POINTER_MOTION_MASK         = 0x000004, -- 1 << 2,
--              GDK_POINTER_MOTION_HINT_MASK    = 0x000008, -- 1 << 3,
--              GDK_BUTTON_MOTION_MASK          = 0x000010, -- 1 << 4,
--              GDK_BUTTON1_MOTION_MASK         = 0x000020, -- 1 << 5,
--              GDK_BUTTON2_MOTION_MASK         = 0x000040, -- 1 << 6,
--              GDK_BUTTON3_MOTION_MASK         = 0x000080, -- 1 << 7,
                GDK_BUTTON_PRESS_MASK           = 0x000100, -- 1 << 8,
                GDK_BUTTON_RELEASE_MASK         = 0x000200, -- 1 << 9,
                GDK_KEY_PRESS_MASK              = 0x000400, -- 1 << 10,
--              GDK_KEY_RELEASE_MASK            = 0x000800, -- 1 << 11,
--              GDK_ENTER_NOTIFY_MASK           = 0x001000, -- 1 << 12,
--              GDK_LEAVE_NOTIFY_MASK           = 0x002000, -- 1 << 13,
--              GDK_FOCUS_CHANGE_MASK           = 0x004000, -- 1 << 14,
--              GDK_STRUCTURE_MASK              = 0x008000, -- 1 << 15,
--              GDK_PROPERTY_CHANGE_MASK        = 0x010000, -- 1 << 16,
--              GDK_VISIBILITY_NOTIFY_MASK      = 0x020000, -- 1 << 17,
--              GDK_PROXIMITY_IN_MASK           = 0x040000, -- 1 << 18,
--              GDK_PROXIMITY_OUT_MASK          = 0x080000, -- 1 << 19,
--              GDK_SUBSTRUCTURE_MASK           = 0x100000, -- 1 << 20,
--              GDK_SCROLL_MASK                 = 0x200000, -- 1 << 21,
--              GDK_ALL_EVENTS_MASK             = 0x3FFFFE
--              } GdkEventMask;
--              GDK_BKE_MASK = 0b0101_0000_0010,
--              GDK_BKE_MASK = 0x0502,
                GDK_KBR_MASK = or_all({--GDK_STRUCTURE_MASK,
                                       GDK_KEY_PRESS_MASK,
                                       GDK_BUTTON_PRESS_MASK,
                                       GDK_BUTTON_RELEASE_MASK,
                                       GDK_POINTER_MOTION_MASK}),
--              GDK_KEY_ESCAPE = #FF1B,
                GDK_CONTROL_MASK = #04,
                GDK_SHIFT_MASK   = #01,
                GDK_ALT_MASK     = #08,
                GDK_LEFT_MASK    = #100,
                GDK_MIDDLE_MASK  = #200,
                GDK_RIGHT_MASK   = #400,
--              typedef enum {
--              GDK_NOTHING             = -1,
--              GDK_DELETE              =  0,
--              GDK_DESTROY             =  1,
--              GDK_EXPOSE              =  2,
--              GDK_MOTION_NOTIFY       =  3,
--              GDK_BUTTON_PRESS        =  4,
--              GDK_2BUTTON_PRESS       =  5,
--              GDK_DOUBLE_BUTTON_PRESS =  5,   -- (==GDK_2BUTTON_PRESS, alias added in 3.6)
--              GDK_3BUTTON_PRESS       =  6,
                GDK_TRIPLE_BUTTON_PRESS =  6,   -- (==GDK_3BUTTON_PRESS, alias added in 3.6)
--              GDK_BUTTON_RELEASE      =  7,
--              GDK_KEY_PRESS           =  8,
--              GDK_KEY_RELEASE         =  9,
--              GDK_ENTER_NOTIFY        = 10,
--              GDK_LEAVE_NOTIFY        = 11,
--              GDK_FOCUS_CHANGE        = 12,
--              GDK_CONFIGURE           = 13,
--              GDK_MAP                 = 14,
--              GDK_UNMAP               = 15,
--              GDK_PROPERTY_NOTIFY     = 16,
--              GDK_SELECTION_CLEAR     = 17,
--              GDK_SELECTION_REQUEST   = 18,
--              GDK_SELECTION_NOTIFY    = 19,
--              GDK_PROXIMITY_IN        = 20,
--              GDK_PROXIMITY_OUT       = 21,
--              GDK_DRAG_ENTER          = 22,
--              GDK_DRAG_LEAVE          = 23,
--              GDK_DRAG_MOTION         = 24,
--              GDK_DRAG_STATUS         = 25,
--              GDK_DROP_START          = 26,
--              GDK_DROP_FINISHED       = 27,
--              GDK_CLIENT_EVENT        = 28,
--              GDK_VISIBILITY_NOTIFY   = 29,
--              GDK_NO_EXPOSE           = 30,
--              GDK_SCROLL              = 31,
--              GDK_WINDOW_STATE        = 32,
--              GDK_SETTING             = 33,
--              GDK_OWNER_CHANGE        = 34,
--              GDK_GRAB_BROKEN         = 35,
--              GDK_DAMAGE              = 36,
--              GDK_EVENT_LAST  /* helper variable for decls */
--              } GdkEventType;

--              GTK_ORIENTATION_HORIZONTAL = 0,
                GTK_ORIENTATION_VERTICAL = 1,
                GTK_POLICY_AUTOMATIC = 1,
--              GTK_PROGRESS_CONTINUOUS = 0,
--              GTK_PROGRESS_DISCRETE = 1,
--              GTK_PROGRESS_LEFT_TO_RIGHT = 0,
--              GTK_PROGRESS_RIGHT_TO_LEFT = 1,
                GTK_PROGRESS_BOTTOM_TO_TOP = 2,
--              GTK_PROGRESS_TOP_TO_BOTTOM = 3,
                GTK_SELECTION_NONE = 0,
--              GTK_SELECTION_SINGLE = 1
                GDK_SELECTION_CLIPBOARD = 69,
                GTK_STYLE_PROVIDER_PRIORITY_APPLICATION = 600,
--              GTK_STYLE_PROVIDER_PRIORITY_USER = 800,
                GTK_WINDOW_TOPLEVEL = 0,
--              GTK_WINDOW_POPUP = 1,               -- menu/tooltip (not a proper sub-window)
--              PANGO_STYLE_ITALIC = 2,
--              PANGO_UNDERLINE_SINGLE = 1,
--              PANGO_WEIGHT_BOLD = 700,

                WM_USER = #400, -- (1024)
--              BI_RGB = 0,
                BM_GETCHECK = 240,
                BM_SETCHECK = 241,
--              BM_GETSTATE = 242,
--              BM_SETSTATE = 243,
                -- Button Styles
                BS_PUSHBUTTON = 0,
--              BS_DEFPUSHBUTTON = 1,
--              BS_CHECKBOX = 2,
                BS_AUTOCHECKBOX = 3,
--              BS_RADIOBUTTON = 4,
--              BS_3STATE = 5,
--              BS_AUTO3STATE = 6,
                BS_GROUPBOX = 7,
--              BS_USERBUTTON = 8,
                BS_AUTORADIOBUTTON = 9,
--              BS_OWNERDRAW =  11,
--              BS_TEXT = 0,
--              BS_LEFTTEXT = #20,
--              BS_RIGHTBUTTON = #20,
--              BS_ICON = #40,
--              BS_BITMAP = #80,
--              BS_LEFT = #100,
--              BS_RIGHT = #200,
--              BS_CENTER =  #300,
--              BS_TOP =  #400,
--              BS_BOTTOM =  #800,
--              BS_VCENTER =  #C00,
--              BS_PUSHLIKE = #1000,
--              BS_MULTILINE =  #2000,
--              BS_NOTIFY =  #4000,
--              BS_FLAT =  #8000,
--              BST_UNCHECKED = 0,
                BST_CHECKED = 1,
--              BST_INDETERMINATE = 2,
--              BST_PUSHED = 4,
--              BST_FOCUS = 8,
--              CB_GETEDITSEL = 320,
--              CB_LIMITTEXT = 321,
--              CB_SETEDITSEL = 322,
                CB_ADDSTRING = 323,
--              CB_DELETESTRING = 324,
--              CB_DIR = 325,
--              CB_GETCOUNT = 326,
                CB_GETCURSEL = 327,
                CB_GETLBTEXT = 328,
                CB_GETLBTEXTLEN = 329,
--              CB_INSERTSTRING = 330,
                CB_RESETCONTENT = 331,
--              CB_FINDSTRING = 332,
--              CB_SELECTSTRING = 333,
                CB_SETCURSEL = 334,
--              CB_SHOWDROPDOWN = 335,
--              CB_GETITEMDATA = 336,
--              CB_SETITEMDATA = 337,
--              CB_GETDROPPEDCONTROLRECT = 338,
--              CB_SETITEMHEIGHT = 339,
--              CB_GETITEMHEIGHT = 340,
--              CB_SETEXTENDEDUI = 341,
--              CB_GETEXTENDEDUI = 342,
--              CB_GETDROPPEDSTATE = 343,
--              CB_FINDSTRINGEXACT = 344,
--              CB_SETLOCALE = 345,
--              CB_GETLOCALE = 346,
--              CB_GETTOPINDEX = 347,
--              CB_SETTOPINDEX = 348,
--              CB_GETHORIZONTALEXTENT = 349,
--              CB_SETHORIZONTALEXTENT = 350,
--              CB_GETDROPPEDWIDTH = 351,
--              CB_SETDROPPEDWIDTH = 352,
--              CB_INITSTORAGE = 353,
                CB_ERR = -1,
                CBM_INIT = #4,   --  initialize bitmap
--              CBS_SIMPLE = 1,
                CBS_DROPDOWN = 2,
                CBS_DROPDOWNLIST = 3,
--              CBS_OWNERDRAWFIXED = #10,
--              CBS_OWNERDRAWVARIABLE = #20,
                CBS_AUTOHSCROLL = #40,
--              CBS_OEMCONVERT = #80,
--              CBS_SORT = #100,
--              CBS_HASSTRINGS = #200,
--              CBS_NOINTEGRALHEIGHT =  #400,
--              CBS_DISABLENOSCROLL =  #800,
--              CBS_UPPERCASE =  #2000,
--              CBS_LOWERCASE =  #4000,

                -- CLIPBOARD FORMATS
                CF_TEXT             = 1,
--              CF_BITMAP           = 2,
--              CF_METAFILEPICT     = 3,
--              CF_DIF              = 5,
--              CF_SYLK             = 4,
--              CF_TIFF             = 6,
--              CF_OEMTEXT          = 7,
--              CF_DIB              = 8,
--              CF_PALETTE          = 9,
--              CF_PENDATA          = 10,
--              CF_RIFF             = 11,
--              CF_WAVE             = 12,
                CF_UNICODETEXT      = 13,
--              CF_ENHMETAFILE      = 14,
--              CF_HDROP            = 15,
--              CF_LOCALE           = 16,
--              CF_DSPTEXT          = 129,
--              CF_DSPBITMAP        = 130,
--              CF_DSPMETAFILEPICT  = 131,
--              CF_DSPENHMETAFILE   = 142,
--              CF_GDIOBJFIRST      = #300,
--              CF_GDIOBJLAST       = #3FF,
--              CF_OWNERDISPLAY     = #80,
--              CF_PRIVATEFIRST     = #200,
--              CF_PRIVATELAST      = #2FF,

                COLOR_BTNFACE = 15,
                CS_VREDRAW = 1,
                CS_HREDRAW = 2,
                CS_DBLCLKS = 8,
                CS_OWNDC = #20,
                CW_USEDEFAULT = #80000000,
                DEFAULT_CHARSET = 1,
                DIB_RGB_COLORS = 0,

                -- DrawText CONSTANTS
--              DT_TOP = 0,
--              DT_LEFT = 0,
--              DT_CENTER = 1,
--              DT_RIGHT = 2,
--              DT_VCENTER = 4,
--              DT_BOTTOM = 8,
--              DT_WORDBREAK = #10,
--              DT_SINGLELINE = #20,
--              DT_EXPANDTABS = #40,
--              DT_TABSTOP = #80,
                DT_NOCLIP = #100,
--              DT_EXTERNALLEADING = #200,
--              DT_CALCRECT = #400,
--              DT_NOPREFIX = #800,
--              DT_INTERNAL = #1000,
--              DT_EDITCONTROL = #2000,
--              DT_PATH_ELLIPSIS = #4000,
--              DT_END_ELLIPSIS = #8000,
--              DT_MODIFYSTRING = #10000,
--              DT_RTLREADING = #20000,
--              DT_SINGLECENTER = or_all({DT_SINGLELINE,DT_CENTER,DT_VCENTER}),
                DTM_FIRST = #1000,
--              DTM_GETSYSTEMTIME = DTM_FIRST + 1
--              DTM_SETSYSTEMTIME = DTM_FIRST + 2
--              DTM_GETRANGE = DTM_FIRST + 3
--              DTM_SETRANGE = DTM_FIRST + 4
--              DTM_SETFORMAT = DTM_FIRST + 5
--              DTM_SETMCCOLOR = DTM_FIRST + 6
--              DTM_GETMCCOLOR = DTM_FIRST + 7
--              DTM_GETMONTHCAL = DTM_FIRST + 8
--              DTM_SETMCFONT = DTM_FIRST + 9
--              DTM_GETMCFONT = DTM_FIRST + 10
--              DTM_SETMCSTYLE = DTM_FIRST + 11
--              DTM_GETMCSTYLE = DTM_FIRST + 12
--              DTM_CLOSEMONTHCAL = DTM_FIRST + 13
--              DTM_GETDATETIMEPICKERINFO = DTM_FIRST + 14
                DTM_GETIDEALSIZE = DTM_FIRST + 15,
--              DTS_SHORTDATECENTURYFORMAT = 0xC,
--      #DEFINE DTS_SHORTDATEFORMAT        0x00 
--      #DEFINE DTS_UPDOWN                 0x01
--      #DEFINE DTS_SHOWNONE               0x02
--      #DEFINE DTS_LONGDATEFORMAT         0x04
--      #DEFINE DTS_TIMEFORMAT             0x09
--      #DEFINE DTS_APPCANPARSE            0x10
--      #DEFINE DTS_RIGHTALIGN             0x20
--      #DEFINE DTS_SHORTDATECENTURYFORMAT 0x0C

--              ERROR_CLASS_ALREADY_EXISTS = 1410,
                ES_AUTOHSCROLL = #80,
--              ES_AUTOVSCROLL = 64,
                ES_LEFT = 0,
--              ES_CENTER = 1,
--              ES_RIGHT = 2,
--              ES_MULTILINE = 4,
--              ES_UPPERCASE = 8,
--              ES_LOWERCASE = #10,
--              ES_PASSWORD = #20,
--              ES_NOHIDESEL = #100,
--              ES_OEMCONVERT =  #400,
--              ES_READONLY =  #800,
--              ES_WANTRETURN = #1000,
--              ES_NUMBER =  #2000,
                FW_NORMAL = 400,
                FW_BOLD = 700,

                -- GLOBAL MEMORY FLAGS
--              GMEM_FIXED = #0,
                GMEM_MOVEABLE = #2,
--              GMEM_NOCOMPACT = #10,
--              GMEM_NODISCARD = #20,
--              GMEM_ZEROINIT = #40,
--              GMEM_MODIFY = #80,
--              GMEM_DISCARDABLE = #100,
--              GMEM_NOT_BANKED = #1000,
--              GMEM_SHARE = #2000,
                GMEM_DDESHARE = #2000,
--              GMEM_NOTIFY = #4000,
--              GMEM_LOWER = GMEM_NOT_BANKED,
--              GMEM_VALID_FLAGS = #7F72,
--              GMEM_INVALID_HANDLE = #8000,
                GMEM_CLIPBOARD = or_all({GMEM_MOVEABLE,GMEM_DDESHARE}),

--              GWL_EXSTYLE = -20,
                GWL_STYLE = -16,
                GWL_USERDATA = -21,
                GWL_WNDPROC = -4,
                HWND_TOPMOST = -1,
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
--              LBN_DBLCLK = 2,
--              LBN_ERRSPACE = -2,
--              LBN_KILLFOCUS = 5,
--              LBN_SELCANCEL = 3,
                LBN_SELCHANGE = 1,
--              LBN_SETFOCUS = 4,
                LF_FACESIZE = 32,
                LOGPIXELSY = 90,
                LPSTR_TEXTCALLBACK = -1,
                MF_BYCOMMAND  = 0,
                MF_BYPOSITION = #400,
                MF_STRING     = #0000,
                MF_GRAYED     = #0001,
                MF_CHECKED    = #0008,
                MF_POPUP      = #0010,
                MF_SEPARATOR  = #0800,
                MIIM_ID = 0x00000002,
                MIIM_TYPE = 0x00000010,
                MIIM_STRING = 0x00000040,
                -- FLAGS USED IN WM_XXX messages
                MK_LBUTTON = #01,
                MK_RBUTTON = #02,
                MK_SHIFT   = #04,
                MK_CONTROL = #08,
                MK_MBUTTON = #10,

                NULL_BRUSH = 5,
                -- progress bar styles
                PBS_SMOOTH      = 1,
                PBS_VERTICAL    = 4,
--              PBS_MARQUEE     = 8,
                -- progress bar messages
                PBM_SETRANGE    = WM_USER + 1,      -- set min/max
                PBM_SETPOS      = WM_USER + 2,      -- set position
--              PBM_DELTAPOS    = WM_USER + 3,      -- change by requested increment
--              PBM_SETSTEP     = WM_USER + 4,      -- set step size
--              PBM_STEPIT      = WM_USER + 5,      -- step ahead by 1 step
--              PBM_SETRANGE32  = WM_USER + 6,      -- range is double length word
--              PBM_GETRANGE    = WM_USER + 7,      -- get range
--              PBM_GETPOS      = WM_USER + 8,      -- get position
--              PBM_SETMARQUEE  = WM_USER + 10,

                -- pen styles
--              PS_SOLID         = #00000000,       -- === XPG_CONTINUOUS = 0,
--              PS_DASH          = #00000001,       -- === XPG_DASHED = 1,
--              PS_DOT           = #00000002,       -- === XPG_DOTTED = 2,
--              PS_DASHDOT       = #00000003,       -- === XPG_DASH_DOT = 3,
--              PS_DASHDOTDOT    = #00000004,       -- === XPG_DASH_DOT_DOT = 4
--              PS_NULL          = #00000005,
--              PS_INSIDEFRAME   = #00000006,
--              PS_USERSTYLE     = #00000007,
--              PS_ALTERNATE     = #00000008,
--              PS_STYLE_MASK    = #0000000F,
--              PS_ENDCAP_ROUND  = #00000000,
--              PS_ENDCAP_SQUARE = #00000100,
--              PS_ENDCAP_FLAT   = #00000200,
--              PS_ENDCAP_MASK   = #00000F00,
--              PS_JOIN_ROUND    = #00000000,
--              PS_JOIN_BEVEL    = #00001000,
--              PS_JOIN_MITER    = #00002000,
--              PS_JOIN_MASK     = #0000F000,
--              PS_COSMETIC      = #00000000,
--              PS_GEOMETRIC     = #00010000,
--              PS_TYPE_MASK     = #000F0000,

--              R2_BLACK = 1,           -- Pixel is always 0.
--              R2_NOTMERGEPEN = 2,     -- Pixel is the inverse of the R2_MERGEPEN color.
--              R2_MASKNOTPEN = 3,      -- combination common screen and inverse of pen.
--              R2_NOTCOPYPEN = 4,      -- Pixel is the inverse of the pen color.
--              R2_MASKPENNOT = 5,      -- combination common to pen and inverse of screen.
--              R2_NOT = 6,             -- Pixel is the inverse of the screen color.
--              R2_XORPEN = 7,          -- Pixel is the inverse of the R2_XORPEN color.
--              R2_NOTMASKPEN = 8,      -- Pixel is the inverse of the R2_MASKPEN color.
                R2_MASKPEN = 9,         -- combination common to pen and the screen.
--              R2_NOTXORPEN = 10,      -- combination of colors in pen and screen, but not in both.
--              R2_NOP = 11,            -- Pixel remains unchanged.
--              R2_MERGENOTPEN = 12,    -- combination of screen and inverse of pen.
--              R2_COPYPEN = 13,        -- Pixel is the pen color.
--              R2_MERGEPENNOT = 14,    -- combination of pen color and inverse of screen color.
--              R2_MERGEPEN = 15,       -- combination of pen color and the screen color.
--              R2_WHITE = 16,          -- Pixel is always 1.
--              R2_LAST = 16,

                RDW_INVALIDATE      = #0001,
                RDW_INTERNALPAINT   = #0002,
                RDW_ERASE           = #0004,
--              RDW_VALIDATE        = #0008,
--              RDW_NOINTERNALPAINT = #0010,
--              RDW_NOERASE         = #0020,
--              RDW_NOCHILDREN      = #0040,
--              RDW_ALLCHILDREN     = #0080,
                RDW_UPDATENOW       = #0100,
--              RDW_ERASENOW        = #0200,
--              RDW_FRAME           = #0400,
--              RDW_NOFRAME         = #0800,
                -- sort
                SB_HORZ = 0,
                SB_VERT = 1,
--              SB_CTL = 2,
--              SB_BOTH = 3,
                -- scroll info flags
                SIF_RANGE = #1,
                SIF_PAGE = #2,
                SIF_POS = #4,
--              SIF_DISABLENOSCROLL = #8,
                SIF_TRACKPOS = #10,
                SIF_ALL = or_all({SIF_RANGE,SIF_PAGE,SIF_POS,SIF_TRACKPOS}),
                SM_CXSCREEN = 0,
                SM_CYSCREEN = 1,
                SM_CXSMICON = 49,
                SM_CYSMICON = 50,
--              SND_FILENAME    = #00020000,
--              SND_ASYNC       = #00000001,
--              SND_FILEASYNC   = or_bits(SND_FILENAME,SND_ASYNC),
                SRCCOPY         = #CC0020,
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
--              TA_LEFT = 0,
--              TA_TOP = 0,
--              TA_RIGHT = 2,
--              TA_CENTER = 6,
--              TA_BOTTOM = 8,
--              TA_BASELINE = 24,
--              TA_RTLREADING = 256,
--              TA_NOUPDATECP = 0,
--              TA_UPDATECP = 1,
--TA_BASELINE  The reference point will be on the base line of the text.
--TA_BOTTOM  The reference point will be on the bottom edge of the bounding rectangle.
--TA_TOP    The reference point will be on the top edge of the bounding rectangle.
--TA_CENTER  The reference point will be aligned horizontally with the center of the bounding rectangle.
--TA_LEFT  The reference point will be on the left edge of the bounding rectangle.
--TA_RIGHT  The reference point will be on the right edge of the bounding rectangle.

                TB_LINEUP           = 0,
                TB_LINEDOWN         = 1,
                TB_PAGEUP           = 2,
                TB_PAGEDOWN         = 3,
--              TB_THUMBPOSITION    = 4,
                TB_THUMBTRACK       = 5,
                TB_TOP              = 6,
                TB_BOTTOM           = 7,
--              TB_ENDTRACK         = 8,
--              TBM_CLEARSEL        = 1043,
--              TBM_CLEARTICS       = 1033,
--              TBM_GETCHANNELRECT  = 1050,
--              TBM_GETLINESIZE     = 1048,
--              TBM_GETNUMTICS      = 1040,
--              TBM_GETPAGESIZE     = 1046,
                TBM_GETPOS          = 1024,
--              TBM_GETPTICS        = 1038,
--              TBM_GETRANGEMAX     = 1026,
--              TBM_GETRANGEMIN     = 1025,
--              TBM_GETSELEND       = 1042,
--              TBM_GETSELSTART     = 1041,
--              TBM_GETTHUMBLENGTH  = 1052,
--              TBM_GETTHUMBRECT    = 1049,
--              TBM_GETTIC          = 1027,
--              TBM_GETTICPOS       = 1039,
--              TBM_SETLINESIZE     = 1047,
--              TBM_SETPAGESIZE     = 1045,
                TBM_SETPOS          = 1029,
--              TBM_SETRANGE        = 1030,
--              TBM_SETRANGEMAX     = 1032,
--              TBM_SETRANGEMIN     = 1031,
--              TBM_SETSEL          = 1034,
--              TBM_SETSELEND       = 1036,
--              TBM_SETSELSTART     = 1035,
--              TBM_SETTHUMBLENGTH  = 1051,
--              TBM_SETTIC          = 1028,
--              TBM_SETTICFREQ      = 1044,
                TBS_HORZ            = 0,
                TBS_VERT            = 2,
--              TBS_AUTOTICKS       = 1,
--              TBS_NOTICKS         = #10,
--              TBS_TOP             = 4,
--              TBS_BOTTOM          = 0,
--              TBS_LEFT            = 4,
--              TBS_RIGHT           = 0,
--              TBS_BOTH            = 8,
--              TBS_ENABLESELRANGE  = #20,
--              TBS_FIXEDLENGTH     = #40,
--              TBS_NOTHUMB         = #80,
                ---------------
                -- TAB CONTROLS
                ---------------
                -- MESSAGES
--              TCM_ADJUSTRECT = 4904,
--              TCM_DELETEALLITEMS = 4873,
--              TCM_DELETEITEM = 4872,
--              TCM_GETCURFOCUS = 4911,
--              TCM_GETCURSEL = 4875,
--              TCM_GETIMAGELIST = 4866,
--              TCM_GETITEMW = 4924,
--              TCM_INSERTITEMW = 4926,
--              TCM_SETITEMW = 4925,
--              TCM_GETITEMA = 4869,
                TCM_INSERTITEMA = 4871,
                TCM_SETITEMA = 4870,
                TCM_GETITEMCOUNT = 4868,
--              TCM_GETITEMRECT = 4874,
--              TCM_GETROWCOUNT = 4908,
--              TCM_GETTOOLTIPS = 4909,
--              TCM_HITTEST = 4877,
--              TCM_REMOVEIMAGE = 4906,
--              TCM_SETCURFOCUS = 4912,
--              TCM_SETCURSEL = 4876,
--              TCM_SETIMAGELIST = 4867,
--              TCM_SETITEMEXTRA = 4878,
--              TCM_SETITEMSIZE = 4905,
--              TCM_SETPADDING = 4907,
--              TCM_SETTOOLTIPS = 4910,
                -- NOTIFICATIONS
--              TCN_KEYDOWN = -550,
--              TCN_SELCHANGE = -551,
--              TCN_SELCHANGING = -552,
                -- STYLE
--              TCS_FORCEICONLEFT = #10,
--              TCS_FORCELABELLEFT = #20,
--              TCS_BUTTONS = #100,
--              TCS_MULTILINE = #200,
--              TCS_FIXEDWIDTH = #400,
--              TCS_RAGGEDRIGHT = #800,
                TCS_FOCUSONBUTTONDOWN = #1000,
--              TCS_OWNERDRAWFIXED = #2000,
--              TCS_TOOLTIPS = #4000,
--              TCS_FOCUSNEVER = #8000,
--              TCS_RIGHTJUSTIFY = 0,
--              TCS_SINGLELINE = 0,
--              TCS_TABS = 0,
--              TCS_BOTTOM = 2,
--              TCS_FOCUSNEVER = #8000,
--              TCS_FLATBUTTONS = 8,
                -- TC_ITEM FLAGS
                TCIF_TEXT = 1,
--              TCIF_IMAGE = 2,
--              TCIF_PARAM = 8,
--              TCIF_RTLREADING = 4,
                -- HIT-TEST
--              TCHT_NOWHERE = 1,
--              TCHT_ONITEM = 6,
--              TCHT_ONITEMICON = 2,
--              TCHT_ONITEMLABEL = 4,

                TPM_RETURNCMD = #100,
                -----------------------
                -- TOOLTIPS
                -----------------------
                TTF_IDISHWND = #01,
                TTF_SUBCLASS = #10,
--              TTF_TRACK    = #20,
--              TTF_ABSOLUTE = #80,
                TTS_ALWAYSTIP = #01,
--              TTS_NOPREFIX  = #02,
                TTS_BALLOON   = #40,
--              TTM_ACTIVATE        = WM_USER + 1,
--              TTM_SETDELAYTIME    = WM_USER + 3,
                TTM_ADDTOOLA        = WM_USER + 4,
--              TTM_TRACKACTIVATE   = WM_USER + 17,
--              TTM_TRACKPOSITION   = WM_USER + 18,
--              TTM_DELTOOLA = 1029,
--              TTM_SETTOOLINFO = 1078,
                TV_FIRST            = #1100,
                TVE_COLLAPSE        = 1,
                TVE_EXPAND          = 2,
                TVGN_CHILD          = 4,
--              TVGN_ROOT           = 0,
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
--              WM_GETTEXT = 13,
--              WM_GETTEXTLENGTH = 14,
                WM_PAINT = 15,
                WM_CLOSE = 16,
                WM_ERASEBKGND = 20,
--              WM_SHOWWINDOW = 24,
--              WM_ACTIVATEAPP = 28,
--              WM_SETCURSOR = 32,
--              WM_MOUSEACTIVATE = 33,
--              WM_GETMINMAXINFO = 36,
                WM_SETFONT = 48,
--              WM_WINDOWPOSCHANGING = 70,
--              WM_WINDOWPOSCHANGED = 71,
                WM_NOTIFY = 78,
--              WM_NOTIFYFORMAT = 85,
--              WM_GETICON = 127,
--              WM_NCCREATE = 129,
--              WM_NCCALCSIZE = 131,
--              WM_NCHITTEST = 132,
--              WM_NCPAINT = 133,
--              WM_NCACTIVATE = 134,
--              WM_NCMOUSEMOVE = 160,
--              WM_NCLBUTTONDOWN = 161,
                WM_KEYDOWN = 256,
--              WM_KEYUP = 257,
                WM_CHAR = 258,
                WM_SYSKEYDOWN = 260,
                WM_SYSCHAR = 262,   -- 0x106
--              WM_INITDIALOG = 272,
                WM_COMMAND = 273,
--              WM_TIMER = 275,
                WM_HSCROLL = 276,
                WM_VSCROLL = 277,
                WM_MENUSELECT = 287,
--              WM_MENUCOMMAND = 294,
--              WM_CTLCOLORSTATIC = 312,
                WM_MOUSEMOVE = 512,
                WM_LBUTTONDOWN = 513,
--              WM_LBUTTONUP = 514,
--              WM_LBUTTONDBLCLK = 515,
--              WM_RBUTTONDOWN = 516,
--              WM_RBUTTONUP = 517,
--              WM_RBUTTONDBLCLK = 518,
--              WM_MBUTTONDOWN = 519,
--              WM_MBUTTONUP = 520,
                WM_MBUTTONDBLCLK = 521,
--              WM_PARENTNOTIFY = 528,
--              WM_SIZING = 532,
--              WM_IME_SETCONTEXT = 641,
--              WM_IME_NOTIFY = 642,
--              WM_MOUSEHOVER = 673,    --  #02A1
--              WM_NCMOUSELEAVE = 674,  --  #02A2
--              WM_MOUSELEAVE = 675,    --  #02A3
--              WM_PRINTCLIENT = 792,

                WS_CHILD            = #40000000,
                WS_POPUP            = #80000000,
--              WS_POPUPWINDOW      = #80880000,    -- = WS_POPUP+WS+BORDER+WS_SYSMENU
                WS_CLIPCHILDREN     = #02000000,
--              WS_CLIPSIBLINGS     = #04000000,
                WS_EX_ACCEPTFILES   = #00000010,
                WS_EX_CLIENTEDGE    = #00000200,
                WS_EX_CONTROLPARENT = #00010000,
--              WS_EX_NOACTIVATE    = #08000000,
--              WS_EX_TOPMOST       = #00000008,
                WS_BORDER           = #00800000,
                WS_CAPTION          = #00C00000,    -- (==WS_DLGFRAME+WS_BORDER)
--              WS_MINIMIZE         = #20000000,
--              WS_MINIMIZEBOX      = #00020000,
--              WS_MAXIMIZEBOX      = #00010000,
--              WS_MINMAXBOX        = #00030000,
                WS_OVERLAPPED       = #00000000,
                WS_SYSMENU          = #00080000,
                WS_TABSTOP          = #00010000,
--              WS_THICKFRAME       = #00040000,    -- (==WS_SIZEBOX)
                WS_MINMAXTHICK      = #00070000,
                WS_HSCROLL          = #00100000,
                WS_VSCROLL          = #00200000,
                WS_VISIBLE          = #10000000,
                WS_GROUP            = #00020000,
--              WS_OVERLAPPEDWINDOW = or_all({WS_OVERLAPPED,WS_CAPTION,WS_SYSMENU,
--                                            WS_THICKFRAME,WS_MINIMIZEBOX,WS_MAXIMIZEBOX})
--              WS_OVERLAPPEDWINDOW = or_all({WS_OVERLAPPED,WS_CAPTION,WS_SYSMENU,WS_THICKFRAME}),
                WS_OVERLAPPEDWINDOW = or_all({WS_OVERLAPPED,WS_CAPTION,WS_SYSMENU}),
--              WS_OVERLAPPEDWINDOW = #00CF0000,
--              WS_OVERLAPPEDWINDOW = #00CB0000,
                -- SetBkMode CONSTANTS
                TRANSPARENT = 1,
--              OPAQUE = 2,
                XPG_PB_MAX = 32000

local bool bGTK3
local integer   cairo_arc,
                cairo_close_path,
                cairo_curve_to,
                cairo_destroy,
                cairo_fill,
--              cairo_get_source,
                cairo_line_to,
                cairo_move_to,
                cairo_new_path,
                cairo_paint,
                cairo_rectangle,
                cairo_restore,
                cairo_rotate,
                cairo_save,
                cairo_scale,
                cairo_set_dash,
                cairo_set_line_width,
--              cairo_set_source,
                cairo_set_source_rgb,
                cairo_stroke,
                cairo_translate,
--              gdk_atom_intern,
                gdk_cairo_create,
--              gdk_cairo_rectangle,
                gdk_cairo_set_source_pixbuf,
                gdk_display_get_default,
                gdk_display_get_monitor,
                gdk_display_get_pointer,
                gdk_get_default_root_window,
                gdk_monitor_get_geometry,
--              gdk_pixbuf_get_from_drawable,
--              gdk_pixbuf_get_from_surface,
--              gdk_pixbuf_get_from_window,
                gdk_pixbuf_get_pixels_,
                gdk_pixbuf_get_type,
                gdk_pixbuf_new_from_data,
                gdk_pixbuf_new_from_xpm_data,
                gdk_screen_get_default,
                gdk_screen_get_height,
                gdk_screen_get_width,
--              gdk_screen_get_root_window,
--              gdk_window_focus,
--              gdk_window_get_geometry,
                gdk_window_get_height,
                gdk_window_get_width,
                gdk_window_get_origin,
                gdk_window_get_root_origin,
--              gdk_window_get_position,
                gdk_window_invalidate_rect,
--              gdk_window_move_resize,
                gdk_window_process_updates,
                gtk_adjustment_new,
--              gtk_box_pack_start,
                gtk_button_get_label,
                gtk_button_new_with_mnemonic,
                gtk_button_set_label,
                gtk_cell_renderer_pixbuf_new,
                gtk_cell_renderer_text_new,
                gtk_check_button_new_with_mnemonic,
                gtk_radio_button_new_with_mnemonic,
                gtk_radio_button_get_group,
                gtk_check_version,
                gtk_clipboard_clear,
                gtk_clipboard_get,
--              gtk_clipboard_get_for_display,
                gtk_clipboard_set_text,
                gtk_clipboard_wait_for_text,
                gtk_clipboard_wait_is_text_available,
                gtk_combo_box_get_active,
                gtk_combo_box_set_active,
                gtk_combo_box_text_append_text,
                gtk_combo_box_text_get_active_text,
                gtk_combo_box_text_new,
                gtk_combo_box_text_new_with_entry,
                gtk_combo_box_text_remove,
                gtk_container_add,
                gtk_container_set_border_width,
                gtk_css_provider_new,
                gtk_css_provider_load_from_data,
                gtk_drawing_area_new,
                gtk_entry_new,
                gtk_entry_get_buffer,
                gtk_entry_buffer_get_text,
                gtk_entry_buffer_set_text,
                gtk_fixed_new,
                gtk_fixed_put,
                gtk_fixed_move,
                gtk_frame_new,
                gtk_frame_set_label,
                gtk_frame_get_label,
                gtk_get_current_event_time,
--              gtk_hbox_new,
                gtk_label_new,
                gtk_label_new_with_mnemonic,
                gtk_label_set_text_with_mnemonic,
                gtk_label_get_text,
--              gtk_list_store_newv,
                gtk_list_store_append,
--              gtk_list_store_set_valuesv, -- (borken)
                gtk_list_store_set,
                gtk_main,
                gtk_main_quit,
                gtk_menu_bar_new,
                gtk_menu_item_get_label,
                gtk_menu_item_set_label,
                gtk_menu_item_new_with_mnemonic,
                gtk_check_menu_item_new_with_mnemonic,
                gtk_check_menu_item_set_active,
                gtk_check_menu_item_get_active,
                gtk_radio_menu_item_get_group,
                gtk_radio_menu_item_new_with_mnemonic,
--              gtk_radio_menu_item_new_with_mnemonic_from_widget,
                gtk_menu_item_set_submenu,
                gtk_menu_new,
                gtk_menu_popup,
                gtk_menu_popup_at_pointer,
                gtk_menu_shell_append,
                gtk_notebook_get_n_pages,
                gtk_notebook_insert_page,
                gtk_notebook_new,
                gtk_notebook_set_scrollable,
                gtk_notebook_set_tab_label_text,
                gtk_orientable_set_orientation,
                gtk_progress_bar_new,
                gtk_progress_bar_set_bar_style,
                gtk_progress_bar_set_fraction,
                gtk_progress_bar_set_inverted,
                gtk_progress_bar_set_orientation,
                gtk_range_get_value,
                gtk_range_set_value,
                gtk_scale_new,  -- GTK3
                gtk_hscale_new, -- GTK2
                gtk_vscale_new, -- GTK2
                gtk_scrolled_window_new,
                gtk_scrolled_window_set_policy,
                gtk_separator_menu_item_new,
--              gtk_style_context_add_provider,
                gtk_style_context_add_provider_for_screen,
--              gtk_text_view_new,
                gtk_toggle_button_get_active,
                gtk_toggle_button_set_active,
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
--              gtk_vbox_new,
--              gtk_widget_get_allocated_width,
--              gtk_widget_get_allocated_height,
                gtk_widget_get_allocation,
                gtk_widget_get_can_focus,
--              gtk_widget_get_root_window,
                gtk_widget_get_parent,
                gtk_widget_get_sensitive,
                gtk_widget_get_style_context,
                gtk_widget_get_window,
                gtk_widget_grab_focus,
                gtk_widget_hide,
--              gtk_widget_is_sensitive,
                gtk_widget_modify_bg,
                gtk_widget_modify_font,
                gtk_widget_override_font,
                gtk_widget_queue_draw,
                gtk_widget_realize,
                gtk_widget_set_can_focus,
                gtk_widget_set_events,
                gtk_widget_set_realized,
                gtk_widget_set_sensitive,
                gtk_widget_set_size_request,
                gtk_widget_set_tooltip_text,
                gtk_widget_show,
                gtk_widget_show_all,
                gtk_widget_size_request,
--              gtk_widget_get_preferred_size,
--              gtk_window_is_active,
                gtk_window_move,
                gtk_window_new,
                gtk_window_set_default_size,
                gtk_window_set_title,
                gtk_window_set_transient_for,
                gtk_window_get_transient_for,
                gtk_window_get_position,
                gtk_window_get_size,
                gtk_window_get_title,
                gtk_window_resize,
                g_free,
                g_signal_connect_data,
                g_object_get_property,
                g_object_set_property,
                g_source_remove,
                g_timeout_add,
--              g_value_get_int,
--              g_value_init,
--              g_value_unset,
                pango_cairo_create_layout,
                pango_cairo_show_layout,
                pango_font_description_free,
                pango_font_description_from_string,
                pango_layout_context_changed,
                pango_layout_get_pixel_size,
                pango_layout_set_font_description,
                pango_layout_set_text,
                xg_object_unref,
                idGdkEventButton,
                idGdkEventConfigure,
                idGdkEventFocus,
                idGdkEventKey,
                idGdkEventMotion,
                idGdkRectangle,
                idGtkRequisition,
                GTK_ID_LOOKUP,
                GTK_MENU_LOOKUP,
                GTK_MENU_UPLOOK,
                GDK_TYPE_PIXBUF,

--              xAngleArc,
                xAppendMenu,           
                xArc,
                xBeginPaint,
                xBitBlt,
                xCallWindowProc,
                xCheckMenuItem,
                xCheckMenuRadioItem,
                xChord,    
                xCloseClipboard,
                xCreateCompatibleDC,
                xCreateDIBitmap,
                xCreateFontIndirect,
                xCreateMenu,
                xCreatePopupMenu,
                xCreatePen,
                xCreateSolidBrush,
                xCreateWindowEx,
                xDefWindowProc,
                xDeleteDC,
                xDeleteObject,
                xDestroyWindow,
                xDispatchMessage,
                xDrawMenuBar,          
                xDrawText,
                xEmptyClipboard,
                xEnableMenuItem,           
                xEnableWindow,         
                xEndPaint,
                xFillRect,         
                xGetClientRect,
                xGetClipboardData,         
                xGetConsoleWindow,
                xGetCursorPos,
                xGetDC,
                xGetDeviceCaps,
                xGetFocus,
                xGetForegroundWindow,
                xGetKeyState,
                xGetLastError,
--              xGetMenuItemCount,
--              xGetMenuItemID,
                xGetMenuItemInfo,
                xGetMenuState,         
--              xGetMenuString,
                xGetMessage,
                xGetPixel,
                xGetStockObject,
                xGetSystemMetrics,
                xGetTextExtentPoint32,
                xGetTextExtentPoint32W,
                xGetWindowLong,
                xGetWindowRect,
                xGetWindowText,
                xGetWindowTextLength,
                xGlobalAlloc,
                xGlobalFree,
                xGlobalLock,
                xGlobalSize,           
                xGlobalUnlock,
                xImageList_Add,
                xImageList_Create,
                xIsClipboardFormatAvailable,           
                xKillTimer,
                xLineTo,
                xLoadCursor,
                xLoadIcon,
                xMoveToEx,
                xMoveWindow,
                xOpenClipboard,
                xPie,
--              xPlaySound,
                xPolyBezier,           
                xPostQuitMessage,
                xRectangle,
                xRedrawWindow,
                xRegisterClassEx,
                xReleaseDC,
                xRoundRect,
                xSelectObject,
                xSendMessage,
                xSetBkMode,
                xSetClipboardData,
                xSetFocus,
                xSetMenu,
                xSetMenuItemInfo,                  
                xSetParent,
                xSetPixelV,
                xSetROP2,
                xSetScrollInfo,
                xSetTextAlign,
                xSetTextColor,
                xSetTimer,
                xSetWindowLong,
                xSetWindowPos,
                xSetWindowText,
                xShowWindow,
--              xTextOut,
                xTrackPopupMenuEx,
                xTransparentBlt,           
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
                idTVINSERTSTRUCT,
                idLOGFONT,
--              idPIXELFORMATDESCRIPTOR,
                idTOOLINFO,
                idSCROLLINFO,
                idTCITEM,
                idMENUITEMINFO,
                WINAPI_SUBMENUS, -- both key:handle, data:id 
                                 -- and key:{mid,id}, data:{menu,pos}
                WIN_MENU_CHECKED -- key:{mid,id}, data:0 or radio group as {{ids},mh}

local atom szAppName, pData, pPAINTSTRUCT, pRECT, pSIZE, pTVINSERTSTRUCT, 
                             pTVITEMEX, pMSG, pLOGFONT, pTOOLINFO, pPOINT,
--                          pPIXELFORMATDESCRIPTOR,
                             pSCROLLINFO, pTCITEM, pMENUITEMINFO, NullBrushID,
                             pGtkRequisition

local sequence gtk_version

local function xpg_to_bool(object v)
    -- convert "YES"/"NO"/etc to true/false (as a common requirement)
    if string(v) then
        string uv = upper(v)
        if find(uv,{"YES","ON","1","TRUE"}) then v = true elsif
           find(uv,{"NO","OFF","0","FALSE"}) then v = false
        else crash(`unrecognised("%s")`,{v}) end if
    else
        assert(v=true or v=false)
    end if
    return v
end function

--local function xpg_to_int(object v)
--  -- convert eg "20" to 20, but v may already be integer
--  if string(v) then
--      v = to_number(v)
--  end if
--  assert(integer(v))
--  return v
--end function


--DEV:
--/*
    function intints(val, w, h, name) {
        // version of intint() for dialog [raster]size, supporting eg "QUARTERxEIGHTH"
        if (typeof(val) === "string") {
            // convert eg "225x75" to [225,75]
            //  (ie a js Array of length 2)
            let x = val.indexOf('x'), y;
            if (x !== -1) {
                y = val.slice(x+1);
                x = val.slice(0,x);
                function fulleighth(s,x,f,name) {
                    let n = Number(s);
                    if (Number.isInteger(n)) {
                        if (name === "SIZE") {
                            n *= f;
                        }
                        return floor(n);
                    }
                    if (s === "FULL") { s = x; }
                    if (s === "HALF") { s = floor(x/2); }
                    if (s === "THIRD") { s = floor(x/3); }
                    if (s === "QUARTER") { s = floor(x/4); }
                    if (s === "EIGHTH") { s = floor(x/8); }
                    return s;
                }
                x = fulleighth(x,w,6/4,name);
                y = fulleighth(y,h,15/8,name);
                if (Number.isInteger(x) &&
                    Number.isInteger(y)) {
                    val = [x,y];
                }
            }
        }
        if (!Array.isArray(val) ||
            val.length !== 2) {
            crash("invalid intint value");
        }
        return val;
    }
--*/
--DEV (re-?)translate this for xpGUI.js:
--DEV/SUG allow "px:10x15" == "10x15", "pt|[r]em:10x15" in points/[relative]em...
local function xpg_intint(string val)
    -- convert eg "225x75" to {225,75}, and in fact 
    --            "50x10x20x30" to {50,10,20,30},
    --            "1"->{1}, and "1x2x3"->{1,2,3}.
--  sequence res = apply(split(val,'x',false),to_number)
    sequence res
    if val[1]='{' then
        assert(val[$]='}')
        res = split(val[2..-2],',',false)
    else
        res = split(val,'x',false)
    end if
    res = apply(true,to_number,{res,0})

    -- It could do this provided callers ignored any -1, which 
    -- would make "50x" not the same as "50x0", matching pGUI.
--  sequence res = apply(true,to_number,{split(val,'x',false),-1})
    return res
end function

--?xpg_intint("1")
--?xpg_intint("1x2x3")
--?xpg_intint("225x75")
--?xpg_intint("50x10x20x30")
--?xpg_intint("50x")

global function gGetDialog(gdx id)
    while id and ctrl_types[id]!=DIALOG do
        id = parent_ids[id]
    end while
    return id
end function

--gdk_window_get_width() and gdk_window_get_height() instead, 
--because it avoids the global 
local function xpg_get_window_rect(gdx id)
--global function xpg_get_window_rect(gdx id)
    -- returns {left,top,width,height}
--  xpg_handle handle = ctrl_handles[id]
    atom handle = ctrl_handles[id]
    integer ct = ctrl_types[id],
            left, top, width, height
    if backend=GTK then
--/*
        idGdkRectangle = define_struct("""typedef struct GdkRectangle {
                                          int x;
                                          int y;
                                          int width;
                                          int height;
                                        }""")
        woff = get_field_details(idGdkRectangle,"width")[1]
        hoff = get_field_details(idGdkRectangle,"height")[1]
--              integer width = get_struct_field(idGdkRectangle,pRECT,"width"),
--                     height = get_struct_field(idGdkRectangle,pRECT,"height")

        idRECT = define_struct("""typedef struct _RECT {
                                    LONG left;
                                    LONG top;
                                    LONG right;
                                    LONG bottom;
                                  } RECT, *PRECT;""")
                    r = c_func(xGetWindowRect,{handle,pRECT})
                    assert(r!=0)
--DEV suspect... (not really, but let's just code this way throughout...)
--                  sequence wrect = peek4s({pRECT,4})
                    integer ww = get_struct_field(idRECT,pRECT,"right")
                               - get_struct_field(idRECT,pRECT,"left"),
                            wh = get_struct_field(idRECT,pRECT,"bottom")
                               - get_struct_field(idRECT,pRECT,"top")

--*/
        -- [DEV] seems fine on windows 64-bit, might not be on Linux 64 bit...
--      atom pX = pRECT, pY = pX+4, pW = pY+4, pH = pW+4
--DEV/SUG assign these just the once, when pRECT is created?
        atom pX = pRECT+get_field_details(idGdkRectangle,"x")[1],
             pY = pRECT+get_field_details(idGdkRectangle,"y")[1],
             pW = pRECT+get_field_details(idGdkRectangle,"width")[1],
             pH = pRECT+get_field_details(idGdkRectangle,"height")[1]
        if ct!=DIALOG then
            handle = ctrl_handles[gGetDialog(id)]
        end if
--      if ctrl_types[id]=DIALOG then
            c_proc(gtk_window_get_position,{handle,pX,pY})
            c_proc(gtk_window_get_size,{handle,pW,pH})
--      else
--          atom window = c_func(gtk_widget_get_window,{handle})
--?{"WINDOW",window,handle}
--          c_proc(gtk_window_get_position,{window,pX,pY})
--          c_proc(gtk_window_get_size,{window,pW,pH})
--      end if
          left = get_struct_field(idGdkRectangle,pRECT,"x")
           top = get_struct_field(idGdkRectangle,pRECT,"y")
         width = get_struct_field(idGdkRectangle,pRECT,"width")
        height = get_struct_field(idGdkRectangle,pRECT,"height")
        if ct=DIALOG then -- GTK yields the client size...
--?"gwr" 
            atom window = c_func(gtk_widget_get_window,{handle})
            c_proc(gdk_window_get_origin,{window,pX,pY})
            -- (pW, pH are used as pFrameX, pFrameY here)
            c_proc(gdk_window_get_root_origin,{window,pW,pH})
            integer border = get_struct_field(idGdkRectangle,pRECT,"x")
                           - get_struct_field(idGdkRectangle,pRECT,"width"),
                    caption = get_struct_field(idGdkRectangle,pRECT,"y")
                            - get_struct_field(idGdkRectangle,pRECT,"height")-border
            width += 2*border
            height += 2*border+caption
--?"gwre"
        end if
--/*

static void gtkDialogGetWindowDecor(Ihandle* ih, int *win_border, int *win_caption)
{
  int x, y, frame_x, frame_y;
  gdk_window_get_origin(gtk_widget_get_window(ih->handle), &x, &y);
  gdk_window_get_root_origin(gtk_widget_get_window(ih->handle), &frame_x, &frame_y);
  *win_border = x-frame_x;
  *win_caption = y-frame_y-*win_border;
}
  if (w) *w = width + 2*border;
  if (h) *h = height + 2*border + caption;  /* menu is inside the dialog_manager */
--*/
    elsif backend=WinAPI then
        integer r = c_func(xGetWindowRect,{handle,pRECT})
        assert(r!=0)
          left = get_struct_field(idRECT,pRECT,"left")
           top = get_struct_field(idRECT,pRECT,"top")
         width = get_struct_field(idRECT,pRECT,"right")-left
        height = get_struct_field(idRECT,pRECT,"bottom")-top
        if ct=DIALOG then
            if and_bits(ctrl_flags[id],CF_RESIZE) then
                width -= 14; height -= 7;
            else
                width -= 4; height -= 12;
            end if
        end if
    else
        ?9/0 -- (unknown backend)
    end if
--DEV suspect...
--  sequence res = peek4s({pRECT,4})
--  if backend=WinAPI then
--      res[3] -= res[1]    -- right ==> width
--      res[4] -= res[2]    -- btm ==> height
--  end if
--  return res
    return {left,top,width,height}
end function

local function xpg_get_client_rect(gdx id)
    -- returns {0,0,width,height}
    xpg_handle handle = ctrl_handles[id]
    integer width, height
    if backend=GTK then
        -- [DEV] seems fine on windows 64-bit, might not be on Linux 64 bit...
-->
--      atom pX = pRECT, pY = pX+4, pW = pY+4, pH = pW+4
--      poke4(pRECT,{0,0})
--NO HELP...
--if ctrl_types[id]=DIALOG then
--  assert(length(children_ids[id])=1)
--  integer child = children_ids[id][1]
--  handle = ctrl_handles[child]
--end if
        atom pW = pRECT+get_field_details(idGdkRectangle,"width")[1],
             pH = pRECT+get_field_details(idGdkRectangle,"height")[1]
        c_proc(gtk_window_get_size,{handle,pW,pH})
          width = get_struct_field(idGdkRectangle,pRECT,"width")
         height = get_struct_field(idGdkRectangle,pRECT,"height")
    elsif backend=WinAPI then
        integer r = c_func(xGetClientRect,{handle,pRECT})
        assert(r!=0)
--      integer left = get_struct_field(idRECT,pRECT,"left")
--               top = get_struct_field(idRECT,pRECT,"top")
--       width = get_struct_field(idRECT,pRECT,"right")-left
--      height = get_struct_field(idRECT,pRECT,"bottom")-top
         width = get_struct_field(idRECT,pRECT,"right")
        height = get_struct_field(idRECT,pRECT,"bottom")
    else
        ?9/0 -- (unknown backend)
    end if
    return {0,0,width,height}
end function

local function xpg_lm_get_dialog_decoration_size(gdx id)
    assert(ctrl_types[id]=DIALOG)
    sequence w = xpg_get_window_rect(id),
             c = xpg_get_client_rect(id)
    integer width = w[3]-c[3],
            height = w[4]-c[4]
    -- or maybe, border is height/2... (nah)
--?{"lmgdds",w,c,width,height}
?{"lmgdds",width,height}
    return {width,height}
end function

--local function xpg_gtk_subtract_dialog_decorations(gdx id, integer dw, dh)
----local function xpg_lm_get_dialog_client_size(id)
--  if dw or dh then
--      -- subtract dialog decorations
--      sequence w = xpg_get_window_rect(id),
--               c = xpg_get_client_rect(id)
--?{"winrect",w}
--?{"clientrect",c}
--?{dw,dh}
--      if dw then dw -= w[3]-c[3] end if
--      if dh then dh -= w[4]-c[4] end if
--?{dw,dh}
--  end if
--  return {dw,dh}
--end function

--DEV:
local sequence fontcache = {},
             cachedfonts = {},
        cached_fontnames = {}

procedure xpg_set_font(gdx id, string v, atom angle=0)
    ctrl_fonts[id] = v
    atom handle = ctrl_handles[id], fontdesc
    integer ct = ctrl_types[id],
         comma = find(',',v),
      fontsize = 0, k
    string face = v[1..comma-1],
       facerest = "",
         ctname = ctrl_names[ct] -- (debug aid)
    sequence styles = {}, fsf
    if comma then
        styles = split(v[comma+1..$])
        fontsize = to_integer(styles[$])
        if fontsize then styles = styles[1..-2] end if
        if find(styles,{{"Normal"},{"normal"}}) then styles = {} end if
        for i,s in styles do
--          assert(find(s,{"Bold","Italic","Underline","Strikeout"}))
            integer s1 = s[1], us1 = upper(s1)  
            if s1!=us1 then s[1] = us1; styles[i] = s end if
            assert(find(s,{"Bold","Italic"}))
        end for
        for s in styles do
            facerest &= " "&s
        end for
    end if
    if fontsize<0 then
        facerest &= sprintf(" %dpx",-fontsize)
    elsif fontsize then
        facerest &= sprintf(" %d",fontsize)
    end if
    if backend=GTK then
        if find(face,{"Helvetica","Arial"}) then
            face = "Sans"
        elsif find(face,{"Courier","Courier New"}) then
            face = "Monospace"
        elsif find(face,{"Times","Times New Roman"}) then
            face = "Serif"
        elsif not find(face,{"Sans","Monospace","Serif"}) then
            printf(1,"Warning: FONT %s may not be supported...\n",{face})
        end if
        fsf = {face,styles,fontsize}
        k = find(fsf,fontcache)
        if k then
            fontdesc = cachedfonts[k]
        else
            fontdesc = c_func(pango_font_description_from_string,{face&facerest})
            ctrl_font[id] = 0
            for k=length(fontcache) to 1 by -1 do
                if not find(k,ctrl_font) then exit end if
            end for
            if k then
                fontcache[k] = fsf
                c_proc(pango_font_description_free,{cachedfonts[k]})
                cachedfonts[k] = fontdesc
            else
                fontcache = append(fontcache,fsf)
                cachedfonts &= fontdesc
                cached_fontnames &= 0
                k = length(fontcache)
            end if
            ctrl_font[id] = k
            cached_fontnames[k] = face&","&facerest
        end if
--19/6/23 turns out we don't need this, gCanvasText uses it, via gGetTextExtent, anyway...
--          (it actually caused a non-stop busy-redraw loop)
--NOTE: we may still need it when id is not a canvas... (erm, put that back in without testing)
--if fOK then
        if ct!=CANVAS then
            if bGTK3 then
                c_proc(gtk_widget_override_font,{handle,fontdesc})
            else
                c_proc(gtk_widget_modify_font,{handle,fontdesc})
            end if
        end if
        ctrl_font[id] = k
        ctrl_fontd[id] = fontdesc
--DEV no! we want to cache fonts!
--      if fontdesc!=NULL then
--          c_proc(pango_font_description_free,{fontdesc})
--      end if
    elsif backend=WinAPI then
        if ct=CANVAS then
            -- (WinAPI, save for (re-)applying lfEscapement/lfOrientation)
            ctrl_xtra[id][CX_TXTANGLE] = angle
        end if
        if find(face,{"Helvetica","Sans"}) then
            face = "Arial"
        elsif find(face,{"Courier","Monospace"}) then
            face = "Courier New"
        elsif find(face,{"Times","Serif"}) then
            face = "Times New Roman"
        end if
        atom hDC = c_func(xGetDC,{handle}), hFont
        fsf = {face,styles,fontsize}
        k = find(fsf,fontcache)
--?{fsf,k}
        if k then
            hFont = cachedfonts[k]
        else
            if fontsize>0 then
                fontsize = floor(-c_func(xGetDeviceCaps,{hDC,LOGPIXELSY})*fontsize/72)
            end if
            set_struct_field(idLOGFONT,pLOGFONT,"lfHeight",fontsize)
            set_struct_field(idLOGFONT,pLOGFONT,"lfCharSet",DEFAULT_CHARSET)
            set_struct_field(idLOGFONT,pLOGFONT,"lfEscapement",-angle*10)
            set_struct_field(idLOGFONT,pLOGFONT,"lfOrientation",-angle*10)
--          set_struct_field(idLOGFONT,pLOGFONT,"lfQuality",DEFAULT_QUALITY (==0)) -- (best)

            assert(length(face)<LF_FACESIZE)
            set_struct_field(idLOGFONT,pLOGFONT,"lfFaceName",face&0)
            integer fw = iff(find("Bold",styles)?FW_BOLD:FW_NORMAL),
                    ib = find("Italic",styles)!=0,
                    ub = find("Underline",styles)!=0,
                    sb = find("Strikeout",styles)!=0
            set_struct_field(idLOGFONT,pLOGFONT,"lfWeight",fw)
            set_struct_field(idLOGFONT,pLOGFONT,"lfItalic",ib)
            set_struct_field(idLOGFONT,pLOGFONT,"lfUnderline",ub)
            set_struct_field(idLOGFONT,pLOGFONT,"lfStrikeOut",sb)
            hFont = c_func(xCreateFontIndirect,{pLOGFONT})
            ctrl_font[id] = 0
            for k=length(fontcache) to 1 by -1 do
                if not find(k,ctrl_font) then exit end if
            end for
            if k then
                fontcache[k] = fsf
                c_proc(xDeleteObject,{cachedfonts[k]})
                cachedfonts[k] = hFont
            else
                fontcache = append(fontcache,fsf)
                cachedfonts &= hFont
                cached_fontnames &= 0
                k = length(fontcache)
            end if
--          ctrl_font[id] = k
--          face &= facerest
            cached_fontnames[k] = face&","&facerest
--          cached_fontnames[k] = v
        end if
        atom prevFont = c_func(xSelectObject,{hDC,hFont})
        -- (from MSDN: "this message does not return a value")
        {} = c_func(xSendMessage,{handle,WM_SETFONT,hFont,true})
--DEV no, we want to cache fonts...
--      c_proc(xDeleteObject,{prevFont})
        bool bOK = c_func(xReleaseDC,{handle,hDC})
        assert(bOK,`xpg_set_font(%s,"%s",%d) failure`,{ctname,v,angle})
--if not bOK then ?"not bOK: xpg_set_font line 2579..." end if
        ctrl_font[id] = k
        ctrl_fontd[id] = hFont
    else
        ?9/0 -- (unknown backend)
    end if
end procedure

global function gGetTextExtent(gdx id, sequence text, bool bSumHeights=true)
    integer w = 0, h = 0, wi, hi, ct = ctrl_types[id]
    atom handle = ctrl_handles[id], cairo=NULL, layout, pW, pH, hDC
    assert(handle!=NULL)
    bool bOK, bDestroy = false
    if backend=GTK then
        if ct=CANVAS then
            -- (re-use the existing, when available:)
            {cairo,layout} = ctrl_xtra[id][CX_CANVAS_HDC]
        end if
        if cairo=NULL then
            bDestroy = true
            atom window = c_func(gtk_widget_get_window,{handle})
--?"cairo_create(GTE)"
            cairo = c_func(gdk_cairo_create,{window})
            layout = c_func(pango_cairo_create_layout,{cairo})
        end if
--      atom fontdesc = ctrl_fontd[id]
--      if fontdesc!=NULL then
--          c_proc(pango_layout_set_font_description,{layout,fontdesc})
--      end if
        integer k = ctrl_font[id]
        if k then
            c_proc(pango_layout_set_font_description,{layout,cachedfonts[k]})
        end if
        pW = pRECT+get_field_details(idGdkRectangle,"width")[1]
        pH = pRECT+get_field_details(idGdkRectangle,"height")[1]
    else
        hDC = c_func(xGetDC,{handle})
--      atom hFont = ctrl_fontd[id]
        integer k = ctrl_font[id]
        assert(k!=UNDEFINED)
        atom hFont = cachedfonts[k]
----DEV scan up?? (if this ever triggers)
--      assert(hFont!=NULL and hFont!=UNDEFINED)
        hFont = c_func(xSelectObject,{hDC,hFont})
    end if
--  if string(text) then text = split(text,'\n') end if
    if string(text) then text = {text} end if
    for line in text do
        if backend=GTK then
            c_proc(pango_layout_set_text,{layout,line,length(line)})
            c_proc(pango_layout_get_pixel_size,{layout,pW,pH})
            wi = get_struct_field(idGdkRectangle,pRECT,"width")
            hi = get_struct_field(idGdkRectangle,pRECT,"height")
        else
            bOK = c_func(xGetTextExtentPoint32,{hDC,line,length(line),pSIZE})
            assert(bOK)
            wi = get_struct_field(idSIZE,pSIZE,"cx")
            hi = get_struct_field(idSIZE,pSIZE,"cy")
        end if
        w = max(w,wi)
        if bSumHeights then
            h += hi
        else
            h = hi
        end if
    end for
    if backend=GTK then
        if bDestroy then
            c_proc(xg_object_unref,{layout})
--?"cairo_destroy(GTE)"
            c_proc(cairo_destroy,{cairo})
        end if
    else
        bOK = c_func(xReleaseDC,{handle,hDC})
        assert(bOK)
    end if
    return {w,h}
end function

local function xpg_return_default_attr(gdx id, string name, object dflt)
    if dflt=999_999_999 then
        string cn = iff(id?ctrl_names[ctrl_types[id]]:"NULL")
        crash("gGetAttribute(%s,%s)",{cn,name})
    end if
    return dflt
end function

global function gGetAttribute(gdx id, string name, object dflt=999_999_999)
    if id!=0 then
        integer ct = ctrl_types[id]
        if name="CLASSNAME" then 
            return ctrl_names[ct]
        elsif name="EXPAND" then
            integer hvdx = and_bits(ctrl_flags[id],CF_EXPANDH+CF_EXPANDV)+1
            return {"NONE","HORIZONTAL","VERTICAL","BOTH"}[hvdx]
        elsif name="GAP" then
            assert(ct=BOX)
            return ctrl_xtra[id][CX_BOX_GAP]
        elsif name="MARGIN"
           or name="PADDING" then
--/*
            object cmi = ctrl_mp[id]
            if atom(cmi) then return 0 end if
            integer mpdx = find(name,{"MARGIN","PADDING"})
            if ct=DIALOG then mpdx = 1 end if
--          assert(mpdx=MP_MARGIN or mpdx=MP_PADDING) --- (dev search aid..)
            return cmi[mpdx]
--*/
--/!* DEV/SUG:
            assert(ct!=DIALOG,"no margin or padding on dialogs!")
--          integer mpdx = iff(name="MARGIN" or ct=DIALOG?SZ_MARGIN:SZ_PADDING)
            integer mpdx = iff(name="MARGIN"?SZ_MARGIN:SZ_PADDING)
--          assert(mpdx=SZ_MARGIN or ct!=DIALOG,"no padding on dialogs!")
            assert(mpdx=SZ_MARGIN or find(ct,{BUTTON,DATEPICK,DROPDOWN,TEXT}))
--      Note that border has direct use on <a href="gButton.htm">gButton</a>, <a href="gDatePick.htm">gDatePick</a>, 
--      <a href="gDropDown.htm">gDropDown</a>, <a href="gFrame.htm">gFrame</a>, <a href="gProgressBar.htm">gProgressBar</a>, and
--      <a href="gText.htm">gText</a>, where it is a fixed non-overrideable width of 1, and there is no sense allowing it to be set on 

--          object mp = ctrl_size[mpdx]
            return ctrl_size[id][mpdx] -- 0 | {1} | {1,2} | {1,2,3} | {1,2,3,4}
--*!/
        elsif name="MINSIZE"
           or name="MAXSIZE" then
            integer mmdx = find(name,{"MINSIZE","MAXSIZE"}),
               {mwx,mhx} = {{SZ_MIN_W,SZ_MIN_H},
                            {SZ_MAX_W,SZ_MAX_H}}[mmdx]
            return {ctrl_size[id][mwx],
                    ctrl_size[id][mhx]}
--/* DEV/SUG:
            integer mmdx = iff(name="MINSIZE"?SZ_MIN:SZ_MAX)
            object csm = ctrl_size[id][mmdx]
            return iff(atom(csm)?{0,0}:csm)
--*/
        end if
        bool bMapped = and_bits(ctrl_flags[id],CF_MAPPED)!=0
        if bMapped then
            atom handle = ctrl_handles[id]
            integer w, h
            if name="ACTIVE" then
                -- do it this way for inheritance, and virtual controls
                while id do
                    if and_bits(ctrl_flags[id],CF_INACTIVE) then
                        return false
                    end if
                    id = parent_ids[id]
                end while
                return true
            elsif name="CANFOCUS" then
                if backend=GTK then
                    return c_func(gtk_widget_get_can_focus,{handle})
                elsif backend=WinAPI then
                    atom dwStyle = c_func(xGetWindowLong,{handle,GWL_STYLE})
                    return and_bits(dwStyle,WS_TABSTOP)!=0
                else
                    ?9/0 -- (unknown backend)
                end if
--          elsif name="SIZE" then
----NB from gCanvas, untested on anything else...
----better (once the layout manager is working)
--              return {ctrl_size[id][SZ_W],
--                      ctrl_size[id][SZ_H]}
--          elsif name="NATURALSIZE" then
            elsif name="NATURALSIZE" 
               or name="SIZE" then
if false then
                return {ctrl_size[id][SZ_NATURAL_W],
                        ctrl_size[id][SZ_NATURAL_H]}
else --DEV temp:
                if backend=GTK then
                    if bGTK3 then
                        c_proc(gtk_widget_size_request,{handle,pGtkRequisition})
                        return {get_struct_field(idGtkRequisition,pGtkRequisition,"width"),
                                get_struct_field(idGtkRequisition,pGtkRequisition,"height")}
                    end if
                    atom window = c_func(gtk_widget_get_window,{handle})
----                atom window = handle
                    return {c_func(gdk_window_get_width,{window}),
                            c_func(gdk_window_get_height,{window})}
--                  c_proc(gtk_widget_size_request,{handle,pGtkRequisition})
--                  return {get_struct_field(idGtkRequisition,pGtkRequisition,"width"),
--                          get_struct_field(idGtkRequisition,pGtkRequisition,"height")}
--  gdk_drawable_get_size(window, &w, &h);
--  gtk_window_get_size(window, &w, &h);
--                  atom pW = pRECT+4, pH = pW+4
--                  c_proc(gtk_window_get_size,{window,pW,pH})
--                  return peek4s({pW,2})
--                  return {c_func(gtk_widget_get_allocated_width,{handle}),
--                          c_func(gtk_widget_get_allocated_height,{handle})}
--                  c_proc(gtk_widget_get_allocation,{handle,pRECT})
--                  integer width = get_struct_field(idGdkRectangle,pRECT,"width"),
--                         height = get_struct_field(idGdkRectangle,pRECT,"height")
--                  return {width,height}
                elsif backend=WinAPI then
                    if ct=LABEL then
                        string title = gGetAttribute(id,"TITLE","")
                        {w,h} = gGetTextExtent(id,split(title,'\n'))
--?{"GAlabel",title,w,h} 
                    else
                        integer r = c_func(xGetWindowRect,{handle,pRECT})
                        assert(r!=0)
--DEV suspect...
-->
--/*
        idRECT = define_struct("""typedef struct _RECT {
                                    LONG left;
                                    LONG top;
                                    LONG right;
                                    LONG bottom;
                                  } RECT, *PRECT;""")
        woff = get_field_details(idGdkRectangle,"width")[1]
        hoff = get_field_details(idGdkRectangle,"height")[1]
                    r = c_func(xGetWindowRect,{handle,pRECT})
                    assert(r!=0)
--DEV suspect... (not really, but let's just code this way throughout...)
--                  sequence wrect = peek4s({pRECT,4})
                    integer ww = get_struct_field(idRECT,pRECT,"right")
                               - get_struct_field(idRECT,pRECT,"left"),
                            wh = get_struct_field(idRECT,pRECT,"bottom")
                               - get_struct_field(idRECT,pRECT,"top")
--*/
--                  sequence res = peek4s({pRECT,4})
--                  return {res[3]-res[1],res[4]-res[2]}
                        w = get_struct_field(idRECT,pRECT,"right")
                          - get_struct_field(idRECT,pRECT,"left")
                        h = get_struct_field(idRECT,pRECT,"bottom")
                          - get_struct_field(idRECT,pRECT,"top")
                    end if
                    return {w,h}
                else
                    ?9/0 -- (unknown backend)
                end if
end if
            elsif name="USERSIZE" then
                return {ctrl_size[id][SZ_USER_W],
                        ctrl_size[id][SZ_USER_H]}
            elsif name="FONT" then
--DEV inherit? (put in but untested)
--              return ctrl_fonts[id]
--/*
                object font = ctrl_fonts[id]
                while font=UNDEFINED and parent_ids[id] do
                    id = parent_ids[id]
                    font = ctrl_fonts[id]
                end while
--DEV
--/*
                if font!=UNDEFINED then
                    font &= ???
                end if
--*/
                return font
--*/
                integer font = ctrl_font[id]
                while font=UNDEFINED and parent_ids[id] do
                    id = parent_ids[id]
                    font = ctrl_font[id]
                end while
                string fontname = cached_fontnames[font]
                return fontname
            end if
--22/6/23
--/*
            integer geta = ctrl_msg[ct][CM_GET]
--?{name,id,geta,get_routine_info(geta)}
            return geta(id,name,dflt)
        end if
--*/
        else
--DEV/SUG
--          integer iid = id
--          while iid do
            sequence did = deferred_attr[id]
            for k=length(did) to 1 by -1 do
                if did[k][1]=name then
                    return did[k][2]
                end if
            end for
--          if not find(name,{"ACTIVE","FONT"}) then exit end if
--          iid = parent_ids[iid]
--      end while
--22/6/23
        end if
        if bMapped or and_bits(ctrl_flags[id],CF_UNMAPATTR)!=0 then
            integer geta = ctrl_msg[ct][CM_GET]
--?{name,id,geta,get_routine_info(geta)}
            return geta(id,name,dflt)
        end if
    end if
    return xpg_return_default_attr(id,name,dflt)
end function

global function gGetInt(gdx id, string name, object dflt=999_999_999)
    if name="EXPAND" then
        return and_bits(ctrl_flags[id],CF_EXPANDH+CF_EXPANDV) -- 0..3
    end if
    integer res = gGetAttribute(id,name,dflt)
    return res
end function

global function gGetIntInt(gdx id, string name, object dflt={0,0})
    sequence res = gGetAttribute(id,name,dflt)
    assert(length(res)=2 and integer(res[1]) and integer(res[2]))
    return res
end function

global function gGetDouble(gdx id, string name, object dflt=999_999_999)
    atom res = gGetAttribute(id,name,dflt)
    return res
end function

local function xpg_gtk_mnemonicalize(string s)
    -- convert eg "&Help" to "_Help" for GTK, more examples below
    string res = ""
    integer l = length(s), skip = false
    for i=1 to l do
        if skip then
            skip = false
        else
            integer c = s[i]
            if c='_' then
                res &= "__"
            elsif c!='&' or i=l then
                res &= c
            elsif s[i+1]='&' then
                res &= c
                skip = true
            else
                res &= '_'
            end if
        end if
    end for
    return res
end function

--constant tests = {{"",""},
--                {"&&","&"},
--                {"&x","_x"},
--                {"_x","__x"},
--                {"x_&y","x___y"},
--                {"&&&&&&","&&&"},
--                {"&1&2&3","_1_2_3"},
--                {"&&&2&&","&_2&"},
--                {"&1&&&3","_1&_3"}}
--for t in tests do
--  string {p,q} = t, r = xpg_gtk_mnemonicalize(p)
--  if r!=q then
--      printf(1,"%s: wanted %s but got %s\n",{p,q,r})
--  end if
--end for

local function xpg_demnemonicalize(string s)
    -- convert eg "&Help" to "Help" for sizing, more examples below
    string res = ""
    integer l = length(s), skip = false,
          ech = iff(backend=GTK?'_':
                iff(backend=WinAPI?'&':9/0))
    for i=1 to l do
        if skip then
            skip = false
        else
            integer c = s[i]
            if c!=ech or i=l then
                res &= c
            else
                res &= s[i+1]
                skip = true
            end if
        end if
    end for
    return res
end function

--/*
constant dtest = {{GTK,{{"",""},
                        {"__","_"},
                        {"_x","x"},
                        {"&x","&x"},
                        {"x&_y","x&y"},
                        {"______","___"},
                        {"_1_2_3","123"},
                        {"___2__","_2_"},
                        {"_1___3","1_3"},
                        {"_Help","Help"}}},
                  {WinAPI,{{"",""},
                           {"&&","&"},
                           {"&x","x"},
                           {"_x","_x"},
                           {"x_&y","x_y"},
                           {"&&&&&&","&&&"},
                           {"&1&2&3","123"},
                           {"&&&2&&","&2&"},
                           {"&1&&&3","1&3"},
                           {"&Help","Help"}}}}
procedure d_test()
    for b in dtest do
        backend = b[1]
        for t in b[2] do
            string {p,q} = t, r = xpg_demnemonicalize(p)
            if r!=q then
                printf(1,"%s: wanted %s but got %s (%s)\n",{p,q,r,backdesc[backend]})
            end if
        end for
    end for
--Xstring dNULL = xpg_demnemonicalize(NULL)
--Xif dNULL!="" then
--X printf(1,"NULL: wanted `` but got %s\n",{dNULL})
--Xend if
    backend = find(platform(),SPLAT)
end procedure
d_test()
--*/

--DEV... (killme)
--local function xpg_margap(gdx id)
--  integer ct = ctrl_types[id]
--  if ct!=BOX then return {{0,0,0,0},0} end if
--  return {ctrl_xtra[id][CX_BOX_MARGIN],
--          ctrl_xtra[id][CX_BOX_GAP]}
--end function

local function xpg_get_mp(gdx id, integer mp, mpx)
    -- get the margin or padding
--  -- mp should be either MP_MARGIN or MP_PADDING
    -- mp should be either SZ_MARGIN or SZ_PADDING
    -- mpx should be MP_TOP, MP_RGT, MP_BTM, or MP_LFT
--  object cmi = ctrl_mp[id]
    object cmi = ctrl_size[id][mp]
    if atom(cmi) then return 0 end if
--  object cmimp = cmi[mp]
--  if atom(cmimp) then return cmimp end if
    -- short sequences spell trbl...
    sequence mpdx = {{1,1,1,1},
                     {1,2,1,2},
                     {1,2,3,2},
                     {1,2,3,4}}[length(cmi)]
--                   {1,2,3,4}}[length(cmimp)]
    integer mpdxi = mpdx[mpx]
    return cmi[mpdxi]
--  return cmimp[mpdxi]
end function

local procedure xpg_lm_set_element_sizes(gdx id)
    integer ct = ctrl_types[id]
--if ct=CANVAS then ?{"ans(CANVAS)",ctrl_size[id][SZ_NATURAL_W],ctrl_size[id][SZ_NATURAL_H]} end if
    object children = children_ids[id]
    if sequence(children) then
--/*
--      integer {mt,mr,mb,ml} = xpg_margins(ct),
--              nw = ml, nh = mt, 
--      integer {{nh,mr,mb,nw},gap} = xpg_margap(id),
        integer nh = xpg_get_mp(id,MP_PADDING,MP_TOP),
                nw = xpg_get_mp(id,MP_PADDING,MP_LFT),
                gap2 = iff(ct=BOX?ctrl_xtra[id][CX_BOX_GAP]:0),
                gap = 0
--              cl = length(children)
--if ct=BOX then ?{id,xpg_margap(id)} end if
        bool bVert = and_bits(ctrl_flags[id],CF_VERTICAL)!=0
--*/
        for i,child in children do
            xpg_lm_set_element_sizes(child)
--/*
--          integer cw = ctrl_size[child][SZ_NATURAL_W],
--                  ch = ctrl_size[child][SZ_NATURAL_H],
            integer cw = max(ctrl_size[child][SZ_NATURAL_W],ctrl_size[child][SZ_USER_W]),
                    ch = max(ctrl_size[child][SZ_NATURAL_H],ctrl_size[child][SZ_USER_H]),
                    mt = xpg_get_mp(child,MP_MARGIN,MP_TOP),
                    ml = xpg_get_mp(child,MP_MARGIN,MP_LFT),
                    mb = xpg_get_mp(child,MP_MARGIN,MP_BTM),
                    mr = xpg_get_mp(child,MP_MARGIN,MP_RGT)
?{"lmas",child,cw,ch}
--          ctrl_size[child][SZ_W] = cw
--          ctrl_size[child][SZ_H] = ch
            cw = ml+cw+mr
            ch = mt+ch+mb
--ctrl_mp
--          integer cw = ctrl_size[child][SZ_W],
--                  ch = ctrl_size[child][SZ_H]
            -- aside: for a single child this is of course
            --        equivalent to nw:=cw and nh:=ch.
            nw = iff(bVert?max(nw,cw):nw+gap+cw)
            nh = iff(bVert?nh+gap+ch:max(nh,ch))
            if i=1 then gap = gap2 end if

--          if bVert then cy += ch else cx += cw end if
--DEV
--          gap = ???
--          if i=1 and ct=BOX then
--              gap = ctrl_xtra[id][CX_BOX_GAP]
--          end if
--*/
        end for
--/*
--DEV...
--      if ct=BOX then
        nw += xpg_get_mp(id,MP_PADDING,MP_RGT)
        nh += xpg_get_mp(id,MP_PADDING,MP_BTM)
--      end if
--      ctrl_size[id][SZ_W] = nw
--      ctrl_size[id][SZ_H] = nh
        if ct=FRAME then
--DEV default margins of {2,2,2,2}... (should now be done)
--          nw += 4
--          nh += 4
            string title = gGetAttribute(id,"TITLE","")
--?{"frame title",title}
            if length(title) then
                integer {tw,th} = gGetTextExtent(id,title)
                nh += th -- (height)
                --DEV/SUG minwidth from tw??
            end if
        elsif ct=DIALOG then
--local function xpg_get_window_rect(gdx id)
--local function xpg_get_client_rect(gdx id)
--local function xpg_gtk_subtract_dialog_decorations(gdx id, integer dw, dh)
            integer {dw,dh} = xpg_lm_get_dialog_decoration_size(id)
            nw += dw
            nh += dh
        end if
--      if backend=GTK
--      and (ct=FRAME or ct=BOX or ct=DIALOG) then -- [or, probably, DATEPICK, TABS]
--      if ct=FRAME or ct=BOX or ct=DIALOG then -- [or, probably, DATEPICK/GTK, TABS]
            ctrl_size[id][SZ_NATURAL_W] = nw
            ctrl_size[id][SZ_NATURAL_H] = nh
--      end if
--*/
    elsif find(ct,{BUTTON,CHECKBOX,DROPDOWN,LABEL,TEXT}) then
--     or (ct=CANVAS and xpg_sizeable_custom_canvas_control(id)) then
--     or (ct=CANVAS and find(ctrl_xtra[id][CX_??],{BUTTON,CHECKBOX,DROPDOWN,LABEL,TEXT})) then
        integer w, h
        string title = gGetAttribute(id,"TITLE","")
        if backend=GTK then
--      if backend=GTK and ct!=CANVAS then
            atom handle = ctrl_handles[id]
            c_proc(gtk_widget_size_request,{handle,pGtkRequisition})
            w = get_struct_field(idGtkRequisition,pGtkRequisition,"width")
            h = get_struct_field(idGtkRequisition,pGtkRequisition,"height")
--?{"gtknatsize",w,h,title}
        elsif backend=WinAPI then
            if ct=BUTTON
            or ct=CHECKBOX
            or ct=LABEL then
                {w,h} = gGetTextExtent(id,split(xpg_demnemonicalize(title),'\n'))
--DEV default padding???
                if ct=BUTTON then
                    w += 21
                    h += 11
                elsif ct=CHECKBOX then
                    w += 26
                    h += 14
--no help...
--              elsif ct=LABEL then
--                  w += 12 -- (in case italic)
                end if
            elsif ct=DROPDOWN then
--/*
                sequence options = ctrl_xtra[id]
                {w,h} = gGetTextExtent(id,options,false)
--              w += 18
--              w += 23
--              w += 24
                w += 25
--              w += 28
                h += 7
--*/
                {w,h} = sq_add(gGetTextExtent(id,ctrl_xtra[id],false),{25,7})
            elsif ct=TEXT then
--DEV CUEBANNER?
                {w,h} = sq_add(gGetTextExtent(id,"W"),{140,5})
--/*
                {w,h} = gGetTextExtent(id,"W")
                w += 140
                h += 10
--*/
            else
                ?9/0
            end if
--?{"winnatsize",w,h,title}
        else
            ?9/0 -- (unknown backend)
        end if
        ctrl_size[id][SZ_NATURAL_W] = w
        ctrl_size[id][SZ_NATURAL_H] = h
--DEV to go:
--  else
--      integer gnat = ctrl_msg[ct][CM_GNT]
--      if gnat then
--if not find(ct,{CHECKBOX,DROPDOWN,LABEL,TEXT}) then crash(ctrl_names[ct]) end if
--          gnat(id)
--      end if
    elsif not find(ct,{CANVAS}) then
        printf(1,"xpg_lm_set_element_sizes(%s)?\n",{ctrl_names[ct]})
    end if
--if ct=CANVAS then ?{"<ans(CANVAS)",ctrl_size[id][SZ_NATURAL_W],ctrl_size[id][SZ_NATURAL_H]} end if
--  integer uw = ctrl_size[id][SZ_USER_W],
--          uh = ctrl_size[id][SZ_USER_H]
--  nw = max(nw,uw)
--  nh = max(nh,uh)
----DEV temp (until xpg_lm_distribute_any_slack() is working):
----    ctrl_size[id][SZ_W] = w
----    ctrl_size[id][SZ_H] = h
end procedure

local procedure check_unmapped(gdx ids, integer flag, rdx=0)
    -- used by gRadio() and gNormalise()
    for id in ids do
        integer cfi = ctrl_flags[id],
                cti = ctrl_types[id]
        bool bMapped = and_bits(cfi,CF_MAPPED)!=0
        assert(not bMapped)
        if flag=CF_RADIO then
            assert(cti=CHECKBOX)
            -- (>1 normaliser group ok, but radios 1 only)
            assert(and_bits(cfi,CF_RADIO)=0)
--DEV erm, >1 group??
            ctrl_xtra[id] = rdx
        elsif flag=CF_NORMAL then
            -- ensure there are no kids on normalised items
            -- (or maybe check that no parent of id is also
            --  a member of ids, would need order tweakage)
            assert(ctrl_kids[cti]=0)
        else
            ?9/0 -- unknown flag?
        end if
        cfi = or_bits(cfi,flag)
        ctrl_flags[id] = cfi
    end for
end procedure

global procedure gNormalise(gdx ids, string hvb="BOTH")
    integer ihvb = find(hvb,{"HORIZONTAL","VERTICAL","BOTH"})
    assert(ihvb!=0) -- nb: ihvb is now a bitfield, 0b01/0b10/0b11
    check_unmapped(ids,CF_NORMAL)
    norm_groups = append(norm_groups,{ihvb,ids})
end procedure

--DEV not tried... cannot think of any reason not to do this, heck, let's just slap it in.
global constant integer gNormalize = gNormalise;
--global procedure gNormalize(gdx ids, string hvb="BOTH")
--  gNormalise(ids,hvb)
--end procedure

--DEV...
local function xpg_lm_gather_normal_groups(sequence ngused, gdx id)
    integer cfi = ctrl_flags[id]
    assert(and_bits(cfi,CF_MAPPED)!=0)
    object children = children_ids[id]
    if sequence(children) then
        for i,child in children do
            ngused = xpg_lm_gather_normal_groups(ngused,child)
--          integer cw = ctrl_size[child][SZ_NATURAL_W],
--                  ch = ctrl_size[child][SZ_NATURAL_H]
        end for
    elsif and_bits(cfi,CF_NORMAL) then
        for i=1 to length(ngused) do
            if not ngused[i]
            and find(id,norm_groups[i][2]) then
                ngused[i] = true
            end if
        end for
    end if  
    -- copy now for all non-normalised elements:
if ctrl_kids[ctrl_types[id]]=0 then
--  integer cw = ctrl_size[id][SZ_NATURAL_W],
--          ch = ctrl_size[id][SZ_NATURAL_H]
    integer cw = max(ctrl_size[id][SZ_NATURAL_W],ctrl_size[id][SZ_USER_W]),
            ch = max(ctrl_size[id][SZ_NATURAL_H],ctrl_size[id][SZ_USER_H])
--?{"gng",id,cw,ch}
    ctrl_size[id][SZ_NORMAL_W] = cw
    ctrl_size[id][SZ_NORMAL_H] = ch
end if
    return ngused
end function

local procedure xpg_lm_normalise_sizes(gdx id)
--DEV can we reorder this such that when we normalise any containers, all child elements have already been done??
--perhaps we start with crtl_done = repeat(false,length(ctrl_handles)) or
--                      crtl_done = apply(tagset(length(ctrl_handles)),is_leaf) or
--                      crtl_done = sq_and_bits(ctrl_flags,CF_LEAF) or
--  sequence crtl_done = extract(ctrl_kids,ctrl_types)
--  ^^ gone with "no containers" instead, at least for now...
--  ?{"xpg_lm_normalise_sizes",id}
--SUG:
    integer l = length(norm_groups)
    sequence ngused = repeat(false,l),
           max_natw = repeat(0,l),
           max_nath = repeat(0,l),
         child_sets = {},
       child_groups = {}
    --
    -- First, traverse the dialog hierarcy collecting used normaliser groups,
    -- and while you're doing that set sensible defaults for non-"" elements:
    --
    ngused = xpg_lm_gather_normal_groups(ngused,id)
    --
    -- Second, collect the max sizes for all used groups, and 
    --  generate all the lists of groups used by each child
    --
    for ngdx=1 to l do
        if ngused[ngdx] then
            integer mw = 0, mh = 0
            for c in norm_groups[ngdx][2] do
                mw = max(mw,ctrl_size[c][SZ_NATURAL_W])
                mh = max(mh,ctrl_size[c][SZ_NATURAL_H])
                integer k = find(c,child_sets)
                if k then
                    child_groups[k] &= ngdx
                else
                    child_sets &= c
                    child_groups &= {{ngdx}}
                end if
            end for
            max_natw[ngdx] = mw
            max_nath[ngdx] = mh
        end if
    end for
    --
    -- Third/lastly, copy max(group_max) onto each element
    --
    for i,c in child_sets do
        integer cw = ctrl_size[c][SZ_NORMAL_W],
                ch = ctrl_size[c][SZ_NORMAL_H]
--      integer cw = ctrl_size[c][SZ_NATURAL_W],
--              ch = ctrl_size[c][SZ_NATURAL_H]
        for ngdx in child_groups[i] do
            {integer ngf, sequence ngc} = norm_groups[ngdx]
            if and_bits(ngf,0b01) then -- HORIZONTAL
                cw = max(cw,max_natw[ngdx])
            end if
            if and_bits(ngf,0b10) then -- VERTICAL
                ch = max(ch,max_nath[ngdx])
            end if
        end for
        ctrl_size[c][SZ_NORMAL_W] = cw
        ctrl_size[c][SZ_NORMAL_H] = ch
    end for
--DOCS:
--Normalisation applies to the natural sizes only, and ignores any user sizes.
--Note that gNormalise() is only formally supported on non-container elements.
--Should you try normalising, say, gHbox that further contain normalised labels,
--it is formally undefined as to which normalisation occurs first. Likewise if
--you tried to normalise a gHbox with an element it contains, it would be quite
--right for it to go absolutely bonkers. All that said, however, there is no 
--deliberate bar, and if it works, it works, but if not you&rsquo;ll have to 
--set up a resize handler and set the user sizes explicitly yourself, ditto 
--should you seek normalisation that takes your user sizes into account.
--Lastly note that you cannot later change your mind about normalisation: once
--set the groups persist until program termination, and should not be recreated.

--....  
------ should, I think, do natural+user(percolating up) -> normal...
----        integer dw = max(ctrl_size[id][SZ_USER_W],ctrl_size[id][SZ_NATURAL_W]),
----                dh = max(ctrl_size[id][SZ_USER_H],ctrl_size[id][SZ_NATURAL_H])
----
end procedure


local procedure xpg_lm_accumulate_sizes(gdx id)
    integer ct = ctrl_types[id]
--if ct=CANVAS then ?{"ans(CANVAS)",ctrl_size[id][SZ_NATURAL_W],ctrl_size[id][SZ_NATURAL_H]} end if
    object children = children_ids[id]
    if sequence(children) then
--      integer {mt,mr,mb,ml} = xpg_margins(ct),
--              nw = ml, nh = mt, 
--      integer {{nh,mr,mb,nw},gap} = xpg_margap(id),
--      integer nh = xpg_get_mp(id,MP_PADDING,MP_TOP),
--              nw = xpg_get_mp(id,MP_PADDING,MP_LFT),
--      integer nh = xpg_get_mp(id,SZ_MARGIN,MP_TOP),
--              nw = xpg_get_mp(id,SZ_MARGIN,MP_LFT),
        integer nw = 0, nh = 0, gap = 0,
                gap2 = iff(ct=BOX?ctrl_xtra[id][CX_BOX_GAP]:0)
--              gap = 0
--?{"lmas1",id,nw,nh}
--              cl = length(children)
--if ct=BOX then ?{id,xpg_margap(id)} end if
        bool bVert = and_bits(ctrl_flags[id],CF_VERTICAL)!=0
        for i,child in children do
            xpg_lm_accumulate_sizes(child)
            integer cw = ctrl_size[child][SZ_NORMAL_W],
                    ch = ctrl_size[child][SZ_NORMAL_H],
--          integer cw = max(ctrl_size[child][SZ_NATURAL_W],ctrl_size[child][SZ_USER_W]),
--                  ch = max(ctrl_size[child][SZ_NATURAL_H],ctrl_size[child][SZ_USER_H]),
                    mt = xpg_get_mp(child,SZ_MARGIN,MP_TOP),
                    ml = xpg_get_mp(child,SZ_MARGIN,MP_LFT),
                    mb = xpg_get_mp(child,SZ_MARGIN,MP_BTM),
                    mr = xpg_get_mp(child,SZ_MARGIN,MP_RGT)
--?{"lmas",child,cw,ch}
--          ctrl_size[child][SZ_W] = cw
--          ctrl_size[child][SZ_H] = ch
            cw = ml+cw+mr
            ch = mt+ch+mb
--ctrl_mp
--          integer cw = ctrl_size[child][SZ_W],
--                  ch = ctrl_size[child][SZ_H]
            -- aside: for a single child this is of course
            --        equivalent to nw:=cw and nh:=ch.
            nw = iff(bVert?max(nw,cw):nw+gap+cw)
            nh = iff(bVert?nh+gap+ch:max(nh,ch))
            if i=1 then gap = gap2 end if

--          if bVert then cy += ch else cx += cw end if
--DEV
--          gap = ???
--          if i=1 and ct=BOX then
--              gap = ctrl_xtra[id][CX_BOX_GAP]
--          end if
        end for
--DEV...
--      if ct=BOX then
--      nw += xpg_get_mp(id,MP_PADDING,MP_RGT)
--      nh += xpg_get_mp(id,MP_PADDING,MP_BTM)
--?{"lmas2",id,nw,nh}
--      nw += xpg_get_mp(id,SZ_MARGIN,MP_LFT)
--          + xpg_get_mp(id,SZ_MARGIN,MP_RGT)
--      nh += xpg_get_mp(id,SZ_MARGIN,MP_TOP)
--          + xpg_get_mp(id,SZ_MARGIN,MP_BTM)
--?{"lmas3",id,nw,nh}
--      end if
--      ctrl_size[id][SZ_W] = nw
--      ctrl_size[id][SZ_H] = nh
        if ct=FRAME then
--DEV default margins of {2,2,2,2}... (should now be done)
--          nw += 4
--          nh += 4
            if backend=GTK then
                if bGTK3 then
                    nw += 2
                    nh += 2
                else
                    nw += 4
                    nh += 4
                end if
            elsif backend=WinAPI then
                nw += 2
                nh += 1
            else
                ?9/0
            end if
            string title = gGetAttribute(id,"TITLE","")
--          if title="" then title="X" end if
--?{"frame title",title}
            if length(title) then
                integer {tw,th} = gGetTextExtent(id,title)
                nh += th -- (height)
                --DEV/SUG minwidth from tw??
            elsif backend=WinAPI then
                nh += 9
            end if
        elsif ct=DIALOG then
--local function xpg_get_window_rect(gdx id)
--local function xpg_get_client_rect(gdx id)
--local function xpg_gtk_subtract_dialog_decorations(gdx id, integer dw, dh)
            integer {dw,dh} = xpg_lm_get_dialog_decoration_size(id)
            nw += dw
            nh += dh
        end if
--      if backend=GTK
--      and (ct=FRAME or ct=BOX or ct=DIALOG) then -- [or, probably, DATEPICK, TABS]
--      if ct=FRAME or ct=BOX or ct=DIALOG then -- [or, probably, DATEPICK/GTK, TABS]
--          ctrl_size[id][SZ_NATURAL_W] = nw
--          ctrl_size[id][SZ_NATURAL_H] = nh
--      end if
--if ct=DIALOG then ?{"lmas",id,nw,nh} end if
?{"lmas",id,nw,nh}
        ctrl_size[id][SZ_NORMAL_W] = nw
        ctrl_size[id][SZ_NORMAL_H] = nh
--/*
    elsif find(ct,{BUTTON,CHECKBOX,DROPDOWN,LABEL,TEXT}) then
--     or (ct=CANVAS and xpg_sizeable_custom_canvas_control(id)) then
--     or (ct=CANVAS and find(ctrl_xtra[id][CX_??],{BUTTON,CHECKBOX,DROPDOWN,LABEL,TEXT})) then
        integer w, h
        if backend=GTK then
--      if backend=GTK and ct!=CANVAS then
            atom handle = ctrl_handles[id]
            c_proc(gtk_widget_size_request,{handle,pGtkRequisition})
            w = get_struct_field(idGtkRequisition,pGtkRequisition,"width")
            h = get_struct_field(idGtkRequisition,pGtkRequisition,"height")
--?{"gtknatsize",w,h,title}
        elsif backend=WinAPI then
            if ct=BUTTON
            or ct=CHECKBOX
            or ct=LABEL then
                string title = gGetAttribute(id,"TITLE")
                {w,h} = gGetTextExtent(id,split(xpg_demnemonicalize(title),'\n'))
--DEV default padding???
                if ct=BUTTON then
                    w += 21
                    h += 11
                elsif ct=CHECKBOX then
                    w += 26
                    h += 14
--no help...
--              elsif ct=LABEL then
--                  w += 12 -- (in case italic)
                end if
            elsif ct=DROPDOWN then
--/*
                sequence options = ctrl_xtra[id]
                {w,h} = gGetTextExtent(id,options,false)
--              w += 18
--              w += 23
--              w += 24
                w += 25
--              w += 28
                h += 7
--*/
                {w,h} = sq_add(gGetTextExtent(id,ctrl_xtra[id],false),{25,7})
            elsif ct=TEXT then
                {w,h} = sq_add(gGetTextExtent(id,"W"),{140,10})
--/*
                {w,h} = gGetTextExtent(id,"W")
                w += 140
                h += 10
--*/
            else
                ?9/0
            end if
--?{"winnatsize",w,h,title}
        else
            ?9/0 -- (unknown backend)
        end if
        ctrl_size[id][SZ_NATURAL_W] = w
        ctrl_size[id][SZ_NATURAL_H] = h
--*/
--DEV to go:
--  else
--      integer gnat = ctrl_msg[ct][CM_GNT]
--      if gnat then
--if not find(ct,{CHECKBOX,DROPDOWN,LABEL,TEXT}) then crash(ctrl_names[ct]) end if
--          gnat(id)
--      end if
--  elsif not find(ct,{CANVAS}) then
--      printf(1,"xpg_lm_accumulate_sizes(%s)?\n",{ctrl_names[ct]})
    end if
--if ct=CANVAS then ?{"<ans(CANVAS)",ctrl_size[id][SZ_NATURAL_W],ctrl_size[id][SZ_NATURAL_H]} end if
--  integer uw = ctrl_size[id][SZ_USER_W],
--          uh = ctrl_size[id][SZ_USER_H]
--  nw = max(nw,uw)
--  nh = max(nh,uh)
----DEV temp (until xpg_lm_distribute_any_slack() is working):
----    ctrl_size[id][SZ_W] = w
----    ctrl_size[id][SZ_H] = h
end procedure

-- xpg_apply_spacing?
--local procedure xpg_lm_disperse_user_sizes(gdx id, integer pw=0, ph=0)
local procedure xpg_lm_disperse_user_sizes(gdx id, integer cw, ch)
    -- Since GTK insists on being such an absolute dick about such matters,
    -- cw,ch are/must be the **client** size, not the window size.

--  integer w = ctrl_size[id][SZ_NORMAL_W],
--          h = ctrl_size[id][SZ_NORMAL_H]
    integer w = max(ctrl_size[id][SZ_NORMAL_W],ctrl_size[id][SZ_USER_W]),
            h = max(ctrl_size[id][SZ_NORMAL_H],ctrl_size[id][SZ_USER_H])
--integer ct = ctrl_types[id]
--if ct=DIALOG and backend=WinAPI then cw += 14; ch += 7; end if
--/*
BOOL CScreenImage::CaptureWindow(HWND hWnd) {
    CImage image;
    WINDOWINFO info;
    info.cbSize = sizeof(WINDOWINFO);
    BOOL bRet = FALSE;
    if (::IsWindow(hWnd)) {
        CRect rect;
        ::GetWindowRect(hWnd, rect);
        ::GetWindowInfo(hWnd, &info);
        if (IsZoomed(hWnd)) {
            OutputDebugString("Is Maximized\n");
            rect.top += info.cyWindowBorders;
            rect.left += info.cxWindowBorders;
            rect.bottom -= info.cyWindowBorders;
            rect.right -= info.cxWindowBorders;
        } else if (!(info.dwStyle & WS_THICKFRAME)) {
            OutputDebugString("Is a popup window\n");
            rect.top += info.cyWindowBorders / 4;
            rect.left += info.cxWindowBorders / 4;
            rect.bottom -= info.cyWindowBorders / 4;
            rect.right -= info.cxWindowBorders / 4;
        } else if (info.dwStyle & WS_OVERLAPPEDWINDOW) {
            OutputDebugString("Is a Thickframe window\n");
            rect.left += info.cxWindowBorders;
            rect.bottom -= info.cyWindowBorders;
            rect.right -= info.cxWindowBorders;
        }
        bRet = CaptureRect(rect);
    }
    return bRet;
}
--or:
   SetWindowPos(hWnd,   HWND_TOP,  100,  100,  300,  70,  SWP_SHOWWINDOW);
   RECT rcClient, rcWind;
   GetClientRect(hWnd, &rcClient);
   GetWindowRect(hWnd, &rcWind);
   int xdiff =  (rcWind.right-rcWind.left)-(rcClient.right-rcClient.left);//16
   int ydiff =  (rcWind.bottom-rcWind.top)-(rcClient.bottom-rcClient.top);//39
   int cxSizeFrame = GetSystemMetrics(SM_CXSIZEFRAME);
   int cySizeFrame = GetSystemMetrics(SM_CXSIZEFRAME);
   int cyCaption = GetSystemMetrics(SM_CYCAPTION);
   int cyBorder = GetSystemMetrics(SM_CYBORDER);.
--*/
?{"xpg_lm_disperse_user_sizes",id,cw,ch,{w,h}}
    object children = children_ids[id]
    if sequence(children) then
--  if sequence(children) and (w or h) then
        bool bVert = and_bits(ctrl_flags[id],CF_VERTICAL)!=0,
             bBoth = ctrl_types[id]!=BOX
--DEV deep bug here...
--?{"bBoth",bBoth,"bVert",bVert}
        integer flag = iff(bVert?CF_EXPANDV:CF_EXPANDH)
--      sequence ec = {} -- expandable children
        sequence bexpand = repeat(0,length(children))
        atom aw = 0, ah = 0,    -- all width/height
             ew = 0, eh = 0     -- expandable ""
        integer n = 0
--/*
        integer ct = ctrl_types[id],
            {mt,mr,mb,ml} = iff(ct=BOX?ctrl_xtra[id][CX_BOX_MARGIN]:{0,0,0,0})
                nw = ml, nh = mt, gap = 0,
        end for
        nw += mr
        nh += mb
--*/

--      sequence expandable = {}
--      for child in children do
--          if and_bits(ctrl_flags[child],flag)!=0 then
----                expandable &= child
--              n += 1
--          end if
--      end for
        for i,child in children do
            integer cflags = ctrl_flags[child],
                    ccw = ctrl_size[child][SZ_NORMAL_W],
                    cch = ctrl_size[child][SZ_NORMAL_H]
            aw += ccw
            ah += cch
--          if and_bits(cflags,flag)!=0 then
            if bBoth or and_bits(cflags,flag)!=0 then
--          if and_bits(ctrl_flags[child],flag)!=0 then
--              ec &= child
                bexpand[i] = true
                n += 1
                ew += ccw
                eh += cch
--              ew += ctrl_size[child][SZ_NORMAL_W]
--              eh += ctrl_size[child][SZ_NORMAL_H]
            end if
        end for
--?{"dus.ec:",ec}
--      integer n = length(ec),
--              wslack = cw-aw,
--              hslack = ch-ah
        integer wslack = max(0,cw-aw),
                hslack = max(0,ch-ah)
--      integer wslack = max(0,w-aw),
--              hslack = max(0,h-ah)
--      if n=0 then
--          ec = children
--          n = length(ec)
--          ew = aw
--          eh = ah
--      end if
--DEV should this not do all children, checking if bexpand[i] is true????
--      for child in ec do
        for i,child in children do
            integer ecw = ctrl_size[child][SZ_NORMAL_W],
                    ech = ctrl_size[child][SZ_NORMAL_H]
            integer xw = 0, xh = 0
            if bexpand[i] then
                if bBoth or bVert then
                    xw = floor(wslack/n)
                    wslack -= xw
                end if
                if bBoth or not bVert then
                    xh = floor(hslack/n)
                    hslack -= xh
                end if
--?{"xwxh",xw,xh}
--          ecw = ???
                n -= 1
            end if
            xpg_lm_disperse_user_sizes(child,ecw+xw,ech+xh)
        end for
----        integer n = length(expendable)
--      while n do
----        if length(expandable) then
--          n -= 1
--      end while
    end if
--DEV??
--if parent_ids[id] then
--totally messes up resize in gCanvas.exw under GTK:
--  ctrl_size[id][SZ_W] = cw
--  ctrl_size[id][SZ_H] = ch
    ctrl_size[id][SZ_W] = w
    ctrl_size[id][SZ_H] = h
--end if
end procedure

local procedure xpg_lm_calculate_offsets(gdx id, integer x=0, y=0)
    bool bMapped = and_bits(ctrl_flags[id],CF_MAPPED)!=0
    if not bMapped then return end if
--?{"xpg_lm_calculate_offsets",id,x,y}
    ctrl_size[id][SZ_X] = x
    ctrl_size[id][SZ_Y] = y
--  integer w = ctrl_size[id][SZ_W],
--          h = ctrl_size[id][SZ_H]
    object children = children_ids[id]
    if sequence(children) then
--untried:
--  if sequence(children) and length(children) then
        integer ct = ctrl_types[id],
--              pt = xpg_get_mp(id,MP_PADDING,MP_TOP),
--              pl = xpg_get_mp(id,MP_PADDING,MP_LFT)
                pt = xpg_get_mp(id,SZ_MARGIN,MP_TOP),
                pl = xpg_get_mp(id,SZ_MARGIN,MP_LFT)
--DEV/SUG: (need to get expand working first...)
--         spacing = ctrl_xtra[id][CX_BOX_SPACE],
--              sl = and_bits(spacing,0b100)!=0,
--              sc = and_bits(spacing,0b010)!=0,
--              sr = and_bits(spacing,0b001)!=0
--?{"lmco",id,pt,pl}
        if ctrl_handles[id]=NULL then -- WinAPI virtual h/vbox
            x += pl
            y += pt
        else
            x = pl
            y = pt
        end if
        if backend=WinAPI then
            if ct=FRAME then
--              x += 1
----            y += ctrl_size[id][SZ_H]
--              y += ctrl_size[id][SZ_NATURAL_H]
                x += 1
--              y += 2
                string title = gGetAttribute(id,"TITLE","")
                if length(title) then
                    integer {tw,th} = gGetTextExtent(id,title)
                    y += th-1
                else
                    y += 8
                end if
--/* (maybe...)
            elsif ct=TABS then
                x += 2
                y += 2+gGetTextExtent(id,"X")[2]
--*/
            end if
        end if
        bool bVert = and_bits(ctrl_flags[id],CF_VERTICAL)!=0
--/*
        integer ct = ctrl_types[id],
            {mt,mr,mb,ml} = iff(ct=BOX?ctrl_xtra[id][CX_BOX_MARGIN]:{0,0,0,0})
                nw = ml, nh = mt, gap = 0,
        end for
        nw += mr
        nh += mb
--*/
--      integer cx = 0, cy = 0
        integer gap = iff(ct=BOX?gGetAttribute(id,"GAP",0):0)
--              slack = ??
        for child in children do
            xpg_lm_calculate_offsets(child,x,y)
            if bVert then
                y += ctrl_size[child][SZ_H]+gap
--              y += ctrl_size[child][SZ_H]
--              y += ctrl_size[child][SZ_NATURAL_H]
--              y += ctrl_size[child][SZ_NORMAL_H]+gap
            else
                x += ctrl_size[child][SZ_W]+gap
--              x += ctrl_size[child][SZ_W]
--              x += ctrl_size[child][SZ_NATURAL_W]
--              x += ctrl_size[child][SZ_NORMAL_W]+gap
            end if
        end for
--      if n then       
--      end if
    end if
end procedure

forward global procedure gRedraw(gdx id, integer flags=0b111)

sequence bodge = {}

-- debug aid, otherwise undocumented: 
global procedure gSetBodge(integer id, sequence xywh, xywhg={}, xywhg3={}, bool bCrash=false)
-- can be invoked as gSetBodge(id,{0,0,0,0}) to get a rough starting point
-- bCrash can be set true so that after getting something to work, a quick
-- run of demo/xpGUI/gButton.exw..gTreeview.exw does not need careful/slow 
-- visual checking that it isn't being re-triggered, but otherwise is more
-- of a hindrance than help when something actually goes wrong, probably.
-- Note that bCrash=true is expected to trigger crashes on any resize...
    if id>length(bodge) then bodge &= repeat(0,id-length(bodge)) end if
    if backend=GTK then
        -- The g and g3 args can be used when things differ under GTK[3].
        if length(xywhg) then xywh = xywhg end if
        if bGTK3 and length(xywhg3) then xywh = xywhg3 end if
    end if
    bodge[id] = {xywh,bCrash}   
end procedure

local procedure xpg_lm_apply_offsets(gdx id, parent=0)
--?{"xpg_lm_apply_offsets",id}
--  sequence sizes = ctrl_size[id] -- (maybe)
    integer p = parent_ids[id],
            x = ctrl_size[id][SZ_X],
            y = ctrl_size[id][SZ_Y],
--DEV these please:
            w = ctrl_size[id][SZ_W],
            h = ctrl_size[id][SZ_H]

--          w = ctrl_size[id][SZ_NORMAL_W],
--          h = ctrl_size[id][SZ_NORMAL_H]
--          w = max(ctrl_size[id][SZ_NORMAL_W],ctrl_size[id][SZ_USER_W]),
--          h = max(ctrl_size[id][SZ_NORMAL_H],ctrl_size[id][SZ_USER_H])

--          w = max(ctrl_size[id][SZ_NATURAL_W],ctrl_size[id][SZ_USER_W]),
--          h = max(ctrl_size[id][SZ_NATURAL_H],ctrl_size[id][SZ_USER_H])
----        integer dw = max(ctrl_size[id][SZ_USER_W],ctrl_size[id][SZ_NATURAL_W]),
----                dh = max(ctrl_size[id][SZ_USER_H],ctrl_size[id][SZ_NATURAL_H])
    object children = children_ids[id]
    if sequence(children) then
--/*
        integer ct = ctrl_types[id],
            {mt,mr,mb,ml} = iff(ct=BOX?ctrl_xtra[id][CX_BOX_MARGIN]:{0,0,0,0})
                nw = ml, nh = mt, gap = 0,
        end for
        nw += mr
        nh += mb
--*/
        for child in children do
            xpg_lm_apply_offsets(child,id)
        end for
    end if
--  if parent!=0 then
--  if parent!=0 or backend=WinAPI then
        xpg_handle handle = ctrl_handles[id]
        integer pt = iff(parent?ctrl_types[parent]:0),
                ct = ctrl_types[id]
--      if p and ctrl_kids[ct]=0 then
--          x -= ctrl_size[p][SZ_X]
--          y -= ctrl_size[p][SZ_Y]
--      end if
if id<=length(bodge) then
--if id<=length(bodge) and backend!=GTK then
    object bi = bodge[id], xywh = {x,y,w,h}
    if sequence(bi) then
        bool bCrash = bi[2]
        bi = bi[1]
        if bi!=xywh then
            string t = gGetAttribute(id,"TITLE","")
            if bCrash then
                crash("bodge[%d(%s)]: %v should be %v\n",{id,t,xywh,bi})
            end if
            printf(1,"bodge[%d(%s)]: %v should be %v\n",{id,t,xywh,bi})
            {x,y,w,h} = bi
        end if
    end if
end if
        if backend=GTK then
            if pt=BOX then
                c_proc(gtk_fixed_move,{ctrl_handles[parent],handle,x,y})
--          else
--?{"gtk_window_move(o)",id}
--              c_proc(gtk_window_move,{handle,x,y})
--?{"<gtk_window_move(o)",id}
            end if
--          if w>0 and h>0 then
--          if w>0 and h>0 and pt!=DIALOG then
--          if false then --DEV gTable: right width, wrong height...
--              gtk_widget_set_size_request(widget,width,height);
--?{"xpg_lm_apply_offsets",id,iff(pt=BOX?{x,y}:"n/a"),w,h}
if ct=DIALOG then
    w -= 2; h -= 32;
end if
                c_proc(gtk_widget_set_size_request,{handle,w,h}) 
--          end if
        elsif backend=WinAPI then
--          if handle then
--          if pt and handle and w and h then
            if handle and w and h then
--              integer flags = SWP_NOZORDER
                integer flags = iff(pt?SWP_NOZORDER:SWP_NOMOVE+SWP_NOZORDER)
--              bool ok
--10/5/23:
--              if parent then
--                  integer flags = SWP_NOZORDER+SWP_NOSIZE
--                  integer flags = SWP_NOZORDER
--                  ok = c_func(xSetWindowPos,{handle,NULL,x,y,0,0,flags})
--if ct=DIALOG then w += 14; h += 7; end if
--if ct=DIALOG then w += 2; h += 32; end if
if ct=DIALOG then
    if and_bits(ctrl_flags[id],CF_RESIZE) then
        w += 2+14; h += 32+7;
    else
--DEV wrong!
        w += 6; h += 24;
    end if
end if
?{"xpg_lm_apply_offsets",id,x,y,w,h,pt}
                    bool ok = c_func(xSetWindowPos,{handle,NULL,x,y,w,h,flags})
                    assert(ok)
--/*
                else
                    integer r = c_func(xGetClientRect,{handle,pRECT})
                    assert(r!=0)
--                  sequence crect = peek4u({pRECT,4})
                    integer cw = get_struct_field(idRECT,pRECT,"right"),
                            ch = get_struct_field(idRECT,pRECT,"bottom")
                    r = c_func(xGetWindowRect,{handle,pRECT})
                    assert(r!=0)
--DEV suspect... (not really, but let's just code this way throughout...)
--                  sequence wrect = peek4s({pRECT,4})
                    integer ww = get_struct_field(idRECT,pRECT,"right")
                               - get_struct_field(idRECT,pRECT,"left"),
                            wh = get_struct_field(idRECT,pRECT,"bottom")
                               - get_struct_field(idRECT,pRECT,"top")

--?{"xpg_lm_apply_offsets","crect",crect,cw,ch,"wrect",wrect,ww,wh}
                    w += ww-cw
                    h += wh-ch-1
                    flags = SWP_NOMOVE+SWP_NOZORDER+SWP_NOACTIVATE
                    ok = c_func(xSetWindowPos,{handle,NULL,0,0,w,h,flags})
                    assert(ok)
                end if
--*/
--else
--  ?{"nope",pt,handle,w,h}
            end if
        else
            ?9/0 -- (unknown backend)
        end if
--  end if
--I might yet rinstate this...
--  if parent=0 then
--      gRedraw(id)
--  end if
end procedure

local procedure xpg_defer_attr(gdx id, string name, object v)
--DEV scan for name?
    deferred_attr[id] &= {{name,v}}
--  sequence daid = deferred_attr[id]
--  sequence daid = deep_copy(deferred_attr[id])
--  deferred_attr[id] = 0
--  daid &= {{name,v}}
--  deferred_attr[id] = daid
end procedure

local atom hwndTip = NULL

local procedure xpg_set_tip(integer id, string v)
    atom handle = ctrl_handles[id]
    assert(handle!=NULL)
    if backend=GTK then
        c_proc(gtk_widget_set_tooltip_text,{handle,v})
    elsif backend=WinAPI then
        if hwndTip=NULL then
            atom pHwnd = ctrl_handles[gGetDialog(id)],
                dwStyle = or_all({WS_POPUP,TTS_ALWAYSTIP,TTS_BALLOON})
            assert(pHwnd!=NULL)
            sequence cw_params = {NULL,                 -- extended style
                                  "tooltips_class32",   -- window class name
                                  NULL,                 -- window caption or Button text etc..
                                  dwStyle,          -- window style
                                  CW_USEDEFAULT,    -- initial x position
                                  CW_USEDEFAULT,    -- initial y position
                                  CW_USEDEFAULT,    -- initial x size
                                  CW_USEDEFAULT,    -- initial y size
                                  pHwnd,            -- parent window handle
                                  NULL,             -- window menu handle OR user id for child windows
                                  NULL,             -- program instance handle - Legacy of Win16 apps. 0 will work too.
                                  NULL}             -- creation parameters
            hwndTip = c_func(xCreateWindowEx,cw_params)
            assert(hwndTip!=NULL)
--          crash("CreateWindowEx error #%08x",{c_func(xGetLastError,{})})
            integer flags = SWP_NOMOVE+SWP_NOSIZE+SWP_NOACTIVATE
            bool ok = c_func(xSetWindowPos,{hwndTip,HWND_TOPMOST,0,0,0,0,flags})
            assert(ok)
            set_struct_field(idTOOLINFO,pTOOLINFO,"cbSize",get_struct_size(idTOOLINFO))
            set_struct_field(idTOOLINFO,pTOOLINFO,"uFlags",TTF_IDISHWND+TTF_SUBCLASS)
            set_struct_field(idTOOLINFO,pTOOLINFO,"hwnd",hwndTip)
        end if
        set_struct_field(idTOOLINFO,pTOOLINFO,"lpszText",xpg_raw_string_ptr(v))
        set_struct_field(idTOOLINFO,pTOOLINFO,"uId",handle)
        {} = c_func(xSendMessage,{hwndTip,TTM_ADDTOOLA,0,pTOOLINFO})
    else
        ?9/0 -- (unknown backend)
    end if
end procedure

--with trace
global procedure gSetAttribute(gdx id, string name, object v, sequence args={})
    if length(args) then v = sprintf(v,args) end if
    if sequence(id) then
        for i in id do
            assert(integer(i))
            gSetAttribute(i,name,v)
        end for
        return
    end if
--if name="XMAX" then trace(1) end if
    -- nb NULL id is *not* permitted/supported (as per docs)
    integer ct = ctrl_types[id],
       setattr = ctrl_msg[ct][CM_SET]
    atom handle = ctrl_handles[id]
    bool bMapped = and_bits(ctrl_flags[id],CF_MAPPED)!=0
    if name="ACTIVE" then
        if string(v) then v = xpg_to_bool(v) end if
        xpg_set_ctrl_flag(id,CF_INACTIVE,not v)
        if not bMapped then
            xpg_defer_attr(id,name,v)
        else
            if backend=GTK then
                c_proc(gtk_widget_set_sensitive,{handle,v})
            elsif backend=WinAPI then
-- (or make it an error to enable/disable v/hbox...)
                if handle=NULL then
                    gSetAttribute(children_ids[id],name,v)
                else
                    c_proc(xEnableWindow,{handle,v})
                end if
            else
                ?9/0 -- (unknown backend)
            end if
        end if
    elsif name="CANFOCUS" then 
        assert(not find(ct,{DIALOG,BOX,CLIPBOARD,FRAME,TIMER}))
        if string(v) then v = xpg_to_bool(v) end if
        if not bMapped then
            xpg_defer_attr(id,name,v)
        else
            if backend=GTK then
                c_proc(gtk_widget_set_can_focus,{handle,v})
            elsif backend=WinAPI then
                atom dwStyle = c_func(xGetWindowLong,{handle,GWL_STYLE})
                dwStyle -= and_bits(dwStyle,WS_TABSTOP)
                if v then dwStyle += WS_TABSTOP end if
                dwStyle = c_func(xSetWindowLong,{handle,GWL_STYLE,dwStyle})
            else
                ?9/0 -- (unknown backend)
            end if
        end if
    elsif name="EXPAND" then
        assert(string(v))
--      if not bMapped then
--          xpg_defer_attr(id,name,v)
--      else
--DEV fatal error disabling on a gH/Vbox which is a direct child of a gDialog/gFrame/gTabs. (as per docs) [DONE]
--DEV kill this nonsense: [DONE]
--          bool bFree = false
--          bFree = length(v)>4 and v[-4..-1]="FREE"
--          if bFree then v = v[1..-5] end if
            if v="FALSE" or v="NO" then v="NONE" end if
            if v="TRUE" or v="YES" then v="BOTH" end if
            integer k = find(v,{"NONE","HORIZONTAL","VERTICAL","BOTH"})
--Value: "YES" (both directions), "HORIZONTAL", "VERTICAL", "HORIZONTALFREE", "VERTICALFREE" or "NO".
--Default: "NO". For containers the default is "YES".
            assert(k!=0)
            v = k-1
            if ct=BOX and find(ctrl_types[parent_ids[id]],{DIALOG,FRAME,TABS}) then
                assert(v=0b11,`cannot disable expansion on a "sole child" gH/Vbox`) -- (as per docs)
            end if
            integer f = ctrl_flags[id]
--          f -= and_bits(f,CF_EX_FREE)
--          f -= and_bits(f,CF_EX_HV)
            f -= and_bits(f,CF_EXPANDH+CF_EXPANDV)
--          f = or_bits(f,(k-1)*CF_EXPANDH+CF_FREE*bFree)
--          if bFree then f += CF_FREE end if
            if and_bits(v,0b01) then f += CF_EXPANDH end if
            if and_bits(v,0b10) then f += CF_EXPANDV end if
            ctrl_flags[id] = f
--      end if
    elsif name="FONT" then
--      "<face>, <styles> <size>". 
        assert(string(v))
        if not bMapped then
            xpg_defer_attr(id,name,v)
-- (or make it an error to set fonts on a v/hbox...)
        elsif handle=NULL then
            assert(backend=WinAPI)
            gSetAttribute(children_ids[id],name,v)
        else
            xpg_set_font(id,v)
--          printf(1,"gSetAttribute(%s,\"%s\",face:%s, styles:%v, size:%d)...\n",{ctrl_names[ct],name,face,styles,fontsize})
        end if
    elsif name="GAP" then
        if string(v) then v = to_number(v) end if
        assert(ct=BOX)
        ctrl_xtra[id][CX_BOX_GAP] = v
    elsif name="MARGIN"
       or name="PADDING" then
        if string(v) then v = xpg_intint(v) end if
--/*
        if atom(ctrl_mp[id]) then ctrl_mp[id] = {0,0,0} end if
        integer mpdx = find(name,{"MARGIN","PADDING"})
        if ct=DIALOG then
            mpdx = 1 -- assume ... or did I mean 2 here??? [DEV]
        end if
--      assert(mpdx=MP_MARGIN or mpdx=MP_PADDING) --- (dev search aid..)
        ctrl_mp[id][mpdx] = v
--*/
        assert(ct!=DIALOG,"no margin or padding on dialogs!")
        integer mpdx = iff(name="MARGIN"?SZ_MARGIN:SZ_PADDING)
        if mpdx!=SZ_MARGIN and not find(ct,{BUTTON,DATEPICK,DROPDOWN,TEXT}) then
            crash("gSetAttribute(%s,%s)",{ctrl_names[ct],name})
        end if
--      return 
        ctrl_size[id][mpdx] = v
--      return true
--  elsif name="PADDING" then
--      if not bMapped then
--          xpg_defer_attr(id,name,v)
--      else
--          if string(v) then v = xpg_intint(v) end if
--          printf(1,"gSetAttribute(%s,\"%s\",%v)...\n",{ctrl_names[ct],name,v})
--      end if
    elsif name="MINSIZE"
       or name="MAXSIZE" then
        if string(v) then v = xpg_intint(v) end if
        integer {w,h} = v,
--               mmdx = find(name,{"MINSIZE","MAXSIZE"}),
--          {mwx,mhx} = {{SZ_MIN_W,SZ_MIN_H},
--                       {SZ_MAX_W,SZ_MAX_H}}[mmdx]
            {mwx,mhx} = iff(name="MINSIZE"?{SZ_MIN_W,SZ_MIN_H}
                                          :{SZ_MAX_W,SZ_MAX_H})
        ctrl_size[id][mwx] = w
        ctrl_size[id][mhx] = h
--/* DEV/SUG:
        assert(length(v)=2)
        integer mmdx = iff(name="MINSIZE"?SZ_MIN:SZ_MAX)
        ctrl_size[id][mmdx] = v
--*/
    elsif name="SHRINK" then
        assert(string(v))
        if not bMapped then
            xpg_defer_attr(id,name,v)
        else
            printf(1,"gSetAttribute(%s,\"%s\",%s)...\n",{ctrl_names[ct],name,v})
            -- erm, a bit more like EXPAND, please, but both better:
            if string(v) then v = xpg_to_bool(v) end if
--DEV: CF_SHRINK is not yet actually used anywhere...
            xpg_set_ctrl_flag(id,CF_SHRINK,v)
        end if
    elsif name="SIZE" then
        if v=NULL or v="NULL" then v = {0,0} 
        elsif string(v) then v = xpg_intint(v) end if
        if not bMapped then
            xpg_defer_attr(id,name,v)
        else
--DEV NULL should be allowed too?? (above added, but as yet untested)
            integer {width,hight} = v
--DEVtemp:
--if backend=WinAPI then
--  if ct=DIALOG then width += 14; hight += 7; end if
--end if
--DEV otherwise unused as yet...
            ctrl_size[id][SZ_USER_W] = width
            ctrl_size[id][SZ_USER_H] = hight
--DEV: gRedraw(gGetDialog(id))
--          if width=0 then width = ctrl_size[id][SZ_NATURAL_W] end if
--          if hight=0 then hight = ctrl_size[id][SZ_NATURAL_H] end if
            width = max(width,ctrl_size[id][SZ_NATURAL_W])
            hight = max(hight,ctrl_size[id][SZ_NATURAL_H])
            if backend=GTK then
                if ct=DIALOG then
--                  {width,hight} = xpg_gtk_subtract_dialog_decorations(id,width,hight)
                    integer {dw,dh} = xpg_lm_get_dialog_decoration_size(id)
--?{width,hight,"SIZE"}
--                  c_proc(gtk_window_set_default_size,{handle,width,hight}) 
--DEV makes gTable.exw better...
                    c_proc(gtk_window_set_default_size,{handle,width-dw,hight-dh}) 
--                  c_proc(gtk_window_set_default_size,{handle,width,hight-dh}) 
                elsif ct=BUTTON then
                    c_proc(gtk_window_set_default_size,{handle,width,hight}) 
--                  c_proc(gtk_window_set_default_size,{handle,width-30,hight-10}) 
--                  c_proc(gtk_window_resize,{handle,width,hight}) 
--                  c_proc(gtk_window_resize,{handle,width-30,hight-10}) 
                elsif ct=DROPDOWN
                   or ct=FRAME then
--                 or ct=FRAME
--                 or ct=CANVAS then
                    c_proc(gtk_widget_set_size_request,{handle,width,hight}) 
--                  c_proc(gtk_widget_set_size_request,{handle,width-10,hight-30}) 
--                  c_proc(gtk_window_set_default_size,{handle,width,hight}) 
--?"w-10,h-30"
--                  c_proc(gtk_window_set_default_size,{handle,width-10,hight-30}) 
--                  c_proc(gtk_window_resize,{handle,width,hight}) 
--                  c_proc(gtk_window_resize,{handle,width-10,hight-30}) 
                else
                    -- (probably just need to figure out which of the above two works)
                    printf(1,"gSetAttribute[GTK](%s,\"SIZE\",%v)\n",{ctrl_names[ct],v})
--                  ?9/0 -- placeholder
                end if
            elsif backend=WinAPI then
--?{"WINAPISIZE",width,hight,id}
if handle then
                integer flags = SWP_NOMOVE+SWP_NOZORDER+SWP_NOACTIVATE
--DEV more closely matches gtk on a DIALOG... (I suspect GTK is wrong...)
--              bool ok = c_func(xSetWindowPos,{handle,NULL,0,0,width+22,hight+45,flags})
--if ct=DIALOG then width += 34; hight += 7; end if
--if ct=DIALOG then width += 14; hight += 7; end if
--if ct=DIALOG then width += 13; hight += 14; end if
--?{"gSetAttribute(SIZE):",id,width,hight}
                bool ok = c_func(xSetWindowPos,{handle,NULL,0,0,width,hight,flags})
                assert(ok)
end if
--if ct=DIALOG then ?xpg_get_window_rect(id) end if
            else
                ?9/0 -- (unknown backend)
            end if
        end if
    elsif name="TABTITLE" then
--      assert(string(v))
        if not bMapped then
            xpg_defer_attr(id,name,v)
        else
            integer pid = parent_ids[id],
                    posn = find(id,children_ids[pid])
            assert(ctrl_types[pid]=TABS)
            assert(posn!=0)
            sequence tabtitles = ctrl_xtra[pid][CX_TABTITLES]
            ctrl_xtra[pid][CX_TABTITLES] = 0
            while length(tabtitles)<posn do tabtitles &=0 end while
            tabtitles[posn] = v
            ctrl_xtra[pid][CX_TABTITLES] = tabtitles
            -- and actually set it, if post-xpg_add_tabs()
--?{"tabtitle",posn,v}
            atom notebook = ctrl_handles[pid], n
            if backend=GTK then
--DEV this "doubles up" the sample.exw tabs...
                n = c_func(gtk_notebook_get_n_pages,{notebook})
                if n>=posn then
?{"set TABTITLE",n,posn,v}
                    c_proc(gtk_notebook_set_tab_label_text,{notebook,handle,v})
                end if
            elsif backend=WinAPI then
                n = c_func(xSendMessage,{notebook,TCM_GETITEMCOUNT,0,0}) 
                if n>=posn then
                    set_struct_field(idTCITEM,pTCITEM,"mask",TCIF_TEXT)
--                  set_struct_field(idTCITEM,pTCITEM,"pszText",xpg_raw_string_ptr(v))
                    set_struct_field(idTCITEM,pTCITEM,"pszText",v)
                    {} = c_func(xSendMessage,{notebook,TCM_SETITEMA,posn-1,pTCITEM})
                end if
            else
                ?9/0 -- (unknown backend)
            end if
        end if
    elsif name="TIP" then
--      assert(string(v))
        if not bMapped then
            xpg_defer_attr(id,name,v)
        else
            xpg_set_tip(id,v)
        end if

--DEV/SUG: NORMALIZE_WIDTH|HEIGHT|BOTH > add to normalisers (and bMapped)
--dict normalizers: key id, data is {group_name[?&"W|H|B"?]}, key is group_name, data is {ids}.
-- (doh, investigate what IUP does...)
--  elsif begins("NORMALIZE",name) then
    elsif not setattr(id,name,v,bMapped) then
--  else
        -- placeholder (add more code to the [] rtn) / typo catcher:
        -- (commenting out the "with debug" on line ~67 would be the best
        --  bet for getting errors to be shown in the user's calling code)
        string san = get_routine_info(setattr,true)[4] -- (name/debug aid)
--if not setattr(id,name,v,bMapped) then
        if v=NULL then v="NULL" end if
        crash("gSetAttribute[%s](%s,%s,%v)",{san,ctrl_names[ct],name,v})
--end if
    end if
end procedure

--local bool bFromDeferred = false

local procedure xpg_apply_deferred_attributes(integer id)
--  bFromDeferred = true
    integer ct = ctrl_types[id]
    sequence inherit = iff(ct=MENU or ct=BOX?{}:{"FONT"})
    for nv in deferred_attr[id] do
        {string name, object v} = nv
        gSetAttribute(id,name,v)
        integer k = find(name,inherit)
        if k then inherit[k..k] = {} end if
    end for
    deferred_attr[id] = {}
    for name in inherit do
--?{name,"inherit",ct,ctrl_names[ct]}
        gSetAttribute(id,name,gGetAttribute(id,name))
    end for
--  bFromDeferred = true
end procedure 

global procedure gSetInt(gdx id, string name, integer i)
    gSetAttribute(id,name,i)
end procedure

global procedure gSetDouble(gdx id, string name, atom a)
    gSetAttribute(id,name,a)
end procedure

global procedure gToggleInt(gdx id, string name)
    bool bNot = not gGetAttribute(id,name,false)
    gSetAttribute(id,name,bNot)
end procedure

--/*
--I think this is the one:
struct {
  int count;
  double coordx[100];
  double coordy[100];
} glob;

static gboolean on_draw_event(GtkWidget *widget, cairo_t *cr, gpointer user_data) {
  cairo_set_source_rgb(cr,0,0,0);
  cairo_set_line_width(cr,0.5);

  int i, j;
  for (i = 0; i <= glob.count - 1; i++ ) {
      for (j = 0; j <= glob.count - 1; j++ ) {
          cairo_move_to(cr,glob.coordx[i],glob.coordy[i]);
          cairo_line_to(cr,glob.coordx[j],glob.coordy[j]);
      }
  }

  glob.count = 0;
  cairo_stroke(cr);    

  return FALSE;
}

static gboolean clicked(GtkWidget *widget, GdkEventButton *event, gpointer user_data) {
    if (event->button == 1) {
        glob.coordx[glob.count] = event->x;
        glob.coordy[glob.count++] = event->y;
    }

    if (event->button == 3) {
        gtk_widget_queue_draw(widget);
    }

    return TRUE;
}


int main(int argc, char *argv[])
{
  GtkWidget *window;
  GtkWidget *darea;
  
  glob.count = 0;

  gtk_init(&argc,&argv);

  window = gtk_window_new(GTK_WINDOW_TOPLEVEL);

  darea = gtk_drawing_area_new();
  gtk_container_add(GTK_CONTAINER(window),darea);
 
  gtk_widget_add_events(window,GDK_BUTTON_PRESS_MASK);

  g_signal_connect(G_OBJECT(darea),"draw",G_CALLBACK(on_draw_event),NULL); 
  g_signal_connect(window,"destroy",G_CALLBACK(gtk_main_quit),NULL);  
    
  g_signal_connect(window,"button-press-event",G_CALLBACK(clicked),NULL);
 
  gtk_window_set_position(GTK_WINDOW(window),GTK_WIN_POS_CENTER);
  gtk_window_set_default_size(GTK_WINDOW(window),400,300); 
  gtk_window_set_title(GTK_WINDOW(window),"Lines");

  gtk_widget_show_all(window);

  gtk_main();

  return 0;
}
-- possibly easier?:
static gboolean on_draw_event(GtkWidget *widget, cairo_t *cr, gpointer user_data) {    
  cairo_set_source_rgb(cr,0,0,0);
  cairo_select_font_face(cr,"Sans",CAIRO_FONT_SLANT_NORMAL,CAIRO_FONT_WEIGHT_NORMAL);
  cairo_set_font_size(cr,40.0);

  cairo_move_to(cr,10.0,50.0);
  cairo_show_text(cr,"Disziplin ist Macht.");     (Discipline is Power)
  return FALSE;
}

int main(int argc, char *argv[])
{
  GtkWidget *window;
  GtkWidget *darea;

  window = gtk_window_new(GTK_WINDOW_TOPLEVEL);

  darea = gtk_drawing_area_new();
  gtk_container_add(GTK_CONTAINER(window),darea);

  g_signal_connect(G_OBJECT(darea),"draw",G_CALLBACK(on_draw_event),NULL); 
  g_signal_connect(window,"destroy",G_CALLBACK(gtk_main_quit),NULL);

  gtk_window_set_position(GTK_WINDOW(window),GTK_WIN_POS_CENTER);
  gtk_window_set_default_size(GTK_WINDOW(window),400,90); 
  gtk_window_set_title(GTK_WINDOW(window),"GTK window");

  gtk_widget_show_all(window);

  gtk_main();

  return 0;
}
--*/
--gtk_container_set_border_width(window,15);
-- https://www.codeproject.com/Articles/1042516/Custom-Controls-in-Win-API-Scrolling


global procedure gSetAttributes(gdx id, string attributes, sequence args={})
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
--                      names = append(names,name)
--                      vals = append(vals,name)
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

global procedure gCanvasSetBackground(gdx canvas, atom colour)
    ctrl_xtra[canvas][CX_CANVAS_BACK] = colour
    ctrl_xtra[canvas][CX_CANVAS_FLAGS] ||= CXCF_REPEN
end procedure

global function gCanvasGetBackground(gdx canvas)
    return ctrl_xtra[canvas][CX_CANVAS_BACK]
end function

global procedure gCanvasSetForeground(gdx canvas, atom colour)
    ctrl_xtra[canvas][CX_CANVAS_FORE] = colour
    ctrl_xtra[canvas][CX_CANVAS_FLAGS] ||= CXCF_REPEN
end procedure

global function gCanvasGetForeground(gdx canvas)
    return ctrl_xtra[canvas][CX_CANVAS_FORE]
end function

local procedure xpg_register_handler(integer ct, string name, sequence sigs)
    setd({ct,name},sigs,handler_sigs)
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
--      assert(integer(id) and id>=1 and id<=length(ctrl_handles)) -- (already covered)
        integer ct = ctrl_types[id]
        if name="ACTION" then
            -- be helpful:
            if ct=CANVAS then
                crash(`gSetHandler(CANVAS,"ACTION",rtn) should be changed to "REDRAW"`)
            elsif ct=CHECKBOX or ct=TEXT then
                crash(`gSetHandler(%s,"ACTION",rtn) should be changed to "VALUE_CHANGED"`,{ct})
            end if
        end if
        -- avoid time-bombs like that old KEY_CB thing (whereby a program would work fine,
        -- but crash when you hit a function key, at one point I ended up with 92 of 'em):
        if handler!=NULL then
            sequence sig = get_routine_info(handler,false),         -- (actual)
                     rig = getdd({ct,name},{"???"},handler_sigs)    --  (rqd)
--          if sig!=rig then
            if not find(sig,rig) then
                rig = getdd({0,name},rig,handler_sigs)    --  (common handlers?)
                if not find(sig,rig) then
                    string cn = ctrl_names[ct]
--                  crash("%s(%s) handler signature error (%v!=%v)",{cn,name,sig,rig})
                    crash("%s(%s) handler signature error find(%v,%v)=0",{cn,name,sig,rig})
                end if
            end if
        end if
        -- aside: using deld() for handler==NULL should make no real difference
        setd({id,name},handler,handlers)
    end if
end procedure

global procedure gSetHandlers(gdx id, sequence name, rtn handler=NULL)
    -- (simple alias that implies but does not enforce that h|name|both are non-unitary)
    gSetHandler(id,name,handler)
end procedure

global function gGetHandler(gdx id, string name, integer dflt=0)
    rtn rid = getdd({id,name},dflt,handlers)
    return rid
end function

local function gGetInheritedHandler(gdx id, string name)
    rtn rid = getd({id,name},handlers)
    while rid=0 and parent_ids[id] do
        id = parent_ids[id]
        rid = getd({id,name},handlers)
    end while
--  return {id,rid}
    return rid
end function

local procedure xpg_setID(atom handle, integer id)
    ctrl_handles[id] = handle
    ctrl_flags[id] = or_bits(ctrl_flags[id],CF_MAPPED)
    integer ct = ctrl_types[id]
    assert(ct!=TIMER)
    if backend=GTK then
        setd(handle,id,GTK_ID_LOOKUP)
    elsif backend=WinAPI then
        if handle!=NULL then -- (not a virtual box)
            {} = c_func(xSetWindowLong,{handle,GWL_USERDATA,id})
            if ct=DIALOG then
                wnd_proc_addr[id] = c_func(xGetWindowLong,{handle,GWL_WNDPROC})
            end if
        end if
    else
        ?9/0 -- (unknown backend)
    end if
end procedure

local function xpg_getID(xpg_handle handle)
    -- returns: a gdx
    integer id = 0
    if handle!=NULL then
        if backend=GTK then
            id = getd(handle,GTK_ID_LOOKUP)
        elsif backend=WinAPI then
            id = c_func(xGetWindowLong,{handle,GWL_USERDATA})
        else
            ?9/0 -- (unknown backend)
        end if
        assert(iff(id?ctrl_handles[id]=handle
                     :find(handle,ctrl_handles)=0))
    end if
    return id
end function

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

global procedure gHide(gdx id)
--  assert(ctrl_types[id]=DIALOG)   -- placeholder...
    if ctrl_types[id]!=DIALOG then
        gSetAttribute(id,"VISIBLE",false)
        return
    end if
    atom handle = ctrl_handles[id]
    if backend=GTK then
        c_proc(gtk_widget_hide,{handle})
--DEV is dialog?
        if c_func(gtk_window_get_transient_for,{handle})=NULL then
            c_proc(gtk_main_quit)
        end if
    elsif backend=WinAPI then
--DEV missing call? and/or test as above?
        {} = c_func(xSendMessage,{handle,WM_CLOSE,0,0}) -- this line has the better way of closing!!!
--      {} = c_func(xCloseWindow,{handle})  -- this function will minimize a child window or minimize & 
--                                          --  half close a main window. very poor name & behaviour!
    else
        ?9/0 -- (unknown backend)
    end if
end procedure

--local function xpg_gtk_quit() -- (GTK only)
--local function xpg_gtk_quit((GtkObject *object, gpointer user_data))  -- (GTK only)
local function xpg_gtk_quit(atom handle, gdx id) -- (GTK only)
--  c_proc(gtk_main_quit) 
    assert(id=xpg_getID(handle))
--  gHide(id)
    gHide(gGetDialog(id))
    return 0 
end function 

global function gGetKeyName(integer c)
    for k in key_mappings do
        if c=k[2] then return k[1] end if
    end for
    if c>=' ' and c<='~' then
        return ""&c
    end if
    return "unknown"
end function

global function gGetColourName(atom c)
    if c=-1 then return "-1" end if
    for k in known_colours do
        if c=k[2] then return k[1] end if
    end for
    return sprintf("#%06x",c)
end function

integer key_map = NULL

local function xpg_map_key(integer c)
    if key_map=NULL then
        key_map = new_dict()
        integer kdx
        if backend=GTK then
            kdx = 4
        elsif backend=WinAPI then
            kdx = 3
        else
            ?9/0
        end if
        for k in key_mappings do
--          setd(k[2],k[kdx],key_map)
--?{"setd",k[kdx],k[2]}
            setd(k[kdx],k[2],key_map)
--local constant key_mappings = {{"VK_ESC",VK_ESC,   #1B, #FF1B}}
--?k
        end for
    end if
--?{"xpg_map_key",c,getdd(c,c,key_map)}
    return getdd(c,c,key_map)
end function

local function xpg_key_handler(gdx id, integer key, bool ctrl, shift, alt)
    -- GTK and WinAPI part
--?{id,key,gGetKeyName(key),ctrl,shift,alt}
--  integer khandler = gGetHandler(id,"KEY",true)
    integer khandler = gGetInheritedHandler(id,"KEY")
--  integer khandler
--  {id,khandler} = gGetInheritedHandler(id,"KEY")
    integer res = XPG_DEFAULT
    if khandler then
        sequence sig = get_routine_info(khandler,false)
        if sig={5,5,"FOIIII"} then
            res = khandler(id,key,ctrl,shift,alt)
        elsif not alt then
            if sig={4,4,"FOIII"} then
                res = khandler(id,key,ctrl,shift)
            elsif (not shift) 
               or (key>=#21 and key<=#7E) 
               or key=VK_POUND then
                if sig={3,3,"FOII"} then
                    res = khandler(id,key,ctrl)
                elsif not ctrl then
                    if sig={2,2,"FOI"} then
                        res = khandler(id,key)
                    else
                        ?9/0 -- unknown sig...
                    end if
                end if
            end if
        end if
        if res=XPG_CLOSE then
--          gHide(id)
            gHide(gGetDialog(id))
            return true
        end if
        if res=XPG_IGNORE then
            return true
        end if
    end if
--DEV TBC:
--  integer k = iff(
--  if not or_all({ctrl,shift,alt}) and res=XPG_DEFAULT then
--      if key=VK_CR then
----           CX_DEF_ENTER,
----           CX_DEF_ESCAPE,
--      elsif key=VK_ESC then
--      end if
--  end if
    if key=VK_ESC and not or_all({ctrl,shift,alt}) then
        id = gGetDialog(id)
        bool bClose = and_bits(CF_CLOSE_ON_ESC,ctrl_flags[id])!=0
        if bClose then
--?"close..."
--sleep(2)
            gHide(id)
            return true
        end if
    end if
    return false
end function

--local function xpg_gtk_check_escape(atom handle, event, /*data*/) -- (GTK only)
local function xpg_gtk_check_escape(atom handle, event, gdx id) -- (GTK only)
    -- GTK only part (WinAPI equivalent in xpg_WinAPI_WndProc)
    -- connected to the dialog, not any of the child elements of one,
    -- but (I assume that) gtk bubbles up any messages appropriately.
    -- (aside: not really worth checking handle is an xpg_handle)
--  integer id = xpg_getID(handle),
--  event_type = get_struct_field(idGdkEventKey,event,"event_type"),
    assert(id=xpg_getID(handle))
    integer keyval = get_struct_field(idGdkEventKey,event,"keyval"),
             state = get_struct_field(idGdkEventKey,event,"state"),
               key = xpg_map_key(keyval),
              ctrl = and_bits(state,GDK_CONTROL_MASK)!=0,
             shift = and_bits(state,GDK_SHIFT_MASK)!=0,
               alt = and_bits(state,GDK_ALT_MASK)!=0
--printf(1,"keyval:#%x, state:#%x, key:#%x\n",{keyval,state,key})
--?{"event_type",event_type} -- 8 aka GDK_KEY_PRESS

    return xpg_key_handler(id,key,ctrl,shift,alt)
end function

local function xpg_click_handler(gdx id, sequence status, integer x, y)
    -- GTK and WinAPI part
    integer clickhandler = gGetHandler(id,"CLICK")
    if clickhandler then
        sequence sig = get_routine_info(clickhandler,false)
        if sig={4,4,"FOPII"} then
            return clickhandler(id,status,x,y)
        elsif sig={2,2,"FOP"} then
            return clickhandler(id,status)
        else
            ?9/0 -- unknown sig...
        end if
    end if
    return false
end function

-- DEV probably wants attaching to a few more things...
--static gboolean clicked(GtkWidget *widget, GdkEventButton *event, gpointer user_data)
local function xpg_gtk_click(atom widget, event, gdx id)
    -- GTK only part (WinAPI equivalent in xpg_WinAPI_SubProc)
    assert(id=xpg_getID(widget))
    integer event_type = get_struct_field(idGdkEventButton,event,"event_type")
    if event_type!=GDK_TRIPLE_BUTTON_PRESS then -- (ignore(6), since WinAPI has nothing similar)
        integer btn = get_struct_field(idGdkEventButton,event,"button"),
             button = "LMR"[btn],
            pressed = "SD?R"[event_type-3], -- GDK_BUTTON_PRESS..GDK_BUTTON_RELEASE
              state = get_struct_field(idGdkEventButton,event,"state"),
               ctrl = and_bits(state,GDK_CONTROL_MASK)!=0,
              shift = and_bits(state,GDK_SHIFT_MASK)!=0,
                alt = and_bits(state,GDK_ALT_MASK)!=0,
                  x = get_struct_field(idGdkEventButton,event,"x",true),
                  y = get_struct_field(idGdkEventButton,event,"y",true)
        sequence status = {button,pressed,ctrl,shift,alt}
        return xpg_click_handler(id,status,x,y)
    end if
    return false
end function

local procedure xpg_mousemove_handler(gdx id, integer x,y, left,middle,right, ctrl,shift,alt)
    -- GTK and WinAPI part
    integer mousehandler = gGetHandler(id,"MOUSEMOVE")
    if mousehandler then
        sequence sig = get_routine_info(mousehandler,false)
        if sig={3,3,"POII"} then
            mousehandler(id,x,y)
        elsif sig={6,6,"POIIIII"} then
            mousehandler(id,x,y,left,middle,right)
        elsif sig={9,9,"POIIIIIIII"} then
            mousehandler(id,x,y,left,middle,right,ctrl,shift,alt)
        else
            ?9/0 -- unknown sig...
        end if
    end if
end procedure

-- DEV probably wants attaching to a few more things...
--static gboolean clicked(GtkWidget *widget, GdkEventButton *event, gpointer user_data)
local function xpg_gtk_mousemove(atom widget, event, gdx id)
--?"xpg_gtk_mousemove"
    -- GTK only part (WinAPI equivalent in xpg_WinAPI_SubProc)
    assert(id=xpg_getID(widget))
    integer event_type = get_struct_field(idGdkEventMotion,event,"event_type"),
--?9/0
--                 btn = get_struct_field(idGdkEventMotion,event,"button"),
--           button = "LMR"[btn],
--          pressed = "SD?R"[event_type-3], -- GDK_BUTTON_PRESS..GDK_BUTTON_RELEASE
                 state = get_struct_field(idGdkEventMotion,event,"state"),
                  left = and_bits(state,GDK_LEFT_MASK)!=0,
                middle = and_bits(state,GDK_MIDDLE_MASK)!=0,
                 right = and_bits(state,GDK_RIGHT_MASK)!=0,
                  ctrl = and_bits(state,GDK_CONTROL_MASK)!=0,
                 shift = and_bits(state,GDK_SHIFT_MASK)!=0,
                   alt = and_bits(state,GDK_ALT_MASK)!=0,
--DEV under 64-bit GTK3, x was 68.22259521 ... summat not right... [round() prob.temp]
--                   x = get_struct_field(idGdkEventMotion,event,"x",true),
--                   y = get_struct_field(idGdkEventMotion,event,"y",true)
                     x = round(get_struct_field(idGdkEventMotion,event,"x",true)),
                     y = round(get_struct_field(idGdkEventMotion,event,"y",true))

--printf(1,"mousemove: x:%d, y:%d, ctrl:%d, shift:%d, alt:%d, state:%b\n",
--          {x,y,ctrl,shift,alt,state})
--printf(1,"mousemove: x:%d, y:%d, left:%d, middle:%d, right:%d, ctrl:%d, shift:%d, alt:%d\n",
--          {x,y,left,middle,right,ctrl,shift,alt})
--printf(1,"mousemove: GDK_LEFT_MASK:%b, GDK_MIDDLE_MASK:%b, GDK_RIGHT_MASK:%b, state:%b\n",
--          {GDK_LEFT_MASK,GDK_MIDDLE_MASK,GDK_RIGHT_MASK,state})

--      sequence status = {button,left,midle,right,ctrl,shift,alt}
    xpg_mousemove_handler(id,x,y,left,middle,right,ctrl,shift,alt)
--/*
  GDK_SHIFT_MASK    = 1 << 0,
  GDK_LOCK_MASK     = 1 << 1,
  GDK_CONTROL_MASK  = 1 << 2,
  GDK_MOD1_MASK     = 1 << 3,
  GDK_MOD2_MASK     = 1 << 4,
  GDK_MOD3_MASK     = 1 << 5,
  GDK_MOD4_MASK     = 1 << 6,
  GDK_MOD5_MASK     = 1 << 7,
  GDK_BUTTON1_MASK  = 1 << 8,
  GDK_BUTTON2_MASK  = 1 << 9,
  GDK_BUTTON3_MASK  = 1 << 10,
  GDK_BUTTON4_MASK  = 1 << 11,
  GDK_BUTTON5_MASK  = 1 << 12,

        string tGdkEventMotion = """typedef struct GdkEventMotion {
                                      GdkEventType type;
                                      GdkWindow* window;
                                      gint8 send_event;
                                      guint32 time;
                                      gdouble x;
                                      gdouble y;
                                      gdouble* axes;
                                      GdkModifierType* state;
                                      gint16 is_hint;
                                      GdkDevice* device;
                                      gdouble x_root;
                                      gdouble y_root;
                                    };"""

--*/
    return true
end function

--/!*
local procedure xpg_resize(gdx id, integer w,h)
    --
    -- Common to WinAPI and GTK, check for and invoke RESIZE handler
    --
    -- Since GTK insists on being such an absolute dick about such matters,
    -- w,h are/must be the **client** size, not the window size.
    --
    assert(ctrl_types[id]=DIALOG)
--21/7/23: (no help...erm...)
--  ctrl_size[id][SZ_USER_W] = w
--  ctrl_size[id][SZ_USER_H] = h

    integer cfi = ctrl_flags[id]
--  if and_bits(cfi,CF_IGNORESIZE)=0 then
    if and_bits(cfi,CF_MAPPED)!=0 and
       and_bits(cfi,CF_IGNORESIZE)=0 then
?{"xpg_resize",id,w,h,ctrl_handles[id],xpg_get_window_rect(id)}
--if w=0 and h=0 then ?9/0 end if
        ctrl_flags[id] = cfi+CF_IGNORESIZE
--probably lots more to do here...

--(untried, would need to actually apply them as well...)
        xpg_lm_disperse_user_sizes(id,w,h)
        xpg_lm_calculate_offsets(id)
        xpg_lm_apply_offsets(id)

--...and I'm going to leave this undocumented...
        integer resize = gGetHandler(id,"RESIZE")
        if resize then
            resize(id,w,h)
        end if
        ctrl_flags[id] -= CF_IGNORESIZE
    end if
--/*
Searching for: RESIZE_CB
 Files scanned 1344, Directories scanned 61, Lines 588921
E:\downloads\misc\Phix\demo\libxlsxwriter\iup-3.24_Sources\iup\include\iupcbs.h:22 typedef int (*IFnii)(Ihandle*, int, int);  /* resize_cb, caret_cb, matrix_mousemove_cb, enteritem_cb, leaveitem_cb, scrolltop_cb, dr*/
E:\downloads\misc\Phix\demo\libxlsxwriter\iup-3.24_Sources\iup\include\iupdef.h:35 #define IUP_RESIZE_CB      "RESIZE_CB"
E:\downloads\misc\Phix\demo\libxlsxwriter\iup-3.24_Sources\iup\src\iup_canvas.c:140   iupClassRegisterCallback(ic, "RESIZE_CB", "ii");
E:\downloads\misc\Phix\demo\libxlsxwriter\iup-3.24_Sources\iup\src\iup_dialog.c:937   iupClassRegisterCallback(ic, "RESIZE_CB", "ii");
E:\downloads\misc\Phix\demo\libxlsxwriter\iup-3.24_Sources\iup\src\gtk\iupgtk_canvas.c:377   IFnii cb = (IFnii)IupGetCallback(ih, "RESIZE_CB");
E:\downloads\misc\Phix\demo\libxlsxwriter\iup-3.24_Sources\iup\src\gtk\iupgtk_dialog.c:767     cb = (IFnii)IupGetCallback(ih, "RESIZE_CB");
  g_signal_connect(G_OBJECT(ih->handle), "configure-event",    G_CALLBACK(xpg_gtk_configure_event), ih);
static gboolean xpg_gtk_configure_event(GtkWidget *widget, GdkEventConfigure *evt, Ihandle *ih)
struct _GdkEventConfigure
{
  GdkEventType type;
  GdkWindow *window;
  gint8 send_event;
  gint x, y;
  gint width;
  gint height;
};
    if (ih->data->ignore_resize) return FALSE; 

    int border, caption, menu;
    iupdrvDialogGetDecoration(ih, &border, &caption, &menu);
    cb = (IFnii)IupGetCallback(ih, "RESIZE_CB");
    /* width and height here are for the client area */
    if (!cb || cb(ih, evt->width, evt->height - menu)!=IUP_IGNORE) {
      ih->data->ignore_resize = 1;
      IupRefresh(ih);
      ih->data->ignore_resize = 0;
    }

E:\downloads\misc\Phix\demo\libxlsxwriter\iup-3.24_Sources\iup\src\win\iupwin_canvas.c:444       IFnii cb = (IFnii)IupGetCallback(ih, "RESIZE_CB");
E:\downloads\misc\Phix\demo\libxlsxwriter\iup-3.24_Sources\iup\src\win\iupwin_dialog.c:391   cb = (IFnii)IupGetCallback(ih, "RESIZE_CB");
static int winDialogBaseProc(Ihandle* ih, UINT msg, WPARAM wp, LPARAM lp, LRESULT *result)
  case WM_SIZE:
      if (ih->data->ignore_resize) break;
      switch(wp)
      case SIZE_MAXIMIZED:
          winDialogResize(ih, LOWORD(lp), HIWORD(lp));
      case SIZE_RESTORED:
          winDialogResize(ih, LOWORD(lp), HIWORD(lp));
static void winDialogResize(Ihandle* ih, int width, int height) {
  iupdrvDialogGetSize(ih, NULL, &(ih->currentwidth), &(ih->currentheight));
  IFnii cb = (IFnii)IupGetCallback(ih, "RESIZE_CB");
  /* width and height here are for the client area */
  if (!cb || cb(ih, width, height)!=IUP_IGNORE) {
    ih->data->ignore_resize = 1;
    IupRefresh(ih);
    ih->data->ignore_resize = 0;
  }
}
--*/
end procedure
--*!/

local function xpg_gtk_configure_event(atom widget, event, gdx id)
--?"xpg_gtk_configure_event"
    -- GTK only part (WinAPI equivalent in xpg_WinAPI_resize)
    assert(id=xpg_getID(widget))
    integer ct = ctrl_types[id] 
    assert(ct=DIALOG)
--  integer event_type = get_struct_field(idGdkEventConfigure,event,"event_type"),
    integer w = get_struct_field(idGdkEventConfigure,event,"width"),
            h = get_struct_field(idGdkEventConfigure,event,"height")
    xpg_resize(id,w,h)
--NB: this stopped gCanvas resizing itself...
--  return true
    return false
end function

gdx gtk_focusid = NULL

local function xpg_gtk_focusinout(atom widget, event, gdx id)
    assert(id=xpg_getID(widget))
-- Maybe: FOCUS_CB here...
    integer e_in = get_struct_field(idGdkEventFocus,event,"in")
--?{"focus",e_in}
    if e_in then
        gtk_focusid = id
    else
        gtk_focusid = NULL
    end if
    return false
end function

global function gGetBrother(gdx id, bool bPrev = false)
    integer pid = parent_ids[id]
    sequence children = children_ids[pid]
    integer k = find(id,children)
    assert(k!=0)
    if bPrev then
        return iff(k=1?NULL:children[k-1])
    end if
    return iff(k=length(children)?NULL:children[k+1])
end function

global function gGetChild(gdx id, integer pos)
    return children_ids[id][pos]
end function

global function gGetChildCount(gdx id)
    return length(children_ids[id])
end function

global function gGetParent(gdx id)
    return parent_ids[id]
end function

--local function xpg_get_parent_handle(gdx id)
local function xpg_get_parent_id(gdx id)
    -- skip any virtual containders (WinAPI only) ...
    integer parent = parent_ids[id]
    while parent!=NULL and ctrl_handles[parent]=NULL do
        parent = parent_ids[parent]
    end while
--  return ctrl_handles[parent]
    return parent
end function

local procedure xpg_gtk_add_to(gdx parent, atom child_handle)
    atom ph = ctrl_handles[parent]
    if ctrl_types[parent]=BOX then
--      c_proc(gtk_box_pack_start,{ph,child_handle,false,false,0})
        c_proc(gtk_fixed_put,{ph,child_handle,0,0})
    else
        c_proc(gtk_container_add,{ph,child_handle})
    end if
end procedure

global procedure gSetFocus(gdx id)
    if id=NULL then return end if   -- quietly ignore
    integer ct = ctrl_types[id]
    if backend=GTK then
--/*
        gdx pid = gGetDialog(id)
        atom ph = ctrl_handles[pid]
        bool bOK = c_func(gtk_window_is_active,{ph})
?{"bOK",bOK}
        if not bOK then
            atom cet = c_func(gtk_get_current_event_time,{})
?cet
--gtk_widget_get_window(widget);
--          c_proc(gdk_window_focus,(iupgtkGetWindow(dialog->handle),cet);
            c_proc(gdk_window_focus,{ph,cet})
?"focus"
        end if
--*/
        -- (ah, might be inactive:)
        integer cf = ctrl_flags[id]
        if not and_bits(cf,CF_INACTIVE) then
            bool bOK = c_func(gtk_widget_grab_focus,{ctrl_handles[id]})
--?{"bOK2",bOK}
            assert(bOK)
        end if

    elsif backend=WinAPI then
        if ct=DIALOG then
            -- Attempts to programmatically set the focus onto a window
            -- (like Edita does with main) must get rid of any auto-focus info.
--          ctrl_xtra[id] = UNDEFINED
            ctrl_xtra[id][CX_LAST_FOCUS] = UNDEFINED
        end if
        c_proc(xSetFocus,{ctrl_handles[id]})
--/*
--from arwen:
    if ct=TabItem then
        if ObjectExtra[id] then
            gdx pID = ObjectParent[id]
            atom hwnd = ObjectHwnd[pID]
            if c_func(xIsWindowVisible,{hwnd}) then
                integer iPart = c_func(xSendMessage,{hwnd,TCM_GETCURSEL,0,0}) + 1
                if iPart then
                    integer idCurrent = ObjectChildren[pID][iPart]
                    iPart = find(id,ObjectChildren[pID])
                    assert(iPart!=0)
                    if idCurrent!=id then -- Aha, time to switch over tab selections
                        void = c_func(xSendMessage,{hwnd,TCM_SETCURSEL,iPart-1,0})
                        setVisible(ObjectChildren[idCurrent],False) -- hide the old tab
                        setVisible(ObjectChildren[id],True) -- show the new tab
                        c_proc(xSetFocus,{hwnd}) -- VERY IMPORTANT LINE!!!!
                    else -- merely force focus onto tab (which is already active)
                        c_proc(xSetFocus,{hwnd}) -- VERY IMPORTANT LINE!!!!
                        void = c_func(xSendMessage,{hwnd,TCM_SETCURFOCUS,iPart-1,0})
                    end if
                end if
            end if
        end if
--*/
    else
        ?9/0 -- (unknown backend)
    end if
end procedure

global function gGetFocus()
--  ?{"gGetFocus"}
    gdx id = 0
    if backend=GTK then
        return gtk_focusid
    elsif backend=WinAPI then
        atom hFocus = c_func(xGetFocus,{})
        if hFocus then
            id = xpg_getID(hFocus)
--from arwen:
--          if id and ObjectType[id]=TabControl then
--              id = getIndex(id)
--          end if
        end if
    else
        ?9/0 -- (unknown backend)
    end if
    return id
end function

integer nMapDepth = 0

local procedure xpg_WinAPI_resize(gdx id, integer w=0, h=0)
    if nMapDepth!=0 then return end if
--?9/0
    integer ct = ctrl_types[id] 
--  if ct=DIALOG then
--      {w,h} = xpg_get_client_rect(id)[3..4]
--  end if
-- start simple:get a canvas to fill properly... [DONE, also treeview[BLUFF]]
--  if ct=DIALOG then
if false then
        -- single child cases:
        sequence children = children_ids[id]
        integer lc = length(children)
--      assert(lc=1)
        assert(lc<=1)
if lc=1 then
--if false then
        gdx chid = children[1]
  if ctrl_types[chid]=CANVAS then 
--if h=14 then ?9/0 end if
        atom handle = ctrl_handles[chid],
--           flags = SWP_NOMOVE+SWP_NOZORDER+SWP_NOACTIVATE
             flags = SWP_NOZORDER+SWP_NOACTIVATE
?{"xpg_WinAPI_resize(temp/CANVAS)",id,ct,ctrl_names[ct],{w,h},children,handle} -- this should be done elsewhere!
        if handle!=NULL  -- (mid-map?)
        or ct=BOX then -- (under winAPI boxes are virtual)
            if handle!=NULL then
--          if handle!=NULL and ct!=DIALOG then
--if w>6 and h>6 then
--              if w<=6 or h<=6 then flags ||= SWP_NOSIZE end if
?{"xpg_WinAPI_resize",id,w,h}
                bool ok = c_func(xSetWindowPos,{handle,NULL,0,0,w,h,flags})
--DEV/test/temp:
--              bool ok = c_func(xSetWindowPos,{handle,NULL,3,3,w-6,h-6,flags})
                assert(ok)
--end if
            end if
--DEV only if has children??
--          xpg_WinAPI_resize(chid,w,h)
        end if
  end if
end if
    end if
--  

--/*
    // recursively, honouring EXPAND (etc) and invoking any RESIZE_CB/REDRAW_CB
    let cn = ih.classList[0];
    if (cn !== "dialog-header" &&   // (there can be no ["ACTION"] attached
        cn !== "dialog-resizers") { //  to either, so simplify debugging..)
        let bSizeChanged = false,
             bDoChildren = false,
                  prev_w,
                  prev_h;

        if (cn === "canvas") {
            prev_w = ih.width;
            prev_h = ih.height;
            if (prev_w !== w || prev_h !== h) {
                let ctx = ih.getContext("2d") || ih.getContext("webgl"),
                    // save, since they get trashed(!!):
                    fs = ctx.fillStyle,
                    ss = ctx.strokeStyle,
                    ta = ctx.textAlign,
                    tb = ctx.textBaseline,
                    cf = ctx.font;
    
                ih.width = w;
                ih.height = h;
                ctx.fillStyle = fs;
                ctx.strokeStyle = ss;
                ctx.textAlign = ta;
                ctx.textBaseline = tb;
                ctx.font = cf;
                bSizeChanged = true;
            }
        } else if (cn === "container" ||
                   cn === "table") {
        } else {
            prev_w = ih.offsetWidth,
            prev_h = ih.offsetHeight;
            let nw = w-ih.offsetLeft,
                nh = h-(ih.offsetTop-45);
            if (prev_w !== nw || prev_h !== nh) {
                bSizeChanged = true;    
                let expand = ih.EXPAND;
                if (expand && expand !== "NO") {
                    bDoChildren = true;
                }
            }
        }

        if (bSizeChanged) {
            let resize_cb = ih.RESIZE_CB,
//DEV redraw...
                action_cb = ih.ACTION;

            if (resize_cb) {
                resize_cb(ih,w,h);
            }
            if (action_cb && cn === "canvas") { 
                action_cb(ih); 
            }

            if (bDoChildren || cn === "dialog" || cn === "dialog-body") {
                // for eg {button,fill,button,fill,button}, we want {0,1,0,2,0} and v_count of 2.
                // each non-zero gets (2-1+1)/2, (2-2+1)/1 of the remaining prev_w-w, iyswim.
                let w_children = [],    w_count = 0,    // width candidates
                    h_children = [],    h_count = 0,    // height candidates
                      children = ih.childNodes,
                  l = children.length;

                for (let i=0; i<l; i += 1) {
                    let ci = children[i],
                        expand = ci.EXPAND,
                        cin = ci.classList[0] || ci.localName;
                    if (cin === "dialog-body") {
                        expand = "YES";
                    } else if (!expand) {
                        let ccv = ci.classList.contains("expandv"),
                            cch = ci.classList.contains("expandh");
                        expand = ccv ? (cch ? "YES" : "HORIZONTAL")
                                     : (cch ? "VERTICAL" : "NO");
                    }
                    if (cin === "fill") {
//DEV more complex: If User size is not defined, then when inside a gHbox EXPAND is HORIZONTAL, 
//                                                    when inside a gVbox EXPAND is VERTICAL.
//                  If User size is defined then EXPAND is NO. 
                        if (cn === "hbox") {
                            expand = "HORIZONTAL";
                        } else if (cn === "vbox") {
                            expand = "VERTICAL";
                        }
                    }
                    if (expand && expand !== "NO") {
//Value: "YES" (both directions), "HORIZONTAL", "VERTICAL", "HORIZONTALFREE", "VERTICALFREE" or "NO".
//Default: "NO". For containers the default is "YES".
                        if (expand === "YES" ||
                            expand === "HORIZONTAL") {
                            w_count += 1;
                            w_children[i] = w_count;
                        }
                        if (expand === "YES" ||
                            expand === "VERTICAL") {
                            h_count += 1;
                            h_children[i] = h_count;
                        }
                    }
                }
                if (w_count || h_count) {
                    let rem_w = w-prev_w,
                        rem_h = h-prev_h,
                        wcrem = w_count,
                        hcrem = h_count;
                    for (let i=0; i<l; i += 1) {
                        let w = w_children[i],
                            h = h_children[i];
                        if (w || h) {
                            let ci = children[i],
                                cw = ci.offsetWidth,
                                ch = ci.clientHeight;
                            if (w) {
                                let dw = ((w_count-w+1)/wcrem)*rem_w;
                                cw += dw;
                                rem_w -= dw;
                                wcrem -= 1;
                            }
                            if (h) {
                                let dh = ((h_count-h+1)/hcrem)*rem_h;
                                ch += dh;
                                rem_h -= dh;
                                hcrem -= 1;
                            }
                            $resize_children(ci,cw,ch);
                        }
                    }
                    if ((w_count !== 0 && (rem_w !== 0 || wcrem !== 0)) ||
                        (h_count !== 0 && (rem_h !== 0 || hcrem !== 0))) {
                        crash("uh?");
                    }
                }
            }
        }
    }
}
--*/
    if ct=DIALOG then
--w -=2; h -= 32;
        xpg_resize(id,w,h)
    end if
end procedure

--global function rgba(atom red, green, blue, alpha=0xFF)
global function rgba(atom red, green, blue, alpha=0)
--  return and_bits(r,#FF) + and_bits(g,#FF)*#100 + and_bits(b,#FF)*#10000
--  return and_bits(r,#FF)*#10000 + and_bits(g,#FF)*#100 + and_bits(b,#FF)
--  alpha = 0xFF-and_bits(alpha,0xFF)
--  alpha = and_bits(alpha,0xFF)
    atom colour = and_bits(alpha,#FF)*#1000000 +
                  and_bits(red,  #FF)*#10000 + 
                  and_bits(green,#FF)*#100 + 
                  and_bits(blue, #FF)
    return colour
end function

--global function hsv_to_rgba(atom h, s, v, a=0xFF)
global function hsv_to_rgba(atom h, s, v, a=0)
    integer i = floor(h*6)
    atom f = h*6-i
    integer p = and_bits(v*(1-s)*#FF,#FF),
            q = and_bits(v*(1-s*f)*#FF,#FF),
            t = and_bits(v*(1-s*(1-f))*#FF,#FF),
            r,g,b
    v = and_bits(v*#FF,#FF)
--  a = #FF-and_bits(a*#FF,#FF)
    a = and_bits(a*#FF,#FF)
    switch i do
        case 0,
             6: {r,g,b} = {v,t,p}
        case 1: {r,g,b} = {q,v,p}
        case 2: {r,g,b} = {p,v,t}
        case 3: {r,g,b} = {p,q,v}
        case 4: {r,g,b} = {t,p,v}
        case 5: {r,g,b} = {v,p,q}
    end switch
--  return rgba(r*255,g*255,b*255,0xFF-a)
    return rgba(r,g,b,a)
end function

global function to_rgba(atom colour)
    integer red = and_bits(colour,#FF0000)/#10000,
          green = and_bits(colour,#FF00)/#100,
           blue = and_bits(colour,#FF),
          alpha = and_bits(colour,#FF000000)/#1000000
--  return {red,green,blue,#FF-alpha}
    return {red,green,blue,alpha}
end function    

--local function xpg_GTK_rgba_to_argb(atom colour)
--  integer a = #FF-and_bits(colour,#FF)*#100000,
--       argb = floor(colour/#100)+a
--  return argb
--end function

local function xpg_WinAPI_rgb_to_bgr(atom colour)
    -- (aside: in case it helps, this is bi-directional)
    atom r = and_bits(colour,#00FF0000)/#10000,
         b = and_bits(colour,#000000FF)*#10000,
         c = and_bits(colour,#FF00FF00)+r+b
--  colour -= r+b
--  r /= 0x10000
--  b *= 0x10000
--  colour += r+b
--  return colour
    return c
end function

local sequence brushColours = {}, brushes = {}

local function xpg_WinAPI_get_brush(atom colour)
    integer k = find(colour,brushColours)
    if k=0 then
        brushColours = append(brushColours,colour)
        atom bgr = xpg_WinAPI_rgb_to_bgr(colour)
        brushes = append(brushes,c_func(xCreateSolidBrush,{bgr}))
        k = length(brushes)
    end if
    return brushes[k]
end function

--/*
integer wm_dict = NULL

function begins_wm(string s) return begins("WM_",s) end function

function win_msg(integer msg)
    -- diagnostics assistant
--  static [wm_dict]
--  integer wm_dict = NULL
    sequence res = get_possible_constant_names(msg,include_file(),false)
--  nested function begins_wm(string s)
--      return begins("WM_",s)
--  end nested function
    sequence wm_only = filter(res,begins_wm)
    if length(wm_only) then res = wm_only end if
    if length(res)=0 then
--DEV/TEST: I hope the compiler does not optimise this away!! (when static/nested, that is)
        if wm_dict=NULL then
            --
            -- Otherwise unused messages we might want to put a name to.
            --
            -- The point of this is to avoid unused warnings for xpGUI.e:
            -- feel free to add more, and remove any that have actually
            -- found a real use. Most entries here typically also have a
            -- commented-out declaration above, for convenience sake only.
            --
            wm_dict = new_dict({{  1,"WM_CREATE"},
                                {  3,"WM_MOVE"},
                                {  6,"WM_ACTIVATE"},
                                {  8,"WM_KILLFOCUS"},
                                { 12,"WM_SETTEXT"},
                                { 13,"WM_GETTEXT"},
                                { 14,"WM_GETTEXTLENGTH"},
                                { 24,"WM_SHOWWINDOW"},
                                { 28,"WM_ACTIVATEAPP"},
                                { 32,"WM_SETCURSOR"},
                                { 33,"WM_MOUSEACTIVATE"},
                                { 36,"WM_GETMINMAXINFO"},
                                { 70,"WM_WINDOWPOSCHANGING"},
                                { 71,"WM_WINDOWPOSCHANGED"},
                                { 85,"WM_NOTIFYFORMAT"},
                                {123,"WM_CONTEXTMENU"},
                                {127,"WM_GETICON"},
                                {129,"WM_NCCREATE"},
                                {130,"WM_NCDESTROY"},
                                {131,"WM_NCCALCSIZE"},
                                {132,"WM_NCHITTEST"},
                                {133,"WM_NCPAINT"},
                                {134,"WM_NCACTIVATE"},
                                {160,"WM_NCMOUSEMOVE"},
                                {161,"WM_NCLBUTTONDOWN"},
                                {162,"WM_NCLBUTTONUP"},
                                {242,"BM_GETSTATE"},
                                {243,"BM_SETSTATE"},
                                {257,"WM_KEYUP"},
                                {274,"WM_SYSCOMMAND"},
                                {312,"WM_CTLCOLORSTATIC"},
                                {514,"WM_LBUTTONUP"},
                                {515,"WM_LBUTTONDBLCLK"},
                                {516,"WM_RBUTTONDOWN"},
                                {517,"WM_RBUTTONUP"},
                                {518,"WM_RBUTTONDBLCLK"},
                                {519,"WM_MBUTTONDOWN"},
                                {520,"WM_MBUTTONUP"},
                                {528,"WM_PARENTNOTIFY"},
--                              {532,"WM_SIZING"},
                                {533,"WM_CAPTURECHANGED"},
                                {561,"WM_ENTERSIZEMOVE"},
                                {562,"WM_EXITSIZEMOVE"},
                                {641,"WM_IME_SETCONTEXT"},
                                {642,"WM_IME_NOTIFY"},
                                {674,"WM_NCMOUSELEAVE"},
                                {675,"WM_MOUSELEAVE"},
                                {792,"WM_PRINTCLIENT"}})
        end if
        string s = getd(msg,"",wm_dict)
        if length(s) then res = {s} end if
    end if
    return res
end function

function ignorable_message(sequence s, integer msg)
    if length(s)=1 then
        string s1 = s[1]
        if begins("WM_NC",s1)
        or begins("WM_IME_",s1)
        or begins("WM_MOUSE",s1)
        or find(msg,{799,49314}) then   -- ???
            return true
        end if
    end if
    return false
end function
--*/

local function xpg_process_key(integer id, msg, atom wParam, lParam)
    --
    -- Firstly: get things down to one and precisely one message per keystroke.
    -- WM_CHAR is best for graphical characters, and distinguishes eg '1' from '!'
    -- WM_KEYDOWN overlaps a bit, but gets eg Insert (nb as the vk_code '-'/#2D...)
    -- WM_SYSCHAR also handles '1' vs '!', and critically allows supressing BELLs.
    -- WM_SYSKEYDOWN is finally needed for any remaining non-graphical alt-keys.
    --
--?{msg,wParam,lParam}
--  printf(1,"msg:%d, wParam:#%x, lParam:#%x\n",{msg,wParam,lParam})
--  bool hide = false -- (temp/debug: hide plain ctrl/shift/alt keys)
    bool ctrl = and_bits(c_func(xGetKeyState,{VK_CTRL}),#8000)!=0,
        shift = and_bits(c_func(xGetKeyState,{VK_SHIFT}),#8000)!=0,
          alt = and_bits(c_func(xGetKeyState,{VK_ALT}),#8000)!=0,
     bGraphic = (wParam>=#30 and wParam<=#5A)   -- 0..9 & A..Z
             or (wParam>=#60 and wParam<=#6F)   -- Numpad
             or (wParam>=#BA and wParam<=#E2)   -- OEM
--  string what = ""
    integer key = 0
    if msg=WM_CHAR then
        if not ctrl 
        and wParam>=#20 then
--          what = "WM_CHAR"
            key = wParam
            if key=#A3 then key=VK_POUND end if -- fudge...
        end if
    elsif msg=WM_KEYDOWN then
--    if not hide or (wParam!=VK_SHIFT and wParam!=VK_CTRL) then
--      if ctrl or not bGraphic then
        if ctrl or (not bGraphic and wParam!=VK_SP) then
--          what = "WM_KEYDOWN"
            if wParam>=VK_SHIFT and wParam<=VK_ALT then
                wParam=floor(lParam/#10000)
            end if
            key = xpg_map_key(wParam)
        end if
--    end if
    elsif msg=WM_SYSCHAR then
--      if wParam!=VK_BS then
--          what = "WM_SYSCHAR"
            key = wParam
--      end if
    elsif msg=WM_SYSKEYDOWN then
--    if not hide or wParam!=VK_ALT then
        if not bGraphic and (wParam>' ' or wParam=VK_ALT) then
--          what = "WM_SYSKEYDOWN"
            key = xpg_map_key(wParam)
            if wParam=VK_ALT then key=VK_LALT end if
        end if
--    end if
    end if
    if key then
--DEV/temp:
--      printf(1,"%s: key:%x (%s), ctrl:%d, shift:%d, alt:%d\n",
--              {what,key,gGetKeyName(key),ctrl,shift,alt})

        integer res = xpg_key_handler(id,key,ctrl,shift,alt)
        if res then return 0 end if
    end if
--/!*
--DEV temp code... needs the full proper handling
    if wParam=VK_TAB then
        integer fid = gGetFocus()
        if fid=0 then fid = gGetDialog(id) end if
        if fid then
            integer window = gGetDialog(fid),
                     child = children_ids[window][1]
            gSetFocus(child)
            return 0
        end if
    end if
--*!/
    return true -- (carry on with xCallWindowProc/xDefWindowProc)
end function

local procedure drawMenuBar(gdx menu)
    integer pid = parent_ids[menu]
    if pid then
        assert(ctrl_types[pid]=BOX)
        pid = parent_ids[pid]
        assert(ctrl_types[pid]=DIALOG)
        bool bOK = c_func(xDrawMenuBar,{ctrl_handles[pid]})
        assert(bOK)
    end if
end procedure

--function xpg_set_menu_attribute(gdx id, string name, object v, bool bMapped)
global procedure gMenuSetAttribute(gdx menu, integer id, string name, object v)
    bool bMapped = and_bits(ctrl_flags[menu],CF_MAPPED)!=0, bOK
    atom handle = ctrl_handles[menu], mh
    integer ct = ctrl_types[menu]
    assert(ct=MENU)
    assert(id!=0)
    if bMapped then
        if backend=GTK then
            mh = getd({menu,id},GTK_MENU_UPLOOK)
        elsif backend=WinAPI then
            mh = ctrl_handles[menu]
        else
            ?9/0 -- (unknown backend)
        end if
        if name="TITLE" then
            assert(string(v))
            if backend=GTK then
                c_proc(gtk_menu_item_set_label,{mh,v})
            elsif backend=WinAPI then
                object mi = getd({menu,id},WINAPI_SUBMENUS)
                bool byPos = sequence(mi)
                if byPos then {mh,id} = mi end if
                integer misize = get_struct_size(idMENUITEMINFO)

--DEV: quite why wiping this after a gMenuGetAttribute helps I dunno, but it **really** does.
--    (really wierd stuff, from suddenly invalid routine ids, to pause-then-quiet-termination)
-- (and obvs. similar handling elsewhere would be a good idea)
                mem_set(pMENUITEMINFO,0,misize)
                set_struct_field(idMENUITEMINFO,pMENUITEMINFO,"cbSize",misize)

                set_struct_field(idMENUITEMINFO,pMENUITEMINFO,"fMask",MIIM_TYPE)
                bOK = c_func(xGetMenuItemInfo,{mh,id,byPos,pMENUITEMINFO})
                assert(bOK)
                set_struct_field(idMENUITEMINFO,pMENUITEMINFO,"dwTypeData",xpg_raw_string_ptr(v))
                bOK = c_func(xSetMenuItemInfo,{mh,id,byPos,pMENUITEMINFO})
                assert(bOK)
                drawMenuBar(menu)
            else
                ?9/0 -- (unknown backend)
            end if
            return
        elsif name="ACTIVE" then
?{"ACTIVE",menu,id}
?def_menu_attr[menu]
            if string(v) then v = xpg_to_bool(v) end if
            if backend=GTK then
                c_proc(gtk_widget_set_sensitive,{mh,v})
            elsif backend=WinAPI then
                integer flags = or_bits(MF_BYCOMMAND,MF_GRAYED*(not v)),
                          res = c_func(xEnableMenuItem,{mh,id,flags})
                if res = -1 then -- not a menu item...
                    {mh,id} = getd({menu,id},WINAPI_SUBMENUS)
                    res = c_func(xEnableMenuItem,{mh,id,flags+MF_BYPOSITION})
                end if
                drawMenuBar(menu)
            else
                ?9/0 -- (unknown backend)
            end if
            return
        elsif name="CHECKED" then
?{"CHECKED",menu,id}
?def_menu_attr[menu]
            if string(v) then v = xpg_to_bool(v) end if
            if backend=GTK then
                c_proc(gtk_check_menu_item_set_active,{mh,v})
            elsif backend=WinAPI then
                object g = getd({menu,id},WIN_MENU_CHECKED)
                if sequence(g) then -- a radio group
                    atom state = c_func(xGetMenuState,{mh,id,MF_BYCOMMAND})
                    bool checked = state!=#FFFFFFFF and and_bits(state,MF_CHECKED)
                    integer idx = find(id,g[1])
                    assert(idx)
                    if v and not checked then
                        {g,mh} = g
                        bOK = c_func(xCheckMenuRadioItem,{mh,g[1],g[$],id,MF_BYCOMMAND})
                        assert(bOK,"CheckMenuRadioItem error #%08x (%d)",c_func(xGetLastError,{}))
                    end if
                else
                    c_proc(xCheckMenuItem,{mh,id,or_bits(MF_BYCOMMAND,MF_CHECKED*v)})
                end if
            else
                ?9/0 -- (unknown backend)
            end if
            return
        end if
    else -- not bMapped
--      integer k = find(menu,def_menu_attr[id][1])
        integer k = find(id,def_menu_attr[menu][1])
--  def_menu_attr = {{{1},{{`CHECKED`,1}}}}
        if k then
--          def_menu_attr[id][2][k] &= {{name,v}}
--          def_menu_attr[menu][2][k] &= {{name,v}}
            sequence nvk = def_menu_attr[menu][2][k]
            def_menu_attr[menu][2][k] = 0
            nvk &= {{name,v}}
            def_menu_attr[menu][2][k] = nvk
        else
--Grr...
--          def_menu_attr[id][1] &= menu
--          def_menu_attr[id][2] &= {{name,v}}
--          def_menu_attr[menu][1] &= id
--          def_menu_attr[menu][2] &= {{name,v}}
--  def_menu_attr = {{{},{}}}
            sequence {ids,nvs} = def_menu_attr[menu]
            def_menu_attr[menu]=0
            ids &= id
            nvs &= {{{name,v}}}
            def_menu_attr[menu]={ids,nvs}
        end if
        return
    end if
    crash("gMenuSetAttribute(%s) not supported",{name})
end procedure

--function xpg_get_menu_attribute(gdx id, string name, object dflt)
global function gMenuGetAttribute(gdx menu, integer id, string name)
    bool bMapped = and_bits(ctrl_flags[menu],CF_MAPPED)!=0,
         bOK, byPos = false
    integer ct = ctrl_types[menu]
    assert(ct=MENU)
    assert(id!=0)
    atom mh
    if bMapped then
        if backend=GTK then
            mh = getd({menu,id},GTK_MENU_UPLOOK)
        elsif backend=WinAPI then
            mh = ctrl_handles[menu]
        else
            ?9/0 -- (unknown backend)
        end if
    end if
    if name="TITLE" then
        assert(bMapped)
        if backend=GTK then
            atom pStr = c_func(gtk_menu_item_get_label,{mh})
            return peek_string(pStr)
        elsif backend=WinAPI then
            set_struct_field(idMENUITEMINFO,pMENUITEMINFO,"fMask",MIIM_STRING)
            set_struct_field(idMENUITEMINFO,pMENUITEMINFO,"dwTypeData",NULL)
            bOK = c_func(xGetMenuItemInfo,{mh,id,byPos,pMENUITEMINFO})
            if not bOK then
                {{mh,id},byPos} = {getd({menu,id},WINAPI_SUBMENUS),true}
                bOK = c_func(xGetMenuItemInfo,{mh,id,byPos,pMENUITEMINFO})
                assert(bOK)
            end if
            integer cch = get_struct_field(idMENUITEMINFO,pMENUITEMINFO,"cch")+1
            atom pMem = allocate(cch)
            set_struct_field(idMENUITEMINFO,pMENUITEMINFO,"cch",cch)
            set_struct_field(idMENUITEMINFO,pMENUITEMINFO,"dwTypeData",pMem)
            bOK = c_func(xGetMenuItemInfo,{mh,id,byPos,pMENUITEMINFO})
            string res = peek_string(pMem)
            free(pMem)
            return res
        else
            ?9/0 -- (unknown backend)
        end if
--/*
--probably not:
BOOL WINAPI SetMenuItemBitmaps(
  _In_      HMENU hMenu,
  _In_      UINT uPosition,
  _In_      UINT uFlags,
  _In_opt_  HBITMAP hBitmapUnchecked,
  _In_opt_  HBITMAP hBitmapChecked
);


BOOL WINAPI ModifyMenu(
  _In_      HMENU hMnu,
  _In_      UINT uPosition,
  _In_      UINT uFlags,
  _In_      UINT_PTR uIDNewItem,
  _In_opt_  LPCTSTR lpNewItem
);
*/
    elsif bMapped then
        if name="ACTIVE" then
            if backend=GTK then
                return c_func(gtk_widget_get_sensitive,{mh})
            elsif backend=WinAPI then
                atom state = c_func(xGetMenuState,{mh,id,MF_BYCOMMAND})
                if state=#FFFFFFFF then
                    {mh,id} = getd({menu,id},WINAPI_SUBMENUS)
                    state = c_func(xGetMenuState,{mh,id,MF_BYPOSITION})
                    assert(state!=#FFFFFFFF)
                end if
                bool res = not and_bits(state,MF_GRAYED)
                return res
            else
                ?9/0 -- (unknown backend)
            end if
        elsif name="CHECKED" then
            if backend=GTK then
                return c_func(gtk_check_menu_item_get_active,{mh})
            elsif backend=WinAPI then
                -- menu items only...
                atom state = c_func(xGetMenuState,{mh,id,MF_BYCOMMAND})
                return state!=#FFFFFFFF and and_bits(state,MF_CHECKED)
            else
                ?9/0 -- (unknown backend)
            end if
        end if
    else -- not bMapped
        integer k = find(id,def_menu_attr[menu][1])
        if k then
            integer l = find(name,vslice(def_menu_attr[menu][2][k],1))
            if l then return def_menu_attr[menu][2][k][l][2] end if
        end if
        return NULL
    end if
    crash("gMenuGetAttribute(%s) not supported",{name})
end function

local function xpg_menu_common(integer pid, integer id, bool bSelected)
    -- common code for GTK clicked and selected events, and WinAPI
    --assert(id=xpg_getID(handle)) -- NO!!
    assert(pid!=NULL)
    if backend=WinAPI and not bSelected
    and getd_index({pid,id},WIN_MENU_CHECKED)!=NULL then
        bool checked = gMenuGetAttribute(pid,id,"CHECKED")
        gMenuSetAttribute(pid,id,"CHECKED",not checked)
    end if
    integer handler = gGetHandler(pid,"HANDLER")
    assert(handler!=NULL)
    sequence sig = get_routine_info(handler,false)
    integer res = XPG_DEFAULT -- (anything but XPG_CLOSE)
    if sig={3,3,"FOII"} then
        res = handler(pid,id,bSelected)
--DEV JavaScript as a typeless language would struggle to distinguish these two... and as per "FI" below.
--  elsif sig={2,2,"FII"} then
--      res = handler(id,bSelected)
    elsif sig={2,2,"FOI"} then
        if not bSelected then
            res = handler(pid,id)
        end if
--This was never a good idea anyway, tbh. (If I got an ex.err with id=7 in it, I'd rather like to know which menu it actually came from...)
--  elsif sig={1,1,"FI"} then
--      if not bSelected then
--          res = handler(id)
--      end if
    else
        ?9/0 -- unknown sig...
    end if
    if res=XPG_CLOSE then
        integer p = gGetDialog(pid)
        if p then gHide(p) end if
        return true
    end if
    return 0 -- (ignored?)
end function

local function xpg_button_clicked(gdx id) -- (WinAPI and GTK)
    integer ct = ctrl_types[id]
    assert(ct=BUTTON)
    integer action = gGetHandler(id,"ACTION")
    if action then
        integer res = action(id)
--      if res=XPG_IGNORE then
--                  return 0
        if res=XPG_CLOSE then
            gHide(gGetDialog(id))
--          return 0
        end if
    end if
    return false
end function

local function xpg_gtk_button_clicked(atom handle, gdx id) -- (GTK only)
--?{"clicked",id}
    assert(id=xpg_getID(handle))
--temp:
--gdx parent = xpg_get_parent_id(id)
--integer {w,h} = gGetTextExtent(id,{"port from pGUI"})
--?{"click size",w,h}
    return xpg_button_clicked(id)
--  integer action = gGetHandler(id,"ACTION")
--  if action then
--      integer res = action(id)
--      if res=XPG_CLOSE then
--          gHide(gGetDialog(id))
--      end if
--  end if
--  return false
end function


local integer assume_id -- for betwixt xCreateWindowEx() and xpg_setID()... (windows only)
local atom assumed_hwnd -- ""
-- In other words, xpg_WinAPI_WndProc is likely to get WM_CREATE etc 
--  messages, before it has managed to properly log the new control.

local function xpg_WinAPI_SubProc(atom hwnd, msg, wParam, lParam)
--
-- Subclassing lets us hook into the process after Windows has preprocessed
-- the messages a little bit, making them a bit easier to understand. Quite
-- what Windows has/does to them is, naturally, all rather somewhat vague.
--
    integer id = xpg_getID(hwnd),
            ct = ctrl_types[id]
--if ct=BUTTON then
--  ?{id,msg}
--end if
--?{"xpg_WinAPI_SubProc",id,win_msg(msg),ctrl_names[ct]}

    if msg=WM_SETFOCUS then
        --
        -- save the item being focussed on in the parent window
        -- (quite why this don't work in WndProc is beyond me)
        --
        integer pid = gGetDialog(id)
--      ctrl_xtra[pid] = id
        ctrl_xtra[pid][CX_LAST_FOCUS] = id

    elsif msg=WM_CHAR
       or msg=WM_KEYDOWN
       or msg=WM_SYSCHAR
       or msg=WM_SYSKEYDOWN then
        --
        -- Windows will have done quite a bit more to these messages
        --  for us (erm, TranslateMessage?) by the time it gets here,
        --  that is, compared to the messages that WndProc might get.
        --  Frinst, '1'|'!' & 'a'|'A'|Shift|CapsLock make sense here.
        --
        if xpg_process_key(id,msg,wParam,lParam)=0 then return 0 end if
    --
    -- Everything else could easily go in xpg_WinAPI_WndProc(), but just as 
    -- long as we're not replicating code, we may as well leave it in here.
    --
    elsif msg=BM_SETCHECK and ct=CHECKBOX then
        integer value_changed = gGetHandler(id,"VALUE_CHANGED")
        if value_changed then
            if (not and_bits(ctrl_flags[id],CF_RADIO))
            or wParam=BST_CHECKED then
                value_changed(id,wParam=BST_CHECKED)
            end if
        end if

    elsif msg=WM_COMMAND then
        if ct=DROPDOWN then
            if floor(wParam/#10000)=LBN_SELCHANGE then
--?{"DROPDOWN LBN_SELCHANGE",id,lParam,xpg_getID(lParam)}
                integer changed = gGetHandler(id,"CHANGED")
                if changed then
                    changed(id)
                end if
            end if

--      elsif ct=BUTTON then
            -- buttons which are indirect children of the dialog, being
            -- those in a gFrame/gTabs, as opposed to gVbox/gHbox-only.
--          return xpg_button_clicked(id)
else
integer oid = id
        if lParam then -- (see win00inglod.exw for more...)
            id = xpg_getID(lParam)
            if id then
                ct = ctrl_types[id]
                if ct=BUTTON then
                    return xpg_button_clicked(id)
                end if
            end if
        end if
    printf(1,"SubProc WM_COMMAND(%s): wParam:%08x, lParam:%08x, oid:%d, id:%d\n",{ctrl_names[ct],wParam,lParam,oid,id})
        end if
--elsif msg=WM_MENUCOMMAND then
--  printf(1,"SubProc WM_MENUCOMMAND(%s,%d): wParam:%08x, lParam:%08x\n",{ctrl_names[ct],id,wParam,lParam})
    end if
    return c_func(xCallWindowProc,{wnd_proc_addr[id],hwnd,msg,wParam,lParam})
end function
local constant WINAPI_SUBPROC_CB = call_back(xpg_WinAPI_SubProc)

--local procedure xpg_WinAPI_sub_class_control(gdx id, xpg_handle hwnd)
--  integer ct = ctrl_types[id]
--  -- certain control types do not get subclassed:
----    if ct!=StaticBitmap and ct!=DIALOG 
----    and ct!=ReBarBand then--and ct!=HyperText then
--  if ct!=DIALOG then
----    if ct!=DIALOG and ct!=FRAME and ct!=BUTTON then
----    if false then
----?{"subclassing!",id}
--      assert(sub_proc_addr[id]==UNDEFINED)
--      atom prev_wnd_proc = c_func(xSetWindowLong,{hwnd,GWL_WNDPROC,WINAPI_SUBPROC_CB})
--      wnd_proc_addr[id] = prev_wnd_proc
--      sub_proc_addr[id] = WINAPI_SUBPROC_CB
--  end if
--end procedure

--local procedure xpg_WinAPI_unsub_class_control(integer id)
--  if sub_proc_addr[id]!=UNDEFINED then
--      atom PrevWndProc = wnd_proc_addr[id]
--      {} = c_func(xSetWindowLong,{ctrl_handles[id],GWL_WNDPROC,PrevWndProc})
--      sub_proc_addr[id] = UNDEFINED
--  end if
--end procedure

local integer tracked_menu = 0

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
        -- ... and these all seemed fair enough for that
--      if not find(msg,{WM_CREATE,WM_MOVE,WM_SIZE,WM_SHOWWINDOW,
--                    WM_GETMINMAXINFO,WM_NCCREATE,WM_NCCALCSIZE}) then
--          ?{"bAssumed","msg",msg,"id",id}
--      end if
    end if
    integer ct = ctrl_types[id]
--/*
--if id!=2 then
--if id=1 then
sequence r = win_msg(msg)
if not ignorable_message(r,msg) then
    ?{"xpg_WinAPI_WndProc",id,msg,r}
end if
--*/

--      case WM_ACTIVATE:
--          if bAssumed then return 0 end if
--          return c_func(xDefWindowProc,{hwnd,iMsg,wParam,lParam})

--/!*
--  if msg=WM_INITDIALOG then
--  if msg=WM_CREATE then
------      ?{"WM_INITDIALOG",xpg_get_window_rect(id),xpg_get_client_rect(id)}
--if bAssumed then
--  assert(ctrl_handles[id]==NULL)
--  ctrl_handles[id] = hwnd
--end if
--      ?{"WM_CREATE",xpg_get_window_rect(id),xpg_get_client_rect(id)}
--  end if
--/*
SetWindowPos(hWnd,  HWND_TOP,  100,  100,  300,  70,  SWP_SHOWWINDOW);
   RECT rcClient, rcWind;
   GetClientRect(hWnd, &rcClient);
   GetWindowRect(hWnd, &rcWind);
   int xdiff =  (rcWind.right-rcWind.left)-(rcClient.right-rcClient.left);//16
   int ydiff =  (rcWind.bottom-rcWind.top)-(rcClient.bottom-rcClient.top);//39
   int cxSizeFrame = GetSystemMetrics(SM_CXSIZEFRAME);
   int cySizeFrame = GetSystemMetrics(SM_CXSIZEFRAME);
   int cyCaption = GetSystemMetrics(SM_CYCAPTION);
   int cyBorder = GetSystemMetrics(SM_CYBORDER);.
--*/
    if msg=WM_COMMAND then
--/*
--subproc:
        -- interpret parameters..
        temp = 0 -- default
        if lParam=0 then -- menu or accel key
            id = and_bits(wParam,#FFFF)
            if id=wParam then -- menu for sure
                --temp = 0
            else -- accelerator key
                temp = 1
            end if
        else -- control such as a Button
            id = getID(lParam)
            --temp = 0
        end if
--wndproc:
    -- intercept & redirect the broad WM_COMMAND msg
    elsif msg=WM_COMMAND then

        -- interpret parameters..
        if lParam=0 then -- menu or accel key
            id = and_bits(wParam,#FFFF)
            if id=wParam then -- menu for sure
                --lParam = 0
            else -- accelerator key
                lParam = 1
            end if
        else -- control such as a Button
            id = getID(lParam)
--          lParam = 0
            lParam = wParam
        end if

        -- call handler if available
        if id then
            iHandler = HandlerRoutine[id]
            if iHandler=UNDEFINED and ObjectParent[id] then
                iHandler = HandlerRoutine[ObjectParent[id]]
            end if
            if iHandler!=UNDEFINED then
                return_value = call_func(iHandler,{id,msg,floor(wParam/#10000),lParam})
            end if
        end if
        return 0

--*/
--integer oid = id
        if lParam then -- (see win00inglod.exw for more...)
            id = xpg_getID(lParam)
        end if
        if id then
            ct = ctrl_types[id]
            if ct=BUTTON then
                -- buttons which are implicitly direct children of the dialog,
                -- because the gVbox() and gHbox() are "virtual" under WinAPI.
                return xpg_button_clicked(id)
--?"wndclick"
--              integer action = gGetHandler(id,"ACTION")
--              if action then
--                  integer res = action(id)
--                  if res=XPG_IGNORE then
--                      return 0
--                  elsif res=XPG_CLOSE then
--                      gHide(gGetDialog(id))
--                      return 0
--                  end if
--              end if
            --
            -- It would make no odds to do these here rather than in SubProc. [FLW...]
            --
--          elsif ct=CHECKBOX then
--              integer value_changed = gGetHandler(id,"VALUE_CHANGED")
--              if value_changed then
--                  bool bState = c_func(xSendMessage,{lParam,BM_GETCHECK,0,0})
--                  value_changed(id,bState)
--              end if
--          elsif ct=DROPDOWN and floor(wParam/#10000)=LBN_SELCHANGE then
----printf(1,"DROPDOWN di:%d, wParam:%08x, lParam:%08x\n",{id,wParam,lParam})
--              integer changed = gGetHandler(id,"CHANGED")
--              if changed then
--                  changed(id)
--              end if
            elsif ct=DIALOG then
                -- a menu command
--  printf(1,"WndProc WM_COMMAND(%s,%d): id:%d, wParam:%08x, lParam:%08x\n",{ctrl_names[ct],oid,id,wParam,lParam})
                id = children_ids[id][1]
                assert(ctrl_types[id]=BOX)
                id = children_ids[id][1]
                assert(ctrl_types[id]=MENU)
                return xpg_menu_common(id,wParam,false)
--local sequence ctrl_handles = {}, -- (native handles)
--               ctrl_types = {},   -- (also ctrl_free_list)
--               ctrl_flags = {},   -- (CF_XXX settings, see above)
--               parent_ids = {},   -- (as gdx, nb see xpg_get_parent_handle)
--             children_ids = {},   -- (as gdx) (also cairo context?)
--
--  printf(1,"WndProc WM_COMMAND(%s): id:%d, wParam:%08x, lParam:%08x\n",{ctrl_names[ct],id,wParam,lParam})
            end if
        end if
--      if ct=DROPDOWN and floor(wParam/#10000)=LBN_SELCHANGE then

    elsif msg=WM_MENUSELECT then
        integer flags = floor(wParam/#10000),
                  mid = and_bits(wParam,#FFFF)
        if and_bits(flags,MF_POPUP) then
--          mid = c_func(xGetMenuItemID,{lParam,mid})
            -- since GetMenuItemID obstinately returns -1 for submenus:
            set_struct_field(idMENUITEMINFO,pMENUITEMINFO,"fMask",MIIM_ID)
            bool bOK = c_func(xGetMenuItemInfo,{lParam,mid,true,pMENUITEMINFO})
            if bOK then
                atom mh = get_struct_field(idMENUITEMINFO,pMENUITEMINFO,"wID")
                -- at least we can get a submenu handle, but we have to convert
                -- that into a menu identifier ourselves (I can half see why..)
                mid = getd(mh,WINAPI_SUBMENUS)
            else
                mid = 0
            end if
        end if
        if mid>0 then
            if tracked_menu then
--              return xpg_menu_common(tracked_menu,mid,true)
                id = tracked_menu
            else
                id = children_ids[id][1]
                assert(ctrl_types[id]=BOX)
                id = children_ids[id][1]
                assert(ctrl_types[id]=MENU)
--              printf(1,"WndProc WM_MENUSELECT(%s,%d): wParam:%08x, lParam:%08x\n",{ctrl_names[ct],id,wParam,lParam})
            end if
            return xpg_menu_common(id,mid,true)
        end if
--elsif msg=WM_MENUCOMMAND then
--  printf(1,"WndProc WM_MENUCOMMAND(%s,%d): wParam:%08x, lParam:%08x\n",{ctrl_names[ct],id,wParam,lParam})

--  elsif msg=BM_SETCHECK and ct=CHECKBOX then
--      integer value_changed = gGetHandler(id,"VALUE_CHANGED")
--      if value_changed then
--          value_changed(id,wParam=BST_CHECKED)
--      end if

--*!/
    elsif msg=WM_CHAR
       or msg=WM_KEYDOWN
       or msg=WM_SYSCHAR
       or msg=WM_SYSKEYDOWN then

        -- Note that the same call is made from SubProc, and we don't even
        -- get these messages here for several child elements of a dialog..
        -- Also, you're on your own with '1'|'!' & 'a'|'A'|Shift|CapsLock,
        --  that is at least as I understand/believe these things to be.

        if xpg_process_key(id,msg,wParam,lParam)=0 then return 0 end if

    elsif msg>=WM_LBUTTONDOWN
      and msg<=WM_MBUTTONDBLCLK then
        integer btn = floor((msg-510)/3),
             button = "LRM"[btn],
                rb3 = remainder(msg-513,3)+1,
            pressed = "SRD"[rb3],
                  x = and_bits(lParam,#FFFF),
                  y = floor(lParam/#10000)
        bool ctrl = and_bits(c_func(xGetKeyState,{VK_CTRL}),#8000)!=0,
            shift = and_bits(c_func(xGetKeyState,{VK_SHIFT}),#8000)!=0,
              alt = and_bits(c_func(xGetKeyState,{VK_ALT}),#8000)!=0
        sequence status = {button,pressed,ctrl,shift,alt}
        return xpg_click_handler(id,status,x,y)

    elsif msg=WM_MOUSEMOVE then
        integer x = and_bits(lParam,#FFFF),
                y = floor(lParam/#10000)
        bool left = and_bits(wParam,MK_LBUTTON)!=0,
           middle = and_bits(wParam,MK_MBUTTON)!=0,
            right = and_bits(wParam,MK_RBUTTON)!=0,
             ctrl = and_bits(wParam,MK_CONTROL)!=0,
            shift = and_bits(wParam,MK_SHIFT)!=0,
              alt = and_bits(c_func(xGetKeyState,{VK_ALT}),#8000)!=0
        xpg_mousemove_handler(id,x,y,left,middle,right,ctrl,shift,alt)

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
                    integer branchopen = gGetHandler(id,"BRANCHOPEN")
                    if branchopen then
                        -- aside: the argument to branchopen() is whatever the Windows branches of 
                        --  gTreeGetUserId()/gTreeAddNodes(), as defined in this very source file, 
                        --  actually need. (backend-specific)
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
--          gSetFocus(TREE1)
        else
            void = sendMessage(TREE1,TVM_EXPAND,TVE_TOGGLE,tree_items[treeIdx][tHandle])
        end if
        return {0}
--*/

    elsif msg=WM_PAINT and ct=CANVAS then
        atom hdc = c_func(xBeginPaint,{hwnd,pPAINTSTRUCT})
        ctrl_xtra[id][CX_CANVAS_HDC] = hdc
        -- (R2_MASKPEN is the only one worth having, in my tests)
        integer prev = c_func(xSetROP2,{hdc,R2_MASKPEN})
        assert(prev!=0) -- shd be R2_COPYPEN(13) or R2_MASKPEN(9)
        integer redraw = gGetHandler(id,"REDRAW")
        if redraw then
            atom back = gCanvasGetBackground(id)
            redraw(id)
            gCanvasSetBackground(id,back)
        end if
        c_proc(xEndPaint,{hwnd,pPAINTSTRUCT})
        return 1

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

    elsif msg=WM_ERASEBKGND and ct=CANVAS then
--?"WM_ERASEBKGND"
--DEV removing this made no difference... (it /is/ called, tho)
--/!*
        integer r = c_func(xGetClientRect,{hwnd,pRECT})
        assert(r!=0)
        atom hdc = wParam,
             back = gCanvasGetBackground(id),
             hBrush = xpg_WinAPI_get_brush(back)
        r = c_func(xFillRect,{hdc,pRECT,hBrush})
        assert(r!=0)
--*!/
        return 1 -- (signal done)

    elsif msg=WM_SIZE then
--no help:
--  elsif msg=WM_SIZE
--     or msg=WM_SIZING then
--/*
        --DEV if expand... and all children, obvs.
        object cid = children_ids[id]
        if sequence(cid) and length(cid) then
            integer {x,y,w,h} = xpg_get_client_rect(id),
                         tree = cid[1]
            if ctrl_types[tree]=TREEVIEW then
                c_proc(xMoveWindow,{ctrl_handles[tree],x+10,y+10,w-20,h-20,1})
            end if
        end if
--*/
--      integer ct = ctrl_types[id] 
        if ct=DIALOG then
            integer {w,h} = xpg_get_client_rect(id)[3..4]
?{"WM_SIZE",id,w,h}
            xpg_WinAPI_resize(id,w,h)
        end if
--/*
        integer w = and_bits(lParam,#FFFF),
                h = floor(lParam/#10000)
?{"WM_SIZE",id,w,h}
--if nMapDepth=0 and id=2 and w=203 then ?9/0 end if
        xpg_WinAPI_resize(id,w,h)
--*/
--DEV
--      xpg_lm_distribute_any_slack(id)

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
        c_proc(xPostQuitMessage,{0})
--DEVto go:
--      -- unsubclass all subclassed controls
--      for obj_id=1 to length(ctrl_types) do
--          xpg_WinAPI_unsub_class_control(obj_id)
--      end for
        return  0

    elsif msg=WM_SETFOCUS then
        -- focus on the saved item rather than the window
        -- (for task switching, eg Alt-Tab)
--      if ctrl_types[id]=DIALOG then
        if ct=DIALOG then
--maybe:
--          integer last_focus = ctrl_xtra[id]
            integer last_focus = ctrl_xtra[id][CX_LAST_FOCUS]
            if last_focus!=UNDEFINED
            and ctrl_types[last_focus]!=DIALOG then
                c_proc(xSetFocus,{ctrl_handles[last_focus]})
                return 0
            end if
-- No help...
--      else
--          -- save the item being focussed on in the parent window
--          integer pid = gGetDialog(id)
--          ctrl_xtra[pid] = id
--          ctrl_xtra[pid][CX_LAST_FOCUS] = id
        end if
    elsif msg=WM_HSCROLL
       or msg=WM_VSCROLL then
--      if ctrl_types[id]=SLIDER and lParam then
        if lParam then
            -- STAND ALONE CONTROLS - Scroll bar OR trackbar controls OR TabControl arrow buttons
                -- get the id of the control
            integer sid = xpg_getID(lParam)
            if ctrl_types[sid]=SLIDER then
                wParam = and_bits(wParam,#FFFF)
                integer value_changed = gGetHandler(sid,"VALUE_CHANGED")
                if value_changed
                and find(wParam,{TB_LINEUP,TB_LINEDOWN,TB_TOP,TB_BOTTOM,
                                 TB_PAGEUP,TB_PAGEDOWN,TB_THUMBTRACK}) then
--                  atom pos = c_func(xSendMessage,{lParam,TBM_GETPOS,0,0})
                    value_changed(sid)
                end if
--?{id,sid,sprintf("%x",wParam),"..."}
--/*
                -- do default processing for TAB arrow buttons the exit, do not trap for user
                if id=0 then -- must be tab control arrow button
                    return c_func(xCallWindowProc,{PrevWndProc,hwnd,msg,wParam,lParam})
                end if
                -- process scrollbar or trackbar messages (say, within TabControls)
                temp = proc_ScrollMessage(id,SB_CTL,wParam)
                -- call user handler if scroll processing was valid
                iHandler = HandlerRoutine[id]
                if atom(temp) and iHandler!=UNDEFINED then
                    void = call_func(iHandler,{id,msg,temp,0})
                end if
--*/
            end if
        end if      
    end if
    return c_func(xDefWindowProc,{hwnd,msg,wParam,lParam})
end function

local integer xpg_create_image_list, -- see xpg_xpm.e
              xpg_winAPI_create_DIB_from_xpm -- "" 
--            xpg_winAPI_size                   -- ""
local atom closed_folder, open_folder, dot  -- for GTK
local atom tree_himl                        -- for WinAPI

local function xpg_xpm_callback(object xpm)
    -- low-level operations for xpg_xpm.e (avoids making anything global)
    if backend=GTK then
        return c_func(gdk_pixbuf_new_from_xpm_data,{xpg_word_array(xpm)})
    elsif backend=WinAPI then
        string what = xpm[1]
        if what="NEWLIST" then
            -- create imagelist using the recommended sizes for small icons:
            return c_func(xImageList_Create,{c_func(xGetSystemMetrics,{SM_CXSMICON}),
                                             c_func(xGetSystemMetrics,{SM_CYSMICON}),
                                             ILC_COLOR8+ILC_MASK,1,32})
        elsif what="NEWDIB" then    
            atom {mem,hdrSize} = xpm[2],
                 hdc = c_func(xGetDC,{0}),      -- Get the screen's device context.
                hDIB = c_func(xCreateDIBitmap, {hdc,                -- create DDB for/compatible with this
                                                mem,                -- info about the DIB to create, eg h*w
                                                CBM_INIT,           -- initialise it please with...
                                                mem+hdrSize,        --      this bitmap,
                                                mem,                --      which has this structure
                                                DIB_RGB_COLORS})    --      and has explicit RGB values
            assert(hDIB!=NULL)
            bool bOK = c_func(xReleaseDC,{0,hdc})
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
    else
        ?9/0 -- (unknown backend)
    end if
end function

local procedure xpg_Init()
    bInit = true
    handlers = new_dict()   -- key is {gdx,name}, data is integer routine_id
    handler_sigs = new_dict()   -- key is {ct,name}, data is sig
    bool L = platform()=LINUX,
         bWinders = platform()=WINDOWS
    integer MB = machine_bits()
    if backend=GTK then
--hmm: just pop libcairo-2.dll, libpng13.dll and zlib1.dll into your working directory...
        string gdk = iff(L?"libgdk-3.so.0"
                          :"libgdk-3-0.dll"), -- (win32bit replaced below)
               gtk = iff(L?"libgtk-3.so.0"
                          :"libgtk-3-0.dll"), -- (win32bit replaced below)
               gto = iff(L?"libgobject-2.0.so.0"
                          :"libgobject-2.0-0.dll"),
               gtp = iff(L?"libgdk_pixbuf-2.0.so.0"
                          :"libgdk_pixbuf-2.0-0.dll"),
               glb = iff(L?"libglib-2.0.so.0"
                          :"libglib-2.0-0.dll"),
               pan = iff(L?"libpango-1.0.so.0"
                          :"libpango-1.0-0.dll"),
               pnc = iff(L?"libpangocairo-1.0.so.0"
                          :"libpangocairo-1.0-0.dll"),
               car = iff(L?"libcairo.so.2"
                          :"libcairo-2.dll"),
        initial_dir = current_dir()
        if bWinders then
            if MB=32 then
                gdk = "libgdk-win32-2.0-0.dll"
                gtk = "libgtk-win32-2.0-0.dll"
            end if
            -- NB not formally supported [aka gUseGTK() *has been* invoked].
            string dll_dir = sprintf(`%s\win_gtk%d`,{include_path("xpGUI"),MB})
            assert(chdir(dll_dir),"gUseGTK()???")   
        end if
        atom GTKLIB = open_dll(gtk),
             GDKLIB = open_dll(gdk),
             GTKGDO = open_dll(gto),
             GDKGTP = open_dll(gtp),
             LBGLIB = open_dll(glb),
             PANGO  = open_dll(pan),
             PANCAR = open_dll(pnc),
             CAIRO  = open_dll(car)
        if bWinders then
            assert(chdir(initial_dir))
        end if
        integer gtk_init_check = define_c_func(GTKLIB,"gtk_init_check",
            {C_PTR,     --  int* argc
             C_PTR},    --  char*** argv
            C_INT)      -- gboolean
        if c_func(gtk_init_check,{0,0})=0 then  
            crash("Failed to initialize GTK library!")  
        end if
        gtk_check_version = define_c_func(GTKLIB,"gtk_check_version",
            {C_INT,     --  guint required_major
             C_INT,     --  guint required_minor
             C_INT},    --  guint required_micro
            C_PTR)      -- const gchar * (null means OK)
        bGTK3 = (c_func(gtk_check_version,{3,0,0})=null)
--printf(1,"GTK3: %t\n",bGTK3)
        gtk_version = {"gtk_major_version","gtk_minor_version","gtk_micro_version"}
        for i,v in gtk_version do
            if bGTK3 then
                gtk_version[i] = c_func(define_c_func(GTKLIB,"gtk_get"&v[4..$],{},C_INT),{})
            else
                gtk_version[i] = peek4s(define_c_var(GTKLIB,v))
            end if
        end for
--      ?gtk_version
        cairo_arc = define_c_proc(CAIRO,"cairo_arc",
            {C_PTR,     --  cairo_t* cr
             C_DBL,     --  double xc
             C_DBL,     --  double yc
             C_DBL,     --  double radius
             C_DBL,     --  double angle1
             C_DBL})    --  double angle2
        cairo_close_path = define_c_proc(CAIRO,"cairo_close_path",
            {C_PTR})    --  cairo_t* cr
        cairo_curve_to = define_c_proc(CAIRO,"cairo_curve_to",
            {C_PTR,     --  cairo_t* cr
             C_DBL,     --  double x1
             C_DBL,     --  double y1
             C_DBL,     --  double x2
             C_DBL,     --  double y2
             C_DBL,     --  double x3
             C_DBL})    --  double y3
        cairo_destroy = define_c_proc(CAIRO,"cairo_destroy",
            {C_PTR})    --  cairo_t* cr
        cairo_fill = define_c_proc(CAIRO,"cairo_fill",
            {C_PTR})    --  cairo_t* cr
--      cairo_get_source = define_c_func(CAIRO,"cairo_get_source",
--          {C_PTR},    --  cairo_t *cr
--          C_PTR)      -- cairo_pattern_t *
        cairo_line_to  = define_c_proc(CAIRO,"cairo_line_to",
            {C_PTR,     --  cairo_t* cr
             C_DBL,     --  double x
             C_DBL})    --  double y
        cairo_move_to  = define_c_proc(CAIRO,"cairo_move_to",
            {C_PTR,     --  cairo_t* cr
             C_DBL,     --  double x
             C_DBL})    --  double y
        cairo_new_path = define_c_proc(CAIRO,"cairo_new_path",
            {C_PTR})    --  cairo_t* cr
        cairo_paint = define_c_proc(CAIRO,"cairo_paint",
            {C_PTR})    --  cairo_t* cr
        cairo_rectangle = define_c_proc(CAIRO,"cairo_rectangle",
            {C_PTR,     --  cairo_t* cr
             C_DBL,     --  double x
             C_DBL,     --  double y
             C_DBL,     --  double width
             C_DBL})    --  double height
        cairo_restore = define_c_proc(CAIRO,"cairo_restore",
            {C_PTR})    --  cairo_t* cr
        cairo_rotate = define_c_proc(CAIRO,"cairo_rotate",
            {C_PTR,     --  cairo_t* cr
             C_DBL})    --  double angle
        cairo_save = define_c_proc(CAIRO,"cairo_save",
            {C_PTR})    --  cairo_t* cr
        cairo_scale = define_c_proc(CAIRO,"cairo_scale",
            {C_PTR,     --  cairo_t* cr
             C_DBL,     --  double sx
             C_DBL})    --  double sy
        cairo_set_dash = define_c_proc(CAIRO,"cairo_set_dash",
            {C_PTR,     --  cairo_t* cr
             C_PTR,     --  const double *dashes
             C_INT,     --  int num_dashes
             C_DBL})    --  double offset (always 0 here)
        cairo_set_line_width = define_c_proc(CAIRO,"cairo_set_line_width",
            {C_PTR,     --  cairo_t* cr
             C_DBL})    --  double width
--      cairo_set_source = define_c_proc(CAIRO,"cairo_set_source",
--          {C_PTR,     --  cairo_t* cr
--           C_PTR})    --  cairo_pattern_t * sourse
        cairo_set_source_rgb = define_c_proc(CAIRO,"cairo_set_source_rgb",
            {C_PTR,     --  cairo_t* cr
             C_DBL,     --  double red
             C_DBL,     --  double green
             C_DBL})    --  double blue
        cairo_stroke = define_c_proc(CAIRO,"cairo_stroke",
            {C_PTR})    --  cairo_t* cr
        cairo_translate = define_c_proc(CAIRO,"cairo_translate",
            {C_PTR,     --  cairo_t* cr
             C_DBL,     --  double tx
             C_DBL})    --  double ty
--      gdk_atom_intern = define_c_func(GDKLIB,"gdk_atom_intern",
--          {C_PTR,     --  const gchar* atom_name
--           C_INT},    --  gboolean only_if_exists
--          C_PTR)      -- GdkAtom
        gdk_cairo_create = define_c_func(GDKLIB,"gdk_cairo_create",
            {C_PTR},    --  GdkDrawable *drawable
            C_PTR)      -- cairo_t *
--      gdk_cairo_rectangle = define_c_proc(GDKLIB,"gdk_cairo_rectangle",
--          {C_PTR,     --  cairo_t* cr
--           C_PTR})    --  const GdkRectangle* rectangle
        gdk_cairo_set_source_pixbuf = define_c_proc(GDKLIB,"gdk_cairo_set_source_pixbuf",
            {C_PTR,     --  cairo_t* cr
             C_PTR,     --  const GdkPixbuf *pixbuf
             C_DBL,     --  double pixbuf_x
             C_DBL})    --  double pixbuf_y
        gdk_display_get_default = define_c_func(GDKLIB,"gdk_display_get_default",
            {},         --  void
            C_PTR)      -- GdkDisplay*
        gdk_get_default_root_window = define_c_func(GDKLIB,"gdk_get_default_root_window",
            {},         --  void
            C_PTR)      -- GdkWindow*
    if not bGTK3 then -- (pre-gtk 3.0.0)
        gdk_screen_get_width = define_c_func(GDKLIB,"gdk_screen_get_width",
            {},         --  void
            C_INT)      -- gint
        gdk_screen_get_height = define_c_func(GDKLIB,"gdk_screen_get_height",
            {},         --  void
            C_INT)      -- gint
    else
        gdk_display_get_monitor = define_c_func(GDKLIB,"gdk_display_get_monitor",
            {C_PTR,     --  GdkDisplay*
             C_INT},    --  int monitor_num
            C_PTR)      -- GdkMonitor*
        gdk_monitor_get_geometry = define_c_proc(GDKLIB,"gdk_monitor_get_geometry",
            {C_PTR,     --  GdkMonitor* monitor
             C_PTR})    --  GdkRectangle* geometry
    end if
        gdk_display_get_pointer = define_c_proc(GDKLIB,"gdk_display_get_pointer",
            {C_PTR,     --  GdkDisplay *display
             C_PTR,     --  GdkScreen **screen (or NULL)
             C_PTR,     --  gint *x
             C_PTR,     --  gint *y
             C_PTR})    --  GdkModifierType *mask
--      gdk_pixbuf_get_from_surface = define_c_func(GDKLIB,"gdk_pixbuf_get_from_surface",
--          {C_PTR,     --  cairo_surface_t *surface
--           C_INT,     --  gint src_x
--           C_INT,     --  gint src_y
--           C_INT,     --  gint width
--           C_INT},    --  gint height
--          C_PTR)      -- GdkPixbuf*
--??X11 only...
--  if bGTK3 then
--      gdk_pixbuf_get_from_window = define_c_func(GDKLIB,"gdk_pixbuf_get_from_window",
--          {C_PTR,     --  GdkWindow *window
--           C_INT,     --  gint src_x
--           C_INT,     --  gint src_y
--           C_INT,     --  gint width
--           C_INT},    --  gint height
--          C_PTR)      -- GdkPixbuf*
--  else
--      gdk_pixbuf_get_from_drawable = define_c_func(GDKLIB,"gdk_pixbuf_get_from_drawable",
--          {C_PTR,     --  GdkPixbuf *dest -- (NULL here, create new)
--           C_PTR,     --  GdkDrawable *src
--           C_PTR,     --  GdkColormap *cmap -- (NULL here)
--           C_INT,     --  int src_x
--           C_INT,     --  int src_y
--           C_INT,     --  int dest_x
--           C_INT,     --  int dest_y
--           C_INT,     --  int width
--           C_INT},    --  int height
--          C_PTR)      -- GdkPixbuf*
--  end if
        gdk_pixbuf_get_pixels_ = define_c_func(GDKGTP,"gdk_pixbuf_get_pixels",
            {C_PTR},    --  const GdkPixbuf* pixbuf
            C_PTR)      -- guchar*
        gdk_pixbuf_get_type = define_c_func(GDKGTP,"gdk_pixbuf_get_type",
            {},         --  void
            C_INT)      -- GType
        gdk_pixbuf_new_from_data = define_c_func(GDKGTP,"gdk_pixbuf_new_from_data",
            {C_PTR,     --  const guchar *data
             C_INT,     --  GdkColorspace colorspace
             C_INT,     --  gboolean has_alpha
             C_INT,     --  int bits_per_sample
             C_INT,     --  int width
             C_INT,     --  int height
             C_INT,     --  int rowstride
             C_PTR,     --  GdkPixbufDestroyNotify destroy_fn
             C_PTR},    --  gpointer destroy_fn_data
            C_PTR)      -- GdkPixbuf*
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
--      gdk_window_focus = define_c_proc(GDKLIB,"gdk_window_focus",
--          {C_PTR,     --  GdkWindow *window
--           C_INT})    --  guint32 timestamp
--      gdk_window_get_geometry = define_c_proc(GDKLIB,"gdk_window_get_geometry",
--          {C_PTR,     --  GdkWindow* window
--           C_PTR,     --  gint* x
--           C_PTR,     --  gint* y
--           C_PTR,     --  gint* width
--           C_PTR,     --  gint* height
--           C_PTR})    --  gint* depth
        gdk_window_get_height = define_c_func(GDKLIB,"gdk_window_get_height",
            {C_PTR},    --  GdkWindow *window
            C_INT)      -- int
        gdk_window_get_width = define_c_func(GDKLIB,"gdk_window_get_width",
            {C_PTR},    --  GdkWindow *window
            C_INT)      -- int
        gdk_window_get_origin = define_c_proc(GDKLIB,"gdk_window_get_origin",
            {C_PTR,     --  GdkWindow *window
             C_PTR,     --  gint* x
             C_PTR})    --  gint* y
        gdk_window_get_root_origin = define_c_proc(GDKLIB,"gdk_window_get_root_origin",
            {C_PTR,     --  GdkWindow *window
             C_PTR,     --  gint* x
             C_PTR})    --  gint* y
--      gdk_window_get_position = define_c_proc(GDKLIB,"gdk_window_get_position",
--          {C_PTR,     --  GdkWindow *window
--           C_PTR,     --  gint *x
--           C_PTR})    --  gint *y
        gdk_window_invalidate_rect = define_c_proc(GDKLIB,"gdk_window_invalidate_rect",
            {C_PTR,     --  GdkWindow *window
             C_PTR,     --  const GdkRectangle *rect (NULL here)
             C_INT})    --  gboolean invalidate_children
--      gdk_window_move_resize = define_c_proc(GDKLIB,"gdk_window_move_resize",
--          {C_PTR,     --  GdkWindow* window
--           C_INT,     --  gint x
--           C_INT,     --  gint y
--           C_INT,     --  gint width
--           C_INT})    --  gint height
         gdk_window_process_updates = define_c_proc(GDKLIB,"gdk_window_process_updates",
            {C_PTR,     --  GdkWindow* window
             C_INT})    --  gboolean update_children
        gtk_adjustment_new = define_c_func(GTKLIB,"gtk_adjustment_new",
            {C_DBL,     --  gdouble value
             C_DBL,     --  gdouble lower
             C_DBL,     --  gdouble upper
             C_DBL,     --  gdouble step_increment
             C_DBL,     --  gdouble page_increment
             C_DBL},    --  gdouble page_size
            C_PTR)      -- GtkObject*
--gtk_alignment_new (then gtk_container_add)
--/*
--  GtkWidget* gtk_alignment_new(gfloat xalign,
--                               gfloat yalign,
--                               gfloat xscale,
--                               gfloat yscale);
--
--  GList*
--  gtk_container_get_children (
--    GtkContainer* container
--  )
--  void
--  gtk_container_set_border_width (
--    GtkContainer* container,
--    guint border_width
--  )
--
--  void
--  gtk_alignment_set_padding (
--    GtkAlignment* alignment,
--    guint padding_top,
--    guint padding_bottom,
--    guint padding_left,
--    guint padding_right
--  )
--*/
--      gtk_box_pack_start = define_c_proc(GTKLIB,"gtk_box_pack_start",
--          {C_PTR,     --  GtkBox* box
--           C_PTR,     --  GtkWidget* child
--           C_INT,     --  gboolean expand
--           C_INT,     --  gboolean fill
--           C_INT})    --  guint padding
        gtk_button_get_label = define_c_func(GTKLIB,"gtk_button_get_label",
            {C_PTR},    --  GtkButton *button
            C_PTR)      -- const gchar *
        gtk_button_new_with_mnemonic = define_c_func(GTKLIB,"gtk_button_new_with_mnemonic",
            {C_PTR},    --  const gchar* label
            C_PTR)      -- GtkWidget*
        gtk_button_set_label = define_c_proc(GTKLIB,"gtk_button_set_label",
            {C_PTR,     --  GtkButton *button
             C_PTR})    --  const gchar* label
        gtk_cell_renderer_pixbuf_new = define_c_func(GTKLIB,"gtk_cell_renderer_pixbuf_new",
            {},         --  void
            C_PTR)      -- GtkCellRenderer*
        gtk_cell_renderer_text_new = define_c_func(GTKLIB,"gtk_cell_renderer_text_new",
            {},         --  void
            C_PTR)      -- GtkCellRenderer*
        gtk_check_button_new_with_mnemonic = define_c_func(GTKLIB,"gtk_check_button_new_with_mnemonic",
            {C_PTR},    --  const gchar* label
            C_PTR)      -- GtkWidget*
        gtk_radio_button_new_with_mnemonic = define_c_func(GTKLIB,"gtk_radio_button_new_with_mnemonic",
            {C_PTR,     --  GSList* group
             C_PTR},    --  const gchar* label
            C_PTR)      -- GtkWidget*
        gtk_radio_button_get_group = define_c_func(GTKLIB,"gtk_radio_button_get_group",
            {C_PTR},    --  GtkRadioButton* radio_button
            C_PTR)      -- GSList*
        gtk_clipboard_clear = define_c_proc(GTKLIB,"gtk_clipboard_clear",
            {C_PTR})    --  GtkClipboard* clipboard
        gtk_clipboard_get = define_c_func(GTKLIB,"gtk_clipboard_get",
            {C_PTR},    --  GdkAtom selection
            C_PTR)      -- GtkClipboard*
--      gtk_clipboard_get_for_display = define_c_func(GTKLIB,"gtk_clipboard_get_for_display",
--          {C_PTR,     --  GdkDisplay* display
--           C_PTR},    --  GdkAtom selection
--          C_PTR)      -- GtkClipboard*
        gtk_clipboard_set_text = define_c_proc(GTKLIB,"gtk_clipboard_set_text",
            {C_PTR,     --  GtkClipboard* clipboard
             C_PTR,     --  const gchar* text
             C_INT})    --  gint len (-1 to use strlen)
        gtk_clipboard_wait_for_text = define_c_func(GTKLIB,"gtk_clipboard_wait_for_text",
            {C_PTR},    --  GtkClipboard* clipboard
            C_PTR)      -- gchar*
        gtk_clipboard_wait_is_text_available = define_c_func(GTKLIB,"gtk_clipboard_wait_is_text_available",
            {C_PTR},    --  GtkClipboard* clipboard
            C_INT)      -- gboolean
        gtk_container_add = define_c_proc(GTKLIB,"gtk_container_add",
            {C_PTR,     --  GtkContainer* container
             C_PTR})    --  GtkTreeIter* parent
        gtk_container_set_border_width = define_c_proc(GTKLIB,"gtk_container_set_border_width",
            {C_PTR,     --  GtkContainer* container
             C_INT})    --  guint border_width
        gtk_css_provider_new = define_c_func(GTKLIB,"gtk_css_provider_new",
            {},         --  void
            C_PTR,      -- GtkCssProvider*
            false)      -- bCrash (3.0+ only)
--?gtk_css_provider_new (-1 on 2.0)
    if bGTK3 then
        gtk_css_provider_load_from_data = define_c_proc(GTKLIB,"gtk_css_provider_load_from_data",
            {C_PTR,     --  GtkCssProvider* css_provider
             C_PTR,     --  const gchar* data
             C_INT,     --  gssize length (-1)
             C_INT})    --  GError** error (NULL)
--          C_INT)      -- gboolean (unreliable anyway, it will either work or not)
--          false)      -- bCrash (3.0+ only)
    end if
        gtk_drawing_area_new = define_c_func(GTKLIB,"gtk_drawing_area_new",
            {},         --  void
            C_PTR)      -- GtkWidget*
        gtk_combo_box_get_active = define_c_func(GTKLIB,"gtk_combo_box_get_active",
            {C_PTR},    --  GtkComboBox* combo_box
            C_PTR)      -- gint
        gtk_combo_box_set_active = define_c_proc(GTKLIB,"gtk_combo_box_set_active",
            {C_PTR,     --  GtkComboBox* combo_box
             C_INT})    --  gint index_
        gtk_combo_box_text_append_text = define_c_proc(GTKLIB,"gtk_combo_box_text_append_text",
            {C_PTR,     --  GtkComboBoxText *combo_box
             C_PTR})    --  const gchar *text
        gtk_combo_box_text_get_active_text = define_c_func(GTKLIB,"gtk_combo_box_text_get_active_text",
            {C_PTR},    --  GtkComboBoxText* combo_box
            C_PTR)      -- gchar*
        gtk_combo_box_text_new = define_c_func(GTKLIB,"gtk_combo_box_text_new",
            {},         --  void
            C_PTR)      -- GtkWidget*
        gtk_combo_box_text_new_with_entry = define_c_func(GTKLIB,"gtk_combo_box_text_new_with_entry",
            {},         --  void
            C_PTR)      -- GtkWidget*
        gtk_combo_box_text_remove = define_c_proc(GTKLIB,"gtk_combo_box_text_remove",
            {C_PTR,     --  GtkComboBoxText* combo_box
             C_INT})    --  gint position
        gtk_entry_new = define_c_func(GTKLIB,"gtk_entry_new",
            {},         --  void
            C_PTR)      -- GtkWidget*
        gtk_entry_get_buffer = define_c_func(GTKLIB,"gtk_entry_get_buffer",
            {C_PTR},    --  GtkEntry* entry
            C_PTR)      -- GtkEntryBuffer*
        gtk_entry_buffer_get_text = define_c_func(GTKLIB,"gtk_entry_buffer_get_text",
            {C_PTR},    --  GtkEntryBuffer* buffer
            C_PTR)      -- const char*
        gtk_entry_buffer_set_text = define_c_proc(GTKLIB,"gtk_entry_buffer_set_text",
            {C_PTR,     --  GtkEntryBuffer* buffer
             C_PTR,     --  const char* chars
             C_INT})    --  int n_chars
        gtk_fixed_new = define_c_func(GTKLIB,"gtk_fixed_new",
            {},         --  (void)
            C_PTR)      -- GtkWidget*
        gtk_fixed_put = define_c_proc(GTKLIB,"gtk_fixed_put",
            {C_PTR,     --  GtkFixed *fixed
             C_PTR,     --  GtkWidget *widget
             C_INT,     --  gint x
             C_INT})    --  gint y
        gtk_fixed_move = define_c_proc(GTKLIB,"gtk_fixed_move",
            {C_PTR,     --  GtkFixed *fixed
             C_PTR,     --  GtkWidget *widget
             C_INT,     --  gint x
             C_INT})    --  gint y
        gtk_frame_new = define_c_func(GTKLIB,"gtk_frame_new",
            {C_PTR},    --  const gchar *label
            C_PTR)      -- GtkWidget*
        gtk_frame_set_label = define_c_proc(GTKLIB,"gtk_frame_set_label",
            {C_PTR,     --  GtkFrame *frame
             C_PTR})    --  const gchar *label
        gtk_frame_get_label = define_c_func(GTKLIB,"gtk_frame_get_label",
            {C_PTR},    --  GtkFrame *frame
            C_PTR)      -- const gchar *
        gtk_get_current_event_time = define_c_func(GTKLIB,"gtk_get_current_event_time",
            {},         --  (void)
            C_INT)      -- guint32
--      gtk_hbox_new = define_c_func(GTKLIB,"gtk_hbox_new",
--          {C_INT,     --  gboolean homogeneous
--           C_INT},    --  gint spacing
--          C_PTR)      -- GtkWidget*
        gtk_label_new = define_c_func(GTKLIB,"gtk_label_new",
            {C_PTR},    --  const gchar* str
            C_PTR)      -- GtkWidget*
        gtk_label_new_with_mnemonic = define_c_func(GTKLIB,"gtk_label_new_with_mnemonic",
            {C_PTR},    --  const gchar* str
            C_PTR)      -- GtkWidget*
        gtk_label_set_text_with_mnemonic = define_c_proc(GTKLIB,"gtk_label_set_text_with_mnemonic",
            {C_PTR,     --  GtkLabel *label
             C_PTR})    --  const gchar *str
        gtk_label_get_text = define_c_func(GTKLIB,"gtk_label_get_text",
            {C_PTR},    --  GtkLabel *label
            C_PTR)      -- const gchar *
--C:\GTK\include\gtk-2.0\gtk\gtklabel.h:110 GtkWidget*          gtk_label_new               (const gchar   *str);
--C:\GTK\include\gtk-2.0\gtk\gtklabel.h:111 GtkWidget*          gtk_label_new_with_mnemonic (const gchar   *str);
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
        gtk_menu_bar_new = define_c_func(GTKLIB,"gtk_menu_bar_new",
            {},         --  (void)
            C_PTR)      -- GtkWidget*
        gtk_menu_item_get_label = define_c_func(GTKLIB,"gtk_menu_item_get_label",
            {C_PTR},    --  GtkMenuItem *menu_item
            C_PTR)      -- const gchar *
        gtk_menu_item_set_label = define_c_proc(GTKLIB,"gtk_menu_item_set_label",
            {C_PTR,     --  GtkMenuItem *menu_item
             C_PTR})    --  const gchar *label
        gtk_menu_item_new_with_mnemonic = define_c_func(GTKLIB,"gtk_menu_item_new_with_mnemonic",
            {C_PTR},    --  const gchar *label
            C_PTR)      -- GtkWidget*
        gtk_check_menu_item_new_with_mnemonic = define_c_func(GTKLIB,"gtk_check_menu_item_new_with_mnemonic",
            {C_PTR},    --  const gchar *label
            C_PTR)      -- GtkWidget*
        gtk_check_menu_item_set_active = define_c_proc(GTKLIB,"gtk_check_menu_item_set_active",
            {C_PTR,     --  GtkCheckMenuItem *check_menu_item
             C_BOOL})   --  gboolean is_active
        gtk_check_menu_item_get_active = define_c_func(GTKLIB,"gtk_check_menu_item_get_active",
            {C_PTR},    --  GtkCheckMenuItem *check_menu_item
            C_BOOL)     -- gboolean
        gtk_radio_menu_item_get_group = define_c_func(GTKLIB,"gtk_radio_menu_item_get_group",
            {C_PTR},    --  GtkRadioMenuItem *radio_menu_item
            C_PTR)      -- GSList *
        gtk_radio_menu_item_new_with_mnemonic = define_c_func(GTKLIB,"gtk_radio_menu_item_new_with_mnemonic",
            {C_PTR,     --  GSList *group
             C_PTR},    --  const gchar *label
            C_PTR)      -- GtkWidget*
--      gtk_radio_menu_item_new_with_mnemonic_from_widget = define_c_func(GTKLIB,"gtk_radio_menu_item_new_with_mnemonic_from_widget",
--          {C_PTR,     --  GtkRadioMenuItem* group
--           C_PTR},    --  const gchar *label
--          C_PTR)      -- GtkWidget*
        gtk_menu_item_set_submenu = define_c_proc(GTKLIB,"gtk_menu_item_set_submenu",
            {C_PTR,     --  GtkMenuItem *menu_item
             C_PTR})    --  GtkWidget *submenu
        gtk_menu_new = define_c_func(GTKLIB,"gtk_menu_new",
            {},         --  (void)
            C_PTR)      -- GtkWidget*
    if bGTK3 then
        gtk_menu_popup_at_pointer = define_c_proc(GTKLIB,"gtk_menu_popup_at_pointer",
            {C_PTR,     --  GtkMenu *menu
             C_PTR})    --  const GdkEvent* trigger_event
    else
        gtk_menu_popup = define_c_proc(GTKLIB,"gtk_menu_popup",
            {C_PTR,     --  GtkMenu *menu
             C_PTR,     --  GtkWidget *parent_menu_shell (NULL here)
             C_PTR,     --  GtkWidget *parent_menu_item (NULL here)
             C_PTR,     --  GtkMenuPositionFunc func (can be NULL)
             C_PTR,     --  gpointer data (can be NULL)
             C_PTR,     --  guint button
             C_PTR})    --  guint32 activate_time
    end if
        gtk_menu_shell_append = define_c_proc(GTKLIB,"gtk_menu_shell_append",
            {C_PTR,     --  GtkMenuShell *menu_shell
             C_PTR})    --  GtkWidget *child
        gtk_notebook_get_n_pages = define_c_func(GTKLIB,"gtk_notebook_get_n_pages",
            {C_PTR},    --  GtkNotebook *notebook
            C_INT)      -- int
        gtk_notebook_insert_page = define_c_func(GTKLIB,"gtk_notebook_insert_page",
            {C_PTR,     --  GtkNotebook *notebook
             C_PTR,     --  GtkWidget *child
             C_PTR,     --  GtkWidget *tab_label
             C_INT},    --  gint position (-1 to append)
            C_INT)      -- gint (inserted index, or -1 on failure)
        gtk_notebook_new = define_c_func(GTKLIB,"gtk_notebook_new",
            {},         --  (void)
             C_PTR)     -- GtkWidget*
        gtk_notebook_set_scrollable = define_c_proc(GTKLIB,"gtk_notebook_set_scrollable",
            {C_PTR,     --  GtkNotebook* notebook
             C_INT})    --  gboolean scrollable
        gtk_notebook_set_tab_label_text = define_c_proc(GTKLIB,"gtk_notebook_set_tab_label_text",
            {C_PTR,     --  GtkNotebook* notebook
             C_PTR,     --  GtkWidget* child
             C_PTR})    --  const char* tab_text
    if bGTK3 then
        gtk_orientable_set_orientation = define_c_proc(GTKLIB,"gtk_orientable_set_orientation",
            {C_PTR,     --  GtkOrientable *orientable
             C_DBL})    --  GtkOrientation orientation (GTK_ORIENTATION_*)
        gtk_progress_bar_set_inverted = define_c_proc(GTKLIB,"gtk_progress_bar_set_inverted",
            {C_PTR,     --  GtkProgressBar *pbar
             C_INT})    --  gboolean inverted
    else
        gtk_progress_bar_set_orientation  = define_c_proc(GTKLIB,"gtk_progress_bar_set_orientation",
            {C_PTR,     --  GtkProgressBar *pbar
             C_INT})    --  GtkProgressBarOrientation orientation (GTK_PROGRESS_*)
    end if
        gtk_progress_bar_new = define_c_func(GTKLIB,"gtk_progress_bar_new",
            {},         --  (void)
             C_PTR)     -- GtkWidget*
        gtk_progress_bar_set_fraction = define_c_proc(GTKLIB,"gtk_progress_bar_set_fraction",
            {C_PTR,     --  GtkProgressBar *pbar
             C_DBL})    --  gdouble fraction
    if not bGTK3 then
        gtk_progress_bar_set_bar_style = define_c_proc(GTKLIB,"gtk_progress_bar_set_bar_style",
            {C_PTR,     --  GtkProgressBar *pbar
             C_DBL})    --  GtkProgressBarStyle style (GTK_PROGRESS_(CONTINUOUS|DISCRETE))
    end if
        gtk_range_get_value = define_c_func(GTKLIB,"gtk_range_get_value",
            {C_PTR},    --  GtkRange *range
             C_DBL)     -- gdouble
        gtk_range_set_value = define_c_proc(GTKLIB,"gtk_range_set_value",
            {C_PTR,     --  GtkRange *range
             C_DBL})    --  gdouble value
    if bGTK3 then
        gtk_scale_new = define_c_func(GTKLIB,"gtk_scale_new",
            {C_INT,     --  GtkOrientation orientation
             C_PTR},    --  GtkAdjustment *adjustment
            C_PTR)      -- GtkWidget*
    else
        gtk_hscale_new = define_c_func(GTKLIB,"gtk_hscale_new",
            {C_PTR},    --  GtkAdjustment *adjustment
            C_PTR)      -- GtkWidget*
        gtk_vscale_new = define_c_func(GTKLIB,"gtk_vscale_new",
            {C_PTR},    --  GtkAdjustment *adjustment
            C_PTR)      -- GtkWidget*
    end if
        gtk_scrolled_window_new = define_c_func(GTKLIB,"gtk_scrolled_window_new",
            {C_PTR,     --  GtkAdjustment* hadjustment (NULL)
             C_PTR},    --  GtkAdjustment* vadjustment (NULL)
            C_PTR)      -- GtkWidget*
        gtk_scrolled_window_set_policy = define_c_proc(GTKLIB,"gtk_scrolled_window_set_policy",
            {C_PTR,     --  GtkScrolledWindow* scrolled_window
             C_PTR,     --  GtkPolicyType hscrollbar_policy
             C_PTR})    --  GtkPolicyType vscrollbar_policy
        gtk_separator_menu_item_new = define_c_func(GTKLIB,"gtk_separator_menu_item_new",
            {},         --  (void)
            C_PTR)      -- GtkWidget*
--      gtk_style_context_add_provider = define_c_proc(GTKLIB,"gtk_style_context_add_provider",
--          {C_PTR,     --  GtkStyleContext *context
--           C_PTR,     --  GtkStyleProvider *provider
--           C_PTR})    --  guint priority
        gtk_style_context_add_provider_for_screen = define_c_proc(GTKLIB,"gtk_style_context_add_provider_for_screen",
            {C_PTR,     --  GdkScreen* screen
             C_PTR,     --  GtkCssProvider* css_provider
             C_INT},    --  guint priority
            false)      -- bCrash (3.0+ only)
--multiline?
--      gtk_text_view_new = define_c_func(GTKLIB,"gtk_text_view_new",
--          {},         --  void
--          C_PTR)      -- GtkWidget*
        gtk_toggle_button_get_active = define_c_func(GTKLIB,"gtk_toggle_button_get_active",
            {C_PTR},    --  GtkToggleButton *toggle_button
            C_INT)      -- gboolean
        gtk_toggle_button_set_active = define_c_proc(GTKLIB,"gtk_toggle_button_set_active",
            {C_PTR,     --  GtkToggleButton* toggle_button
             C_BOOL})   --  gboolean is_active
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
--          C_INT)      -- gint (the number of columns after insertion)
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
--      gtk_vbox_new = define_c_func(GTKLIB,"gtk_vbox_new",
--          {C_INT,     --  gboolean homogeneous
--           C_INT},    --  gint spacing
--          C_PTR)      -- GtkWidget*
--      gtk_widget_get_allocated_height = define_c_func(GTKLIB,"gtk_widget_get_allocated_height",
--          {C_PTR},    --  GtkWidget* widget
--          C_INT)      -- int
--      gtk_widget_get_allocated_width = define_c_func(GTKLIB,"gtk_widget_get_allocated_width",
--          {C_PTR},    --  GtkWidget* widget
--          C_INT)      -- int
        gtk_widget_get_allocation = define_c_proc(GTKLIB,"gtk_widget_get_allocation",
            {C_PTR,     --  GtkWidget *widget
             C_PTR})    --  GtkAllocation *allocation (aka a GtkRectange)
        gtk_widget_get_can_focus = define_c_func(GTKLIB,"gtk_widget_get_can_focus",
            {C_PTR},    --  GtkWidget* widget
            C_INT)      -- gboolean
--      gtk_widget_get_root_window = define_c_func(GTKLIB,"gtk_widget_get_root_window",
--          {C_PTR},    --  GtkWidget* widget
--          C_PTR)      -- GdkWindow*
        gtk_widget_get_parent = define_c_func(GTKLIB,"gtk_widget_get_parent",
            {C_PTR},    --  GtkWidget* widget
            C_PTR)      -- GtkWidget*
        gtk_widget_get_sensitive = define_c_func(GTKLIB,"gtk_widget_get_sensitive",
            {C_PTR},    --  GtkWidget* widget
            C_INT)      -- gboolean
    if bGTK3 then
        gtk_widget_get_style_context = define_c_func(GTKLIB,"gtk_widget_get_style_context",
            {C_PTR},    --  GtkWidget* widget
            C_PTR)      -- GtkStyleContext *
    end if
        gtk_widget_get_window = define_c_func(GTKLIB,"gtk_widget_get_window",
            {C_PTR},    --  GtkWidget* widget
            C_PTR)      -- GdkWindow*
        gtk_widget_grab_focus = define_c_func(GTKLIB,"gtk_widget_grab_focus",
            {C_PTR},    --  GtkWidget *widget
            C_PTR)      -- gboolean
        gtk_widget_hide = define_c_proc(GTKLIB,"gtk_widget_hide",
            {C_PTR})    --  GtkWindow* window,  // aka handle
--      gtk_widget_is_sensitive = define_c_func(GTKLIB,"gtk_widget_is_sensitive",
--          {C_PTR},    --  GtkWidget* widget
--          C_INT)      -- gboolean
        gtk_widget_modify_bg = define_c_proc(GTKLIB,"gtk_widget_modify_bg",
            {C_PTR,     --  GtkWidget* widget
             C_INT,     --  GtkStateType state
             C_PTR})    --  const GdkColor *color
    if bGTK3 then
        gtk_widget_override_font = define_c_proc(GTKLIB,"gtk_widget_override_font",
            {C_PTR,     --  GtkWidget* widget
             C_PTR})    --  PangoFontDescription *font_desc
    else
        gtk_widget_modify_font = define_c_proc(GTKLIB,"gtk_widget_modify_font",
            {C_PTR,     --  GtkWidget* widget
             C_PTR})    --  PangoFontDescription *font_desc
    end if
        gtk_widget_queue_draw = define_c_proc(GTKLIB,"gtk_widget_queue_draw",
            {C_PTR})    --  GtkWidget* widget
        gtk_widget_realize = define_c_proc(GTKLIB,"gtk_widget_realize",
            {C_PTR})    --  GtkWidget* widget
        gtk_widget_set_can_focus = define_c_proc(GTKLIB,"gtk_widget_set_can_focus",
            {C_PTR,     --  GtkWidget* widget
             C_INT})    --  gboolean can_focus
        gtk_widget_set_events = define_c_proc(GTKLIB,"gtk_widget_set_events",
            {C_PTR,     --  GtkWidget* widget
             C_INT})    --  gint events
        gtk_widget_set_realized = define_c_proc(GTKLIB,"gtk_widget_set_realized",
            {C_PTR,     --  GtkWidget* widget
             C_INT})    --  gboolean realized
        gtk_widget_set_sensitive = define_c_proc(GTKLIB,"gtk_widget_set_sensitive",
            {C_PTR,     --  GtkWidget* widget
             C_INT})    --  gboolean sensitive
        gtk_widget_set_size_request = define_c_proc(GTKLIB,"gtk_widget_set_size_request",
            {C_PTR,     --  GtkWidget* widget   // aka handle
             C_INT,     --  gint width,
             C_INT})    --  gint height
        gtk_widget_set_tooltip_text = define_c_proc(GTKLIB,"gtk_widget_set_tooltip_text",
            {C_PTR,     --  GtkWidget* widget   // aka handle
             C_PTR})    --  const gchar *text
        gtk_widget_show = define_c_proc(GTKLIB,"gtk_widget_show",
            {C_PTR})    --  GtkWindow* window,  // aka handle
        gtk_widget_show_all = define_c_proc(GTKLIB,"gtk_widget_show_all",
            {C_PTR})    --  GtkWindow* window,  // aka handle
--  if bGTK3 then
----        gtk_widget_get_preferred_size = define_c_proc(GTKLIB,"gtk_widget_get_preferred_size",
----void
----gtk_widget_get_preferred_width (
----  GtkWidget* widget,
----  gint* minimum_width,
----  gint* natural_width
----)
--      gtk_widget_get_preferred_size = define_c_proc(GTKLIB,"gtk_widget_get_preferred_width",
--          {C_PTR,     --  GtkWidget *widget
--           C_PTR,     --  GtkRequisition* minimum_size (null here)
--           C_PTR})    --  GtkRequisition *natural_size
--  end if
--  else
        gtk_widget_size_request = define_c_proc(GTKLIB,"gtk_widget_size_request",
            {C_PTR,     --  GtkWidget *widget
             C_PTR})    --  GtkRequisition *requisition
--  end if
        gtk_window_get_position = define_c_proc(GTKLIB,"gtk_window_get_position",
            {C_PTR,     --  GtkWindow *window
             C_PTR,     --  gint* root_x
             C_PTR})    --  gint* root_y
        gtk_window_get_size = define_c_proc(GTKLIB,"gtk_window_get_size",
            {C_PTR,     --  GtkWindow* window
             C_PTR,     --  gint* width
             C_PTR})    --  gint* height
        gtk_window_get_title = define_c_func(GTKLIB,"gtk_window_get_title",
            {C_PTR},    --  GtkWindow* window,  // aka handle
             C_PTR)     --  const gchar*
        gtk_window_get_transient_for = define_c_func(GTKLIB,"gtk_window_get_transient_for",
            {C_PTR},    --  GtkWindow* window,  // aka handle
             C_PTR)     --  GtkWindow* parent
--      gtk_window_is_active = define_c_func(GTKLIB,"gtk_window_is_active",
--          {C_PTR},    --  GtkWindow *window
--          C_INT)      -- gboolean
        gtk_window_move = define_c_proc(GTKLIB,"gtk_window_move",
            {C_PTR,     --  GtkWindow* window
             C_INT,     --  gint x
             C_INT})    --  gint y
        gtk_window_new = define_c_func(GTKLIB,"gtk_window_new",
            {C_INT},    --  GtkWindowType type // usually GTK_WINDOW_TOPLEVEL (nb gone in GTK4)
            C_PTR)      -- GtkWidget* // handle
        gtk_window_resize = define_c_proc(GTKLIB,"gtk_window_resize",
            {C_PTR,     --  GtkWindow* window
             C_INT,     --  gint width
             C_INT})    --  gint height
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
        g_free = define_c_proc(LBGLIB,"g_free",
            {C_PTR})    --  gpointer mem
        -- note that g_signal_connect is defined in the GTK sources as a #define of
        -- g_signal_connect_data(....,NULL,0), and is not exported from the dll/so.
        g_signal_connect_data = define_c_func(GTKGDO,"g_signal_connect_data",
            {C_PTR,     --  GObject* instance,              // aka handle
             C_PTR,     --  const gchar* detailed_signal,   // a string
             C_PTR,     --  GCallback c_handler,            // a callback
             C_PTR,     --  gpointer data,                  // data for ""
             C_PTR,     --  GClosureNotify destroy_data,    // (NULL here)
             C_INT},    --  GConnectFlags connect_flags     //     ""
            C_INT)      -- gulong // handler id (>0 for success)
        g_object_set_property = define_c_proc(GTKGDO,"g_object_set_property",
            {C_PTR,     --  GObject* object
             C_PTR,     --  const gchar* property_name
             C_PTR})    --  const GValue* value
        g_object_get_property = define_c_proc(GTKGDO,"g_object_get_property",
            {C_PTR,     --  GObject* object
             C_PTR,     --  const gchar* property_name
             C_PTR})    --  GValue* value
        g_source_remove = define_c_func(LBGLIB,"g_source_remove",
            {C_INT},    --  guint tag
            C_INT)      -- gboolean
        g_timeout_add = define_c_func(LBGLIB,"g_timeout_add",
            {C_INT,     --  guint interval
             C_PTR,     --  GSourceFunc function
             C_PTR},    --  gpointer data
            C_INT)      -- guint
--      g_value_get_int = define_c_func(GTKGDO,"g_value_get_int",
--          {C_PTR},    --  const GValue* value
--          C_INT)      -- gint
--      g_value_init = define_c_func(GTKGDO,"g_value_init",
--          {C_PTR,     --  GValue* value
--           C_INT},    --  GType g_type
--          C_PTR)      -- GValue*
--      g_value_unset = define_c_proc(GTKGDO,"g_value_unset",
--          {C_PTR})    --  GValue* value
        pango_cairo_create_layout = define_c_func(PANCAR,"pango_cairo_create_layout",
            {C_PTR},    --  cairo_t *cr
            C_PTR)      -- PangoLayout *
        pango_cairo_show_layout = define_c_proc(PANCAR,"pango_cairo_show_layout",
            {C_PTR,     --  cairo_t *cr
             C_PTR})    --  PangoLayout *layout
        pango_font_description_free = define_c_proc(PANGO,"pango_font_description_free",
            {C_PTR})    --  PangoFontDescription* desc
        pango_font_description_from_string = define_c_func(PANGO,"pango_font_description_from_string",
            {C_PTR},    --  const char *str
            C_PTR)      -- PangoFontDescription *
        pango_layout_context_changed = define_c_proc(PANGO,"pango_layout_context_changed",
            {C_PTR})    --  PangoLayout *layout
        pango_layout_get_pixel_size = define_c_proc(PANGO,"pango_layout_get_pixel_size",
            {C_PTR,     --  PangoLayout *layout
             C_PTR,     --  int *width
             C_PTR})    --  int *height
        pango_layout_set_font_description = define_c_proc(PANGO,"pango_layout_set_font_description",
            {C_PTR,     --  PangoLayout *layout
             C_PTR})    --  const PangoFontDescription *desc
        pango_layout_set_text = define_c_proc(PANGO,"pango_layout_set_text",
            {C_PTR,     --  PangoLayout *layout
             C_PTR,     --  const char *text
             C_INT})    --  int length
        xg_object_unref = define_c_proc(GTKGDO,"g_object_unref",{C_PTR})
        idGdkRectangle = define_struct("""typedef struct GdkRectangle {
                                          int x;
                                          int y;
                                          int width;
                                          int height;
                                        }""")
        idGtkRequisition = define_struct("""typedef struct {
                                              gint width;
                                              gint height;
                                            } GtkRequisition;""")
-- NB: cffi.e updated to my best guess for these...
-- (this works on 64 bit, the one below didn't [some mods to cffi.e were commented back out])
-- (source: https://api.gtkd.org/gdk.c.types.GdkEventKey.html )
--      idGdkEventKey = define_struct("""typedef struct GdkEventKey {
--                                        GdkEventType ty"""&"""pe;
        string tGdkEventKey = """typedef struct GdkEventKey {
                                          GdkEventType event_type;
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
--      ?define_struct(tGdkEventKey,bAdd:=0)
        string tGdkEventButton = """typedef struct GdkEventButton {
                                      GdkEventType event_type;
                                      GdkWindow *window;
                                      gint8 send_event;
                                      guint32 time;
                                      gdouble x;
                                      gdouble y;
                                      gdouble *axes;
                                      ModifierType state;
                                      guint button;
                                      GdkDevice *device;
                                      gdouble x_root, y_root;
                                    };"""
--struct GdkEventButton {
--  GdkEventType type;
--  GdkWindow* window;
--  gint8 send_event;
--  guint32 time;
--  gdouble x;
--  gdouble y;
--  gdouble* axes;
--  GdkModifierType* state;
--  guint button;
--  GdkDevice* device;
--  gdouble x_root;
--  gdouble y_root;
--}

        idGdkEventButton = define_struct(tGdkEventButton)
--?define_struct(tGdkEventButton,bAdd:=0)

        string tGdkEventConfigure = """typedef struct GdkEventConfigure {
                                         GdkEventType type;
                                         GdkWindow *window;
                                         gint8 send_event;
                                         gint x, y;
                                         gint width;
                                         gint height;
                                       };"""
        idGdkEventConfigure = define_struct(tGdkEventConfigure)

        string tGdkEventFocus = """typedef struct GdkEventFocus {
                                     GdkEventType event_type;
                                     GdkWindow *window;
                                     gint8 send_event;
                                     gint16 in;
                                   };"""
        idGdkEventFocus = define_struct(tGdkEventFocus)

        string tGdkEventMotion = """typedef struct GdkEventMotion {
                                      GdkEventType event_type;
                                      GdkWindow* window;
                                      gint8 send_event;
                                      guint32 time;
                                      gdouble x;
                                      gdouble y;
                                      gdouble* axes;
                                      GdkModifierType* state;
                                      gint16 is_hint;
                                      GdkDevice* device;
                                      gdouble x_root;
                                      gdouble y_root;
                                    };"""
        idGdkEventMotion = define_struct(tGdkEventMotion)

        pData = allocate(machine_word())
        pRECT = allocate_struct(idGdkRectangle)
        pGtkRequisition = allocate_struct(idGtkRequisition)
        GTK_ID_LOOKUP = new_dict() -- GTK only (key:handle, data:id)
        GTK_MENU_LOOKUP = new_dict() -- GTK only (key:handle, data:id)
        GTK_MENU_UPLOOK = new_dict() -- GTK only (key:{menu,id}, data:handle)
        GDK_TYPE_PIXBUF = c_func(gdk_pixbuf_get_type,{})
        {closed_folder, open_folder, dot} = xpg_create_image_list("GTK",xpg_xpm_callback)
    elsif backend=WinAPI then
        atom COMCTL32 = open_dll("comctl32.dll"),
             GDI32 = open_dll("gdi32.dll"),
--           WINMM = open_dll("winmm.dll"),
             USER32 = open_dll("user32.dll"),
             MSIMG32 = open_dll("msimg32.dll"),
             KERNEL32 = open_dll("kernel32.dll")
        xAppendMenu = define_c_func(USER32,"AppendMenuA",
            {C_PTR,     --  HMENU hMenu
             C_INT,     --  UINT uFlags
             C_INT,     --  UINT_PTR uIDNewItem
             C_PTR},    --  LPCTSTR lpNewItem
            C_INT)      -- BOOL
--      xAngleArc = define_c_func(GDI32,"AngleArc",
--          {C_PTR,     --  HDC hdc
--           C_INT,     --  int X
--           C_INT,     --  int Y
--           C_DWORD,   --  DWORD dwRadius
--           C_FLOAT,   --  FLOAT eStartAngle
--           C_FLOAT},  --  FLOAT eSweepAngle
--          C_INT)      -- BOOL
        xArc = define_c_func(GDI32,"Arc",
            {C_PTR,     --  HDC hdc
             C_INT,     --  int nLeftRect
             C_INT,     --  int nTopRect
             C_INT,     --  int nRightRect
             C_INT,     --  int nBottomRect
             C_INT,     --  int nXStartArc
             C_INT,     --  int nYStartArc
             C_INT,     --  int nXEndArc
             C_INT},    --  int nYEndArc
            C_INT)      -- BOOL
        xBeginPaint = define_c_func(USER32,"BeginPaint",
            {C_PTR,     --  HWND  hwnd              // handle of window
             C_PTR},    --  LPPAINTSTRUCT  lpPaint  // address of structure for paint information
            C_PTR)      -- HDC
        xBitBlt = define_c_func(GDI32, "BitBlt",
            {C_PTR,     --  HDC hdcDest
             C_INT,     --  int nXDest
             C_INT,     --  int nYDest
             C_INT,     --  int nWidth
             C_INT,     --  int nHeight
             C_PTR,     --  HDC hdcSrc
             C_INT,     --  int nXSrc
             C_INT,     --  int nYSrc
             C_LONG},   --  DWORD dwRop
            C_INT)      -- BOOL
        xCallWindowProc = define_c_func(USER32,"CallWindowProcA",
            {C_PTR,     --  WNDPROC lpPrevWndFunc
             C_PTR,     --  HWND hWnd
             C_UINT,    --  UINT Msg
             C_UINT,    --  WPARAM wParam
             C_UINT},   --  LPARAM lParam
            C_PTR)      -- LRESULT
        xCheckMenuItem = define_c_proc(USER32,"CheckMenuItem",
            {C_PTR,     --  HMENU hMenu
             C_UINT,    --  UINT uIDCheckItem
             C_UINT})   --  UINT uCheck
--          C_LONG)     -- DWORD (was checked)
        xCheckMenuRadioItem = define_c_func(USER32,"CheckMenuRadioItem",
            {C_PTR,     --  HMENU hmenu,
             C_UINT,    --  UINT idFirst
             C_UINT,    --  UINT idLast
             C_UINT,    --  UINT idCheck
             C_UINT},   --  UINT uFlags
            C_BOOL)     -- BOOL
        xChord = define_c_func(GDI32,"Chord",
            {C_PTR,     --  HDC hdc
             C_INT,     --  int nLeftRect
             C_INT,     --  int nTopRect
             C_INT,     --  int nRightRect
             C_INT,     --  int nBottomRect
             C_INT,     --  int nXRadial1
             C_INT,     --  int nYRadial1
             C_INT,     --  int nXRadial2
             C_INT},    --  int nYRadial2
            C_INT)      -- BOOL
        xCloseClipboard = define_c_proc(USER32,"CloseClipboard",
            {})         --  (void)
--          C_INT)      -- BOOL (0 on failure)
        xCreateCompatibleDC = define_c_func(GDI32, "CreateCompatibleDC",
            {C_PTR},    --  HDC  hdc    // handle of memory device context
            C_PTR)      -- HDC
        xCreateDIBitmap = define_c_func(GDI32, "CreateDIBitmap",
            {C_PTR,     --  HDC hdc
             C_PTR,     --  const BITMAPINFOHEADER *lpbmih
             C_LONG,    --  DWORD fdwInit
             C_LONG,    --  const VOID *lpbInit
             C_PTR,     --  const BITMAPINFO *lpbmi
             C_LONG},   --  UINT fuUsage
            C_PTR)      -- HBITMAP
        xCreateFontIndirect = define_c_func(GDI32, "CreateFontIndirectA",
            {C_PTR},    --  CONST LOGFONT  *lplf        // address of logical font structure
            C_LONG)     -- HFONT (handle of a logical font)
        xCreateMenu = define_c_func(USER32,"CreateMenu",
            {},         --  (void)
            C_PTR)      -- HMENU
        xCreatePopupMenu = define_c_func(USER32,"CreatePopupMenu",
            {},         --  (void)
            C_PTR)      -- HMENU
        xCreatePen = define_c_func(GDI32,"CreatePen",
            {C_INT,     --  int fnPenStyle
             C_INT,     --  int nWidth
             C_INT},    --  COLORREF crColor
            C_PTR)      -- HPEN handle to pen
        xCreateSolidBrush = define_c_func(GDI32, "CreateSolidBrush",
            {C_UINT},   --  COLORREF  crColor   // brush color value
            C_PTR)      -- HBRUSH
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
        xDeleteDC = define_c_func(GDI32, "DeleteDC",
            {C_PTR},    --  HDC  hdc    // handle of device context 
            C_INT)      -- BOOL
        xDeleteObject = define_c_proc(GDI32, "DeleteObject",
            {C_PTR})    --  HGDIOBJ  hObject    // handle of graphic object
--          C_BOOL)     -- BOOL
        xDestroyWindow = define_c_proc(USER32,"DestroyWindow",
            {C_PTR})    --  HWND  hWnd  // handle of window to destroy
--          C_BOOL)     -- BOOL
        xDispatchMessage = define_c_proc(USER32,"DispatchMessageA",
            {C_PTR})    --  CONST MSG  * lpmsg  // address of structure with message
--          C_LONG)     -- LONG (generally ignored)
        xDrawMenuBar = define_c_func(USER32,"DrawMenuBar",
            {C_PTR},    --  HWND hWnd
            C_INT)      -- BOOL
--      xDrawText = define_c_func(USER32,"DrawTextA",
        xDrawText = define_c_func(USER32,"DrawTextW",
            {C_INT,     --  HDC  hDC            // handle of device context
             C_INT,     --  LPCTSTR  lpString   // address of string to draw
             C_INT,     --  int  nCount     // string length, in characters
             C_INT,     --  LPRECT  lpRect  // address of structure with formatting dimensions
             C_INT},    --  UINT  uFormat   // text-drawing flags
            C_INT)      -- int height of the text
        xEmptyClipboard = define_c_func(USER32,"EmptyClipboard",
            {},         --  (void)
            C_BOOL)     -- BOOL
        xEnableMenuItem = define_c_func(USER32,"EnableMenuItem",
            {C_PTR,     --  HMENU hMenu
             C_UINT,    --  UINT uIDEnableItem
             C_UINT},   --  UINT uEnable
            C_BOOL)     -- BOOL (was enabled)
        xEnableWindow = define_c_proc(USER32,"EnableWindow",
            {C_PTR,     --  HWND hWnd
             C_INT})    --  BOOL bEnable
--          C_BOOL)     -- BOOL (was enabled)
        xEndPaint = define_c_proc(USER32,"EndPaint",
            {C_PTR,     --  HWND  hWnd                  // handle of window
             C_PTR})    -- CONST PAINTSTRUCT  *lpPaint  // address of structure for paint data
--          C_INT)      -- BOOL (function always returns true so linked as c_proc)
        xFillRect = define_c_func(USER32,"FillRect",
            {C_PTR,     --  HDC hDC
             C_PTR,     --  const RECT *lprc
             C_PTR},    --  HBRUSH hbr
            C_LONG)     -- int (0 on failure)
        xGetClientRect = define_c_func(USER32,"GetClientRect",
            {C_PTR,     --  HWND hWnd
             C_PTR},    --  LPRECT lpRect
            C_LONG)     -- BOOL
        xGetClipboardData = define_c_func(USER32,"GetClipboardData",
            {C_UINT},   --  UINT uFormat
            C_PTR)      -- HANDLE
        xGetConsoleWindow = define_c_func(KERNEL32,"GetConsoleWindow",
            {},         --  (void)
            C_PTR)      -- HWND of the console window, or NULL.
        xGetCursorPos = define_c_func(USER32,"GetCursorPos",
            {C_PTR},    --  LPPOINT lpPoint
            C_INT)      -- BOOL
        xGetDC = define_c_func(USER32,"GetDC",
            {C_PTR},    --  HWND  hWnd  // handle of window
            C_PTR)      -- HDC
        xGetDeviceCaps = define_c_func(GDI32,"GetDeviceCaps",
            {C_PTR,     --  HDC  hdc,   // device-context handle
             C_INT},    --  int  nIndex // index of capability to query
            C_INT)      -- int
        xGetFocus = define_c_func(USER32,"GetFocus",
            {},         --  (void)
            C_PTR)      -- HWND
        xGetForegroundWindow = define_c_func(USER32,"GetForegroundWindow",
            {},         --  (void)
            C_PTR)      -- HWND of the forground window.
        xGetKeyState = define_c_func(USER32, "GetKeyState",
            {C_INT},    --  int  nVirtKey       // virtual-key code
            C_INT)      -- SHORT
        xGetLastError = define_c_func(KERNEL32,"GetLastError",
            {},         -- (void)
            C_INT)      -- DWORD
--      xGetMenuItemCount = define_c_func(USER32, "GetMenuItemCount",
--          {C_PTR},    --  HMENU hMenu
--          C_INT)      -- int
--      xGetMenuItemID = define_c_func(USER32,"GetMenuItemID",
--          {C_PTR,     --  HMENU hMenu
--           C_INT},    --  int nPos
--  --      C_UINT)     -- UINT
--          C_INT)      -- UINT
        xGetMenuItemInfo = define_c_func(USER32,"GetMenuItemInfoA",
            {C_PTR,     --  HMENU hMenu
             C_INT,     --  UINT uItem
             C_INT,     --  BOOL fByPosition
             C_PTR},    --  LPMENUITEMINFO lpmii
            C_INT)      -- BOOL
        xGetMenuState = define_c_func(USER32,"GetMenuState",
            {C_PTR,     --  HMENU hMenu
             C_UINT,    --  UINT uId
             C_UINT},   --  UINT uFlags
            C_UINT)     -- UINT
--      xGetMenuString = define_c_func(USER32,"GetMenuStringA",
--          {C_PTR,     --  HMENU hMenu
--           C_INT,     --  UINT uIDItem
--           C_INT,     --  LPTSTR lpString
--           C_INT,     --  int nMaxCount
--           C_INT},    --  UINT uFlag
--          C_INT)      -- int
        xGetMessage = define_c_func(USER32,"GetMessageA",
            {C_PTR,     --  LPMSG  lpMsg    // address of structure with message
             C_PTR,     --  HWND  hWnd      // handle of window
             C_UINT,    --  UINT  wMsgFilterMin  // first message
             C_UINT},   --  UINT  wMsgFilterMax  // last message
             C_INT)     -- BOOL
--DEV... (to go)
        xGetPixel = define_c_func(GDI32,"GetPixel",
            {C_PTR,     --  HDC hdc
             C_INT,     --  int X
             C_INT},    --  int Y
            C_INT)      -- COLORREF crColor
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
        xGetTextExtentPoint32W = define_c_func(GDI32, "GetTextExtentPoint32W",
            {C_PTR,     --  HDC  hdc,   // handle of device context
             C_PTR,     --  LPCTSTR  lpString,  // address of text string
             C_INT,     --  int  cbString,  // number of characters in string
             C_PTR},    --  LPSIZE  lpSize  // address of structure for string size
            C_INT)      -- BOOL
        xGetWindowLong = define_c_func(USER32,iff(MB=32?"GetWindowLongA"
                                                       :"GetWindowLongPtrA"),
            {C_PTR,     --  HWND  hWnd      // handle of window
             C_UINT},   --  int  nIndex     // offset of value to retrieve
            C_LONG)     -- LONG/LONG_PTR
        xGetWindowRect = define_c_func(USER32,"GetWindowRect",
            {C_PTR,     --  HWND hWnd
             C_PTR},    --  LPRECT lpRect
            C_INT)      -- BOOL
        xGetWindowText = define_c_proc(USER32,"GetWindowTextA",
            {C_PTR,     --  HWND hWnd
             C_PTR,     --  LPTSTR lpString
             C_INT})    --  int nMaxCount
--          C_INT)      -- int
        xGetWindowTextLength = define_c_func(USER32,"GetWindowTextLengthA",
            {C_PTR},    --  HWND hWnd
            C_INT)      -- int
        xGlobalAlloc = define_c_func(KERNEL32,"GlobalAlloc",
            {C_UINT,    --  UINT uFlags
             C_UINT},   --  SIZE_T dwBytes
            C_PTR)      -- HGLOBAL
        xGlobalFree = define_c_func(KERNEL32,"GlobalFree",
            {C_PTR},    --  HGLOBAL hMem
            C_PTR)      -- HGLOBAL (null on success)
        xGlobalLock = define_c_func(KERNEL32,"GlobalLock",
            {C_PTR},    --  HGLOBAL hMem
            C_PTR)      -- LPVOID
        xGlobalSize = define_c_func(KERNEL32,"GlobalSize",
            {C_PTR},    --  HGLOBAL hMem
            C_INT)      -- SIZE_T
        xGlobalUnlock = define_c_proc(KERNEL32,"GlobalUnlock",
            {C_PTR})    --  HGLOBAL hMem
--          C_INT)      -- BOOL (non-0: success but still locked, 0: check success/failure via GetLastError)
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
        xIsClipboardFormatAvailable = define_c_func(USER32,"IsClipboardFormatAvailable",
            {C_UINT},   --  UINT format
            C_INT)      -- BOOL
        xKillTimer = define_c_func(USER32, "KillTimer",
            {C_PTR,     --  HWND hWnd (NULL here)
             C_UINT},   --  UINT_PTR uIDEvent
            C_INT)      -- BOOL
        xLineTo = define_c_func(GDI32, "LineTo",
            {C_PTR,     --  HDC  hdc,   // device context handle
             C_INT,     --  int  nXEnd, // x-coordinate of line's ending point
             C_INT},    --  int  nYEnd  // y-coordinate of line's ending point
            C_INT)      -- BOOL
        xLoadCursor = define_c_func(USER32,"LoadCursorA",
            {C_PTR,     --  HINSTANCE hInstance
             C_PTR},    --  LPCTSTR lpCursorName
            C_PTR)      -- HCURSOR
        xLoadIcon = define_c_func(USER32,"LoadIconA",
            {C_PTR,     --  HINSTANCE hInstance
             C_PTR},    --  LPCTSTR lpIconName
            C_PTR)      -- HICON
        xMoveToEx = define_c_func(GDI32, "MoveToEx",
            {C_PTR,     --  HDC  hdc,   // handle of device context
             C_INT,     --  int  X, // x-coordinate of new current position
             C_INT,     --  int  Y, // y-coordinate of new current position
             C_PTR},    --  LPPOINT  lpPoint    // address of old current position
            C_INT)      -- BOOL
        xMoveWindow = define_c_proc(USER32,"MoveWindow",
            {C_PTR,     --  HWND hWnd
             C_INT,     --  int X
             C_INT,     --  int Y
             C_INT,     --  int nWidth
             C_INT,     --  int nHeight
             C_INT})    --  BOOL bRepaint
--          C_INT)      -- BOOL
        xOpenClipboard = define_c_func(USER32,"OpenClipboard",
            {C_PTR},    --  HWND hWndNewOwner
            C_PTR)      -- BOOL
        xPie = define_c_func(GDI32,"Pie",
            {C_PTR,     --  HDC hdc
             C_INT,     --  int nLeftRect
             C_INT,     --  int nTopRect
             C_INT,     --  int nRightRect
             C_INT,     --  int nBottomRect
             C_INT,     --  int nXRadial1
             C_INT,     --  int nYRadial1
             C_INT,     --  int nXRadial2
             C_INT},    --  int nYRadial2
            C_INT)      -- BOOL
--      xPlaySound = define_c_proc(WINMM,"PlaySound",
--  --  xPlaySound = define_c_func(WINMM,"PlaySoundA",
--          {C_PTR,     --  LPCTSTR pszSound
--           C_PTR,     --  HMODULE hmod
--           C_INT})    --  DWORD   fdwSound
--  --      C_INT)      -- BOOL
        xPolyBezier = define_c_func(GDI32, "PolyBezier",
            {C_PTR,     --  HDC hdc
             C_PTR,     --  const POINT *lppt
             C_INT},    --  DWORD cPoints
            C_INT)      -- BOOL
        xPostQuitMessage = define_c_proc(USER32,"PostQuitMessage",
            {C_INT})    --  int  nExitCode      // exit code
        xRectangle = define_c_func(GDI32, "Rectangle",
            {C_PTR,     --  HDC hdc
             C_INT,     --  int nLeftRect
             C_INT,     --  int nTopRect
             C_INT,     --  int nRightRect
             C_INT},    --  int nBottomRect
            C_INT)      -- BOOL
        xRedrawWindow = define_c_func(USER32, "RedrawWindow",
            {C_PTR,     --  HWND hWnd
             C_PTR,     --  const RECT *lprcUpdate (null here)
             C_PTR,     --  HRGN hrgnUpdate            "" 
             C_UINT},   --  UINT flags
            C_INT)      -- BOOL
        xRegisterClassEx = define_c_func(USER32,"RegisterClassExA",
            {C_PTR},    --  CONST WNDCLASSEX FAR *lpwcx // address of structure with class data
            C_PTR)      -- ATOM
        xReleaseDC = define_c_func(USER32, "ReleaseDC",
            {C_PTR,     --  HWND  hwnd, // handle of window
             C_PTR},    --  HDC  hdc    // handle of device context
            C_INT)      -- BOOL
        xRoundRect = define_c_func(GDI32, "RoundRect",
            {C_PTR,     --  HDC hdc
             C_INT,     --  int nLeftRect
             C_INT,     --  int nTopRect
             C_INT,     --  int nRightRect
             C_INT,     --  int nBottomRect
             C_INT,     --  int nWidth
             C_INT},    --  int nHeight
            C_INT)      -- BOOL
        xSelectObject = define_c_func(GDI32, "SelectObject",
            {C_PTR,     --  HDC  hdc,   // handle of device context
             C_PTR},    --  HGDIOBJ  hgdiobj    // handle of object
            C_PTR)      -- HGDIOBJ
        xSendMessage = define_c_func(USER32,"SendMessageA",
            {C_PTR,     --  HWND  hwnd  // handle of destination window
             C_UINT,    --  UINT  uMsg  // message to send
             C_UINT,    --  WPARAM  wParam  // first message parameter
             C_UINT},   --  LPARAM  lParam  // second message parameter
            C_LONG)     -- LRESULT
        xSetBkMode = define_c_func(GDI32, "SetBkMode",
            {C_PTR,     --  HDC  hdc,   // handle of device context
             C_INT},    --  int  iBkMode    // flag specifying background mode
            C_INT)      -- int
        xSetClipboardData = define_c_func(USER32, "SetClipboardData",
            {C_UINT,    --  UINT uFormat
             C_PTR},    --  HANDLE hMem
            C_INT)      -- HANDLE (NULL means failure)
        xSetFocus = define_c_proc(USER32,"SetFocus",
            {C_PTR})    --  HWND hWnd
--          C_PTR)      -- HWND (previous focus)
        xSetMenu = define_c_func(USER32,"SetMenu",
            {C_PTR,     --  HWND hWnd
             C_PTR},    --  HMENU hMenu
            C_INT)      -- BOOL
        xSetMenuItemInfo = define_c_func(USER32,"SetMenuItemInfoA",
            {C_PTR,     --  HMENU hMenu
             C_INT,     --  UINT uItem
             C_INT,     --  BOOL fByPosition
             C_PTR},    --  LPMENUITEMINFO lpmii
            C_INT)      -- BOOL
        xSetParent = define_c_func(USER32,"SetParent",
            {C_PTR,     --  HWND  hwndChild         // handle of window whose parent is changing
             C_PTR},    --  HWND  hwndNewParent     // handle of new parent window
            C_PTR)      -- HWND of the previous parent window.
        xSetPixelV = define_c_func(GDI32,"SetPixelV",
            {C_PTR,     --  HDC hdc
             C_INT,     --  int X
             C_INT,     --  int Y
             C_INT},    --  COLORREF crColor
            C_INT)      -- BOOL (non-zero for success)
        xSetROP2 = define_c_func(GDI32,"SetROP2",
            {C_PTR,     --  HDC hdc
             C_INT},    --  int fnDrawMode
            C_INT)      -- int
        xSetScrollInfo = define_c_func(USER32, "SetScrollInfo",
            {C_PTR,     --  HWND hwnd
             C_INT,     --  int fnBar
             C_PTR,     --  LPCSCROLLINFO lpsi
             C_UINT},   --  BOOL fRedraw
            C_INT)      -- int
        xSetTextAlign = define_c_func(GDI32, "SetTextAlign",
            {C_PTR,     --  HDC hdc
             C_UINT},   --  UINT fMode
            C_UINT)     -- UINT
        xSetTextColor = define_c_func(GDI32, "SetTextColor",
            {C_PTR,     --  HDC  hdc,   // handle of device context
             C_PTR},    --  COLORREF  crColor   // text color
            C_PTR)      -- COLORREF
        xSetTimer = define_c_func(USER32, "SetTimer",
            {C_PTR,     --  HWND hWnd (NULL here)
             C_UINT,    --  UINT_PTR nIDEvent (0 here)
             C_UINT,    --  UINT uElapse
             C_PTR},    --  TIMERPROC lpTimerFunc
            C_PTR)      -- UINT_PTR
        xSetWindowLong = define_c_func(USER32,iff(MB=32?"SetWindowLongA"
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
--      xTextOut = define_c_func(GDI32, "TextOutW",
--          {C_PTR,     --  HDC  hdc,   // handle of device context
--           C_INT,     --  int  nXStart,       // x-coordinate of starting position
--           C_INT,     --  int  nYStart,       // y-coordinate of starting position
--           C_PTR,     --  LPCTSTR  lpString,  // address of string
--           C_INT},    --  int  cbString       // number of characters in string
--          C_INT)      -- BOOL success
        xTrackPopupMenuEx = define_c_func(USER32, "TrackPopupMenuEx",
            {C_PTR,     --  HMENU  hmenu,       
             C_UINT,    --  UINT  fuFlags,      
             C_INT,     --  int  x,     
             C_INT,     --  int  y,     
             C_PTR,     --  HWND  hwnd,         
             C_PTR},    --  LPTPMPARAMS  lptpm  (NULL here)
            C_LONG)     -- BOOL
        xTransparentBlt = define_c_func(MSIMG32,"TransparentBlt",
            {C_PTR,     --  HDC hdcDest
             C_INT,     --  int xoriginDest
             C_INT,     --  int yoriginDest
             C_INT,     --  int wDest
             C_INT,     --  int hDest
             C_PTR,     --  HDC hdcSrc
             C_INT,     --  int xoriginSrc
             C_INT,     --  int yoriginSrc
             C_INT,     --  int wSrc
             C_INT,     --  int hSrc
             C_UINT},   --  UINT crTransparent
            C_LONG)     -- BOOL
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

        set_unicode(0)
        idLOGFONT = define_struct("""typedef struct {
                                      LONG  lfHeight;
                                      LONG  lfWidth;
                                      LONG  lfEscapement;
                                      LONG  lfOrientation;
                                      LONG  lfWeight;
                                      BYTE  lfItalic;
                                      BYTE  lfUnderline;
                                      BYTE  lfStrikeOut;
                                      BYTE  lfCharSet;
                                      BYTE  lfOutPrecision;
                                      BYTE  lfClipPrecision;
                                      BYTE  lfQuality;
                                      BYTE  lfPitchAndFamily;
                                      TCHAR lfFaceName[LF_FACESIZE];
                                     } LOGFONT, *PLOGFONT;""")

--DEV might deserve to be in opengl.e... might deserve a separate glCanvas(), or gCanvas(...,bool bUseOpenGL=false)...
--      idPIXELFORMATDESCRIPTOR = define_struct("""typedef struct tagPIXELFORMATDESCRIPTOR {
--                                                  WORD  nSize;
--                                                  WORD  nVersion;
--                                                  DWORD dwFlags;
--                                                  BYTE  iPixelType;
--                                                  BYTE  cColorBits;
--                                                  BYTE  cRedBits;
--                                                  BYTE  cRedShift;
--                                                  BYTE  cGreenBits;
--                                                  BYTE  cGreenShift;
--                                                  BYTE  cBlueBits;
--                                                  BYTE  cBlueShift;
--                                                  BYTE  cAlphaBits;
--                                                  BYTE  cAlphaShift;
--                                                  BYTE  cAccumBits;
--                                                  BYTE  cAccumRedBits;
--                                                  BYTE  cAccumGreenBits;
--                                                  BYTE  cAccumBlueBits;
--                                                  BYTE  cAccumAlphaBits;
--                                                  BYTE  cDepthBits;
--                                                  BYTE  cStencilBits;
--                                                  BYTE  cAuxBuffers;
--                                                  BYTE  iLayerType;
--                                                  BYTE  bReserved;
--                                                  DWORD dwLayerMask;
--                                                  DWORD dwVisibleMask;
--                                                  DWORD dwDamageMask;
--                                                 } PIXELFORMATDESCRIPTOR, *PPIXELFORMATDESCRIPTOR, *LPPIXELFORMATDESCRIPTOR;""")


        idTOOLINFO = define_struct("""typedef struct {
                                        UINT      cbSize;
                                        UINT      uFlags;
                                        HWND      hwnd;
                                        UINT_PTR  uId;
                                        RECT      rect;
                                        HINSTANCE hinst;
                                        LPTSTR    lpszText;
                                        LPARAM    lParam;
                                        void     *lpReserved;
                                      } TOOLINFO, *PTOOLINFO, *LPTOOLINFO;""")

        idSCROLLINFO = define_struct("""typedef struct tagSCROLLINFO {
                                          UINT cbSize;
                                          UINT fMask;
                                          int  nMin;
                                          int  nMax;
                                          UINT nPage;
                                          int  nPos;
                                          int  nTrackPos;
                                        } SCROLLINFO, *LPCSCROLLINFO;""")

        idTCITEM = define_struct("""typedef struct {
                                      UINT   mask;
                                      DWORD  dwState;
                                      DWORD  dwStateMask;
                                      LPTSTR pszText;
                                      int    cchTextMax;
                                      int    iImage;
                                      LPARAM lParam;
                                    } TCITEM, *LPTCITEM;""")

        idMENUITEMINFO = define_struct("""typedef struct tagMENUITEMINFO {
                                            UINT cbSize;
                                            UINT fMask;
                                            UINT fType;
                                            UINT fState;
                                            UINT wID;
                                            HMENU hSubMenu;
                                            HBITMAP hbmpChecked;
                                            HBITMAP hbmpUnchecked;
                                            ULONG_PTR dwItemData;
                                            LPTSTR    dwTypeData;
                                            UINT     cch;
                                            HBITMAP hbmpItem;
                                          } MENUITEMINFO, *LPMENUITEMINFO;""")
           
--(tested, 16 bytes on both 32 and 64 bit:)
--printf(1,"Windows %d bit, pRECT is %d bytes\n",{machine_bits(),get_struct_size(idRECT)})
           
        pPAINTSTRUCT = allocate_struct(idPAINTSTRUCT)
        pRECT = allocate_struct(idRECT)
        pSIZE = allocate_struct(idSIZE)
        pTVINSERTSTRUCT = allocate_struct(idTVINSERTSTRUCT)
        pTVITEMEX = allocate_struct(idTVITEMEX)
        pMSG = allocate_struct(idMESSAGE)
        pLOGFONT = allocate_struct(idLOGFONT)
--      pPIXELFORMATDESCRIPTOR = allocate_struct(idPIXELFORMATDESCRIPTOR)
        pTOOLINFO = allocate_struct(idTOOLINFO)
        pSCROLLINFO = allocate_struct(idSCROLLINFO)
        set_struct_field(idSCROLLINFO,pSCROLLINFO,"cbSize",get_struct_size(idSCROLLINFO))
        pTCITEM = allocate_struct(idTCITEM)
        pPOINT = allocate_struct(idPOINT)
        pMENUITEMINFO = allocate_struct(idMENUITEMINFO)
        set_struct_field(idMENUITEMINFO,pMENUITEMINFO,"cbSize",get_struct_size(idMENUITEMINFO))
        NullBrushID = c_func(xGetStockObject,{NULL_BRUSH})
        WINAPI_SUBMENUS = new_dict()
        WIN_MENU_CHECKED = new_dict() -- WIN only (autotoggle)


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
                 hCursor = c_func(xLoadCursor,{NULL,IDC_ARROW}),
             class_style = or_all({CS_DBLCLKS,CS_OWNDC,CS_HREDRAW,CS_VREDRAW}),
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

        -- and one for gCanvas:
        set_struct_field(idWNDCLASSEX,pWNDCLASSEX,"lpszClassName",xpg_raw_string_ptr("gCanvas"))
        set_struct_field(idWNDCLASSEX,pWNDCLASSEX,"hbrBackground",NULL) -- use WM_ERASEBKGND
        if c_func(xRegisterClassEx,{pWNDCLASSEX})=0 then
            crash("RegisterClassEx error #%08x (%d)",c_func(xGetLastError,{}))
        end if

        tree_himl = xpg_create_image_list("WinAPI",xpg_xpm_callback)
    else
        ?9/0 -- (unknown backend)
    end if
    -- finally setup common handlers:
    xpg_register_handler(0,"KEY",{{2,2,"FOI"},{3,3,"FOII"},{4,4,"FOIII"},{5,5,"FOIIII"}})
    xpg_register_handler(0,"CLICK",{{4,4,"FOPII"},{2,2,"FOP"}})
    xpg_register_handler(0,"MOUSEMOVE",{{3,3,"POII"},{6,6,"POIIIII"},{9,9,"POIIIIIIII"}})
--DEV...
--  xpg_register_handler(0,"REDRAW",{{1,1,"PO"},{0,0,"P"}})
end procedure

global function gVersion(bool bBack=false)
    if not bInit then xpg_Init() end if
    if bBack then return backdesc[backend] end if
    integer pdx = find(platform(),{WINDOWS,LINUX,ARM,JS})
    string bare = "xpGUI 0.1", backing,
           plat = {"Windows","Linux","Arm","JavaScript"}[pdx]
    if backend=GTK then
        backing = sprintf("GTK %d.%d.%d",gtk_version)
    elsif backend=WinAPI then
        backing = "WinAPI"
    else
        ?9/0 -- (unknown backend)
    end if
    return sprintf("%s on Phix version %s (%d bits) on %s, using %s",
                   {bare,version(),machine_bits(),plat,backing})
end function

global function gGetGlobalIntInt(string name)
    if not bInit then xpg_Init() end if
    if name="SCREENSIZE" then 
        integer width, height
        if backend=GTK then
            if machine_bits()=32 then
                width = c_func(gdk_screen_get_width)
                height = c_func(gdk_screen_get_height)
            else -- GTK/64 bit (and/or post-2.0)
                atom display = c_func(gdk_display_get_default,{}),
                     monitor = c_func(gdk_display_get_monitor,{display,0})
                c_proc(gdk_monitor_get_geometry,{monitor,pRECT})
                width = get_struct_field(idGdkRectangle,pRECT,"width")
                height = get_struct_field(idGdkRectangle,pRECT,"height")
            end if
        elsif backend=WinAPI then
            width = c_func(xGetSystemMetrics,{SM_CXSCREEN})
            height = c_func(xGetSystemMetrics,{SM_CYSCREEN})
        else
            ?9/0 -- (unknown backend)
        end if
        return {width,height}
    elsif name="MOUSEPOS" then
        integer x, y
        if backend=GTK then
            atom display = c_func(gdk_display_get_default,{}),
                 pX = allocate(4), pY = allocate(4)
            c_proc(gdk_display_get_pointer,{display,NULL,pX,pY,NULL})
            x = peek4u(pX)
            y = peek4u(pY)
            free({pX,pY})
        elsif backend=WinAPI then
            bool bOK = c_func(xGetCursorPos,{pPOINT})
            assert(bOK)
            x = get_struct_field(idPOINT,pPOINT,"x")
            y = get_struct_field(idPOINT,pPOINT,"y")
        else
            ?9/0 -- (unknown backend)
        end if
        return {x,y}
    end if
    crash("gGetGlobalIntInt(%s) not supported",{name})
end function

global function gGetGlobal(string name)
    if name="COPYRIGHT" then
        return COPYRIGHT
    elsif name="MOUSEPOS" then
        return gGetGlobalIntInt("MOUSEPOS")
    elsif name="SCREENSIZE" then 
--      return sprintf("%dx%x",gGetGlobalIntInt(name))
        return gGetGlobalIntInt(name)
    elsif name="VERSION" then
        return gVersion()
    end if
    crash("gGetGlobal(%s) not supported",{name})
end function

global procedure gSetGlobal(string name, object v)
    if name="XPM_INIT" then 
        {xpg_create_image_list,xpg_winAPI_create_DIB_from_xpm} = v
    else
        crash("gSetGlobal(%s,%v) not supported",{name,v})
    end if
end procedure

include xpg_xpm.e   -- factored out for neatness, avoids defining any globals via gSetGlobal("XPM_INIT")

--DEV w,h of no real value here??
local function xpg_WinAPI_create(integer id, string class_name, nullable_string lbl, atom pID, w, h, dwStyle, dwExStyle)
    atom pHwnd = iff(pID?ctrl_handles[pID]:NULL),
        lpszClassName = xpg_raw_string_ptr(class_name)
--     lpszWindowName = xpg_raw_string_ptr(lbl)

    sequence cw_params = {dwExStyle,        -- extended style
                          lpszClassName,    -- window class name
--                        lpszWindowName,   -- window caption or Button text etc..
                          lbl,              -- window caption or Button text etc..
                          dwStyle,          -- window style
                          CW_USEDEFAULT,    -- initial x position
                          CW_USEDEFAULT,    -- initial y position
                          w,                -- initial x size
                          h,                -- initial y size
                          pHwnd,            -- parent window handle
                          NULL,             -- window menu handle OR user id for child windows
--                        id,               -- window menu handle OR user id for child windows
                          NULL,             -- program instance handle - Legacy of Win16 apps. 0 will work too.
                          NULL}             -- creation parameters
--DEV
--if class_name="button" then
----    cw_params[5..8] = {10,10,120,30}
--  cw_params[7..8] = {120,30}
--end if

    assumed_hwnd = NULL
    assume_id = id
    atom hwnd = c_func(xCreateWindowEx,cw_params)
    if hwnd=0 then
--CreateWindowEx error #0000057E
        crash("CreateWindowEx error #%08x",{c_func(xGetLastError,{})})
    end if
    assert(assumed_hwnd=NULL or assumed_hwnd=hwnd)
    assume_id = 0

--DEV?? (nope, still needed before subclassing...) [which[DEV] we might be able to move...]
--  ctrl_handles[id] = hwnd
--
--  atom tmp = c_func(xGetWindowLong,{hwnd,GWL_WNDPROC})
--  wnd_proc_addr[id] = tmp

--DEV?? (now does above too)
    xpg_setID(hwnd,id)

    -- Let Windows do a bit of pre-processing on a few messages for us:
--  xpg_WinAPI_sub_class_control(id,hwnd)

--  if ctrl_type=DIALOG and PrimaryWindowID=UNDEFINED then
--      PrimaryWindowID = id
--  end if

    integer ct = ctrl_types[id]
    if ct=DIALOG then
        if pID then
            assert(ctrl_types[pID]=DIALOG)
--DEV we could, I suppose, recursively check for/through any virtual children...
        end if
    else
        -- subclass non-dialogs:
--local procedure xpg_WinAPI_sub_class_control(gdx id, xpg_handle hwnd)
--  integer ct = ctrl_types[id]
    -- certain control types do not get subclassed:
--  if ct!=StaticBitmap and ct!=DIALOG 
--  and ct!=ReBarBand then--and ct!=HyperText then
--  if ct!=DIALOG then
--  if ct!=DIALOG and ct!=FRAME and ct!=BUTTON then
--  if false then
--?{"subclassing!",id}
--      assert(sub_proc_addr[id]==UNDEFINED)
        atom prev_wnd_proc = c_func(xSetWindowLong,{hwnd,GWL_WNDPROC,WINAPI_SUBPROC_CB})
        wnd_proc_addr[id] = prev_wnd_proc
--      sub_proc_addr[id] = WINAPI_SUBPROC_CB
--  end if
--end procedure
        if pID then
--DEV trash this, one control at a time: (erm, no, eg ProgressBar ...)
if not find(ct,{BUTTON,DROPDOWN}) then
--if not find(ct,{BUTTON,DROPDOWN,CANVAS}) then
--          integer r = c_func(xGetWindowRect,{hwnd,pRECT})
--,iw=w, ih=h
--          assert(r!=0)
--          w = get_struct_field(idRECT,pRECT,"right")
--            - get_struct_field(idRECT,pRECT,"left")
--          h = get_struct_field(idRECT,pRECT,"bottom")
--            - get_struct_field(idRECT,pRECT,"top")
--?{"xpg_WinAPI_create NS:",id,w,h,{iw,ih}}
            ctrl_size[id][SZ_NATURAL_W] = w
            ctrl_size[id][SZ_NATURAL_H] = h
end if
            -- aside: pID is now from xpg_get_parent_id(), skipping v/hbox, so use
            --        parent_ids[id] instead - not that this probably helps much.
--          assert(find(id,children_ids[pID])!=0)
            assert(find(id,children_ids[parent_ids[id]])!=0)
        end if
    end if

    return hwnd  -- (nb handle not id!)

end function

include builtins/ptypes.e

local type dword_seq(object s)  -- (technically qword_seq on 64-bit)
    return sequence(s) and not string(s)
end type

local function paranormalise_traa(object title, click, sequence attributes, dword_seq args)
-- used by gButton([nullable_string title=NULL,] [rtn click=NULL,] string attributes="", sequence args={})
-- and gCheckbox(), gLink(), gMenuItem(), and gValuator(). (See the docs for the full details)
-- This routine is designed to crash on the slightest oddity, hopefully with a decent easily understood message.
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
    if click=NULL and attributes="" and string(title) and find('=',title) then
        attributes = title
        title = ""
    end if
    assert(nullable_string(title),"not nullable_string title",nFrames:=nFrames)
    assert(rtn(click),"not rtn click",nFrames:=nFrames)
    assert(string(attributes),"not string attributes",nFrames:=nFrames)
    if length(trim(attributes))!=0 then
        assert(find('=',attributes),"'=' missing from attributes",nFrames:=nFrames)
    end if
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
--  5   gButton(attributes)             -- (see note in docs)
--  6   gButton(title)
--  7   gButton(title,click)
--  8   gButton(title,attributes[,args])
--  9   gButton(title,click,attributes[,args])
--
-- (with [title=NULL, click=NULL, attr="", args={}] defaults after "  ":)
--/*
constant tra = paranormalise_traa  -- (nb still>last_xpgui_rid here)
assert(paranormalise_traa(         NULL,NULL,"",{})=={NULL,NULL,"",{}})         -- 1
assert(paranormalise_traa(tra          ,NULL,"",{})=={NULL,tra,"",{}})          -- 2
assert(paranormalise_traa(tra,"x=1"         ,"",{})=={NULL,tra,"x=1",{}})       -- 3
assert(paranormalise_traa(tra,"x=1",{1}        ,{})=={NULL,tra,"x=1",{1}})      -- 3
assert(paranormalise_traa("x=1",{1}         ,"",{})=={NULL,NULL,"x=1",{1}})     -- 4
assert(paranormalise_traa("x=1"        ,NULL,"",{})=={"",NULL,"x=1",{}})        -- 5
assert(paranormalise_traa("title"      ,NULL,"",{})=={"title",NULL,"",{}})      -- 6
assert(paranormalise_traa("title",tra       ,"",{})=={"title",tra,"",{}})       -- 7
assert(paranormalise_traa(NULL,tra          ,"",{})=={NULL,tra,"",{}})          -- 7
assert(paranormalise_traa("title","x=1"     ,"",{})=={"title",NULL,"x=1",{}})   -- 8
assert(paranormalise_traa("title","x=1",{1}    ,{})=={"title",NULL,"x=1",{1}})  -- 8
assert(paranormalise_traa("title",tra,"x=1"    ,{})=={"title",tra,"x=1",{}})    -- 9
assert(paranormalise_traa("title",tra,"x=1",{1}   )=={"title",tra,"x=1",{1}})   -- 9
assert(paranormalise_traa(NULL,tra,"x=1"       ,{})=={NULL,tra,"x=1",{}})       -- 9
assert(paranormalise_traa(NULL,tra,"x=1",{1}      )=={NULL,tra,"x=1",{1}})      -- 9
--named parameters:
assert(paranormalise_traa(NULL,tra,"",{})=={NULL,tra,"",{}})    -- click:=tra
assert(paranormalise_traa(NULL,NULL,"x=1",{})=={NULL,NULL,"x=1",{}}) -- attributes:="x=1"
--*/

function paranormalise_qraa(object q, rid, sequence attributes, dword_seq args)
-- used by gTreeView([sequence q={},] [rtn rid=NULL,] string attributes="", sequence args={})
-- (where q is tree_nodes and rid is branchopen)  (See the docs for the full details)
-- Also gDropDown(), where q is options and rid is selected, "".
-- Also gTabs(), where q is children and rid is tabchange, "".
-- This routine is designed to crash on the slightest oddity, hopefully with a decent easily understood message.
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

function paranormalise_raa(object rid, sequence attributes, dword_seq args, bool bCheckRid=true)
-- used by gText([rtn rid=NULL,] string attributes="", sequence args={}) and gCanvas(), gDatePick(), 
--  gDetachBox(), gList(), and gSpinBox(). (See the docs for the full details)
-- This routine is designed to crash on the slightest oddity, hopefully with a decent easily understood message.
    integer nFrames = 3 -- (change this back to 1 to debug)
    if dword_seq(attributes) then
        -- assume gText(attributes,args)                                    -- (intended)
        --              ^ rid      ^ attributes                             -- (actual)
        asserteq(args,{},"original default",nFrames:=nFrames)
        args = attributes                                                   -- (verified dword_seq now)
        attributes = rid                                                    -- (verified string below)
        rid = NULL
    elsif string(rid) then
        -- assume gText(attributes)                                         -- (intended)
        --              ^ rid                                               -- (actual)
        asserteq(args,{},"original default",nFrames:=nFrames)
        attributes = rid                                                    -- (verified string below)      
        rid = NULL
    end if
    assert(integer(rid),"not integer rid",nFrames:=nFrames)
    if bCheckRid then
        assert(rtn(rid),"not rtn rid",nFrames:=nFrames)
    end if
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

function paranormalise_taa(nullable_string title, sequence attributes, dword_seq args)
-- used by gLabel([nullable_string title=NULL,] string attributes="", sequence args={}) (only)
-- This routine is designed to crash on the slightest oddity, hopefully with a decent easily understood message.
    integer nFrames = 3 -- (change this back to 1 to debug)
    if dword_seq(attributes) then
        -- assume gLabel(attributes,args)                                   -- (intended)
        --               ^ title    ^ attributes                            -- (actual)
        asserteq(args,{},"original default",nFrames:=nFrames)
        args = attributes                                                   -- (verified dword_seq now)
        attributes = title                                                  -- (verified string below)
        title = NULL
    elsif string(title) and attributes="" and find('=',title) then
        -- assume gText(attributes)                                         -- (intended)
        --              ^ title                                             -- (actual)
        asserteq(args,{},"original default",nFrames:=nFrames)
--      asserteq(attributes,"","original default",nFrames:=nFrames)
        attributes = title                                                  -- (verified string below)      
        title = NULL
    end if
--  assert(nullable_string(title),"not null_str title",nFrames:=nFrames)    -- (ple/autoverified)
    assert(string(attributes),"not string attributes",nFrames:=nFrames)
--  assert(dword_seq(args),nFrames:=nFrames)                                -- (ple/autoverified)
    if attributes="" then
        asserteq(args,{},"original default",nFrames:=nFrames)
    end if
    return {title,attributes,args}
end function

--tests (eg):
--  1   gLabel()
--  2   gLabel(title)
--  3   gLabel(attributes[,args])
--  4   gLabel(title,attributes[,args])
--/*
?"paranormalise_taa"
-- (with [title=NULL, attr="", args={}] defaults after "  ":)
assert(paranormalise_taa(      NULL,"",{})=={NULL,"",{}})       -- 1
assert(paranormalise_taa("t"       ,"",{})=={"t","",{}})        -- 2
assert(paranormalise_taa("x=1"     ,"",{})=={NULL,"x=1",{}})    -- 3
assert(paranormalise_taa("x=1",{1}    ,{})=={NULL,"x=1",{1}})   -- 3
assert(paranormalise_taa("t","x=1"    ,{})=={"t","x=1",{}})     -- 4
assert(paranormalise_taa("t","x=1",{1}   )=={"t","x=1",{1}})    -- 4
--named parameters:
assert(paranormalise_taa("t"  ,"",{})=={"t","",{}})   -- title:="t" [===2]
assert(paranormalise_taa(NULL,"x=1",{})=={NULL,"x=1",{}}) -- attributes:="x=1"
--*/

--with trace
--integer nptaab = 0
function paranormalise_ptaab(object parent, title, attr, args, bool bEsc)
--
-- used by gDialog(gdx child, [parent=NULL,] [string [title="",] attr=""[, sequence args={}]], bool bEsc=true)
-- (however the non-optional child is not actually passed here)  (See the docs for the full details)
-- This routine is designed to crash on the slightest oddity, hopefully with a decent easily understood message.
-- The code itself is a complete mindfuck, no other word for it, as it re-jigs one of 23 intended meanings into
-- the correct variable names, from the completely confusing counter-intuitive names they all just arrived in.
-- It is simultaneously utterly trivial, and yet guaranteed to lead you on a merry dance down the garden path.
-- Having some decent unit tests is way beyond being simply a good idea, and has become an absolute necessity.
-- There is probably some more validation that could be done, but my brain is already dribbling out of my ears.
--
    integer nFrames = 3 -- (change this back to 1 to debug)
--  integer nFrames = 1 -- (change this back to 1 to debug)
--nptaab += 1
    if not gdx(parent) then
        -- assume parent omitted, and at least one other parameter passed,
        -- and in fact the first **must** then be the (string) attr/title.
        assert(string(parent),"not string attr/title",nFrames:=nFrames)
        asserteq(bEsc,true,"original default",nFrames:=nFrames)
        if find('=',parent) then    -- assume parent is attr (and not title)
            if bool(title) then
                -- assume gDialog(child,attr,bEsc)                  -- (intended)
                --                parent^    ^title                 -- (actual)
                bEsc = title
            else
                -- assume gDialog(child,attr,args[,bEsc])           -- (intended)
                --                parent^  title^  ^attr            -- (actual)
                if bool(attr) then
                    bEsc = attr                                     -- (verified bool now)
                else -- (should be original default)
                    asserteq(attr,{},"and not bool bEsc",nFrames:=nFrames)
                end if
                if dword_seq(title) then
                    args = title                                    -- (verified dword_seq below)
--              elsif bool(title) then
--                  bEsc = title
                end if
            end if
            title = ""
            attr = parent                                           -- (verified string below)
        else -- assume parent is really title
            if string(title) then   -- attr
                if dword_seq(attr) then
                    if bool(args) then
                        bEsc = args
                    end if
                    args = attr
                elsif bool(attr) then
                    bEsc = attr
                end if
                attr = title
            else
                assert(bool(title),"don't understand what title is")
                asserteq(attr,"","and not dword_seq args",nFrames:=nFrames)
                bEsc = title
            end if
            title = parent
        end if
        parent = NULL
    elsif bool(title) then
        -- assume gDialog(child,parent,bEsc):                   -- (intended)
        --                             ^title                   -- (actual)
        asserteq(attr,"","original default)",nFrames:=nFrames)
        asserteq(args,{},"original default)",nFrames:=nFrames)
        asserteq(bEsc,true,"original default",nFrames:=nFrames)
        bEsc = title                                            -- (verified bool now)
        title = ""
    elsif find('=',title) then
        -- assume gDialog(child,parent,attr[,args][,bEsc]):     -- (intended)
        --                        title^     ^attr  ^args       -- (actual)
        if bool(attr) then
            bEsc = attr
        elsif dword_seq(attr) then
            if bool(args) then
                bEsc = args
            end if
            args = attr
        end if
        attr = title
        title = ""
    elsif bool(attr) then
        -- assume gDialog(child,parent,title,bEsc):     -- (intended)
        --                                   ^attr      -- (actual)
        asserteq(bEsc,true,"original default",nFrames:=nFrames)
        bEsc = attr                                             -- (verified bool now)
        attr = ""
    elsif bool(args) then
        -- assume gDialog(child,parent,title,attr,bEsc):        -- (intended)
        --                                        ^args         -- (actual)
        asserteq(bEsc,true,"original default",nFrames:=nFrames)
        bEsc = args                                             -- (verified bool now)
        args = {}
    end if
    assert(gdx(parent),"not gdx parent",nFrames:=nFrames)
    assert(string(title),"not string title",nFrames:=nFrames)
    assert(find('=',title)=0,"'=' in title (attr?)",nFrames:=nFrames)
    assert(string(attr),"not string attr",nFrames:=nFrames)
    assert(length(trim(attr))=0 or find('=',attr),"'=' missing from attr",nFrames:=nFrames)
    assert(dword_seq(args),"not dword_seq args",nFrames:=nFrames)
--  assert(bool(bEsc),nFrames:=nFrames)                         -- (ple/autoverified)
    if attr="" then
        asserteq(args,{},"original default",nFrames:=nFrames)
    end if
--?{parent,title,attr,args,bEsc,{nptaab}}
    return {parent,title,attr,args,bEsc}
end function

--tests (child omitted):
--  1   gDialog()
--  2   gDialog(attributes)                 -- (NB must contain >=1 '=')
--  3   gDialog(attributes,args)
--  4   gDialog(attributes,args,bEsc)
--  5   gDialog(attributes,bEsc)
--  6   gDialog(title)                              -- (NB no '=')
--  7   gDialog(title,attributes)                   --     "", verified
--  8   gDialog(title,attributes,args)
--  9   gDialog(title,attributes,args,bEsc)
--  10  gDialog(title,attributes,bEsc)
--  -   gDialog(bEsc)   -- (invalid)
--  11  gDialog(title,bEsc)                         -- (NB no '=')
--  12  gDialog(parent)
--  13  gDialog(parent,bEsc)
--  14  gDialog(parent,attributes)
--  15  gDialog(parent,attributes,bEsc)
--  16  gDialog(parent,attributes,args)
--  17  gDialog(parent,attributes,args,bEsc)
--  18  gDialog(parent,title)                       -- (NB no '=')
--  19  gDialog(parent,title,bEsc)                  --     ""
--  20  gDialog(parent,title,attributes)            --     "", verified 
--  21  gDialog(parent,title,attributes,bEsc)
--  22  gDialog(parent,title,attributes,args)
--  23  gDialog(parent,title,attributes,args,bEsc)
--/*
?"paranormalise_ptaab"
gdx p = xpg_add_control(DIALOG) -- [otherwise all the gdx() tests will fail]
-- (with [parent=NULL, title="", attr="", args={}, bEsc=true] defaults after "  ,":)
assert(paranormalise_ptaab(         NULL,"","",{},true)=={NULL,"","",{},true})          -- 1
assert(paranormalise_ptaab("x=1"        ,"","",{},true)=={NULL,"","x=1",{},true})       -- 2
assert(paranormalise_ptaab("x=1",{1}       ,"",{},true)=={NULL,"","x=1",{1},true})      -- 3
assert(paranormalise_ptaab("x=1",{1},false    ,{},true)=={NULL,"","x=1",{1},false})     -- 4
assert(paranormalise_ptaab("x=1",false     ,"",{},true)=={NULL,"","x=1",{},false})      -- 5
assert(paranormalise_ptaab("x"          ,"","",{},true)=={NULL,"x","",{},true})         -- 6
assert(paranormalise_ptaab("x","x=1"       ,"",{},true)=={NULL,"x","x=1",{},true})      -- 7
assert(paranormalise_ptaab("x","x=1",{1}      ,{},true)=={NULL,"x","x=1",{1},true})     -- 8
assert(paranormalise_ptaab("x","x=1",{1},false   ,true)=={NULL,"x","x=1",{1},false})    -- 9
assert(paranormalise_ptaab("x","x=1",false    ,{},true)=={NULL,"x","x=1",{},false})     -- 10
assert(paranormalise_ptaab("x",false       ,"",{},true)=={NULL,"x","",{},false})        -- 11
assert(paranormalise_ptaab(p            ,"","",{},true)=={p,"","",{},true})             -- 12
assert(paranormalise_ptaab(p,false         ,"",{},true)=={p,"","",{},false})            -- 13
assert(paranormalise_ptaab(p,"x=1"         ,"",{},true)=={p,"","x=1",{},true})          -- 14
assert(paranormalise_ptaab(p,"x=1",false      ,{},true)=={p,"","x=1",{},false})         -- 15
assert(paranormalise_ptaab(p,"x=1",{1}        ,{},true)=={p,"","x=1",{1},true})         -- 16
assert(paranormalise_ptaab(p,"x=1",{1},false     ,true)=={p,"","x=1",{1},false})        -- 17
assert(paranormalise_ptaab(p,"x"           ,"",{},true)=={p,"x","",{},true})            -- 18
assert(paranormalise_ptaab(p,"x",false        ,{},true)=={p,"x","",{},false})           -- 19
assert(paranormalise_ptaab(p,"x","x=1"        ,{},true)=={p,"x","x=1",{},true})         -- 20
assert(paranormalise_ptaab(p,"x","x=1",false     ,true)=={p,"x","x=1",{},false})        -- 21
assert(paranormalise_ptaab(p,"x","x=1",{1}       ,true)=={p,"x","x=1",{1},true})        -- 22
assert(paranormalise_ptaab(p,"x","x=1",{1},false      )=={p,"x","x=1",{1},false})       -- 23
assert(paranormalise_ptaab(NULL,false      ,"",{},true)=={NULL,"","",{},false})         -- (docs==13)
assert(paranormalise_ptaab("",false        ,"",{},true)=={NULL,"","",{},false})         -- (docs==6)
--named parameters:
assert(paranormalise_ptaab(p,"","",{},true)=={p,"","",{},true})       -- parent:=p
assert(paranormalise_ptaab(NULL,"x","",{},true)=={NULL,"x","",{},true}) -- title:="x"
assert(paranormalise_ptaab(NULL,"","",{},false)=={NULL,"","",{},false})   -- bEsc:=false
assert(paranormalise_ptaab(NULL,"","x=1",{},true)=={NULL,"","x=1",{},true}) -- attributes:="x=1"
--*/

--local procedure xpg_signal_connect(atom handle, string signal, rtn rid, atom user_data=NULL)
--14/6/23 id is not a gdx for menus... (should we assert??)
--local procedure xpg_signal_connect(atom handle, string signal, integer rid, gdx id)
local procedure xpg_signal_connect(atom handle, string signal, integer rid, integer id)
    -- note that g_signal_connect is defined in the GTK sources as a #define of
    -- g_signal_connect_data(....,NULL,0), and is not exported from the dll/so.
    atom r = c_func(g_signal_connect_data,{handle,signal,call_back({'+',rid}),id,NULL,0})
    assert(r>0)
end procedure

procedure xpg_Dialog(integer id)
    integer parent = parent_ids[id]
    atom handle
    if backend=GTK then
        handle = c_func(gtk_window_new,{GTK_WINDOW_TOPLEVEL}) 
        assert(handle!=NULL)
        xpg_setID(handle,id)
        if parent!=NULL then
            c_proc(gtk_window_set_transient_for,{handle,ctrl_handles[parent]}) 
        end if
        c_proc(gtk_widget_set_events,{handle,GDK_KBR_MASK})
        xpg_signal_connect(handle,"key_press_event",xpg_gtk_check_escape,id)
        xpg_signal_connect(handle,"button-press-event",xpg_gtk_click,id)
        xpg_signal_connect(handle,"button-release-event",xpg_gtk_click,id)
        xpg_signal_connect(handle,"focus-in-event",xpg_gtk_focusinout,id)
        xpg_signal_connect(handle,"focus-out-event",xpg_gtk_focusinout,id)
        xpg_signal_connect(handle,"motion-notify-event",xpg_gtk_mousemove,id)
        xpg_signal_connect(handle,"configure-event",xpg_gtk_configure_event,id)
        c_proc(gtk_widget_realize,{handle})
    elsif backend=WinAPI then
        -- (aside: ctrl_types[id] is already DIALOG)
        atom d = CW_USEDEFAULT, -- (==#80000000)
             dwStyle = WS_OVERLAPPEDWINDOW
        if gGetInt(id,"RESIZE",true) then dwStyle += WS_MINMAXTHICK end if
--CS_OWNDC?
        handle = xpg_WinAPI_create(id,"xpGUI","",parent,d,d,dwStyle,WS_EX_ACCEPTFILES)
--?{"xpg_DIALOG",xpg_get_window_rect(id)}
--      atom mainDC = c_func(xGetDC,{handle})
--      ctrl_xtra[id][CE_MAINDC] = mainDC
--SUG: naturalsize for a dialog is windowsize-clientsize?? (or just rely on clientsize)
    else
        ?9/0 -- (unknown backend)
    end if
end procedure

function xpg_set_dialog_attribute(gdx id, string name, object v, bool bMapped)
    atom handle = ctrl_handles[id]
    if name="TITLE" then
        assert(string(v))
        if not bMapped then
            xpg_defer_attr(id,name,v)
        elsif backend=GTK then
            c_proc(gtk_window_set_title,{handle,v}) 
        elsif backend=WinAPI then
            c_proc(xSetWindowText,{handle,v})
        else
            ?9/0 -- (unknown backend)
        end if
        return true
--/*
    elsif name="DEFAULTENTER" then
       or name="DEFAULTESC" then
        assert(v=NULL or v=XPG_CLOSE or v=XPG_IGNORE or (gdx(v) and ctrl_types[v]=BUTTON))
        integer dx = iff(name="DEFAULTENTER"?CX_DEF_ENTER:CX_DEF_ESCAPE)
        ctrl_xtra[id][dx] = v
--      printf(1,"gSetAttribute(DIALOG,\"%s\",%v)...\n",{name,v})
        return true
--*/
    elsif name="RESIZE" then
        if string(v) then v = xpg_to_bool(v) end if
        xpg_set_ctrl_flag(id,CF_RESIZE,v)
        return true
--RESIZE:
--/*
  if (iupAttribGet(ih, "TITLE"))
    has_titlebar = 1;
  if (iupAttribGetBoolean(ih, "RESIZE")) {
    functions   |= GDK_FUNC_RESIZE;
    decorations |= GDK_DECOR_RESIZEH;

    decorations |= GDK_DECOR_BORDER;  /* has_border */
  } else {
    iupAttribSet(ih, "MAXBOX", "NO");
  } 
  if (iupAttribGetBoolean(ih, "MENUBOX")) {
    functions   |= GDK_FUNC_CLOSE;
    decorations |= GDK_DECOR_MENU;
    has_titlebar = 1;
  }
  if (iupAttribGetBoolean(ih, "MAXBOX")) {
    functions   |= GDK_FUNC_MAXIMIZE;
    decorations |= GDK_DECOR_MAXIMIZE;
    has_titlebar = 1;
  }
  if (iupAttribGetBoolean(ih, "MINBOX")) {
    functions   |= GDK_FUNC_MINIMIZE;
    decorations |= GDK_DECOR_MINIMIZE;
    has_titlebar = 1;
  }
  if (has_titlebar) {
    functions   |= GDK_FUNC_MOVE;
    decorations |= GDK_DECOR_TITLE;
    gtk_window_set_title((GtkWindow*)ih->handle, "");
  }
  if (iupAttribGetBoolean(ih, "BORDER") || has_titlebar)
    decorations |= GDK_DECOR_BORDER;  /* has_border */

  if (decorations == 0)
    gtk_window_set_decorated((GtkWindow*)ih->handle, FALSE);
  else if (!iupAttribGetBoolean(ih, "HIDETITLEBAR")) {
    GdkWindow* window = iupgtkGetWindow(ih->handle);
    if (window) {
      gdk_window_set_decorations(window, (GdkWMDecoration)decorations);
      gdk_window_set_functions(window, (GdkWMFunction)functions);
    }
  }

  title = iupAttribGet(ih, "TITLE"); 
  if (title)
    has_titlebar = 1;

  if (iupAttribGetBoolean(ih, "RESIZE")) {
    dwStyle |= WS_THICKFRAME;
    has_border = 1;
  } else {
    iupAttribSet(ih, "MAXBOX", "NO");
  } 
  if (iupAttribGetBoolean(ih, "MENUBOX")) {
    dwStyle |= WS_SYSMENU;
    has_titlebar = 1;
  }
  if (iupAttribGetBoolean(ih, "MAXBOX")) {
    dwStyle |= WS_MAXIMIZEBOX;
    has_titlebar = 1;
  }
  if (iupAttribGetBoolean(ih, "MINBOX")) {
    dwStyle |= WS_MINIMIZEBOX;
    has_titlebar = 1;
  }
  if (iupAttribGetBoolean(ih, "BORDER") || has_titlebar)
    has_border = 1;

  native_parent = iupDialogGetNativeParent(ih);

  if (native_parent) {
      if (iupAttribGetBoolean(ih, "SAVEUNDER"))
        classname = TEXT("IupDialogSaveBits");

      dwStyle |= WS_POPUP;

      if (has_titlebar)
        dwStyle |= WS_CAPTION;
      else if (has_border)
        dwStyle |= WS_BORDER;
  } else {
    if (has_titlebar) {
        dwStyle |= WS_OVERLAPPED;
    } else {
        if (has_border)
          dwStyle |= WS_POPUP | WS_BORDER;
        else
          dwStyle |= WS_POPUP;

        dwExStyle |= WS_EX_NOACTIVATE; /* this will hide it from the taskbar */ 
    }
  }
  if (iupAttribGetBoolean(ih, "DIALOGFRAME") && native_parent)
    dwExStyle |= WS_EX_DLGMODALFRAME;  /* this will hide the MENUBOX but not the close button */

--*/
    end if
    return false
end function

function xpg_get_dialog_attribute(gdx id, string name, object dflt)
    atom handle = ctrl_handles[id]
    if name="TITLE" then
        bool bMapped = and_bits(ctrl_flags[id],CF_MAPPED)!=0
        if not bMapped then
            if dflt=999_999_999 then dflt = "" end if
        elsif backend=GTK then
            return peek_string(c_func(gtk_window_get_title,{handle}))
        elsif backend=WinAPI then
            integer l = c_func(xGetWindowTextLength,{handle})
            string title = repeat(' ',l)
            c_proc(xGetWindowText,{handle,title,l+1})
            return title
        else
            ?9/0 -- (unknown backend)
        end if
    elsif name="RESIZE" then
        return and_bits(ctrl_flags[id],CF_RESIZE)
--/*
    elsif name="DEFAULTENTER" then
       or name="DEFAULTESC" then
        integer dx = iff(name="DEFAULTENTER"?CX_DEF_ENTER:CX_DEF_ESCAPE)
        return ctrl_xtra[id][dx]
--*/
    end if
    return xpg_return_default_attr(id,name,dflt)
end function

global function gDialog(gdx child, object parent=NULL, title="", attributes="", args={}, bool bEsc=true)
    {parent,title,attributes,args,bEsc} = paranormalise_ptaab(parent,title,attributes,args,bEsc)
    if not bInit then xpg_Init() end if
    assert(parent=NULL or ctrl_types[parent]=DIALOG)
    assert(child=NULL or ctrl_types[child]!=DIALOG)
    integer flags = iff(bEsc?CF_CLOSE_ON_ESC:0)+CF_NEVER_SHOWN+CF_RESIZE+CF_UNMAPATTR,
               id = xpg_add_control(DIALOG,parent,flags,bHasChildren:=true)
    if child then
        parent_ids[child] = id
        children_ids[id] = {child}
    end if
--DEV not yet implemented anywhere...
    xpg_register_handler(DIALOG,"CLOSE",{{1,1,"FO"}})
--  xpg_register_handler(DIALOG,"RESIZE",{{3,3,"FOII"}})
    set_ctrl_msg(DIALOG,xpg_Dialog,xpg_set_dialog_attribute,
                                   xpg_get_dialog_attribute)
    object cxi = repeat(0,CX_DLG_LEN)
    cxi[CX_LAST_FOCUS] = UNDEFINED
    ctrl_xtra[id] = cxi
    gSetAttribute(id,"FONT","Helvetica, 9")
    if length(title) then
        gSetAttribute(id,"TITLE",title)
    end if
    if length(attributes) then
        gSetAttributes(id,attributes,args)
    end if
    return id
end function

local procedure xpg_Box(gdx id)
    -- (invoked via xpg_map)
    gdx parent = xpg_get_parent_id(id)
    atom box = NULL
--  bool bVert = and_bits(ctrl_flags[id],CF_VERTICAL)!=0
    if backend=GTK then
--      integer f = iff(bVert?gtk_vbox_new:gtk_hbox_new),
--              spacing = 0
--?{f,id,ctrl_flags[id],CF_VERTICAL,gtk_vbox_new,gtk_hbox_new}
--      box = c_func(f,{false,spacing})
        box = c_func(gtk_fixed_new,{})
--DEV... (this is like HTML margin?)
--      c_proc(gtk_container_set_border_width,{box,10})
        xpg_gtk_add_to(parent,box)
        xpg_setID(box,id)
        c_proc(gtk_widget_realize,{box})
    elsif backend=WinAPI then
        -- (under winAPI boxes are virtual)
        ctrl_flags[id] = or_bits(ctrl_flags[id],CF_MAPPED)
    else
        ?9/0 -- (unknown backend)
    end if
end procedure

function xpg_set_box_attribute(gdx id, string name, object v, bool bMapped)
--  atom handle = ctrl_handles[id]
--DEV... (I appear to have commented it out in the docs...)
--  if name="ALIGNMENT" then
----Possible values: "ATOP", "ACENTER", "ABOTTOM". Default: "ATOP". 
--      printf(1,"gSetAttribute(BOX,\"%s\",%v)...\n",{name,v})
--      return true
--  elsif name="RADIO" then
--      printf(1,"gSetAttribute(BOX,\"%s\",%v)...\n",{name,v})
--      return true
--  elsif name="NORMALIZESIZE" then
--      printf(1,"gSetAttribute(BOX,\"%s\",%v)...\n",{name,v})
--      return true
    if name="GAP" then
        printf(1,"gSetAttribute(BOX,\"%s\",%v)...\n",{name,v})
        return true
    elsif name="SPACE" then
--DEV may as well test these both ways round... [DONE]
--      if integer(v) then v = gGetBoxSpacingName(v) end if
--      printf(1,"gSetAttribute(BOX,\"SPACE\",%v)...\n",{v})
        if string(v) then
            integer k = find(v,box_spacing_descs)
            assert(k!=0,`gSetAttribute(BOX,"SPACE","%s")`,{v})
            v = box_spacing_masks[k]
        end if
        ctrl_xtra[id][CX_BOX_SPACE] = v
--      string s = box_spacing_descs[find(v,box_spacing_masks)]
--      printf(1,"gSetAttribute(BOX,\"SPACE\",%s)..\n",{s})
        return true
    end if
    return false
end function

function xpg_get_box_attribute(gdx id, string name, object dflt)
    if name="SPACE" then
        return ctrl_xtra[id][CX_BOX_SPACE]
    elsif name="ORIENTATION" then
        return iff(and_bits(ctrl_flags[id],CF_VERTICAL)?"VERTICAL":"HORIZONTAL")
    end if
    return xpg_return_default_attr(id,name,dflt)
end function

local function gBox(integer hv, sequence children, string attributes, sequence args)
    if not bInit then xpg_Init() end if
    integer id = xpg_add_control(BOX)
--  xpg_register_handler(BOX,"XXX",{{1,1,"PO"}})
    set_ctrl_msg(BOX,xpg_Box,xpg_set_box_attribute,
                             xpg_get_box_attribute)
    if hv='V' then ctrl_flags[id] = CF_VERTICAL end if
    object cxi = repeat(0,CX_BOX_LEN)
--DEV...
--  cxi[CX_BOX_MARGIN] = repeat(0,4)
    cxi[CX_BOX_SPACE] = XPG_SPACE_RIGHT
    ctrl_xtra[id] = cxi
    cxi = 0 -- (kill refcount)
    children_ids[id] = children
    for child in children do
        parent_ids[child] = id
    end for
    if length(attributes) then
        gSetAttributes(id,attributes,args)
    end if
    return id
end function 

--complete bust:
--local function xpg_gtk_widget_realized(atom handle, gdx id) -- (GTK only)
--  -- (GTK only) save the natural sizes, once they're actually non-zero.
----?{"realised",id}
--  assert(id=xpg_getID(handle))
--  c_proc(gtk_widget_get_allocation,{handle,pRECT})
--  integer width = get_struct_field(idGdkRectangle,pRECT,"width"),
--         height = get_struct_field(idGdkRectangle,pRECT,"height")
--?{"realized",id,width,height}
----DEV a frame may want special handling... (6 wide and char height+3 high?)
--  ctrl_size[id][SZ_NATURAL_W] = width
--  ctrl_size[id][SZ_NATURAL_H] = height
--  return false
--end function

--local function xpg_gtk_widget_mapped(atom handle, event, gdx id) -- (GTK only)
--  -- (GTK only) save the natural sizes, once they're actually non-zero.
----?{"mapped",id,event}
----    assert(id=xpg_getID(handle))
--integer hid = xpg_getID(handle)
--atom hih = ctrl_handles[hid]
--  c_proc(gtk_widget_get_allocation,{handle,pRECT})
--  integer width = get_struct_field(idGdkRectangle,pRECT,"width"),
--         height = get_struct_field(idGdkRectangle,pRECT,"height")
--?{"mapped",id,handle,{hid,hih},width,height}
--?ctrl_handles
----DEV a frame may want special handling... (6 wide and char height+3 high?)
--  ctrl_size[id][SZ_NATURAL_W] = width
--  ctrl_size[id][SZ_NATURAL_H] = height
--  return false
--end function

--atom Xbutton -- DEV/temp

local procedure xpg_Button(gdx id)
    -- (invoked via xpg_map)
    gdx parent = xpg_get_parent_id(id)
    atom button
    nullable_string title = gGetAttribute(id,"TITLE",NULL)
--  if title=NULL then title="" end if
--  sequence split_title = split(xpg_demnemonicalize(title),'\n')
--  integer {w,h} = gGetTextExtent(parent,split_title)
--test_elem = id
    if backend=GTK then
--DOCS, maybe (I made this up) note the child of a dialog should normally be a vbox or hbox; should
--      you create, for instance, a dialog with a single button child, it will expand to fill the
--      entire dialog.
        button = c_func(gtk_button_new_with_mnemonic,{title})
        c_proc(gtk_widget_set_events,{button,GDK_KBR_MASK})
        xpg_setID(button,id)
        xpg_gtk_add_to(parent,button)
--GDK_STRUCTURE_MASK
--      c_proc(gtk_widget_set_events,{button,GDK_KBR_MASK})
        xpg_signal_connect(button,"clicked",xpg_gtk_button_clicked,id)
--      xpg_signal_connect(button,"realize",xpg_gtk_widget_realized,id)
--      xpg_signal_connect(button,"map",xpg_gtk_widget_mapped,id)
--      xpg_signal_connect(button,"map-event",xpg_gtk_widget_mapped,id)
        xpg_signal_connect(button,"focus-in-event",xpg_gtk_focusinout,id)
        xpg_signal_connect(button,"focus-out-event",xpg_gtk_focusinout,id)

--X As a (temporary) workaround, I once stuck a v/hbox in:
--      atom vbox = c_func(gtk_vbox_new,{false,6})
--      atom hbox = c_func(gtk_hbox_new,{false,6})
--      c_proc(gtk_box_pack_start,{hbox,button,false,false,0})
--      c_proc(gtk_box_pack_start,{vbox,hbox,false,false,0})
--      c_proc(gtk_container_add,{ctrl_handles[parent],vbox})
        c_proc(gtk_widget_realize,{button})
--      title = xpg_demnemonicalize(title)
--if bGTK3 then
--  c_proc(gtk_widget_get_preferred_size,{button,NULL,pGtkRequisition})
--  c_proc(gtk_widget_get_preferred_size,{button,pGtkRequisition,pGtkRequisition})
--Xbutton = button
--end if
--  c_proc(gtk_widget_get_allocation,{button,pRECT})
--?"gwga"
--  ?{get_struct_field(idGdkRectangle,pRECT,"width"),
--    get_struct_field(idGdkRectangle,pRECT,"height")}
--else
--if not bGTK3 then
--  c_proc(gtk_widget_size_request,{button,pGtkRequisition})
--  integer w = get_struct_field(idGtkRequisition,pGtkRequisition,"width"),
--          h = get_struct_field(idGtkRequisition,pGtkRequisition,"height")
--  ?{w,h}
--end if
--?{"button",w,h,title}
--idGtkRequisition
--/*
static void iButtonComputeNaturalSizeMethod(Ihandle* ih, int *w, int *h, int *children_expand)
{
  int has_border = 1;
  int natural_w = 0,
      natural_h = 0,
--    type = ih->data->type;
  (void)children_expand; /* unset if not a container */

--  if (!ih->handle)
--  {
--  /* if not mapped must initialize the internal values */
--  char* value = iupAttribGet(ih,"IMAGE");
--  if (value)
--  {
--    char* title = iupAttribGet(ih,"TITLE");
--    type = IUP_BUTTON_IMAGE;
--    if (title && *title!=0)
--      type |= IUP_BUTTON_TEXT;
--  }
--  else
--    type = IUP_BUTTON_TEXT;
--  }

  if (type & IUP_BUTTON_IMAGE)
  {
    iupImageGetInfo(iupAttribGet(ih,"IMAGE"),&natural_w,&natural_h,NULL);

    if (type & IUP_BUTTON_TEXT)
    {
      int text_w, text_h;
      char* title = iupAttribGet(ih,"TITLE");
      char* str = iupStrProcessMnemonic(title,NULL,0);   /* remove & */
      iupdrvFontGetMultiLineStringSize(ih,str,&text_w,&text_h);
      if (str && str != title) free(str);

      if (ih->data->img_position == IUP_IMGPOS_RIGHT ||
          ih->data->img_position == IUP_IMGPOS_LEFT)
      {
        natural_w += text_w + ih->data->spacing;
        natural_h = iupMAX(natural_h,text_h);
      }
      else
      {
        natural_w = iupMAX(natural_w,text_w);
        natural_h += text_h + ih->data->spacing;
      }
    }
  }
  else /* IUP_BUTTON_TEXT only */
  {
    char* title = iupAttribGet(ih,"TITLE");
    char* str = iupStrProcessMnemonic(title,NULL,0);     /* remove & */
    iupdrvFontGetMultiLineStringSize(ih,str,&natural_w,&natural_h);
    if (str && str!=title) free(str);
  }

  if (ih->data->type & IUP_BUTTON_IMAGE &&
      iupAttribGet(ih,"IMPRESS") &&
      !iupAttribGetBoolean(ih,"IMPRESSBORDER"))
    has_border = 0;

  if (has_border)
    iupdrvButtonAddBorders(&natural_w,&natural_h);

  natural_w += 2*ih->data->horiz_padding;
  natural_h += 2*ih->data->vert_padding;

  *w = natural_w;
  *h = natural_h;
}
static int winButtonGetBorder(void)
{
  return 4;
}

void iupdrvButtonAddBorders(int *x, int *y)
{
  /* LAYOUT_DECORATION_ESTIMATE */
  int border_size = winButtonGetBorder() * 2;
  (*x) += border_size;
  (*y) += border_size;
}
void iupdrvButtonAddBorders(int *x, int *y)
{
  /* LAYOUT_DECORATION_ESTIMATE */
#ifdef WIN32
  int border_size = 2*5;
#else
#ifdef HILDON
  int border_size = 2*7+1; /* borders are not symmetric */
#else
  int border_size = 2*5+1; /* borders are not symmetric */
#endif
#endif
  (*x) += border_size;
  (*y) += border_size;
}

--*/
    elsif backend=WinAPI then
--      sequence stitle = split(title,'\n')
        atom dwStyle = or_all({WS_CHILD,WS_VISIBLE,BS_PUSHBUTTON,WS_TABSTOP})
--      if gGetAttribute(id,"FLAT",false) then dwStyle ||= BS_FLAT end if
--             {w,h} = xpg_set_button_natural_size(id,title)
--    if string(text) then text = split(text,'\n') end if
--             {w,h} = gGetTextExtent(parent,split(title,'\n'))
--             {w,h} = gGetTextExtent(parent,split(xpg_demnemonicalize(title),'\n'))
--?{"winbtn",w,h,title}
--      h *= length(stitle)
--      ctrl_size[id][SZ_NATURAL_W] = w
--      ctrl_size[id][SZ_NATURAL_H] = h
--      button = xpg_WinAPI_create(id,"button",title,parent,w,h,dwStyle,0)
--      button = xpg_WinAPI_create(id,"button",title,parent,w+7,h+10,dwStyle,0)
        button = xpg_WinAPI_create(id,"button",title,parent,50,25,dwStyle,0)
    else
        ?9/0 -- (unknown backend)
    end if
--?xpg_get_window_rect(id) -- {0,0,0,0}
end procedure

local function xpg_set_button_attribute(gdx id, string name, object v, bool bMapped)
    atom handle = ctrl_handles[id]
    if name="TITLE" then
        assert(string(v))
        if not bMapped then
            xpg_defer_attr(id,name,v)
        elsif backend=GTK then
            c_proc(gtk_button_set_label,{handle,xpg_gtk_mnemonicalize(v)})
        elsif backend=WinAPI then
            c_proc(xSetWindowText,{handle,v})
--          xpg_set_button_natural_size(id,v)
        else
            ?9/0 -- (unknown backend)
        end if
        return true -- (dealt with)
--/*
    elsif name="PADDING" then
static int gtkButtonSetPaddingAttrib(Ihandle* ih, const char* value)
{
  iupStrToIntInt(value,&ih->data->horiz_padding,&ih->data->vert_padding,'x');
  if (ih->handle)
  {
#if GTK_CHECK_VERSION(3,4,0)
    iupgtkSetMargin(ih->handle,ih->data->horiz_padding,ih->data->vert_padding,0);
void iupgtkSetMargin(GtkWidget* widget,int horiz_padding,int vert_padding,int mandatory_gtk3)
{
  if (mandatory_gtk3)
  {
    gtk_widget_set_margin_top(widget,vert_padding);
    gtk_widget_set_margin_bottom(widget,vert_padding);
    gtk_widget_set_margin_start(widget,horiz_padding);
    gtk_widget_set_margin_end(widget,horiz_padding);
  }
  (void)widget;
  (void)horiz_padding;
  (void)vert_padding;
  (void)mandatory_gtk3;
}

#else
    if (ih->data->type == IUP_BUTTON_TEXT)   /* text only */
    {
      GtkMisc* misc = (GtkMisc*)gtk_bin_get_child((GtkBin*)ih->handle);
      gtk_misc_set_padding(misc,ih->data->horiz_padding,ih->data->vert_padding);
//void gtk_misc_set_padding(GtkMisc* misc, gint xpad, gint ypad)
    }
    else
    {
      GtkAlignment* alignment = (GtkAlignment*)gtk_bin_get_child((GtkBin*)ih->handle);
      if (GTK_IS_ALIGNMENT(alignment))
        gtk_alignment_set_padding(alignment,ih->data->vert_padding,ih->data->vert_padding,
                                            ih->data->horiz_padding,ih->data->horiz_padding);
    }
#endif
    return 0;
  }
  else
    return 1; /* store until not mapped, when mapped will be set again */
}

--*/
--/*
    elsif name="FLAT" then
        if string(v) then v = xpg_to_bool(v) end if
        if not bMapped then
            xpg_defer_attr(id,name,v)
        elsif backend=GTK then
?9/0
--          c_proc(gtk_button_set_label,{handle,xpg_gtk_mnemonicalize(v)})
        elsif backend=WinAPI then
            atom dwStyle = c_func(xGetWindowLong,{handle,GWL_STYLE})
            if not and_bits(dwStyle,BS_FLAT) then
                dwStyle ||= BS_FLAT
                dwStyle = c_func(xSetWindowLong,{handle,GWL_STYLE,dwStyle})
--else
--  ?"FLATas"
            end if
--          c_proc(xSetWindowText,{handle,v})
--          xpg_set_button_natural_size(id,v)
        else
            ?9/0 -- (unknown backend)
        end if
        return true -- (dealt with)
--*/
    end if
    return false
end function

local function xpg_get_button_attribute(gdx id, string name, object dflt)
    atom handle = ctrl_handles[id]
    if name="TITLE" then
        if backend=GTK then
            return peek_string(c_func(gtk_button_get_label,{handle}))
        elsif backend=WinAPI then
            integer l = c_func(xGetWindowTextLength,{handle})
            string title = repeat(' ',l)
            c_proc(xGetWindowText,{handle,title,l+1})
            return title
        else
            ?9/0 -- (unknown backend)
        end if
--/*
    elsif name="FLAT" then
        if backend=GTK then
?9/0
--          return peek_string(c_func(gtk_button_get_label,{handle}))
        elsif backend=WinAPI then
            atom dwStyle = c_func(xGetWindowLong,{handle,GWL_STYLE})
            return and_bits(dwStyle,BS_FLAT)!=0
--xGetWindowLong
--          integer l = c_func(xGetWindowTextLength,{handle})
--          string title = repeat(' ',l)
--          c_proc(xGetWindowText,{handle,title,l+1})
--          return title
        else
            ?9/0 -- (unknown backend)
        end if
--*/
    end if
    return xpg_return_default_attr(id,name,dflt)
end function

--with trace
--global function gButton(nullable_string title=NULL, object action=NULL, sequence attributes="", args={})
global function gButton(object title=NULL, action=NULL, sequence attributes="", args={})
    {title,action,attributes,args} = paranormalise_traa(title,action,attributes,args)
    if not bInit then xpg_Init() end if
    integer id = xpg_add_control(BUTTON)
    xpg_register_handler(BUTTON,"ACTION",{{1,1,"FO"}})
    set_ctrl_msg(BUTTON,xpg_Button,xpg_set_button_attribute,
                                   xpg_get_button_attribute)
--  gSetAttribute(id,"FONT","Helvetica, 9")
    if title!=NULL then
        gSetAttribute(id,"TITLE",title)
    end if
    if action!=NULL then
        gSetHandler(id,"ACTION",action)
    end if
    if length(attributes) then
        gSetAttributes(id,attributes,args)
    end if
--  gSetAttribute(id,"EXPAND",false)
--  gSetInt(id,"EXPAND",false)
    return id
end function 

--static gboolean gtk_draw(GtkWidget*widget,cairo_t*cr,gpointer data)
local function xpg_gtk_canvas_draw(atom canvas, cairo, gdx id) -- (GTK only)
    assert(id=xpg_getID(canvas))
    if not bGTK3 then
        atom window = c_func(gtk_widget_get_window,{canvas})
--?"cairo_create"
        cairo = c_func(gdk_cairo_create,{window})
    end if
    atom layout = c_func(pango_cairo_create_layout,{cairo})
    ctrl_xtra[id][CX_CANVAS_HDC] = {cairo,layout}
    atom back = gCanvasGetBackground(id),
      {r,g,b} = to_rgba(back)
    c_proc(cairo_set_source_rgb,{cairo,r/255,g/255,b/255})
    c_proc(cairo_paint,{cairo})
    integer redraw = gGetHandler(id,"REDRAW")
    if redraw then
--fOK = false
        redraw(id)
--fOK = true
        gCanvasSetBackground(id,back)
    end if
    if not bGTK3 then
        ctrl_xtra[id][CX_CANVAS_HDC] = {NULL,NULL}
--?"cairo_destroy"
        c_proc(cairo_destroy,{cairo})
    end if
    c_proc(xg_object_unref,{layout})
    return true
end function

local procedure xpg_set_canvas_pen(gdx id, atom hdc, bool bBack=false)
    integer penstyle = ctrl_xtra[id][CX_PENSTYLE],
            penwidth = ctrl_xtra[id][CX_PENWIDTH],
           pencolour = ctrl_xtra[id][CX_CANVAS_FORE]
    if backend=GTK then
        c_proc(cairo_set_line_width,{hdc,penwidth})
--DEV...
        if penstyle=XPG_CONTINUOUS then
            c_proc(cairo_set_dash,{hdc,0,0,0})
        else
            sequence dashes = {{6,2},{2,2},{6,2,2,2},{6,2,2,2,2,2}}[penstyle]
            atom pDash = xpg_double_array(dashes)
            c_proc(cairo_set_dash,{hdc,pDash,length(dashes),0})
            free(pDash)
        end if
        if bBack then pencolour = gCanvasGetBackground(id) end if
        atom {r,g,b} = to_rgba(pencolour)
        c_proc(cairo_set_source_rgb,{hdc,r/255,g/255,b/255})
    elsif backend=WinAPI then
        if and_bits(ctrl_xtra[id][CX_CANVAS_FLAGS],CXCF_REPEN) then
            atom bgr = xpg_WinAPI_rgb_to_bgr(pencolour),
                 hPen = c_func(xCreatePen,{penstyle,penwidth,bgr})
            hPen = c_func(xSelectObject,{hdc,hPen})
            c_proc(xDeleteObject,{hPen})
            ctrl_xtra[id][CX_CANVAS_FLAGS] -= CXCF_REPEN
        end if
    else
        ?9/0 -- (unknown backend)
    end if
end procedure

local function xpg_gtk_get_cairo(gdx id, bool bBack=false)
    atom cairo = ctrl_xtra[id][CX_CANVAS_HDC][1]
    xpg_set_canvas_pen(id,cairo,bBack)
    return cairo
end function

--function gCanvasArc(ctx, xc, yc, w, h, angle1, angle2) {
--//    let ch = ctx.canvas.clientHeight;
--  let ch = ctx.canvas.clientHeight - yc;
--  if (xc > 0 && ch > 0 && w >= 2 && h >= 2) {
--      ctx.beginPath();
--//    ctx.arc(xc,ch-yc,w/2,angle1*Math.PI/180,angle2*Math.PI/180);
--//    ctx.ellipse(xc,ch-yc,w/2,h/2,0,angle1*Math.PI/180,angle2*Math.PI/180);
--      ctx.ellipse(xc,ch,w/2,h/2,0,angle1*Math.PI/180,angle2*Math.PI/180);
--      ctx.stroke();
--  }
--}

--function gCanvasSector(ctx, xc, yc, w, h, angle1, angle2, sector=true) {
--  let ch = ctx.canvas.clientHeight,
--      fs = ctx.fillStyle;
--  ctx.fillStyle = ctx.strokeStyle;
--  ctx.beginPath();
--//void ctx.arc(x, y, radius, startAngle, endAngle [, counterclockwise]);
--//void ctx.ellipse(x, y, radiusX, radiusY, rotation, startAngle, endAngle [, counterclockwise]);
--//    ctx.arc(xc,ch-yc,w/2,angle1*Math.PI/180,angle2*Math.PI/180);
--  ctx.ellipse(xc,ch-yc,w/2,h/2,0,(360-angle2)*Math.PI/180,(360-angle1)*Math.PI/180);
--  if (sector) { ctx.lineTo(xc,ch-yc); } // else chord
--  ctx.fill();
--  ctx.fillStyle = fs;
--}
--
--function gCanvasChord(ctx, xc, yc, w, h, angle1, angle2) {
--  cdCanvasSector(ctx,xc,yc,w,h,angle1,angle2,false);
--}
--

--DEV do I need to set CXCF_REPEN any more???
global procedure gCanvasSetLineWidth(gdx canvas, atom width)
    ctrl_xtra[canvas][CX_PENWIDTH] = width
--  if width!=1 then
--      ctrl_xtra[canvas][CX_PENSTYLE] = XPG_CONTINUOUS
--  end if
    ctrl_xtra[canvas][CX_CANVAS_FLAGS] ||= CXCF_REPEN
end procedure

global function gCanvasGetLineWidth(gdx canvas)
    return ctrl_xtra[canvas][CX_PENWIDTH]
end function

global procedure gCanvasSetLineStyle(gdx canvas, object style)
    if sequence(style) then
        for b in style do
            assert(b=0 or b=1)
        end for
        assert(style[1]=1 and style[$]=0)
        dashers[XPG_CUSTOM_DASH] = style
        style = XPG_CUSTOM_DASH
    end if
--DEV
--  if atom(dgx) then
    ctrl_xtra[canvas][CX_PENSTYLE] = style
--  if style!=XPG_CONTINUOUS then
--      ctrl_xtra[canvas][CX_PENWIDTH] = 1
--  end if
    ctrl_xtra[canvas][CX_CANVAS_FLAGS] ||= CXCF_REPEN
end procedure

global function gCanvasGetLineStyle(gdx canvas)
    integer style = ctrl_xtra[canvas][CX_PENSTYLE]
    return iff(style=XPG_CUSTOM_DASH?dashers[style]:style)
end function

--DEV temp:
--bool wuline = false
local constant wuline = true
--global procedure gwu(bool w) wuline = w end procedure

--integer gdots = 0, gten = 1000

integer rB, gB, bB, 
        rL, gL, bL

local procedure plot(gdx canvas, atom x, y, c, bool bFlip=false)
--  plot the pixel at (x, y) with brightness c (where 0 <= c <= 1)
--  note that bFlip is exclusively intended for wu_line() only.
    if bFlip then {x,y} = {y,x} end if
    atom C = 1-c, hDC, bgr
--  if backend=WinAPI then
    if false then
        hDC = ctrl_xtra[canvas][CX_CANVAS_HDC]
--DEV horribly slow... (but deal with that *after* gImage...)
        bgr = c_func(xGetPixel,{hDC,x,y})
        {rB,gB,bB} = to_rgba(bgr)
    end if
    integer r = and_bits(rL*c+rB*C,#FF),
            g = and_bits(gL*c+gB*C,#FF),
            b = and_bits(bL*c+bB*C,#FF)
--  c = rgba(r,g,b)
--  gCanvasPixel(canvas,x,y,c)
    if backend=GTK then
        atom cairo = ctrl_xtra[canvas][CX_CANVAS_HDC][1]
        c_proc(cairo_set_source_rgb,{cairo,r/255,g/255,b/255})
        c_proc(cairo_set_line_width,{cairo,0.5})
        c_proc(cairo_move_to,{cairo,x,y})
        c_proc(cairo_line_to,{cairo,x+1,y+1}) -- best I can do
        c_proc(cairo_stroke,{cairo})
    elsif backend=WinAPI then
        hDC = ctrl_xtra[canvas][CX_CANVAS_HDC]
        bgr = b*#10000+g*#100+r
--if gdots then
--  gdots -= 1
--  gten -= 1
--  if gten<2 then
--      printf(1,"gPixel(%d,%d,%08x [%f])\n",{x,y,bgr,c})
--      if gten=0 then gten=1000 end if
--  end if
--end if
        bool bOK = c_func(xSetPixelV,{hDC,x,y,bgr})
--      assert(bOK)
--      if not bOK then ?{"plot",x,y,bgr} end if
    else
        ?9/0
    end if
end procedure

procedure plot2(gdx canvas, atom x, y, f, xgap, integer width, bool bFlip)
    integer d = floor(width/2)
    plot(canvas,x,y-d,(1-f)*xgap,bFlip)
    if d=0 then
        plot(canvas,x,y+1,(f)*xgap,bFlip)
    else
        for i=-d+1 to d-1 do 
            plot(canvas,x,y+i,1,bFlip)
        end for
        plot(canvas,x,y+d,(f)*xgap,bFlip)
    end if
end procedure

local function fpart(atom x)
    return x-floor(x)   -- fractional part of x
end function

procedure wu_line(gdx canvas, atom x1, y1, x2, y2)
    bool flip := abs(y2-y1) > abs(x2-x1) -- (aka steep)
    if flip then
        {x1,y1,x2,y2} := {y1,x1,y2,x2}
    end if
    if x1>x2 then
        {x1,x2,y1,y2} := {x2,x1,y2,y1}
    end if
    
    atom dx := x2-x1,
         dy := y2-y1,
         gradient := iff(dx=0? 1 : dy/dx),
         x1end := round(x1),
         x2end := round(x2),
         y1end := y1 + gradient*(x1end-x1),
         y2end := y2 + gradient*(x2end-x2)

    integer dots = 1,
            width = ctrl_xtra[canvas][CX_PENWIDTH],
            style = ctrl_xtra[canvas][CX_PENSTYLE]
    sequence dasher = dashers[style+1]

    -- handle first endpoint
    plot2(canvas,x1end,floor(y1end),fpart(y1end),1-fpart(x1+0.5),width,flip)

    
    -- main loop
    atom intery := y1end + gradient -- first y-intersection for the main loop
    for x = x1end+1 to x2end-1 do
        dots = iff(dots=length(dasher)?1:dots+1)
        if dasher[dots] then
            plot2(canvas,x,floor(intery),fpart(intery),1,width,flip)
        end if
        intery += gradient
    end for

    -- handle second endpoint
    plot2(canvas,x2end,floor(y2end),fpart(y2end),fpart(x2+0.5),width,flip)

end procedure

global procedure gCanvasLine(gdx canvas, atom x1, y1, x2, y2, integer style=-1, width=-1, atom colour=-1)
    integer pstyle, pwidth
    if style!=-1 then pstyle = gCanvasGetLineStyle(canvas) gCanvasSetLineStyle(canvas,style) end if
--  if width!=-1 then pwidth = gCanvasGetLineWidth(canvas) gCanvasSetLineWidth(canvas,width) end if
    if width!=-1 then pwidth = ctrl_xtra[canvas][CX_PENWIDTH] gCanvasSetLineWidth(canvas,width) end if
    atom pcolour
    if colour!=-1 then
--      pcolour = gCanvasGetForeground(canvas)
        pcolour = ctrl_xtra[canvas][CX_CANVAS_FORE]
        gCanvasSetForeground(canvas,colour)
    end if
    bool bUseNative = x1=x2 or y1=y2
if wuline and not bUseNative then
    {rB,gB,bB} = to_rgba(gCanvasGetBackground(canvas))
    {rL,gL,bL} = to_rgba(gCanvasGetForeground(canvas))
    wu_line(canvas,x1,y1,x2,y2)
else
    if backend=GTK then
--?{"line",ctrl_xtra[id][CX_CANVAS_FLAGS]}
        atom cairo = xpg_gtk_get_cairo(canvas)
        c_proc(cairo_move_to,{cairo,x1,y1})
        c_proc(cairo_line_to,{cairo,x2,y2})
        c_proc(cairo_stroke,{cairo})
    elsif backend=WinAPI then
        atom hDC = ctrl_xtra[canvas][CX_CANVAS_HDC]
        xpg_set_canvas_pen(canvas,hDC)
        bool bOK = c_func(xMoveToEx,{hDC,x1,y1,NULL})
        assert(bOK!=0)
        bOK = c_func(xLineTo,{hDC,x2,y2})
        assert(bOK!=0)
    else
        ?9/0 -- (unknown backend)
    end if
end if
    if style!=-1 then gCanvasSetLineStyle(canvas,pstyle) end if
    if width!=-1 then gCanvasSetLineWidth(canvas,pwidth) end if
    if colour!=-1 then
        gCanvasSetForeground(canvas,pcolour)
    end if
end procedure

--global integer spew=0

procedure plot_4_points(gdx canvas, integer xc, yc, x, y, atom f, angle1=0, angle2=360, angle=0)
    integer x1 = xc+x, x2 = xc-x,
            y1 = yc+y, y2 = yc-y
--DEV angles have changed meaning, comments may be out of date/inverted...
--if angle1<0 or angle1>360 or angle2<0 or angle2>360 then ?9/0 end if
    if angle<0 or angle>90.01 then ?9/0 end if
    -- check for/draw 0..90, 180..90, 180..270, and 360..270, ie 2 clockwise and 2 counter-clockwise.
--DEV if filled then inside are respectively above/left, above/right, below/right, below/left.
    if      angle >=angle1 and      angle <=angle2 then plot(canvas,x1,y1,f) end if -- btm right
    if (180-angle)>=angle1 and (180-angle)<=angle2 then plot(canvas,x2,y1,f) end if -- btm left
    if (180+angle)>=angle1 and (180+angle)<=angle2 then plot(canvas,x2,y2,f) end if -- top left
    if (360-angle)>=angle1 and (360-angle)<=angle2 then plot(canvas,x1,y2,f) end if -- top right
--if spew then ?{angle1,angle2,angle} end if
end procedure

procedure wu_ellipse(gdx canvas, atom xc, yc, w, h, angle1=0, angle2=360)
--, integer width=1)
--procedure wu_ellipse(gdx canvas, atom xc, yc, h, w, angle1=0, angle2=360)
--procedure wu_ellipse(gdx canvas, atom yc, xc, w, h, angle1=0, angle2=360)
--
-- (draws a circle when w=h) credit:
-- https://yellowsplash.wordpress.com/2009/10/23/fast-antialiased-circles-and-ellipses-from-xiaolin-wus-concepts/
--
    if w<=0 or h<=0 then return end if
--  integer width = gCanvasGetLineWidth(canvas)
    xc = round(xc)
    yc = round(yc)
    w = round(w)
    h = round(h)
--if angle1<0 or angle1>360 or angle2<0 or angle2>360 then ?9/0 end if
    angle1 = mod(angle1,360)
    angle2 = mod(angle2,360)
--if angle1<0 or angle1>360 or angle2<0 or angle2>360 then ?9/0 end if

    -- Match gCanvasArc/Sector angles:
    angle1 = remainder(atan2((h/2)*sin(angle1*XPG_DEG2RAD),(w/2)*cos(angle1*XPG_DEG2RAD))*XPG_RAD2DEG+360,360)
    angle2 = remainder(atan2((h/2)*sin(angle2*XPG_DEG2RAD),(w/2)*cos(angle2*XPG_DEG2RAD))*XPG_RAD2DEG+360,360)

    if angle2<=angle1 then angle2 += 360 end if

    atom a := w/2, asq := a*a,
         b := h/2, bsq := b*b,
         sqab = sqrt(asq+bsq)
    if sqab then
        atom ffd = round(asq/sqab), -- forty-five-degree coord
             xj, yj, frc, flr, angle

        -- draw top right, and the 3 mirrors of it in horizontal fashion
        --  (ie 90 to 45 degrees for a circle)

        integer style = ctrl_xtra[canvas][CX_PENSTYLE],
                width = ctrl_xtra[canvas][CX_PENWIDTH],
                dots = 1, d = floor(width/2)
        sequence dasher = dashers[style+1]
--?{"width",width} -- 1
--DEV/SUG if filled then width=1...

        for xi=0 to ffd do
            dots = iff(dots=length(dasher)?1:dots+1)
            if dasher[dots] then
                yj := b*sqrt(1-xi*xi/asq)   -- real y value
                frc := fpart(yj)
                flr := floor(yj)
                angle := iff(xi=0?90:arctan(yj/xi)*XPG_RAD2DEG)
                plot_4_points(canvas,xc,yc,xi,flr-d,1-frc,angle1,angle2,angle) --,'l')
                if d=0 then
                    plot_4_points(canvas,xc,yc,xi,flr+1,frc,angle1,angle2,angle) --,'r')
                else
                    for i=-d+1 to d-1 do
                        plot_4_points(canvas,xc,yc,xi,flr+i,1,angle1,angle2,angle)
                    end for
                    plot_4_points(canvas,xc,yc,xi,flr+d,frc,angle1,angle2,angle)
                end if
            end if
        end for

        -- switch from horizontal to vertial mode for the rest, ditto 3
        -- (ie 45..0 degrees for a circle)

        ffd = round(bsq/sqab)
        for yi=0 to ffd do
            dots = iff(dots=length(dasher)?1:dots+1)
            if dasher[dots] then
                xj := a*sqrt(1-yi*yi/bsq)   -- real x value
                frc := fpart(xj)
                flr := floor(xj)
                angle = iff(xj=0?0:arctan(yi/xj)*XPG_RAD2DEG)
                plot_4_points(canvas,xc,yc,flr-d,yi,1-frc,angle1,angle2,angle) --,'t')
                if d=0 then
                    plot_4_points(canvas,xc,yc,flr+1,yi,frc,angle1,angle2,angle) --,'b')
                else
                    for i=-d+1 to d-1 do
                        plot_4_points(canvas,xc,yc,flr+i,yi,1,angle1,angle2,angle)
                    end for
                    plot_4_points(canvas,xc,yc,flr+d,yi,frc,angle1,angle2,angle)
                end if
            end if
        end for
    end if

end procedure

global procedure gCanvasArc(gdx canvas, atom xc, yc, w, h, angle1, angle2, integer flags=0, style=-1, width=-1, atom colour=-1, fillcolour=-1)
    integer pstyle, pwidth
    if style!=-1 then pstyle = gCanvasGetLineStyle(canvas) gCanvasSetLineStyle(canvas,style) end if
    if width!=-1 then pwidth = gCanvasGetLineWidth(canvas) gCanvasSetLineWidth(canvas,width) end if
    atom pcolour, pfillcolour = gCanvasGetBackground(canvas)
    if colour!=-1 then
--      pcolour = gCanvasGetForeground(canvas)
        pcolour = ctrl_xtra[canvas][CX_CANVAS_FORE]
        gCanvasSetForeground(canvas,colour)
    end if
    if fillcolour!=-1 then
--      pfillcolour = gCanvasGetBackground(canvas)
        gCanvasSetBackground(canvas,fillcolour)
    end if
    atom a1r = angle1*PI/180,
         a2r = angle2*PI/180,
         -- The arc starts at the point (xc+(w/2)*cos(angle1),
         --                              yc+(h/2)*sin(angle1))
         -- and ditto ends/angle2 (but WinAPI goes counter-clockwise):
         sx = xc+(w/2)*cos(a2r),
         sy = yc+(h/2)*sin(a2r),
         ex = xc+(w/2)*cos(a1r),
         ey = yc+(h/2)*sin(a1r)
    bool bFilled = and_bits(flags,XPG_FILLED)

--if wuline then
if wuline and not bFilled then -- DEV
--DEV/SUG if filled then width=1...
--  I think we want to collect all pairs, sort by x[h?], then paint outer and fill inner.
--  Slightly sticky point being that is ok for h-pairs but v-pairs would need some care.
--  Alternatively draw outer and collect inner, then flood-fill all inner inclusive [?]
--  Might just stick with cairo under GTK.

--  {rB,gB,bB} = to_rgba(gCanvasGetBackground(canvas))
    {rB,gB,bB} = to_rgba(pfillcolour)
    {rL,gL,bL} = to_rgba(gCanvasGetForeground(canvas))
--?{"wu_ellipse",xc,yc,w,h,angle1,angle2,flags,{rB,gB,bB},{rL,gL,bL}}
    wu_ellipse(canvas,xc,yc,w,h,angle1,angle2)
    if and_bits(flags,XPG_SECTOR) then
        wu_line(canvas,sx,sy,xc,yc)
        wu_line(canvas,xc,yc,ex,ey)
    elsif and_bits(flags,XPG_CHORD) then
        wu_line(canvas,sx,sy,ex,ey)
    end if
else
    if backend=GTK then
        atom cairo = xpg_gtk_get_cairo(canvas,bFilled)
        for iter=1 to iff(bFilled?2:1) do
            c_proc(cairo_new_path,{cairo})
            c_proc(cairo_save,{cairo})
            -- This took me forever to figure out. I finally found (just one) item of interest,
            -- httpdead://github.com/ELWIN-MAO/gtk/blob/master/gtkroundedbox.c (dead link) said:
            --  cairo_translate (cr,xc,yc);
            --  cairo_scale (cr,xradius,yradius);
            --  cairo_arc (cr,0,0,1.0,angle1,angle2);
            -- of course, nowt in the docs, SO, or any tutorial covers this [in "English"]..
            c_proc(cairo_translate,{cairo,xc,yc})
            c_proc(cairo_scale,{cairo,w/2,h/2})
            c_proc(cairo_arc,{cairo,0,0,1,a1r,a2r})
            if and_bits(flags,XPG_SECTOR) then
                c_proc(cairo_line_to,{cairo,0,0})
                c_proc(cairo_close_path,{cairo})
            end if
            c_proc(cairo_restore,{cairo})
            if not bFilled then exit end if
            c_proc(cairo_fill,{cairo})
            bFilled = false
            atom {r,g,b} = to_rgba(ctrl_xtra[canvas][CX_CANVAS_FORE])
            c_proc(cairo_set_source_rgb,{cairo,r/255,g/255,b/255})
--if wuline then exit end if
        end for
        c_proc(cairo_stroke,{cairo})
    elsif backend=WinAPI then
--if not wuline then
        atom hDC = ctrl_xtra[canvas][CX_CANVAS_HDC],
           bgClr = gCanvasGetBackground(canvas),
          hBrush = iff(bFilled?xpg_WinAPI_get_brush(bgClr):NullBrushID),
              lr = xc-w/2,
              tr = yc-h/2,
              rr = xc+w/2,
              br = yc+h/2
--printf(1,"gCanvasArc: bgClr=%08x\n",bgClr)
        xpg_set_canvas_pen(canvas,hDC)
        hBrush = c_func(xSelectObject,{hDC,hBrush})
        bool bOK
--?{"winAPI",flags}
        if flags=0 then
            bOK = c_func(xArc,{hDC,lr,tr,rr,br,sx,sy,ex,ey})
        elsif and_bits(flags,XPG_SECTOR) then
            bOK = c_func(xPie,{hDC,lr,tr,rr,br,sx,sy,ex,ey})
        else -- XPG_CHORD or XPG_FILLED (same thing)
            bOK = c_func(xChord,{hDC,lr,tr,rr,br,sx,sy,ex,ey})
        end if
        assert(bOK!=0)
        hBrush = c_func(xSelectObject,{hDC,hBrush}) -- (restore)
        -- aside: don't delete local sequence brushes[] entries!
--end if
    else
        ?9/0 -- (unknown backend)
    end if
end if

    if style!=-1 then gCanvasSetLineStyle(canvas,pstyle) end if
    if width!=-1 then gCanvasSetLineWidth(canvas,pwidth) end if
    if colour!=-1 then gCanvasSetForeground(canvas,pcolour) end if
--  if fillcolour!=-1 then gCanvasSetForeground(canvas,pfillcolour) end if
    if fillcolour!=-1 then gCanvasSetBackground(canvas,pfillcolour) end if
end procedure

global procedure gCanvasCircle(gdx canvas, atom xc, yc, radius, boolean filled=false, integer style=-1, width=-1, atom colour=-1, fillcolour=-1)
    atom diameter = radius*2
    gCanvasArc(canvas,xc,yc,diameter,diameter,0,360,filled,style,width,colour,fillcolour)
end procedure

--/!*
--Aside: my very first use drew a 600 pixel wide and 420 pixel high curve, and
--       called itself recursively 2,458 times, with a maximum call depth of a
--       mere 12, and plotting just 1,230 pixels, pretty good methinks for a
--       wild stab of "dx<=1 and dy<=1". Clearly 615 pixels would not suffice,
--       so that puts a hard ceiling of less than twofold improvement on it.
--       update: with the ceils, just 821 pixel pairs now get plotted.
--       
--integer cbc = 0
--integer maxd = 0, d = 0
--integer cbp = 0
--integer dots = 0
--integer wu_d
sequence wub_dasher
integer wub_dots = 1, wu_d = 1
procedure wu_bezier(gdx canvas, atom x1, y1, x2, y2, x3, y3, x4, y4)
--if cbc=0 then
--  ?{"CanvasCubicBezier",x1,y1,x2,y2,x3,y3,x4,y4}
--end if
--cbc += 1
--d += 1
--if d>maxd then maxd = d end if
    -- draw a cubic bezier curve from x1,y1 to x4,y4 
    --          using control points x2,y2 and x3,y3
    -- aside: {x2,y2}={x3,y3} shd == gCanvasQuadBezier
    --
    -- Uses the de Castejau method, with particular credit to 
    -- https://hcklbrrfnn.files.wordpress.com/2012/08/bez.pdf
    --
    atom dx = abs(x1-x4),
         dy = abs(y1-y4)
--?{dx,dy}
--sleep(1)
    if dx<=1 and dy<=1 then
        --
        -- It is quite likely we will have split a segment length 1.5
        -- into 0.76 and 0.77, since nowt here is genuinely straight,
        -- and one of those straddles (say) y=12 whereas the other is 
        -- entirely between y=12 and y=13. For the straddler estimate
        -- x using straight-line algebra and draw two pixels, whereas
        -- we can/should just ignore the other and let prev/next segs
        -- set their more accurate pixels (on y=12 and y=13).
        --
        -- For a steep line we want to draw x and x+1 for some int y.
        -- For a flat line we want to draw y and y+1 for some int x.
        --
        bool bSteep = abs(dx)<abs(dy)
        if bSteep then
            integer ly = ceil(y1)
            if ly!=ceil(y4) then -- straddler
                wub_dots = iff(wub_dots=length(wub_dasher)?1:wub_dots+1)
                if wub_dasher[wub_dots] then
                    x1 += (ly-y1)*dx/dy
                    integer x = floor(x1)
                    atom fx = x1-x
                    plot(canvas,x-wu_d,ly,1-fx)
                    if wu_d=0 then
                        plot(canvas,x+1,ly,fx)
                    else
                        for i=-wu_d+1 to wu_d-1 do
                            plot(canvas,x+i,ly,1)
                        end for
                        plot(canvas,x+wu_d,ly,fx)
                    end if
                end if
            end if
        else -- flat
            integer lx = ceil(x1)
            if lx!=ceil(x4) then -- straddler
                wub_dots = iff(wub_dots=length(wub_dasher)?1:wub_dots+1)
                if wub_dasher[wub_dots] then
                    y1 += (lx-x1)*dy/dx
                    integer y = floor(y1)
                    atom fy = y1-y
                    if wu_d=0 then
                        plot(canvas,lx,y,1-fy)
                        plot(canvas,lx,y+1,fy)
                    else
                        lx -= 1
                        plot(canvas,lx,y-wu_d,1-fy)
                        for i=-wu_d+1 to wu_d-1 do
                            plot(canvas,lx,y+i,1)
                        end for
                        plot(canvas,lx,y+wu_d,fy)
                    end if
                end if
            end if
        end if
    else

--DEV/SUG as per gCanvas.exw, pen a ditty to show the de Casteljau construction
--        similar to the second diagram in above doc, with draggable end-points.

        -- l,m,r are the first mid-points,
        -- n,p are the secondary mid-points,
        -- q is the (single) found point on the curve.
        atom lx = (x1+x2)/2,  ly = (y1+y2)/2,
             mx = (x2+x3)/2,  my = (y2+y3)/2,
             rx = (x3+x4)/2,  ry = (y3+y4)/2,
             nx = (lx+mx)/2,  ny = (ly+my)/2,
             px = (mx+rx)/2,  py = (my+ry)/2,
             qx = (nx+px)/2,  qy = (ny+py)/2
        wu_bezier(canvas,x1,y1,lx,ly,nx,ny,qx,qy)
--      gCanvasPixel(canvas,qx,qy,XPG_BLUE) -- (temp)
        wu_bezier(canvas,qx,qy,px,py,rx,ry,x4,y4)
    end if
--d -= 1
end procedure
--*!/

--/*
procedure xCanvasCubicBezier(gdx canvas, atom x1, y1, xc1, yc1, xc2, yc2, x2, y2, integer style=-1, width=-1, atom colour=-1, bool aa=true)
    integer pstyle, pwidth
    if style!=-1 then
        pstyle = gCanvasGetLineStyle(canvas)
        gCanvasSetLineStyle(canvas,style)
    end if
    if width!=-1 then
        pwidth = gCanvasGetLineWidth(canvas)
        gCanvasSetLineWidth(canvas,width)
    end if
    atom pcolour
    if colour!=-1 then
        pcolour = gCanvasGetForeground(canvas)
        gCanvasSetForeground(canvas,colour)
    end if
    if aa then
--      wu_d = floor(width/2)
        wu_d = floor(gCanvasGetLineWidth(canvas)/2)
        wu_bezier(canvas,x1,y1,xc1,yc1,xc2,yc2,x2,y2)
    else
        gCanvasCubicBezier(canvas,x1,y1,xc1,yc1,xc2,yc2,x2,y2)
    end if
    if colour!=-1 then gCanvasSetForeground(canvas,pcolour) end if
    if width!=-1 then gCanvasSetLineWidth(canvas,pwidth) end if
    if style!=-1 then gCanvasSetLineStyle(canvas,pstyle) end if
end procedure
--*/

global procedure gCanvasCubicBezier(gdx canvas, atom x1, y1, xc1, yc1, xc2, yc2, x2, y2, colour=-1)
--global procedure gCanvasBezier(gdx canvas, atom x1, y1, xc1, yc1, xc2, yc2, x2, y2, integer style=-1, width=-1, atom colour=-1)
----Hmmm...
--  integer pstyle = gCanvasGetLineStyle(canvas),
--          pwidth = gCanvasGetLineWidth(canvas)
--  if pstyle!=XPG_CONTINUOUS then gCanvasSetLineStyle(canvas,XPG_CONTINUOUS) end if
--??--  if pwidth!=1 then gCanvasSetLineWidth(canvas,1) end if
    atom pcolour
    if colour!=-1 then
--      pcolour = gCanvasGetForeground(canvas)
        pcolour = ctrl_xtra[canvas][CX_CANVAS_FORE]
        gCanvasSetForeground(canvas,colour)
    end if
    if backend=GTK then
        atom cairo = xpg_gtk_get_cairo(canvas)
        c_proc(cairo_move_to,{cairo,x1,y1})
        c_proc(cairo_curve_to,{cairo,xc1,yc1,xc2,yc2,x2,y2})
        c_proc(cairo_stroke,{cairo})
    elsif backend=WinAPI then
if wuline then
--DEV ugh, this really needs gGetPixel... (mind you, we have that under winAPI...)
    integer style = ctrl_xtra[canvas][CX_PENSTYLE],
            width = ctrl_xtra[canvas][CX_PENWIDTH]
    wub_dasher = dashers[style+1]
    wub_dots = 1
    wu_d = floor(width/2)
    {rB,gB,bB} = to_rgba(gCanvasGetBackground(canvas))
    {rL,gL,bL} = to_rgba(gCanvasGetForeground(canvas))
    wu_bezier(canvas,x1,y1,xc1,yc1,xc2,yc2,x2,y2)
else
        integer onelong = get_struct_size(idPOINT)/2
        atom p4 = allocate(onelong*8),
            hDC = ctrl_xtra[canvas][CX_CANVAS_HDC],
         hBrush = NullBrushID
        xpg_set_canvas_pen(canvas,hDC)
        hBrush = c_func(xSelectObject,{hDC,hBrush})
        pokeN(p4,{x1,y1,xc1,yc1,xc2,yc2,x2,y2},onelong)
        bool bOK = c_func(xPolyBezier,{hDC,p4,4})
        assert(bOK)
        hBrush = c_func(xSelectObject,{hDC,hBrush}) -- (restore)
        -- aside: don't delete local sequence brushes[] entries!
        free(p4)
--     get_struct_size(idPOINT)
--      xPolyBezier = define_c_func(GDI32,"PolyBezier",
--          {C_PTR,     --  HDC hdc
--           C_PTR,     --  const POINT *lppt
--           C_INT},    --  DWORD cPoints
--          C_INT)      -- BOOL
end if
    else
        ?9/0 -- (unknown backend)
    end if

--
--  if pstyle!=XPG_CONTINUOUS then gCanvasSetLineStyle(canvas,pstyle) end if
--  if pwidth!=1 then gCanvasSetLineWidth(canvas,pwidth) end if
    if colour!=-1 then gCanvasSetForeground(canvas,pcolour) end if
end procedure

--/*
procedure gCanvasQuadBezier(gdx canvas, atom x1, y1, cx, cy, x2, y2, integer style=-1, width=-1, colour=-1, bool aa=true)
    -- draw a cubic bezier curve from x1,y1 to x2,y2 
    --                 using the control point cx,cy
    gCanvasCubicBezier(canvas,x1,y1,cx,cy,cx,cy,x2,y2,style,width,colour,aa)
end procedure
--*/

global procedure gCanvasPixel(gdx canvas, atom x, y, colour=-1)
--  if colour=-1 then colour = gCanvasGetForeground(canvas) end if
    if colour=-1 then colour = ctrl_xtra[canvas][CX_CANVAS_FORE] end if
    if backend=GTK then
        atom cairo = ctrl_xtra[canvas][CX_CANVAS_HDC][1]
        atom {r,g,b} = to_rgba(colour)
        c_proc(cairo_set_source_rgb,{cairo,r/255,g/255,b/255})
        c_proc(cairo_set_line_width,{cairo,0.5})
--      c_proc(cairo_set_line_width,{cairo,1})
        c_proc(cairo_move_to,{cairo,x,y})
--      c_proc(cairo_arc,{cairo,x,y,0.5,0,2*PI})
--      c_proc(cairo_line_to,{cairo,x,y})   -- invisible
--      c_proc(cairo_line_to,{cairo,x+1,y}) -- washed out
        c_proc(cairo_line_to,{cairo,x+1,y+1}) -- best I can do
--      c_proc(cairo_fill,{cairo})
        c_proc(cairo_stroke,{cairo})
--      c_proc(cairo_set_source,{cairo,old_source}) -- (crashes??)
    elsif backend=WinAPI then
        atom hDC = ctrl_xtra[canvas][CX_CANVAS_HDC],
             bgr = xpg_WinAPI_rgb_to_bgr(colour)
        bool bOK = c_func(xSetPixelV,{hDC,x,y,bgr})
--      assert(bOK!=0)
    else
        ?9/0 -- (unknown backend)
    end if
end procedure

global procedure gCanvasRect(gdx canvas, atom xmin, xmax, ymin, ymax, bool bFill=false, integer rc=0, style=-1, width=-1, atom colour=-1, fillcolour=-1)
    integer pstyle, pwidth
    if style!=-1 then pstyle = gCanvasGetLineStyle(canvas) gCanvasSetLineStyle(canvas,style) end if
    if width!=-1 then pwidth = gCanvasGetLineWidth(canvas) gCanvasSetLineWidth(canvas,width) end if
    atom pcolour, pfillcolour
    if colour!=-1 then
--      pcolour = gCanvasGetForeground(canvas)
        pcolour = ctrl_xtra[canvas][CX_CANVAS_FORE]
        gCanvasSetForeground(canvas,colour)
    end if
    if fillcolour!=-1 then
        assert(bFill,"no point setting a fill colour!")
        pfillcolour = gCanvasGetBackground(canvas)
        gCanvasSetBackground(canvas,fillcolour)
    end if
    if backend=GTK then
        atom cairo = xpg_gtk_get_cairo(canvas,bFill)
        for iter=1 to iff(bFill?2:1) do
            if rc then
                c_proc(cairo_new_path,{cairo})
                c_proc(cairo_arc,{cairo,xmin+rc,ymin+rc,rc,PI,3*PI/2})
                c_proc(cairo_arc,{cairo,xmax-rc,ymin+rc,rc,3*PI/2,2*PI})
                c_proc(cairo_arc,{cairo,xmax-rc,ymax-rc,rc,0,PI/2})
                c_proc(cairo_arc,{cairo,xmin+rc,ymax-rc,rc,PI/2,PI})
                c_proc(cairo_close_path,{cairo})
            else
                c_proc(cairo_rectangle,{cairo,xmin,ymin,xmax-xmin,ymax-ymin})
            end if
            if not bFill then exit end if
            c_proc(cairo_fill,{cairo})
            bFill = false
            atom {r,g,b} = to_rgba(ctrl_xtra[canvas][CX_CANVAS_FORE])
            c_proc(cairo_set_source_rgb,{cairo,r/255,g/255,b/255})
        end for
        c_proc(cairo_stroke,{cairo})
    elsif backend=WinAPI then
        atom hDC = ctrl_xtra[canvas][CX_CANVAS_HDC],
           bgClr = gCanvasGetBackground(canvas),
          hBrush = iff(bFill?xpg_WinAPI_get_brush(bgClr):NullBrushID)
        xpg_set_canvas_pen(canvas,hDC)
        bool bOK
        hBrush = c_func(xSelectObject,{hDC,hBrush})
        if rc then
            bOK = c_func(xRoundRect,{hDC,xmin,ymin,xmax,ymax,rc,rc})
        else
            bOK = c_func(xRectangle,{hDC,xmin,ymin,xmax,ymax})
        end if
        assert(bOK!=0)
        hBrush = c_func(xSelectObject,{hDC,hBrush}) -- (restore)
    else
        ?9/0 -- (unknown backend)
    end if
    if style!=-1 then gCanvasSetLineStyle(canvas,pstyle) end if
    if width!=-1 then gCanvasSetLineWidth(canvas,pwidth) end if
    if colour!=-1 then
        gCanvasSetForeground(canvas,pcolour)
    end if
    if fillcolour!=-1 then
        gCanvasSetForeground(canvas,pfillcolour)
    end if
end procedure

local function xpg_shifta(atom x, y, w, h, rot_pt, angle)
--
-- shift the NW corner to effect rotation about rot_pt
--
    --
    -- Other angles are obviously doable, but need basic trig & more significantly a
    --  bucketload of testing that I simply don't have time for.
    -- Of course I'd very happily adopt code with either extended restrictions such as
    --  {+90,-90}, and/or a decent demo/proof that it all works for all cases.
    -- For these 9 simple cases I literally drew 9 tiny diagrams to work out the shifts,
    --  ie figuring out where to put the NW corner so it'll all end up as asked for.
--DEV see copied use of CD_CENTER etc below...
    --
--DEV write a proper test case and run WinAPI side-by-side with GTK. 
--    Get gSetAttribute(DIALOG,"SIZE") consistent (matching Iup RASTERSIZE) first...
--  if backend=GTK and and_bits(rot_pt,XPG_N) then y -= 1 end if
--  if and_bits(rot_pt,XPG_N) then y += iff(backend=GTK?-1:1) end if
--  if backend=WinAPI and and_bits(rot_pt,XPG_E) then x += 1 end if
    if rot_pt=XPG_NW or angle=0 then
--  if rot_pt=XPG_NW then
        -- do nowt case
    else
        assert(angle=90)
        if rot_pt=XPG_W then
            x += h/2
            y += h/2
        elsif rot_pt=XPG_SW then
--DEV re-test this in gCanvas.exw...
--          if backend=GTK then
--              x += h-1
--          elsif backend=WinAPI then
--              x += h-2
--          else
--              ?9/0
--          end if
            x += h
            y += h
        elsif rot_pt=XPG_N then
            x += w/2
            y -= w/2
        elsif rot_pt=XPG_S then
            x += w/2+h
            y += h-w/2
        elsif rot_pt=XPG_NE then
            x += w
            y -= w
        elsif rot_pt=XPG_E then
            x += w+h/2
            y += h/2-w
        elsif rot_pt=XPG_SE then
            x += w+h
            y += h/2-w
        elsif rot_pt=XPG_C 
--         or rot_pt=XPG_CENTER then    -- (gShow one, really, but let it pass here)
           or rot_pt=#FFF3 then -- (can't be bothered to move it)
            x += w+h/2
            y += h/2-w/2
        else
            ?9/0
        end if
    end if
    return {x,y}
end function

local constant XPG_NORESET = 0x4 -- for redraw_list()

global procedure gCanvasText(gdx canvas, atom x, y, string text, integer align=XPG_E, object angle=0, atom colour=-1, style=-1)
--?"gCanvasText"
    if length(text)=0 then return end if
    integer N = and_bits(align,XPG_N),
            E = and_bits(align,XPG_E),
            W = and_bits(align,XPG_W),
            S = and_bits(align,XPG_S),
            rot_pt = XPG_NW
    if sequence(angle) then
        {rot_pt,angle} = angle
    end if
    atom pcolour
    if colour!=-1 then
--      pcolour = gCanvasGetForeground(canvas)
        pcolour = ctrl_xtra[canvas][CX_CANVAS_FORE]
        gCanvasSetForeground(canvas,colour)
    end if
    string pstyle
    if style!=-1 then
        pstyle = gGetAttribute(canvas,"FONT")
--?{style,pstyle}
        integer comma = find(',',pstyle), fontsize = 0
        string face = pstyle[1..comma-1]&", "
        sequence styles = split(pstyle[comma+1..$])
        if length(styles) then      
            fontsize = to_integer(styles[$])
            styles = {}
        end if
        styles = {}
        if and_bits(style,#1) then styles &= {"Bold"} end if
        if and_bits(style,#2) then styles &= {"Italic"} end if
        if length(styles) then
            face &= join(styles)&" "
        end if
        if fontsize<0 then
            face &= sprintf("%dpx",-fontsize)
        elsif fontsize then
            face &= sprintf("%d",fontsize)
        end if
--?face
--?fOK
        gSetAttribute(canvas,"FONT",face)
    end if
    if backend=GTK then
--DEV/SUG:
--      {w,h,cairo,layout} = xpg_get_max_text_extent(canvas,{text})
--      atom cairo = xpg_gtk_get_cairo(canvas)
--      atom layout = c_func(pango_cairo_create_layout,{cairo}),
        atom {cairo,layout} = ctrl_xtra[canvas][CX_CANVAS_HDC]
--      xpg_set_canvas_pen(canvas,cairo)
        atom {r,g,b} = to_rgba(ctrl_xtra[canvas][CX_CANVAS_FORE])
        c_proc(cairo_set_source_rgb,{cairo,r/255,g/255,b/255})
        integer {w,h} = gGetTextExtent(canvas,split(text,'\n'))
        x -= iff(W?w+2:iff(E?-2:ceil(w/2)))
        y -= iff(N?h+1:iff(S?0:ceil(h/2)+1))
        {x,y} = xpg_shifta(x,y,w,h,rot_pt,angle)
        c_proc(cairo_move_to,{cairo,x,y})
--pango_cairo_show_layout (cr,layout);
--or maybe:
--pango_cairo_show_layout_line (cr,pango_layout_get_line(layout,0));
--      c_proc(pango_cairo_show_layout,{cairo,layout})
        c_proc(cairo_save,{cairo})
        if angle then
--DEV something like this...
-- from https://stackoverflow.com/questions/22960353/understanding-cairo-rotate :
--/*        ("" trying to draw a cross)
--  You are rotating around the origin, which is at the top-left corner of the image. 
--  To rotate around the center of the image, you must translate as well:
--
--          cairo_move_to(cr,0,HEIGHT/2.); 
--          cairo_line_to(cr,WIDTH,HEIGHT/2.);
--          cairo_stroke(cr);
--          cairo_translate(cr,WIDTH/2,HEIGHT/2); // translate origin to the center
--          cairo_rotate(cr,90.*(M_PI/180.));
--          cairo_translate(cr,-WIDTH/2,-HEIGHT/2); // translate origin back
--          cairo_move_to(cr,0,HEIGHT/2.);
--          cairo_line_to(cr,WIDTH,HEIGHT/2.);
--          cairo_stroke(cr);
--
--  Depending on your application, it might also make sense to actually draw everything relative to the center:
--
--          int half_w = WIDTH/2;
--          int half_h = HEIGHT/2;
--          cairo_translate(cr,half_w,half_h);
--          cairo_move_to(cr,-half_w,0); 
--          cairo_line_to(cr,half_w,0);
--          cairo_stroke(cr); // horizontal line
--          cairo_rotate(cr,90.*(M_PI/180.));
--          cairo_move_to(cr,-half_w,0);
--          cairo_line_to(cr,half_w,0);
--          cairo_stroke(cr); // vertical line
--*/
--          c_proc(cairo_translate,{cairo,0,h/2})
--?"rotate"
            c_proc(cairo_rotate,{cairo,angle*PI/180})
--          c_proc(cairo_rotate,{cairo,(360-angle)*PI/180})
--          c_proc(cairo_translate,{cairo,-w/2,-h/2})
--          c_proc(cairo_translate,{cairo,0,-h/2})
--          c_proc(cairo_move_to,{cairo,x,y})
--          c_proc(pango_layout_context_changed,{layout})
        end if
        c_proc(pango_cairo_show_layout,{cairo,layout})
--      c_proc(xg_object_unref,{layout})
        c_proc(cairo_restore,{cairo})
--or
--/*
--  cr->save();
--  cr->rotate_degrees(90);
--  Glib::RefPtr<Pango::Layout> y_axis_label = this->create_pango_layout("y-axis label");
--  y_axis_label->set_font_description(font);
--  cr->move_to(...); // wherever you wanna put it
--  y_axis_label->show_in_cairo_context(cr);
--  cr->restore();
--*/
--whereas IUP does this:
--/*
--  static void cdtext(cdCtxCanvas *ctxcanvas, int x, int y, const char *s, int len)
--  {
--    PangoFontMetrics* metrics;
--    int w, h, desc, dir = -1;
--    int ox = x, oy = y;
--
--    s = cdgStrToSystem(s,&len,ctxcanvas);
--    pango_layout_set_text(ctxcanvas->fontlayout,s,len);
--    
--    pango_layout_get_pixel_size(ctxcanvas->fontlayout,&w,&h);
--    metrics = pango_context_get_metrics(ctxcanvas->fontcontext,ctxcanvas->fontdesc,pango_context_get_language(ctxcanvas->fontcontext));
--    desc = (((pango_font_metrics_get_descent(metrics)) + PANGO_SCALE/2) / PANGO_SCALE);
--
--    switch (ctxcanvas->canvas->text_alignment)
--    {
--      case CD_BASE_RIGHT:
--      case CD_NORTH_EAST:
--      case CD_EAST:
--      case CD_SOUTH_EAST:
--        x = x - w;
--        break;
--      case CD_BASE_CENTER:
--      case CD_CENTER:
--      case CD_NORTH:
--      case CD_SOUTH:
--        x = x - w/2;
--        break;
--      case CD_BASE_LEFT:
--      case CD_NORTH_WEST:
--      case CD_WEST:
--      case CD_SOUTH_WEST:
--        x = x;
--        break;
--    }
--
--    if (ctxcanvas->canvas->invert_yaxis)
--      dir = 1;
--
--    switch (ctxcanvas->canvas->text_alignment)
--    {
--      case CD_BASE_LEFT:
--      case CD_BASE_CENTER:
--      case CD_BASE_RIGHT:
--        y = y - (dir*h - desc);
--        break;
--      case CD_SOUTH_EAST:
--      case CD_SOUTH_WEST:
--      case CD_SOUTH:
--        y = y - (dir*h);
--        break;
--      case CD_NORTH_EAST:
--      case CD_NORTH:
--      case CD_NORTH_WEST:
--        y = y;
--        break;
--      case CD_CENTER:
--      case CD_EAST:
--      case CD_WEST:
--        y = y - (dir*(h/2));
--        break;
--    }
--
--    if(!ctxcanvas->canvas->use_matrix)
--    {
--      ctxcanvas->fontmatrix.xx = 1;     ctxcanvas->fontmatrix.xy = 0;
--      ctxcanvas->fontmatrix.yx = 0;     ctxcanvas->fontmatrix.yy = 1;
--      ctxcanvas->fontmatrix.x0 = 0;     ctxcanvas->fontmatrix.y0 = 0;
--    }
--
--    if (ctxcanvas->canvas->use_matrix || ctxcanvas->canvas->text_orientation)
--    {
--      PangoRectangle rect;
--      double angle = ctxcanvas->canvas->text_orientation;
--
--      if (ctxcanvas->canvas->text_orientation)
--        pango_matrix_rotate(&ctxcanvas->fontmatrix,angle);
--
--      pango_context_set_matrix (ctxcanvas->fontcontext,&ctxcanvas->fontmatrix);
--      pango_layout_context_changed (ctxcanvas->fontlayout);
--
--      pango_layout_get_pixel_extents(ctxcanvas->fontlayout,NULL,&rect);
--  --#if PANGO_VERSION_CHECK(1,16,0)
--      pango_matrix_transform_pixel_rectangle(&ctxcanvas->fontmatrix,&rect);
--  --#endif
--
--      if (ctxcanvas->canvas->text_orientation)
--      {
--        double cos_angle = cos(angle*CD_DEG2RAD);
--        double sin_angle = sin(angle*CD_DEG2RAD);
--        cdRotatePoint(ctxcanvas->canvas,x,y,ox,oy,&x,&y,sin_angle,cos_angle);
--      }
--      
--      if (ctxcanvas->canvas->use_matrix)
--        cdMatrixTransformPoint(ctxcanvas->xmatrix,x,y,&x,&y);
--
--      /* Defines the new position (X,Y), considering the Pango rectangle transformed */
--      x += (int)rect.x;
--      y += (int)rect.y;
--    }
--
--    cdgdkCheckSolidStyle(ctxcanvas,1);
--
--    if (ctxcanvas->canvas->new_region)
--    {
--      GdkRegion *rgn;
--      gint *idx;
--      gint range;
--
--      pango_layout_line_get_x_ranges(pango_layout_get_line(ctxcanvas->fontlayout,0),0,len,&idx,&range);
--
--      /* TODO: this is only the bounding box of the layout, not the text itself,
--               must transform the text into a polygon. */
--      rgn = gdk_pango_layout_get_clip_region(ctxcanvas->fontlayout,x,y,idx,range);
--
--      sCombineRegion(ctxcanvas,rgn);
--    }
--    else
--      gdk_draw_layout(ctxcanvas->wnd,ctxcanvas->gc,x,y,ctxcanvas->fontlayout);
--
--    pango_context_set_matrix(ctxcanvas->fontcontext,NULL);
--
--    cdgdkCheckSolidStyle(ctxcanvas,0);
--
--    pango_font_metrics_unref(metrics); 
--  }
--
--*/
    elsif backend=WinAPI then
        if angle!=ctrl_xtra[canvas][CX_TXTANGLE] then
--          object v = ctrl_fonts[canvas]
--          if not string(v) then 
--              -- should no longer happen - cgCanvas() now sets a default of "Helvetica, 9"
--              crash("FONT must be explicitly set on a Canvas before text can be rotated",nFrames:=2)
--          end if
            integer font = ctrl_font[canvas]
            string fontname = cached_fontnames[font]
--          xpg_set_font(canvas,v,angle)
            xpg_set_font(canvas,fontname,angle)
        end if
        atom hDC = ctrl_xtra[canvas][CX_CANVAS_HDC],
             clr = ctrl_xtra[canvas][CX_CANVAS_FORE],
             bgr = xpg_WinAPI_rgb_to_bgr(clr),
            prev = c_func(xSetTextColor,{hDC,bgr})
        prev = c_func(xSetBkMode,{hDC,TRANSPARENT})
        bool bOK
        sequence utf16 = utf8_to_utf16(text)
        integer l = length(utf16)
        atom pUTF16 = allocate(2*l)
        poke2(pUTF16,utf16)
        bOK = c_func(xGetTextExtentPoint32W,{hDC,pUTF16,l,pSIZE})
        assert(bOK)
        integer lc = sum(sq_eq('\n',text))+1,
                 w = get_struct_field(idSIZE,pSIZE,"cx"),
                 h = get_struct_field(idSIZE,pSIZE,"cy")*lc
--  bOK = c_func(xReleaseDC,{pHwnd,hDC})
--?{"WinAPIsize",w,h}
        x -= iff(W?w+1:iff(E?-1:ceil(w/2)))
        y -= iff(N?h:iff(S?0:floor(h/2+1)))
        {x,y} = xpg_shifta(x,y,w,h,rot_pt,angle)
--      bOK = c_func(xTextOut,{hDC,x,y,pUTF16,l}) -- does not do linebreaks...
        set_struct_field(idRECT,pRECT,"left",x)
        set_struct_field(idRECT,pRECT,"top",y)
        set_struct_field(idRECT,pRECT,"right",x+w)
        set_struct_field(idRECT,pRECT,"bottom",y+h)
        h = c_func(xDrawText,{hDC,pUTF16,l,pRECT,DT_NOCLIP})
        free(pUTF16)
--IUP does this:
--/*
--  static void sTextOutBlt(cdCtxCanvas* ctxcanvas, int px, int py, const char* s, int len)
--  {
--    HDC hBitmapDC;
--    HBITMAP hBitmap, hOldBitmap;
--    HFONT hOldFont;
--    int w, h, wt, ht, x, y, off, px_off = 0, py_off = 0;
--    double angle = ctxcanvas->canvas->text_orientation*CD_DEG2RAD;
--    double cos_teta = cos(angle);
--    double sin_teta = sin(angle);
--    
--  --  cdgettextsize(ctxcanvas,s,len,&w,&h);
--  --static void cdgettextsize (cdCtxCanvas* ctxcanvas, const char *s, int len, int *width, int *height)
--  --{
--    SIZE size;
--
--    TCHAR* wstr = cdwStrToSystemLen(s,&len,ctxcanvas->utf8mode);
--    GetTextExtentPoint32(ctxcanvas->hDC,wstr,len,&size);
--    
--    if (width)  
--      *width  = size.cx;
--    
--    if (height) 
--      *height = size.cy;
--  --}
--
--    wt = w;
--    ht = h;
--
--    if (ctxcanvas->canvas->text_orientation)
--    {
--      /* new image size */
--      w = (int)(w * cos_teta + h * sin_teta);
--      h = (int)(h * cos_teta + w * sin_teta);
--    }
--
--    /* place in the center of the image */
--    y = h/2;
--    x = w/2; 
--
--    /* fix center alignment */
--    off = ht/2 - ctxcanvas->font.descent;
--    if (ctxcanvas->canvas->text_orientation)
--    {
--      y += (int)(off * cos_teta);
--      x += (int)(off * sin_teta);
--    }
--    else
--      y += off;
--
--    /* calculates the alignment of the image on the canvas */
--    if (ctxcanvas->canvas->text_orientation)
--    {
--      switch (ctxcanvas->canvas->text_alignment)
--      {
--      case CD_CENTER:
--        py_off = 0;
--        px_off = 0;
--        break;
--      case CD_BASE_LEFT:
--        py_off = - (int)(off * cos_teta + w/2 * sin_teta);
--        px_off =   (int)(w/2 * cos_teta - off * sin_teta);         
--        break;
--      case CD_BASE_CENTER:
--        py_off = - (int)(off * cos_teta);
--        px_off = - (int)(off * sin_teta);
--        break;
--      case CD_BASE_RIGHT:
--        py_off = - (int)(off * cos_teta - w/2 * sin_teta);
--        px_off = - (int)(w/2 * cos_teta + off * sin_teta);
--        break;
--      case CD_NORTH:
--        py_off = (int)(ht/2 * cos_teta);
--        px_off = (int)(ht/2 * sin_teta);  
--        break;
--      case CD_SOUTH:
--        py_off = - (int)(ht/2 * cos_teta);
--        px_off = - (int)(ht/2 * sin_teta);  
--        break;
--      case CD_EAST:
--        py_off =   (int)(wt/2 * sin_teta);
--        px_off = - (int)(wt/2 * cos_teta);
--        break;
--      case CD_WEST:
--        py_off = - (int)(wt/2 * sin_teta);
--        px_off =   (int)(wt/2 * cos_teta);         
--        break;
--      case CD_NORTH_EAST:
--        py_off = (int)(ht/2 * cos_teta + wt/2 * sin_teta);
--        px_off = (int)(ht/2 * sin_teta - wt/2 * cos_teta);  
--        break;
--      case CD_SOUTH_WEST:
--        py_off = - (int)(ht/2 * cos_teta + wt/2 * sin_teta);
--        px_off = - (int)(ht/2 * sin_teta - wt/2 * cos_teta);  
--        break;
--      case CD_NORTH_WEST:
--        py_off = (int)(ht/2 * cos_teta - wt/2 * sin_teta);
--        px_off = (int)(ht/2 * sin_teta + wt/2 * cos_teta);  
--        break;
--      case CD_SOUTH_EAST:
--        py_off = - (int)(ht/2 * cos_teta - wt/2 * sin_teta);
--        px_off = - (int)(ht/2 * sin_teta + wt/2 * cos_teta);  
--        break;
--      }
--    }
--    else
--    {
--      switch (ctxcanvas->canvas->text_alignment)
--      {
--      case CD_BASE_RIGHT:
--      case CD_NORTH_EAST:
--      case CD_EAST:
--      case CD_SOUTH_EAST:
--        px_off = - w/2;
--        break;
--      case CD_BASE_CENTER:
--      case CD_CENTER:
--      case CD_NORTH:
--      case CD_SOUTH:
--        px_off = 0;  
--        break;
--      case CD_BASE_LEFT:
--      case CD_NORTH_WEST:
--      case CD_WEST:
--      case CD_SOUTH_WEST:
--        px_off = w/2;         
--        break;
--      }
--
--      switch (ctxcanvas->canvas->text_alignment)
--      {
--      case CD_BASE_LEFT:
--      case CD_BASE_CENTER:
--      case CD_BASE_RIGHT:
--        py_off = - off;
--        break;
--      case CD_SOUTH_EAST:
--      case CD_SOUTH_WEST:
--      case CD_SOUTH:
--        py_off = - h/2;
--        break;
--      case CD_NORTH_EAST:
--      case CD_NORTH:
--      case CD_NORTH_WEST:
--        py_off = + h/2;
--        break;
--      case CD_CENTER:
--      case CD_EAST:
--      case CD_WEST:
--        py_off = 0;
--        break;
--      }
--    }
--
--    /* moves from the center of the image to the upper left corner of the image */
--    px_off -= w/2;
--    py_off -= h/2;
--
--    /* shifts the given point */
--    if (ctxcanvas->canvas->invert_yaxis)
--    {
--      px += px_off;
--      py += py_off;
--    }
--    else
--    {
--      px += px_off;
--      py -= py_off;
--    }
--
--    hBitmap = CreateCompatibleBitmap(ctxcanvas->hDC,w,h);
--    hBitmapDC = CreateCompatibleDC(ctxcanvas->hDC);
--    
--    hOldBitmap = SelectObject(hBitmapDC,hBitmap);
--
--    /* copy a canvas area to a bitmap */
--    BitBlt(hBitmapDC,0,0,w,h,ctxcanvas->hDC,px,py,SRCCOPY);
--
--    /* compensates ROP before drawing */
--    BitBlt(hBitmapDC,0,0,w,h,ctxcanvas->hDC,px,py,ctxcanvas->RopBlt);
--
--    SetBkMode(hBitmapDC,TRANSPARENT);
--    SetBkColor(hBitmapDC,ctxcanvas->bg);
--    SetTextColor(hBitmapDC,ctxcanvas->fg);
--    SetTextAlign(hBitmapDC,TA_CENTER|TA_BASELINE);
--    hOldFont = SelectObject(hBitmapDC,ctxcanvas->hFont);
--
--    {
--  --(windows only)
--  WCHAR* cdwStringToUnicodeLen(const char* strint *len, int utf8mode)
--  {
--    static WCHAR wstr[10240] = L"";
--
--    if (utf8mode)
--      *len = MultiByteToWideChar(CP_UTF8,0,str,*len,wstr,10240);
--    else
--      *len = MultiByteToWideChar(CP_ACP,0,str,*len,wstr,10240);
--
--    if (*len<0)
--      *len = 0;
--
--    wstr[*len] = 0;
--
--    return wstr;
--  }
--
--      TCHAR* wstr = cdwStrToSystemLen(s,&len,ctxcanvas->utf8mode);
--      TextOut(hBitmapDC,x,y,wstr,len);
--    }
--    
--    if (ctxcanvas->canvas->invert_yaxis)
--      BitBlt(ctxcanvas->hDC,px,py,w,h,hBitmapDC,0,0,ctxcanvas->RopBlt);
--    else
--      StretchBlt(ctxcanvas->hDC,px,py,w,-h,hBitmapDC,0,0,w,h,ctxcanvas->RopBlt);
--
--    SelectObject(hBitmapDC,hOldFont);
--    SelectObject(hBitmapDC,hOldBitmap);
--    
--    DeleteObject(hBitmap);
--    DeleteDC(hBitmapDC);
--  }
--*/
        assert(bOK!=0)
    else
        ?9/0 -- (unknown backend)
    end if
    if colour!=-1 then
        gCanvasSetForeground(canvas,pcolour)
    end if
    if style!=-1 and not and_bits(style,XPG_NORESET) then
        gSetAttribute(canvas,"FONT",pstyle)
    end if
end procedure

local procedure xpg_Canvas(gdx id)
    -- (invoked via xpg_map)
    gdx parent = xpg_get_parent_id(id)
    atom canvas
--  bool bShrink = gGetAttribute(id,"USERSIZE")!={0,0} and gGetAttribute(id,"SHRINK")
    if backend=GTK then
        canvas = c_func(gtk_drawing_area_new,{})
        xpg_setID(canvas,id)
        c_proc(gtk_widget_set_events,{canvas,GDK_KBR_MASK})
--      xpg_gtk_add_to(parent,canvas)
--      c_proc(gtk_container_add,{ctrl_handles[parent],canvas})
        xpg_signal_connect(canvas,iff(bGTK3?"draw":"expose-event"),xpg_gtk_canvas_draw,id)
        xpg_signal_connect(canvas,"button-press-event",xpg_gtk_click,id)
        xpg_signal_connect(canvas,"button-release-event",xpg_gtk_click,id)
        xpg_signal_connect(canvas,"motion-notify-event",xpg_gtk_mousemove,id)
        xpg_signal_connect(canvas,"focus-in-event",xpg_gtk_focusinout,id)
        xpg_signal_connect(canvas,"focus-out-event",xpg_gtk_focusinout,id)
        ctrl_xtra[id][CX_CANVAS_HDC] = {NULL,NULL}
--10/5/23:
--      c_proc(gtk_widget_realize,{canvas})
--/"*
-- (need to fix
--      if gGetAttribute(id,"SCROLLABLE",false) then
            atom sandle = c_func(gtk_scrolled_window_new,{NULL,NULL}) 
            c_proc(gtk_scrolled_window_set_policy,{sandle,GTK_POLICY_AUTOMATIC,GTK_POLICY_AUTOMATIC})
            c_proc(gtk_container_add,{sandle,canvas})
            xpg_gtk_add_to(parent,sandle)
            c_proc(gtk_widget_realize,{sandle})
--      end if
--      c_proc(gtk_widget_realize,{canvas})
--*!/
    elsif backend=WinAPI then
--      atom dwStyle = or_all({WS_CHILD,WS_VISIBLE,WS_TABSTOP,TVS_HASLINES,TVS_LINESATROOT,TVS_HASBUTTONS})
--      integer {w,h} = gGetAttribute(id,"SIZE",{0,0})
--      tree_view = xpg_WinAPI_create(id,"SysTreeView32","",parent,w,h,dwStyle,WS_EX_CLIENTEDGE)
--      atom dwStyle = or_all({WS_CHILD,WS_VISIBLE,BS_PUSHBUTTON,WS_TABSTOP}),
--DEV you have to properly manage the scrollbars. First, though, log all SetWindowPos (etc) and make sure we can easily do the same for GTK...
-- SetScrollInfo(), SB_LINEUP (etc), and maybe ScrollWindowEx()
-- S/GetScrollInfo(), SB_LINEUP (etc), and maybe ScrollWindowEx()
        atom dwStyle = or_all({WS_CHILD,WS_VISIBLE,WS_HSCROLL,WS_VSCROLL,BS_PUSHBUTTON,WS_TABSTOP}), w, h
--DEV !=CANVAS?
        if ctrl_xtra[id][CX_CANVAS_TYPE]=TABLE then -- its a gTable then
--DEV why not just use SZ_NATURAL_W? similar for gList? gGraph?
            w = ctrl_xtra[id][CX_GTL_ATTRS][TX_NATWIDTH]
            h = ctrl_xtra[id][CX_GTL_ATTRS][TX_NATHIGHT]
        else
            {w,h} = gGetTextExtent(parent,"W")
        end if
--?{"canvas",w,h}
--      ctrl_size[id][SZ_NATURAL_W] = w
--      ctrl_size[id][SZ_NATURAL_H] = h
        canvas = xpg_WinAPI_create(id,"gCanvas","",parent,w,h,dwStyle,0)
        -- initially disable the builtin scrollbars, at least until formally sized:
        set_struct_field(idSCROLLINFO,pSCROLLINFO,"fMask",SIF_ALL)
        set_struct_field(idSCROLLINFO,pSCROLLINFO,"nMin",0)
        set_struct_field(idSCROLLINFO,pSCROLLINFO,"nMax",1)
        set_struct_field(idSCROLLINFO,pSCROLLINFO,"nPage",2)
        set_struct_field(idSCROLLINFO,pSCROLLINFO,"nPos",0)
--      set_struct_field(idSCROLLINFO,pSCROLLINFO,"nTrackPos",0) -- (ignored)
--       idSCROLLINFO = define_struct("""typedef struct tagSCROLLINFO {
--                                        UINT cbSize;
--                                        UINT fMask;
--                                        int  nMin;
--                                        int  nMax;
--                                        UINT nPage;
--                                        int  nPos;
--                                        int  nTrackPos;
--                                      } SCROLLINFO, *LPCSCROLLINFO;""")

        integer cp -- currrent_position
        cp = c_func(xSetScrollInfo,{canvas,SB_HORZ,pSCROLLINFO,true})
        cp = c_func(xSetScrollInfo,{canvas,SB_VERT,pSCROLLINFO,true})
--       = define_c_func(USER32,"SetScrollInfo",
--          {C_PTR,     --  HWND hwnd
--           C_INT,     --  int fnBar
--           C_PTR,     --  LPCSCROLLINFO lpsi
--           C_UINT},   --  BOOL fRedraw
--          C_INT)      -- int

    else
        ?9/0 -- (unknown backend)
    end if
end procedure

--local function xpg_get_canvas_type(integer l)

local function xpg_set_canvas_attribute(gdx id, string name, object v, bool bMapped)
    integer ct = ctrl_xtra[id][CX_CANVAS_TYPE]
--?{"xpg_set_canvas_attribute",id,ct,CANVAS}
--  atom handle = ctrl_handles[id]
--DEV dunno where this came from... SCROLLINFO/SCROLLABLE??
    if false then
--  if name="SCROLLBAR"
--  or name="XMAX"
--  or name="YMAX" then
        if not bMapped then
            xpg_defer_attr(id,name,v)
--      elsif backend=GTK then
--          c_proc(gtk_window_set_title,{handle,v}) 
--      elsif backend=WinAPI then
--          c_proc(xSetWindowText,{handle,v})
        else
--          ?9/0 -- (unknown backend)
            printf(1,"gSetAttribute(CANVAS,\"%s\",%v)...\n",{name,v})
        end if
        return true
    elsif ct!=CANVAS then   -- gGraph/gTable/gList
--      sequence gtlattr = ctrl_xtra[id][CX_GTL_ATTRS]
        integer lGTL = find(ct,gtl_types),
                gxdx = find(name,gtl_names[lGTL])
--?{"gxdx",gxdx,name,"lGTL",lGTL}
?{"xpg_set_canvas_attribute",id,ct,name,gxdx}
        if gxdx then -- (else trigger std error handling)
            integer vtype = gtl_sigs[lGTL][gxdx]
            if vtype='B' then
                if string(v) then v = xpg_to_bool(v) end if
                assert(v=true or v=false)
            elsif vtype='I' then
                if string(v) then assert(is_integer(v))
                                  v = to_integer(v)
                             else assert(integer(v)) end if
            elsif vtype='N' then
                if string(v) then v = to_number(v) end if
                assert(atom(v)) -- (fail "" yields {})
            elsif vtype='S' then
                assert(string(v))
            elsif name="LEGENDXY" 
              and lGTL=1 then -- gGraph
                if string(v) then v = xpg_intint(v) end if
                assert(sequence(v) and not string(v) and length(v)=2)
            elsif vtype!='O' then
                ?9/0
            end if
            ctrl_xtra[id][CX_GTL_ATTRS][gxdx] = v
--          ctrl_xtra[id][CX_GTL_ATTRS] = 0 -- (kill refcount)
--          gtlattr[gxdx] = v
--          ctrl_xtra[id][CX_GTL_ATTRS] = gtlattr
            return true
        end if
    end if
    return false
end function

function xpg_get_canvas_attribute(gdx id, string name, object dflt)
    -- nb id may not be mapped, unlike most other xpg_get_xxx_attribute rouines...
    atom handle = ctrl_handles[id]
    integer ct = ctrl_xtra[id][CX_CANVAS_TYPE]
    if ct!=CANVAS then -- gGraph/gTable/gList
        integer lGTL = find(ct,gtl_types),
                gxdx = find(name,gtl_names[lGTL])
        if gxdx then return ctrl_xtra[id][CX_GTL_ATTRS][gxdx] end if
        if ct=LIST and name="VALUESTR" then
--?{lGTL,name,"..."}
            return `gGetAttribute(LIST,"VALUESTR")...`
        end if
    end if
    return xpg_return_default_attr(id,name,dflt)
end function

global function gCanvas(object redraw=NULL, sequence attributes="", args={})
    {redraw,attributes,args} = paranormalise_raa(redraw,attributes,args)
    if not bInit then xpg_Init() end if
    integer id = xpg_add_control(CANVAS,flags:=CF_UNMAPATTR)
    xpg_register_handler(CANVAS,"REDRAW",{{1,1,"PO"}})
    xpg_register_handler(CANVAS,"DROP",{{1,1,"FI"}})
    set_ctrl_msg(CANVAS,xpg_Canvas,xpg_set_canvas_attribute,
                                   xpg_get_canvas_attribute)
    object cxi = repeat(0,CX_CANVAS_LEN)
    cxi[CX_CANVAS_TYPE] = CANVAS
    cxi[CX_CANVAS_FLAGS] = CXCF_REPEN
--  cxi[CX_CANVAS_FLAGS] = or_all({CXCF_REPEN,CXCF_REFONT})
    cxi[CX_CANVAS_BACK] = XPG_WHITE
    cxi[CX_CANVAS_FORE] = XPG_BLACK
    cxi[CX_PENSTYLE] = XPG_CONTINUOUS
    cxi[CX_PENWIDTH] = 1
    ctrl_xtra[id] = cxi
    cxi = 0 -- (kill refcount)
--  gSetAttribute(id,"FONT","Helvetica, 9")
    if redraw!=NULL then
        gSetHandler(id,"REDRAW",redraw)
    end if
    if length(attributes) then
        gSetAttributes(id,attributes,args)
    end if
    return id
end function 

global procedure gRadio(gdx ids)
    check_unmapped(ids,CF_RADIO,length(radio_groups)+1)
    radio_groups = append(radio_groups,{NULL,ids})
end procedure

global function gRadioItem(gdx id)
    assert(ctrl_types[id]=CHECKBOX)
    integer rdx = ctrl_xtra[id]
    -- (of course this will typecheck should you store it in a gdx:)
    if rdx=0 then return -1 end if
    for id in radio_groups[rdx][2] do
        if gGetAttribute(id,"VALUE") then return id end if
    end for
    return 0 -- (whereas "" and this are fine being stored in a gdx)
end function

local function xpg_gtk_toggle_clicked(atom handle, gdx id) -- (GTK only)
    assert(id=xpg_getID(handle))
    integer value_changed = gGetHandler(id,"VALUE_CHANGED")
    if value_changed then
        bool bChecked = c_func(gtk_toggle_button_get_active,{handle})
        if bChecked or (not and_bits(ctrl_flags[id],CF_RADIO)) then
            value_changed(id,bChecked)
        end if
    end if
    return 0 -- (ignored)
end function

local procedure xpg_Checkbox(gdx id)
    -- (invoked via xpg_map)
    gdx parent = xpg_get_parent_id(id)
    atom checkbox
    string title = gGetAttribute(id,"TITLE")
    bool bRadio = and_bits(ctrl_flags[id],CF_RADIO)!=0
    integer rdx = ctrl_xtra[id]
    if backend=GTK then
        if bRadio then
            atom radio_group = radio_groups[rdx][1]
            checkbox = c_func(gtk_radio_button_new_with_mnemonic,{radio_group,title})
            radio_group = c_func(gtk_radio_button_get_group,{checkbox})
            radio_groups[rdx][1] = radio_group
        else
            checkbox = c_func(gtk_check_button_new_with_mnemonic,{title})
        end if
        xpg_setID(checkbox,id)

--      c_proc(gtk_container_add,{ctrl_handles[parent],button})
--      xpg_signal_connect(checkbox,"realize",xpg_gtk_widget_realized,id)
        xpg_signal_connect(checkbox,"clicked",xpg_gtk_toggle_clicked,id)
        xpg_signal_connect(checkbox,"focus-in-event",xpg_gtk_focusinout,id)
        xpg_signal_connect(checkbox,"focus-out-event",xpg_gtk_focusinout,id)
        xpg_gtk_add_to(parent,checkbox)
--(temp?:)
--      atom vbox = c_func(gtk_vbox_new,{false,6})
--      atom hbox = c_func(gtk_hbox_new,{false,6})
--      c_proc(gtk_box_pack_start,{hbox,checkbox,false,false,0})
--      c_proc(gtk_box_pack_start,{vbox,hbox,false,false,0})
--      c_proc(gtk_container_add,{ctrl_handles[parent],vbox})
        c_proc(gtk_widget_realize,{checkbox})
    elsif backend=WinAPI then
        atom dwStyle = iff(bRadio?BS_AUTORADIOBUTTON:BS_AUTOCHECKBOX)
        if bRadio and id=radio_groups[rdx][2][1] then dwStyle += WS_GROUP end if
        dwStyle = or_all({dwStyle,WS_CHILD,WS_VISIBLE,WS_TABSTOP})
        checkbox = xpg_WinAPI_create(id,"button",title,parent,100,25,dwStyle,0)
    else
        ?9/0 -- (unknown backend)
    end if
end procedure

--DEV common up with button
function xpg_set_checkbox_attribute(gdx id, string name, object v, bool bMapped)
    atom handle = ctrl_handles[id]
    if name="TITLE" then
--      ?9/0 -- placeholder
        assert(string(v))
        if not bMapped then
            xpg_defer_attr(id,name,v)
        elsif backend=GTK then
--          c_proc(gtk_window_set_title,{handle,v}) 
            c_proc(gtk_button_set_label,{handle,xpg_gtk_mnemonicalize(v)})
--          c_proc(gtk_label_set_text_with_mnemonic,{handle,xpg_gtk_mnemonicalize(v)})
        elsif backend=WinAPI then
            c_proc(xSetWindowText,{handle,v})
        else
            ?9/0 -- (unknown backend)
        end if
        return true
    elsif name="VALUE" then
        if string(v) then
            v = iff(v="TOGGLE"?not gGetAttribute(id,"VALUE"):xpg_to_bool(v))
        end if
        bool bRadio = and_bits(ctrl_flags[id],CF_RADIO)!=0
        integer offid = id
        if bRadio then
            if not v then
--          if not v and not bFromDeferred then
                crash(`gSetAttribute(RADIO,"VALUE",false) is illegal`)
            end if
            offid = gRadioItem(id)
--          if offid=id then offid = 0 end if
            v = not v
        end if
        for i=1 to 2 do -- ([offid then] id)
            if not bMapped then
                xpg_defer_attr(id,name,v)
            elsif backend=GTK then
                c_proc(gtk_toggle_button_set_active,{handle,v})
            elsif backend=WinAPI then
                {} = c_func(xSendMessage,{handle,BM_SETCHECK,v,0})
            else
                ?9/0 -- (unknown backend)
            end if
            if offid=id then exit end if
            offid = id
            handle = ctrl_handles[id]
            v = not v
        end for
--      printf(1,"gSetAttribute(CHECKBOX,\"%s\",%v)...\n",{name,v})
        return true
    end if
    return false
end function

function xpg_get_checkbox_attribute(gdx id, string name, object dflt)
    -- nb id may not be mapped, unlike most other xpg_get_xxx_attribute rouines...
    atom handle = ctrl_handles[id]
    bool bMapped = and_bits(ctrl_flags[id],CF_MAPPED)!=0
    if name="TITLE" then
        if not bMapped then
            if dflt=999_999_999 then dflt = "" end if
        elsif backend=GTK then
            return peek_string(c_func(gtk_button_get_label,{handle}))
        elsif backend=WinAPI then
            integer l = c_func(xGetWindowTextLength,{handle})
            string title = repeat(' ',l)
            c_proc(xGetWindowText,{handle,title,l+1})
            return title
        else
            ?9/0 -- (unknown backend)
        end if
    elsif name="VALUE" then
        if not bMapped then
            if dflt=999_999_999 then dflt = false end if
        elsif backend=GTK then
            return c_func(gtk_toggle_button_get_active,{handle})
        elsif backend=WinAPI then
            -- yields one of: BST_UNCHECKED = 0,  BST_CHECKED = 1,  BST_INDETERMINATE = 2
            return c_func(xSendMessage,{handle,BM_GETCHECK,0,0})
        else
            ?9/0 -- (unknown backend)
        end if
    end if
    return xpg_return_default_attr(id,name,dflt)
end function

global function gCheckbox(nullable_string title=NULL, object value_changed=NULL, sequence attributes="", args={})
    {title,value_changed,attributes,args} = paranormalise_traa(title,value_changed,attributes,args)
    if not bInit then xpg_Init() end if
    integer id = xpg_add_control(CHECKBOX,flags:=CF_UNMAPATTR)
--test_elem = id
    xpg_register_handler(CHECKBOX,"VALUE_CHANGED",{{2,2,"POI"}})
    set_ctrl_msg(CHECKBOX,xpg_Checkbox,xpg_set_checkbox_attribute,
                                       xpg_get_checkbox_attribute)
    -- ctrl_xtra[id] is the radio group (index), or 0 if none.
--  gSetAttribute(id,"FONT","Helvetica, 9")
    if title!=NULL then
        gSetAttribute(id,"TITLE",title)
    end if
    if value_changed!=NULL then
        gSetHandler(id,"VALUE_CHANGED",value_changed)
    end if
    if length(attributes) then
        gSetAttributes(id,attributes,args)
    end if
    return id
end function 

local integer xpg_clipboard_id = 0

function xpg_set_clipboard_attribute(gdx id, string name, object v, bool /*bMapped*/)
    assert(id=xpg_clipboard_id)
    integer fmt = find(name,{"TEXT","UNICODETEXT"})
    if fmt then
        if backend=GTK then
            atom clipboard = ctrl_handles[id]
            c_proc(gtk_clipboard_clear,{clipboard})
            if v!=NULL then
                c_proc(gtk_clipboard_set_text,{clipboard,v,-1})
            end if
        elsif backend=WinAPI then
--          atom hWnd = c_func(xGetActiveWindow,{}) -- (what arwen uses)
            atom hWnd = c_func(xGetForegroundWindow,{})
            if c_func(xOpenClipboard,{hWnd}) then
                if c_func(xEmptyClipboard,{}) and sequence(v) then
                    sequence text = v
                    v = NULL
                    integer len = length(text) 
                    if len!=0 then
                        if text[len]!=0 then
                            text &= 0
                            len += 1
                        end if
                        fmt = {CF_TEXT,CF_UNICODETEXT}[fmt]
                        if fmt=CF_UNICODETEXT then
                            len *= 2
                        end if
                        atom hGlobal = c_func(xGlobalAlloc,{GMEM_CLIPBOARD,len})
                        if hGlobal!=NULL then
                            atom pData = c_func(xGlobalLock,{hGlobal})
                            if pData=NULL then
                                hGlobal = c_func(xGlobalFree,{hGlobal})
--DEV or just warn...? or log...
                                assert(hGlobal=NULL)
                            else
                                if fmt=CF_TEXT then
                                    poke(pData,text)
                                else
                                    poke2(pData,text)
                                end if
                                c_proc(xGlobalUnlock,{hGlobal})
                                
                                -- copy data to clipboard & close
                                hGlobal = c_func(xSetClipboardData,{fmt,hGlobal})
                                --Note that we should NOT GlobalFree hGlobal,
                                --      since the clipboard now owns it.
                                assert(hGlobal!=NULL)
                            end if
                        end if
                    end if
                end if
                c_proc(xCloseClipboard,{})
            end if
        else
            ?9/0 -- (unknown backend)
        end if
        return true
    end if
    return false
end function

function xpg_get_clipboard_attribute(gdx id, string name, object dflt)
    assert(id=xpg_clipboard_id)
    integer fmt = find(name,{"TEXT","UNICODETEXT"})
    if fmt then
        fmt = {CF_TEXT,CF_UNICODETEXT}[fmt]
        object clip = NULL
        if backend=GTK then
            atom clipboard = ctrl_handles[id],
                     pClip = c_func(gtk_clipboard_wait_for_text,{clipboard})
            if pClip!=NULL then
                if fmt=CF_TEXT then
                    clip = peek_string(pClip)
                else -- if fmt=CF_UNICODETEXT then
                    clip = peek_wstring(pClip)
                end if
            end if
        elsif backend=WinAPI then
--          atom hWnd = c_func(xGetActiveWindow,{}) -- (what arwen uses)
            atom hWnd = c_func(xGetForegroundWindow,{})
            if c_func(xOpenClipboard,{hWnd}) then
                if c_func(xIsClipboardFormatAvailable,{fmt}) then
                    atom hClip = c_func(xGetClipboardData,{fmt}),
                         pData = c_func(xGlobalLock,{hClip})
                    if pData then
                        integer size = c_func(xGlobalSize,{hClip})
                        if fmt=CF_TEXT then
                            clip = peek({pData,size})
                        elsif fmt=CF_UNICODETEXT then
                            clip = peek2u({pData,floor(size/2)})
                        else
                            ?9/0
                        end if
                        c_proc(xGlobalUnlock,{hClip})
                        size = find(0,clip)
                        if size then
                            clip = clip[1..size-1]
                        end if
                    end if
                end if
                c_proc(xCloseClipboard,{})
            end if
        else
            ?9/0 -- (unknown backend)
        end if
        return clip
    elsif name="TEXTAVAILABLE" then
        if backend=GTK then
            atom clipboard = ctrl_handles[id]
            return c_func(gtk_clipboard_wait_is_text_available,{clipboard})
        elsif backend=WinAPI then
            return c_func(xIsClipboardFormatAvailable,{CF_TEXT})
        else
            ?9/0 -- (unknown backend)
        end if
    end if
    return xpg_return_default_attr(id,name,dflt)
end function

global function gClipboard()
    integer id = xpg_clipboard_id
    if id=0 then
        if not bInit then xpg_Init() end if
        id = xpg_add_control(CLIPBOARD,flags:=CF_MAPPED)
        if backend=GTK then
            atom clipboard = c_func(gtk_clipboard_get,{GDK_SELECTION_CLIPBOARD})
            ctrl_handles[id] = clipboard
        end if
-- hmm, I wonder whether we can get a VALUE_CHANGED handler to work?
--      xpg_register_handler(CLIPBOARD,"REDRAW",{{1,1,"PO"}})
        set_ctrl_msg(CLIPBOARD,0,xpg_set_clipboard_attribute,
                                 xpg_get_clipboard_attribute)
        xpg_clipboard_id = id
    end if
    return id
end function 

local function xpg_gtk_dropdown_changed(atom handle, gdx id) -- (GTK only)
    assert(id=xpg_getID(handle))
    integer changed = gGetHandler(id,"CHANGED")
    if changed then
        changed(id)
    end if
    return 0 -- (ignored)
end function

local procedure xpg_DropDown(gdx id)
    -- (invoked via xpg_map)
    gdx parent = xpg_get_parent_id(id)
    atom dropdown
    sequence options = ctrl_xtra[id]
    bool editable = gGetAttribute(id,"EDITABLE",false)
--  bool editable = gGetAttribute(id,"EDITABLE",true)
--Hmm, no idea, seems fine without it...
--  integer v = gGetAttribute(id,"VALINT",0) -- (grab a copy /before/ mapping)
    if backend=GTK then
        if editable then
            dropdown = c_func(gtk_combo_box_text_new_with_entry,{})
        else
            dropdown = c_func(gtk_combo_box_text_new,{})
        end if
        xpg_setID(dropdown,id)
        xpg_gtk_add_to(parent,dropdown)
--      xpg_signal_connect(dropdown,"realize",xpg_gtk_widget_realized,id)
        xpg_signal_connect(dropdown,"changed",xpg_gtk_dropdown_changed,id)
        xpg_signal_connect(dropdown,"focus-in-event",xpg_gtk_focusinout,id)
        xpg_signal_connect(dropdown,"focus-out-event",xpg_gtk_focusinout,id)
        c_proc(gtk_widget_realize,{dropdown})
    elsif backend=WinAPI then
        atom dwStyle = or_all({WS_CHILD,WS_VISIBLE,WS_VSCROLL,WS_TABSTOP,CBS_AUTOHSCROLL})
                     + iff(editable?CBS_DROPDOWN:CBS_DROPDOWNLIST)
--/*
               {w,h} = gGetTextExtent(parent,options,false)
?{"dd",w,h,{w+38,h}}
        w += 38
--DEV 10/5/23 I think this is/was pointless:
--      ctrl_size[id][SZ_NATURAL_W] = w
--      ctrl_size[id][SZ_NATURAL_H] = h
--      dropdown = xpg_WinAPI_create(id,"combobox","",parent,w,h,dwStyle,WS_EX_CLIENTEDGE)
--*/
        dropdown = xpg_WinAPI_create(id,"combobox","",parent,50,25,dwStyle,WS_EX_CLIENTEDGE)
    else
        ?9/0 -- (unknown backend)
    end if
    gSetAttribute(id,"OPTIONS",options)
--  if v then gSetAttribute(id,"VALINT",v) end if
end procedure

function xpg_set_dropdown_attribute(gdx id, string name, object v, bool bMapped)
    atom handle = ctrl_handles[id]
    if name="OPTIONS" then
        ctrl_xtra[id] = v
        if not bMapped then
            xpg_defer_attr(id,name,v)
        else
            integer changed
            if backend=GTK then
                changed = gGetHandler(id,"CHANGED")
                -- disable while adding all the entries...
                if changed then gSetHandler(id,"CHANGED",NULL) end if
                while true do
                    c_proc(gtk_combo_box_set_active,{handle,1})
                    if c_func(gtk_combo_box_get_active,{handle})!=1 then exit end if
                    c_proc(gtk_combo_box_text_remove,{handle,0})
                end while
                c_proc(gtk_combo_box_text_remove,{handle,0})
            elsif backend=WinAPI then
                {} = c_func(xSendMessage,{handle,CB_RESETCONTENT,0,0})
            else
                ?9/0 -- (unknown backend)
            end if
            for i=1 to length(v) do
                string s = v[i]
                if backend=GTK then
                    c_proc(gtk_combo_box_text_append_text,{handle,s})
                elsif backend=WinAPI then
                    {} = c_func(xSendMessage,{handle,CB_ADDSTRING,0,s})
                else
                    ?9/0 -- (unknown backend)
                end if
            end for
            if backend=GTK and changed then -- restore
                gSetHandler(id,"CHANGED",changed)
            end if
        end if
        return true
    elsif name="VALINT" then
        if string(v) then v = to_integer(v) end if
        if not bMapped then
            xpg_defer_attr(id,name,v)
        else
            if backend=GTK then
                c_proc(gtk_combo_box_set_active,{handle,v-1})
            elsif backend=WinAPI then
                {} = c_func(xSendMessage,{handle,CB_SETCURSEL,v-1,0})
                -- (GTK does this automatically...)
                integer changed = gGetHandler(id,"CHANGED")
                if changed then
                    changed(id)
                end if
            else
                ?9/0 -- (unknown backend)
            end if
        end if
        return true
    elsif name="FGCOLOR" then
        printf(1,"gSetAttribute(DROPDOWN,\"%s\",%v)...\n",{name,v})
        return true
    end if
    return false
end function

function xpg_get_dropdown_attribute(gdx id, string name, object dflt)
    atom handle = ctrl_handles[id]
    if name="VALINT" then
        atom atmres
        if backend=GTK then
            atmres = c_func(gtk_combo_box_get_active,{handle})
        elsif backend=WinAPI then
            atmres = c_func(xSendMessage,{handle,CB_GETCURSEL,0,0})
        else
            ?9/0 -- (unknown backend)
        end if
        integer res = and_bits(atmres,-1)
        return res+1
    elsif name="VALSTR" then
        string res = ""
        if backend=GTK then
            atom pText = c_func(gtk_combo_box_text_get_active_text,{handle})
            if pText then res = peek_string(pText) end if
            c_proc(g_free,{pText})
        elsif backend=WinAPI then
            integer pos = c_func(xSendMessage,{handle,CB_GETCURSEL,0,0})
            if pos!=CB_ERR then
                integer len = c_func(xSendMessage,{handle,CB_GETLBTEXTLEN,pos,0})
                if len!=CB_ERR then
                    atom pMem = allocate(len+1)
                    len = c_func(xSendMessage,{handle,CB_GETLBTEXT,pos,pMem})
--                  if len!=CB_ERR then res = peek_string({pMem,len}) end if
                    if len!=CB_ERR then res = peek_string(pMem) end if
                    free(pMem)
                end if
            end if
        else
            ?9/0 -- (unknown backend)
        end if
        return res
    end if
    return xpg_return_default_attr(id,name,dflt)
end function

global function gDropDown(object options, selected=NULL, sequence attributes="", args={})
    {options,selected,attributes,args} = paranormalise_qraa(options,selected,attributes,args)
    if not bInit then xpg_Init() end if
    integer id = xpg_add_control(DROPDOWN)
--test_elem = id
    xpg_register_handler(DROPDOWN,"CHANGED",{{1,1,"PO"}})
    set_ctrl_msg(DROPDOWN,xpg_DropDown,xpg_set_dropdown_attribute,
                                       xpg_get_dropdown_attribute)
--  gSetAttribute(id,"FONT","Helvetica, 9")
    if selected!=NULL then
        gSetHandler(id,"CHANGED",selected)
    end if
    if length(attributes) then
        gSetAttributes(id,attributes,args)
    end if
    ctrl_xtra[id] = options
    return id
end function 

local procedure xpg_Frame(gdx id)
    -- (invoked via xpg_map)
    gdx parent = xpg_get_parent_id(id)
    atom frame
    string title = gGetAttribute(id,"TITLE","")
    if backend=GTK then
--if title="" then tiotle=NULL end if
--      frame = c_func(gtk_frame_new,{title})
        frame = c_func(gtk_frame_new,{iff(title=""?NULL:title)})
        xpg_setID(frame,id)
        xpg_gtk_add_to(parent,frame)
--      c_proc(gtk_container_add,{ctrl_handles[parent],frame})
-- DEV this is of no help here...
--      xpg_signal_connect(frame,"realize",xpg_gtk_widget_realized,id)
        c_proc(gtk_widget_realize,{frame})
    elsif backend=WinAPI then
--      if title="" then title="Y" end if
        atom dwStyle = or_all({WS_CHILD,WS_VISIBLE,BS_GROUPBOX}),
--DEV...
               {w,h} = gGetTextExtent(parent,title)
--             {w,h} = gGetTextExtent(parent,iff(title=""?"X":title))
--      ctrl_size[id][SZ_NATURAL_W] = w  -- no! leave this at 0!
--      ctrl_size[id][SZ_NATURAL_H] = h
        frame = xpg_WinAPI_create(id,"button",title,parent,w,h,dwStyle,0)
--      frame = xpg_WinAPI_create(id,"button",iff(title=""?NULL:title),parent,w,h,dwStyle,0)
--DEV this may want to be 6 wide and char height+3 high...
--      ctrl_size[id][SZ_NATURAL_W] = 0
--      ctrl_size[id][SZ_NATURAL_H] = 0
    else
        ?9/0 -- (unknown backend)
    end if
end procedure

function xpg_set_frame_attribute(gdx id, string name, object v, bool bMapped)
    atom handle = ctrl_handles[id]
    if name="TITLE" then
        assert(string(v))
        if not bMapped then
            xpg_defer_attr(id,name,v)
        elsif backend=GTK then
            c_proc(gtk_frame_set_label,{handle,v})
        elsif backend=WinAPI then
            c_proc(xSetWindowText,{handle,v})
        else
            ?9/0 -- (unknown backend)
        end if
        return true
    elsif name="FGCOLOR" then
        printf(1,"gSetAttribute(FRAME,\"%s\",%v)...\n",{name,v})
        return true
    end if
    return false
end function

function xpg_get_frame_attribute(gdx id, string name, object dflt)
    atom handle = ctrl_handles[id]
    if name="TITLE" then
        if backend=GTK then
            return peek_string(c_func(gtk_frame_get_label,{handle}))
        elsif backend=WinAPI then
            integer l = c_func(xGetWindowTextLength,{handle})
            string title = repeat(' ',l)
            c_proc(xGetWindowText,{handle,title,l+1})
            return title
        else
            ?9/0 -- (unknown backend)
        end if
    end if
    return xpg_return_default_attr(id,name,dflt)
end function

--global function gFrame(gdx child, [nullable_string title=NULL,] sequence attributes="", args={})
global function gFrame(gdx child, object title=NULL, sequence attributes="", args={})
    {title,attributes,args} = paranormalise_taa(title,attributes,args)
    if not bInit then xpg_Init() end if
    integer id = xpg_add_control(FRAME,bHasChildren:=true)
    if child then
        parent_ids[child] = id
        children_ids[id] = {child}
    end if
--  xpg_register_handler(FRAME,"XXX",{{1,1,"PO"}})
    set_ctrl_msg(FRAME,xpg_Frame,xpg_set_frame_attribute,
                                 xpg_get_frame_attribute)
--  gSetAttribute(id,"FONT","Helvetica, 9")
--  gSetAttribute(id,"MARGIN",2)
--  gSetAttribute(id,"PADDING",2)
    if title!=NULL then
        gSetAttribute(id,"TITLE",title)
    end if
    if length(attributes) then
        gSetAttributes(id,attributes,args)
    end if
    return id
end function 

global function gHbox(sequence children={}, string attributes="", sequence args={})
    return gBox('H',children,attributes,args)
end function

global function gImageRGBA(integer width, height, sequence pixels)
    if backend=GTK then
--      return c_func(gdk_pixbuf_new_from_xpm_data,{xpg_word_array(xpm)})
    elsif backend=WinAPI then
--          atom {mem,hdrSize} = xpm[2],
--               hdc = c_func(xGetDC,{0}),      -- Get the screen's device context.
--              hDIB = c_func(xCreateDIBitmap, {hdc,                -- create DDB for/compatible with this
--                                              mem,                -- info about the DIB to create, eg h*w
--                                              CBM_INIT,           -- initialise it please with...
--                                              mem+hdrSize,        --      this bitmap,
--                                              mem,                --      which has this structure
--                                              DIB_RGB_COLORS})    --      and has explicit RGB values
--          assert(hDIB!=NULL)
--          bool bOK = c_func(xReleaseDC,{0,hdc})
--          assert(bOK)
--          return hDIB
    else
        ?9/0 -- (unknown backend)
    end if
--/*
    let id = document.createElement("canvas");
    id.width = width;
    id.height = height;
    let ctx = id.getContext("2d"),
        imgData = ctx.createImageData(width,height),
        len = length(pixels);
    for (let i = 0; i < len; i += 1) {
        imgData.data[i] = pixels[i+1];
    }
    ctx.putImageData(imgData,0,0);
//  return id;
//or, maybe:
    let res = id.toDataURL();
    return res;
--*/
    return {width,height,pixels}
end function

global function gImageRGB(integer width, height, sequence pixels)
--/*
    let id = document.createElement("canvas");
    id.width = width;
    id.height = height;
    let ctx = id.getContext("2d"),
        imgData = ctx.createImageData(width,height),
        len = length(pixels),
        k = 0;
    for (let i = 0; i < len; i += 3) {
        imgData.data[k+0] = pixels[i+1];
        imgData.data[k+1] = pixels[i+2];
        imgData.data[k+2] = pixels[i+3];
        imgData.data[k+3] = 255; // (alpha)
        k += 4
    }
    ctx.putImageData(imgData,0,0);
    let res = id.toDataURL();
    return res;
--*/
    return {width,height,pixels}
--  return {"gImage",width,height,pixels}
end function

global function gImage(integer width, height, sequence pixels, palette={})
    if palette={} then
        palette = {{  0,  0,  0}, // ( 0 XPG_BLACK         = 0x000000)
                   {#80,  0,  0}, // ( 1 XPG_DARK_RED      = 0x800000)
                   {  0,#80,  0}, // ( 2 XPG_DARK_GREEN    = 0x008000) 
                   {#80,#80,  0}, // ( 3 XPG_DARK_YELLOW   = 0x808000 aka XPG_OLIVE)
                   {  0,  0,#80}, // ( 4 XPG_DARK_BLUE     = 0x000080 aka XPG_NAVY)
                   {#80,  0,#80}, // ( 5 XPG_DARK_MAGENTA  = 0x800080) 
                   {  0,#80,#80}, // ( 6 XPG_DARK_CYAN     = 0x008080) 
                   {#C0,#C0,#C0}, // ( 7 XPG_GRAY          = 0xC0C0C0 aka XPG_GREY, XPG_SILVER)
                   {#80,#80,#80}, // ( 8 XPG_DARK_GRAY     = 0x808080 aka XPG_DARK_GREY)
                   {#FF,  0,  0}, // ( 9 XPG_RED           = 0xFF0000)   
                   {  0,#FF,  0}, // (10 XPG_GREEN         = 0x00FF00 aka XPG_LIGHT_GREEN)
                   {#FF,#FF,  0}, // (11 XPG_YELLOW        = 0xFFFF00)
                   {  0,  0,#FF}, // (12 XPG_BLUE          = 0x0000FF)
                   {#FF,  0,#FF}, // (13 XPG_MAGENTA       = 0xFF00FF)
                   {  0,#FF,#FF}, // (14 XPG_CYAN          = 0x00FFFF)
                   {#FF,#FF,#FF}} // (15 XPG_WHITE         = 0xFFFFFF)
    else
        for i,pi in palette do
            if atom(pi) or string(pi) then
                palette[i] = to_rgba(pi)
            end if
        end for
    end if
--/*
    let id = document.createElement("canvas");
    id.width = width;
    id.height = height;
    let ctx = id.getContext("2d"),
        imgData = ctx.createImageData(width,height),
        len = length(pixels),
        k = 0;
//DEV defer this until actually used, or better yet optionally allow a palette.
    const $gImageDefaultColours = [

//  if (length(palette) === 0) { palette = ["sequence",XPG_BLACK, etc.]; }
    for (let i = 0; i < len; i += 1) {
//      let pi = pixels[i+1],
        let /*integer*/ pi = $subse(pixels,i+1),
            p3 = $gImageDefaultColours[pi];
//          p3 = to_rgba(palette[pi]);
        for (let j = 0; j < 3; j += 1) {
            imgData.data[k] = p3[j];
            k += 1;
        }   
        imgData.data[k] = 255; // (alpha)
        k += 1;
    }
    ctx.putImageData(imgData,0,0);
    let res = id.toDataURL();
    return res;
}
--*/
--dev:
    return {"gImage",width,height,pixels}
end function

global function gImage_from_XPM(sequence xpm)
    if not bInit then xpg_Init() end if
    if string(xpm) then xpm = split(xpm,'\n') end if
    object img
    if backend=GTK then
        img = xpg_xpm_callback(xpm)
    elsif backend=WinAPI then
        img = xpg_winAPI_create_DIB_from_xpm(xpm,xpg_xpm_callback)
    else
        ?9/0 -- (unknown backend)
    end if
    gdc res = {"gImage",img}
    return res
end function

global procedure gImageDraw(gdc src, tgt, atom x=0, y=0)
    assert(sequence(src)) // **NOT** a gCanvas (but tgt can be)
    if backend=GTK then
        if atom(tgt) then   -- a gCanvas
            atom handle = ctrl_handles[tgt],
                 window = c_func(gtk_widget_get_window,{handle}),
                  cairo = c_func(gdk_cairo_create,{window}),
                 pixbuf = src[2]
            c_proc(gdk_cairo_set_source_pixbuf,{cairo,pixbuf,x,y})
            c_proc(cairo_paint,{cairo})
            c_proc(cairo_fill,{cairo})
            c_proc(cairo_destroy,{cairo})
        else
            ?9/0 -- gImage -> gImage
        end if
    elsif backend=WinAPI then
        if atom(tgt) then   -- a gCanvas
--DEV/SUG (not quite the time for me to add a buglette right now...)
--          assert(gdx(tgt) and ctrl_types[tgt]=CANVAS)
            atom destDC = ctrl_xtra[tgt][CX_CANVAS_HDC],
                  srcDC = c_func(xCreateCompatibleDC,{destDC}),
                {hDIB,w,h,cTrans} = src[2],
                 prevBM = c_func(xSelectObject,{srcDC,hDIB})
            bool bOK
--DEV or use AlphaBlend if it is a 32-bit bitmap??
            if cTrans then
                cTrans = xpg_WinAPI_rgb_to_bgr(cTrans)
                bOK = c_func(xTransparentBlt,{destDC,x,y,w,h,srcDC,0,0,w,h,cTrans})
            else                
                bOK = c_func(xBitBlt,{destDC,x,y,w,h,srcDC,0,0,SRCCOPY})
            end if
            assert(bOK)
            prevBM = c_func(xSelectObject,{srcDC,prevBM})
            bOK = c_func(xDeleteDC,{srcDC})
            assert(bOK)
        else
            ?9/0 -- gImage -> gImage
        end if
    else
        ?9/0 -- (unknown backend)
    end if
end procedure

--/*
--dang, 2.32, not in my 2.24:
--GdkPixbuf*
--gdk_pixbuf_new_from_bytes (
--  GBytes* data,
--  GdkColorspace colorspace,
--  gboolean has_alpha,
--  int bits_per_sample,
--  int width,
--  int height,
--  int rowstride
--)
--deprecated: (gone in gtk3)
--GdkPixbuf*
--gdk_pixbuf_new_from_inline (
--  gint data_length,
--  const guint8* data,
--  gboolean copy_pixels,
--  GError** error
--)
GdkPixbuf*
gdk_pixbuf_add_alpha (
    const GdkPixbuf* pixbuf,
    gboolean substitute_color,
    guchar r,
    guchar g,
    guchar b
)

GdkPixbuf*
gdk_pixbuf_new_from_data (
  const guchar* data,
  GdkColorspace colorspace,
  gboolean has_alpha,
  int bits_per_sample,
  int width,
  int height,
  int rowstride,
  GdkPixbufDestroyNotify destroy_fn,
  gpointer destroy_fn_data
)
void
gdk_pixbuf_fill (
  GdkPixbuf* pixbuf,
  guint32 pixel
)
int
gdk_pixbuf_get_bits_per_sample (
  const GdkPixbuf* pixbuf
)
both gtk2 and gtk3:
gdk_pixbuf_get_bits_per_sample
gdk_pixbuf_get_colorspace
gdk_pixbuf_get_file_info
gdk_pixbuf_get_formats
gdk_pixbuf_get_has_alpha
gdk_pixbuf_get_height
gdk_pixbuf_get_n_channels
gdk_pixbuf_get_option
gdk_pixbuf_get_pixels
gdk_pixbuf_get_rowstride
gdk_pixbuf_get_type
gdk_pixbuf_get_width

constant GDK_COLORSPACE_RGB = 0
static void put_pixel(GdkPixbuf *pixbuf, int x, int y, guchar red, guchar green, guchar blue, guchar alpha) {
  int n_channels = gdk_pixbuf_get_n_channels(pixbuf);

  // Ensure that the pixbuf is valid
  g_assert (gdk_pixbuf_get_colorspace (pixbuf) == GDK_COLORSPACE_RGB);

  g_assert (gdk_pixbuf_get_bits_per_sample (pixbuf) == 8);
  g_assert (gdk_pixbuf_get_has_alpha (pixbuf));
  g_assert (n_channels == 4);

  int width = gdk_pixbuf_get_width (pixbuf);
  int height = gdk_pixbuf_get_height (pixbuf);

  // Ensure that the coordinates are in a valid range
  g_assert (x >= 0 && x < width);
  g_assert (y >= 0 && y < height);

  int rowstride = gdk_pixbuf_get_rowstride (pixbuf);

  // The pixel buffer in the GdkPixbuf instance
  guchar *pixels = gdk_pixbuf_get_pixels (pixbuf);

  // The pixel we wish to modify
  guchar *p = pixels + y * rowstride + x * n_channels;
  p[0] = red;
  p[1] = green;
  p[2] = blue;
  p[3] = alpha;
}

-- demo\rosetta\Bitmap_Greyscale.exw  (runnable version)

function to_grey(sequence image)
    integer dimx = length(image),
            dimy = length(image[1])
    for x=1 to dimx do
        for y=1 to dimy do
            integer pixel = image[x][y]          -- red,green,blue
            sequence r_g_b  =  sq_and_bits(pixel,{#FF0000,#FF00,#FF})
            integer {r,g,b} = sq_floor_div(r_g_b,{#010000,#0100,#01})
            image[x][y] = floor(0.2126*r + 0.7152*g + 0.0722*b)*#010101
        end for
    end for
    return image
end function

--include ppm.e   -- read_ppm(), write_ppm(), to_grey()  (as distributed, instead of the above)

sequence img = read_ppm("Lena.ppm")
img = to_grey(img)
write_ppm("LenaGray.ppm",img)

Hmm, put it anywhere with translate etc, from https://www.cairographics.org/samples/image/ :
int              w, h;
cairo_surface_t *image;

image = cairo_image_surface_create_from_png ("data/romedalen.png");
w = cairo_image_surface_get_width (image);
h = cairo_image_surface_get_height (image);

cairo_translate (cr,128.0,128.0);
cairo_rotate (cr,45*M_PI/180);
cairo_scale  (cr,256.0/w,256.0/h);
cairo_translate (cr,-0.5*w,-0.5*h);

cairo_set_source_surface (cr,image,0,0);
--void cairo_set_source_surface (cairo_t *cr, cairo_surface_t *surface, double x,  double y);
cairo_paint (cr);
cairo_surface_destroy (image);
ooh: gdk_cairo_set_source_pixbuf
--gdk_draw_pixbuf -- deprecated, 2.0 only
static gboolean do_expose (GtkWidget *da, GdkEvent *event, gpointer data) {
    (void)event; (void)data;
    GError *err = NULL;
    GdkPixbuf *pix = gdk_pixbuf_new_from_file("/usr/share/icons/cab_view.png",&err);
    if(err) {
        printf("Error : %s\n",err->message);
        g_error_free(err);
        return FALSE;
    }
    cairo_t *cr = gdk_cairo_create (da->window);
    gdk_cairo_set_source_pixbuf(cr,pix,0,0);
    cairo_paint(cr);
    cairo_fill (cr);
    cairo_destroy (cr);
    return FALSE;
}
updated/completed:
// gcc expose.c -o expose `pkg-config gtk+-3.0 --cflags --libs`
#include <gtk/gtk.h>
#include <stdlib.h>

static gboolean on_window_draw (GtkWidget *da, GdkEvent *event, gpointer data) {
    (void)event; (void)data;
    GError *err = NULL;
    GdkPixbuf *pix = gdk_pixbuf_new_from_file("/usr/share/icons/cab_view.png",&err);
    if(err) {
        printf("Error : %s\n",err->message);
        g_error_free(err);
        return FALSE;
    }
    cairo_t *cr = gdk_cairo_create(gtk_widget_get_window(da));
    //    cr = gdk_cairo_create(da->window);
    gdk_cairo_set_source_pixbuf(cr,pix,0,0);
    cairo_paint(cr);
    //    cairo_fill(cr);
    cairo_destroy(cr);
    //    return FALSE;
}

int main(int argc, char **argv) {
    GtkWidget *window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    GtkWidget *canvas;
    gtk_widget_set_size_request(window,50,50);

    g_signal_connect(window,"destroy",gtk_main_quit,NULL);
    canvas = gtk_drawing_area_new();
    gtk_container_add(window,canvas);
    g_signal_connect(canvas,"draw",on_window_draw,NULL);

    gtk_widget_set_app_paintable(canvas,TRUE);
    gtk_widget_show_all(window);
    gtk_main();
    return 0;
}
--umm:
void access_pixel( Glib::RefPtr<Gdk::Pixbuf> imageptr, int x, int y) {
   if ( !imageptr ) return;
   Gdk::Pixbuf & image = *imageptr.operator->(); // just for convenience

   if ( ! image.get_colorspace() == Gdk::COLORSPACE_RGB ) return;
   if ( ! image.get_bits_per_sample() == 8 ) return;
   if ( !( x>=0 && y>=0 && x<image.get_width() && y<image.get_height() ) ) return;

   int offset = y*image.get_rowstride() + x*image.get_n_channels();
   guchar * pixel = &image.get_pixels()[ offset ]; // get pixel pointer
   if ( pixel[0]>128 ) pixel[1] = 0; // conditionally modify the green channel

   queue_draw(); // redraw after modify
}
void MyArea::on_draw(const Cairo::RefPtr<Cairo::Context>& cr, int width, int height) {
  auto image = Gdk::Pixbuf::create_from_file("myimage.png");
  // Draw the image at 110, 90, except for the outermost 10 pixels.
  Gdk::Cairo::set_source_pixbuf(cr,image,100,80);
  cr->rectangle(110,90,image->get_width()-20,image->get_height()-20);
  cr->fill();
}

--button with image: (let's get an image on a gCanvas working first...)
GtkWidget *image = gtk_image_new_from_file ("...");
GtkWidget *button = gtk_button_new_with_label ("...");
gtk_button_set_always_show_image (GTK_BUTTON (button),TRUE);
gtk_button_set_image (GTK_BUTTON (button),image);

button must have BS_BITMAP at creation
SendMessage(hButton,BM_SETIMAGE,IMAGE_BITMAP,hBmp);

--scrollable canvas:
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <ctype.h>
#include <gtk/gtk.h>

#define ZOOMING_STEP 1.2
#define SCALING_FACTOR_INIT 1.0/10.0

typedef struct m_data {
    double scaling_factor;
    cairo_surface_t *rectangle_surface_p, *final_surface_p;
    GtkWidget *window_p, *drawing_area_p;
    GtkAdjustment *hadjust_p, *vadjust_p;
} m_data_struct;

static void activate(GtkApplication *, gpointer);
static gboolean zoom_it(GtkWidget *, GdkEvent *, gpointer);
static gboolean configure_it(GtkWidget *, GdkEventConfigure *, gpointer);
static gboolean draw_it(GtkWidget *, cairo_t *, gpointer);

int main(int argc, char **argv) {
    m_data_struct my_data;
    my_data.scaling_factor = SCALING_FACTOR_INIT;
    my_data.final_surface_p = NULL;
    GtkApplication *app_p = gtk_application_new("calc.foil",G_APPLICATION_FLAGS_NONE);
    g_signal_connect(app_p,"activate",G_CALLBACK(activate),&my_data);
    int status = g_application_run(G_APPLICATION(app_p),0,NULL);
    g_object_unref(app_p);
}

static void activate(GtkApplication *app_p, gpointer g_data_p) {

    m_data_struct *my_data_p = (m_data_struct *)g_data_p;
    my_data_p->window_p = gtk_application_window_new(app_p);
    gtk_window_set_title(GTK_WINDOW(my_data_p->window_p),"Fot Calculation");
    gtk_container_set_border_width(GTK_CONTAINER(my_data_p->window_p),8);
    GtkWidget *notebook_p = gtk_notebook_new();
    gtk_container_add(GTK_CONTAINER(my_data_p->window_p), notebook_p);
    GtkWidget *grid0_p = gtk_grid_new();
    gtk_notebook_append_page(GTK_NOTEBOOK(notebook_p), grid0_p, gtk_label_new("First Tab"));
    GtkWidget *grid1_p = gtk_grid_new();
    gtk_grid_attach(GTK_GRID(grid0_p), grid1_p, 2, 2, 2, 50);

    GtkWidget *frame_p = gtk_frame_new("Rectangle");
    gtk_frame_set_shadow_type(GTK_FRAME(frame_p), GTK_SHADOW_NONE);
    GtkWidget *frame0_p = gtk_frame_new(NULL);
    GtkWidget *scrolled_window_p = gtk_scrolled_window_new(NULL, NULL);
    my_data_p->hadjust_p = gtk_scrolled_window_get_hadjustment(GTK_SCROLLED_WINDOW(scrolled_window_p));
    my_data_p->vadjust_p = gtk_scrolled_window_get_vadjustment(GTK_SCROLLED_WINDOW(scrolled_window_p));
    GtkWidget *drawing_area_p = gtk_drawing_area_new();
    gtk_widget_set_size_request(scrolled_window_p, 300, 300);
    g_signal_connect(drawing_area_p, "configure-event", G_CALLBACK(configure_it), g_data_p);
    g_signal_connect(drawing_area_p, "draw", G_CALLBACK(draw_it), g_data_p);
    g_signal_connect(drawing_area_p, "scroll-event", G_CALLBACK(zoom_it), g_data_p);
    gtk_widget_set_events(drawing_area_p, gtk_widget_get_events(drawing_area_p)|GDK_SCROLL_MASK);
    my_data_p->drawing_area_p = drawing_area_p;

    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window_p), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_container_add(GTK_CONTAINER(scrolled_window_p), drawing_area_p);
    gtk_container_add(GTK_CONTAINER(frame_p), frame0_p);
    gtk_container_add(GTK_CONTAINER(frame0_p), scrolled_window_p);
    gtk_grid_attach(GTK_GRID(grid1_p), frame_p, 1, 0, 1, 2);
    my_data_p->rectangle_surface_p = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, 3000, 3000);
    cairo_t *cr_p = cairo_create(my_data_p->rectangle_surface_p);
    cairo_set_line_width(cr_p, 50);
    cairo_rectangle(cr_p, 100, 100, 1375, 1375);
    cairo_rectangle(cr_p, 1525, 1525, 1375, 1375);
    cairo_set_source_rgb(cr_p, 0, 0, 0);
    cairo_stroke(cr_p);

    gtk_widget_show_all(my_data_p->window_p);
}

static gboolean zoom_it(GtkWidget *widget_p, GdkEvent *event_p, gpointer g_data_p) {
    gdouble new_x, new_y;
    int do_zoom = 0;
    m_data_struct *my_data_p = (m_data_struct *)g_data_p;
    GdkEventScroll *this_event_p = (GdkEventScroll *)event_p;
    if (this_event_p->direction == GDK_SCROLL_UP) {
        my_data_p->scaling_factor *= ZOOMING_STEP;
        /* we need to calc the new upper to set value, +1 for inaccuracy */
        gtk_adjustment_set_upper(my_data_p->hadjust_p,gtk_adjustment_get_upper(my_data_p->hadjust_p)*ZOOMING_STEP+1);
        gtk_adjustment_set_upper(my_data_p->vadjust_p,gtk_adjustment_get_upper(my_data_p->vadjust_p)*ZOOMING_STEP+1);      
        new_x = this_event_p->x * ZOOMING_STEP;
        new_y = this_event_p->y * ZOOMING_STEP;
        do_zoom = 1;
    }
    if (this_event_p->direction == GDK_SCROLL_DOWN) {
        double sf = my_data_p->scaling_factor / ZOOMING_STEP;
        if (sf >= SCALING_FACTOR_INIT / (1 + (ZOOMING_STEP - 1) / 2)) {
            /* zoom out max till level 0 but preventing inability */
            /* not to zoom to level 0 due to inaccurancy */
            my_data_p->scaling_factor = sf;
            new_x = this_event_p->x / ZOOMING_STEP;
            new_y = this_event_p->y / ZOOMING_STEP;
            do_zoom = 1;
        }
    }
    if (do_zoom) {
        gtk_adjustment_set_value(my_data_p->hadjust_p,new_x+gtk_adjustment_get_value(my_data_p->hadjust_p)-this_event_p->x);
        gtk_adjustment_set_value(my_data_p->vadjust_p,new_y+gtk_adjustment_get_value(my_data_p->vadjust_p)-this_event_p->y);
        configure_it(widget_p, (GdkEventConfigure *)event_p, g_data_p);
        gtk_widget_queue_draw(widget_p);
    }
    return TRUE;
}

static gboolean configure_it(GtkWidget *widget_p, GdkEventConfigure *event_p, gpointer g_data_p) {
    m_data_struct *my_data_p = (m_data_struct *)g_data_p;
    if (my_data_p->final_surface_p) { cairo_surface_destroy(my_data_p->final_surface_p); }
    int s = 3000 * my_data_p->scaling_factor;
    my_data_p->final_surface_p = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, s, s);
    gtk_widget_set_size_request(my_data_p->drawing_area_p, s, s);
    cairo_t *cr_p = cairo_create(my_data_p->final_surface_p);
    cairo_scale(cr_p, my_data_p->scaling_factor, my_data_p->scaling_factor);
    cairo_set_source_surface(cr_p, my_data_p->rectangle_surface_p, 0, 0);
    cairo_paint(cr_p);
    cairo_destroy(cr_p);
    return TRUE;
}

static gboolean draw_it(GtkWidget *widget_p, cairo_t *cr_p, gpointer g_data_p) {
    cairo_set_source_surface(cr_p, ((m_data_struct *)g_data_p)->final_surface_p, 0, 0);
    cairo_paint(cr_p);
    return FALSE;
}
from https://stackoverflow.com/questions/67145644/how-do-i-draw-a-pixmap-with-gtk
#include <gtk/gtk.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

const int WIDTH = 1080;
const int HEIGHT = 720;

GtkWidget* mainWindow;

int currentCol = 0;

uint32_t* framebuffer = NULL;
GdkPixbuf* pixbuf = NULL;


typedef struct _rgbColor {
    uint8_t red;
    uint8_t green;
    uint8_t blue;
    uint8_t alpha;
}rgbColor;

void encodePixel(uint32_t* fb, rgbColor c, int x, int y) {
    uint32_t r, g, b, a;

    r = c.red;
    g = c.green << 8;
    b = c.blue << 16;
    a = c.alpha << 24;
    
//  *(fb + (sizeof(uint32_t)*y+x)) = b | g | r | a;
    fb[WIDTH*y+x] = b | g | r | a;
}

void fillWithColour(uint32_t* fb, rgbColor c) {
    for (int y = 0; y < HEIGHT; y++) {
        for (int x = 0; x < WIDTH; x++) {
            encodePixel(fb, c, x, y);
        }
    }
}

void fillEveryInterval(uint32_t* fb, rgbColor c, int interval) {
    for (int y = 1; y < HEIGHT; y += interval) {
        for (int x = 0; x < WIDTH; x++) {
            encodePixel(fb, c, x, y);
        }
    }
}

gboolean onTimerTick(gpointer user_data) {
    rgbColor c = {0, 0, 0, 255};
    if (currentCol == 0) {
        c.red = 255;
    }
    if (currentCol == 1) {
        c.green = 255;
    }
    if (currentCol == 2) {
        c.blue = 255;
        currentCol = -1;
    }
    currentCol++;
    fillWithColour(framebuffer, c);

    rgbColor c1 = {0, 0, 255, 255};
    fillEveryInterval(framebuffer, c1, 20);
    gtk_widget_queue_draw(mainWindow);
    return 1;
}

gboolean onDraw(GtkWidget* widget, cairo_t *cr, gpointer user_data) {
    gdk_cairo_set_source_pixbuf(cr, pixbuf, 0, 0);
    cairo_paint(cr);
    return 0;
}

void onWindowDestroy (GtkWidget* object, gpointer user_data) {
    gtk_main_quit();
}

int main() {
    framebuffer = malloc(sizeof(uint32_t)*WIDTH*HEIGHT);
    rgbColor c = {255, 0, 0, 255};
    fillWithColour(framebuffer, c);


    gtk_init(NULL, NULL);
    mainWindow = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_default_size(GTK_WINDOW(mainWindow), WIDTH, HEIGHT);
    gtk_window_set_title(GTK_WINDOW(mainWindow), "Framebuffer test");

    GtkWidget* drawingArea = gtk_drawing_area_new();

    gtk_container_add(GTK_CONTAINER(mainWindow), drawingArea);

    g_signal_connect(GTK_WINDOW(mainWindow), "destroy", (GCallback)onWindowDestroy, NULL);
    g_signal_connect(GTK_DRAWING_AREA(drawingArea), "draw", (GCallback)onDraw, NULL);

    g_timeout_add(500, onTimerTick, NULL);

    gtk_widget_show_all(GTK_WINDOW(mainWindow));
    pixbuf = gdk_pixbuf_new_from_data(framebuffer, GDK_COLORSPACE_RGB, true, 8, WIDTH, HEIGHT, WIDTH*4, NULL, NULL);
    gtk_main();
}
[DONE:] GdkPixbuf *gdk_pixbuf_new_from_data (const guchar *data,
                     GdkColorspace colorspace,
                     gboolean has_alpha,
                     int bits_per_sample,
                     int width, int height,
                     int rowstride,
                     GdkPixbufDestroyNotify destroy_fn,
                     gpointer destroy_fn_data);
typedef enum {
    GDK_COLORSPACE_RGB
} GdkColorspace;

--*/

local procedure xpg_Label(gdx id)
    -- (invoked via xpg_map)
    gdx parent = xpg_get_parent_id(id)
    atom label
    nullable_string title = gGetAttribute(id,"TITLE",NULL)
--  if title=NULL then title="" end if
    if backend=GTK then
        label = c_func(gtk_label_new,{title})
        xpg_setID(label,id)
        xpg_gtk_add_to(parent,label)
--      c_proc(gtk_container_add,{ctrl_handles[parent],label})
--      xpg_signal_connect(label,"realize",xpg_gtk_widget_realized,id)
        c_proc(gtk_widget_realize,{label})
    elsif backend=WinAPI then
        atom dwStyle = or_all({WS_CHILD,WS_VISIBLE,ES_LEFT})
--             {w,h} = xpg_set_label_natural_size(id,title)
--integer
--             {w,h} = gGetTextExtent(parent,split(title,'\n'))
--?{"label",title,w,h} 
--h*=4 -- works!
--      ctrl_size[id][SZ_NATURAL_W] = w
--      ctrl_size[id][SZ_NATURAL_H] = h
        label = xpg_WinAPI_create(id,"static",title,parent,50,15,dwStyle,0)
    else
        ?9/0 -- (unknown backend)
    end if
end procedure

function xpg_set_label_attribute(gdx id, string name, object v, bool bMapped)
    atom handle = ctrl_handles[id]
    if name="TITLE" then
        assert(string(v))
        if not bMapped then
            xpg_defer_attr(id,name,v)
        elsif backend=GTK then
            c_proc(gtk_label_set_text_with_mnemonic,{handle,xpg_gtk_mnemonicalize(v)})
        elsif backend=WinAPI then
            c_proc(xSetWindowText,{handle,v})
--          xpg_set_label_natural_size(id,title)
        else
            ?9/0 -- (unknown backend)
        end if
        return true
    elsif name="ALIGNMENT" then
--Possible values: "ALEFT", "ACENTER" and "ARIGHT", combined to "ATOP", "ACENTER" and "ABOTTOM". 
--Default: "ALEFT:ACENTER". 
--Partial values are also accepted, like "ARIGHT" or ":ATOP", the other value will be used from the current alignment. 
        printf(1,"gSetAttribute(LABEL,\"%s\",%v)...\n",{name,v})
        return true
    elsif name="SEPARATOR" then
        printf(1,"gSetAttribute(LABEL,\"%s\",%v)...\n",{name,v})
        return true
    end if
    return false
end function

function xpg_get_label_attribute(gdx id, string name, object dflt)
    atom handle = ctrl_handles[id]
    if name="TITLE" then
        if backend=GTK then
            return peek_string(c_func(gtk_label_get_text,{handle}))
        elsif backend=WinAPI then
            integer l = c_func(xGetWindowTextLength,{handle})
            string title = repeat(' ',l)
            c_proc(xGetWindowText,{handle,title,l+1})
            return title
        else
            ?9/0 -- (unknown backend)
        end if
    end if
    return xpg_return_default_attr(id,name,dflt)
end function

global function gLabel(nullable_string title=NULL, sequence attributes="", args={})
    {title,attributes,args} = paranormalise_taa(title,attributes,args)
    if not bInit then xpg_Init() end if
    integer id = xpg_add_control(LABEL)
--test_elem = id
--  xpg_register_handler(LABEL,"XXX",{{1,1,"PO"}})
    set_ctrl_msg(LABEL,xpg_Label,xpg_set_label_attribute,
                                 xpg_get_label_attribute)
--DEV...
--  gSetAttribute(id,"FONT","Helvetica, 9")
    if title!=NULL then
if backend=GTK then title &= " " end if     --DEV in case italic...
        gSetAttribute(id,"TITLE",title)
    end if
    if length(attributes) then
        gSetAttributes(id,attributes,args)
    end if
    return id
end function 

local function xpg_default_DROP(integer rid) return rid end function
rtn_default_drop = xpg_default_DROP; -- (let/permit/allow this to be a perfectly valid rtn)

local constant lSigs = {{1,1,"FI"},{2,2,"FOI"}}
local enum              lFI,       lFOI

local procedure redraw_list(gdx list)
--?9/0
--?"redraw_table"
-- [DEV] columns of {80,20,100} get 80/200,(4/10) 20/120 (1/6), and 100/100 (100%) of any slack respectively...
-- (later) First job: determine whether there is a vertical scrollbar... [unless that's already taken care of...]
    object lstattr = ctrl_xtra[list][CX_GTL_ATTRS]
--  if sequence(sortcol) then sortcol = sortcol[1] end if
    object data = lstattr[LX_DATA]
--  sequence columns = deep_copy(tblattr[TX_COLUMNS]),
--           actcols = repeat(80,length(columns)),
--           colalgn = repeat('L',length(columns)),
--           ttlalgn = repeat('L',length(columns))
--           data = tblattr[TX_DATA],
--           tags = tblattr[TX_TAGSET]
--  lstattr = 0 -- (kill refcount)
--/*
    for i,c in columns do
        if not string(c) then
--          c = c[2]
--          if c<0 then c =-c; colexpd[i]='N' end if
            actcols[i] = c[2]
            if length(c)=3 then
                string align = c[3]
                ttlalgn[i] = align[1]
                colalgn[i] = align[$]
            end if
            columns[i] = c[1]
        end if
    end for
--*/
--          {width, height} = gGetAttribute(list, "DRAWSIZE"),
    integer {width, height} = gGetAttribute(list, "SIZE"),
            n_rows, sIdx, lh = gGetTextExtent(list,"X")[2],
            drop = gGetHandler(list,"DROP",xpg_default_DROP),
--          xCanvasSetForeground = drop(gCanvasSetForeground),
--          xCanvasRect = drop(gCanvasRect),
--          xCanvasLine = drop(gCanvasLine),
            xCanvasText = drop(gCanvasText),
            -- /now/ it feels safe to shadow the globals:
--          gCanvasSetForeground = xCanvasSetForeground,
--          gCanvasRect = xCanvasRect,
--          gCanvasLine = xCanvasLine,
            gCanvasText = xCanvasText
    if integer(data) then
        sequence sig = get_routine_info(data,false)
        sIdx = find(sig,lSigs)
        if sIdx=lFI then
            n_rows = data(0)
        elsif sIdx=lFOI then
            n_rows = data(list,0)
        else
            ?9/0 -- unrecognised sig
        end if
    else
        n_rows = length(data)
    end if
    sequence si
--  integer nr = min(n_rows,ceil(height/lh)-1), vy = 13
    integer nr = min(n_rows,ceil(height/lh)-1), vy = 9
    for r=1 to nr do
        if integer(data) then
            if sIdx=lFI then
                si = data(r)
            elsif sIdx=lFOI then
                si = data(list,r)
            end if
        else
            si = data[r]
        end if
        if string(si) then
            gCanvasText(list,2,vy,si)
        else
            atom vx = 2
            for sii in si do
                atom sic = -1, sis = -1
                if not string(sii) then
                    if not sequence(sii) then
                        crash("fragments must be string or {string,colour[,style}}")
                    end if
                    if length(sii)=2 then
                        {sii, sic} = sii
                    elsif length(sii)=3 then
                        {sii, sic, sis} = sii
                    else
                        crash("unrecognised fragment[length]:%v",{sii})
                    end if
                end if
                assert(string(sii))
--              gCanvasText(list,vx,vy,sii,colour:=sic,style:=sis)
                gCanvasText(list,vx,vy,sii,XPG_E,0,sic,sis)
                vx += gGetTextExtent(list,sii)[1]
            end for
        end if
        vy += lh
    end for
--/*
--          colsum = sum(actcols),
            colsum = sum(filter(actcols,">",0)),
--          slack = width-colsum
            slack = width-sum(sq_abs(actcols))
    if slack>0 then
        for i,c in actcols do
            if c>0 then
                integer ci = floor((c/colsum)*slack)
                actcols[i] += ci
                slack -= ci
                colsum -= c
            end if
        end for
    end if
    actcols = sq_abs(actcols)
    -- and save it for subsequent mouse handling:
    ctrl_xtra[table][CX_GTL_ATTRS][TX_ACTCOLS] = actcols
    
--string actcolstr = sprintf("%v",{actcols})
--string actcolstr = join(actcols,fmt:="%d")
--?{"width",width,"height",height,"columns",columns,"actcols",actcolstr,sum(actcols)}
    gCanvasRect(table,0,width,0,24,true,colour:=XPG_GREY,fillcolour:=XPG_GREY)
    gCanvasSetForeground(table,XPG_BLACK)
    integer vx = 0, sortcol = 0, csgn = 0,
            nr = min(length(data[1]),ceil(height/24)-1)
    if sequence(ctrl_xtra[table][CX_GTL_ATTRS][TX_SORTCOLS]) then
        sortcol = ctrl_xtra[table][CX_GTL_ATTRS][TX_SORTCOLS][1]
        csgn = sign(sortcol)
        sortcol = abs(sortcol)
    end if
    for c,a in actcols do   -- the vertical lines and column titles
        gCanvasLine(table,vx,0,vx,height,colour:=XPG_LIGHT_GREY)
        integer ta = ttlalgn[c],
                tc = iff(c=sortcol?12:0), -- (space for chevron)
                {tx, talign} = iff(ta='L'?{vx+5,XPG_EAST}:
                               iff(ta='C'?{vx+floor(a/2),XPG_CENTRE}:
                               iff(ta='R'?{vx+a-5-tc,XPG_WEST}:9/0)))
        gCanvasText(table,tx,13,columns[c],talign)
        if tc then
            -- if sorted then draw the chevron
            integer hx = vx+a-10, hy = 12
            gCanvasLine(table,hx-5,hy-5*csgn,hx,hy)
            gCanvasLine(table,hx,hy,hx+5,hy-5*csgn)
            hy += 4*csgn
            gCanvasLine(table,hx-5,hy-5*csgn,hx,hy)
            gCanvasLine(table,hx,hy,hx+5,hy-5*csgn)
        end if
        integer ca = colalgn[c],
                {cx, calign} = iff(ca='L'?{vx+5,XPG_EAST}:
                               iff(ca='C'?{vx+floor(a/2),XPG_CENTRE}:
                               iff(ca='R'?{vx+a-5,XPG_WEST}:9/0)))
--DEV use the tagset[DONE], and scroll info:
        for r=1 to nr do
            integer vy = 13+24*r,
                    tr = tags[r]
            object dtrc = data[1][tr][c]
            if c<=length(data[2]) then
                object d2c = data[2][c]
                if string(d2c) then
--  d2c = `%[3]2d/%[2]02/%[1]4d`
                    dtrc = sprintf(d2c,dtrc)
                elsif sequence(d2c) then
                    dtrc = d2c[r]
                elsif d2c!=0 then
                    integer fn = d2c
                    dtrc = fn(data[1],r,c)
                end if
            end if
            if not string(dtrc) then
                dtrc = sprint(dtrc)
            end if
            gCanvasText(table,cx,vy,dtrc,calign)
        end for
        vx += a
    end for
    gCanvasSetForeground(table,XPG_LIGHT_GREY)
    gCanvasLine(table,width-1,0,width-1,height)

    for vy=0 to height by 24 do -- the horizontal lines
        gCanvasLine(table,0,vy,width,vy)
    end for 
--  gCanvasSetForeground(table,XPG_BLACK)
--*/
end procedure
rtn_list_redraw = redraw_list; -- (let/permit/allow this to be a perfectly valid rtn)

--global function gTable(sequence columns, data, integer rows=10, sequence attributes="", dword_seq args={})
--global function gList(object data, sequence attributes="", dword_seq args={})
global function gList(object data, object selected=NULL, sequence attributes="", dword_seq args={})
--  {rows,attributes,args} = paranormalise_raa(rows,attributes,args,bCheckRid:=false)
    {selected,attributes,args} = paranormalise_raa(selected,attributes,args)
--  if rows=0 then rows=10 end if
    gdx list = gCanvas(redraw_list)
--  gdx list = gCanvas(redraw_list,attributes,args)
--?9/0
--DEV/SUG:
--  if rows<0 then rows = -rows; gSetAttribute(table,"SCROLLABLE",false) end if

--  gSetAttribute(graph,"BGCOLOR",XPG_WHITE) -- (set a default)
--erm...
--  gCanvasSetForeground(graph,XPG_WHITE) -- (set a default)
--  xpg_register_handler(CANVAS,"DRID",{{1,1,"FO"}})
--  gSetHandler(graph,"DRID",drid)
--  integer natural_width = 0,
----DEV...
----            natural_height = 10+rows*10
--          natural_height = (rows+1)*24
--  for c in columns do
--      if string(c) then
--          natural_width += 80
--      else
--          natural_width += abs(c[2])
--      end if
--  end for
    ctrl_xtra[list][CX_CANVAS_TYPE] = LIST
    object l_attr = repeat(0,LX_LEN)
    l_attr[LX_DATA] = data
    ctrl_xtra[list][CX_GTL_ATTRS] = l_attr
    l_attr = 0 -- (kill refcount)
--  gSetAttributes(table,"ROWS",rows)
--  if length(attributes) then
--      gSetAttributes(list,attributes,args)
--  end if

    xpg_register_handler(CANVAS,"SELECTED",{{2,2,"POI"},{2,2,"POP"},{1,1,"PI"},{1,1,"PP"}})
    if selected!=NULL then
        gSetHandler(list,"SELECTED",selected)
    end if
    if length(attributes) then
        gSetAttributes(list,attributes,args)
    end if
--  ctrl_xtra[table] = {columns,data,rows}
    return list
end function 

local procedure xpg_ProgressBar(gdx id)
    -- (invoked via xpg_map)
    gdx parent = xpg_get_parent_id(id)
    atom progress_bar
    bool orientation = gGetAttribute(id,"ORIENTATION",0)
    if backend=GTK then
        progress_bar = c_func(gtk_progress_bar_new,{})
        xpg_setID(progress_bar,id)
        if orientation then
            if bGTK3 then
                c_proc(gtk_orientable_set_orientation,{progress_bar,GTK_ORIENTATION_VERTICAL})
                c_proc(gtk_progress_bar_set_inverted,{progress_bar,true})
            else
                c_proc(gtk_progress_bar_set_orientation,{progress_bar,GTK_PROGRESS_BOTTOM_TO_TOP})
            end if
--/* --??? (SEE IF WE CAN GET BY W/O THIS...)
  else
  {
#if GTK_CHECK_VERSION(3,0,0)
    gtk_orientable_set_orientation(GTK_ORIENTABLE(ih->handle),GTK_ORIENTATION_HORIZONTAL);
#else
    gtk_progress_bar_set_orientation((GtkProgressBar*)ih->handle,GTK_PROGRESS_LEFT_TO_RIGHT);
#endif
  }
--*/
        end if
        xpg_gtk_add_to(parent,progress_bar)
--      c_proc(gtk_container_add,{ctrl_handles[parent],progress_bar})
        c_proc(gtk_widget_realize,{progress_bar})
    elsif backend=WinAPI then
        atom dwStyle = or_all({WS_CHILD,WS_VISIBLE}),
--WS_CLIPSIBLINGS?
             {w,h} = iff(orientation?{30,200}:{200,30})
--      ctrl_size[id][SZ_NATURAL_W] = w
--      ctrl_size[id][SZ_NATURAL_H] = h
        if orientation then
            dwStyle += PBS_VERTICAL
        end if
        if not gGetAttribute(id,"DASHED",0) then
            dwStyle += PBS_SMOOTH
        end if
        progress_bar = xpg_WinAPI_create(id,"msctls_progress32","",parent,w,h,dwStyle,0)
        atom lParam = XPG_PB_MAX*#10000+0
        {} = c_func(xSendMessage,{progress_bar,PBM_SETRANGE,0,lParam})
    else
        ?9/0 -- (unknown backend)
    end if
    if orientation then
        ctrl_flags[id] += CF_VERTICAL
    end if
    ctrl_xtra[id] = {0,1,0}     -- {pbmin, pbmax, pbval}
end procedure

function xpg_set_progress_attribute(gdx id, string name, object v, bool bMapped)
    atom handle = ctrl_handles[id]
    integer k = find(name,{"MIN","MAX","VALUE"})
    if k then
        if string(v) then v = to_number(v) end if
        if not bMapped then
            xpg_defer_attr(id,name,v)
        else
            ctrl_xtra[id][k] = v
            atom {pbmin,pbmax,pbval} = ctrl_xtra[id]
            pbval = max(pbmin,min(pbval,pbmax))
            atom f = (pbval-pbmin)/(pbmax-pbmin)
            if backend=GTK then
                c_proc(gtk_progress_bar_set_fraction,{handle,f})
            elsif backend=WinAPI then
                f = floor(XPG_PB_MAX*f)
                {} = c_func(xSendMessage,{handle,PBM_SETPOS,f,0})
            else
                ?9/0 -- (unknown backend)
            end if
        end if
        return true
    elsif name="ORIENTATION" then
        if string(v) then
            integer hv = find(v,{"HORIZONTAL","VERTICAL"})
            assert(hv!=0)
            v = hv-1
        end if
        if not bMapped then
            xpg_defer_attr(id,name,v)
            return true
        end if
        -- you cannot change this after mapping...?
--      return true
    elsif name="DASHED" then
        if string(v) then v = xpg_to_bool(v) end if
        if not bMapped then
            xpg_defer_attr(id,name,v)
        elsif backend=GTK then
            if not bGTK3 then
                -- v shd be GTK_PROGRESS_CONTINUOUS|GTK_PROGRESS_DISCRETE
                c_proc(gtk_progress_bar_set_bar_style,{handle,v})
            else
                ?9/0
            end if
        elsif backend=WinAPI then
            atom dwStyle = c_func(xGetWindowLong,{handle,GWL_STYLE})
            if v then
                if and_bits(dwStyle,PBS_SMOOTH) then
                    dwStyle -= PBS_SMOOTH
                end if
            else
                if not and_bits(dwStyle,PBS_SMOOTH) then
                    dwStyle += PBS_SMOOTH
                end if
            end if  
            dwStyle = c_func(xSetWindowLong,{handle,GWL_STYLE,dwStyle})
        else
            ?9/0 -- (unknown backend)
        end if
        return true
    end if
    return false
end function

function xpg_get_progress_attribute(gdx id, string name, object dflt)
    atom handle = ctrl_handles[id]
    integer k = find(name,{"MIN","MAX","VALUE"})
    if k then
        return ctrl_xtra[id][k]
    elsif name="ORIENTATION" then
        return and_bits(ctrl_flags[id],CF_VERTICAL)!=0
    end if
    return xpg_return_default_attr(id,name,dflt)
end function

global function gProgressBar(string attributes="", args={})
    if not bInit then xpg_Init() end if
    integer id = xpg_add_control(PROGRESSBAR)
--  xpg_register_handler(PROGRESSBAR,"VALUE_CHANGED",{{1,1,"PO"}})
    set_ctrl_msg(PROGRESSBAR,xpg_ProgressBar,xpg_set_progress_attribute,
                                             xpg_get_progress_attribute)
    if length(attributes) then
        gSetAttributes(id,attributes,args)
    end if
--  gSetAttribute(id,"EXPAND",false)
--  gSetInt(id,"EXPAND",false)
    return id
end function 

--/*
static int gtkProgressBarMapMethod(Ihandle* ih)
{
--  ih->handle = gtk_progress_bar_new();
--  if (iupStrEqualNoCase(iupAttribGetStr(ih,"ORIENTATION"),"VERTICAL"))
--  {
--#if GTK_CHECK_VERSION(3,0,0)
--  gtk_orientable_set_orientation(GTK_ORIENTABLE(ih->handle),GTK_ORIENTATION_VERTICAL);
--void         gtk_orientable_set_orientation (GtkOrientable  *orientable,
--                                             GtkOrientation  orientation);
----GtkOrientation gtk_orientable_get_orientation (GtkOrientable    *orientable);
--typedef enum
--{
--  GTK_ORIENTATION_HORIZONTAL,
--  GTK_ORIENTATION_VERTICAL
--} GtkOrientation;
--  gtk_progress_bar_set_inverted(GTK_PROGRESS_BAR(ih->handle),TRUE);
--void gtk_progress_bar_set_inverted(GtkProgressBar *pbar, gboolean inverted);
--#else
--  gtk_progress_bar_set_orientation(GTK_PROGRESS_BAR(ih->handle),GTK_PROGRESS_BOTTOM_TO_TOP);
--void gtk_progress_bar_set_orientation(GtkProgressBar *pbar, GtkProgressBarOrientation orientation);
--typedef enum
--{
--  GTK_PROGRESS_LEFT_TO_RIGHT,
--  GTK_PROGRESS_RIGHT_TO_LEFT,
--  GTK_PROGRESS_BOTTOM_TO_TOP,
--  GTK_PROGRESS_TOP_TO_BOTTOM
--} GtkProgressBarOrientation;
--
--#endif

    if (ih->userheight < ih->userwidth) {
      int tmp = ih->userheight;
      ih->userheight = ih->userwidth;
      ih->userwidth = tmp;
    }
  } else {
#if GTK_CHECK_VERSION(3,0,0)
    gtk_orientable_set_orientation(GTK_ORIENTABLE(ih->handle),GTK_ORIENTATION_HORIZONTAL);
#else
    gtk_progress_bar_set_orientation((GtkProgressBar*)ih->handle,GTK_PROGRESS_LEFT_TO_RIGHT);
#endif
  }

  return IUP_NOERROR;
}

void iupdrvProgressBarInitClass(Iclass* ic) {
  /* Driver Dependent Class functions */
  ic->Map = gtkProgressBarMapMethod;

  /* Driver Dependent Attribute functions */
  
  /* Visual */
  iupClassRegisterAttribute(ic,"BGCOLOR",NULL,iupdrvBaseSetBgColorAttrib,IUPAF_SAMEASSYSTEM,"DLGBGCOLOR",IUPAF_DEFAULT);
  
  /* Special */
  iupClassRegisterAttribute(ic, "FGCOLOR", NULL, NULL, NULL, NULL, IUPAF_DEFAULT);

  /* IupProgressBar only */
  iupClassRegisterAttribute(ic, "VALUE",  iProgressBarGetValueAttrib,  gtkProgressBarSetValueAttrib,  NULL, NULL, IUPAF_NO_DEFAULTVALUE|IUPAF_NO_INHERIT);
#if !GTK_CHECK_VERSION(3, 0, 0)
  iupClassRegisterAttribute(ic, "DASHED", iProgressBarGetDashedAttrib, gtkProgressBarSetDashedAttrib, NULL, NULL, IUPAF_NO_INHERIT);
#endif
  iupClassRegisterAttribute(ic, "ORIENTATION", NULL, NULL, IUPAF_SAMEASSYSTEM, "HORIZONTAL", IUPAF_NO_INHERIT);
  iupClassRegisterAttribute(ic, "DASHED",      NULL, NULL, NULL, NULL, IUPAF_NO_INHERIT);
}


static int winProgressBarSetValueAttrib(Ihandle* ih, const char* value) {
  if (!value)
    ih->data->value = 0;
  else
    iupStrToDouble(value,&(ih->data->value));

  iProgressBarCropValue(ih);
void iProgressBarCropValue(Ihandle* ih) {
  if(ih->data->value > ih->data->vmax)
    ih->data->value = ih->data->vmax;
  else if(ih->data->value < ih->data->vmin)
    ih->data->value = ih->data->vmin;
}


    double factor = (ih->data->value - ih->data->vmin) / (ih->data->vmax - ih->data->vmin);
    int val = (int)(XPG_PB_MAX * factor);
    SendMessage(ih->handle,PBM_SETPOS,(WPARAM)val,0);

    return 0;
}
static int winProgressBarMapMethod(Ihandle* ih) {
  DWORD dwStyle = WS_CHILD|WS_CLIPSIBLINGS;

  if (!ih->parent)
    return IUP_ERROR;

  if (iupStrEqualNoCase(iupAttribGetStr(ih,"ORIENTATION"),"VERTICAL"))
  {
    dwStyle |= PBS_VERTICAL;

    if (ih->userheight < ih->userwidth)
    {
      int tmp = ih->userheight;
      ih->userheight = ih->userwidth;
      ih->userwidth = tmp;
    }
  }

  if (!iupwin_comctl32ver6 && !iupAttribGetBoolean(ih,"DASHED"))
    dwStyle |= PBS_SMOOTH;

  if (!iupwinCreateWindow(ih,PROGRESS_CLASS,0,dwStyle,NULL))
    className[ProgressBar] = PROGRESS_CLASSA
    classBase[ProgressBar] = PROGRESSBAR
    classStyle[ProgressBar] = or_all({WS_CHILD,
                                      WS_VISIBLE})
    PROGRESS_CLASSA     = "msctls_progress32",
    return IUP_ERROR;

  /* configure the native range */
  SendMessage(ih->handle,PBM_SETRANGE,0,MAKELPARAM(0,XPG_PB_MAX));

  return IUP_NOERROR;
}

void iupdrvProgressBarInitClass(Iclass* ic)
{
  /* Driver Dependent Class functions */
  ic->Map = winProgressBarMapMethod;

  /* Visual */
  iupClassRegisterAttribute(ic,"BGCOLOR", NULL, winProgressBarSetBgColorAttrib, IUPAF_SAMEASSYSTEM, "DLGBGCOLOR", IUPAF_DEFAULT);   

  /* Special */
  /* Only works when using Classic style */
  if (iupwin_comctl32ver6)
    iupClassRegisterAttribute(ic, "FGCOLOR", NULL, winProgressBarSetFgColorAttrib, IUPAF_SAMEASSYSTEM, "DLGFGCOLOR", IUPAF_DEFAULT);
  else
    iupClassRegisterAttribute(ic, "FGCOLOR", NULL, NULL, NULL, NULL, IUPAF_NOT_MAPPED);

  /* IupProgressBar only */
  iupClassRegisterAttribute(ic, "VALUE",  iProgressBarGetValueAttrib,  winProgressBarSetValueAttrib,  NULL, NULL, IUPAF_NO_DEFAULTVALUE|IUPAF_NO_INHERIT);
  iupClassRegisterAttribute(ic, "ORIENTATION", NULL, NULL, IUPAF_SAMEASSYSTEM, "HORIZONTAL", IUPAF_NO_INHERIT);
  iupClassRegisterAttribute(ic, "MARQUEE",     NULL, winProgressBarSetMarqueeAttrib, NULL, NULL, IUPAF_NO_INHERIT);
  iupClassRegisterAttribute(ic, "DASHED",      NULL, NULL, NULL, NULL, IUPAF_NO_INHERIT);
}
--*/

local function xpg_gtk_slider_changed(atom handle, gdx id) -- (GTK only)
    assert(id=xpg_getID(handle))
    integer value_changed = gGetHandler(id,"VALUE_CHANGED")
    if value_changed then
        value_changed(id)
    end if
    return 0 -- (ignored)
end function

local procedure xpg_Slider(gdx id)
    -- (invoked via xpg_map)
    gdx parent = xpg_get_parent_id(id)
    atom slider
    bool orientation = gGetAttribute(id,"ORIENTATION",0)
    if backend=GTK then
--      atom adjustment = c_func(gtk_adjustment_new,{0,0,1.0,0.01,0.1,0})
        atom adjustment = c_func(gtk_adjustment_new,{0,0,100,1,20,0})
--          {C_DBL,     --  gdouble value
--           C_DBL,     --  gdouble lower
--           C_DBL,     --  gdouble upper
--           C_DBL,     --  gdouble step_increment
--           C_DBL,     --  gdouble page_increment
--           C_DBL},    --  gdouble page_size

        if bGTK3 then
            slider = c_func(gtk_scale_new,{orientation,adjustment})
        elsif orientation then
            slider = c_func(gtk_vscale_new,{adjustment})
        else
            slider = c_func(gtk_hscale_new,{adjustment})
        end if
        xpg_setID(slider,id)
        xpg_gtk_add_to(parent,slider)
--      c_proc(gtk_container_add,{ctrl_handles[parent],slider})
--      xpg_signal_connect(slider,"realize",xpg_gtk_widget_realized,id)
        xpg_signal_connect(slider,"value-changed",xpg_gtk_slider_changed,id)
        xpg_signal_connect(slider,"focus-in-event",xpg_gtk_focusinout,id)
        xpg_signal_connect(slider,"focus-out-event",xpg_gtk_focusinout,id)
        c_proc(gtk_widget_realize,{slider})
    elsif backend=WinAPI then
        atom HV = iff(orientation?TBS_VERT:TBS_HORZ),
        dwStyle = or_all({WS_CHILD,WS_VISIBLE,HV,WS_TABSTOP}),
--                                  TBS_AUTOTICKS,
--                                  TBS_ENABLESELRANGE,
          {w,h} = iff(orientation?{30,100}:{100,30})
--      ctrl_size[id][SZ_NATURAL_W] = w
--      ctrl_size[id][SZ_NATURAL_H] = h
        slider = xpg_WinAPI_create(id,"msctls_trackbar32","",parent,w,h,dwStyle,0)
    else
        ?9/0 -- (unknown backend)
    end if
    if orientation then
        ctrl_flags[id] += CF_VERTICAL
    end if
end procedure

function xpg_set_slider_attribute(gdx id, string name, object v, bool bMapped)
    atom handle = ctrl_handles[id]
    if name="ORIENTATION" then
        if string(v) then
            integer hv = find(v,{"HORIZONTAL","VERTICAL"})
            assert(hv!=0)
            v = hv-1
        end if
        if not bMapped then
            xpg_defer_attr(id,name,v)
            return true
        end if
        -- you cannot change this after mapping...?
--      return true
    elsif name="VALUE" then
        if string(v) then v = to_number(v) end if
        if not bMapped then
            xpg_defer_attr(id,name,v)
        elsif backend=GTK then
            c_proc(gtk_range_set_value,{handle,v})
        elsif backend=WinAPI then
            {} = c_func(xSendMessage,{handle,TBM_SETPOS,true,v})
        else
            ?9/0 -- (unknown backend)
        end if
        return true
    end if
    return false
end function

function xpg_get_slider_attribute(gdx id, string name, object dflt)
    atom handle = ctrl_handles[id]
    if name="ORIENTATION" then
        return and_bits(ctrl_flags[id],CF_VERTICAL)!=0
    elsif name="VALUE" then
        if backend=GTK then
            return c_func(gtk_range_get_value,{handle})
        elsif backend=WinAPI then
            return c_func(xSendMessage,{handle,TBM_GETPOS,0,0})
        else
            ?9/0 -- (unknown backend)
        end if
    end if
    return xpg_return_default_attr(id,name,dflt)
end function

--global function gSlider(nullable_string orientation=NULL, rtn value_changed=NULL, sequence attributes="", args={})
global function gSlider(object orientation=NULL, value_changed=NULL, sequence attributes="", args={})
    {orientation,value_changed,attributes,args} = paranormalise_traa(orientation,value_changed,attributes,args)
    if not bInit then xpg_Init() end if
    integer id = xpg_add_control(SLIDER)
    xpg_register_handler(SLIDER,"VALUE_CHANGED",{{1,1,"PO"}})
    set_ctrl_msg(SLIDER,xpg_Slider,xpg_set_slider_attribute,
                                   xpg_get_slider_attribute)
    if orientation!=NULL then
        gSetAttribute(id,"ORIENTATION",orientation)
    end if
    if value_changed!=NULL then
        gSetHandler(id,"VALUE_CHANGED",value_changed)
    end if
    if length(attributes) then
        gSetAttributes(id,attributes,args)
    end if
--  gSetAttribute(id,"EXPAND",false)
--  gSetInt(id,"EXPAND",false)
    return id
end function 

local procedure redraw_table(gdx table)
--?9/0
--?"redraw_table"
-- [DEV] columns of {80,20,100} get 80/200,(4/10) 20/120 (1/6), and 100/100 (100%) of any slack respectively...
-- (later) First job: determine whether there is a vertical scrollbar... [unless that's already taken care of...]
    object tblattr = ctrl_xtra[table][CX_GTL_ATTRS]
--  if sequence(sortcol) then sortcol = sortcol[1] end if
    sequence columns = deep_copy(tblattr[TX_COLUMNS]),
             actcols = repeat(80,length(columns)),
             colalgn = repeat('L',length(columns)),
             ttlalgn = repeat('L',length(columns)),
--DEV object, and call if integer:
             data = tblattr[TX_DATA],
             tags = tblattr[TX_TAGSET]
    tblattr = 0 -- (kill refcount)
    for i,c in columns do
        if not string(c) then
--          c = c[2]
--          if c<0 then c =-c; colexpd[i]='N' end if
            actcols[i] = c[2]
            if length(c)=3 then
                string align = c[3]
                ttlalgn[i] = align[1]
                colalgn[i] = align[$]
            end if
            columns[i] = c[1]
        end if
    end for
--          {width, height} = gGetAttribute(table,"DRAWSIZE"),
    integer {width, height} = gGetAttribute(table,"SIZE"),
--          colsum = sum(actcols),
            colsum = sum(filter(actcols,">",0)),
--          slack = width-colsum
            slack = width-sum(sq_abs(actcols))
    if slack>0 then
        for i,c in actcols do
            if c>0 then
                integer ci = floor((c/colsum)*slack)
                actcols[i] += ci
                slack -= ci
                colsum -= c
            end if
        end for
    end if
    actcols = sq_abs(actcols)
    -- and save it for subsequent mouse handling:
    ctrl_xtra[table][CX_GTL_ATTRS][TX_ACTCOLS] = actcols
    
    -- ("DROP" is shorthand for "Drawing Operation")
    integer drop = gGetHandler(table,"DROP",xpg_default_DROP),
            xCanvasSetForeground = drop(gCanvasSetForeground),
            xCanvasRect = drop(gCanvasRect),
            xCanvasLine = drop(gCanvasLine),
            xCanvasText = drop(gCanvasText),
            -- /now/ it feels safe to shadow the globals:
            gCanvasSetForeground = xCanvasSetForeground,
            gCanvasRect = xCanvasRect,
            gCanvasLine = xCanvasLine,
            gCanvasText = xCanvasText
            
--string actcolstr = sprintf("%v",{actcols})
--string actcolstr = join(actcols,fmt:="%d")
--?{"width",width,"height",height,"columns",columns,"actcols",actcolstr,sum(actcols)}
--  gCanvasRect(table,0,width,0,24,true,colour:=XPG_GREY,fillcolour:=XPG_GREY)
    gCanvasRect(table,0,width,0,24,true,0,-1,-1,XPG_GREY,XPG_GREY)
    gCanvasSetForeground(table,XPG_BLACK)
    integer vx = 0, sortcol = 0, csgn = 0,
            nr = min(length(data[1]),ceil(height/24)-1)
    if sequence(ctrl_xtra[table][CX_GTL_ATTRS][TX_SORTCOLS]) then
        sortcol = ctrl_xtra[table][CX_GTL_ATTRS][TX_SORTCOLS][1]
        csgn = sign(sortcol)
        sortcol = abs(sortcol)
    end if
    for c,a in actcols do   -- the vertical lines and column titles
--      gCanvasLine(table,vx,0,vx,height,colour:=XPG_LIGHT_GREY)
        gCanvasLine(table,vx,0,vx,height,-1,-1,XPG_LIGHT_GREY)
        integer ta = ttlalgn[c],
                tc = iff(c=sortcol?12:0), -- (space for chevron)
                {tx, talign} = iff(ta='L'?{vx+5,XPG_EAST}:
                               iff(ta='C'?{vx+floor(a/2),XPG_CENTRE}:
                               iff(ta='R'?{vx+a-5-tc,XPG_WEST}:9/0)))
        gCanvasText(table,tx,13,columns[c],talign)
        if tc then
            -- if sorted then draw the chevron
            integer hx = vx+a-10
            for hy in {12,12+4*csgn} do
                gCanvasLine(table,hx-5,hy-5*csgn,hx,hy)
                gCanvasLine(table,hx,hy,hx+5,hy-5*csgn)
            end for
--          hy += 4*csgn
--          gCanvasLine(table,hx-5,hy-5*csgn,hx,hy)
--          gCanvasLine(table,hx,hy,hx+5,hy-5*csgn)
--          if dump then
--              printf(1,"gCanvasLine(table,%d,%d,%d,%d)\n",{hx-5,hy-5*csgn,hx,hy})
--              printf(1,"gCanvasLine(table,%d,%d,%d,%d)\n",{hx,hy,hx+5,hy-5*csgn})
--          end if
        end if
        integer ca = colalgn[c],
                {cx, calign} = iff(ca='L'?{vx+5,XPG_EAST}:
                               iff(ca='C'?{vx+floor(a/2),XPG_CENTRE}:
                               iff(ca='R'?{vx+a-5,XPG_WEST}:9/0)))
--DEV use the tagset[DONE], and scroll info:

        for r=1 to nr do
            integer vy = 13+24*r,
                    tr = tags[r]
            object dtrc = data[1][tr][c]
            if c<=length(data[2]) then
                object d2c = data[2][c]
                if string(d2c) then
--  d2c = `%[3]2d/%[2]02/%[1]4d`
                    dtrc = sprintf(d2c,dtrc)
                elsif sequence(d2c) then
                    dtrc = d2c[r]
                elsif d2c!=0 then
                    integer fn = d2c
                    dtrc = fn(data[1],r,c)
                end if
            end if
            if not string(dtrc) then
                dtrc = sprint(dtrc)
            end if
            gCanvasText(table,cx,vy,dtrc,calign)
        end for
        vx += a
    end for
    gCanvasSetForeground(table,XPG_LIGHT_GREY)
    gCanvasLine(table,width-1,0,width-1,height)
    for vy=0 to height by 24 do -- the horizontal lines
        gCanvasLine(table,0,vy,width,vy)
    end for 
--  gCanvasSetForeground(table,XPG_BLACK)
end procedure
rtn_table_redraw = redraw_table; -- (let/permit/allow this to be a perfectly valid rtn)

--DEV object data:
--global function gTable(sequence columns, data, integer rows=10, sequence attributes="", dword_seq args={})
global function gTable(sequence columns, data, object rows=10, sequence attributes="", dword_seq args={})
    {rows,attributes,args} = paranormalise_raa(rows,attributes,args,bCheckRid:=false)
    if rows=0 then rows=10 end if
--  if not bInit then xpg_Init() end if
--  integer id = xpg_add_control(TABLE)
    gdx table = gCanvas(redraw_table)
--  gdx table = gCanvas(redraw_table,attributes,args)
    ctrl_xtra[table][CX_CANVAS_TYPE] = TABLE
--DEV/SUG:
--  if rows<0 then rows = -rows; gSetAttribute(table,"SCROLLABLE",false) end if

--  gSetAttribute(graph,"BGCOLOR",XPG_WHITE) -- (set a default)
--erm...
--  gCanvasSetForeground(graph,XPG_WHITE) -- (set a default)
--  xpg_register_handler(CANVAS,"DRID",{{1,1,"FO"}})
--  gSetHandler(graph,"DRID",drid)
    integer natural_width = 0,
            natural_hight = (rows+1)*24
    for c in columns do
        if string(c) then
            natural_width += 80
        else
            natural_width += abs(c[2])
        end if
    end for
    object t_attr = repeat(0,TX_LEN)
    t_attr[TX_COLUMNS] = columns
    t_attr[TX_DATA] = data
    t_attr[TX_TAGSET] = tagset(length(data[1]))
--  t_attr[TX_SORTCOLS] = 0
--  t_attr[TX_SORTCOLS] = {3}
    t_attr[TX_ROWS] = rows
--DEV why not just use SZ_NATURAL_W?
    t_attr[TX_NATWIDTH] = natural_width
    t_attr[TX_NATHIGHT] = natural_hight
    ctrl_xtra[table][CX_GTL_ATTRS] = t_attr
    t_attr = 0 -- (kill refcount)
    ctrl_size[table][SZ_NATURAL_W] = natural_width
    ctrl_size[table][SZ_NATURAL_H] = natural_hight

--  gSetAttributes(table,"ROWS",rows)
--  if length(attributes) then
--  gSetAttributes(table,attributes,args)
--  end if

--  xpg_register_handler(TREEVIEW,"BRANCHOPEN",{{1,1,"PO"}})
--  if branchopen!=NULL then
--      gSetHandler(id,"BRANCHOPEN",branchopen)
--  end if
    if length(attributes) then
        gSetAttributes(table,attributes,args)
    end if
--  ctrl_xtra[table] = {columns,data,rows}
    return table
end function 

--static void gtkTabsSwitchPage(GtkNotebook* notebook, void* page, int pos, Ihandle* ih)
local function xpg_gtk_switch_page(atom notebook, /*page*/, integer pos, gdx id)
    assert(id=xpg_getID(notebook))
    ?{"xpg_gtk_switch_page",id,pos}
    return false -- (ignored)
end function

local procedure xpg_add_tabs(gdx id)
?"xpg_add_tabs"
    sequence children = children_ids[id],
            tabtitles = ctrl_xtra[id][CX_TABTITLES]
    ctrl_xtra[id][CX_TABTITLES] = 0
    integer lc = length(children),
            lt = length(tabtitles)
    if lt<lc then
        tabtitles &= repeat(0,lc-lt)
        lt = lc
    end if
--DEV unless this is where it would get stored anyway...
--  for i=1 to lc do
--      object ti = gGetAttribute(children[i],"TABTITLE",0)
--      if string(ti) then tabtitles[i] = ti end if
--  end for
    for i,ti in tabtitles do
        if not string(ti) then tabtitles[i] = sprintf("(tab %d)",i) end if
    end for
    ctrl_xtra[id][CX_TABTITLES] = tabtitles
    atom notebook = ctrl_handles[id]
    if backend=GTK then
--DEV TABTITLE putting everything in twice...
        integer n = c_func(gtk_notebook_get_n_pages,{notebook})
?{"TITTITLES:",tabtitles,n,length(tabtitles)}
--if false then
        for i,desc in tabtitles do
--if i>n then
            atom label = c_func(gtk_label_new_with_mnemonic,{desc}),
--DEV deeply suspect use of [i] here... (spotted in passing) maybe I meant children[i]...
                 child = iff(i<=lc?ctrl_handles[i]:c_func(gtk_fixed_new,{}))
            integer r = c_func(gtk_notebook_insert_page,{notebook,child,label,-1})
--          assert(r>=0)
--          assert(r==i-1)
            if r!=i-1 then ?{"warning: gtk_notebook_insert_page returned",r,"not",i-1} end if
        end for
--end if
        c_proc(gtk_widget_realize,{notebook})
    else
--and maybe TCIF_IMAGE...
        set_struct_field(idTCITEM,pTCITEM,"mask",TCIF_TEXT)
        for i,desc in tabtitles do
            set_struct_field(idTCITEM,pTCITEM,"pszText",xpg_raw_string_ptr(desc))
            set_struct_field(idTCITEM,pTCITEM,"cchTextMax",length(desc))
            {} = c_func(xSendMessage,{notebook,TCM_INSERTITEMA,i-1,pTCITEM})
        end for
--TCM_GETITEMCOUNT
--TCM_DELETEITEM 
--WM_NOTIFY TCN_SELCHANGING TCN_SELCHANGE 
--TCM_GETCURSEL TCM_SETCURSEL
--TCM_SETIMAGELIST 
--https://learn.microsoft.com/en-us/windows/win32/controls/tab-controls
--TCM_INSERTITEM
--      idTCITEM = define_struct("""typedef struct {
--                                    UINT   mask;
--                                    DWORD  dwState;
--                                    DWORD  dwStateMask;
--                                    LPTSTR pszText;
--                                    int    cchTextMax;
--                                    int    iImage;
--                                    LPARAM lParam;
--                                  } TCITEM, *LPTCITEM;""")
    end if
end procedure

local procedure xpg_Tabs(gdx id)
    -- (invoked via xpg_map)
    gdx parent = xpg_get_parent_id(id)
    atom handle
    if backend=GTK then
        handle = c_func(gtk_notebook_new,{})
        c_proc(gtk_notebook_set_scrollable,{handle,true})
        xpg_setID(handle,id)
        xpg_gtk_add_to(parent,handle)
--      c_proc(gtk_container_add,{ctrl_handles[parent],handle})
--      xpg_signal_connect(handle,"realize",xpg_gtk_widget_realized,id)
        xpg_signal_connect(handle,"focus-in-event",xpg_gtk_focusinout,id)
        xpg_signal_connect(handle,"focus-out-event",xpg_gtk_focusinout,id)
        xpg_signal_connect(handle,"switch-page",xpg_gtk_switch_page,id)
--/*
void gtk_notebook_remove_page(GtkNotebook *notebook, gint page_num);
gint gtk_notebook_get_current_page(GtkNotebook *notebook);
void gtk_notebook_set_current_page(GtkNotebook *notebook, gint page_num);
void gtk_notebook_reorder_child(GtkNotebook *notebook, GtkWidget *child, gint position);
void gtk_notebook_set_tab_reorderable(GtkNotebook *notebook, GtkWidget *child, gboolean reorderable);
      gtk_notebook_set_menu_label_text((GtkNotebook*)ih->handle, tab_page, gtk_label_get_text((GtkLabel*)tab_label));
        gtk_image_set_from_pixbuf((GtkImage*)tab_image, pixbuf);
    gtk_widget_show(tab_page);
  else
    gtk_widget_hide(tab_page);
  if (tab_container) gtk_widget_show(tab_container);   /* show new page, if any */
  if (prev_tab_container) gtk_widget_hide(prev_tab_container);  /* hide previous page, if any */
static void gtkTabsSwitchPage(GtkNotebook* notebook, void* page, int pos, Ihandle* ih)
  (void)notebook;
  (void)page;
}
  g_signal_connect(G_OBJECT(ih->handle),"switch-page",G_CALLBACK(gtkTabsSwitchPage),ih);
    /* RIGHTCLICK_CB will not work without the eventbox */
    evtBox = gtk_event_box_new();
    gtk_widget_add_events(evtBox,GDK_BUTTON_PRESS_MASK);
    g_signal_connect(G_OBJECT(evtBox),"button-press-event",G_CALLBACK(gtkTabsButtonPressEvent),child);
static gboolean gtkTabsButtonPressEvent(GtkWidget *widget, GdkEventButton *evt, Ihandle *child)
    int pos = gtk_notebook_page_num((GtkNotebook*)ih->handle,tab_page);
    if(ih->data->show_close)
    {
#if GTK_CHECK_VERSION(3,10,0)
      GtkWidget* image = gtk_image_new_from_icon_name("window-close",GTK_ICON_SIZE_MENU);
#else
      GtkWidget* image = gtk_image_new_from_stock(GTK_STOCK_CLOSE,GTK_ICON_SIZE_MENU);
#endif

      tab_close = gtk_button_new();
      gtk_button_set_image((GtkButton*)tab_close,image);
      gtk_button_set_relief((GtkButton*)tab_close,GTK_RELIEF_NONE);
      gtk_button_set_focus_on_click((GtkButton*)tab_close,FALSE);
      iupgtkSetCanFocus(tab_close,FALSE);

      g_signal_connect(G_OBJECT(tab_close),"clicked",G_CALLBACK(gtkTabsCloseButtonClicked),child);
    }
static void gtkTabsCloseButtonClicked(GtkButton *widget, Ihandle* child)
    /* Can not hide the tab_page, or the tab will be automatically hidden.
       So create a secondary container to hide its child instead. */
    if (tabtitle) {
      tab_label = gtk_label_new(NULL);
      iupgtkSetMnemonicTitle(ih,(GtkLabel*)tab_label,tabtitle);
    if (tabimage && tabtitle) {
      gtk_container_add((GtkContainer*)box,tab_image);
      gtk_container_add((GtkContainer*)box,tab_label);
      
      if(ih->data->show_close)
        gtk_container_add((GtkContainer*)box,tab_close);

      gtk_container_add((GtkContainer*)evtBox,box);
      gtk_notebook_insert_page((GtkNotebook*)ih->handle,tab_page,evtBox,pos);
      gtk_notebook_set_menu_label_text((GtkNotebook*)ih->handle,tab_page,gtk_label_get_text((GtkLabel*)tab_label));
    }
    else if(tabimage && ih->data->show_close)
    {
      gtk_container_add((GtkContainer*)box,tab_image);
      gtk_container_add((GtkContainer*)box,tab_close);
      gtk_container_add((GtkContainer*)evtBox,box);
      gtk_notebook_insert_page((GtkNotebook*)ih->handle,tab_page,evtBox,pos);
    }
    else if(tabtitle && ih->data->show_close)
    {
      gtk_container_add((GtkContainer*)box,tab_label);
      gtk_container_add((GtkContainer*)box,tab_close);
      gtk_container_add((GtkContainer*)evtBox,box);
      gtk_notebook_insert_page((GtkNotebook*)ih->handle,tab_page,evtBox,pos);
      gtk_notebook_set_menu_label_text((GtkNotebook*)ih->handle,tab_page,gtk_label_get_text((GtkLabel*)tab_label));
    }
    else if (tabimage)
    {
      gtk_container_add((GtkContainer*)evtBox,tab_image);
      gtk_notebook_insert_page((GtkNotebook*)ih->handle,tab_page,evtBox,pos);
    }
    else
    {
      gtk_container_add((GtkContainer*)evtBox,tab_label);
      gtk_notebook_insert_page((GtkNotebook*)ih->handle,tab_page,evtBox,pos);
    }

    gtk_widget_realize(tab_page);
    if (tabtitle)
    {
      iupgtkUpdateWidgetFont(ih,tab_label);

      iupgtkSetBgColor(tab_label,r,g,b);

      iupStrToRGB(IupGetAttribute(ih,"FGCOLOR"),&r,&g,&b);
      iupgtkSetFgColor(tab_label,r,g,b);

      gtk_widget_show(tab_label);
      gtk_widget_realize(tab_label);
    }

    if (tabimage)
    {
      gtk_widget_show(tab_image);
      gtk_widget_realize(tab_image);
    }

    if (ih->data->show_close)
    {
      gtk_widget_show(tab_close);
      gtk_widget_realize(tab_close);
    }
static int gtkTabsMapMethod(Ihandle* ih)
{
  ih->handle = gtk_notebook_new();
  if (!ih->handle)
    return IUP_ERROR;

  gtk_notebook_set_scrollable((GtkNotebook*)ih->handle,TRUE);

  gtkTabsUpdateTabType(ih);

  /* add to the parent, all GTK controls must call this. */
  iupgtkAddToParent(ih);

  gtk_widget_add_events(ih->handle,GDK_ENTER_NOTIFY_MASK|GDK_LEAVE_NOTIFY_MASK);

  g_signal_connect(G_OBJECT(ih->handle),"enter-notify-event",   G_CALLBACK(iupgtkEnterLeaveEvent),ih);
  g_signal_connect(G_OBJECT(ih->handle),"leave-notify-event",   G_CALLBACK(iupgtkEnterLeaveEvent),ih);
  g_signal_connect(G_OBJECT(ih->handle),"focus-in-event",       G_CALLBACK(iupgtkFocusInOutEvent),ih);
  g_signal_connect(G_OBJECT(ih->handle), "focus-out-event",     G_CALLBACK(iupgtkFocusInOutEvent), ih);
  g_signal_connect(G_OBJECT(ih->handle), "key-press-event",     G_CALLBACK(iupgtkKeyPressEvent),   ih);
  g_signal_connect(G_OBJECT(ih->handle), "show-help",           G_CALLBACK(iupgtkShowHelp),        ih);

  g_signal_connect(G_OBJECT(ih->handle), "switch-page",         G_CALLBACK(gtkTabsSwitchPage), ih);

  gtk_widget_realize(ih->handle);

  /* Create pages and tabs */
  if (ih->firstchild)
  {
    Ihandle* child;
    Ihandle* current_child = (Ihandle*)iupAttribGet(ih,"_IUPTABS_VALUE_HANDLE");

    for (child = ih->firstchild; child; child = child->brother)
      gtkTabsChildAddedMethod(ih,child);

    if (current_child)
    {
      IupSetAttribute(ih,"VALUE_HANDLE",(char*)current_child);

      /* current value is now given by the native system */
      iupAttribSet(ih,"_IUPTABS_VALUE_HANDLE",NULL);
    }
  }

  return IUP_NOERROR;
}
static void winTabsInitializeCloseImage(void)
{
  Ihandle *image_close;

  unsigned char img_close[ITABS_CLOSE_SIZE * ITABS_CLOSE_SIZE] =
  {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 
    0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 
    0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 
    0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 
    0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 
    0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 
    0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  };
  
  image_close = IupImage(ITABS_CLOSE_SIZE,ITABS_CLOSE_SIZE,img_close);
  IupSetAttribute(image_close,"0","BGCOLOR");
  IupSetAttribute(image_close,"1","0 0 0");
  IupSetHandle("IMGCLOSE",image_close);

  image_close = IupImage(ITABS_CLOSE_SIZE,ITABS_CLOSE_SIZE,img_close);
  IupSetAttribute(image_close,"0","BGCOLOR");
  IupSetStrAttribute(image_close,"1",IupGetGlobal("TXTHLCOLOR"));
  IupSetHandle("IMGCLOSEHIGH",image_close);
}
  num_tabs = (int)SendMessage(ih->handle,TCM_GETITEMCOUNT,0,0);
    SendMessage(ih->handle,TCM_GETITEM,p,(LPARAM)&tie);
    if (tab_container == (HWND)tie.lParam)
      return p;
void iupdrvTabsSetCurrentTab(Ihandle* ih, int pos) {
  int p = winTabsPosFixToWin(ih,pos);
  if (p >= 0)
  {
    int curr_pos = iupdrvTabsGetCurrentTab(ih);
    HWND tab_container = winTabsGetPageWindow(ih,curr_pos);
    if (tab_container)
      ShowWindow(tab_container,SW_HIDE);

    SendMessage(ih->handle,TCM_SETCURSEL,p,0);

    tab_container = winTabsGetPageWindow(ih,pos);
    if (tab_container)
      ShowWindow(tab_container,SW_SHOW);
  }
}
int iupdrvTabsGetCurrentTab(Ihandle* ih) {
  return winTabsPosFixFromWin(ih,(int)SendMessage(ih->handle,TCM_GETCURSEL,0,0));
}
  image_list = (HIMAGELIST)SendMessage(ih->handle,TCM_GETIMAGELIST,0,0);
  if (!image_list)
  {
    UINT flags = ILC_COLOR32 | (bpp == 8 ? ILC_MASK : 0);

    /* create the image list if does not exist */
    image_list = ImageList_Create(width,height,flags,0,50);
    SendMessage(ih->handle,TCM_SETIMAGELIST,0,(LPARAM)image_list);
  }
static void winTabsDrawPageBackground(Ihandle* ih, HDC hDC, RECT* rect) {
  unsigned char r=0, g=0, b=0;
  char* color = iupAttribGetInheritNativeParent(ih,"BGCOLOR");
  if (!color) color = iupAttribGetInheritNativeParent(ih,"BACKGROUND");
  if (!color) color = iupAttribGet(ih,"BACKGROUND");
  if (!color) color = IupGetGlobal("DLGBGCOLOR");
  iupStrToRGB(color,&r,&g,&b);
  SetDCBrushColor(hDC,RGB(r,g,b));
  FillRect(hDC,rect,(HBRUSH)GetStockObject(DC_BRUSH));
}

static LRESULT CALLBACK winTabsPageWndProc(HWND hWnd, UINT msg, WPARAM wp, LPARAM lp) { 
  switch (msg) {
  case WM_ERASEBKGND: {
      RECT rect;
      HDC hDC = (HDC)wp;
      Ihandle* ih = iupwinHandleGet(hWnd);
      GetClientRect(ih->handle,&rect); 
      winTabsDrawPageBackground(ih,hDC,&rect);

      /* return non zero value */
      return 1;
    }
  SendMessage(ih->handle,TCM_INSERTITEM,p,(LPARAM)&tie);
  /* Make sure tab container is hidden */
  ShowWindow(tab_container,SW_HIDE);

  SendMessage(ih->handle,TCM_DELETEITEM,p,0);
      TCITEM tie;

      tie.mask = TCIF_TEXT;
      tie.pszText = iupwinStrToSystem(value);
      tie.cchTextMax = lstrlen(tie.pszText);

      iupwinSetMnemonicTitle(ih,pos,value);

      SendMessage(ih->handle,TCM_SETITEM,p,(LPARAM)&tie);
    TCITEM tie;

    tie.mask = TCIF_IMAGE;
    if (value)
      tie.iImage = winTabsGetImageIndex(ih,value);
    else
      tie.iImage = -1;

    SendMessage(ih->handle,TCM_SETITEM,p,(LPARAM)&tie);
static int winTabsWmNotify(Ihandle* ih, NMHDR* msg_info, int *result) {
  (void)result;

  if (msg_info->code == TCN_SELCHANGING) {
    IFnnn cb = (IFnnn)IupGetCallback(ih,"TABCHANGE_CB");
    int prev_pos = iupdrvTabsGetCurrentTab(ih);
    iupAttribSetInt(ih,"_IUPWINTABS_PREV_CHILD_POS",prev_pos);

    /* save the previous handle if callback exists */
    if (cb)
    {
      Ihandle* prev_child = IupGetChild(ih,prev_pos);
      iupAttribSet(ih,"_IUPWINTABS_PREV_CHILD",(char*)prev_child);
    }

    return 0;
  }

  if (msg_info->code == TCN_SELCHANGE)
  {
    IFnnn cb = (IFnnn)IupGetCallback(ih,"TABCHANGE_CB");
    int pos = iupdrvTabsGetCurrentTab(ih);
    int prev_pos = iupAttribGetInt(ih,"_IUPWINTABS_PREV_CHILD_POS");

    HWND tab_container = winTabsGetPageWindow(ih,pos);
    HWND prev_tab_container = winTabsGetPageWindow(ih,prev_pos);

    if (tab_container) ShowWindow(tab_container,SW_SHOW);    /* show new page, if any */
    if (prev_tab_container) ShowWindow(prev_tab_container,SW_HIDE); /* hide previous page, if any */

    if (cb)
    {
      Ihandle* child = IupGetChild(ih,pos);
      Ihandle* prev_child = (Ihandle*)iupAttribGet(ih,"_IUPWINTABS_PREV_CHILD");
      iupAttribSet(ih,"_IUPWINTABS_PREV_CHILD",NULL);

      /* avoid duplicate calls when a Tab is inside another Tab. */
      if (prev_child)
        cb(ih,child,prev_child);
    }
    else
    {
      IFnii cb2 = (IFnii)IupGetCallback(ih,"TABCHANGEPOS_CB");
      if (cb2)
        cb2(ih,pos,prev_pos);
    }

    return 0;
  }

  if (msg_info->code == NM_RCLICK)
  {
    IFni cb = (IFni)IupGetCallback(ih,"RIGHTCLICK_CB");
    if (cb)
    {
      TCHITTESTINFO ht;
      int p;

      GetCursorPos(&ht.pt);
      ScreenToClient(ih->handle,&ht.pt);
      
      p = (int)SendMessage(ih->handle,TCM_HITTEST,0,(LPARAM)&ht);
      if (p >= 0)
      {
        int pos = winTabsPosFixFromWin(ih,p);
        cb(ih,pos);
      }
    }

    return 0;
  }

  return 0; /* result not used */
}

static int winTabsMsgProc(Ihandle* ih, UINT msg, WPARAM wp, LPARAM lp, LRESULT *result)
{
  switch (msg)
  {
  case WM_SIZE:
  {
    RECT rect;
    WNDPROC oldProc = (WNDPROC)IupGetCallback(ih,"_IUPWIN_OLDWNDPROC_CB");
    CallWindowProc(oldProc,ih->handle,msg,wp,lp);

    SetRect(&rect,0,0,LOWORD(lp),HIWORD(lp));
    SendMessage(ih->handle,TCM_ADJUSTRECT,FALSE,(LPARAM)&rect);

    winTabsPlacePageWindows(ih,&rect);

    *result = 0;
    return 1;
  }
  case WM_MOUSELEAVE:
    if (ih->data->show_close)
    {
      int high_p = iupAttribGetInt(ih,"_IUPTABS_CLOSEHIGH");
      if (high_p != -1)
      {
        iupAttribSetInt(ih,"_IUPTABS_CLOSEHIGH",-1);
        iupdrvRedrawNow(ih);
      }
    }
    break;
  case WM_MOUSEMOVE:
    if (ih->data->show_close)
    {
      TCHITTESTINFO ht;
      int p, high_p, press_p;

      ht.pt.x = GET_X_LPARAM(lp);
      ht.pt.y = GET_Y_LPARAM(lp);
      p = (int)SendMessage(ih->handle,TCM_HITTEST,0,(LPARAM)&ht);

      high_p = iupAttribGetInt(ih,"_IUPTABS_CLOSEHIGH");
      if (winTabsIsInsideCloseButton(ih,p))
      {
        if (high_p != p)
        {
          /* must be called so WM_MOUSELEAVE will be called */
          iupwinTrackMouseLeave(ih);

          iupAttribSetInt(ih,"_IUPTABS_CLOSEHIGH",p);
          iupdrvRedrawNow(ih);
        }
      }
      else
      {
        if (high_p != -1)
        {
          iupAttribSetInt(ih,"_IUPTABS_CLOSEHIGH",-1);
          iupdrvRedrawNow(ih);
        }
      }

      press_p = iupAttribGetInt(ih,"_IUPTABS_CLOSEPRESS");
      if (press_p != -1 && !winTabsIsInsideCloseButton(ih,press_p))
      {
        iupAttribSetInt(ih,"_IUPTABS_CLOSEPRESS",-1);
        iupdrvRedrawNow(ih);
      }
    }
    break;
  case WM_LBUTTONDOWN:
    if (ih->data->show_close)
    {
      TCHITTESTINFO ht;
      int p;

      ht.pt.x = GET_X_LPARAM(lp);
      ht.pt.y = GET_Y_LPARAM(lp);
      p = (int)SendMessage(ih->handle,TCM_HITTEST,0,(LPARAM)&ht);

      if (p >= 0 && winTabsIsInsideCloseButton(ih,p))
      {
        iupAttribSetInt(ih,"_IUPTABS_CLOSEPRESS",p);    /* used for press feedback */
        iupdrvRedrawNow(ih);

        *result = 0;
        return 1;
      }
      else
        iupAttribSetInt(ih,"_IUPTABS_CLOSEPRESS",-1);
    }
    break;
  case WM_LBUTTONUP:
    if (ih->data->show_close)
    {
      int press_p = iupAttribGetInt(ih,"_IUPTABS_CLOSEPRESS");
      if (press_p != -1)
      {
        if (winTabsIsInsideCloseButton(ih,press_p))
        {
          int pos = winTabsPosFixFromWin(ih,press_p);
          Ihandle *child = IupGetChild(ih,pos);
          HWND tab_container = (HWND)iupAttribGet(child,"_IUPTAB_CONTAINER");

          iupAttribSetInt(ih,"_IUPTABS_CLOSEPRESS",-1);

          if (tab_container)
          {
            int ret = IUP_DEFAULT;
            IFni cb = (IFni)IupGetCallback(ih,"TABCLOSE_CB");
            if (cb)
              ret = cb(ih,pos);

            if (ret == IUP_CONTINUE) /* destroy tab and children */
            {
              IupDestroy(child);
              IupRefreshChildren(ih);
            }
            else if (ret == IUP_DEFAULT) /* hide tab and children */
            {
              iupTabsCheckCurrentTab(ih,pos,0);
              winTabsSetVisibleArrayItem(ih,pos,0);  /* to invisible */
              winTabsDeleteItem(ih,press_p,tab_container);
            }
            else if (ret == IUP_IGNORE)
            {
              *result = 0;
              return 1;
            }
          }
        }

        iupdrvRedrawNow(ih);
      }
    }
    break;
  }

  return iupwinBaseContainerMsgProc(ih, msg, wp, lp, result);
}
static int winTabsMapMethod(Ihandle* ih)
{
  DWORD dwStyle = WS_CHILD | WS_CLIPSIBLINGS | TCS_HOTTRACK | WS_TABSTOP,
      dwExStyle = 0;

  if (!ih->parent)
    return IUP_ERROR;

  if (ih->data->type == ITABS_BOTTOM)
    dwStyle |= TCS_BOTTOM;
  else if (ih->data->type == ITABS_RIGHT)
    dwStyle |= TCS_VERTICAL|TCS_RIGHT;  
  else if (ih->data->type == ITABS_LEFT)
    dwStyle |= TCS_VERTICAL;

  if (ih->data->is_multiline)
    dwStyle |= TCS_MULTILINE;

  if (ih->data->show_close)
    dwStyle |= TCS_OWNERDRAWFIXED;

  iupwinGetNativeParentStyle(ih,&dwExStyle,&dwStyle);

  if (dwExStyle & WS_EX_COMPOSITED && !ih->data->is_multiline && iupwinIsVistaOrNew())
  {
    /* workaround for composite bug in Vista */
    ih->data->is_multiline = 1;  
    dwStyle |= TCS_MULTILINE;
  }

  if (!iupwinCreateWindow(ih,WC_TABCONTROL,dwExStyle,dwStyle,NULL))
    return IUP_ERROR;

  /* replace the WinProc to handle other messages */
  IupSetCallback(ih,"_IUPWIN_CTRLMSGPROC_CB",(Icallback)winTabsMsgProc);

  /* Process WM_NOTIFY */
  IupSetCallback(ih,"_IUPWIN_NOTIFY_CB",(Icallback)winTabsWmNotify);

  /* Process background color */
  IupSetCallback(ih,"_IUPWIN_CTLCOLOR_CB",(Icallback)winTabsCtlColor);

  if (ih->data->show_close)
  {
    /* change tab width to draw an close image */
    SendMessage(ih->handle,TCM_SETPADDING,0,MAKELPARAM(ITABS_CLOSE_SIZE,0));

    iupAttribSetInt(ih,"_IUPTABS_CLOSEHIGH",-1);
    iupAttribSetInt(ih,"_IUPTABS_CLOSEPRESS",-1);

    /* owner draw tab image + tab title + close image */
    IupSetCallback(ih,"_IUPWIN_DRAWITEM_CB",(Icallback)winTabsDrawItem);
  }

  if (iupwin_comctl32ver6 && (ih->data->type != ITABS_TOP || ih->data->show_close))
  {
    /* XP Styles support only TABTYPE=TOP,
       show_close ownerdraw will work only with classic style */ 
    iupwinDrawRemoveTheme(ih->handle);
  }

  /* Change children background */
  if (winTabsUsingXPStyles(ih))
  {
    char* color = iupAttribGetInheritNativeParent(ih,"BGCOLOR");
    if (!color) 
      color = iupAttribGetInheritNativeParent(ih,"BACKGROUND");
    if (!color)
    {
      COLORREF cr;
      if (iupwinDrawGetThemeTabsBgColor(ih->handle,&cr))
        iupAttribSetStrf(ih,"BACKGROUND","%d %d %d",(int)GetRValue(cr),(int)GetGValue(cr),(int)GetBValue(cr));
    }
  }

  /* Create pages and tabs */
  if (ih->firstchild)
  {
    Ihandle* child;
    Ihandle* current_child = (Ihandle*)iupAttribGet(ih,"_IUPTABS_VALUE_HANDLE");

    for (child = ih->firstchild; child; child = child->brother)
      winTabsChildAddedMethod(ih,child);

    if (current_child)
    {
      IupSetAttribute(ih,"VALUE_HANDLE",(char*)current_child);

      /* current value is now given by the native system */
      iupAttribSet(ih,"_IUPTABS_VALUE_HANDLE",NULL);
    }
  }

  return IUP_NOERROR;
}
-- NOTIFY MESSAGE (FROM TABCONTROL?)
    elsif msg=WM_NOTIFY then

        -- get handle & id of the control
        hwnd = peek4u(lParam)
        id = getID(hwnd)

        -- get notification code
        nCode = peek4s(lParam + NMHDR_code)

        -- trap notifications if from tab controls
        if id and ObjectType[id] = TabControl
        and (nCode=TCN_SELCHANGING or nCode=TCN_SELCHANGE) then

            -- get the tab item
            iPart = sendMessage(id,TCM_GETCURSEL,0,0)
            iPart += 1 -- make 1-based

            iPart = ObjectChildren[id][iPart] -- iPart is now the id of the tabitem

            -- do not allow any clicking change to be made TO a disabled TabItem
            if nCode=TCN_SELCHANGING then

                -- get the coords of the arrow when the message was made
                temp = c_func(xGetMessagePos,{})
                -- populate a structure
                atom pStruct = allocate_TextSpace(sizeofstruct(TC_HITTESTINFO))
                poke4(pStruct+POINT_x,and_bits(temp,#FFFF))
                poke4(pStruct+POINT_Y,floor(temp/#10000))
                -- convert coords
                void = c_func(xScreenToClient,{hwnd,pStruct})
                -- test coords to see if a hit was definitely made on a tab
                temp = sendMessage(id,TCM_HITTEST,0,pStruct) + 1
                if temp and and_bits(peek4u(pStruct+TC_HITTESTINFO_flags),TCHT_ONITEM) then -- valid hit
                    temp = ObjectChildren[id][temp] -- temp is now the id of the tabitem
                    if ObjectExtra[temp]=0 then -- has struck a disabled tab item
                        return True -- to prevent change
                    end if
                end if
            end if

            -- attempt to get item handler, if unavailable then get tab control handler
            iHandler = HandlerRoutine[iPart]
            if iHandler=UNDEFINED then
                iHandler = HandlerRoutine[id]
            end if

            -- call the item handler if possible
            if iHandler!=UNDEFINED then
                return_value = call_func(iHandler,{iPart,WM_NOTIFY,nCode,lParam})
            end if

---------------

            -- is the tab closing?
            if nCode=TCN_SELCHANGING and return_value=0 then
                setVisible(ObjectChildren[iPart],False)

            -- is the tab opening?
            elsif nCode=TCN_SELCHANGE then
                setVisible(ObjectChildren[iPart],True)

            end if

            return return_value

    WC_TABCONTROLA      = "SysTabControl32",
-- A TabControl is a container control. It can have one or more /TabItems.
-- Selecting a /TabItem in a TabControl activates it, and displays any
-- controls that are associated with it.
--
-- Since the TabControl is a container, controls can be placed into it,
-- just like placing them into a window. However, it becomes the developers's
-- responsibility to show and hide various controls as different /TabItems
-- are selected. It is easier to associate controls with /TabItems, since
-- ARWEN will automatically take care of those details.

    className[TabControl] = WC_TABCONTROLA
    classBase[TabControl] = COMMON_CONTROL
    classStyle[TabControl] = or_all({WS_CHILD, 
                                     WS_VISIBLE,
                                     WS_CLIPCHILDREN,
--                                   WS_SIZEBOX,
--                                   WS_TABSTOP,
                                     TCS_FOCUSONBUTTONDOWN,
-- the TCS_OWNERDRAWFIXED flag means that disabled text can be drawn on the TabItem
                                     TCS_OWNERDRAWFIXED
                                    })
    classStyleEx[TabControl] = WS_EX_CONTROLPARENT  
                        
--  classAttr[TabControl] = w32Clickable

-----------------------------------------------------------------------------

    className[TabItem] = ""
--  classBase[TabItem] = COMMON_CONTROL
    classStyle[TabItem] = WS_TABSTOP
--function getIndex(integer id)
    elsif objectType=TabControl then
        selcount = c_func(xSendMessage,{hwnd,TCM_GETCURSEL,0,0}) + 1
        if selcount then
            selcount = ObjectChildren[id][selcount]
        end if
        return selcount -- will be an id OR 0
--function isVisible(integer id)

    -- make the visibility state of a TabItem the same as the parent TabControl
    if ObjectType[id]=TabItem then
        id = ObjectParent[id]
    end if
--function getFocus()
    if ObjectType[id]=TabControl then
        integer temp = getIndex(id)
        if temp then
            return temp
        else
            return 0
        end if
    end if
--function getCount(integer id)  elsif objectType=TabControl then
        return c_func(xSendMessage,{hwnd,TCM_GETITEMCOUNT,0,0})
    -- TABITEM - TabItems are dealt with entirely from here (create a tab item, ie, a tab on a tab control)
    if objType=TabItem then
        -- make the control
        id = createTabItem(pID,lbl,hBitmap,style)
        -- store id with the parent tab control
        if id then
            ObjectExtra[id] = 1 -- default setting is "enabled"
--          ObjectChildren[pID] &= id
            ObjectChildren[pID] = ObjectChildren[pID] & id  -- 2.4 bug
            void = setAccelerator(id,lbl) --PL
            setTextColour(id,Black)
        end if
        -- exit
        return id

    -- TAB-OWNED CONTROL
    elsif pID and ObjectType[pID]=TabItem then
        -- adjust the coordinate values to properly sit in the client area of the tab control
        hwnd = ObjectHwnd[ObjectParent[pID]]
        lpRect = allocate_Rect()
        void = c_func(xGetClientRect,{hwnd,lpRect})
        void = c_func(xSendMessage,{hwnd,TCM_ADJUSTRECT,0,lpRect})
        x += peek4s(lpRect)
        y += peek4s(lpRect+4)
        -- make the control but the 'grandfather' tabcontrol will be the parent, in Window's eyes
        id = create(objType,lbl,hBitmap,ObjectParent[pID],x,y,nWidth,nHeight,style)
        ObjectParent[id] = pID -- reverse wrong side-effect of above call

        -- make control invisible if it is not on the first tab
        if find(pID,ObjectChildren[ObjectParent[pID]])!=1 then
            setVisible(id,False)
        end if
        -- add control to the list maintained by the TabItem
        ObjectChildren[pID] &= id --? ObjectChildren[pID]
        -- exit
        return id
    elsif msg=WM_PAINT and ObjectType[id]=TabControl then
        -- ensure child controls are also repainted
        temp = getIndex(id)
        if temp then
            temp = ObjectChildren[temp]
            for i=1 to length(temp) do
                void = c_func(xInvalidateRect,{ObjectHwnd[temp[i]],NULL,0})
            end for
        end if

--*/
    elsif backend=WinAPI then
        atom {w,h} = gGetTextExtent(parent,ctrl_xtra[id][CX_TABTITLES],false),
           dwStyle = or_all({WS_CHILD,WS_VISIBLE,WS_CLIPCHILDREN,TCS_FOCUSONBUTTONDOWN}),
         dwStyleEx = WS_EX_CONTROLPARENT
--      ctrl_size[id][SZ_NATURAL_W] = w
--      ctrl_size[id][SZ_NATURAL_H] = h
        -- (WC_TABCONTROLA = "SysTabControl32")
--InitCommonControlsEx needed??
        handle = xpg_WinAPI_create(id,"SysTabControl32","",parent,w+10,h+10,dwStyle,dwStyleEx)
    else
        ?9/0 -- (unknown backend)
    end if
end procedure

function xpg_set_tabs_attribute(gdx id, string name, object v, bool bMapped)
--  atom handle = ctrl_handles[id]
--  if name="ALIGNMENT" then
----Possible values: "ALEFT", "ARIGHT", "ACENTER". Default: "ALEFT". 
--      printf(1,"gSetAttribute(BOX,\"%s\",%v)...\n",{name,v})
--      return true
--  end if
    return false
end function

function xpg_get_tabs_attribute(gdx id, string name, object dflt)
--  if name="TITLE" then
--      ?9/0 -- placeholder
--  end if
    return xpg_return_default_attr(id,name,dflt)
end function

--global function gTabs(gdx children={}, string attributes="", dword_seq args={})
global function gTabs(object children={}, tabchanged=NULL, sequence attributes="", args={})
    {children,tabchanged,attributes,args} = paranormalise_qraa(children,tabchanged,attributes,args)
    if not bInit then xpg_Init() end if
    integer id = xpg_add_control(TABS,bHasChildren:=true)
    xpg_register_handler(TABS,"TABCHANGED",{{3,3,"POII"}})
    set_ctrl_msg(TABS,xpg_Tabs,xpg_set_tabs_attribute,
                               xpg_get_tabs_attribute)
--  gSetAttribute(id,"FONT","Helvetica, 9")
    ctrl_xtra[id] = repeat({},CX_TABLENGTH)
    integer lc = length(children)
    if lc then
--DEV may want to improve this...
        -- Each element of children can be a gdx, title, {gdx,title[,image]}, or {title[,image]}. 
        object c1 = children[1]
        if string(c1) then
            ctrl_xtra[id][CX_TABTITLES] = children
        else
            if sequence(c1) then
                integer tdx = iff(string(c1[1])?1:2)
                sequence titles = trim_tail(vslice(children,tdx,{""}),""),
                         images = trim_tail(vslice(children,tdx+1,{NULL}),NULL)
                integer lt = length(titles)
                assert(lt=0 or lt=lc)
                ctrl_xtra[id][CX_TABTITLES] = titles
                ctrl_xtra[id][CX_TABIMAGES] = images
                children = iff(tdx=1?{}:vslice(children,1))
            end if
            assert(gdx(children))
            for child in children do
                parent_ids[child] = id
            end for
            children_ids[id] = children
        end if
    end if
    if tabchanged!=NULL then
        gSetHandler(id,"TABCHANGED",tabchanged)
    end if
    if length(attributes) then
        gSetAttributes(id,attributes,args)
    end if
    return id
end function

local procedure xpg_Text(gdx id)
    -- (invoked via xpg_map)
    gdx parent = xpg_get_parent_id(id)
    atom handle
    if backend=GTK then
--      handle = c_func(gtk_text_view_new,{})
        handle = c_func(gtk_entry_new,{})
        xpg_setID(handle,id)
        xpg_gtk_add_to(parent,handle)
--      c_proc(gtk_container_add,{ctrl_handles[parent],handle})
--      xpg_signal_connect(handle,"realize",xpg_gtk_widget_realized,id)
        xpg_signal_connect(handle,"focus-in-event",xpg_gtk_focusinout,id)
        xpg_signal_connect(handle,"focus-out-event",xpg_gtk_focusinout,id)
        c_proc(gtk_widget_realize,{handle})
    elsif backend=WinAPI then
--      string text = g
        atom dwStyle = or_all({WS_CHILD,WS_VISIBLE,WS_TABSTOP,WS_BORDER,ES_AUTOHSCROLL,ES_LEFT}),
--           dwStyleEx = or_all({WS_EX_CLIENTEDGE,WS_EX_ACCEPTFILES})
             dwStyleEx = WS_EX_ACCEPTFILES
--               {w,h} = gGetTextExtent(parent,"W")
--?{"pg_Text",w,h}
--      ctrl_size[id][SZ_NATURAL_W] = w
--      ctrl_size[id][SZ_NATURAL_H] = h
--      handle = xpg_WinAPI_create(id,"edit","",parent,w+100,h+10,dwStyle,dwStyleEx)
        handle = xpg_WinAPI_create(id,"edit","",parent,100,20,dwStyle,dwStyleEx)
    else
        ?9/0 -- (unknown backend)
    end if
end procedure

function xpg_set_text_attribute(gdx id, string name, object v, bool bMapped)
    atom handle = ctrl_handles[id]
--  if name="ALIGNMENT" then
----Possible values: "ALEFT", "ARIGHT", "ACENTER". Default: "ALEFT". 
--      printf(1,"gSetAttribute(TEXT,\"%s\",%v)...\n",{name,v})
--      return true
--  els
    if name="VALUE" then
        assert(string(v))
        if not bMapped then
            xpg_defer_attr(id,name,v)
        elsif backend=GTK then
            atom buffer = c_func(gtk_entry_get_buffer,{handle})
            c_proc(gtk_entry_buffer_set_text,{buffer,v,-1})
        elsif backend=WinAPI then
            c_proc(xSetWindowText,{handle,v})
        else
            ?9/0 -- (unknown backend)
        end if
        return true
    end if
    return false
end function

function xpg_get_text_attribute(gdx id, string name, object dflt)
    atom handle = ctrl_handles[id]
    if name="VALUE" then
        if backend=GTK then
            atom buffer = c_func(gtk_entry_get_buffer,{handle})
            return peek_string(c_func(gtk_entry_buffer_get_text,{buffer}))
        elsif backend=WinAPI then
            integer l = c_func(xGetWindowTextLength,{handle})
            string title = repeat(' ',l)
            c_proc(xGetWindowText,{handle,title,l+1})
            return title
        else
            ?9/0 -- (unknown backend)
        end if
    end if
    return xpg_return_default_attr(id,name,dflt)
end function

global function gText(object value_changed=NULL, sequence attributes="", args={})
    {value_changed,attributes,args} = paranormalise_raa(value_changed,attributes,args)
    if not bInit then xpg_Init() end if
    integer id = xpg_add_control(TEXT)
--test_elem = id
    xpg_register_handler(TEXT,"CLICK",{{4,4,"FOPII"},{2,2,"FOP"}})
    xpg_register_handler(TEXT,"VALUE_CHANGED",{{1,1,"PO"}})
    set_ctrl_msg(TEXT,xpg_Text,xpg_set_text_attribute,
                               xpg_get_text_attribute)
--  gSetAttribute(id,"FONT","Helvetica, 9")
    if value_changed!=NULL then
        gSetHandler(id,"VALUE_CHANGED",value_changed)
    end if
    if length(attributes) then
        gSetAttributes(id,attributes,args)
    end if
    return id
end function 

integer timer_ids = NULL

local function xpg_gtk_timer_proc(integer id)
    integer action = gGetHandler(id,"ACTION")
    if action then
        action(id)
    end if
    return true
end function
local constant xpg_gtk_timer_proc_cb = call_back({'+',xpg_gtk_timer_proc})

local function xpg_win_timer_proc(atom /*hwnd*/, integer /*msg*/, atom wid, /*mstime*/)
--  assert(hwnd=NULL)
--  assert(msg=WM_TIMER)
--  if mstime<0 then mstime = and_bitsu(mstime,#FFFFFFFF) end if -- (unsign)
    integer id = getd(wid,timer_ids)
    return xpg_gtk_timer_proc(id)
end function
local constant xpg_win_timer_proc_cb = call_back({'+',xpg_win_timer_proc})

function xpg_set_timer_attribute(gdx id, string name, object v, bool bMapped)
    -- nb bMapped (ie CF_MAPPED) means there is an active running timer
    atom handle = ctrl_handles[id], flags = ctrl_flags[id]
    bool bOK = false, bRun = bMapped
    if bMapped then
        if backend=GTK then     
            assert(c_func(g_source_remove,{handle}))
        elsif backend=WinAPI then
            assert(c_func(xKillTimer,{NULL,handle}))
            deld(handle,timer_ids)
        else
            ?9/0 -- (unknown backend)
        end if
        handle = NULL
        flags -= CF_MAPPED
    end if
    if name="MSECS"
    or name="MSEC"
    or name="TIME" then
        if string(v) then v = to_number(v) end if
        assert(v>=0)
        deferred_attr[id] = v
        bOK = true
    elsif name="RUN"
       or name="RUNNING"
       or name="ACTIVE" then
        if string(v) then v = xpg_to_bool(v) end if
        bRun = v
        bOK = true
    end if
    if bOK then
        integer msecs = deferred_attr[id]
        if bRun and msecs then
            if backend=GTK then
                handle = c_func(g_timeout_add,{msecs,xpg_gtk_timer_proc_cb,id})
            elsif backend=WinAPI then
                handle = c_func(xSetTimer,{NULL,0,msecs,xpg_win_timer_proc_cb})
            else
                ?9/0 -- (unknown backend)
            end if
            setd(handle,id,timer_ids)
            flags += CF_MAPPED
        end if
        ctrl_handles[id] = handle
        ctrl_flags[id] = flags
    end if
    return bOK
end function

function xpg_get_timer_attribute(gdx id, string name, object dflt)
    if name="MSECS"
    or name="MSEC"
    or name="TIME" then
        return deferred_attr[id]
    elsif name="RUN"
       or name="RUNNING"
       or name="ACTIVE" then
        return and_bits(ctrl_flags[id],CF_MAPPED)!=0
    end if
--  return xpg_return_default_attr(id,name,dflt)
    crash(`gGetAttribute(TIMER,"%s")`,{name})
end function

global function gTimer(rtn action=NULL, integer msecs=0, boolean active=false)
    if not bInit then xpg_Init() end if
    if timer_ids=NULL then timer_ids = new_dict() end if
    integer id = xpg_add_control(TIMER)
    xpg_register_handler(TIMER,"ACTION",{{1,1,"PO"}})
    set_ctrl_msg(TIMER,0,xpg_set_timer_attribute,
                         xpg_get_timer_attribute)
    if action!=NULL then
        gSetHandler(id,"ACTION",action)
    end if
    gSetAttribute(id,"MSECS",msecs)
    gSetAttribute(id,"RUN",active)
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
        c_proc(cfn,{store,iter,column,di,-1})
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
    if backend=GTK then
        {tree_store, path, iter} = what
    elsif backend=WinAPI then
        {hItem, tIdx} = what
    else
        ?9/0 -- (unknown backend)
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
                if backend=GTK then
                    c_proc(gtk_tree_view_collapse_row,{tree_view,path})
                elsif backend=WinAPI then
                    {} = c_func(xSendMessage,{tree_view,TVM_EXPAND,TVE_COLLAPSE,hItem})
                else
                    ?9/0 -- (unknown backend)
                end if
            elsif state="EXPANDED" then
                if backend=GTK then
                    c_proc(gtk_tree_view_expand_row,{tree_view,path,false})
                elsif backend=WinAPI then
                    {} = c_func(xSendMessage,{tree_view,TVM_EXPAND,TVE_EXPAND,hItem})
                else
                    ?9/0 -- (unknown backend)
                end if
            else
                crash("invalid STATE:"&state)
            end if
        elsif name="USERDATA" then
            integer user_data = attrs[i+1]
            if backend=GTK then
                gtk_store_set(tree_store,iter,{USERDATA},{user_data},1,gtk_tree_store_set)
            elsif backend=WinAPI then
--NO! lParam is the tree_items index!
--              set_struct_field(idTVITEMEX,pTVITEMEX,"hItem",hItem)
--              set_struct_field(idTVITEMEX,pTVITEMEX,"mask",TVIF_HANDLE+TVIF_PARAM)
--              set_struct_field(idTVITEMEX,pTVITEMEX,"lParam",user_data)
--              bool bOK = c_func(xSendMessage,{tree_view,TVM_SETITEM,0,pTVITEMEX})
--              assert(bOK)
                tree_items[tIdx][tUserData] = user_data
            else
                ?9/0 -- (unknown backend)
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
    if backend=GTK then
        {tree_store, iter, piter} = args
        c_proc(gtk_tree_store_append,{tree_store,iter,piter})
    elsif backend=WinAPI then
        pdx = args
    else
        ?9/0 -- (unknown backend)
    end if
    if string(tree_nodes) then tree_nodes = {tree_nodes} end if
    string desc = tree_nodes[1]
    integer l = min(length(tree_nodes),3)
    bool bLeaf = l=1 or atom(tree_nodes[l])
    if backend=GTK then
        icon = iff(bLeaf?dot:closed_folder)
        integer userdata = 0 -- (set/overidden later via attributes)
        coldata = {icon,desc,"#000000","#FFFFFF","",userdata}
        gtk_store_set(tree_store,iter,STD_COLS,coldata,NUM_COLS,gtk_tree_store_set) 
    elsif backend=WinAPI then
        integer {iImage,iExpanded} = iff(bLeaf?{2,2}:{0,1}),    -- dot or closed/open folder
                cChildren = iff(bLeaf?0:length(tree_nodes[l]))
        tIdx = xpg_WinAPI_new_tree_item(desc,bLeaf,pdx)
--      if pdx=null then
----        if ??[id]=UNDEFINED then ??[id] = tIdx end if
--      end if
        atom hParen = iff(pdx?tree_items[pdx][tHandle]:NULL)
        hItem = xpg_WinAPI_addTVItemEx(tree_view,hParen,tIdx,iImage,iExpanded,cChildren)
        tree_items[tIdx][tHandle] = hItem
    else
        ?9/0 -- (unknown backend)
    end if
    if not bLeaf or l=3 then
        if backend=GTK then
            path = c_func(gtk_tree_model_get_path,{tree_store,iter})
        end if
        object recargs
        if not bLeaf then
            if backend=GTK then
                chiter = allocate(32,true)
                recargs = {tree_store,chiter,iter}
            elsif backend=WinAPI then
                recargs = tIdx
            else
                ?9/0 -- (unknown backend)
            end if
            sequence children = tree_nodes[l]
            for child in children do
                xpg_tree_add_nodes_rec(child,tree_view,recargs)
            end for
            if backend=WinAPI then
                set_struct_field(idTVITEMEX,pTVITEMEX,"mask",TVIF_HANDLE+TVIF_CHILDREN)
                set_struct_field(idTVITEMEX,pTVITEMEX,"hItem",hItem)
                set_struct_field(idTVITEMEX,pTVITEMEX,"cChildren",length(children))
                bool bOK = c_func(xSendMessage,{tree_view,TVM_SETITEM,0,pTVITEMEX})
                assert(bOK)
            end if
        end if
        if backend=GTK then
            c_proc(gtk_tree_view_expand_to_path,{tree_view,path})
        elsif backend=WinAPI then
            {} = c_func(xSendMessage,{tree_view,TVM_EXPAND,TVE_EXPAND,hItem})
        else
            ?9/0 -- (unknown backend)
        end if
        if l=3 then
            if backend=GTK then
                recargs = {tree_store,path,iter}
            elsif backend=WinAPI then
                recargs = {hItem,tIdx}
            else
                ?9/0 -- (unknown backend)
            end if
            xpg_TreeSetNodeAttributes(tree_view,recargs,tree_nodes[2])
        end if
        if backend=GTK then
            c_proc(gtk_tree_path_free,{path})
        end if
    end if
end procedure

global function gTreeGetUserId(object treenode)
    integer res
    if backend=GTK then
        atom {tree_view,iter,path} = treenode,
             tree_store = c_func(gtk_tree_view_get_model,{tree_view}),
             pWord = allocate(machine_word(),true)
        c_proc(gtk_tree_model_get,{tree_store,iter,USERDATA,pWord,-1})
        res = peekns(pWord)
        free(pWord)
    elsif backend=WinAPI then
        integer tIdx = treenode[2]
        res = tree_items[tIdx][tUserData]
    else
        ?9/0 -- (unknown backend)
    end if
    return res
end function

bool bNodesAdded = false

--DEV...
--global 
--global procedure gTreeDeleteChildren(object treenode)
--local procedure gTreeDeleteChildren(object treenode, integer nc=0)
local procedure xpg_tree_delete_children(object treenode)
    xpg_handle tree_view = treenode[1]
    if backend=GTK then
--      bNodesAdded = true
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
    elsif backend=WinAPI then
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
                xpg_tree_delete_children({tree_view,childIdx})
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
--      set_struct_field(idTVITEMEX,pTVITEMEX,"cChildren",nc)
        bOK = c_func(xSendMessage,{tree_view,TVM_SETITEM,0,pTVITEMEX})
        assert(bOK)
    else
        ?9/0 -- (unknown backend)
    end if
end procedure

--DEV id treenode is a gdx, then splat the whole tree (but error if not mapped)
global procedure gTreeAddNodes(object treenode, sequence children)
    xpg_tree_delete_children(treenode)
    if gdx(treenode) then -- whole tree replacement
        if and_bits(ctrl_flags[treenode],CF_MAPPED)=0 then
            ctrl_xtra[treenode] = children
        else
            xpg_handle tree_view = ctrl_handles[treenode]
            if backend=GTK then
                atom iter = allocate(32,true),
                     tree_store = c_func(gtk_tree_view_get_model,{tree_view})
                c_proc(gtk_tree_store_clear,{tree_store})
                xpg_tree_add_nodes_rec(children,tree_view,{tree_store,iter,null})
--?"setAdded(1)"
                bNodesAdded = true
            elsif backend=WinAPI then
--              xpg_tree_delete_children(treenode)
--/*
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
                    xpg_tree_delete_children({tree_view,rootIdx})
                    bOK = c_func(xSendMessage,{tree_view,TVM_DELETEITEM,0,pHwnd})
                    assert(bOK)
                    xpg_WinAPI_free_tree_item(rootIdx)
                end if
--*/
                xpg_tree_add_nodes_rec(children,tree_view,0)
            else
                ?9/0 -- (unknown backend)
            end if
        end if -- </mapped>
    else -- from branchopen, a subnode update
        atom tree_view = treenode[1]
--      xpg_tree_delete_children(tree_view)
--      xpg_tree_delete_children(treenode)
        object args
        if backend=GTK then
            atom tree_store = c_func(gtk_tree_view_get_model,{tree_view}),
                     chiter = allocate(32,true),
                      piter = treenode[2]
            args = {tree_store,chiter,piter}
        elsif backend=WinAPI then
            integer treeIdx = treenode[2]
            args = treeIdx
        else
            ?9/0 -- (unknown backend)
        end if
        for child in children do
            xpg_tree_add_nodes_rec(child,tree_view,args)
        end for
        if backend=GTK then
--?"setAdded(2)"
            bNodesAdded := true
--?{"bbNodesAdded",bNodesAdded}
        elsif backend=WinAPI then
--DEV... this is now doing what I want (gTreeView2), but thrice-over it seems...
--          atom hTreeItem = tree_items[treeIdx][tHandle]
            atom hTreeItem = tree_items[args][tHandle]
            assert(hTreeItem!=NULL)
            set_struct_field(idTVITEMEX,pTVITEMEX,"mask",TVIF_HANDLE+TVIF_CHILDREN)
            set_struct_field(idTVITEMEX,pTVITEMEX,"hItem",hTreeItem)
            set_struct_field(idTVITEMEX,pTVITEMEX,"cChildren",length(children))
            bool bOK = c_func(xSendMessage,{tree_view,TVM_SETITEM,0,pTVITEMEX})
            assert(bOK)
            {} = c_func(xSendMessage,{tree_view,TVM_EXPAND,TVE_EXPAND,hTreeItem})
        end if
    end if
end procedure

local function gtk_row_collapsed(xpg_handle tree_view, atom iter, /*path*/, gdx /*user_data*/)  -- (GTK only)
    atom tree_store = c_func(gtk_tree_view_get_model,{tree_view})
    gtk_store_set(tree_store,iter,{COL_IMG},{closed_folder},1,gtk_tree_store_set) 
    return true
end function

bool bInAdd = false

local function gtk_row_expanded(xpg_handle tree_view, atom iter, path, gdx /*user_data*/)   -- (GTK only)
if not bInAdd then
    bInAdd = true
    atom tree_store = c_func(gtk_tree_view_get_model,{tree_view})
    gtk_store_set(tree_store,iter,{COL_IMG},{open_folder},1,gtk_tree_store_set) 
    integer id = xpg_getID(tree_view),
            branchopen = gGetHandler(id,"BRANCHOPEN")
    if branchopen!=NULL then
        -- aside: user_data is 4th arg of g_signal_connect_data, no use here.
--DEV compiler overzealous...
--?"bNodesAdded:=false"
--      bNodesAdded = false
        branchopen({tree_view,iter,path})
--?{"bNodesAdded",bNodesAdded}
        if bNodesAdded then
--?{"bNodesADDed",bNodesAdded}
            bNodesAdded = false
            c_proc(gtk_tree_view_expand_row,{tree_view,path,false})
--          return true -- (no help)
        end if
    end if
    bInAdd = false
end if
    return 0 /* ignored */
end function

--DEV these need to be in ctrl_xtra...
local bool bPrev = false
local atom sel_iter = allocate(32),
             ppPath = allocate(machine_word())

local function gtk_treeview_cursor_changed(xpg_handle tree_view, gdx /*id*/) -- (GTK only)
    atom tree_store = c_func(gtk_tree_view_get_model,{tree_view})
    if bPrev then
        gtk_store_set(tree_store,sel_iter,{F_COLOUR,B_COLOUR},{"#000000","#FFFFFF"},2,gtk_tree_store_set)
        bPrev = false
    end if
    c_proc(gtk_tree_view_get_cursor,{tree_view,ppPath,NULL})
    atom path = peekns(ppPath)
    if path!=NULL
    and c_func(gtk_tree_model_get_iter,{tree_store,sel_iter,path}) then
        gtk_store_set(tree_store,sel_iter,{F_COLOUR,B_COLOUR},{"#FFFFFF","#0000FF"},2,gtk_tree_store_set)
        bPrev = true
    end if
    return 0 /* ignored */
end function

--DOCS: note that treenodes do not have any attributes, you would have to manage that sort of thing  
--      through a single USERDATA integer, taking care not to cripple any deferred loading handling.

local procedure xpg_TreeView(gdx id)
    -- (invoked via xpg_map)
    sequence tree_nodes = ctrl_xtra[id]
    ctrl_xtra[id] = 0
    gdx parent = xpg_get_parent_id(id)
    atom tree_view
    if backend=GTK then
        tree_view = c_func(gtk_tree_view_new,{})
        xpg_setID(tree_view,id)
        atom selection = c_func(gtk_tree_view_get_selection,{tree_view})
        c_proc(gtk_tree_view_set_headers_visible,{tree_view,false})
        c_proc(gtk_tree_view_set_enable_search,{tree_view,false})
        c_proc(gtk_tree_view_set_enable_tree_lines,{tree_view,true})
        c_proc(gtk_tree_selection_set_mode,{selection,GTK_SELECTION_NONE})
--DEV or maybe on sandle?
--      xpg_signal_connect(tree_view,"realize",xpg_gtk_widget_realized,id)
        xpg_signal_connect(tree_view,"row-collapsed",gtk_row_collapsed,id)
        xpg_signal_connect(tree_view,"row-expanded",gtk_row_expanded,id)
        xpg_signal_connect(tree_view,"cursor-changed",gtk_treeview_cursor_changed,id)
        xpg_signal_connect(tree_view,"focus-in-event",xpg_gtk_focusinout,id)
        xpg_signal_connect(tree_view,"focus-out-event",xpg_gtk_focusinout,id)
        atom column = c_func(gtk_tree_view_column_new,{}),
             img_renderer = c_func(gtk_cell_renderer_pixbuf_new,{}),
             txt_renderer = c_func(gtk_cell_renderer_text_new,{}),
             spc_renderer = c_func(gtk_cell_renderer_text_new,{})
        c_proc(gtk_tree_view_column_pack_start,{column,img_renderer,false})
        c_proc(gtk_tree_view_column_add_attribute,{column,img_renderer,"pixbuf",COL_IMG})
        c_proc(gtk_tree_view_column_pack_start,{column,txt_renderer,false})
        c_proc(gtk_tree_view_column_add_attribute,{column,txt_renderer,"text",COL_NAME})
        c_proc(gtk_tree_view_column_add_attribute,{column,txt_renderer,"foreground",F_COLOUR})
        c_proc(gtk_tree_view_column_add_attribute,{column,txt_renderer,"background",B_COLOUR})
        c_proc(gtk_tree_view_column_pack_start,{column,spc_renderer,true})
        c_proc(gtk_tree_view_column_add_attribute,{column,spc_renderer,"text",COLSPACE})
        c_proc(gtk_tree_view_insert_column,{tree_view,column,-1})

        sequence gtypes = {GDK_TYPE_PIXBUF,G_TYPE_STRING,G_TYPE_STRING,G_TYPE_STRING,G_TYPE_STRING,G_TYPE_INT}
        atom tree_store = c_func(gtk_tree_store_newv,{NUM_COLS,xpg_word_array(gtypes)})
        c_proc(gtk_tree_view_set_model,{tree_view,tree_store})
        c_proc(xg_object_unref,{tree_store})

        atom iter = allocate(32,true)
        xpg_tree_add_nodes_rec(tree_nodes,tree_view,{tree_store,iter,null})

        atom sandle = c_func(gtk_scrolled_window_new,{NULL,NULL}) 
        c_proc(gtk_scrolled_window_set_policy,{sandle,GTK_POLICY_AUTOMATIC,GTK_POLICY_AUTOMATIC})
        c_proc(gtk_container_add,{sandle,tree_view})
        xpg_gtk_add_to(parent,sandle)
--      c_proc(gtk_container_add,{ctrl_handles[parent],sandle})
        c_proc(gtk_widget_realize,{sandle})
        c_proc(gtk_widget_realize,{tree_view})
    elsif backend=WinAPI then

        atom dwStyle = or_all({WS_CHILD,WS_VISIBLE,WS_TABSTOP,TVS_HASLINES,TVS_LINESATROOT,TVS_HASBUTTONS})
        integer {w,h} = gGetAttribute(id,"SIZE",{0,0})
        tree_view = xpg_WinAPI_create(id,"SysTreeView32","",parent,w,h,dwStyle,WS_EX_CLIENTEDGE)
        {} = c_func(xSendMessage,{tree_view,TVM_SETIMAGELIST,TVSIL_NORMAL,tree_himl})
        xpg_tree_add_nodes_rec(tree_nodes,tree_view,0)

    else
        ?9/0 -- (unknown backend)
    end if
end procedure

function xpg_set_treeview_attribute(gdx id, string name, object v, bool bMapped)
--  atom handle = ctrl_handles[id]
    return false
end function

function xpg_get_treeview_attribute(gdx id, string name, object dflt)
--  if name="TITLE" then
--      ?9/0 -- placeholder
--  end if
    return xpg_return_default_attr(id,name,dflt)
end function

global function gTreeView(object tree_nodes={}, branchopen=NULL, sequence attributes="", args={})
    {tree_nodes,branchopen,attributes,args} = paranormalise_qraa(tree_nodes,branchopen,attributes,args)
    if not bInit then xpg_Init() end if
    integer id = xpg_add_control(TREEVIEW)
    xpg_register_handler(TREEVIEW,"BRANCHOPEN",{{1,1,"PO"}})
    set_ctrl_msg(TREEVIEW,xpg_TreeView,xpg_set_treeview_attribute,
                                       xpg_get_treeview_attribute)
    if branchopen!=NULL then
        gSetHandler(id,"BRANCHOPEN",branchopen)
    end if
    if length(attributes) then
        gSetAttributes(id,attributes,args)
    end if
    ctrl_xtra[id] = tree_nodes
    return id
end function 

global function gVbox(sequence children={}, string attributes="", sequence args={})
    return gBox('V',children,attributes,args)
end function

-- nb: uses gLabel, gText, so must be defined after them!
local procedure xpg_DatePick(gdx id)
    -- (invoked via xpg_map)
    gdx parent = xpg_get_parent_id(id)
    atom datepick
    if backend=GTK then
--DEV do this in hll first...
        datepick = 9/0 -- c_func(gtk_frame_new,{title})
        xpg_setID(datepick,id)
--/*
--  static void iDatePickUpdateDayLimits(Ihandle* ih) {
--    Ihandle* txt_month = (Ihandle*)iupAttribGet(ih,"_IUP_DATE_MONTH");
--    Ihandle* txt_day = (Ihandle*)iupAttribGet(ih,"_IUP_DATE_DAY");
--    int day = IupGetInt(txt_day,"VALUE");
--    int month = IupGetInt(txt_month,"VALUE");
--    if month == 2 then
--      Ihandle* txt_year = (Ihandle*)iupAttribGet(ih,"_IUP_DATE_YEAR");
--      int year = gGetInt(txt_year,"VALUE");
--      if is_leap_year(year) then
--        gSetAttribute(txt_day,"MASKINT","1:29");
--        if day > 29 then
--          gSetInt(txt_day,"VALUE",29);
--        end if
--      else
--        gSetAttribute(txt_day,"MASKINT","1:28");
--        if day > 28 then
--          gSetInt(txt_day,"VALUE",28);
--        end if
--      end if
--    elsif month == 4 
--       or month == 6
--       or month == 9
--       or month == 11 then
--      IupSetAttribute(txt_day,"MASKINT","1:30");
--      if day > 30 then
--        gSetInt(txt_day,"VALUE",30);
--      end if
--    else
--      IupSetAttribute(txt_day,"MASKINT","1:31");
--    end if
--  }
--
--  static int iDatePickCalendarValueChanged_CB(Ihandle* ih_calendar) {
--    Ihandle* ih = (Ihandle*)iupAttribGet(ih_calendar,"_IUP_DATEPICK");
--
--    gSetStrAttribute(ih,"VALUE",gGetAttribute(ih_calendar,"VALUE"));
--
--    iupBaseCallValueChangedCb(ih);
--    return IUP_DEFAULT;
--  }
--
--  static int iDatePickCalendarKillFocus_CB(Ihandle* ih_calendar) {
--    Ihandle* ih_toggle = (Ihandle*)iupAttribGet(ih_calendar,"_IUP_DATEPICK_TOGGLE");
--    if not iupAttribGet(ih_toggle,"_IUP_DATEPICK_INSIDETOGGLE") then
--      IupSetAttribute(ih_toggle,"VALUE","OFF");
--    end if
--    IupHide(IupGetDialog(ih_calendar));
--    return IUP_DEFAULT;
--  }
--
--  static int iDatePickToggleEnterWindow_CB(Ihandle* ih_toggle) {
--    iupAttribSet(ih_toggle,"_IUP_DATEPICK_INSIDETOGGLE","1");
--    return IUP_DEFAULT;
--  }
--
--  static int iDatePickToggleLeaveWindow_CB(Ihandle* ih_toggle) {
--    iupAttribSet(ih_toggle,"_IUP_DATEPICK_INSIDETOGGLE",NULL);
--    return IUP_DEFAULT;
--  }
--
--  static int iDatePickToggleAction_CB(Ihandle* ih_toggle,int state) {
--    Ihandle* ih = IupGetParent(IupGetParent(ih_toggle));
--    Ihandle* calendar = (Ihandle*)iupAttribGet(ih,"_IUP_CALENDAR");
--
--    if (state == 1) {
--      int x, y;
--
--      if (!calendar) {
--        Ihandle* dlg;
--        char* weeknumbers;
--
--        calendar = IupCalendar();
--        IupSetCallback(calendar,"VALUECHANGED_CB",iDatePickCalendarValueChanged_CB);
--        IupSetCallback(calendar,"KILLFOCUS_CB",iDatePickCalendarKillFocus_CB);
--        iupAttribSet(calendar,"_IUP_DATEPICK",(char*)ih);
--        iupAttribSet(calendar,"_IUP_DATEPICK_TOGGLE",(char*)ih_toggle);
--        iupAttribSet(ih,"_IUP_CALENDAR",(char*)calendar);
--
--        dlg = IupDialog(calendar);
--        IupSetAttribute(dlg,"BORDER","NO");
--        IupSetAttribute(dlg,"MENUBOX","NO");
--        IupSetAttribute(dlg,"MAXBOX","NO");
--        IupSetAttribute(dlg,"MINBOX","NO");
--        IupSetAttribute(dlg,"RESIZE","NO");
--      }
--
--      IupSetStrAttribute(calendar,"VALUE",IupGetAttribute(ih,"VALUE"));
--
--      x = IupGetInt(ih,"X");
--      y = IupGetInt(ih,"Y");
--      y += IupGetInt2(ih,"RASTERSIZE");
--
--      IupShowXY(IupGetDialog(calendar),x,y);
--      IupSetFocus(calendar);
--    } else {
--      if (calendar)
--        IupHide(IupGetDialog(calendar));
--    }
--    return IUP_DEFAULT;
--  }
--
--  static int iDatePickTextValueChanged_CB(Ihandle* ih_text) {
--    Ihandle* ih = IupGetParent(IupGetParent(ih_text));
--
--    if ((Ihandle*)iupAttribGet(ih,"_IUP_DATE_DAY") != ih_text)
--      iDatePickUpdateDayLimits(ih);
--
--    iupBaseCallValueChangedCb(ih);
--    return IUP_DEFAULT;
--  }
--
--  static int iDatePickTextKAny_CB(Ihandle* ih_text, int key) {
--    if (key == K_UP || key == K_plus || key == K_sPlus) {
--      int value = IupGetInt(ih_text,"VALUE");
--      value++;
--      gSetAttribute(ih_text,"VALUE","%02d",value);
--
--      if (IupGetInt(ih_text,"VALUE") == value)
--        iDatePickTextValueChanged_CB(ih_text);
--      return IUP_IGNORE;
--    } else if (key == K_DOWN || key == K_minus || key == K_sMinus) {
--      int value = IupGetInt(ih_text,"VALUE");
--      value -= 1
--      gSetAttribute(ih_text,"VALUE","%02d",value);
--      if (IupGetInt(ih_text,"VALUE") == value)
--        iDatePickTextValueChanged_CB(ih_text);
--      return IUP_IGNORE;
--    } else if (key == K_LEFT) {
--      int caret = IupGetInt(ih_text,"CARET");
--      if (caret == 1) {
--        int pos = IupGetChildPos(IupGetParent(ih_text),ih_text);
--        if (pos == 2) {
--          Ihandle* next = IupGetChild(IupGetParent(ih_text),0);
--          int count = IupGetInt(next,"COUNT");
--          IupSetFocus(next);
--          IupSetInt(next,"CARET",count+1);
--        } else if (pos == 4) {
--          Ihandle* next = IupGetChild(IupGetParent(ih_text),2);
--          int count = IupGetInt(next,"COUNT");
--          IupSetFocus(next);
--          IupSetInt(next,"CARET",count+1);
--        }
--      }
--    } else if (key == K_RIGHT) {
--      int caret = IupGetInt(ih_text,"CARET");
--      int count = IupGetInt(ih_text,"COUNT");
--      if (caret == count + 1) {
--        int pos = IupGetChildPos(IupGetParent(ih_text),ih_text);
--        if (pos == 0) {
--          Ihandle* next = IupGetChild(IupGetParent(ih_text),2);
--          IupSetFocus(next);
--          IupSetInt(next,"CARET",1);
--        } else if (pos == 2) {
--          Ihandle* next = IupGetChild(IupGetParent(ih_text),4);
--          IupSetFocus(next);
--          IupSetInt(next,"CARET",1);
--        }
--      }
--    }
--    return IUP_CONTINUE;
--  }
--
--  static int iDatePickSetValueAttrib(Ihandle* ih, const char* value) {
--    if upper(value)="TODAY" then
--      Ihandle* txt_year = (Ihandle*)iupAttribGet(ih,"_IUP_DATE_YEAR");
--      Ihandle* txt_month = (Ihandle*)iupAttribGet(ih,"_IUP_DATE_MONTH");
--      Ihandle* txt_day = (Ihandle*)iupAttribGet(ih,"_IUP_DATE_DAY");
--      struct tm * timeinfo;
--      time_t timer;
--      time(&timer);
--      timeinfo = localtime(&timer);
--
--      IupSetInt(txt_year,"VALUE",timeinfo->tm_year + 1900);
--      gSetAttribute(txt_month,"VALUE","%02d",timeinfo->tm_mon + 1);
--      gSetAttribute(txt_day,"VALUE","%02d",timeinfo->tm_mday);
--
--      iDatePickUpdateDayLimits(ih);
--    else
--      int year, month, day;
--      if (sscanf(value,"%d/%d/%d",&year,&month,&day) == 3) {
--        Ihandle* txt_year = (Ihandle*)iupAttribGet(ih,"_IUP_DATE_YEAR");
--        Ihandle* txt_month = (Ihandle*)iupAttribGet(ih,"_IUP_DATE_MONTH");
--        Ihandle* txt_day = (Ihandle*)iupAttribGet(ih,"_IUP_DATE_DAY");
--
--        if (month < 1) month = 1;
--        if (month > 12) month = 12;
--
--        IupSetInt(txt_year,"VALUE",year);
--        gSetAttribute(txt_month,"VALUE","%02d",month);
--
--        iDatePickUpdateDayLimits(ih);
--
--        gSetAttribute(txt_day,"VALUE","%02d",day);
--      end if
--    end if
--    return 0; /* do not store value in hash table */
--  }
--
--  static char* iDatePickGetValueAttrib(Ihandle* ih) {
--    Ihandle* txt_year = (Ihandle*)iupAttribGet(ih,"_IUP_DATE_YEAR");
--    int year = IupGetInt(txt_year,"VALUE");
--
--    Ihandle* txt_month = (Ihandle*)iupAttribGet(ih,"_IUP_DATE_MONTH");
--    int month = IupGetInt(txt_month,"VALUE");
--
--    Ihandle* txt_day = (Ihandle*)iupAttribGet(ih,"_IUP_DATE_DAY");
--    int day = IupGetInt(txt_day,"VALUE");
--
--    return iupStrReturnStrf("%d/%d/%d",year,month,day);
--  }
--
--  static int iDatePickSetSeparatorAttrib(Ihandle* ih, const char* value) {
--    Ihandle* lbl = IupGetChild(ih->firstchild,1);
--    IupSetStrAttribute(lbl,"TITLE",value);
--    lbl = IupGetChild(ih->firstchild,3);
--    IupSetStrAttribute(lbl,"TITLE",value);
--    return 1;
--  }
--
--  static void iDatePickSetDayTextBox(Ihandle* ih, int pos) {
--    Ihandle* txt = IupGetChild(ih->firstchild,pos);
--    IupSetAttribute(txt,"MASKINT","1:31");
--    IupSetAttribute(txt,"MASKNOEMPTY","Yes");
--    IupSetAttribute(txt,"NC","2");
--    IupSetAttribute(txt,"SIZE","14x");
--
--    iupAttribSet(ih,"_IUP_DATE_DAY",(char*)txt);
--  }
--
--  static void iDatePickSetMonthTextBox(Ihandle* ih, int pos) {
--    Ihandle* txt = IupGetChild(ih->firstchild,pos);
--
--    IupSetAttribute(txt,"MASKINT","1:12");
--    IupSetAttribute(txt,"MASKNOEMPTY","Yes");
--    IupSetAttribute(txt,"NC","2");
--    IupSetAttribute(txt,"SIZE","14x");
--
--    iupAttribSet(ih,"_IUP_DATE_MONTH",(char*)txt);
--  }
--
--  static void iDatePickSetYearTextBox(Ihandle* ih, int pos) {
--    Ihandle* txt = IupGetChild(ih->firstchild,pos);
--    IupSetAttribute(txt,"MASK",IUP_MASK_UINT);
--    IupSetAttribute(txt,"MASKNOEMPTY","Yes");
--    IupSetAttribute(txt,"NC","4");
--    IupSetAttribute(txt,"SIZE","26x");
--
--    iupAttribSet(ih,"_IUP_DATE_YEAR",(char*)txt);
--  }
--
--  static int iDatePickSetOrderAttrib(Ihandle* ih, const char* value) {
--    int i;
--
--    if (!value || strlen(value) != 3)
--      return 0;
--
--    for (i = 0; i < 3; i++)
--    {
--      if (value[i] == 'D' || value[i] == 'd')
--        iDatePickSetDayTextBox(ih,i * 2);
--      else if (value[i] == 'M' || value[i] == 'm')
--        iDatePickSetMonthTextBox(ih,i * 2);
--      else if (value[i] == 'Y' || value[i] == 'y')
--        iDatePickSetYearTextBox(ih,i * 2);
--      else
--        return 0;
--    }
--    return 1;
--  }
--
--  static char* iDatePickGetTodayAttrib(Ihandle* ih) {
--    struct tm * timeinfo;
--    time_t timer;
--    time(&timer);
--    timeinfo = localtime(&timer);
--    (void)ih;
--    return iupStrReturnStrf("%d/%d/%d",timeinfo->tm_year + 1900,timeinfo->tm_mon + 1,timeinfo->tm_mday);
--  }
--
--  static Ihandle* iDatePickCreateText(void) {
--    Ihandle* txt = IupText(NULL);
--    IupSetAttribute(txt,"BORDER","NO");
--    IupSetAttribute(txt,"NOHIDESEL","NO");
--    IupSetAttribute(txt,"ALIGNMENT","ACENTER");
--    IupSetCallback(txt,"K_ANY",(Icallback)iDatePickTextKAny_CB);
--    IupSetCallback(txt,"VALUECHANGED_CB",iDatePickTextValueChanged_CB);
--    return txt;
--  }
--
--  static int iDatePickCreateMethod(Ihandle* ih, void** params) {
--    Ihandle *box, *tgl;
--    (void)params;
--    
--    tgl = gCheckbox(NULL,NULL);
--    IupSetAttribute(tgl,"IMAGE","IupArrowDown");
--    IupSetAttribute(tgl,"EXPAND","VERTICALFREE");
--    IupSetAttribute(tgl,"FLAT","YES");
--    IupSetAttribute(tgl,"IGNOREDOUBLECLICK","YES");
--    IupSetCallback(tgl,"ACTION",(Icallback)iDatePickToggleAction_CB);
--    IupSetCallback(tgl,"ENTERWINDOW_CB",(Icallback)iDatePickToggleEnterWindow_CB);
--    IupSetCallback(tgl,"LEAVEWINDOW_CB",(Icallback)iDatePickToggleLeaveWindow_CB);
--
--    box = IupHbox(iDatePickCreateText(),IupLabel("/"),iDatePickCreateText(),IupLabel("/"),iDatePickCreateText(),tgl,NULL);
--    iupChildTreeAppend(ih,box);
--    box->flags |= IUP_INTERNAL;
--
--    iDatePickSetOrderAttrib(ih,"DMY");
--    iDatePickSetValueAttrib(ih,iDatePickGetTodayAttrib(ih));
--
--    IupSetAttribute(box,"MARGIN","0x0");
--    IupSetAttribute(box,"GAP","0");
--
--    IupSetAttribute(ih,"BGCOLOR",IupGetGlobal("TXTBGCOLOR"));
--
--    return IUP_NOERROR;
--  }
--
--  static void iDatePickUnMapMethod(Ihandle* ih) {
--    Ihandle* calendar = (Ihandle*)iupAttribGet(ih,"_IUP_CALENDAR");
--    if (iupObjectCheck(calendar))
--    {
--      IupDestroy(IupGetDialog(calendar));
--      iupAttribSet(ih,"_IUP_CALENDAR",NULL);
--    }
--  }
--
--  Iclass* iupDatePickNewClass(void) {
--    Iclass* ic = iupClassNew(iupRegisterFindClass("frame"));
--
--    ic->name = "datepick";
--    ic->format = NULL;  /* no parameters */
--    ic->nativetype = IUP_TYPECONTROL;
--    ic->childtype = IUP_CHILDNONE;
--    ic->is_interactive = 1;
--
--    /* Class functions */
--    ic->New = iupDatePickNewClass;
--    ic->Create = iDatePickCreateMethod;
--    ic->UnMap = iDatePickUnMapMethod;
--
--    /* Callbacks */
--    iupClassRegisterCallback(ic,"VALUECHANGED_CB","");
--
--    iupClassRegisterAttribute(ic,"VALUE",iDatePickGetValueAttrib,iDatePickSetValueAttrib,IUPAF_SAMEASSYSTEM,"TODAY",IUPAF_NO_DEFAULTVALUE | IUPAF_NO_INHERIT);
--    iupClassRegisterAttribute(ic,"TODAY",iDatePickGetTodayAttrib,NULL,NULL,NULL,IUPAF_NOT_MAPPED | IUPAF_READONLY | IUPAF_NO_DEFAULTVALUE | IUPAF_NO_INHERIT);
--
--    iupClassRegisterAttribute(ic,"SEPARATOR",NULL,iDatePickSetSeparatorAttrib,IUPAF_SAMEASSYSTEM,"/",IUPAF_NOT_MAPPED | IUPAF_NO_INHERIT);
--    iupClassRegisterAttribute(ic,"ORDER",NULL,iDatePickSetOrderAttrib,IUPAF_SAMEASSYSTEM,"DMY",IUPAF_NOT_MAPPED|IUPAF_NO_INHERIT);
--
--    return ic;
--  }
--
--  Ihandle *IupDatePick(void)
--  {
--    return IupCreate("datepick");
--  }
--*/
        xpg_gtk_add_to(parent,datepick)
        c_proc(gtk_widget_realize,{datepick})
    elsif backend=WinAPI then

        atom dwStyle = or_all({WS_CHILD,WS_VISIBLE})
        if gGetAttribute(id,"CANFOCUS",false) then
            dwStyle ||= WS_TABSTOP
        end if
        integer {w,h} = {109,21}    -- best guess (font dependent)
        datepick = xpg_WinAPI_create(id,"SysDateTimePick32","",parent,w,h,dwStyle,0)
        bool bOK = c_func(xSendMessage,{datepick,DTM_GETIDEALSIZE,0,pSIZE})
        assert(bOK)
        w = get_struct_field(idSIZE,pSIZE,"cx")
        h = get_struct_field(idSIZE,pSIZE,"cy")
        atom flags = SWP_NOMOVE+SWP_NOZORDER+SWP_NOACTIVATE
--?{"xpg_DatePick(create)",id,w,h}
        bool ok = c_func(xSetWindowPos,{datepick,NULL,0,0,w,h,flags})
        assert(ok)
        ctrl_size[id][SZ_NATURAL_W] = w
        ctrl_size[id][SZ_NATURAL_H] = h
    else
        ?9/0 -- (unknown backend)
    end if
end procedure

function xpg_set_datepick_attribute(gdx id, string name, object v, bool bMapped)
    atom handle = ctrl_handles[id]
    if name="TITLE" then
        assert(string(v))
        if not bMapped then
            xpg_defer_attr(id,name,v)
        elsif backend=GTK then
            c_proc(gtk_frame_set_label,{handle,v})
        elsif backend=WinAPI then
            c_proc(xSetWindowText,{handle,v})
        else
            ?9/0 -- (unknown backend)
        end if
        return true
    elsif name="FGCOLOR" then
        printf(1,"gSetAttribute(DATEPICK,\"%s\",%v)...\n",{name,v})
        return true
    end if
    return false
end function

function xpg_get_datepick_attribute(gdx id, string name, object dflt)
--  if name="TITLE" then
--      ?9/0 -- placeholder
--  end if
    return xpg_return_default_attr(id,name,dflt)
end function

global function gDatePick(string attributes="", args={})
    if not bInit then xpg_Init() end if
    integer id = xpg_add_control(DATEPICK)
    xpg_register_handler(DATEPICK,"VALUE_CHANGED",{{1,1,"PO"}})
    set_ctrl_msg(DATEPICK,xpg_DatePick,xpg_set_datepick_attribute,
                                       xpg_get_datepick_attribute)
    if length(attributes) then
        gSetAttributes(id,attributes,args)
    end if
    return id
end function 

-- builtins/IupGraph.e
-- todo: bar graphs[stacked or side], grid style, piechart, ... [DEV]

local procedure redraw_graph(gdx graph)
--?"redraw_graph"

    rtn drid = gGetHandler(graph,"DRID")
    sequence sig = get_routine_info(drid,false),
             datasets = iff(sig={0,0,"F"}?drid(),
                        iff(sig={1,1,"FO"}?drid(graph):9/0)),
             grattrbs = ctrl_xtra[graph][CX_GTL_ATTRS]
    integer grid = grattrbs[GX_GRID],
            gridcolour = grattrbs[GX_GRIDCOLOR],
--          {width, hight} = gGetAttribute(graph,"DRAWSIZE"),
            {width, hight} = gGetAttribute(graph,"SIZE"),
            dsdx = 1,
            drop = gGetHandler(graph,"DROP",xpg_default_DROP),
            xCanvasSetForeground = drop(gCanvasSetForeground),
            xCanvasSetBackground = drop(gCanvasSetBackground),
            xCanvasRect = drop(gCanvasRect),
            xCanvasLine = drop(gCanvasLine),
            xCanvasText = drop(gCanvasText),
            -- /now/ it feels safe to shadow the globals:
            gCanvasSetForeground = xCanvasSetForeground,
            gCanvasSetBackground = xCanvasSetBackground,
            gCanvasRect = xCanvasRect,
            gCanvasLine = xCanvasLine,
            gCanvasText = xCanvasText

--?{"graph size",width,hight}
    // nb: "XTICK" etc may be/get set in the drid() call.
    atom xtick = grattrbs[GX_XTICK],
         xmin = grattrbs[GX_XMIN],
         xmax = grattrbs[GX_XMAX],
         xmargin = grattrbs[GX_XMARGIN],
         xyshift = grattrbs[GX_XYSHIFT],
         xangle = grattrbs[GX_XANGLE],
         ytick = grattrbs[GX_YTICK],
         ymin = grattrbs[GX_YMIN],
         ymax = grattrbs[GX_YMAX],
         ymargin = grattrbs[GX_YMARGIN],
         yxshift = grattrbs[GX_YXSHIFT],
         yangle = grattrbs[GX_YANGLE],
         bgclr = gCanvasGetBackground(graph)
?{"xtick",xtick,"xmin",xmin,"xmax",xmax}

    string title = grattrbs[GX_GTITLE],
           xname = grattrbs[GX_XNAME],
           yname = grattrbs[GX_YNAME],
           xfmt = grattrbs[GX_XTICKFMT],
           yfmt = grattrbs[GX_YTICKFMT],
           mode = grattrbs[GX_MODE],
           barmode = grattrbs[GX_BARMODE],
           markstyle = grattrbs[GX_MARKSTYLE]
--         fontface = "Helvetica"
--  integer fontstyle = CD_PLAIN,
--          fontsize = 9,
--          bgclr = gGetAttribute(graph,"BGCOLOR"),
--          titlestyle = gGetInt(graph,"TITLESTYLE",CD_PLAIN),
    integer marksize = grattrbs[GX_MARKSIZE],
            legend = 0, -- (idx to datasets)
            lx, ly -- (CD_EAST of the first legend text)
--  cdCanvas cd_canvas = gGetAttribute(graph,"CD_CANVAS")
    integer xacross = grattrbs[GX_XACROSS],
            yacross = grattrbs[GX_YACROSS],
            xrid = gGetHandler(graph,"XRID"),
            yrid = gGetHandler(graph,"YRID"),
            post = gGetHandler(graph,"POST")
    while true do
        sequence ds = datasets[dsdx],
                 s = ds[1]
        if not string(s) then exit end if
--      if s="BCOLOR" then
--          bgclr = ds[2]
        if s="NAMES" then
            legend = dsdx
        elsif s="POST" then
            post = ds[2]
        else
--          {fontface,fontstyle,fontsize} = ds
            ?9/0
        end if
        dsdx += 1
    end while
--  gCanvasSetBackground(graph,bgclr)
--  cdCanvasActivate(graph)
--  cdCanvasClear(graph)

    -- draw title and axis names
    gCanvasSetForeground(graph,XPG_BLACK)
    if title!="" then
        gCanvasText(graph,width/2+4,0,title,XPG_SOUTH)
    end if
    if yname!="" then
        gCanvasText(graph,34,hight/2,yname,XPG_SOUTH,90)
    end if
    if xname!="" then
        gCanvasText(graph,width/2,hight-4,xname,XPG_NORTH)
    end if
    
    -- draw the x/y-axis labels and vertical gridlines
    xacross = iff(xacross?round((0-xmin)/xtick)+1:1)
    yacross = iff(yacross?round((0-ymin)/ytick)+1:1)

    integer vb = (barmode="VERTICAL"),
            hb = (barmode="HORIZONTAL"),
            nx = round((xmax-xmin)/xtick)+vb,
            ny = round((ymax-ymin)/ytick)+hb
--DEV
if nx<=0 or ny<=0 then
    ?{"gGrapg: nx,ny:",nx,ny}
    nx = max(nx,1)
    ny = max(ny,1)
end if
    if barmode!="" then
        assert(vb or hb,"invalid BARMODE")
        assert(mode="" or mode="BAR","invalid MODE for BARMODE")
        mode = "BAR"
    elsif mode="BAR" then
        vb = true
    end if
    if markstyle!="" and mode!="MARK" and mode!="MARKLINE" then
        assert(mode="","invalid MODE for MARKSTYLE")
        mode = "MARK"
    end if
    atom dx = (width-60-xmargin)/nx,
         dy = (hight-60-ymargin)/ny,
         vx = 30+xmargin,
         vy = 30+ymargin,
         x = xmin,
         y = ymin
    for i=1 to nx+1-vb do   -- the vertical lines
        if (grid and not vb) or i=xacross then
            gCanvasSetForeground(graph,iff(i=xacross?XPG_BLACK:gridcolour))
--          gCanvasLine(graph,vx,30+ymargin,vx,hight-30)
            gCanvasLine(graph,vx,hight-(30+ymargin),vx,28)
        end if
        gCanvasSetForeground(graph,XPG_BLACK)
        integer align = iff(xangle=0?XPG_SOUTH:
                        iff(xangle=90?XPG_EAST:
                        iff(xangle=-90?XPG_WEST:9/0)))
        string xtext = iff(xrid?xrid(x):sprintf(xfmt,x))
        atom ty = 25+ymargin+xyshift+(yacross-1)*dy
--      gCanvasText(graph,vx+dx*vb/2,ty,xtext,align,xangle)
--      gCanvasText(graph,vx+dx*vb/2,hight-ty,xtext,align,xangle)
--      gCanvasText(graph,vx+dx*vb/2+1,hight-ty,xtext,align,xangle)
        gCanvasText(graph,vx+dx*vb/2+1,hight-ty,xtext,align,{XPG_W,xangle})
        vx += dx
        x += xtick
    end for
    for i=1 to ny+1-hb do   -- the horizontal lines
        if (grid and not hb) or i=yacross then
            gCanvasSetForeground(graph,iff(i=yacross?XPG_BLACK:gridcolour))
--          gCanvasLine(graph,31+xmargin,vy,width-30,vy)
            gCanvasLine(graph,31+xmargin,hight-vy,width-30,hight-vy)
        end if
        gCanvasSetForeground(graph,XPG_BLACK)
        integer align = iff(yangle=0?XPG_WEST:
                        iff(yangle=90?XPG_NORTH:
                        iff(yangle=-90?XPG_SOUTH:9/0)))
        string ytext = iff(yrid?yrid(y):sprintf(yfmt,y))
        atom tx = 25+xmargin+yxshift+(xacross-1)*dx
--      gCanvasText(graph,tx,vy+dy*hb/2,ytext,align,yangle)
--      gCanvasText(graph,tx,hight-(vy+dy*hb/2),ytext,align,yangle)
        gCanvasText(graph,tx,hight-(vy+dy*hb/2)+2,ytext,align,yangle)
        vy += dy
        y += ytick
    end for 

    integer lw,lh -- (legend text hight, per line)
--DEV does this not want to be drawn last??
    if legend then
        sequence legendnames = datasets[legend][2]
--      cdCanvasSetTextOrientation(graph,0)
--      cdCanvasSetTextAlignment(graph,CD_EAST)
--DEV/SUG:
--      atom {lw,lh,cairo,layout} = xpg_get_max_text_extent(graph,legendnames)
--      integer ll = length(legendnames), lw=0, lwi
--/*
        for i=1 to ll do
--          {lwi,lh} = cdCanvasGetTextSize(graph,legendnames[i])
--          string text = legendnames[i]
--          if backend=GTK then
--              c_proc(pango_layout_set_text,{layout,text,length(text)})
--              c_proc(pango_layout_get_pixel_size,{layout,pWidth,pHight})
--              lwi = get_struct_field(idGdkRectangle,pRECT,"width")
--              lh = get_struct_field(idGdkRectangle,pRECT,"hight")
--          elsif backend=WinAPI then
--(DEV well, that one was waiting to blow up anyway!)
--              {lwi,lh} = gGetTextExtent(graph,text)
--          else
--              ?9/0 -- (unknown backend)
--          end if
--          {lwi,lh} = gGetTextExtent(graph,legendnames[i])
--          lw = max(lw,lwi)
            lw = max(lw,gGetTextExtent(graph,legendnames[i])[1])
        end for
--*/
        {lw,lh} = gGetTextExtent(graph,legendnames,false)
--      lh = floor(lh/length(legendnames))
--      if backend=GTK then
--          c_proc(xg_object_unref,{layout})
--      end if
        string legendpos = gGetAttribute(graph,"LEGENDPOS","TOPRIGHT")
        if legendpos="XY" then
--          {lx,ly} = gGetIntInt(graph,"LEGENDPOSXY") -- (untested)
            {lx,ly} = gGetAttribute(graph,"LEGENDXY") -- (untested)
        else
            if legendpos[1]='T' then
                assert(legendpos[1..3]="TOP")
                legendpos = legendpos[4..$]
                ly = 10
            else
                assert(legendpos[1..6]="BOTTOM")
                legendpos = legendpos[7..$]
--              ly = hight-50-ll*lh
                ly = hight-50-lh
            end if
            if legendpos="LEFT" then
                lx = 30+xmargin+lw
            elsif legendpos="CENTER" then
                lx = floor((xmargin+width+lw)/2)
            else
                assert(legendpos="RIGHT")   
                lx = width-30
            end if
        end if
--      if gGetInt(graph,"LEGENDBOX") then
        if grattrbs[GX_LEGENDBOX] then
--          gCanvasSetForeground(graph,bgclr)
            gCanvasSetForeground(graph,XPG_BLACK)
            integer lxl = lx-lw-25, lxr = lx+10,
--                  lyt = hight-(ly+15), lyb = hight-(ly+ll*lh+25)
--DEV tryme:
--                  lyt = ly+15, lyb = ly+ll*lh+25
                    lyt = ly+15, lyb = ly+lh+25
-- or maybe:
--                  lyb = ly+15, lyt = ly+ll*lh+25
            gCanvasRect(graph,lxl,lxr,lyt,lyb,true)
--          gCanvasSetForeground(graph,XPG_BLACK)
--          gCanvasRect(graph,lxl,lxr,lyt,lyb)
        end if
    end if

    -- and finally draw/plot the points!
    atom w = dx/xtick,
         h = dy/ytick
    vx = 30+xmargin + (xacross-1)*dx
    vy = 30+ymargin + (yacross-1)*dy
    integer lm1 = dsdx-1
    for d=dsdx to length(datasets) do
        sequence dd = datasets[d],
                 {px,py} = dd
        integer ldd = length(dd),
                 mm = (mode="MARK")
        string dms = markstyle,
               dmm = mode
        if ldd>=4 then
            mm = true
            dms = dd[4]
            if ldd>=5 then
                assert(dd[5]="MARKLINE")
                dmm = "MARKLINE"
            end if
        end if          
        atom fgclr = iff(ldd>=3?dd[3]:XPG_BLACK)
        gCanvasSetForeground(graph,fgclr)
        gCanvasSetBackground(graph,fgclr)
--gdots = 14000
        if length(px) then
            atom x1 = 30+xmargin+(px[1]-xmin)*w,
                 y1 = 30+ymargin+(py[1]-ymin)*h
            for i=2-(vb or hb or mm) to length(px) do
                atom x2 = 30+xmargin+(px[i]-xmin)*w+dx*vb/2,
                     y2 = 30+ymargin+(py[i]-ymin)*h+dy*hb/2
                if mode="BAR" then
--                  gCanvasSetBackground(graph,fgclr)
                    if vb then
--                      gCanvasRect(graph,x2-dx/2+1,x2+dx/2-1,vy,y2,true)
                        gCanvasRect(graph,x2-dx/2+1,x2+dx/2-1,hight-vy,hight-y2,true)
                    elsif hb then
--                      gCanvasRect(graph,vx,x2,y2-dy/2+1,y2+dy/2-1,true)
                        gCanvasRect(graph,vx,x2,hight-(y2-dy/2+1),hight-(y2+dy/2-1),true)
                    end if
                else
                    if mm then
-- (from IupPlot:) mark style of the current dataset. 
--        Can be: "HOLLOW_CIRCLE", "PLUS", "X", [DONE]
--          "STAR", "CIRCLE", "BOX", "DIAMOND",
--          "HOLLOW_BOX", "HOLLOW_DIAMOND". Default "X". 
--        (rest to be implemented as and when needed)
                        if dms="HOLLOW_CIRCLE" then
--                          gCanvasCircle(graph,x2,y2,8)
                            gCanvasCircle(graph,x2,hight-y2,2*(marksize+1))
                        elsif dms="PLUS" then
--                          gCanvasLine(graph,x2,y2-3,x2,y2+3)
                            gCanvasLine(graph,x2,hight-(y2-marksize),x2,hight-(y2+marksize+1))
--                          gCanvasLine(graph,x2-3,y2,x2+3,y2)
                            gCanvasLine(graph,x2-marksize+1,hight-y2,x2+marksize,hight-y2)
                        elsif dms="X" then
--                          gCanvasLine(graph,x2-3,y2-3,x2+3,y2+3)
                            gCanvasLine(graph,x2-marksize,hight-(y2-marksize),x2+marksize,hight-(y2+marksize))
--                          gCanvasLine(graph,x2-3,y2+3,x2+3,y2-3)
                            gCanvasLine(graph,x2-marksize,hight-(y2+marksize),x2+marksize,hight-(y2-marksize))
                        elsif dms="DOT" then
                            gCanvasPixel(graph,x2,hight-y2)
                        else
                            crash("unknown MARKSTYLE (%s)",{dms})
                        end if
                    end if
                    if not mm or (dmm="MARKLINE" and i>=2) then
--                      gCanvasLine(graph,x1,y1,x2,y2)
--if abs(x2-x1)<.5 and abs(y2-y1)<.5 then
--                      gCanvasPixel(graph,x2,hight-y2)
--else
                        gCanvasLine(graph,x1,hight-y1,x2,hight-y2)
--end if
                    end if
                end if
                x1 = x2
                y1 = y2
            end for
        end if
--gdots = 0
        if legend then
            integer lX = lx-20,
--                  lY = hight-ly-25
                    lY = ly-25
            if mode="BAR" then -- (untested)
                gCanvasRect(graph,lX,lX+10,lY-5,lY+5,true)
--              gCanvasRect(graph,lX,lX+10,hight-(lY-5),hight-(lY+5),true)
            else
                if mm then
                    if dms="HOLLOW_CIRCLE" then
                        gCanvasCircle(graph,lX+15,lY,8)
                    elsif dms="PLUS" then
                        gCanvasLine(graph,lX+15,lY-5,lX+15,lY-5)
                        gCanvasLine(graph,lX+10,lY,lX+20,lY)
                    elsif dms="X" then
                        gCanvasLine(graph,lX+10,lY+5,lX+20,lY-5)
                        gCanvasLine(graph,lX+10,lY-5,lX+20,lY+5)
                    elsif dms="DOT" then
                        gCanvasPixel(graph,lX+15,lY)
                    else --default/x
                        ?9/0
                    end if
                end if
                if not mm or dmm="MARKLINE" then
                    gCanvasLine(graph,lX+5,lY,lX+25,lY)
                end if
            end if
            gCanvasSetForeground(graph,XPG_BLACK)
            gCanvasText(graph,lX,lY,datasets[legend][2][d-lm1])
--DEV erm...            
            ly += lh
--          ly -= lh
        end if
    end for

    gCanvasSetBackground(graph,bgclr)
--  cdCanvasFlush(graph)
--  IupSetAttribute(graph,"RASTERSIZE",NULL) -- release the minimum limitation
    if post then post(graph) end if
--?"graph drawn"
end procedure
rtn_graph_redraw = redraw_graph; -- (let/permit/allow this to be a perfectly valid rtn)

global function gGraph(rtn drid, string attributes="", sequence args={})
    gdx graph = gCanvas(redraw_graph)
--  gdx graph = gCanvas(redraw_graph,attributes,args)
--  gdx graph = gCanvas(redraw_graph,attributes,args,GRAPH)
    ctrl_xtra[graph][CX_CANVAS_TYPE] = GRAPH
--  gSetAttribute(graph,"BGCOLOR",XPG_WHITE) -- (set a default)
--erm...
--  gCanvasSetForeground(graph,XPG_WHITE) -- (set a default)
    xpg_register_handler(CANVAS,"DRID",{{0,0,"F"},{1,1,"FO"}})
    xpg_register_handler(CANVAS,"XRID",{{1,1,"FI"}})
    xpg_register_handler(CANVAS,"YRID",{{1,1,"FI"}})
    gSetHandler(graph,"DRID",drid)
    object grattrbs = repeat(0,GX_LEN)
    grattrbs[GX_GRID] = true        -- (show the grid by default)
    grattrbs[GX_LEGENDBOX] = true   -- (ditto box around legend)
    grattrbs[GX_GRIDCOLOR] = XPG_GREY
    grattrbs[GX_XTICK] = 1
    grattrbs[GX_YTICK] = 1
    grattrbs[GX_XMARGIN] = 10
    grattrbs[GX_YMARGIN] = 10
    grattrbs[GX_GTITLE] = ""
    grattrbs[GX_XNAME] = ""
    grattrbs[GX_YNAME] = ""
    grattrbs[GX_XTICKFMT] = "%g"
    grattrbs[GX_YTICKFMT] = "%g"
    grattrbs[GX_MODE] = ""
    grattrbs[GX_BARMODE] = ""
    grattrbs[GX_MARKSTYLE] = ""
    grattrbs[GX_MARKSIZE] = 3
    ctrl_xtra[graph][CX_GTL_ATTRS] = grattrbs
    grattrbs = 0 -- (kill refcount)
    if length(attributes) then
        gSetAttributes(graph,attributes,args)
    end if
    return graph    
end function

--DOH: the child of a dialog **must** be a vbox for a menu to be attached.
--DOC: there is no MENU attribute of a gDialog, it's ""...

local function xpg_gtk_menu_clicked(atom handle, integer id) -- (GTK only)
    return xpg_menu_common(getd(handle,GTK_MENU_LOOKUP),id,false)
end function

local function xpg_gtk_menu_selected(atom handle, integer id) -- (GTK only)
    return xpg_menu_common(getd(handle,GTK_MENU_LOOKUP),id,true)
end function

local procedure xpg_add_menu(integer pmid, atom menu, sequence children, bool bRadio)
--?"xpg_add_menu"
    bool bOK
    string text
    integer mdx
    atom radio_group = NULL
    sequence rdi = {}
    for pos,c in children do
        if string(c) then
            assert(c="|" or c="-")
            c = c[1]
        end if
        if atom(c) then
            assert(c='|' or c='-')
            if backend=GTK then
                atom sep = c_func(gtk_separator_menu_item_new,{})
                c_proc(gtk_widget_show,{sep}) -- (needed for popup menus)
                c_proc(gtk_menu_shell_append,{menu,sep})
                if radio_group!=NULL then
                    bRadio = false
                end if
            elsif backend=WinAPI then
                bOK = c_func(xAppendMenu,{menu,MF_SEPARATOR,0,NULL})
                assert(bOK)
                if length(rdi) then
                    bRadio = false
                end if
            else
                ?9/0 -- (unknown backend)
            end if
        elsif length(c)=2 and integer(c[2]) then
            -- eg {"Cut",CUT=$}
            {text,mdx} = c
            bool bCheck = text[1]='?'
            if bCheck then text = text[2..$] end if
            if backend=GTK then
                text = xpg_gtk_mnemonicalize(text)
                atom mitem
                if bCheck then
                    if bRadio then
                        mitem = c_func(gtk_radio_menu_item_new_with_mnemonic,{radio_group,text})
                        assert(mitem!=NULL)
                        radio_group = c_func(gtk_radio_menu_item_get_group,{mitem})
                    else
                        mitem = c_func(gtk_check_menu_item_new_with_mnemonic,{text})
                    end if
                else
                    mitem = c_func(gtk_menu_item_new_with_mnemonic,{text})
                end if
--?{mdx,text,mitem}
                c_proc(gtk_widget_show,{mitem}) -- (needed for popup menus)
                setd(mitem,pmid,GTK_MENU_LOOKUP)
                if mdx then 
                    setd({pmid,mdx},mitem,GTK_MENU_UPLOOK)
                end if
                xpg_signal_connect(mitem,"activate",xpg_gtk_menu_clicked,mdx)
                xpg_signal_connect(mitem,"select",xpg_gtk_menu_selected,mdx)
                c_proc(gtk_menu_shell_append,{menu,mitem})
--/*

GtkWidget*
gtk_radio_menu_item_new_with_mnemonic_from_widget (
  GtkRadioMenuItem* group,
  const gchar* label
)
GtkWidget*
gtk_check_menu_item_new_with_mnemonic (
  const gchar* label
)

  GtkWidget *box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL,6);
  GtkWidget *icon = gtk_image_new_from_icon_name ("folder-music-symbolic",GTK_ICON_SIZE_MENU);
  GtkWidget *label = gtk_accel_label_new ("Music");
  GtkWidget *menu_item = gtk_menu_item_new ();
  GtkAccelGroup *accel_group = gtk_accel_group_new ();

  gtk_container_add (GTK_CONTAINER (box),icon);

  gtk_label_set_use_underline (GTK_LABEL (label),TRUE);
  gtk_label_set_xalign (GTK_LABEL (label),0.0);

  gtk_widget_add_accelerator (menu_item,"activate",accel_group,
                              GDK_KEY_m,GDK_CONTROL_MASK,GTK_ACCEL_VISIBLE);
  gtk_accel_label_set_accel_widget (GTK_ACCEL_LABEL (label),menu_item);

  gtk_box_pack_end (GTK_BOX (box),label,TRUE,TRUE,0);

  gtk_container_add (GTK_CONTAINER (menu_item),box);

  gtk_widget_show_all (menu_item);
--*/
            elsif backend=WinAPI then

--              bOK = c_func(xAppendMenu,{menu,MF_STRING+MF_CHECKED*bCheck,mdx,text})
                bOK = c_func(xAppendMenu,{menu,MF_STRING,mdx,text})
--?{mdx,text}
                assert(bOK)
--              if bCheck and bRadio then rdi &= mdx end if
                if bCheck then
--                  c_proc(xCheckMenuItem,{menu,mdx,MF_BYCOMMAND+MF_CHECKED})
                    if bRadio then
                        rdi &= mdx -- (full set stored for each below)
                    else
                        setd({pmid,mdx},0,WIN_MENU_CHECKED)
                    end if
                end if
            else
                ?9/0 -- (unknown backend)
            end if
        else
            text = c[1]
            mdx = iff(length(c)=3?c[2]:0)
            sequence grandkids = c[$]
            bool bSubRadio = text[1]='?'
            if bSubRadio then text = text[2..$] end if
            if backend=GTK then
                text = xpg_gtk_mnemonicalize(text)
                atom mitem = c_func(gtk_menu_item_new_with_mnemonic,{text}),
                   submenu = c_func(gtk_menu_new,{})
                xpg_add_menu(pmid,submenu,grandkids,bSubRadio)
                c_proc(gtk_menu_item_set_submenu,{mitem,submenu})
                if mdx then
                    setd(mitem,pmid,GTK_MENU_LOOKUP)
--?{{pmid,mdx},text}
                    setd({pmid,mdx},mitem,GTK_MENU_UPLOOK)
                    xpg_signal_connect(mitem,"activate",xpg_gtk_menu_clicked,mdx)
                    xpg_signal_connect(mitem,"select",xpg_gtk_menu_selected,mdx)
                end if
                c_proc(gtk_widget_show,{mitem}) -- (needed for popup menus)
                c_proc(gtk_menu_shell_append,{menu,mitem})
                if radio_group!=NULL then
                    bRadio = false
                end if
            elsif backend=WinAPI then
                atom submenu = c_func(xCreatePopupMenu,{})
--?{"submenu",submenu,text,mdx,pmid}
                if mdx then
                    setd(submenu,mdx,WINAPI_SUBMENUS)
                    setd({pmid,mdx},{menu,pos-1},WINAPI_SUBMENUS)
                end if
                xpg_add_menu(pmid,submenu,grandkids,bSubRadio)
                bOK = c_func(xAppendMenu,{menu,or_bits(MF_POPUP,MF_STRING),submenu,text})
                assert(bOK)
                if length(rdi) then
                    bRadio = false
                end if
            else
                ?9/0 -- (unknown backend)
            end if
        end if
    end for
    if length(rdi) then -- (implicitly WinAPI only)
        rdi = sort(rdi)
        integer f = rdi[1], l = rdi[$]
        assert((l-f)=(length(rdi)-1))
        bOK = c_func(xCheckMenuRadioItem,{menu,f,l,f,MF_BYCOMMAND})
        -- and stash the full set against every entry
        for mdx in rdi do
            setd({pmid,mdx},{rdi,menu},WIN_MENU_CHECKED)
        end for
    end if
end procedure

-- or, if parent is a DIALOG it is a menubar...
local procedure xpg_Menu(gdx id)
    -- (invoked via xpg_map)
    gdx parent = xpg_get_parent_id(id)
    atom menu
    sequence children = ctrl_xtra[id][1]
    bool bRadio = ctrl_xtra[id][2]
    assert(parent=NULL or bRadio=false)
    ctrl_xtra[id] = 0
    if backend=GTK then
--      assert(vbox?)
        if parent then
            menu = c_func(gtk_menu_bar_new,{})
            atom vbox = ctrl_handles[parent]
            c_proc(gtk_fixed_put,{vbox,menu,0,0})
        else
            menu = c_func(gtk_menu_new,{})
        end if
--      xpg_signal_connect(checkbox,"realize",xpg_gtk_widget_realized,id)
--      xpg_signal_connect(menu,"clicked",xpg_gtk_menu_clicked,id)
--      xpg_signal_connect(checkbox,"focus-in-event",xpg_gtk_focusinout,id)
--      xpg_signal_connect(checkbox,"focus-out-event",xpg_gtk_focusinout,id)
--      c_proc(gtk_widget_realize,{menu})
    elsif backend=WinAPI then
--             {w,h} = gGetTextExtent(parent,title)
--      ctrl_size[id][SZ_NATURAL_W] = w?
--      ctrl_size[id][SZ_NATURAL_H] = h
        if parent then
            menu = c_func(xCreateMenu,{})
            bool bOK = c_func(xSetMenu,{ctrl_handles[parent],menu})
            assert(bOK)
        else
            menu = c_func(xCreatePopupMenu,{})
        end if
    else
        ?9/0 -- (unknown backend)
    end if
    xpg_setID(menu,id)
    xpg_add_menu(id,menu,children,false)
--dev deferred attributes?
--?def_menu_attr[menu]
--{{1,2},{{{"CHECKED",1},{"ACTIVE",1}},{{"ACTIVE",0}}}}
    sequence {ids,attrs} = def_menu_attr[id]
    def_menu_attr[id] = 0
    for i,idx in ids do
        for nv in attrs[i] do
            {string name, object v} = nv
            gMenuSetAttribute(id,idx,name,v)
        end for
    end for
    if backend=WinAPI then drawMenuBar(id) end if
end procedure

global function gMenu(sequence children, rtn handler, bool bRadio=false)
    if not bInit then xpg_Init() end if
    integer id = xpg_add_control(MENU)
    ctrl_xtra[id] = {children,bRadio}
--  xpg_register_handler(MENU,"HANDLER",{{3,3,"FOII"},{2,2,"FOI"},{2,2,"FII"},{1,1,"FI"}})
--  xpg_register_handler(MENU,"HANDLER",{{3,3,"FOII"},{2,2,"FOI"},{1,1,"FI"}})
    xpg_register_handler(MENU,"HANDLER",{{3,3,"FOII"},{2,2,"FOI"}})
    set_ctrl_msg(MENU,xpg_Menu,0,0) -- (ie gMenuGet[/Set]Attribute shd be used instead)
--  gSetAttribute(id,"FONT","Helvetica, 9") -- WinAPI really don't like that!
    assert(handler!=NULL)
    gSetHandler(id,"HANDLER",handler)
    def_menu_attr[id] = {{},{}} -- {{ids},{name,v} pairs}
    return id
end function 

--From: https://zetcode.com/gui/gtk2/menusandtoolbars/

-- https://stackoverflow.com/questions/5561236/win32-splitter-control

--DEV/temp:
procedure xpg_lm_dump_ctrls()
    printf(1,"id ----ctyp----   x   y   w   h  nw  nh  uw  uh  p  children   flags\n")
    for id=1 to length(ctrl_types) do
        integer ct = ctrl_types[id],
                 x = ctrl_size[id][SZ_X],
                 y = ctrl_size[id][SZ_Y],
                 w = ctrl_size[id][SZ_W],
                 h = ctrl_size[id][SZ_H],
--              nw = ctrl_size[id][SZ_NATURAL_W],
--              nh = ctrl_size[id][SZ_NATURAL_H],
                nw = ctrl_size[id][SZ_NORMAL_W],
                nh = ctrl_size[id][SZ_NORMAL_H],
                uw = ctrl_size[id][SZ_USER_W],
                uh = ctrl_size[id][SZ_USER_H],
                 p = parent_ids[id]
        string idt = ctrl_names[ct]
        object c = children_ids[id]
        atom flags = ctrl_flags[id]
        bool bMapped = and_bits(flags,CF_MAPPED)!=0
        if not bMapped then
            printf(1,"%2d %-12s %57s\n",{id,idt,"Not mapped"})
        else
            if ct=BOX then
                integer v = and_bits(flags,CF_VERTICAL)
                idt := iff(v?"Vbox":"Hbox")
                flags -= v -- (maybe/maybe not)
            end if
            string f = decode_flags(cf_glags,flags)
            if not find(ct,{BOX,MENU,TIMER,CLIPBOARD}) then
                f = sprintf("%v",{xpg_get_window_rect(id)})
            end if
            printf(1,"%2d %-12s %3d %3d %3d %3d %3d %3d %3d %3d %2d  %-10v %s\n",
                     { id,  idt,  x,  y,  w,  h, nw, nh, uw, uh,  p,     c, f})
        end if
    end for
end procedure

--IupMap(ih) is
--  /* calculate position and size for all children */
--  iupLayoutCompute(ih) {
--        /* usually called only for the dialog */
--        int shrink = iupAttribGetBoolean(ih,"SHRINK");
--        /* Compute the natural size for all elements in the dialog, using the minimum visible size and the defined user size.
--           The minimum visible size is the size where all the controls can display all their contents.
--           The defined user size is used to increase the value of the minimum visible size for containers,
--           for standard controls will replace the minimum visible size.
--           So the native size will be the maximum value between minimum visible size and defined user size.
--           Also calculates the expand configuration for each element, but expand is used only in SetChildrenCurrentSize.
--           SEQUENCE: will first calculate the native size for the children, then for the element. */
--        iupBaseComputeNaturalSize(ih) {
--            /* always initialize the natural size using the user size */
--            ih->naturalwidth = ih->userwidth;
--            ih->naturalheight = ih->userheight;
--                  /* pre-defined dialogs can restrict the number of children */
--            if (ih->iclass->childtype != IUP_CHILDNONE || ih->iclass->nativetype == IUP_TYPEDIALOG) {
--              int w=0, h=0, children_expand=0;  /* if there is no children will not expand, when not a dialog */
--              /* If a container then update the "expand" member from the EXPAND attribute.
--                 The ih->expand member can not be used for the container attribute because
--                 it is used to combine the container value with the children value. */
--              iupBaseContainerUpdateExpand(ih) {
--                char *expand = iupAttribGetInherit(ih,"EXPAND");
--                if (!expand)
--                  ih->expand = IUP_EXPAND_BOTH;  /* default for containers is YES */
--                else {
--                  if (iupStrEqualNoCase(expand,"NO"))
--                    ih->expand = IUP_EXPAND_NONE;
--                  else if (iupStrEqualNoCase(expand,"HORIZONTAL"))
--                    ih->expand = IUP_EXPAND_WIDTH;
--                  else if (iupStrEqualNoCase(expand,"VERTICAL"))
--                    ih->expand = IUP_EXPAND_HEIGHT;
--                  else if (iupStrEqualNoCase(expand,"HORIZONTALFREE"))
--                    ih->expand = IUP_EXPAND_WFREE;
--                  else if (iupStrEqualNoCase(expand,"VERTICALFREE"))
--                    ih->expand = IUP_EXPAND_HFREE;
--                  else
--                    ih->expand = IUP_EXPAND_BOTH;  /* default for containers is YES */
--                }
--              }
--              /* for containers always compute */
--              iupClassObjectComputeNaturalSize(ih,&w,&h,&children_expand) {
--                ic:=ih->iclass;
--                if (ic->parent)
--                  iClassComputeNaturalSize(ic->parent,ih,w,h,children_expand);
--
--                if (ic->ComputeNaturalSize)
--                  ic->ComputeNaturalSize(ih,w,h,children_expand); // (element-specific, but predictable enough)
--              }
--              if (ih->iclass->nativetype == IUP_TYPEDIALOG) {
--                /* only update the natural size if user size is not defined. */
--                /* IupDialog is the only container where this must be done */ 
--                /* if the natural size is bigger than the actual dialog size then
--                   the dialog will be resized, if smaller then the dialog remains with the same size. */
--                ih->expand |= children_expand;
--                if (ih->naturalwidth <= 0) ih->naturalwidth = iupMAX(ih->currentwidth,w);
--                if (ih->naturalheight <= 0) ih->naturalheight = iupMAX(ih->currentheight,h);
--              } else {
--                /* combine to only expand if the children can expand */
--                ih->expand &= children_expand; 
--                ih->naturalwidth = iupMAX(ih->naturalwidth,w);
--                ih->naturalheight = iupMAX(ih->naturalheight,h);
--              }
--            } else  {
--              /* for non-container only compute if user size is not defined */
--              if (ih->naturalwidth <= 0 || ih->naturalheight <= 0) {
--                int w=0, h=0, children_expand;    /* unused if not a container */
--                iupClassObjectComputeNaturalSize(ih,&w,&h,&children_expand);
--                if (ih->naturalwidth <= 0) ih->naturalwidth = w;
--                if (ih->naturalheight <= 0) ih->naturalheight = h;
--              }
--            }
--            /* crop the natural size */
--            iupLayoutApplyMinMaxSize(ih,&(ih->naturalwidth),&(ih->naturalheight));
--              void iupLayoutApplyMinMaxSize(Ihandle* ih, int *w, int *h) {
--                if (ih->flags & IUP_MINSIZE) {
--                  char* value = iupAttribGet(ih,"MINSIZE");
--                  int min_w = 0, min_h = 0;          /* MINSIZE default value */
--                  iupStrToIntInt(value,&min_w,&min_h,'x');
--                  if (w && *w < min_w) *w = min_w;
--                  if (h && *h < min_h) *h = min_h;
--                }
--                if (ih->flags & IUP_MAXSIZE) {
--                  char* value = iupAttribGet(ih,"MAXSIZE");
--                  int max_w = 65535, max_h = 65535;  /* MAXSIZE default value */
--                  iupStrToIntInt(value,&max_w,&max_h,'x');
--                  if (w && *w > max_w) *w = max_w;
--                  if (h && *h > max_h) *h = max_h;
--                }
--              }
--          }
--        }
--
--        /* Set the current size (not reflected in the native element yet) based on the natural size and the expand configuration. 
--           If shrink is 0 (default) the current size of containers can be only larger than the natural size,
--           the result will depend on the EXPAND attribute.
--           If shrink is 1 the containers can be resized to sizes smaller than the natural size.
--           SEQUENCE: will first calculate the current size of the element, then for the children. */
--        iupBaseSetCurrentSize(ih,0,0,shrink);
--          void iupBaseSetCurrentSize(Ihandle* ih, int w, int h, int shrink) {
--            if (ih->iclass->nativetype == IUP_TYPEDIALOG) {
--              /* w and h parameters here are ignored, because they are always 0 for the dialog. */
--              /* current size is zero before map and when reset by the application */
--              /* after that the current size must follow the actual size of the dialog */
--              if (!ih->currentwidth)  ih->currentwidth  = ih->naturalwidth;
--              if (!ih->currentheight) ih->currentheight = ih->naturalheight;
--            } else {
--              if (ih->iclass->childtype != IUP_CHILDNONE && !shrink) {
--                /* shrink is only used by containers, usually is 0 */
--                /* for non containers is always 1, so they always can be smaller than the natural size */
--                w = iupMAX(ih->naturalwidth,w);
--                h = iupMAX(ih->naturalheight,h);
--              }
--              /* if expand use the given size, else use the natural size */
--              ih->currentwidth = (ih->expand & IUP_EXPAND_WIDTH || ih->expand & IUP_EXPAND_WFREE) ? w : ih->naturalwidth;
--              ih->currentheight = (ih->expand & IUP_EXPAND_HEIGHT || ih->expand & IUP_EXPAND_HFREE) ? h : ih->naturalheight;
--            }
--            /* crop also the current size if some expanded */
--            if (ih->expand & IUP_EXPAND_WIDTH || ih->expand & IUP_EXPAND_HEIGHT ||
--                ih->expand & IUP_EXPAND_WFREE || ih->expand & IUP_EXPAND_HFREE)
--              iupLayoutApplyMinMaxSize(ih,&(ih->currentwidth),&(ih->currentheight));  -- [see above]
--            if (ih->firstchild)
--              iupClassObjectSetChildrenCurrentSize(ih,shrink) {
--                  Iclass* ic := ih->iclass;
--                  if (ic->parent)
--                      iClassSetChildrenCurrentSize(ic->parent,ih,shrink); -- (recurse)
--
--                  if (ic->SetChildrenCurrentSize)
--                      ic->SetChildrenCurrentSize(ih,shrink); // element-specific, messy, spreads slack
--              }
--
--        /* Now that the current size is known, set the position of the elements relative to the parent.
--           SEQUENCE: will first set the position of the element, then for the children. */
--        iupBaseSetPosition(ih,0,0);
--          void iupBaseSetPosition(Ihandle* ih,int x,int y) {
--            ih->x = x;
--            ih->y = y;
--            if (ih->firstchild)
--              iupClassObjectSetChildrenPosition(ih,x,y); eg:
--              static void iFrameSetChildrenPositionMethod(Ihandle* ih, int x, int y) {
--                if (ih->firstchild) {
--                  char* offset = iupAttribGet(ih,"CHILDOFFSET");
--                  /* Native container, position is reset */
--                  x = 0;
--                  y = 0;
--                  if (offset) iupStrToIntInt(offset,&x,&y,'x');
--                  /* In Windows the position of the child is still
--                  relative to the top-left corner of the frame.
--                  So we must manually add the decorations. */
--                  if (iupdrvFrameHasClientOffset()) { // gtk:0, WinAPI:1
--                    /* Windows Only */
--                    int dx = 0, dy = 0;
--                    iupdrvFrameGetDecorOffset(&dx,&dy); // gtk:2, WinApi: 3 or 2 (let's go with 3)
--                    if (iupAttribGet(ih,"_IUPFRAME_HAS_TITLE") || iupAttribGet(ih,"TITLE"))
--                      dy += iupFrameGetTitleHeight(ih);
--                    x += dx;
--                    y += dy;
--                  }
--                  /* Child coordinates are relative to client left-top corner. */
--                  iupBaseSetPosition(ih->firstchild,x,y);
--                }
--              }
--          }
--  }
--  /* moves and resizes the elements to reflect the layout computation */
--  iupLayoutUpdate(ih);
--  void iupLayoutUpdate(Ihandle* ih) {
--    Ihandle* child;
--    if (ih->flags & IUP_FLOATING_IGNORE) return;
--    /* update size and position of the native control */
--    iupClassObjectLayoutUpdate(ih);
--      ic := ih->iclass;
--      static void iClassLayoutUpdate(Iclass* ic, Ihandle *ih) {
--        if (ic->parent) iClassLayoutUpdate(ic->parent,ih);
--        if (ic->LayoutUpdate) ic->LayoutUpdate(ih);
--          static void winDialogLayoutUpdateMethod(Ihandle *ih) {
--            if (ih->data->ignore_resize)  return;
--            ih->data->ignore_resize = 1;
--            /* for dialogs the position is not updated here */
--            SetWindowPos(ih->handle,0,0,0,ih->currentwidth,ih->currentheight,
--                         SWP_NOMOVE|SWP_NOZORDER|SWP_NOACTIVATE|SWP_NOOWNERZORDER|SWP_NOSENDCHANGING);
--            ih->data->ignore_resize = 0;
--          }
--          void iupgtkNativeContainerMove(GtkWidget* container, GtkWidget* widget, int x, int y) {
--            gtk_fixed_move(GTK_FIXED(container),widget,x,y);
--          }
--          void iupgtkSetPosSize(GtkContainer* parent, GtkWidget* widget, int x, int y, int width, int height) {
--            iupgtkNativeContainerMove((GtkWidget*)parent,widget,x,y);
--            if (width > 0 && height > 0)
--              gtk_widget_set_size_request(widget,width,height);
--          }
--          void iupdrvBaseLayoutUpdateMethod(Ihandle *ih) {
--            GtkWidget* parent = gtkGetNativeParent(ih);
--            GtkWidget* widget = (GtkWidget*)iupAttribGet(ih,"_IUP_EXTRAPARENT");
--            if (!widget) widget = ih->handle;
--            iupgtkSetPosSize(GTK_CONTAINER(parent),widget,ih->x,ih->y,ih->currentwidth,ih->currentheight);
--          }
--          void iupdrvBaseLayoutUpdateMethod(Ihandle *ih) {
--            if (ih->currentwidth > 0 && ih->currentheight > 0)
--              SetWindowPos(ih->handle,NULL,ih->x,ih->y,ih->currentwidth,ih->currentheight,
--                           SWP_NOZORDER | SWP_NOACTIVATE | SWP_NOOWNERZORDER);
--            else /* move only */
--              SetWindowPos(ih->handle,NULL,ih->x,ih->y,0,0,
--                           SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE | SWP_NOOWNERZORDER);
--          }
--      }
--    /* update its children */
--    for (child = ih->firstchild; child; child = child->brother) {
--      if (child->handle) iupLayoutUpdate(child); // (recurse)
--    }
--  }

--local procedure xpg_realise(gdx id)
--  object children = children_ids[id]
--  if sequence(children) then
--      for child in children do
--          xpg_realise(child)
--      end for
--  end if
--  c_proc(gtk_widget_realize,{ctrl_handles[id]})
--end procedure

--global procedure gMap(gdx id, bool bRealise=true)
global procedure gMap(gdx id)
    --
    -- What this really addresses is that WinAPI demands parents are created first
    -- and specified as the child elements are created, however it is more natural
    -- to define say text, checkbox, and hbox({text,checkbox}), then dialog(hbox),
    -- hence actual control creation is deferred until the gShow(dialog) at which
    -- point it can/should be able to create things dialog-first.
    --
    nMapDepth += 1
--  assert(and_bits(ctrl_flags[id],CF_MAPPED)=0)
--?{"map",id}
    integer ct = ctrl_types[id]
    call_proc(ctrl_msg[ct][CM_MAP],{id})
--if id=Ibutton then ?{"Ibutton upfront:",deferred_attr[id]} end if
--?{"upfront:",deferred_attr[id]}
    xpg_apply_deferred_attributes(id)
--  assert(and_bits(ctrl_flags[id],CF_MAPPED)!=0)
--  integer w = ctrl_size[id][SZ_NATURAL_W],
--          h = ctrl_size[id][SZ_NATURAL_H]
--erm, CW_USEDEFAULT = #80000000 (ok, never stored)
    object children = children_ids[id]
    if sequence(children) then
--      bool bVert = and_bits(ctrl_flags[id],CF_VERTICAL)!=0
        for child in children do
            gMap(child)
--          integer cw = ctrl_size[child][SZ_NATURAL_W],
--                  ch = ctrl_size[child][SZ_NATURAL_H]
--          integer cw = ctrl_size[child][SZ_W],
--                  ch = ctrl_size[child][SZ_H]
--          w = iff(bVert?max(w,cw):w+cw)
--          h = iff(bVert?h+ch:max(h,ch))
        end for
--      ctrl_size[id][SZ_NATURAL_W] = w
--      ctrl_size[id][SZ_NATURAL_H] = h
--      ctrl_size[id][SZ_W] = w
--      ctrl_size[id][SZ_H] = h
        if ct=TABS then
            xpg_add_tabs(id)
        end if
    end if
--  if bRealise and backend=GTK then
--      xpg_realise(id)
--  end if
--  if and_bits(ctrl_flags[id],CF_MAPPED) then -- (skip virtual v/hbox)
--if id=Ibutton then ?{"Ibutton deferred:",deferred_attr[id]} end if
--?{"deferred:",deferred_attr[id]}
--  xpg_apply_deferred_attributes(id)
--  end if
--?{id,w,h}
--  ctrl_size[id][SZ_W] = w
--  ctrl_size[id][SZ_H] = h
--?{"/map",id}
    nMapDepth -= 1
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
    integer res = is                                -- (default for xy==XPG_CURRENT)
    if xy<XPG_CURRENT then
        res = xy
    elsif xy>XPG_CURRENT then
        switch xy && 0b0011 do
            case 0b11: res = os+floor((ol-il)/2)    -- centre
            case 0b10: res = os+ol-il               -- right/btm
            case 0b01: res = os                     -- left/top
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

local function gtk_menu_position_func(atom /*pMenu*/, pX, pY, pPushIn, xy)
--?{"gtk_menu_position_func","x",floor(xy/#8000),"y",and_bits(xy,#7FFF)}
    poke4(pX,floor(xy/#8000))
    poke4(pY,and_bits(xy,#7FFF))
    poke4(pPushIn,false)
    return 0 -- (ignored)
end function

local function any_active_window() -- WinAPI only
    for id,ct in ctrl_types do
        if ct=DIALOG
        and parent_ids[id]=0
        and and_bits(ctrl_flags[id],CF_MAPPED) then
            return ctrl_handles[id]
        end if
    end for
    crash("no active window!?")
end function

global procedure gPopupMenu(gdx menu, integer x=XPG_MOUSEPOS, y=XPG_MOUSEPOS)
    integer ct = ctrl_types[menu]
    assert(ct=MENU)
    bool bMapped = and_bits(ctrl_flags[menu],CF_MAPPED)!=0
    if not bMapped then gMap(menu) end if
    atom handle = ctrl_handles[menu]
    if x=XPG_MOUSEPOS
    or y=XPG_MOUSEPOS then
        assert(x==y,"gPopupMenu: XPG_MOUSEPOS is both-only")
        {x,y} = gGetGlobal("MOUSEPOS")
    end if
    if backend=GTK then
        if bGTK3 then
            c_proc(gtk_menu_popup_at_pointer,{handle,NULL})
        else
            atom cb = call_back(gtk_menu_position_func),
               data = x*#8000+y,
             cevntt = c_func(gtk_get_current_event_time,{})
            c_proc(gtk_menu_popup,{handle,NULL,NULL,cb,data,0,cevntt})
        end if
    elsif backend=WinAPI then
        atom hwnd = any_active_window()
        tracked_menu = menu
        integer res = c_func(xTrackPopupMenuEx,{handle,TPM_RETURNCMD,x,y,hwnd,NULL})
        tracked_menu = 0
        if res then
            {} = xpg_menu_common(menu,res,false)
        end if
    else
        ?9/0 -- (unknown backend)
    end if
end procedure

--DEV/SUG gSetPrimaryWindow(), or ",bool bPrimary=false", or rely on parent!=NULL, even if not shown...:

global procedure gShow(gdx id, integer x=XPG_CURRENT, y=XPG_CURRENT)
--global procedure gShow(gdx id, integer x=XPG_CURRENT, y=XPG_CURRENT, bool bModal=false)   --DEV
--  (The plan is bModal sets all visible dialogs as inactive, constructing a list to restore/undo that when id is closed...)
puts(1,"") -- DEV while xpg_lm_dump_ctrls() is creating a console...
    integer ct = ctrl_types[id],
           pid = parent_ids[id],
       {sx,sy} = {0,0},
       {sw,sh} = gGetGlobalIntInt("SCREENSIZE")
--  assert(ct=DIALOG,"gShow() parameter must be a DIALOG")
    if ct!=DIALOG then
        gSetAttribute(id,"VISIBLE",true)
        return
    end if
    -- catch the handle of the first window displayed (nb arwen saves first created)
    if PrimaryWindowID=UNDEFINED then
        if pid!=0 then
            xpg_lm_dump_ctrls()
            printf(1,"gShow(%d) where parent(%d) is %d\n",{id,id,pid})
            crash("first window displayed must be parent-less",nFrames:=2)
        end if
        PrimaryWindowID = id
    end if
    atom cfi = ctrl_flags[id]
    bool bMapped = and_bits(cfi,CF_MAPPED)!=0
    if not bMapped then gMap(id) end if
    integer {dw,dh} = xpg_lm_get_dialog_decoration_size(id)

--xpg_lm_dump_ctrls()
--?xpg_get_window_rect(id)
    atom handle = ctrl_handles[id]
--c_proc(xShowWindow,{handle,SW_SHOWNORMAL})
--return
    if and_bits(cfi,CF_NEVER_SHOWN) then
        nMapDepth += 1
        -- GTK3 refuses to disclose any size info until after being shown, so we
        -- //have// to show wrong size, then correct.. (what a bunch of cretins)
        -- (to be fair, GTK is so pig-awful slow a tiny flicker'l bother no-one)
        if backend=GTK and bGTK3 then
            c_proc(gtk_widget_show_all,{handle})
        end if
--xpg_lm_dump_ctrls()
--?{gGetAttribute(id,"SIZE"),"good"}
--if 0 then

        xpg_lm_set_element_sizes(id)
--xpg_lm_dump_ctrls()
--trace(1)
        xpg_lm_normalise_sizes(id)
        xpg_lm_accumulate_sizes(id)
--/*
        <li>xpg_lm_get_dialog_decoration_size()</li>
        <li>xpg_lm_set_element_sizes()</li>
        <li>xpg_lm_gather_normal_groups()</li>
        <li>xpg_lm_normalise_sizes()</li>
        <li>xpg_lm_accumulate_sizes()</li>
        <li>xpg_lm_disperse_user_sizes()</li>
        <li>xpg_lm_calculate_offsets()</li>
        <li>xpg_lm_apply_offsets()</li>
        <li>xpg_lm_apply_offsets()</li>

--*/
--DEV
--      integer w = max(ctrl_size[id][SZ_USER_W],ctrl_size[id][SZ_NATURAL_W]),
--              h = max(ctrl_size[id][SZ_USER_H],ctrl_size[id][SZ_NATURAL_H]),
        integer w = max(ctrl_size[id][SZ_USER_W],ctrl_size[id][SZ_NORMAL_W]),
                h = max(ctrl_size[id][SZ_USER_H],ctrl_size[id][SZ_NORMAL_H])
--?{dw,dh}
--      {dw,dh} = xpg_gtk_subtract_dialog_decorations(id,dw,dh)
--        {dw,dh} = xpg_lm_get_dialog_decoration_size(id)
--?{dw,dh,"post-sdd"}
--      {dw,dh} = xpg_lm_get_dialog_client_size(id,dw,dh)
?{"lmdus",w-dw,h-dh,w,h,dw,dh}
        xpg_lm_disperse_user_sizes(id,w-dw,h-dh)
--xpg_lm_dump_ctrls()
        xpg_lm_calculate_offsets(id)
--?{gGetAttribute(id,"SIZE"),"bad"}
--?"XPG_APPLY_OPFFSETS"
        xpg_lm_apply_offsets(id)
--end if
--void gtk_widget_map(GtkWidget* widget) -- not used in IUP except by IupScintilla
--void gtk_widget_realize(GtkWidget* widget) -- used heavily in IUP...
--DEV rescan for sizes... (or is that WM_SIZE/whatever, and can we ditch level?)
xpg_lm_dump_ctrls()
--?"calculate natural sizes..."
--      if backend=WinAPI then
--          xpg_WinAPI_resize(id)
--      end if
--?{gGetAttribute(id,"SIZE"),"bad"}
        
--  end if
--xpg_lm_dump_ctrls()
        if x=XPG_CURRENT then x=XPG_CENTER end if
        if y=XPG_CURRENT then y=XPG_CENTER end if
        ctrl_flags[id] -= CF_NEVER_SHOWN
        nMapDepth -= 1
    end if
--xpg_lm_dump_ctrls()
--  if not bMapped then
----        xpg_lm_normalise_sizes(id)
--      xpg_lm_accumulate_sizes(id)
----        xpg_lm_accumulate_shrinkage(id)
--  end if
--  xpg_lm_disperse_user_sizes(id,?,?)
--  xpg_lm_distribute_any_slack(id) --maybe...
--  xpg_lm_calculate_offsets(id)
--  xpg_lm_apply_offsets(id)
--  xpg_lm_apply_updates()
--DEV... (also WM_SIZE,...)
    bool bPx = x>=XPG_CURRENT and and_bits(x,XPG_PARENT),
         bPy = y>=XPG_CURRENT and and_bits(y,XPG_PARENT)
    if pid!=0 and (bPx or bPy) then
        integer {px,py,pw,ph} = xpg_get_window_rect(pid)
        if bPx then {sx,sw} = {px,pw} end if
        if bPy then {sy,sh} = {py,ph} end if
    end if  

    integer {wx,wy,ww,wh} = xpg_get_window_rect(id)
--  sequence gwr = xpg_get_window_rect(id)
--  integer {wx,wy,ww,wh} = gwr
    if x=XPG_MOUSEPOS
    or y=XPG_MOUSEPOS then
        assert(x==y,"gShow: XPG_MOUSEPOS is both-only")
        ?9/0 -- (placeholder)
--      {wx,wy} = gGetGlobalIntInt("MOUSEPOS") -- (nee CURSORPOS)
    end if
    wx = xpg_placement(x,sx,sw,wx,ww)
    wy = xpg_placement(y,sy,sh,wy,wh)
--?{"win/rect",{h,gwr},{x},wx,ww,{y},wy,wh}
--  xpg_move_window(id,wx,wy)
--DEV only used in gShow(), so inline there...
--local procedure xpg_move_window(gdx id, atom x, y)
--  xpg_handle handle = ctrl_handles[id]
    if backend=GTK then
--?{"gtk_window_move",id}
        c_proc(gtk_window_move,{handle,wx,wy})
--?{"<gtk_window_move",id}
    elsif backend=WinAPI then
--if handle then
        integer flags = SWP_NOZORDER+SWP_NOSIZE
--      integer flags = SWP_NOZORDER
        integer w = ctrl_size[id][SZ_W]+dw,
                h = ctrl_size[id][SZ_H]+dh
?{"gShow(initial position)",id,wx,wy,w,h}
--nMapDepth += 1
        bool ok = c_func(xSetWindowPos,{handle,NULL,wx,wy,w,h,flags})
--      bool ok = c_func(xSetWindowPos,{handle,NULL,wx,wy,w+16,h+29,flags})
--nMapDepth -= 1
?{"gShow:swp returned",ok}
        assert(ok)
--end if
    else
        ?9/0 -- (unknown backend)
    end if
--end procedure
--?"moved"
--DEV setAttribute(h,"VISIBLE",true)...
    if backend=GTK then
--?{gGetAttribute(id,"SIZE"),"good"}
        if c_func(gtk_window_get_transient_for,{handle})=NULL then
--          if XPG_PARENTDIALOG!=NULL
--          and XPG_PARENTDIALOG!=h then
--              c_proc(gtk_window_set_transient_for,{h,XPG_PARENTDIALOG}) 
--          else
                xpg_signal_connect(handle,"destroy",xpg_gtk_quit,id)
--          end if
        end if
--      c_proc(gtk_widget_show,{id})
--?{gGetAttribute(id,"SIZE"),"good"}
        c_proc(gtk_widget_show_all,{handle})
--?{gGetAttribute(id,"SIZE"),"bad"}
--if bGTK3 then
----    c_proc(gtk_widget_get_preferred_size,{Xbutton,NULL,pGtkRequisition})
----    c_proc(gtk_widget_get_preferred_size,{button,pGtkRequisition,pGtkRequisition})
--  c_proc(gtk_widget_size_request,{Xbutton,pGtkRequisition})
--  ?{get_struct_field(idGtkRequisition,pGtkRequisition,"width"),
--    get_struct_field(idGtkRequisition,pGtkRequisition,"height")}
----    c_proc(gtk_widget_get_allocation,{Xbutton,pRECT})
----    ?{get_struct_field(idGdkRectangle,pRECT,"width"),
----      get_struct_field(idGdkRectangle,pRECT,"height")}
----    c_proc(gtk_widget_size_request,{button,pGtkRequisition})
----    c_proc(gtk_widget_size_request,{Xbutton,pRECT})
----    ?{get_struct_field(idGdkRectangle,pRECT,"x"),
----      get_struct_field(idGdkRectangle,pRECT,"y"),
----      get_struct_field(idGdkRectangle,pRECT,"width"),
----      get_struct_field(idGdkRectangle,pRECT,"height")}
--
--end if
    elsif backend=WinAPI then
--?{gGetAttribute(id,"SIZE"),"bad"}
        c_proc(xShowWindow,{handle,SW_SHOWNORMAL})
        c_proc(xUpdateWindow,{handle})
    else
        ?9/0 -- (unknown backend)
    end if
if test_elem then
    ?{test_elem,ctrl_names[ctrl_types[test_elem]],gGetAttribute(test_elem,"SIZE")}
end if

xpg_lm_dump_ctrls()
?xpg_get_window_rect(id)
end procedure

--/*
void iupdrvBaseLayoutUpdateMethod(Ihandle *ih)
{
  if (ih->currentwidth > 0 && ih->currentheight > 0)
    SetWindowPos(ih->handle,NULL,ih->x,ih->y,ih->currentwidth,ih->currentheight,
                 SWP_NOZORDER | SWP_NOACTIVATE | SWP_NOOWNERZORDER);
  else /* move only */
    SetWindowPos(ih->handle,NULL,ih->x,ih->y,0,0,
                 SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE | SWP_NOOWNERZORDER);
}
        integer flags = SWP_NOZORDER+SWP_NOSIZE
        bool ok = c_func(xSetWindowPos,{handle,NULL,x,y,0,0,flags})
        assert(ok)



void iupdrvRedrawNow(Ihandle *ih)
{
  /* REDRAW Now - IupRedraw */
  RedrawWindow(ih->handle,NULL,NULL,RDW_ERASE|RDW_INVALIDATE|RDW_INTERNALPAINT|RDW_UPDATENOW);
}

void iupdrvPostRedraw(Ihandle *ih)
{
  /* Post a REDRAW - IupUpdate */
  /* can NOT use RDW_NOCHILDREN because IupList has internal children that needs to be redraw */
  RedrawWindow(ih->handle,NULL,NULL,RDW_ERASE|RDW_INVALIDATE|RDW_INTERNALPAINT);  
}
--*/

--DEV/SUG should we just invoke this from gShow()????
global procedure gRedraw(gdx id, integer flags=0b111)
--DEV? (docs say...)
    assert(and_bits(ctrl_flags[id],CF_MAPPED)!=0)
    bool bNow = and_bits(flags,0b001)   --DEV made up... see docs
    atom handle = ctrl_handles[id]
    if backend=GTK then
        atom window = c_func(gtk_widget_get_window,{handle})
        if window then
            c_proc(gdk_window_invalidate_rect,{window,NULL,true})
        end if
        c_proc(gtk_widget_queue_draw,{handle})
        if window and bNow then
            c_proc(gdk_window_process_updates,{window,true})
        end if
    elsif backend=WinAPI then
        atom dwFlags = or_all({RDW_ERASE,RDW_INVALIDATE,RDW_INTERNALPAINT})
        if bNow then dwFlags = or_bits(dwFlags,RDW_UPDATENOW) end if
        bool bOK = c_func(xRedrawWindow,{handle,NULL,NULL,dwFlags})
        assert(bOK)
    else
        ?9/0 -- (unknown backend)
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

global procedure gMainLoop()
    if backend=GTK then
--/!*
--      if gtk_css_provider_new!=-1 then    -- (available on 3.0+ only)
        if bGTK3 then
            atom screen = c_func(gdk_screen_get_default,{}),
                 provider = c_func(gtk_css_provider_new,{})
            c_proc(gtk_css_provider_load_from_data,{provider,gtk_css_text,-1,NULL})
            c_proc(gtk_style_context_add_provider_for_screen,{screen,provider,GTK_STYLE_PROVIDER_PRIORITY_APPLICATION})
--Squared Plus  &#8862; &#x229E;
--Squared Minus   &#8863; &#x229F;
        end if
--*!/
        c_proc(gtk_main)
    elsif backend=WinAPI then
        while c_func(xGetMessage,{pMSG,NULL,0,0}) do
--DEV
--          if not translateAccelerator() then
            c_proc(xTranslateMessage,{pMSG})
            c_proc(xDispatchMessage,{pMSG})
        end while
    else
        ?9/0 -- (unknown backend)
    end if
end procedure

last_xpgui_rid = gMainLoop -- (or whatever ends up last in this file)

--/*
--DEV prolly better to set up an idle doobrie...
global procedure gLoopStep()
    if backend=GTK then
--      if (gtk_main_iteration_do(FALSE)) return IUP_CLOSE;
--      return IUP_DEFAULT;
    elsif backend=WinAPI then
--      MSG msg;
--      if (PeekMessage(&msg,0,0,0,PM_REMOVE)) return winLoopProcessMessage(&msg);
--static int winLoopProcessMessage(MSG* msg)
--{
--  if (msg->message == WM_QUIT)    /* IUP_CLOSE returned in a callback or IupHide in a popup dialog or all dialogs closed */
--  return IUP_CLOSE;
--  else
--  {
--  TranslateMessage(msg);
--  DispatchMessage(msg);
--  return IUP_DEFAULT;
--  }
--}
--      else if (win_idle_cb) return winLoopCallIdle();
--      return IUP_DEFAULT;
    else
        ?9/0 -- (unknown backend)
    end if
end procedure
--*/

global function gQuit(gdx /*id*/)
    -- standard "Close"/"Quit" button shorthand
    -- (nb > last_xpgui_rid to pass rtn() checks)
?"gQuit"
    return XPG_CLOSE
end function

--/*
--one off the mailing list: (we definately need a natural size (and always have), at least for win32)
  Ihandle *dlg, *frame, *button;
   
  IupOpen(NULL,NULL);
   
  button = IupButton("Button",NULL);
   
  frame = IupFrame(button);
  IupSetAttribute(frame,"TITLE","A title wider than the button");
 
  dlg = IupDialog(frame);
  IupSetAttribute(dlg,"TITLE","Test bug length of frame when title is set");
  IupSetAttribute(dlg,"SIZE","200x80");

  IupShowXY(dlg,IUP_CENTER,IUP_CENTER);
  IupMainLoop();
  IupClose();
-- one reply (which I think rather misses the point!), use a v/hbox: 
  Ihandle *dlg, *frame, *button;
  Ihandle *hbox;
  Ihandle *vbox;

  IupOpen(NULL,NULL);

  button = IupButton("Button",NULL);

  hbox = IupHbox(button,NULL);
  IupSetAttribute(hbox,"SIZE","200x80");

  frame = IupFrame(hbox);
  IupSetAttribute(frame,"TITLE","A title wider than the button");

  vbox = IupVbox(frame,NULL);
  dlg = IupDialog(vbox);
  IupSetAttribute(dlg,"TITLE","Test bug length of frame when title is set");
  IupSetAttribute(dlg,"SIZE","200x80");

  IupShowXY(dlg,IUP_CENTER,IUP_CENTER);
  IupMainLoop();
  IupClose();
--*/

--/*
--constant GTK_STATE_FLAG_NORMAL = 0,
--       GTK_STATE_FLAG_SELECTED = 4

int ChoosePixelFormat(
  HDC                         hdc,
  const PIXELFORMATDESCRIPTOR *ppfd
);

typedef struct tagPIXELFORMATDESCRIPTOR {
  WORD  nSize;
  WORD  nVersion;
  DWORD dwFlags;
  BYTE  iPixelType;
  BYTE  cColorBits;
  BYTE  cRedBits;
  BYTE  cRedShift;
  BYTE  cGreenBits;
  BYTE  cGreenShift;
  BYTE  cBlueBits;
  BYTE  cBlueShift;
  BYTE  cAlphaBits;
  BYTE  cAlphaShift;
  BYTE  cAccumBits;
  BYTE  cAccumRedBits;
  BYTE  cAccumGreenBits;
  BYTE  cAccumBlueBits;
  BYTE  cAccumAlphaBits;
  BYTE  cDepthBits;
  BYTE  cStencilBits;
  BYTE  cAuxBuffers;
  BYTE  iLayerType;
  BYTE  bReserved;
  DWORD dwLayerMask;
  DWORD dwVisibleMask;
  DWORD dwDamageMask;
} PIXELFORMATDESCRIPTOR, *PPIXELFORMATDESCRIPTOR, *LPPIXELFORMATDESCRIPTOR;

PFD_DRAW_TO_WINDOW
0x00000004
The buffer can draw to a window or device surface.
PFD_DRAW_TO_BITMAP
0x00000008
The buffer can draw to a memory bitmap.
PFD_SUPPORT_GDI
0x00000010
The buffer supports GDI drawing. This flag and PFD_DOUBLEBUFFER are mutually exclusive in the current generic implementation.
PFD_SUPPORT_OPENGL
0x00000020
The buffer supports OpenGL drawing.
PFD_GENERIC_ACCELERATED
0x00001000
The pixel format is supported by a device driver that accelerates the generic implementation. 
If this flag is clear and the PFD_GENERIC_FORMAT flag is set, the pixel format is supported by the generic implementation only.
PFD_GENERIC_FORMAT
0x00000040
The pixel format is supported by the GDI software implementation, which is also known as the generic implementation. 
If this bit is clear, the pixel format is supported by a device driver or hardware.
PFD_NEED_PALETTE
0x00000080
The buffer uses RGBA pixels on a palette-managed device. A logical palette is required to achieve the best results for this pixel type. 
Colors in the palette should be specified according to the values of the cRedBits, cRedShift, cGreenBits, cGreenShift, cBluebits, and cBlueShift members. 
The palette should be created and realized in the device context before calling wglMakeCurrent.
PFD_NEED_SYSTEM_PALETTE
0x00000100
Defined in the pixel format descriptors of hardware that supports one hardware palette in 256-color mode only. 
For such systems to use hardware acceleration, the hardware palette must be in a fixed order (for example, 3-3-2) when in RGBA mode or 
must match the logical palette when in color-index mode.
When this flag is set, you must call SetSystemPaletteUse in your program to force a one-to-one mapping of the logical palette and the system palette. 
If your OpenGL hardware supports multiple hardware palettes and the device driver can allocate spare hardware palettes for OpenGL, this flag is typically clear.
This flag is not set in the generic pixel formats.

PFD_DOUBLEBUFFER
0x00000001
The buffer is double-buffered. This flag and PFD_SUPPORT_GDI are mutually exclusive in the current generic implementation.
PFD_STEREO
0x00000002
The buffer is stereoscopic. This flag is not supported in the current generic implementation.
PFD_SWAP_LAYER_BUFFERS
0x00000800
Indicates whether a device can swap individual layer planes with pixel formats that include double-buffered overlay or underlay planes. 
Otherwise all layer planes are swapped together as a group. When this flag is set, wglSwapLayerBuffers is supported.

PFD_DEPTH_DONTCARE
0x20000000
The requested pixel format can either have or not have a depth buffer. 
To select a pixel format without a depth buffer, you must specify this flag. 
The requested pixel format can be with or without a depth buffer. 
Otherwise, only pixel formats with a depth buffer are considered.
PFD_DOUBLEBUFFER_DONTCARE
0x40000000
The requested pixel format can be either single- or double-buffered.
PFD_STEREO_DONTCARE
0x80000000
The requested pixel format can be either monoscopic or stereoscopic.

PFD_SWAP_COPY
0x00000400
Specifies the content of the back buffer in the double-buffered main color plane following a buffer swap. 
Swapping the color buffers causes the content of the back buffer to be copied to the front buffer. 
The content of the back buffer is not affected by the swap. 
PFD_SWAP_COPY is a hint only and might not be provided by a driver.
PFD_SWAP_EXCHANGE
0x00000200
Specifies the content of the back buffer in the double-buffered main color plane following a buffer swap. 
Swapping the color buffers causes the exchange of the back buffer's content with the front buffer's content. 
Following the swap, the back buffer's content contains the front buffer's content before the swap. 
PFD_SWAP_EXCHANGE is a hint only and might not be provided by a driver.

iPixelType

Specifies the type of pixel data. The following types are defined.

Value   Meaning
PFD_TYPE_RGBA
0
RGBA pixels. Each pixel has four components in this order: red, green, blue, and alpha.
PFD_TYPE_COLORINDEX
1
Color-index pixels. Each pixel uses a color-index value.
--*/

--/* For a gSplit doobrie:
static gboolean on_crossing(GtkWidget *darea, GdkEventCrossing *event) {
    GdkDisplay *display = gtk_widget_get_display(darea);
    GdkCursor *cursor = gdk_cursor_new_from_name(display,"col-resize" or "row-resize");
    // Assign the cursor to the window
    gdk_window_set_cursor(gtk_widget_get_window(darea),cursor);

--*/


