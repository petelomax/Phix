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
-- (behave: it's just mov reg,[proc_addr]; call reg; rather than call proc... flw)
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
--     plus there are no prebuilt binaries(/support) of IUP for an ARM machine.
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

-- Test E:\downloads\misc\FASM\guicpuid.asm on a 64-bit lnx box... 
-- ... check cpu use, and maybe update the rc/Windows/X11 task.
-- ... and get filedump to start code at offset 490
-- ==> having xpGUI target X11 directly might become a possibility...
-- maybe: https://board.flatassembler.net/topic.php?t=21503 (32/64 bit xlib)


without debug
include pprntfN.e
include cffi.e
include pcfunc.e
include scanf.e
include ubits.e
with debug -- (leaving ths commented out often makes asserts etc more helpful)

--with trace

local bool bInit = false -- (xpg_Init() not yet called)
local constant UNDEFINED = 0 -- (==NULL)
global constant XPG_GTK = 1, XPG_WINAPI = 2, XPG_JS = 3
               
local integer PrimaryWindowID = UNDEFINED,  -- id of the main application window
              handlers,                     -- key is {gdx,name}, data is integer routine_id
              handler_sigs,                 -- key is {ct,name}, data is allowed sig[1..3]
              backend = find(platform(),{LINUX,WINDOWS}) -- (must match the constants!)
--            backend = iff(platform()= LINUX  ? XPG_GTK
--                     :iff(platform()=WINDOWS ? XPG_WINAPI
--                     :9/0))
global procedure gUseGTK()
    assert(not bInit)
    backend = XPG_GTK
end procedure

-- Aside: This source is littered with
--
--      if backend=XPG_GTK then
--          ...
--      elsif backend=XPG_WINAPI then
--          ...
--      else
--          ?9/0 -- (unknown backend)
--      end if
--
-- While only a few of those ?9/0 can actually trigger now, mid-way through adding say Qt
--  they should theoretically be helpful to locate "what next to get next demo working".
--


local constant control_set = {{    DIALOG:=$,"Dialog"   },
                              {       BOX:=$,"Box"      },
                              {    BUTTON:=$,"Button"   },
                              {    CANVAS:=$,"Canvas"   }, -- (also gGraph/gList/gTable/gSplit[2])
                              {  CHECKBOX:=$,"Checkbox" },
                              { CLIPBOARD:=$,"Clipboard"},
                              {  DATEPICK:=$,"DatePick" },
                              {  DROPDOWN:=$,"DropDown" },
                              {     FRAME:=$,"Frame"    },
                              {     GRAPH:=$,"Graph"    }, -- (in [CX_CANVAS_TYPE] only)
                              {     LABEL:=$,"Label"    },
                              {      LIST:=$,"list"     }, -- (in [CX_CANVAS_TYPE] only)
                              {      MENU:=$,"Menu"     },
                              {  PROGRESS:=$,"Progress" },
                              {    SLIDER:=$,"Slider"   },
                              {      SPIN:=$,"Spin"     },
                              {      TABS:=$,"Tabs"     },
                              {     TABLE:=$,"Table"    }, -- (in [CX_CANVAS_TYPE] only)
                              {      TEXT:=$,"Text"     },
                              {     TIMER:=$,"Timer"    },
                              {  TREEVIEW:=$,"TreeView" }},
                       lcs = length(control_set),
                ctrl_names = vslice(control_set,2), -- (for error messages only)
                  cf_glags = {{     CF_EXPANDB:=0x00000001, "CF_EXPANDB"      },    -- both
                              {     CF_EXPANDH:=0x00000002, "CF_EXPANDH"      },    -- horizontal only
                              {     CF_EXPANDV:=0x00000004, "CF_EXPANDV"      },    -- vertical only
                              {   CF_CONTAINER:=0x00000100, "CF_CONTAINER"    },    -- (gBox/Dialog/Frame/Tabs only)
                              {   CF_DECORATED:=0x00000200, "CF_DECORATED"    },    -- (only Dialog/Frame/Tabs)
                              {    CF_VERTICAL:=0x00000400, "CF_VERTICAL"     },    -- (only meaningful on some controls*)
                              {      CF_RESIZE:=0x00000800, "CF_RESIZE"       },    -- (only meaningful on gDialog ctrls)
--DEV erm, NO: just put expand=NO on the container??!!
--                            {        CF_FREE:=0x00001000, "CF_FREE"         },
                              {      CF_SHRINK:=0x00002000, "CF_SHRINK"       },
                              {       CF_SPLIT:=0x00004000, "CF_SPLIT"        },    -- (the gH/Vbox|canvas of a gSplit control)
                              {      CF_NORMAL:=0x00010000, "CF_NORMAL"       },    -- ctrl is part of a normaliser group
                              {       CF_RADIO:=0x00020000, "CF_RADIO"        },    -- ctrl is part of a radio group
--                            {       CF_ACCEL:=0x00040000, "CF_ACCEL"        },    -- ctrl has an accelerator key (??) DEV/SUG
                              {    CF_INACTIVE:=0x00100000, "CF_INACTIVE"     },
                              { CF_NEVER_SHOWN:=0x00200000, "CF_NEVER_SHOWN"  },    -- (only meaningful on dialogs)
                              {CF_CLOSE_ON_ESC:=0x00400000, "CF_CLOSE_ON_ESC" },    -- (only meaningful on dialogs)
                              {  CF_IGNORESIZE:=0x00800000, "CF_IGNORESIZE"   },    -- (ignore [recursive] resize events)
                              {      CF_MAPPED:=0x01000000, "CF_MAPPED"       },    -- (re-used for "Active" on timers)
                              {   CF_UNMAPATTR:=0x02000000, "CF_UNMAPATTR"    }},   -- (these can get attributes unmapped)
                 CF_EXPAND = CF_EXPANDV+CF_EXPANDH+CF_EXPANDB,                      -- 0b111 mask, with 0/1/2/4 for N/B/H/V
                CF_HEXPAND = CF_EXPANDH+CF_EXPANDB,                                 -- 0b011 mask (expands horizontally)
                CF_VEXPAND = CF_EXPANDV+CF_EXPANDB                                  -- 0b101 mask (expands vertically)
                             -- aside: I used three bits for expansion even though 2 would suffice,
                             --        mainly so that true===both, and hence "both" cannot be 0b11,
                             --        and h-/v-only as 0b10 and 0b11 would be begging for trouble.

--*CF_VERTICAL distinguishes a gVbox from a gHbox, and is ORIENTATION=VERTICAL for gProgressBar and gSlider

local function cf_flags_x14(atom cflags)
--
--  returns a string of the form XCDVRSPNRINEGMU
--                               123456789012345
--
--  string res = repeat(' ',15)
    string res = repeat(' ',5)
    res[1] = " BH?V"[and_bits(cflags,#F)+1]
    res[2] = " C"[and_bits(floor(cflags/CF_CONTAINER),1)+1]
    res[3] = " D"[and_bits(floor(cflags/CF_DECORATED),1)+1]
    res[4] = " V"[and_bits(floor(cflags/CF_VERTICAL),1)+1]
    res[5] = " R"[and_bits(floor(cflags/CF_RESIZE),1)+1]
--/*
    res[6] = " S"[and_bits(floor(cflags/CF_SHRINK),1)+1]
    res[7] = " P"[and_bits(floor(cflags/CF_SPLIT),1)+1]
    res[8] = " N"[and_bits(floor(cflags/CF_NORMAL),1)+1]
    res[9] = " R"[and_bits(floor(cflags/CF_RADIO),1)+1]
    res[10] = " I"[and_bits(floor(cflags/CF_INACTIVE),1)+1]
    res[11] = " N"[and_bits(floor(cflags/CF_NEVER_SHOWN),1)+1]
    res[12] = " E"[and_bits(floor(cflags/CF_CLOSE_ON_ESC),1)+1]
    res[13] = " G"[and_bits(floor(cflags/CF_IGNORESIZE),1)+1]
    res[14] = " M"[and_bits(floor(cflags/CF_MAPPED),1)+1]
    res[15] = " U"[and_bits(floor(cflags/CF_UNMAPATTR),1)+1]
--*/
    return res
end function

--indexed by [DIALOG..TEEVIEW][map/set/get],
--the routine info (2nd column) is for diagnostics only:
local sequence ctrl_msg = repeat(0,lcs)
local enum CM_MAP, CM_MRI,              -- map/create
           CM_SET, CM_SRI,              -- set attributes
           CM_GET, CM_GRI,              -- get attributes   
           CM_LEN=$

local procedure xpg_set_ctrl_msg(integer ct, rmap, rset, rget)
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
                  ctrl_font = {},   -- (UNDEFINED or index to fontcache)
                 ctrl_fonts = {},   -- (NULL or string)
--              ctrl_styles = {},   -- XPG_NORMAL/(NULL or string)
                 ctrl_fontd = {},   -- (NULL?, pango fontdesc, WinAPI hFont atom)
                    ctrl_bg = {},   -- (-1 or background colour [text only for now])
                  ctrl_xtra = {},   -- (control-specific, see CX_XXX below)
                  user_data = {},
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
local enum SZ_W,            -- actual/final
           SZ_H,
           SZ_NORMAL_W,     -- layout manager works from these
           SZ_NORMAL_H,
           SZ_NATURAL_W,    -- initial
           SZ_NATURAL_H,
           SZ_USER_W,       -- explicit (initial) overrides
           SZ_USER_H,
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
-- margins/padding (trbl), erm...
--         SZ_MARGIN_TOP,
--         SZ_MARGIN_RGT,
--         SZ_MARGIN_BTM,
--         SZ_MARGIN_LFT,
           SZ_MARGIN,   -- (see MP_XXX below)
           SZ_PADDING,  -- "", BUTTON/DATEPICK/DROPDOWN/TEXT only
--DEV x,y (or just calculate as we go, why not...)
           SZ_X,
           SZ_Y,
           SZ_LENGTH = $

local enum CX_LAST_FOCUS,
--abandoned (for now)
--         CX_DEF_ENTER,
--         CX_DEF_ESCAPE,
           CX_DLG_LEN = $

local enum MP_TOP,
           MP_RGT,
           MP_BTM,
           MP_LFT

-- ctrl_xtra for gCanvas()
local enum --CX_BOX_MARGIN, -- trbl
           CX_BOX_GAP,
           CX_BOX_SPACE_H,
           CX_BOX_SPACE_V,
           CX_BOX_LEN = $
--local enum CX_BM_TOP,
--         CX_BM_RIGHT,
--         CX_BM_BOTTOM,
--         CX_BM_LEFT

local constant CXCF_REPEN = 0b01,
--             CXCF_REFONT = 0b10 -- (Windows only)
               CXCF_SCROLL = 0b100 -- (===[CX_SBINFO]!=NULL, w/o p2js violation)
local enum CX_CANVAS_TYPE,      -- CANVAS/GRAPH/LIST/TABLE...
           CX_CANVAS_FLAGS,     -- CXCF_REPEN (etc)
           CX_CANVAS_BACK,      -- eg XPG_PARCHMENT (default XPG_WHITE)
           CX_CANVAS_FORE,      -- default XPG_BLACK
           CX_CANVAS_HDC,       -- XPG_WINAPI:hDC, XPG_GTK:{cairo,layout}
           CX_PENSTYLE,         -- eg/default XPG_CONTINUOUS
           CX_PENWIDTH,         -- default 1
           CX_TXTANGLE,         -- default 0 (degrees, not radians)
           CX_GTL_ATTRS,        -- gGraph/gTable/gList, see GX/LX/TX below
           CX_SBINFO,           -- scrollbar info, NULL or see SB_XXX below 
           CX_CANVAS_LEN = $    -- (init ctrl_xtra[id] this size)

-- content of ctrl_xtra[canvas][CX_SBINFO], maybe others
local enum SB_HVISB, -- horizontal scrollbar visible
           SB_VVISB, -- vertical scrollbar visible
           SB_VTTOP, -- vertical thumb top
           SB_VTEND, -- vertical thumb end
           SB_HTLFT, -- horizontal thumb left
           SB_HTEND, -- hirizontal thumb end
           SB_TIMER, -- timer for click-holds
           SB_CLIKX, -- where clickdown
           SB_CLIKY, --     """
           SB_DRAGG, -- true if dragging
           SB_MONLD, -- (mouse on scrollbar as last drawn, UA/AT/VT/BT/DA
                     --    (with `` meaning on nowt)       LA/LT/HT/RT/RA)
           SB_CKMON, -- "" at point of click
-- (above are probably all best kept private)
           SB_VWIDE, -- vertical scrollbar width (=17)
           SB_HHIGH, -- horizontal scrollbar width (=17)
--DEV SCROLLSIZE ("" as optional 3/4th args?)
           SB_SCRLW, -- SCROLLSIZE
           SB_SCRLH, --  "", next two are the VIEWPORT
--DEV VIEWPORT:
           SB_ORIGX,    -- 0..SCROLLW-(w-VSBVIS*SBWID) [nb client offset, not scrollbar]
           SB_ORIGY,    -- 0..SCROLLH-(h-HSBVIS*SBHGH)              """
           SB_INFOLEN = $

-- ctrl_xtra[CX_GTL_ATTRS] for for gGraph():
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

-- ctrl_xtra[CX_GTL_ATTRS] for gTable():
-- (first 5 & last 3 are creation-only or internal and cannot be set via gSetAttribute)
local constant table_attrs = {{   TX_COLUMNS:=$,'X',"?COLUMNS?"     },
                              {  TX_LINESTEP:=$,'I',"LINESTEP"      },
--DEV might yet make this get_data-style...
                              {      TX_DATA:=$,'X',"?DATA?"        },
                              {   TX_ACTCOLS:=$,'X',"?ACTCOLS?"     },
                              {    TX_TAGSET:=$,'X',"?TAGSET?"      },
                              {  TX_SORTCOLS:=$,'X',"?SORTCOLS?"    },
--NO: scrollbar wants to ovelap titles, [x]pGUI.js arguably has it wrong.
--X                           {   TX_TCANVAS:=$,'X',"?TCANVAS?"     },
--X                           {   TX_BCANVAS:=$,'X',"?BCANVAS?"     },
--                            {       TX_XXX:=$,'?',"XXX"           },
                              {      TX_ROWS:=$,'X',"?ROWS?"        }}, -- (visible lines)
--DEV why not just use SZ_NATURAL_W?
--                            {  TX_NATHIGHT:=$,'X',"?NAT_HIGHT?"   },
--                            {  TX_NATWIDTH:=$,'X',"?NAT_WIDTH?"   }},
                    TX_LEN = length(table_attrs),
              t_attr_types = vslice(table_attrs,2),
              t_attr_names = vslice(table_attrs,3)

-- ctrl_xtra[CX_GTL_ATTRS] for gList():
local constant list_attrs = {{      LX_DATA:=$,'I',"DATA"       },
                             {  LX_LINESTEP:=$,'I',"LINESTEP"   },
--                           {LX_AUTOSCROLL:=$,'B',"AUTOSCROLL" },
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
--if LX_AUTOSCROLL then end if -- DEV currently unused...

local constant gtl_sets = {{GRAPH,grattr_names,grattr_types},
                           { LIST,l_attr_names,l_attr_types},
                           {TABLE,t_attr_names,t_attr_types}},
              {gtl_types,
               gtl_names,
               gtl_sigs} = columnize(gtl_sets)

-- content of ctrl_xtra[gSplit(canvas)][CX_GTL_ATTRS]
local enum PX_DRAG,     -- Dragging (mouse button is down)
           PX_CLICK,    -- where clicked (x **OR** y only)
           PX_FRAC,     -- -1 not in use, else 0.0..1.0
--SUG:
--         PX_CURSOR, -- cursor (over inactive)
           PX_LEN = $

-- ctrl_xtra for gTabs():
local enum CX_TABTITLES,
           CX_TABIMAGES,
           CX_TABWIDTH,
           CX_TABHIGHT,
           CX_TABLENGTH = $ -- (init ctrl_xtra[id] this size)

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

--global type gdc(object h)
--  -- a single gdx from gCanvas or a single gImage.
----    if atom(h) then return h!=NULL and gdx(h) and ctrl_types[h]=CANVAS and ctrl_xtra[h][CX_GTL_ATTRS]=0 end if
--  if atom(h) then return h!=NULL and gdx(h) and ctrl_types[h]=CANVAS end if
----    return sequence(h) and length(h)=2 and h[1]="gImage" and atom(h[2])
--  return sequence(h) and length(h)=3 and h[1]="gImage"
--end type

local integer last_xpgui_rid = gdx -- (overwritten with gMainLoop[or whatever] later on in this file)
sequence internal_rtns = {}

--DEV better off saying >rtn and get_routine_info() says ok??
global type rtn(object rid)
    return integer(rid) and (rid=NULL or rid>last_xpgui_rid or find(rid,internal_rtns))
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
        ctrl_font[id] = UNDEFINED
        ctrl_fonts[id] = UNDEFINED
        ctrl_fontd[id] = UNDEFINED
        ctrl_bg[id] = -1
        ctrl_xtra[id] = UNDEFINED
        user_data[id] = UNDEFINED
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
        ctrl_font &= UNDEFINED
        ctrl_fonts &= UNDEFINED
        ctrl_fontd &= UNDEFINED
        ctrl_bg &= -1
        ctrl_xtra &= UNDEFINED
        user_data &= UNDEFINED
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

--local function xpg_peek_double(object pDouble)
local function xpg_peek_double(atom pDouble)
--X pDouble is either atom or {atom,n}, the former being equivalent to {atom,1}.
--  if atom(pDouble) then
        return float64_to_atom(peek({pDouble,8}))
--  else
--      sequence doubles = {}
--      for i=1 to pDouble[2] do
--          doubles &= float64_to_atom(peek({pDouble[1]+8*(i-1),8}))
--      end for
--      return doubles
--  end if
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
--/*
-- DEV I did this...
                               {"VK_DEL",       VK_DEL      := #1F,     #2E, #FFFF},
                               {"VK_NUMLOCK",   VK_NUMLOCK  := #90,     #90, #FF7F},
                               {"VK_DOWN",      VK_DOWN     := #A2,     #28, #FF54},
                               {"VK_POUND",     VK_POUND    := #E3,     #A3, #00A3},
                               {"VK_LEFT",      VK_LEFT     := #A4,     #25, #FF51},
                               {"VK_RIGHT",     VK_RIGHT    := #A6,     #27, #FF53},
                               {"VK_UP",        VK_UP       := #A8,     #26, #FF52},
                               {"VK_APPS",      VK_APPS     := #E0,     #5D, #FF67},
                               {"VK_INS",       VK_INS      := #E1,     #2D, #FF63},
                               {"VK_SCROLL",    VK_SCROLL   := #E2,     #91, #FF14},
                               {"VK_PGUP",      VK_PGUP     := #E4,     #21, #FF55},
                               {"VK_PGDN",      VK_PGDN     := #E5,     #22, #FF56},
                               {"VK_END",       VK_END      := #E6,     #23, #FF57},
                               {"VK_HOME",      VK_HOME     := #E7,     #24, #FF50},
-- ... but then found this (and bottled it!)
     bGraphic = (wParam>=#30 and wParam<=#5A)   -- 0..9 & A..Z
             or (wParam>=#60 and wParam<=#6F)   -- Numpad
             or (wParam>=#BA and wParam<=#E2)   -- OEM
--*/
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
                               {"4",            '4',                    #00, #FFB4}, -- (Numpad 4, GTK only)
                               {"5",            '5',                    #00, #FFB5}, -- (Numpad 5, GTK only)
                               {"6",            '6',                    #00, #FFB6}, -- (Numpad 6, GTK only)
                               {"7",            '7',                    #00, #FFB7}, -- (Numpad 7, GTK only)
                               {"8",            '8',                    #00, #FFB8}, -- (Numpad 8, GTK only)
                               {"9",            '9',                    #00, #FFB9}, -- (Numpad 9, GTK only)
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
--              key_values = vslice(key_mappings,2),
--              key_names = vslice(key_mappings,1)

local constant known_colours = {{"XPG_BLACK",           XPG_BLACK           := #00000000},
                                {"XPG_NAVY",            XPG_NAVY            := #00000080},
                                {"XPG_BLUE",            XPG_BLUE            := #000000FF},
                                {"XPG_LIGHT_BLUE",      XPG_LIGHT_BLUE      := #004363D8},
                                {"XPG_TEAL",            XPG_TEAL            := #00008080},
                                {"XPG_DARK_CYAN",       XPG_DARK_CYAN       := #0000C0C0},
                                {"XPG_CYAN",            XPG_CYAN            := #0000FFFF},
                                {"XPG_DARK_GREEN",      XPG_DARK_GREEN      := #00008000},
                                {"XPG_GREEN",           XPG_GREEN           := #003CB44B},
                                {"XPG_LIGHT_GREEN",     XPG_LIGHT_GREEN     := #0000FF00},
                                {"XPG_OLIVE",           XPG_OLIVE           := #00808000},
                                {"XPG_ORANGE",          XPG_ORANGE          := #00FF8C00},
                                {"XPG_AMBER",           XPG_AMBER           := #00FFBF00},
                                {"XPG_DARK_YELLOW",     XPG_DARK_YELLOW     := #00EBEB00},
                                {"XPG_YELLOW",          XPG_YELLOW          := #00FFFF00},
                                {"XPG_INDIGO",          XPG_INDIGO          := #004B0082},
                                {"XPG_PURPLE",          XPG_PURPLE          := #00911EB4},
                                {"XPG_DARK_PURPLE",     XPG_DARK_PURPLE     := #00800080},
                                {"XPG_MAGENTA",         XPG_MAGENTA         := #00FF00FF},
                                {"XPG_DARK_VIOLET",     XPG_DARK_VIOLET     := #00F032E6},
                                {"XPG_VIOLET",          XPG_VIOLET          := #00EE82EE},
                                {"XPG_DARK_RED",        XPG_DARK_RED        := #00800000},
                                {"XPG_RED",             XPG_RED             := #00FF0000},
                                {"XPG_SLATE",           XPG_SLATE           := #00404040},
                                {"XPG_DARK_GREY",       XPG_DARK_GREY       := #00808080},  
                                {"XPG_GREY",            XPG_GREY            := #00C0C0C0},  
                                {"XPG_LIGHT_GREY",      XPG_LIGHT_GREY      := #00E4E4E4},
                                {"XPG_PARCHMENT",       XPG_PARCHMENT       := #00FFFFE0},
                                {"XPG_LIGHT_PARCHMENT", XPG_LIGHT_PARCHMENT := #00FAF8EF},
                                {"XPG_WHITE",           XPG_WHITE           := #00FFFFFF}},
               known_colour_names = vslice(known_colours,1),
               known_colour_values = vslice(known_colours,2)

local constant line_styles = {{"XPG_CONTINUOUS",    XPG_CONTINUOUS   := 1},
                              {"XPG_DASHED",        XPG_DASHED       := 2},
                              {"XPG_DOTTED",        XPG_DOTTED       := 3},
                              {"XPG_DASH_DOT",      XPG_DASH_DOT     := 4},
                              {"XPG_DASH_DOT_DOT",  XPG_DASH_DOT_DOT := 5},
                              {"XPG_CUSTOM_DASH",   XPG_CUSTOM_DASH  := 6}},
               line_style_descs = vslice(line_styles,1) & {"-1"}

-- stored in ctrl_xtra[id][CX_BOX_SPACE_H|V] (as integer):
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

global constant XPG_CONTINUE = -4,
                XPG_DEFAULT = -3,
                XPG_IGNORE = -2,
                XPG_CLOSE = -1,

                XPG_GRAY = XPG_GREY,
                XPG_SILVER = XPG_GREY,
                XPG_DARK_GRAY = XPG_DARK_GREY,
                XPG_LIGHT_GRAY = XPG_LIGHT_GREY,
                
                XPG_NORMAL      = 0b000,
                XPG_BOLD        = 0b001,
                XPG_ITALIC      = 0b010,
                XPG_BOLDITALIC  = 0b011,
-- (local)      XPG_NORESET     = 0x100, -- for xpg_redraw_list(), defined below

                XPG_FILLED = 0b001, -- (nb same as true[/false])
                XPG_CHORD  = 0b010,
                XPG_SECTOR = 0b100,

                        -- WENS (note absence of both 0b11xx and 0bxx11)
                XPG_C  = 0b0000,    XPG_CENTRE    = XPG_C,
                XPG_N  = 0b0010,    XPG_NORTH     = XPG_N,
                XPG_S  = 0b0001,    XPG_SOUTH     = XPG_S,
                XPG_NE = 0b0110,    XPG_NORTHEAST = XPG_NE,
                XPG_E  = 0b0100,    XPG_EAST      = XPG_E,
                XPG_SE = 0b0101,    XPG_SOUTHEAST = XPG_SE,
                XPG_SW = 0b1001,    XPG_SOUTHWEST = XPG_SW,
                XPG_W  = 0b1000,    XPG_WEST      = XPG_W,
                XPG_NW = 0b1010,    XPG_NORTHWEST = XPG_NW,

                XPG_CENTER = #FFF3, -- (rest defined below, just before gShow)

                XPG_RAD2DEG = 180/PI,
                XPG_DEG2RAD = PI/180

global function gGetAlignName(integer d)
--  if d=-1 then return "-1" end if -- erm, no
    if d=XPG_C then return "XPG_C" end if
    string res = "XPG_"
    for bc in {{XPG_N,'N'},{XPG_S,'S'},{XPG_E,'E'},{XPG_W,'W'}} do
        if and_bits(d,bc[1]) then res &= bc[2] end if
    end for
    return res
end function

--/*
constant
                        -- WENS (note absence of both 0b11xx and 0bxx11)
                ZPG_NW = 0b1010,    ZPG_NORTHWEST = ZPG_NW,
                ZPG_W  = 0b1000,    ZPG_WEST      = ZPG_W,
                ZPG_SW = 0b1001,    ZPG_SOUTHWEST = ZPG_SW,
                ZPG_N  = 0b0010,    ZPG_NORTH     = ZPG_N,
                ZPG_S  = 0b0001,    ZPG_SOUTH     = ZPG_S,
                ZPG_NE = 0b0110,    ZPG_NORTHEAST = ZPG_NE,
                ZPG_E  = 0b0100,    ZPG_EAST      = ZPG_E,
                ZPG_SE = 0b0101,    ZPG_SOUTHEAST = ZPG_SE,
                ZPG_C  = 0b0000,    ZPG_CENTRE    = ZPG_C


global function gGetAlignName(integer d)
--  if d=-1 then return "-1" end if -- erm, no
    if d=ZPG_C then return "XPG_C" end if
    string res = "XPG_"
    for bc in {{ZPG_N,'N'},{ZPG_S,'S'},{ZPG_E,'E'},{ZPG_W,'W'}} do
        if and_bits(d,bc[1]) then res &= bc[2] end if
    end for
    return res
end function

--*/

sequence dashers = {{1},                                -- XPG_CONTINUOUS
                    {1,1,1,1,1,1,0,0},                  -- XPG_DASHED
--                  {1,1,0,0},                          -- XPG_DOTTED
                    {1,0},                              -- XPG_DOTTED (now that we're using ExtCreatePen)
                    {1,1,1,1,1,1,0,0,1,1,0,0},          -- XPG_DASH_DOT
                    {1,1,1,1,1,1,0,0,1,1,0,0,1,1,0,0},  -- XPG_DASH_DOT_DOT
                    {1}}                                -- XPG_CUSTOM_DASH

--DEV(minor) should XPG_CUSTOM_DASH be canvas-specific??
global function gGetLineStyleName(object s)
    if sequence(s) then
        assert(s==dashers[XPG_CUSTOM_DASH])
        return "XPG_CUSTOM_DASH"
    end if
    return line_style_descs[s]
--  if s=-1 then return "-1" end if
----    if s=XPG_CUSTOM_DASH then return "XPG_CUSTOM_DASH" end if
--  for sn in line_styles do
--      if s=sn[2] then return sn[1] end if
--  end for
--  return sprintf("** unknown style:%d **",s)
end function

global function gGetBoxSpacingName(object s, integer id=NULL)
    assert(id=NULL or ctrl_types[id]=BOX)
    if sequence(s) then
        assert(length(s)=2)
        integer {s1,s2} = s -- (force typecheck)
        string r1 = gGetBoxSpacingName(s1),
               r2 = gGetBoxSpacingName(s2,-1)
        return sprintf("{%s,%s}",{r1,r2})
    end if
--  assert(integer(s)) -- (caught next anyway)
    integer k = find(s,box_spacing_masks)
    assert(k!=0,"unknown box spacing:0b%b",s)
    if id!=NULL
    and (id=-1 or and_bits(ctrl_flags[id],CF_VERTICAL)) 
    and box_spacing_masks[k+1]==s then
        k += 1
    end if
    return box_spacing_descs[k]
end function

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
                GDK_LEAVE_NOTIFY_MASK           = 0x002000, -- 1 << 13,
--              GDK_FOCUS_CHANGE_MASK           = 0x004000, -- 1 << 14,
--              GDK_STRUCTURE_MASK              = 0x008000, -- 1 << 15,
--              GDK_PROPERTY_CHANGE_MASK        = 0x010000, -- 1 << 16,
--              GDK_VISIBILITY_NOTIFY_MASK      = 0x020000, -- 1 << 17,
--              GDK_PROXIMITY_IN_MASK           = 0x040000, -- 1 << 18,
--              GDK_PROXIMITY_OUT_MASK          = 0x080000, -- 1 << 19,
--              GDK_SUBSTRUCTURE_MASK           = 0x100000, -- 1 << 20,
                GDK_SCROLL_MASK                 = 0x200000, -- 1 << 21,
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
                GDK_LEAVE_NOTIFY        = 11,
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
                GDK_SELECTION_CLIPBOARD = 69,
--              typedef enum {
--  GDK_HINT_POS           = 1 << 0,
--  GDK_HINT_MIN_SIZE    = 1 << 1,
--  GDK_HINT_MAX_SIZE    = 1 << 2,
--  GDK_HINT_BASE_SIZE   = 1 << 3,
--  GDK_HINT_ASPECT    = 1 << 4,
--  GDK_HINT_RESIZE_INC  = 1 << 5,
--  GDK_HINT_WIN_GRAVITY = 1 << 6,
--  GDK_HINT_USER_POS    = 1 << 7,
--  GDK_HINT_USER_SIZE   = 1 << 8
--} GdkWindowHints;
--              GDK_HINT_MINMAX = 0b0110, -- (GDK_HINT_MIN_SIZE | GDK_HINT_MAX_SIZE)
                GDK_HINT_MIN_SIZE = 0b0010,
                GDK_HINT_MAX_SIZE = 0b0100,
                GDK_SB_H_DOUBLE_ARROW = 108,
                GDK_SB_V_DOUBLE_ARROW = 116,

--              GTK_ORIENTATION_HORIZONTAL = 0,
                GTK_ORIENTATION_VERTICAL = 1,
--              GTK_POLICY_ALWAYS    = 0,
                GTK_POLICY_AUTOMATIC = 1,
--              GTK_POLICY_NEVER     = 2,
--              GTK_POLICY_EXTERNAL  = 3,
--              GTK_PROGRESS_CONTINUOUS = 0,
--              GTK_PROGRESS_DISCRETE = 1,
--              GTK_PROGRESS_LEFT_TO_RIGHT = 0,
--              GTK_PROGRESS_RIGHT_TO_LEFT = 1,
                GTK_PROGRESS_BOTTOM_TO_TOP = 2,
--              GTK_PROGRESS_TOP_TO_BOTTOM = 3,
                GTK_SELECTION_NONE = 0,
--              GTK_SELECTION_SINGLE = 1
--              GTK_SHADOW_NONE = 0,
--GTK2:
                GTK_STATE_NORMAL = 0,
--              GTK_STATE_ACTIVE = 1,
--              GTK_STATE_PRELIGHT = 2,
--              GTK_STATE_SELECTED = 3,
--              GTK_STATE_INSENSITIVE = 4,
--GTK3:
                GTK_STATE_FLAG_NORMAL       = 0,
--              GTK_STATE_FLAG_ACTIVE       = 1 << 0,
--              GTK_STATE_FLAG_PRELIGHT     = 1 << 1,
--              GTK_STATE_FLAG_SELECTED     = 1 << 2,
--              GTK_STATE_FLAG_INSENSITIVE  = 1 << 3,
--              GTK_STATE_FLAG_INCONSISTENT = 1 << 4,
--              GTK_STATE_FLAG_FOCUSED      = 1 << 5,
--              GTK_STATE_FLAG_BACKDROP     = 1 << 6
                GTK_STYLE_PROVIDER_PRIORITY_APPLICATION = 600,
--              GTK_STYLE_PROVIDER_PRIORITY_USER = 800,
                GTK_WINDOW_TOPLEVEL = 0,
--              GTK_WINDOW_POPUP = 1,               -- menu/tooltip (not a proper sub-window)
--              GTK_WIN_POS_NONE = 0,
--              GTK_WIN_POS_CENTER = 1,
--              GTK_WIN_POS_MOUSE = 2,
--              GTK_WIN_POS_CENTER_ALWAYS = 3,
--              GTK_WIN_POS_CENTER_ON_PARENT = 4,
--              PANGO_STYLE_ITALIC = 2,
--              PANGO_UNDERLINE_SINGLE = 1,
--              PANGO_WEIGHT_BOLD = 700,

                WM_USER = #400, -- (1024)
--              BI_RGB = 0,
                BM_GETCHECK = 240,
                BM_SETCHECK = 241,
--              BM_GETSTATE = 242,
--              BM_SETSTATE = 243,
                BM_SETIMAGE = 247,
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
                BS_ICON = #40,
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

--              COLOR_SCROLLBAR = 0,
--              COLOR_BACKGROUND = 1,
--              COLOR_DESKTOP = 1,
--              COLOR_ACTIVECAPTION = 2,
--              COLOR_INACTIVECAPTION = 3,
--              COLOR_MENU = 4,
--              COLOR_WINDOW = 5,
--              COLOR_WINDOWFRAME = 6,
--              COLOR_MENUTEXT = 7,
--              COLOR_WINDOWTEXT = 8,
--              COLOR_CAPTIONTEXT = 9,
--              COLOR_ACTIVEBORDER = 10,
--              COLOR_INACTIVEBORDER = 11,
--              COLOR_APPWORKSPACE = 12,
--              COLOR_HIGHLIGHT = 13,
--              COLOR_HIGHLIGHTTEXT = 14,
                COLOR_BTNFACE = 15,
--              COLOR_3DFACE = 15,
--              COLOR_BTNSHADOW = 16,
--              COLOR_3DSHADOW = 16,
--              COLOR_GRAYTEXT = 17,
--              COLOR_BTNTEXT = 18,
--              COLOR_INACTIVECAPTIONTEXT = 19,
--              COLOR_BTNHIGHLIGHT = 20,
--              COLOR_BTNHILIGHT = 20,
--              COLOR_3DHILIGHT = 20,
--              COLOR_3DDKSHADOW = 21,
--              COLOR_3DLIGHT = 22,
--              COLOR_INFOTEXT = 23,
--              COLOR_INFOBK = 24,
--/*
--              COLOR_HOTLIGHT = 26 ,
--  Color for a hyperlink or hot-tracked item.
-- 
--              COLOR_GRADIENTACTIVECAPTION = 27,
--  Right side color in the color gradient of an active window's title bar. 
--  COLOR_ACTIVECAPTION specifies the left side color. 
--  Use SPI_GETGRADIENTCAPTIONS with the SystemParametersInfo function to determine whether the gradient effect is enabled. 
-- 
--              COLOR_GRADIENTINACTIVECAPTION = 28,
--  Right side color in the color gradient of an inactive window's title bar. 
--  COLOR_INACTIVECAPTION specifies the left side color.
-- 
--              COLOR_MENUHILIGHT = 29,
--  The color used to highlight menu items when the menu appears as a flat menu (see SystemParametersInfo). 
--  The highlighted menu item is outlined with COLOR_HIGHLIGHT. 
--
--              COLOR_MENUBAR = 30,
--  The background color for the menu bar when menus appear as flat menus (see SystemParametersInfo). 
--  However, COLOR_MENU continues to specify the background color of the menu popup. 
--*/
                CS_VREDRAW = 1,
                CS_HREDRAW = 2,
                CS_DBLCLKS = 8,
                CS_OWNDC = #20,
                CW_USEDEFAULT = #80000000,
                DEFAULT_CHARSET = 1,
                DIB_RGB_COLORS = 0,
--              DLGC_WANTMESSAGE = 0x0004,

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
--              DTS_SHORTDATEFORMAT        = 0x00,
--              DTS_UPDOWN                 = 0x01,
--              DTS_SHOWNONE               = 0x02,
--              DTS_LONGDATEFORMAT         = 0x04,
--              DTS_TIMEFORMAT             = 0x09,
--              DTS_APPCANPARSE            = 0x10,
--              DTS_RIGHTALIGN             = 0x20,
--              DTS_SHORTDATECENTURYFORMAT = 0x0C,

--              EM_SETBKGNDCOLOR = 1091,
--              ERROR_CLASS_ALREADY_EXISTS = 1410,
                ES_AUTOHSCROLL = #80,
--              ES_AUTOVSCROLL = 64,
                ES_LEFT = 0,
--              ES_CENTER = 1,
                ES_RIGHT = 2,
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
                IDC_SIZENS = 32645, -- Double-pointed arrow pointing north and south
                IDC_SIZEWE = 32644, -- Double-pointed arrow pointing west and east
                ILC_MASK        = 1,
--              ILC_COLOR       = 0,
--              ILC_COLORDDB    = #FE,
--              ILC_COLOR4      = #00,
                ILC_COLOR8      = #08,
--              ILC_COLOR16     = #10,
--              ILC_COLOR24     = #18,
--              ILC_COLOR32     = #20,
                IMAGE_BITMAP = 0,
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
                MF_SYSMENU    = #2000,
                MIIM_ID = 0x00000002,
                MIIM_TYPE = 0x00000010,
                MIIM_STRING = 0x00000040,
                MIIM_BITMAP = 0x00000080,
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
                PS_SOLID         = #00000000,       -- XPG_CONTINUOUS = 1,
--              PS_DASH          = #00000001,       -- XPG_DASHED = 2,
--              PS_DOT           = #00000002,       -- XPG_DOTTED = 3,
--              PS_DASHDOT       = #00000003,       -- XPG_DASH_DOT = 4,
--              PS_DASHDOTDOT    = #00000004,       -- XPG_DASH_DOT_DOT = 5
--              PS_NULL          = #00000005,
--              PS_INSIDEFRAME   = #00000006,
--              PS_USERSTYLE     = #00000007,
                PS_ALTERNATE     = #00000008,
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
--              R2_MASKPEN = 9,         -- combination common to pen and the screen.
--              R2_NOTXORPEN = 10,      -- combination of colors in pen and screen, but not in both.
--              R2_NOP = 11,            -- Pixel remains unchanged.
--              R2_MERGENOTPEN = 12,    -- combination of screen and inverse of pen.
--              R2_COPYPEN = 13,        -- Pixel is the pen color.
--              R2_MERGEPENNOT = 14,    -- combination of pen color and inverse of screen color.
--              R2_MERGEPEN = 15,       -- combination of pen color and the screen color.
--              R2_WHITE = 16,          -- Pixel is always 1.
--              R2_LAST = 16,

--              RDW_INVALIDATE      = #0001,
--              RDW_INTERNALPAINT   = #0002,
--              RDW_ERASE           = #0004,
--              RDW_VALIDATE        = #0008,
--              RDW_NOINTERNALPAINT = #0010,
--              RDW_NOERASE         = #0020,
--              RDW_NOCHILDREN      = #0040,
--              RDW_ALLCHILDREN     = #0080,
--              RDW_UPDATENOW       = #0100,
--              RDW_ERASENOW        = #0200,
--              RDW_FRAME           = #0400,
--              RDW_NOFRAME         = #0800,
                -- sort
--              SB_HORZ = 0,
--              SB_VERT = 1,
--              SB_CTL = 2,
--              SB_BOTH = 3,
                -- scroll info flags
--              SIF_RANGE = #1,
--              SIF_PAGE = #2,
--              SIF_POS = #4,
--              SIF_DISABLENOSCROLL = #8,
--              SIF_TRACKPOS = #10,
--              SIF_ALL = or_all({SIF_RANGE,SIF_PAGE,SIF_POS,SIF_TRACKPOS}),
                SM_CXSCREEN = 0,
                SM_CYSCREEN = 1,
                SM_CXSMICON = 49,   -- see/use SM_XICON,
                SM_CYSMICON = 50,   --         SM_YICON
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
                TCM_SETIMAGELIST = 4867,
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
                TCIF_IMAGE = 2,
--              TCIF_PARAM = 8,
--              TCIF_RTLREADING = 4,
                -- HIT-TEST
--              TCHT_NOWHERE = 1,
--              TCHT_ONITEM = 6,
--              TCHT_ONITEMICON = 2,
--              TCHT_ONITEMLABEL = 4,

                TME_CANCEL = 0x80000000,
                TME_LEAVE  = 0x00000002,
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
--              UDM_SETRANGE = 1125,
                UDM_SETBUDDY = 1129,
                UDM_SETACCEL = 1131,
                UDM_GETACCEL = 1132,
                UDM_SETRANGE32 = 1135,
                UDM_GETRANGE32 = 1136,
--              WM_USER = #400, -- (1024)
--              UDM_SETPOS32 = (WM_USER+113),
--              UDM_GETPOS32 = (WM_USER+114),
                UDM_SETPOS32 = 1137,
                UDM_GETPOS32 = 1138,
                UDN_DELTAPOS = -722,
                UDS_WRAP = #001,
                UDS_SETBUDDYINT = #002,
                UDS_ALIGNRIGHT = #004,
                UDS_ARROWKEYS = #020,
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
--              WM_SYSCOLORCHANGE = 21,
--              WM_SHOWWINDOW = 24,
--              WM_ACTIVATEAPP = 28,
--              WM_SETCURSOR = 32,
--              WM_MOUSEACTIVATE = 33,
                WM_GETMINMAXINFO = 36,
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
--              WM_GETDLGCODE = 135,
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
                WM_CTLCOLOREDIT = 307,
--              WM_CTLCOLORBTN = 309,
--              WM_CTLCOLORSTATIC = 312,
                WM_MOUSEMOVE = 512,
                WM_LBUTTONDOWN = 513,
                WM_LBUTTONUP = 514,
--              WM_LBUTTONDBLCLK = 515,
--              WM_RBUTTONDOWN = 516,
--              WM_RBUTTONUP = 517,
--              WM_RBUTTONDBLCLK = 518,
--              WM_MBUTTONDOWN = 519,
--              WM_MBUTTONUP = 520,
--              WM_MBUTTONDBLCLK = 521,
                WM_MOUSEWHEEL = 522,
                WM_XBUTTONDOWN = 523,
--              WM_PARENTNOTIFY = 528,
--              WM_SIZING = 532,
--              WM_IME_SETCONTEXT = 641,
--              WM_IME_NOTIFY = 642,
--              WM_MOUSEHOVER = 673,    --  #02A1
--              WM_NCMOUSELEAVE = 674,  --  #02A2
                WM_MOUSELEAVE = 675,    --  #02A3
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
--              WS_HSCROLL          = #00100000,
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
                OPAQUE = 2,
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
--              cairo_surface_destroy,
                cairo_translate,
--              gdk_atom_intern,
                gdk_cairo_create,
--              gdk_cairo_rectangle,
                gdk_cairo_set_source_pixbuf,
                gdk_color_parse,
                gdk_cursor_new_for_display,
--              gdk_cursor_new_from_name,
                gdk_display_get_default,
                gdk_display_get_monitor,
                gdk_display_get_pointer,
                gdk_get_default_root_window,
                gdk_monitor_get_geometry,
--              gdk_pixbuf_get_from_drawable,
--              gdk_pixbuf_get_from_surface,
--              gdk_pixbuf_get_from_window,
                gdk_pixbuf_get_height_,
                gdk_pixbuf_get_pixels_,
                gdk_pixbuf_get_type,
                gdk_pixbuf_get_width_,
                gdk_pixbuf_new_from_data,
                gdk_pixbuf_new_from_xpm_data,
                gdk_rgba_parse,
                gdk_screen_get_default,
                gdk_screen_get_height,
                gdk_screen_get_width,
--              gdk_screen_get_root_window,
--              gdk_window_focus,
                gdk_window_get_display,
--              gdk_window_get_geometry,
                gdk_window_get_height,
                gdk_window_get_width,
                gdk_window_get_origin,
                gdk_window_get_root_origin,
--              gdk_window_get_position,
                gdk_window_invalidate_rect,
--              gdk_window_move_resize,
                gdk_window_process_updates,
                gdk_window_set_cursor,
                gtk_adjustment_new,
--              gtk_adjustment_set_lower,
--              gtk_adjustment_set_step_increment,
--              gtk_adjustment_set_page_increment,
--              gtk_adjustment_get_upper,
--              gtk_adjustment_set_upper,
--              gtk_adjustment_set_page_size,
--              gtk_adjustment_changed,
--GTK3 only:
--              gtk_box_new,
--              gtk_box_pack_end,
                gtk_box_pack_start,
                gtk_button_get_label,
--              gtk_button_new_with_label,
                gtk_button_new_with_mnemonic,
                gtk_button_set_image,
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
--              gtk_grid_attach,
--              gtk_grid_new,
                gtk_hbox_new,
                gtk_image_menu_item_new_with_mnemonic,
                gtk_image_menu_item_set_always_show_image,
                gtk_image_menu_item_set_image,
--              gtk_image_menu_item_set_use_stock,
                gtk_image_new_from_pixbuf,
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
--              gtk_menu_item_new,
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
                gtk_misc_set_alignment,
--              gtk_notebook_append_page,
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
--              gtk_scrolled_window_add_with_viewport,
                gtk_scrolled_window_new,
                gtk_scrolled_window_set_policy,
--              gtk_scrolled_window_set_shadow_type,
                gtk_separator_menu_item_new,
                gtk_spin_button_get_increments,
                gtk_spin_button_get_range,
                gtk_spin_button_get_value,
                gtk_spin_button_get_wrap,
                gtk_spin_button_new_with_range,
                gtk_spin_button_set_increments,
                gtk_spin_button_set_range,
                gtk_spin_button_set_value,
                gtk_spin_button_set_wrap,
                gtk_style_context_add_provider,
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
--              gtk_viewport_new,
--              gtk_viewport_set_shadow_type,
--              gtk_widget_get_allocated_width,
--              gtk_widget_get_allocated_height,
--              gtk_widget_get_allocation,
                gtk_widget_get_can_focus,
--              gtk_widget_get_root_window,
--              gtk_widget_get_parent,
                gtk_widget_get_pointer,
                gtk_widget_get_sensitive,
                gtk_widget_get_style_context,
                gtk_widget_get_window,
                gtk_widget_grab_focus,
                gtk_widget_hide,
--              gtk_widget_is_sensitive,
                gtk_widget_modify_base,
--              gtk_widget_modify_bg,
                gtk_widget_modify_font,
--              gtk_widget_modify_style,
                gtk_widget_override_background_color,
                gtk_widget_override_font,
                gtk_widget_queue_draw,
--              gtk_widget_queue_draw_area,
                gtk_widget_realize,
                gtk_widget_set_can_focus,
                gtk_widget_set_events,
--              gtk_widget_set_halign,
                gtk_widget_set_realized,
                gtk_widget_set_sensitive,
                gtk_widget_set_size_request,
                gtk_widget_set_tooltip_text,
--              gtk_widget_set_usize,
--              gtk_widget_set_hexpand,
--              gtk_widget_set_vexpand,
                gtk_widget_show,
                gtk_widget_show_all,
                gtk_widget_size_request,
--              gtk_widget_get_preferred_size,
--              gtk_window_is_active,
                gtk_window_move,
                gtk_window_new,
                gtk_window_set_default_size,
                gtk_window_set_geometry_hints,
--              gtk_window_set_policy,
                gtk_window_set_title,
                gtk_window_set_transient_for,
                gtk_window_get_transient_for,
--              gtk_window_set_position,
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
                pango_layout_get_pixel_extents,
                pango_layout_get_pixel_size,
                pango_layout_set_font_description,
                pango_layout_set_text,
                xg_object_unref,
                idGdkEventButton,
                idGdkEventConfigure,
                idGdkEventFocus,
                idGdkEventKey,
                idGdkEventMotion,
                idGdkEventScroll,
                idGdkEventCrossing,
                idGdkRectangle,
                idGtkRequisition,
                idGdkColor,
                idGdkRGBA,
                idGdkGeometry,
                gtk_col_resize_cursor = NULL,
                gtk_row_resize_cursor = NULL,
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
                xCreateCompatibleBitmap,           
                xCreateCompatibleDC,
                xCreateDIBitmap,
                xCreateFontIndirect,
                xCreateMenu,
                xCreatePopupMenu,
                xCreatePen,
                xExtCreatePen,
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
                xGetDIBColorTable,
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
--              xGetNextDlgTabItem,
--              xGetParent,
                xGetPixel,
                xGetStockObject,
--              xGetScrollInfo,
                xGetSysColor,
                xGetSysColorBrush,         
                xGetSystemMetrics,
                xGetTextExtentPoint32,
                xGetTextExtentPoint32W,
--              xGetWindow,
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
                xInvalidateRect,           
                xIsClipboardFormatAvailable,
                xIsDialogMessage,
                xKillTimer,
                xLineTo,
                xLoadCursor,
                xLoadIcon,
                xMoveToEx,
                xMoveWindow,
                xOpenClipboard,
                xPie,
--              xPlaySound,
--              xPolyBezier,           
                xPostQuitMessage,
                xRectangle,
                xRedrawWindow,
                xRegisterClassEx,
                xReleaseCapture,
                xReleaseDC,
                xRoundRect,
                xScreenToClient,           
                xSelectObject,
                xSendMessage,
                xSetBkColor,
                xSetBkMode,
                xSetCapture,
                xSetClipboardData,
                xSetDIBColorTable,
                xSetFocus,
                xSetMenu,
--              xSetMenuItemBitmaps,
                xSetMenuItemInfo,                  
                xSetParent,
                xSetPixelV,
--              xGetROP2,
                xSetROP2,
--              xSetScrollInfo,
                xSetTextAlign,
                xSetTextColor,
                xSetTimer,
                xSetWindowLong,
                xSetWindowPos,
                xSetWindowText,
                xShowWindow,
--              xTextOut,
                xTrackMouseEvent,
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
                idNMUPDOWN,
                idUDACCEL,
                idTVITEM,
                idTVITEMEX,
                idNMTREEVIEW,
                idTVINSERTSTRUCT,
                idLOGFONT,
                idLOGBRUSH,
--              idPIXELFORMATDESCRIPTOR,
                idTOOLINFO,
--              idSCROLLINFO,
                idTCITEM,
                idMENUITEMINFO,
                idTRACKMOUSEEVENT,
                idMINMAXINFO,
                SM_XICON,
                SM_YICON,
                WINAPI_SUBMENUS,  -- both key:handle, data:id 
                                  -- and key:{mid,id}, data:{menu,pos}
                WINAPI_MENU_IMGS, -- key:{mid,id}, data: imgid (idx to xpm_texts)
                WIN_MENU_CHECKED  -- key:{mid,id}, data:0 or radio group as {{ids},mh}

local atom szAppName, pData, pPAINTSTRUCT, pRECT, pX, pY, pW, pH, pSIZE, pTVINSERTSTRUCT, 
                             pTVITEMEX, pMSG, pLOGFONT, pLOGBRUSH, pTOOLINFO, pPOINT, pUDACCEL,
--                          pPIXELFORMATDESCRIPTOR, pSCROLLINFO, 
                             pTCITEM, pMENUITEMINFO, pTRACKMOUSEEVENT, NullBrushID,
                             pGtkRequisition, pGDKCOLOR, pGDKRGBA, pGdkGeometry

local sequence gtk_version

--DEV/SUG: (or a whole bunch of #ilASM{}-based specifics)
--/*
local function g_func(integer rid, sequence args)
    atom res = c_func(rid,args)
    return res
end function

local procedure g_proc(integer rid, sequence args)
    c_proc(rid, args)
end procedure
--*/

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
--  function intints(val, w, h, name) {
        // version of intint() for dialog [raster]size, supporting eg "QUARTERxEIGHTH"
        if (typeof(val) === "string") {
            // convert eg "225x75" to [225,75]
            //  (ie a js Array of length 2)
            let x = val.indexOf('x'), y;
            if (x !== -1) {
                y = val.slice(x+1);
                x = val.slice(0,x);
--              function fulleighth(s,x,f,name) {
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
--              }
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
--  }
--*/
--DEV (re-?)translate this for xpGUI.js:
--DEV/SUG allow "px:10x15" == "10x15", "pt|[r]em:10x15" in points/[relative]em...
local function xpg_intint(string val)
    --
    -- convert eg "225x75" to {225,75}, and in fact 
    --            "50x10x20x30" to {50,10,20,30},
    --            "1"->{1}, and "1x2x3"->{1,2,3}.
    -- likewise eg "{225,75}" -> {225,75}, etc.
    --
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
--  res = apply(true,to_number,{res,-1})
    return res
end function

--?xpg_intint("1")
--?xpg_intint("1x2x3")
--?xpg_intint("225x75")
--?xpg_intint("50x10x20x30")
--?xpg_intint("50x")

--/* DEV/SUG (for the SPACE attribute...)
local function xpg_words(string val)
    -- convert eg "AROUND" to {"AROUND"}, and
    --            "{AROUND,TOP}" to {"AROUND","TOP"}.
    -- note however "AROUNDxTOP" is /not/ supported.
    -- caller to validate length(res) is acceptable,
    -- as well as each word in it being recognised.
    sequence res
    if val[1]='{' then
        assert(val[$]='}')
        res = split(val[2..-2],',',false)
    else
        res = {val}
    end if
    return res
end function
--*/

global function gGetDialog(gdx id)
    if id>1 and ctrl_types[id]=TEXT and ctrl_types[id-1]=SPIN then id -= 1 end if
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
    if backend=XPG_GTK then
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
--      atom pX = pRECT+get_field_details(idGdkRectangle,"x")[1],
--           pY = pRECT+get_field_details(idGdkRectangle,"y")[1],
--           pW = pRECT+get_field_details(idGdkRectangle,"width")[1],
--           pH = pRECT+get_field_details(idGdkRectangle,"height")[1]
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
--?{"gwr",left,top,width,height}
--29/9/23:
--/*
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
--*/
--  xpg_lm_get_dialog_decoration_size(gdx id)
            width += 2
            height += 32
--?{"gwre",left,top,width,height}
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
    elsif backend=XPG_WINAPI then
        integer r = c_func(xGetWindowRect,{handle,pRECT})
        assert(r!=0)
          left = get_struct_field(idRECT,pRECT,"left")
           top = get_struct_field(idRECT,pRECT,"top")
         width = get_struct_field(idRECT,pRECT,"right")-left
        height = get_struct_field(idRECT,pRECT,"bottom")-top
--DEV and now I cannot remember where these came from or what they were for, so...
-- taking out the first/CF_RESIZE mullered initial size of gSplit.exw...
        if ct=DIALOG then
--?{"xpg_get_window_rect",and_bits(ctrl_flags[id],CF_RESIZE)}
            if and_bits(ctrl_flags[id],CF_RESIZE) then
                width -= 14; height -= 7;
--              width -= 16; height -= 16;
            else
?"xpg_get_window_rect(no CF_RESIZE)..."
--              width -= 4; height -= 12;
            end if
        end if
    else
        ?9/0 -- (unknown backend)
    end if
--DEV suspect...
--  sequence res = peek4s({pRECT,4})
--  if backend=XPG_WINAPI then
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
    if backend=XPG_GTK then
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
--      atom pW = pRECT+get_field_details(idGdkRectangle,"width")[1],
--           pH = pRECT+get_field_details(idGdkRectangle,"height")[1]
        c_proc(gtk_window_get_size,{handle,pW,pH})
          width = get_struct_field(idGdkRectangle,pRECT,"width")
         height = get_struct_field(idGdkRectangle,pRECT,"height")
    elsif backend=XPG_WINAPI then
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
if backend=XPG_GTK then
--  ?{"lmgdds(GTK)",width,height}
    width = 2
    height = 32
end if
--?{"lmgdds",width,height}
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
--DEV test this with multiple commas, eg "Arial, bold, italic, 9" [untested]
--      styles = split(v[comma+1..$])
        styles = split_any(v[comma+1..$]," ,")
        fontsize = to_integer(styles[$])
        if fontsize then styles = styles[1..-2] end if
        if find(styles,{{"Normal"},{"normal"}}) then styles = {} end if
        for i,s in styles do
--DEV the latter two should set some flags, somewhere, for GTK, which should use/trigger ...
--  gboolean
--  pango_parse_markup (
--    const char* markup_text,
--    int length,
--    gunichar accel_marker,
--    PangoAttrList** attr_list,
--    char** text,
--    gunichar* accel_char,
--    GError** error
--  )
-- with surrounding <u> and <s>, and maybe <b> and <i> as well... and creating fewer fontdescs.
--  <big>, <small>, <sub>, <sup>, and <tt> should also be re-considered (iff WinAPI will comply).
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
    if backend=XPG_GTK then
--      if find(face,{"Helvetica","Arial"}) then
        if find(face,{"Helvetica","Arial","Calibri"}) then
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
    elsif backend=XPG_WINAPI then
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
--      fsf = {face,styles,fontsize}
        fsf = {face,styles,fontsize,angle}
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
--?{"styles",styles,fw,hFont}
            ctrl_font[id] = 0
            -- find an unused k
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
--2/10/23:
        if ct=CANVAS then
            atom mdc = ctrl_xtra[id][CX_CANVAS_HDC]
            prevFont = c_func(xSelectObject,{mdc,hFont})    
--20/10/23:
        elsif ct=SPIN then
            handle = ctrl_handles[id+1]
            {} = c_func(xSendMessage,{handle,WM_SETFONT,hFont,true})
        end if
    else
        ?9/0 -- (unknown backend)
    end if
end procedure

global function gGetTextExtent(gdx id, sequence text, bool bSumHeights=true)
    integer w = 0, h = 0, wi, hi, ct = ctrl_types[id]
--  atom handle = ctrl_handles[id], cairo=NULL, layout, pW, pH, hDC
    atom handle = ctrl_handles[id], cairo=NULL, layout, hDC
    assert(handle!=NULL)
    bool bOK, bDestroy = false
    if backend=XPG_GTK then
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
--      pW = pRECT+get_field_details(idGdkRectangle,"width")[1]
--      pH = pRECT+get_field_details(idGdkRectangle,"height")[1]
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
--      if not string(line) then
--          line = line[1]
--          assert(string(line))
--      end if
        if backend=XPG_GTK then
            c_proc(pango_layout_set_text,{layout,line,length(line)})
--1/12/23: using pango_layout_get_pixel_extents mullered text positioning in gCanvas.exw...
--          (now if I could only remember what this was pigging well supposed to fix...)
            c_proc(pango_layout_get_pixel_size,{layout,pW,pH})
--          c_proc(pango_layout_get_pixel_extents,{layout,NULL,pRECT})
--          c_proc(pango_layout_get_pixel_extents,{layout,pRECT,NULL})
--void
--pango_layout_get_pixel_extents (
--  PangoLayout* layout,
--  PangoRectangle* ink_rect,
--  PangoRectangle* logical_rect
--)
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
    if backend=XPG_GTK then
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
--  if id!=0 then
        integer ct = ctrl_types[id]
        if name="CLASSNAME" then 
            return ctrl_names[ct]
        elsif name="EXPAND" then
            integer hvdx = and_bits(ctrl_flags[id],CF_EXPAND) -- 0/1/2/4
            string res = {"NONE","BOTH","HORIZONTAL",0,"VERTICAL"}[hvdx+1]
            return res
        elsif name="GAP" then
            assert(ct=BOX)
            return ctrl_xtra[id][CX_BOX_GAP]
        elsif name="MARGIN"
           or name="PADDING" then
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
        elsif name="USER_DATA" then
            return user_data[id]
        end if
        bool bMapped = and_bits(ctrl_flags[id],CF_MAPPED)!=0
        if bMapped then
            atom handle = ctrl_handles[id]
--          integer w, h
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
                if backend=XPG_GTK then
                    return c_func(gtk_widget_get_can_focus,{handle})
                elsif backend=XPG_WINAPI then
                    atom dwStyle = c_func(xGetWindowLong,{handle,GWL_STYLE})
                    return and_bits(dwStyle,WS_TABSTOP)!=0
                else
                    ?9/0 -- (unknown backend)
                end if
            elsif name="SIZE" then
----NB from gCanvas, untested on anything else...
----better (once the layout manager is working)
                return {ctrl_size[id][SZ_W],
                        ctrl_size[id][SZ_H]}
            elsif name="NATURALSIZE" then
                return {ctrl_size[id][SZ_NATURAL_W],
                        ctrl_size[id][SZ_NATURAL_H]}
--/*
--          elsif name="NATURALSIZE" 
--             or name="SIZE" then
                integer nw = ctrl_size[id][SZ_NATURAL_W],
                        nh = ctrl_size[id][SZ_NATURAL_H]
if name="SIZE" then
--  nw = ctrl_size[id][SZ_NORMAL_W]
--  nh = ctrl_size[id][SZ_NORMAL_H]
    nw = ctrl_size[id][SZ_W]
    nh = ctrl_size[id][SZ_H]
end if
-- seriously mullers xgUI/gCanvas.exw... [1/11/23] [FINALLY FIXED, under WinAPI anyway/only]
--if false then
--if backend=XPG_WINAPI then
if true then
--/*
Under GTK, false:
id ----ctyp----   x   y   w   h  nw  nh  uw  uh  p  children   flags
 1 Canvas         0   0   0   0   0   0   0   0  2  0          {840,462,238,124}
 2 Dialog       840 462 240 156   2  32 240 156  0  {1}        {840,462,240,156}
{"gGetAttribute",1,"SIZE",{0,0},{238,124'|'}}
{"gGetAttribute",1,"SIZE",{0,0},{238,124'|'}}
true:
id ----ctyp----   x   y   w   h  nw  nh  uw  uh  p  children   flags
 1 Canvas         0   0   0   0   0   0   0   0  2  0          {840,462,238,124}
 2 Dialog       840 462 240 156   2  32 240 156  0  {1}        {840,462,240,156}
Under WinAPI, false:
{"gGetAttribute",1,"SIZE",{238,124'|'},{238,124'|'}}
{"gGetAttribute",1,"SIZE",{238,124'|'},{238,124'|'}}
id ----ctyp----   x   y   w   h  nw  nh  uw  uh  p  children   flags
 1 Canvas         0   0 238 124  11  15   0   0  2  0          {848,493,238,124}
 2 Dialog       840 462 240 156  13  47 240 156  0  {1}        {840,462,240,156}
true:
id ----ctyp----   x   y   w   h  nw  nh  uw  uh  p  children   flags
 1 Canvas         0   0 238 124  11  15   0   0  2  0          {848,493,238,124}
 2 Dialog       840 462 240 156  13  47 240 156  0  {1}        {840,462,240,156}
--*/
                return {nw,nh}
else --DEV temp:
--?{name,nw,nh}
                if backend=XPG_GTK then
--DEV fixes scroller.exw:
--                  if bGTK3 then
--                  if false then
--                      c_proc(gtk_widget_size_request,{handle,pGtkRequisition})
--                      w = get_struct_field(idGtkRequisition,pGtkRequisition,"width")
--                      h = get_struct_field(idGtkRequisition,pGtkRequisition,"height")
--                  else
                        atom window = c_func(gtk_widget_get_window,{handle})
                        w = c_func(gdk_window_get_width,{window})
                        h = c_func(gdk_window_get_height,{window})
--                  end if

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
                elsif backend=XPG_WINAPI then
crash("DEAD CODE") -- (I hope)
                    if ct=LABEL then
                        string title = gGetAttribute(id,"TITLE","")
                        {w,h} = gGetTextExtent(id,split(title,'\n'))
--?{"GAlabel",title,w,h} 
                    else
                        if ct=BOX then
                            do
--                              id = gGetParent(id)
                                id = parent_ids[id]
                                ct = ctrl_types[id]
                            until ct!=BOX
                            handle = ctrl_handles[id]
                        end if
                        integer r = c_func(xGetWindowRect,{handle,pRECT})
                        assert(r!=0)
--                      if r!=0 then ?"r!=0 line 2672 in p[GUI.e!!" end if
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
--                  return {w,h}
                else
                    ?9/0 -- (unknown backend)
                end if
  if nw!=w or nh!=h then
    ?{"gGetAttribute",id,name,{nw,nh},{w,h}}
  end if
                return {w,h}
end if
--*/
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
            elsif name="MOUSEPOS" then
                if backend=XPG_GTK then
                    c_proc(gtk_widget_get_pointer,{handle,pX,pY})
                    return {get_struct_field(idGdkRectangle,pRECT,"x"),
                            get_struct_field(idGdkRectangle,pRECT,"y")}
                elsif backend=XPG_WINAPI then
                    bool bOK = c_func(xGetCursorPos,{pPOINT})
                    assert(bOK)
                    bOK = c_func(xScreenToClient,{handle,pPOINT})
                    assert(bOK)
                    return {get_struct_field(idPOINT,pPOINT,"x"),
                            get_struct_field(idPOINT,pPOINT,"y")}
--/*
--  JavaScript
--  In a browser environment, it's impossible to actually get the cursor position at the specific moment. 
--  You must wait for user input (movement, click, etc). One of many ways to add an event listener:
--
--  document.addEventListener('mousemove', function(e){
--    var position = { x: e.clientX, y: e.clientY }
--  }
--  In the above example, the window may not be external. It must in fact be a web browser window, which runs the script.
--*/
                else
                    ?9/0 -- (unknown backend)
                end if              
            end if
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
--              if not find(name,{"ACTIVE","FONT"}) then exit end if
--              iid = parent_ids[iid]
--          end while
        end if
        if bMapped or and_bits(ctrl_flags[id],CF_UNMAPATTR)!=0 then
            integer geta = ctrl_msg[ct][CM_GET]
--?{name,id,geta,get_routine_info(geta)}
            return geta(id,name,dflt)
        end if
--  end if
    return xpg_return_default_attr(id,name,dflt)
end function

global function gGetInt(gdx id, string name, object dflt=999_999_999)
    if name="EXPAND" then
        return and_bits(ctrl_flags[id],CF_EXPAND) -- 0/1/2/4
    end if
    object ores = gGetAttribute(id,name,dflt)
    integer res = iff(string(ores)?to_number(ores):ores)
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
          ech = iff(backend=XPG_GTK?'_':
                iff(backend=XPG_WINAPI?'&':9/0))
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
constant dtest = {{XPG_GTK,{{"",""},
                            {"__","_"},
                            {"_x","x"},
                            {"&x","&x"},
                            {"x&_y","x&y"},
                            {"______","___"},
                            {"_1_2_3","123"},
                            {"___2__","_2_"},
                            {"_1___3","1_3"},
                            {"_Help","Help"}}},
                  {XPG_WINAPI,{{"",""},
                               {"&&","&"},
                               {"&x","x"},
                               {"_x","_x"},
                               {"x_&y","x_y"},
                               {"&&&&&&","&&&"},
                               {"&1&2&3","123"},
                               {"&&&2&&","&2&"},
                               {"&1&&&3","1&3"},
                               {"&Help","Help"}}}}
--forward function gVersion(integer bBack=false)
--procedure d_test()
    for b in dtest do
        backend = b[1]
        for t in b[2] do
            string {p,q} = t, r = xpg_demnemonicalize(p)
            if r!=q then
                string b = gVersion(true)
                printf(1,"%s: wanted %s but got %s (%s)\n",{p,q,r,b})
            end if
        end for
    end for
--Xstring dNULL = xpg_demnemonicalize(NULL)
--Xif dNULL!="" then
--X printf(1,"NULL: wanted `` but got %s\n",{dNULL})
--Xend if
    backend = find(platform(),SPLAT)
--end procedure
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
    -- mp should be either SZ_MARGIN or SZ_PADDING
    -- mpx should be MP_TOP, MP_RGT, MP_BTM, or MP_LFT
    object cmi = ctrl_size[id][mp]
    if atom(cmi) then return 0 end if
    -- short sequences spell trbl...
    sequence mpdx = {{1,1,1,1},
                     {1,2,1,2},
                     {1,2,3,2},
                     {1,2,3,4}}[length(cmi)]
    integer mpdxi = mpdx[mpx]
    return cmi[mpdxi]
end function

local procedure xpg_lm_set_element_sizes(gdx id)
    --
    -- set [SZ_NATURAL_W/H], /after/ any FONT etc applied.
    -- (could perhaps be done nearer the creation, but afaik won't 
    --  work right until after gtk_widget_realize/CreateWindowEx,
    --  and may need re-doing after a set "TEXT"/gRedraw() anyway.)
    --
    integer ct = ctrl_types[id]
--if ct=CANVAS then ?{"ans(CANVAS)",ctrl_size[id][SZ_NATURAL_W],ctrl_size[id][SZ_NATURAL_H]} end if
    object children = children_ids[id]
    if sequence(children) then
--/*
--      integer {mt,mr,mb,ml} = xpg_margins(ct),
--              nw = ml, nh = mt, 
--      integer {{nh,mr,mb,nw},gap} = xpg_margap(id),
        integer nh = xpg_get_mp(id,SZ_PADDING,MP_TOP),
                nw = xpg_get_mp(id,SZ_PADDING,MP_LFT),
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
                    mt = xpg_get_mp(child,SZ_MARGIN,MP_TOP),
                    ml = xpg_get_mp(child,SZ_MARGIN,MP_LFT),
                    mb = xpg_get_mp(child,SZ_MARGIN,MP_BTM),
                    mr = xpg_get_mp(child,SZ_MARGIN,MP_RGT)
--?{"lmas",child,cw,ch}
--          ctrl_size[child][SZ_W] = cw
--          ctrl_size[child][SZ_H] = ch
            cw = ml+cw+mr
            ch = mt+ch+mb
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
        nw += xpg_get_mp(id,SZ_PADDING,MP_RGT)
        nh += xpg_get_mp(id,SZ_PADDING,MP_BTM)
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
--      if backend=XPG_GTK
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
        if backend=XPG_GTK then
--      if backend=XPG_GTK and ct!=CANVAS then
            atom handle = ctrl_handles[id]
--10/11/23 (as per gGetAttribute/scroller.exw)
--13/11/23 mullered gText.exw...
            c_proc(gtk_widget_size_request,{handle,pGtkRequisition})
            w = get_struct_field(idGtkRequisition,pGtkRequisition,"width")
            h = get_struct_field(idGtkRequisition,pGtkRequisition,"height")
--          atom window = c_func(gtk_widget_get_window,{handle})
--          w = c_func(gdk_window_get_width,{window})
--          h = c_func(gdk_window_get_height,{window})
--DEV bad idea...
--          if ct=BUTTON then
----                if backend=XPG_GTK then
--              if bGTK3 then
--                  w -=16
--                  h -= 8
----                    c_proc(gtk_window_set_default_size,{handle,w,h}) 
--                  c_proc(gtk_widget_set_size_request,{handle,w,h}) 
--              end if
--          end if
--?{"gtknatsize",w,h,title}
        elsif backend=XPG_WINAPI then
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
    elsif not find(ct,{CANVAS,MENU}) then
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

local procedure xpg_check_unmapped(gdx ids, integer flag, rdx=0)
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
--          assert(ctrl_kids[cti]=0)
            assert(and_bits(ctrl_flags[id],CF_CONTAINER)==0)
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
    xpg_check_unmapped(ids,CF_NORMAL)
    norm_groups = append(norm_groups,{ihvb,ids})
end procedure

--DEV not tried... cannot think of any reason not to do this, heck, let's just slap it in.
global constant integer gNormalize = gNormalise;
--global procedure gNormalize(gdx ids, string hvb="BOTH")
--  gNormalise(ids,hvb)
--end procedure

--DEV...
local function xpg_lm_gather_normal_groups(sequence ngused, gdx id)
    -- (determine which if any of the norm_groups actually apply)
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
--if ctrl_kids[ctrl_types[id]]=0 then
if and_bits(ctrl_flags[id],CF_CONTAINER)==0 then
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
    --
    -- set [SZ_NORMAL_W/H] from max([SZ_USER_W/H],[SZ_NATURAL_W/H]), and then
    --  normalise, ie set all rqd w/h in each group to max w/h in each group.
    --
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
--?"xpg_lm_accumulate_sizes"
    --
    -- sum [SZ_NORMAL_W/H] with margins etc.
    --
    integer ct = ctrl_types[id]
--if ct=CANVAS then ?{"ans(CANVAS)",ctrl_size[id][SZ_NATURAL_W],ctrl_size[id][SZ_NATURAL_H]} end if
    object children = children_ids[id]
    if sequence(children) then
--      integer {mt,mr,mb,ml} = xpg_margins(ct),
--              nw = ml, nh = mt, 
--      integer {{nh,mr,mb,nw},gap} = xpg_margap(id),
--      integer nh = xpg_get_mp(id,SZ_PADDING,MP_TOP),
--              nw = xpg_get_mp(id,SZ_PADDING,MP_LFT),
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
--      nw += xpg_get_mp(id,SZ_PADDING,MP_RGT)
--      nh += xpg_get_mp(id,SZ_PADDING,MP_BTM)
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
            if backend=XPG_GTK then
                if bGTK3 then
                    nw += 2
                    nh += 2
                else
                    nw += 4
                    nh += 4
                end if
            elsif backend=XPG_WINAPI then
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
            elsif backend=XPG_WINAPI then
                nh += 9
            end if
        elsif ct=DIALOG then
--local function xpg_get_window_rect(gdx id)
--local function xpg_get_client_rect(gdx id)
--local function xpg_gtk_subtract_dialog_decorations(gdx id, integer dw, dh)
            integer {dw,dh} = xpg_lm_get_dialog_decoration_size(id)
            nw += dw
            nh += dh
        elsif ct=TABS then
--?{"nw",nw,ctrl_xtra[id][CX_TABWIDTH]}
            nw = max(nw,ctrl_xtra[id][CX_TABWIDTH])
            nh += ctrl_xtra[id][CX_TABHIGHT]
        end if
--      if backend=XPG_GTK
--      and (ct=FRAME or ct=BOX or ct=DIALOG) then -- [or, probably, DATEPICK, TABS]
--      if ct=FRAME or ct=BOX or ct=DIALOG then -- [or, probably, DATEPICK/GTK, TABS]
--          ctrl_size[id][SZ_NATURAL_W] = nw
--          ctrl_size[id][SZ_NATURAL_H] = nh
--      end if
--if ct=DIALOG then ?{"lmas",id,nw,nh} end if
--?{"lmas",id,nw,nh}
        ctrl_size[id][SZ_NORMAL_W] = nw
        ctrl_size[id][SZ_NORMAL_H] = nh
--/*
    elsif find(ct,{BUTTON,CHECKBOX,DROPDOWN,LABEL,TEXT}) then
--     or (ct=CANVAS and xpg_sizeable_custom_canvas_control(id)) then
--     or (ct=CANVAS and find(ctrl_xtra[id][CX_??],{BUTTON,CHECKBOX,DROPDOWN,LABEL,TEXT})) then
        integer w, h
        if backend=XPG_GTK then
--      if backend=XPG_GTK and ct!=CANVAS then
            atom handle = ctrl_handles[id]
            c_proc(gtk_widget_size_request,{handle,pGtkRequisition})
            w = get_struct_field(idGtkRequisition,pGtkRequisition,"width")
            h = get_struct_field(idGtkRequisition,pGtkRequisition,"height")
--?{"gtknatsize",w,h,title}
        elsif backend=XPG_WINAPI then
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

-- https://www.joshwcomeau.com/css/interactive-guide-to-grid/
-- https://www.joshwcomeau.com/css/interactive-guide-to-flexbox/
-- older: https://css-tricks.com/snippets/css/a-guide-to-flexbox/
-- hmm: https://flexboxfroggy.com/
-- unrelated: https://www.joshwcomeau.com/operator-lookup/

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

--DEV: this is not taking margins etc into account....
--with trace
-- xpg_lm_apply_spacing? xpg_lm_expand_to_fit?
--local procedure xpg_lm_disperse_user_sizes(gdx id, integer pw=0, ph=0)
local procedure xpg_lm_disperse_user_sizes(gdx id, integer cw, ch)
?{"xpg_lm_disperse_user_sizes",id, cw, ch}
    --
    -- Since GTK insists on being such an absolute dick about such matters,
    -- cw,ch are/must be the **client** size, not the window size.
    -- Mind you, WinAPI is not much better, with its "invisible margins".
    --
    integer w = max(ctrl_size[id][SZ_NORMAL_W],ctrl_size[id][SZ_USER_W]),
            h = max(ctrl_size[id][SZ_NORMAL_H],ctrl_size[id][SZ_USER_H]),
           ct = ctrl_types[id]
    object children = children_ids[id]
    if sequence(children) then
        assert(find(ct,{DIALOG,BOX,FRAME,TABS})) -- sanity check
        integer cfi = ctrl_flags[id]
        bool bVert = and_bits(cfi,CF_VERTICAL)!=0,
             bBoth = ct!=BOX,
            bSplit = ct==BOX and and_bits(cfi,CF_SPLIT)!=0 and 
                     ctrl_xtra[children[2]][CX_GTL_ATTRS][PX_FRAC]!=-1
--DEV flags are 0bVHB, ie 4: vertical, 2: horiontal, 1:both, 0:none
--      -- aside: Technically, 6===1, however I wanted EXPAND=TRUE
        --               to behave exactly the same as EXPAND=BOTH.
        --        Slightly more confusing, but same # of and_bits().
--               CF_EXPAND = CF_EXPANDV+CF_EXPANDH+CF_EXPANDB,                      -- 0b111 mask, with 0/1/2/4 for N/B/H/V
--              CF_HEXPAND = CF_EXPANDH+CF_EXPANDB,                                 -- 0b011 mask (expands horizontally)
--              CF_VEXPAND = CF_EXPANDV+CF_EXPANDB                                  -- 0b101 mask (expands vertically)
--      integer flag = iff(bVert?0b011:0b101)
--      integer flag = iff(bVert?0b101:0b011)
        integer flag = iff(bVert?CF_HEXPAND:CF_VEXPAND)
--      integer flag = iff(bVert?CF_VEXPAND:CF_HEXPAND)
        sequence bexpand = repeat(false,length(children))
        atom aw = 0, ah = 0,    -- all width/height
             ew = 0, eh = 0     -- expandable ""

        -- collect children which expand in the primary direction:
        for i,child in children do
            integer cflags = ctrl_flags[child],
                    ccw = ctrl_size[child][SZ_NORMAL_W],
                    cch = ctrl_size[child][SZ_NORMAL_H]
            aw += ccw
            ah += cch
            if bBoth or and_bits(cflags,flag)!=0 then
                bexpand[i] = true
                ew += ccw
                eh += cch
            end if
        end for
        if bSplit then
            assert(bexpand=={true,false,true})
        end if
        integer wslack = max(0,cw-aw),
                hslack = max(0,ch-ah)
        for i,child in children do
            integer cflags = ctrl_flags[child],
                    ecw = ctrl_size[child][SZ_NORMAL_W],
                    ech = ctrl_size[child][SZ_NORMAL_H]
--          integer xw = iff(bVert and and_bits(cflags,0b101)>0?cw-ecw:0),
            integer xw = iff(bVert and and_bits(cflags,CF_VEXPAND)>0?cw-ecw:0),
--          integer xw = iff(bVert and and_bits(cflags,0b011)>0?cw-ecw:0),
--          integer xw = iff(bVert and and_bits(cflags,CF_HEXPAND)>0?cw-ecw:0),
--                  xh = iff(bVert  or and_bits(cflags,0b011)=0?0:ch-ech)
                    xh = iff(bVert  or and_bits(cflags,CF_HEXPAND)=0?0:ch-ech)
--                  xh = iff(bVert  or and_bits(cflags,0b101)=0?0:ch-ech)
--                  xh = iff(bVert  or and_bits(cflags,CF_VEXPAND)=0?0:ch-ech)
            if bexpand[i] then
                if bVert then
                    if (bBoth or and_bits(cflags,0b011)) and eh!=0 then
--                  if (bBoth or and_bits(cflags,0b101)) and eh!=0 then
                        xh = round((ech/eh)*hslack)
                        hslack -= xh
                        eh -= ech
                    end if
                else -- bHoriz
                    if (bBoth or and_bits(cflags,0b101)) and ew!=0 then
--                  if (bBoth or and_bits(cflags,0b011)) and ew!=0 then
                        xw = round((ecw/ew)*wslack)
                        wslack -= xw
                        ew -= ecw
                    end if
                end if
            end if
            if bSplit then
                bexpand[i] = {ecw+xw,ech+xh} -- (for futher processing below)
            else
                xpg_lm_disperse_user_sizes(child,ecw+xw,ech+xh)
            end if
        end for
        if bSplit then
            gdx {c1,splitter,c2} = children
            atom f = ctrl_xtra[splitter][CX_GTL_ATTRS][PX_FRAC]
            integer bdx = 1+bVert,
                    s1 = bexpand[1][bdx],
                    s2 = bexpand[3][bdx],
                   s12 = s1+s2
            -- modify s1,s2 to match f as closely as possible...
            -- s1+s2 should not change, keep both within MIN/MAX. [DEV]
            ?{"bSplit",bexpand,f,bdx,s1,s2,s1/s12}
            s1 = max(round(f*s12),1)
            s2 = max(s12-s1,1)
            s1 = s12-s2
            ?{"bSplit",bexpand,f,bdx,s1,s2,s1/s12}
            bexpand[1][bdx] = s1
            bexpand[3][bdx] = s2
            for i=1 to 3 do
                integer {sw,sh} = bexpand[i]
                xpg_lm_disperse_user_sizes(children[i],sw,sh)
            end for
        end if
    end if
    -- totally messes up resize in gCanvas.exw under GTK: [FIXED]
    if ct=DIALOG then
        cw += 2
        ch += 32
    end if
-- messed up gCheckbox.exw
-- ...but this has now messed up gSplit.exw
--if backend=XPG_GTK then
--if backend=XPG_GTK and not find(ct,{CANVAS}) then
--for 15puzzle_game: (no help)
--if backend=XPG_GTK and not find(ct,{CANVAS,DIALOG}) then
--?{id,ctrl_names[ct],w,h,cw,ch}
if false then -- oops, bad idea!
    ctrl_size[id][SZ_W] = w
    ctrl_size[id][SZ_H] = h
else
    ctrl_size[id][SZ_W] = cw
    ctrl_size[id][SZ_H] = ch
end if
end procedure

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
    if backend=XPG_GTK then
        -- The g and g3 args can be used when things differ under GTK[3].
        if length(xywhg) then xywh = xywhg end if
        if bGTK3 and length(xywhg3) then xywh = xywhg3 end if
    end if
    bodge[id] = {xywh,bCrash}   
end procedure

--DEV this may (seriously) want merging with xpg_lm_disperse_user_sizes...
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
--              pt = xpg_get_mp(id,SZ_PADDING,MP_TOP),
--              pl = xpg_get_mp(id,SZ_PADDING,MP_LFT)
                pt = xpg_get_mp(id,SZ_MARGIN,MP_TOP),
                pl = xpg_get_mp(id,SZ_MARGIN,MP_LFT)
--DEV/SUG: (need to get expand working first...)
--         spacing = ctrl_xtra[id][CX_BOX_SPACE_(H|V)],
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
        if backend=XPG_WINAPI then
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
            integer w = ctrl_size[child][SZ_W],
                    h = ctrl_size[child][SZ_H]
if child<=length(bodge) and sequence(bodge[child]) then 
    {?,?,w,h} = bodge[child][1]
end if

            if bVert then
                y += h+gap
--              y += ctrl_size[child][SZ_H]+gap
--              y += ctrl_size[child][SZ_H]
--              y += ctrl_size[child][SZ_NATURAL_H]
--              y += ctrl_size[child][SZ_NORMAL_H]+gap
            else
                x += w+gap
--              x += ctrl_size[child][SZ_W]+gap
--              x += ctrl_size[child][SZ_W]
--              x += ctrl_size[child][SZ_NATURAL_W]
--              x += ctrl_size[child][SZ_NORMAL_W]+gap
            end if
        end for
--      if n then       
--      end if
    end if
end procedure

--forward global procedure gRedraw(gdx id, integer flags=0b111)
forward global procedure gRedraw(gdx id, integer flags=0b110)

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
--  if parent!=0 or backend=XPG_WINAPI then
        xpg_handle handle = ctrl_handles[id]
        integer pt = iff(parent?ctrl_types[parent]:0),
                ct = ctrl_types[id]
--      if p and ctrl_kids[ct]=0 then
--          x -= ctrl_size[p][SZ_X]
--          y -= ctrl_size[p][SZ_Y]
--      end if
if ct=DIALOG then ?{"xpg_lm_apply_offsets",id} end if
if id<=length(bodge) then
--if id<=length(bodge) and backend!=XPG_GTK then
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
        if backend=XPG_GTK then
--          if w>0 and h>0 then
--          if w>0 and h>0 and pt!=DIALOG then
--          if false then --DEV gTable: right width, wrong height...
--2/10/23: (DEV: it would probably be much better if we got the size(esp height) right!)
            if ct!=MENU then
--              gtk_widget_set_size_request(widget,width,height);
--?{"xpg_lm_apply_offsets",id,iff(pt=BOX?{x,y}:"n/a"),w,h}
if ct=DIALOG then
    w -= 2; h -= 32;
end if
--?{"lm:gtk_widget_set_size_request",id,w,h}
--19/10/23:
if ct!=DIALOG then              
                c_proc(gtk_widget_set_size_request,{handle,w,h}) 
else
--5/11/23 commenting this out helped gCheckbox.exw a bit...
                c_proc(gtk_widget_set_size_request,{handle,1,1}) 
--?{"gtk_window_resize line 4148",handle}
                c_proc(gtk_window_resize,{handle,w,h}) 
end if
            end if
--          if pt=BOX then
            if pt=BOX or pt=CANVAS then
--?{"lm:gtk_fixed_move",id,x,y}
                c_proc(gtk_fixed_move,{ctrl_handles[parent],handle,x,y})
--          else
--?{"gtk_window_move(o)",id}
--              c_proc(gtk_window_move,{handle,x,y})
--?{"<gtk_window_move(o)",id}
            end if
        elsif backend=XPG_WINAPI then
--          if handle then
--          if pt and handle and w and h then
--          if handle and w and h then
            if handle and w and h and ct!=SPIN then
--              integer flags = SWP_NOZORDER
--NB: using ct completely fouled up the margins in gButton.exw
                integer flags = iff(pt?SWP_NOZORDER:SWP_NOMOVE+SWP_NOZORDER)
--              integer flags = iff(ct=DIALOG?SWP_NOZORDER:SWP_NOMOVE+SWP_NOZORDER)
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
--      w += 2+14; h += 32+7;
        w += 14; h += 7;
    else
--DEV wrong!
        w += 6; h += 24;
    end if
--?{"lm:xSetWindowPos(1)",id,"(nomove)",w,h}
else
--?{"lm:xSetWindowPos(2)",id,x,y,w,h}
end if
                    bool ok = c_func(xSetWindowPos,{handle,NULL,x,y,w,h,flags})
                    assert(ok)
--/*
--              else
--                  integer r = c_func(xGetClientRect,{handle,pRECT})
--                  assert(r!=0)
----                    sequence crect = peek4u({pRECT,4})
--                  integer cw = get_struct_field(idRECT,pRECT,"right"),
--                          ch = get_struct_field(idRECT,pRECT,"bottom")
--                  r = c_func(xGetWindowRect,{handle,pRECT})
--                  assert(r!=0)
----DEV suspect... (not really, but let's just code this way throughout...)
----                    sequence wrect = peek4s({pRECT,4})
--                  integer ww = get_struct_field(idRECT,pRECT,"right")
--                             - get_struct_field(idRECT,pRECT,"left"),
--                          wh = get_struct_field(idRECT,pRECT,"bottom")
--                             - get_struct_field(idRECT,pRECT,"top")
--
----?{"xpg_lm_apply_offsets","crect",crect,cw,ch,"wrect",wrect,ww,wh}
--                  w += ww-cw
--                  h += wh-ch-1
--                  flags = SWP_NOMOVE+SWP_NOZORDER+SWP_NOACTIVATE
--                  ok = c_func(xSetWindowPos,{handle,NULL,0,0,w,h,flags})
--                  assert(ok)
--              end if
--*/
--else
--  ?{"nope",pt,handle,w,h}
            end if
        else
            ?9/0 -- (unknown backend)
        end if
--  end if
--I might yet reinstate this...
--  if parent=0 then
--      gRedraw(id)
--  end if
end procedure

local procedure xpg_defer_attr(gdx id, string name, object v)
--DEV scan for name?
--25/11/23 for gRadio() doc simplification, untested. Erm, not that it actually helps with that in any way, anyway.
--  for i=1 to length(deferred_attr[id]) do
--      if deferred_attr[id][i][1]=name then
--          deferred_attr[id][i][2]=v
--          return
--      end if
--  end for
-- or
    integer k = find(name,vslice(deferred_attr[id],1))
--(might yet still need this...)
--  object did = deferred_attr[id]
--  integer k = find(name,vslice(did,1))
--  did = 0
    if k then
        deferred_attr[id][k][2] = v 
    else
        deferred_attr[id] &= {{name,v}}
    end if
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
    if backend=XPG_GTK then
        c_proc(gtk_widget_set_tooltip_text,{handle,v})
    elsif backend=XPG_WINAPI then
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
--if name="WRAP" then trace(1) end if
    if name="ACTIVE" then
        if string(v) then v = xpg_to_bool(v) end if
        xpg_set_ctrl_flag(id,CF_INACTIVE,not v)
        if not bMapped then
            xpg_defer_attr(id,name,v)
        else
            if backend=XPG_GTK then
                c_proc(gtk_widget_set_sensitive,{handle,v})
            elsif backend=XPG_WINAPI then
-- (or make it an error to enable/disable v/hbox...)
                if handle=NULL then
                    gSetAttribute(children_ids[id],name,v)
                else
                    c_proc(xEnableWindow,{handle,v})
                end if
            else
                ?9/0 -- (unknown backend)
            end if
--DEV
--          if ct=SPIN then gSetAttribute(id+1,name,v) end if
        end if
    elsif name="CANFOCUS" then 
        assert(not find(ct,{DIALOG,BOX,CLIPBOARD,FRAME,TIMER}))
        if string(v) then v = xpg_to_bool(v) end if
        if not bMapped then
            xpg_defer_attr(id,name,v)
        else
            if backend=XPG_GTK then
                c_proc(gtk_widget_set_can_focus,{handle,v})
            elsif backend=XPG_WINAPI then
                atom dwStyle = c_func(xGetWindowLong,{handle,GWL_STYLE})
                dwStyle -= and_bits(dwStyle,WS_TABSTOP)
                if v then dwStyle += WS_TABSTOP end if
                dwStyle = c_func(xSetWindowLong,{handle,GWL_STYLE,dwStyle})
            else
                ?9/0 -- (unknown backend)
            end if
--DEV
--          if ct=SPIN then gSetAttribute(id+1,name,v) end if
        end if
    elsif name="EXPAND" then
--DEV/Sug allow 0..3, or one of "NHVB"... [DONE, but not tested]
--      assert(string(v))
        if string(v) then
--DEV fatal error disabling on a gH/Vbox which is a direct child of a gDialog/gFrame/gTabs. (as per docs) [DONE]
--DEV kill this nonsense: [DONE]
--          bool bFree = false
--          bFree = length(v)>4 and v[-4..-1]="FREE"
--          if bFree then v = v[1..-5] end if
            if v="FALSE" or v="NO" then v="NONE" end if
            if v="TRUE" or v="YES" then v="BOTH" end if
--          integer k = find(v,{"NONE","HORIZONTAL","VERTICAL","BOTH"})
--          integer k = find(v,iff(length(v)=1?"NHVBTY":{"NONE","HORIZONTAL","VERTICAL","BOTH"}))
            integer k = find(v,iff(length(v)=1?"NBHHVTY":{"NONE","BOTH","HORIZONTAL",0,"VERTICAL"}))
--Value: "YES" (both directions), "HORIZONTAL", "VERTICAL", "HORIZONTALFREE", "VERTICALFREE" or "NO".
--Default: "NO". For containers the default is "YES".
--SUG:
--          if k=0 and length(v)=1 then
--              k = find(v[1],"NHVB")
----                if k!=0 then
----                    printf(1,"Warning: gSetAttribute(id,\"EXPAND\",\"%s\"): \"%c\" assumed\n",{v,"NHVB"[k]})
----                end if
--          end if
            assert(k!=0)
--          if k>5 then k=2 end if
--          v = min(k,4)-1
--          v = k-1
            v = iff(k>5?1:k-1)
        else
            if v='F' then v = 'N' end if
            if v='T' or v='Y' then v = 'B' end if
--          if v>0b11 then v = find(v,"NHVB")-1 end if
            if v>0b100 then v = find(v,"NBHHV")-1 end if
--          assert(v>=0 and v<=3)
--          assert(v>=0 and v<=4 and v!=3)
        end if
        assert(v=0b000 or v=0b001 or v=0b010 or v=0b100)
        if ct=BOX and find(ctrl_types[parent_ids[id]],{DIALOG,FRAME,TABS}) then
--          assert(v=0b11,`cannot disable expansion on a "sole child" gH/Vbox`) -- (as per docs)
            assert(v=0b001,`cannot disable expansion on a "sole child" gH/Vbox`) -- (as per docs)
        end if
--DEV xpg_set_ctrl_flag()??
        integer f = ctrl_flags[id]
--      f -= and_bits(f,0b111)
        f -= and_bits(f,CF_EXPAND)
        f += v
        ctrl_flags[id] = f
    elsif name="FONT" then
--      "<face>, <styles> <size>". 
        assert(string(v))
        if not bMapped then
            xpg_defer_attr(id,name,v)
-- (or make it an error to set fonts on a v/hbox...)
        elsif handle=NULL then
            assert(backend=XPG_WINAPI)
            gSetAttribute(children_ids[id],name,v)
        else
            xpg_set_font(id,v)
--          printf(1,"gSetAttribute(%s,\"%s\",face:%s, styles:%v, size:%d)...\n",{ctrl_names[ct],name,face,styles,fontsize})
        end if
-- now done in xpg_set_font:
--      if ct=SPIN then gSetAttribute(id+1,name,v) end if
--  elsif name="GAP" then
--      if string(v) then v = to_number(v) end if
--      assert(ct=BOX)
--      ctrl_xtra[id][CX_BOX_GAP] = v
    elsif name="MARGIN"
       or name="PADDING" then
        if string(v) then v = xpg_intint(v) end if
        assert(ct!=DIALOG,"no margin or padding on dialogs!")
        integer mpdx = iff(name="MARGIN"?SZ_MARGIN:SZ_PADDING)
        if mpdx!=SZ_MARGIN and not find(ct,{BUTTON,DATEPICK,DROPDOWN,TEXT}) then
--          crash("gSetAttribute(%s,%s)",{ctrl_names[ct],name})
            string cn = gGetAttribute(id,"CLASSNAME")
            crash("no PADDING on a %s: you need to set MARGIN instead",{cn})
        end if
        ctrl_size[id][mpdx] = v -- (save as 1..4 integers)
    elsif name="MINSIZE"
       or name="MAXSIZE" then
        if string(v) then v = xpg_intint(v) end if
        integer {w,h} = v,
            {mwx,mhx} = iff(name="MINSIZE"?{SZ_MIN_W,SZ_MIN_H}
                                          :{SZ_MAX_W,SZ_MAX_H})
        ctrl_size[id][mwx] = w
        ctrl_size[id][mhx] = h
--/* DEV/SUG:
        assert(length(v)=2)
        integer mmdx = iff(name="MINSIZE"?SZ_MIN:SZ_MAX)
        ctrl_size[id][mmdx] = v
--*/
        if backend=XPG_GTK then
            if not bMapped then
                xpg_defer_attr(id,name,v)
            else
--DEV: works fine in demo\xpGUI\gtk_fixed.exw, bar MAXSIZE in GTK2[tough!]
                integer {dw,dh} = xpg_lm_get_dialog_decoration_size(id), -- {2,32}
                        minw = max(ctrl_size[id][SZ_MIN_W]-dw,1),
                        minh = max(ctrl_size[id][SZ_MIN_H]-dh,1),
                        maxw = ctrl_size[id][SZ_MAX_W],
                        maxh = ctrl_size[id][SZ_MAX_H],
                        flags = 0
                if minw>1 
                or maxw>1 then
                    flags += GDK_HINT_MIN_SIZE
                    set_struct_field(idGdkGeometry,pGdkGeometry,"min_width",minw)
                    set_struct_field(idGdkGeometry,pGdkGeometry,"min_height",minh)
                end if
                maxw = iff(maxw>dw and maxw>minw?maxw-dw:65535)
                maxh = iff(maxh>dh and maxh>minh?maxh-dh:65535)
                if maxw<65535
                or maxh<65535 then
                    flags += GDK_HINT_MAX_SIZE
                    set_struct_field(idGdkGeometry,pGdkGeometry,"max_width",maxw)
                    set_struct_field(idGdkGeometry,pGdkGeometry,"max_height",maxh)
                end if
                assert(flags!=0)
?{"geom",minw,minh,maxw,maxh,flags}
--wait_key()
--              c_proc(gtk_window_set_geometry_hints,{handle,handle,pGdkGeometry,flags})
            end if 
        end if 
    elsif name="SHRINK" then
        assert(string(v))
        if not bMapped then
            xpg_defer_attr(id,name,v)
        else
-- (dialog only, I think...)
            printf(1,"gSetAttribute(%s,\"%s\",%s)...\n",{ctrl_names[ct],name,v})
            -- erm, a bit more like EXPAND, please, but both better:
            if string(v) then v = xpg_to_bool(v) end if
--DEV: CF_SHRINK is not yet actually used anywhere...
            xpg_set_ctrl_flag(id,CF_SHRINK,v)
        end if
    elsif name="SIZE" then
        if v=NULL or v="NULL" then v = {0,0} 
        elsif string(v) then v = xpg_intint(v) end if
        if integer(v) then v = {v,0}
        elsif length(v)=1 then v = {v[1],0} end if
        integer {width,hight} = v
        ctrl_size[id][SZ_USER_W] = width
        ctrl_size[id][SZ_USER_H] = hight
        if not bMapped then
            xpg_defer_attr(id,name,v)
        else
--DEV NULL should be allowed too?? (above added, but as yet untested)
--DEVtemp:
--if backend=XPG_WINAPI then
--  if ct=DIALOG then width += 14; hight += 7; end if
--end if
            integer nw = ctrl_size[id][SZ_NATURAL_W],
                    nh = ctrl_size[id][SZ_NATURAL_H]
            if ct=CANVAS then -- (specifically for gSplit)
                if width and width<nw then nw = width; ctrl_size[id][SZ_NATURAL_W] = nw end if
                if hight and hight<nh then nh = hight; ctrl_size[id][SZ_NATURAL_H] = nh end if
                -- (aside: next is still needed to deal with any w/h of 0)
            end if
--DEV: gRedraw(gGetDialog(id))
--          if width=0 then width = ctrl_size[id][SZ_NATURAL_W] end if
--          if hight=0 then hight = ctrl_size[id][SZ_NATURAL_H] end if
            width = max(width,nw)
            hight = max(hight,nh)
            if backend=XPG_GTK then
                if ct=DIALOG then
--                  {width,hight} = xpg_gtk_subtract_dialog_decorations(id,width,hight)
                    integer {dw,dh} = xpg_lm_get_dialog_decoration_size(id)
--?{width,hight,"SIZE"}
--                  c_proc(gtk_window_set_default_size,{handle,width,hight}) 
--DEV makes gTable.exw better...
--?{"sa:gtk_window_set_default_size",id,width,-dw,hight,-dh}
--                  c_proc(gtk_window_set_default_size,{handle,width-dw,hight-dh}) 
--                  c_proc(gtk_window_set_default_size,{handle,width,hight-dh}) 
--DEV one of these looks odd...
--?{"gSetAttribute:gtk_window_resize",id,width,-dw,hight,-dh}
--?{"gtk_window_resize line 4581",handle}
                    c_proc(gtk_window_resize,{handle,width-dw,hight-dh}) 
--20/8/23:
                    c_proc(gtk_window_set_default_size,{handle,1,1})
--19/10/23:
                    c_proc(gtk_widget_set_size_request,{handle,1,1})
                elsif ct=BUTTON then
--              elsif ct=BUTTON or ct=CANVAS then
--?{"sa:gtk_window_set_default_size",id,width,hight}
--                  c_proc(gtk_window_set_default_size,{handle,width,hight}) 
--?{"gSetAttribute:gtk_window_resize",id,width,hight}
?{"gtk_window_resize line 4592 (disabled...?)",handle}
--?9/0
--                  c_proc(gtk_window_resize,{handle,width,hight}) 
--                  c_proc(gtk_window_set_default_size,{handle,width-30,hight-10}) 
--                  c_proc(gtk_window_resize,{handle,width,hight}) 
--                  c_proc(gtk_window_resize,{handle,width-30,hight-10}) 
                elsif ct=DROPDOWN
                   or ct=FRAME then
--                 or ct=FRAME
--                 or ct=CANVAS then
--?{"sa:gtk_widget_set_size_request",id,width,hight}
--                  c_proc(gtk_widget_set_size_request,{handle,width,hight}) 
--?{"gSetAttribute:gtk_window_resize",id,width,hight}
--?{"gtk_window_resize line 4604",handle}
                    c_proc(gtk_window_resize,{handle,width,hight}) 

--                  c_proc(gtk_widget_set_size_request,{handle,width-10,hight-30}) 
--                  c_proc(gtk_window_set_default_size,{handle,width,hight}) 
--?"w-10,h-30"
--                  c_proc(gtk_window_set_default_size,{handle,width-10,hight-30}) 
--                  c_proc(gtk_window_resize,{handle,width,hight}) 
--                  c_proc(gtk_window_resize,{handle,width-10,hight-30}) 
--              elsif ct=CANVAS then
--                  c_proc(gtk_widget_set_size_request,{handle,width,hight}) 
--                  c_proc(gtk_widget_set_size_request,{handle,0,0}) 
--                  c_proc(gtk_window_resize,{handle,width,hight}) 
                else
                    -- (probably just need to figure out which of the above works)
                    printf(1,"gSetAttribute[GTK](%s,\"SIZE\",%v)\n",{ctrl_names[ct],v})
--                  ?9/0 -- placeholder
                end if
            elsif backend=XPG_WINAPI then
?{"WINAPISIZE",width,hight,id}
if ct=DIALOG then width+=16 hight += 16 end if
if handle then
                integer flags = SWP_NOMOVE+SWP_NOZORDER+SWP_NOACTIVATE
--DEV more closely matches gtk on a DIALOG... (I suspect GTK is wrong...)
--              bool ok = c_func(xSetWindowPos,{handle,NULL,0,0,width+22,hight+45,flags})
--if ct=DIALOG then width += 34; hight += 7; end if
--if ct=DIALOG then width += 14; hight += 7; end if
--if ct=DIALOG then width += 13; hight += 14; end if
--?{"gSetAttribute(SIZE):",id,width,hight}
--?{"gSetAttribute:xSetWindowPos(3)[saSIZE]",id,"(nomove)",width,hight}
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
            if backend=XPG_GTK then
--DEV this "doubles up" the sample.exw tabs...
                n = c_func(gtk_notebook_get_n_pages,{notebook})
                if n>=posn then
?{"set TABTITLE",n,posn,v}
                    c_proc(gtk_notebook_set_tab_label_text,{notebook,handle,v})
                end if
            elsif backend=XPG_WINAPI then
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
--DEV
--      if ct=SPIN then gSetAttribute(id+1,name,v) end if

--DEV/SUG: NORMALIZE_WIDTH|HEIGHT|BOTH > add to normalisers (and bMapped)
--dict normalizers: key id, data is {group_name[?&"W|H|B"?]}, key is group_name, data is {ids}.
-- (doh, investigate what IUP does...)
--  elsif begins("NORMALIZE",name) then
    elsif name="USER_DATA" then
        user_data[id] = v
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
    -- invoked within gMap() [DEV: inline?]
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

-- (this might one day become a builtin??)
local procedure crash_hat(string msg, txt, integer carat, nFrames=1)
    -- neither txt nor "^ msg" should fall off rhs (79 chars)
    integer lm = carat+1+length(msg)
    if lm>79 then
        txt = "..."&txt[lm-75..$]
        carat -= lm-79
    end if
    if length(txt)>79 then
        txt = txt[1..76]&"..."
    end if
    msg = sprintf("%s\n%s^ %s\n",{txt,repeat(' ',carat-1),msg})
    crash(msg,nFrames:=nFrames)
end procedure


without trace
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
        if attributes[1]='=' then
            if attributes="==" then
                for i=1 to length(args) do
                    {string n, object v} = args[i]
                    gSetAttribute(id,n,v)
                end for
                return
            end if
            sequence names = split(attributes[2..$],',')
            assert(length(names)==length(args),"names/args must be same length")
            for i=1 to length(names) do
                gSetAttribute(id,names[i],args[i])
            end for
            return
        end if
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
                    case '{':
                        delims = "}"
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
                            --DEV/SUG "source line with ^"-style error, if we can:
                            -- maybe crash_hat("bad hex char",attributes,i,1,nFrames)
                            --   ==>    gSetAttributes(dlg,"BGCOLOR=#1234J678")
                            --                                           ^ bad hex char
                            -- or       bad hex char ("...234(>>J<<)678...") if no match
                            -- better:      "...234J678..."
                            --                     ^ bad hex char
                            -- crash_hat() attempts to deliver a source-level error with
                            -- a caret indicating the exact point of failure, however and
                            -- of course a compiled program has no access to source code 
                            -- to point at, so that will be more like a normal crash().
                            -- You provide a code fragment, offset and length, which it
                            -- tries to match against the source code line if it can.
                            -- To keep things simple, there is no auto-sprintf(msg,args)
                            -- option in crash_hat(). For an example see xpGUI.e
--                          assert(find(ch,"#01234567890ABCDEFabcdef")!=0,"bad hex char")
--DEV untested:
                            if find(ch,"#01234567890ABCDEFabcdef")=0
                            or (ch='#' and token!="") then
                                crash_hat("bad hex char",attributes,i-1,2)
                            end if
--                          assert(ch!='#' or token="","multiple # chars?")
--                          if ch='#' and token!="" then
--                              crash_hat("multiple # chars?",attributes,i-1,2)
--                          end if
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
    if sequence(canvas) then
        for c in canvas do
            gCanvasSetBackground(c, colour)
        end for
        return
    end if
    ctrl_xtra[canvas][CX_CANVAS_BACK] = colour
    ctrl_xtra[canvas][CX_CANVAS_FLAGS] ||= CXCF_REPEN
end procedure

global function gCanvasGetBackground(gdx canvas)
    return ctrl_xtra[canvas][CX_CANVAS_BACK]
end function

global procedure gCanvasSetForeground(gdx canvas, atom colour)
    if sequence(canvas) then
--DEV/SUG:
--      for c from canvas do
        for c in canvas do
            gCanvasSetForeground(c, colour)
        end for
        return
    end if
    ctrl_xtra[canvas][CX_CANVAS_FORE] = colour
    ctrl_xtra[canvas][CX_CANVAS_FLAGS] ||= CXCF_REPEN
end procedure

global function gCanvasGetForeground(gdx canvas)
    return ctrl_xtra[canvas][CX_CANVAS_FORE]
end function

local procedure xpg_register_handler(integer ct, string name, sequence sigs)
    setd({ct,name},sigs,handler_sigs)
end procedure

--local function find_opt(sequence sig, rig)
--  if find(sig,rig) then return true end if
--  -- be a little more forgiving with optional args:
--  {integer maxp, integer minp, string sigstr} = sig
--  for ri in rig do
--      if maxp=ri[1]
--      and sigstr=ri[3] then
--          return true
--      end if
--  end for
--  return false
---- or (untried)
----    return find(sig,rig) or find(extract(sig,{1,1,3}),rig)
--end function

--global procedure gSetHandler(gdx id, sequence name, rtn handler=NULL)
global procedure gSetHandler(object id, sequence name, rtn handler=NULL)
    if sequence(id) then
        for i in id do
--          assert(integer(i))
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
        assert(gdx(id))
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
--          sequence sig = get_routine_info(handler,false),         -- (actual)
--                   rig = getdd({ct,name},{"???"},handler_sigs)    --  (rqd)
            string sig = get_routine_info(handler,false)[3]         -- (actual)
            sequence rig = getdd({ct,name},{},handler_sigs)         --  (rqd)
--          if sig!=rig then
            if not find(sig,rig) then
                sequence rig0 = getdd({0,name},{},handler_sigs)     --  (common handlers?)
                if not find(sig,rig0) then
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

local function xpg_collect_child_handlers(gdx id, string name)
    sequence rids = {}
    object c = children_ids[id]
    if sequence(c) then
        for ch in c do
            rtn rid = getd({ch,name},handlers)
--          if rid then rids &= rid end if
            if rid then rids = append(rids,{ch,rid}) end if
--  end for
--  if length(rids)=0 then
--      for ch in children_ids[id] do
            rids &= xpg_collect_child_handlers(ch,name)
        end for
    end if
    return rids
end function

local function gGetInheritedHandler(gdx id, string name)
    rtn rid = getd({id,name},handlers)
    if rid=0 then
        sequence chrid = xpg_collect_child_handlers(id,name)
        integer l = length(chrid)
        if l=1 then
--          rid = chrid[1]
            {id,rid} = chrid[1]
--      elsif l=0 then
        else
            while rid=0 and parent_ids[id] do
                id = parent_ids[id]
                rid = getd({id,name},handlers)
            end while
        end if
    end if
--  while rid=0 and length(children_ids[id])=1 do
--      id = children_ids[id][1]
--      rid = getd({id,name},handlers)
--  end while
    return {id,rid}
--  return rid
end function

local procedure xpg_setID(atom handle, integer id)
    ctrl_handles[id] = handle
    ctrl_flags[id] = or_bits(ctrl_flags[id],CF_MAPPED)
    integer ct = ctrl_types[id]
    assert(ct!=TIMER)
    if backend=XPG_GTK then
        setd(handle,id,GTK_ID_LOOKUP)
    elsif backend=XPG_WINAPI then
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
        if backend=XPG_GTK then
            id = getd(handle,GTK_ID_LOOKUP)
        elsif backend=XPG_WINAPI then
            id = c_func(xGetWindowLong,{handle,GWL_USERDATA})
        else
            ?9/0 -- (unknown backend)
        end if
--DEV might be time to let this one go...
--      assert(iff(id?ctrl_handles[id]=handle
        assert(iff(id?(ctrl_handles[id]=handle or (ctrl_types[id]=SPIN and ctrl_xtra[id]=handle))
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
    if backend=XPG_GTK then
        c_proc(gtk_widget_hide,{handle})
--DEV is dialog?
        if c_func(gtk_window_get_transient_for,{handle})=NULL then
            c_proc(gtk_main_quit)
        end if
    elsif backend=XPG_WINAPI then
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
--  return "unknown"
    return sprintf("unknown(#%x)",c)
end function

global function gGetColourName(atom c)
    if c=-1 then return "-1" end if
    integer k = find(c,known_colour_values)
--  for k in known_colours do
    if k then
--      if c=k[2] then return k[1] end if
        return known_colour_names[k]
--  end for
    end if
    return sprintf("#%06x",c)
end function

local function xpg_get_colour_value(string s)
    integer k = find(s,known_colour_names)
    if k then return known_colour_values[k] end if
    integer clr = to_number(s) -- (typecheck on error)
    return clr
end function

integer key_map = NULL

local function xpg_map_key(integer c)
    if key_map=NULL then
        key_map = new_dict()
        integer kdx
        if backend=XPG_GTK then
            kdx = 4
        elsif backend=XPG_WINAPI then
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

local integer shiftkey = false,
               ctrlkey = false,
                altkey = false

local function xpg_key_handler(gdx id, integer key, bool ctrl, shift, alt)
    -- GTK and WinAPI part
    -- returns true if the key was handled / should not be propagated
    shiftkey = shift    -- keep for gGetGlobal
    ctrlkey = ctrl      -- ""
    altkey = alt
--?{id,key,gGetKeyName(key),ctrl,shift,alt}
--  integer khandler = gGetHandler(id,"KEY",true)
    integer {kid,khandler} = gGetInheritedHandler(id,"KEY")
--  integer khandler
--  {id,khandler} = gGetInheritedHandler(id,"KEY")
    integer res = XPG_DEFAULT
    if khandler then
--      sequence sig = get_routine_info(khandler,false)
        string sig = get_routine_info(khandler,false)[3]
--      if sig={5,5,"FOIIII"} then
        if sig="FOIIII" then
            res = khandler(kid,key,ctrl,shift,alt)
        elsif not alt then
--          if sig={4,4,"FOIII"} then
            if sig="FOIII" then
                res = khandler(kid,key,ctrl,shift)
            elsif (not shift) 
               or (key>=#21 and key<=#7E) 
               or key=VK_POUND then
--              if sig={3,3,"FOII"} then
                if sig="FOII" then
                    res = khandler(kid,key,ctrl)
                elsif not ctrl then
--                  if sig={2,2,"FOI"} then
                    if sig="FOI" then
                        res = khandler(kid,key)
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
        if res=XPG_IGNORE
        or (res=XPG_DEFAULT and not find(key,{VK_ESC,VK_F5})) then
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

local procedure xpg_mousewheel(gdx id, integer direction, bool ctrl, shift, alt)
    -- GTK and WinAPI part
    integer mwandler = gGetHandler(id,"MOUSEWHEEL")
    if mwandler then
        mwandler(id,direction,ctrl,shift,alt)
    end if
end procedure

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

local function xpg_gtk_release_key(atom handle, event, gdx id) -- (GTK only)
    -- (clear ctrl/shiftkey when/if those keys are released)
    assert(id=xpg_getID(handle))
    integer state = get_struct_field(idGdkEventKey,event,"state")
    altkey = and_bits(state,GDK_ALT_MASK)!=0
    ctrlkey = and_bits(state,GDK_CONTROL_MASK)!=0
    shiftkey = and_bits(state,GDK_SHIFT_MASK)!=0
    return false -- (propagate please)
end function

local function xpg_click_handler(gdx id, sequence status, integer x, y)
    -- GTK and WinAPI part
    integer clickhandler = gGetHandler(id,"CLICK")
    if clickhandler then
--      sequence sig = get_routine_info(clickhandler,false)
        string sig = get_routine_info(clickhandler,false)[3]
--      if sig={4,4,"FOPII"} then
        if sig="FOPII" then
            return clickhandler(id,status,x,y)
--      elsif sig={4,4,"POPII"} then
        elsif sig="POPII" then
            clickhandler(id,status,x,y)
            return XPG_IGNORE
--      elsif sig={2,2,"FOP"} then
        elsif sig="FOP" then
            return clickhandler(id,status)
--      elsif sig={2,2,"POP"} then
        elsif sig="POP" then
            clickhandler(id,status)
            return XPG_IGNORE
        else
            ?9/0 -- unknown sig...
        end if
    end if
-- XPG_CONTINUE?
--  return false
    return XPG_CONTINUE
end function

-- DEV probably wants attaching to a few more things...
--static gboolean clicked(GtkWidget *widget, GdkEventButton *event, gpointer user_data)
local function xpg_gtk_click(atom widget, event, gdx id)
    -- GTK only part (WinAPI equivalent in xpg_WinAPI_WndProc)
    assert(id=xpg_getID(widget))
    integer event_type = get_struct_field(idGdkEventButton,event,"event_type")
    if event_type!=GDK_TRIPLE_BUTTON_PRESS then -- (ignore(6), since WinAPI has nothing similar)
        integer btn = get_struct_field(idGdkEventButton,event,"button"),
             button = "LMRXY"[btn],
            pressed = "SD?R"[event_type-3], -- GDK_BUTTON_PRESS..GDK_BUTTON_RELEASE
              state = get_struct_field(idGdkEventButton,event,"state"),
               ctrl = and_bits(state,GDK_CONTROL_MASK)!=0,
              shift = and_bits(state,GDK_SHIFT_MASK)!=0,
                alt = and_bits(state,GDK_ALT_MASK)!=0,
                  x = get_struct_field(idGdkEventButton,event,"x",true),
                  y = get_struct_field(idGdkEventButton,event,"y",true)
        sequence status = {button,pressed,ctrl,shift,alt}
        return xpg_click_handler(id,status,x,y)=XPG_IGNORE
    end if
-- TRUE to stop other handlers from being invoked for the event. FALSE to propagate the event further.
    return false
end function

local procedure xpg_mousemove_handler(gdx id, integer x,y, left,middle,right, ctrl,shift,alt)
    -- GTK and WinAPI part
    integer mousehandler = gGetHandler(id,"MOUSEMOVE")
    if mousehandler then
--      sequence sig = get_routine_info(mousehandler,false)
        string sig = get_routine_info(mousehandler,false)[3]
--      if sig={3,3,"POII"} then
        if sig="POII" then
            mousehandler(id,x,y)
--      elsif sig={6,6,"POIIIII"} then
        elsif sig="POIIIII" then
            mousehandler(id,x,y,left,middle,right)
--      elsif sig={9,9,"POIIIIIIII"} then
        elsif sig="POIIIIIIII" then
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
    -- GTK only part (WinAPI equivalent in xpg_WinAPI_WndProc)
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

--bool bLoopy = false
--/!*
local procedure xpg_resize(gdx id, integer w,h)
?{"xpg_resize",id,w,h}
    --
    -- Common to WinAPI and GTK, check for and invoke RESIZE handler
    --
    -- Since GTK insists on being such an absolute dick about such matters,
    -- w,h are/must be the **client** size, not the window size.
    --
--removed 4/12/23:
--  assert(ctrl_types[id]=DIALOG)
--21/7/23: (no help...erm...)
--  ctrl_size[id][SZ_USER_W] = w
--  ctrl_size[id][SZ_USER_H] = h

    integer cfi = ctrl_flags[id]
--  if and_bits(cfi,CF_IGNORESIZE)=0 then
    if and_bits(cfi,CF_MAPPED)!=0 and
       and_bits(cfi,CF_IGNORESIZE)=0 then
        atom handle = ctrl_handles[id]
--?{"xpg_resize",id,w,h,handle,xpg_get_window_rect(id)}
--if w=0 and h=0 then ?9/0 end if
        ctrl_flags[id] = cfi+CF_IGNORESIZE
--probably lots more to do here...

--(untried, would need to actually apply them as well...)
        xpg_lm_disperse_user_sizes(id,w,h)
--      xpg_lm_calculate_offsets(id)
        xpg_lm_calculate_offsets(id,ctrl_size[id][SZ_X],ctrl_size[id][SZ_Y])
        xpg_lm_apply_offsets(id)
--if backend=XPG_GTK then
--?{"xpg_resize:gtk_window_set_default_size(1,1)",id}
--  c_proc(gtk_window_set_default_size,{handle,1,1})
--end if

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
--?"xpg_gtk_focusinout"
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
    if backend=XPG_GTK then
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
--?{"bOK2",bOK,id}
            assert(bOK)
        end if

    elsif backend=XPG_WINAPI then
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
    if backend=XPG_GTK then
        return gtk_focusid
    elsif backend=XPG_WINAPI then
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
--?{"xpg_WinAPI_resize",id, w, h}
    if nMapDepth!=0 then return end if
    integer ct = ctrl_types[id] 
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
--SUG (untested)
--  nested function ff(atom a)
--      if a and a<=1 then a *= 255 end if
--      return and_bits(a,#FF)
--  end nested function
--  atom colour = #1000000*ff(alpha)
--              +   #10000*ff(red)
--              +     #100*ff(green)
--              +          ff(blue)
--perhaps better:
--  assert(sum(sq_lt({red,green,blue,alpha},0))=0,"no negatives!")
--  integer m = iff(sum(sq_le({red,green,blue,alpha},1))=4?255:1)
--  atom colour = and_bits(m*alpha,#FF)*#1000000 +
--                and_bits(m*red,  #FF)*#10000 + 
--                and_bits(m*green,#FF)*#100 + 
--                and_bits(m*blue, #FF)
--or maybe:
--  atom colour = and_bits(alpha,#FF)
--  for c in {red,green,blue} do
--?     assert(c>=0)
--      colour = colour*#100 + and_bits(c,#FF)
--  end for
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

--function begins_wm(string s) return begins("WM_",s) end function

--function win_msg(integer msg)
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
                                {792,"WM_PRINTCLIENT"}})
        end if
        string s = getd(msg,"",wm_dict)
        if length(s) then res = {s} end if
    end if
    return res
--end function

--function ignorable_message(sequence s, integer msg)
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
--end function
--*/

--DEV: cleanup/rename as xpg_WinAPI_XXX, maybe use gGetBrother() etc, ...

/*
function xpg_tab_is_enterable(gdx id)-- is this control normally enterable?
--/*
integer tmp
    tmp = ObjectType[id]
    if tmp!=StatusBar then
        if tmp!=Window then
            if tmp!=TabItem then
                if tmp!=Menu then
                    if tmp!=MenuItem then
                        return true
                    end if
                end if
            end if
        end if
    end if
    return false
--DEV tryme:
--  return not find(ObjectType[id],{StatusBar,Window,TabItem,Menu,MenuItem})
--*/
    integer ct = ctrl_types[id]
    bool res = not find(ct,{DIALOG,MENU})
--if not find(ct,{}) then
--  printf(1,"xpg_tab_is_enterable(%d[a %s]):%t\n",{id,ctrl_names[ct],res})
--end if
    return res
end function
*/

--/*
--local function xpg_WinAPI_Tab_isFocussable(gdx id) -- can the focus exist on this control right now?
----    bool res = false
----    if hasWS_TABSTOP(ObjectType[id]) then
--  atom hwnd = ctrl_handles[id],
--      style = c_func(xGetWindowLong,{hwnd,GWL_STYLE})
--  bool res = and_bits(style,WS_TABSTOP)!=0
----    if and_bits(style,WS_TABSTOP) then
------      if isEnabled(id) then
------          if isVisible(id) then
----                res = true
------          end if
------      end if
----    end if
----integer ct = ctrl_types[id]
----if not find(ct,{}) then
----    printf(1,"xpg_WinAPI_Tab_isFocussable(%d[a %s]):%t\n",{id,ctrl_names[ct],res})
----end if
--  return res
--end function
--*/

local function xpg_tab_to_next_valid_control(integer id, integer nextsibling, integer owner, bool bPrev)

    if not id then -- 2ndpass, from start
        id = owner
    elsif id=owner and nextsibling then -- 1st pass, hit end so now exit
        return id
    end if

    integer pID = parent_ids[id]
    object siblings

    if pID and nextsibling then
        siblings = children_ids[pID]
        if integer(siblings) then
            return NULL
        end if
        integer index = find(id, siblings)
        if not index then
            return NULL
        end if
        index += iff(bPrev?-1:+1)
        if index<1 or index>length(siblings) then
            return xpg_tab_to_next_valid_control(pID, 1, owner, bPrev)
        end if
        id = siblings[index]
--      if xpg_WinAPI_Tab_isFocussable(id) then
        if gGetAttribute(id,"CANFOCUS") then
            return id
        end if
    end if

    -- loop through all children from first to last, possibly recursively, seeking a valid TAB spot
    siblings = children_ids[id]
    pID = id
    if sequence(siblings) then
        integer {first,last,step} = iff(bPrev?{length(siblings),1,-1}:
                                              {1,length(siblings),+1})
        for i=first to last by step do
            id = siblings[i]
--          if xpg_WinAPI_Tab_isFocussable(id) then
            if gGetAttribute(id,"CANFOCUS") then
                return id
--          elsif xpg_tab_is_enterable(id) then
            elsif not find(ctrl_types[id],{DIALOG,MENU}) then
                id = xpg_tab_to_next_valid_control(id, 0, owner, bPrev) -- go down a level
                if id then
                    return id
                end if
            end if
        end for
    end if

    -- exit, failure here, go up to next level
    return xpg_tab_to_next_valid_control(pID, 1, owner, bPrev)

end function

local procedure xpg_find_next_tab_stop(gdx id, bool shift)
    integer ct = ctrl_types[id],
        window = gGetDialog(id) -- if id is already a window then will return itself
--  bool ctrl = false
--  Tab_moveFocus(id, window, ctrl, bPrev)
--procedure Tab_moveFocus(gdx id, window, bool ctrl, bPrev)
    integer newid = xpg_tab_to_next_valid_control(id, 1, window, shift) -- 1st pass, to start
    if newid=window then
        newid = xpg_tab_to_next_valid_control(0, 0, window, shift) -- 2nd pass, from end
    end if
    if newid!=NULL then
        gSetFocus(newid)
    end if
end procedure

--/*
--procedure Tab_moveFocus(gdx id, window, bool ctrl, bPrev)
--
----/*
--  integer objtype = ObjectType[id],
--          pID = ObjectParent[id],
--          newid
--
--  -- DEAL TO TABITEMS & CONTROLS OWNED BY THEM
--  if objtype=TabItem then
--      if shift then -- move to prev tab
--          newid = Tab_prevValidControl(id, 1, window)
--      elsif ctrl then -- move to next tab
--          newid = Tab_nextValidControl(id, 1, window)
--      else -- cycle through subordinates, NB: ONE WAY!
--          newid = Tab_nextValidControl(id, 0, pID)
--          if newid=pID then
--              newid = id
--          end if
--          setFocus(newid)
--          return
--      end if
--
--  elsif pID and ObjectType[pID]=TabItem then
--      if shift then
--          id = Tab_prevValidControl(pID, 1, window)
--      elsif ctrl then
--          id = Tab_nextValidControl(pID, 1, window)
--      else
--          id = Tab_nextValidControl(id, 1, pID)
--      end if
--      setFocus(id)
--      return
--
--  end if
--
--  -- Attempt to retrieve the next valid Tab spot
--  if not shift then -- forward
--      newid = Tab_nextValidControl(id, 1, window) -- 1st pass, to end
--      if newid=window then
--          newid = Tab_nextValidControl(0, 0, window) -- 2nd pass, from start
--      end if
--
--  else -- backward
--      newid = Tab_prevValidControl(id, 1, window) -- 1st pass, to start
--      if newid=window then
--          newid = Tab_prevValidControl(0, 0, window) -- 2nd pass, from end
--      end if
--
--  end if
----*/
--  integer newid = xpg_tab_to_next_valid_control(id, 1, window, bPrev) -- 1st pass, to start
--  if newid=window then
--      newid = xpg_tab_to_next_valid_control(0, 0, window, bPrev) -- 2nd pass, from end
--  end if
--
--
--  -- if an error happened then simply retrieve the owner window as a last resort
----    if newid=NULL then
----        newid = window
----    end if
--
--  -- for TabItems force tab focus, otherwise set focus as normal
----    setFocus(newid)
--  if newid!=NULL then
--      gSetFocus(newid)
--  end if
--end procedure
--*/

--/*
procedure moveFocusNextAvailable(integer id)

    integer wID = getParentWindow(id)

    if not wID then
        return
    end if

    integer next = Tab_nextValidControl(id, 0, wID)
    if next=wID then
        next = Tab_nextValidControl(0, 0, wID)
    end if

    if next=wID or next=0 then
        return
    end if

    setFocus(next)

end procedure

function proc_KeyDownMessage(integer id, integer msg, atom wParam, atom lParam)

    -- 0 means return 0 upon return to caller
    -- 1 means continue

    if wParam=VK_TAB then -- process tabbing between controls
        if msg or lParam then end if    --DEV suppress warnings

        integer tmp = id

        id = getFocus()

        -- ensure an actual control/window is referenced
        if not id then
            id = getParentWindow(tmp)
            if not id then
                return 1
            end if
        end if

        -- ensure an owner window is present
        integer window = getParentWindow(id) -- if id is already a window then will return itself

--      integer objtype = ObjectType[id]

        integer ctrl = getKeyState(VK_CONTROL),
                shift = getKeyState(VK_SHIFT)

        -- move tab forward or backward depending on the system keys
        Tab_moveFocus(id, window, ctrl, shift)

        return 0

    end if

    return 1

end function

--*/

local function xpg_WinAPI_process_key(integer id, msg, atom wParam, lParam)
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
--                {what,key,gGetKeyName(key),ctrl,shift,alt})
        integer res = xpg_key_handler(id,key,ctrl,shift,alt)
        if res then return 0 end if
        -- GTK equivalent done in xpg_gtk_focus(), ie 
        -- the "focus" event not the "key_press_event":
        if msg=WM_KEYDOWN and key=VK_TAB then
--      if msg=WM_CHAR and key=VK_TAB then  -- (or, maybe...)
            xpg_find_next_tab_stop(id,shift)
        end if
    end if
--*!/
    return true -- (carry on with xCallWindowProc/xDefWindowProc)
end function

local procedure xpg_WinAPI_draw_menu_bar(gdx menu)
    integer pid = parent_ids[menu]
    if pid then
        assert(ctrl_types[pid]=BOX)
        pid = parent_ids[pid]
        assert(ctrl_types[pid]=DIALOG)
        bool bOK = c_func(xDrawMenuBar,{ctrl_handles[pid]})
        assert(bOK)
    end if
end procedure

local integer xpg_create_image_list, -- see xpg_xpm.e
              xpg_winAPI_create_DIB_from_xpm -- "" 
--            xpg_winAPI_size                   -- ""
local atom closed_folder, open_folder, dot  -- for GTK
local atom tree_himl                        -- for WinAPI

--with trace
local function xpg_xpm_callback(object xpm)
    -- low-level operations for xpg_xpm.e (avoids making anything global)
    if backend=XPG_GTK then
--      return c_func(gdk_pixbuf_new_from_xpm_data,{xpg_word_array(xpm)})
--trace(1)
        atom pixbuf = c_func(gdk_pixbuf_new_from_xpm_data,{xpg_word_array(xpm)})
        integer w = c_func(gdk_pixbuf_get_width_,{pixbuf}),
                h = c_func(gdk_pixbuf_get_height_,{pixbuf})
        return {pixbuf,w,h}
    elsif backend=XPG_WINAPI then
        string what = xpm[1]
        if what="NEWLIST" then
            -- create imagelist using the recommended sizes for small icons:
--          return c_func(xImageList_Create,{c_func(xGetSystemMetrics,{SM_CXSMICON}),
--                                           c_func(xGetSystemMetrics,{SM_CYSMICON}),
--                                           ILC_COLOR8+ILC_MASK,1,32})
            return c_func(xImageList_Create,{SM_XICON,SM_YICON,ILC_COLOR8+ILC_MASK,1,32})
        elsif what="NEWDIB" then    
            atom {mem,hdrSize} = xpm[2],
                 hDC = c_func(xGetDC,{0}),      -- Get the screen's device context.
                hDIB = c_func(xCreateDIBitmap, {hDC,                -- create DDB for/compatible with this
                                                mem,                -- info about the DIB to create, eg h*w
                                                CBM_INIT,           -- initialise it please with...
                                                mem+hdrSize,        --      this bitmap,
                                                mem,                --      which has this structure
                                                DIB_RGB_COLORS})    --      and has explicit RGB values
            assert(hDIB!=NULL)
            bool bOK = c_func(xReleaseDC,{0,hDC})
            assert(bOK)
--?{"NEWDIB",hDIB}
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

--/*
-- Hmm, nothing gets the same as mspaint's colour picker (ie the eyedropper icon)....
for i=0 to 30 do
    clr = c_func(xGetSysColor,{i})
    printf(1,"Syscolour(%d) = %08x\n",{i,clr})
end for
Syscolour(0) = 00C8C8C8
Syscolour(1) = 00000000
Syscolour(2) = 00D1B499
Syscolour(3) = 00DBCDBF
Syscolour(4) = 00F0F0F0 === COLOR_MENU
Syscolour(5) = 00FFFFFF
Syscolour(6) = 00646464
Syscolour(7) = 00000000
Syscolour(8) = 00000000
Syscolour(9) = 00000000
Syscolour(10) = 00B4B4B4
Syscolour(11) = 00FCF7F4
Syscolour(12) = 00ABABAB
Syscolour(13) = 00D77800
Syscolour(14) = 00FFFFFF
Syscolour(15) = 00F0F0F0
Syscolour(16) = 00A0A0A0
Syscolour(17) = 006D6D6D
Syscolour(18) = 00000000
Syscolour(19) = 00000000
Syscolour(20) = 00FFFFFF
Syscolour(21) = 00696969
Syscolour(22) = 00E3E3E3
Syscolour(23) = 00000000
Syscolour(24) = 00E1FFFF
Syscolour(25) = 00000000
Syscolour(26) = 00CC6600
Syscolour(27) = 00EAD1B9
Syscolour(28) = 00F2E4D7
Syscolour(29) = 00D77800
Syscolour(30) = 00F0F0F0
--*/

local sequence xpm_texts = {},
               xpm_clrs = {}

local constant XPG_BTN_BG   = #00E1E1E1,
               XPG_MENUBG   = #00F2F2F2,
               XPG_MENUHLT  = #0091C9F7

local function xpg_WinAPI_replace_bgclr(integer imgid, atom bgclr)
    -- if there is already one of this background colour, use that
    sequence ci = xpm_clrs[imgid]
    for prev in ci do
        if prev[2][4]=bgclr then
            return prev
        end if
    end for
    -- else create a new bitmap
    sequence xpm = xpm_texts[imgid],
             img = xpg_winAPI_create_DIB_from_xpm(xpm,xpg_xpm_callback,bgclr),
             res = {"gImage",img,imgid}
    xpm_clrs[imgid] = NULL -- (kill refcount)
    ci = append(ci,res)
    xpm_clrs[imgid] = ci
    return res
end function

global function gImageRGBA(integer width, height, sequence pixels)
    if backend=XPG_GTK then
--      return c_func(gdk_pixbuf_new_from_xpm_data,{xpg_word_array(xpm)})
    elsif backend=XPG_WINAPI then
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
--  return {"gImage",width,height,pixels}
    return {width,height,pixels}
end function

--DEV...
--forward local procedure xpg_Init()
forward procedure xpg_Init()

--global function gImage_from_XPM(sequence xpm, string usage="CANVAS")
global function gImage_from_XPM(sequence xpm, atom transparent_colour=XPG_WHITE)
--global function gImage_from_XPM(sequence xpm, object transparent_colour=XPG_WHITE)
--  if string(transparent_colour) then
--      if transparent_colour="MENU" then
--          transparent_colour = XPG_MENUBG
--      else
--          ?9/0
--      end if
--  end if
    if not bInit then xpg_Init() end if
    if string(xpm) then xpm = split(xpm,'\n') end if
    integer imgid = find(xpm,xpm_texts)
    if imgid then
        if backend=XPG_GTK then
            return xpm_clrs[imgid]
        elsif backend=XPG_WINAPI then
            return xpg_WinAPI_replace_bgclr(imgid,transparent_colour)
        else
            ?9/0
        end if
--      ?9/0 -- placeholder (post killing off usage)
--      return ??[k],clr)
    end if
    xpm_texts = append(xpm_texts,xpm)
    imgid = length(xpm_texts)
    object img
    if backend=XPG_GTK then
--      img = xpg_xpm_callback(xpm) -- (a GtkPixbuf)
        img = xpg_xpm_callback(xpm) -- {GtkPixbuf,w,h}
    elsif backend=XPG_WINAPI then
--/*
--XPG_WHITE
--      integer clr = 0x00FFFFFF
        integer clr = XPG_WHITE
        -- note: several more cases are expected here...
        if usage!="CANVAS" then -- (supports transparency)
            if usage="MENU" then  --    (disny "")
--              clr = c_func(xGetSysColor,{COLOR_MENU})
--              clr = c_func(xGetSysColor,{COLOR_WINDOW})
--              clr = c_func(xGetSysColor,{COLOR_BTNFACE})
--              clr = c_func(xGetSysColorBrush,{COLOR_MENU})
--              clr = c_func(xGetSysColorBrush,{COLOR_WINDOW})
--              clr = c_func(xGetSysColorBrush,{COLOR_BTNFACE})
--              clr = #00FFFFFF
--              clr = #00000000
--              clr = #00F2F2F2
                clr = XPG_MENUBG
            elsif usage="BUTTON" then
--              clr = c_func(xGetSysColor,{COLOR_BTNFACE+1})
--              clr = #00E1E1E1
                clr = XPG_BTN_BG
            else
                crash("unknown xpm usage:%s",{usage})
            end if
        end if -- (usage not canvas)
--*/
        -- img is {hDIB,w,h,t}
        img = xpg_winAPI_create_DIB_from_xpm(xpm,xpg_xpm_callback,transparent_colour)
--      xpm_clrs = append(xpm_clrs,img)
    else
        ?9/0 -- (unknown backend)
    end if
    sequence res = {"gImage",img,imgid}
    if backend=XPG_GTK then
        xpm_clrs = append(xpm_clrs,res)
    elsif backend=XPG_WINAPI then
        xpm_clrs = append(xpm_clrs,{res})
    else
        ?9/0
    end if
    return res
end function

global procedure gImageDraw(sequence src, gdx tgt, atom x=0, y=0)
    assert(src[1]="gImage")
    assert(ctrl_types[tgt]=CANVAS)
    if backend=XPG_GTK then
        atom handle = ctrl_handles[tgt],
             window = c_func(gtk_widget_get_window,{handle}),
              cairo = c_func(gdk_cairo_create,{window}),
--           pixbuf = src[2]
             pixbuf = src[2][1]
--or:       {pixbuf,w,h} = src[2]
        c_proc(gdk_cairo_set_source_pixbuf,{cairo,pixbuf,x,y})
        c_proc(cairo_paint,{cairo})
        c_proc(cairo_fill,{cairo})
        c_proc(cairo_destroy,{cairo})
    elsif backend=XPG_WINAPI then
--See the DEV in gCanvas.exw, when src is a string:
--?"bang"
--      atom {hDIB,w,h,cTrans} = src[2]
--?"crash"
        atom destDC = ctrl_xtra[tgt][CX_CANVAS_HDC],
              srcDC = c_func(xCreateCompatibleDC,{destDC}),
             {hDIB,w,h,cTrans} = src[2],
             prevBM = c_func(xSelectObject,{srcDC,hDIB})
--?"crash"
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

--function to_grey(sequence image)
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
--end function

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

--function xpg_set_menu_attribute(gdx id, string name, object v, bool bMapped)
global procedure gMenuSetAttribute(gdx menu, integer id, string name, object v)
    bool bMapped = and_bits(ctrl_flags[menu],CF_MAPPED)!=0, bOK
    atom handle = ctrl_handles[menu], mh
    integer ct = ctrl_types[menu]
    assert(ct=MENU)
    assert(id!=0)
    if bMapped then
        if backend=XPG_GTK then
            mh = getd({menu,id},GTK_MENU_UPLOOK)
        elsif backend=XPG_WINAPI then
            mh = ctrl_handles[menu]
        else
            ?9/0 -- (unknown backend)
        end if
        if name="TITLE" then
            assert(string(v))
            if backend=XPG_GTK then
                c_proc(gtk_menu_item_set_label,{mh,v})
            elsif backend=XPG_WINAPI then
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
                xpg_WinAPI_draw_menu_bar(menu)
            else
                ?9/0 -- (unknown backend)
            end if
            return
        elsif name="ACTIVE" then
?{"ACTIVE",menu,id}
?def_menu_attr[menu]
            if string(v) then v = xpg_to_bool(v) end if
            if backend=XPG_GTK then
                c_proc(gtk_widget_set_sensitive,{mh,v})
            elsif backend=XPG_WINAPI then
                integer flags = or_bits(MF_BYCOMMAND,MF_GRAYED*(not v)),
                          res = c_func(xEnableMenuItem,{mh,id,flags})
                if res = -1 then -- not a menu item...
                    {mh,id} = getd({menu,id},WINAPI_SUBMENUS)
                    res = c_func(xEnableMenuItem,{mh,id,flags+MF_BYPOSITION})
                end if
                xpg_WinAPI_draw_menu_bar(menu)
            else
                ?9/0 -- (unknown backend)
            end if
            return
        elsif name="CHECKED" then
?{"CHECKED",menu,id}
?def_menu_attr[menu]
            if string(v) then v = xpg_to_bool(v) end if
            if backend=XPG_GTK then
                c_proc(gtk_check_menu_item_set_active,{mh,v})
            elsif backend=XPG_WINAPI then
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
        elsif name="IMAGE" then
--?{"IMAGE",menu,id}
--?def_menu_attr[menu]
--          if string(v) then v = xpg_to_bool(v) end if
            if string(v) then
                v = gImage_from_XPM(v,XPG_MENUBG)
            end if
            assert(v[1]="gImage") -- shd be {"gImage",{GtkPixbuf,w,h},iid} [GTK]
                                  --     or {"gImage",{hDIB,w,h,t},iid} [WinAPI]
            if backend=XPG_GTK then
--DEV DOCS: Note that under GTK xpGUI initially creates a GtkImageMenuItem rather than GtkMenuItem when
--          the menu is first created, hence you //cannot// set an image on a menu item which has not
--          had one from the get-go, you can however replace it.
--?9/0
--              assert(v[1]="gImage") -- shd be {"gImage",GtkPixbuf}
--              atom pixbuf = v[2], -- (a GtkPixBuf)
                atom pixbuf = v[2][1], -- (a GtkPixBuf)
--DEV leak??
                    gtk_img = c_func(gtk_image_new_from_pixbuf,{pixbuf})
                c_proc(gtk_image_menu_item_set_image,{mh,gtk_img})
--              c_proc(gtk_widget_set_sensitive,{mh,v})
            elsif backend=XPG_WINAPI then
--DEV I think we need to store this somewhere...
--              integer flags = or_bits(MF_BYCOMMAND,MF_GRAYED*(not v)),
--                        res = c_func(xEnableMenuItem,{mh,id,flags})
--              if res = -1 then -- not a menu item...
--                  {mh,id} = getd({menu,id},WINAPI_SUBMENUS)
--                  res = c_func(xEnableMenuItem,{mh,id,flags+MF_BYPOSITION})
--              end if
--/*
                if string(v) then
--?1111
--                  v = gImage_from_XPM(v,"MENU")
                    v = gImage_from_XPM(v,XPG_MENUBG)
--erm, no...
--              elsif backend=XPG_WINAPI then
--?2222
--?v
--                  v = xpg_WinAPI_replace_bgclr(v[3],XPG_MENUBG)
--?v
                end if
--*/
--              assert(v[1]="gImage") -- v shd be {"gImage",{hDIB,w,h,t},iid}
                atom hBitmap = v[2][1]
                integer iid = v[3]
                setd({menu,id},iid,WINAPI_MENU_IMGS)
--              bool 
--              bOK = c_func(xSetMenuItemBitmaps,{mh,id,MF_BYCOMMAND,hBitmap,hBitmap});
--              assert(bOK)
                integer misize = get_struct_size(idMENUITEMINFO)
                mem_set(pMENUITEMINFO,0,misize)
                set_struct_field(idMENUITEMINFO,pMENUITEMINFO,"cbSize",misize)
                set_struct_field(idMENUITEMINFO,pMENUITEMINFO,"fMask",MIIM_BITMAP)
                set_struct_field(idMENUITEMINFO,pMENUITEMINFO,"hbmpItem",hBitmap)

--              set_struct_field(idMENUITEMINFO,pMENUITEMINFO,"fMask",MIIM_TYPE)
--              bOK = c_func(xGetMenuItemInfo,{mh,id,byPos,pMENUITEMINFO})
--              assert(bOK)
--              set_struct_field(idMENUITEMINFO,pMENUITEMINFO,"dwTypeData",xpg_raw_string_ptr(v))
                bOK = c_func(xSetMenuItemInfo,{mh,id,false,pMENUITEMINFO})
                assert(bOK)
--              xpg_WinAPI_draw_menu_bar(menu)
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
        if backend=XPG_GTK then
            mh = getd({menu,id},GTK_MENU_UPLOOK)
        elsif backend=XPG_WINAPI then
            mh = ctrl_handles[menu]
        else
            ?9/0 -- (unknown backend)
        end if
    end if
    if name="TITLE" then
        assert(bMapped)
        if backend=XPG_GTK then
            atom pStr = c_func(gtk_menu_item_get_label,{mh})
            return peek_string(pStr)
        elsif backend=XPG_WINAPI then
--?{"gMenuGetattribute(TITLE)",mh,id}
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
            if backend=XPG_GTK then
                return c_func(gtk_widget_get_sensitive,{mh})
            elsif backend=XPG_WINAPI then
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
            if backend=XPG_GTK then
                return c_func(gtk_check_menu_item_get_active,{mh})
            elsif backend=XPG_WINAPI then
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
    if backend=XPG_WINAPI and not bSelected
    and getd_index({pid,id},WIN_MENU_CHECKED)!=NULL then
        bool checked = gMenuGetAttribute(pid,id,"CHECKED")
        gMenuSetAttribute(pid,id,"CHECKED",not checked)
    end if
    integer handler = gGetHandler(pid,"HANDLER")
    assert(handler!=NULL)
--  sequence sig = get_routine_info(handler,false)
    string sig = get_routine_info(handler,false)[3]
    integer res = XPG_DEFAULT -- (anything but XPG_CLOSE)
--  if sig={3,3,"FOII"} then
    if sig="FOII" then
        res = handler(pid,id,bSelected)
--DEV JavaScript as a typeless language would struggle to distinguish these two... and as per "FI" below.
--  elsif sig={2,2,"FII"} then
--      res = handler(id,bSelected)
--  elsif sig={2,2,"FOI"} then
    elsif sig="FOI" then
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
        if res=XPG_CLOSE then
            gHide(gGetDialog(id))
        end if
    end if
    return false
end function

local function xpg_gtk_button_clicked(atom handle, gdx id) -- (GTK only)
    assert(id=xpg_getID(handle))
    return xpg_button_clicked(id)
end function

local procedure xpg_check_changed(gdx id, bool bChecked)
    -- common to WinAPI and GTK
    integer value_changed = gGetHandler(id,"VALUE_CHANGED")
    if value_changed then
        bool bRadio = and_bits(ctrl_flags[id],CF_RADIO)!=0
        if bRadio then
            if bChecked then
--              sequence sig = get_routine_info(value_changed,false)
                string sig = get_routine_info(value_changed,false)[3]
                if sig="PO" then
                    value_changed(id)
                elsif sig="POI" then
                    value_changed(id,bChecked)
                else
                    ?9/0
                end if
            end if
        else
            value_changed(id,bChecked)
        end if
    end if
end procedure

local forward procedure xpg_redraw_scrollbars(gdx canvas)

local procedure xpg_redraw_canvas(gdx canvas, atom back)
    -- (shared code between GTK, via xpg_gtk_canvas_draw,
    --  and WinAPI, via xpg_WinAPI_WndProc and WM_PAINT.)
    integer {w, h} = gGetIntInt(canvas,"SIZE")
    object sbinfo = ctrl_xtra[canvas][CX_SBINFO]
    if sequence(sbinfo) then
        integer mox = sbinfo[SB_SCRLW]-(w-sbinfo[SB_VVISB]*sbinfo[SB_VWIDE]),
                moy = sbinfo[SB_SCRLH]-(h-sbinfo[SB_HVISB]*sbinfo[SB_HHIGH]),
                origx = max(min(sbinfo[SB_ORIGX],mox),0),
                origy = max(min(sbinfo[SB_ORIGY],moy),0)
        sbinfo = {} -- (kill refcount)
        ctrl_xtra[canvas][CX_SBINFO][SB_ORIGX] = origx
        ctrl_xtra[canvas][CX_SBINFO][SB_ORIGY] = origy
    end if
    integer redraw = gGetHandler(canvas,"REDRAW")
--  assert(redraw!=NULL,"gCanvas: MUST have a REDRAW procedure")
--  redraw(canvas)
--DEV temp (for demo\xpGUI\sample.exw)
if redraw!=NULL then
    string sig = get_routine_info(redraw,false)[3]
    if sig="PO" then
        redraw(canvas)
    elsif sig="POII" then
        redraw(canvas,w,h)
    else
        ?9/0
    end if
end if
--DEV gotta be a neater way to do this (and still avoid p2js violations)... [FIXED]
--  if sequence(sbinfo) then xpg_redraw_scrollbars(canvas) end if
--  bool bRS = sequence(ctrl_xtra[canvas][CX_SBINFO])
--  if bRS then xpg_redraw_scrollbars(canvas) end if
--  sbinfo = ctrl_xtra[canvas][CX_SBINFO]
--  sbinfo = iff(sequence(sbinfo)?{}:0)
--  if sequence(sbinfo) then xpg_redraw_scrollbars(canvas) end if
    if and_bits(ctrl_xtra[canvas][CX_CANVAS_FLAGS],CXCF_SCROLL) then
        -- aside: ^^ that flag may well have changed inside redraw()
        xpg_redraw_scrollbars(canvas)
    end if
--?{"gCanvasSetBackground",canvas,gGetColourName(back)}
    gCanvasSetBackground(canvas,back)
end procedure

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
--/*
if ct=TEXT or ct=SPIN then
integer WM_MOVE = 3,
WM_KILLFOCUS = 8,
--WM_SETTEXT = 12, -- maybe...
WM_GETTEXT = 13,
WM_GETTEXTLENGTH = 14,
WM_SETCURSOR = 32,
WM_MOUSEACTIVATE = 33,
WM_GETFONT = 49,
WM_WINDOWPOSCHANGING = 70,
WM_WINDOWPOSCHANGED = 71,
WM_NOTIFYFORMAT = 85,
WM_NCCALCSIZE = 131,
WM_NCHITTEST = 132,
WM_NCPAINT = 133,
WM_NCMOUSEMOVE = 160,
EM_GETSEL = 176,
EM_SETSEL = 177,
EM_GETRECT = 178,
EM_GETLINECOUNT = 186,
EM_LINELENGTH = 193,
EM_LINEFROMCHAR = 201,
EM_POSFROMCHAR = 214,
EM_CHARFROMPOS = 215,
WM_CAPTURECHANGED = 533,
WM_IME_SETCONTEXT = 641,
WM_IME_NOTIFY = 642,
EM_GETOLEINTERFACE = 1084
 if not find(msg,{WM_MOVE,WM_SIZE,WM_SETFOCUS,WM_KILLFOCUS,WM_PAINT,/*WM_SETTEXT,*/WM_GETTEXT,WM_GETTEXTLENGTH,WM_ERASEBKGND,
                  WM_SETCURSOR,WM_SETFONT,WM_GETFONT,WM_WINDOWPOSCHANGING,WM_WINDOWPOSCHANGED,WM_NOTIFYFORMAT,
                  WM_NCCALCSIZE,WM_NCHITTEST,WM_NCPAINT,WM_NCMOUSEMOVE,WM_CAPTURECHANGED,WM_LBUTTONUP,WM_LBUTTONDOWN,
                  WM_MOUSEMOVE,WM_IME_SETCONTEXT,WM_IME_NOTIFY,WM_MOUSELEAVE,EM_GETOLEINTERFACE,WM_MOUSEACTIVATE,
                  EM_GETSEL,EM_SETSEL,EM_GETRECT,EM_GETLINECOUNT,EM_LINELENGTH,EM_LINEFROMCHAR,EM_POSFROMCHAR,EM_CHARFROMPOS}) then
    ?{"sub",id,msg}
 end if
end if
--?{"xpg_WinAPI_SubProc",id,win_msg(msg),ctrl_names[ct]}
--*/
    if msg=WM_SETFOCUS then
        --
        -- save the item being focussed on in the parent window
        -- (quite why this don't work in WndProc is beyond me)
        --
        integer pid = gGetDialog(id)
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
        if xpg_WinAPI_process_key(id,msg,wParam,lParam)=0 then return 0 end if
    --
    -- Everything else could easily go in xpg_WinAPI_WndProc(), but just as 
    -- long as we're not replicating code, we may as well leave it in here.
    --
    elsif msg=BM_SETCHECK and ct=CHECKBOX then
        bool bChecked = wParam=BST_CHECKED
        xpg_check_changed(id,bChecked)

    elsif msg=WM_COMMAND then
        if ct=DROPDOWN then
            if floor(wParam/#10000)=LBN_SELCHANGE then
                integer changed = gGetHandler(id,"CHANGED")
                if changed then
                    changed(id)
                end if
            end if
        end if
--/*
    elsif msg=WM_GETDLGCODE and lParam!=NULL
      and get_struct_field(idMESSAGE,lParam,"message")=WM_KEYDOWN
      and get_struct_field(idMESSAGE,lParam,"wParam")=VK_ESC then
--integer m = get_struct_field(idMESSAGE,lParam,"message"),
--      k = get_struct_field(idMESSAGE,lParam,"wParam")
----?{m,WM_KEYDOWN,k,VK_ESC,"(sub)"}
--  if m=WM_KEYDOWN
--  and k=VK_ESC then
--?"return 4!(s)"
        return DLGC_WANTMESSAGE;
--  end if
--end if
--*/
    end if
    return c_func(xCallWindowProc,{wnd_proc_addr[id],hwnd,msg,wParam,lParam})
end function
local constant WINAPI_SUBPROC_CB = call_back(xpg_WinAPI_SubProc)

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
--if ct=TEXT or ct=SPIN then
--  ?{"winproc",msg,id}
--end if
--/*
--if id!=2 then
--if id=1 then
sequence r = win_msg(msg)
if not ignorable_message(r,msg) then
    ?{"xpg_WinAPI_WndProc",id,msg,r}
end if
--*/
--if not find(msg,{1,3,5,6,7,8,12,15,20,24,28,32,36,48,70,71,85,127,129,13,132,133,134,169,297,312,512,528,641,642,674,792,799,49326}) then
--{"gShow",39''',135,2,326,32' ',{133,294}}
--39 Dialog   892 377 135 326 135 326   0   0  0  {38}       CF_CONTAINER+CF_DECORATED+CF_RESIZE+CF_CLOSE_ON_ESC+CF_MAPPED+CF_UNMAPATTR
--dialog size: {135,326}
--"wasDialogMessage!"
--"wasDialogMessage!"
--WM_GETDLGCODE = 135,
--{"xpg_WinAPI_WndProc",39''',135,27,9827984}
--WM_COMMAND = 273
--{"xpg_WinAPI_WndProc",39''',273,2,0}
--"wasDialogMessage!"
--WM_KEYUP = 257
--{"xpg_WinAPI_WndProc",39''',257,27,-1073676287}
--{"xpg_WinAPI_WndProc",39''',257,27,#C0010001}
--  ?{"xpg_WinAPI_WndProc",id,msg,wParam,lParam}
--end if
--if msg = 135 then return 0x0080 end if -- as next
--if msg = 135 then return 0x0084 end if -- as next
--if msg = 135 then return 0x0004 end if -- but kills tab handling
--if msg = 135 then return 0x0002 end if -- kills tab and esc handling
--if msg = 135 and wParam!=VK_TAB then return 0x0004 end if -- but kills esc handling!!
--if msg = 135 and msg!=NULL and wParam!=VK_TAB then return 0x0004 end if -- but kills esc handling!!
--if msg = 135 and lParam!=NULL
--then
----and get_struct_field(idMESSAGE,pMSG,"message")=WM_KEYDOWN
----and get_struct_field(idMESSAGE,pMSG,"wParam")=VK_ESC then
--integer m = get_struct_field(idMESSAGE,lParam,"message"),
--      k = get_struct_field(idMESSAGE,lParam,"wParam")
----?{m,WM_KEYDOWN,k,VK_ESC}
--  if m=WM_KEYDOWN
--  and k=VK_ESC then
----?"return 4!"
----        return DLGC_WANTMESSAGE;
--      return 0x0004
--  end if
--end if


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
--integer oid = id, oct = ct
        if lParam then -- (see win00inglod.exw for more...)
            id = xpg_getID(lParam)
        end if
        if id then
            ct = ctrl_types[id]
--?{"WM_COMMAND",oid,id,lParam,wParam,oct,ct,TEXT,SPIN,ctrl_types[id-1]}
            if ct=BUTTON then
                -- buttons which are implicitly direct children of the dialog,
                -- because the gVbox() and gHbox() are "virtual" under WinAPI.
                return xpg_button_clicked(id)
            --
            -- It would make no odds to do these here rather than in SubProc. [FLW...]
            --
--          elsif ct=CHECKBOX then
--              bool bChecked = c_func(xSendMessage,{lParam,BM_GETCHECK,0,0})
--              xpg_check_changed(id,bChecked)
--          elsif ct=DROPDOWN and floor(wParam/#10000)=LBN_SELCHANGE then
----printf(1,"DROPDOWN di:%d, wParam:%08x, lParam:%08x\n",{id,wParam,lParam})
--              integer changed = gGetHandler(id,"CHANGED")
--              if changed then
--                  changed(id)
--              end if
            elsif ct=TEXT then
                if id>1 and ctrl_types[id-1]=SPIN then id -= 1 end if
                integer value_changed = gGetHandler(id,"VALUE_CHANGED")
                if value_changed then
                    value_changed(id)
                end if
            elsif ct=DIALOG then
                -- menu command - must be a gDialog(gVbox({gMenu(),...}),...) construct.
--  printf(1,"WndProc WM_COMMAND(%s,%d): id:%d, wParam:%08x, lParam:%08x\n",{ctrl_names[ct],oid,id,wParam,lParam})
                id = children_ids[id][1]
--              assert(ctrl_types[id]=BOX)
                if ctrl_types[id]=BOX then
                    id = children_ids[id][1]
--              assert(ctrl_types[id]=MENU)
                    if ctrl_types[id]=MENU then
                        return xpg_menu_common(id,wParam,false)
                    end if
                end if
--  printf(1,"WndProc WM_COMMAND(%s): id:%d, wParam:%08x, lParam:%08x\n",{ctrl_names[ct],id,wParam,lParam})
            end if
        end if

    elsif msg=WM_MENUSELECT then
        integer flags = floor(wParam/#10000),
                  mid = and_bits(wParam,#FFFF)
        if not and_bits(flags,MF_SYSMENU) then
            if and_bits(flags,MF_POPUP) then
--              mid = c_func(xGetMenuItemID,{lParam,mid})
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
--?{"WM_MENUSELECT",hwnd,wParam,flags,mid,id}
            if tracked_menu then
                id = tracked_menu
            else
                id = children_ids[id][1]
                assert(ctrl_types[id]=BOX)
                id = children_ids[id][1]
            end if
            assert(ctrl_types[id]=MENU)
            if sequence(ctrl_xtra[id]) then
                integer {pmid,previd} = ctrl_xtra[id]
                sequence normal_img = xpg_WinAPI_replace_bgclr(previd,XPG_MENUBG)
                gMenuSetAttribute(id,pmid,"IMAGE",normal_img)
                ctrl_xtra[id] = 0
            end if
            if mid>0 then
                -- (aside: //all// images have an id, btw, since they are leaf-only)
                integer iid = getdd({id,mid},0,WINAPI_MENU_IMGS)
                if iid then
                    sequence highlit_img = xpg_WinAPI_replace_bgclr(iid,XPG_MENUHLT)
                    gMenuSetAttribute(id,mid,"IMAGE",highlit_img)
                    ctrl_xtra[id] = {mid,iid}
                end if
                return xpg_menu_common(id,mid,true)
            end if
        end if

--  elsif msg=BM_SETCHECK and ct=CHECKBOX then
--      bool bChecked = wParam=BST_CHECKED
--      xpg_check_changed(id,bChecked)
--*!/
    elsif msg=WM_CHAR
       or msg=WM_KEYDOWN
       or msg=WM_SYSCHAR
       or msg=WM_SYSKEYDOWN then

        -- Note that the same call is made from SubProc, and we don't even
        -- get these messages here for several child elements of a dialog..
        -- Also, you're on your own with '1'|'!' & 'a'|'A'|Shift|CapsLock,
        --  that is at least as I understand/believe these things to be.

        if xpg_WinAPI_process_key(id,msg,wParam,lParam)=0 then return 0 end if

    elsif msg=WM_MOUSEWHEEL then
        integer delta = sign(floor(wParam/#10000))
        bool ctrl = and_bits(c_func(xGetKeyState,{VK_CTRL}),#8000)!=0,
            shift = and_bits(c_func(xGetKeyState,{VK_SHIFT}),#8000)!=0,
              alt = and_bits(c_func(xGetKeyState,{VK_ALT}),#8000)!=0
--      printf(1,"WM_MOUSEWHEEL: delta:%d, ctrl:%d, shift:%d, alt:%d\n",{delta,ctrl,shift,alt})
        xpg_mousewheel(id,delta,ctrl,shift,alt)

    elsif msg>=WM_LBUTTONDOWN
--    and msg<=WM_MBUTTONDBLCLK then
      and msg<=WM_XBUTTONDOWN then -- (excluding WM_MOUSEWHEEL just handled)
        -- see definitions of WM_LBUTTONDOWN..WM_MBUTTONDBLCLK, 3 groups of 3:
        integer btn = floor((msg-510)/3),
                rb3 = remainder(msg-513,3)+1
        if msg=WM_XBUTTONDOWN then -- (but this is different)
            btn = floor(wParam/#10000)
            assert(btn=1 or btn=2)
            rb3 = 2-(and_bits(btn*#20,wParam)!=0) -- (no dbl)
            btn += 3
        elsif msg=WM_LBUTTONDOWN then
            c_proc(xSetCapture,{hwnd})
        elsif msg=WM_LBUTTONUP then
            c_proc(xReleaseCapture,{})
        end if
        integer button = "LRMXY"[btn],
               pressed = "SRD"[rb3],
--DEV let's have a local routine to deal with this:
                  x = and_bits(lParam,#FFFF),
                  y = floor(lParam/#10000)
        if x>#7FFF then x -= #10000 end if
        if y>#7FFF then y -= #10000 end if
        bool ctrl = and_bits(c_func(xGetKeyState,{VK_CTRL}),#8000)!=0,
            shift = and_bits(c_func(xGetKeyState,{VK_SHIFT}),#8000)!=0,
              alt = and_bits(c_func(xGetKeyState,{VK_ALT}),#8000)!=0
        sequence status = {button,pressed,ctrl,shift,alt}
-- If an application processes this message, it should return zero. (so false is do not propagate)
--      return xpg_click_handler(id,status,x,y)
        return xpg_click_handler(id,status,x,y)!=XPG_IGNORE

    elsif msg=WM_MOUSEMOVE then
        integer xm = and_bits(lParam,#FFFF),
                ym = floor(lParam/#10000)
        bool bLeft = and_bits(wParam,MK_LBUTTON)!=0,
           bMiddle = and_bits(wParam,MK_MBUTTON)!=0,
            bRight = and_bits(wParam,MK_RBUTTON)!=0,
             bCtrl = and_bits(wParam,MK_CONTROL)!=0,
            bShift = and_bits(wParam,MK_SHIFT)!=0,
              bAlt = and_bits(c_func(xGetKeyState,{VK_ALT}),#8000)!=0
--4/12/23. (Dragging a gSplit sizer *is* going to get -ve x/y fairly quicky)
        if xm>#7FFF then xm -= #10000 end if
        if ym>#7FFF then ym -= #10000 end if
        xpg_mousemove_handler(id,xm,ym,bLeft,bMiddle,bRight,bCtrl,bShift,bAlt)

    elsif msg=WM_MOUSELEAVE then
        -- first off, signal there is no longer any need to cancel TME_LEAVE:
        set_struct_field(idTRACKMOUSEEVENT,pTRACKMOUSEEVENT,"dwFlags",0)
        assert(ct=CANVAS)
--NO...
--      xpg_redraw_scrollbars(id)
        bool bOKlm = c_func(xInvalidateRect,{hwnd,NULL,true})
        assert(bOKlm)

    elsif msg=WM_NOTIFY then

        -- get handle & id of the control
        hwnd = get_struct_field(idNMTREEVIEW,lParam,"hdr.hwndFrom")
        id = xpg_getID(hwnd)

        if id then
--          integer ct = ctrl_types[id]
            ct = ctrl_types[id]
            if ct=TREEVIEW then
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
            elsif ct=SPIN then
                wParam = get_struct_field(idNMHDR,lParam,"code")
                if wParam == UDN_DELTAPOS then
                    integer iPos = get_struct_field(idNMUPDOWN,lParam,"iPos"),
                            iDelta = get_struct_field(idNMUPDOWN,lParam,"iDelta"),
                            v = iPos+iDelta
--                  if (value < UD_MIN_POS) { value = UD_MIN_POS; }
--                  if (value > UD_MAX_POS) { value = UD_MAX_POS; }
                    integer value_changed = gGetHandler(id,"VALUE_CHANGED")
                    if value_changed then
                        value_changed(id)
                    end if
                end if
--elsif ct=TEXT then -- nope...
--?"WM_NOTIFYtext"
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

        atom pchdc = c_func(xBeginPaint,{hwnd,pPAINTSTRUCT}),
             pcmdc = c_func(xCreateCompatibleDC,{pchdc}),
            pcback = gCanvasGetBackground(id),
          pchBrush = xpg_WinAPI_get_brush(pcback)
        integer pcr = c_func(xGetClientRect,{hwnd,pRECT})
        assert(pcr!=0)
        integer pcleft = get_struct_field(idRECT,pRECT,"left"),
                 pctop = get_struct_field(idRECT,pRECT,"top"),
               pcwidth = get_struct_field(idRECT,pRECT,"right")-pcleft,
              pcheight = get_struct_field(idRECT,pRECT,"bottom")-pctop
        atom pcbmp = c_func(xCreateCompatibleBitmap,{pchdc,pcwidth,pcheight}),
          oldpcbmp = c_func(xSelectObject,{pcmdc,pcbmp})
        -- (R2_MASKPEN is the only one worth having, in my tests)
--DEV/UPDATE: actually, it's rubbish...
--      integer prev = c_func(xSetROP2,{mdc,R2_MASKPEN})
--      assert(prev=R2_COPYPEN)
        pcr = c_func(xFillRect,{pcmdc,pRECT,pchBrush})
        assert(pcr!=0)
--2/10/23: (OR USE ctrl_fontd[id] = hFont??)
--      integer k = ctrl_font[id]
--      assert(k!=UNDEFINED)
--      atom hFont = cachedfonts[k]
------DEV scan up?? (if this ever triggers)
----        assert(hFont!=NULL and hFont!=UNDEFINED)
--      hFont = c_func(xSelectObject,{mdc,hFont})
        atom pchFont = ctrl_fontd[id],
          pcprevFont = c_func(xSelectObject,{pcmdc,pchFont})

        ctrl_xtra[id][CX_CANVAS_HDC] = pcmdc

--Update: improves gCanvas, but (without flicker) fouls up colours...
--     --> I think every canvas should have a gImage backing, for proper anti-aliasing.
-- Some good advice here: https://stackoverflow.com/questions/60911015/in-c-win32-how-do-i-prevent-the-window-from-flickering
-- from https://stackoverflow.com/questions/25460367/how-do-i-implement-double-buffering-in-the-winapi (linked to from above)
--/*
--case WM_PAINT: 
--  {
--      // skipped the initialization part to preserve space
--      // just copy those, they are irrelevant for your problem
--
--      hdc = BeginPaint(hwnd, &ps);
--
--      // create memory DC and memory bitmap where we shall do our drawing
--
--      HDC memDC = CreateCompatibleDC( hdc );
--
--      // get window's client rectangle. We need this for bitmap creation.
--      RECT rcClientRectangle;
--      GetClientRect( hwnd, &rcClientRect );
--
--      // now we can create bitmap where we shall do our drawing
--      HBITMAP bmp = CreateCompatibleBitmap( hdc, 
--          rcClientRect.right - rcClientRect.left, 
--          rcClientRect.bottom - rcClientRect.top );
--
--      // we need to save original bitmap, and select it back when we are done,
--      // in order to avoid GDI leaks!
--      HBITMAP oldBmp = (HBITMAP)SelectObject( memDC, bmp );
--
--      // now you draw your stuff in memory dc; 
--      // just substitute hdc with memDC in your drawing code, 
--      // like I did below:
--
--      TextOut( memDC, //...
--      TextOut( memDC, //...
--      for (int i = 0; i < 20; i++) 
--      {
--          for (int j = 0; j < 20; j++) 
--          {
--              if (level1[j][i] == '1') 
--              {
--                  SetTextColor( memDC, //...
--                  SetBkColor( memDC, //...
--                  TextOut( memDC, //...
--              }
--              SetBkColor( memDC, //...
--              if (level1[j][i] == '0') 
--              {
--                  SetTextColor( memDC, //...
--                  TextOut( memDC, //...
--              }
--              SetTextColor( memDC, //...
--              if (i == x && j == y)
--                  TextOut( memDC, //...
--              if (level1[j][i] == 'e')
--                  TextOut( memDC, //...
--          }
--      }
--
--      // OK, everything is drawn into memory DC, 
--      // now is the time to draw that final result into our target DC
--
--      BitBlt( hdc, 0, 0, rcClientRect.right - rcClientRect.left, 
--          rcClientRect.bottom - rcClientRect.top, memDC, 0, 0, SRCCOPY );
--
--      // all done, now we need to cleanup
--      SelectObject( memDC, oldBmp ); // select back original bitmap
--      DeleteObject( bmp ); // delete bitmap since it is no longer required
--      DeleteDC( memDC );   // delete memory DC since it is no longer required
--
--      EndPaint(hwnd, &ps);
--      break;
--  }
--*/
--      integer prev = c_func(xSetROP2,{hdc,R2_MASKPEN})
--      integer prev = c_func(xGetROP2,{hdc})
--      bool clear = prev!=R2_MASKPEN
--if true then
--      prev = c_func(xSetROP2,{hdc,R2_MASKPEN})
--      integer prev = c_func(xSetROP2,{hdc,R2_MASKPEN})
--      assert(prev!=0) -- shd be R2_COPYPEN(13) or R2_MASKPEN(9)
--end if
--/*
        integer r = c_func(xGetClientRect,{hwnd,pRECT})
        assert(r!=0)
        atom --hdc = wParam,
             back = gCanvasGetBackground(id),
             hBrush = xpg_WinAPI_get_brush(back)
--if clear then
        r = c_func(xFillRect,{hdc,pRECT,hBrush})
        assert(r!=0)
--end if
--sleep(1)
--*/
        xpg_redraw_canvas(id,pcback)
--/!*
        bool bOKpc = c_func(xBitBlt,{pchdc,0,0,pcwidth,pcheight,pcmdc,0,0,SRCCOPY})
        atom pcpBmp = c_func(xSelectObject,{pcmdc,oldpcbmp}); // select back original bitmap
        assert(pcpBmp==pcbmp)
        c_proc(xDeleteObject,{pcbmp}); // delete bitmap since it is no longer required
        bOKpc = c_func(xDeleteDC,{pcmdc}); // delete memory DC since it is no longer required
        assert(bOKpc)
--*!/
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
--arwendemo/scroller.exw:
        if viewBM=NULL
        or mainX>viewX
        or mainY>viewY then
            {viewX,viewY} = {mainX,mainY}
            viewBM = c_func(xCreateCompatibleBitmap,{mainDC, viewX, viewY})
            {} = c_func(xDeleteObject,{c_func(xSelectObject,{viewDC, viewBM})})
        end if
--*/

-- 31/10/23: a nicely simple fix for flashing during resize of demo\xpGUI\r3d.exw:
--  elsif msg=WM_ERASEBKGND and ct=CANVAS then
    elsif msg=WM_ERASEBKGND then
        --  4/11/23: but introduced a horrible ghosting on guess the number 3.... [FIXED]
        if ct!=DIALOG
        or (length(children_ids[id])=1 and ctrl_types[children_ids[id][1]]=CANVAS) then
--SUG/untried:
--      if (ct!=DIALOG and ct!=CANVAS)
--      or (ct=DIALOG and length(children_ids[id])=1 and ctrl_types[children_ids[id][1]]=CANVAS) then
--?{"WM_ERASEBKGND",ct,gGetAttribute(id,"CLASSNAME")}

-- good stuff: https://stackoverflow.com/questions/53000291/how-to-smooth-ugly-jitter-flicker-jumping-when-resizing-windows-especially-drag
--DEV removing this made no difference... (it /is/ called, tho)
--/*
        integer r = c_func(xGetClientRect,{hwnd,pRECT})
        assert(r!=0)
        atom hdc = wParam,
             back = gCanvasGetBackground(id),
             hBrush = xpg_WinAPI_get_brush(back)
        r = c_func(xFillRect,{hdc,pRECT,hBrush})
        assert(r!=0)
--*/
            return 1 -- (signal done)
--      return 0 -- (signal done) [no diff...]
        end if

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
--28/11/23: window_rect mucked up gButton.exw
            integer {w,h} = xpg_get_client_rect(id)[3..4]
--          integer {w,h} = xpg_get_window_rect(id)[3..4]
?{"WM_SIZE",id,w,h}
            xpg_WinAPI_resize(id,w,h)
--23/11/23 (no help with guess the number 3)
--bool bOK = c_func(xInvalidateRect,{hwnd,NULL,true});
--assert(bOK)
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

--/*
    elsif msg=WM_GETDLGCODE and lParam!=NULL
      and get_struct_field(idMESSAGE,lParam,"message")=WM_KEYDOWN
      and get_struct_field(idMESSAGE,lParam,"wParam")=VK_ESC then
        return DLGC_WANTMESSAGE;
--*/    
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
--DEV to go...:
--      -- unsubclass all subclassed controls
--      for obj_id=1 to length(ctrl_types) do
--          if sub_proc_addr[obj_id]!=UNDEFINED then
--              atom hnd = ctrl_handles[obj_id],
--                   wpa = wnd_proc_addr[obj_id]
--              {} = c_func(xSetWindowLong,{hnd,GWL_WNDPROC,wpa})
--              sub_proc_addr[obj_id] = UNDEFINED
--          end if
--      end for
        return  0

    elsif msg=WM_SETFOCUS then
        -- focus on the saved item rather than the window
        -- (for task switching, eg/ie Alt-Tab)
        if ct=DIALOG then
            integer last_focus = ctrl_xtra[id][CX_LAST_FOCUS]
            if last_focus!=UNDEFINED
            and ctrl_types[last_focus]!=DIALOG then
                c_proc(xSetFocus,{ctrl_handles[last_focus]})
                return 0
            end if
--      else -- (now stored in xpg_WinAPI_SubProc() instead)
        end if
    elsif msg=WM_HSCROLL
       or msg=WM_VSCROLL then
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
-- (no help)
--  elsif msg=WM_CTLCOLORBTN then
--      atom hBrush = xpg_WinAPI_get_brush(#E1E1E1)
--      return hBrush
    elsif msg=WM_CTLCOLOREDIT then
        integer editid = xpg_getID(lParam)
        if ctrl_bg[editid]!=-1 then
--?"WM_CTLCOLOREDIT"
            atom hDC = wParam,
                 clr = ctrl_bg[editid],
                 hBrush = xpg_WinAPI_get_brush(clr)
--          SetTextColor(hdcStatic, RGB(0, 255, 0));
            {} = c_func(xSetBkColor,{hDC,xpg_WinAPI_rgb_to_bgr(clr)})
--          {} = c_func(xSetBkMode,{hDC,TRANSPARENT}) -- (no help)
            return hBrush
        end if
    elsif msg=WM_GETMINMAXINFO then
--      string tMINMAXINFO = """typedef struct tagMINMAXINFO {
--                                POINT ptReserved;
--                                POINT ptMaxSize;
--                                POINT ptMaxPosition;
--                                POINT ptMinTrackSize;
--                                POINT ptMaxTrackSize;
--                              } MINMAXINFO, *PMINMAXINFO, *LPMINMAXINFO;"""
--      idMINMAXINFO = define_struct(tMINMAXINFO)
--arwen:
--  if msg=WM_GETMINMAXINFO and id=Win then
--      poke4(lParam + MINMAXINFO_ptMinTrackSize, {min_width,min_height}) 
--      poke4(lParam + MINMAXINFO_ptMaxTrackSize, {max_width,max_height}) 
--  elsif msg=WM_GETMINMAXINFO then
--      poke4(lParam+MINMAXINFO_ptMinTrackSize+POINT_x,550)
--      poke4(lParam+MINMAXINFO_ptMinTrackSize+POINT_Y,340)
--MINSIZE
--DEV:
--gtk_window_set_resizable(GTK_WINDOW(dialog), FALSE);
--from D: A "modal" dialog (that is, one which freezes the rest of the application from user input), can be created by calling Window.setModal on the dialog. 
--        When using Dialog.newWithButtons you can also pass the GTK_DIALOG_MODAL flag to make a dialog modal.
--        gtk_window_set_modal().

        atom pMM = lParam
--?{"WM_GETMINMAXINFO",id}
        integer minw = ctrl_size[id][SZ_MIN_W],
                minh = ctrl_size[id][SZ_MIN_H],
                maxw = ctrl_size[id][SZ_MAX_W],
                maxh = ctrl_size[id][SZ_MAX_H]
        if minw!=0 then set_struct_field(idMINMAXINFO,pMM,"ptMinTrackSize.x",minw) end if
        if minh!=0 then set_struct_field(idMINMAXINFO,pMM,"ptMinTrackSize.y",minh) end if
        if maxw!=0 then set_struct_field(idMINMAXINFO,pMM,"ptMaxTrackSize.x",maxw) end if
        if maxh!=0 then set_struct_field(idMINMAXINFO,pMM,"ptMaxTrackSize.y",maxh) end if
        return 0
    end if
    return c_func(xDefWindowProc,{hwnd,msg,wParam,lParam})
end function

--with trace
local procedure xpg_Init()
    bInit = true
    handlers = new_dict()   -- key is {gdx,name}, data is integer routine_id
    handler_sigs = new_dict()   -- key is {ct,name}, data is sig
    bool L = platform()=LINUX,
         bWinders = platform()=WINDOWS
    integer MB = machine_bits()
    if backend=XPG_GTK then
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
--?"libs"
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
--?"libs2"
        integer gtk_init_check = define_c_func(GTKLIB,"gtk_init_check",
            {C_PTR,     --  int* argc
             C_PTR},    --  char*** argv
            C_BOOL)     -- gboolean
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
--      cairo_surface_destroy = define_c_proc(CAIRO,"cairo_surface_destroy",
--          {C_PTR})    --  cairo_surface_t *surface
        cairo_translate = define_c_proc(CAIRO,"cairo_translate",
            {C_PTR,     --  cairo_t* cr
             C_DBL,     --  double tx
             C_DBL})    --  double ty
--      gdk_atom_intern = define_c_func(GDKLIB,"gdk_atom_intern",
--          {C_PTR,     --  const gchar* atom_name
--           C_BOOL},   --  gboolean only_if_exists
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
        gdk_color_parse = define_c_func(GDKLIB,"gdk_color_parse",
            {C_PTR,     --  const gchar* spec
             C_PTR},    --  GdkColor* color
            C_BOOL)     -- gboolean
        gdk_cursor_new_for_display = define_c_func(GDKLIB,"gdk_cursor_new_for_display",
            {C_PTR,     --  GdkDisplay* display
             C_INT},    --  GdkCursorType cursor_type
            C_PTR)      -- GdkCursor*
--      gdk_cursor_new_from_name = define_c_func(GDKLIB,"gdk_cursor_new_from_name",
--          {C_PTR,     --  GdkDisplay* display
--           C_PTR},    --  const gchar* name
--          C_PTR)      -- GdkCursor*
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
        gdk_pixbuf_get_height_ = define_c_func(GDKGTP,"gdk_pixbuf_get_height",
            {C_PTR},    --  const GdkPixbuf* pixbuf
            C_INT)      -- int
        gdk_pixbuf_get_pixels_ = define_c_func(GDKGTP,"gdk_pixbuf_get_pixels",
            {C_PTR},    --  const GdkPixbuf* pixbuf
            C_PTR)      -- guchar*
        gdk_pixbuf_get_type = define_c_func(GDKGTP,"gdk_pixbuf_get_type",
            {},         --  void
            C_INT)      -- GType
        gdk_pixbuf_get_width_ = define_c_func(GDKGTP,"gdk_pixbuf_get_width",
            {C_PTR},    --  const GdkPixbuf* pixbuf
            C_INT)      -- int
        gdk_pixbuf_new_from_data = define_c_func(GDKGTP,"gdk_pixbuf_new_from_data",
            {C_PTR,     --  const guchar *data
             C_INT,     --  GdkColorspace colorspace
             C_BOOL,    --  gboolean has_alpha
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
    if bGTK3 then
        gdk_rgba_parse = define_c_func(GDKLIB,"gdk_rgba_parse",
            {C_PTR,         --  GdkRGBA *rgba
             C_PTR},        --  const gchar *spec
            C_BOOL)     -- gboolean

    end if
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
        gdk_window_get_display = define_c_func(GDKLIB,"gdk_window_get_display",
            {C_PTR},    --  GdkWindow *window
            C_PTR)      -- GdkDisplay *
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
             C_BOOL})   --  gboolean invalidate_children
--      gdk_window_move_resize = define_c_proc(GDKLIB,"gdk_window_move_resize",
--          {C_PTR,     --  GdkWindow* window
--           C_INT,     --  gint x
--           C_INT,     --  gint y
--           C_INT,     --  gint width
--           C_INT})    --  gint height
        gdk_window_process_updates = define_c_proc(GDKLIB,"gdk_window_process_updates",
            {C_PTR,     --  GdkWindow* window
             C_BOOL})   --  gboolean update_children
        gdk_window_set_cursor = define_c_proc(GDKLIB,"gdk_window_set_cursor",
            {C_PTR,     --  GdkWindow* window
             C_PTR})    --  GdkCursor* cursor
        gtk_adjustment_new = define_c_func(GTKLIB,"gtk_adjustment_new",
            {C_DBL,     --  gdouble value
             C_DBL,     --  gdouble lower
             C_DBL,     --  gdouble upper
             C_DBL,     --  gdouble step_increment
             C_DBL,     --  gdouble page_increment
             C_DBL},    --  gdouble page_size
            C_PTR)      -- GtkObject*
--      gtk_adjustment_get_upper = define_c_proc(GTKLIB,"gtk_adjustment_get_upper",
--          {C_PTR},    --  GtkAdjustment *adjustment
--           C_DBL)     -- gdouble
--      gtk_adjustment_set_page_size = define_c_proc(GTKLIB,"gtk_adjustment_set_page_size",
--          {C_PTR},    --  GtkAdjustment *adjustment
--           C_DBL)     -- gdouble
--      gtk_adjustment_set_lower = define_c_proc(GTKLIB,"gtk_adjustment_set_lower",
--          {C_PTR,     --  GtkAdjustment *adjustment
--           C_DBL})    --  gdouble lower
--      gtk_adjustment_set_step_increment = define_c_proc(GTKLIB,"gtk_adjustment_set_step_increment",
--          {C_PTR,     --  GtkAdjustment *adjustment
--           C_DBL})    --  gdouble step_increment
--      gtk_adjustment_set_page_increment = define_c_proc(GTKLIB,"gtk_adjustment_set_page_increment",
--          {C_PTR,     --  GtkAdjustment *adjustment
--           C_DBL})    --  gdouble page_increment
--      gtk_adjustment_set_upper = define_c_proc(GTKLIB,"gtk_adjustment_set_upper",
--          {C_PTR,     --  GtkAdjustment *adjustment
--           C_DBL})    --  gdouble upper
--      gtk_adjustment_set_page_size = define_c_proc(GTKLIB,"gtk_adjustment_set_page_size",
--          {C_PTR,     --  GtkAdjustment *adjustment
--           C_DBL})    --  gdouble page_size
--      gtk_adjustment_changed = define_c_proc(GTKLIB,"gtk_adjustment_changed",
--          {C_PTR})    --  GtkAdjustment *adjustment

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
--      gtk_box_new = define_c_func(GTKLIB,"gtk_box_new",
--          {C_INT,     --  GtkOrientation orientation
--           C_BOOL,    --  gboolean homogeneous
--           C_INT},    --  gint spacing
--          C_PTR)      -- GtkWidget*
--      gtk_box_pack_end = define_c_proc(GTKLIB,"gtk_box_pack_end",
--          {C_PTR,     --  GtkBox* box
--           C_PTR,     --  GtkWidget* child
--           C_BOOL,    --  gboolean expand
--           C_BOOL,    --  gboolean fill
--           C_INT})    --  guint padding
        -- aside: for notebook tabs with image only
        gtk_box_pack_start = define_c_proc(GTKLIB,"gtk_box_pack_start",
            {C_PTR,     --  GtkBox* box
             C_PTR,     --  GtkWidget* child
             C_BOOL,    --  gboolean expand
             C_BOOL,    --  gboolean fill
             C_INT})    --  guint padding
        gtk_button_get_label = define_c_func(GTKLIB,"gtk_button_get_label",
            {C_PTR},    --  GtkButton *button
            C_PTR)      -- const gchar *
--      gtk_button_new_with_label = define_c_func(GTKLIB,"gtk_button_new_with_label",
--          {C_PTR},    --  const gchar* label
--          C_PTR)      -- GtkWidget*
        gtk_button_new_with_mnemonic = define_c_func(GTKLIB,"gtk_button_new_with_mnemonic",
            {C_PTR},    --  const gchar* label
            C_PTR)      -- GtkWidget*
        gtk_button_set_image = define_c_proc(GTKLIB,"gtk_button_set_image",
            {C_PTR,     --  GtkButton *button
             C_PTR})    --  GtkWidget *image
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
            C_BOOL)     -- gboolean
        gtk_container_add = define_c_proc(GTKLIB,"gtk_container_add",
            {C_PTR,     --  GtkContainer* container
             C_PTR})    --  GtkWidget *widget
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
--          C_BOOL)     -- gboolean (unreliable anyway, it will either work or not)
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
--GTK3 only:
--      gtk_grid_attach = define_c_proc(GTKLIB,"gtk_grid_attach",
--          {C_PTR,     --  GtkGrid *grid
--           C_PTR,     --  GtkWidget *child
--           C_INT,     --  gint left
--           C_INT,     --  gint top
--           C_INT,     --  gint width
--           C_INT})    --  gint height
--      gtk_grid_new = define_c_func(GTKLIB,"gtk_grid_new",
--          {},         --  (void)
--          C_PTR)      -- GtkWidget*
        -- aside: for notebook tabs with images, only
        gtk_hbox_new = define_c_func(GTKLIB,"gtk_hbox_new",
            {C_BOOL,    --  gboolean homogeneous
             C_INT},    --  gint spacing
            C_PTR)      -- GtkWidget*
        gtk_image_menu_item_new_with_mnemonic = define_c_func(GTKLIB,"gtk_image_menu_item_new_with_mnemonic",
            {C_PTR},    --  const gchar* label
            C_PTR)      -- GtkWidget*
        gtk_image_menu_item_set_always_show_image = define_c_proc(GTKLIB,"gtk_image_menu_item_set_always_show_image",
            {C_PTR,     --  GtkImageMenuItem *image_menu_item
             C_BOOL})   --  gboolean always_show
        gtk_image_menu_item_set_image = define_c_proc(GTKLIB,"gtk_image_menu_item_set_image",
            {C_PTR,     --  GtkImageMenuItem *image_menu_item
             C_PTR})    --  GtkWidget *image
--      gtk_image_menu_item_set_use_stock = define_c_proc(GTKLIB,"gtk_image_menu_item_set_use_stock",
--          {C_PTR,     --  GtkImageMenuItem *image_menu_item
--           C_BOOL})   --  gboolean use_stock
        gtk_image_new_from_pixbuf = define_c_func(GTKLIB,"gtk_image_new_from_pixbuf",
            {C_PTR},    --  GdkPixbuf* pixbuf
            C_PTR)      -- GtkWidget*
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
--      gtk_menu_item_new = define_c_func(GTKLIB,"gtk_menu_item_new",
--          {},         --  void
--          C_PTR)      -- GtkWidget*
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
--      gtk_notebook_append_page = define_c_func(GTKLIB,"gtk_notebook_append_page",
--          {C_PTR,     --  GtkNotebook *notebook
--           C_PTR,     --  GtkWidget   *child
--           C_PTR},    --  GtkWidget   *tab_label
--          C_INT)      -- gint
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
             C_BOOL})   --  gboolean scrollable
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
             C_BOOL})   --  gboolean inverted
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
--  if bGTK3 then
--      gtk_scrollbar_new = define_c_func(GTKLIB,"gtk_scrollbar_new",
--          {C_PTR,     --  GtkOrientation orientation
--           C_PTR},    --  GtkAdjustment *adjustment
--          C_PTR)      -- GtkWidget*
--  else
--      gtk_hscrollbar_new = define_c_func(GTKLIB,"gtk_hscrollbar_new",
--          {C_PTR},    --  GtkAdjustment *adjustment
--          C_PTR)      -- GtkWidget*
--      gtk_vscrollbar_new = define_c_func(GTKLIB,"gtk_vscrollbar_new",
--          {C_PTR},    --  GtkAdjustment *adjustment
--          C_PTR)      -- GtkWidget*
--  end if
--      gtk_scrolled_window_add_with_viewport = define_c_proc(GTKLIB,"gtk_scrolled_window_add_with_viewport",
--          {C_PTR,     --  GtkScrolledWindow *scrolled_window
--           C_PTR})    --  GtkWidget *child
        gtk_scrolled_window_new = define_c_func(GTKLIB,"gtk_scrolled_window_new",
            {C_PTR,     --  GtkAdjustment* hadjustment (NULL)
             C_PTR},    --  GtkAdjustment* vadjustment (NULL)
            C_PTR)      -- GtkWidget*
        gtk_scrolled_window_set_policy = define_c_proc(GTKLIB,"gtk_scrolled_window_set_policy",
            {C_PTR,     --  GtkScrolledWindow* scrolled_window
             C_PTR,     --  GtkPolicyType hscrollbar_policy
             C_PTR})    --  GtkPolicyType vscrollbar_policy
--      gtk_scrolled_window_set_shadow_type = define_c_proc(GTKLIB,"gtk_scrolled_window_set_shadow_type",
--          {C_PTR,     --  GtkScrolledWindow* scrolled_window
--           C_INT})    --  GtkShadowType type
        gtk_separator_menu_item_new = define_c_func(GTKLIB,"gtk_separator_menu_item_new",
            {},         --  (void)
            C_PTR)      -- GtkWidget*
        gtk_spin_button_get_increments = define_c_proc(GTKLIB,"gtk_spin_button_get_increments",
            {C_PTR,     --  GtkSpinButton *spin_button
             C_PTR,     --  double* step
             C_PTR})    --  double* page
        gtk_spin_button_get_range = define_c_proc(GTKLIB,"gtk_spin_button_get_range",
            {C_PTR,     --  GtkSpinButton *spin_button
             C_PTR,     --  double* min
             C_PTR})    --  double* max
        gtk_spin_button_get_value = define_c_func(GTKLIB,"gtk_spin_button_get_value",
            {C_PTR},    --  GtkSpinButton* spin_button
            C_DBL)      -- double
        gtk_spin_button_get_wrap = define_c_func(GTKLIB,"gtk_spin_button_get_wrap",
            {C_PTR},    --  GtkSpinButton* spin_button
            C_BOOL)     -- gboolean
        gtk_spin_button_new_with_range = define_c_func(GTKLIB,"gtk_spin_button_new_with_range",
            {C_DBL,     --  gdouble min
             C_DBL,     --  gdouble max
             C_DBL},    --  gdouble step
            C_PTR)      -- GtkWidget*
        gtk_spin_button_set_increments = define_c_proc(GTKLIB,"gtk_spin_button_set_increments",
            {C_PTR,     --  GtkSpinButton *spin_button
             C_DBL,     --  gdouble step
             C_DBL})    --  gdouble page
        gtk_spin_button_set_range = define_c_proc(GTKLIB,"gtk_spin_button_set_range",
            {C_PTR,     --  GtkSpinButton *spin_button
             C_DBL,     --  gdouble min
             C_DBL})    --  gdouble max
        gtk_spin_button_set_value = define_c_proc(GTKLIB,"gtk_spin_button_set_value",
            {C_PTR,     --  GtkSpinButton *spin_button
             C_DBL})    --  double value
        gtk_spin_button_set_wrap = define_c_proc(GTKLIB,"gtk_spin_button_set_wrap",
            {C_PTR,     --  GtkSpinButton *spin_button
             C_BOOL})   --  gboolean wrap
--/*
--double
--gtk_spin_button_get_value (
--  GtkSpinButton* spin_button
--)
--*/

    if bGTK3 then
        gtk_style_context_add_provider = define_c_proc(GTKLIB,"gtk_style_context_add_provider",
            {C_PTR,     --  GtkStyleContext *context
             C_PTR,     --  GtkStyleProvider *provider
             C_PTR})    --  guint priority
        gtk_style_context_add_provider_for_screen = define_c_proc(GTKLIB,"gtk_style_context_add_provider_for_screen",
            {C_PTR,     --  GdkScreen* screen
             C_PTR,     --  GtkCssProvider* css_provider
             C_INT})    --  guint priority
--          false)      -- bCrash (3.0+ only)
    end if
--multiline?
--      gtk_text_view_new = define_c_func(GTKLIB,"gtk_text_view_new",
--          {},         --  void
--          C_PTR)      -- GtkWidget*
        gtk_toggle_button_get_active = define_c_func(GTKLIB,"gtk_toggle_button_get_active",
            {C_PTR},    --  GtkToggleButton *toggle_button
            C_BOOL)     -- gboolean
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
            C_BOOL)     -- gboolean
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
            C_BOOL)     -- gboolean
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
--          C_BOOL)     -- gboolean
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
            C_BOOL)     -- gboolean
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
             C_BOOL})   --  gboolean expand
        gtk_tree_view_column_add_attribute = define_c_proc(GTKLIB,"gtk_tree_view_column_add_attribute",
            {C_PTR,     --  GtkTreeViewColumn* tree_column
             C_PTR,     --  GtkCellRenderer* cell_renderer
             C_PTR,     --  const gchar* attribute
             C_INT})    --  gint column
        gtk_tree_view_collapse_row = define_c_proc(GTKLIB,"gtk_tree_view_collapse_row",
            {C_PTR,     --  GtkTreeView* tree_view
             C_PTR})    --  GtkTreePath* path
--          C_BOOL)     -- gboolean
        gtk_tree_view_expand_row = define_c_proc(GTKLIB,"gtk_tree_view_expand_row",
            {C_PTR,     --  GtkTreeView* tree_view
             C_PTR,     --  GtkTreePath* path
             C_BOOL})   --  gboolean open_all (children, recursively: false==just immediate children)
--          C_BOOL)     -- gboolean
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
             C_BOOL})   --  gboolean enable_search
        gtk_tree_view_set_enable_tree_lines = define_c_proc(GTKLIB,"gtk_tree_view_set_enable_tree_lines",
            {C_PTR,     --  GtkTreeView* tree_view
             C_BOOL})   --  gboolean enabled
--      gtk_tree_view_set_grid_lines = define_c_proc(GTKLIB,"gtk_tree_view_set_grid_lines",
--          {C_PTR,     --  GtkTreeView* tree_view
--           C_INT})    --  GtkTreeViewGridLines grid_lines
        gtk_tree_view_set_headers_visible = define_c_proc(GTKLIB,"gtk_tree_view_set_headers_visible",
            {C_PTR,     --  GtkTreeView* tree_view
             C_BOOL})   --  gboolean headers_visible
        gtk_tree_view_set_model = define_c_proc(GTKLIB,"gtk_tree_view_set_model",
            {C_PTR,     --  GtkTreeView* tree_view
             C_PTR})    --  GtkTreeModel* model
--      gtk_tree_view_set_search_column = define_c_proc(GTKLIB,"gtk_tree_view_set_search_column",
--          {C_PTR,     --  GtkTreeView* tree_view
--           C_INT})    --  gint column
--      gtk_vbox_new = define_c_func(GTKLIB,"gtk_vbox_new",
--          {C_BOOL,    --  gboolean homogeneous
--           C_INT},    --  gint spacing
--          C_PTR)      -- GtkWidget*
--      gtk_viewport_new = define_c_func(GTKLIB,"gtk_viewport_new",
--          {C_PTR,     --  GtkAdjustment *hadjustment
--           C_PTR},    --  GtkAdjustment *vadjustment
--          C_PTR)      -- GtkWidget*
--      gtk_viewport_set_shadow_type = define_c_proc(GTKLIB,"gtk_viewport_set_shadow_type",
--          {C_PTR,     --  GtkViewport *viewport
--           C_INT})    --  GtkShadowType type
--      gtk_widget_get_allocated_height = define_c_func(GTKLIB,"gtk_widget_get_allocated_height",
--          {C_PTR},    --  GtkWidget* widget
--          C_INT)      -- int
--      gtk_widget_get_allocated_width = define_c_func(GTKLIB,"gtk_widget_get_allocated_width",
--          {C_PTR},    --  GtkWidget* widget
--          C_INT)      -- int
--      gtk_widget_get_allocation = define_c_proc(GTKLIB,"gtk_widget_get_allocation",
--          {C_PTR,     --  GtkWidget *widget
--           C_PTR})    --  GtkAllocation *allocation (aka a GtkRectange)
        gtk_widget_get_can_focus = define_c_func(GTKLIB,"gtk_widget_get_can_focus",
            {C_PTR},    --  GtkWidget* widget
            C_BOOL)     -- gboolean
--      gtk_widget_get_root_window = define_c_func(GTKLIB,"gtk_widget_get_root_window",
--          {C_PTR},    --  GtkWidget* widget
--          C_PTR)      -- GdkWindow*
--      gtk_widget_get_parent = define_c_func(GTKLIB,"gtk_widget_get_parent",
--          {C_PTR},    --  GtkWidget* widget
--          C_PTR)      -- GtkWidget*
        gtk_widget_get_pointer = define_c_proc(GTKLIB,"gtk_widget_get_pointer",
            {C_PTR,     --  GtkWidget *widget
             C_PTR,     --  gint* x
             C_PTR})    --  gint* y
        gtk_widget_get_sensitive = define_c_func(GTKLIB,"gtk_widget_get_sensitive",
            {C_PTR},    --  GtkWidget* widget
            C_BOOL)     -- gboolean
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
            C_BOOL)     -- gboolean
        gtk_widget_hide = define_c_proc(GTKLIB,"gtk_widget_hide",
            {C_PTR})    --  GtkWindow* window,  // aka handle
--      gtk_widget_is_sensitive = define_c_func(GTKLIB,"gtk_widget_is_sensitive",
--          {C_PTR},    --  GtkWidget* widget
--          C_BOOL)     -- gboolean
        gtk_widget_modify_base = define_c_proc(GTKLIB,"gtk_widget_modify_base",
            {C_PTR,     --  GtkWidget* widget
             C_INT,     --  GtkStateType state
             C_PTR})    --  const GdkColor *color
--      gtk_widget_modify_bg = define_c_proc(GTKLIB,"gtk_widget_modify_bg",
--          {C_PTR,     --  GtkWidget* widget
--           C_INT,     --  GtkStateType state
--           C_PTR})    --  const GdkColor *color
    if bGTK3 then
        gtk_widget_override_background_color = define_c_proc(GTKLIB,"gtk_widget_override_background_color",
            {C_PTR,     --  GtkWidget* widget
             C_INT,     --  GtkStateFlags state
             C_PTR})    --  const GdkRGBA *color
        gtk_widget_override_font = define_c_proc(GTKLIB,"gtk_widget_override_font",
            {C_PTR,     --  GtkWidget* widget
             C_PTR})    --  PangoFontDescription *font_desc
    else
        gtk_widget_modify_font = define_c_proc(GTKLIB,"gtk_widget_modify_font",
            {C_PTR,     --  GtkWidget* widget
             C_PTR})    --  PangoFontDescription *font_desc
    end if
--      gtk_widget_modify_style = define_c_proc(GTKLIB,"gtk_widget_modify_style",
--          {C_PTR,     --  GtkWidget* widget
--           C_PTR})    --  GtkRcStyle* style
        gtk_widget_queue_draw = define_c_proc(GTKLIB,"gtk_widget_queue_draw",
            {C_PTR})    --  GtkWidget* widget
--      gtk_widget_queue_draw_area = define_c_proc(GTKLIB,"gtk_widget_queue_draw_area",
--          {C_PTR,     --  GtkWidget *widget
--           C_INT,     --  gint x
--           C_INT,     --  gint y
--           C_INT,     --  gint width
--           C_INT})    --  gint height
        gtk_widget_realize = define_c_proc(GTKLIB,"gtk_widget_realize",
            {C_PTR})    --  GtkWidget* widget
        gtk_widget_set_can_focus = define_c_proc(GTKLIB,"gtk_widget_set_can_focus",
            {C_PTR,     --  GtkWidget* widget
             C_BOOL})   --  gboolean can_focus
        gtk_widget_set_events = define_c_proc(GTKLIB,"gtk_widget_set_events",
            {C_PTR,     --  GtkWidget* widget
             C_INT})    --  gint events
--GTK3?
--    if bGTK3 then
--      gtk_widget_set_halign = define_c_proc(GTKLIB,"gtk_widget_set_halign",
--          {C_PTR,     --  GtkWidget* widget
--           C_INT})    --  GtkAlign align
--/*
typedef enum
{
  GTK_ALIGN_FILL,
  GTK_ALIGN_START,
  GTK_ALIGN_END,
  GTK_ALIGN_CENTER
} GtkAlign;
--*/
--    else  
        gtk_misc_set_alignment = define_c_proc(GTKLIB,"gtk_misc_set_alignment",
            {C_PTR,     --  GtkMisc *misc
             C_FLOAT,   --  gfloat xalign
             C_FLOAT})  --  gfloat yalign
--    end if
        gtk_widget_set_realized = define_c_proc(GTKLIB,"gtk_widget_set_realized",
            {C_PTR,     --  GtkWidget* widget
             C_BOOL})   --  gboolean realized
        gtk_widget_set_sensitive = define_c_proc(GTKLIB,"gtk_widget_set_sensitive",
            {C_PTR,     --  GtkWidget* widget
             C_BOOL})   --  gboolean sensitive
        -- Aside: This is an "asshole function" that GTK largely ignores and turns into "geometry contraints".
        --        Should only be used to set wxh to 1x1 to tell the stupid GTK geometry manager to "fuck off".
        gtk_widget_set_size_request = define_c_proc(GTKLIB,"gtk_widget_set_size_request",
            {C_PTR,     --  GtkWidget* widget   // aka handle
             C_INT,     --  gint width,
             C_INT})    --  gint height
        gtk_widget_set_tooltip_text = define_c_proc(GTKLIB,"gtk_widget_set_tooltip_text",
            {C_PTR,     --  GtkWidget* widget   // aka handle
             C_PTR})    --  const gchar *text
--      gtk_widget_set_usize = define_c_proc(GTKLIB,"gtk_widget_set_usize",
--          {C_PTR,     --  GtkWidget* widget
--           C_INT,     --  gint width
--           C_INT})    --  gint height
--  if bGTK3 then
--      gtk_widget_set_hexpand = define_c_proc(GTKLIB,"gtk_widget_set_hexpand",
--          {C_PTR,     --  GtkWidget* widget
--           C_BOOL})   --  gboolean expand
--      gtk_widget_set_vexpand = define_c_proc(GTKLIB,"gtk_widget_set_vexpand",
--          {C_PTR,     --  GtkWidget* widget
--           C_BOOL})   --  gboolean expand
--  end if
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
        -- obtain the preferred size of a widget:
        gtk_widget_size_request = define_c_proc(GTKLIB,"gtk_widget_size_request",
            {C_PTR,     --  GtkWidget *widget
             C_PTR})    --  GtkRequisition *requisition
--  end if
--      gtk_window_set_position = define_c_proc(GTKLIB,"gtk_window_set_position",
--          {C_PTR,     --  GtkWindow *window
--           C_INT})    --  GtkWindowPosition position
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
--          C_BOOL)     -- gboolean
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
            {C_PTR,     --  GtkWindow* window   // aka handle
             C_INT,     --  gint width
             C_INT})    --  gint height
        gtk_window_set_geometry_hints = define_c_proc(GTKLIB,"gtk_window_set_geometry_hints",
            {C_PTR,     --  GtkWindow* window
             C_PTR,     --  GtkWidget *geometry_widget
             C_PTR,     --  GdkGeometry *geometry
             C_INT})    --  GdkWindowHints geom_mask
--GTK2 only...
--      gtk_window_set_policy = define_c_proc(GTKLIB,"gtk_window_set_policy",
--          {C_PTR,     --  GtkWindow* window,
--           C_INT,     --  gint allow_shrink
--           C_INT,     --  gint allow_grow
--           C_INT})    --  gint auto_shrink
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
        -- use this instead of g_object_set(), which is a null-terminated vararg, one at a time.
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
            C_BOOL)     -- gboolean
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
        pango_layout_get_pixel_extents = define_c_proc(PANGO,"pango_layout_get_pixel_extents",
            {C_PTR,     --  PangoLayout *layout
             C_PTR,     --  PangoRectangle* ink_rect
             C_PTR})    --  PangoRectangle* logical_rect
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
                                         GdkEventType event_type;
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

--                                    GdkModifierType* state;
--                                    GdkScrollDirection direction;
--                                    guint is_stop : 1;
        string tGdkEventScroll = """typedef struct GdkEventScroll {
                                      GdkEventType event_type;
                                      GdkWindow* window;
                                      gint8 send_event;
                                      guint32 time;
                                      gdouble x;
                                      gdouble y;
                                      guint state;
                                      guint direction;
                                      GdkDevice* device;
                                      gdouble x_root;
                                      gdouble y_root;
                                      gdouble delta_x;
                                      gdouble delta_y;
                                      guint is_stop;
                                    };"""
        idGdkEventScroll = define_struct(tGdkEventScroll)

        string tGdkEventCrossing = """typedef struct GdkEventCrossing {
                                        GdkEventType event_type;
                                        GdkWindow* window;
                                        gint8 send_event;
                                        GdkWindow* subwindow;
                                        guint32 time;
                                        gdouble x;
                                        gdouble y;
                                        gdouble x_root;
                                        gdouble y_root;
                                        GdkCrossingMode mode;
                                        GdkNotifyType detail;
                                        gboolean focus;
                                        GdkModifierType* state;
                                      };"""
        idGdkEventCrossing = define_struct(tGdkEventCrossing)

        string tGdkColor = """typedef struct GdkColor {
                               guint32 pixel;
                               guint16 red;
                               guint16 green;
                               guint16 blue;
                              };"""
        idGdkColor = define_struct(tGdkColor)

        string tGdkRGBA = """typedef struct GdkRGBA {
                              gdouble red;
                              gdouble green;
                              gdouble blue;
                              gdouble alpha;
                             };"""
        idGdkRGBA = define_struct(tGdkRGBA)

        string tGdkGeometry = """typedef struct GdkGeometry {
                                  gint min_width;
                                  gint min_height;
                                  gint max_width;
                                  gint max_height;
                                  gint base_width;
                                  gint base_height;
                                  gint width_inc;
                                  gint height_inc;
                                  gdouble min_aspect;
                                  gdouble max_aspect;
                                  gint win_gravity;
                                };"""
--                                GdkGravity win_gravity;
        idGdkGeometry = define_struct(tGdkGeometry)

        pData = allocate(machine_word())
        pRECT = allocate_struct(idGdkRectangle)
        pGDKCOLOR = allocate_struct(idGdkColor)
        pGDKRGBA = allocate_struct(idGdkRGBA)
        pGdkGeometry = allocate_struct(idGdkGeometry)
--SUG [local] get_field_ptr(pRECT,idGdkRectangle,"x")
        pX = pRECT+get_field_details(idGdkRectangle,"x")[1]
        pY = pRECT+get_field_details(idGdkRectangle,"y")[1]
        pW = pRECT+get_field_details(idGdkRectangle,"width")[1]
        pH = pRECT+get_field_details(idGdkRectangle,"height")[1]
        pGtkRequisition = allocate_struct(idGtkRequisition)
        GTK_ID_LOOKUP = new_dict() -- GTK only (key:handle, data:id)
        GTK_MENU_LOOKUP = new_dict() -- GTK only (key:handle, data:id)
        GTK_MENU_UPLOOK = new_dict() -- GTK only (key:{menu,id}, data:handle)
        GDK_TYPE_PIXBUF = c_func(gdk_pixbuf_get_type,{})
--trace(1)
--      {closed_folder, open_folder, dot} = xpg_create_image_list("GTK",xpg_xpm_callback)
        {closed_folder, open_folder, dot} = vslice(xpg_create_image_list("GTK",xpg_xpm_callback),1)
--sequence il = xpg_create_image_list("GTK",xpg_xpm_callback),
--il1 = vslice(il,1)
--      {closed_folder, open_folder, dot} = il1
    elsif backend=XPG_WINAPI then
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
            C_BOOL)     -- BOOL
--      xAngleArc = define_c_func(GDI32,"AngleArc",
--          {C_PTR,     --  HDC hdc
--           C_INT,     --  int X
--           C_INT,     --  int Y
--           C_DWORD,   --  DWORD dwRadius
--           C_FLOAT,   --  FLOAT eStartAngle
--           C_FLOAT},  --  FLOAT eSweepAngle
--          C_BOOL)     -- BOOL
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
            C_BOOL)     -- BOOL
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
            C_BOOL)     -- BOOL
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
            C_BOOL)     -- BOOL
        xCloseClipboard = define_c_proc(USER32,"CloseClipboard",
            {})         --  (void)
--          C_BOOL)     -- BOOL (0 on failure)
        xCreateCompatibleBitmap = define_c_func(GDI32,"CreateCompatibleBitmap",
            {C_PTR,     --  HDC hdc
             C_INT,     --  int nWidth
             C_INT},    --  int nHeight
            C_PTR)      -- HBITMAP
        xCreateCompatibleDC = define_c_func(GDI32,"CreateCompatibleDC",
            {C_PTR},    --  HDC hdc // handle of memory device context
            C_PTR)      -- HDC
        xCreateDIBitmap = define_c_func(GDI32,"CreateDIBitmap",
            {C_PTR,     --  HDC hdc
             C_PTR,     --  const BITMAPINFOHEADER *lpbmih
             C_LONG,    --  DWORD fdwInit
             C_LONG,    --  const VOID *lpbInit
             C_PTR,     --  const BITMAPINFO *lpbmi
             C_LONG},   --  UINT fuUsage
--see gMenu.exw
--          C_PTR)      -- HBITMAP
            C_LONG)     -- HBITMAP
        xCreateFontIndirect = define_c_func(GDI32,"CreateFontIndirectA",
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
        xExtCreatePen = define_c_func(GDI32,"ExtCreatePen",
            {C_INT,     --  DWORD fnPenStyle
             C_INT,     --  DWORD nWidth
             C_PTR,     --  const LOGBRUSH *lplb
             C_INT,     --  DWORD dwStyleCount
             C_INT},    --  const DWORD *lpStyle
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
            C_BOOL)     -- BOOL
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
            C_BOOL)     -- BOOL
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
             C_BOOL})   --  BOOL bEnable
--          C_BOOL)     -- BOOL (was enabled)
        xEndPaint = define_c_proc(USER32,"EndPaint",
            {C_PTR,     --  HWND  hWnd                  // handle of window
             C_PTR})    -- CONST PAINTSTRUCT  *lpPaint  // address of structure for paint data
--          C_BOOL)     -- BOOL (function always returns true so linked as c_proc)
        xFillRect = define_c_func(USER32,"FillRect",
            {C_PTR,     --  HDC hDC
             C_PTR,     --  const RECT *lprc
             C_PTR},    --  HBRUSH hbr
            C_LONG)     -- int (0 on failure)
        xGetClientRect = define_c_func(USER32,"GetClientRect",
            {C_PTR,     --  HWND hWnd
             C_PTR},    --  LPRECT lpRect
            C_BOOL)     -- BOOL
        xGetClipboardData = define_c_func(USER32,"GetClipboardData",
            {C_UINT},   --  UINT uFormat
            C_PTR)      -- HANDLE
        xGetConsoleWindow = define_c_func(KERNEL32,"GetConsoleWindow",
            {},         --  (void)
            C_PTR)      -- HWND of the console window, or NULL.
        xGetCursorPos = define_c_func(USER32,"GetCursorPos",
            {C_PTR},    --  LPPOINT lpPoint
            C_BOOL)     -- BOOL
        xGetDC = define_c_func(USER32,"GetDC",
            {C_PTR},    --  HWND  hWnd  // handle of window
            C_PTR)      -- HDC
        xGetDeviceCaps = define_c_func(GDI32,"GetDeviceCaps",
            {C_PTR,     --  HDC  hdc,   // device-context handle
             C_INT},    --  int  nIndex // index of capability to query
            C_INT)      -- int
        xGetDIBColorTable = define_c_func(GDI32,"GetDIBColorTable",
            {C_PTR,     -- HDC hdc
             C_UINT,    -- UINT uStartIndex
             C_UINT,    -- UINT cEntries
             C_PTR},    -- RGBQUAD* pColors
            C_UINT)     -- UINT (0 on failure)
        xGetFocus = define_c_func(USER32,"GetFocus",
            {},         --  (void)
            C_PTR)      -- HWND
        xGetForegroundWindow = define_c_func(USER32,"GetForegroundWindow",
            {},         --  (void)
            C_PTR)      -- HWND of the forground window.
        xGetKeyState = define_c_func(USER32, "GetKeyState",
            {C_INT},    --  int  nVirtKey       // virtual-key code
            C_INT)      -- SHORT
        -- see NO_ERROR on...
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
             C_BOOL,    --  BOOL fByPosition
             C_PTR},    --  LPMENUITEMINFO lpmii
            C_BOOL)     -- BOOL
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
             C_BOOL)    -- BOOL
--      xGetNextDlgTabItem = define_c_func(USER32,"GetNextDlgTabItem",
--          {C_PTR,     --  HWND hDlg
--           C_PTR,     --  HWND hCtl
--           C_BOOL},   --  BOOL bPrevious
--           C_PTR)     -- HWND
--      xGetParent = define_c_func(USER32,"GetParent",
--          {C_PTR},    --  HWND  hWnd  // handle of child window
--          C_PTR)      -- HWND
--DEV... (to go)
        xGetPixel = define_c_func(GDI32,"GetPixel",
            {C_PTR,     --  HDC hdc
             C_INT,     --  int X
             C_INT},    --  int Y
            C_INT)      -- COLORREF crColor
--      xGetScrollInfo = define_c_func(USER32,"GetScrollInfo",
--          {C_PTR,     --  HWND hwnd
--           C_INT,     --  int fnBar
--           C_PTR},    --  LPCSCROLLINFO lpsi
--          C_BOOL)     -- BOOL
        xGetStockObject = define_c_func(GDI32,"GetStockObject",
            {C_INT},    --  int  fnObject   // type of stock object
            C_PTR)      -- HGDIOBJ GetStockObject(
        xGetSysColor = define_c_func(USER32,"GetSysColor",
            {C_INT},    --  int  nIndex     // display element
            C_LONG)     -- DWORD
        xGetSysColorBrush = define_c_func(USER32,"GetSysColorBrush",
            {C_INT},    --  int  nIndex     // display element
            C_LONG)     -- HBRUSH
        xGetSystemMetrics = define_c_func(USER32,"GetSystemMetrics",
            {C_INT},    --  int nIndex
            C_INT)      -- int
        xGetTextExtentPoint32 = define_c_func(GDI32,"GetTextExtentPoint32A",
            {C_PTR,     --  HDC  hdc,   // handle of device context
             C_PTR,     --  LPCTSTR  lpString,  // address of text string
             C_INT,     --  int  cbString,  // number of characters in string
             C_PTR},    --  LPSIZE  lpSize  // address of structure for string size
            C_BOOL)     -- BOOL
        xGetTextExtentPoint32W = define_c_func(GDI32,"GetTextExtentPoint32W",
            {C_PTR,     --  HDC  hdc,   // handle of device context
             C_PTR,     --  LPCTSTR  lpString,  // address of text string
             C_INT,     --  int  cbString,  // number of characters in string
             C_PTR},    --  LPSIZE  lpSize  // address of structure for string size
            C_BOOL)     -- BOOL
--      xGetWindow = define_c_func(USER32,"GetWindow",
--          {C_PTR,     --  HWND hWnd
--           C_UINT},   --  UINT uCmd
--          C_PTR)      -- HWND
        xGetWindowLong = define_c_func(USER32,iff(MB=32?"GetWindowLongA"
                                                       :"GetWindowLongPtrA"),
            {C_PTR,     --  HWND  hWnd      // handle of window
             C_UINT},   --  int  nIndex     // offset of value to retrieve
            C_LONG)     -- LONG/LONG_PTR
        xGetWindowRect = define_c_func(USER32,"GetWindowRect",
            {C_PTR,     --  HWND hWnd
             C_PTR},    --  LPRECT lpRect
            C_BOOL)     -- BOOL
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
--          C_BOOL)     -- BOOL (non-0: success but still locked, 0: check success/failure via GetLastError)
        xImageList_Add = define_c_proc(COMCTL32,"ImageList_Add",
            {C_PTR,     --  HIMAGELIST  himl,   // handle to the image list
             C_PTR,     --  HBITMAP  hbmImage,  // handle to the bitmap containing the image
             C_PTR})    --  HBITMAP  hbmMask    // handle to the bitmap containing the mask
--          C_INT)      -- int
        xImageList_Create = define_c_func(COMCTL32,"ImageList_Create",
            {C_INT,     --  int cx (Specifies the width, in pixels, of each image.)
             C_INT,     --  int cy (Specifies the height, in pixels, of each image.)
             C_UINT,    --  UINT  flags (ILC_xxx values, usually ILC_COLOR8)
             C_INT,     --  int  cInitial (Number of images that the image list initially contains.)
             C_INT},    --  int  cGrow (?Number of images to grow by when resized?)
            C_PTR)      -- HIMAGELIST (handle to the image list, NULL on failure)
        xInvalidateRect = define_c_func(USER32,"InvalidateRect",
            {C_PTR,     --  HWND hWnd
             C_PTR,     --  const RECT *lpRect
             C_BOOL},   --  BOOL bErase
            C_BOOL)     -- BOOL
        xIsClipboardFormatAvailable = define_c_func(USER32,"IsClipboardFormatAvailable",
            {C_UINT},   --  UINT format
            C_BOOL)     -- BOOL
        xIsDialogMessage = define_c_func(USER32,"IsDialogMessageA",
            {C_PTR,     --  HWND hDlg
             C_PTR},    --  LPMSG lpMsg
            C_BOOL)     -- BOOL
        xKillTimer = define_c_func(USER32,"KillTimer",
            {C_PTR,     --  HWND hWnd (NULL here)
             C_UINT},   --  UINT_PTR uIDEvent
            C_BOOL)     -- BOOL
        xLineTo = define_c_func(GDI32,"LineTo",
            {C_PTR,     --  HDC  hdc,   // device context handle
             C_INT,     --  int  nXEnd, // x-coordinate of line's ending point
             C_INT},    --  int  nYEnd  // y-coordinate of line's ending point
            C_BOOL)     -- BOOL
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
            C_BOOL)     -- BOOL
        xMoveWindow = define_c_proc(USER32,"MoveWindow",
            {C_PTR,     --  HWND hWnd
             C_INT,     --  int X
             C_INT,     --  int Y
             C_INT,     --  int nWidth
             C_INT,     --  int nHeight
             C_BOOL})   --  BOOL bRepaint
--          C_BOOL)     -- BOOL
        xOpenClipboard = define_c_func(USER32,"OpenClipboard",
            {C_PTR},    --  HWND hWndNewOwner
            C_BOOL)     -- BOOL
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
            C_BOOL)     -- BOOL
--      xPlaySound = define_c_proc(WINMM,"PlaySound",
--  --  xPlaySound = define_c_func(WINMM,"PlaySoundA",
--          {C_PTR,     --  LPCTSTR pszSound
--           C_PTR,     --  HMODULE hmod
--           C_INT})    --  DWORD   fdwSound
--  --      C_BOOL)     -- BOOL
--      xPolyBezier = define_c_func(GDI32,"PolyBezier",
--          {C_PTR,     --  HDC hdc
--           C_PTR,     --  const POINT *lppt
--           C_INT},    --  DWORD cPoints
--          C_BOOL)     -- BOOL
        xPostQuitMessage = define_c_proc(USER32,"PostQuitMessage",
            {C_INT})    --  int  nExitCode      // exit code
        xRectangle = define_c_func(GDI32,"Rectangle",
            {C_PTR,     --  HDC hdc
             C_INT,     --  int nLeftRect
             C_INT,     --  int nTopRect
             C_INT,     --  int nRightRect
             C_INT},    --  int nBottomRect
            C_BOOL)     -- BOOL
        xRedrawWindow = define_c_func(USER32,"RedrawWindow",
            {C_PTR,     --  HWND hWnd
             C_PTR,     --  const RECT *lprcUpdate (null here)
             C_PTR,     --  HRGN hrgnUpdate            "" 
             C_UINT},   --  UINT flags
            C_BOOL)     -- BOOL
        xRegisterClassEx = define_c_func(USER32,"RegisterClassExA",
            {C_PTR},    --  CONST WNDCLASSEX FAR *lpwcx // address of structure with class data
            C_PTR)      -- ATOM
        xReleaseCapture = define_c_proc(USER32,"ReleaseCapture",
            {})         --  (void)
--          C_BOOL)     -- BOOL (ignored)
        xReleaseDC = define_c_func(USER32,"ReleaseDC",
            {C_PTR,     --  HWND  hwnd, // handle of window
             C_PTR},    --  HDC  hdc    // handle of device context
            C_BOOL)     -- BOOL
        xRoundRect = define_c_func(GDI32,"RoundRect",
            {C_PTR,     --  HDC hdc
             C_INT,     --  int nLeftRect
             C_INT,     --  int nTopRect
             C_INT,     --  int nRightRect
             C_INT,     --  int nBottomRect
             C_INT,     --  int nWidth
             C_INT},    --  int nHeight
            C_BOOL)     -- BOOL
        xScreenToClient = define_c_func(USER32, "ScreenToClient",
            {C_PTR,     --  HWND hWnd
             C_PTR},    --  LPPOINT lpPoint
            C_BOOL)     -- BOOL
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
        xSetBkColor = define_c_func(GDI32, "SetBkColor",
            {C_PTR,     --  HDC  hdc,   // handle of device context
             C_PTR},    --  COLORREF  crColor   // background color value
            C_PTR)      -- COLORREF
        xSetBkMode = define_c_func(GDI32, "SetBkMode",
            {C_PTR,     --  HDC  hdc,   // handle of device context
             C_INT},    --  int  iBkMode    // flag specifying background mode
            C_INT)      -- int
        xSetCapture = define_c_proc(USER32,"SetCapture",
            {C_PTR})    --  HWND hWnd
--          C_PTR)      -- HWND (ignored)
        xSetClipboardData = define_c_func(USER32, "SetClipboardData",
            {C_UINT,    --  UINT uFormat
             C_PTR},    --  HANDLE hMem
            C_INT)      -- HANDLE (NULL means failure)
        xSetDIBColorTable = define_c_func(GDI32,"SetDIBColorTable",
            {C_PTR,     -- HDC hdc
             C_UINT,    -- UINT uStartIndex
             C_UINT,    -- UINT cEntries
             C_PTR},    -- const RGBQUAD* pColors
            C_UINT)     -- UINT (0 on failure)
        xSetFocus = define_c_proc(USER32,"SetFocus",
            {C_PTR})    --  HWND hWnd
--          C_PTR)      -- HWND (previous focus)
        xSetMenu = define_c_func(USER32,"SetMenu",
            {C_PTR,     --  HWND hWnd
             C_PTR},    --  HMENU hMenu
            C_BOOL)     -- BOOL
--      xSetMenuItemBitmaps = define_c_func(USER32,"SetMenuItemBitmaps",
--          {C_PTR,     --  HMENU hMenu
--           C_INT,     --  UINT uPosition
--           C_INT,     --  UINT uFlags,
--           C_PTR,     --  HBITMAP hBitmapUnchecked
--           C_PTR},    --  HBITMAP hBitmapChecked
--          C_BOOL)     -- BOOL
        xSetMenuItemInfo = define_c_func(USER32,"SetMenuItemInfoA",
            {C_PTR,     --  HMENU hMenu
             C_INT,     --  UINT uItem
             C_BOOL,    --  BOOL fByPosition
             C_PTR},    --  LPMENUITEMINFO lpmii
            C_BOOL)     -- BOOL
        xSetParent = define_c_func(USER32,"SetParent",
            {C_PTR,     --  HWND  hwndChild         // handle of window whose parent is changing
             C_PTR},    --  HWND  hwndNewParent     // handle of new parent window
            C_PTR)      -- HWND of the previous parent window.
        xSetPixelV = define_c_func(GDI32,"SetPixelV",
            {C_PTR,     --  HDC hdc
             C_INT,     --  int X
             C_INT,     --  int Y
             C_INT},    --  COLORREF crColor
            C_BOOL)     -- BOOL (non-zero for success)
--      xGetROP2 = define_c_func(GDI32,"GetROP2",
--          {C_PTR},    --  HDC hdc
--          C_INT)      -- int
        xSetROP2 = define_c_func(GDI32,"SetROP2",
            {C_PTR,     --  HDC hdc
             C_INT},    --  int fnDrawMode
            C_INT)      -- int
--      xSetScrollInfo = define_c_func(USER32, "SetScrollInfo",
--          {C_PTR,     --  HWND hwnd
--           C_INT,     --  int fnBar
--           C_PTR,     --  LPCSCROLLINFO lpsi
--           C_BOOL},   --  BOOL fRedraw
--          C_INT)      -- int
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
            C_BOOL)     -- BOOL
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
--          C_BOOL)     -- BOOL success
        xTrackMouseEvent = define_c_func(USER32, "TrackMouseEvent",
            {C_PTR},    --  LPTRACKMOUSEEVENT lpEventTrack
            C_BOOL)     -- BOOL
        xTrackPopupMenuEx = define_c_func(USER32, "TrackPopupMenuEx",
            {C_PTR,     --  HMENU  hmenu,       
             C_UINT,    --  UINT  fuFlags,      
             C_INT,     --  int  x,     
             C_INT,     --  int  y,     
             C_PTR,     --  HWND  hwnd,         
             C_PTR},    --  LPTPMPARAMS  lptpm  (NULL here)
            C_BOOL)     -- BOOL
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
            C_BOOL)     -- BOOL
        xTranslateMessage = define_c_proc(USER32,"TranslateMessage",
            {C_PTR})    --  CONST MSG  *lpmsg   // address of structure with message
--          C_BOOL)     -- BOOL (true if was translated...)
        xUpdateWindow = define_c_proc(USER32,"UpdateWindow",
            {C_PTR})    --  HWND hWnd
--          C_BOOL)     -- BOOL

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

        idNMUPDOWN = define_struct("""typedef struct _NM_UPDOWN {
                                       NMHDR hdr;
                                       int   iPos;
                                       int   iDelta;
                                      } NMUPDOWN, *LPNMUPDOWN;""")

        idUDACCEL = define_struct("""typedef struct {
                                      UINT nSec;
                                      UINT nInc;
                                     } UDACCEL, *LPUDACCEL;""")

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

        idLOGBRUSH = define_struct("""typedef struct tagLOGBRUSH {
                                       UINT     lbStyle;
                                       COLORREF lbColor;
                                       ULONG_PTR lbHatch;
                                      } LOGBRUSH, *PLOGBRUSH;""")

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

--      idSCROLLINFO = define_struct("""typedef struct tagSCROLLINFO {
--                                        UINT cbSize;
--                                        UINT fMask;
--                                        int  nMin;
--                                        int  nMax;
--                                        UINT nPage;
--                                        int  nPos;
--                                        int  nTrackPos;
--                                      } SCROLLINFO, *LPCSCROLLINFO;""")

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

        idTRACKMOUSEEVENT = define_struct("""typedef struct tagTRACKMOUSEEVENT {
                                               DWORD cbSize;
                                               DWORD dwFlags;
                                               HWND hwndTrack;
                                               DWORD dwHoverTime;
                                             } TRACKMOUSEEVENT, *LPTRACKMOUSEEVENT;""")

        string tMINMAXINFO = """typedef struct tagMINMAXINFO {
                                  POINT ptReserved;
                                  POINT ptMaxSize;
                                  POINT ptMaxPosition;
                                  POINT ptMinTrackSize;
                                  POINT ptMaxTrackSize;
                                } MINMAXINFO, *PMINMAXINFO, *LPMINMAXINFO;"""
        idMINMAXINFO = define_struct(tMINMAXINFO)
           
--(tested, 16 bytes on both 32 and 64 bit:)
--printf(1,"Windows %d bit, pRECT is %d bytes\n",{machine_bits(),get_struct_size(idRECT)})
           
        pPAINTSTRUCT = allocate_struct(idPAINTSTRUCT)
        pRECT = allocate_struct(idRECT)
--/* DEV... (pLeft/Right/Top/Bottom)
        atom pX = pRECT+get_field_details(idGdkRectangle,"x")[1],
             pY = pRECT+get_field_details(idGdkRectangle,"y")[1],
             pW = pRECT+get_field_details(idGdkRectangle,"width")[1],
             pH = pRECT+get_field_details(idGdkRectangle,"height")[1]
--*/
        pSIZE = allocate_struct(idSIZE)
        pTVINSERTSTRUCT = allocate_struct(idTVINSERTSTRUCT)
        pTVITEMEX = allocate_struct(idTVITEMEX)
        pMSG = allocate_struct(idMESSAGE)
        pLOGFONT = allocate_struct(idLOGFONT)
        pLOGBRUSH = allocate_struct(idLOGBRUSH)
--      pPIXELFORMATDESCRIPTOR = allocate_struct(idPIXELFORMATDESCRIPTOR)
        pTOOLINFO = allocate_struct(idTOOLINFO)
--      pSCROLLINFO = allocate_struct(idSCROLLINFO)
--      set_struct_field(idSCROLLINFO,pSCROLLINFO,"cbSize",get_struct_size(idSCROLLINFO))
        pTCITEM = allocate_struct(idTCITEM)
        pUDACCEL = allocate_struct(idUDACCEL)
        pPOINT = allocate_struct(idPOINT)
        pMENUITEMINFO = allocate_struct(idMENUITEMINFO)
        set_struct_field(idMENUITEMINFO,pMENUITEMINFO,"cbSize",get_struct_size(idMENUITEMINFO))
        pTRACKMOUSEEVENT = allocate_struct(idTRACKMOUSEEVENT)
        set_struct_field(idTRACKMOUSEEVENT,pTRACKMOUSEEVENT,"cbSize",get_struct_size(idTRACKMOUSEEVENT))
        NullBrushID = c_func(xGetStockObject,{NULL_BRUSH})
        SM_XICON = c_func(xGetSystemMetrics,{SM_CXSMICON})
        SM_YICON = c_func(xGetSystemMetrics,{SM_CYSMICON})
        WINAPI_SUBMENUS = new_dict()
        WINAPI_MENU_IMGS = new_dict()
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
        -- plus two for gSplit:
        set_struct_field(idWNDCLASSEX,pWNDCLASSEX,"lpszClassName",xpg_raw_string_ptr("gHSplit"))
        hCursor = c_func(xLoadCursor,{NULL,IDC_SIZEWE})
        set_struct_field(idWNDCLASSEX,pWNDCLASSEX,"hCursor",hCursor)
        if c_func(xRegisterClassEx,{pWNDCLASSEX})=0 then
            crash("RegisterClassEx error #%08x (%d)",c_func(xGetLastError,{}))
        end if
        set_struct_field(idWNDCLASSEX,pWNDCLASSEX,"lpszClassName",xpg_raw_string_ptr("gVSplit"))
        hCursor = c_func(xLoadCursor,{NULL,IDC_SIZENS})
        set_struct_field(idWNDCLASSEX,pWNDCLASSEX,"hCursor",hCursor)
        if c_func(xRegisterClassEx,{pWNDCLASSEX})=0 then
            crash("RegisterClassEx error #%08x (%d)",c_func(xGetLastError,{}))
        end if

        tree_himl = xpg_create_image_list("WinAPI",xpg_xpm_callback)
    else
        ?9/0 -- (unknown backend)
    end if
    -- finally setup common handlers:
--  xpg_register_handler(0,"KEY",{{2,2,"FOI"},{3,3,"FOII"},{4,4,"FOIII"},{5,5,"FOIIII"}})
--  xpg_register_handler(0,"CLICK",{{4,4,"FOPII"},{4,4,"POPII"},{2,2,"FOP"},{2,2,"POP"}})
--  xpg_register_handler(0,"MOUSEMOVE",{{3,3,"POII"},{6,6,"POIIIII"},{9,9,"POIIIIIIII"}})
    xpg_register_handler(0,"KEY",{"FOI","FOII","FOIII","FOIIII"})
    xpg_register_handler(0,"CLICK",{"FOPII","POPII","FOP","POP"})
    xpg_register_handler(0,"MOUSEMOVE",{"POII","POIIIII","POIIIIIIII"})
--DEV...
--  xpg_register_handler(0,"REDRAW",{{1,1,"PO"},{0,0,"P"}})
--  xpg_register_handler(0,"REDRAW",{"PO","P"})
end procedure

global function gVersion(integer bBack=false)
    if not bInit then xpg_Init() end if
    if bBack then
--      return iff(bBack=-1?backend:backdesc[backend]) 
        if bBack=-1 then return backend end if
        if backend=XPG_GTK then
            return iff(bGTK3?"GTK3":"GTK2")
        elsif backend=XPG_WINAPI then
            return sprintf("Win%dAPI",machine_bits())
        elsif backend=XPG_JS then
            return "JS"
        else
            ?9/0 -- placeholder
        end if
    end if
    integer pdx = find(platform(),{WINDOWS,LINUX,ARM,JS})
    string bare = "xpGUI 0.1", backing,
           plat = {"Windows","Linux","Arm","JavaScript"}[pdx]
    if backend=XPG_GTK then
        backing = sprintf("GTK %d.%d.%d",gtk_version)
    elsif backend=XPG_WINAPI then
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
        if backend=XPG_GTK then
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
        elsif backend=XPG_WINAPI then
            width = c_func(xGetSystemMetrics,{SM_CXSCREEN})
            height = c_func(xGetSystemMetrics,{SM_CYSCREEN})
        else
            ?9/0 -- (unknown backend)
        end if
        return {width,height}
    elsif name="MOUSEPOS" then
        integer x, y
        if backend=XPG_GTK then
            atom display = c_func(gdk_display_get_default,{}),
                 pX = allocate(4), pY = allocate(4)
            c_proc(gdk_display_get_pointer,{display,NULL,pX,pY,NULL})
            x = peek4u(pX)
            y = peek4u(pY)
            free({pX,pY})
        elsif backend=XPG_WINAPI then
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
    elsif name="CTRLKEY"
       or name="CONTROLKEY" then
        if backend=XPG_WINAPI then
            ctrlkey = and_bits(c_func(xGetKeyState,{VK_CTRL}),#8000)!=0
        end if
        return ctrlkey
    elsif name="SHIFTKEY" then
        if backend=XPG_WINAPI then
            shiftkey = and_bits(c_func(xGetKeyState,{VK_SHIFT}),#8000)!=0
        end if
        return shiftkey
    elsif name="ALTKEY" then
        if backend=XPG_WINAPI then
            altkey = and_bits(c_func(xGetKeyState,{VK_ALT}),#8000)!=0
        end if
        return shiftkey
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
--      atom prev_wnd_proc = c_func(xSetWindowLong,{hwnd,GWL_WNDPROC,WINAPI_SUBPROC_CB})
--      wnd_proc_addr[id] = prev_wnd_proc
        wnd_proc_addr[id] = c_func(xSetWindowLong,{hwnd,GWL_WNDPROC,WINAPI_SUBPROC_CB})

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
--if ct!=SPIN then
--          integer pid = parent_ids[id]
--          if pid=0 then
--              assert(ct=TEXT and ctrl_types[id-1]=SPIN)
--          else
--              assert(find(id,children_ids[pid])!=0)
--          end if
        end if
    end if

    return hwnd  -- (nb handle not id!)

end function

include builtins/ptypes.e

local type dword_seq(object s)  -- (technically qword_seq on 64-bit)
    return sequence(s) and not string(s)
end type

--DEV/SUG it might not be complete madness to have a single 
--  paranormalise(string|int sig, sequence args) routine,
--  where the caller /is/ expected to desequence as now, 
--    and sig is "TRAA", "QRAA", "RAA", "TAA", or "PTAAB"(/"PTAAE"?) (or "PTMRAA")
--  or maybe you could have some bit-flags...  (and maybe version 2+...)
--                               -- PTQRMAAE
--   eg local constant XPG_PARP = 0b10000000, -- Parent
--                     XPG_PART = 0b01000000, -- Title
--                     XPG_PARQ = 0b00100000, -- seQuence (as per gDropDown/gTabs/gTreeView)
--                     XPG_PARR = 0b00010000, -- Rtn
--                     XPG_PARM = 0b00001000, -- Msg
--                     XPG_PARA = 0b00000110, -- attributes/args (must always be set in sig)
--                     XPG_PARE = 0b00000001, -- bEsc
--                     XPG_TRAA = 0b01010110,
--                     XPG_QRAA = 0b00110110,
--                     XPG_RAA  = 0b00010110,
--                     XPG_TAA  = 0b01000110,
--                     XPG_PTAA = 0b11000111,
--                     XPG_PRTM = 0b11011110
--  assert(and_bits(sig,XPG_PARA)==XPG_PARA)

local function paranormalise_traa(object title, click, sequence attributes, dword_seq args)
-- used by gButton([nullable_string title=NULL,] [rtn click=NULL,] string attributes="", sequence args={})
-- and gCheckbox(), gLink(), gMenuItem(), and gValuator(). (See the docs for the full details)
-- This routine is designed to crash on the slightest oddity, hopefully with a decent easily understood message.
    integer nFrames = 3 -- (change this back to 1 to debug)
--  integer nFrames = 1 -- (change this back to 1 to debug)
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

--local procedure xpg_gtk_signal_connect(atom handle, string signal, rtn rid, atom user_data=NULL)
--14/6/23 id is not a gdx for menus... (should we assert??)
--local procedure xpg_gtk_signal_connect(atom handle, string signal, integer rid, gdx id)
local procedure xpg_gtk_signal_connect(atom handle, string signal, integer rid, integer id)
    -- note that g_signal_connect is defined in the GTK sources as a #define of
    -- g_signal_connect_data(....,NULL,0), and is not exported from the dll/so.
    atom r = c_func(g_signal_connect_data,{handle,signal,call_back({'+',rid}),id,NULL,0})
    assert(r>0)
end procedure

integer timer_ids = NULL

local function xpg_gtk_timer_proc(integer id)
    integer action = gGetHandler(id,"ACTION")
    if action then
        action(id)
    end if
    return true
end function
local constant xpg_gtk_timer_proc_cb = call_back({'+',xpg_gtk_timer_proc})

local function xpg_WinAPI_timer_proc(atom /*hwnd*/, integer /*msg*/, atom wid, /*mstime*/)
--  assert(hwnd=NULL)
--  assert(msg=WM_TIMER)
--  if mstime<0 then mstime = and_bitsu(mstime,#FFFFFFFF) end if -- (unsign)
    integer id = getd(wid,timer_ids)
    return xpg_gtk_timer_proc(id)
end function
local constant xpg_WinAPI_timer_proc_cb = call_back({'+',xpg_WinAPI_timer_proc})

function xpg_set_timer_attribute(gdx id, string name, object v, bool bMapped)
    -- nb bMapped (ie CF_MAPPED) means there is an active running timer
    atom handle = ctrl_handles[id], flags = ctrl_flags[id]
    bool bOK = false, bRun = bMapped
    if bMapped then
        if backend=XPG_GTK then     
            assert(c_func(g_source_remove,{handle}))
        elsif backend=XPG_WINAPI then
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
            if backend=XPG_GTK then
                handle = c_func(g_timeout_add,{msecs,xpg_gtk_timer_proc_cb,id})
            elsif backend=XPG_WINAPI then
                handle = c_func(xSetTimer,{NULL,0,msecs,xpg_WinAPI_timer_proc_cb})
            else
                ?9/0 -- (unknown backend)
            end if
            setd(handle,id,timer_ids)
            flags += CF_MAPPED
        end if
--?{"gTimer:SetAttribute",name,bRun,msecs,flags,CF_MAPPED}
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

--global function gTimer(rtn action=NULL, integer msecs=0, boolean active=false, object user_data=NULL)
global function gTimer(rtn action=NULL, integer msecs=40, boolean active=true, object user_data=NULL)
--?{"gTimer",action,msecs,active,user_data}
    if not bInit then xpg_Init() end if
    if timer_ids=NULL then timer_ids = new_dict() end if
    integer id = xpg_add_control(TIMER)
--  xpg_register_handler(TIMER,"ACTION",{{1,1,"PO"}})
    xpg_register_handler(TIMER,"ACTION",{"PO"})
    xpg_set_ctrl_msg(TIMER,0,xpg_set_timer_attribute,
                             xpg_get_timer_attribute)
    if action!=NULL then
        gSetHandler(id,"ACTION",action)
    end if
    gSetAttribute(id,"MSECS",msecs)
    gSetAttribute(id,"RUN",active)
    gSetAttribute(id,"USER_DATA",user_data)
    return id
end function 

--gboolean focus(GtkWidget* self, GtkDirectionType direction, gpointer user_data)
local function xpg_gtk_focus(atom widget, integer direction, gdx id)
    -- GTK only part (WinAPI equivalent in xpg_WinAPI_process_key)
    -- Same tab handling as WinAPI, since GTK often gets it wrong.
    assert(id=xpg_getID(widget))
    integer ct = ctrl_types[id] 
    assert(ct=DIALOG)
--?{"xpg_gtk_focus",direction,id,ctrl_names[ct]}
    gdx fid = gGetFocus()
    if fid then id = fid end if
    xpg_find_next_tab_stop(id, direction)
    return true
end function

procedure xpg_Dialog(integer id)
    integer parent = parent_ids[id]
    atom handle
    if backend=XPG_GTK then
        handle = c_func(gtk_window_new,{GTK_WINDOW_TOPLEVEL}) 
        assert(handle!=NULL)
        xpg_setID(handle,id)
        if parent!=NULL then
            c_proc(gtk_window_set_transient_for,{handle,ctrl_handles[parent]}) 
        end if
        c_proc(gtk_widget_set_events,{handle,GDK_KBR_MASK})
        xpg_gtk_signal_connect(handle,"key_press_event",xpg_gtk_check_escape,id)
        xpg_gtk_signal_connect(handle,"key_release_event",xpg_gtk_release_key,id)
        xpg_gtk_signal_connect(handle,"button-press-event",xpg_gtk_click,id)
        xpg_gtk_signal_connect(handle,"button-release-event",xpg_gtk_click,id)
        xpg_gtk_signal_connect(handle,"focus-in-event",xpg_gtk_focusinout,id)
        xpg_gtk_signal_connect(handle,"focus-out-event",xpg_gtk_focusinout,id)
        xpg_gtk_signal_connect(handle,"motion-notify-event",xpg_gtk_mousemove,id)
        xpg_gtk_signal_connect(handle,"configure-event",xpg_gtk_configure_event,id)
        xpg_gtk_signal_connect(handle,"focus",xpg_gtk_focus,id)
        c_proc(gtk_widget_realize,{handle})
    elsif backend=XPG_WINAPI then
        -- (aside: ctrl_types[id] is already DIALOG)
        atom d = CW_USEDEFAULT, -- (==#80000000)
             dwStyle = WS_OVERLAPPEDWINDOW,
           dwStyleEx = WS_EX_ACCEPTFILES
--         dwStyleEx = WS_EX_CONTROLPARENT + WS_EX_ACCEPTFILES

        if gGetInt(id,"RESIZE",true) then dwStyle += WS_MINMAXTHICK end if
--CS_OWNDC?
--CreateDialog (0 hits)
--DialogBox (1 "about" in Gui.e, no others)
--#define WM_NEXTDLGCTL                 0x0028 (commented out, no uses)
--BS_DEFPUSHBUTTON (no uses) [try in combination with SetFocus(), presumably that is if it is a button]
--DM_SETDEFID (0 hits)
--   if (WM_KEYDOWN == msg && VK_TAB == wParam) {
--      HWND next = GetNextDlgTabItem(mainWindow, control, (int)(GetKeyState(VK_SHIFT) & 0x8000));  -- (0 hits) [****FIXED!!!***]
--      SetFocus(next);
--      return 0;
--  }
--IsDialogMessage -- (0 uses)
--WS_EX_CONTROLPARENT

        handle = xpg_WinAPI_create(id,"xpGUI","",parent,d,d,dwStyle,dwStyleEx)
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
        elsif backend=XPG_GTK then
            c_proc(gtk_window_set_title,{handle,v}) 
        elsif backend=XPG_WINAPI then
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
        elsif backend=XPG_GTK then
            return peek_string(c_func(gtk_window_get_title,{handle}))
        elsif backend=XPG_WINAPI then
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
    integer flags = iff(bEsc?CF_CLOSE_ON_ESC:0)+CF_NEVER_SHOWN+CF_RESIZE+CF_UNMAPATTR+CF_CONTAINER+CF_DECORATED,
               id = xpg_add_control(DIALOG,parent,flags,bHasChildren:=true)
    if child then
        parent_ids[child] = id
        children_ids[id] = {child}
    end if
--DEV not yet implemented anywhere...
--  xpg_register_handler(DIALOG,"CLOSE",{{1,1,"FO"}})
    xpg_register_handler(DIALOG,"CLOSE",{"FO"})
--  xpg_register_handler(DIALOG,"RESIZE",{{3,3,"FOII"}})
    xpg_set_ctrl_msg(DIALOG,xpg_Dialog,xpg_set_dialog_attribute,
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
    if backend=XPG_GTK then
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
    elsif backend=XPG_WINAPI then
        -- (under winAPI boxes are virtual)
        ctrl_flags[id] = or_bits(ctrl_flags[id],CF_MAPPED)
    else
        ?9/0 -- (unknown backend)
    end if
end procedure

function xpg_set_box_attribute(gdx id, string name, object v, bool bMapped)
    integer cfi = ctrl_flags[id]
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
        if string(v) then v = to_number(v) end if
--      assert(ct=BOX)
        ctrl_xtra[id][CX_BOX_GAP] = v
--      printf(1,"gSetAttribute(BOX,\"%s\",%v)...\n",{name,v})
        return true
    elsif name="SPACE" then
--DEV may as well test these both ways round... [DONE]
--      if integer(v) then v = gGetBoxSpacingName(v) end if
--      printf(1,"gSetAttribute(BOX,\"SPACE\",%v)...\n",{v})
        if string(v) then
            if v[1]='{' then
                assert(v[$]='}')
                v = v[2..-2]
            end if
            if find(',',v) then
                v = split(v,',')
            else
                v = {v,v}
            end if
--      end if
--      if integer(v) then
        elsif integer(v) then
            v = {v,v}
        elsif length(v)=1 then
            v = append(v,v[1])
        end if
        assert(length(v)=2)
        for i=CX_BOX_SPACE_H to CX_BOX_SPACE_V do
            object vi = v[i-(CX_BOX_SPACE_H-1)]
            if string(vi) then
                integer k = find(vi,box_spacing_descs)
                assert(k!=0,`gSetAttribute(BOX,"SPACE","%s")`,{vi})
                vi = box_spacing_masks[k]
            end if              
            assert(integer(vi) and vi>=0b000 and vi<=0b111)
            ctrl_xtra[id][i] = vi
        end for
--      string s = box_spacing_descs[find(v,box_spacing_masks)]
--      printf(1,"gSetAttribute(BOX,\"SPACE\",%s)..\n",{s})
        return true
    elsif name="FRAC" and and_bits(cfi,CF_SPLIT)!=0 then
        if string(v) then v = to_number(v) end if
        assert(atom(v) and (v=-1 or (v>=0.0 and v<=1.0)))
        ctrl_xtra[id][CX_GTL_ATTRS][PX_FRAC] = v
        return true
    end if
    return false
end function

function xpg_get_box_attribute(gdx id, string name, object dflt)
    integer cfi = ctrl_flags[id]
    if name="SPACE" then
        integer h = ctrl_xtra[id][CX_BOX_SPACE_H],
                v = ctrl_xtra[id][CX_BOX_SPACE_V]
        return iff(h==v?h:{h,v})
--      return {h,v}
    elsif name="ORIENTATION" then
        return iff(and_bits(cfi,CF_VERTICAL)?"VERTICAL":"HORIZONTAL")
    elsif name="FRAC" and and_bits(cfi,CF_SPLIT)!=0 then
        return ctrl_xtra[id][CX_GTL_ATTRS][PX_FRAC]
    end if
    return xpg_return_default_attr(id,name,dflt)
end function

local function gBox(integer hv, sequence children, string attributes, sequence args)
    if not bInit then xpg_Init() end if
--  integer id = xpg_add_control(BOX,flags:=CF_CONTAINER+iff(hv='V'?CF_VERTICAL:0))
--DEV probably...:
    integer id = xpg_add_control(BOX,flags:=CF_CONTAINER+CF_EXPANDB+iff(hv='V'?CF_VERTICAL:0))
--  xpg_register_handler(BOX,"XXX",{{1,1,"PO"}})
    xpg_set_ctrl_msg(BOX,xpg_Box,xpg_set_box_attribute,
                                 xpg_get_box_attribute)
    object cxi = repeat(0,CX_BOX_LEN)
--DEV...
--  cxi[CX_BOX_MARGIN] = repeat(0,4)
--DEV or NONE??
--  cxi[CX_BOX_SPACE_H] = XPG_SPACE_RIGHT
    cxi[CX_BOX_SPACE_H] = XPG_SPACE_NONE
--  cxi[CX_BOX_SPACE_V] = XPG_SPACE_BOTTOM
    cxi[CX_BOX_SPACE_V] = XPG_SPACE_NONE
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
    if backend=XPG_GTK then
--DOCS, maybe (I made this up) note the child of a dialog should normally be a vbox or hbox; should
--      you create, for instance, a dialog with a single button child, it will expand to fill the
--      entire dialog.
        button = c_func(gtk_button_new_with_mnemonic,{title})
--?{"created button",button}
        if ctrl_xtra[id]!=UNDEFINED then
            sequence img = ctrl_xtra[id]
            assert(img[1]="gImage") -- shd be {"gImage",GtkPixbuf}
--          atom pixbuf = img[2], -- (a GtkPixBuf)
            atom pixbuf = img[2][1], -- (a GtkPixBuf)
--DEV leak??
                 image = c_func(gtk_image_new_from_pixbuf,{pixbuf})
            c_proc(gtk_button_set_image,{button,image})
        end if
        c_proc(gtk_widget_set_events,{button,GDK_KBR_MASK})
        xpg_setID(button,id)
        xpg_gtk_add_to(parent,button)
--GDK_STRUCTURE_MASK
--      c_proc(gtk_widget_set_events,{button,GDK_KBR_MASK})
        xpg_gtk_signal_connect(button,"clicked",xpg_gtk_button_clicked,id)
--      xpg_gtk_signal_connect(button,"realize",xpg_gtk_widget_realized,id)
--      xpg_gtk_signal_connect(button,"map",xpg_gtk_widget_mapped,id)
--      xpg_gtk_signal_connect(button,"map-event",xpg_gtk_widget_mapped,id)
        xpg_gtk_signal_connect(button,"focus-in-event",xpg_gtk_focusinout,id)
        xpg_gtk_signal_connect(button,"focus-out-event",xpg_gtk_focusinout,id)

--X As a (temporary) workaround, I once stuck a v/hbox in:
--      atom vbox = c_func(gtk_vbox_new,{false,6})
--      atom hbox = c_func(gtk_hbox_new,{false,6})
--      c_proc(gtk_box_pack_start,{hbox,button,false,false,0})
--      c_proc(gtk_box_pack_start,{vbox,hbox,false,false,0})
--      c_proc(gtk_container_add,{ctrl_handles[parent],vbox})
--?{"realise",button}
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
    elsif backend=XPG_WINAPI then
--      sequence stitle = split(title,'\n')
        atom dwStyle = or_all({WS_CHILD,WS_VISIBLE,BS_PUSHBUTTON,WS_TABSTOP})
        bool bIcon = ctrl_xtra[id]!=UNDEFINED
--      if bIcon and title=NULL then dwStyle += BS_ICON end if
        if bIcon then dwStyle += BS_ICON end if
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
        if bIcon then
            sequence img = ctrl_xtra[id]
            assert(img[1]="gImage") -- img is {"gImage",{hDIB,w,h,t}}
            {} = c_func(xSendMessage,{button,BM_SETIMAGE,IMAGE_BITMAP,img[2][1]})
--IMAGE_BITMAP 
--IMAGE_ICON 
--lParam 
        end if
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
        elsif backend=XPG_GTK then
            c_proc(gtk_button_set_label,{handle,xpg_gtk_mnemonicalize(v)})
        elsif backend=XPG_WINAPI then
            c_proc(xSetWindowText,{handle,v})
--          xpg_set_button_natural_size(id,v)
        else
            ?9/0 -- (unknown backend)
        end if
        return true -- (dealt with)
    elsif name="IMAGE" then
--DEV erm, not quite true: image *can* be changed, but not the presence/absence of an image....
        assert(not bMapped,"image cannot be changed after mapping")
--      ?{"xpg_set_button_attribute",id,name,v,ctrl_xtra[id]}
        if string(v) then
--          v = gImage_from_XPM(v,"BUTTON")
            v = gImage_from_XPM(v,XPG_BTN_BG)
--DEV this didn't work the other place I tried it... also (sip) [3]???
        elsif backend=XPG_WINAPI then
            v = xpg_WinAPI_replace_bgclr(v[3],XPG_BTN_BG)
        end if
        assert(sequence(v) and v[1]="gImage")
        ctrl_xtra[id] = v
        return true
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
    elsif name="FLAT" then
--/*
        if string(v) then v = xpg_to_bool(v) end if
        if not bMapped then
            xpg_defer_attr(id,name,v)
        elsif backend=XPG_GTK then
?9/0
--          c_proc(gtk_button_set_label,{handle,xpg_gtk_mnemonicalize(v)})
        elsif backend=XPG_WINAPI then
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
--*/
printf(1,"gSetAttribute(BUTTON,\"FLAT\") not yet implemented\n")
        return true -- (dealt with)
    end if
    return false
end function

local function xpg_get_button_attribute(gdx id, string name, object dflt)
    atom handle = ctrl_handles[id]
    if name="TITLE" then
        if backend=XPG_GTK then
--?{"getting label",handle}
            return peek_string(c_func(gtk_button_get_label,{handle}))
        elsif backend=XPG_WINAPI then
            integer l = c_func(xGetWindowTextLength,{handle})
            string title = repeat(' ',l)
            c_proc(xGetWindowText,{handle,title,l+1})
            return title
        else
            ?9/0 -- (unknown backend)
        end if
--/*
    elsif name="FLAT" then
        if backend=XPG_GTK then
?9/0
--          return peek_string(c_func(gtk_button_get_label,{handle}))
        elsif backend=XPG_WINAPI then
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
-- gButton([nullable_string title=NULL,] [rtn action=NULL,] sequence attributes="", args={})
global function gButton(object title=NULL, action=NULL, sequence attributes="", args={})
    {title,action,attributes,args} = paranormalise_traa(title,action,attributes,args)
    if not bInit then xpg_Init() end if
--22/11/23 (???!!!)
--  integer id = xpg_add_control(BUTTON,flags:=CF_UNMAPATTR)
    integer id = xpg_add_control(BUTTON)
--  xpg_register_handler(BUTTON,"ACTION",{{1,1,"FO"}})
    xpg_register_handler(BUTTON,"ACTION",{"FO"})
    xpg_set_ctrl_msg(BUTTON,xpg_Button,xpg_set_button_attribute,
                                       xpg_get_button_attribute)
    -- aside: ctrl_xtra on a button is UNDEFINED/NULL or a gImage
--  gSetAttribute(id,"FONT","Helvetica, 9")
    if title!=NULL then
        gSetAttribute(id,"TITLE",title)
    end if
    if action!=NULL then
        gSetHandler(id,"ACTION",action)
    end if
--printf(1,"button:%x\n",ctrl_flags[id])
--trace(1)
    if length(attributes) then
        gSetAttributes(id,attributes,args)
    end if
--printf(1,"button:%x\n",ctrl_flags[id])
--  gSetAttribute(id,"EXPAND",false)
--  gSetInt(id,"EXPAND",false)
    return id
end function 

--static gboolean gtk_draw(GtkWidget*widget,cairo_t*cr,gpointer data)
local function xpg_gtk_canvas_draw(atom canvas, cairo, gdx id) -- (GTK only)
--?"xpg_gtk_canvas_draw"
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
    xpg_redraw_canvas(id,back)
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
    if backend=XPG_GTK then
        c_proc(cairo_set_line_width,{hdc,penwidth})
--DEV...
        if penstyle=XPG_CONTINUOUS then
            c_proc(cairo_set_dash,{hdc,0,0,0})
        else
--          sequence dashes = {{6,2},{2,2},{6,2,2,2},{6,2,2,2,2,2}}[penstyle]
            sequence dashes = {{6,2},{2,2},{6,2,2,2},{6,2,2,2,2,2}}[penstyle-1]
            atom pDash = xpg_double_array(dashes)
            c_proc(cairo_set_dash,{hdc,pDash,length(dashes),0})
            free(pDash)
        end if
        if bBack then pencolour = gCanvasGetBackground(id) end if
        atom {r,g,b} = to_rgba(pencolour)
        c_proc(cairo_set_source_rgb,{hdc,r/255,g/255,b/255})
    elsif backend=XPG_WINAPI then
        if and_bits(ctrl_xtra[id][CX_CANVAS_FLAGS],CXCF_REPEN) then
            atom bgr = xpg_WinAPI_rgb_to_bgr(pencolour), hPen
--               hPen = c_func(xCreatePen,{penstyle,penwidth,bgr})
--               hPen = c_func(xCreatePen,{penstyle-1,penwidth,bgr})
            if penstyle=XPG_DOTTED then
                set_struct_field(idLOGBRUSH,pLOGBRUSH,"lbStyle",PS_SOLID)
                set_struct_field(idLOGBRUSH,pLOGBRUSH,"lbColor",bgr)
                -- (PS_COSMETIC implied)
                hPen = c_func(xExtCreatePen,{PS_ALTERNATE,1,pLOGBRUSH,0,NULL})
            else
                hPen = c_func(xCreatePen,{penstyle-1,penwidth,bgr})
            end if
--               hPen = c_func(xCreatePen,{penstyle-2,penwidth,bgr})
--               hPen = c_func(xCreatePen,{penstyle+2,penwidth,bgr})
--?{"penstyle",penstyle+2}
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

--integer gdots = 0, gten = 1000

integer rB, gB, bB, 
        rL, gL, bL

local procedure plot(gdx canvas, atom x, y, c, bool bFlip=false)
--  plot the pixel at (x, y) with brightness c (where 0 <= c <= 1)
--  note that bFlip is exclusively intended for wu_line() only.
    if bFlip then {x,y} = {y,x} end if
    atom C = 1-c, hDC, bgr
    if backend=XPG_WINAPI then
--And you really shouldn't use SetPixel that's so slow. 
--Create a memory DC using CreateDIBSection and modify the pixels (RGBQUADs) directly.
--  if false then
        hDC = ctrl_xtra[canvas][CX_CANVAS_HDC]
--DEV horribly slow... (but deal with that *after* gImage...)
        bgr = c_func(xGetPixel,{hDC,x,y})
--12/12/23:
--      {rB,gB,bB} = to_rgba(bgr)
--      {rB,gB,bB} = to_rgba(xpg_WinAPI_rgb_to_bgr(bgr))
        {bB,gB,rB} = to_rgba(bgr)
    end if
    integer r = and_bits(rL*c+rB*C,#FF),
            g = and_bits(gL*c+gB*C,#FF),
            b = and_bits(bL*c+bB*C,#FF)
--  c = rgba(r,g,b)
--  gCanvasPixel(canvas,x,y,c)
    if backend=XPG_GTK then
        atom cairo = ctrl_xtra[canvas][CX_CANVAS_HDC][1]
        c_proc(cairo_set_source_rgb,{cairo,r/255,g/255,b/255})
        c_proc(cairo_set_line_width,{cairo,0.5})
        c_proc(cairo_move_to,{cairo,x,y})
        c_proc(cairo_line_to,{cairo,x+1,y+1}) -- best I can do
        c_proc(cairo_stroke,{cairo})
    elsif backend=XPG_WINAPI then
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
--  sequence dasher = dashers[style+1]
    sequence dasher = dashers[style]

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
    if backend=XPG_GTK then
--?{"line",ctrl_xtra[id][CX_CANVAS_FLAGS]}
        atom cairo = xpg_gtk_get_cairo(canvas)
--      integer penwidth = ctrl_xtra[canvas][CX_PENWIDTH]
--      if penwidth=1 and (x1=x2 or y1=y2) then
--          c_proc(cairo_set_line_width,{cairo,2})
--      end if
--      c_proc(cairo_move_to,{cairo,x1,y1})
--      c_proc(cairo_move_to,{cairo,x1-.5,y1-.5})
        c_proc(cairo_move_to,{cairo,x1+.5,y1+.5})
--      c_proc(cairo_move_to,{cairo,x1,y1-.5})
--      c_proc(cairo_line_to,{cairo,x2,y2})
        c_proc(cairo_line_to,{cairo,x2+.5,y2+.5})
--      c_proc(cairo_line_to,{cairo,x2-.5,y2-.5})
--      c_proc(cairo_line_to,{cairo,x2,y2-.5})
        c_proc(cairo_stroke,{cairo})
    elsif backend=XPG_WINAPI then
        bool bUseNative = x1=x2 or y1=y2
--DEV bUseNative:=true for flat/vertical?? (sip)
        if wuline and not bUseNative then
            {rB,gB,bB} = to_rgba(gCanvasGetBackground(canvas))
            {rL,gL,bL} = to_rgba(gCanvasGetForeground(canvas))
            wu_line(canvas,x1,y1,x2,y2)
        else
            atom hDC = ctrl_xtra[canvas][CX_CANVAS_HDC]
            xpg_set_canvas_pen(canvas,hDC)
            bool bOK = c_func(xMoveToEx,{hDC,x1,y1,NULL})
            assert(bOK!=0)
            bOK = c_func(xLineTo,{hDC,x2,y2})
            assert(bOK!=0)
        end if
    else
        ?9/0 -- (unknown backend)
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
--      sequence dasher = dashers[style+1]
        sequence dasher = dashers[style]
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

local procedure xpg_WinAPI_wu_bezier(gdx canvas, atom x1, y1, x2, y2, x3, y3, x4, y4)
    --
    -- draw a cubic bezier curve from x1,y1 to x4,y4 
    --          using control points x2,y2 and x3,y3
    -- aside: {x2,y2}=={x3,y3} shd == gCanvasQuadBezier
    --
    integer style = ctrl_xtra[canvas][CX_PENSTYLE],
            width = ctrl_xtra[canvas][CX_PENWIDTH],
            wu_d = floor(width/2), wub_dots = 1
    {rB,gB,bB} = to_rgba(gCanvasGetBackground(canvas))
    {rL,gL,bL} = to_rgba(gCanvasGetForeground(canvas))

--if x1<x4 then
--  {x1,y1,x2,y2,x3,y3,x4,y4} = {x4,y4,x3,y3,x2,y2,x1,y1}
--end if
--if integer(x1) then x1 += 0.00001 end if
--if integer(y1) then y1 += 0.00001 end if
--if integer(x4) then x4 += 0.00001 end if
--if integer(y4) then y4 += 0.00001 end if
    sequence curve = {{x1,y1,x2,y2,x3,y3,x4,y4}},
        wub_dasher = dashers[style]
    while length(curve) do -- main bisect loop
        {x1,y1,x2,y2,x3,y3,x4,y4} = curve[$]
        --
        -- Uses the de Castejau method, with particular credit to 
        -- https://hcklbrrfnn.files.wordpress.com/2012/08/bez.pdf
        -- (or see a similar diagram in gCanvas.htm/technicalia)
        --
        --   -- l,m,r are the first mid-points,
        atom lx = (x1+x2)/2,  ly = (y1+y2)/2,
             mx = (x2+x3)/2,  my = (y2+y3)/2,
             rx = (x3+x4)/2,  ry = (y3+y4)/2,
             -- n,p are the secondary mid-points,
             nx = (lx+mx)/2,  ny = (ly+my)/2,
             px = (mx+rx)/2,  py = (my+ry)/2,
             -- q is the (single) found point on the curve.
             qx = (nx+px)/2,  qy = (ny+py)/2,
             -- the size of the square containing s,q,e:
             sx = max({x1,qx,x4})-min({x1,qx,x4}),
             sy = max({y1,qy,y4})-min({y1,qy,y4})

            atom dx = x1-x4,
                 dy = y1-y4
            bool bSteep = abs(dx)<abs(dy)

        bool bSubPixel = sx<1 and sy<1
        if not bSubPixel then
            --
            -- bisect curve[$] until it is:
            --
--if integer(qx) then qx += 0.00001 end if
--if integer(qy) then qy += 0.00001 end if
            curve[$] = {qx,qy,px,py,rx,ry,x4,y4}
            curve  &= {{x1,y1,lx,ly,nx,ny,qx,qy}}
        else
            curve = curve[1..$-1] -- discard/done
            --
            -- It is quite likely we will have split a segment length 1.5
            -- into 0.76 and 0.77, since nowt here is genuinely straight,
            -- and one of those straddles (say) y=12 whereas the other is 
            -- entirely between y=12 and y=13. For the straddler estimate
            -- x using straight-line algebra and draw two pixels, whereas
            -- we can/should just ignore the other and let prev/next segs
            -- set their more accurate pixels (on y=12 and y=13).
            --
            wub_dots = iff(wub_dots=length(wub_dasher)?1:wub_dots+1)
            if wub_dasher[wub_dots] then
                if ceil(y1)!=ceil(y4) then
                    atom cy = floor(max(y1,y4)),
                         iy = x1 + dx/dy*(cy-y1)
                    integer x = floor(iy)
                    atom fx = iy-x
                    if wu_d=0 then
                        plot(canvas,x,cy,1-fx)
                        plot(canvas,x+1,cy,fx)
                    else
                        plot(canvas,x-wu_d,cy,1-fx)
                        for i=-wu_d+1 to wu_d-1 do
                            plot(canvas,x+i,cy,1)
                        end for
                        plot(canvas,x+wu_d,cy,fx)
                    end if
                end if -- straddles a unit y
                if ceil(x1)!=ceil(x4) then
                    atom cx = floor(max(x1,x4)),
                         ix = y1 + dy/dx*(cx-x1)
                    integer y = floor(ix)
                    atom fy = ix-y
                    if wu_d=0 then
                        plot(canvas,cx,y,1-fy)
                        plot(canvas,cx,y+1,fy)
                    else
                        cx -= 1
                        plot(canvas,cx,y-wu_d,1-fy)
                        for i=-wu_d+1 to wu_d-1 do
                            plot(canvas,cx,y+i,1)
                        end for
                        plot(canvas,cx,y+wu_d,fy)
                    end if
                end if -- straddles a unit x
            end if -- dasher
        end if -- subpixel
    end while -- main bisect loop
end procedure

--/*
--procedure xCanvasCubicBezier(gdx canvas, atom x1, y1, xc1, yc1, xc2, yc2, x2, y2, integer style=-1, width=-1, atom colour=-1, bool aa=true)
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
--      wu_d = floor(gCanvasGetLineWidth(canvas)/2)
        xpg_WinAPI_wu_bezier(canvas,x1,y1,xc1,yc1,xc2,yc2,x2,y2)
    else
        gCanvasCubicBezier(canvas,x1,y1,xc1,yc1,xc2,yc2,x2,y2)
    end if
    if colour!=-1 then gCanvasSetForeground(canvas,pcolour) end if
    if width!=-1 then gCanvasSetLineWidth(canvas,pwidth) end if
    if style!=-1 then gCanvasSetLineStyle(canvas,pstyle) end if
--end procedure
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
    if backend=XPG_GTK then
        atom cairo = xpg_gtk_get_cairo(canvas)
--      c_proc(cairo_move_to,{cairo,x1,y1})
        c_proc(cairo_move_to,{cairo,x1+.5,y1+.5})
--      c_proc(cairo_curve_to,{cairo,xc1,yc1,xc2,yc2,x2,y2})
        c_proc(cairo_curve_to,{cairo,xc1+.5,yc1+.5,xc2+.5,yc2+.5,x2+.5,y2+.5})
        c_proc(cairo_stroke,{cairo})
    elsif backend=XPG_WINAPI then
--if wuline then -- (DEV/always)
--if true then
        xpg_WinAPI_wu_bezier(canvas,x1,y1,xc1,yc1,xc2,yc2,x2,y2)
--else
--      integer onelong = get_struct_size(idPOINT)/2
--      atom p4 = allocate(onelong*8),
--          hDC = ctrl_xtra[canvas][CX_CANVAS_HDC],
--       hBrush = NullBrushID
--      xpg_set_canvas_pen(canvas,hDC)
--      hBrush = c_func(xSelectObject,{hDC,hBrush})
--      pokeN(p4,{x1,y1,xc1,yc1,xc2,yc2,x2,y2},onelong)
--      bool bOK = c_func(xPolyBezier,{hDC,p4,4})
--      assert(bOK)
--      hBrush = c_func(xSelectObject,{hDC,hBrush}) -- (restore)
--      -- aside: don't delete local sequence brushes[] entries!
--      free(p4)
----       get_struct_size(idPOINT)
----        xPolyBezier = define_c_func(GDI32,"PolyBezier",
----            {C_PTR,     --  HDC hdc
----             C_PTR,     --  const POINT *lppt
----             C_INT},    --  DWORD cPoints
----            C_INT)      -- BOOL
--end if
    else
        ?9/0 -- (unknown backend)
    end if

--
--  if pstyle!=XPG_CONTINUOUS then gCanvasSetLineStyle(canvas,pstyle) end if
--  if pwidth!=1 then gCanvasSetLineWidth(canvas,pwidth) end if
    if colour!=-1 then gCanvasSetForeground(canvas,pcolour) end if
end procedure

--     procedure gCanvasQuadBezier(gdx canvas, atom x1, y1, cx, cy, x2, y2, integer style=-1, width=-1, colour=-1, bool aa=true)
global procedure gCanvasQuadBezier(gdx canvas, atom x1, y1, cx, cy, x2, y2, integer colour=-1)
    -- draw a cubic bezier curve from x1,y1 to x2,y2 
    --                 using the control point cx,cy
--  gCanvasCubicBezier(canvas,x1,y1,cx,cy,cx,cy,x2,y2,style,width,colour,aa)
    gCanvasCubicBezier(canvas,x1,y1,cx,cy,cx,cy,x2,y2,colour)
end procedure

global function gRotatePolygon(sequence poly, object angle)
--
--  DEV this is a fair bit slower than I thought it would be...
--  In demo/xpGUI/gCanvasPolygon.exw the Phix logo with 33 {x,y}
--  rotates worse than twice as slowly as the house with 5 {x,y}
--  whereas I expected it would be keyboard-repeat-rate bound...
--
    atom cx, cy
    if sequence(angle) then
        {angle,cx,cy} = angle
    else
        {cx,cy} = poly[1]
    end if
    if angle!=0 then
--      poly = deep_copy(poly)
        integer lp = length(poly)
        sequence res = repeat(0,lp)
        atom r = angle*XPG_DEG2RAD,
             s = sin(r),
             c = cos(r)
--      for i=1 to length(poly) do
        for i=1 to lp do
            sequence pi = poly[i]
            integer lpi = length(pi)
--          if lpi!=0 then
            if lpi=0 then
                res[i] = {}
            else
--              poly[i] = 0 -- (kill refcount)
                sequence ri = repeat(0,lpi)
                for j=1 to lpi-1 by 2 do
                    integer k = j+1
                    atom x = pi[j] - cx,
                         y = pi[k] - cy
                    -- counter-clockwise:
--                  {x,y} = {y*s + x*c + cx, 
--                           y*c - x*s + cy}
                    -- clockwise:
--                  {x,y} = {x*c - y*s + cx, 
--                           x*s + y*c + cy}
--                  pi[j] = x
--                  pi[k] = y
--                  pi[j] = x*c - y*s + cx
--                  pi[k] = x*s + y*c + cy
                    ri[j] = x*c - y*s + cx
                    ri[k] = x*s + y*c + cy
                end for
--              poly[i] = pi
                res[i] = ri
            end if
        end for
        return res
    end if
    return poly
end function

global procedure gCanvasPolygon(gdx canvas, sequence poly, bool bFilled=true, integer colour=-1, style=-1, width=-1)
--global procedure gCanvasPolygon(gdx canvas, sequence poly, bool bFilled=true, integer colour=-1, style=-1, width=-1, object angle=0)
    --
    -- draw a (possibly multi-part) polygon.
    -- much credit and thanks to http://alienryderflex.com/polygon/ (but not much final code)
    --
    integer pcolour
    if colour!=-1 then
        pcolour = ctrl_xtra[canvas][CX_CANVAS_FORE]
        if colour!=-1 then gCanvasSetForeground(canvas,colour) end if
    end if
    assert(not bFilled or style=-1 or style=XPG_CONTINUOUS)
    assert(not bFilled or width=-1 or width=1)
    integer pstyle, pwidth
    if style!=-1 then pstyle = gCanvasGetLineStyle(canvas) gCanvasSetLineStyle(canvas,style) end if
    if width!=-1 then pwidth = ctrl_xtra[canvas][CX_PENWIDTH] gCanvasSetLineWidth(canvas,width) end if
--  if filled and (pwidth!=1 or (width!=-1 and width!=1)) then
--      width = 1
--  end if
    -- draw outline and if filled collect all y unit axis intersections
    sequence intersect = {}
    integer l = length(poly), start = 1;
    for i=1 to l do
        sequence pi = poly[i]
        integer lpi = length(pi)
        if lpi==0 then -- (new loop)
            start = i+1
        else
            integer j = iff(i=l or poly[i+1]={}?start:i+1)
            atom {iX,iY} = pi,
                 {aX,aY} = iff(lpi>2?pi[3..4]:{0,0}),
                 {bX,bY} = iff(lpi>4?pi[5..6]:{aX,aY}),
                 {jX,jY} = poly[j]
            if lpi==2 then  // straight line
                gCanvasLine(canvas,iX,iY,jX,jY)
            else
                gCanvasCubicBezier(canvas,iX,iY,aX,aY,bX,bY,jX,jY)
            end if
            if bFilled then
                -- collect y unit intersections.
                -- avoid any direct vertex hits:
                if integer(iY) then iY += 0.00001 end if
                if integer(jY) then jY += 0.00001 end if
                if lpi=2 then -- line
                    if iY>jY then {iY,iX,jY,jX} = {jY,jX,iY,iX} end if
                    atom dx = jX-iX,
                         dy = jY-iY
                    for y=ceil(iY) to floor(jY) do
                        intersect = append(intersect,{y,iX+dx/dy*(y-iY)})
                    end for
                else -- bezier
                    sequence curve = {{iX,iY,aX,aY,bX,bY,jX,jY}}
                    while length(curve) do -- main bisect loop
                        {iX,iY,aX,aY,bX,bY,jX,jY} = curve[$]
                        -- (these two may be superfluous...)
                        if integer(iY) then iY += 0.00001 end if
                        if integer(jY) then jY += 0.00001 end if
                        --   -- l,m,r are the first mid-points,
                        atom lx = (iX+aX)/2,  ly = (iY+aY)/2,
                             mx = (aX+bX)/2,  my = (aY+bY)/2,
                             rx = (bX+jX)/2,  ry = (bY+jY)/2,
                             -- n,p are the secondary mid-points,
                             nx = (lx+mx)/2,  ny = (ly+my)/2,
                             px = (mx+rx)/2,  py = (my+ry)/2,
                             -- q is the (single) found point on the curve.
                             qx = (nx+px)/2,  qy = (ny+py)/2,
                             -- the size of the square containing s,q,e:
                             sx = max({iX,qx,jX})-min({iX,qx,jX}),
                             sy = max({iY,qy,jY})-min({iY,qy,jY})
                        if sx>=1 or sy>=1 then
                            -- bisect curve[$] until it is subpixel
                            curve[$] = {qx,qy,px,py,rx,ry,jX,jY}
                            curve  &= {{iX,iY,lx,ly,nx,ny,qx,qy}}
                        else
                            curve = curve[1..$-1] -- discard/done
                            if ceil(iY)!=ceil(jY) then -- straddler
                                integer y = floor(max(iY,jY))
                                atom iy = iX + (iX-jX)/(iY-jY)*(y-iY)
                                intersect = append(intersect,{y,iy})
                            end if -- straddles
                        end if -- subpixel
                    end while -- bisect loop
                end if -- line/bezier segment
            end if -- bFilled
        end if -- (not new loop)
    end for
    if bFilled then
        assert(even(length(intersect)))
        intersect = sort(intersect)
        for i=1 to length(intersect) by 2 do
            atom {y1,x1} = intersect[i],
                 {y2,x2} = intersect[i+1]
            assert(y1==y2 and integer(y1))
            x1 = ceil(x1)
--          x1 = floor(x1) -- no...
--          x1 = floor(x1+1) -- ok... (but that proves nothing...)
--          x2 = floor(x2) -- but also no...
            x2 = ceil(x2) -- (note added to docs re scattered +1s)
--          x2 = floor(x2+1) -- ok... (ditto)
            if x1<x2 then
--          if x1<=x2 then -- (hmm, does not seem to make any difference...)
                gCanvasLine(canvas,x1,y1,x2,y1)
            end if
        end for
    end if
    if colour!=-1 then gCanvasSetForeground(canvas,pcolour) end if
    if style!=-1 then gCanvasSetLineStyle(canvas,pstyle) end if
    if width!=-1 then gCanvasSetLineWidth(canvas,pwidth) end if
end procedure

global procedure gCanvasArc(gdx canvas, atom xc, yc, w, h, angle1, angle2, integer flags=0, style=-1, width=-1, atom colour=-1)
    integer pstyle, pwidth, pcolour
    if style!=-1 then pstyle = gCanvasGetLineStyle(canvas) gCanvasSetLineStyle(canvas,style) end if
    if width!=-1 then pwidth = gCanvasGetLineWidth(canvas) gCanvasSetLineWidth(canvas,width) end if
    if colour!=-1 then pcolour = ctrl_xtra[canvas][CX_CANVAS_FORE] gCanvasSetForeground(canvas,colour) end if
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

    if backend=XPG_GTK then
        atom cairo = xpg_gtk_get_cairo(canvas),
           {r,g,b} = to_rgba(ctrl_xtra[canvas][CX_CANVAS_FORE])
        c_proc(cairo_set_source_rgb,{cairo,r/255,g/255,b/255})

--      for iter=1 to iff(bFilled?2:1) do
            c_proc(cairo_new_path,{cairo})
            c_proc(cairo_save,{cairo})
            -- This took me forever to figure out. I finally found (just one) item of interest,
            -- httpdead://github.com/ELWIN-MAO/gtk/blob/master/gtkroundedbox.c (dead link) said:
            --  cairo_translate (cr,xc,yc);
            --  cairo_scale (cr,xradius,yradius);
            --  cairo_arc (cr,0,0,1.0,angle1,angle2);
            -- of course, nowt in the docs, SO, or any tutorial covers this in "English"..
            c_proc(cairo_translate,{cairo,xc,yc})
--          c_proc(cairo_translate,{cairo,xc+0.5,yc+0.5})
            c_proc(cairo_scale,{cairo,w/2,h/2})
            c_proc(cairo_arc,{cairo,0,0,1,a1r,a2r})
            if and_bits(flags,XPG_SECTOR) then
                c_proc(cairo_line_to,{cairo,0,0})
                c_proc(cairo_close_path,{cairo})
            end if
            c_proc(cairo_restore,{cairo})
--          if not bFilled then exit end if
        if bFilled then
            c_proc(cairo_fill,{cairo})
        else
--          bFilled = false
--          atom {r,g,b} = to_rgba(ctrl_xtra[canvas][CX_CANVAS_FORE])
--          c_proc(cairo_set_source_rgb,{cairo,r/255,g/255,b/255})
--if wuline then exit end if
--      end for
            c_proc(cairo_stroke,{cairo})
        end if
    elsif backend=XPG_WINAPI then
--if wuline then
if wuline and not bFilled then -- DEV
--DEV/SUG if filled then width=1...
--  I think we want to collect all pairs, sort by x[h?], then paint outer and fill inner.
--  Slightly sticky point being that is ok for h-pairs but v-pairs would need some care.
--  Alternatively draw outer and collect inner, then flood-fill all inner inclusive [?]
--  Might just stick with cairo under GTK.

    {rB,gB,bB} = to_rgba(gCanvasGetBackground(canvas))
--  {rB,gB,bB} = to_rgba(pfillcolour)
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
--if not wuline then
        atom hDC = ctrl_xtra[canvas][CX_CANVAS_HDC],
--         bgClr = gCanvasGetBackground(canvas),
            bClr = gCanvasGetForeground(canvas),
--        hBrush = iff(bFilled?xpg_WinAPI_get_brush(bgClr):NullBrushID),
          hBrush = iff(bFilled?xpg_WinAPI_get_brush(bClr):NullBrushID),
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
end if
    else
        ?9/0 -- (unknown backend)
    end if

    if style!=-1 then gCanvasSetLineStyle(canvas,pstyle) end if
    if width!=-1 then gCanvasSetLineWidth(canvas,pwidth) end if
    if colour!=-1 then gCanvasSetForeground(canvas,pcolour) end if
--  if fillcolour!=-1 then gCanvasSetForeground(canvas,pfillcolour) end if
--  if fillcolour!=-1 then gCanvasSetBackground(canvas,pfillcolour) end if
end procedure

global procedure gCanvasCircle(gdx canvas, atom xc, yc, radius, boolean filled=false, integer style=-1, width=-1, atom colour=-1)
    atom diameter = radius*2
    gCanvasArc(canvas,xc,yc,diameter,diameter,0,360,filled,style,width,colour)
end procedure

global procedure gCanvasPixel(gdx canvas, atom x, y, colour=-1)
--  if colour=-1 then colour = gCanvasGetForeground(canvas) end if
    if colour=-1 then colour = ctrl_xtra[canvas][CX_CANVAS_FORE] end if
    if backend=XPG_GTK then
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
    elsif backend=XPG_WINAPI then
        atom hDC = ctrl_xtra[canvas][CX_CANVAS_HDC],
             bgr = xpg_WinAPI_rgb_to_bgr(colour)
        bool bOK = c_func(xSetPixelV,{hDC,x,y,bgr})
--      assert(bOK!=0)
    else
        ?9/0 -- (unknown backend)
    end if
end procedure

--global procedure gCanvasRect(gdx canvas, atom xmin, xmax, ymin, ymax, bool bFill=false, integer rc=0, style=-1, width=-1, atom colour=-1, fillcolour=-1)
global procedure gCanvasRect(gdx canvas, atom xmin, xmax, ymin, ymax, bool bFill=false, integer rc=0, style=-1, width=-1, atom colour=-1)
    integer pstyle, pwidth
    if style!=-1 then pstyle = gCanvasGetLineStyle(canvas) gCanvasSetLineStyle(canvas,style) end if
    if width!=-1 then pwidth = gCanvasGetLineWidth(canvas) gCanvasSetLineWidth(canvas,width) end if
--  atom pcolour, pfillcolour
    atom pcolour
    if colour!=-1 then
--      pcolour = gCanvasGetForeground(canvas)
        pcolour = ctrl_xtra[canvas][CX_CANVAS_FORE]
        gCanvasSetForeground(canvas,colour)
    end if
--  if fillcolour!=-1 then
--      assert(bFill,"no point setting a fill colour!")
--      pfillcolour = gCanvasGetBackground(canvas)
--      gCanvasSetBackground(canvas,fillcolour)
--  end if
    if backend=XPG_GTK then
--      atom cairo = xpg_gtk_get_cairo(canvas,bFill)
        atom cairo = xpg_gtk_get_cairo(canvas),
           {r,g,b} = to_rgba(ctrl_xtra[canvas][CX_CANVAS_FORE])
        c_proc(cairo_set_source_rgb,{cairo,r/255,g/255,b/255})
--      for iter=1 to iff(bFill?2:1) do
            if rc then
                c_proc(cairo_new_path,{cairo})
                c_proc(cairo_arc,{cairo,xmin+rc+.5,ymin+rc+.5,rc,PI,3*PI/2})
                c_proc(cairo_arc,{cairo,xmax-rc+.5,ymin+rc+.5,rc,3*PI/2,2*PI})
                c_proc(cairo_arc,{cairo,xmax-rc+.5,ymax-rc+.5,rc,0,PI/2})
                c_proc(cairo_arc,{cairo,xmin+rc+.5,ymax-rc+.5,rc,PI/2,PI})
                c_proc(cairo_close_path,{cairo})
            else
--              c_proc(cairo_rectangle,{cairo,xmin+.5,ymin+.5,xmax-xmin+1,ymax-ymin+1})
                c_proc(cairo_rectangle,{cairo,xmin+.5,ymin+.5,xmax-xmin,ymax-ymin})
            end if
--          if not bFill then exit end if
--          c_proc(cairo_fill,{cairo})
--          bFill = false
--          atom {r,g,b} = to_rgba(ctrl_xtra[canvas][CX_CANVAS_FORE])
--          c_proc(cairo_set_source_rgb,{cairo,r/255,g/255,b/255})
--      end for
        if bFill then
            c_proc(cairo_fill,{cairo})
        else
            c_proc(cairo_stroke,{cairo})
        end if
    elsif backend=XPG_WINAPI then
        atom hDC = ctrl_xtra[canvas][CX_CANVAS_HDC],
--         bgClr = gCanvasGetBackground(canvas),
            bClr = gCanvasGetForeground(canvas),
--        hBrush = iff(bFill?xpg_WinAPI_get_brush(bgClr):NullBrushID)
          hBrush = iff(bFill?xpg_WinAPI_get_brush(bClr):NullBrushID)
        xpg_set_canvas_pen(canvas,hDC)
        bool bOK
        hBrush = c_func(xSelectObject,{hDC,hBrush})
        if rc then
            bOK = c_func(xRoundRect,{hDC,xmin,ymin,xmax,ymax,rc,rc})
        else
            -- MSDN: The rectangle that is drawn excludes the bottom and right edges.
            bOK = c_func(xRectangle,{hDC,xmin,ymin,xmax+1,ymax+1})
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
--DEV erm, that was a bit wrong anyway!!
--  if fillcolour!=-1 then
--      gCanvasSetForeground(canvas,pfillcolour)
--  end if
end procedure

--/*
function rotate_point(atom x, y, cx, cy, angle)
  atom r = angle*XPG_DEG2RAD, s = sin(r), c = cos(r);
  // translate point back to origin:
  x -= cx;
  y -= cy;
  // rotate point and translate it back:
--anticlockwise
--  sequence xy = {x*c - y*s + cx, 
--               x*s + y*c + cy}
--clockwise
  sequence xy = {y*s + x*c + cx, 
                 y*c - x*s + cy}
  return xy
end function
--*/

local function xpg_shifta(atom x, y, w, h, rot_pt, angle)
--
-- shift the NW corner to effect rotation about rot_pt
--
    atom nx = x, ny = y
    if rot_pt!=XPG_NW and angle!=0 then -- (not the do nowt cases)
--maybe: (erm, but with rot_pt in consideration...)
-- 90: {x,y} -> {-y,x}
-- 180: {x,y} -> {-x,-y}
-- 270: {x,y} -> {y,-x}
--      if angle=90 then
--          {tx,ty} = {-vy,vx}
--      elsif angle=180 then
--          {tx,ty} = {-vx,-vy}
--      elsif angle=270 then
--          {tx,ty} = {vy,-vx}
--      else
        --
        -- {vx,vy} is the vector from rot_pt to {x,y} at XPG_NW, in other
        --          words a transform of {x,y} such that rot_pt is {0,0}.
        --          All non-0 vx|y will in practice be -ve, which makes
        --          sense when you consider where XPG_NW is, vs other 8.
        -- {tx,ty} is {vx,vy} rotated about {0,0} by angle (std geometry).
        --          Note that standard geometry has y=0 at the bottom,
        --          whereas xpGUI has it at the top: signs / [counter]
        --          clockwise direction may therefore be a bit squiffy.
        -- {nx,ny} is obvs {x,y} adjusted by difference between v and t.
        --
        -- reminder: XPG_{N|E|S|W} are four-bit 0bWENS bitmasks, with
        --           neither 0b1100 nor 0b0011 ever simultaneously set.
        atom vx = {-w/2,-w,0}[floor(rot_pt/0b100)+1],
             --ie (  C, E, W) aka Centre/East/West, as (0|1|2)+1
             vy = {-h/2,-h,0}[rmdr(rot_pt,0b100)+1],
             --ie (  C, S, N) aka Centre/South/North,     ""
             ra = (360-angle)*XPG_DEG2RAD, -- (cw <==> ccw, radians)
              c = cos(ra),
              s = sin(ra),
             tx = vy*s+vx*c,
             ty = vy*c-vx*s
        nx += tx-vx
        ny += ty-vy
    end if
    return {nx,ny}
end function

local constant XPG_NORESET = 0x4 -- for xpg_redraw_list() [DEV not actually used yet]

global procedure gCanvasText(gdx canvas, atom x, y, string text, integer align=XPG_E, object angle=0, colour=-1, integer style=-1)
--?"gCanvasText"
    if length(text)!=0 then
        x = round(x)
        y = round(y)
        if align=XPG_CENTER then align=XPG_C end if
        integer N = and_bits(align,XPG_N),
                E = and_bits(align,XPG_E),
                W = and_bits(align,XPG_W),
                S = and_bits(align,XPG_S),
                rot_pt = XPG_NW
        if sequence(angle) then
            {rot_pt,angle} = angle
            if rot_pt=XPG_CENTER then rot_pt=XPG_C end if
        end if
        atom {fgclr, bgclr} = iff(sequence(colour)?colour:{colour,-1}), pfgclr
        if fgclr!=-1 then
--          pfgclr = gCanvasGetForeground(canvas)
            pfgclr = ctrl_xtra[canvas][CX_CANVAS_FORE]
            gCanvasSetForeground(canvas,fgclr)
        end if
        string pstyle
        if style!=-1 then
            -- style: may be XPG_NORMAL, XPG_BOLD, XPG_ITALIC, or XPG_BOLDITALIC.
            pstyle = gGetAttribute(canvas,"FONT")
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
            if pstyle==face then
                style = -1
            else
                gSetAttribute(canvas,"FONT",face)
            end if
        end if

        if backend=XPG_GTK then
            atom {cairo,layout} = ctrl_xtra[canvas][CX_CANVAS_HDC]
            -- aside: this actually invokes pango_layout_set_text...
            integer {w,h} = gGetTextExtent(canvas,split(text,'\n'))
            x -= iff(W?w+2:iff(E?-2:ceil(w/2)))
            y -= iff(N?h+1:iff(S?0:ceil(h/2)+1))
            if angle!=0 then
                {x,y} = xpg_shifta(x,y,w,h,rot_pt,angle)
            end if
            if bgclr!=-1 then
                if angle=0 then
                    gCanvasRect(canvas,x,x+w,y,y+h,true,colour:=bgclr)
                else
                    sequence poly = {{x,y},{x+w,y},{x+w,y+h},{x,y+h}}
                    poly = gRotatePolygon(poly, angle)
                    gCanvasPolygon(canvas,poly,true,colour:=bgclr)
                end if
            end if
            c_proc(cairo_move_to,{cairo,x,y})
            c_proc(cairo_save,{cairo})
            if angle then
                c_proc(cairo_rotate,{cairo,angle*PI/180})
            end if
            atom {r,g,b} = to_rgba(ctrl_xtra[canvas][CX_CANVAS_FORE])
            c_proc(cairo_set_source_rgb,{cairo,r/255,g/255,b/255})
            c_proc(pango_cairo_show_layout,{cairo,layout})
    --      c_proc(xg_object_unref,{layout})
            c_proc(cairo_restore,{cairo})
        elsif backend=XPG_WINAPI then
            if angle!=ctrl_xtra[canvas][CX_TXTANGLE] then
                integer font = ctrl_font[canvas]
                string fontname = cached_fontnames[font]
                xpg_set_font(canvas,fontname,angle)
            end if
            atom hDC = ctrl_xtra[canvas][CX_CANVAS_HDC],
                 clr = ctrl_xtra[canvas][CX_CANVAS_FORE],
                 bgr = xpg_WinAPI_rgb_to_bgr(clr), prev
            if bgclr=-1 then
                {} = c_func(xSetBkMode,{hDC,TRANSPARENT})
            else
                {} = c_func(xSetBkMode,{hDC,OPAQUE})
                atom bgbgr = xpg_WinAPI_rgb_to_bgr(bgclr)
                {} = c_func(xSetBkColor,{hDC,bgbgr})
            end if
    --DEV should we not be restoring this somewhere/sometimes?? (spotted in passing)
            prev = c_func(xSetTextColor,{hDC,bgr})
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
            assert(bOK!=0)
        else
            ?9/0 -- (unknown backend)
        end if
        if fgclr!=-1 then
            gCanvasSetForeground(canvas,pfgclr)
        end if
        if style!=-1 and not and_bits(style,XPG_NORESET) then
            gSetAttribute(canvas,"FONT",pstyle)
        end if
    end if
end procedure

--gboolean scroll_event(GtkWidget* self, GdkEventScroll event, gpointer user_data)
local function xpg_gtk_scroll(atom /*widget*/, event, gdx id)
    integer direction = get_struct_field(idGdkEventScroll,event,"direction"),
            state = get_struct_field(idGdkEventScroll,event,"state"),
            delta = iff(direction?-1:+1),
            shift = and_bits(state,0b01)!=0,
            ctrl = and_bits(state,0b100)!=0,
            alt = and_bits(state,0b1000)!=0
--  ?{"xpg_gtk_scroll",id,"direction",direction,"state",state,"delta",delta,"shift",shift,"ctrl",ctrl,"alt",alt}
    xpg_mousewheel(id, delta, ctrl, shift, alt)
--see also WM_MOUSEWHEEL

--/*
typedef enum
{
  GDK_SCROLL_UP,
  GDK_SCROLL_DOWN,
  GDK_SCROLL_LEFT,
  GDK_SCROLL_RIGHT
} GdkScrollDirection;

GDK_SCROLL_UP
struct GdkEventScroll {
  GdkEventType type; -- GDK_SCROLL
  GdkWindow* window;
  gint8 send_event;
  guint32 time;
  gdouble x;
  gdouble y;
  GdkModifierType* state;
  GdkScrollDirection direction;
  GdkDevice* device;
  gdouble x_root;
  gdouble y_root;
  gdouble delta_x;
  gdouble delta_y;
  guint is_stop : 1;
}
--*/
    -- TRUE to stop other handlers from being invoked for the event. FALSE to propagate the event further.
    return false
end function

--DEV per canvas??
bool bGTKredraw_on_leave = false

-- gboolean leave_notify_event(GtkWidget* self, GdkEventCrossing event, gpointer user_data)
local function xpg_gtk_mouseleave(atom widget, event, gdx canvas)
    assert(canvas=xpg_getID(widget))
    integer event_type = get_struct_field(idGdkEventCrossing,event,"event_type")
    if event_type=GDK_LEAVE_NOTIFY
    and bGTKredraw_on_leave then
        gRedraw(canvas)
    end if
    -- TRUE to stop other handlers from being invoked for the event. FALSE to propagate the event further.
    return false
end function

local procedure xpg_Canvas(gdx id)
    -- (invoked via xpg_map)
    gdx parent = xpg_get_parent_id(id)
    gdx pid = gGetParent(id) -- nb parent skips gH/Vbox under WinAPI
    atom canvas, w, h
--DEV !=CANVAS?
    if ctrl_xtra[id][CX_CANVAS_TYPE]=TABLE then -- its a gTable then
--DEV why not just use SZ_NATURAL_W? similar for gList? gGraph?
--          w = ctrl_xtra[id][CX_GTL_ATTRS][TX_NATWIDTH]
--          h = ctrl_xtra[id][CX_GTL_ATTRS][TX_NATHIGHT]
        w = ctrl_size[id][SZ_NATURAL_W]
        h = ctrl_size[id][SZ_NATURAL_H]
    else
        {w,h} = gGetTextExtent(parent,"W")
        -- (specifically for gSplit:)
        integer uw = ctrl_size[id][SZ_USER_W],
                uh = ctrl_size[id][SZ_USER_H]
--?{"xpg_Canvas",w,h,uw,uh}
        if uw and uw<w then w = uw end if
        if uh and uh<h then h = uh end if
--?{"xpg_Canvas",w,h,uw,uh}
    end if

--  bool bShrink = gGetAttribute(id,"USERSIZE")!={0,0} and gGetAttribute(id,"SHRINK")
    if backend=XPG_GTK then
        canvas = c_func(gtk_drawing_area_new,{})
--1/12/23 (for gSplit)
        c_proc(gtk_widget_set_size_request,{canvas,w,h}) 
        xpg_setID(canvas,id)
--1/12/23... (for gSplit - certainly helps, but may only need to be a temp thing)
        ctrl_size[id][SZ_NATURAL_W] = w
        ctrl_size[id][SZ_NATURAL_H] = h

        c_proc(gtk_widget_set_events,{canvas,GDK_KBR_MASK+GDK_SCROLL_MASK+GDK_LEAVE_NOTIFY_MASK})
        xpg_gtk_signal_connect(canvas,iff(bGTK3?"draw":"expose-event"),xpg_gtk_canvas_draw,id)
        xpg_gtk_signal_connect(canvas,"button-press-event",xpg_gtk_click,id)
        xpg_gtk_signal_connect(canvas,"button-release-event",xpg_gtk_click,id)
        xpg_gtk_signal_connect(canvas,"motion-notify-event",xpg_gtk_mousemove,id)
        xpg_gtk_signal_connect(canvas,"focus-in-event",xpg_gtk_focusinout,id)
        xpg_gtk_signal_connect(canvas,"focus-out-event",xpg_gtk_focusinout,id)
        xpg_gtk_signal_connect(canvas,"scroll-event",xpg_gtk_scroll,id)
        xpg_gtk_signal_connect(canvas,"leave-notify-event",xpg_gtk_mouseleave,id)
        ctrl_xtra[id][CX_CANVAS_HDC] = {NULL,NULL}
        xpg_gtk_add_to(parent,canvas)
        c_proc(gtk_widget_realize,{canvas})
        if gGetAttribute(id,"SPLIT") then -- (private/undocumented)
            -- set the appropriate resizing cursor (WinAPI uses sep. classes)
            string orientation = gGetAttribute(pid,"ORIENTATION")
            bool bHoriz = (orientation="HORIZONTAL")
            atom window = c_func(gtk_widget_get_window,{canvas}),
                 display = c_func(gdk_window_get_display,{window}), csr
            if bHoriz then
                if gtk_col_resize_cursor=NULL then
                    gtk_col_resize_cursor = c_func(gdk_cursor_new_for_display,{display,GDK_SB_H_DOUBLE_ARROW})
                end if
                csr = gtk_col_resize_cursor
            else
                if gtk_row_resize_cursor=NULL then
                    gtk_row_resize_cursor = c_func(gdk_cursor_new_for_display,{display,GDK_SB_V_DOUBLE_ARROW})
                end if
                csr = gtk_row_resize_cursor
            end if
            assert(csr!=NULL)
--DEV/SUG we might want to store this for IN<-->ACTIVE, and maybe set NULL now: (test before trying!)
--          ctrl_xtra[id][CX_GTL_ATTRS][PX_CURSOR] = csr
            c_proc(gdk_window_set_cursor,{window,csr})
--          c_proc(gdk_window_set_cursor,{window,NULL}) -- (yep, this is fine!)
        end if
    elsif backend=XPG_WINAPI then
        atom dwStyle = or_all({WS_CHILD,WS_VISIBLE,BS_PUSHBUTTON,WS_TABSTOP})
        string class_name = "gCanvas"
        if gGetAttribute(id,"SPLIT") then -- (private/undocumented)
            bool bVert = and_bits(ctrl_flags[pid],CF_VERTICAL)!=0
            class_name = iff(bVert?"gVSplit":"gHSplit")
        end if
        canvas = xpg_WinAPI_create(id,class_name,"",parent,w,h,dwStyle,0)
    else
        ?9/0 -- (unknown backend)
    end if
end procedure

local function xpg_get_mouse_on_scrollbar(gdx canvas, integer mx, my)
    -- return "" if not on a scrollbar, else a two char code
    sequence sbinfo = ctrl_xtra[canvas][CX_SBINFO]
    integer {w, h} = gGetIntInt(canvas,"SIZE"),
            vsbvis = sbinfo[SB_VVISB],  sbwid = sbinfo[SB_VWIDE],
            hsbvis = sbinfo[SB_HVISB],  sbhgh = sbinfo[SB_HHIGH],
            sbvend = h-hsbvis*sbhgh, -- where vertical scrollbar ends
            sbhend = w-vsbvis*sbwid, -- where horizontal scrollbar ends
            vttop = sbinfo[SB_VTTOP],
            vtend = sbinfo[SB_VTEND],
            htlft = sbinfo[SB_HTLFT],
            htend = sbinfo[SB_HTEND]
--?{"xpg_get_mouse_on_scrollbar",mx,my,w,h}
--DEV not quite: eliminates looping when both, but not when only one...
--  (it might be the timer isn't stopping as it should when it hits the end??)
--  (or perhaps we should apply a MINSIZE such that arrows cannot overlap?)
--  bool bVert = vsbvis and mx>=w-sbwid and mx<=w,
--       bHorz = hsbvis and my>=h-sbhgh and my<=h
--  if bVert and ((not bHorz) or my<h-sbhgh) then
--  if vsbvis and mx>=w-sbwid and mx<=w then
    if vsbvis and mx>=w-sbwid and mx<w then
        if my>=0 and my<=sbhgh then     return "UA" -- uparrow
        elsif my<vttop then             return "AT" -- above thumb
        elsif my<=vtend then            return "VT" -- vertical thumb
        elsif my<sbvend-sbhgh then      return "BT" -- below thumb
        elsif my<=sbvend then           return "DA" -- downarrow
        end if
--  elsif bHorz and ((not bVert) or mx<w-sbwid) then
--  elsif hsbvis and my>=h-sbhgh and my<=h then
    elsif hsbvis and my>=h-sbhgh and my<h then
        if mx>=0 and mx<=sbwid then     return "LA" -- left arrow
        elsif mx<htlft then             return "LT" -- left of thumb
        elsif mx<=htend then            return "HT" -- horizontal thumb
        elsif mx<sbhend-sbwid then      return "RT" -- right of thumb
        elsif mx<=sbhend then           return "RA" -- right arrow
        end if
    end if
    return "" -- (not on any of the ten scrollbar regions)
end function

local constant integer SBC_BACK = #F0F0F0,
                     SBC_ARROWS = #606060, -- (inactive)
                     SBC_MONARB = #DADADA, -- mouse hovering on arrow background
                      SBC_THUMB = #CDCDCD, -- (inactive)
                  SBC_THUMBDRAG = #606060, -- thumb being dragged 
                   SBC_THUMBHOV = #A6A6A6, -- mouse hovering on a thumb
                  SBC_SCROLLHOV = #C0C0C0, -- hover off thumb
                     SBC_RESIZE = #BFBFBF

local procedure xpg_redraw_scrollbars(gdx canvas)
    integer {w, h} = gGetIntInt(canvas,"SIZE")
    integer {scrollw,scrollh} = gGetAttribute(canvas,"SCROLLSIZE")
    gCanvasSetLineStyle(canvas,XPG_CONTINUOUS)
    gCanvasSetLineWidth(canvas,1)
    integer {mx,my} = gGetAttribute(canvas,"MOUSEPOS"),
             x, y -- scratch vars

    string monsba = xpg_get_mouse_on_scrollbar(canvas,mx,my)
--?{"xpg_redraw_scrollbars",monsba,mx,my}
    sequence sbinfo = ctrl_xtra[canvas][CX_SBINFO]
    ctrl_xtra[canvas][CX_SBINFO] = NULL
    integer --scrollw = sbinfo[SB_SCRLW],
            --scrollh = sbinfo[SB_SCRLH],
            sbwid = sbinfo[SB_VWIDE],
            sbhgh = sbinfo[SB_HHIGH],
            origx = sbinfo[SB_ORIGX],
            origy = sbinfo[SB_ORIGY]
    bool hsbvis = w<scrollw, -- (w/o vertical, for now...)
         vsbvis = (h-(hsbvis*sbwid))<scrollh,
         bDragging = sbinfo[SB_DRAGG],
         bTrackMouse = false
    string clikmon = sbinfo[SB_CKMON]
    if vsbvis then
--      gCanvasRect(canvas,w-sbwid,w,0,h,true,colour:=SBC_BACK,fillcolour:=SBC_BACK)
        gCanvasRect(canvas,w-sbwid,w,0,h,true,colour:=SBC_BACK)
        hsbvis = (w-sbwid)<scrollw -- (ok, /with/ vertical)
        if hsbvis then
            -- draw both (later) and a corner resizer thumb (now)
            gCanvasSetForeground(canvas,SBC_RESIZE)
            for i=1 to 3 do
                x = w-4
--                  y = h-i*3-1
                y = h-i*3
                for j=1 to i do
--DEV: I seem to have ended up drawing a 3x3 to get a 2x2 to appear....
--DEV: this may/shd be temporary... (resize doobrie)
if backend=XPG_WINAPI then
                    gCanvasRect(canvas,x-1,x+1,y-1,y+1)
elsif backend=XPG_GTK then
                    gCanvasRect(canvas,x,x,y,y)
else
    ?9/0
end if
                    x -= 3
                    y += 3
                end for
            end for
--      else
--          -- draw vertical only (full height, later)
        end if
        integer sbvend = h - hsbvis*sbhgh
        -- up and down arrows on the vertial:
        -- (aside: arrow regions are square...)
        integer uarrclr = SBC_ARROWS,
                darrclr = SBC_ARROWS
        if monsba="UA" then -- highlight uparrow background
--          gCanvasRect(canvas,w-sbwid+1,w-1,0,sbwid,true,colour:=SBC_MONARB,fillcolour:=SBC_MONARB)
            gCanvasRect(canvas,w-sbwid+1,w-1,0,sbwid,true,colour:=SBC_MONARB)
            uarrclr = XPG_BLACK
            bTrackMouse = true
        elsif monsba="DA" then -- highlight downarrow background
--          gCanvasRect(canvas,w-sbwid+1,w-1,sbvend-sbwid,sbvend,true,colour:=SBC_MONARB,fillcolour:=SBC_MONARB)
            gCanvasRect(canvas,w-sbwid+1,w-1,sbvend-sbwid,sbvend,true,colour:=SBC_MONARB)
            darrclr = XPG_BLACK
            bTrackMouse = true
        end if
        -- draw the up and down arrows themselves
        --DEV: these 3/6/7/8/9 should probably be derived from SBWID...
        x = w-9
        for i=0 to 3 do -- 1 centre and 3 staggered side lines, ie/eg:
                        --              X            X     X
                        --             XXX           XX   XX
                        --            XX XX           XX XX
                        --           XX   XX           XXX
                        --           X     X            X
                        --          (3210123)       (3210123)
                        -- (but X chars are less square than pixels)
            integer xi = x-i,  yut = 7+i,  ydt = sbvend-8-i,
                               yub = 9+i,  ydb = sbvend-6-i
            gCanvasLine(canvas,xi,yut,xi,yub,colour:=uarrclr)
            gCanvasLine(canvas,xi,ydt,xi,ydb,colour:=darrclr)
            if i then
                xi = x+i
                gCanvasLine(canvas,xi,yut,xi,yub,colour:=uarrclr)
                gCanvasLine(canvas,xi,ydt,xi,ydb,colour:=darrclr)
            end if
        end for
        -- vertical thumb:
        integer vttop = sbwid,
                vtend = sbvend-sbwid,
                vtmax = sbvend-sbwid*2,
                vtlen = ceil((sbvend/scrollh)*vtmax)
        if vtend<vttop then
            -- draw as single line in the middle then
            vttop = floor((vttop+vtend)/2)
--DEV this ain't quite right either... (end==top draws nowt, when it should be a line)
--          vtend = vttop
--          vtend = vttop+1
            vtlen = 1
        end if
        vttop += round((origy/scrollh)*vtmax)
        vtend = vttop+vtlen
--      gCanvasRect(canvas,w-sbwid+1,w-1,vttop,vtend,true,colour:=SBC_THUMB,fillcolour:=SBC_THUMB)
        integer thumbclr = SBC_THUMB
        if bDragging and clikmon = "VT" then
            thumbclr = SBC_THUMBDRAG
        elsif monsba="VT" then
            thumbclr = SBC_THUMBHOV
        elsif monsba!="" then
            thumbclr = SBC_SCROLLHOV
        end if
        gCanvasRect(canvas,w-sbwid+1,w-1,vttop,vtend,true,colour:=thumbclr)
        sbinfo[SB_VTTOP] = vttop
        sbinfo[SB_VTEND] = vtend
    end if
    if hsbvis then
        integer sbhend = w - vsbvis*sbwid
--      gCanvasRect(canvas,0,sbhend,h-sbhgh,h,true,colour:=SBC_BACK,fillcolour:=SBC_BACK)
        gCanvasRect(canvas,0,sbhend,h-sbhgh,h,true,colour:=SBC_BACK)
        -- left and right arrows on the horizontal:
        integer larrclr = SBC_ARROWS,
                rarrclr = SBC_ARROWS
        if monsba="LA" then -- highlight leftarrow background
--          gCanvasRect(canvas,0,sbhgh,h-sbhgh+1,h-1,true,colour:=SBC_MONARB,fillcolour:=SBC_MONARB)
            gCanvasRect(canvas,0,sbhgh,h-sbhgh+1,h-1,true,colour:=SBC_MONARB)
            larrclr = XPG_BLACK
            bTrackMouse = true
        elsif monsba="RA" then -- highlight rightarrow background
--          gCanvasRect(canvas,sbhend-sbhgh,sbhend,h-sbhgh+1,h-1,true,colour:=SBC_MONARB,fillcolour:=SBC_MONARB)
            gCanvasRect(canvas,sbhend-sbhgh,sbhend,h-sbhgh+1,h-1,true,colour:=SBC_MONARB)
            rarrclr = XPG_BLACK
            bTrackMouse = true
        end if
        --DEV: these 3/5/7/9 should probably be derived from SBHGH...
        y = h-9
        for i=0 to 3 do -- 1 centre with 3 staggered lines, ie/eg:
                        --         XX       (3)       XX
                        --        XX        (2)        XX
                        --       XX         (1)         XX
                        --      XX          (0)          XX 
                        --       XX         (1)         XX
                        --        XX        (2)        XX
                        --         XX       (3)       XX
                        -- (but X chars are less square than pixels)
            integer yi = y-i,  xll = 5+i,  xrl = sbhend-9-i,
                               xlr = 7+i,  xrr = sbhend-7-i
            gCanvasLine(canvas,xll,yi,xlr,yi,colour:=larrclr)
            gCanvasLine(canvas,xrl,yi,xrr,yi,colour:=rarrclr)
            if i then
                yi = y+i
                gCanvasLine(canvas,xll,yi,xlr,yi,colour:=larrclr)
                gCanvasLine(canvas,xrl,yi,xrr,yi,colour:=rarrclr)
            end if
        end for
        -- horizontal thumb:
        integer htlft = sbhgh,
                htend = sbhend-sbhgh,
                htmax = sbhend-sbhgh*2,
                htlen = ceil((sbhend/scrollw)*htmax)
        if htend<htlft then
            -- draw as one line in the middle then
            htlft = floor((htlft+htend)/2)
--          htend = htlft
            htlen = 1
        end if
        htlft += round((origx/scrollw)*htmax)
--DEV this overruns by 1 pixel, plus I think I actually want a (min) 1-pixel gap.
        htend = htlft+htlen
--      gCanvasRect(canvas,htlft,htend,h-sbhgh+1,h-1,true,colour:=SBC_THUMB,fillcolour:=SBC_THUMB)
        integer thumbclr = SBC_THUMB
        if bDragging and clikmon = "HT" then
            thumbclr = SBC_THUMBDRAG
        elsif monsba="HT" then
            thumbclr = SBC_THUMBHOV
        elsif monsba!="" then
            thumbclr = SBC_SCROLLHOV
        end if
        gCanvasRect(canvas,htlft,htend,h-sbhgh+1,h-1,true,colour:=thumbclr)
        sbinfo[SB_HTLFT] = htlft
        sbinfo[SB_HTEND] = htend
    end if
    sbinfo[SB_HVISB] = hsbvis
    sbinfo[SB_VVISB] = vsbvis
    sbinfo[SB_MONLD] = monsba
    ctrl_xtra[canvas][CX_SBINFO] = sbinfo
--GTK equivalent to TrackMouseEvent and WM_MOUSELEAVE
--def window_exit(widget, event, user_data)
--The event is very important because its variable 'event.detail' tells us exactly what kind of leave-event happened. 
--In your case, you want to test if it is equal to 'gtk.gdk.NOTIFY_NONLINEAR', because that means the pointer has "truly" left the window.
--So, you should probably put something like
--if (event.detail != gtk.gdk.NOTIFY_NONLINEAR)  { return; }
--window.connect("leave-notify-event", window_exit, "")
    if backend=XPG_GTK then
        bGTKredraw_on_leave = bTrackMouse
    elsif backend=XPG_WINAPI then
        if bTrackMouse
        or get_struct_field(idTRACKMOUSEEVENT,pTRACKMOUSEEVENT,"dwFlags")=TME_LEAVE then
            -- ask for a WM_MOUSELEAVE msg, when/if it actually does:
            atom dwFlags = iff(bTrackMouse?0:TME_CANCEL)+TME_LEAVE
            set_struct_field(idTRACKMOUSEEVENT,pTRACKMOUSEEVENT,"hwndTrack",ctrl_handles[canvas])
            set_struct_field(idTRACKMOUSEEVENT,pTRACKMOUSEEVENT,"dwFlags",dwFlags)
            bool bOK = c_func(xTrackMouseEvent,{pTRACKMOUSEEVENT})
            assert(bOK)
        end if
    else
        ?9/0
    end if
end procedure

local function xpg_scrollbar_key_handler(gdx canvas, integer c, bool ctrl, shift, alt)
    object sbinfo = ctrl_xtra[canvas][CX_SBINFO]
    if sequence(sbinfo) then
--? was 0 13/11/23...
        object ltattr = ctrl_xtra[canvas][CX_GTL_ATTRS]
        integer {w, h} = gGetIntInt(canvas,"SIZE"), 
                pg = (c=VK_PGUP or c=VK_PGDN),
                ud = (c=VK_UP or c=VK_DOWN),
                sa = (shift or alt),
--              sa = (shift or alt or (ctrl_xtra[canvas][CX_CANVAS_TYPE]!=CANVAS)),
                ct = ctrl_xtra[canvas][CX_CANVAS_TYPE],
--ctrl_xtra[list][CX_GTL_ATTRS][LX_LINESTEP] = lh
                vpg = h-sbinfo[SB_HVISB]*sbinfo[SB_HHIGH], -- vertical page size
                hpg = w-sbinfo[SB_VVISB]*sbinfo[SB_VWIDE], -- horizontal page size
                origx = sbinfo[SB_ORIGX],
                origy = sbinfo[SB_ORIGY],
--DEV: note this implements up/downarrow scrolling on a gList(), but actually those
--      keys should alter the selected line, and only scroll when that hits top/btm.
--      (obvs. that key handling simply does not belong on a plain canvas/graph,
--            and a gTable has both that and a bit of left/right handling to do)
                step1 = iff(ct=LIST and ud?ltattr[LX_LINESTEP]:
                        iff(ct=TABLE and ud?ltattr[TX_LINESTEP]:1)),
                step = iff(pg?vpg
                             :iff(ctrl?iff(ud?vpg:hpg)
                                      :iff(sa?10:step1)))
        switch c do
            case VK_LEFT:           origx -= step
            case VK_RIGHT:          origx += step
            case VK_UP,VK_PGUP:     origy -= step
            case VK_DOWN,VK_PGDN:   origy += step
            default:          return XPG_CONTINUE
        end switch
        sbinfo = 0 -- (kill refcount)
        ctrl_xtra[canvas][CX_SBINFO][SB_ORIGX] = origx
        ctrl_xtra[canvas][CX_SBINFO][SB_ORIGY] = origy
        gRedraw(canvas)
    end if
    return XPG_CONTINUE
end function

-- aside: VT/HT missing from this set on purpose.
local constant {TMON,TKEY,TCTRL} = columnize({{"UA",VK_UP,false},
                                              {"AT",VK_UP,true},
                                              {"BT",VK_DOWN,true},
                                              {"DA",VK_DOWN,false},
                                              {"LA",VK_LEFT,false},
                                              {"LT",VK_LEFT,true},
                                              {"RT",VK_RIGHT,true},
                                              {"RA",VK_RIGHT,false}})

local procedure xpg_scrollbar_timer_action(gdx canvas)
    -- for when a mouse button is pressed and held down on a scrollbar:
    object sbinfo = ctrl_xtra[canvas][CX_SBINFO]
    if sequence(sbinfo) then
        string clikmon = sbinfo[SB_CKMON]
        integer k = find(clikmon,TMON), 
               ox = sbinfo[SB_ORIGX], 
               oy = sbinfo[SB_ORIGY]
        sbinfo = NULL -- (kill refcount)
        bool bCtrl = TCTRL[k],
             bShift = gGetGlobal("SHIFTKEY"),
             bAlt = gGetGlobal("ALTKEY")
        {} = xpg_scrollbar_key_handler(canvas,TKEY[k],bCtrl,bShift,bAlt)
        gdx timer = ctrl_xtra[canvas][CX_SBINFO][SB_TIMER]
        if  ox==ctrl_xtra[canvas][CX_SBINFO][SB_ORIGX]
        and oy==ctrl_xtra[canvas][CX_SBINFO][SB_ORIGY] then     -- no change
            gSetAttribute(timer,"RUN",false) -- switch off
        elsif timer then
            gSetAttribute(timer,"TIME",100)
        end if
    end if
end procedure

local procedure xpg_scrollbar_timer_cb(gdx timer)
    gdx canvas = gGetAttribute(timer,"USER_DATA")
    xpg_scrollbar_timer_action(canvas)
end procedure

--local function xpg_scrollbar_click(gdx canvas, sequence status, integer x, y)
local procedure xpg_scrollbar_click(gdx canvas, sequence status, integer x, y)
--  integer {button,pressed,ctrl,shift,alt} = status
--  printf(1,"click(button:%c, pressed:%c, ctrl:%d, shift:%d, alt:%d, x:%d, y:%d)\n",
--           {button,pressed,ctrl,shift,alt,x,y})
    object sbinfo = ctrl_xtra[canvas][CX_SBINFO]
    if sequence(sbinfo) then
        sbinfo = NULL
        string clikmon = xpg_get_mouse_on_scrollbar(canvas,x,y)
        ctrl_xtra[canvas][CX_SBINFO][SB_CKMON] = clikmon
--?{"CLIKMON",clikmon}
        ctrl_xtra[canvas][CX_SBINFO][SB_CLIKX] = x
        ctrl_xtra[canvas][CX_SBINFO][SB_CLIKY] = y
        bool dragg = status[2]!='R',
           wasdrag = ctrl_xtra[canvas][CX_SBINFO][SB_DRAGG]
        ctrl_xtra[canvas][CX_SBINFO][SB_DRAGG] = dragg
        gdx timer = ctrl_xtra[canvas][CX_SBINFO][SB_TIMER]
        if dragg then
            if not find(clikmon,{"","VT","HT"}) then
                xpg_scrollbar_timer_action(canvas)
                if not timer then
                    timer = gTimer(xpg_scrollbar_timer_cb,500,true,canvas)
                    ctrl_xtra[canvas][CX_SBINFO][SB_TIMER] = timer
                else
                    gSetAttribute(timer,"TIME",500)
                    gSetAttribute(timer,"RUN",true)
                end if
                ctrl_xtra[canvas][CX_SBINFO][SB_DRAGG] = false
                dragg = false
            end if
        else
            if timer then gSetAttribute(timer,"RUN",false) end if
        end if
        if dragg!=wasdrag then
--NO...
--          xpg_redraw_scrollbars(canvas)
            atom handle = ctrl_handles[canvas]
            bool bOK = c_func(xInvalidateRect,{handle,NULL,true})
            assert(bOK)
        end if
    end if
--DEV did I mean XPG_IGNORE here? (or should this just be a procedure?)
--  return false
--end function
end procedure

local procedure xpg_scrollbar_mousemove(gdx canvas, integer x,y, bool left,middle,right)
    object sbinfo = ctrl_xtra[canvas][CX_SBINFO]
    if sequence(sbinfo) then
        string monsba = xpg_get_mouse_on_scrollbar(canvas,x,y)
        string clikmon = sbinfo[SB_CKMON]
        bool dragg = sbinfo[SB_DRAGG]
        if dragg then
            if left or right then
                integer origx = sbinfo[SB_ORIGX],
                        origy = sbinfo[SB_ORIGY],
                        clikx = sbinfo[SB_CLIKX],
                        cliky = sbinfo[SB_CLIKY]
                if clikmon="" then
                    origy += cliky-y
                    cliky = y
                    origx += clikx-x
                    clikx = x
                else
                    integer {w, h} = gGetIntInt(canvas,"SIZE")
                    if clikmon="VT" then
                        integer vtmax = h - sbinfo[SB_HVISB]*sbinfo[SB_HHIGH]-sbinfo[SB_VWIDE]*2
                        origy += round((y-cliky)*sbinfo[SB_SCRLH]/vtmax)
                        cliky = y
                    elsif clikmon="HT" then
                        integer htmax = w - sbinfo[SB_VVISB]*sbinfo[SB_VWIDE]-sbinfo[SB_HHIGH]*2
                        origx += round((x-clikx)*sbinfo[SB_SCRLW]/htmax)
                        clikx = x
                    end if
                end if
                sbinfo = NULL
                ctrl_xtra[canvas][CX_SBINFO][SB_ORIGX] = origx
                ctrl_xtra[canvas][CX_SBINFO][SB_ORIGY] = origy
                ctrl_xtra[canvas][CX_SBINFO][SB_CLIKX] = clikx
                ctrl_xtra[canvas][CX_SBINFO][SB_CLIKY] = cliky
                gRedraw(canvas)     
            else
                sbinfo = NULL
                ctrl_xtra[canvas][CX_SBINFO][SB_DRAGG] = false
            end if
        elsif monsba!=sbinfo[SB_MONLD] then -- (only if needed)
            sbinfo = NULL
            gRedraw(canvas)
        end if
    end if
end procedure

procedure xpg_canvas_wheel_handler(gdx canvas, integer direction, bool ctrl, shift, alt) 
--?{"xpg_canvas_wheel_handler",id, direction, ctrl, chift, alt}
    integer key = iff(ctrl?iff(direction=-1?VK_RIGHT:VK_LEFT)
                          :iff(direction=-1?VK_DOWN:VK_UP))
    {} = xpg_scrollbar_key_handler(canvas,key,false,shift,alt)
end procedure

--local function xpg_get_canvas_type(integer l)

local function xpg_set_canvas_attribute(gdx id, string name, object v, bool bMapped)
    integer ct = ctrl_xtra[id][CX_CANVAS_TYPE]
--?{"xpg_set_canvas_attribute",id,ct,CANVAS}
--  atom handle = ctrl_handles[id]
    if name="SCROLLSIZE"
    or name="SCROLLPORT"
    or name="VIEWPORT" then
        object sbinfo = ctrl_xtra[id][CX_SBINFO]
        ctrl_xtra[id][CX_SBINFO] = NULL
        integer flags = ctrl_xtra[id][CX_CANVAS_FLAGS]
        if v=NULL then
            flags -= and_bits(flags,CXCF_SCROLL)
            ctrl_xtra[id][CX_CANVAS_FLAGS] = flags
        else
            if atom(sbinfo) then
                sbinfo = repeat(0,SB_INFOLEN)
                sbinfo[SB_VWIDE] = 17
                sbinfo[SB_HHIGH] = 17
                sbinfo[SB_MONLD] = ""
                sbinfo[SB_CKMON] = ""
                if not find(xpg_scrollbar_click,internal_rtns) then
                    -- (let/permit/allow these to be a perfectly valid rtn)
                    internal_rtns &= xpg_scrollbar_click; 
                    internal_rtns &= xpg_scrollbar_mousemove; 
                    internal_rtns &= xpg_scrollbar_key_handler; 
                    internal_rtns &= xpg_canvas_wheel_handler; 
                    internal_rtns &= xpg_scrollbar_timer_cb; 
                end if
                gSetHandler(id,"CLICK",xpg_scrollbar_click)
                gSetHandler(id,"MOUSEMOVE",xpg_scrollbar_mousemove)
                gSetHandler(id,"KEY",xpg_scrollbar_key_handler)
                gSetHandler(id,"MOUSEWHEEL",xpg_canvas_wheel_handler)
                ctrl_xtra[id][CX_CANVAS_FLAGS] = or_bits(flags,CXCF_SCROLL)
            end if
            if string(v) then v = xpg_intint(v) end if
            if name="SCROLLSIZE" then
                integer {w,h} = v
                sbinfo[SB_SCRLW] = w
                sbinfo[SB_SCRLH] = h
            else -- name="SCROLLPORT" or name="VIEWPORT" then   
    --          if string(v) then v = xpg_intint(v) end if
    --?{"set VIEWPORT",v}
                integer {x,y} = v
                sbinfo[SB_ORIGX] = x
                sbinfo[SB_ORIGY] = y
            end if
            ctrl_xtra[id][CX_SBINFO] = sbinfo
        end if
        return true
    elsif ct!=CANVAS then   -- gGraph/gTable/gList
--      sequence gtlattr = ctrl_xtra[id][CX_GTL_ATTRS]
        integer lGTL = find(ct,gtl_types),
                gxdx = find(name,gtl_names[lGTL])
--?{"gxdx",gxdx,name,"lGTL",lGTL}
--?{"xpg_set_canvas_attribute",id,ct,name,gxdx}
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
    elsif name="SPLIT" then -- (private/undocumented)
        assert(v="AYE")
        -- (just do both...)
        assert(and_bits(ctrl_flags[id],CF_SPLIT)=0)
        ctrl_flags[id] += CF_SPLIT
        gdx parent = parent_ids[id] -- nb cannot still be null!
        assert(ctrl_types[parent]==BOX)
        assert(length(children_ids[parent])==3)
        assert(and_bits(ctrl_flags[parent],CF_SPLIT)=0)
        ctrl_flags[parent] += CF_SPLIT
        return true
    end if
    return false
end function

function xpg_get_canvas_attribute(gdx id, string name, object dflt)
    -- nb id may not be mapped, unlike most other xpg_get_xxx_attribute rouines...
    atom handle = ctrl_handles[id]
    integer ct = ctrl_xtra[id][CX_CANVAS_TYPE]
--?{"xpg_get_canvas_attribute",id, name, dflt,{ct,CANVAS}}
    if name="SCROLLSIZE"
    or name="SCROLLPORT"
    or name="VIEWPORT" then
        integer w = 0, h = 0, x = 0, y = 0,
                hsv = false, hsh = 0,
                vsv = false, vsw = 0,
                {sw,sh} = gGetAttribute(id,"SIZE")
        object sbinfo = ctrl_xtra[id][CX_SBINFO]
        if sequence(sbinfo) then
            w = sbinfo[SB_SCRLW]
            h = sbinfo[SB_SCRLH]
            x = sbinfo[SB_ORIGX]
            y = sbinfo[SB_ORIGY]
            hsv = sbinfo[SB_HVISB]
            hsh = sbinfo[SB_HHIGH]
            vsv = sbinfo[SB_VVISB]
            vsw = sbinfo[SB_VWIDE]
        end if
        if name="SCROLLSIZE" then
            if w=0 then w = sw end if
            if h=0 then h = sh end if
            sbinfo = {w,h}
        else --if name="SCROLLPORT" or name="VIEWPORT" then
            sw -= vsv*vsw
            sh -= hsv*hsh
            sbinfo = {x,y,sw,sh}
        end if
        return sbinfo
    elsif name="SPLIT" then -- (private/undocumented/once parent "known")
        return and_bits(ctrl_flags[id],CF_SPLIT)!=0
--      gdx parent = parent_ids[id]
--      assert(ctrl_types[parent]==BOX)
--      assert(length(children_ids[parent])==3)
--      return and_bits(ctrl_flags[parent],CF_SPLIT)!=0
    elsif ct!=CANVAS then -- gGraph/gTable/gList
        integer lGTL = find(ct,gtl_types),
                gxdx = find(name,gtl_names[lGTL])
        if gxdx then return ctrl_xtra[id][CX_GTL_ATTRS][gxdx] end if
--      if ct=LIST and name="VALUESTR" then
----?{lGTL,name,"..."}
--          return `gGetAttribute(LIST,"VALUESTR")...`
--      end if
    end if
    return xpg_return_default_attr(id,name,dflt)
end function

global function gCanvas(object redraw=NULL, sequence attributes="", args={})
    {redraw,attributes,args} = paranormalise_raa(redraw,attributes,args)
    if not bInit then xpg_Init() end if
    integer id = xpg_add_control(CANVAS,flags:=CF_UNMAPATTR+CF_EXPANDB)
--  xpg_register_handler(CANVAS,"REDRAW",{{1,1,"PO"}})
    xpg_register_handler(CANVAS,"REDRAW",{"PO","POII"})
--  xpg_register_handler(CANVAS,"DROP",{{1,1,"FI"}})
    xpg_register_handler(CANVAS,"DROP",{"FI"})
--  xpg_register_handler(CANVAS,"MOUSEWHEEL",{{5,5,"POIIII"}})
    xpg_register_handler(CANVAS,"MOUSEWHEEL",{"POIIII"})
    xpg_set_ctrl_msg(CANVAS,xpg_Canvas,xpg_set_canvas_attribute,
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
    xpg_check_unmapped(ids,CF_RADIO,length(radio_groups)+1)
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
    bool bChecked = c_func(gtk_toggle_button_get_active,{handle})
    xpg_check_changed(id,bChecked)
    return 0 -- (ignored)
end function

local procedure xpg_Checkbox(gdx id)
    -- (invoked via xpg_map)
    gdx parent = xpg_get_parent_id(id)
    atom checkbox
    string title = gGetAttribute(id,"TITLE")
    bool bRadio = and_bits(ctrl_flags[id],CF_RADIO)!=0
    integer rdx = ctrl_xtra[id]
    if backend=XPG_GTK then
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
--      xpg_gtk_signal_connect(checkbox,"realize",xpg_gtk_widget_realized,id)
        xpg_gtk_signal_connect(checkbox,"clicked",xpg_gtk_toggle_clicked,id)
        xpg_gtk_signal_connect(checkbox,"focus-in-event",xpg_gtk_focusinout,id)
        xpg_gtk_signal_connect(checkbox,"focus-out-event",xpg_gtk_focusinout,id)
        xpg_gtk_add_to(parent,checkbox)
--(temp?:)
--      atom vbox = c_func(gtk_vbox_new,{false,6})
--      atom hbox = c_func(gtk_hbox_new,{false,6})
--      c_proc(gtk_box_pack_start,{hbox,checkbox,false,false,0})
--      c_proc(gtk_box_pack_start,{vbox,hbox,false,false,0})
--      c_proc(gtk_container_add,{ctrl_handles[parent],vbox})
        c_proc(gtk_widget_realize,{checkbox})
    elsif backend=XPG_WINAPI then
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
        elsif backend=XPG_GTK then
--          c_proc(gtk_window_set_title,{handle,v}) 
            c_proc(gtk_button_set_label,{handle,xpg_gtk_mnemonicalize(v)})
--          c_proc(gtk_label_set_text_with_mnemonic,{handle,xpg_gtk_mnemonicalize(v)})
        elsif backend=XPG_WINAPI then
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
            elsif backend=XPG_GTK then
                c_proc(gtk_toggle_button_set_active,{handle,v})
            elsif backend=XPG_WINAPI then
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
        elsif backend=XPG_GTK then
            return peek_string(c_func(gtk_button_get_label,{handle}))
        elsif backend=XPG_WINAPI then
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
        elsif backend=XPG_GTK then
            return c_func(gtk_toggle_button_get_active,{handle})
        elsif backend=XPG_WINAPI then
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
--  xpg_register_handler(CHECKBOX,"VALUE_CHANGED",{{2,2,"POI"},{1,1,"PO"}})
    xpg_register_handler(CHECKBOX,"VALUE_CHANGED",{"POI","PO"})
    xpg_set_ctrl_msg(CHECKBOX,xpg_Checkbox,xpg_set_checkbox_attribute,
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
        if backend=XPG_GTK then
            atom clipboard = ctrl_handles[id]
            c_proc(gtk_clipboard_clear,{clipboard})
            if v!=NULL then
                c_proc(gtk_clipboard_set_text,{clipboard,v,-1})
            end if
        elsif backend=XPG_WINAPI then
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
        if backend=XPG_GTK then
            atom clipboard = ctrl_handles[id],
                     pClip = c_func(gtk_clipboard_wait_for_text,{clipboard})
            if pClip!=NULL then
                if fmt=CF_TEXT then
                    clip = peek_string(pClip)
                else -- if fmt=CF_UNICODETEXT then
                    clip = peek_wstring(pClip)
                end if
            end if
        elsif backend=XPG_WINAPI then
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
        if backend=XPG_GTK then
            atom clipboard = ctrl_handles[id]
            return c_func(gtk_clipboard_wait_is_text_available,{clipboard})
        elsif backend=XPG_WINAPI then
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
        if backend=XPG_GTK then
            atom clipboard = c_func(gtk_clipboard_get,{GDK_SELECTION_CLIPBOARD})
            ctrl_handles[id] = clipboard
        end if
-- hmm, I wonder whether we can get a VALUE_CHANGED handler to work?
--      xpg_register_handler(CLIPBOARD,"REDRAW",{{1,1,"PO"}})
        xpg_set_ctrl_msg(CLIPBOARD,0,xpg_set_clipboard_attribute,
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
    if backend=XPG_GTK then
        if editable then
            dropdown = c_func(gtk_combo_box_text_new_with_entry,{})
        else
            dropdown = c_func(gtk_combo_box_text_new,{})
        end if
        xpg_setID(dropdown,id)
        xpg_gtk_add_to(parent,dropdown)
--      xpg_gtk_signal_connect(dropdown,"realize",xpg_gtk_widget_realized,id)
        xpg_gtk_signal_connect(dropdown,"changed",xpg_gtk_dropdown_changed,id)
        xpg_gtk_signal_connect(dropdown,"focus-in-event",xpg_gtk_focusinout,id)
        xpg_gtk_signal_connect(dropdown,"focus-out-event",xpg_gtk_focusinout,id)
        c_proc(gtk_widget_realize,{dropdown})
    elsif backend=XPG_WINAPI then
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
            if backend=XPG_GTK then
                changed = gGetHandler(id,"CHANGED")
                -- disable while adding all the entries...
                if changed then gSetHandler(id,"CHANGED",NULL) end if
                while true do
                    c_proc(gtk_combo_box_set_active,{handle,1})
                    if c_func(gtk_combo_box_get_active,{handle})!=1 then exit end if
                    c_proc(gtk_combo_box_text_remove,{handle,0})
                end while
                c_proc(gtk_combo_box_text_remove,{handle,0})
            elsif backend=XPG_WINAPI then
                {} = c_func(xSendMessage,{handle,CB_RESETCONTENT,0,0})
            else
                ?9/0 -- (unknown backend)
            end if
            for i=1 to length(v) do
                string s = v[i]
                if backend=XPG_GTK then
                    c_proc(gtk_combo_box_text_append_text,{handle,s})
                elsif backend=XPG_WINAPI then
                    {} = c_func(xSendMessage,{handle,CB_ADDSTRING,0,s})
                else
                    ?9/0 -- (unknown backend)
                end if
            end for
            if backend=XPG_GTK and changed then -- restore
                gSetHandler(id,"CHANGED",changed)
            end if
        end if
        return true
    elsif name="VALINT" then
        if string(v) then v = to_integer(v) end if
        if not bMapped then
            xpg_defer_attr(id,name,v)
        else
            if backend=XPG_GTK then
                c_proc(gtk_combo_box_set_active,{handle,v-1})
            elsif backend=XPG_WINAPI then
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
        if backend=XPG_GTK then
            atmres = c_func(gtk_combo_box_get_active,{handle})
        elsif backend=XPG_WINAPI then
            atmres = c_func(xSendMessage,{handle,CB_GETCURSEL,0,0})
        else
            ?9/0 -- (unknown backend)
        end if
        integer res = and_bits(atmres,-1)
        return res+1
    elsif name="VALSTR" then
        string res = ""
        if backend=XPG_GTK then
            atom pText = c_func(gtk_combo_box_text_get_active_text,{handle})
            if pText then res = peek_string(pText) end if
            c_proc(g_free,{pText})
        elsif backend=XPG_WINAPI then
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
--  xpg_register_handler(DROPDOWN,"CHANGED",{{1,1,"PO"},{1,0,"PO"}})
    xpg_register_handler(DROPDOWN,"CHANGED",{"PO"})
    xpg_set_ctrl_msg(DROPDOWN,xpg_DropDown,xpg_set_dropdown_attribute,
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
    if backend=XPG_GTK then
--if title="" then tiotle=NULL end if
--      frame = c_func(gtk_frame_new,{title})
        frame = c_func(gtk_frame_new,{iff(title=""?NULL:title)})
        xpg_setID(frame,id)
        xpg_gtk_add_to(parent,frame)
--      c_proc(gtk_container_add,{ctrl_handles[parent],frame})
-- DEV this is of no help here...
--      xpg_gtk_signal_connect(frame,"realize",xpg_gtk_widget_realized,id)
        c_proc(gtk_widget_realize,{frame})
    elsif backend=XPG_WINAPI then
--      if title="" then title="Y" end if
        atom dwStyle = or_all({WS_CHILD,WS_VISIBLE,BS_GROUPBOX}),
           dwStyleEx = 0,
--         dwStyleEx = WS_EX_CONTROLPARENT,
--DEV...
               {w,h} = gGetTextExtent(parent,title)
--             {w,h} = gGetTextExtent(parent,iff(title=""?"X":title))
--      ctrl_size[id][SZ_NATURAL_W] = w  -- no! leave this at 0!
--      ctrl_size[id][SZ_NATURAL_H] = h
        frame = xpg_WinAPI_create(id,"button",title,parent,w,h,dwStyle,dwStyleEx)
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
        elsif backend=XPG_GTK then
            c_proc(gtk_frame_set_label,{handle,v})
        elsif backend=XPG_WINAPI then
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
        if backend=XPG_GTK then
            return peek_string(c_func(gtk_frame_get_label,{handle}))
        elsif backend=XPG_WINAPI then
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
    integer id = xpg_add_control(FRAME,flags:=CF_CONTAINER+CF_DECORATED,bHasChildren:=true)
    if child then
        parent_ids[child] = id
        children_ids[id] = {child}
    end if
--  xpg_register_handler(FRAME,"XXX",{{1,1,"PO"}})
    xpg_set_ctrl_msg(FRAME,xpg_Frame,xpg_set_frame_attribute,
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

local function xpg_default_IDROP(integer rid) return rid end function
internal_rtns &= xpg_default_IDROP; -- (let/permit/allow this to be a perfectly valid rtn)

-- builtins/IupGraph.e
-- todo: bar graphs[stacked or side], grid style, piechart, ... [DEV]

local procedure xpg_redraw_graph(gdx graph)
--?"xpg_redraw_graph"

    rtn drid = gGetHandler(graph,"DRID")
--  sequence sig = get_routine_info(drid,false),
    string sig = get_routine_info(drid,false)[3]
--  sequence datasets = iff(sig={0,0,"F"}?drid(),
    sequence datasets = iff(sig="F"?drid(),
--                      iff(sig={1,1,"FO"}?drid(graph):9/0)),
                        iff(sig="FO"?drid(graph):9/0)),
--DEV let's just use gGetAttributes() and friends, ensure we've implemented them correctly...
             grattrbs = ctrl_xtra[graph][CX_GTL_ATTRS]
--  integer grid = grattrbs[GX_GRID],
    integer grid = gGetInt(graph,"GRID"),
--          gridcolour = grattrbs[GX_GRIDCOLOR],
            gridcolour = gGetInt(graph,"GRIDCOLOR"),
--          {width, hight} = gGetAttribute(graph,"DRAWSIZE"),
            {width, hight} = gGetAttribute(graph,"SIZE"),
            dsdx = 1,
            idrop = gGetHandler(graph,"IDROP",xpg_default_IDROP),
            xCanvasSetForeground = idrop(gCanvasSetForeground),
            xCanvasSetBackground = idrop(gCanvasSetBackground),
            xCanvasRect = idrop(gCanvasRect),
            xCanvasLine = idrop(gCanvasLine),
            xCanvasText = idrop(gCanvasText),
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
--?{"xtick",xtick,"xmin",xmin,"xmax",xmax}

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
            lx, ly -- (XPG_EAST of the first legend text)
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
        assert(s=="NAMES")
--      if s="NAMES" then
            legend = dsdx
--      elsif s="POST" then
--          post = ds[2]
--      else
--          {fontface,fontstyle,fontsize} = ds
--          ?9/0
--      end if
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
--printf(1,"xacross:%d, yacross:%d\n",{xacross,yacross})    
    -- draw the x/y-axis labels and vertical gridlines
    xacross = iff(xacross?round((0-xmin)/xtick)+1:1)
    yacross = iff(yacross?round((0-ymin)/ytick)+1:1)
--printf(1,"xacross:%d, yacross:%d\n",{xacross,yacross})

    integer vb = (barmode="VERTICAL"),
            hb = (barmode="HORIZONTAL"),
            nx = max(round((xmax-xmin)/xtick)+vb,1),
            ny = max(round((ymax-ymin)/ytick)+hb,1)

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
        atom tx = vx+dx*vb/2+1,
             ty = hight-(25+ymargin+xyshift+(yacross-1)*dy)
--printf(1,"ty: %d, ymargin:%d, xyshift:%d, yacross:%d, dy:%d\n",{ty,ymargin,xyshift,yacross,dy})
--      gCanvasText(graph,vx+dx*vb/2,ty,xtext,align,xangle)
--      gCanvasText(graph,vx+dx*vb/2,hight-ty,xtext,align,xangle)
--      gCanvasText(graph,vx+dx*vb/2+1,hight-ty,xtext,align,xangle)
--      gCanvasLine(graph,tx-5,ty-5,tx+5,ty+5,-1,-1,XPG_RED)
--      gCanvasLine(graph,tx-5,ty+5,tx+5,ty-5,-1,-1,XPG_RED)

        gCanvasText(graph,tx,ty,xtext,align,{XPG_W,xangle})
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
        atom tx = 25+xmargin+yxshift+(xacross-1)*dx,
             ty = hight-((vy+dy*hb/2)+2)
--      gCanvasText(graph,tx,vy+dy*hb/2,ytext,align,yangle)
--      gCanvasText(graph,tx,hight-(vy+dy*hb/2),ytext,align,yangle)
        gCanvasText(graph,tx,ty,ytext,align,yangle)
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
--          if backend=XPG_GTK then
--              c_proc(pango_layout_set_text,{layout,text,length(text)})
--              c_proc(pango_layout_get_pixel_size,{layout,pWidth,pHight})
--              lwi = get_struct_field(idGdkRectangle,pRECT,"width")
--              lh = get_struct_field(idGdkRectangle,pRECT,"hight")
--          elsif backend=XPG_WINAPI then
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
--      if backend=XPG_GTK then
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
--          gCanvasRect(graph,lxl,lxr,lyt,lyb,true,colour:=XPG_BLACK) -- NO!
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
                 mm = (mode="MARK"),
                dsz = marksize
        string dms = markstyle,
               dmm = mode
        if ldd>=4 then
            mm = true
            dms = dd[4]
            if ldd>=5 then
//DEV and/or MARKSIZE...
                object dd5 = dd[5]
                if string(dd5) then
                    assert(dd5="MARKLINE")
                    dmm = "MARKLINE"
                    if ldd>=6 then
                        dsz = dd[6]
                    end if
                else
                    dsz = dd5
                end if
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
                            gCanvasCircle(graph,x2,hight-y2,2*(dsz+1))
                        elsif dms="PLUS" then
--                          gCanvasLine(graph,x2,y2-3,x2,y2+3)
                            gCanvasLine(graph,x2,hight-(y2-dsz),x2,hight-(y2+dsz+1))
--                          gCanvasLine(graph,x2-3,y2,x2+3,y2)
                            gCanvasLine(graph,x2-dsz+1,hight-y2,x2+dsz,hight-y2)
                        elsif dms="X" then
--                          gCanvasLine(graph,x2-3,y2-3,x2+3,y2+3)
                            gCanvasLine(graph,x2-dsz,hight-(y2-dsz),x2+dsz,hight-(y2+dsz))
--                          gCanvasLine(graph,x2-3,y2+3,x2+3,y2-3)
                            gCanvasLine(graph,x2-dsz,hight-(y2+dsz),x2+dsz,hight-(y2-dsz))
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
internal_rtns &= xpg_redraw_graph; -- (let/permit/allow this to be a perfectly valid rtn)

global function gGraph(rtn drid, string attributes="", sequence args={})
    gdx graph = gCanvas(xpg_redraw_graph)
--  gdx graph = gCanvas(xpg_redraw_graph,attributes,args)
--  gdx graph = gCanvas(xpg_redraw_graph,attributes,args,GRAPH)
    ctrl_xtra[graph][CX_CANVAS_TYPE] = GRAPH
--  gSetAttribute(graph,"BGCOLOR",XPG_WHITE) -- (set a default)
--erm...
--  gCanvasSetForeground(graph,XPG_WHITE) -- (set a default)
--  xpg_register_handler(CANVAS,"DRID",{{0,0,"F"},{1,1,"FO"}})
    xpg_register_handler(CANVAS,"DRID",{"F","FO"})
--  xpg_register_handler(CANVAS,"XRID",{{1,1,"FI"}})
    xpg_register_handler(CANVAS,"XRID",{"FI"})
--  xpg_register_handler(CANVAS,"YRID",{{1,1,"FI"}})
    xpg_register_handler(CANVAS,"YRID",{"FI"})
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

global function gHbox(sequence children={}, string attributes="", sequence args={})
    return gBox('H',children,attributes,args)
end function

local procedure xpg_Label(gdx id)
    -- (invoked via xpg_map)
    gdx parent = xpg_get_parent_id(id)
    atom label
    nullable_string title = gGetAttribute(id,"TITLE",NULL)
--  if title=NULL then title="" end if
    if backend=XPG_GTK then
        label = c_func(gtk_label_new,{title})
--      if bGTK3 then
----GTK3:
--void
--gtk_widget_set_halign (
--  GtkWidget* widget,
--  GtkAlign align
--)
        c_proc(gtk_misc_set_alignment,{label,0,0})
        xpg_setID(label,id)
        xpg_gtk_add_to(parent,label)
--      c_proc(gtk_container_add,{ctrl_handles[parent],label})
--      xpg_gtk_signal_connect(label,"realize",xpg_gtk_widget_realized,id)
        c_proc(gtk_widget_realize,{label})
    elsif backend=XPG_WINAPI then
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
        elsif backend=XPG_GTK then
            c_proc(gtk_label_set_text_with_mnemonic,{handle,xpg_gtk_mnemonicalize(v)})
        elsif backend=XPG_WINAPI then
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
        if backend=XPG_GTK then
            return peek_string(c_func(gtk_label_get_text,{handle}))
        elsif backend=XPG_WINAPI then
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
    xpg_set_ctrl_msg(LABEL,xpg_Label,xpg_set_label_attribute,
                                     xpg_get_label_attribute)
--DEV...
--  gSetAttribute(id,"FONT","Helvetica, 9")
    if title!=NULL then
if backend=XPG_GTK then title &= " " end if     --DEV in case italic...
        gSetAttribute(id,"TITLE",title)
    end if
    if length(attributes) then
        gSetAttributes(id,attributes,args)
    end if
    return id
end function 

--DEV this was supposed to use XPG_NORESET, presumably for performance/resource reasons, but doesn't yet...
--      it would probably need some kind of look-ahead or probably better a simple single step buffering...
local procedure xpg_redraw_list(gdx list)
--?">xpg_redraw_list"
    object lstattr = ctrl_xtra[list][CX_GTL_ATTRS],
            sbinfo = ctrl_xtra[list][CX_SBINFO]
--  if sequence(sortcol) then sortcol = sortcol[1] end if
    integer data = lstattr[LX_DATA],
--          sbw = 17, --[SB_VWIDE],
            sbw = iff(sequence(sbinfo)?sbinfo[SB_VWIDE]:17),
            {width, height} = gGetAttribute(list, "SIZE"),
            {ox,oy,vw,vh} = gGetAttribute(list,"VIEWPORT"),
            line_height = gGetTextExtent(list,"X")[2],
            longest_line,
            n_rows, 
--DEV kill this if it impacts performance in any way...
            idrop = gGetHandler(list,"IDROP",xpg_default_IDROP),
--          xCanvasSetForeground = idrop(gCanvasSetForeground),
--          xCanvasRect = idrop(gCanvasRect),
--          xCanvasLine = idrop(gCanvasLine),
            xCanvasText = idrop(gCanvasText),
            -- /now/ it feels safe to shadow the globals:
--          gCanvasSetForeground = xCanvasSetForeground,
--          gCanvasRect = xCanvasRect,
--          gCanvasLine = xCanvasLine,
            gCanvasText = xCanvasText
    lstattr = 0 -- (kill refcount)
    sbinfo = 0 -- (kill refcount)
    ctrl_xtra[list][CX_GTL_ATTRS][LX_LINESTEP] = line_height
--?{ox,oy,vw,vh}
    string sig = get_routine_info(data,false)[3]
    if sig="FI" then
        n_rows = data(0)
        longest_line = data(-1)
    elsif sig="FOI" then
        n_rows = data(list,0)
        longest_line = data(list,-1)
    else
        ?9/0 -- unrecognised sig
    end if
    if longest_line=0 then
--DEV             sbinfo[SB_VWIDE] = 17
--      gSetAttribute(list,"SCROLLSIZE",{width-17,line_height*(n_rows+1)})
        gSetAttribute(list,"SCROLLSIZE",{width-sbw,line_height*(n_rows+1)})
    end if
    sequence si
    integer l1 = max(1,floor(oy/line_height)+1),
            nr = min(n_rows-l1+1,ceil(vh/line_height)),
--?? (untried)
--          nr = min(n_rows-l1+1,ceil(height/line_height)),
            vy = oy
    gCanvasSetForeground(list,XPG_BLACK) -- (nb *NOT* saved/restored between redraws)

    for rll=iff(longest_line?0:1) to nr do
        integer r = iff(rll=0?longest_line:rll+l1-1), vx = 2
        if sig="FI" then
            si = data(r)
        elsif sig="FOI" then
            si = data(list,r)
        else
            ?9/0
        end if
        if string(si) then
            if rll=0 then
                vx += gGetTextExtent(list,si)[1]
            else
                gCanvasText(list,vx-ox,vy-oy,si,XPG_SE)
            end if
        else
            for sii in si do
                atom sic = -1, -- colour
                     sis = -1  -- style
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
                    assert(string(sii))
                end if
                if rll!=0 then
                    gCanvasText(list,vx-ox,vy-oy,sii,XPG_SE,0,sic,sis)
                end if
                vx += gGetTextExtent(list,sii)[1]
            end for
        end if
        if rll=0 then
--DEV bit suspect... (not taking SB_VWIDE/SB_HHIGH into account?) [NAH!]
            gSetAttribute(list,"SCROLLSIZE",{vx,line_height*(n_rows+1)})
--          gSetAttribute(list,"SCROLLSIZE",{vx+sbw,line_height*(n_rows+1)})
        else
            vy += line_height
        end if      
    end for
--?"<xpg_redraw_list"
end procedure
internal_rtns &= xpg_redraw_list; -- (let/permit/allow this to be a perfectly valid rtn)

--global function gList(object data, object selected=NULL, sequence attributes="", dword_seq args={})
global function gList(rtn data, object selected=NULL, sequence attributes="", dword_seq args={})
    {selected,attributes,args} = paranormalise_raa(selected,attributes,args)
    gdx list = gCanvas(xpg_redraw_list)

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

--  xpg_register_handler(CANVAS,"SELECTED",{{2,2,"POI"},{2,2,"POP"},{1,1,"PI"},{1,1,"PP"}})
    xpg_register_handler(CANVAS,"SELECTED",{"POI","POP","PI","PP"})
    if selected!=NULL then
        gSetHandler(list,"SELECTED",selected)
    end if
    if length(attributes) then
        gSetAttributes(list,attributes,args)
    end if
--  ctrl_xtra[table] = {columns,data,rows}
    return list
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
            if backend=XPG_GTK then
                atom sep = c_func(gtk_separator_menu_item_new,{})
                c_proc(gtk_widget_show,{sep}) -- (needed for popup menus)
                c_proc(gtk_menu_shell_append,{menu,sep})
                if radio_group!=NULL then
                    bRadio = false
                end if
            elsif backend=XPG_WINAPI then
                bOK = c_func(xAppendMenu,{menu,MF_SEPARATOR,0,NULL})
                assert(bOK)
                if length(rdi) then
                    bRadio = false
                end if
            else
                ?9/0 -- (unknown backend)
            end if
--      elsif length(c)=2 and integer(c[2]) then
        elsif integer(c[$]) then
            -- eg {"Cut",[img,]CUT=$}
            bool bImg = length(c)==3
            object img = iff(bImg?c[2]:NULL)
--          {text,mdx} = c
            text = c[1]
            mdx = c[$]
            bool bCheck = text[1]='?'
            if bCheck then text = text[2..$] end if
            if backend=XPG_GTK then
                text = xpg_gtk_mnemonicalize(text)
                atom mitem
                if bCheck then
                    assert(not bImg,"images on radio or check buttons not allowed")
-- erm, gtk_menu_set_reserve_toggle_size(menu, false) [or not! - that may be gtk_menu_item() only]
                    if bRadio then
                        mitem = c_func(gtk_radio_menu_item_new_with_mnemonic,{radio_group,text})
                        assert(mitem!=NULL)
                        radio_group = c_func(gtk_radio_menu_item_get_group,{mitem})
                    else
                        mitem = c_func(gtk_check_menu_item_new_with_mnemonic,{text})
                    end if
                else
                    if bImg then
                        mitem = c_func(gtk_image_menu_item_new_with_mnemonic,{text})
                        c_proc(gtk_image_menu_item_set_always_show_image,{mitem,true})
--                      c_proc(gtk_image_menu_item_set_use_stock,{mitem,false}) -- (not actually needed)
--20/11/23...
--          if string(xpm) then xpm = split(xpm,'\n') end if
--/*
                        assert(img[1]="gImage") -- img is {"gImage",GtkPixbuf}
                        atom gtk_image = c_func(gtk_image_new_from_pixbuf,{img[2]})
                        c_proc(gtk_image_menu_item_set_image,{mitem,gtk_image})
--*/
                        assert(mdx!=0)
--                      gMenuSetAttribute(pmid,mdx,"IMAGE",img)
                    else
                        mitem = c_func(gtk_menu_item_new_with_mnemonic,{text})
                    end if
                end if
--?{mdx,text,mitem}
                c_proc(gtk_widget_show,{mitem}) -- (needed for popup menus)
                setd(mitem,pmid,GTK_MENU_LOOKUP)
                if mdx then 
                    setd({pmid,mdx},mitem,GTK_MENU_UPLOOK)
                    if bImg then
                        gMenuSetAttribute(pmid,mdx,"IMAGE",img)
                    end if
                end if
                xpg_gtk_signal_connect(mitem,"activate",xpg_gtk_menu_clicked,mdx)
                xpg_gtk_signal_connect(mitem,"select",xpg_gtk_menu_selected,mdx)
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
--*/
            elsif backend=XPG_WINAPI then

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
                if bImg then
--/*
                    assert(img[1]="gImage") -- img is {"gImage",{hDIB,w,h,t}}
--/*
    v = {1.844674407e+19,24,24,16777215}
                object mi = getd({menu,id},WINAPI_SUBMENUS)
                bool byPos = sequence(mi)
                if byPos then {mh,id} = mi end if
--*/
                    integer misize = get_struct_size(idMENUITEMINFO)
                    mem_set(pMENUITEMINFO,0,misize)
                    set_struct_field(idMENUITEMINFO,pMENUITEMINFO,"cbSize",misize)
                    set_struct_field(idMENUITEMINFO,pMENUITEMINFO,"fMask",MIIM_BITMAP)
                    set_struct_field(idMENUITEMINFO,pMENUITEMINFO,"hbmpItem",img[2][1])
                    bOK = c_func(xSetMenuItemInfo,{menu,mdx,false,pMENUITEMINFO})
--  SetMenuItemBitmaps((HMENU)ih->handle, (UINT)ih->serial, MF_BYCOMMAND, hBitmapUnchecked, hBitmapChecked);
                    assert(bOK)
--*/
                    gMenuSetAttribute(pmid,mdx,"IMAGE",img)

--/*
CreateCompatibleDC 
                bOK = c_func(xGetMenuItemInfo,{mh,id,byPos,pMENUITEMINFO})
                assert(bOK)
                set_struct_field(idMENUITEMINFO,pMENUITEMINFO,"dwTypeData",xpg_raw_string_ptr(v))
                bOK = c_func(xSetMenuItemInfo,{mh,id,byPos,pMENUITEMINFO})
                assert(bOK)

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

static int winItemSetTitleImageAttrib(Ihandle* ih, const char* value)
{
  HBITMAP hBitmap;

  /* check if the submenu handle was created in winSubmenuAddToParent */
  if (ih->handle == (InativeHandle*)-1)
    return 1;

  hBitmap = iupImageGetImage(value, ih, 0);

  {
    MENUITEMINFO menuiteminfo;
    menuiteminfo.cbSize = sizeof(MENUITEMINFO); 
MIIM_BITMAP 
0x00000080 
    menuiteminfo.fMask = MIIM_BITMAP; 
    menuiteminfo.hbmpItem = hBitmap;
    SetMenuItemInfo((HMENU)ih->handle, (UINT)ih->serial, FALSE, &menuiteminfo);
  }

  winMenuUpdateBar(ih);

  return 1;
}
--*/
                end if
--MIIM_BITMAP 
            else
                ?9/0 -- (unknown backend)
            end if
        else
            text = c[1]
            mdx = iff(length(c)=3?c[2]:0)
            sequence grandkids = c[$]
            bool bSubRadio = text[1]='?'
            if bSubRadio then text = text[2..$] end if
            if backend=XPG_GTK then
                text = xpg_gtk_mnemonicalize(text)
                atom mitem = c_func(gtk_menu_item_new_with_mnemonic,{text}),
                   submenu = c_func(gtk_menu_new,{})
                xpg_add_menu(pmid,submenu,grandkids,bSubRadio)
                c_proc(gtk_menu_item_set_submenu,{mitem,submenu})
                if mdx then
                    setd(mitem,pmid,GTK_MENU_LOOKUP)
--?{{pmid,mdx},text}
                    setd({pmid,mdx},mitem,GTK_MENU_UPLOOK)
                    xpg_gtk_signal_connect(mitem,"activate",xpg_gtk_menu_clicked,mdx)
                    xpg_gtk_signal_connect(mitem,"select",xpg_gtk_menu_selected,mdx)
                end if
                c_proc(gtk_widget_show,{mitem}) -- (needed for popup menus)
                c_proc(gtk_menu_shell_append,{menu,mitem})
                if radio_group!=NULL then
                    bRadio = false
                end if
            elsif backend=XPG_WINAPI then
                atom submenu = c_func(xCreatePopupMenu,{})
--?{"submenu",submenu,text,mdx,pmid}
                if mdx then
                    setd(submenu,mdx,WINAPI_SUBMENUS)
                    setd({pmid,mdx},{menu,pos-1},WINAPI_SUBMENUS)
                end if
--17/11/23 moved so that gMenuSetAttribute("IMAGE") works [on the children]
--              xpg_add_menu(pmid,submenu,grandkids,bSubRadio)
                bOK = c_func(xAppendMenu,{menu,or_bits(MF_POPUP,MF_STRING),submenu,text})
                assert(bOK)
                xpg_add_menu(pmid,submenu,grandkids,bSubRadio)
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
    ctrl_xtra[id] = 0 -- (no undo, yet)
    if backend=XPG_GTK then
--      assert(vbox?)
        if parent then
            menu = c_func(gtk_menu_bar_new,{})
            atom vbox = ctrl_handles[parent]
            c_proc(gtk_fixed_put,{vbox,menu,0,0})
        else
            menu = c_func(gtk_menu_new,{})
        end if
--      xpg_gtk_signal_connect(checkbox,"realize",xpg_gtk_widget_realized,id)
--      xpg_gtk_signal_connect(menu,"clicked",xpg_gtk_menu_clicked,id)
--      xpg_gtk_signal_connect(checkbox,"focus-in-event",xpg_gtk_focusinout,id)
--      xpg_gtk_signal_connect(checkbox,"focus-out-event",xpg_gtk_focusinout,id)
--      c_proc(gtk_widget_realize,{menu})
    elsif backend=XPG_WINAPI then
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
    if backend=XPG_WINAPI then xpg_WinAPI_draw_menu_bar(id) end if
end procedure

global function gMenu(sequence children, rtn handler, bool bRadio=false)
    if not bInit then xpg_Init() end if
    integer id = xpg_add_control(MENU)
    ctrl_xtra[id] = {children,bRadio} -- as used by xpg_Menu, then undo highlight id for WinAPI
--  xpg_register_handler(MENU,"HANDLER",{{3,3,"FOII"},{2,2,"FOI"},{2,2,"FII"},{1,1,"FI"}})
--  xpg_register_handler(MENU,"HANDLER",{{3,3,"FOII"},{2,2,"FOI"},{1,1,"FI"}})
--  xpg_register_handler(MENU,"HANDLER",{{3,3,"FOII"},{2,2,"FOI"}})
    xpg_register_handler(MENU,"HANDLER",{"FOII","FOI"})
    xpg_set_ctrl_msg(MENU,xpg_Menu,0,0) -- (ie gMenuGet[/Set]Attribute shd be used instead)
--  gSetAttribute(id,"FONT","Helvetica, 9") -- WinAPI really don't like that!
    assert(handler!=NULL)
    gSetHandler(id,"HANDLER",handler)
    def_menu_attr[id] = {{},{}} -- {{ids},{name,v} pairs}
if backend=XPG_GTK then -- we certainly need it on this...
--  if bGTK3 then
--      ctrl_size[id][SZ_NATURAL_H] = 13
--  else
--      ctrl_size[id][SZ_NATURAL_H] = 25
        ctrl_size[id][SZ_NATURAL_H] = 26
--      ctrl_size[id][SZ_NATURAL_H] = 27
--  end if
end if
    return id
end function 

--From: https://zetcode.com/gui/gtk2/menusandtoolbars/

local procedure xpg_ProgressBar(gdx id)
    -- (invoked via xpg_map)
    gdx parent = xpg_get_parent_id(id)
    atom progress_bar
    bool orientation = gGetAttribute(id,"ORIENTATION",0)
    if backend=XPG_GTK then
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
    elsif backend=XPG_WINAPI then
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
            if backend=XPG_GTK then
                c_proc(gtk_progress_bar_set_fraction,{handle,f})
            elsif backend=XPG_WINAPI then
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
        elsif backend=XPG_GTK then
            if not bGTK3 then
                -- v shd be GTK_PROGRESS_CONTINUOUS|GTK_PROGRESS_DISCRETE
                c_proc(gtk_progress_bar_set_bar_style,{handle,v})
            else
                ?9/0
            end if
        elsif backend=XPG_WINAPI then
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
    integer id = xpg_add_control(PROGRESS)
--  xpg_register_handler(PROGRESS,"VALUE_CHANGED",{{1,1,"PO"}})
    xpg_set_ctrl_msg(PROGRESS,xpg_ProgressBar,xpg_set_progress_attribute,
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
-- (also used for spin changes)
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
    if backend=XPG_GTK then
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
--      xpg_gtk_signal_connect(slider,"realize",xpg_gtk_widget_realized,id)
        xpg_gtk_signal_connect(slider,"value-changed",xpg_gtk_slider_changed,id)
        xpg_gtk_signal_connect(slider,"focus-in-event",xpg_gtk_focusinout,id)
        xpg_gtk_signal_connect(slider,"focus-out-event",xpg_gtk_focusinout,id)
        c_proc(gtk_widget_realize,{slider})
    elsif backend=XPG_WINAPI then
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
        elsif backend=XPG_GTK then
            c_proc(gtk_range_set_value,{handle,v})
        elsif backend=XPG_WINAPI then
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
        if backend=XPG_GTK then
            return c_func(gtk_range_get_value,{handle})
        elsif backend=XPG_WINAPI then
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
--  xpg_register_handler(SLIDER,"VALUE_CHANGED",{{1,1,"PO"}})
    xpg_register_handler(SLIDER,"VALUE_CHANGED",{"PO"})
    xpg_set_ctrl_msg(SLIDER,xpg_Slider,xpg_set_slider_attribute,
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

local procedure xpg_Spin(gdx id)
    -- (invoked via xpg_map)
    gdx parent = xpg_get_parent_id(id)
    atom spin
    bool bWrap = gGetAttribute(id,"WRAP",false)
    if backend=XPG_GTK then
        spin = c_func(gtk_spin_button_new_with_range,{0,100,1})
        if bWrap then
            c_proc(gtk_spin_button_set_wrap,{spin,true})
        end if
        xpg_setID(spin,id)
        xpg_gtk_add_to(parent,spin)
        xpg_gtk_signal_connect(spin,"value-changed",xpg_gtk_slider_changed,id)
        xpg_gtk_signal_connect(spin,"focus-in-event",xpg_gtk_focusinout,id)
        xpg_gtk_signal_connect(spin,"focus-out-event",xpg_gtk_focusinout,id)
        c_proc(gtk_widget_realize,{spin})
    elsif backend=XPG_WINAPI then
        atom dwStyle = or_all({WS_CHILD,WS_VISIBLE,ES_RIGHT,WS_TABSTOP}),
--           edit = xpg_WinAPI_create(id+1,"Edit",NULL,parent,70,25,dwStyle,WS_EX_CLIENTEDGE)
             edit = xpg_WinAPI_create(id+1,"Edit",NULL,parent,70,22,dwStyle,WS_EX_CLIENTEDGE)
        dwStyle = or_all({WS_CHILD,WS_VISIBLE,UDS_SETBUDDYINT,UDS_ALIGNRIGHT,UDS_ARROWKEYS})
        if bWrap then dwStyle += UDS_WRAP end if
--      spin = xpg_WinAPI_create(id,"msctls_updown32",NULL,parent,20,20,dwStyle,0)
        spin = xpg_WinAPI_create(id,"msctls_updown32",NULL,parent,16,16,dwStyle,0)
--DEV/temp:
            integer flags = SWP_NOSIZE+SWP_NOACTIVATE
            bool ok = c_func(xSetWindowPos,{edit,NULL,13,13,0,0,flags})
            assert(ok)
--      ctrl_xtra[id] = spin
        {} = c_func(xSendMessage,{spin,UDM_SETBUDDY,edit,0})
        {} = c_func(xSendMessage,{spin,UDM_SETRANGE32,0,100})
    else
        ?9/0 -- (unknown backend)
    end if
end procedure

function xpg_set_spin_attribute(gdx id, string name, object v, bool bMapped)
    atom handle = ctrl_handles[id]
    if name="RANGE" then
        if string(v) then v = xpg_intint(v) end if
        assert(length(v)=2)
        integer {minv,maxv} = v -- (delib typecheck)
        if not bMapped then
            xpg_defer_attr(id,name,v)
        elsif backend=XPG_GTK then
            c_proc(gtk_spin_button_set_range,{handle,minv,maxv})
        elsif backend=XPG_WINAPI then
            {} = c_func(xSendMessage,{handle,UDM_SETRANGE32,minv,maxv})
        else
            ?9/0 -- (unknown backend)
        end if
        return true
    elsif name="STEP" then
        if string(v) then v = to_number(v) end if
        assert(integer(v) and v>0,"positive integer step only")
        if not bMapped then
            xpg_defer_attr(id,name,v)
        elsif backend=XPG_GTK then
            c_proc(gtk_spin_button_set_increments,{handle,v,v})
        elsif backend=XPG_WINAPI then
            set_struct_field(idUDACCEL,pUDACCEL,"nSec",0)
            set_struct_field(idUDACCEL,pUDACCEL,"nInc",v)
            integer bOK = c_func(xSendMessage,{handle,UDM_SETACCEL,1,pUDACCEL})
            assert(bOK)
        else
            ?9/0 -- (unknown backend)
        end if
        return true
    elsif name="VALUE" then
        if string(v) then v = to_number(v) end if
        assert(integer(v))
        if not bMapped then
            xpg_defer_attr(id,name,v)
        elsif backend=XPG_GTK then
            c_proc(gtk_spin_button_set_value,{handle,v})
        elsif backend=XPG_WINAPI then
            {} = c_func(xSendMessage,{handle,UDM_SETPOS32,0,v})
            --DEV do we want to set the edit as well here??
        else
            ?9/0 -- (unknown backend)
        end if
        return true
    elsif name="WRAP" then
        if string(v) then v = xpg_to_bool(v) end if
        if not bMapped then
            xpg_defer_attr(id,name,v)
        else -- [as already dealt with in xpg_Spin()..]
            assert(v==gGetAttribute(id,"WRAP",false))
        end if
        return true
    end if
    return false
end function

function xpg_get_spin_attribute(gdx id, string name, object dflt)
    atom handle = ctrl_handles[id]
    if name="RANGE" then
        if backend=XPG_GTK then
            atom pMin = allocate(8),
                 pMax = allocate(8)
            c_proc(gtk_spin_button_get_range,{handle,pMin,pMax})
            sequence res = {xpg_peek_double(pMin),
                            xpg_peek_double(pMax)}
            free({pMin,pMax})
            return res
        elsif backend=XPG_WINAPI then
            atom piMin = allocate_word(),
                 piMax = allocate_word()
            {} = c_func(xSendMessage,{handle,UDM_GETRANGE32,piMin,piMax})
            sequence res = {peekns(piMin),
                            peekns(piMax)}
            free({piMin,piMax})
            return res
        else
            ?9/0 -- (unknown backend)
        end if
    elsif name="STEP" then
        if backend=XPG_GTK then
            atom pStep = allocate(8)
            c_proc(gtk_spin_button_get_increments,{handle,pStep,NULL})
            atom res = xpg_peek_double(pStep)
            free(pStep)
            return res
        elsif backend=XPG_WINAPI then
            integer n = c_func(xSendMessage,{handle,UDM_GETACCEL,1,pUDACCEL})
            return get_struct_field(idUDACCEL,pUDACCEL,"nInc")
        else
            ?9/0 -- (unknown backend)
        end if
    elsif name="VALUE" then
        if backend=XPG_GTK then
            return c_func(gtk_spin_button_get_value,{handle})
        elsif backend=XPG_WINAPI then
            return c_func(xSendMessage,{handle,UDM_GETPOS32,0,NULL})
        else
            ?9/0 -- (unknown backend)
        end if
    elsif name="WRAP" then
        if backend=XPG_GTK then
            return c_func(gtk_spin_button_get_wrap,{handle})
        elsif backend=XPG_WINAPI then
            atom dwStyle = c_func(xGetWindowLong,{handle,GWL_STYLE})
            return and_bits(dwStyle,UDS_WRAP)!=0
        else
            ?9/0 -- (unknown backend)
        end if
    end if
    return xpg_return_default_attr(id,name,dflt)
end function

global function gSpin(object value_changed=NULL, sequence attributes="", args={})
    {value_changed,attributes,args} = paranormalise_raa(value_changed,attributes,args)
    if not bInit then xpg_Init() end if
    integer id = xpg_add_control(SPIN,flags:=CF_UNMAPATTR),
            ide = xpg_add_control(TEXT)
    xpg_register_handler(SPIN,"VALUE_CHANGED",{"PO"})
    xpg_set_ctrl_msg(SPIN,xpg_Spin,xpg_set_spin_attribute,
                                   xpg_get_spin_attribute)
    if value_changed!=NULL then
        gSetHandler(id,"VALUE_CHANGED",value_changed)
    end if
    if length(attributes) then
        gSetAttributes(id,attributes,args)
    end if
    return id
end function 

local procedure xpg_redraw_table(gdx table)
--?9/0
--?"xpg_redraw_table"
-- [DEV] columns of {80,20,100} get 80/200,(4/10) 20/120 (1/6), and 100/100 (100%) of any slack respectively...
-- (later) First job: determine whether there is a vertical scrollbar... [unless that's already taken care of...]
    object tblattr = ctrl_xtra[table][CX_GTL_ATTRS]
--  if sequence(sortcol) then sortcol = sortcol[1] end if
--  integer data = tblattr[TX_DATA] or better yet:
--  integer data = gGetHandler(table,"DATA")
--DEV data(table,0)...
    sequence columns = deep_copy(tblattr[TX_COLUMNS]),
             actcols = repeat(80,length(columns)),
             colalgn = repeat('L',length(columns)),
             ttlalgn = repeat('L',length(columns)),
--DEV object, and call if integer:
             data = tblattr[TX_DATA],
             tags = tblattr[TX_TAGSET]
    tblattr = 0 -- (kill refcount)
--DEV config?, use throughout
    integer line_height = 24
    ctrl_xtra[table][CX_GTL_ATTRS][TX_LINESTEP] = line_height
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

--DEV
--?{"gSetAttribute",table,"SCROLLSIZE",{width-17,24*(length(data[1])+1)},{width,height}}
    gSetAttribute(table,"SCROLLSIZE",{width-17,line_height*(length(data[1])+1)})
--  gSetAttribute(table,"SCROLLSIZE",{colsum+max(slack,0)-17,line_height*(length(data[1])+1)})
--DEV table size is not being set properly... (works a smidge better unde GTK)
--?{colsum,slack,sum(actcols),actcols}
--?gGetAttribute(table,"SCROLLSIZE")
--?gGetAttribute(table,"SIZE")
--  gSetAttribute(table,"VIEWPORT",{width,height}) -- (erm, should not change anything...)

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
    
    -- ("IDROP" is shorthand for "Intercept Drawing Operation")
    integer idrop = gGetHandler(table,"IDROP",xpg_default_IDROP),
            xCanvasSetForeground = idrop(gCanvasSetForeground),
            xCanvasRect = idrop(gCanvasRect),
            xCanvasLine = idrop(gCanvasLine),
            xCanvasText = idrop(gCanvasText),
            -- /now/ it feels safe to shadow the globals:
            gCanvasSetForeground = xCanvasSetForeground,
            gCanvasRect = xCanvasRect,
            gCanvasLine = xCanvasLine,
            gCanvasText = xCanvasText
            
--string actcolstr = sprintf("%v",{actcols})
--string actcolstr = join(actcols,fmt:="%d")
--?{"width",width,"height",height,"columns",columns,"actcols",actcolstr,sum(actcols)}
--  gCanvasRect(table,0,width,0,line_height,true,colour:=XPG_GREY,fillcolour:=XPG_GREY)
--  gCanvasRect(table,0,width,0,line_height,true,0,-1,-1,XPG_GREY,XPG_GREY)
    gCanvasRect(table,0,width,0,line_height,true,0,-1,-1,XPG_GREY)
    gCanvasSetForeground(table,XPG_BLACK)
    integer vx = 0, sortcol = 0, csgn = 0,
            nr = min(length(data[1]),ceil(height/line_height)-1)
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
--DEV keep a record of where the column titles are, for the CLICK handler (sort)
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
            integer vy = 13+line_height*r,
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
    for vy=0 to height by line_height do -- the horizontal lines
        gCanvasLine(table,0,vy,width,vy)
    end for 
--  gCanvasSetForeground(table,XPG_BLACK)
end procedure
internal_rtns &= xpg_redraw_table; -- (let/permit/allow this to be a perfectly valid rtn)

--DEV object data:
--global function gTable(sequence columns, data, integer rows=10, sequence attributes="", dword_seq args={})
global function gTable(sequence columns, data, object rows=10, sequence attributes="", dword_seq args={})
    {rows,attributes,args} = paranormalise_raa(rows,attributes,args,bCheckRid:=false)
    if rows=0 then rows=10 end if
--  if not bInit then xpg_Init() end if
--  integer id = xpg_add_control(TABLE)
    gdx table = gCanvas(xpg_redraw_table)
--  gdx table = gCanvas(xpg_redraw_table,attributes,args)
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
--  t_attr[TX_NATWIDTH] = natural_width
--  t_attr[TX_NATHIGHT] = natural_hight
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

local atom tab_himl = NULL
local sequence tab_imgs

local function xpg_WinAPI_add_tab_icon(sequence img, atom notebook)
    assert(img[1]="gImage") -- img is {"gImage",{hDIB,w,h,t}}
    atom {hDib,w,h,t} = img[2]
    assert(w==SM_XICON)
    assert(h==SM_YICON)
    if tab_himl==NULL then
        tab_himl = c_func(xImageList_Create,{SM_XICON,SM_YICON,ILC_COLOR8+ILC_MASK,1,32})
        tab_imgs = {}
    end if
    integer iImageIdx = find(img,tab_imgs)
    if iImageIdx=0 then
        c_proc(xImageList_Add,{tab_himl,hDib,NULL})
        tab_imgs = append(tab_imgs,img)
        iImageIdx = length(tab_imgs)
    end if
    {} = c_func(xSendMessage,{notebook,TCM_SETIMAGELIST,0,tab_himl})
    return iImageIdx-1
end function

local procedure xpg_add_tabs(gdx id)
?"xpg_add_tabs"
    sequence children = children_ids[id],
            tabtitles = ctrl_xtra[id][CX_TABTITLES],
            tabimages = ctrl_xtra[id][CX_TABIMAGES]
    ctrl_xtra[id][CX_TABTITLES] = 0 -- kill refcount/replaced below
    integer lc = length(children),
            lt = length(tabtitles),
            li = length(tabimages)
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
    if backend=XPG_GTK then
--DEV TABTITLE putting everything in twice...
        integer n = c_func(gtk_notebook_get_n_pages,{notebook})
?{"TABTITLES:",tabtitles,n,length(tabtitles)}
--?gGetAttribute(id,"FONT") -- Sans 9
--if false then
        integer summ32 = 0, tabh
        for i,desc in tabtitles do
--if i>n then
            atom label = c_func(gtk_label_new_with_mnemonic,{desc}),
--DEV deeply suspect use of [i] here... (spotted in passing) maybe I meant children[i]...
                 child = iff(i<=lc?ctrl_handles[i]:c_func(gtk_fixed_new,{}))
            if i<=li and tabimages[i]!=NULL then
                sequence img = tabimages[i]
--?{i,img}
                assert(img[1]="gImage") -- shd be {"gImage",GtkPixbuf}
--              atom pixbuf = img[2], -- (a GtkPixBuf)
                atom pixbuf = img[2][1], -- (a GtkPixBuf)
                     image = c_func(gtk_image_new_from_pixbuf,{pixbuf}),
                     hbox = c_func(gtk_hbox_new,{false,0})
                c_proc(gtk_box_pack_start,{hbox,image,false,false,0})
                c_proc(gtk_box_pack_start,{hbox,label,false,false,0})
                label = hbox
                c_proc(gtk_widget_show_all,{label})
            end if
            integer r = c_func(gtk_notebook_insert_page,{notebook,child,label,-1})
--          assert(r>=0)
            assert(r==i-1)
            integer {w,h} = gGetTextExtent(id,desc)
?{desc,w,h}
            if bGTK3 then w = max(w,32) end if 
            summ32 += w
--          if r!=i-1 then ?{"warning: gtk_notebook_insert_page returned",r,"not",i-1} end if
--DEV
--/*
            assert(img[1]="gImage") -- shd be {"gImage",GtkPixbuf}
            atom pixbuf = img[2], -- (a GtkPixBuf)
                 image = c_func(gtk_image_new_from_pixbuf,{pixbuf})
            c_proc(gtk_image_menu_item_set_image,{mitem,image})
            c_proc(gtk_button_set_image,{button,image})
gtk_window_set_icon

https://stackoverflow.com/questions/70593712/how-do-i-create-a-close-button-for-notebook-tab-with-gtk-c
Here is a common way to create a big close button for GTK 2.24, GTK 3 and 4, problem is buttons are too big:

GtkWidget *head, *content, *image, *btn, *label = gtk_label_new ("Title");

// create empty boxes
#if GTK_CHECK_VERSION (3,0,0)
    head = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
--  content = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
#else
    head = gtk_hbox_new (FALSE, 0);
--  content = gtk_vbox_new (FALSE, 0);
#endif

// add text to box
#if GTK_CHECK_VERSION (4,0,0)
    gtk_box_append (GTK_BOX (head), label);
#else
    gtk_box_pack_start (GTK_BOX (head), label, FALSE, FALSE, 0);
#endif

// create close button
#if GTK_CHECK_VERSION (4,0,0)
    btn = gtk_button_new_from_icon_name ("gtk-close");
    gtk_button_set_has_frame (GTK_BUTTON (btn), FALSE);
#else
    btn = gtk_button_new ();
    gtk_button_set_image (GTK_BUTTON (btn), gtk_image_new_from_icon_name ("gtk-close", GTK_ICON_SIZE_MENU));
    gtk_button_set_relief (GTK_BUTTON (btn), GTK_RELIEF_NONE);
#endif
#if GTK_CHECK_VERSION (3,20,0)
    gtk_widget_set_focus_on_click (btn, FALSE);
#else
    gtk_button_set_focus_on_click (GTK_BUTTON (btn), FALSE);
#endif

// add button to box
#if GTK_CHECK_VERSION (4,0,0)
    gtk_box_append (GTK_BOX (head), btn);
#else
    gtk_box_pack_start (GTK_BOX (head), btn, FALSE, FALSE, 0);
#endif

#if !GTK_CHECK_VERSION (4,0,0)
    gtk_widget_show_all (head);
#endif

// add boxes to notebook
gtk_notebook_append_page (GTK_NOTEBOOK (notebook), content, head);
gtk_notebook_set_tab_reorderable (GTK_NOTEBOOK (notebook), content, TRUE);

answer:
#if GTK_CHECK_VERSION (4,0,0)
    GtkCssProvider* provider = gtk_css_provider_new ();
    gtk_css_provider_load_from_data (provider, "* { padding:0; }", -1);
    gtk_style_context_add_provider (
        gtk_widget_get_style_context (GTK_WIDGET (btn)),
        GTK_STYLE_PROVIDER (provider),
        GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
#elif GTK_CHECK_VERSION (3,0,0)
    GtkCssProvider* provider = gtk_css_provider_new ();
    gtk_css_provider_load_from_data (provider, "* { padding:0; }", -1, NULL);
    gtk_style_context_add_provider (
        gtk_widget_get_style_context (GTK_WIDGET (btn)),
        GTK_STYLE_PROVIDER (provider),
        GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
#else
    gtk_widget_set_name (btn, "close-button");
    gtk_rc_parse_string ("style \"close-button\"\n"
        "{\n"
            "GtkWidget::focus-padding = 0\n"
            "GtkWidget::focus-line-width = 0\n"
            "xthickness = 0\n"
            "ythickness = 0\n"
        "}\n"
        "widget \"*.close-button\" style \"close-button\"");
#endif
--*/
        end for
--end if
        c_proc(gtk_widget_realize,{notebook})
        if bGTK3 then
            summ32 += (lt-1)*30+40
            tabh = 39
        else
            summ32 += (lt-1)*10+13
            tabh = 30
        end if 
--DEV naturals be getting clobbered...
?{"nw,nh",summ32,tabh}
--      ctrl_size[id][SZ_NATURAL_W] = summ32
--      ctrl_size[id][SZ_NATURAL_H] = tabh
        ctrl_xtra[id][CX_TABWIDTH] = summ32
        ctrl_xtra[id][CX_TABHIGHT] = tabh

    elsif backend=XPG_WINAPI then
--and maybe TCIF_IMAGE...
--      atom img_list = 
        integer summ30 = 0
        for i,desc in tabtitles do
            integer mask = TCIF_TEXT
            if i<=li and tabimages[i]!=NULL then
                mask += TCIF_IMAGE
                integer iImageIdx = xpg_WinAPI_add_tab_icon(tabimages[i],notebook)
--              sequence img = tabimages[i]
--?{"inAPI",i,img}
--              assert(img[1]="gImage") -- img is {"gImage",{hDIB,w,h,t}}
--no: iImage Type: int Index in the tab control's image list, or -1 if there is no image for the tab. 
--TCM_SETIMAGELIST 
--              set_struct_field(idTCITEM,pTCITEM,"iImage",img[2][1])
                set_struct_field(idTCITEM,pTCITEM,"iImage",iImageIdx)
            end if
            set_struct_field(idTCITEM,pTCITEM,"mask",mask)
            set_struct_field(idTCITEM,pTCITEM,"pszText",xpg_raw_string_ptr(desc))
            set_struct_field(idTCITEM,pTCITEM,"cchTextMax",length(desc))
            {} = c_func(xSendMessage,{notebook,TCM_INSERTITEMA,i-1,pTCITEM})
            integer {w,h} = gGetTextExtent(id,desc)
            summ30 += max(w,30)
--?{desc,w,h,summ30}
        end for
        summ30 += (lt-1)*12+19
--?{"summ30",summ30}
        -- note these take no account (as yet) of any children...
        -- (it might be worth keeping these somewhere safe??)
--      ctrl_size[id][SZ_NATURAL_W] = summ30
--      ctrl_size[id][SZ_NATURAL_H] = 24
        -- like here...
        ctrl_xtra[id][CX_TABWIDTH] = summ30
        ctrl_xtra[id][CX_TABHIGHT] = 24


--?{"summ32",summ32,summ32+12*length(tabtitles)+1}
--TCM_GETITEMCOUNT
--TCM_DELETEITEM 
--WM_NOTIFY TCN_SELCHANGING TCN_SELCHANGE 
--TCM_GETCURSEL TCM_SETCURSEL
--TCM_SETIMAGELIST 
--https://learn.microsoft.com/en-us/windows/win32/controls/tab-controls
-- ImageList_Create
--/*
        elsif what="ADDICON" then
            atom tree = xpm[2], icon = xpm[3]
            c_proc(xImageList_Add,{tree,icon,NULL})
            c_proc(xDeleteObject,{icon})
        if what="NEWLIST" then
            -- create imagelist using the recommended sizes for small icons:
--          return c_func(xImageList_Create,{c_func(xGetSystemMetrics,{SM_CXSMICON}),
--                                           c_func(xGetSystemMetrics,{SM_CYSMICON}),
--                                           ILC_COLOR8+ILC_MASK,1,32})
            return c_func(xImageList_Create,{SM_XICON,SM_YICON,ILC_COLOR8+ILC_MASK,1,32})

atom hILsmall = 0

--function addIcon(atom hIcon)
    if hILsmall = 0 then
        integer width = c_func(xGetSystemMetrics, {SM_CXSMICON}),
                height = c_func(xGetSystemMetrics, {SM_CYSMICON})
        hILsmall = c_func(xImageList_Create, {width, height, ILC_MASK+ILC_COLOR8,1,1})
        {} = c_func(xImageList_SetBkColor, {hILsmall, CLR_NONE})
    end if
    integer iIcon = c_func(xImageList_AddIcon, {hILsmall,hIcon})
    return iIcon+1
--end function

--*/
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
    else
        ?9/0
    end if
end procedure

local procedure xpg_Tabs(gdx id)
    -- (invoked via xpg_map)
    gdx parent = xpg_get_parent_id(id)
    atom handle
    if backend=XPG_GTK then
        handle = c_func(gtk_notebook_new,{})
        c_proc(gtk_notebook_set_scrollable,{handle,true})
        xpg_setID(handle,id)
        xpg_gtk_add_to(parent,handle)
--      c_proc(gtk_container_add,{ctrl_handles[parent],handle})
--      xpg_gtk_signal_connect(handle,"realize",xpg_gtk_widget_realized,id)
        xpg_gtk_signal_connect(handle,"focus-in-event",xpg_gtk_focusinout,id)
        xpg_gtk_signal_connect(handle,"focus-out-event",xpg_gtk_focusinout,id)
        xpg_gtk_signal_connect(handle,"switch-page",xpg_gtk_switch_page,id)
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
    elsif backend=XPG_WINAPI then
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
    integer id = xpg_add_control(TABS,flags:=CF_CONTAINER+CF_DECORATED,bHasChildren:=true)
--  xpg_register_handler(TABS,"TABCHANGED",{{3,3,"POII"}})
    xpg_register_handler(TABS,"TABCHANGED",{"POII"})
    xpg_set_ctrl_msg(TABS,xpg_Tabs,xpg_set_tabs_attribute,
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

local function xpg_gtk_text_changed(atom widget, gdx id)
    assert(id=xpg_getID(widget))
    integer value_changed = gGetHandler(id,"VALUE_CHANGED")
    if value_changed then
        value_changed(id)
    end if
    return 0 -- (ignored)
end function

local procedure xpg_Text(gdx id)
    -- (invoked via xpg_map)
    gdx parent = xpg_get_parent_id(id)
    atom handle
    if backend=XPG_GTK then
--      handle = c_func(gtk_text_view_new,{})
        handle = c_func(gtk_entry_new,{})
--(no help)
--          if bGTK3 then
--              bool bOK = c_func(gdk_rgba_parse,{pGDKRGBA,"#FFFFFF"})
--              assert(bOK)
--              c_proc(gtk_widget_override_background_color,{handle,GTK_STATE_FLAG_NORMAL,pGDKRGBA})
--          end if

        xpg_setID(handle,id)
        xpg_gtk_add_to(parent,handle)
--      c_proc(gtk_container_add,{ctrl_handles[parent],handle})
--      xpg_gtk_signal_connect(handle,"realize",xpg_gtk_widget_realized,id)
        xpg_gtk_signal_connect(handle,"focus-in-event",xpg_gtk_focusinout,id)
        xpg_gtk_signal_connect(handle,"focus-out-event",xpg_gtk_focusinout,id)
        xpg_gtk_signal_connect(handle,"changed", xpg_gtk_text_changed,id);
        c_proc(gtk_widget_realize,{handle})

    elsif backend=XPG_WINAPI then
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
        elsif backend=XPG_GTK then
            atom buffer = c_func(gtk_entry_get_buffer,{handle})
            c_proc(gtk_entry_buffer_set_text,{buffer,v,-1})
        elsif backend=XPG_WINAPI then
            c_proc(xSetWindowText,{handle,v})
        else
            ?9/0 -- (unknown backend)
        end if
        return true
    elsif name="BGCOLOR"
       or name="BGCOLOUR" then
--      assert(string(v))
        if string(v) then v = xpg_get_colour_value(v) end if
        if not bMapped then
            xpg_defer_attr(id,name,v)
        elsif backend=XPG_GTK then
--          atom buffer = c_func(gtk_entry_get_buffer,{handle})
--          c_proc(gtk_entry_buffer_set_text,{buffer,v,-1})
--?9/0
--gtk_widget_override_background_color -- GTK3 only
--gtk_widget_modify_base
            v = sprintf("#%06x",v)
            bool bOK
            if bGTK3 then
                bOK = c_func(gdk_rgba_parse,{pGDKRGBA,v})
                assert(bOK)
                c_proc(gtk_widget_override_background_color,{handle,GTK_STATE_FLAG_NORMAL,pGDKRGBA})
--/* same problem:
                atom css = c_func(gtk_css_provider_new,{})
                string gtk_css_text = sprintf("* {background-image:none; background-color:%s;}",{v})
                c_proc(gtk_css_provider_load_from_data,{css,gtk_css_text,-1,NULL})
                atom context = c_func(gtk_widget_get_style_context,{handle})
                c_proc(gtk_style_context_add_provider,{context,css,GTK_STYLE_PROVIDER_PRIORITY_USER})
                c_proc(xg_object_unref,{css})
--*/
            else
                bOK = c_func(gdk_color_parse,{v,pGDKCOLOR})
                assert(bOK)
                c_proc(gtk_widget_modify_base,{handle,GTK_STATE_NORMAL,pGDKCOLOR})
            end if
--gtk_widget_modify_style
--/*
void pushButton( GtkWidget* button ) {
    // from your code
    gtk_button_set_label( GTK_BUTTON( button ), "new_text" );

    // You need an object to store css information: the CSS Provider
    GtkCssProvider * cssProvider = gtk_css_provider_new();
    // Load CSS into the object ("-1" says, that the css string is \0-terminated)
    gtk_css_provider_load_from_data(css, "* { background-image:none; background-color:red;}",-1,NULL); 

    // The "Style context" manages CSS providers (as there can be more of them)            
    GtkStyleContext * context = gtk_widget_get_style_context(button);   
    // So we want to add our CSS provider (that contains the CSS) to that "style manager".
    gtk_style_context_add_provider(context, GTK_STYLE_PROVIDER(css),GTK_STYLE_PROVIDER_PRIORITY_USER);


    // I'm not sure, if you need this. I took it from mame89's code
    g_object_unref (css);  
}
Although: Be aware, that this code will create a new provider everytime the button was pushed. 
I guess it's better practice to store the provider somewhere and removing and adding it to the (style)context when needed.

============================

GdkDisplay *display;
GdkScreen *screen;
display = gdk_display_get_default ();
screen = gdk_display_get_default_screen (display);

GtkCssProvider *provider;

provider = gtk_css_provider_new ();

gtk_style_context_add_provider_for_screen (screen, GTK_STYLE_PROVIDER (provider), GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);

gtk_css_provider_load_from_data(provider, CSS, -1, NULL);

g_object_unref (provider);

/* styling background color to black */
GtkCssProvider* provider = gtk_css_provider_new();
GdkDisplay* display = gdk_display_get_default();
GdkScreen* screen = gdk_display_get_default_screen(display);


gtk_style_context_add_provider_for_screen(screen,
GTK_STYLE_PROVIDER(provider),
GTK_STYLE_PROVIDER_PRIORITY_USER);

gtk_css_provider_load_from_data(GTK_CSS_PROVIDER(p rovider),
"#main_drawing_region,GtkDrawingArea { \n"
" background-color: black; \n"
"} \n", -1, NULL);
g_object_unref(provider);



#some-widget-name,
gtk-widget-type-goes-here#title-label-if-any {
background-color : black;
}


void
gtk_widget_modify_style (
  GtkWidget* widget,
  GtkRcStyle* style
)
GtkRcStyle* gtk_widget_get_modifier_style (GtkWidget* widget);
--          atom rcstyle = c_func(gtk_widget_get_modifier_style,{handle})
void        gtk_widget_modify_bg          (GtkWidget            *widget,
                       GtkStateType          state,
                       const GdkColor       *color);
typedef enum
{
  GTK_STATE_NORMAL,
  GTK_STATE_ACTIVE,
  GTK_STATE_PRELIGHT,
  GTK_STATE_SELECTED,
  GTK_STATE_INSENSITIVE
} GtkStateType;

gboolean
gdk_color_parse (
  const gchar* spec,
  GdkColor* color
)




  GtkRcStyle *rc_style;  
  GdkColor color;

  iupgdkColorSetRGB(&color, r, g, b);

  rc_style = gtk_widget_get_modifier_style(handle);

  rc_style->base[GTK_STATE_NORMAL]   = rc_style->bg[GTK_STATE_NORMAL] = rc_style->bg[GTK_STATE_INSENSITIVE] = color;
  rc_style->base[GTK_STATE_ACTIVE]   = rc_style->bg[GTK_STATE_ACTIVE] = gtkDarkerColor(&color);
  rc_style->base[GTK_STATE_PRELIGHT] = rc_style->bg[GTK_STATE_PRELIGHT] = rc_style->base[GTK_STATE_INSENSITIVE] = gtkLighterColor(&color);

  rc_style->color_flags[GTK_STATE_NORMAL] |= GTK_RC_BASE | GTK_RC_BG;
  rc_style->color_flags[GTK_STATE_ACTIVE] |= GTK_RC_BASE | GTK_RC_BG;
  rc_style->color_flags[GTK_STATE_PRELIGHT] |= GTK_RC_BASE | GTK_RC_BG;
  rc_style->color_flags[GTK_STATE_INSENSITIVE] |= GTK_RC_BASE | GTK_RC_BG;

  gtk_widget_modify_style(handle, rc_style);

 case WM_CTLCOLORSTATIC:
    {
        HDC hdcStatic = (HDC)wParam;
        if(lParam == (LPARAM)staticTextFieldTwo)
        {
            SetTextColor(hdcStatic, RGB(0, 255, 0));
            SetBkColor(hdcStatic, RGB(0, 255, 255));
            if (!hbrush)
                hbrush = CreateSolidBrush(RGB(0, 255, 255));
            return (LRESULT)hbrush;
        }
--*/
        elsif backend=XPG_WINAPI then
--          c_proc(xSetWindowText,{handle,v})
--    SendMessage(ih->handle, EM_SETBKGNDCOLOR, 0, (LPARAM)color);
--bust...
--?{"EM_SETBKGNDCOLOR",handle,v}
--          {} = c_func(xSendMessage,{handle,EM_SETBKGNDCOLOR,0,v})
            ctrl_bg[id] = v
            if bMapped then
-- or maybe (untested) 
--              bool bOK = c_func(xRedrawWindow({handle,NULL,NULL,or_all({RDW_ERASE,RDW_FRAME,RDW_INVALIDATE})
                bool bOK = c_func(xInvalidateRect,{handle,NULL,true})
                assert(bOK)
            end if
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
        if backend=XPG_GTK then
            atom buffer = c_func(gtk_entry_get_buffer,{handle})
            return peek_string(c_func(gtk_entry_buffer_get_text,{buffer}))
        elsif backend=XPG_WINAPI then
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
--DEV do we need to do this?? (given the last few lines in xpg_Init())
--  xpg_register_handler(TEXT,"CLICK",{{4,4,"FOPII"},{4,4,"POPII"},{2,2,"FOP"},{2,2,"POP"}})
--  xpg_register_handler(TEXT,"VALUE_CHANGED",{{1,1,"PO"}})
    xpg_register_handler(TEXT,"VALUE_CHANGED",{"PO"})
    xpg_set_ctrl_msg(TEXT,xpg_Text,xpg_set_text_attribute,
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

local procedure xpg_gtk_store_set(atom store, iter, sequence columns, data, integer n, cfn)
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
    -- (internal routine, common to WinAPI and GTK)
    atom tree_store, path, iter,    -- (GTK only)
         hItem, tIdx                -- (WinAPI only)
    if backend=XPG_GTK then
        {tree_store, path, iter} = what
    elsif backend=XPG_WINAPI then
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
                if backend=XPG_GTK then
                    c_proc(gtk_tree_view_collapse_row,{tree_view,path})
                elsif backend=XPG_WINAPI then
                    {} = c_func(xSendMessage,{tree_view,TVM_EXPAND,TVE_COLLAPSE,hItem})
                else
                    ?9/0 -- (unknown backend)
                end if
            elsif state="EXPANDED" then
                if backend=XPG_GTK then
                    c_proc(gtk_tree_view_expand_row,{tree_view,path,false})
                elsif backend=XPG_WINAPI then
                    {} = c_func(xSendMessage,{tree_view,TVM_EXPAND,TVE_EXPAND,hItem})
                else
                    ?9/0 -- (unknown backend)
                end if
            else
                crash("invalid STATE:"&state)
            end if
        elsif name="USERDATA" then
            integer user_data = attrs[i+1]
            if backend=XPG_GTK then
                xpg_gtk_store_set(tree_store,iter,{USERDATA},{user_data},1,gtk_tree_store_set)
            elsif backend=XPG_WINAPI then
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
    if backend=XPG_GTK then
        {tree_store, iter, piter} = args
        c_proc(gtk_tree_store_append,{tree_store,iter,piter})
    elsif backend=XPG_WINAPI then
        pdx = args
    else
        ?9/0 -- (unknown backend)
    end if
    if string(tree_nodes) then tree_nodes = {tree_nodes} end if
    string desc = tree_nodes[1]
    integer l = min(length(tree_nodes),3)
    bool bLeaf = l=1 or atom(tree_nodes[l])
    if backend=XPG_GTK then
        icon = iff(bLeaf?dot:closed_folder)
        integer userdata = 0 -- (set/overidden later via attributes)
        coldata = {icon,desc,"#000000","#FFFFFF","",userdata}
        xpg_gtk_store_set(tree_store,iter,STD_COLS,coldata,NUM_COLS,gtk_tree_store_set) 
    elsif backend=XPG_WINAPI then
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
        if backend=XPG_GTK then
            path = c_func(gtk_tree_model_get_path,{tree_store,iter})
        end if
        object recargs
        if not bLeaf then
            if backend=XPG_GTK then
                chiter = allocate(32,true)
                recargs = {tree_store,chiter,iter}
            elsif backend=XPG_WINAPI then
                recargs = tIdx
            else
                ?9/0 -- (unknown backend)
            end if
            sequence children = tree_nodes[l]
            for child in children do
                xpg_tree_add_nodes_rec(child,tree_view,recargs)
            end for
            if backend=XPG_WINAPI then
                set_struct_field(idTVITEMEX,pTVITEMEX,"mask",TVIF_HANDLE+TVIF_CHILDREN)
                set_struct_field(idTVITEMEX,pTVITEMEX,"hItem",hItem)
                set_struct_field(idTVITEMEX,pTVITEMEX,"cChildren",length(children))
                bool bOK = c_func(xSendMessage,{tree_view,TVM_SETITEM,0,pTVITEMEX})
                assert(bOK)
            end if
        end if
        if backend=XPG_GTK then
            c_proc(gtk_tree_view_expand_to_path,{tree_view,path})
        elsif backend=XPG_WINAPI then
            {} = c_func(xSendMessage,{tree_view,TVM_EXPAND,TVE_EXPAND,hItem})
        else
            ?9/0 -- (unknown backend)
        end if
        if l=3 then
            if backend=XPG_GTK then
                recargs = {tree_store,path,iter}
            elsif backend=XPG_WINAPI then
                recargs = {hItem,tIdx}
            else
                ?9/0 -- (unknown backend)
            end if
            xpg_TreeSetNodeAttributes(tree_view,recargs,tree_nodes[2])
        end if
        if backend=XPG_GTK then
            c_proc(gtk_tree_path_free,{path})
        end if
    end if
end procedure

global function gTreeGetUserId(object treenode)
    integer res
    if backend=XPG_GTK then
        atom {tree_view,iter,path} = treenode,
             tree_store = c_func(gtk_tree_view_get_model,{tree_view}),
             pWord = allocate(machine_word(),true)
        c_proc(gtk_tree_model_get,{tree_store,iter,USERDATA,pWord,-1})
        res = peekns(pWord)
        free(pWord)
    elsif backend=XPG_WINAPI then
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
    if backend=XPG_GTK then
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
    elsif backend=XPG_WINAPI then
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
            if backend=XPG_GTK then
                atom iter = allocate(32,true),
                     tree_store = c_func(gtk_tree_view_get_model,{tree_view})
                c_proc(gtk_tree_store_clear,{tree_store})
                xpg_tree_add_nodes_rec(children,tree_view,{tree_store,iter,null})
--?"setAdded(1)"
                bNodesAdded = true
            elsif backend=XPG_WINAPI then
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
        if backend=XPG_GTK then
            atom tree_store = c_func(gtk_tree_view_get_model,{tree_view}),
                     chiter = allocate(32,true),
                      piter = treenode[2]
            args = {tree_store,chiter,piter}
        elsif backend=XPG_WINAPI then
            integer treeIdx = treenode[2]
            args = treeIdx
        else
            ?9/0 -- (unknown backend)
        end if
        for child in children do
            xpg_tree_add_nodes_rec(child,tree_view,args)
        end for
        if backend=XPG_GTK then
--?"setAdded(2)"
            bNodesAdded := true
--?{"bbNodesAdded",bNodesAdded}
        elsif backend=XPG_WINAPI then
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

local function xpg_gtk_row_collapsed(xpg_handle tree_view, atom iter, /*path*/, gdx /*user_data*/)  -- (GTK only)
    atom tree_store = c_func(gtk_tree_view_get_model,{tree_view})
    xpg_gtk_store_set(tree_store,iter,{COL_IMG},{closed_folder},1,gtk_tree_store_set) 
    return true
end function

--DEV?? (if we really need to, can we set some flag in (say) ctrl_flags[id]??)
bool bInAdd = false

local function xpg_gtk_row_expanded(xpg_handle tree_view, atom iter, path, gdx /*user_data*/)   -- (GTK only)
if not bInAdd then
    bInAdd = true
    atom tree_store = c_func(gtk_tree_view_get_model,{tree_view})
    xpg_gtk_store_set(tree_store,iter,{COL_IMG},{open_folder},1,gtk_tree_store_set) 
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

local function xpg_gtk_treeview_cursor_changed(xpg_handle tree_view, gdx /*id*/) -- (GTK only)
    atom tree_store = c_func(gtk_tree_view_get_model,{tree_view})
    if bPrev then
        xpg_gtk_store_set(tree_store,sel_iter,{F_COLOUR,B_COLOUR},{"#000000","#FFFFFF"},2,gtk_tree_store_set)
        bPrev = false
    end if
    c_proc(gtk_tree_view_get_cursor,{tree_view,ppPath,NULL})
    atom path = peekns(ppPath)
    if path!=NULL
    and c_func(gtk_tree_model_get_iter,{tree_store,sel_iter,path}) then
        xpg_gtk_store_set(tree_store,sel_iter,{F_COLOUR,B_COLOUR},{"#FFFFFF","#0000FF"},2,gtk_tree_store_set)
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
    if backend=XPG_GTK then
        tree_view = c_func(gtk_tree_view_new,{})
        xpg_setID(tree_view,id)
        atom selection = c_func(gtk_tree_view_get_selection,{tree_view})
        c_proc(gtk_tree_view_set_headers_visible,{tree_view,false})
        c_proc(gtk_tree_view_set_enable_search,{tree_view,false})
        c_proc(gtk_tree_view_set_enable_tree_lines,{tree_view,true})
        c_proc(gtk_tree_selection_set_mode,{selection,GTK_SELECTION_NONE})
--DEV or maybe on sandle?
--      xpg_gtk_signal_connect(tree_view,"realize",xpg_gtk_widget_realized,id)
        xpg_gtk_signal_connect(tree_view,"row-collapsed",xpg_gtk_row_collapsed,id)
        xpg_gtk_signal_connect(tree_view,"row-expanded",xpg_gtk_row_expanded,id)
        xpg_gtk_signal_connect(tree_view,"cursor-changed",xpg_gtk_treeview_cursor_changed,id)
        xpg_gtk_signal_connect(tree_view,"focus-in-event",xpg_gtk_focusinout,id)
        xpg_gtk_signal_connect(tree_view,"focus-out-event",xpg_gtk_focusinout,id)
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
    elsif backend=XPG_WINAPI then

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
--  xpg_register_handler(TREEVIEW,"BRANCHOPEN",{{1,1,"PO"}})
    xpg_register_handler(TREEVIEW,"BRANCHOPEN",{"PO"})
    xpg_set_ctrl_msg(TREEVIEW,xpg_TreeView,xpg_set_treeview_attribute,
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

-- The more hll-style controls: owt using (say) gLabel, gText, gHbox, or gVbox must/shd be defined after them!

local procedure xpg_DatePick(gdx id)
    -- (invoked via xpg_map)
    gdx parent = xpg_get_parent_id(id)
    atom datepick
    if backend=XPG_GTK then
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
    elsif backend=XPG_WINAPI then

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
        elsif backend=XPG_GTK then
            c_proc(gtk_frame_set_label,{handle,v})
        elsif backend=XPG_WINAPI then
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

global function gDatePick(object value_changed=NULL, sequence attributes="", args={})
    {value_changed,attributes,args} = paranormalise_raa(value_changed,attributes,args)
    if not bInit then xpg_Init() end if
    integer id = xpg_add_control(DATEPICK)
--  xpg_register_handler(DATEPICK,"VALUE_CHANGED",{{1,1,"PO"}})
    xpg_register_handler(DATEPICK,"VALUE_CHANGED",{"PO"})
    xpg_set_ctrl_msg(DATEPICK,xpg_DatePick,xpg_set_datepick_attribute,
                                           xpg_get_datepick_attribute)
    if value_changed!=NULL then
        gSetHandler(id,"VALUE_CHANGED",value_changed)
    end if
    if length(attributes) then
        gSetAttributes(id,attributes,args)
    end if
    return id
end function 

local procedure xpg_splitter_redraw(gdx canvas, integer w,h)
    string orientation = gGetAttribute(gGetParent(canvas),"ORIENTATION")
    if orientation="HORIZONTAL" then
        gCanvasLine(canvas,1,0,1,h-1)
    else
        gCanvasLine(canvas,0,1,w-1,1)
    end if
end procedure
internal_rtns &= xpg_splitter_redraw; -- (let/permit/allow this to be a perfectly valid rtn)

local procedure xpg_splitter_click(gdx canvas, sequence status, integer x, y)
    gdx parent = gGetParent(canvas)
    assert(ctrl_types[parent]=BOX)
    bool bDrag = status[2]!='R',
         bVert = and_bits(ctrl_flags[parent],CF_VERTICAL)!=0
    integer cldx = 1 + bVert,
            click = {x,y}[cldx]
    ctrl_xtra[canvas][CX_GTL_ATTRS][PX_DRAG] = bDrag
    ctrl_xtra[canvas][CX_GTL_ATTRS][PX_CLICK] = click
--Temp?: (no help anyway)
--if not bDrag then
--  gRedraw(parent)
--end if
end procedure
internal_rtns &= xpg_splitter_click; -- (let/permit/allow this to be a perfectly valid rtn)

local procedure xpg_splitter_mousemove(gdx canvas, integer x,y, bool left,middle,right)
--?{"xpg_splitter_mousemove",canvas, x,y, left,middle,right}
    bool bDrag = ctrl_xtra[canvas][CX_GTL_ATTRS][PX_DRAG]
    if bDrag then
        gdx parent = gGetParent(canvas)
        bool bVert = and_bits(ctrl_flags[parent],CF_VERTICAL)!=0
        gdx {c1,s,c2} = children_ids[parent]
        assert(s==canvas)
        integer cldx = 1 + bVert,
               mouse = {x,y}[cldx],
               click = ctrl_xtra[canvas][CX_GTL_ATTRS][PX_CLICK],
                diff = click-mouse, -- (+ve left/up, -ve right/down)
              sizedx = SZ_W + bVert,
                  s1 = ctrl_size[c1][sizedx],
                  s2 = ctrl_size[c2][sizedx],
                 s12 = s1+s2,
                   d = max(min(diff,s1),-s2)
        atom f = (s1-d)/s12
        assert(f>=0.0 and f<=1.0)
--?{"f",f}
        -- (aside: f is applied by xpg_lm_disperse_user_sizes)
        ctrl_xtra[canvas][CX_GTL_ATTRS][PX_FRAC] = f
--DEV this might be better on a timer??
        gRedraw(parent)
    end if
end procedure
internal_rtns &= xpg_splitter_mousemove; -- (let/permit/allow this to be a perfectly valid rtn)

global function gSplit(gdx child1, child2, string orientation="VERTICAL")
--, sequence attributes="", args={})
--  {orientation,attributes,args} = paranormalise_taa(orientation,attributes,args)
--  if orientation="" then orientation = "VERTICAL" end if
    bool bVert = {true,false}[find(orientation,{"VERTICAL","HORIZONTAL"})]
    if not bInit then xpg_Init() end if
--  gdx splitter = gCanvas(xpg_splitter_redraw,"SPLIT=AYE"), id
    gdx splitter = gCanvas(xpg_splitter_redraw), id
    gSetAttribute(splitter,"EXPAND",orientation)
    gCanvasSetLineStyle(splitter,XPG_DOTTED)
    gCanvasSetBackground(splitter,#F0F0F0)
    gCanvasSetForeground(splitter,#C0C0C0)
    gSetHandler(splitter,"CLICK",xpg_splitter_click)
    gSetHandler(splitter,"MOUSEMOVE",xpg_splitter_mousemove)
    object p_attr = repeat(0,PX_LEN)
    p_attr[PX_FRAC] = -1 -- (not initially in use)
    ctrl_xtra[splitter][CX_GTL_ATTRS] = p_attr
    p_attr = 0 -- (kill refcount)
    if bVert then
        gSetAttribute(splitter,"SIZE","x3")
        id = gVbox({child1,splitter,child2})
    else
        gSetAttribute(splitter,"SIZE","3x")
        id = gHbox({child1,splitter,child2})
    end if
    gSetAttribute(splitter,"SPLIT","AYE") -- (private/undocumented/once parent "known")
    return id
--/*
--maybe, if inactive: (and 
else if (msg == WM_SETCURSOR) {
    if ((HWND)wParam == hwndWhatever) {
--      SetCursor(hCursorHand);
        SetCursor(NULL);
        return(TRUE);
    }
--*/
end function 

--/*
//Some js:
let ismdwn = 0
rpanrResize.addEventListener('mousedown', mD)

function mD(event) {
  ismdwn = 1
  document.body.addEventListener('mousemove', mV)
  document.body.addEventListener('mouseup', end)
}

function mV(event) {
  if (ismdwn === 1) {
    pan1.style.flexBasis = event.clientX + "px"
  } else {
    end()
  }
}
const end = (e) => {
  ismdwn = 0
  document.body.removeEventListener('mouseup', end)
  rpanrResize.removeEventListener('mousemove', mV)
}
div {
  display: flex;
  border: 1px black solid;
  width: 100%;
  height: 200px;
}

#pan1 {
  flex-grow: 1;
  flex-shrink: 0;
  flex-basis: 50%; /* initial status */
}

#pan2 {
  flex-grow: 0;
  flex-shrink: 1;
  overflow-x: auto;
}

#rpanrResize {
  flex-grow: 0;
  flex-shrink: 0;
  background: #1b1b51;
  width: 0.2rem;
  cursor: col-resize;
  margin: 0 0 0 auto;
}
<div>
  <div id="pan1">MENU</div>
  <div id="rpanrResize">&nbsp;</div>
  <div id="pan2">BODY</div>
</div>
--*/
--and
--/*
Improving on Reza's answer:

prevent the browser from interfering with a drag
prevent setting an element to a negative size
prevent drag getting out of sync with the mouse due to incremental delta interaction with element width saturation
<html><head><style>

.splitter {
    width: 100%;
    height: 100px;
    display: flex;
}

#separator {
    cursor: col-resize;
    background-color: #aaa;
    background-image: url("data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='10' height='30'><path d='M2 0 v30 M5 0 v30 M8 0 v30' fill='none' stroke='black'/></svg>");
    background-repeat: no-repeat;
    background-position: center;
    width: 10px;
    height: 100%;

    /* Prevent the browser's built-in drag from interfering */
    -moz-user-select: none;
    -ms-user-select: none;
    user-select: none;
}

#first {
    background-color: #dde;
    width: 20%;
    height: 100%;
    min-width: 10px;
}

#second {
    background-color: #eee;
    width: 80%;
    height: 100%;
    min-width: 10px;
}

</style></head><body>

<div class="splitter">
    <div id="first"></div>
    <div id="separator" ></div>
    <div id="second" ></div>
</div>

<script>

// A function is used for dragging and moving
function dragElement(element, direction)
{
    var   md; // remember mouse down info
    const first  = document.getElementById("first");
    const second = document.getElementById("second");

    element.onmousedown = onMouseDown;

    function onMouseDown(e)
    {
        //console.log("mouse down: " + e.clientX);
        md = {e,
              offsetLeft:  element.offsetLeft,
              offsetTop:   element.offsetTop,
              firstWidth:  first.offsetWidth,
              secondWidth: second.offsetWidth
             };

        document.onmousemove = onMouseMove;
        document.onmouseup = () => {
            //console.log("mouse up");
            document.onmousemove = document.onmouseup = null;
        }
    }

    function onMouseMove(e)
    {
        //console.log("mouse move: " + e.clientX);
        var delta = {x: e.clientX - md.e.clientX,
                     y: e.clientY - md.e.clientY};

        if (direction === "H" ) // Horizontal
        {
            // Prevent negative-sized elements
            delta.x = Math.min(Math.max(delta.x, -md.firstWidth),
                       md.secondWidth);

            element.style.left = md.offsetLeft + delta.x + "px";
            first.style.width = (md.firstWidth + delta.x) + "px";
            second.style.width = (md.secondWidth - delta.x) + "px";
        }
    }
}


dragElement( document.getElementById("separator"), "H" );

</script></body></html>
--*/
--Reza's original, for better style aspects [perhaps]
--/*
function onload()
{
    dragElement( document.getElementById("separator"), "H" );
}

// This function is used for dragging and moving
function dragElement( element, direction, handler )
{
  // Two variables for tracking positions of the cursor
  const drag = { x : 0, y : 0 };
  const delta = { x : 0, y : 0 };
  /* If present, the handler is where you move the DIV from
     otherwise, move the DIV from anywhere inside the DIV */
  handler ? ( handler.onmousedown = dragMouseDown ): ( element.onmousedown = dragMouseDown );

  // A function that will be called whenever the down event of the mouse is raised
  function dragMouseDown( e )
  {
    drag.x = e.clientX;
    drag.y = e.clientY;
    document.onmousemove = onMouseMove;
    document.onmouseup = () => { document.onmousemove = document.onmouseup = null; }
  }

  // A function that will be called whenever the up event of the mouse is raised
  function onMouseMove( e )
  {
    const currentX = e.clientX;
    const currentY = e.clientY;

    delta.x = currentX - drag.x;
    delta.y = currentY - drag.y;

    const offsetLeft = element.offsetLeft;
    const offsetTop = element.offsetTop;


    const first = document.getElementById("first");
    const second = document.getElementById("second");
    let firstWidth = first.offsetWidth;
    let secondWidth = second.offsetWidth;
    if (direction === "H" ) // Horizontal
    {
        element.style.left = offsetLeft + delta.x + "px";
        firstWidth += delta.x;
        secondWidth -= delta.x;
    }
    drag.x = currentX;
    drag.y = currentY;
    first.style.width = firstWidth + "px";
    second.style.width = secondWidth + "px";
  }
}
.splitter {
    width: 500px;
    height: 100px;
    display: flex;
}

#separator {
    cursor: col-resize;
    background: url(https://raw.githubusercontent.com/RickStrahl/jquery-resizable/master/assets/vsizegrip.png) center center no-repeat #535353;
    width: 10px;
    height: 100px;
    min-width: 10px;
}

#first {
    background-color: green;
    width: 100px;
    height: 100px;
    min-width: 10px;
}

#second {
    background-color: red;
    width: 390px;
    height: 100px;
    min-width: 10px;
}
<html>

    <head>
        <link rel="stylesheet" href="T10-Splitter.css">
        <script src="T10-Splitter.js"></script>
    </head>

    <body onload="onload()">
        <div class="splitter">
            <div id="first"></div>
            <div id="separator"></div>
            <div id="second"></div>
        </div>
    </body>

</html>
--*/

-- https://stackoverflow.com/questions/5561236/win32-splitter-control

--DEV/temp:
procedure xpg_lm_dump_ctrls()
    printf(1,"id ---ctyp---   x   y   w   h  nw  nh  uw  uh  p  children   flags\n")
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
            printf(1,"%2d %-10s %57s\n",{id,idt,"Not mapped"})
        else
            if ct=BOX then
                integer v = and_bits(flags,CF_VERTICAL)
                idt := iff(v?"Vbox":"Hbox")
                flags -= v
            end if
--          integer e = and_bits(flags,CF_EXPAND)
--          flags -= e
            string f = decode_flags(cf_glags,flags)
--          if e then
--              f &= ",CF_EXPAND"&("BHHV"[e])
--          end if
--          if not find(ct,{BOX,MENU,TIMER,CLIPBOARD}) then
--              f = sprintf("%v",{xpg_get_window_rect(id)})
--          end if
            printf(1,"%2d %-10s %3d %3d %3d %3d %3d %3d %3d %3d %2d  %-10v %s\n",
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
--  if bRealise and backend=XPG_GTK then
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

--SUG: if we can make XPG_CENTER #FFF0 here, then we could have XPG_CENTER===XPG_CENTRE...

-- gShow x and y less than #FFF0 (65520) are treated as actual pixel locations,
-- otherwise the low 4 bits determine the parent/mouse-relative positioning:
--                                                PBT (for y)
--                                                 RL (for x)
global constant XPG_CURRENT      = #FFF0,   -- 0b0000   -- 65520
                XPG_LEFT         = #FFF1,   -- 0b0001   -- 65521
                XPG_RIGHT        = #FFF2,   -- 0b0010   -- 65522
--              XPG_CENTER       = #FFF3,   -- 0b0011   -- 65523 -- (used above for -> XPG_C)
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
--?{"xpg_placement",xy, os, ol, is, il}
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

local function xpg_gtk_menu_position_func(atom /*pMenu*/, pX, pY, pPushIn, xy)
--?{"xpg_gtk_menu_position_func","x",floor(xy/#8000),"y",and_bits(xy,#7FFF)}
    poke4(pX,floor(xy/#8000))
    poke4(pY,and_bits(xy,#7FFF))
    poke4(pPushIn,false)
    return 0 -- (ignored)
end function

local function xpg_WinAPI_any_active_window() -- WinAPI only
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
    if backend=XPG_GTK then
        if bGTK3 then
            c_proc(gtk_menu_popup_at_pointer,{handle,NULL})
        else
            atom cb = call_back(xpg_gtk_menu_position_func),
               data = x*#8000+y,
             cevntt = c_func(gtk_get_current_event_time,{})
            c_proc(gtk_menu_popup,{handle,NULL,NULL,cb,data,0,cevntt})
        end if
    elsif backend=XPG_WINAPI then
        atom hwnd = xpg_WinAPI_any_active_window()
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
        -- (to be fair GTK3 is so pig-awful slow a tiny flicker'l bother no-one)
        if backend=XPG_GTK and bGTK3 then
            c_proc(gtk_widget_show_all,{handle})
        end if
--DEV gRefresh(id)...
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
--?{"lmdus",w-dw,h-dh,w,h,dw,dh}
?{"gShow",id,w,dw,h,dh,{w-dw,h-dh}}
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
--xpg_lm_dump_ctrls()
--?"calculate natural sizes..."
--      if backend=XPG_WINAPI then
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
--?{"gShow",id,wx,wy}
    ctrl_size[id][SZ_X] = wx
    ctrl_size[id][SZ_Y] = wy
    if backend=XPG_GTK then
--?{"gtk_window_move",id}
--?{"gShow:gtk_window_move",id,wx,wy}
        c_proc(gtk_window_move,{handle,wx,wy})
--?{"<gtk_window_move",id}
    elsif backend=XPG_WINAPI then
--if handle then
        integer flags = SWP_NOZORDER+SWP_NOSIZE
--      integer flags = SWP_NOZORDER
        integer w = ctrl_size[id][SZ_W]+dw,
                h = ctrl_size[id][SZ_H]+dh
--?{"gShow(initial position)",id,wx,wy,{w,h,"??"}}
--nMapDepth += 1
        bool ok = c_func(xSetWindowPos,{handle,NULL,wx,wy,w,h,flags})
--      bool ok = c_func(xSetWindowPos,{handle,NULL,wx,wy,w+16,h+29,flags})
--nMapDepth -= 1
--?{"gShow:swp returned",ok}
        assert(ok)
--end if
    else
        ?9/0 -- (unknown backend)
    end if
--end procedure
--?"moved"
--DEV setAttribute(h,"VISIBLE",true)...
    if backend=XPG_GTK then
--?{gGetAttribute(id,"SIZE"),"good"}
        if c_func(gtk_window_get_transient_for,{handle})=NULL then
--          if XPG_PARENTDIALOG!=NULL
--          and XPG_PARENTDIALOG!=h then
--              c_proc(gtk_window_set_transient_for,{h,XPG_PARENTDIALOG}) 
--          else
                xpg_gtk_signal_connect(handle,"destroy",xpg_gtk_quit,id)
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
    elsif backend=XPG_WINAPI then
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
--?{"xpg_get_window_rect(final)",xpg_get_window_rect(id),id}
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

--DEV/SUG should we just invoke this from gShow()???? **** WHERE IS THE xpg_lm_xxx() ??!! ***
--global procedure gRedraw(gdx id, integer flags=0b111)
global procedure gRedraw(gdx id, integer flags=0b110)
--DEV? (docs say...)
    assert(and_bits(ctrl_flags[id],CF_MAPPED)!=0)
--7/11/23... (no change at all to Booker.exw...)
    integer {w,h} = gGetAttribute(id,"SIZE")
    bool bNow = and_bits(flags,0b001),  --DEV made up... see docs
         bResize = and_bits(flags,0b010)
    if bResize then
        xpg_resize(id,w,h)
    end if
    atom handle = ctrl_handles[id]
    if backend=XPG_GTK then
        atom window = c_func(gtk_widget_get_window,{handle})
        if window then
            c_proc(gdk_window_invalidate_rect,{window,NULL,true})
        end if
        c_proc(gtk_widget_queue_draw,{handle})
        if window and bNow then
            c_proc(gdk_window_process_updates,{window,true})
        end if
    elsif backend=XPG_WINAPI then
--/*
        atom dwFlags = or_all({RDW_ERASE,RDW_INVALIDATE,RDW_INTERNALPAINT})
--      atom dwFlags = or_all({RDW_INVALIDATE,RDW_INTERNALPAINT})
--?{"bNow",bNow}
        if bNow then dwFlags = or_bits(dwFlags,RDW_UPDATENOW) end if
        bool bOK = c_func(xRedrawWindow,{handle,NULL,NULL,dwFlags})
        assert(bOK)
--*/
--flipped back 4/11/23: (no help with guess the number 3...)
--      bool bOK = c_func(xInvalidateRect,{handle,NULL,true});
        bool bOK = c_func(xInvalidateRect,{handle,NULL,false});
        assert(bOK)
    else
        ?9/0 -- (unknown backend)
    end if
end procedure 

global function gQuit(gdx /*id*/)
    -- standard "Close"/"Quit" button shorthand
?"gQuit"
    return XPG_CLOSE
end function
internal_rtns &= gQuit; -- (let/permit/allow this to be a perfectly valid rtn)

local gdx mbid = NULL, lbl

--procedure gMsgBox(gdx parent,string title, msg, sequence args={}, bool bWrap=true)
global procedure gMsgBox(gdx parent=NULL, string title="", msg="", sequence args={})
    if length(args) then msg = sprintf(msg,args) end if
--DEV this may be an IUP-only thing.... quite how to resize for wordwrap remains an open question...
--  if string(msg) and find('\n',msg) and bWrap then
--      -- make each paragraph a single line, improves wordwrap
--      -- (note: this may be a windows only thing, not yet tested on lnx)
--      msg = substitute(msg,"\n\n","\r\r")
--      msg = substitute(msg,"\n"," ")
--      msg = substitute(msg,"  "," ")
--      msg = substitute(msg,"\r\r","\n\n")
--  end if
    if mbid=NULL then
--      lbl = gLabel(msg,"MARGIN=10x10")
        lbl = gLabel(msg)
--      if not find(gQuit,internal_rtns) then internal_rtns &= gQuit end if
        gdx btn = gButton("OK",gQuit),
             ok = gHbox({btn},"SPACE=LEFT"), -- DEV not aligning...
          child = gVbox({lbl,ok},"MARGIN=10x10,GAP=10")
--DEV/SUG a min width of 230, or possibly based on text_extent(title)...
        mbid = gDialog(child,parent,title)
--BS_DEFPUSHBUTTON
    else
        --DEV reparent??
        gSetAttribute(mbid,"TITLE",title)
        gSetAttribute(lbl,"TITLE",msg)
--      gSetAttribute(mbid,"SIZE",NULL) -- DEV oops
        gRedraw(mbid,0b111)             -- DEV not resizing...
    end if
--DEV modal??
--  ?{"gMsgBox",parent,title,msg,bWrap}
    gShow(mbid,XPG_CENTERPARENT,XPG_CENTERPARENT) -- DEV not repositioning...
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
    if backend=XPG_GTK then
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
    elsif backend=XPG_WINAPI then
        while c_func(xGetMessage,{pMSG,NULL,0,0}) do
--DEV
--          if not translateAccelerator() then
--
-- https://devblogs.microsoft.com/oldnewthing/20201231-00/?p=104627
-- https://devblogs.microsoft.com/oldnewthing/20230329-00/?p=107983
--Xhttp://blogs.msdn.com/oldnewthing/archive/2003/10/21/55384.aspx
--Xhttps://devblogs.microsoft.com/oldnewthing/20031021-00/?p=55384
--https://devblogs.microsoft.com/oldnewthing/20031021-00/?p=42083
--
-- This worked, and even possibly better, but in the end I needed 
--  xpg_find_next_tab_stop() for GTK anyway, and given the hoops
--  I had to jump through to get VK_ESC working alongside it...
--
--/*
            bool dmsg = false
            atom hwnd = get_struct_field(idMESSAGE,pMSG,"hwnd")
            if hwnd then
                integer id = xpg_getID(hwnd)
                if id then
                    integer pid = gGetDialog(id)
                    atom phwnd = ctrl_handles[pid]
                    dmsg = c_func(xIsDialogMessage,{phwnd,pMSG})
                end if
            end if
            if not dmsg then
--*/
                c_proc(xTranslateMessage,{pMSG})
                c_proc(xDispatchMessage,{pMSG})
--          end if
        end while
    else
        ?9/0 -- (unknown backend)
    end if
end procedure

last_xpgui_rid = gMainLoop -- (or whatever ends up last in this file)

--/*
--DEV prolly better to set up an idle doobrie...
global procedure gLoopStep()
    if backend=XPG_GTK then
--      if (gtk_main_iteration_do(FALSE)) return IUP_CLOSE;
--      return IUP_DEFAULT;
    elsif backend=XPG_WINAPI then
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

--/*
C:\Program Files (x86)\Phix\demo\xpGUI\aaline.exw:24 include xpGUI.e                        -- needs aacircle/arc, properly porting into xpGUI.e
C:\Program Files (x86)\Phix\demo\xpGUI\gButton.exw:21 include xpGUI.e                       -- GOOD
C:\Program Files (x86)\Phix\demo\xpGUI\gCanvas.exw:16 include xpGUI.e -- DEV silly size under WinAPI [FIXED 20/8/23]
C:\Program Files (x86)\Phix\demo\xpGUI\gCanvasPolygon.exw:31 include xpGUI.e
C:\Program Files (x86)\Phix\demo\xpGUI\gCheckbox.exw:10 include xpGUI.e
C:\Program Files (x86)\Phix\demo\xpGUI\gDatePick.exw:7 include xpGUI.e
C:\Program Files (x86)\Phix\demo\xpGUI\gDialog.exw:6 include xpGUI.e
C:\Program Files (x86)\Phix\demo\xpGUI\gDialogs.exw:16 include xpGUI.e
C:\Program Files (x86)\Phix\demo\xpGUI\gDropDown.exw:7 include xpGUI.e
C:\Program Files (x86)\Phix\demo\xpGUI\gFrame.exw:13 include xpGUI.e
C:\Program Files (x86)\Phix\demo\xpGUI\gGraph.exw:8 include xpGUI.e                     -- DEV silly size... [FIXED, but minsize on GTK remains...]
C:\Program Files (x86)\Phix\demo\xpGUI\gGraph1.exw:11 include xpGUI.e
C:\Program Files (x86)\Phix\demo\xpGUI\gGraph2.exw:7 include xpGUI.e
C:\Program Files (x86)\Phix\demo\xpGUI\gGraph4.exw:6 include xpGUI.e
C:\Program Files (x86)\Phix\demo\xpGUI\gHbox.exw:7 include xpGUI.e
C:\Program Files (x86)\Phix\demo\xpGUI\gLabel.exw:8 include xpGUI.e
C:\Program Files (x86)\Phix\demo\xpGUI\gList.exw:8 include xpGUI.e
C:\Program Files (x86)\Phix\demo\xpGUI\gMenu.exw:20 include xpGUI.e
C:\Program Files (x86)\Phix\demo\xpGUI\gProgressBar.exw:7 include xpGUI.e
C:\Program Files (x86)\Phix\demo\xpGUI\gRadio.exw:7 include xpGUI.e
C:\Program Files (x86)\Phix\demo\xpGUI\GraphR.exw:25 include xpGUI.e
C:\Program Files (x86)\Phix\demo\xpGUI\gSample.exw:20 include xpGUI.e                   -- [RUBBISH]
C:\Program Files (x86)\Phix\demo\xpGUI\gSlider.exw:7 include xpGUI.e
C:\Program Files (x86)\Phix\demo\xpGUI\gSpin.exw:10 include xpGUI.e
C:\Program Files (x86)\Phix\demo\xpGUI\gSplit.exw:26 include xpGUI.e
C:\Program Files (x86)\Phix\demo\xpGUI\gTable.exw:42 include xpGUI.e
C:\Program Files (x86)\Phix\demo\xpGUI\gTabs.exw:10 include xpGUI.e
C:\Program Files (x86)\Phix\demo\xpGUI\gText.exw:10 include xpGUI.e
C:\Program Files (x86)\Phix\demo\xpGUI\gTimer.exw:13 include xpGUI.e
C:\Program Files (x86)\Phix\demo\xpGUI\gTreeView.exw:9 include xpGUI.e -- ok
C:\Program Files (x86)\Phix\demo\xpGUI\gTreeView2.exw:50 include xpGUI.e
C:\Program Files (x86)\Phix\demo\xpGUI\gVbox.exw:7 include xpGUI.e
C:\Program Files (x86)\Phix\demo\xpGUI\r3d.exw:13 include xpGUI.e                       -- GOOD (cf 15_puzzle_game_in_3D)
C:\Program Files (x86)\Phix\demo\xpGUI\sample.exw:32 include xpGUI.e
C:\Program Files (x86)\Phix\demo\xpGUI\scroller.exw:27 include xpGUI.e                  -- GOOD, apart frm GTK3 (no scrollbars at all, gulp)
C:\Program Files (x86)\Phix\demo\xpGUI\test.exw:46 --include xpGUI.e                    -- [TRASH]

demo\pGUI\astextrix.exw [??]
demo\pGUI\boids3d.exw [but with a separate/hide-able settings window?]
demo\pGUI\filedump.exw
demo\pGUI\gears.exw [openGL]
demo\pGUI\HelloF.exw [openGL]
--demo\pGUI\IupSampleDialog.exw (see/compare with demo\xpGUI\sample.exw)
demo\pGUI\pdemo.exw
demo\pGUI\rubik.exw -- blimey, leave that one alone for now!
Searching for: include pGUI.e
 Files scanned 496, Directories scanned 7, Lines 761071
C:\Program Files (x86)\Phix\demo\rosetta\15_puzzle_game.exw:6 include pGUI.e                -- GOOD p2js: No font size(??), Esc to gMsgBox closes whole page...
C:\Program Files (x86)\Phix\demo\rosetta\15_puzzle_game_in_3D.exw:21 include pGUI.e         -- GOOD, except timer is horribly slow under WinAPI (good-ish on GTK)
                                                                                            --       and "3" seems to be anti-aliasing with an orange background
C:\Program Files (x86)\Phix\demo\rosetta\2048.exw:14 include pGUI.e                         -- GOOD (bar the final gMsgBox, needs a MINSIZE, and to be modal)
C:\Program Files (x86)\Phix\demo\rosetta\21_Game.exw:24 include pGUI.e                      -- partial (looks rubbish...)
C:\Program Files (x86)\Phix\demo\rosetta\9billionnames.exw:75 include pGUI.e                -- FAIR: wierd colour, grid slightly off, MINSIZE not working.
C:\Program Files (x86)\Phix\demo\rosetta\Abelian_sandpile_model.exw:28 include pGUI.e       -- FAIR: but usual canvas resizing issues, ""
C:\Program Files (x86)\Phix\demo\rosetta\animate_pendulum.exw:14 include pGUI.e             -- FAIR: needs anti-aliased circles under WinAPI
C:\Program Files (x86)\Phix\demo\rosetta\Animation.exw:8 include pGUI.e                     -- FAIR: no label click [dlg OK], colours iffy [GTK OK], margins on wrong thing??
C:\Program Files (x86)\Phix\demo\rosetta\AplusB.exw:14 include pGUI.e                       -- POOR: slightly better in GTK
C:\Program Files (x86)\Phix\demo\rosetta\Archimedean_spiral.exw:6 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Arithmetic_Integer.exw:1 (not yet part of distro)  -- ??
C:\Program Files (x86)\Phix\demo\rosetta\AudioAlarm.exw:20 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Audio_frequency_generator.exw:5 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\B-spline.exw:9 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Babylonian_spiral.exw:73 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\BarnsleyFern.exw:5 include pGUI.e                      -- GOOD, but might benefit from a pre/once-built gImage??
C:\Program Files (x86)\Phix\demo\rosetta\Bilinear_interpolation.exw:6 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Black_Box.exw:467 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\BrownianTree.exw:9 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\BullsAndCows.exw:34 include pGUI.e                     -- POOR: layout awful, inital focus, SIZE=NULL totally mishandled
C:\Program Files (x86)\Phix\demo\rosetta\Canny_Edge_Detection.exw:7 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Chaos_game.exw:6 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\ChatClient.exw:12 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\ChatServer.exw:13 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Clock.exw:6 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Color_quantization.exw:13 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Colour_bars.exw:7 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Colour_bars.exw:43 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Colour_pinstripe.exw:6 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Colour_separation.exw:7 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Colour_wheel.exw:8 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Compare_sorting_algorithms.exw:11 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Convex_hull.exw:65 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Conways_Game_of_Life.exw:6 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Create2Darray.exw:11 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\DeathStar.exw:8 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Determine_if_two_triangles_overlap.exw:5
C:\Program Files (x86)\Phix\demo\rosetta\DragonCurve.exw:8 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\DrawRotatingCube.exw:11 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Draw_a_sphere.exw:6 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\draw_cuboid.exw:18 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Draw_pixel_2.exw:5 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\ElevatorSimulation.exw:8 include pGUI.e                -- GOOD, except disable resize
C:\Program Files (x86)\Phix\demo\rosetta\Euler_method.exw:40 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\FibonacciFractal.exw:5 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Finite_State_Machine.exw:47 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Forest_fire.exw:13 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\FractalTree.exw:6 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Gallery_Generator.exw:203 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\GaltonBox.exw:10 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Gaussian_primes.exw:65 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Goldbachs_comet.exw:29 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Greed.exw:5                                            --DEV make this a gui (xpGUI/include pGUI.e) make @ more visible, write a solver?
C:\Program Files (x86)\Phix\demo\rosetta\Greyscale_bars.exw:8 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Guess_the_number.exw:5 include pGUI.e                  -- ?
C:\Program Files (x86)\Phix\demo\rosetta\Guess_the_number2.exw:5 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Guess_the_number3.exw:6 include pGUI.e                 -- FAIR: margin not applied on labels, no label resize on set value
C:\Program Files (x86)\Phix\demo\rosetta\GUI_component_interaction.exw:6 include pGUI.e         -- ?
C:\Program Files (x86)\Phix\demo\rosetta\GUI_enabling_and_disabling_controls.exw:5              -- ?
C:\Program Files (x86)\Phix\demo\rosetta\GUI_maximum_window_dimensions.exw:23                   -- ?
C:\Program Files (x86)\Phix\demo\rosetta\hexapawn.exw:27 include pGUI.e                         -- ?
C:\Program Files (x86)\Phix\demo\rosetta\hilbert_curve.exw:8 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Hough_transform.exw:11 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\ImageNoise.exw:7 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Image_convolution.exw:8 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Inventory_sequence.exw:35 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Julia_set.exw:7 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Julia_set.exw:170 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Keyboard_macros.exw:6 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Koch_curve.exw:6 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\K_means_clustering.exw:17 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\mastermind.exw:54 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Matrix_Digital_Rain.exw:14 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\minQBN.exw:7 --include pGUI.e                          -- ?
C:\Program Files (x86)\Phix\demo\rosetta\minQBN.exw:10 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Modified_random_distribution.exw:44 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Morpion_solitaire.exw:70 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Morse_code.exw:6 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Mouse_position.exw:12 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Munching_squares.exw:6 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Musical_scale.exw:5 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\NumberTripletsGame.exw:7 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\OpenGL.exw:12 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\OpenGLShader.exw:13 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\OpenGLShader.exw:244 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Order_by_pair_comparisons.exw:316 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Particle_fountain.exw:6 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\peano_curve.exw:8 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Penrose_tiling.exw:11 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Pentagram.exw:9 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Perceptron.exw:17 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Peripheral_Drift_Illusion.exw:12 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\plasma.exw:7 include pGUI.e                            -- ?
C:\Program Files (x86)\Phix\demo\rosetta\Playing_cards.exw:60 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Plot_coordinate_pairs.exw:6 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Polynomial_regression.exw:56 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Polyspiral.exw:12 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Pseudorandom_number_generator_image.exw:7
C:\Program Files (x86)\Phix\demo\rosetta\PythagorasTree.exw:8 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Rank_History.exw:26 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Raster_bars.exw:10 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Run_examples.exw:32 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\safe_mode.exw:109 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Sierpinski_arrowhead_curve.exw:10 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Sierpinski_curve.exw:10 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Sierpinski_square_curve.exw:9 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\SierpinskyPentagon.exw:7 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\SierpinskyPentagon.svg.exw:29 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\SierpinskyPentagon.svg.exw:524 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\SierpinskyTriangle.exw:6 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Simple_window.exw:5 include pGUI.e                     -- ?
C:\Program Files (x86)\Phix\demo\rosetta\Simulate_keyboard_input.exw:6 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Simulate_mouse_input.exw:8 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Single_instance.exw:8 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\sleep.exw:23 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Snake.exw:19 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Snake_AI.exw:158 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Speak.exw:13 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Speech.exw:12 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Spinning_rod_animation.exw:37 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\SpliRT.exw:12 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Sudoku.exw:1038 --include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Sunflower.exw:8 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Superellipse.exw:8 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Sutherland_Hodgman_polygon_clipping.exw:53
C:\Program Files (x86)\Phix\demo\rosetta\tamagotchi.exw:10 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Tetrominoes.exw:22 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Tic_tac_toe.exw:11 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Tupper.exw:39 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\turtle.e:15 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Uno.exw:18 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\User_Input_Graphical.exw:8 include pGUI.e              -- ?
C:\Program Files (x86)\Phix\demo\rosetta\vibrect.exw:13 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\viewppm.exw:10 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\virtunome.exw:11 include pGUI.e                        -- ?
C:\Program Files (x86)\Phix\demo\rosetta\VoronoiDiagram.exw:9 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\WaveFunctionCollapse.exw:7 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\WaveFunctionCollapse.exw:339 --include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\WaveFunctionCollapse.exw:342 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Window_creation.exw:6 include pGUI.e                   -- ?
C:\Program Files (x86)\Phix\demo\rosetta\Window_management.exw:6 include pGUI.e                 -- ?
C:\Program Files (x86)\Phix\demo\rosetta\Wireworld.exw:98 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Wordiff.exw:48 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\XiaolinWuLine.exw:19 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Yellowstone_sequence.exw:30 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\Yin_and_yang.exw:6 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\7guis\Booker.exw:6 include pGUI.e                      -- meh: dlg size shd dflt from TITLE, expand off
C:\Program Files (x86)\Phix\demo\rosetta\7guis\Cells.exw:6 include pGUI.e                       -- **needs** editable fields on a gTable...
C:\Program Files (x86)\Phix\demo\rosetta\7guis\CircleDraw.exw:7 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\7guis\Converter.exw:6 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\7guis\Counter.exw:6 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\7guis\CRUD.exw:6 include pGUI.e
C:\Program Files (x86)\Phix\demo\rosetta\7guis\Timer.exw:6 include pGUI.e
C:\Program Files (x86)\Phix\pwa\phix\hello_world.exw:1 (needs {{libheader|Phix/pGUI}})          -- ?
 [https://rosettacode.org/wiki/Hello_world/Newbie#Phix]
https://rosettacode.org/wiki/Integer_comparison#Phix add a gui
<syntaxhighlight lang="wren">
WM_GETMINMAXINFO                0x0024
           
hmm: https://hoyoung2.blogspot.com/2011/06/gtk-tutorial.html
     https://github.com/tindzk/GTK/blob/master/examples/fixed/fixed.c

--*/

