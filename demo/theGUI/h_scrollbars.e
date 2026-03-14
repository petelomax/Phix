--
-- demo\theGUI\h_scrollbars.e
--
--  Common code for hscroller.exw and hTable.exw
--

--sequence scrollsize
--  ctrl_xtra, CXCF_SCROLL, CX_SBINFO, SB_SCRLW
--tg_XPM_from_shorthand
--MOUSEPOS
--constant tg_XPM_from_shorthand = gGetGlobal(`tg_XPM_from_shorthand`) -- (undocumented, now removed)

enum CANVAS, GRAPH --, LIST, TABLE
--DEV these should be the real (global) replacements...
--constant CXLIST_LINEHT = 0,
--       CXTBL_LINEHT = 0

--DEV:
global -- for hTable,...
sequence ctrl_xtra = {} -- NB hscroller/desktop-specific [BLUFF... this stuff should all work in a browser just fine!]
--enum CX_SBINFO, -- (fake version of the same-named thing in theGUI.e)
--   CX_CANVAS_FLAGS, -- ""
--   CX_LEN = $

-- ctrl_xtra for gCanvas()
local enum CX_CANVAS_TYPE,      -- CANVAS/GRAPH/LIST/TABLE/TABS... [BUTTON/TEXT/TREE]
           CX_CANVAS_FLAGS,     -- CXCF_SCROLL (etc, see below)
--         CX_CANVAS_HDC,       -- TG_WINAPI:hDC, TG_GTK:{cairo,layout}
--         CX_PENSTYLE,         -- eg/default TG_CONTINUOUS
--         CX_PENWIDTH,         -- default 1
--         CX_TXTANGLE,         -- default 0 (degrees, not radians)
           Cx_GTL_ATTRS,        -- gGraph/gList/gTable/Tabs, see GX/LX/CXxxx
           CX_SBINFO,           -- scrollbar info, NULL or see SB_XXX below 
--         CX_TRACKLEAVE,       -- track mouse leave events (and redraw)
--         CX_WU_BLEND,         -- WU_BLEND alpha/background blending flag
           CX_CANVAS_LEN = $    -- (init ctrl_xtra[id] this size)

-- for hTable,...
global constant CX_GTL_ATTRS = Cx_GTL_ATTRS -- not actually used here...

-- for CX_CANVAS_FLAGS:
local constant CXCF_SCROLL = 0b100 -- (===[CX_SBINFO]!=NULL, w/o p2js violation)

-- content of ctrl_xtra[canvas][CX_SBINFO], maybe others
local enum SB_HVISB, -- horizontal scrollbar visible
           SB_VVISB, -- vertical scrollbar visible
           SB_VTTOP, -- vertical thumb top
           SB_VTEND, -- vertical thumb end
           SB_HTLFT, -- horizontal thumb left
           SB_HTEND, -- horizontal thumb end
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
           SB_SCRFX, -- SCROLLFIX
           SB_SCRLW, -- SCROLLSIZE
           SB_SCRLH, --  "", next two are the VIEWPORT
           SB_ORIGX,    -- 0..SCROLLW-(w-VSBVIS*SBWID) [nb client offset, not scrollbar]
           SB_ORIGY,    -- 0..SCROLLH-(h-HSBVIS*SBHGH)              """
--DEV killme:
--         SB_SHOWG,    -- 1 = show drag (=== SHOWDRAG)
           SB_INFOLEN = $

global function tg_get_sig(rtn rid) -- (copied verbatim from theGUI.e)
    return get_routine_info(rid,false)[3] -- eg `FI`
end function

local function tg_to_int(object v) -- (copied verbatim from theGUI.e)
    -- convert eg `20` to 20, but v may already be integer
    if string(v) then
        v = to_number(v)
    end if
    assert(integer(v))
    return v
end function

local function tg_intint(string val)
    --
    -- convert eg `225x75` to {225,75}, and in fact 
    --            `50x10x20x30` to {50,10,20,30},
    --            `1`->{1}, and `1x2x3`->{1,2,3}.
    -- likewise eg `{225,75}` -> {225,75}, etc.
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
    -- would make `50x` not the same as `50x0`, matching pGUI.
--  res = apply(true,to_number,{res,-1})
    return res
end function


local function tg_set_canvas_attribute(gdx id, string name, object v, bool bMapped)
    integer ct = ctrl_xtra[id][CX_CANVAS_TYPE]
--  atom handle = ctrl_handles[id]
    if name=`SCROLLFIX` then
        v = tg_to_int(v)
        assert(v==v && 0b11)
        ctrl_xtra[id][CX_SBINFO][SB_SCRFX] = v
    elsif name=`SCROLLSIZE`
       or name=`SCROLLPORT`
       or name=`VIEWPORT` then
if bMapped then end if
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
                sbinfo[SB_MONLD] = ``
                sbinfo[SB_CKMON] = ``
                ctrl_xtra[id][CX_CANVAS_FLAGS] = or_bits(flags,CXCF_SCROLL)
            end if
--          if name=`SHOWDRAG` then
--              sbinfo[SB_SHOWG] = tg_to_bool(v)
--          else
                if string(v) then v = tg_intint(v) end if
                if name=`SCROLLSIZE` then
                    integer {w,h} = v
                    sbinfo[SB_SCRLW] = w
                    sbinfo[SB_SCRLH] = h
                else -- name=`SCROLLPORT` or name=`VIEWPORT` then   
--?{`set VIEWPORT`,v}
                    integer {x,y} = v
                    sbinfo[SB_ORIGX] = x
                    sbinfo[SB_ORIGY] = y
                end if
--          end if
            ctrl_xtra[id][CX_SBINFO] = sbinfo
        end if
        return true
--/*
    elsif name=`LINEWIDTH` then
        assert(atom(v),`placeholder`)
        ctrl_xtra[id][CX_PENWIDTH] = v
        return true
    elsif name=`LINESTYLE` then
        object style = v
        assert(not string(style),`placeholder`)
        if sequence(style) then
            for b in style do
                assert(b=0 or b=1)
            end for
            assert(style[1]=1 and style[$]=0)
            dashers[TG_CUSTOM_DASH] = style
            style = TG_CUSTOM_DASH
        end if
        ctrl_xtra[id][CX_PENSTYLE] = style
        return true
    elsif name=`WU_BLEND` then
        v = tg_to_bool(v)
        ctrl_xtra[id][CX_WU_BLEND] = v
        return true
    elsif ct!=CANVAS then   -- gGraph/gTable/gList/gTabs
--      sequence gtlattr = ctrl_xtra[id][CX_GTL_ATTRS]
        name = substitute(name,`COLOUR`,`COLOR`)
        integer lGTL = find(ct,gtl_types),
                gxdx = find(name,gtl_names[lGTL])
--?{`gxdx`,gxdx,name,`lGTL`,lGTL}
--?{`tg_set_canvas_attribute`,id,ct,name,gxdx}
        if gxdx then -- (else trigger std error handling)
            integer vtype = gtl_sigs[lGTL][gxdx]
            if vtype='B' then
--              if string(v) then v = tg_to_bool(v) end if
                v = tg_to_bool(v)
                assert(v=true or v=false)
            elsif vtype='C' then
                v = tg_get_colour_value(v)
                assert(atom(v))
            elsif vtype='I' then
                if string(v) then assert(is_integer(v))
                                  v = to_integer(v)
                             else assert(integer(v)) end if
                if ct=LIST and gxdx=CXLIST_CARET then
                    gdx timer = ctrl_xtra[id][CX_GTL_ATTRS][CXLIST_CARETIMER]
                    if timer=0 then
-- (sip... no need to create a timer?)
if v then -- (RUN)
                        timer = gTimer(cursorblink_action,400,v,id)
                        ctrl_xtra[id][CX_GTL_ATTRS][CXLIST_CARETIMER] = timer
end if
                    else
                        gSetAttribute(timer,`RUN`,v)
                    end if
                    ctrl_xtra[id][CX_GTL_ATTRS][CXLIST_CARETIME] = time()+0.375
                end if
            elsif vtype='N' then
                if string(v) then v = to_number(v) end if
                if ct=TREE and name=`BGSEL` and sequence(v) then
X                   atom {bgsel,fgsel,XselfulX} = v
--DEV (when hTree migrated here, also theGUI.js!)
?9/0
--                  ctrl_xtra[id][CX_GTL_ATTRS][CXTREE_FGSEL] = fgsel
--  X               ctrl_xtra[id][CX_GTL_ATTRS][CXTREE_SELFULL] = selful    X
                    v = bgsel
                end if
                assert(atom(v)) -- (fail `` yields {})
            elsif vtype='S' then
                assert(string(v))
            elsif name=`LEGENDXY` 
              and lGTL=1 then -- gGraph
                if string(v) then v = tg_intint(v) end if
                assert(sequence(v) and not string(v) and length(v)=2)
            elsif vtype='P' then
                assert(sequence(v))
            elsif vtype!='O' then
                ?9/0
            end if
            ctrl_xtra[id][CX_GTL_ATTRS][gxdx] = v
            if ct=TABS then
                if gxdx=CXTABS_TABCOUNT then
                    crash(`TABCOUNT is read only`)
                elsif gxdx=CXTABS_SHOWCLS 
                  and v = true
                  and ctrl_xtra[id][CX_GTL_ATTRS][CXTABS_STATIC]!=0 then
                    crash(`SHOWCLOSE:=true invalid on static tabitems`)
                end if
                if tg_mapped(id) then
                    gRedraw(id)
                end if
            end if
--          ctrl_xtra[id][CX_GTL_ATTRS] = 0 -- (kill refcount)
--          gtlattr[gxdx] = v
--          ctrl_xtra[id][CX_GTL_ATTRS] = gtlattr
            return true
        end if
    elsif name=`SPLIT` then -- (private/undocumented)
        assert(v=`AYE`)
        -- (just do both...)
        assert(and_bits(ctrl_flags[id],CF_SPLIT)=0)
        ctrl_flags[id] += CF_SPLIT
        gdx parent = parent_ids[id] -- nb cannot still be null!
        assert(ctrl_types[parent]==BOX)
        assert(length(children_ids[parent])==3)
        assert(and_bits(ctrl_flags[parent],CF_SPLIT)=0)
        ctrl_flags[parent] += CF_SPLIT
        return true
--*/
    end if
    return false
end function

global procedure hSetAttribute(gdx canvas, string name, object v)
    if name=`SCROLLSIZE` then
--?9/0
--      gSetAttribute(canvas,name,v)
--?{name,v}
        assert(tg_set_canvas_attribute(canvas, name, v, true))
    elsif name=`VIEWPORT` then
        integer {x,y} = v
        ctrl_xtra[canvas][CX_SBINFO][SB_ORIGX] = x
        ctrl_xtra[canvas][CX_SBINFO][SB_ORIGY] = y
    else
        ?9/0
    end if
end procedure

function tg_get_canvas_attribute(gdx id, string name, object dflt)
    -- nb id may not be mapped, unlike most other tg_get_xxx_attribute rouines...
--  atom handle = ctrl_handles[id]
    integer ct = ctrl_xtra[id][CX_CANVAS_TYPE]
--?{`tg_get_canvas_attribute`,id, name, dflt,{ct,CANVAS}}
    if name=`SCROLLFIX` then
        return ctrl_xtra[id][CX_SBINFO][SB_SCRFX]
    elsif name=`SCROLLSIZE`
       or name=`SCROLLPORT`
       or name=`VIEWPORT` then
--     or name=`SHOWDRAG` then
if dflt then end if
        integer w = 0, h = 0, x = 0, y = 0,
                hsv = false, hsh = 0,
                vsv = false, vsw = 0,
                {sw,sh} = gGetAttribute(id,`SIZE`)
        object sbinfo = ctrl_xtra[id][CX_SBINFO]
        if sequence(sbinfo) then
--?sbinfo
            w = sbinfo[SB_SCRLW]
            h = sbinfo[SB_SCRLH]
            x = sbinfo[SB_ORIGX]
            y = sbinfo[SB_ORIGY]
            hsv = sbinfo[SB_HVISB]
            hsh = sbinfo[SB_HHIGH]
            vsv = sbinfo[SB_VVISB]
            vsw = sbinfo[SB_VWIDE]
        end if
--      if name=`SCROLLDRAG` then
--          return sequence(sbinfo) and sbinfo[SB_SHOWG]
        if name=`SCROLLSIZE` then
            if w=0 then w = sw end if
            if h=0 then h = sh end if
            sbinfo = {w,h}
        else --if name=`SCROLLPORT` or name=`VIEWPORT` then
--DEV/ERM, for hTable.exw, seems to work fine w/o doing this (and not when doing it!) [19/9/25]
            sw -= vsv*vsw
            sh -= hsv*hsh
            sbinfo = {x,y,sw,sh}
        end if
        return sbinfo
--/*
    elsif name=`SPLIT` then -- (private/undocumented/once parent "known")
        return and_bits(ctrl_flags[id],CF_SPLIT)!=0
--      gdx parent = parent_ids[id]
--      assert(ctrl_types[parent]==BOX)
--      assert(length(children_ids[parent])==3)
--      return and_bits(ctrl_flags[parent],CF_SPLIT)!=0
--DEV now done in gGetAttribute (kill off CX_CANVASBACK/FORE...)
--  elsif name=`BGCLR` then
--      return ctrl_xtra[id][CX_CANVAS_BACK]
--  elsif name=`FGCLR` then
--      return ctrl_xtra[id][CX_CANVAS_FORE]
    elsif name=`LINEWIDTH` then
        return ctrl_xtra[id][CX_PENWIDTH]
    elsif name=`LINESTYLE` then
        integer style = ctrl_xtra[id][CX_PENSTYLE]
        return iff(style=TG_CUSTOM_DASH?dashers[style]:style)
    elsif name=`WU_BLEND` then
        return ctrl_xtra[id][CX_WU_BLEND]
    elsif ct!=CANVAS then -- gGraph/gTable/gList/gTabs
        name = substitute(name,`COLOUR`,`COLOR`)
        bool dPlus = (ct=TREE and name=`DATA+`) 
        if dPlus then name = `DATA` end if
        integer lGTL = find(ct,gtl_types),
                gxdx = find(name,gtl_names[lGTL])
        if ct=TABS and gxdx=CXTABS_TABCOUNT then
            rtn tabitems = gGetHandler(id,`TABITEMS`)
            assert(tabitems!=NULL) -- (won't ever be/error in *this* source file)
            string sig = tg_get_sig(tabitems)
            integer tabcount = iff(sig=`FOI`?tabitems(id,0)
                                            :tabitems(0))
            ctrl_xtra[id][CX_GTL_ATTRS][CXTABS_TABCOUNT] = tabcount
            return tabcount
        end if
        if gxdx then
            object v = ctrl_xtra[id][CX_GTL_ATTRS][gxdx]
            integer vtype = gtl_sigs[lGTL][gxdx]
            if vtype='B' then
                assert(v=true or v=false)
            elsif vtype='C' then
                assert(atom(v))
            elsif vtype='I' then
                assert(integer(v))
--              if ct=LIST and gxdx=CXLIST_CARET then
--                  if v then v = gGetAttribute(v,`RUN`) end if
--              end if
            elsif vtype='N' then
                assert(atom(v))
            elsif vtype='S' then
                if not string(v) then v = dflt end if
                assert(string(v))
            elsif name=`LEGENDXY` 
              and lGTL=1 then -- gGraph
                assert(sequence(v) and not string(v) and length(v)=2)
            elsif vtype!='O' then
                ?9/0
            end if
            if ct=LIST and name=`SELECTED` and sequence(v) then
                integer k = sum(v)
                if k=0 then
                    v = 0
                elsif k=1 then
                    v = find(1,v)
                end if
            elsif ct=TREE and name=`DATA` then
                ?9/0 --DEV (following shd be ready to go, once they actually exist:)
--              if dPlus then
--                  ctrl_xtra[id][CX_GTL_ATTRS][CX_TREE_DATA] = 0
--              else
--                  v = {v,{tg_get_vnc,
--                          tg_tree_n2d,
--                          tg_tree_d2n,
--                          tg_tree_collapse_node,
--                          tg_tree_expand_node,
--                          line_height}}
--              end if
            end if
            return v
        end if
--      if ct=LIST and name=`VALUESTR` then
----?{lGTL,name,`...`}
--          return `gGetAttribute(LIST,"VALUESTR")...`
--      end if
--*/
    end if
--  return tg_return_default_attr(id,name,dflt)
?9/0
end function

global function hGetIntInt(gdx canvas, string name)
    if name=`SCROLLSIZE` then
--DEV
--?9/0
--      return gGetIntInt(canvas,name)
        return tg_get_canvas_attribute(canvas, name, 0)
    end if
end function

global function hGetAttribute(gdx canvas, string name)
    if name=`VIEWPORT` then
--DEV
--?9/0
--      return gGetAttribute(canvas,name)
        return tg_get_canvas_attribute(canvas, name, 0)
--DEV for hTable, theGUI.e/js should instead use it directly
    elsif name=`SBINFO` then
--?`hGetAttribute, SBINFO`
        object sbinfo = ctrl_xtra[canvas][CX_SBINFO]
        if sequence(sbinfo) then
            integer hsv = sbinfo[SB_HVISB],
                    hsh = sbinfo[SB_HHIGH],
                    vsv = sbinfo[SB_VVISB],
                    vsw = sbinfo[SB_VWIDE]
            return {hsv*hsh,vsv*vsw}
        end if
        return {0,0}
    elsif name="SCROLLFIX" then
--DEV...
        return 0
    end if
end function

integer hll_redraw
integer hll_mousemove = NULL
integer hll_click = NULL
integer hll_selected = NULL

global procedure hSetHandler(gdx /*canvas*/, string name, integer rid)
    if name=`MOUSEMOVE` then
        hll_mousemove = rid
    elsif name=`CLICK` then
        hll_click = rid
    elsif name=`SELECTED` then
        hll_selected = rid
    else
        ?9/0
    end if
end procedure

-- only because gSetHandler is setting REDRAW to tg_redraw_canvas here:
--  (the real theGUI.e invokes said directly, /not/ via Get/SetHandler)
global function hGetHandler(gdx /*canvas*/, string name)
    if name=`REDRAW` then
        return hll_redraw
    elsif name=`SELECTED` then
        return hll_selected
    end if
end function

--/*
local procedure tg_track_mouseleave(gdx canvas, bool bTrackMouse)
    bool wasTrackMouse = ctrl_xtra[canvas][CX_TRACKLEAVE]
    bool bHovlist = ctrl_xtra[canvas][CX_CANVAS_TYPE]=LIST and 
                    ctrl_xtra[canvas][CX_GTL_ATTRS][CXLIST_HOVLINE]!=0
    if bHovlist then bTrackMouse := true end if
    ctrl_xtra[canvas][CX_TRACKLEAVE] = bTrackMouse
    if backend=TG_WINAPI then
        if bTrackMouse
        or wasTrackMouse then
            -- ask for a WM_MOUSELEAVE msg, when/if it actually does:
            atom dwFlags = iff(bTrackMouse?0:TME_CANCEL)+TME_LEAVE
            set_struct_field(idTRACKMOUSEEVENT,pTRACKMOUSEEVENT,`hwndTrack`,ctrl_handles[canvas])
            set_struct_field(idTRACKMOUSEEVENT,pTRACKMOUSEEVENT,`dwFlags`,dwFlags)
            bool bOK = c_func(xTrackMouseEvent,{pTRACKMOUSEEVENT})
            assert(bOK)
        end if
    elsif backend!=TG_GTK then
        -- gtk done via `leave-notify-event`/tg_gtk_mouseleave, for other backends:
        ?9/0
    end if
end procedure

local procedure tg_canvas_mouseleave(gdx canvas)
    -- common to WinAPI and GTK, invoked from tg_gtk_mouseleave/WM_MOUSELEAVE
    integer mouseleave = gGetHandler(canvas,`MOUSELEAVE`)
    if mouseleave then
        string sig = tg_get_sig(mouseleave)
        if sig=`PO` then
            mouseleave(canvas)
        elsif sig=`P` then
            mouseleave()
        else
            ?9/0 -- unknown sig...
        end if
    end if
    if ctrl_xtra[canvas][CX_CANVAS_TYPE]=LIST then
        ctrl_xtra[canvas][CX_GTL_ATTRS][CXLIST_HOVLINE] = 0
    end if
    ctrl_xtra[canvas][CX_TRACKLEAVE] = false
    gRedraw(canvas,0)
end procedure

local function tg_gtk_mouseleave(atom widget, event, gdx canvas)
    assert(canvas=tg_getID(widget))
    integer event_type = get_struct_field(idGdkEventCrossing,event,`event_type`)
    if event_type=GDK_LEAVE_NOTIFY
    and ctrl_xtra[canvas][CX_TRACKLEAVE] then
        tg_canvas_mouseleave(canvas)
    end if
    -- TRUE to stop other handlers from being invoked for the event. FALSE to propagate the event further.
    return false
end function

--*/

local function tg_get_mouse_on_scrollbar(gdx canvas, integer mx, my)
    -- return `` if not on a scrollbar, else a two char code
    sequence sbinfo = ctrl_xtra[canvas][CX_SBINFO]
    bool bDragging = sbinfo[SB_DRAGG]
    if not bDragging then
        integer {w, h} = gGetIntInt(canvas,`SIZE`),
                vsbvis = sbinfo[SB_VVISB],  sbwid = sbinfo[SB_VWIDE],
                hsbvis = sbinfo[SB_HVISB],  sbhgh = sbinfo[SB_HHIGH],
                sbvend = h-hsbvis*sbhgh, -- where vertical scrollbar ends
                sbhend = w-vsbvis*sbwid, -- where horizontal scrollbar ends
                vttop = sbinfo[SB_VTTOP],
                vtend = sbinfo[SB_VTEND],
                htlft = sbinfo[SB_HTLFT],
                htend = sbinfo[SB_HTEND]
--?{`tg_get_mouse_on_scrollbar`,mx,my,w,h}
--DEV not quite: eliminates looping when both, but not when only one...
--  (it might be the timer isn't stopping as it should when it hits the end??)
--  (or perhaps we should apply a MINSIZE such that arrows cannot overlap?)
--  bool bVert = vsbvis and mx>=w-sbwid and mx<=w,
--       bHorz = hsbvis and my>=h-sbhgh and my<=h
--  if bVert and ((not bHorz) or my<h-sbhgh) then
--  if vsbvis and mx>=w-sbwid and mx<=w then
        if vsbvis and mx>=w-sbwid and mx<w then
            if my>=0 and my<=sbhgh then     return `UA` -- uparrow
            elsif my<vttop then             return `AT` -- above thumb
            elsif my<=vtend then            return `VT` -- vertical thumb
            elsif my<sbvend-sbhgh then      return `BT` -- below thumb
            elsif my<=sbvend then           return `DA` -- downarrow
            end if
--  elsif bHorz and ((not bVert) or mx<w-sbwid) then
--  elsif hsbvis and my>=h-sbhgh and my<=h then
        elsif hsbvis and my>=h-sbhgh and my<h then
            if mx>=0 and mx<=sbwid then     return `LA` -- left arrow
            elsif mx<htlft then             return `LT` -- left of thumb
            elsif mx<=htend then            return `HT` -- horizontal thumb
            elsif mx<sbhend-sbwid then      return `RT` -- right of thumb
            elsif mx<=sbhend then           return `RA` -- right arrow
            end if
        end if
    end if
    return `` -- (not on any of the ten scrollbar regions)
end function

local constant integer SBC_BACK = #F0F0F0,
-- W10:
                     SBC_ARROWS = #606060, -- (inactive)
                     SBC_MONARB = #DADADA, -- mouse hovering on arrow background
                      SBC_THUMB = #CDCDCD, -- (inactive)
                  SBC_THUMBDRAG = #606060, -- thumb being dragged 
                   SBC_THUMBHOV = #A6A6A6, -- mouse hovering on a thumb
                  SBC_SCROLLHOV = #C0C0C0, -- hover off thumb
--                   SBC_RESIZE = #BFBFBF
-- W11:
                       SBC_THMB = #858585, -- thin/fat thumb
                       SBC_SOFT = #B8B8B8 -- (to soften thumb ends)

object ab_uldr = NULL -- ua,ub,la,lb,da,db,ra,rb (W11 scrollbar arrows)

--DEV use tg_XPM_from_shorthand(`scroll_arrows`) instead?
-- W11 scrollbar uparrow and a bold/hover version of it
local constant ua = `
10 10 9 1
. c #f0f0f0
! c #dcdcdc
# c #8f8f8f
$ c #ececec
% c #959595
& c #858585
' c #b2b2b2
( c #d2d2d2
) c #919191
..........
..........
...!##!...
..$%&&%$..
..'&&&&'..
.(&&&&&&(.
.)&&&&&&).
.%&&&&&&%.
..........
..........`
local constant ub = `
10 10 14 1
. c #f0f0f0
! c #efefef
# c #c7c7c7
$ c #a7a7a7
% c #666666
& c #cecece
' c #676767
( c #e8e8e8
) c #767676
* c #989898
+ c #c5c5c5
, c #aaaaaa
- c #e9e9e9
/ c #bcbcbc
..........
...!##!...
...$%%$...
..&'%%'&..
.()%%%%)(.
.*%%%%%%*.
+%%%%%%%%+
,%%%%%%%%,
-////////-
..........`

local function tg_create_scroll_arrows()
    sequence xpma = split(ua,'\n'),
             xpmb = split(ub,'\n'),
             res = {}
    for i=1 to 4 do
        if i>1 then -- create left/down/right by rotating the uparrows.
            xpma = deep_copy(xpma)
            xpma[-10..-1] = reverse(columnize(xpma[-10..-1],defval:=' '))
            xpmb = deep_copy(xpmb)
            xpmb[-10..-1] = reverse(columnize(xpmb[-10..-1],defval:=' '))
        end if
        res = append(res,xpma)
        res = append(res,xpmb)
    end for
    -- aside: all 10x10 simplifies both rotation and drawing
    return res -- ua,ub,la,lb,da,db,ra,rb
end function

local procedure tg_get_scroll_arrows()
--  if name=`scroll_arrows` then return tg_create_scroll_arrows() end if
--  ab_uldr = apply(tg_XPM_from_shorthand(`scroll_arrows`),gImage_from_XPM)
    ab_uldr = apply(tg_create_scroll_arrows(),gImage_from_XPM)
end procedure

--integer rsc = 0

--local --(made global for hTable.exw) 
global procedure tg_redraw_scrollbars(gdx canvas, bool draw=true)
--?`tg_redraw_scrollbars`
--rsc +=1 
--if rsc=6 then ?9/0 end if
    integer {w,h} = gGetIntInt(canvas,`SIZE`),
--      scrollfix = gGetInt(canvas,`SCROLLFIX`),
        scrollfix = hGetAttribute(canvas,`SCROLLFIX`),
--    {scbw,scbh} = gGetIntInt(canvas,`SCROLLSIZE`),
      {scbw,scbh} = hGetIntInt(canvas,`SCROLLSIZE`),
              W10 = platform()=WINDOWS and gVersion(-2)=10
--            W10 = true
--?{`tg_redraw_scrollbars`,{w,h},{scbw,scbh}}
    gSetAttribute(canvas,`LINESTYLE`,TG_CONTINUOUS)
    gSetAttribute(canvas,`LINEWIDTH`,1)
    integer {mx,my} = gGetAttribute(canvas,`MOUSEPOS`),
             x, y -- scratch vars

--?{`tg_redraw_scrollbars`,monsba,mx,my}
    sequence sbinfo = ctrl_xtra[canvas][CX_SBINFO]
--?{`tg_redraw_scrollbars`,sbinfo}
    integer sbwid = sbinfo[SB_VWIDE],
            sbhgh = sbinfo[SB_HHIGH],
            origx = sbinfo[SB_ORIGX],
            origy = sbinfo[SB_ORIGY]
    bool hsbvis = (scrollfix && 0b10) or w<scbw, -- (w/o vertical, for now...)
         vsbvis = (scrollfix && 0b01) or (h-(hsbvis*sbwid))<scbh,
         bDragging = sbinfo[SB_DRAGG],
--       bShowDrag = sbinfo[SB_SHOWG],
         bTrackMouse = false
    string monsba = tg_get_mouse_on_scrollbar(canvas,mx,my),
          clikmon = sbinfo[SB_CKMON]
    ctrl_xtra[canvas][CX_SBINFO] = NULL

    if vsbvis then
        hsbvis = (w-sbwid)<scbw -- (ok, /with/ vertical)
--      if hsbvis then
--/*
        if hsbvis and bShowDrag then
            -- draw both (later) and a corner resizer thumb (now)
            gSetAttribute(canvas,`FGCLR`,SBC_RESIZE)
            for i=1 to 3 do
                x = w-4
--                  y = h-i*3-1
                y = h-i*3
                for j=1 to i do
--DEV: I seem to have ended up drawing a 3x3 to get a 2x2 to appear....
--DEV: this may/shd be temporary... (resize doobrie)
if backend=TG_WINAPI then
--                  gDrawRect(canvas,x-1,y-1,x+1,y+1)
elsif backend=TG_GTK then
--                  gDrawRect(canvas,x,y,x,y)
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
--*/
        integer sbvend = h - hsbvis*sbhgh
        -- vertical thumb:
        integer vttop = sbwid,
                vtend = sbvend-sbwid,
                vtmax = sbvend-sbwid*2,
                vtlen = ceil((sbvend/scbh)*vtmax)
        if vtend<vttop then
            -- draw as single line in the middle then
            vttop = floor((vttop+vtend)/2)
--DEV this ain't quite right either... (end==top draws nowt, when it should be a line)
--          vtend = vttop
--          vtend = vttop+1
            vtlen = 1
        end if
        vttop += round((origy/scbh)*vtmax)
        vtend = vttop+vtlen
        -- (aside: arrow regions are square...)
        if draw then
            if W10 then
                gDrawRect(canvas,w-sbwid,0,w,h,true,colour:=SBC_BACK)
                integer uarrclr = SBC_ARROWS,
                        darrclr = SBC_ARROWS
                if monsba=`UA` then -- highlight uparrow background
                    gDrawRect(canvas,w-sbwid+1,0,w-1,sbwid,true,colour:=SBC_MONARB)
                    uarrclr = TG_BLACK
                    bTrackMouse = true
                elsif monsba=`DA` then -- highlight downarrow background
                    gDrawRect(canvas,w-sbwid+1,sbvend-sbwid,w-1,sbvend,true,colour:=SBC_MONARB)
                    darrclr = TG_BLACK
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
                    gDrawLine(canvas,xi,yut,xi,yub,colour:=uarrclr)
                    gDrawLine(canvas,xi,ydt,xi,ydb,colour:=darrclr)
                    if i then
                        xi = x+i
                        gDrawLine(canvas,xi,yut,xi,yub,colour:=uarrclr)
                        gDrawLine(canvas,xi,ydt,xi,ydb,colour:=darrclr)
                    end if
                end for
                integer thumbclr = SBC_THUMB
                if bDragging and clikmon = `VT` then
                    thumbclr = SBC_THUMBDRAG
                    bTrackMouse = true
                elsif monsba=`VT` then
                    thumbclr = SBC_THUMBHOV
                    bTrackMouse = true
                elsif monsba!=`` then
                    thumbclr = SBC_SCROLLHOV
                    bTrackMouse = true
                end if
                gDrawRect(canvas,w-sbwid+1,vttop,w-1,vtend,true,colour:=thumbclr)
            else -- W11
                gDrawRect(canvas,w-sbwid+1,0,w,h,true,colour:=SBC_BACK)
                bTrackMouse = iff(bDragging?clikmon=`VT`
                                           :find(monsba,{`UA`,`AT`,`VT`,`BT`,`DA`})!=0)
                if bTrackMouse then
                    -- draw the up and down arrows
                    if ab_uldr=NULL then tg_get_scroll_arrows() end if
                    atom ua = ab_uldr[1+(monsba=`UA`)],
                         da = ab_uldr[5+(monsba=`DA`)]
                    gDrawImage(ua,canvas,w-13,5)
                    gDrawImage(da,canvas,w-13,sbvend-13)
                end if
                -- draw the fat/thin thumb
                --DEV the real thing also has a fade in/out timer...
                if bTrackMouse then
                    gDrawRect(canvas,w-10,vttop+0,w-7,vtend-0,true,colour:=SBC_SOFT)
                    gDrawRect(canvas,w-11,vttop+1,w-6,vtend-1,true,colour:=SBC_SOFT)
                    gDrawRect(canvas,w-10,vttop+1,w-7,vtend-1,true,colour:=SBC_THMB)
                    gDrawRect(canvas,w-11,vttop+2,w-6,vtend-2,true,colour:=SBC_THMB)
                end if
                gDrawRect(canvas,w-9,vttop,w-8,vtend,true,colour:=SBC_THMB)
            end if -- W10/11
        end if -- draw
        sbinfo[SB_VTTOP] = vttop
        sbinfo[SB_VTEND] = vtend
    end if
    if hsbvis then
        integer sbhend = w - vsbvis*sbwid
        -- horizontal thumb:
        integer htlft = sbhgh,
                htend = sbhend-sbhgh,
                htmax = sbhend-sbhgh*2,
                htlen = ceil((sbhend/scbw)*htmax)
        if htend<htlft then
            -- draw as one line in the middle then
            htlft = floor((htlft+htend)/2)
--          htend = htlft
            htlen = 1
        end if
        htlft += round((origx/scbw)*htmax)
--DEV this overruns by 1 pixel, plus I think I actually want a (min) 1-pixel gap.
        htend = htlft+htlen
        if draw then
            if W10 then
                gDrawRect(canvas,0,h-sbhgh,sbhend,h,true,colour:=SBC_BACK)
                -- left and right arrows on the horizontal:
                integer larrclr = SBC_ARROWS,
                        rarrclr = SBC_ARROWS
                if monsba=`LA` then -- highlight leftarrow background
                    gDrawRect(canvas,0,h-sbhgh+1,sbhgh,h-1,true,colour:=SBC_MONARB)
                    larrclr = TG_BLACK
                    bTrackMouse = true
                elsif monsba=`RA` then -- highlight rightarrow background
                    gDrawRect(canvas,sbhend-sbhgh,h-sbhgh+1,sbhend,h-1,true,colour:=SBC_MONARB)
                    rarrclr = TG_BLACK
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
                    gDrawLine(canvas,xll,yi,xlr,yi,colour:=larrclr)
                    gDrawLine(canvas,xrl,yi,xrr,yi,colour:=rarrclr)
                    if i then
                        yi = y+i
                        gDrawLine(canvas,xll,yi,xlr,yi,colour:=larrclr)
                        gDrawLine(canvas,xrl,yi,xrr,yi,colour:=rarrclr)
                    end if
                end for
--              gDrawRect(canvas,htlft,h-sbhgh+1,htend,h-1,true,colour:=SBC_THUMB,fillcolour:=SBC_THUMB)
                integer thumbclr = SBC_THUMB
                if bDragging and clikmon = `HT` then
                    thumbclr = SBC_THUMBDRAG
                    bTrackMouse = true
                elsif monsba=`HT` then
                    thumbclr = SBC_THUMBHOV
                    bTrackMouse = true
                elsif monsba!=`` then
                    thumbclr = SBC_SCROLLHOV
                    bTrackMouse = true
                end if
                gDrawRect(canvas,htlft,h-sbhgh+1,htend,h-1,true,colour:=thumbclr)
            else -- W11
                gDrawRect(canvas,0,h-sbhgh+1,sbhend,h,true,colour:=SBC_BACK)
                bool wasTrackMouse = bTrackMouse
                bTrackMouse = iff(bDragging?clikmon=`HT`
                                           :find(monsba,{`LA`,`LT`,`HT`,`RT`,`RA`})!=0)
                if bTrackMouse then
                    -- draw the left and right arrows
                    if ab_uldr=NULL then tg_get_scroll_arrows() end if
                    atom la = ab_uldr[3+(monsba=`LA`)],
                         ra = ab_uldr[7+(monsba=`RA`)]
                    gDrawImage(la,canvas,5,h-13)
                    gDrawImage(ra,canvas,sbhend-13,h-13)
                end if
                -- draw the fat/thin thumb
                --DEV the real thing also has a fade in/out timer...
                if bTrackMouse then
                    gDrawRect(canvas,htlft+0,h-10,htend-0,h-7,true,colour:=SBC_SOFT)
                    gDrawRect(canvas,htlft+1,h-11,htend-1,h-6,true,colour:=SBC_SOFT)
                    gDrawRect(canvas,htlft+1,h-10,htend-1,h-7,true,colour:=SBC_THMB)
                    gDrawRect(canvas,htlft+2,h-11,htend-2,h-6,true,colour:=SBC_THMB)
                end if
                gDrawRect(canvas,htlft,h-9,htend,h-8,true,colour:=SBC_THMB)
                bTrackMouse ||= wasTrackMouse
            end if -- W10/11
        end if -- draw
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
--window.connect(`leave-notify-event`, window_exit, ``)
-- WinAPI-specific, note we're [still] relying on CX_TRACKLEAVE in theGUI.e 
-- and expect a js `mouseleave` event in theGUI.js to do roughly what gtk does...
-- hmm, even WinAPI seems fine...
--  ?`tg_track_mouseleave(canvas,bTrackMouse)`
end procedure

local procedure tg_redraw_canvas(gdx canvas)
--?`tg_redraw_canvas`
    -- (shared code between GTK, via tg_gtk_canvas_draw,
    --  and WinAPI, via tg_WinAPI_WndProc and WM_PAINT.)
    if and_bits(ctrl_xtra[canvas][CX_CANVAS_FLAGS],CXCF_SCROLL) then
        -- (nb: this makes hTable(nrows=2) show right first time)
        tg_redraw_scrollbars(canvas,FALSE) -- (just set sbinfo)
    end if
    atom back = gGetAttribute(canvas,`BGCLR`)
    integer {w, h} = gGetIntInt(canvas,`SIZE`)
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
    integer ct = ctrl_xtra[canvas][CX_CANVAS_TYPE],
--          redraw = gGetHandler(canvas,`REDRAW`)
            redraw = hGetHandler(canvas,`REDRAW`)
    if ct=GRAPH then
        ?`tg_redraw_graph(canvas,w,h)`
--DEV
--  elsif ct=TREE then
--      tg_redraw_tree(canvas,w,h)
    elsif redraw==NULL then
        crash(`gCanvas: MUST have a REDRAW procedure`)
    end if
    if redraw then
--      string sig = tg_get_sig(redraw)
        integer l = length(tg_get_sig(redraw))
--      if sig=`PO` then
        if l=2 then -- PO/PI
            redraw(canvas)
        elsif l=4 then -- POII/PIII
            redraw(canvas,w,h)
        elsif l=6 then -- POIIII/PIIIII
--          integer {ox,oy} = gGetIntInt(canvas,`VIEWPORT`)
--          integer {ox,oy} = hGetIntInt(canvas,`VIEWPORT`)
            integer {ox,oy} = hGetAttribute(canvas,`VIEWPORT`)
            redraw(canvas,w,h,ox,oy)
        else
            ?9/0
        end if
    end if
    if and_bits(ctrl_xtra[canvas][CX_CANVAS_FLAGS],CXCF_SCROLL) then
        -- aside: ^^ that flag may well have changed inside redraw()
        tg_redraw_scrollbars(canvas)
    end if
    gSetAttribute(canvas,`BGCLR`,back)
end procedure

-- (SUG: make global for hList left/right, once(/if ever) that actually includes/uses it...)
local function tg_scrollbar_key_handler(gdx canvas, integer c, bool ctrl, shift, alt)
--global function tg_scrollbar_key_handler(gdx canvas, integer c, bool ctrl, shift, alt)
--?`tg_scrollbar_key_handler`
--?9/0
    object sbinfo = ctrl_xtra[canvas][CX_SBINFO]
    if sequence(sbinfo) then
--? was 0 13/11/23...
--      object ltattr = ctrl_xtra[canvas][CX_GTL_ATTRS]
        integer {w, h} = gGetIntInt(canvas,`SIZE`), 
                pg = (c=VK_PGUP or c=VK_PGDN),
                ud = (c=VK_UP or c=VK_DOWN),
                sa = (shift or alt),
--              sa = (shift or alt or (ctrl_xtra[canvas][CX_CANVAS_TYPE]!=CANVAS)),
                ct = ctrl_xtra[canvas][CX_CANVAS_TYPE],
--ctrl_xtra[list][CX_GTL_ATTRS][CXLIST_LINEHT] = lh
                vpg = h-sbinfo[SB_HVISB]*sbinfo[SB_HHIGH], -- vertical page size
                hpg = w-sbinfo[SB_VVISB]*sbinfo[SB_VWIDE], -- horizontal page size
                origx = sbinfo[SB_ORIGX],
                origy = sbinfo[SB_ORIGY],
--DEV: note this implements up/downarrow scrolling on a gList(), but actually those
--      keys should alter the selected line, and only scroll when that hits top/btm.
--      (obvs. that key handling simply does not belong on a plain canvas/graph,
--            and a gTable has both that and a bit of left/right handling to do)
--              step1 = iff(ct=LIST and ud?ltattr[CXLIST_LINEHT]:
--                      iff(ct=TABLE and ud?ltattr[CXTBL_LINEHT]/*|24*/:1)),
                step1 = 1,  
                step = iff(pg?vpg
                             :iff(ctrl?iff(ud?vpg:hpg)
                                      :iff(sa?10:step1)))
        switch c do
            case VK_LEFT:           origx -= step
            case VK_RIGHT:          origx += step
            case VK_UP,VK_PGUP:     origy -= step
            case VK_DOWN,VK_PGDN:   origy += step
            case VK_HOME:           origy = 0; origx = 0
            case VK_END:            origy = sbinfo[SB_SCRLH]-vpg
                                    origx = sbinfo[SB_SCRLW]-hpg
            default: return TG_CONTINUE
        end switch
        sbinfo = 0 -- (kill refcount)
        ctrl_xtra[canvas][CX_SBINFO][SB_ORIGX] = origx
        ctrl_xtra[canvas][CX_SBINFO][SB_ORIGY] = origy
        gRedraw(canvas)
    end if
    return TG_CONTINUE
end function

-- aside: VT/HT missing from this set on purpose.
local constant {TMON,TKEY,TCTRL} = columnize({{`UA`,VK_UP,false},
                                              {`AT`,VK_UP,true},
                                              {`BT`,VK_DOWN,true},
                                              {`DA`,VK_DOWN,false},
                                              {`LA`,VK_LEFT,false},
                                              {`LT`,VK_LEFT,true},
                                              {`RT`,VK_RIGHT,true},
                                              {`RA`,VK_RIGHT,false}})

local procedure tg_scrollbar_timer_action(gdx canvas)
    -- for when a mouse button is pressed and held down on a scrollbar:
    object sbinfo = ctrl_xtra[canvas][CX_SBINFO]
    if sequence(sbinfo) then
        string clikmon = sbinfo[SB_CKMON]
        integer k = find(clikmon,TMON), 
               ox = sbinfo[SB_ORIGX], 
               oy = sbinfo[SB_ORIGY]
        sbinfo = NULL -- (kill refcount)
--DEV 2/12/24 k=0 and clikmon=`` here... (in xpEditor, window max'd, trying a mousewheel)
        bool bCtrl = TCTRL[k],
             bShift = gGetGlobal(`SHIFTKEY`),
             bAlt = gGetGlobal(`ALTKEY`)
        {} = tg_scrollbar_key_handler(canvas,TKEY[k],bCtrl,bShift,bAlt)
        gdx timer = ctrl_xtra[canvas][CX_SBINFO][SB_TIMER]
        if  ox==ctrl_xtra[canvas][CX_SBINFO][SB_ORIGX]
        and oy==ctrl_xtra[canvas][CX_SBINFO][SB_ORIGY] then     -- no change
            gSetAttribute(timer,`RUN`,false) -- switch off
        elsif timer then
            gSetAttribute(timer,`TIME`,100)
        end if
    end if
end procedure

local procedure tg_scrollbar_timer_cb(gdx timer)
    gdx canvas = gGetAttribute(timer,`USER_DATA`)
    tg_scrollbar_timer_action(canvas)
end procedure

--local -- (made global for hTable.exw) 
global function tg_scrollbar_click(gdx canvas, sequence status, integer x, y)
--?`tg_scrollbar_click`
--?9/0
--  integer {button,pressed,ctrl,shift,alt} = status
--  printf(1,"click(button:%c, pressed:%c, ctrl:%d, shift:%d, alt:%d, x:%d, y:%d)\n",
--           {button,pressed,ctrl,shift,alt,x,y})
    object sbinfo = ctrl_xtra[canvas][CX_SBINFO]
    if not sequence(sbinfo) then return false end if
    sbinfo = NULL
    string clikmon = tg_get_mouse_on_scrollbar(canvas,x,y)
    ctrl_xtra[canvas][CX_SBINFO][SB_CKMON] = clikmon
--?{`CLIKMON`,clikmon}
    ctrl_xtra[canvas][CX_SBINFO][SB_CLIKX] = x
    ctrl_xtra[canvas][CX_SBINFO][SB_CLIKY] = y
    bool dragg = status[2]!='R',
       wasdrag = ctrl_xtra[canvas][CX_SBINFO][SB_DRAGG]
    ctrl_xtra[canvas][CX_SBINFO][SB_DRAGG] = dragg
    gdx timer = ctrl_xtra[canvas][CX_SBINFO][SB_TIMER]
    if dragg then
        if not find(clikmon,{``,`VT`,`HT`}) then
            tg_scrollbar_timer_action(canvas)
            if not timer then
                timer = gTimer(tg_scrollbar_timer_cb,500,true,canvas)
                ctrl_xtra[canvas][CX_SBINFO][SB_TIMER] = timer
            else
                gSetAttribute(timer,`TIME`,500)
                gSetAttribute(timer,`RUN`,true)
            end if
            ctrl_xtra[canvas][CX_SBINFO][SB_DRAGG] = false
            dragg = false
        end if
    else
        if timer then gSetAttribute(timer,`RUN`,false) end if
    end if
    if hll_click and clikmon=`` then
--      integer l = length(tg_get_sig(hll_click))
        string sig = tg_get_sig(hll_click)
        integer l = length(sig)
        if l=3 then     -- POS
            hll_click(canvas, status)
        elsif l=5 then  -- POSII
            hll_click(canvas, status, x, y)
        else
            ?9/0
        end if
    end if
    if dragg!=wasdrag then
--NO...
--          tg_redraw_scrollbars(canvas)
--          atom handle = ctrl_handles[canvas]
--          bool bOK = c_func(xInvalidateRect,{handle,NULL,true})
--          assert(bOK)
        gRedraw(canvas)
    end if
--  end if
--DEV did I mean TG_IGNORE here? (or should this just be a procedure?)
--  return false
    return clikmon!=``
end function
--end procedure

--local function tg_scrollbar_mousemove(gdx canvas, integer x,y, bool left,/*middle*/,right)
--local procedure tg_scrollbar_mousemove(gdx canvas, integer x,y, bool left,middle,right)
local procedure tg_scrollbar_mousemove(gdx canvas, integer x,y, bool left,middle,right, ctrl,shift,alt)
--?{`tg_scrollbar_mousemove`,x,y}
--?9/0
    object sbinfo = ctrl_xtra[canvas][CX_SBINFO]
--  if not sequence(sbinfo) then return false end if
    if not sequence(sbinfo) then return end if
    string clikmon = sbinfo[SB_CKMON]
    bool dragg = sbinfo[SB_DRAGG]
--?{`dragg`,dragg}
-- 17/9/25 for hTable... (may want to be not TABLE? or hList/hTree depend on not setting MOUSEMOVE, whereas theEditor will want to block select, not scroll)
--  if dragg then
    if dragg and (hll_mousemove=NULL or clikmon!=``)then
        if left or right then
            integer origx = sbinfo[SB_ORIGX],
                    origy = sbinfo[SB_ORIGY],
                    clikx = sbinfo[SB_CLIKX],
                    cliky = sbinfo[SB_CLIKY]
            if clikmon=`` then
                origy += cliky-y
                cliky = y
                origx += clikx-x
                clikx = x
            else
                integer {w, h} = gGetIntInt(canvas,`SIZE`)
                if clikmon=`VT` then
                    integer vtmax = h - sbinfo[SB_HVISB]*sbinfo[SB_HHIGH]-sbinfo[SB_VWIDE]*2
                    origy += round((y-cliky)*sbinfo[SB_SCRLH]/vtmax)
                    cliky = y
                elsif clikmon=`HT` then
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
    else
--      string monsba = tg_get_mouse_on_scrollbar(canvas,x,y)
        clikmon = tg_get_mouse_on_scrollbar(canvas,x,y)
--      if monsba!=sbinfo[SB_MONLD] then -- (only if needed)
        if clikmon!=sbinfo[SB_MONLD] then -- (only if needed)
            sbinfo = NULL
            gRedraw(canvas)
        end if
--      if hll_mousemove then
--          hll_mousemove(canvas,x,y,left,middle,right,monsba!=``)
--      end if
    end if
    if hll_mousemove then
        sbinfo = NULL
--      hll_mousemove(canvas,x,y,left,middle,right,clikmon!=``)
        hll_mousemove(canvas,x,y,left,middle,right,ctrl,shift,alt,clikmon!=``)
    end if
--      return
--  end if
--  return monsba!=``
--end function
end procedure
--/*
--procedure tg_canvas_wheel_handler(gdx canvas, integer direction, bool ctrl, shift, alt)
----?{`tg_canvas_wheel_handler`,id, direction, ctrl, chift, alt}
--  integer key = iff(ctrl?iff(direction=-1?VK_RIGHT:VK_LEFT)
--                        :iff(direction=-1?VK_DOWN:VK_UP))
--  {} = tg_scrollbar_key_handler(canvas,key,false,shift,alt)
--end procedure

local procedure cursorblink_action(gdx cursorblink)
    gdx list = user_data[cursorblink]
    gRedraw(list)
end procedure
--*/

local procedure tg_mousewheel(gdx id, integer direction, bool ctrl, shift, alt)
    -- GTK and WinAPI part
    integer ct = ctrl_xtra[id][CX_CANVAS_TYPE]
--DEV surely not just CANVAS?? (sip) [^ be warned, hTable.exw sets a different one!]
    if ct=CANVAS and and_bits(ctrl_xtra[id][CX_CANVAS_FLAGS],CXCF_SCROLL) then
--      tg_canvas_wheel_handler
        integer key = iff(ctrl?iff(direction=-1?VK_RIGHT:VK_LEFT)
                              :iff(direction=-1?VK_DOWN:VK_UP))
        {} = tg_scrollbar_key_handler(id,key,false,shift,alt)
    end if
--/*
    do
        integer mwandler = gGetHandler(id,`MOUSEWHEEL`)
        if mwandler then
            mwandler(id,direction,ctrl,shift,alt)
        end if
-- maybe, maybe not...
--      id = gGetParent(id)
        id = tg_get_parent_id(id)
    until id=NULL
--*/
end procedure


--procedure mouse_wheel(gdx id, integer direction, bool ctrl, shift, alt)
--  ?{id,`MOUSEWHEEL`,direction, ctrl, shift, alt}
--  ?9/0
--end procedure

-- let's NOT paranormalise this!
global function hCanvas(rtn redraw)--, string attr, sequence args)
    -- just do we can  setup our local ctrl_xtra, etc:
--  gdx canvas = gCanvas(redraw)--,attr,args)
    hll_redraw = redraw
    gdx canvas = gCanvas(tg_redraw_canvas)
    if platform()=JS then
        -- fudge: astonishingly, this works fine despite the fact it does things like
        --        ctrl_xtra[canvas] all over the shop, as if it were an integer, when
        --        it is in fact an HtmlElement...
        ctrl_xtra = {}
    else
        ctrl_xtra = repeat(0,canvas)
    end if
    object cxi = repeat(0,CX_CANVAS_LEN)
    cxi[CX_CANVAS_TYPE] = CANVAS
--  cxi[CX_PENSTYLE] = TG_CONTINUOUS
--  cxi[CX_PENWIDTH] = 1
    ctrl_xtra[canvas] = cxi
--/*
    tg_register_handler(CANVAS,`MOUSEWHEEL`,{`POIIII`})
    tg_register_handler(CANVAS,`MOUSELEAVE`,{`PO`,`P`})
--*/
    gSetHandler(canvas,`KEY`,tg_scrollbar_key_handler)
    gSetHandler(canvas,`CLICK`,tg_scrollbar_click)
    gSetHandler(canvas,`MOUSEMOVE`,tg_scrollbar_mousemove)
    gSetHandler(canvas,`MOUSEWHEEL`,tg_mousewheel)
    return canvas
end function

