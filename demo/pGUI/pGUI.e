--
 -- pGUI.e
----------
-- =====
--
-- At last count (29/4/16) there are ~173 routines yet to be documented... 
--  (none of which are used in any of the demos, and some of which may get culled)
--           [all done to IupSbox, ~line 2061] DEV
--  (11/11/16 all done to IupSpin, ~line 2790)
--  (29/10/17 all done to IupColorbar, ~line 3309)
--  (22/11/17 all done to IupWebBrowser, ~line 3675)
--  (07/01/18 all done to IupDrawGetImageInfo, ~line 3843)
--  (02/05/18 all done to cdCanvasSimulate, ~line 5788)
--  (17/05/18 all done to cdCanvasMark, ~line 6169)
--  (09/08/19 all done to IupOleControl, ~line 8419)

--DEV removed from phix7zip.lst 15/12/19:
--demo\pGUI\win32\iupmatrixex.dll
--demo\pGUI\win64\iupmatrixex.dll

global type Ihandle(atom i)
    return i>0
end type

global type Ihandles(object o)
--19/6/19... (no help)
--if not object(o) then ?9/0 end if
    if atom(o) then return Ihandle(o) end if
    for i=1 to length(o) do
        if not Ihandle(o[i]) then return 0 end if
    end for
    return 1
end type

global type Ihandln(atom i)
    return i>=0
end type

global type Ihandlns(object o)
    if atom(o) then return Ihandln(o) end if
    for i=1 to length(o) do
        if not Ihandln(o[i]) then return 0 end if
    end for
    return 1
end type

sequence callbacks = {}
sequence cbnames = {},
         cbrids = {}

global type cbfunc(atom cb)
    --
    -- (aside: rfind() is used (instead of find) on the assumption that 
    --         nine cases out of ten will use the last one just created
    --         at least in the IupButton("ok",Icallback("ok_cb")) and 
    --         [hence] IupSetCallback() etc common use cases anyway.)
    --
    return cb=NULL or rfind(cb,callbacks)!=0
end type

global function Icallback(string name, integer rid = routine_id(name))
--  if rid<=0 then ?9/0 end if -- (call_back() does better than that anyway)
--4/2/21 (!!):
--  atom cb = call_back({'+', rid})
--  integer k = find(cb,callbacks)
    integer k = find(rid,cbrids)
    atom cb
    if k=0 then
        cb = call_back({'+', rid})
        callbacks = append(callbacks,cb)
        cbnames = append(cbnames,name)
        cbrids = append(cbrids,rid)
    else
        if cbnames[k]!=name then ?9/0 end if
        cb = callbacks[k]
    end if
    return cb
end function

--
-- 4/10/2020 to allow Icallback(action_cb) as well as Icallback("action_cb")
--           Note that pmain.e substitutes Icallback with Icallbacki, that
--           is instead of raising an error, and the signatures of both
--           Icallback and Icallbacki are effectively hard-coded there.
--           (There is no problem in calling this explicitly, of course.)
--
global function Icallbacki(integer rid)
    string name = get_routine_info(rid)[4]
    return Icallback(name,rid)
end function


global function iup_name_from_cb(atom cb)
    string name = cbnames[find(cb,callbacks)]
    return name
end function

function rid_from_cb(atom cb)
    integer rid = cbrids[find(cb,callbacks)]
    return rid
end function

global function iup_cb_from_name(string name)
    integer idx = find(name,cbnames)
    if idx=0 then return NULL end if
    if find(name,cbnames,idx+1)!=0 then ?9/0 end if
    atom cb = callbacks[idx]
    return cb
end function

--type non_null_atom(object o)
--  return atom(o) and o>NULL and o=floor(o)
--end type

--global type nullable_atom(object o)
--  return atom(o) and o>=NULL and o=floor(o)
--end type

include builtins/ptypes.e

--global type nullable_string(object o)
--  return string(o) or o=NULL
--end type

-- used by IupSetAttribute, IupSetGlobal, and cdCreateCanvas:
--global type atom_string(object o)
--  return string(o) 
--      or (integer(o) and o>=NULL) 
--      or (atom(o) and o>=NULL and o=floor(o))
--end type

--type boolean(object o)
--  return integer(o) and (o=0 or o=1)
--end type

type dword_seq(sequence s)  -- (technically qword_seq on 64-bit)
    return not string(s)
end type

-- erm, there is no chance of this ever working on any flavour of OE.
--ifdef not EU4_1 then
--global 
function sizeof(atom ctype)
    return and_bits(ctype, #FF)
end function
--end ifdef

-- (defaulted parameters version is for running the unit tests that follow)
--function paranormalise(object action=NULL, object func=NULL, sequence attributes="", dword_seq args={})
function paranormalise(object action, func, sequence attributes, dword_seq args) -- (see Technicalia below)
--
--  This routine implements a little trickery to make certain routines easier to use, for example the documentation contains
--
--      IupButton(string title, [[nullable_string action=NULL,] atom func=NULL,] string attributes="", sequence args={})
--
--  with the idea being that [action, func] are doubly-optional. I made up the term paranormalise as a jokey way to explain
--  that you have much more flexibility than normal in how you can provide the parameters. In short you can assume
--
--  1   IupButton(title) and
--  2   IupButton(title,action,func) [NB func is /not/ optional in this case] and
--  3   IupButton(title,func) and
--  4   IupButton(title,attributes[,args]) and
--  5   IupButton(title,func,attributes[,args]) and
--  6   IupButton(title,action,func,attributes[,args])
--
--  are all valid. Note that (unless using named parameters) you cannot change the order, eg:
--
--      IupButton(title,attributes[,args][,action],func)    -- !INVALID!
--      IupButton(title,func,attributes,args,action)        -- !Stop this madness!
--      IupButton(action,attributes,args,func,title)        -- !You're being silly now!
--
--  The action and func parameters are actually declared as object, attributes as sequence, and args as dword_seq,
--  but manually and thoroughly verified to be of the documented types, after they have all been repositioned.
--
--  There is an underlying assumption that if func is NULL then action is meaningless; what that specifically
--  means is IupButton(title,action,func) is ok, however IupButton(title,action) is not supported - it would 
--  in fact be misinterpreted as IupButton(title,attributes).
--
--  If any of this troubles you, just provide the full set of parameters, every time. Alternatively, break the 
--  line up into several discrete statements, ie/eg:
--
--      Ihandle button = IupButton(title)
--      IupSetCallback(button, action, func) 
--      IupSetAttributes(button, attributes[, args]) 
--      IupSetAttribute(button, name, v) 
--
-- Technicalia: The action name is partially defunct, or at least I've seen many things work happily without one.
--  It originated (I believe) from the LED roots of IUP, where you had to specify a unique name to an interface 
--  element and only then could you associate a handler to that name, whereas nowadays IupSetAttributeHandle can 
--  be used instead of an IupSetHandle/IupSetAttribute combo: although you can still specify an action name, it
--  suggests to me that a layer of indirection has been removed (sometime in the past two decades), or maybe it
--  has just been given a suitable default. It may only be so when there is only one handler, but some elements 
--  may have more than one, and of course in those cases you may still need a name to specify which (see the
--  documentation of the specific interface elements).
--
-- Since this is a private internal function, which is always called with all four parameters, the defaults
--  are omitted. Logically you can/should think of it as having the commented-out defaults, however they
--  are provided by and individually implemented by every callee, so duplicating that exact same handling 
--  here is just completely unnecessary overhead, that would only ever encourage/permit internal slip-ups.
--  (Obviously if there was any way this could be the one and only place that implements said defaulting,
--   I would gladly take it, but there ain't, so I can't. No biggie. Unit tests below now also removed.)
--
-- This routine is designed to crash on the slightest oddity. It may be posssible to crib fatalN (eg from   [DEV/SUG]
--  builtins\VM\pcallfunc.e) and use crash_message() to get an error reported on offending user code line.
--

    if not nullable_string(action) then
        -- assume action omitted (and at least one other parameter passed)
        if atom(action) then    -- (and, of course, action is not NULL here)
            -- assume p1[action] is really func (cases 3,5)
            if length(args) then ?9/0 end if
            if length(attributes) then
                -- (func,attributes,args)
                args = attributes
--              if not dword_seq(args) then ?9/0 end if -- (ple/autoverified)
                attributes = func           -- (forced typecheck)
--          elsif sequence(func) then   // replaced 27/9/2020 (when working on pGUI.e)
            elsif string(func) then
                -- (func,attributes)
                attributes = func           -- (typecheck avoided)
            elsif func!=NULL then
                -- (func,???)?
                ?9/0    -- something odd passed (atom!=NULL, atom!=NULL)
            end if
            if not string(attributes) then ?9/0 end if
            func = action
            if not cbfunc(func) then ?9/0 end if
            action = NULL
--          if not nullable_string(action) then ?9/0 end if -- (pointless)
        else
            -- assume p1 is attributes[, p2 is args]
            if length(attributes) or length(args) then ?9/0 end if
            if sequence(func) then
                args = func
--              if not dword_seq(args) then ?9/0 end if     -- (ple/autoverified)
            end if
            attributes = action
            if not string(attributes) then ?9/0 end if
            func = NULL
--          if not cbfunc(func) then ?9/0 end if            -- (pointless)
            action = NULL
--          if not nullable_string(action) then ?9/0 end if -- (pointless)
        end if
    elsif func=NULL then
        if length(attributes) then
            -- assume 3 or 4 parameters were passed (or named parameters were used)
            -- (the following all get done below anyway)
--          if not dword_seq(args) then ?9/0 end if
--          if not string(attributes) then ?9/0 end if
--          if not cbfunc(func) then ?9/0 end if
--          if not nullable_string(action) then ?9/0 end if
        elsif length(args)!=0 then
            ?9/0    -- something odd passed (attributes="", args!={})
        elsif action!=NULL then
            -- assume IupButton(title,attributes)
            if length(args) then ?9/0 end if
            attributes = action -- p3:=p1
            if not string(attributes) then ?9/0 end if
            action = NULL
        end if
    elsif sequence(func) then
        -- assume (attributes,args), ie func (p2) is actually args
        -- [it would be odd for args (in func) to be {} here in a static call, but things might be table-driven]
        -- first, quickly check that p3 and p4 were /not/ passed:
        if length(args) then ?9/0 end if
        if length(attributes) then ?9/0 end if
        -- then validate action,func as attributes,args:
        if length(action)=0 and length(func)!=0 then ?9/0 end if -- (odd, as above)
        attributes = action     -- p3:=p1
        if not string(attributes) then ?9/0 end if
        args = func             -- p4:=p2
--      if not dword_seq(args) then ?9/0 end if     -- (ple/autoverified)
        action = NULL
        func = NULL
--  else
        -- assume 3 or 4 parameters were passed (action,func,attributes[,args])
    end if
--  if not dword_seq(args) then ?9/0 end if         -- (ple/autoverified)
    if not string(attributes) then ?9/0 end if
    if not cbfunc(func) then ?9/0 end if
    if not nullable_string(action) then ?9/0 end if
    return {action,func,attributes,args}
end function

--tests:
--  1   IupButton(title)
--  2   IupButton(title,action,func) [NB func is /not/ optional in this case]
--  3   IupButton(title,func)
--  4   IupButton(title,attributes[,args])
--  5   IupButton(title,func,attributes[,args])
--  6   IupButton(title,action,func,attributes[,args])
--/* -- You would need to restore the default args for these unit tests to work:
function testcb() return 1 end function constant tcb = Icallback("testcb")
if paranormalise()!={NULL,NULL,"",{}} then ?9/0 end if                      -- 1
if paranormalise(NULL,tcb)!={NULL,tcb,"",{}} then ?9/0 end if               -- 2
if paranormalise("act",tcb)!={"act",tcb,"",{}} then ?9/0 end if             -- 2
if paranormalise(tcb)!={NULL,tcb,"",{}} then ?9/0 end if                    -- 3
if paranormalise("x")!={NULL,NULL,"x",{}} then ?9/0 end if                  -- 4
if paranormalise("x",{1})!={NULL,NULL,"x",{1}} then ?9/0 end if             -- 4
if paranormalise(tcb,"x")!={NULL,tcb,"x",{}} then ?9/0 end if               -- 5
if paranormalise(tcb,"x",{1})!={NULL,tcb,"x",{1}} then ?9/0 end if          -- 5
if paranormalise(NULL,tcb,"x")!={NULL,tcb,"x",{}} then ?9/0 end if          -- 6
if paranormalise(NULL,tcb,"x",{1})!={NULL,tcb,"x",{1}} then ?9/0 end if     -- 6
--named parameters:
if paranormalise(func:=tcb)!={NULL,tcb,"",{}} then ?9/0 end if
if paranormalise(attributes:="x")!={NULL,NULL,"x",{}} then ?9/0 end if
--*/

--DEV (make this a builtin)
global function rand_range(integer lo, integer hi)
    if lo>hi then {lo,hi} = {hi,lo} end if
    lo -= 1
    return lo+rand(hi-lo)
end function

constant W = machine_word()

-- (This is an example of an application-specific variation of OpenEuphoria's allocate_pointer_array)
--function iup_ptr_array(Ihandles pointers)
type atoms(object o)
    if atom(o) then return o!=NULL end if
    for i=1 to length(o) do
        if not atom(o[i]) or o[i]=NULL then return 0 end if
    end for
    return 1
end type

function iup_ptr_array(atoms pointers)
    sequence p0 = deep_copy(pointers) & 0
    atom pList = allocate(length(p0)*W)
    pokeN(pList, p0, W)
    return pList
end function

global function iup_peek_double(object pDouble)
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

--DEV fixme (failed attempt at the following, crashed in pilx86.e line 10509)
--procedure iup_poke_double(atom ptr, object ddata={})
procedure iup_poke_double(atom ptr, object ddata)
    if atom(ddata) then
        poke(ptr,atom_to_float64(ddata))
    else
        for i=1 to length(ddata) do
            poke(ptr+8*(i-1),atom_to_float64(ddata[i]))
        end for
    end if
end procedure
--DEV/fixme (xType=0 [pEmit2.e line 4247] on ddata (renamed from data, pls undo when done), when compiling simple_paint):
if "abc"="def" then
    iup_poke_double(0,1)
end if

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
    while length(strings)<n do
        pstr = peekNS(ptr,W,0)
        if pstr=NULL then exit end if
        strings = append(strings, peek_string(pstr))
--      if length(strings)=n then exit end if
        ptr += W
    end while
    return strings
end function

global function IupRawStringPtr(string s)
--
-- Returns a raw string pointer for s, somewhat like allocate_string(s), but using the existing memory.
-- NOTE: The return is only valid as long as the value passed as the parameter remains in scope.
--       In particular, callbacks must make a semi-permanent copy somewhere other than locals/temps.
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

--DEV should this allocate too? (spotted in passing)
procedure iup_poke_string_pointer_array(atom ptr, sequence strings)
    for i=1 to length(strings) do
        string si = strings[i]
--DEV (spotted in passing) one of these next two lines is certainly wrong anyway...
-- I suspect it should read IupRawStringPtr(si) - changed w/o any testing.
--      pokeN(ptr,si,W)
        pokeN(ptr,IupRawStringPtr(si),W)
        ptr += W
    end for
end procedure

--DEV from IUP docs:
--/*
The iup_isprint(key) macro informs if a key can be directly used as a printable character. 
The iup_isXkey(key) macro informs if a given key is an extended code. 
The iup_isShiftXkey(key) macro informs if a given key is an extended code using the Shift modifier, 
the iup_isCtrlXkey(key) macro for the Ctrl modifier, 
the iup_isAltXkey(key) macro for the Alt modifier, and 
the iup_isSysXkey(key) macro for the Sys modifier. 
To obtain a key code for a generic combination you can start with the base key from the table and combine it repeated times using the macros 
iup_XkeyShift(key), iup_XkeyCtrl(key), iup_XkeyAlt(key) and iup_XkeySys(key).
--*/


global function iup_isprint(atom c)
    return c>31 and c<127   -- K_SP..K_tidle
end function

/* from 32 to 126, all character sets are equal, the key code is the same as the ASCii character code. */

global function iup_isCtrlXkey(atom c)
    return and_bits(c, 0x20000000)!=0
end function

global function iup_isAltXkey(atom c)
    return and_bits(c, 0x40000000)!=0
end function

global function iup_isShiftXkey(atom c)
    return and_bits(c, 0x10000000)!=0
end function

global function iup_isSysXkey(atom c)
    return and_bits(c, 0x80000000)!=0
end function

global function iup_XkeyBase(atom c)
    return and_bits(c, 0x0FFFFFFF)
end function

global function iup_XkeyCtrl(atom c)
    return or_bits(c, 0x20000000)
end function

global function iup_XkeyAlt(atom c)
    return or_bits(c, 0x40000000)
end function

global function iup_XkeyShift(atom c)
    return or_bits(c, 0x10000000)
end function

--/*
global function iup_XkeySys( atom _c)
    return or_bits( _c, 0x80000000)
end function
--*/

global function alpha(atom color)
    color = and_bits(color, #FF000000)
    color = floor(color/#1000000)
    return 255-color
end function

global function red(atom color)
    return floor(and_bits(color, #FF0000)/#10000)
end function

global function green(atom color)
    return floor(and_bits(color, #FF00)/#100)
end function

global function blue(atom color)
    return remainder(color, #100)
end function

global function rgb(atom red, green, blue, alpha=0)
--  return and_bits(r,#FF) + and_bits(g,#FF)*#100 + and_bits(b,#FF)*#10000
--  return and_bits(r,#FF)*#10000 + and_bits(g,#FF)*#100 + and_bits(b,#FF)
    atom colour = and_bits(alpha,#FF)*#1000000 +
                  and_bits(red,  #FF)*#10000 + 
                  and_bits(green,#FF)*#100 + 
                  and_bits(blue, #FF)
    return colour
end function

global function to_rgb(atom colour)
    return {red(colour),green(colour),blue(colour),alpha(colour)}
end function    


global constant
    -- Common Flags and Return Values
    IUP_ERROR       = 1,
    IUP_NOERROR     = 0,
    IUP_OPENED      = -1,
    IUP_INVALID     = -1,
    IUP_INVALID_ID  = -10,
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
    IUP_GETPARAM_MAP     = -5,
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
    IUP_TOPPARENT    = 0xFFF9, /* 65529 */
    IUP_BOTTOMPARENT = 0xFFF8, /* 65528 */
    IUP_RIGHTPARENT  = IUP_BOTTOMPARENT,
    IUP_LEFTPARENT   = IUP_TOPPARENT,
    IUP_TOP          = IUP_LEFT,
    IUP_BOTTOM       = IUP_RIGHT,
    IUP_ANYWHERE     = IUP_CURRENT,

    IUP_MASK_UINT = "/d+",

    K_BS = '\b',            -- 8
    K_TAB = '\t',           -- 9
    K_LF = '\n',            -- 10 (0x0A)
    K_CR = '\r',            -- 13 (0x0D)
    K_SP = ' ',             -- 32 (0x20)
    K_exclam = '!',         -- 33 (0x21)
    K_quotedbl = '\"',      -- 34 (0x22)
    K_numbersign = '#',     -- 35 (0x23)
    K_dollar = '$',         -- 36 (0x24)
    K_percent = '%',        -- 37 (0x25)
    K_ampersand = '&',      -- 38 (0x26)
    K_apostrophe = '\'',    -- 39 (0x27)
    K_parentleft = '(',     -- 40 (0x28)
    K_parentright = ')',    -- 41 (0x29)
    K_asterisk = '*',       -- 42 (0x2A)
    K_plus = '+',           -- 43 (0x2B)
    K_comma = ',',          -- 44 (0x2C)
    K_minus = '-',          -- 45 (0x2D)
    K_period = '.',         -- 46 (0x2E)
    K_slash = '/',          -- 47 (0x2F)
    K_0 = '0',              -- 48 (0x30)
    K_1 = '1',              -- 49 (0x31)
    K_2 = '2',              -- 50 (0x32)
    K_3 = '3',              -- 51 (0x33)
    K_4 = '4',              -- 52 (0x34)
    K_5 = '5',              -- 53 (0x35)
    K_6 = '6',              -- 54 (0x36)
    K_7 = '7',              -- 55 (0x37)
    K_8 = '8',              -- 56 (0x38)
    K_9 = '9',              -- 57 (0x39)
    K_colon = ':',          -- 58 (0x3A)
    K_semicolon = ';',      -- 59 (0x3B)
    K_less = '<',           -- 60 (0x3C)
    K_equal = '=',          -- 61 (0x3D)
    K_greater = '>',        -- 62 (0x3E)
    K_question = '?',       -- 63 (0x3F)
    K_at = '@',             -- 64 (0x40)
    K_A = 'A',              -- 65 (0x41)
    K_B = 'B',              -- 66 (0x42)
    K_C = 'C',              -- 67 (0x43)
    K_D = 'D',              -- 68 (0x44)
    K_E = 'E',              -- 69 (0x45)
    K_F = 'F',              -- 70 (0x46)
    K_G = 'G',              -- 71 (0x47)
    K_H = 'H',              -- 72 (0x48)
    K_I = 'I',              -- 73 (0x49)
    K_J = 'J',              -- 74 (0x4A)
    K_K = 'K',              -- 75 (0x4B)
    K_L = 'L',              -- 76 (0x4C)
    K_M = 'M',              -- 77 (0x4D)
    K_N = 'N',              -- 78 (0x4E)
    K_O = 'O',              -- 79 (0x4F)
    K_P = 'P',              -- 80 (0x50)
    K_Q = 'Q',              -- 81 (0x51)
    K_R = 'R',              -- 82 (0x52)
    K_S = 'S',              -- 83 (0x53)
    K_T = 'T',              -- 84 (0x54)
    K_U = 'U',              -- 85 (0x55)
    K_V = 'V',              -- 86 (0x56)
    K_W = 'W',              -- 87 (0x57)
    K_X = 'X',              -- 88 (0x58)
    K_Y = 'Y',              -- 89 (0x59)
    K_Z = 'Z',              -- 90 (0x5A)
    K_bracketleft = '[',    -- 91 (0x5B)
    K_backslash = '\\',     -- 92 (0x5C)
    K_bracketright = ']',   -- 93 (0x5D)
    K_circum = '^',         -- 94 (0x5E)
    K_underscore = '_',     -- 95 (0x5F)
    K_grave = '`',          -- 96 (0x60)
    K_a = 'a',              -- 97 (0x61)
    K_b = 'b',              -- 98 (0x62)
    K_c = 'c',              -- 99 (0x63)
    K_d = 'd',              -- 100 (0x64)
    K_e = 'e',              -- 101 (0x65)
    K_f = 'f',              -- 102 (0x66)
    K_g = 'g',              -- 103 (0x67)
    K_h = 'h',              -- 104 (0x68)
    K_i = 'i',              -- 105 (0x69)
    K_j = 'j',              -- 106 (0x6A)
    K_k = 'k',              -- 107 (0x6B)
    K_l = 'l',              -- 108 (0x6C)
    K_m = 'm',              -- 109 (0x6D)
    K_n = 'n',              -- 110 (0x6E)
    K_o = 'o',              -- 111 (0x6F)
    K_p = 'p',              -- 112 (0x70)
    K_q = 'q',              -- 113 (0x71)
    K_r = 'r',              -- 114 (0x72)
    K_s = 's',              -- 115 (0x73)
    K_t = 't',              -- 116 (0x74)
    K_u = 'u',              -- 117 (0x75)
    K_v = 'v',              -- 118 (0x76)
    K_w = 'w',              -- 119 (0x77)
    K_x = 'x',              -- 120 (0x78)
    K_y = 'y',              -- 121 (0x79)
    K_z = 'z',              -- 122 (0x7A)
    K_braceleft = '{',      -- 123 (0x7B)
    K_bar = '|',            -- 124 (0x7C)
    K_braceright = '}',     -- 125 (0x7D)
    K_tilde = '~',          -- 126 (0x7E)

    K_MIDDLE = 0xFF0B,
    K_PAUSE  = 0xFF13,
    K_SCROLL = 0xFF14,
    K_ESC    = 0xFF1B,
    K_HOME   = 0xFF50,
    K_LEFT   = 0xFF51,
    K_UP     = 0xFF52,
    K_RIGHT  = 0xFF53,
    K_DOWN   = 0xFF54,
    K_PGUP   = 0xFF55,
    K_PGDN   = 0xFF56,
    K_END    = 0xFF57,
    K_Print  = 0xFF61,
    K_INS    = 0xFF63,
    K_Menu   = 0xFF67,
    K_NUM    = 0xFF7F,
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
    /* no Shift/Ctrl/Alt */
    K_LSHIFT = 0xFFE1,
    K_RSHIFT = 0xFFE2,
    K_LCTRL  = 0xFFE3,
    K_RCTRL  = 0xFFE4,
    K_LALT   = 0xFFE9,
    K_RALT   = 0xFFEA,
    K_CAPS   = 0xFFE5,
    K_DEL    = 0xFFFF,

--DEV: (need testing)
--/*
    K_sHOME = iup_XkeyShift(K_HOME),
    K_sUP = iup_XkeyShift(K_UP),
    K_sPGUP = iup_XkeyShift(K_PGUP),
    K_sLEFT = iup_XkeyShift(K_LEFT),
    K_sMIDDLE = iup_XkeyShift(K_MIDDLE),
    K_sRIGHT = iup_XkeyShift(K_RIGHT),
    K_sEND = iup_XkeyShift(K_END),
    K_sDOWN = iup_XkeyShift(K_DOWN),
    K_sPGDN = iup_XkeyShift(K_PGDN),
    K_sINS = iup_XkeyShift(K_INS),
    K_sDEL = iup_XkeyShift(K_DEL),
--*/
    K_sSP = iup_XkeyShift(K_SP),
--/*
    K_sTAB = iup_XkeyShift(K_TAB),
    K_sCR = iup_XkeyShift(K_CR),
    K_sBS = iup_XkeyShift(K_BS),
    K_sPAUSE = iup_XkeyShift(K_PAUSE),
    K_sESC = iup_XkeyShift(K_ESC),
    K_sF1 = iup_XkeyShift(K_F1),
    K_sF2 = iup_XkeyShift(K_F2),
    K_sF3 = iup_XkeyShift(K_F3),
    K_sF4 = iup_XkeyShift(K_F4),
    K_sF5 = iup_XkeyShift(K_F5),
    K_sF6 = iup_XkeyShift(K_F6),
    K_sF7 = iup_XkeyShift(K_F7),
    K_sF8 = iup_XkeyShift(K_F8),
    K_sF9 = iup_XkeyShift(K_F9),
    K_sF10 = iup_XkeyShift(K_F10),
    K_sF11 = iup_XkeyShift(K_F11),
    K_sF12 = iup_XkeyShift(K_F12),
    K_sPrint = iup_XkeyShift(K_Print),
    K_sMenu = iup_XkeyShift(K_Menu),
--*/
    K_cHOME = iup_XkeyCtrl(K_HOME),
--/*
    K_cUP = iup_XkeyCtrl(K_UP),
--*/
    K_cPGUP = iup_XkeyCtrl(K_PGUP),
--/*
    K_cLEFT = iup_XkeyCtrl(K_LEFT),
    K_cMIDDLE = iup_XkeyCtrl(K_MIDDLE),
    K_cRIGHT = iup_XkeyCtrl(K_RIGHT),
    K_csRIGHT = iup_XkeyShift(K_cRIGHT),
--*/
    K_cEND = iup_XkeyCtrl(K_END),
--/*
    K_cDOWN = iup_XkeyCtrl(K_DOWN),
--*/
    K_cPGDN = iup_XkeyCtrl(K_PGDN),
--/*
    K_cINS = iup_XkeyCtrl(K_INS),
    K_cDEL = iup_XkeyCtrl(K_DEL),
--*/
    K_cSP = iup_XkeyCtrl(' '),
    K_csSP = iup_XkeyShift(K_cSP),
--/*
    K_cTAB = iup_XkeyCtrl(K_TAB),
    K_cCR = iup_XkeyCtrl(K_CR),
    K_cBS = iup_XkeyCtrl(K_BS),
    K_cPAUSE = iup_XkeyCtrl(K_PAUSE),
    K_cESC = iup_XkeyCtrl(K_ESC),
    K_cCcedilla = iup_XkeyCtrl(K_Ccedilla),
--*/
    K_cF1 = iup_XkeyCtrl(K_F1),
    K_cF2 = iup_XkeyCtrl(K_F2),
    K_cF3 = iup_XkeyCtrl(K_F3),
    K_cF4 = iup_XkeyCtrl(K_F4),
    K_cF5 = iup_XkeyCtrl(K_F5),
    K_cF6 = iup_XkeyCtrl(K_F6),
    K_cF7 = iup_XkeyCtrl(K_F7),
    K_cF8 = iup_XkeyCtrl(K_F8),
    K_cF9 = iup_XkeyCtrl(K_F9),
    K_cF10 = iup_XkeyCtrl(K_F10),
    K_cF11 = iup_XkeyCtrl(K_F11),
    K_cF12 = iup_XkeyCtrl(K_F12),
--/*
    K_cPrint = iup_XkeyCtrl(K_Print),
    K_cMenu = iup_XkeyCtrl(K_Menu),
    K_mHOME = iup_XkeyAlt(K_HOME),
    K_mUP = iup_XkeyAlt(K_UP),
    K_mPGUP = iup_XkeyAlt(K_PGUP),
    K_mLEFT = iup_XkeyAlt(K_LEFT),
    K_mMIDDLE = iup_XkeyAlt(K_MIDDLE),
    K_mRIGHT = iup_XkeyAlt(K_RIGHT),
    K_cmRIGHT = iup_XkeyCtrl(K_mRIGHT),
    K_csmRIGHT = iup_XkeyShift(K_cmRIGHT),
    K_smRIGHT = iup_XkeyShift(K_mRIGHT),
    K_mEND = iup_XkeyAlt(K_END),
    K_mDOWN = iup_XkeyAlt(K_DOWN),
    K_mPGDN = iup_XkeyAlt(K_PGDN),
    K_mINS = iup_XkeyAlt(K_INS),
    K_mDEL = iup_XkeyAlt(K_DEL),
    K_mSP = iup_XkeyAlt(' '),
    K_mTAB = iup_XkeyAlt(K_TAB),
    K_mCR = iup_XkeyAlt(K_CR),
    K_mBS = iup_XkeyAlt(K_BS),
    K_mPAUSE = iup_XkeyAlt(K_PAUSE),
    K_mESC = iup_XkeyAlt(K_ESC),
    K_mCcedilla = iup_XkeyAlt(K_Ccedilla),
    K_mF1 = iup_XkeyAlt(K_F1),
    K_mF2 = iup_XkeyAlt(K_F2),
    K_mF3 = iup_XkeyAlt(K_F3),
    K_mF4 = iup_XkeyAlt(K_F4),
    K_mF5 = iup_XkeyAlt(K_F5),
    K_mF6 = iup_XkeyAlt(K_F6),
    K_mF7 = iup_XkeyAlt(K_F7),
    K_mF8 = iup_XkeyAlt(K_F8),
    K_mF9 = iup_XkeyAlt(K_F9),
    K_mF10 = iup_XkeyAlt(K_F10),
    K_mF11 = iup_XkeyAlt(K_F11),
    K_mF12 = iup_XkeyAlt(K_F12),
    K_mPrint = iup_XkeyAlt(K_Print),
    K_mMenu = iup_XkeyAlt(K_Menu),
    K_yHOME = iup_XkeySys(K_HOME),
    K_yUP = iup_XkeySys(K_UP),
    K_yPGUP = iup_XkeySys(K_PGUP),
    K_yLEFT = iup_XkeySys(K_LEFT),
    K_yMIDDLE = iup_XkeySys(K_MIDDLE),
    K_yRIGHT = iup_XkeySys(K_RIGHT),
    K_yEND = iup_XkeySys(K_END),
    K_yDOWN = iup_XkeySys(K_DOWN),
    K_yPGDN = iup_XkeySys(K_PGDN),
    K_yINS = iup_XkeySys(K_INS),
    K_yDEL = iup_XkeySys(K_DEL),
    K_ySP = iup_XkeySys(' '),
    K_yTAB = iup_XkeySys(K_TAB),
    K_yCR = iup_XkeySys(K_CR),
    K_yBS = iup_XkeySys(K_BS),
    K_yPAUSE = iup_XkeySys(K_PAUSE),
    K_yESC = iup_XkeySys(K_ESC),
    K_yCcedilla = iup_XkeySys(K_Ccedilla),
    K_yF1 = iup_XkeySys(K_F1),
    K_yF2 = iup_XkeySys(K_F2),
    K_yF3 = iup_XkeySys(K_F3),
    K_yF4 = iup_XkeySys(K_F4),
    K_yF5 = iup_XkeySys(K_F5),
    K_yF6 = iup_XkeySys(K_F6),
    K_yF7 = iup_XkeySys(K_F7),
    K_yF8 = iup_XkeySys(K_F8),
    K_yF9 = iup_XkeySys(K_F9),
    K_yF10 = iup_XkeySys(K_F10),
    K_yF11 = iup_XkeySys(K_F11),
    K_yF12 = iup_XkeySys(K_F12),
    K_yPrint = iup_XkeySys(K_Print),
    K_yMenu = iup_XkeySys(K_Menu),
    K_sPlus = iup_XkeyShift(K_plus),
    K_sComma = iup_XkeyShift(K_comma),
    K_sMinus = iup_XkeyShift(K_minus),
    K_sPeriod = iup_XkeyShift(K_period),
    K_sSlash = iup_XkeyShift(K_slash),
    K_sAsterisk = iup_XkeyShift(K_asterisk),
--K_acute (#B4), K_ccedilla (#E7), and K_diaeresis (#A8). 
--                                          
--*/
    K_cA = iup_XkeyCtrl('A'),
    K_cB = iup_XkeyCtrl('B'),
    K_cC = iup_XkeyCtrl('C'),
    K_cD = iup_XkeyCtrl('D'),
    K_cE = iup_XkeyCtrl('E'),
    K_cF = iup_XkeyCtrl('F'),
    K_cG = iup_XkeyCtrl('G'),
    K_cH = iup_XkeyCtrl('H'),
    K_cI = iup_XkeyCtrl('I'),
    K_cJ = iup_XkeyCtrl('J'),
    K_cK = iup_XkeyCtrl('K'),
    K_cL = iup_XkeyCtrl('L'),
    K_cM = iup_XkeyCtrl('M'),
    K_cN = iup_XkeyCtrl('N'),
    K_cO = iup_XkeyCtrl('O'),
    K_cP = iup_XkeyCtrl('P'),
    K_cQ = iup_XkeyCtrl('Q'),
    K_cR = iup_XkeyCtrl('R'),
    K_cS = iup_XkeyCtrl('S'),
    K_cT = iup_XkeyCtrl('T'),
    K_cU = iup_XkeyCtrl('U'),
    K_cV = iup_XkeyCtrl('V'),
    K_cW = iup_XkeyCtrl('W'),
    K_cX = iup_XkeyCtrl('X'),
    K_cY = iup_XkeyCtrl('Y'),
    K_cZ = iup_XkeyCtrl('Z'),
--/*
    K_c1 = iup_XkeyCtrl(K_1),
    K_c2 = iup_XkeyCtrl(K_2),
    K_c3 = iup_XkeyCtrl(K_3),
    K_c4 = iup_XkeyCtrl(K_4),
    K_c5 = iup_XkeyCtrl(K_5),
    K_c6 = iup_XkeyCtrl(K_6),
    K_c7 = iup_XkeyCtrl(K_7),
    K_c8 = iup_XkeyCtrl(K_8),
    K_c9 = iup_XkeyCtrl(K_9),
    K_c0 = iup_XkeyCtrl(K_0),
--*/
--  K_cPlus = iup_XkeyCtrl(K_plus)
    K_cPlus = iup_XkeyCtrl('+'),
--/*
    K_cComma = iup_XkeyCtrl(K_comma),
--*/
--  K_cMinus = iup_XkeyCtrl(K_minus)
    K_cMinus = iup_XkeyCtrl('-'),
--/*
    K_cPeriod = iup_XkeyCtrl(K_period),
    K_cSlash = iup_XkeyCtrl(K_slash),
    K_cSemicolon = iup_XkeyCtrl(K_semicolon),
--*/
--  K_cEqual = iup_XkeyCtrl(K_equal)
    K_cEqual = iup_XkeyCtrl('='),
--/*
    K_cBracketleft = iup_XkeyCtrl(K_bracketleft),
    K_cBracketright = iup_XkeyCtrl(K_bracketright),
    K_cBackslash = iup_XkeyCtrl(K_backslash),
    K_cAsterisk = iup_XkeyCtrl(K_asterisk),
--*/
    K_csA = iup_XkeyShift(K_cA),
    K_csB = iup_XkeyShift(K_cB),
    K_csC = iup_XkeyShift(K_cC),
    K_csD = iup_XkeyShift(K_cD),
    K_csE = iup_XkeyShift(K_cE),
    K_csF = iup_XkeyShift(K_cF),
    K_csG = iup_XkeyShift(K_cG),
    K_csH = iup_XkeyShift(K_cH),
    K_csI = iup_XkeyShift(K_cI),
    K_csJ = iup_XkeyShift(K_cJ),
    K_csK = iup_XkeyShift(K_cK),
    K_csL = iup_XkeyShift(K_cL),
    K_csM = iup_XkeyShift(K_cM),
    K_csN = iup_XkeyShift(K_cN),
    K_csO = iup_XkeyShift(K_cO),
    K_csP = iup_XkeyShift(K_cP),
    K_csQ = iup_XkeyShift(K_cQ),
    K_csR = iup_XkeyShift(K_cR),
    K_csS = iup_XkeyShift(K_cS),
    K_csT = iup_XkeyShift(K_cT),
    K_csU = iup_XkeyShift(K_cU),
    K_csV = iup_XkeyShift(K_cV),
    K_csW = iup_XkeyShift(K_cW),
    K_csX = iup_XkeyShift(K_cX),
    K_csY = iup_XkeyShift(K_cY),
    K_csZ = iup_XkeyShift(K_cZ),
    K_mA = iup_XkeyAlt('A'),
    K_mB = iup_XkeyAlt('B'),
    K_mC = iup_XkeyAlt('C'),
    K_mD = iup_XkeyAlt('D'),
    K_mE = iup_XkeyAlt('E'),
    K_mF = iup_XkeyAlt('F'),
    K_mG = iup_XkeyAlt('G'),
    K_mH = iup_XkeyAlt('H'),
    K_mI = iup_XkeyAlt('I'),
    K_mJ = iup_XkeyAlt('J'),
    K_mK = iup_XkeyAlt('K'),
    K_mL = iup_XkeyAlt('L'),
    K_mM = iup_XkeyAlt('M'),
    K_mN = iup_XkeyAlt('N'),
    K_mO = iup_XkeyAlt('O'),
    K_mP = iup_XkeyAlt('P'),
    K_mQ = iup_XkeyAlt('Q'),
    K_mR = iup_XkeyAlt('R'),
    K_mS = iup_XkeyAlt('S'),
    K_mT = iup_XkeyAlt('T'),
    K_mU = iup_XkeyAlt('U'),
    K_mV = iup_XkeyAlt('V'),
    K_mW = iup_XkeyAlt('W'),
    K_mX = iup_XkeyAlt('X'),
    K_mY = iup_XkeyAlt('Y'),
    K_mZ = iup_XkeyAlt('Z'),
    K_msA = iup_XkeyShift(K_mA),
    K_msB = iup_XkeyShift(K_mB),
    K_msC = iup_XkeyShift(K_mC),
    K_msD = iup_XkeyShift(K_mD),
    K_msE = iup_XkeyShift(K_mE),
    K_msF = iup_XkeyShift(K_mF),
    K_msG = iup_XkeyShift(K_mG),
    K_msH = iup_XkeyShift(K_mH),
    K_msI = iup_XkeyShift(K_mI),
    K_msJ = iup_XkeyShift(K_mJ),
    K_msK = iup_XkeyShift(K_mK),
    K_msL = iup_XkeyShift(K_mL),
    K_msM = iup_XkeyShift(K_mM),
    K_msN = iup_XkeyShift(K_mN),
    K_msO = iup_XkeyShift(K_mO),
    K_msP = iup_XkeyShift(K_mP),
    K_msQ = iup_XkeyShift(K_mQ),
    K_msR = iup_XkeyShift(K_mR),
    K_msS = iup_XkeyShift(K_mS),
    K_msT = iup_XkeyShift(K_mT),
    K_msU = iup_XkeyShift(K_mU),
    K_msV = iup_XkeyShift(K_mV),
    K_msW = iup_XkeyShift(K_mW),
    K_msX = iup_XkeyShift(K_mX),
    K_msY = iup_XkeyShift(K_mY),
    K_msZ = iup_XkeyShift(K_mZ),
--/*
    K_m1 = iup_XkeyAlt(K_1),
    K_m2 = iup_XkeyAlt(K_2),
    K_m3 = iup_XkeyAlt(K_3),
    K_m4 = iup_XkeyAlt(K_4),
    K_m5 = iup_XkeyAlt(K_5),
    K_m6 = iup_XkeyAlt(K_6),
    K_m7 = iup_XkeyAlt(K_7),
    K_m8 = iup_XkeyAlt(K_8),
    K_m9 = iup_XkeyAlt(K_9),
    K_m0 = iup_XkeyAlt(K_0),
    K_mPlus = iup_XkeyAlt(K_plus),
    K_mComma = iup_XkeyAlt(K_comma),
    K_mMinus = iup_XkeyAlt(K_minus),
    K_mPeriod = iup_XkeyAlt(K_period),
    K_mSlash = iup_XkeyAlt(K_slash),
    K_mSemicolon = iup_XkeyAlt(K_semicolon),
    K_mEqual = iup_XkeyAlt(K_equal),
    K_mBracketleft = iup_XkeyAlt(K_bracketleft),
    K_mBracketright = iup_XkeyAlt(K_bracketright),
    K_mBackslash = iup_XkeyAlt(K_backslash),
    K_mAsterisk = iup_XkeyAlt(K_asterisk),
    K_yA = iup_XkeySys(K_A),
    K_yB = iup_XkeySys(K_B),
    K_yC = iup_XkeySys(K_C),
    K_yD = iup_XkeySys(K_D),
    K_yE = iup_XkeySys(K_E),
    K_yF = iup_XkeySys(K_F),
    K_yG = iup_XkeySys(K_G),
    K_yH = iup_XkeySys(K_H),
    K_yI = iup_XkeySys(K_I),
    K_yJ = iup_XkeySys(K_J),
    K_yK = iup_XkeySys(K_K),
    K_yL = iup_XkeySys(K_L),
    K_yM = iup_XkeySys(K_M),
    K_yN = iup_XkeySys(K_N),
    K_yO = iup_XkeySys(K_O),
    K_yP = iup_XkeySys(K_P),
    K_yQ = iup_XkeySys(K_Q),
    K_yR = iup_XkeySys(K_R),
    K_yS = iup_XkeySys(K_S),
    K_yT = iup_XkeySys(K_T),
    K_yU = iup_XkeySys(K_U),
    K_yV = iup_XkeySys(K_V),
    K_yW = iup_XkeySys(K_W),
    K_yX = iup_XkeySys(K_X),
    K_yY = iup_XkeySys(K_Y),
    K_yZ = iup_XkeySys(K_Z),
    K_y1 = iup_XkeySys(K_1),
    K_y2 = iup_XkeySys(K_2),
    K_y3 = iup_XkeySys(K_3),
    K_y4 = iup_XkeySys(K_4),
    K_y5 = iup_XkeySys(K_5),
    K_y6 = iup_XkeySys(K_6),
    K_y7 = iup_XkeySys(K_7),
    K_y8 = iup_XkeySys(K_8),
    K_y9 = iup_XkeySys(K_9),
    K_y0 = iup_XkeySys(K_0),
    K_yPlus = iup_XkeySys(K_plus),
    K_yComma = iup_XkeySys(K_comma),
    K_yMinus = iup_XkeySys(K_minus),
    K_yPeriod = iup_XkeySys(K_period),
    K_ySlash = iup_XkeySys(K_slash),
    K_ySemicolon = iup_XkeySys(K_semicolon),
    K_yEqual = iup_XkeySys(K_equal),
    K_yBracketleft = iup_XkeySys(K_bracketleft),
    K_yBracketright = iup_XkeySys(K_bracketright),
    K_yBackslash = iup_XkeySys(K_backslash),
    K_yAsterisk = iup_XkeySys(K_asterisk),
--*/

    ACTION = "ACTION",
    ACTION_CB = "ACTION_CB",
    IUP_REDRAW = "REDRAW",
    IUP_CARET = "CARET",
    IUP_NUMLIN = "NUMLIN",
    IUP_NUMCOL = "NUMCOL",
    IUP_NUMLIN_VISIBLE = "NUMLIN_VISIBLE",
    IUP_NUMCOL_VISIBLE = "NUMCOL_VISIBLE",
    IUP_EXPAND = "EXPAND",
    IUP_SCROLLBAR = "SCROLLBAR",
    IUP_MARK_MODE = "MARK_MODE",
    IUP_MULTIPLE = "MULTIPLE",
    IUP_TITLE = "TITLE",
    IUP_MENU = "MENU",
    $

-- Record Input Modes
global enum
    IUP_RECBINARY = 0,
    IUP_RECTEXT,
    $

include builtins\VM\pcmdlnN.e       -- command_line()
include builtins\pgetpath.e         -- get_proper_dir()

procedure iup_link_error(sequence name)
    puts(1,"link error: "&name&"\n")
    ?9/0
end procedure

global -- for iup_layoutdlg.e
function iup_c_func(atom dll, sequence name, sequence args, atom result, boolean allow_fail=false)
    integer handle = define_c_func(dll, name, args, result)
    if handle = -1 then
        if allow_fail then
            handle = NULL
        else
            iup_link_error(name)
        end if
    end if
    return handle
end function

global -- for iup_layoutdlg.e
function iup_c_proc(atom dll, sequence name, sequence args)
    integer handle = define_c_proc(dll, name, args)
    if handle = -1 then iup_link_error(name) end if
    return handle
end function

--constant string curr_dir = current_dir()
--string curr_dir = current_dir()
constant integer libidx = iff(platform()=WINDOWS ? 1:
                          iff(platform()=LINUX   ? 2:
                                                   9/0))
constant sequence dirs = {"win","lnx"}
string dll_path
--global constant SLASH = iff(platform()=WINDOWS?'\\':'/')

function iup_open_dll(sequence libs)
string path = libs[libidx]
--?{"iup_open_dll",libs,dll_path,path}
atom res
    if platform()=WINDOWS then
        string curr_dir = current_dir()
--4/7/18:
--      if chdir(dll_path)=0 then ?9/0 end if
        if dll_path!="" and chdir(dll_path)=0 then ?9/0 end if
--      path = current_dir()&SLASH&path
        res = open_dll(path)
        if chdir(curr_dir)=0 then ?9/0 end if
    elsif platform()=LINUX then
--      for now, see demo/pGUI/lnx/installation.txt...
--      path = dll_path&path
        res = open_dll(path)
--no friggin help...
--      if res=0 then
--          if machine_bits()=64 then
--              res = open_dll("/usr/lib64/"&path)
--          else
--              res = open_dll("/usr/lib/"&path)
--          end if
--      end if
    end if
    if res=0 then iup_link_error(path) end if
    return res
end function

constant
         D  = C_DOUBLE, 
         F  = C_FLOAT,      -- NB: VM/pcfunc.e may not be up to this.. [edited 25/2/16]
         I  = C_INT,
         L  = C_LONG,
         P  = C_POINTER, 
         U  = C_UINT,
         UC = C_UCHAR,
         UL = C_ULONG,
         $

global -- for iup_layoutdlg.e
atom
    iup = NULL
atom
    xIupOpen,
    xIupClose,
--DEV these are documented in elements...
    xIupCreate,
    xIupCreatev,
    xIupDestroy,
    xIupMap,
    xIupUnmap,
--DEV attributes? (nah)
    xIupVersion,
    xIupVersionDate,
    xIupVersionNumber,
    xIupVersionShow,

    xIupMainLoop,
    xIupMainLoopLevel,
    xIupLoopStep,
    xIupLoopStepWait,
    xIupExitLoop,
    xIupPostMessage,
    xIupFlush,
    xIupRecordInput,
    xIupPlayInput,
--  xIupGetActionName,

    xIupSetAttribute,
     xIupSetAttributeId,
     xIupSetAttributeId2,
     xIupSetStrAttribute,
      xIupSetStrAttributeId,
      xIupSetStrAttributeId2,
      xIupSetInt,
      xIupSetIntId,
      xIupSetIntId2,
      xIupSetFloat,
      xIupSetFloatId,
      xIupSetFloatId2,
      xIupSetDouble,
      xIupSetDoubleId,
      xIupSetDoubleId2,
      xIupSetRGB,
      xIupSetRGBId,
      xIupSetRGBId2,
      xIupStoreAttribute,
     xIupSetAttributeHandle,
     xIupSetAttributeHandleId,
     xIupSetAttributeHandleId2,
      xIupSetHandle,
    xIupSetAttributes,
    xIupResetAttribute,
    xIupGetAttribute,
     xIupGetAttributeId,
     xIupGetAttributeId2,
--   xIupGetAttributes, --(deprecated)
     xIupGetAllAttributes,
     xIupGetAttributeHandle,
     xIupGetAttributeHandleId,
     xIupGetAttributeHandleId2,
      xIupGetHandle,
     xIupGetInt,
     xIupGetInt2,
     xIupGetIntInt,
     xIupGetIntId,
     xIupGetIntId2,
     xIupGetFloat,
     xIupGetFloatId,
     xIupGetFloatId2,
     xIupGetDouble,
     xIupGetDoubleId,
     xIupGetDoubleId2,
     xIupGetRGB,
     xIupGetRGBId,
     xIupGetRGBId2,
    xIupSetGlobal,
     xIupSetStrGlobal,
     xIupStoreGlobal,
    xIupGetGlobal,
     xIupSetLanguage,
     xIupGetLanguage,
     xIupSetLanguagePack,
     xIupSetLanguageString,
     xIupStoreLanguageString,
     xIupGetLanguageString,
    xIupSetCallback,
     xIupGetCallback,
    xIupGetClassName,
     xIupGetAllClasses,
     xIupGetClassType,
     xIupGetClassAttributes,
     xIupGetClassCallbacks,
     xIupSaveClassAttributes,
     xIupCopyAttributes,
     xIupCopyClassAttributes,
     xIupSetClassDfltAttribute,

    xIupFill,
    xIupSpace,
    xIupHboxv,
    xIupVboxv,
    xIupZboxv,
    xIupRadio,
    xIupNormalizerv,
    xiupObjectCheck,
    xIupCboxv,
    xIupSbox,
    xIupSplit,
    xIupGridBoxv,
    xIupMultiBoxv,

    xIupAppend,
    xIupDetach,
    xIupInsert,
    xIupReparent,
    xIupGetParent,
    xIupGetChild,
    xIupGetChildPos,
    xIupGetChildCount,
    xIupGetNextChild,
    xIupGetBrother,
    xIupGetDialog,
    xIupGetDialogChild,

    xIupRefresh,
    xIupRefreshChildren,
    xIupUpdate,
    xIupUpdateChildren,
    xIupRedraw,
    xIupConvertXYToPos,

    xIupDialog,
    xIupPopup,
    xIupShow,
    xIupShowXY,
    xIupHide,

    xIupAlarm,
    xIupMessage,
    xIupMessageError,
    xIupMessageAlarm,
    xIupMessageDlg,
    xIupCalendar,
    xIupColorDlg,
    xIupDatePick,
    xIupFileDlg,
    xIupFontDlg,
    xIupGetColor,
    xIupGetFile,
    xIupGetParamv,
    xIupGetText,
    xIupListDialog,
    xIupLayoutDialog,
    xIupGlobalsDialog,
    xIupProgressDlg,

    xIupButton,
    xIupCanvas,
    xIupFrame,
    xIupFlatFrame,
    xIupLabel,
    xIupFlatLabel,
    xIupFlatList,
    xIupFlatSeparator,
    xIupList,
    xIupProgressBar,
    xIupGauge,
    xIupSpin,
    xIupSpinbox,
    xIupTabsv,
    xIupFlatTabsv,
    xIupText,
    xIupMultiLine,
    xIupTextConvertLinColToPos,
    xIupTextConvertPosToLinCol,
    xIupToggle,
    xIupFlatToggle,
    xIupTree,
    xIupFlatTree,
    xIupVal,
    xIupFlatVal,

    xIupConfig,
    xIupConfigCopy,
    xIupConfigLoad,
    xIupConfigSave,
    xIupConfigSetVariableInt,
    xIupConfigSetVariableIntId,
    xIupConfigSetVariableDouble,
    xIupConfigSetVariableDoubleId,
    xIupConfigSetVariableStr,
    xIupConfigSetVariableStrId,
--  xIupConfigGetVariableInt,
    xIupConfigGetVariableIntDef,
--  xIupConfigGetVariableIntId,
    xIupConfigGetVariableIntIdDef,
--  xIupConfigGetVariableDouble,
    xIupConfigGetVariableDoubleDef,
--  xIupConfigGetVariableDoubleId,
    xIupConfigGetVariableDoubleIdDef,
--  xIupConfigGetVariableStr,
    xIupConfigGetVariableStrDef,
--  xIupConfigGetVariableStrId,
    xIupConfigGetVariableStrIdDef,
    xIupConfigRecentInit,
    xIupConfigRecentUpdate,
    xIupConfigDialogShow,
    xIupConfigDialogClosed,
    xIupExecute,
    xIupExecuteWait,
    xIupLog,

    xIupPreviousField,
    xIupNextField,
    xIupSetFocus,
    xIupGetFocus,

    xIupMenuv,
    xIupItem,
    xIupSeparator,
    xIupSubmenu,

--  xIupSetHandle,
--  xIupGetHandle,
    xIupGetName,
    xIupGetAllNames,
    xIupGetAllDialogs,

    xIupClipboard,
    xIupTimer,
    xIupUser,
    xIupHelp,

    iupControls,
    xIupControlsOpen,
    
    xIupCells,
    xIupColorbar,       -- moved to main lib in 3.24
    xIupColorBrowser,   -- moved to main lib in 3.24
    xIupDial,           -- moved to main lib in 3.24
    xIupMatrix,
    xIupMatrixEx,
    xIupMatrixList,
--  xIupMatSetAttribute,
--  xIupMatStoreAttribute,
--  xIupMatGetAttribute,
--  xIupMatGetInt,
--  xIupMatGetFloat,

    xIupLoad,
    xIupLoadBuffer,
--  xIupSetLanguage,
--  xIupGetLanguage,
--  xIupSetLanguageString,
--  xIupStoreLanguageString,
--  xIupGetLanguageString,
--  xIupSetLanguagePack,
    xIupGetFunction,
    xIupSetFunction,
    xIupClassMatch,
    xIupScrollBox,
    xIupFlatScrollBox,
    xIupExpander,
    xIupDetachBox,
    xIupBackgroundBox,
    xIupLink,
    xIupFlatButton,
    xIupDropButton,
    xIupStoreAttributeId,
    xIupStoreAttributeId2,
    xIupTreeSetUserId,
    xIupTreeGetUserId,
    xIupTreeGetId,
--  xIupTreeSetAttributeHandle,
--  xIupTreeSetAttribute,
--  xIupTreeStoreAttribute,
--  xIupTreeGetAttribute,
--  xIupTreeGetInt,
--  xIupTreeGetFloat,
--  xIupMapFont,
--  xIupUnMapFont,
--  xIupParamf,
--  xIupParamBox,
    xIupElementPropertiesDialog,
    xIupClassInfoDialog,
    xiupKeyCodeToName,

    $

global function IupCheckVCRuntime(bool bCrash=true)
    if platform()!=WINDOWS then ?9/0 end if -- sanity check
--  string runtime = msvcr120.dll
    -- aside: don't ask me why 64bit apparently needs the "_1"...
    string runtime = iff(machine_bits()=32?"VCRUNTIME140.DLL","VCRUNTIME140_1.DLL")
    if open_dll(runtime)=0 then
        printf(1,"fatal error: %s could not be loaded\n",{runtime})
        if not bCrash then
            -- (maintain this one place as a single source of truth:)
            return {"https://aka.ms/vs/16/release/VC_redist.x86.exe",
                    "https://aka.ms/vs/16/release/VC_redist.x64.exe"} 
        end if
--      puts(1," try installing Visual C++ Redistributable Packages for Visual Studio 2013\n")
--      puts(1," from ht--tps://www.microsoft.com/en-us/download/details.aspx?id=40784 \n")
--      -- ( ht--tps://www.microsoft.com/en-us/download/details.aspx?id=40784 )
        puts(1," try installing Visual C++ Redistributable Packages for Visual Studio 2015..19\n")
        -- aside: these *are* the genuine official urls from Microsoft, btw...
        sequence mo = iff(machine_bits()=32?{"x86","x64"}:{"x64","x86"})
        printf(1," from https://aka.ms/vs/16/release/VC_redist.%s.exe\n"&
                 " [and maybe https://aka.ms/vs/16/release/VC_redist.%s.exe]\n",mo)
        -- ( https://aka.ms/vs/16/release/VC_redist.x86.exe )
        -- ( https://aka.ms/vs/16/release/VC_redist.x64.exe )
--or https://support.microsoft.com/en-us/help/2977003/the-latest-supported-visual-c-downloads
        {} = wait_key()
        ?9/0
    end if
    return {}
end function

--DEV/SUG use this: sequence s = include_paths()
-- Note: pGUI.e must reside in a directory named pGUI; should you require a private copy
--        either to ensure that updates do not break anything, or you want users to be
--        able to run from source without installing Phix[??], do not place pGUI.e in
--        myproject/ but in myproject/pGUI/ along with whatever else you need, such as
--        and specifically the win32/win64/lnx32/lnx64 subfolders (opengl.e and glu.e 
--        are the only other things that I can think of).

procedure iup_init1(nullable_string dll_root)
--if 0 then
--string dll_abs -- (dll_root may be a relative path)
--  if dll_root=NULL then
--      dll_abs = get_proper_dir(command_line()[2])
--  else
--      dll_abs = get_proper_dir(dll_root)
--  end if
----    dll_path = dll_root&sprintf(`\%s%d\`,{dirs[libidx],machine_bits()})
--  dll_path = dll_abs&sprintf("%s%d%s",{dirs[libidx],machine_bits(),SLASH})
--  if get_file_type(dll_path)!=FILETYPE_DIRECTORY then
--      if dll_root=NULL then ?9/0 end if
--      dll_abs = get_proper_dir(get_proper_dir(command_line()[2])&dll_root)
--      dll_path = dll_abs&sprintf("%s%d%s",{dirs[libidx],machine_bits(),SLASH})
--      if get_file_type(dll_path)!=FILETYPE_DIRECTORY then ?9/0 end if
--  end if
--else
    sequence s = include_paths()
    string dll_dir = sprintf("%s%d",{dirs[libidx],machine_bits()})
--?{"iup_init1",s,dll_dir}
--  string dll_dir = sprintf("%s%d",{dirs[libidx],machine_bits()})&"z"
    for i=1 to length(s) do
        sequence sip = split_path(s[i])
        if sip[$]="pGUI" then
--          dll_path = s[i]&sprintf("%s%d%s",{dirs[libidx],machine_bits(),SLASH})
--          dll_path = join_path({s[i],sprintf("%s%d",{dirs[libidx],machine_bits()})},1)
--          sip = append(sip,sprintf("%s%d",{dirs[libidx],machine_bits()}))
            sip = append(sip,dll_dir)
            dll_path = join_path(sip,trailsep:=1)
--dll_root = dll_path
            if get_file_type(dll_path)=FILETYPE_DIRECTORY then
                s = {}
                exit
            end if
        end if
    end for
    if s!={} then
        -- allow pGUI in the app dir, with a win/lnx,32/64 sub-dir:
        for i=1 to length(s) do
--4/5/18...
--          string dll_path = join_path({s[i],dll_dir})
            dll_path = join_path({s[i],dll_dir})
            if get_file_type(dll_path)=FILETYPE_DIRECTORY then
                s = {}
                exit
            end if
        end for
    end if
    if s!={} then
--4/5/18...
--      string dll_path = join_path({dll_root,dll_dir})
--9/1/19:
--      dll_path = join_path({dll_root,dll_dir})
        if dll_root=NULL then
            dll_path = dll_dir
        else
            dll_path = join_path({dll_root,dll_dir})
        end if
        if get_file_type(dll_path)=FILETYPE_DIRECTORY then
            s = {}
        end if
    end if
    if s!={} then
--4/7/18:
--if platform()=WINDOWS then    -- DEV (while manually installed on lnx)
--      printf(1,"unable to locate %s directory\n",dll_dir)
--      {} = wait_key()
--      ?9/0
--end if
        dll_path = ""
    end if
        
--end if

    if platform()=WINDOWS then
        -- Aside: normally I'd expect msvcr120.dll to be loaded from system32/syswow64, 
        --        but if someone puts copies in pGUI\win32|64, it should be alright.
        --        You could also try deleting this test and see if it works anyway, but
        --        don't blame me if that gets you an even more cryptic error message.
        --        (This all depends on how the pre-built binaries were built, natch.)
        string curr_dir = current_dir()
--4/7/18:
--      if chdir(dll_path)=0 then ?9/0 end if
        if dll_path!="" and chdir(dll_path)=0 then ?9/0 end if
        {} = IupCheckVCRuntime()
        if chdir(curr_dir)=0 then ?9/0 end if
    end if

    iup = iup_open_dll({"iup.dll",
                        "libiup.so",
                        "libiup.dylib"})

    -- Control
    xIupOpen            = iup_c_func(iup, "IupOpen", {I,P}, I)
    xIupClose           = iup_c_proc(iup, "IupClose", {})
--DEV these are documented in elements...
    xIupCreate          = iup_c_func(iup, "IupCreate", {P},P)
    xIupCreatev         = iup_c_func(iup, "IupCreatev", {P,P},P)
    xIupDestroy         = iup_c_proc(iup, "IupDestroy", {P})
    xIupMap             = iup_c_func(iup, "IupMap", {P},I)
    xIupUnmap           = iup_c_proc(iup, "IupUnmap", {P})
--DEV attributes? (nah)
    xIupVersion         = iup_c_func(iup, "IupVersion", {}, P)
    xIupVersionDate     = iup_c_func(iup, "IupVersionDate", {}, P)
    xIupVersionNumber   = iup_c_func(iup, "IupVersionNumber", {}, I)
    xIupVersionShow     = iup_c_proc(iup, "IupVersionShow", {})

    xIupMainLoop        = iup_c_proc(iup, "IupMainLoop", {})
    xIupMainLoopLevel   = iup_c_func(iup, "IupMainLoopLevel", {}, I)
    xIupLoopStep        = iup_c_func(iup, "IupLoopStep", {}, I)
    xIupLoopStepWait    = iup_c_func(iup, "IupLoopStepWait", {}, I)
    xIupExitLoop        = iup_c_proc(iup, "IupExitLoop", {})
    xIupPostMessage     = iup_c_proc(iup, "IupPostMessage", {P,P,I,D,P})
    xIupFlush           = iup_c_proc(iup, "IupFlush", {})
    xIupRecordInput     = iup_c_func(iup, "IupRecordInput", {P,I}, I)
    xIupPlayInput       = iup_c_proc(iup, "IupPlayInput", {P})
--  xIupGetActionName   = iup_c_func(iup, "IupGetActionName", {}, P)

    xIupSetAttribute                = iup_c_proc(iup, "IupSetAttribute", {P,P,P})
     xIupSetAttributeId             = iup_c_proc(iup, "IupSetAttributeId", {P,P,I,P})
     xIupSetAttributeId2            = iup_c_proc(iup, "IupSetAttributeId2", {P,P,I,I,P})
     xIupSetStrAttribute            = iup_c_proc(iup, "IupSetStrAttribute", {P,P,P})
      xIupSetStrAttributeId         = iup_c_proc(iup, "IupSetStrAttributeId", {P,P,I,P})
      xIupSetStrAttributeId2        = iup_c_proc(iup, "IupSetStrAttributeId2", {P,P,I,I,P})
      xIupSetInt                    = iup_c_proc(iup, "IupSetInt", {P,P,I})
      xIupSetIntId                  = iup_c_proc(iup, "IupSetIntId", {P,P,I,I})
      xIupSetIntId2                 = iup_c_proc(iup, "IupSetIntId2", {P,P,I,I,I})
      xIupSetFloat                  = iup_c_proc(iup, "IupSetFloat", {P,P,F})
      xIupSetFloatId                = iup_c_proc(iup, "IupSetFloatId", {P,P,I,F})
      xIupSetFloatId2               = iup_c_proc(iup, "IupSetFloatId2", {P,P,I,I,F})
      xIupSetDouble                 = iup_c_proc(iup, "IupSetDouble", {P,P,D})
      xIupSetDoubleId               = iup_c_proc(iup, "IupSetDoubleId", {P,P,I,D})
      xIupSetDoubleId2              = iup_c_proc(iup, "IupSetDoubleId2", {P,P,I,I,D})
      xIupSetRGB                    = iup_c_proc(iup, "IupSetRGB", {P,P,UC,UC,UC})
      xIupSetRGBId                  = iup_c_proc(iup, "IupSetRGBId", {P,P,I,UC,UC,UC})
      xIupSetRGBId2                 = iup_c_proc(iup, "IupSetRGBId2", {P,P,I,I,UC,UC,UC})
      xIupStoreAttribute            = iup_c_proc(iup, "IupStoreAttribute", {P,P,P})
     xIupSetAttributeHandle         = iup_c_proc(iup, "IupSetAttributeHandle", {P,P,P})
     xIupSetAttributeHandleId       = iup_c_proc(iup, "IupSetAttributeHandleId", {P,P,I,P})
     xIupSetAttributeHandleId2      = iup_c_proc(iup, "IupSetAttributeHandleId2", {P,P,I,I,P})
      xIupSetHandle                 = iup_c_proc(iup, "IupSetHandle", {P,P})
    xIupSetAttributes               = iup_c_proc(iup, "IupSetAttributes", {P,P})
    xIupResetAttribute              = iup_c_proc(iup, "IupResetAttribute", {P,P})
    xIupGetAttribute                = iup_c_func(iup, "IupGetAttribute", {P,P}, P)
     xIupGetAttributeId             = iup_c_func(iup, "IupGetAttributeId", {P,P,I}, P)
     xIupGetAttributeId2            = iup_c_func(iup, "IupGetAttributeId2", {P,P,I,I}, P)
--   xIupGetAttributes              = iup_c_func(iup, "IupGetAttributes", {P}, P) --(deprecated)
     xIupGetAllAttributes           = iup_c_func(iup, "IupGetAllAttributes", {P,P,I}, I)
     xIupGetAttributeHandle         = iup_c_func(iup, "IupGetAttributeHandle", {P,P}, P)
     xIupGetAttributeHandleId       = iup_c_func(iup, "IupGetAttributeHandleId", {P,P,I}, P)
     xIupGetAttributeHandleId2      = iup_c_func(iup, "IupGetAttributeHandleId2", {P,P,I,I}, P)
      xIupGetHandle                 = iup_c_func(iup, "IupGetHandle", {P}, P)
     xIupGetInt                     = iup_c_func(iup, "IupGetInt", {P,P}, I)
     xIupGetInt2                    = iup_c_func(iup, "IupGetInt2", {P,P}, I)
     xIupGetIntInt                  = iup_c_func(iup, "IupGetIntInt", {P,P,P,P}, I)
     xIupGetIntId                   = iup_c_func(iup, "IupGetIntId", {P,P,I}, I)
     xIupGetIntId2                  = iup_c_func(iup, "IupGetIntId2", {P,P,I,I}, I)
     xIupGetFloat                   = iup_c_func(iup, "IupGetFloat", {P,P}, F)
     xIupGetFloatId                 = iup_c_func(iup, "IupGetFloatId", {P,P,I}, F)
     xIupGetFloatId2                = iup_c_func(iup, "IupGetFloatId2", {P,P,I,I}, F)
     xIupGetDouble                  = iup_c_func(iup, "IupGetDouble", {P,P}, D)
     xIupGetDoubleId                = iup_c_func(iup, "IupGetDoubleId", {P,P,I}, D)
     xIupGetDoubleId2               = iup_c_func(iup, "IupGetDoubleId2", {P,P,I,I}, D)
     xIupGetRGB                     = iup_c_proc(iup, "IupGetRGB", {P,P,P,P,P})
     xIupGetRGBId                   = iup_c_proc(iup, "IupGetRGBId", {P,P,I,P,P,P})
     xIupGetRGBId2                  = iup_c_proc(iup, "IupGetRGBId2", {P,P,I,I,P,P,P})
    xIupSetGlobal                   = iup_c_proc(iup, "IupSetGlobal", {P,P})
     xIupSetStrGlobal               = iup_c_proc(iup, "IupSetStrGlobal", {P,P})
     xIupStoreGlobal                = iup_c_proc(iup, "IupStoreGlobal", {P,P})
    xIupGetGlobal                   = iup_c_func(iup, "IupGetGlobal", {P}, P)
     xIupSetLanguage                = iup_c_proc(iup, "IupSetLanguage", {P})
     xIupGetLanguage                = iup_c_func(iup, "IupGetLanguage", {},P)
     xIupSetLanguagePack            = iup_c_proc(iup, "IupSetLanguagePack", {P})
     xIupSetLanguageString          = iup_c_proc(iup, "IupSetLanguageString", {P,P})
     xIupStoreLanguageString        = iup_c_proc(iup, "IupStoreLanguageString", {P,P})
     xIupGetLanguageString          = iup_c_func(iup, "IupGetLanguageString", {P},P)
    xIupSetCallback                 = iup_c_func(iup, "IupSetCallback", {P,P,P}, P)
     xIupGetCallback                = iup_c_func(iup, "IupGetCallback", {P,P}, P)
    xIupGetClassName                = iup_c_func(iup, "IupGetClassName", {P},P)
     xIupGetAllClasses              = iup_c_func(iup, "IupGetAllClasses", {P,I}, I)
     xIupGetClassType               = iup_c_func(iup, "IupGetClassType", {P}, P)
     xIupGetClassAttributes         = iup_c_func(iup, "IupGetClassAttributes", {P,P,I}, I)
     xIupGetClassCallbacks          = iup_c_func(iup, "IupGetClassCallbacks", {P,P,I}, I)
     xIupSaveClassAttributes        = iup_c_proc(iup, "IupSaveClassAttributes", {P})
     xIupCopyAttributes             = iup_c_proc(iup, "IupCopyAttributes", {P,P})
     xIupCopyClassAttributes        = iup_c_proc(iup, "IupCopyClassAttributes", {P,P})
     xIupSetClassDfltAttribute      = iup_c_proc(iup, "IupSetClassDefaultAttribute", {P,P,P})

    xIupFill            = iup_c_func(iup, "IupFill", {}, P)
    xIupSpace           = iup_c_func(iup, "IupSpace", {}, P)
    xIupHboxv           = iup_c_func(iup, "IupHboxv", {P}, P)
    xIupVboxv           = iup_c_func(iup, "IupVboxv", {P}, P)
    xIupZboxv           = iup_c_func(iup, "IupZboxv", {P}, P)
    xIupRadio           = iup_c_func(iup, "IupRadio", {P}, P)
    xIupNormalizerv     = iup_c_func(iup, "IupNormalizerv", {P}, P)
    xiupObjectCheck     = iup_c_func(iup, "iupObjectCheck", {P}, P)
    xIupCboxv           = iup_c_func(iup, "IupCboxv", {P}, P)
    xIupSbox            = iup_c_func(iup, "IupSbox", {P}, P)
    xIupSplit           = iup_c_func(iup, "IupSplit", {P,P}, P)
    xIupGridBoxv        = iup_c_func(iup, "IupGridBoxv", {P}, P)
    xIupMultiBoxv       = iup_c_func(iup, "IupMultiBoxv", {P}, P)

    xIupAppend          = iup_c_func(iup, "IupAppend", {P,P}, P)
    xIupDetach          = iup_c_proc(iup, "IupDetach", {P})
    xIupInsert          = iup_c_func(iup, "IupInsert", {P,P,P}, P)
    xIupReparent        = iup_c_func(iup, "IupReparent", {P,P,P}, P)
    xIupGetParent       = iup_c_func(iup, "IupGetParent", {P}, P)
    xIupGetChild        = iup_c_func(iup, "IupGetChild", {P,I}, P)
    xIupGetChildPos     = iup_c_func(iup, "IupGetChildPos", {P,P}, I)
    xIupGetChildCount   = iup_c_func(iup, "IupGetChildCount", {P}, I)
    xIupGetNextChild    = iup_c_func(iup, "IupGetNextChild", {P,P}, P)
    xIupGetBrother      = iup_c_func(iup, "IupGetBrother", {P},P)
    xIupGetDialog       = iup_c_func(iup, "IupGetDialog", {P},P)
    xIupGetDialogChild  = iup_c_func(iup, "IupGetDialogChild", {P,P},P)

    xIupRefresh         = iup_c_proc(iup, "IupRefresh", {P})
    xIupRefreshChildren = iup_c_proc(iup, "IupRefreshChildren", {P})
    xIupUpdate          = iup_c_proc(iup, "IupUpdate", {P})
    xIupUpdateChildren  = iup_c_proc(iup, "IupUpdate", {P})
    xIupRedraw          = iup_c_proc(iup, "IupRedraw", {P,I})
    xIupConvertXYToPos  = iup_c_func(iup, "IupConvertXYToPos", {P,I,I}, I)

    xIupDialog          = iup_c_func(iup, "IupDialog", {P},P)
    xIupPopup           = iup_c_func(iup, "IupPopup", {P,I,I},I)
    xIupShow            = iup_c_func(iup, "IupShow", {P},I)
    xIupShowXY          = iup_c_func(iup, "IupShowXY", {P,I,I},I)
    xIupHide            = iup_c_proc(iup, "IupHide", {P})

    xIupAlarm           = iup_c_func(iup, "IupAlarm", {P,P,P,P,P}, I)
    xIupMessage         = iup_c_proc(iup, "IupMessage", {P,P})
    xIupMessageError    = iup_c_proc(iup, "IupMessageError", {P,P})
    xIupMessageAlarm    = iup_c_func(iup, "IupMessageAlarm", {P,P,P,P}, I)
    xIupMessageDlg      = iup_c_func(iup, "IupMessageDlg", {}, P)
    xIupCalendar        = iup_c_func(iup, "IupCalendar", {}, I)
    xIupColorDlg        = iup_c_func(iup, "IupColorDlg", {}, P)
    xIupDatePick        = iup_c_func(iup, "IupDatePick", {}, I)
    xIupFileDlg         = iup_c_func(iup, "IupFileDlg", {}, P)
    xIupFontDlg         = iup_c_func(iup, "IupFontDlg", {}, P)
    xIupGetColor        = iup_c_func(iup, "IupGetColor", {I,I,P,P,P}, I)
    xIupGetFile         = iup_c_func(iup, "IupGetFile", {P}, I)
    xIupGetParamv       = iup_c_func(iup, "IupGetParamv", {P,P,P,P,I,I,P}, I)
    xIupGetText         = iup_c_func(iup, "IupGetText", {P,P,I}, I)
    xIupListDialog      = iup_c_func(iup, "IupListDialog", {I,P,I,P,I,I,I,P}, I)
    xIupLayoutDialog    = iup_c_func(iup, "IupLayoutDialog", {P}, P)
    xIupGlobalsDialog   = iup_c_func(iup, "IupGlobalsDialog", {}, P)
    xIupProgressDlg     = iup_c_func(iup, "IupProgressDlg", {}, P)

    xIupButton                  = iup_c_func(iup, "IupButton", {P,P}, P)
    xIupCanvas                  = iup_c_func(iup, "IupCanvas", {P}, P)
    xIupFrame                   = iup_c_func(iup, "IupFrame", {P}, P)
    xIupFlatFrame               = iup_c_func(iup, "IupFlatFrame", {P}, P)
    xIupLabel                   = iup_c_func(iup, "IupLabel", {P}, P)
    xIupFlatLabel               = iup_c_func(iup, "IupFlatLabel", {P}, P)
    xIupFlatList                = iup_c_func(iup, "IupFlatList", {}, P)
    xIupFlatSeparator           = iup_c_func(iup, "IupFlatSeparator", {}, P)
    xIupList                    = iup_c_func(iup, "IupList", {P}, P)
    xIupProgressBar             = iup_c_func(iup, "IupProgressBar", {}, P)
    xIupGauge                   = iup_c_func(iup, "IupGauge", {}, P)
    xIupSpin                    = iup_c_func(iup, "IupSpin", {}, P)
    xIupSpinbox                 = iup_c_func(iup, "IupSpinbox", {P}, P)
    xIupTabsv                   = iup_c_func(iup, "IupTabsv", {P}, P)
    xIupFlatTabsv               = iup_c_func(iup, "IupFlatTabsv", {P}, P)
    xIupText                    = iup_c_func(iup, "IupText", {P}, P)
    xIupMultiLine               = iup_c_func(iup, "IupMultiLine", {P},P)
    xIupTextConvertLinColToPos  = iup_c_proc(iup, "IupTextConvertLinColToPos", {P,I,I,P})
    xIupTextConvertPosToLinCol  = iup_c_proc(iup, "IupTextConvertPosToLinCol", {P,I,P,P})
    xIupToggle                  = iup_c_func(iup, "IupToggle", {P,P}, P)
    xIupFlatToggle              = iup_c_func(iup, "IupFlatToggle", {P,P}, P)
    xIupTree                    = iup_c_func(iup, "IupTree", {}, P)
    xIupFlatTree                = iup_c_func(iup, "IupFlatTree", {}, P)
    xIupVal                     = iup_c_func(iup, "IupVal", {P}, P)
    xIupFlatVal                 = iup_c_func(iup, "IupFlatVal", {P}, P)

    xIupConfig                        = iup_c_func(iup, "IupConfig", {}, P)
    xIupConfigCopy                    = iup_c_proc(iup, "IupConfigCopy", {P,P,P})
    xIupConfigLoad                    = iup_c_func(iup, "IupConfigLoad", {P}, I)
    xIupConfigSave                    = iup_c_func(iup, "IupConfigSave", {P}, I)
    xIupConfigSetVariableInt          = iup_c_proc(iup, "IupConfigSetVariableInt", {P,P,P,I})
    xIupConfigSetVariableIntId        = iup_c_proc(iup, "IupConfigSetVariableIntId", {P,P,P,I,I})
    xIupConfigSetVariableDouble       = iup_c_proc(iup, "IupConfigSetVariableDouble", {P,P,P,D})
    xIupConfigSetVariableDoubleId     = iup_c_proc(iup, "IupConfigSetVariableDoubleId", {P,P,P,I,D})
    xIupConfigSetVariableStr          = iup_c_proc(iup, "IupConfigSetVariableStr", {P,P,P,P})
    xIupConfigSetVariableStrId        = iup_c_proc(iup, "IupConfigSetVariableStrId", {P,P,P,I,P})
--  xIupConfigGetVariableInt          = iup_c_func(iup, "IupConfigGetVariableInt", {P,P,P}, I)
    xIupConfigGetVariableIntDef       = iup_c_func(iup, "IupConfigGetVariableIntDef", {P,P,P,I}, I)
--  xIupConfigGetVariableIntId        = iup_c_func(iup, "IupConfigGetVariableIntId", {P,P,P,I}, I)
    xIupConfigGetVariableIntIdDef     = iup_c_func(iup, "IupConfigGetVariableIntIdDef", {P,P,P,I,I}, I)
--  xIupConfigGetVariableDouble       = iup_c_func(iup, "IupConfigGetVariableDouble", {P,P,P}, D)
    xIupConfigGetVariableDoubleDef    = iup_c_func(iup, "IupConfigGetVariableDoubleDef", {P,P,P,D}, D)
--  xIupConfigGetVariableDoubleId     = iup_c_func(iup, "IupConfigGetVariableDoubleId", {P,P,P,I}, D)
    xIupConfigGetVariableDoubleIdDef  = iup_c_func(iup, "IupConfigGetVariableDoubleIdDef", {P,P,P,I,D}, D)
--  xIupConfigGetVariableStr          = iup_c_func(iup, "IupConfigGetVariableStr", {P,P,P}, P)
    xIupConfigGetVariableStrDef       = iup_c_func(iup, "IupConfigGetVariableStrDef", {P,P,P,P}, P)
--  xIupConfigGetVariableStrId        = iup_c_func(iup, "IupConfigGetVariableStrId", {P,P,P,I}, P)
    xIupConfigGetVariableStrIdDef     = iup_c_func(iup, "IupConfigGetVariableStrIdDef", {P,P,P,I,P}, P)
    xIupConfigRecentInit              = iup_c_proc(iup, "IupConfigRecentInit", {P,P,P,I})
    xIupConfigRecentUpdate            = iup_c_proc(iup, "IupConfigRecentUpdate", {P,P})
    xIupConfigDialogShow              = iup_c_proc(iup, "IupConfigDialogShow", {P,P,P})
    xIupConfigDialogClosed            = iup_c_proc(iup, "IupConfigDialogClosed", {P,P,P})
    xIupExecute                       = iup_c_func(iup, "IupExecute", {P,P},I)
    xIupExecuteWait                   = iup_c_func(iup, "IupExecuteWait", {P,P},I)
    xIupLog                           = iup_c_proc(iup, "IupLog", {P,P,P})

    xIupPreviousField   = iup_c_func(iup, "IupPreviousField", {P},P)
    xIupNextField       = iup_c_func(iup, "IupNextField", {P},P)
    xIupSetFocus        = iup_c_proc(iup, "IupSetFocus", {P})
    xIupGetFocus        = iup_c_func(iup, "IupGetFocus", {},P)

    xIupMenuv           = iup_c_func(iup, "IupMenuv", {P}, P)
    xIupItem            = iup_c_func(iup, "IupItem",  {P,P}, P)
    xIupSeparator       = iup_c_func(iup, "IupSeparator", {}, P)
    xIupSubmenu         = iup_c_func(iup, "IupSubmenu", {P,P}, P)

--  xIupSetHandle       = iup_c_proc(iup, "IupSetHandle", {P,P})
--  xIupGetHandle       = iup_c_func(iup, "IupGetHandle", {P},P)
    xIupGetName         = iup_c_func(iup, "IupGetName", {P},P)
    xIupGetAllNames     = iup_c_func(iup, "IupGetAllNames", {P,I},I)
    xIupGetAllDialogs   = iup_c_func(iup, "IupGetAllDialogs", {P,I},I)

    xIupClipboard = iup_c_func(iup, "IupClipboard", {}, P)
    xIupTimer = iup_c_func(iup, "IupTimer", {}, P)
    xIupUser = iup_c_func(iup, "IupUser", {}, P)
    xIupHelp = iup_c_func(iup, "IupHelp", {P}, I)

    iupControls = iup_open_dll({"iupcontrols.dll",
                                "libiupcontrols.so",
                                "libiupcontrols.dylib"})

    xIupControlsOpen        = iup_c_proc(iupControls, "IupControlsOpen", {})
    xIupCells               = iup_c_func(iupControls, "IupCells", {},P)
--  xIupColorbar            = iup_c_func(iupControls, "IupColorbar", {},P)      -- removed in 3.24
--  xIupColorBrowser        = iup_c_func(iupControls, "IupColorBrowser", {},P)  -- removed in 3.24
--  xIupDial                = iup_c_func(iupControls, "IupDial", {P},P)         -- removed in 3.24
    xIupColorbar            = iup_c_func(iup, "IupColorbar", {},P)      -- moved in 3.24
    xIupColorBrowser        = iup_c_func(iup, "IupColorBrowser", {},P)  -- moved in 3.24
    xIupDial                = iup_c_func(iup, "IupDial", {P},P)         -- moved in 3.24
    xIupMatrix              = iup_c_func(iupControls, "IupMatrix", {P},P)
    xIupMatrixEx            = iup_c_func(iupControls, "IupMatrixEx", {},P)
    xIupMatrixList          = iup_c_func(iupControls, "IupMatrixList", {},P)
--  xIupMatSetAttribute     = iup_c_proc(iupControls, "IupMatSetAttribute",{P,P,I,I,P})
--  xIupMatStoreAttribute   = iup_c_proc(iupControls, "IupMatStoreAttribute",{P,P,I,I,P})
--  xIupMatGetAttribute     = iup_c_func(iupControls, "IupMatGetAttribute",{P,P,I,I},P)
--  xIupMatGetInt           = iup_c_func(iupControls, "IupMatGetInt",{P,P,I,I},I)
--  xIupMatGetFloat         = iup_c_func(iupControls, "IupMatGetFloat",{P,P,I,I},F)

    xIupLoad                          = iup_c_func(iup, "+IupLoad", {P}, P)
    xIupLoadBuffer                    = iup_c_func(iup, "+IupLoadBuffer", {P}, P)
--  xIupSetLanguage                   = iup_c_proc(iup, "+IupSetLanguage", {P})
--  xIupGetLanguage                   = iup_c_func(iup, "+IupGetLanguage", {}, P)
--  xIupSetLanguageString             = iup_c_proc(iup, "+IupSetLanguageString", {P,P})
--  xIupStoreLanguageString           = iup_c_proc(iup, "+IupStoreLanguageString", {P,P})
--  xIupGetLanguageString             = iup_c_func(iup, "+IupGetLanguageString", {P}, P)
--  xIupSetLanguagePack               = iup_c_proc(iup, "+IupSetLanguagePack", {P})
    xIupGetFunction                   = iup_c_func(iup, "+IupGetFunction", {P}, P)
    xIupSetFunction                   = iup_c_func(iup, "+IupSetFunction", {P,P}, P)
    xIupClassMatch                    = iup_c_func(iup, "+IupClassMatch", {P,P}, I)
    xIupScrollBox                     = iup_c_func(iup, "+IupScrollBox", {P}, P)
    xIupFlatScrollBox                 = iup_c_func(iup, "+IupFlatScrollBox", {P}, P)
    xIupExpander                      = iup_c_func(iup, "+IupExpander", {P}, P)
    xIupDetachBox                     = iup_c_func(iup, "+IupDetachBox", {P}, P)
    xIupBackgroundBox                 = iup_c_func(iup, "+IupBackgroundBox", {P}, P)
    xIupLink                          = iup_c_func(iup, "+IupLink", {P,P}, P)
    xIupFlatButton                    = iup_c_func(iup, "+IupFlatButton", {P}, P)
    xIupDropButton                    = iup_c_func(iup, "+IupDropButton", {P}, P)
    xIupStoreAttributeId              = iup_c_proc(iup, "+IupStoreAttributeId", {P,P,I,P})
    xIupStoreAttributeId2             = iup_c_proc(iup, "+IupStoreAttributeId2", {P,P,I,I,P})
    xIupTreeSetUserId                 = iup_c_func(iup, "+IupTreeSetUserId", {P,I,P}, I)
    xIupTreeGetUserId                 = iup_c_func(iup, "+IupTreeGetUserId", {P,I}, P)
    xIupTreeGetId                     = iup_c_func(iup, "+IupTreeGetId", {P,P}, I)
--  xIupTreeSetAttributeHandle        = iup_c_proc(iup, "+IupTreeSetAttributeHandle", {P,P,I,P})
--  xIupTreeSetAttribute              = iup_c_proc(iup, "+IupTreeSetAttribute", {P,P,I,P})
--  xIupTreeStoreAttribute            = iup_c_proc(iup, "+IupTreeStoreAttribute", {P,P,I,P})
--  xIupTreeGetAttribute              = iup_c_func(iup, "+IupTreeGetAttribute", {P,P,I}, P)
--  xIupTreeGetInt                    = iup_c_func(iup, "+IupTreeGetInt", {P,P,I}, I)
--  xIupTreeGetFloat                  = iup_c_func(iup, "+IupTreeGetFloat", {P,P,I}, F)
--  xIupMapFont                       = iup_c_func(iup, "+IupMapFont", {P}, P)
--  xIupUnMapFont                     = iup_c_func(iup, "+IupUnMapFont", {P}, P)
--  xIupParamf                        = iup_c_func(iup, "+IupParamf", {P}, P)
--  xIupParamBox                      = iup_c_func(iup, "+IupParamBox", {P,P,I}, P)
    xIupElementPropertiesDialog       = iup_c_func(iup, "+IupElementPropertiesDialog", {P,P}, P)
    xIupClassInfoDialog               = iup_c_func(iup, "+IupClassInfoDialog", {P}, P)
    xiupKeyCodeToName                 = iup_c_func(iup, "iupKeyCodeToName", {I},P)

end procedure

--DEV/SUG?
--sequence cmd = command_line()
--sequence argv = cmd[3..$]
--integer argc = length(argv)
--
--int IupOpen(int *argc, char ***argv);
global procedure IupOpen(nullable_string dll_root=NULL)
    iup_init1(dll_root)
    if c_func(xIupOpen, {NULL,NULL})=IUP_ERROR then ?9/0 end if
end procedure

integer did_iup_controls_open = 0

global procedure IupControlsOpen()
    if not did_iup_controls_open then
        c_proc(xIupControlsOpen, {})
    end if
    did_iup_controls_open = 1
end procedure

global procedure IupClose()
    c_proc(xIupClose, {})
    did_iup_controls_open = 0
end procedure

global function IupLoad(string filename)
    atom pError = c_func(xIupLoad, {filename})
    nullable_string res = iff(pError?peek_string(pError):NULL)
    return res
end function

global function IupLoadBuffer(string buffer)
    atom pError = c_func(xIupLoadBuffer, {buffer})
    nullable_string res = iff(pError?peek_string(pError):NULL)
    return res
end function

--global procedure IupSetLanguage(string language)
--  c_proc(xIupSetLanguage, {language})
--end procedure
--
--global function IupGetLanguage()
--  return peek_string(c_func(xIupGetLanguage, {}))
--end function

global function IupDestroy(Ihandlns ih)
    if sequence(ih) then
        for i=1 to length(ih) do
            ih[i] = IupDestroy(ih[i])
        end for
    elsif ih!=NULL then
        c_proc(xIupDestroy, {ih})
        ih = NULL
    end if
    return ih
end function

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

global procedure IupVersionShow()
    c_proc(xIupVersionShow, {})
end procedure

--constant
--  xIupMainLoop        = iup_c_proc(iup, "IupMainLoop", {}),
--  xIupMainLoopLevel   = iup_c_func(iup, "IupMainLoopLevel", {}, I),
--  xIupLoopStep        = iup_c_func(iup, "IupLoopStep", {}, I),
--  xIupLoopStepWait    = iup_c_func(iup, "IupLoopStepWait", {}, I),
--  xIupExitLoop        = iup_c_proc(iup, "IupExitLoop", {}),
--  xIupFlush           = iup_c_proc(iup, "IupFlush", {}),
--  xIupRecordInput     = iup_c_func(iup, "IupRecordInput", {P,I}, I),
--  xIupPlayInput       = iup_c_proc(iup, "IupPlayInput", {P}),
--  xIupGetActionName   = iup_c_func(iup, "IupGetActionName", {}, P)

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

global procedure IupPostMessage(Ihandle ih, nullable_string s, integer i, atom d, p)
    c_proc(xIupPostMessage, {ih, s, i, d, p})
end procedure

global procedure IupFlush()
    c_proc(xIupFlush, {})
end procedure

global procedure IupRecordInput(nullable_string filename, integer mode)
    if c_func(xIupRecordInput, {filename,mode})!=IUP_NOERROR then ?9/0 end if
end procedure

global procedure IupPlayInput(nullable_string filename=NULL)
    c_proc(xIupPlayInput, {filename})
end procedure

/* DEPRECATED callback management. It will be removed in a future version. */
-- (works only if application used IupSetFunction)
--global function IupGetActionName()
--  atom pName = c_func(xIupGetActionName, {})
--  string name = peek_string(pName)
--  return name
--end function

global procedure IupSetAttribute(Ihandln ih, string name, atom_string v)
    if name!=upper(name) then ?9/0 end if
    c_proc(xIupSetAttribute, {ih, name, v})
end procedure

global procedure IupSetAttributePtr(Ihandln ih, string name, atom v)
--  if name!=upper(name) then ?9/0 end if
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

global procedure IupSetStrAttribute(Ihandle ih, string name, nullable_string val, sequence args={})
    if name!=upper(name) then ?9/0 end if
    if length(args) then
        val = sprintf(val, args)
    end if
    c_proc(xIupSetStrAttribute, {ih,name,val})
end procedure

global procedure IupSetStrAttributeId(Ihandle ih, string name, integer id, nullable_string v=NULL, sequence args={})
    if length(args) then
        v = sprintf(v, args)
    end if
    c_proc(xIupSetStrAttributeId, {ih,name,id,v})
end procedure

global procedure IupSetStrAttributeId2(Ihandle ih, string name, integer lin, col, nullable_string v=NULL, sequence args={})
    if length(args) then
        v = sprintf(v, args)
    end if
    c_proc(xIupSetStrAttributeId2, {ih,name,lin,col,v})
end procedure

global procedure IupSetInt(Ihandles ih, string name, atom v)
    if sequence(ih) then
        for i=1 to length(ih) do
            Ihandle ihi = ih[i]     -- (deliberate typecheck)
            IupSetInt(ihi,name,v)
        end for
    else
        c_proc(xIupSetInt, {ih,name,v})
    end if
end procedure

global procedure IupSetGlobalInt(string name, integer v)
    c_proc(xIupSetInt, {NULL,name,v})
end procedure

global procedure IupSetIntId(Ihandle ih, string name, integer id, integer v)
    c_proc(xIupSetIntId, {ih,name,id,v})
end procedure

global procedure IupSetIntId2(Ihandle ih, string name, integer lin, integer col, integer v)
    c_proc(xIupSetIntId2, {ih,name,lin,col,v})
end procedure

global procedure IupSetFloat(Ihandle ih, string name, atom v)
    c_proc(xIupSetFloat, {ih,name,v})
end procedure

global procedure IupSetFloatId(Ihandle ih, string name, integer id, atom v)
    c_proc(xIupSetFloatId, {ih,name,id,v})
end procedure

global procedure IupSetFloatId2(Ihandle ih, string name, integer lin, integer col, atom v)
    c_proc(xIupSetFloatId2, {ih,name,lin,col,v})
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

global procedure IupStoreAttribute(Ihandln ih, string name, nullable_string val, sequence args={})
    if name!=upper(name) then ?9/0 end if
    if length(args) then
        val = sprintf(val, args)
    end if
    c_proc(xIupStoreAttribute, {ih, name, val})
end procedure

--DEV/SUG rewrite in Phix:
--/*
static const char* env_str = NULL;
static void iAttribCapture(char* env_buffer, char* dlm)
{
  int i=0;
  int c;
  do
  {
    c = *env_str; ++env_str;
    if (i < 256)
      env_buffer[i++] = (char) c;
  } while (c && !strchr(dlm,c));
  env_buffer[i-1]='\0';                                /!* discard delimiter *!/
}

static void iAttribSkipComment(void)
{
  int c;
  do
  {
    c = *env_str; ++env_str;
  } while ((c > 0) && (c != '\n'));
}

static int iAttribToken(char* env_buffer)
{
  for (;;)
  {
    int c = *env_str; ++env_str;
    switch (c)
    {
    case 0:
      return IUPLEX_TK_END;

    case '#':          /* Skip comment */
    case '%':          /* Skip comment */
      iAttribSkipComment();
      continue;

    case ' ':          /* ignore whitespace */
    case '\t':
    case '\n':
    case '\r':
    case '\f':
    case '\v':
      continue;

    case '=':          /* attribuicao */
      return IUPLEX_TK_SET;

    case ',':
      return IUPLEX_TK_COMMA;

    case '\"':          /* string */
--    iAttribCapture(env_buffer, "\"");
      iAttribCapture(env_buffer, `"`);
      return IUPLEX_TK_NAME;

    default:
      if (c > 32)          /* identifier */
      {
        --env_str;                     /* unget first character of env_buffer */
        iAttribCapture(env_buffer, "=, \t\n\r\f\v"); /* get env_buffer until delimiter */
        --env_str;                     /* unget delimiter */
        return IUPLEX_TK_NAME;
      }
    }
  }
}

static void iAttribParse(Ihandle *ih, const char* str)
{
  char env_buffer[256];
  char* name=NULL;
  char* value=NULL;
  char state = 'a';               /* get attribute */
  int end = 0;

  env_str = str;

  for (;;)
  {
    switch (iAttribToken(env_buffer))
    {
    case IUPLEX_TK_END:           /* same as IUPLEX_TK_COMMA */
      end = 1;
    case IUPLEX_TK_COMMA:
      if (name)
      {
        IupStoreAttribute(ih, name, value);
        free(name);
      }
      if (end)
        return;
      name = value = NULL;
      state = 'a';
      break;

    case IUPLEX_TK_SET:
      state = 'v';                /* get value */
      break;

    case IUPLEX_TK_NAME:
      if (state == 'a')
        name = iupStrDup(env_buffer);
      else
        value = env_buffer;
      break;
    }
  }
}

Ihandle* IupSetAttributes(Ihandle *ih, const char* str)
{
  iupASSERT(iupObjectCheck(ih));
  if (!iupObjectCheck(ih))
    return ih;
  if (str)
    iAttribParse(ih, str);
  return ih;
}
--*/
global procedure IupSetAttributes(Ihandles ih, string attributes, sequence args={})
    if length(args) then
        attributes = sprintf(attributes, args)
    end if
    if length(attributes) then
        if atom(ih) then
            c_proc(xIupSetAttributes, {ih, attributes})
        else
            for i=1 to length(ih) do
                c_proc(xIupSetAttributes, {ih[i], attributes})
            end for
        end if
    end if
end procedure

--global function IupSetAttributesf(Ihandle ih, string attributes, sequence args={})
--  IupSetAttributes(ih, attributes, args)
--  return ih
--end function

global procedure IupSetAttributeHandle(Ihandln ih, string name, Ihandln ih_named)
--  if name!=upper(name) then ?9/0 end if
    c_proc(xIupSetAttributeHandle, {ih, name, ih_named})
end procedure

global procedure IupSetAttributeHandleId(Ihandln ih, string name, integer id, Ihandln ih_named)
--  if name!=upper(name) then ?9/0 end if
    c_proc(xIupSetAttributeHandleId, {ih, name, id, ih_named})
end procedure

global procedure IupSetAttributeHandleId2(Ihandle ih, string name, integer lin, col, Ihandln ih_named)
--  if name!=upper(name) then ?9/0 end if
    c_proc(xIupSetAttributeHandle, {ih, name, lin, col, ih_named})
end procedure

-- removed 18/1/17 (unused)
--global function IupSetAttributeHandlef(Ihandln ih, string name, Ihandle ih_named)
--  IupSetAttributeHandle(ih, name, ih_named)
--  return ih
--end function

-- (deprecated, use IupSetAttributeHandle instead)
global procedure IupSetHandle(string name, Ihandle ih)
--  name = upper(name)
    c_proc(xIupSetHandle, {name, ih})
end procedure

--21/6/19. removed (as per docs, as per IupSetAttributesf). iup_layoutdlg.e does however printf() some out.
--global function IupSetAtt(nullable_string name, Ihandle ih, sequence attribute_pairs)
--  if and_bits(length(attribute_pairs),1) then ?9/0 end if
--  for i=1 to length(attribute_pairs) by 2 do
--      IupSetAttribute(ih, attribute_pairs[i], attribute_pairs[i+1])
--  end for
--  if name!=NULL then
--      IupSetHandle(name, ih)
--  end if
--  return ih
--end function

global procedure IupResetAttribute(Ihandle ih, string name)
    c_proc(xIupResetAttribute, {ih,name})
end procedure

global function IupGetAttribute(Ihandln ih, string name)
    atom ptr = c_func(xIupGetAttribute, {ih, name})
    if ptr=NULL then return "" end if
    return peek_string(ptr)
end function

global function IupGetAttributePtr(Ihandln ih, string name)
    atom ptr = c_func(xIupGetAttribute, {ih, name})
    return ptr
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
    atom ptr = allocate(W*n*8)
    mem_set(ptr,0,W*n) -- (not strictly necessary)
    -- (aside: n may shrink here, as (ih,NULL,0) returns a count that includes
    --         internal attributes, which are filtered out in this return.)
    n = c_func(xIupGetAllAttributes, {ih,ptr,n})
    sequence names  = iup_peek_string_pointer_array(ptr, n)
    free(ptr)
    return names
end function

global function IupGetAttributeHandle(Ihandln ih, string name)
    Ihandln ih_named = c_func(xIupGetAttributeHandle, {ih, name})
    return ih_named
end function

global function IupGetAttributeHandleId(Ihandln ih, string name, integer id)
    Ihandln ih_named = c_func(xIupGetAttributeHandleId, {ih, name, id})
    return ih_named
end function

global function IupGetAttributeHandleId2(Ihandln ih, string name, integer lin, col)
    Ihandln ih_named = c_func(xIupGetAttributeHandleId2, {ih, name, lin, col})
    return ih_named
end function

-- (deprecated, use IupGetAttributeHandle instead)
global function IupGetHandle(string name)
    Ihandln ih = c_func(xIupGetHandle, {name})
    return ih
end function

global function IupGetInt(Ihandln ih, string name)
    atom res = c_func(xIupGetInt, {ih, name})
    return res
end function

global function IupGetInt2(Ihandle ih, string name)
    atom res = c_func(xIupGetInt2, {ih, name})
    return res
end function

global function IupGetIntInt(Ihandln ih, string name)
    atom pTwoInts = allocate(8)
    mem_set(pTwoInts,0,8)
    integer count = c_func(xIupGetIntInt, {ih,name,pTwoInts,pTwoInts+4})
--  if count!=2 then ?9/0 end if
    sequence res = peek4s({pTwoInts,2})
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

global function IupGetFloat(Ihandle ih, string name)
    atom result = c_func(xIupGetFloat, {ih,name})
    return result
end function

global function IupGetFloatId(Ihandle ih, string name, integer id)
    atom result = c_func(xIupGetFloatId, {ih,name,id})
    return result
end function

global function IupGetFloatId2(Ihandle ih, string name, integer lin, integer col)
    atom result = c_func(xIupGetFloatId2, {ih,name,lin,col})
    return result
end function

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
    atom pRGB = allocate(3),
         pR = pRGB+0,
         pG = pRGB+1,
         pB = pRGB+2
    c_proc(xIupGetRGB, {ih,name,pR,pG,pB})
    sequence res = peek({pRGB,3})
    free(pRGB)
    return res
end function

global function IupGetRGBId(Ihandle ih, string name, integer id)
    atom pRGB = allocate(3),
         pR = pRGB+0,
         pG = pRGB+1,
         pB = pRGB+2
    c_proc(xIupGetRGBId, {ih,name,id,pR,pG,pB})
    sequence res = peek({pRGB,3})
    free(pRGB)
    return res
end function

global function IupGetRGBId2(Ihandle ih, string name, integer lin, integer col)
    atom pRGB = allocate(3),
         pR = pRGB+0,
         pG = pRGB+1,
         pB = pRGB+2
    c_proc(xIupGetRGBId2, {ih,name,lin,col,pR,pG,pB})
    sequence res = peek({pRGB,3})
    free(pRGB)
    return res
end function

global procedure IupSetGlobal(string name, atom_string v)
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

global function IupGetGlobalInt(string name)
    return IupGetInt(NULL,name)
end function

global procedure IupSetLanguage(string language_name)
    c_proc(xIupSetLanguage, {language_name})
end procedure

global function IupGetLanguage()
    atom ptr = c_func(xIupGetLanguage, {})
    return peek_string(ptr)
end function

global procedure IupSetLanguageString(string name, val)
    c_proc(xIupSetLanguageString, {name, val})
end procedure

global procedure IupStoreLanguageString(string name, val)
    c_proc(xIupStoreLanguageString, {name, val})
end procedure

global function IupGetLanguageString(string name)
    atom ptr = c_func(xIupGetLanguageString, {name})
    return peek_string(ptr)
end function

global procedure IupSetLanguagePack(Ihandln ih)
    c_proc(xIupSetLanguagePack, {ih})
end procedure

global procedure IupSetCallback(Ihandles ih, string name, cbfunc func)
    if sequence(ih) then
        for i=1 to length(ih) do
            Ihandle ihi = ih[i]     -- (typecheck deliberate)
            IupSetCallback(ihi,name,func)
        end for
    else
        if name="K_ANY"
        or name="KEY_CB" then
            if func!=NULL then
                -- verify func takes the right args
                -- Trapped specially because it is particularly irksome to have a
                -- program run fine for ages then suddenly crash just because you
                -- have (accidentally) hit a funny key.
                integer rid = rid_from_cb(func)
                sequence ri = get_routine_info(rid,false)
--              if ri[1]!=2 then ?9/0 end if
--              if ri[2]!=2 then ?9/0 end if
--              if ri[3][1]!='F' then ?9/0 end if
--              if ri[3][3]!='N' then ?9/0 end if
--              if ri[1..3]!={2,2,"FIN"} then
--              if  ri!={2,2,"FIN"}
--              and ri!={2,2,"FNN"} then
                if ri!={2,2,"FNN"} then
                    crash(name&" callback must have func(Ihandle,ATOM) sig",nFrames:=2)
                end if
            end if
            name = "K_ANY"
        end if
        atom prev = c_func(xIupSetCallback, {ih, name, func})
    end if
end procedure

global function IupSetCallbackf(Ihandles ih, string name, cbfunc func)
    IupSetCallback(ih, name, func)
    return ih
end function

global procedure IupSetCallbacks(Ihandles ih, sequence namefuncpairs)
    if and_bits(length(namefuncpairs),1) then ?9/0 end if
    for i=1 to length(namefuncpairs) by 2 do
        IupSetCallback(ih,namefuncpairs[i],namefuncpairs[i+1])
    end for
end procedure

global function IupGetCallback(Ihandle ih, string name)
    if name="KEY_CB" then name = "K_ANY" end if
    atom func = c_func(xIupGetCallback, {ih, name})
    return func
end function

global procedure IupHide(Ihandle ih)
    c_proc(xIupHide, {ih})
end procedure

function key_cb(Ihandle dlg, atom c)
--?dlg
    -- private version for IupCloseOnEscape()
    if c=K_ESC then
        atom close_cb = IupGetCallback(dlg,"CLOSE_CB")
        if close_cb!=NULL then
            c_proc(define_c_proc({},{'+',close_cb},{C_PTR}),{dlg})
        end if
--?{"dlg",dlg,"modal",IupGetInt(dlg,"MODAL"),"parent",IupGetParent(dlg)}
--?{"dlg",dlg,"modal",IupGetInt(dlg,"MODAL"),"parent",IupGetAttributeHandle(dlg,"PARENTDIALOG")}
        if IupGetInt(dlg,"MODAL")
--      or IupGetParent(dlg)=NULL then
        or IupGetAttributeHandle(dlg,"PARENTDIALOG")=NULL then
            return IUP_CLOSE
        end if
        IupHide(dlg)
    end if
    return IUP_DEFAULT
end function
constant cb_key = Icallback("key_cb")

global procedure IupCloseOnEscape(Ihandle dlg, bool bClose=true)
    IupSetCallback(dlg, "K_ANY", iff(bClose?cb_key:NULL))
end procedure

global function IupGetAllClasses()
    integer n = c_func(xIupGetAllClasses, {NULL,0})
    atom ptr = allocate_data(sizeof(P)*n)
    n = c_func(xIupGetAllClasses, {ptr,n})
    sequence names = iup_peek_string_pointer_array(ptr, n)
    free(ptr)
    return names
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
    integer n = c_func(xIupGetClassAttributes, {classname,NULL,0})
    atom ptr = allocate_data(sizeof(P)*n)
    n = c_func(xIupGetClassAttributes, {classname,ptr,n})
    sequence names = iup_peek_string_pointer_array(ptr, n)
    free(ptr)
    return names
end function

global function IupGetClassCallbacks(string classname)
    integer n = c_func(xIupGetClassCallbacks, {classname,NULL,0})
    atom ptr = allocate_data(sizeof(P)*n)
    n = c_func(xIupGetClassCallbacks, {classname,ptr,n})
    sequence names = iup_peek_string_pointer_array(ptr, n)
    free(ptr)
    return names
end function

global procedure IupSaveClassAttributes(Ihandle ih)
    c_proc(xIupSaveClassAttributes, {ih})
end procedure

global procedure IupCopyAttributes(Ihandle src_ih, Ihandle dst_ih)
    c_proc(xIupCopyAttributes, {src_ih,dst_ih})
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

global function IupCreate(string name, Ihandles children={}, string attributes="", sequence args={})
Ihandle ih
    if length(children)=0 then
        ih = c_func(xIupCreate, {name})
    else
        atom pChildren = iup_ptr_array(children)
        ih = c_func(xIupCreatev, {name,pChildren})
        free(pChildren)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

/************************************************************************/
/*               Mouse Button Values and Macros                         */
/************************************************************************/
global constant IUP_BUTTON1 = '1'
global constant IUP_BUTTON2 = '2'
global constant IUP_BUTTON3 = '3'
global constant IUP_BUTTON4 = '4'
global constant IUP_BUTTON5 = '5'

--global function isshift(atom pchar)
--  return peek(pchar)='S'
--end function
--
--global function iscontrol(atom pchar)
--  return peek(pchar+1)='C'
--end function
--
--global function isbutton1(atom pchar)
--  return peek(pchar+2)='1'
--end function
--
--global function isbutton2(atom pchar)
--  return peek(pchar+3)='2'
--end function
--
--global function isbutton3(atom pchar)
--  return peek(pchar+4)='3'
--end function
--
--global function isbutton4(atom pchar)
--  return peek(pchar+8)='4'
--end function
--
--global function isbutton5(atom pchar)
--  return peek(pchar+9)='5'
--end function
--
--global function isdouble(atom pchar)
--  return peek(pchar+5)='D'
--end function
--
--global function isalt(atom pchar)
--  return peek(pchar+6)='A'
--end function
--
--global function issys(atom pchar)
--  return peek(pchar+7)='Y'
--end function

--/*
#define iup_isshift(_s)    (_s[0]=='S')
#define iup_iscontrol(_s)  (_s[1]=='C')
#define iup_isbutton1(_s)  (_s[2]=='1')
#define iup_isbutton2(_s)  (_s[3]=='2')
#define iup_isbutton3(_s)  (_s[4]=='3')
#define iup_isdouble(_s)   (_s[5]=='D')
#define iup_isalt(_s)      (_s[6]=='A')
#define iup_issys(_s)      (_s[7]=='Y')
#define iup_isbutton4(_s)  (_s[8]=='4')
#define iup_isbutton5(_s)  (_s[9]=='5')
--*/
global function iup_isshift(atom pStatus)
-- shift button
    return peek(pStatus)='S'
end function

global function iup_iscontrol(atom pStatus)
-- control button
    return peek(pStatus+1)='C'
end function

global function iup_isbutton1(atom pStatus)
-- left mouse button
    return peek(pStatus+2)='1'
end function

global function iup_isbutton2(atom pStatus)
-- middle mouse button
    return peek(pStatus+3)='2'
end function

global function iup_isbutton3(atom pStatus)
-- right mouse button
    return peek(pStatus+4)='3'
end function

global function iup_isdouble(atom pStatus)
-- double click
    return peek(pStatus+5)='D'
end function

global function iup_isalt(atom pStatus)
-- Alt Key
    return peek(pStatus+6)='A'
end function

global function iup_issys(atom pStatus)
-- Sys Key
    return peek(pStatus+7)='Y'
end function

global function iup_isbutton4(atom pStatus)
    return peek(pStatus+8)='4'
end function

global function iup_isbutton5(atom pStatus)
    return peek(pStatus+9)='5'
end function

global function iupKeyCodeToName(atom ch)
    atom pKeyName = c_func(xiupKeyCodeToName,{ch})
    return peek_string(pKeyName)
end function

global procedure IupImageLibOpen()
atom iupimglib = iup_open_dll({"iupimglib.dll",
                               "libiupimglib.so",
                               "libiupimglib.dylib"})
integer xIupImageLibOpen = iup_c_proc(iupimglib, "IupImageLibOpen", {})
    c_proc(xIupImageLibOpen, {})
end procedure


 -- done to here --
--------------------

--global procedure set_events_callbacks(Ihandle ih, string name, sequence funcs)
--  for i=1 to length(funcs) do
--      IupSetCallback(ih, name, funcs[i])
--  end for
--end procedure

--/*
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
--*/

global function IupFill(string attributes="", sequence args={})
    Ihandle ih = c_func(xIupFill, {})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupSpace(string attributes="", sequence args={})
    Ihandle ih = c_func(xIupSpace, {})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupHbox(Ihandles children={}, string attributes="", sequence args={})
atom pChildren = iup_ptr_array(children)
    Ihandle ih = c_func(xIupHboxv, {pChildren})
    free(pChildren)
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupVbox(Ihandles children={}, string attributes="", sequence args={})
atom pChildren = iup_ptr_array(children)
    Ihandle ih = c_func(xIupVboxv, {pChildren})
    free(pChildren)
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupZbox(Ihandles children={}, string attributes="", sequence args={})
atom pChildren = iup_ptr_array(children)
    Ihandle ih = c_func(xIupZboxv, {pChildren})
    free(pChildren)
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupRadio(Ihandln child=NULL, string attributes="", sequence args={})
    Ihandle ih = c_func(xIupRadio, {child})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupNormalizer(Ihandles ih_list, string attributes="", sequence args={})
    atom p_ih_list = iup_ptr_array(ih_list)
    Ihandle ih = c_func(xIupNormalizerv, {p_ih_list})
    free(p_ih_list)
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

-- only used by IupView.exw, and deliberately not documented.
global function iupObjectCheck(Ihandle ih)
    boolean res = c_func(xiupObjectCheck,{ih})
    return res
end function

global function IupNormaliser(Ihandles ih_list, string attributes="", sequence args={})
    return IupNormalizer(ih_list, attributes, args)
end function

global function IupCbox(Ihandles children={}, string attributes="", sequence args={})
atom pChildren = iup_ptr_array(children)
    Ihandle ih = c_func(xIupCboxv, {pChildren})
    free(pChildren)
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupSbox(Ihandln child=NULL, string attributes="", sequence args={})
    Ihandle ih = c_func(xIupSbox, {child})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupSizeBox(Ihandln child=NULL, string attributes="", sequence args={})
    return IupSbox(child, attributes, args)
end function

global function IupSplit(Ihandln child1=NULL, Ihandln child2=NULL, string attributes="", sequence args={})
    Ihandle ih = c_func(xIupSplit, {child1,child2})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

--(I have no idea how these would be used)
--global enum IGBOX_HORIZONTAL, 
--          IGBOX_VERTICAL

global function IupGridBox(Ihandles children={}, string attributes="", sequence args={})
atom pChildren = iup_ptr_array(children)
    Ihandle ih = c_func(xIupGridBoxv, {pChildren})
    free(pChildren)
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupMultiBox(Ihandles children={}, string attributes="", sequence args={})
atom pChildren = iup_ptr_array(children)
    Ihandle ih = c_func(xIupMultiBoxv, {pChildren})
    free(pChildren)
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

--****
-- === Layout/Hierarchy
--

--constant
--  xIupAppend          = iup_c_func(iup, "IupAppend", {P,P}, P),
--  xIupDetach          = iup_c_proc(iup, "IupDetach", {P}),
--  xIupInsert          = iup_c_func(iup, "IupInsert", {P,P,P}, P),
--  xIupReparent        = iup_c_func(iup, "IupReparent", {P,P,P}, P),
--  xIupGetParent       = iup_c_func(iup, "IupGetParent", {P}, P),
--  xIupGetChild        = iup_c_func(iup, "IupGetChild", {P,I}, P),
--  xIupGetChildPos     = iup_c_func(iup, "IupGetChildPos", {P,P}, I),
--  xIupGetChildCount   = iup_c_func(iup, "IupGetChildCount", {P}, I),
--  xIupGetNextChild    = iup_c_func(iup, "IupGetNextChild", {P,P}, P),
--  xIupGetBrother      = iup_c_func(iup, "IupGetBrother", {P},P),
--  xIupGetDialog       = iup_c_func(iup, "IupGetDialog", {P},P),
--  xIupGetDialogChild  = iup_c_func(iup, "IupGetDialogChild", {P,P},P)

--DEV/SUG: (then again, do we actually need IupAppends? - NO!)
--global procedure IupAppend(Ihandle ih, Ihandles children)
--Ihandle parent, child
--  if atom(children) then
--      parent = c_func(xIupAppend, {ih,children})  -- (error if NULL/fail)
--  else
--      for i=1 to length(children) do
--          child = children[i]           -- (error if NULL or over-nested)
--          parent = c_func(xIupAppend, {ih,child}) -- (error if NULL/fail)
--      end for
--  end if
--end procedure

global procedure IupAppend(Ihandle ih, Ihandle child)
    Ihandle actual_parent = c_func(xIupAppend, {ih,child}) -- (error if NULL/fail)
end procedure

--global procedure IupAppends(Ihandle ih, sequence children)
--  for i=1 to length(children) do
--      IupAppend(ih, children[i])
--  end for
--end procedure

global procedure IupDetach(Ihandle ih)
    c_proc(xIupDetach, {ih})
end procedure

global procedure IupInsert(Ihandle ih, Ihandln ref_child, Ihandle new_child)
    Ihandle parent = c_func(xIupInsert, {ih,ref_child,new_child})   -- (error if NULL/fail)
end procedure

global function IupReparent(Ihandle child, Ihandle new_parent, Ihandln ref_child)
integer err = c_func(xIupReparent, {child, new_parent, ref_child})
    return err
end function

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

global function IupGetNextChild(Ihandln ih, Ihandln child)
    if ih=NULL and child=NULL then ?9/0 end if
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

--constant
--  xIupRefresh         = iup_c_proc(iup, "IupRefresh", {P}),
--  xIupRefreshChildren = iup_c_proc(iup, "IupRefreshChildren", {P}),
--  xIupUpdate          = iup_c_proc(iup, "IupUpdate", {P}),
--  xIupUpdateChildren  = iup_c_proc(iup, "IupUpdate", {P}),
--  xIupRedraw          = iup_c_proc(iup, "IupRedraw", {P,I}),
--  xIupConvertXYToPos  = iup_c_func(iup, "IupConvertXYToPos", {P,I,I}, I)
--
global procedure IupRefresh(Ihandles ih)
    if sequence(ih) then
        for i=1 to length(ih) do
            IupRefresh(ih[i])
        end for
    else
        c_proc(xIupRefresh, {ih})
    end if
end procedure

global procedure IupRefreshChildren(Ihandle ih)
    c_proc(xIupRefreshChildren, {ih})
end procedure

global procedure IupUpdate(Ihandles ih)
    if sequence(ih) then
        for i=1 to length(ih) do
            Ihandle ihi = ih[i]
            c_proc(xIupUpdate, {ihi})
        end for
    else
        c_proc(xIupUpdate, {ih})
    end if
end procedure

global procedure IupUpdateChildren(Ihandle ih)
    c_proc(xIupUpdateChildren, {ih})
end procedure

global procedure IupRedraw(Ihandles ih, boolean children=true)
    if sequence(ih) then
        for i=1 to length(ih) do
            Ihandle ihi = ih[i]
            c_proc(xIupRedraw, {ihi,children})
        end for
    else
        c_proc(xIupRedraw, {ih,children})
    end if
end procedure

global function IupConvertXYToPos(Ihandle ih, integer x, integer y)
    integer pos = c_func(xIupConvertXYToPos, {ih,x,y})
    return pos
end function

--include dialog.e
--constant
--  xIupDialog  = iup_c_func(iup, "IupDialog", {P},P),
--  xIupPopup   = iup_c_func(iup, "IupPopup", {P,I,I},I),
--  xIupShow    = iup_c_func(iup, "IupShow", {P},I),
--  xIupShowXY  = iup_c_func(iup, "IupShowXY", {P,I,I},I),
--X xIupHide    = iup_c_proc(iup, "IupHide", {P})

--global function IupDialog(Ihandln child=NULL, object action=NULL, object func=NULL, sequence attributes="", dword_seq data={})
--  {action,func,attributes,data} = paranormalise(action,func,attributes,data)
--global function IupDialog(Ihandln child=NULL, string attributes="", dword_seq args={}, bool bEsc=true)
global function IupDialog(Ihandln child=NULL, object attributes="", args={}, bool bEsc=true)
    Ihandle ih = c_func(xIupDialog, {child})
--  if func!=NULL and action!=NULL then
--      IupSetCallback(ih, action, func)
--  end if
    if bool(attributes) then
        -- map IupDialog(child,false) to IupDialog(child,bEsc:=false)
        --  - which of course equates to IupDialog(child,"",{},false)
        if args!={} then ?9/0 end if
        if bEsc!=true then ?9/0 end if
        bEsc = attributes
        attributes = ""
    else
        if not string(attributes) then ?9/0 end if
        if bool(args) then
            -- map IupDialog(child,attr,false), ie args param omitted,
            --  to IupDialog(child,attr,bEsc:=false)
            if bEsc!=true then ?9/0 end if
            bEsc = args
            args = {}
        end if
        if not dword_seq(args) then ?9/0 end if
        if length(attributes) then
            IupSetAttributes(ih, attributes, args)
        end if
    end if
    if bEsc then IupCloseOnEscape(ih) end if
    return ih
end function

global procedure IupPopup(Ihandle ih, integer x=IUP_CURRENT, integer y=IUP_CURRENT)
    if c_func(xIupPopup, {ih,x,y})!=IUP_NOERROR then ?9/0 end if
end procedure

--DEV doc... (and tidy IupConfigDialogShow...)
--global procedure IupShow(Ihandle ih)
--  integer r = c_func(xIupShow, {ih})
--  if r!=IUP_NOERROR then ?9/0 end if      -- (r==1 is just IUP_ERROR...)
global procedure IupShow(Ihandle ih, integer x=IUP_CURRENT, y=IUP_CURRENT)
    integer err = c_func(xIupShowXY, {ih, x, y})
    if err!=IUP_NOERROR then ?9/0 end if    -- (r==1 is just IUP_ERROR...)
    IupSetAttribute(ih,"RASTERSIZE",NULL)
end procedure

global procedure IupShowXY(Ihandle ih, integer x=IUP_CURRENT, y=IUP_CURRENT)
    integer err = c_func(xIupShowXY, {ih, x, y})
    if err!=IUP_NOERROR then ?9/0 end if
    IupSetAttribute(ih,"RASTERSIZE",NULL)
end procedure

global function IupAlarm(string title, string msg, string b1, nullable_string b2=NULL, nullable_string b3=NULL)
    return c_func(xIupAlarm, {title,msg,b1,b2,b3})
end function

global procedure IupMessage(nullable_string title=NULL, nullable_string msg=NULL, dword_seq args={}, bool bWrap=true)
    if iup=NULL then iup_init1(NULL) end if
    if length(args) then
        msg = sprintf(msg, args)
    end if
    if string(msg) and find('\n',msg) and bWrap then
        -- make each paragraph a single line, improves wordwrap
        -- (note: this may be a windows only thing, not yet tested on lnx)
        msg = substitute(msg,"\n\n","\r\r")
        msg = substitute(msg,"\n"," ")
        msg = substitute(msg,"  "," ")
        msg = substitute(msg,"\r\r","\n\n")
    end if
    c_proc(xIupMessage, {title,msg})
end procedure

global procedure IupMessageError(Ihandln parent, string message, bool bWrap=true)
    if iup=NULL then iup_init1(NULL) end if
    if bWrap and find('\n',message) then
        -- make each paragraph a single line, improves wordwrap
        -- (note: this may be a windows only thing, not yet tested on lnx)
        message = substitute(message,"\n\n","\r\r")
        message = substitute(message,"\n"," ")
        message = substitute(message,"  "," ")
        message = substitute(message,"\r\r","\n\n")
    end if
    c_proc(xIupMessageError, {parent, message})
end procedure

global function IupMessageAlarm(Ihandln parent, nullable_string title, string message, buttons)
    integer res = c_func(xIupMessageAlarm, {parent, title, message, buttons})
    return res
end function

global function IupMessageDlg()
    Ihandle ih = c_func(xIupMessageDlg, {})
    return ih
end function

global function IupCalendar(object action=NULL, object func=NULL, sequence attributes="", dword_seq args={})
    Ihandle ih = c_func(xIupCalendar, {})
    {action,func,attributes,args} = paranormalise(action,func,attributes,args)
    if func!=NULL then
        if action=NULL then
            action = "VALUECHANGED_CB"
        end if
        IupSetCallback(ih, action, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupColorDlg()
    Ihandle ih = c_func(xIupColorDlg, {})
    return ih
end function

global function IupDatePick(string attributes="", sequence args={})
    Ihandle ih = c_func(xIupDatePick, {})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
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
    atom pRGB=allocate(3),
         pR=pRGB,
         pG=pRGB+1,
         pB=pRGB+2
    poke(pR, {r,g,b})
    boolean result = c_func(xIupGetColor, {x, y, pR, pG, pB})
    sequence res = result&peek({pRGB,3})
    free(pRGB)
    return res
end function

global function IupGetFile(string filefilter)
-- filefilter is eg "../docs/*.txt".
    atom pFilename = allocate(4096) -- (NB: xIupGetFile param is InOut)
    poke(pFilename, filefilter & 0)
    string filename
    if c_func(xIupGetFile, {pFilename})=-1 then
        filename = ""
    else
        filename = peek_string(pFilename)
    end if
    free(pFilename)
    return filename
end function

global function IupGetParam(string title, cbfunc action, atom user_data, string fmt, sequence param_data={})
integer fskip = 0
integer param_count = 0
integer param_extra = 0
string fmts = ""
    IupControlsOpen()
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

global function IupGetText(string title, text, integer maxsize=10240)
    if maxsize=-1 then
        {} = c_func(xIupGetText, {title, text, -1})
        return ""
    end if
    text &= '\0'
    if maxsize=0 then
        maxsize = length(text)
    elsif length(text)>=maxsize then
        ?9/0 -- (prevent memory corruption!)
    end if  
    atom pText = allocate(maxsize)
    poke(pText, text)
    if c_func(xIupGetText, {title, pText, maxsize})=0 then
        text = ""
    else
        text = peek_string(pText)
    end if
    free(pText)
    return text
end function

global function IupListDialog(integer seltype, string title, sequence options, integer isel, integer maxCols, integer maxLines)
    if seltype!=1 and seltype!=2 then ?9/0 end if
    integer size = length(options), result
    sequence pOptAry = {}, marked, selected = {}
    atom pOptions = allocate(W*size),
         pMark = NULL

    for i=1 to size do
--      pOptAry &= allocate_string(options[i],1)
        pOptAry &= IupRawStringPtr(options[i])
    end for
    poke4(pOptions, pOptAry)
    if seltype=2 then
        -- multiple selection
        pMark = allocate(4*size)    --DEV W? [test on 64 bit]
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

global function IupLayoutDialog(Ihandln dialog)
    Ihandle ih = c_func(xIupLayoutDialog, {dialog})
    return ih
end function

global function IupGlobalsDialog(sequence attributes="", dword_seq args={})
    Ihandle ih = c_func(xIupGlobalsDialog, {})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupButton(object title=NULL, object action=NULL, object func=NULL, sequence attributes="", dword_seq args={})
    Ihandle ih = c_func(xIupButton, {title, action})
    {action,func,attributes,args} = paranormalise(action,func,attributes,args)
    if func!=NULL then
        if action=NULL then
            action = "ACTION"
        end if
        IupSetCallback(ih, action, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupCanvas(object action=NULL, object func=NULL, sequence attributes="", dword_seq args={})
    Ihandle ih = c_func(xIupCanvas, {action})
    {action,func,attributes,args} = paranormalise(action,func,attributes,args)
    if func!=NULL then
        if action=NULL then
            action = "ACTION"
        end if
        IupSetCallback(ih, action, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

--DEV: IupFlatFrame

global function IupFrame(Ihandln child=NULL, string attributes="", sequence args={})
    Ihandle ih = c_func(xIupFrame, {child})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupFlatFrame(Ihandln child=NULL, string attributes="", sequence args={})
    Ihandle ih = c_func(xIupFlatFrame, {child})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function


--DEV: IupAnimatedLabel

--global function IupLabel(nullable_string title=NULL, string attributes="", sequence args={})
--global function IupLabel(object title=NULL, object action=NULL, object func=NULL, sequence attributes="", dword_seq args={})
--  {action,func,attributes,args} = paranormalise(action,func,attributes,args)
global function IupLabel(nullable_string title=NULL, string attributes="", dword_seq args={})
    Ihandle ih = c_func(xIupLabel, {title})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupFlatLabel(nullable_string title=NULL, string attributes="", dword_seq args={})
    Ihandle ih = c_func(xIupFlatLabel, {title})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupFlatList(string attributes="", dword_seq args={})
    Ihandle ih = c_func(xIupFlatList, {})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupFlatSeparator(string attributes="", dword_seq args={})
    Ihandle ih = c_func(xIupFlatSeparator, {})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupList(object action=NULL, object func=NULL, sequence attributes="", dword_seq args={})
    Ihandle ih = c_func(xIupList, {NULL})
    {action,func,attributes,args} = paranormalise(action,func,attributes,args)
    if func!=NULL then
        if action=NULL then
            action = "ACTION"
        end if
        IupSetCallback(ih, action, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupProgressBar(sequence attributes="", dword_seq args={})
    Ihandle ih = c_func(xIupProgressBar, {})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupGauge(sequence attributes="", dword_seq args={})
    Ihandle ih = c_func(xIupGauge, {})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupSpin()
    Ihandle ih = c_func(xIupSpin, {})
    return ih
end function

global function IupSpinBox(Ihandln child=NULL, object action=NULL, object func=NULL, sequence attributes="", dword_seq args={})
    Ihandle ih = c_func(xIupSpinbox, {child})
    {action,func,attributes,args} = paranormalise(action,func,attributes,args)
    if func!=NULL then
        if action=NULL then
            action = "SPIN_CB"
        end if
        IupSetCallback(ih, action, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupSpinbox(Ihandln child=NULL, object action=NULL, object func=NULL, sequence attributes="", dword_seq args={})
    return IupSpinBox(child, action, func, attributes, args)
end function

global function IupTabs(Ihandles children={}, sequence attributes="", dword_seq args={})
atom pChildren = iup_ptr_array(children)
    Ihandle ih = c_func(xIupTabsv, {pChildren})
    free(pChildren)
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupFlatTabs(Ihandles children={}, sequence attributes="", dword_seq args={})
    atom pChildren = iup_ptr_array(children)
    Ihandle ih = c_func(xIupFlatTabsv, {pChildren})
    free(pChildren)
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupText(object action=NULL, object func=NULL, sequence attributes="", dword_seq args={})
    Ihandle ih = c_func(xIupText, {NULL})
    {action,func,attributes,args} = paranormalise(action,func,attributes,args)
    if func!=NULL then
        if action=NULL then
            action = "ACTION"
        end if
        IupSetCallback(ih, action, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupMultiLine(object action=NULL, object func=NULL, sequence attributes="", dword_seq args={})
    Ihandle ih = c_func(xIupMultiLine, {NULL})
    {action,func,attributes,args} = paranormalise(action,func,attributes,args)
    if func!=NULL then
        if action=NULL then
            action = "ACTION"
        end if
        IupSetCallback(ih, action, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupTextConvertLinColToPos(Ihandle ih, integer lin, integer col)
    atom pPos = allocate(4)
    c_proc(xIupTextConvertLinColToPos, {ih,lin,col,pPos})
    integer pos = peek4s(pPos)
    free(pPos)
    return pos
end function

global function IupTextConvertPosToLinCol(atom ih, atom pos)
    atom pLineCol = allocate(16)
    c_proc(xIupTextConvertPosToLinCol, {ih,pos,pLineCol,pLineCol+4})
    sequence res = peek4s({pLineCol,2}) -- integer {lin, col}
    free(pLineCol)
    return res
end function

global function IupToggle(nullable_string title=NULL, object action=NULL, object func=NULL, sequence attributes="", dword_seq args={})
    Ihandle ih = c_func(xIupToggle, {title, NULL})
    {action,func,attributes,args} = paranormalise(action,func,attributes,args)
    if func!=NULL then
        if action=NULL then
            action = "ACTION"
        end if
        IupSetCallback(ih, action, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupFlatToggle(nullable_string title=NULL, object action=NULL, object func=NULL, sequence attributes="", dword_seq args={})
    Ihandle ih = c_func(xIupFlatToggle, {title, NULL})
    {action,func,attributes,args} = paranormalise(action,func,attributes,args)
    if func!=NULL then
        if action="ACTION" then ?9/0 end if -- see docs!
        if action=NULL then
            action = "FLAT_ACTION"
        end if
        IupSetCallback(ih, action, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupTree(string attributes="", dword_seq args={})
    Ihandle ih = c_func(xIupTree, {})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupFlatTree(string attributes="", dword_seq args={})
    Ihandle ih = c_func(xIupFlatTree, {})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupValuator(nullable_string orientation=NULL, object action=NULL, object func=NULL, sequence attributes="", dword_seq args={})
    Ihandle ih = c_func(xIupVal, {orientation})
    {action,func,attributes,args} = paranormalise(action,func,attributes,args)
    if func!=NULL then
        if action=NULL then
            action = "ACTION"
        end if
        IupSetCallback(ih, action, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupVal(nullable_string orientation=NULL, object action=NULL, object func=NULL, sequence attributes="", dword_seq args={})
    return IupValuator(orientation, action, func, attributes, args)
end function

global function IupFlatValuator(nullable_string orientation=NULL, string attributes="", dword_seq args={})
    Ihandle ih = c_func(xIupFlatVal, {orientation})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupFlatVal(nullable_string orientation=NULL, string attributes="", dword_seq args={})
    return IupFlatValuator(orientation, attributes, args)
end function


--global constant -- function delcarations
--  xIupConfig                        = iup_c_func(iup, "IupConfig", {}, P),
--  xIupConfigLoad                    = iup_c_func(iup, "IupConfigLoad", {P}, I),
--  xIupConfigSave                    = iup_c_func(iup, "IupConfigSave", {P}, I),
--  xIupConfigSetVariableInt          = iup_c_proc(iup, "IupConfigSetVariableInt", {P,P,P,I}),
--  xIupConfigSetVariableIntId        = iup_c_proc(iup, "IupConfigSetVariableIntId", {P,P,P,I,I}),
--  xIupConfigSetVariableDouble       = iup_c_proc(iup, "IupConfigSetVariableDouble", {P,P,P,D}),
--  xIupConfigSetVariableDoubleId     = iup_c_proc(iup, "IupConfigSetVariableDoubleId", {P,P,P,I,D}),
--  xIupConfigSetVariableStr          = iup_c_proc(iup, "IupConfigSetVariableStr", {P,P,P,P}),
--  xIupConfigSetVariableStrId        = iup_c_proc(iup, "IupConfigSetVariableStrId", {P,P,P,I,P}),
----    xIupConfigGetVariableInt          = iup_c_func(iup, "IupConfigGetVariableInt", {P,P,P}, I),
--  xIupConfigGetVariableIntDef       = iup_c_func(iup, "IupConfigGetVariableIntDef", {P,P,P,I}, I),
----    xIupConfigGetVariableIntId        = iup_c_func(iup, "IupConfigGetVariableIntId", {P,P,P,I}, I),
--  xIupConfigGetVariableIntIdDef     = iup_c_func(iup, "IupConfigGetVariableIntIdDef", {P,P,P,I,I}, I),
----    xIupConfigGetVariableDouble       = iup_c_func(iup, "IupConfigGetVariableDouble", {P,P,P}, D),
--  xIupConfigGetVariableDoubleDef    = iup_c_func(iup, "IupConfigGetVariableDoubleDef", {P,P,P,D}, D),
----    xIupConfigGetVariableDoubleId     = iup_c_func(iup, "IupConfigGetVariableDoubleId", {P,P,P,I}, D),
--  xIupConfigGetVariableDoubleIdDef  = iup_c_func(iup, "IupConfigGetVariableDoubleIdDef", {P,P,P,I,D}, D),
----    xIupConfigGetVariableStr          = iup_c_func(iup, "IupConfigGetVariableStr", {P,P,P}, P),
--  xIupConfigGetVariableStrDef       = iup_c_func(iup, "IupConfigGetVariableStrDef", {P,P,P,P}, P),
----    xIupConfigGetVariableStrId        = iup_c_func(iup, "IupConfigGetVariableStrId", {P,P,P,I}, P),
--  xIupConfigGetVariableStrIdDef     = iup_c_func(iup, "IupConfigGetVariableStrIdDef", {P,P,P,I,P}, P),
--  xIupConfigRecentInit              = iup_c_proc(iup, "IupConfigRecentInit", {P,P,P,I}),
--  xIupConfigRecentUpdate            = iup_c_proc(iup, "IupConfigRecentUpdate", {P,P}),
--  xIupConfigDialogShow              = iup_c_proc(iup, "IupConfigDialogShow", {P,P,P}),
--  xIupConfigDialogClosed            = iup_c_proc(iup, "IupConfigDialogClosed", {P,P,P}),
--  $

global function IupConfig()
    Ihandle config = c_func(xIupConfig, {})
    return config
end function

global procedure IupConfigCopy(Ihandle config1, config2, nullable_string exclude_prefix=NULL)
    c_proc(xIupConfigCopy, {config1,config2,exclude_prefix})
end procedure

global function IupConfigLoad(Ihandle config)
    integer errcode = c_func(xIupConfigLoad, {config})
    return errcode  -- 0=no error; -1=error opening the file; -2=error accessing the file; -3=error during filename construction
end function

global function IupConfigSave(Ihandle config)
    integer errcode = c_func(xIupConfigSave, {config})
    return errcode  -- 0=no error; -1=error opening the file; -2=error accessing the file; -3=error during filename construction
end function

global procedure IupConfigSetVariableInt(Ihandle config, string group, string key, integer v)
    c_proc(xIupConfigSetVariableInt, {config,group,key,v})
end procedure

global procedure IupConfigSetVariableIntId(Ihandle config, string group, string key, integer id, integer v)
    c_proc(xIupConfigSetVariableIntId, {config,group,key,id,v})
end procedure

global procedure IupConfigSetVariableDouble(Ihandle config, string group, string key, atom v)
    c_proc(xIupConfigSetVariableDouble, {config,group,key,v})
end procedure

global procedure IupConfigSetVariableDoubleId(Ihandle config, string group, string key, integer id, atom v)
    c_proc(xIupConfigSetVariableDoubleId, {config,group,key,id,v})
end procedure

global procedure IupConfigSetVariableStr(Ihandle config, string group, string key, nullable_string v)
    if v="" then v=NULL end if
    c_proc(xIupConfigSetVariableStr, {config,group,key,v})
end procedure

global procedure IupConfigSetVariableStrId(Ihandle config, string group, string key, integer id, nullable_string v)
    if v="" then v=NULL end if
    c_proc(xIupConfigSetVariableStrId, {config,group,key,id,v})
end procedure

global function IupConfigGetVariableInt(Ihandle config, string group, string key, integer def=0)
    integer res = c_func(xIupConfigGetVariableIntDef, {config,group,key,def})
    return res
end function

global function IupConfigGetVariableIntId(Ihandle config, string group, string key, integer id, integer def=0)
    integer res = c_func(xIupConfigGetVariableIntIdDef, {config,group,key,id,def})
    return res
end function

global function IupConfigGetVariableDouble(Ihandle config, string group, string key, atom def=0)
    atom res = c_func(xIupConfigGetVariableDoubleDef, {config,group,key,def})
    return res
end function

global function IupConfigGetVariableDoubleId(Ihandle config, string group, string key, integer id, atom def=0)
    atom res = c_func(xIupConfigGetVariableDoubleIdDef, {config,group,key,id,def})
    return res
end function

global function IupConfigGetVariableStr(Ihandle config, string group, string key, nullable_string def=NULL)
    atom pString = c_func(xIupConfigGetVariableStrDef, {config,group,key,def})
    string res = iff(pString=NULL?"":peek_string(pString))
    return res
end function

global function IupConfigGetVariableStrId(Ihandle config, string group, string key, integer id, nullable_string def=NULL)
    atom pString = c_func(xIupConfigGetVariableStrIdDef, {config,group,key,id,def})
    string res = iff(pString=NULL?"":peek_string(pString))
    return res
end function

global procedure IupConfigRecentInit(Ihandle config, Ihandle menu, cbfunc recent_cb, integer max_recent)
    c_proc(xIupConfigRecentInit, {config,menu,recent_cb,max_recent})
end procedure

global procedure IupConfigRecentUpdate(Ihandle config, string filename)
    c_proc(xIupConfigRecentUpdate, {config,filename})
end procedure

--DEV revert to this if needed:
global procedure IupConfigDialogShow0(Ihandle config, Ihandle dialog, string name, boolean maximised=false)
--integer w = IupConfigGetVariableInt(config, name, "Width", NULL)
--integer m = IupConfigGetVariableInt(config, name, "Maximized", maximised)
integer m = IupConfigGetVariableInt(config, name, "MaxiSized", maximised)
--  IupConfigSetVariableInt(config, name, "Maximized", m)
    IupConfigSetVariableInt(config, name, "MaxiSized", m)
--if m or w then
    c_proc(xIupConfigDialogShow, {config,dialog,name})
    if m then
        c_proc(xIupHide,{dialog})
        c_proc(xIupSetAttribute,{dialog,"PLACEMENT","MAXIMIZED"})
--      c_proc(xIupSetAttribute,{dialog,"PLACEMENT",iff(m?"MAXIMIZED":"NORMAL")})
--end if
        if c_func(xIupShow,{dialog})!=IUP_NOERROR then ?9/0 end if
    end if
end procedure

global procedure IupConfigDialogShow(Ihandle config, Ihandle dialog, string name, boolean maximised=false)
integer w = IupConfigGetVariableInt(config, name, "Width", NULL)
--integer m = IupConfigGetVariableInt(config, name, "Maximized", maximised)
integer m = IupConfigGetVariableInt(config, name, "MaxiSized", maximised)
--  IupConfigSetVariableInt(config, name, "Maximized", m)
    IupConfigSetVariableInt(config, name, "MaxiSized", m)
    if m or w then
        c_proc(xIupConfigDialogShow, {config,dialog,name})
        if m then
            c_proc(xIupHide,{dialog})
            c_proc(xIupSetAttribute,{dialog,"PLACEMENT","MAXIMIZED"})
--          c_proc(xIupSetAttribute,{dialog,"PLACEMENT",iff(m?"MAXIMIZED":"NORMAL")})
        end if
    end if
    if c_func(xIupShow,{dialog})!=IUP_NOERROR then ?9/0 end if
end procedure

global procedure IupConfigDialogClosed(Ihandle config, Ihandle dialog, string name)
integer m = IupGetInt(dialog,"MAXIMIZED")   -- (windows-only; 0 elsewhere)
--  IupConfigSetVariableInt(config, name, "Maximized", m)
    if m=0 then
        c_proc(xIupConfigDialogClosed, {config,dialog,name})
    end if
    IupConfigSetVariableInt(config, name, "MaxiSized", m)
end procedure

global function IupExecute(string filename, nullable_string parameters=NULL)
    integer res = c_func(xIupExecute,{filename,parameters})
    return res
end function

global function IupExecuteWait(string filename, nullable_string parameters=NULL)
    integer res = c_func(xIupExecuteWait,{filename,parameters})
    return res
end function

global procedure IupLog(string typ, fmt, sequence args={})
    if length(args) then
        fmt = sprintf(fmt,args)
    end if
    c_proc(xIupLog, {typ,"%s",fmt})
end procedure

--****
-- === Additional
--

global function IupCells(string attributes="", dword_seq args={})
    IupControlsOpen()
    Ihandle ih = c_func(xIupCells, {})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupColorbar(string attributes="", dword_seq args={})
    IupControlsOpen()
    Ihandle ih = c_func(xIupColorbar, {})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupColorBrowser(string attributes="", dword_seq args={})
    IupControlsOpen()
    Ihandle ih = c_func(xIupColorBrowser, {})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupDial(string orientation, string attributes="", dword_seq args={})
    IupControlsOpen()
    Ihandle ih = c_func(xIupDial, {orientation})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupMatrix(string attributes="", sequence args={})
    IupControlsOpen()
    Ihandle ih = c_func(xIupMatrix, {ACTION_CB})
--DEV tryme:
--  Ihandle ih = c_func(xIupMatrix, {NULL})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

--atom
--  iupMatrixEx = 0,
--
--  xIupMatrixEx
----    xIupMatrixExInit,
----    xIupMatrixExOpen
--
--procedure iup_init_matrix()
--  IupControlsOpen()
--  if iupMatrixEx=0 then
--      iupMatrixEx = iup_open_dll({
--                                  "iupmatrixex.dll",
--                                  "libiupmatrixex.so",
--                                  "libiupmatrixex.dylib"
--                                 })
--
--      xIupMatrixEx            = iup_c_func(iupMatrixEx, "IupMatrixEx", {},P)
----        xIupMatrixExInit        = iup_c_proc(iupMatrixEx, "IupMatrixExInit", {P})
----        xIupMatrixExOpen        = iup_c_proc(iupMatrixEx, "IupMatrixExOpen", {})
--  end if
--end procedure

--integer matrixexopen = 0
--global procedure IupMatrixExOpen()
--  iup_init_matrix()
--  if not matrixexopen then
--      c_proc(xIupMatrixExOpen,{})
--      matrixexopen = 1
--  end if
--end procedure

global function IupMatrixEx()
--  IupMatrixExOpen()
    Ihandle res = c_func(xIupMatrixEx,{})
    return res
end function

global function IupMatrixList()
--  IupMatrixExOpen()
    Ihandle res = c_func(xIupMatrixList,{})
    return res
end function

--global procedure IupMatrixExInit(Ihandle ih)
--  IupMatrixExOpen()
--  c_proc(xIupMatrixExInit,{ih})
--end procedure

global procedure IupMatSetAttribute(Ihandle ih, string name, integer lin, integer col, atom_string v)
--  c_proc(xIupMatSetAttribute, {ih, name, lin, col, v})
    c_proc(xIupSetAttributeId2, {ih, name, lin, col, v})
end procedure

global procedure IupMatStoreAttribute(Ihandle ih, string name, integer lin, integer col, string val, sequence args={})
    if length(args) then
        val = sprintf(val, args)
    end if
--  c_proc(xIupMatStoreAttribute, {ih, name, lin, col, val})
    c_proc(xIupSetStrAttributeId2, {ih, name, lin, col, val})
end procedure

global function IupMatGetAttribute(Ihandle ih, string name, integer lin, integer col)
--  atom pValue = c_func(xIupMatGetAttribute, {ih, name, lin, col})
    atom pValue = c_func(xIupGetAttributeId2, {ih, name, lin, col})
    string res = iff(pValue=NULL?"":peek_string(pValue))
    return res
end function

global function IupMatGetInt(Ihandle ih, string name, integer lin, integer col)
--  integer val = c_func(xIupMatGetInt, {ih, name, lin, col})
    integer val = c_func(xIupGetIntId2, {ih, name, lin, col})
    return val
end function

--function IupMatGetFloat(atom ih, object name = NULL, atom lin, atom col)
global function IupMatGetFloat(Ihandle ih, string name, integer lin, integer col)
--  atom val = c_func(xIupMatGetFloat, {ih, name, lin, col})
    atom val = c_func(xIupGetFloatId2, {ih, name, lin, col})
    return val
end function

--
-- IupTable support:
--
--global? (no)
sequence table_datasets = {},
         table_sortcols = {},
         table_sortdirs = {},
         table_tagsets = {},
         table_titles = {},
         table_widths = {}

string semiperm -- (return value of IupTableValue_cb must outlive it)

function IupTableValue_cb(Ihandle table, integer l, integer c)
-- internal, for IupTable (VALUE_CB, non-overrideable)
    if c>0 then
        integer dsidx = IupGetInt(table,"DSIDX")
        sequence data = table_datasets[dsidx]
        if l>length(data[1]) then return NULL end if
        if l==0 then
            return IupRawStringPtr(table_titles[dsidx][c])  -- column title
        end if
        l = table_tagsets[dsidx][l]
        sequence dl = data[1][l]
        if c>length(dl) then return NULL end if
        object dlc = dl[c]
        if c<=length(data[2]) then
            object d2c = data[2][c]
            if sequence(d2c) then
                dlc = d2c[l]
            elsif d2c!=0 then
                integer fn = d2c
--              semiperm = fn(dlc)
                semiperm = fn(data[1],l,c)
                return IupRawStringPtr(semiperm)
            end if
        end if
        if string(dlc) then
            if length(dlc)=0 then return NULL end if
            return IupRawStringPtr(dlc)
        end if
        semiperm = sprint(dlc)
        return IupRawStringPtr(semiperm)
    end if
    return NULL
end function

global function IupTableEnterItem_cb(Ihandle table, integer lin, integer col)
-- default ENTERITEM_CB for IupTable (callable directly, when overridden)
    IupSetAttribute(table,"MARKED", NULL)   /* clear all marks */
    IupMatSetAttribute(table,"MARK", lin, 0, "Yes")
    IupSetStrAttribute(table,"REDRAW", "L%d", {lin})
    IupSetStrAttribute(table,"FOCUSCELL", "%d:%d", {lin,col})
    return IUP_DEFAULT
end function

global function IupTableGetSelected(Ihandle table)
    integer idx = IupGetInt(table,"FOCUSCELL") -- (line only)
    if idx!=0 then
        integer dsidx = IupGetInt(table,"DSIDX")
        idx = table_tagsets[dsidx][idx]
    end if
    return idx 
end function

integer dsidx = 0 -- (for gui-thread-only use)

function by_column(integer i, integer j)
-- internal, for IupTable (sort, non-overrideable)
    sequence data = table_datasets[dsidx][1],
             cols = table_sortcols[dsidx],
             dirs = table_sortdirs[dsidx],
             di = data[i],
             dj = data[j]
    integer c = 0
    for k=1 to length(cols) do
        c = cols[k]
        object dic = iff(c<=length(di)?di[c]:0),
               djc = iff(c<=length(dj)?dj[c]:0)
        c = dirs[k]*compare(dic,djc)
        if c!=0 then exit end if
    end for
    if c=0 then c=compare(i,j) end if -- original order
    return c
end function

global function IupTableClick_cb(Ihandle table, integer l, integer c, atom pStatus)
-- default CLICK_CB for IupTable (callable directly, when overridden)
    integer numcol = IupGetInt(table,"NUMCOL")
    if l=0 and c>0 and c<=numcol then
        dsidx = IupGetInt(table,"DSIDX")
        sequence data = table_datasets[dsidx],
                 cols = table_sortcols[dsidx]
        integer sel = IupTableGetSelected(table),
                  k = find(c,cols)
        integer sortcol = iff(length(cols)?cols[1]:0)
        if k=1 then
            table_sortdirs[dsidx][1] *= -1
        else
            if k then
                table_sortcols[dsidx][k..k] = {}
                table_sortdirs[dsidx][k..k] = {}
            end if
            table_sortcols[dsidx] = prepend(table_sortcols[dsidx],c)
            table_sortdirs[dsidx] = prepend(table_sortdirs[dsidx],1)
        end if
        if sortcol!=0 and sortcol!=c then
            IupSetAttributeId(table,"SORTSIGN",sortcol,"NO")
        end if
        integer sortdir = iff(IupGetAttributeId(table,"SORTSIGN",c)="DOWN"?-1:1)
        IupSetAttributeId(table,"SORTSIGN",c,iff(sortdir=-1?"UP":"DOWN"))
        table_tagsets[dsidx] = custom_sort(routine_id("by_column"),table_tagsets[dsidx])
        IupSetAttribute(table,"REDRAW","ALL")
        -- restore selection - it stays off-screen, but user can use up/down
        --  to force it into the viewport, should they be inclined to do so.
        sel = find(sel,table_tagsets[dsidx])
        {} = IupTableEnterItem_cb(table, sel, 1)
    end if
    return IUP_DEFAULT
end function

function IupTableColResize_cb(Ihandle table, integer col)
-- internal only (COLRESIZE_CB)
    integer dsidx = IupGetInt(table,"DSIDX"),
                w = IupGetInt(table,"NUMCOL")
    if col<w then
        integer thisw = IupGetIntId(table,"RASTERWIDTH",col),
                nextw = IupGetIntId(table,"RASTERWIDTH",col+1),
                diff = table_widths[dsidx][col] - thisw,
                next = table_widths[dsidx][col+1] + diff
        IupSetIntId(table,"RASTERWIDTH",col+1,next)
        table_widths[dsidx][col] = thisw
        table_widths[dsidx][col+1] = next
    else
        -- prevent right hand column resizing:
        integer prev = table_widths[dsidx][col]
        IupSetIntId(table,"RASTERWIDTH",col,prev)
    end if
    return IUP_IGNORE
end function

global function IupTableResize_cb(Ihandle dlg, integer width, /*height*/)
-- default RESIZE_CB for IupTable (callable directly, when overridden)
--width -= 40
    Ihandle table = IupGetAttributePtr(dlg,"TABLE"),
            parent = IupGetParent(table)
--?{"RESIZE",dlg,parent,table}
    integer dsidx = IupGetInt(table,"DSIDX"),
            w = IupGetInt(table,"NUMCOL"),
            h = IupGetInt(table,"NUMLIN"),
--          ty = h*23+28
--          ty = h*23+29
            ty = h*23+30 -- (see notes below)
    sequence new_widths = repeat(0,w)
    for i=1 to w do
        new_widths[i] = IupGetIntId(table,"RASTERWIDTH",i)
    end for
    --
    -- aside: buglette: at one or two sizes this leaves space for a 
    --        scrollbar which never appears. (h*23+28) is probably 
    --        more accurate, however windows can then decide to
    --        display an unnecessary horizontal scrollbar, which
    --        is even uglier and also slightly harder to remove,
    --        that is by jiggling the mouse and/or approaching the
    --        critical limit point from a different direction.
    --        There may or may not be a slighly better setting of
    --        +28/9/30 and (less likely) *8+17* which I missed...
    --        Use demo/pGUI/IupTable.exw to test any changes (as
    --        well as whatever it is that you're working on).
    --
    integer {cx,cy} = IupGetIntInt(parent,"CLIENTSIZE"),
            {mx,my} = IupGetIntInt(parent,"CMARGIN"),
            vs = (cy<ty) -- vertical scrollbar?
--  width -= (w*8+17*vs) -- ( derived by trial and error... )
--  width = cx-mx-(w*8+17*vs) -- ( derived by trial and error... )
--  width = cx-max(mx,0)-(w*8+17*vs) -- ( derived by trial and error... )
    width -= mx*4+(w*8+17*vs) -- ( derived by trial and error... )
    if width<120 then width = 120 end if
    integer total_width = sum(new_widths)
    IupSetInt(table,"RASTERWIDTH0",0)
    for i=1 to w do
        integer new_width = max(round((new_widths[i]/total_width)*width),10)
        total_width -= new_widths[i]
              width -= new_width
        IupSetIntId(table,"RASTERWIDTH",i,new_width)
        new_widths[i] = new_width
    end for
--  IupSetInt(table,"RASTERWIDTH",sum(new_widths)+vs*17) -- (no help)
    table_widths[dsidx] = new_widths -- save for IupTableColResize_cb
/*
integer visible = floor((cy-25)/23)
?{"cy",cy,"ny",ny,"NUMLIN_VISIBLE",IupGetInt(table, "NUMLIN_VISIBLE"),visible}
--?{"cy",cy,"ny",ny,"NUMLIN_VISIBLE",IupGetInt(table, "NUMLIN"),visible}
    IupSetInt(table, "NUMLIN_VISIBLE", visible)
    IupRefresh(table)
--IupResetAttribute(table, "NUMLIN_VISIBLE")
*/
--IupSetInt
    return IUP_DEFAULT
end function

function IupTableMap_cb(Ihandle table)
-- internal only (MAP_CB)
-- nb only suitable for the single-table case, with multiple you
-- must override RESIZE_CB and invoke IupTableResize_cb directly.
-- In most cases, parent = IupDialog() has not been invoked at the point
-- when you invoke table = IupTable(), which is why this is separate.
--/*
    Ihandle parent = IupGetParent(table)
    Ihandln grandp = IupGetParent(parent)
    while grandp!=NULL do
        parent = grandp
        grandp = IupGetParent(parent)
    end while
--*/
    Ihandle parent = IupGetDialog(table)
--?{table,parent}
    IupSetAttributePtr(parent,"TABLE",table)
    IupSetCallback(parent, "RESIZE_CB", Icallback("IupTableResize_cb"))
    return IUP_DEFAULT
-- makes it worse!!
--  integer {width} = IupGetIntInt(parent,"CLIENTSIZE")
--  return IupTableResize_cb(parent,width,0)
end function

function IupTableMouseMove_cb(Ihandle table, integer lin, /*col*/)
    integer hln = IupGetInt(table,"HOVERLINE") -- (pGUI-specific)
    if lin!=hln then
        if hln!=0 then
            IupSetAttribute(table, sprintf("BGCOLOR%d:*",hln), "#FFFFFF")
            IupSetStrAttribute(table,"REDRAW", "L%d", {hln})
        end if
        if lin!=0 then
            IupSetAttribute(table, sprintf("BGCOLOR%d:*",lin), "#e6f7ff")
            IupSetStrAttribute(table,"REDRAW", "L%d", {lin})
        end if
        IupSetInt(table,"HOVERLINE",lin)
    end if
    return IUP_DEFAULT
end function

function IupTableLeaveWindow_cb(Ihandle table)
    return IupTableMouseMove_cb(table,0,0)
end function
    
global function IupTable(sequence columns, data, integer visible=10, sequence attributes="", dword_seq args={})
--function IupTable(sequence columns, data, integer visible=10, sequence attributes="", args={})
--
-- columns should be eg {{"Chq#",40,"ARIGHT"}
--                       {"Date",60,"ACENTER"},
--                       {"Amount",100,"ARIGHT"},
--                       {"Status",50,"ACENTER"},
--                       {"Bank",50,"ALEFT"}}
--
--         or just {"Chq#","Date","Amount","Status","Bank"},
--
-- the latter giving equal-sized columns, all left-aligned.
-- Note that IupMatrix() does not support individual column heading alignment, afaik.
--
-- data should be length 2 with length(data[1])=length(columns).
--      data[1] contains the master/sortable values.
--      data[2] is reserved for (string) display versions of data[1]:
--      if data[2] is too short or data[2][c] is atom, then data[1][c] is displayed as-is
--      for dates, data[1][c][i] should be eg 20201122 or timedate-format, data[2][c][i] string.
--      for case-insensitive sorting, data[1][c][i] should be lower(/upper)(data[2][c][i]).
--
--
-- attributes supported: SIZE?, RASTERSIZE?
--
-- callbacks supported:
--
--  CLICK_CB - If overidden, it should invoke the internal default, which implements
--              column sorting, as follows:
--              function click_cb(Ihandle ih, integer l, integer c, atom pStatus)
--                  if l=0 then return IupTableClick_cb(ih, l, c, NULL) end if
--                  ...
--                  return IUP_DEFAULT
--              end function
--
--  ENTERITEM_CB - If overidden, it should invoke the internal default, which implements
--                  proper line selection/marking/focus settings, as follows:
--                  function enteritem_cb(Ihandle table, integer lin, integer col)
--                      {} = IupTableEnterItem_cb(table,lin,col)
--                      integer idx = IupTableGetSelected(table) -- (usually rqd)
--                      ...
--                      return IUP_DEFAULT
--                  end function
--
--  RESIZE_CB - If overidden, or a dialog contains more than one IupTable, it should
--              replicate/replace the internal/private IupTableMap_cb() as follows:
--              function resize_cb(Ihandle dlg, integer width, height)
--                  for t=1 to length(tables) do -- (declared/set manually)
--                      IupSetAttributePtr(dlg,"TABLE",tables[i])
--                      {} = IupTableResize_cb(dlg, width, height)
--                  end for
--                  ...
--                  return IUP_DEFAULT
--              end function
--
--  K_ANY -- (no special requirements here)
--
--  You may NOT use or override VALUE_CB or COLRESIZE_CB or MAP_CB.
--   (You /can/ use MAP_CB on the dialog, just not on the table.)
--  It also uses MOUSEMOVE_CB and LEAVEWINDOW_CB, if ever needed the
--  internal routines could easily be made global as per CLICK_CB etc,
--  but any override implies the default hover effects are not wanted, 
--  and everything (bar said) will work just fine without them.
--  Other callbacks and attributes of IupMatrix may function in a desktop-only
--  scenario, but nothing else is yet tested or supported under pwa/p2js.
--
    integer l = length(columns),
            m = length(data[1])
    Ihandle table = IupMatrix()
    IupSetInt(table, "NUMCOL", l)
    IupSetInt(table, "NUMCOL_VISIBLE", l)
    IupSetInt(table, "NUMLIN", m)
    IupSetInt(table, "NUMLIN_VISIBLE", visible)
    IupSetInt(table, "WIDTHDEF", 40) -- (now completely overidden anyway)
--  IupSetInt(table, "SHRINK", true)
    table_titles = append(table_titles,repeat("",l))
    table_tagsets = append(table_tagsets,tagset(m))
    table_datasets = append(table_datasets,data)
    table_sortcols = append(table_sortcols,{})
    table_sortdirs = append(table_sortdirs,{})
    table_widths = append(table_widths,repeat(80,l))
    integer dsidx = length(table_datasets)
    for i=1 to l do
        if string(columns[i]) then
            table_titles[dsidx][i] = columns[i]
        else
            {string title, integer width, string align} = columns[i]
            table_titles[dsidx][i] = title
            table_widths[dsidx][i] = width
            IupSetIntId(table, "RASTERWIDTH", i, width)
            IupSetAttributeId(table, "ALIGNMENT", i, align)
            -- I tried...
--          IupSetAttribute(table, sprintf("ALIGN0:%d",i), align)
--          IupSetAttributeId(table, "ALIGNMENTLIN0",i, align)
--          IupSetAttributeId(table, "ALIGNMENTLIN0:",i, align)
--          IupSetAttribute(table, sprintf("ALIGNMENTLIN0:%d",i), align)
        end if
    end for
    --IMPORTANT: HEIGHT0 tells IupMatrix that we are gonna have column titles at line 0
    IupSetInt(table, "HEIGHT0", 10)
    IupSetAttribute(table, "RESIZEMATRIX", "YES")
    IupSetAttribute(table, "RESIZEDRAG", "YES")
    IupSetAttribute(table, "MARKMODE", "LIN")
    IupSetAttribute(table, "MARKAREA", "CONTINUOUS")
--DEV does not seem to work...
--  IupSetAttribute(table, "MULTIPLE", "YES") -- (or is that "MARKMULTIPLE"? the default is NO anyway)

    IupSetAttribute(table, "READONLY", "YES")
    IupSetAttribute(table, "HIDEFOCUS", "YES")
    --
    -- Aside: iupmat_draw.c uses "attenuation", which is equivalent to 
    --        floor((r|g|b)*8/10), so the max actual rgb (from #FF) is 
    --        #CC. Hence it is not possible to get brighter highlight.
    --
    IupSetAttribute(table, "HLCOLOR", "#00AFFF")
    IupSetAttribute(table, "FRAMECOLOR", "220 220 220")
    IupSetAttribute(table, "BORDER", "NO")
    IupSetAttribute(table, "CURSOR", "ARROW")

    IupSetInt(table,"DSIDX",dsidx)
    IupSetCallback(table, "VALUE_CB", Icallback("IupTableValue_cb"))
    IupSetCallback(table, "ENTERITEM_CB", Icallback("IupTableEnterItem_cb"))
    IupSetCallback(table, "CLICK_CB", Icallback("IupTableClick_cb"))
    IupSetCallback(table, "COLRESIZE_CB", Icallback("IupTableColResize_cb"))
    IupSetCallback(table, "MAP_CB", Icallback("IupTableMap_cb"))
--DEV add to docs:
    IupSetCallback(table, "MOUSEMOVE_CB", Icallback("IupTableMouseMove_cb"))
    IupSetCallback(table, "LEAVEWINDOW_CB", Icallback("IupTableLeaveWindow_cb"))
    if length(attributes) then
        IupSetAttributes(table, attributes, args)
    end if
    return table
end function

global function IupTableGetData(Ihandle table)
    integer dsidx = IupGetInt(table,"DSIDX")
    return table_tagsets[dsidx]
end function

global procedure IupTableSetData(Ihandle table, sequence data, bool bReset=true)
-- Replace the data, removing any column-sorting and selection.
    if length(data)!=2 then
         crash("data must be {sortable,display}",2)
    end if
    integer dsidx = IupGetInt(table,"DSIDX"),
            o_len = length(table_datasets[dsidx][1]),
            n_len = length(data[1])
    table_datasets[dsidx] = data
    if bReset or n_len!=o_len then
        table_tagsets[dsidx] = tagset(n_len)
        sequence cols = table_sortcols[dsidx]
        if length(cols) then
            integer sortcol = cols[1]
            IupSetAttributeId(table,"SORTSIGN",sortcol,"NO")
            table_sortcols[dsidx] = {}
            table_sortdirs[dsidx] = {}
        end if
        IupSetAttribute(table,"MARKED", NULL)   /* clear all marks */
        IupSetInt(table, "NUMLIN", n_len)
    end if
    IupRefresh(table)
end procedure

atom iupGLControls = NULL,
     xIupGLControlsOpen,
     xIupGLCanvasBoxv,
     xIupGLButton,
     xIupGLExpander,
     xIupGLFrame,
     xIupGLLabel,
     xIupGLLink,
     xIupGLProgressBar,
     xIupGLScrollBox,
     xIupGLSeparator,
     xIupGLSizeBox,
     xIupGLSubCanvas,
     xIupGLDrawText,
     xIupGLDrawImage,
     xIupGLDrawGetTextSize,
     xIupGLDrawGetImageInfo,
     xIupGLToggle,
     xIupGLText,
     xIupGLVal

procedure iup_gl_controls_init()
    if iupGLControls=NULL then
        iupGLControls = iup_open_dll({"iupglcontrols.dll",
                                      "libiupglcontrols.so",
                                      "libiupglcontrols.dylib"})
        xIupGLControlsOpen      = iup_c_proc(iupGLControls, "IupGLControlsOpen", {})
        xIupGLCanvasBoxv        = iup_c_func(iupGLControls, "IupGLCanvasBoxv", {P}, P)
        xIupGLButton            = iup_c_func(iupGLControls, "IupGLButton", {P}, P)
        xIupGLExpander          = iup_c_func(iupGLControls, "IupGLExpander", {P}, P)
        xIupGLFrame             = iup_c_func(iupGLControls, "IupGLFrame", {P}, P)
        xIupGLLabel             = iup_c_func(iupGLControls, "IupGLLabel", {P}, P)
        xIupGLLink              = iup_c_func(iupGLControls, "IupGLLink", {P,P}, P)
        xIupGLProgressBar       = iup_c_func(iupGLControls, "IupGLProgressBar", {}, P)
        xIupGLScrollBox         = iup_c_func(iupGLControls, "IupGLScrollBox", {P}, P)
        xIupGLSeparator         = iup_c_func(iupGLControls, "IupGLSeparator", {}, P)
        xIupGLSizeBox           = iup_c_func(iupGLControls, "IupGLSizeBox", {P}, P)
        xIupGLSubCanvas         = iup_c_func(iupGLControls, "IupGLSubCanvas", {P}, P)
        xIupGLDrawText          = iup_c_proc(iupGLControls, "IupGLDrawText", {P,P,I,I,I})
        xIupGLDrawImage         = iup_c_proc(iupGLControls, "IupGLDrawImage", {P,P,I,I,I})
        xIupGLDrawGetTextSize   = iup_c_proc(iupGLControls, "IupGLDrawGetTextSize", {P,P,P,P})
        xIupGLDrawGetImageInfo  = iup_c_proc(iupGLControls, "IupGLDrawGetImageInfo", {P,P,P,P})
        xIupGLToggle            = iup_c_func(iupGLControls, "IupGLToggle", {P}, P)
        xIupGLText              = iup_c_func(iupGLControls, "IupGLText", {}, P)
        xIupGLVal               = iup_c_func(iupGLControls, "IupGLVal", {P}, P)
    end if
end procedure

global procedure IupGLControlsOpen()
    iup_gl_controls_init()
    c_proc(xIupGLControlsOpen, {})
end procedure

global function IupGLCanvasBox(Ihandles children, string attributes="", sequence args={})
atom pChildren = iup_ptr_array(children)
    Ihandle ih = c_func(xIupGLCanvasBoxv, {pChildren})
    free(pChildren)
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupGLSubCanvas(string attributes="", sequence args={})
Ihandle ih = c_func(xIupGLSubCanvas,{})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global procedure IupGLDrawText(Ihandle ih, string str, integer len, x, y)
    c_proc(xIupGLDrawText,{ih,str,len,x,y})
end procedure

global procedure IupGLDrawImage(Ihandle ih, string name, integer x, y, active)
    c_proc(xIupGLDrawImage,{ih,name,x,y,active})
end procedure

global function IupGLDrawGetTextSize(Ihandle ih, string str)
    atom pW = allocate(2*W),
         pH = pW+W
    c_proc(xIupGLDrawGetTextSize,{ih,str,pW,pH})
    sequence wh = peekns({pW,2})
    free(pW)
    return wh -- integer {w,h}
end function

global function IupGLDrawGetImageInfo(string name)
    atom pW = allocate(3*W),
         pH = pW+W,
         pBpp = pH+W
    c_proc(xIupGLDrawGetImageInfo,{name,pW,pH,pBpp})
    sequence whb = peekns({pW,3})
    free(pW)
    return whb -- integer {w,h,bpp}
end function

global function IupGLButton(nullable_string title, object action=NULL, object func=NULL, sequence attributes="", dword_seq args={})
Ihandle ih = c_func(xIupGLButton,{title})
    {action,func,attributes,args} = paranormalise(action,func,attributes,args)
    if func!=NULL then
        if action=NULL then
            action = "ACTION"
        end if
        IupSetCallback(ih, action, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupGLExpander(Ihandln child=NULL, string attributes="", sequence args={})
Ihandle ih = c_func(xIupGLExpander,{child})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupGLFrame(Ihandln child=NULL, string attributes="", sequence args={})
Ihandle ih = c_func(xIupGLFrame,{child})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupGLLabel(nullable_string title=NULL, string attributes="", dword_seq args={})
Ihandle ih = c_func(xIupGLLabel,{title})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupGLLink(nullable_string url=NULL, nullable_string title=NULL, object action=NULL, object func=NULL, sequence attributes="", dword_seq args={})
Ihandle ih = c_func(xIupGLLink,{url,title})
    {action,func,attributes,args} = paranormalise(action,func,attributes,args)
    if func!=NULL then
        if action=NULL then
            action = "ACTION"
        end if
        IupSetCallback(ih, action, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupGLProgressBar(sequence attributes="", dword_seq args={})
Ihandle ih = c_func(xIupGLProgressBar,{})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupGLScrollBox(Ihandln child=NULL, string attributes="", sequence args={})
Ihandle ih = c_func(xIupGLScrollBox,{child})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupGLSeparator()
Ihandle ih = c_func(xIupGLSeparator,{})
    return ih
end function

global function IupGLSizeBox(Ihandln child=NULL, string attributes="", sequence args={})
Ihandle ih = c_func(xIupGLSizeBox,{child})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupGLText(object action=NULL, object func=NULL, sequence attributes="", dword_seq args={})
    Ihandle ih = c_func(xIupGLText,{})
    {action,func,attributes,args} = paranormalise(action,func,attributes,args)
    if func!=NULL then
        if action=NULL then
            action = "VALUECHANGED_CB"
        elsif action!="VALUECHANGED_CB" then
            ?9/0
        end if
        IupSetCallback(ih, action, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupGLToggle(nullable_string title=NULL, object action=NULL, object func=NULL, sequence attributes="", dword_seq args={})
Ihandle ih = c_func(xIupGLToggle,{title})
    {action,func,attributes,args} = paranormalise(action,func,attributes,args)
    if func!=NULL then
        if action=NULL then
            action = "ACTION"
        end if
        IupSetCallback(ih, action, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupGLValuator(nullable_string orientation=NULL, object action=NULL, object func=NULL, sequence attributes="", dword_seq args={})
Ihandle ih = c_func(xIupGLVal,{orientation})
    {action,func,attributes,args} = paranormalise(action,func,attributes,args)
    if func!=NULL then
        if action=NULL then
            action = "ACTION"
        end if
        IupSetCallback(ih, action, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupGLVal(nullable_string orientation=NULL, object action=NULL, object func=NULL, sequence attributes="", dword_seq args={})
    return IupValuator(orientation, action, func, attributes, args)
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
        xIupWebBrowserOpen  = iup_c_proc(hIupWeb, "IupWebBrowserOpen", {})
        xIupWebBrowser      = iup_c_func(hIupWeb, "IupWebBrowser", {},P)
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

atom 
    iupIm = 0,
    hIm,
    hImProcess,

    xIupImage,
    xIupImageRGB,
    xIupImageRGBA,
    xiupImageStockLoadAll,
    xIupDrawBegin,
    xIupDrawEnd,
    xIupDrawSetClipRect,
    xIupDrawGetClipRect,
    xIupDrawResetClip,
    xIupDrawParentBackground,
    xIupDrawLine,
    xIupDrawRectangle,
    xIupDrawArc,
    xIupDrawPolygon,
    xIupDrawText,
    xIupDrawImage,
    xIupDrawSelectRect,
    xIupDrawFocusRect,
    xIupDrawGetSize,
    xIupDrawGetTextSize,
    xIupDrawGetImageInfo,
    xIupLoadImage,
    xIupSaveImage,
    xIupSaveImageAsText,
    xIupGetNativeHandleImage,
    xIupGetImageNativeHandle,
    xIupImageGetHandle,
    xIupImageFromImImage,
    xIupImageToImImage,
    ximFileImageLoadBitmap,
    ximImageGetAttribString,
    ximImageSetAttribString,
--(wrong one)
--  ximFileSaveImage,
    ximFileImageSave,
    ximProcessRenderFloodFill,
    ximProcessRenderConstant,
    ximProcessToneGamut,
    ximProcessResize,
    ximProcessMirror,
    ximProcessFlip,
    ximProcessRotate180,
    ximProcessRotate90,
    ximProcessNegative,
    ximImageRemoveAlpha,
    ximImageCreate,
    ximImageCreateBased,
    ximConvertColorSpace,
    ximImageClone,
    ximImageDestroy,
    ximConvertPacking,
    ximImageGetOpenGLData


procedure iup_image_init()
    if iupIm=0 then
        iupIm = iup_open_dll({"iupim.dll",
                              "libiupim.so",
                              "libiupim.dylib"})

        hIm = iup_open_dll({"im.dll",
                            "libim.so",
                            "libim.dylib"})

        hImProcess = iup_open_dll({"im_process.dll",
                                   "libim_process.so",
                                   "libim_process.dylib"})

        xIupImage                   = iup_c_func(iup, "IupImage", {I,I,P}, P)
        xIupImageRGB                = iup_c_func(iup, "IupImageRGB", {I,I,P}, P)
        xIupImageRGBA               = iup_c_func(iup, "IupImageRGBA", {I,I,P}, P)
        xiupImageStockLoadAll       = iup_c_proc(iup, "iupImageStockLoadAll", {})
        xIupDrawBegin               = iup_c_proc(iup, "IupDrawBegin",{P})
        xIupDrawEnd                 = iup_c_proc(iup, "IupDrawEnd",{P})
        xIupDrawSetClipRect         = iup_c_proc(iup, "IupDrawSetClipRect",{P,I,I,I,I})
        xIupDrawGetClipRect         = iup_c_proc(iup, "IupDrawGetClipRect",{P,P,P,P,P})
        xIupDrawResetClip           = iup_c_proc(iup, "IupDrawResetClip",{P})
        xIupDrawParentBackground    = iup_c_proc(iup, "IupDrawParentBackground",{P})
        xIupDrawLine                = iup_c_proc(iup, "IupDrawLine",{P,I,I,I,I})
        xIupDrawRectangle           = iup_c_proc(iup, "IupDrawRectangle",{P,I,I,I,I})
        xIupDrawArc                 = iup_c_proc(iup, "IupDrawArc",{P,I,I,I,I,D,D})
        xIupDrawPolygon             = iup_c_proc(iup, "IupDrawPolygon",{P,P,I})
        xIupDrawText                = iup_c_proc(iup, "IupDrawText",{P,P,I,I,I,I,I})
        xIupDrawImage               = iup_c_proc(iup, "IupDrawImage",{P,P,I,I,I,I})
        xIupDrawSelectRect          = iup_c_proc(iup, "IupDrawSelectRect",{P,I,I,I,I})
        xIupDrawFocusRect           = iup_c_proc(iup, "IupDrawFocusRect",{P,I,I,I,I})
        xIupDrawGetSize             = iup_c_proc(iup, "IupDrawGetSize",{P,P,P})
        xIupDrawGetTextSize         = iup_c_proc(iup, "IupDrawGetTextSize",{P,P,I,P,P})
        xIupDrawGetImageInfo        = iup_c_proc(iup, "IupDrawGetImageInfo",{P,P,P,P})
        xIupLoadImage               = iup_c_func(iupIm, "IupLoadImage", {P}, P)
        xIupSaveImage               = iup_c_func(iupIm, "IupSaveImage", {P,P,P}, I)
        xIupSaveImageAsText         = iup_c_func(iup, "IupSaveImageAsText", {P,P,P,P}, I)
        xIupGetNativeHandleImage    = iup_c_func(iupIm, "IupGetNativeHandleImage", {P}, P)
        xIupGetImageNativeHandle    = iup_c_func(iupIm, "IupGetImageNativeHandle", {P}, P)
        xIupImageGetHandle          = iup_c_func(iup, "IupImageGetHandle", {P}, P)
        xIupImageFromImImage        = iup_c_func(iupIm, "IupImageFromImImage", {P}, P)
        xIupImageToImImage          = iup_c_func(iupIm, "IupImageToImImage", {P}, P)
        ximFileImageLoadBitmap      = iup_c_func(hIm, "imFileImageLoadBitmap", {P,I,P}, P)
        ximImageGetAttribString     = iup_c_func(hIm, "imImageGetAttribString", {P,P}, P)
        ximImageSetAttribString     = iup_c_proc(hIm, "imImageSetAttribString", {P,P,P})
--      ximFileSaveImage            = iup_c_func(hIm, "imFileSaveImage", {P,P}, I)
        ximFileImageSave            = iup_c_func(hIm, "imFileImageSave", {P,P,P}, I)
        ximProcessRenderFloodFill   = iup_c_proc(hImProcess, "imProcessRenderFloodFill", {P,I,I,P,F})
        ximProcessRenderConstant    = iup_c_proc(hImProcess, "imProcessRenderConstant", {P,P})
        ximProcessToneGamut         = iup_c_proc(hImProcess, "imProcessToneGamut", {})
        ximProcessResize            = iup_c_proc(hImProcess, "imProcessResize", {P,P,I})
        ximProcessMirror            = iup_c_proc(hImProcess, "imProcessMirror", {P,P})
        ximProcessFlip              = iup_c_proc(hImProcess, "imProcessFlip", {P,P})
        ximProcessRotate180         = iup_c_proc(hImProcess, "imProcessRotate180", {P,P})
        ximProcessRotate90          = iup_c_proc(hImProcess, "imProcessRotate90", {P,P,I})
        ximProcessNegative          = iup_c_proc(hImProcess, "imProcessNegative", {P,P})
        ximImageRemoveAlpha         = iup_c_proc(hIm, "imImageRemoveAlpha", {P})
        ximImageCreate              = iup_c_func(hIm, "imImageCreate", {I,I,I,I}, P)
        ximImageCreateBased         = iup_c_func(hIm, "imImageCreateBased", {P,I,I,I,I}, P)
        ximConvertColorSpace        = iup_c_func(hIm, "imConvertColorSpace", {P,P}, I)
        ximImageClone               = iup_c_func(hIm, "imImageClone", {P}, P)
        ximImageDestroy             = iup_c_proc(hIm, "imImageDestroy", {P})
        ximConvertPacking           = iup_c_proc(hIm, "imConvertPacking", {P,P,I,I,I,I,I,I})
        ximImageGetOpenGLData       = iup_c_proc(hIm, "imImageGetOpenGLData", {P,P})
    end if
end procedure

global function IupImage(integer width, integer height, object pixels)
    atom pPixels
    if sequence(pixels) then
        pPixels = allocate(length(pixels))
        poke(pPixels, pixels)
    else
        pPixels = pixels
    end if
    iup_image_init()
    Ihandle ih = c_func(xIupImage, {width, height, pPixels})
    if sequence(pixels) then
        free(pPixels)
    end if
    return ih
end function

-- gone: use IupImage directly instead (14/3/2020)
--global function IupImageA(integer width, integer height, atom pPixels)
--  iup_image_init()
--  Ihandle ih = c_func(xIupImage, {width, height, pPixels})
--  return ih
--end function

--DEV update docs: (and likewise IupImageRGB and IupImageRBGA can accept an atom as
--  [^ DONE]        the third parameter as a pointer to already-initialised memory)
--global function IupImageRGB(integer width, integer height, sequence pixels)
global function IupImageRGB(integer width, integer height, object pixels)
    atom pPixels
    if sequence(pixels) then
        pPixels = allocate(length(pixels))
        poke(pPixels, pixels)
    else
        pPixels = pixels
    end if
    iup_image_init()
    Ihandle ih = c_func(xIupImageRGB, {width, height, pPixels})
    if sequence(pixels) then
        free(pPixels)
    end if
    return ih
end function

--global function IupImageRGBA(integer width, integer height, sequence pixels)
global function IupImageRGBA(integer width, integer height, object pixels)
    atom pPixels
    if sequence(pixels) then
        pPixels = allocate(length(pixels))
        poke(pPixels, pixels)
    else
        pPixels = pixels
    end if
    iup_image_init()
    Ihandle ih = c_func(xIupImageRGBA, {width, height, pPixels})
    if sequence(pixels) then
        free(pPixels)
    end if
    return ih
end function

-- only used by IupView.exw (and deliberately not documented)
global procedure iupImageStockLoadAll()
    iup_image_init()
    c_proc(xiupImageStockLoadAll,{})
end procedure

--DEV not tested (*14):
global procedure IupDrawBegin(Ihandle ih)
    iup_image_init()
    c_proc(xIupDrawBegin,{ih})
end procedure

global procedure IupDrawEnd(Ihandle ih)
    iup_image_init()
    c_proc(xIupDrawEnd,{ih})
end procedure

global procedure IupDrawSetClipRect(Ihandle ih, integer x1, y1, x2, y2)
    iup_image_init()
    c_proc(xIupDrawSetClipRect,{ih,x1,y1,x2,y2})
end procedure

global procedure IupDrawResetClip(Ihandle ih)
    iup_image_init()
    c_proc(xIupDrawResetClip,{ih})
end procedure

global function IupDrawGetClipRect(Ihandle ih)
    iup_image_init()
    atom pX = allocate(W*4)
    c_proc(xIupDrawGetClipRect,{ih,pX, pX+W, pX+2*W, pX+3*W})
    sequence s = peeknu({pX,4})
    free(pX)
    return s -- integer {x1,y1,x2,y2}
end function

global procedure IupDrawParentBackground(Ihandle ih)
    iup_image_init()
    c_proc(xIupDrawParentBackground,{ih})
end procedure

global procedure IupDrawLine(Ihandle ih, integer x1, y1, x2, y2)
    iup_image_init()
    c_proc(xIupDrawLine,{ih,x1,y1,x2,y2})
end procedure

global procedure IupDrawRectangle(Ihandle ih, integer x1, y1, x2, y2)
    iup_image_init()
    c_proc(xIupDrawRectangle,{ih,x1,y1,x2,y2})
end procedure

global procedure IupDrawArc(Ihandle ih, integer x1, y1, x2, y2, atom a1, a2)
    iup_image_init()
    c_proc(xIupDrawArc,{ih,x1,y1,x2,y2,a1,a2})
end procedure

global procedure IupDrawPolygon(Ihandle ih, sequence points)
    iup_image_init()
    integer count = length(points)
    atom pPoints = allocate(4*count)
    poke4(pPoints, points)
    c_proc(xIupDrawPolygon,{ih,pPoints,count})
    free(pPoints)
end procedure

global procedure IupDrawText(Ihandle ih, string text, integer x, y, w=0, h=0)
    iup_image_init()
    integer len = length(text)
    c_proc(xIupDrawText,{ih,text,len,x,y,w,h})
end procedure

--global procedure IupDrawImage(Ihandle ih, string name, boolean make_inactive, integer x, y)
global procedure IupDrawImage(Ihandle ih, string name, integer x, y, w=0, h=0)
    iup_image_init()
--  c_proc(xIupDrawImage,{ih,name,make_inactive,x,y})
    c_proc(xIupDrawImage,{ih,name,x,y,w,h})
end procedure

global procedure IupDrawSelectRect(Ihandle ih, integer x1, y1, x2, y2)
    iup_image_init()
    c_proc(xIupDrawSelectRect,{ih,x1,y1,x2,y2})
end procedure

global procedure IupDrawFocusRect(Ihandle ih, integer x1, y1, x2, y2)
    iup_image_init()
    c_proc(xIupDrawFocusRect,{ih,x1,y1,x2,y2})
end procedure

global function IupDrawGetSize(Ihandle ih)
    iup_image_init()
    atom pWH = allocate(8)
    c_proc(xIupDrawGetSize,{ih,pWH,pWH+4})
    sequence wh = peek4s({pWH,2})
    free(pWH)
    return wh -- {width,height}
end function

global function IupDrawGetTextSize(Ihandle ih, string str)
    iup_image_init()
    atom pWH = allocate(2*W)
    c_proc(xIupDrawGetTextSize,{ih,str,length(str),pWH,pWH+W})
    sequence wh = peekns({pWH,2})
    free(pWH)
    return wh -- {width,height}
end function

global function IupDrawGetImageInfo(string name)
    iup_image_init()
    atom pWHB = allocate(12)
    c_proc(xIupDrawGetImageInfo,{name,pWHB,pWHB+4,pWHB+8})
    sequence whb = peek4s({pWHB,3})
    free(pWHB)
    return whb -- {width,height,bpp}
end function

global function IupLoadImage(string filename)
    iup_image_init()
    Ihandln ih = c_func(xIupLoadImage, {filename})
    return ih
end function

global function IupSaveImage(Ihandle ih, string file_name, string fmt)
--  iup_image_init()
    integer result = c_func(xIupSaveImage, {ih,file_name,fmt})
    if result=0 then
        return IupGetGlobal("IUPIM_LASTERROR")
    end if
    return result
end function

global function IupSaveImageAsText(Ihandle ih, string filename, string fmt, nullable_string name=NULL)
--  iup_image_init()
    integer result = c_func(xIupSaveImageAsText, {ih, filename, fmt, name})
    if result=0 then
        return IupGetGlobal("IUPIM_LASTERROR")
    end if
    return result
end function

global function IupGetNativeHandleImage(atom handle)
--  iup_image_init()
    imImage result = c_func(xIupGetNativeHandleImage, {handle})
    return result
end function

global function IupGetImageNativeHandle(imImage image)
--  iup_image_init()
    atom result = c_func(xIupGetImageNativeHandle, {image})
    return result
end function

global function IupImageGetHandle(string name)
--  iup_image_init()
    Ihandln result = c_func(xIupImageGetHandle, {name})
    return result
end function

global function IupImageFromImImage(imImage image)
--  iup_image_init()
    Ihandle ih = c_func(xIupImageFromImImage, {image})
    return ih
end function

global function IupImageToImImage(Ihandle ih)
--  iup_image_init()
    imImage image = c_func(xIupImageToImImage, {ih})
    return image
end function

--/*
imImage* imFileImageLoadBitmap(const char * file_name, int index, int * error)
Loads an image from file, but forces the image to be a bitmap. Open, loads and closes the file. 
index specifies the image number between 0 and image_count-1. 
Returns NULL if failed. Attributes from the file will be stored at the image. See also imErrorCodes.
--*/
global function imFileImageLoadBitmap(string filename, integer index, atom pError)
    iup_image_init()
    imImage image = c_func(ximFileImageLoadBitmap,{filename,index,pError})
    return image
end function

--const char *  imImageGetAttribString (const imImage *image, const char *attrib) 
global function imImageGetAttribString(imImage image, string attrib)
--  iup_image_init()
    atom pString = c_func(ximImageGetAttribString,{image,attrib})
    if pString=NULL then return "" end if
    return peek_string(pString)
end function

--/*
void imImageSetAttribString(const imImage* image, const char* attrib, const char* value)
Changes an extended attribute as a string.
--*/
global procedure imImageSetAttribString(imImage image, string attrib, nullable_string v)
--  iup_image_init()
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
--  iup_image_init()
    integer error = c_func(ximFileImageSave,{filename,fmt,image})
    return error
end function

--/*
void imProcessRenderFloodFill(imImage *image, int start_x, int start_y, float* replace_color, float tolerance)    
Render a color flood fill. 
Image must the IM_RGB color space. replace_color must have 3 components
--*/
global procedure imProcessRenderFloodFill(imImage image, integer start_x, integer start_y, sequence replace_color, atom tolerance)
    if length(replace_color)!=3 then ?9/0 end if
    atom pColour = allocate(12)
    iup_poke_float(pColour,replace_color)
--  iup_image_init()
    c_proc(ximProcessRenderFloodFill,{image,start_x,start_y,pColour,tolerance})
    free(pColour)
end procedure

--/*
int imProcessRenderConstant(imImage *image, float *value)     
Render a constant. The number of values must match the depth of the image.
--*/
global procedure imProcessRenderConstant(imImage image, sequence v)
    atom pColour = allocate(4*length(v))
    iup_poke_float(pColour,v)
--  iup_image_init()
    c_proc(ximProcessRenderConstant,{image,pColour})
    free(pColour)
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
    atom pParams = allocate(4*length(params))
    iup_poke_float(pParams,params)
--  iup_image_init()
    c_proc(ximProcessToneGamut,{src_image, dst_image, op, pParams})
    free(pParams)
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
--  iup_image_init()
    c_proc(ximProcessResize,{src_image, dst_image, order})
end procedure

--/*
void imProcessMirror(const imImage* src_image, imImage* dst_image)
Mirror the image in a horizontal flip. Swap columns. 
Images must be of the same type and size. Can be done in-place.
--*/
global procedure imProcessMirror(imImage src_image, imImage dst_image)
--  iup_image_init()
    c_proc(ximProcessMirror,{src_image, dst_image})
end procedure

--/*
void imProcessFlip(const imImage* src_image, imImage* dst_image)
Apply a vertical flip. Swap lines. 
Images must be of the same type and size. Can be done in-place.
--*/
global procedure imProcessFlip(imImage src_image, imImage dst_image)
--  iup_image_init()
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
In Lua the IM image metatable name is "imImage". 
When converted to a string will return "imImage(%p) [width=%d,height=%d,color_space=%s,data_type=%s,depth=%d]" 
where p is replaced by the userdata address, and other values are replaced by the respective attributes. 
If the image is already destroyed by im.ImageDestroy, then it will return also the suffix "-destroyed".
--*/
global function imImageCreate(integer width, integer height, integer color_space, integer data_type)
    iup_image_init()
    imImage image = c_func(ximImageCreate,{width,height,color_space,data_type})
    return image
end function

--/*
imImage* imImageCreateBased(const imImage * image, int width, int height, int color_space, int data_type)
Creates a new image based on an existing one. 
If the additional parameters are -1, the given image parameters are used. 
The image atributes always are copied. HasAlpha is copied. See also imDataType and imColorSpace.
--*/
global function imImageCreateBased(imImage image, integer width=-1, integer height=-1, integer color_space=-1, integer data_type=-1)
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
global function imImageDestroy(imImage image)
    if image!=NULL then
        iup_image_init()
        c_proc(ximImageDestroy,{image})
    end if
    return NULL
end function

global procedure imConvertPacking(atom pSrcData, pDstData, integer width, height, src_depth, dst_depth, data_type, src_is_packed)
    iup_image_init()
    c_proc(ximConvertPacking,{pSrcData, pDstData, width, height, src_depth, dst_depth, data_type, src_is_packed})
end procedure


--/*
void* imImageGetOpenGLData(const imImage *image, int *glformat)
Returns an OpenGL compatible data buffer. Also returns the correspondent pixel format. 
The memory allocated is stored in the attribute "GLDATA" with BYTE type. And it will exists while the image exists. 
It can be cleared by setting the attribute to NULL. 
MAP images are converted to RGB, and BINARY images are converted to GRAY. 
Alpha channel is considered and Transparency* attributes are converted to alpha channel. 
So calculate depth from glformat, not from image depth.
--*/
global procedure imImageGetOpenGLData(imImage image, atom pRes)
    c_proc(ximImageGetOpenGLData,{image,pRes})
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
Data Fields                 offset(32)
int  width                  0   
int  height                 4
int  color_space            8
int  data_type              12
int  has_alpha              16
int  depth                  20
int  line_size              24
int  plane_size             28
int  size                   32
int  count                  36
void **  data               40
long *  palette 
int  palette_count 
void *  attrib_table 
--*/
global function im_width(imImage image)
    integer width = peek4s(image)
    return width
end function

global function im_height(imImage image)
    integer height = peek4s(image+4)
    return height
end function

global function im_color_space(imImage image)
    integer color_space = peek4s(image+8)
    return color_space
end function

global function im_depth(imImage image)
    integer depth = peek4s(image+20)
    return depth
end function

global function im_data(imImage image)
    atom dataptr = peekNS(image+40,W,0)
    return dataptr
end function

--global function im_palette(imImage image)
--  atom palette_ptr = peekNS(image+40+W,W,0)
--  integer palette_count = peekNS(image+40+2*W,W,0)
--  sequence pal = peek4u({palette_ptr,palette_count}
--  for i=1 to length(pal) do
--      pal[i] = to_rgb(pal[i])
--  end for
--  return pal
--end function

global function im_pixel(imImage image, integer x, integer y)
    if im_color_space(image)!=IM_RGB then ?9/0 end if   --DEV fixme!
    -- (I cribbed this from simple_paint, which converts to IM_RGB on load)
    -- [IupImageFromImImage() in iup_im.c is probly a fair place to start]
--  integer r,g,b
    atom data_ptr = im_data(image)
    integer offset
--if im_color_space(image)=IM_RGB then
    offset = y*im_width(image)+x
    atom {rptr,gptr,bptr} = sq_add(peekNS({data_ptr,3},W,0),offset)
    integer r = peek(rptr),
            g = peek(gptr),
            b = peek(bptr)
    return {r,g,b}
--/* I tried...
  else
--  for (i = 0; i < image->palette_count; i++)
--  {
--    unsigned char r, g, b;
--    imColorDecode(&r, &g, &b, image->palette[i]);
--    IupSetRGBId(iup_image, "", i, r, g, b);
--  }

--      {r,g,b} = peek({data_ptr+offset,3})
--      im_palette()
--      integer line_size = peek4s(image+24)
--      offset = y*line_size+x
        offset = y*im_width(image)+x
        atom palette_ptr = peekNS(image+40+W,W,0)
        integer palette_count = peekNS(image+40+2*W,W,0),
                palette_idx = peek(data_ptr+offset)
--      if palette_idx>palette_count then ?9/0 end if
--      {r,g,b} = to_rgb(peek4u(palette_ptr+palette_idx))
--      {r,g,b} = peek({palette_ptr+palette_idx*4,3})
        return peek({palette_ptr+palette_idx*4,3})
  end if
--*/
end function


--****
-- === Keyboard
--

--constant
--  xIupPreviousField   = iup_c_func(iup, "IupPreviousField", {P},P),
--  xIupNextField       = iup_c_func(iup, "IupNextField", {P},P),
--  xIupSetFocus        = iup_c_proc(iup, "IupSetFocus", {P}),
--  xIupGetFocus        = iup_c_func(iup, "IupGetFocus", {},P)
--
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

--constant
--  xIupMenuv       = iup_c_func(iup, "IupMenuv", {P}, P),
--  xIupItem        = iup_c_func(iup, "IupItem",  {P,P}, P),
--  xIupSeparator   = iup_c_func(iup, "IupSeparator", {}, P),
--  xIupSubmenu     = iup_c_func(iup, "IupSubmenu", {P,P}, P)
--
global function IupMenu(Ihandles children={}, sequence attributes="", dword_seq args={})
atom pChildren = iup_ptr_array(children)
Ihandle ih = c_func(xIupMenuv, {pChildren})
    free(pChildren)
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupMenuItem(nullable_string title=NULL, object action=NULL, object func=NULL, sequence attributes="", dword_seq args={})
    Ihandle ih = c_func(xIupItem, {title, action})
    {action,func,attributes,args} = paranormalise(action,func,attributes,args)
    if func!=NULL then
        if action=NULL then
            action = "ACTION"
        end if
        IupSetCallback(ih, action, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupItem(nullable_string title=NULL, object action=NULL, object func=NULL, sequence attributes="", dword_seq args={})
    return IupMenuItem(title, action, func, attributes, args)
end function

global function IupSeparator()
    Ihandle ih = c_func(xIupSeparator, {})
    return ih
end function

--Ihandle* IupSubmenu(const char* title, Ihandle* child);
global function IupSubmenu(nullable_string title=NULL, Ihandln menu=NULL, string attributes="", sequence args={})
    Ihandle ih = c_func(xIupSubmenu, {title, menu})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupSubMenu(nullable_string title=NULL, Ihandln menu=NULL, string attributes="", sequence args={})
    return IupSubmenu(title, menu, attributes, args)
end function

--****
-- === Names
--

--constant
----    xIupSetHandle       = iup_c_proc(iup, "IupSetHandle", {P,P}),
----    xIupGetHandle       = iup_c_func(iup, "IupGetHandle", {P},P),
--  xIupGetName         = iup_c_func(iup, "IupGetName", {P},P),
--  xIupGetAllNames     = iup_c_func(iup, "IupGetAllNames", {P,I},I),
--  xIupGetAllDialogs   = iup_c_func(iup, "IupGetAllDialogs", {P,I},I)

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
    atom ptr = allocate_data(W*n)
    n = c_func(xIupGetAllNames, {ptr,n})
    sequence names = iup_peek_string_pointer_array(ptr, n)
    free(ptr)
    return names
end function

global function IupGetAllDialogs()
    integer n = c_func(xIupGetAllDialogs, {NULL,0})
    atom ptr = allocate_data(W*n)
    n = c_func(xIupGetAllDialogs, {ptr,n})
    sequence names = iup_peek_string_pointer_array(ptr, n)
    free(ptr)
    return names
end function

--constant
--  xIupClipboard = iup_c_func(iup, "IupClipboard", {}, P),
--  xIupTimer = iup_c_func(iup, "IupTimer", {}, P),
--  xIupUser = iup_c_func(iup, "IupUser", {}, P),
--  xIupHelp = iup_c_func(iup, "IupHelp", {P}, I)

global function IupClipboard()
    Ihandle ih = c_func(xIupClipboard, {})
    return ih
end function

global function IupTimer(atom func=NULL, integer msecs=0, boolean active=true)
    Ihandle ih = c_func(xIupTimer, {})
    if func!=NULL and msecs!=0 then
        IupSetCallback(ih, ACTION_CB, func)
        IupSetAttributes(ih, "TIME=%d,RUN=%s", {msecs, iff(active, "YES", "NO")})
    end if
    return ih
end function

--global function IupTimer(integer rid = -2, integer msecs=0, integer active=1)
--Ihandle ih = c_func(xIupTimer, {})
--  if rid>=0 and msecs!=0 then
--      IupSetCallback(ih, ACTION_CB, rid)
--      IupSetAttributes(ih, "TIME=%d,RUN=%s", {msecs, iff(active, "YES", "NO")})
--  end if
--  return ih
--end function

global function IupUser(string attributes="", dword_seq args={})
    Ihandle ih = c_func(xIupUser, {})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupHelp(string url)
-- returns 1 if successful, -1 if failed, -2 if file not found
    return c_func(xIupHelp, {url})
end function


--cd.e:

--global type cdCanvas(object o)
--  return atom(o) and o>=NULL and o=floor(o)
global type cdCanvas(integer i)
    return i>0
end type

--global type cdCanvas(object o)
--  return atom(o) and o>=NULL and o=floor(o)
--end type

global type cdCanvan(integer i)
    return i>=0
end type

--  Canvas Draw Constants

global constant
    CD_QUERY = -1,
    CD_ERROR = -1,
    CD_OK = 0,

    --
    -- Polygon Mode Constants
    --
    CD_FILL = 0,
    CD_OPEN_LINES = 1,
    CD_CLOSED_LINES = 2,
    CD_CLIP = 3,
    CD_BEZIER = 4,
    CD_REGION = 5,
    CD_PATH = 6,
--undocumented, unused, google search yields nothing useful:
--  CD_POLYCUSTOM = 10,

    --
    -- path actions
    --
    CD_PATH_NEW = 0,
    CD_PATH_MOVETO = 1,
    CD_PATH_LINETO = 2,
    CD_PATH_ARC = 3,
    CD_PATH_CURVETO = 4,
    CD_PATH_CLOSE = 5,
    CD_PATH_FILL = 6,
    CD_PATH_STROKE = 7,
    CD_PATH_FILLSTROKE = 8,
    CD_PATH_CLIP = 9,

    --
    -- Bitmap Type Constants
    --
    --  These definitions are compatible with the IM library
    --
--deprecated (see cdCreateBitmap()/init_bitmap())
    CD_RGB = 0,
    CD_MAP = 1,
    CD_RGBA = #100,
    --
    -- Bitmap Data Constants
    --
    CD_IRED = 0,
    CD_IGREEN = 1,
    CD_IBLUE = 2,
    CD_IALPHA = 3,
    CD_INDEX = 4,
    CD_COLORS = 5,
--</deprecated>
    --
    -- Clip Mode Constants
    --
    CD_CLIPOFF = 0,
    CD_CLIPAREA = 1,
    CD_CLIPPOLYGON = 2,
    CD_CLIPREGION = 3,
    --
    -- Region Combine Mode Constants
    --
    CD_UNION = 0,
    CD_INTERSECT = 1,
    CD_DIFFERENCE = 2,
    CD_NOTINTERSECT = 3,
    --
    -- Fill Mode Constants
    --
--DEV F1 lookup?: (cdCanvasFillMode)
    CD_EVENODD = 0,
    CD_WINDING = 1,
    --
    -- Line Join Constants
    --
    CD_MITER = 0,
    CD_BEVEL = 1,
    CD_ROUND = 2,
    --
    -- Line Cap Constants
    --
    CD_CAPFLAT = 0,
    CD_CAPSQUARE = 1,
    CD_CAPROUND = 2,
    --
    -- Background Opacity Mode Constants
    --
    CD_OPAQUE = 0,
    CD_TRANSPARENT = 1,
    --
    -- Write Mode Constants
    --
    CD_REPLACE = 0,
    CD_XOR = 1,
    CD_NOT_XOR = 2,
    --
    -- Color Allocation Mode Constants
    --
    -- Pallette
    --
    CD_POLITE = 0,
    CD_FORCE = 1,
    --
    -- Line Style Constants
    --
    CD_CONTINUOUS = 0,
    CD_DASHED = 1,
    CD_DOTTED = 2,
    CD_DASH_DOT = 3,
    CD_DASH_DOT_DOT = 4,
    CD_CUSTOM = 5,
    --
    -- Marker Type Constants
    --
    CD_PLUS = 0,
    CD_STAR = 1,
    CD_CIRCLE = 2,
    CD_X = 3,
    CD_BOX = 4,
    CD_DIAMOND = 5,
    CD_HOLLOW_CIRCLE = 6,
    CD_HOLLOW_BOX = 7,
    CD_HOLLOW_DIAMOND = 8,
    --
    -- Hatch Type Constants
    --
    CD_HORIZONTAL = 0,
    CD_VERTICAL = 1,
    CD_FDIAGONAL = 2,
    CD_BDIAGONAL = 3,
    CD_CROSS = 4,
    CD_DIAGCROSS = 5,
    --
    -- Interior Style Constants
    --
    CD_SOLID = 0,
    CD_HATCH = 1,
    CD_STIPPLE = 2,
    CD_PATTERN = 3,
    CD_HOLLOW = 4,
    --
    -- Text Alignment Constants
    --
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
    CD_BASE_RIGHT = 11,
    --
    -- Style Constants
    --
    CD_PLAIN = 0,
    CD_BOLD = 1,
    CD_ITALIC = 2,
    CD_UNDERLINE = 4,
    CD_STRIKEOUT = 8,
    CD_BOLD_ITALIC = or_bits(CD_BOLD, CD_ITALIC),
    --
    -- Font Size Constants
    --
    CD_SMALL = 8,
    CD_STANDARD = 12,
    CD_LARGE = 18,
    --
    -- Canvas Capabilities Constants
    --
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
    --
    -- Canvas Draw Play Constants
    --
    CD_SIZECB = 0,
    CD_ABORT = 1,
    CD_CONTINUE = 0,
    --
    -- Simulation Flag Constants
    --
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
    CD_SIM_FILLS = or_bits(CD_SIM_BOX, or_bits(CD_SIM_SECTOR, or_bits(CD_SIM_CHORD, CD_SIM_POLYGON))),

    --
    -- Conversion Factor Constants
    --
    -- These are simply for convenience
    --
--DEV F1 lookup:
    CD_MM2PT = 2.83465,         -- Millimeters to Points (pt = CD_MM2PT * mm)
--  CD_DEG2RAD = 0.0174533,     -- Degrees to Radians (rad = CD_DEG2RAD * deg)
--  CD_RAD2DEG = 57.2958,       -- Radians to Degrees (deg = CD_RAD2DEG * rad)
    CD_DEG2RAD = PI/180,        -- Degrees to Radians (rad = CD_DEG2RAD * deg)
    CD_RAD2DEG = 180/PI,        -- Radians to Degrees (deg = CD_RAD2DEG * rad)

    CD_IUP          = {"CD_IUP"},
    CD_IUPDRAW      = {"CD_IUPDRAW"},
    CD_PRINTER      = {"CD_PRINTER"},
    CD_PS           = {"CD_PS"},
    CD_PICTURE      = {"CD_PICTURE"},
    CD_GL           = {"CD_GL"},
    CD_IUPDBUFFER   = {"CD_IUPDBUFFER"},
--  CD_NATIVEWINDOW = {"CD_NATIVEWINDOW"},
    CD_DBUFFER      = {"CD_DBUFFER"},
    CD_IMIMAGE      = {"CD_IMIMAGE"},
    CD_PPTX         = {"CD_PPTX"},
    CD_IMAGERGB     = {"CD_IMAGERGB"},
    CD_DBUFFERRGB   = {"CD_DBUFFERRGB"},
    CD_CGM          = {"CD_CGM"},
    CD_METAFILE     = {"CD_METAFILE"},
    CD_WMF          = {"CD_WMF"},
    CD_EMF          = {"CD_EMF"},
    CD_DEBUG        = {"CD_DEBUG"},
    CD_DGN          = {"CD_DGN"},
    CD_DXF          = {"CD_DXF"},
    CD_PDF          = {"CD_PDF"},
    CD_SVG          = {"CD_SVG"},
    CD_CLIPBOARD    = {"CD_CLIPBOARD"},

    --
    -- Predefined Color Constants
    --
    -- These are simply for convenience
    --
--XaddStandardColour("Black",    0,  0,  0) -- ==CD_BLACK
--?addStandardColour("Maroon", 128,  0,  0)
--XaddStandardColour("Green",    0,128,  0) -- ==CD_DARK_GREEN
--XaddStandardColour("Olive",  128,128,  0)
--XaddStandardColour("Navy",     0,  0,128)
--~addStandardColour("Purple", 128,  0,128)
--xaddStandardColour("Teal",     0,128,128) -- ==CD_DARK_CYAN
--addStandardColour("Gray",    128,128,128) -- ==CD_DARK_GREY
--XaddStandardColour("Silver", 192,192,192) -- ==CD_GREY
--XaddStandardColour("Red",    255,  0,  0) -- ==CD_RED
--?addStandardColour("Lime",     0,255,  0)
--XaddStandardColour("Yellow", 255,255,  0) -- ==CD_YELLOW
--XaddStandardColour("Blue",     0,  0,255) -- ==CD_BLUE
--?addStandardColour("Fuchsia",255,  0,255)
--?addStandardColour("Aqua",     0,255,255) -- ==CD_CYAN?
--XaddStandardColour("White",  255,255,255) -- ==CD_WHITE
--constant colour_table={#e6194b,   -- Red
--                       #3cb44b,   -- Green
--                       #ffe119,   -- Yellow
--                       #4363d8,   -- Blue
--                       #f58231,   -- Orange
--                       #911eb4,   -- Purple
--                       #42d4f4,   -- Cyan
--                       #f032e6,   -- Magenta
--                       #bfef45,   -- Lime
--                       #fabebe,   -- Pink
--                       #469990,   -- Teal
--                       #e6beff,   -- Lavender
--                       #9A6324,   -- Brown
--                       #fffac8,   -- Beige
--                       #800000,   -- Maroon
--                       #aaffc3,   -- Mint
--X                      #808000,   -- Olive
--                       #ffd8b1,   -- Apricot
--~                      #000075,   -- Navy
--                       #a9a9a9}   -- Grey

    CD_BLACK        = #000000,
    CD_BLUE         = #0000FF,
    CD_CYAN         = #00FFFF,
    CD_DARK_BLUE    = #000080,
    CD_DARK_CYAN    = #008080,
    CD_DARK_GRAY    = #808080,
    CD_DARK_GREY    = #808080,
    CD_DARK_GREEN   = #008000,
    CD_DARK_MAGENTA = #800080,  -- (not the best...)
    CD_DARK_RED     = #800000,  -- (not the best...)
    CD_GRAY         = #C0C0C0,
    CD_GREY         = #C0C0C0,
    CD_GREEN        = #3cb44b,
    CD_INDIGO       = #4B0082,
    CD_LIGHT_GRAY   = #E4E4E4,
    CD_LIGHT_GREY   = #E4E4E4,
    CD_LIGHT_GREEN  = #00FF00,
    CD_LIGHT_BLUE   = #4363d8,
--  CD_MAGENTA      = #FF00FF,
    CD_MAGENTA      = #f032e6,
    CD_NAVY         = #000080,  -- (=== CD_DARK_BLUE)
    CD_ORANGE       = #FF8C00,
    CD_OLIVE        = #808000,
    CD_PARCHMENT    = #FFFFE0,
--  CD_PURPLE       = #D080D0,
    CD_PURPLE       = #911eb4,
    CD_RED          = #FF0000,
    CD_SILVER       = #C0C0C0,  -- (=== CD_GREY)
    CD_TEAL         = #008080,  -- (=== CD_DARK_CYAN)
    CD_VIOLET       = #EE82EE,
    CD_WHITE        = #FFFFFF,
    CD_YELLOW       = #FFFF00,

    IUP_BLACK        = "#000000",
    IUP_BLUE         = "#0000FF",
    IUP_CYAN         = "#00FFFF",
    IUP_DARK_BLUE    = "#000080",
    IUP_DARK_CYAN    = "#008080",
    IUP_DARK_GRAY    = "#808080",
    IUP_DARK_GREY    = "#808080",
    IUP_DARK_GREEN   = "#008000",
    IUP_DARK_MAGENTA = "#800080",   -- (not the best...)
    IUP_DARK_RED     = "#800000",   -- (not the best...)
    IUP_GRAY         = "#C0C0C0",
    IUP_GREY         = "#C0C0C0",
    IUP_GREEN        = "#3cb44b",
    IUP_INDIGO       = "#4B0082",
    IUP_LIGHT_GRAY   = "#E4E4E4",
    IUP_LIGHT_GREY   = "#E4E4E4",
    IUP_LIGHT_GREEN  = "#00FF00",
    IUP_LIGHT_BLUE   = "#4363d8",
--  IUP_MAGENTA      = "#FF00FF",
    IUP_MAGENTA      = "#f032e6",
    IUP_NAVY         = "#000080",   -- (=== CD_DARK_BLUE)
    IUP_ORANGE       = "#FF8C00",
    IUP_OLIVE        = "#808000",
    IUP_PARCHMENT    = "#FFFFE0",
--  IUP_PURPLE       = "#D080D0",
    IUP_PURPLE       = "#911eb4",
    IUP_RED          = "#FF0000",
    IUP_SILVER       = "#C0C0C0",   -- (=== CD_GREY)
    IUP_TEAL         = "#008080",   -- (=== CD_DARK_CYAN)
    IUP_VIOLET       = "#EE82EE",
    IUP_WHITE        = "#FFFFFF",
    IUP_YELLOW       = "#FFFF00",
    $

--SUG/DOC:
--global function CD2IUP(atom CD_COLOR)
--  return sprintf("%06x",CD_COLOR)
--end function

atom
    hCd = 0,
    hCdIup,
    hCdGL,
    hCdIm,
    hCdPDF,

    xcdVersion,
    xcdVersionDate,
    xcdVersionNumber,
    
    xcdCreateCanvas,
    xcdKillCanvas,
    xcdCanvasGetContext,
    xcdCanvasActivate,
    xcdActiveCanvas,
    xcdCanvasDeactivate,
    xcdUseContextPlus,
    xcdInitContextPlus,

    xcdContextRegisterCallback,
    xcdContextCaps,
    xcdCanvasPlay,

    xcdContextImImage,
    xcdCanvasPutImImage,

    xcdContextIup,
    xcdContextIupDraw,
    xcdContextPrinter,
    xcdContextPS,
    xcdContextPicture,
    xcdContextGL,
    xcdContextIupDBuffer,
--  xcdContextNativeWindow,
    xcdContextDBuffer,
    xcdContextImageRGB,
    xcdContextPPTX,
    xcdContextDBufferRGB,
    xcdContextCGM,
    xcdContextMetafile,
    xcdContextWMF,
    xcdContextEMF,
    xcdContextDebug,
    xcdContextDGN,
    xcdContextDXF,
    xcdContextPDF,
    xcdContextSVG,
    xcdContextClipboard,

    xcdContextNativeWindow,
    xcdGetScreenSize,
    xcdGetScreenColorPlanes,

    XCD_IUP,
    XCD_IUPDRAW,
    XCD_PRINTER,
    XCD_PS,
    XCD_PICTURE,
    XCD_GL,
    XCD_IUPDBUFFER,
--  XCD_NATIVEWINDOW,
    XCD_DBUFFER,
    XCD_IMIMAGE,
    XCD_PPTX,
    XCD_IMAGERGB,
    XCD_DBUFFERRGB,
    XCD_CGM,
    XCD_METAFILE,
    XCD_WMF,
    XCD_EMF,
    XCD_DEBUG,
    XCD_DGN,
    XCD_DXF,
    XCD_PDF,
    XCD_SVG,
    XCD_CLIPBOARD

sequence XCD_STR = {"CD_IUP",
                    "CD_IUPDRAW",
                    "CD_PRINTER",
                    "CD_PS",
                    "CD_PICTURE",
                    "CD_GL",
                    "CD_IUPDBUFFER",
--                  "CD_NATIVEWINDOW",
                    "CD_DBUFFER",
                    "CD_IMIMAGE",
                    "CD_PPTX",
                    "CD_IMAGERGB",
                    "CD_DBUFFERRGB",
                    "CD_CGM",
                    "CD_METAFILE",
                    "CD_WMF",
                    "CD_EMF",
                    "CD_DEBUG",
                    "CD_DGN",
                    "CD_DXF",
                    "CD_PDF",
                    "CD_SVG",
                    "CD_CLIPBOARD"},
         XCDS

atom 
    xcdCanvasSimulate,
    xcdCanvasFlush,
    xcdCanvasClear,
    xcdClear,
    xcdCanvasSaveState,
    xcdCanvasRestoreState,
    xcdReleaseState,
    xcdCanvasSetAttribute,
    xcdCanvasGetAttribute,

    xcdCanvasGetSize,
--  xcdCanvasUpdateYAxis,
    xcdfCanvasUpdateYAxis,
--  xcdCanvasInvertYAxis,
    xcdfCanvasInvertYAxis,
    xcdCanvasMM2Pixel,
--  xcdfCanvasMM2Pixel,
--  xcdCanvasPixel2MM,
    xcdfCanvasPixel2MM,
--  xcdCanvasOrigin,
    xcdfCanvasOrigin,
--  xcdCanvasGetOrigin,
    xcdfCanvasGetOrigin,
    xcdCanvasTransform,
    xcdCanvasGetTransform,
    xcdCanvasTransformMultiply,
    xcdCanvasTransformRotate,
    xcdCanvasTransformScale,
    xcdCanvasTransformTranslate,
--  xcdCanvasTransformPoint,
    xcdfCanvasTransformPoint,

    --
    -- clipping 
    --
    xcdCanvasClip,
--  xcdCanvasClipArea,
    xcdfCanvasClipArea,
--  xcdCanvasGetClipArea,
    xcdfCanvasGetClipArea,

    --
    -- clipping region 
    --
    xcdCanvasIsPointInRegion,
    xcdCanvasOffsetRegion,
    xcdCanvasGetRegionBox,
    xcdCanvasRegionCombineMode,

    --
    -- drawing primitives
    --
    xcdCanvasPixel,
    xcdCanvasMark,
    xcdCanvasLine,
    xcdLine,
    xcdCanvasBegin,
    xcdCanvasVertex,
    xcdCanvasEnd,
    xcdCanvasPathSet,
    xcdCanvasRect,
    xcdCanvasBox,
    xcdCanvasArc,
    xcdCanvasSector,
    xcdCanvasChord,
    xcdCanvasText,
    xcdText,

    --
    -- primitives with double as arguments instead of integer
    --
--  xcdfCanvasLine,
--  xcdfCanvasVertex,
--  xcdfCanvasRect,
--  xcdfCanvasBox,
--  xcdfCanvasArc,
--  xcdfCanvasSector,
--  xcdfCanvasChord,
--  xcdfCanvasText,

    --
    -- attributes
    --
    xcdCanvasSetForeground,
    xcdCanvasSetBackground,
    xcdCanvasForeground,
    xcdCanvasBackground,
    xcdForeground,
    xcdBackground,
    xcdCanvasBackOpacity,
    xcdCanvasWriteMode,
    xcdCanvasLineStyle,
    xcdCanvasLineStyleDashes,
    xcdCanvasLineWidth,
    xcdCanvasLineJoin,
    xcdCanvasLineCap,
    xcdCanvasInteriorStyle,
    xcdCanvasHatch,
    xcdCanvasStipple,
    xcdCanvasGetStipple,
    xcdCanvasPattern,
    xcdCanvasGetPattern,
    xcdCanvasFillMode,
    xcdCanvasFont,
    xcdCanvasGetFont,
    xcdCanvasNativeFont,
    xcdNativeFont,
    xcdCanvasTextAlignment,
    xcdTextAlignment,
    xcdCanvasTextOrientation,
    xcdCanvasMarkType,
    xcdCanvasMarkSize,

    --
    --  vector text
    --
    xcdCanvasVectorText,
    xcdCanvasMultiLineVectorText,

    --
    -- vector text attributes
    --
    xcdCanvasVectorFont,
    xcdCanvasVectorFontSize,
    xcdCanvasGetVectorFontSize,
    xcdCanvasVectorTextDirection,
    xcdCanvasVectorTextTransform,
    xcdCanvasVectorTextSize,
    xcdCanvasVectorCharSize,

    --
    --  vector text properties 
    --
    xcdCanvasGetVectorTextSize,
    xcdCanvasGetVectorTextBounds,

    --
    -- properties
    --
    xcdCanvasGetFontDim,
    xcdCanvasGetTextSize,
    xcdCanvasGetTextBox,
--  xcdfCanvasGetTextBox,
    xcdCanvasGetTextBounds,
    xcdCanvasGetColorPlanes,

    --
    -- color 
    --
    xcdCanvasPalette,

    --
    -- client images 
    --
    xcdCanvasGetImageRGB,
    xcdCanvasPutImageRectRGB,
    xcdCanvasPutImageRectRGBA,
    xcdCanvasPutImageRectMap,

    --
    -- server images 
    --
--  xcdCanvasCreateImage,
--  xcdKillImage,
--  xcdCanvasGetImage,
--  xcdCanvasPutImageRect,
--  xcdCanvasScrollArea,

    --
    -- bitmap 
    --
    xcdCreateBitmap,
    xcdInitBitmapRGB,
    xcdInitBitmapRGBA,
--  xcdInitBitmapMAP,
    xcdKillBitmap,
    xcdBitmapGetData,
    xcdBitmapSetRect,
    xcdCanvasPutBitmap,
    xcdCanvasGetBitmap,
    xcdBitmapRGB2Map,

    --
    -- color 
    --
    xcdEncodeColor,
    xcdEncodeColorAlpha,
    xcdDecodeColor,
    xcdDecodeColorAlpha,
    xcdDecodeAlpha,
    xcdEncodeAlpha,
    xcdRGB2Map,

    --
    --  coordinate transformation
    --
    xwdCanvasWindow,
    xwdCanvasGetWindow,
    xwdCanvasViewport,
    xwdCanvasGetViewport,
    xwdCanvasWorld2Canvas,
    xwdCanvasWorld2CanvasSize,
    xwdCanvasCanvas2World,

    --
    -- clipping region
    --
    xwdCanvasClipArea,
    xwdCanvasGetClipArea,
    xwdCanvasIsPointInRegion,
    xwdCanvasOffsetRegion,
    xwdCanvasGetRegionBox,
    xwdCanvasHardcopy,

    --
    --  world draw primitives
    --
    xwdCanvasPixel,
    xwdCanvasMark,
    xwdCanvasLine,
    xwdCanvasVertex,
    xwdCanvasRect,
    xwdCanvasBox,
    xwdCanvasArc,
    xwdCanvasSector,
    xwdCanvasChord,
    xwdCanvasText,

    --
    -- world draw images
    --
    xwdCanvasPutImageRect,
    xwdCanvasPutImageRectRGB,
    xwdCanvasPutImageRectRGBA,
    xwdCanvasPutImageRectMap,
    xwdCanvasPutBitmap,

    --
    -- world draw attributes
    --
    xwdCanvasLineWidth,
    xwdCanvasFont,
    xwdCanvasGetFont,
    xwdCanvasGetFontDim,
    xwdCanvasMarkSize,
    xwdCanvasGetTextSize,
    xwdCanvasGetTextBox,
    xwdCanvasGetTextBounds,
    xwdCanvasStipple,
    xwdCanvasPattern,

    --
    -- world draw vector text
    --
    xwdCanvasVectorTextDirection,
    xwdCanvasVectorTextSize,
    xwdCanvasGetVectorTextSize,
    xwdCanvasVectorCharSize,
    xwdCanvasVectorText,
    xwdCanvasMultiLineVectorText,
    xwdCanvasGetVectorTextBounds,

    xwdCanvasGetImageRGB,

    $

procedure iup_init_cd()
    if hCd=0 then
        hCd = iup_open_dll({"cd.dll",
                            "libcd.so",
                            "libcd.dylib"})

        hCdIup = iup_open_dll({"iupcd.dll",
                               "libiupcd.so",
                               "libiupcd.dylib"})

        hCdGL = iup_open_dll({"cdgl.dll",
                              "libcdgl.so",
                              "libcdgl.dylib"})

        hCdIm = iup_open_dll({"cdim.dll",
                              "libcdim.so",
                              "libcdim.dylib"})

        hCdPDF = iup_open_dll({"cdpdf.dll",
                              "libcdpdf.so",
                              "libcdpdf.dylib"})

        --
        -- Version Information Routines
        --
        xcdVersion       = iup_c_func(hCd, "cdVersion", {}, P)
        xcdVersionDate   = iup_c_func(hCd, "cdVersionDate", {}, P)
        xcdVersionNumber = iup_c_func(hCd, "cdVersionNumber", {}, I)

        xcdCreateCanvas     = iup_c_func(hCd, "cdCreateCanvas", {P,P}, P)
        xcdKillCanvas       = iup_c_proc(hCd, "cdKillCanvas", {P})
        xcdCanvasGetContext = iup_c_func(hCd, "cdCanvasGetContext", {P}, P)
        xcdCanvasActivate   = iup_c_func(hCd, "cdCanvasActivate", {P}, I)
        xcdActiveCanvas     = iup_c_func(hCd, "cdActiveCanvas", {}, P)
        xcdCanvasDeactivate = iup_c_proc(hCd, "cdCanvasDeactivate", {P})
        xcdUseContextPlus   = iup_c_proc(hCd, "cdUseContextPlus", {I})
        xcdInitContextPlus  = define_c_proc(hCd, "cdInitContextPlus", {})   -- (-1 is handled OK)

        --
        -- Context Routines
        --
        xcdContextRegisterCallback  = iup_c_func(hCd, "cdContextRegisterCallback", {P,I,P}, I)
        xcdContextCaps              = iup_c_func(hCd, "cdContextCaps", {P}, UL)
        xcdCanvasPlay               = iup_c_func(hCd, "cdCanvasPlay", {P,P,I,I,I,I,P}, I)

        xcdContextImImage           = iup_c_func(hCdIm, "cdContextImImage", {}, P)
        xcdCanvasPutImImage         = iup_c_proc(hCdIm, "cdCanvasPutImImage", {P,P,I,I,I,I})

        --
        -- Canvas Initialization Routines
        --
        xcdContextIup           = iup_c_func(hCdIup, "cdContextIup", {}, P)
        xcdContextIupDraw       = iup_c_func(hCdIup, "cdContextIupDraw", {}, P)
        xcdContextPrinter       = iup_c_func(hCd, "cdContextPrinter", {}, P)
        xcdContextPS            = iup_c_func(hCd, "cdContextPS", {}, P)
        xcdContextPicture       = iup_c_func(hCd, "cdContextPicture", {}, P)
        xcdContextGL            = iup_c_func(hCdGL,"cdContextGL", {}, P)
        xcdContextIupDBuffer    = iup_c_func(hCdIup,"cdContextIupDBuffer", {}, P)
        xcdContextDBuffer       = iup_c_func(hCd, "cdContextDBuffer", {}, P)
        xcdContextImageRGB      = iup_c_func(hCd, "cdContextImageRGB", {}, P)
        xcdContextDBufferRGB    = iup_c_func(hCd, "cdContextDBufferRGB", {}, P)
        xcdContextPPTX          = iup_c_func(hCd, "cdContextPPTX", {}, P, allow_fail:=true)
        xcdContextCGM           = iup_c_func(hCd, "cdContextCGM", {}, P)
        xcdContextMetafile      = iup_c_func(hCd, "cdContextMetafile", {}, P)
        xcdContextWMF           = iup_c_func(hCd, "cdContextWMF", {}, P)
        xcdContextEMF           = iup_c_func(hCd, "cdContextEMF", {}, P)
        xcdContextDebug         = iup_c_func(hCd, "cdContextDebug", {}, P)
        xcdContextDGN           = iup_c_func(hCd, "cdContextDGN", {}, P)
        xcdContextDXF           = iup_c_func(hCd, "cdContextDXF", {}, P)
        xcdContextPDF           = iup_c_func(hCdPDF, "cdContextPDF", {}, P)
        xcdContextSVG           = iup_c_func(hCd, "cdContextSVG", {}, P)
        xcdContextClipboard     = iup_c_func(hCd, "cdContextClipboard", {}, P)

--DEV..
if platform()=WINDOWS then
        xcdContextNativeWindow  = iup_c_func(hCd, "cdContextNativeWindow", {},P)
        xcdGetScreenSize        = iup_c_proc(hCd, "cdGetScreenSize", {P,P,P,P})
        xcdGetScreenColorPlanes = iup_c_func(hCd, "cdGetScreenColorPlanes", {},I)
end if


--doc: (done)
-- Note: The constants CD_IUP etc are initialised with abtruse values: by "abtruse" (not a real word) I mean a value designed to trigger an error 
--  if used directly, that makes some sense when debugging said error, and is substituted (within the pGUI wrapper) by a proper value, ie/eg 
--  CD_IUP is {"CD_IUP"}, which is automatically replaced with the result of cd_context({"CD_IUP"}). Should you need the proper values outside 
--  of pGUI, just use eg cd_context(CD_IUP).

        XCD_IUP          = c_func(xcdContextIup, {})
        XCD_IUPDRAW      = c_func(xcdContextIupDraw, {})
        XCD_PRINTER      = c_func(xcdContextPrinter, {})
        XCD_PS           = c_func(xcdContextPS, {})
        XCD_PICTURE      = c_func(xcdContextPicture, {})
        XCD_GL           = c_func(xcdContextGL, {})
        XCD_IUPDBUFFER   = c_func(xcdContextIupDBuffer, {})
--      XCD_NATIVEWINDOW = c_func(xcdContextNativeWindow, {})
        XCD_DBUFFER      = c_func(xcdContextDBuffer, {})
        XCD_IMIMAGE      = c_func(xcdContextImImage,{})
        XCD_PPTX         = iff(xcdContextPPTX=NULL?NULL:c_func(xcdContextPPTX,{}))
        XCD_IMAGERGB     = c_func(xcdContextImageRGB,{})
        XCD_DBUFFERRGB   = c_func(xcdContextDBufferRGB,{})
        XCD_CGM          = c_func(xcdContextCGM,{})
        XCD_METAFILE     = c_func(xcdContextMetafile,{})
        XCD_WMF          = c_func(xcdContextWMF,{})
        XCD_EMF          = c_func(xcdContextEMF,{})
        XCD_DEBUG        = c_func(xcdContextDebug,{})
        XCD_DGN          = c_func(xcdContextDGN,{})
        XCD_DXF          = c_func(xcdContextDXF,{})
        XCD_PDF          = c_func(xcdContextPDF,{})
        XCD_SVG          = c_func(xcdContextSVG,{})
        XCD_CLIPBOARD    = c_func(xcdContextClipboard,{})

        XCDS = {XCD_IUP,
                XCD_IUPDRAW,
                XCD_PRINTER,
                XCD_PS,
                XCD_PICTURE,
                XCD_GL,
                XCD_IUPDBUFFER,
--              XCD_NATIVEWINDOW,
                XCD_DBUFFER,
                XCD_IMIMAGE,
                XCD_PPTX,
                XCD_IMAGERGB,
                XCD_DBUFFERRGB,
                XCD_CGM,
                XCD_METAFILE,
                XCD_WMF,
                XCD_EMF,
                XCD_DEBUG,
                XCD_DGN,
                XCD_DXF,
                XCD_PDF,
                XCD_SVG,
                XCD_CLIPBOARD}

        --
        -- control
        --
        xcdCanvasSimulate       = iup_c_func(hCd, "cdCanvasSimulate", {P,I},I)
        xcdCanvasFlush          = iup_c_proc(hCd, "cdCanvasFlush", {P})
        xcdCanvasClear          = iup_c_proc(hCd, "cdCanvasClear", {P})
        xcdClear                = iup_c_proc(hCd, "cdClear", {})
        xcdCanvasSaveState      = iup_c_func(hCd, "cdCanvasSaveState", {P},P)
        xcdCanvasRestoreState   = iup_c_proc(hCd, "cdCanvasRestoreState", {P,P})
        xcdReleaseState         = iup_c_proc(hCd, "cdReleaseState", {P})
        xcdCanvasSetAttribute   = iup_c_proc(hCd, "cdCanvasSetAttribute", {P,P,P})
        xcdCanvasGetAttribute   = iup_c_func(hCd, "cdCanvasGetAttribute", {P,P},P)

        --
        --  coordinate transformation 
        --
        xcdCanvasGetSize            = iup_c_proc(hCd, "cdCanvasGetSize", {P,I,I,P,P})
--      xcdCanvasUpdateYAxis        = iup_c_func(hCd, "cdCanvasUpdateYAxis", {P,P},I)
        xcdfCanvasUpdateYAxis       = iup_c_func(hCd, "cdfCanvasUpdateYAxis", {P,P},D)
--      xcdCanvasInvertYAxis        = iup_c_func(hCd, "cdCanvasInvertYAxis", {P,I},I)
        xcdfCanvasInvertYAxis       = iup_c_func(hCd, "cdfCanvasInvertYAxis", {P,D},D)
        xcdCanvasMM2Pixel           = iup_c_proc(hCd, "cdCanvasMM2Pixel", {P,D,D,P,P})
--      xcdfCanvasMM2Pixel          = iup_c_proc(hCd, "cdfCanvasMM2Pixel", {P,D,D,P,P})
--      xcdCanvasPixel2MM           = iup_c_proc(hCd, "cdCanvasPixel2MM", {P,I,I,P,P})
        xcdfCanvasPixel2MM          = iup_c_proc(hCd, "cdfCanvasPixel2MM", {P,D,D,P,P})
--      xcdCanvasOrigin             = iup_c_proc(hCd, "cdCanvasOrigin", {P,I,I})
        xcdfCanvasOrigin            = iup_c_proc(hCd, "cdfCanvasOrigin", {P,D,D})
--      xcdCanvasGetOrigin          = iup_c_proc(hCd, "cdCanvasGetOrigin", {P,P,P})
        xcdfCanvasGetOrigin         = iup_c_proc(hCd, "cdfCanvasGetOrigin", {P,P,P})
        xcdCanvasTransform          = iup_c_proc(hCd, "cdCanvasTransform", {P,P})
        xcdCanvasGetTransform       = iup_c_func(hCd, "cdCanvasGetTransform", {P},P)
        xcdCanvasTransformMultiply  = iup_c_proc(hCd, "cdCanvasTransformMultiply", {P,P})
        xcdCanvasTransformRotate    = iup_c_proc(hCd, "cdCanvasTransformRotate", {P,D})
        xcdCanvasTransformScale     = iup_c_proc(hCd, "cdCanvasTransformScale", {P,D,D})
        xcdCanvasTransformTranslate = iup_c_proc(hCd, "cdCanvasTransformTranslate", {P,D,D})
--      xcdCanvasTransformPoint     = iup_c_proc(hCd, "cdCanvasTransformPoint", {P,I,I,P,P})
        xcdfCanvasTransformPoint    = iup_c_proc(hCd, "cdfCanvasTransformPoint", {P,D,D,P,P})

        --
        --   clipping 
        --
        xcdCanvasClip           = iup_c_func(hCd, "cdCanvasClip", {P,I},I)
--      xcdCanvasClipArea       = iup_c_proc(hCd, "cdCanvasClipArea", {P,I,I,I,I})
        xcdfCanvasClipArea      = iup_c_proc(hCd, "cdfCanvasClipArea", {P,D,D,D,D})
--      xcdCanvasGetClipArea    = iup_c_func(hCd, "cdCanvasGetClipArea", {P,P,P,P,P},I)
        xcdfCanvasGetClipArea   = iup_c_func(hCd, "cdfCanvasGetClipArea", {P,P,P,P,P},I)

        --
        -- clipping region 
        --
        xcdCanvasIsPointInRegion    = iup_c_func(hCd, "cdCanvasIsPointInRegion", {P,I,I},I)
        xcdCanvasOffsetRegion       = iup_c_proc(hCd, "cdCanvasOffsetRegion", {P,I,I})
        xcdCanvasGetRegionBox       = iup_c_proc(hCd, "cdCanvasGetRegionBox", {P,P,P,P,P,P})
        xcdCanvasRegionCombineMode  = iup_c_func(hCd, "cdCanvasRegionCombineMode", {P,I},I)

        --
        -- drawing primitives
        --
        xcdCanvasPixel      = iup_c_proc(hCd, "cdCanvasPixel", {P,I,I,I})
        xcdCanvasMark       = iup_c_proc(hCd, "cdCanvasMark", {P,I,I})
        xcdCanvasLine       = iup_c_proc(hCd, "cdCanvasLine", {P,I,I,I,I})
        xcdLine             = iup_c_proc(hCd, "cdLine", {I,I,I,I})
        xcdCanvasBegin      = iup_c_proc(hCd, "cdCanvasBegin", {P,I})
        xcdCanvasVertex     = iup_c_proc(hCd, "cdCanvasVertex", {P,I,I})
        xcdCanvasEnd        = iup_c_proc(hCd, "cdCanvasEnd", {P})
        xcdCanvasPathSet    = iup_c_proc(hCd, "cdCanvasPathSet", {P,I})
        xcdCanvasRect       = iup_c_proc(hCd, "cdCanvasRect", {P,I,I,I,I})
        xcdCanvasBox        = iup_c_proc(hCd, "cdCanvasBox", {P,I,I,I,I})
        xcdCanvasArc        = iup_c_proc(hCd, "cdCanvasArc", {P,I,I,I,I,D,D})
        xcdCanvasSector     = iup_c_proc(hCd, "cdCanvasSector", {P,I,I,I,I,D,D})
        xcdCanvasChord      = iup_c_proc(hCd, "cdCanvasChord", {P,I,I,I,I,D,D})
        xcdCanvasText       = iup_c_proc(hCd, "cdCanvasText", {P,I,I,P})
        xcdText             = iup_c_proc(hCd, "cdText", {I,I,P})

        --
        -- primitives with double as arguments instead of integer
        --
--      xcdfCanvasLine      = iup_c_proc(hCd, "cdfCanvasLine", {P,D,D,D,D})
--      xcdfCanvasVertex    = iup_c_proc(hCd, "cdfCanvasVertex", {P,D,D})
--      xcdfCanvasRect      = iup_c_proc(hCd, "cdfCanvasRect", {P,D,D,D,D})
--      xcdfCanvasBox       = iup_c_proc(hCd, "cdfCanvasBox", {P,D,D,D,D})
--      xcdfCanvasArc       = iup_c_proc(hCd, "cdfCanvasArc", {P,D,D,D,D,D,D})
--      xcdfCanvasSector    = iup_c_proc(hCd, "cdfCanvasSector", {P,D,D,D,D,D,D})
--      xcdfCanvasChord     = iup_c_proc(hCd, "cdfCanvasChord", {P,D,D,D,D,D,D})
--      xcdfCanvasText      = iup_c_proc(hCd, "cdfCanvasText", {P,D,D,P})

        --
        -- attributes
        --
        xcdCanvasSetForeground   = iup_c_proc(hCd, "cdCanvasSetForeground", {P,I})
        xcdCanvasSetBackground   = iup_c_proc(hCd, "cdCanvasSetBackground", {P,I})
        xcdCanvasForeground      = iup_c_func(hCd, "cdCanvasForeground", {P,L}, L)
        xcdCanvasBackground      = iup_c_func(hCd, "cdCanvasBackground", {P,L}, L)
        xcdForeground            = iup_c_proc(hCd, "cdForeground", {L})
        xcdBackground            = iup_c_proc(hCd, "cdBackground", {L})
        xcdCanvasBackOpacity     = iup_c_func(hCd, "cdCanvasBackOpacity", {P,I}, I)
        xcdCanvasWriteMode       = iup_c_func(hCd, "cdCanvasWriteMode", {P,I}, I)
        xcdCanvasLineStyle       = iup_c_func(hCd, "cdCanvasLineStyle", {P,I}, I)
        xcdCanvasLineStyleDashes = iup_c_proc(hCd, "cdCanvasLineStyleDashes", {P,P, I})
        xcdCanvasLineWidth       = iup_c_func(hCd, "cdCanvasLineWidth", {P,I}, I)
        xcdCanvasLineJoin        = iup_c_func(hCd, "cdCanvasLineJoin", {P,I}, I)
        xcdCanvasLineCap         = iup_c_func(hCd, "cdCanvasLineCap", {P,I}, I)
        xcdCanvasInteriorStyle   = iup_c_func(hCd, "cdCanvasInteriorStyle", {P,I}, I)
        xcdCanvasHatch           = iup_c_func(hCd, "cdCanvasHatch", {P,I}, I)
        xcdCanvasStipple         = iup_c_proc(hCd, "cdCanvasStipple", {P,I,I,P})
        xcdCanvasGetStipple      = iup_c_func(hCd, "cdCanvasGetStipple", {P,P,P}, P)
        xcdCanvasPattern         = iup_c_proc(hCd, "cdCanvasPattern", {P,I,I,P})
        xcdCanvasGetPattern      = iup_c_func(hCd, "cdCanvasGetPattern", {P,P,P}, P)
        xcdCanvasFillMode        = iup_c_func(hCd, "cdCanvasFillMode", {P,I}, I)
        xcdCanvasFont            = iup_c_proc(hCd, "cdCanvasFont", {P,P,I,I})
        xcdCanvasGetFont         = iup_c_proc(hCd, "cdCanvasGetFont", {P,P,P,P})
        xcdCanvasNativeFont      = iup_c_func(hCd, "cdCanvasNativeFont", {P,P}, P)
        xcdNativeFont            = iup_c_func(hCd, "cdNativeFont", {P}, P)
        xcdCanvasTextAlignment   = iup_c_func(hCd, "cdCanvasTextAlignment", {P,I}, I)
        xcdTextAlignment         = iup_c_func(hCd, "cdTextAlignment", {I}, I)
        xcdCanvasTextOrientation = iup_c_func(hCd, "cdCanvasTextOrientation", {P,D}, D)
        xcdCanvasMarkType        = iup_c_func(hCd, "cdCanvasMarkType", {P,I}, I)
        xcdCanvasMarkSize        = iup_c_func(hCd, "cdCanvasMarkSize", {P,I}, I)

        --
        --  vector text
        --
        xcdCanvasVectorText             = iup_c_proc(hCd, "cdCanvasVectorText", {P,I,I,P})
        xcdCanvasMultiLineVectorText    = iup_c_proc(hCd, "cdCanvasMultiLineVectorText", {P,I,I,P})

        --
        -- vector text attributes
        --
        xcdCanvasVectorFont             = iup_c_func(hCd, "cdCanvasVectorFont", {P,P},P)
        xcdCanvasVectorFontSize         = iup_c_proc(hCd, "cdCanvasVectorFontSize", {P,D,D})
        xcdCanvasGetVectorFontSize      = iup_c_proc(hCd, "cdCanvasGetVectorFontSize", {P,P,P})
        xcdCanvasVectorTextDirection    = iup_c_proc(hCd, "cdCanvasVectorTextDirection", {P,I,I,I,I})
        xcdCanvasVectorTextTransform    = iup_c_func(hCd, "cdCanvasVectorTextTransform", {P,P},P)
        xcdCanvasVectorTextSize         = iup_c_proc(hCd, "cdCanvasVectorTextSize", {P,I,I,P})
        xcdCanvasVectorCharSize         = iup_c_func(hCd, "cdCanvasVectorCharSize", {P,I},I)

        --
        --  vector text properties 
        --
        xcdCanvasGetVectorTextSize      = iup_c_proc(hCd, "cdCanvasGetVectorTextSize", {P,P,P,P})
        xcdCanvasGetVectorTextBounds    = iup_c_proc(hCd, "cdCanvasGetVectorTextBounds", {P,P,I,I,P})

        --
        -- properties
        --
        xcdCanvasGetFontDim     = iup_c_proc(hCd, "cdCanvasGetFontDim", {P,P,P,P,P})
        xcdCanvasGetTextSize    = iup_c_proc(hCd, "cdCanvasGetTextSize", {P,P,P,P})
        xcdCanvasGetTextBox     = iup_c_proc(hCd, "cdCanvasGetTextBox", {P,P,I,I,P,P,P,P})
--      xcdfCanvasGetTextBox    = iup_c_proc(hCd, "cdfCanvasGetTextBox", {P,D,D,P,P,P,P,P})
        xcdCanvasGetTextBounds  = iup_c_proc(hCd, "cdCanvasGetTextBounds", {P,I,I,P,P})
        xcdCanvasGetColorPlanes = iup_c_func(hCd, "cdCanvasGetColorPlanes", {P},I)

        --
        -- color 
        --
        xcdCanvasPalette = iup_c_proc(hCd, "cdCanvasPalette", {P,I,P,I})

        --
        -- client images 
        --
        xcdCanvasGetImageRGB        = iup_c_proc(hCd, "cdCanvasGetImageRGB", {P,P,P,P,I,I,I,I})
        xcdCanvasPutImageRectRGB    = iup_c_proc(hCd, "cdCanvasPutImageRectRGB", {P,I,I,P,P,P,I,I,I,I,I,I,I,I})
        xcdCanvasPutImageRectRGBA   = iup_c_proc(hCd, "cdCanvasPutImageRectRGBA", {P,I,I,P,P,P,P,I,I,I,I,I,I,I,I})
        xcdCanvasPutImageRectMap    = iup_c_proc(hCd, "cdCanvasPutImageRectMap", {P,I,I,P,P,I,I,I,I,I,I,I,I})

        --
        -- server images 
        --
--      xcdCanvasCreateImage    = iup_c_func(hCd, "cdCanvasCreateImage", {P,I,I},P)
--      xcdKillImage            = iup_c_proc(hCd, "cdKillImage", {P})
--      xcdCanvasGetImage       = iup_c_proc(hCd, "cdCanvasGetImage", {P,P,I,I})
--      xcdCanvasPutImageRect   = iup_c_proc(hCd, "cdCanvasPutImageRect", {P,P,I,I,I,I,I,I})
--      xcdCanvasScrollArea     = iup_c_proc(hCd, "cdCanvasScrollArea", {P,I,I,I,I,I,I})

        --
        -- bitmap 
        --
        xcdCreateBitmap     = iup_c_func(hCd, "cdCreateBitmap", {I,I,I},P)
        xcdInitBitmapRGB    = iup_c_func(hCd, "cdInitBitmap", {I,I,I,P,P,P},P) -- type CD_RGB
        xcdInitBitmapRGBA   = iup_c_func(hCd, "cdInitBitmap", {I,I,I,P,P,P,P},P) -- type CD_RGBA
    --PL unused
    --  xcdInitBitmapMAP    = iup_c_func(hCd, "cdInitBitmap", {I,I,I,P,P},P) -- type CD_MAP
        xcdKillBitmap       = iup_c_proc(hCd, "cdKillBitmap", {P})
        xcdBitmapGetData    = iup_c_func(hCd, "cdBitmapGetData", {P,I},P)
        xcdBitmapSetRect    = iup_c_proc(hCd, "cdBitmapSetRect", {P,I,I,I,I})
        xcdCanvasPutBitmap  = iup_c_proc(hCd, "cdCanvasPutBitmap", {P,P,I,I,I,I})
        xcdCanvasGetBitmap  = iup_c_proc(hCd, "cdCanvasGetBitmap", {P,P,I,I})
        xcdBitmapRGB2Map    = iup_c_proc(hCd, "cdBitmapRGB2Map", {P,P})

        --
        -- color 
        --
        xcdEncodeColor      = iup_c_func(hCd, "cdEncodeColor", {UC,UC,UC},L)
        xcdEncodeColorAlpha = iup_c_func(hCd, "cdEncodeColorAlpha", {UC,UC,UC,UC},L)
        xcdDecodeColor      = iup_c_proc(hCd, "cdDecodeColor", {L,P,P,P})
        xcdDecodeColorAlpha = iup_c_proc(hCd, "cdDecodeColorAlpha", {L,P,P,P,P})
        xcdDecodeAlpha      = iup_c_func(hCd, "cdDecodeAlpha", {L},UC)
        xcdEncodeAlpha      = iup_c_func(hCd, "cdEncodeAlpha", {L,UC},L)
        xcdRGB2Map          = iup_c_proc(hCd, "cdRGB2Map", {I,I,P,P,P,P,I,P})

        --
        --  coordinate transformation
        --
        xwdCanvasWindow             = iup_c_proc(hCd, "wdCanvasWindow", {P,D,D,D,D})
        xwdCanvasGetWindow          = iup_c_proc(hCd, "wdCanvasGetWindow", {P,P,P,P,P})
        xwdCanvasViewport           = iup_c_proc(hCd, "wdCanvasViewport", {P,I,I,I,I})
        xwdCanvasGetViewport        = iup_c_proc(hCd, "wdCanvasGetViewport", {P,P,P,P,P})
        xwdCanvasWorld2Canvas       = iup_c_proc(hCd, "wdCanvasWorld2Canvas", {P,D,D,P,P})
        xwdCanvasWorld2CanvasSize   = iup_c_proc(hCd, "wdCanvasWorld2CanvasSize", {P,D,D,P,P})
        xwdCanvasCanvas2World       = iup_c_proc(hCd, "wdCanvasCanvas2World", {P,I,I,D,D})

        --
        -- clipping region
        --
        xwdCanvasClipArea           = iup_c_proc(hCd, "wdCanvasClipArea", {P,D,D,D,D})
        xwdCanvasGetClipArea        = iup_c_func(hCd, "wdCanvasGetClipArea", {P,P,P,P,P},I)
        xwdCanvasIsPointInRegion    = iup_c_func(hCd, "wdCanvasIsPointInRegion", {P,D,D},I)
        xwdCanvasOffsetRegion       = iup_c_proc(hCd, "wdCanvasOffsetRegion", {P,D,D})
        xwdCanvasGetRegionBox       = iup_c_proc(hCd, "wdCanvasGetRegionBox", {P,P,P,P,P})
        xwdCanvasHardcopy           = iup_c_proc(hCd, "wdCanvasHardcopy", {P,P,P,P})

        --
        --  world draw primitives
        --
        xwdCanvasPixel  = iup_c_proc(hCd, "wdCanvasPixel", {P,D,D,L})
        xwdCanvasMark   = iup_c_proc(hCd, "wdCanvasMark", {P,D,D})
        xwdCanvasLine   = iup_c_proc(hCd, "wdCanvasLine", {P,D,D,D,D})
        xwdCanvasVertex = iup_c_proc(hCd, "wdCanvasVertex", {P,D,D})
        xwdCanvasRect   = iup_c_proc(hCd, "wdCanvasRect", {P,D,D,D,D})
        xwdCanvasBox    = iup_c_proc(hCd, "wdCanvasBox", {P,D,D,D,D})
        xwdCanvasArc    = iup_c_proc(hCd, "wdCanvasArc", {P,D,D,D,D,D,D})
        xwdCanvasSector = iup_c_proc(hCd, "wdCanvasSector", {P,D,D,D,D,D,D})
        xwdCanvasChord  = iup_c_proc(hCd, "wdCanvasChord", {P,D,D,D,D,D,D})
        xwdCanvasText   = iup_c_proc(hCd, "wdCanvasText", {P,D,D,P})

        --
        -- world draw images
        --
        xwdCanvasPutImageRect       = iup_c_proc(hCd, "wdCanvasPutImageRect", {P,P,D,D,D,D,D,D})
        xwdCanvasPutImageRectRGB    = iup_c_proc(hCd, "wdCanvasPutImageRectRGB", {P,I,I,P,P,P,D,D,D,D,D,D,D,D,D,D})
        xwdCanvasPutImageRectRGBA   = iup_c_proc(hCd, "wdCanvasPutImageRectRGBA", {P,I,I,P,P,P,P,D,D,D,D,D,D,D,D})
        xwdCanvasPutImageRectMap    = iup_c_proc(hCd, "wdCanvasPutImageRectMap", {P,I,I,P,P,D,D,D,D,D,D,D,D})
        xwdCanvasPutBitmap          = iup_c_proc(hCd, "wdCanvasPutBitmap", {P,P,D,D,D,D})

        --
        -- world draw attributes
        --
        xwdCanvasLineWidth      = iup_c_func(hCd, "wdCanvasLineWidth", {P,D},D)
        xwdCanvasFont           = iup_c_proc(hCd, "wdCanvasFont", {P,P,I,D})
        xwdCanvasGetFont        = iup_c_proc(hCd, "wdCanvasGetFont", {P,P,P,P})
        xwdCanvasGetFontDim     = iup_c_proc(hCd, "wdCanvasGetFontDim", {P,P,P,P,P})
        xwdCanvasMarkSize       = iup_c_func(hCd, "wdCanvasMarkSize", {P,D},D)
        xwdCanvasGetTextSize    = iup_c_proc(hCd, "wdCanvasGetTextSize", {P,P,P,P})
        xwdCanvasGetTextBox     = iup_c_proc(hCd, "wdCanvasGetTextBox", {P,D,D,P,P,P,P,P})
        xwdCanvasGetTextBounds  = iup_c_proc(hCd, "wdCanvasGetTextBounds", {P,D,D,P,P})
        xwdCanvasStipple        = iup_c_proc(hCd, "wdCanvasStipple", {P,I,I,P,D,D})
        xwdCanvasPattern        = iup_c_proc(hCd, "wdCanvasPattern", {P,I,I,P,D,D})

        --
        -- world draw vector text
        --
        xwdCanvasVectorTextDirection    = iup_c_proc(hCd, "wdCanvasVectorTextDirection", {P,D,D,D,D})
        xwdCanvasVectorTextSize         = iup_c_proc(hCd, "wdCanvasVectorTextSize", {P,D,D,P})
        xwdCanvasGetVectorTextSize      = iup_c_proc(hCd, "wdCanvasGetVectorTextSize", {P,P,P,P})
        xwdCanvasVectorCharSize         = iup_c_func(hCd, "wdCanvasVectorCharSize", {P,D},D)
        xwdCanvasVectorText             = iup_c_proc(hCd, "wdCanvasVectorText", {P,D,D,P})
        xwdCanvasMultiLineVectorText    = iup_c_proc(hCd, "wdCanvasMultiLineVectorText", {P,D,D,P})
        xwdCanvasGetVectorTextBounds    = iup_c_proc(hCd, "wdCanvasGetVectorTextBounds", {P,P,D,D,P})

        xwdCanvasGetImageRGB = iup_c_proc(hCd, "wdCanvasGetImageRGB", {P,P,P,P,D,D,I,I})

    end if
end procedure

global function cdVersion()
    iup_init_cd()
    return peek_string(c_func(xcdVersion, {}))
end function

global function cdVersionDate()
    iup_init_cd()
    return peek_string(c_func(xcdVersionDate, {}))
end function

global function cdVersionNumber()
    iup_init_cd()
    return c_func(xcdVersionNumber, {})
end function

----****
---- === Canvas Initialization Routines
----
--
--global function cdContextIup()    -- removed 2/5/18: replace with cd_context(CD_IUP)
--  iup_init_cd()
--  return XCD_IUP
--end function

global function cd_context(object context)
-- translates eg {"CD_IUP"} to an atom, in that case from c_func(xcdContextIup, {})
    iup_init_cd()
    if sequence(context) then
        context = XCDS[find(context[1],XCD_STR)]
    end if
    return context
end function

global type cdContext(object context)
    return cd_context(context)!=NULL
end type

----ifdef WINDOWS then
--constant
--  xcdContextNativeWindow  = iup_c_func(hCd, "cdContextNativeWindow", {},P),
--  xcdGetScreenSize        = iup_c_proc(hCd, "cdGetScreenSize", {P,P,P,P}),
--  xcdGetScreenColorPlanes = iup_c_func(hCd, "cdGetScreenColorPlanes", {},I)
--
--global constant
--  CD_NATIVEWINDOW = c_func(xcdContextNativeWindow, {})
----end ifdef

global function cdGetScreenSize()
--ifdef WINDOWS then
--DEV machine_bits?
    atom w = allocate(24),
         h = w+4,
         w_mm = h+4,
         h_mm = w_mm+8
    iup_init_cd()
    c_proc(xcdGetScreenSize, {w, h, w_mm, h_mm})
    sequence data = peek4s({w, 2}) & iup_peek_double({w_mm, 2})
    free(w)
    return data
--elsedef
--  return 0
--end ifdef
end function

global function cdGetScreenColorPlanes()
    iup_init_cd()
--ifdef WINDOWS then
    return c_func(xcdGetScreenColorPlanes, {})
--elsedef
--  return 0
--end ifdef
end function

--constant
--  xcdCreateCanvas     = iup_c_func(hCd, "cdCreateCanvas", {P,P}, P),
--  xcdKillCanvas       = iup_c_proc(hCd, "cdKillCanvas", {P}),
--  xcdCanvasGetContext = iup_c_func(hCd, "cdCanvasGetContext", {P}, P),
--  xcdCanvasActivate   = iup_c_func(hCd, "cdCanvasActivate", {P}, I),
--  xcdActiveCanvas     = iup_c_func(hCd, "cdActiveCanvas", {}, P),
--  xcdCanvasDeactivate = iup_c_proc(hCd, "cdCanvasDeactivate", {P}),
--  xcdUseContextPlus   = iup_c_proc(hCd, "cdUseContextPlus", {I}),

global function cdCreateCanvas(object hCdContext, atom_string data, sequence params={})
    iup_init_cd()
    if length(params) then
        data = sprintf(data,params)
    end if
    cdCanvan canvas = c_func(xcdCreateCanvas,{cd_context(hCdContext),data})
    return canvas
end function

global procedure cdKillCanvas(cdCanvas canvas)
    c_proc(xcdKillCanvas, {canvas})
end procedure

global function cdCanvasGetContext(cdCanvas canvas)
    object context = c_func(xcdCanvasGetContext, {canvas})
    context = XCD_STR[find(context,XCDS)]
    return context
end function

global procedure cdCanvasActivate(cdCanvas canvas)
    if c_func(xcdCanvasActivate, {canvas})!=CD_OK then ?9/0 end if
end procedure

global function cdActiveCanvas()
    iup_init_cd()
    cdCanvas res = c_func(xcdActiveCanvas,{})
    return res
end function

global procedure cdCanvasDeactivate(cdCanvas canvas)
    c_proc(xcdCanvasDeactivate, {canvas})
end procedure

global procedure cdUseContextPlus(boolean use)
    iup_init_cd()
    c_proc(xcdUseContextPlus, {use})
end procedure

global procedure cdInitContextPlus()
    iup_init_cd()
    if xcdInitContextPlus!=-1 then
        c_proc(xcdInitContextPlus, {})
    end if
end procedure

----****
---- === Context Routines
----
--
--constant
--  xcdContextRegisterCallback  = iup_c_func(hCd, "cdContextRegisterCallback", {P,I,P}, I),
--  xcdContextCaps              = iup_c_func(hCd, "cdContextCaps", {P}, UL),
--  xcdCanvasPlay               = iup_c_func(hCd, "cdCanvasPlay", {P,P,I,I,I,I,P}, I)
--
global function cdCanvasPlay(cdCanvas canvas, cdContext context, integer xmin, xmax, ymin, ymax, atom_string data)
    integer res = c_func(xcdCanvasPlay, {canvas, cd_context(context), xmin, xmax, ymin, ymax, data})
    return res
end function

global function cdContextRegisterCallback(cdContext context, integer cb, atom cbFunc)
    return c_func(xcdContextRegisterCallback, {cd_context(context), cb, cbFunc})
end function

global function cdContextCaps(cdContext context)
    iup_init_cd()
    atom res = c_func(xcdContextCaps, {cd_context(context)})
    return res
end function

--atom hCdIm = 0,
--   xcdContextImImage,
--   xcdCanvasPutImImage,
--   $
--
--procedure iup_init_cdim()
--  if hCdIm=0 then
--      hCdIm = iup_open_dll({"cdim.dll",
--                            "libcdim.so",
--                            "libcdim.dylib"})
--      xcdContextImImage           = iup_c_func(hCdIm, "cdContextImImage", {}, P)
--      xcdCanvasPutImImage         = iup_c_proc(hCdIm, "cdCanvasPutImImage", {P,P,I,I,I,I})
--  end if
--end procedure
--
--cdContext* cdContextImImage(void)
--global function cdContextImImage()    -- use cd_context(CD_IMIMAGE) instead.
--  iup_init_cd()
----    return c_func(xcdContextImImage,{})
--  return XCD_IMIMAGE
--end function

--global constant CD_IMIMAGE = cdContextImImage()

--void cdCanvasPutImImage(cdCanvas* canvas, const imImage* image, int x, int y, int w, int h); [in C]
--void cdfCanvasPutImImage(cdCanvas* canvas, const imImage* image, double x, double y, double w, double h); [in C]
--void wdCanvasPutImImage(cdCanvas* canvas, const imImage* image, double x, double y, double w, double h); (WC) [in C]
global procedure cdCanvasPutImImage(cdCanvas canvas, imImage image, integer x, integer y, integer w, integer h)
    iup_init_cd()
    c_proc(xcdCanvasPutImImage,{canvas,image,x,y,w,h})
end procedure



-----------------------------------------------------------------------------------------
----
---- control
----
-------------------------------------------------------------------------------------------

--global function canvas_simulate(cdCanvas canvas, integer mode)
global function cdCanvasSimulate(cdCanvas canvas, integer mode)
    integer prev_mode = c_func(xcdCanvasSimulate, {canvas, mode})
    return prev_mode
end function

global procedure cdCanvasClear(cdCanvas canvas)
    c_proc(xcdCanvasClear, {canvas})
end procedure

global procedure cdCanvasFlush(cdCanvas canvas)
    c_proc(xcdCanvasFlush, {canvas})
end procedure

global procedure cdClear()
    c_proc(xcdClear,{})
end procedure

--global function canvas_save_state(cdCanvas canvas)
global function cdCanvasSaveState(cdCanvas canvas)
    atom pCdState = c_func(xcdCanvasSaveState, {canvas})
    return pCdState
end function

--global procedure canvas_restore_state(cdCanvas canvas, atom hCdState)
global procedure cdCanvasRestoreState(cdCanvas canvas, atom pCdState)
    c_proc(xcdCanvasRestoreState, {canvas, pCdState})
end procedure

--global procedure canvas_release_state(cdCanvas hCdState)
global procedure cdReleaseState(atom pCdState)
    c_proc(xcdReleaseState, {pCdState})
end procedure

global procedure cdCanvasSetAttribute(cdCanvas canvas, string name, nullable_string val, sequence args={})
    if length(args) then
        val = sprintf(val, args)
    end if
    c_proc(xcdCanvasSetAttribute, {canvas, name, val})
end procedure

global function cdCanvasGetAttribute(cdCanvas canvas, string name)
    atom pRes = c_func(xcdCanvasGetAttribute, {canvas, name})
    if pRes=NULL then return "" end if
    return peek_string(pRes)
end function


------------------------------------------------------------------------------------------
----
----    coordinate transformation 
----
--------------------------------------------------------------------------------------------
--constant
--  xcdCanvasGetSize            = iup_c_proc(hCd, "cdCanvasGetSize", {P,I,I,P,P}),
----    xcdCanvasUpdateYAxis        = iup_c_func(hCd, "cdCanvasUpdateYAxis", {P,P},I),
--  xcdfCanvasUpdateYAxis       = iup_c_func(hCd, "cdfCanvasUpdateYAxis", {P,P},D),
----    xcdCanvasInvertYAxis        = iup_c_func(hCd, "cdCanvasInvertYAxis", {P,I},I),
--  xcdfCanvasInvertYAxis       = iup_c_func(hCd, "cdfCanvasInvertYAxis", {P,D},D),
--  xcdCanvasMM2Pixel           = iup_c_proc(hCd, "cdCanvasMM2Pixel", {P,D,D,P,P}),
----    xcdfCanvasMM2Pixel          = iup_c_proc(hCd, "cdfCanvasMM2Pixel", {P,D,D,P,P}),
----    xcdCanvasPixel2MM           = iup_c_proc(hCd, "cdCanvasPixel2MM", {P,I,I,P,P}),
--  xcdfCanvasPixel2MM          = iup_c_proc(hCd, "cdfCanvasPixel2MM", {P,D,D,P,P}),
----    xcdCanvasOrigin             = iup_c_proc(hCd, "cdCanvasOrigin", {P,I,I}),
--  xcdfCanvasOrigin            = iup_c_proc(hCd, "cdfCanvasOrigin", {P,D,D}),
----    xcdCanvasGetOrigin          = iup_c_proc(hCd, "cdCanvasGetOrigin", {P,P,P}),
--  xcdfCanvasGetOrigin         = iup_c_proc(hCd, "cdfCanvasGetOrigin", {P,P,P}),
--  xcdCanvasTransform          = iup_c_proc(hCd, "cdCanvasTransform", {P,P}),
--  xcdCanvasGetTransform       = iup_c_func(hCd, "cdCanvasGetTransform", {P},P),
--  xcdCanvasTransformMultiply  = iup_c_proc(hCd, "cdCanvasTransformMultiply", {P,P}),
--  xcdCanvasTransformRotate    = iup_c_proc(hCd, "cdCanvasTransformRotate", {P,D}),
--  xcdCanvasTransformScale     = iup_c_proc(hCd, "cdCanvasTransformScale", {P,D,D}),
--  xcdCanvasTransformTranslate = iup_c_proc(hCd, "cdCanvasTransformTranslate", {P,D,D}),
----    xcdCanvasTransformPoint     = iup_c_proc(hCd, "cdCanvasTransformPoint", {P,I,I,P,P}),
--  xcdfCanvasTransformPoint    = iup_c_proc(hCd, "cdfCanvasTransformPoint", {P,D,D,P,P})

global function cdCanvasGetSize(cdCanvas canvas)
--DEV machine_bits()?
    atom pWidth = allocate(2*4+2*8),
         pHeight = pWidth+4,
         pWidth_mm = pHeight+4,
         pHeight_mm = pWidth_mm+8
    c_proc(xcdCanvasGetSize, {canvas, pWidth, pHeight, pWidth_mm, pHeight_mm})
    sequence size = peek4s({pWidth, 2}) & iup_peek_double({pWidth_mm, 2})
    free(pWidth)
    return size     -- {width,height,width_mm,height_mm}
end function

--global function cdCanvasUpdateYAxis(cdCanvas canvas, integer y)
--  atom pY = allocate(4)
--  poke4(pY, y)
--  integer newy = c_func(xcdCanvasUpdateYAxis, {canvas, pY})
--  free(pY) --peek4s(pY)==newy
--  return newy
--end function

--global function cdCanvasUpdateYAxis(cdCanvas canvas, integer y)
--  atom pY = allocate(W)
--  pokeN(pY, y, W)
--  integer newy = c_func(xcdCanvasUpdateYAxis, {canvas, pY})
--  if newy!=peekNS(pY,W,1) then ?9/0 end if
--  free(pY)
--  return newy
--end function

global function cdCanvasUpdateYAxis(cdCanvas canvas, atom y)
    atom pY = allocate(8)
    iup_poke_double(pY, y)
    atom newy = c_func(xcdfCanvasUpdateYAxis, {canvas, pY})
    -- if this triggers we may need to resurrect the int C function
--atom dbg = iup_peek_double(pY)
    if newy!=iup_peek_double(pY) then ?9/0 end if
    free(pY)
    return newy
end function

--global function cdCanvasInvertYAxis(cdCanvas canvas, integer y)
--  integer newy c_func(xcdCanvasInvertYAxis, {canvas, y})
--  return newy
--end function

global function cdCanvasInvertYAxis(cdCanvas canvas, atom y)
    atom newy = c_func(xcdfCanvasInvertYAxis, {canvas, y})
    return newy
end function

global function cdCanvasMM2Pixel(cdCanvas canvas, atom mm_dx, atom mm_dy)
    atom pDx = allocate(W*2),
         pDy = pDx+W
    c_proc(xcdCanvasMM2Pixel, {canvas, mm_dx, mm_dy, pDx, pDy})
    sequence dx_dy = peekNS({pDx, 2},W,1)
    free(pDx)
    return dx_dy
end function

--global function cdfCanvasMM2Pixel(cdCanvas canvas, atom mm_dx, atom mm_dy)
--  atom pDx = allocate(16),
--       pDy = pDx+8
--  c_proc(xcdfCanvasMM2Pixel, {canvas, mm_dx, mm_dy, pDx, pDy})
--  sequence dx_dy = iup_peek_double({pDx, 2})
--  free(pDx)
--  return dx_dy
--end function

--global function cdCanvasPixel2MM(cdCanvas canvas, atom dx, atom dy)
--  atom pmm_dx = allocate(16),
--       pmm_dy = pmm_dx+8
--  c_proc(xcdCanvasPixel2MM, {canvas, dx, dy, pmm_dx, pmm_dy})
--  sequence mmdx_mmdy = iup_peek_double({pmm_dx, 2})
--  free(pmm_dx)
--  return mmdx_mmdy
--end function

global function cdCanvasPixel2MM(cdCanvas canvas, atom dx, atom dy)
    atom pmm_dx = allocate(16),
         pmm_dy = pmm_dx+8
    c_proc(xcdfCanvasPixel2MM, {canvas, dx, dy, pmm_dx, pmm_dy})
    sequence mmdx_mmdy = iup_peek_double({pmm_dx, 2})
    free(pmm_dx)
    return mmdx_mmdy
end function

--global procedure cdCanvasOrigin(cdCanvas canvas, atom x, atom y)
--  c_proc(xcdCanvasOrigin, {canvas, x, y})
--end procedure

global procedure cdCanvasOrigin(cdCanvas canvas, atom x, atom y)
    c_proc(xcdfCanvasOrigin, {canvas, x, y})
end procedure

--global function cdCanvasGetOrigin(cdCanvas canvas)
--  atom pX = allocate(8),  -- W?
--       pY = pX+4
--  c_proc(xcdCanvasGetOrigin, {canvas, pX, pY})
--  sequence origin = peek4s({pX, 2})
--  free(pX)
--  return origin
--end function

global function cdCanvasGetOrigin(cdCanvas canvas)
    atom pX = allocate(16),
         pY = pX+8
    c_proc(xcdfCanvasGetOrigin, {canvas, pX, pY})
    sequence origin = iup_peek_double({pX, 2})
    free(pX)
    return origin
end function

global procedure cdCanvasTransform(cdCanvas canvas, object matrix)
    atom pMatrix = NULL
    if sequence(matrix) then
        if length(matrix)!=6 then ?9/0 end if
        pMatrix = allocate(6*8,1)
        iup_poke_double(pMatrix, matrix)
    end if
    c_proc(xcdCanvasTransform, {canvas, pMatrix})
    if pMatrix!=NULL then
        free(pMatrix)
    end if
end procedure

global function cdCanvasGetTransform(cdCanvas canvas)
    atom pMatrix = c_func(xcdCanvasGetTransform, {canvas})
    if pMatrix=NULL then return NULL end if
    return iup_peek_double({pMatrix, 6})
end function

global procedure cdCanvasTransformMultiply(cdCanvas canvas, sequence matrix)
    atom pMatrix = allocate(6*8)
    iup_poke_double(pMatrix, matrix)
    c_proc(xcdCanvasTransformMultiply, {canvas, pMatrix})
    free(pMatrix)
end procedure

global procedure cdCanvasTransformRotate(cdCanvas canvas, atom angle)
    c_proc(xcdCanvasTransformRotate, {canvas, angle})
end procedure

global procedure cdCanvasTransformScale(cdCanvas canvas, atom sx, atom sy)
    c_proc(xcdCanvasTransformScale, {canvas, sx, sy})
end procedure

global procedure cdCanvasTransformTranslate(cdCanvas canvas, atom dx, atom dy)
    c_proc(xcdCanvasTransformTranslate, {canvas, dx, dy})
end procedure

--global function cdCanvasTransformPoint(cdCanvas canvas, atom x, atom y)
--  atom pX = allocate(8),  -- W?
--       pY = pX+4
--  c_proc(xcdCanvasTransformPoint, {canvas, x, y, pX, pY})
--  sequence tx_ty = peek4s({pX, 2})
--  free(pX)
--  return tx_ty
--end function

global function cdCanvasTransformPoint(cdCanvas canvas, atom x, atom y)
    atom pX = allocate(16),
         pY = pX+8
    c_proc(xcdfCanvasTransformPoint, {canvas, x, y, pX, pY})
    sequence tx_ty = iup_peek_double({pX, 2})
    free(pX)
    return tx_ty
end function

------------------------------------------------------------------------------------------
----
----     clipping 
----
--------------------------------------------------------------------------------------------

--global function canvas_clip(cdCanvas canvas, integer mode)
global function cdCanvasClip(cdCanvas canvas, integer mode)
    integer prev_mode = c_func(xcdCanvasClip, {canvas, mode})
    return prev_mode
end function --cdCanvasClip

--global procedure canvas_clip_area(cdCanvas canvas, atom xmin, atom xmax, atom ymin, atom ymax)
--global procedure cdCanvasClipArea(cdCanvas canvas, atom xmin, atom xmax, atom ymin, atom ymax)
--  c_proc(xcdCanvasClipArea, {canvas, xmin, xmax, ymin, ymax})
--end procedure

--global procedure f_canvas_clip_area(cdCanvas canvas, atom xmin, atom xmax, atom ymin, atom ymax)
--global procedure cdfCanvasClipArea(cdCanvas canvas, atom xmin, atom xmax, atom ymin, atom ymax)
global procedure cdCanvasClipArea(cdCanvas canvas, atom xmin, atom xmax, atom ymin, atom ymax)
    c_proc(xcdfCanvasClipArea, {canvas, xmin, xmax, ymin, ymax})
end procedure

--global function canvas_get_clip_area(cdCanvas canvas)
--global function cdCanvasGetClipArea(canvas canvas)
--  atom pXmin = allocate(16),
--       pXmax = pXmin+4,
--       pYmin = pXmax+4,
--       pYmax = pYmin+4
--  integer mode = c_func(xcdCanvasGetClipArea, {canvas, pXmin, pXmax, pYmin, pYmax})
--  sequence res = mode & peek4s({pXmin, 4})
--  free(pXmin)
--  return res
--end function

--global function f_canvas_get_clip_area(cdCanvas canvas)
--global function cdfCanvasGetClipArea(cdCanvas canvas)
global function cdCanvasGetClipArea(cdCanvas canvas)
    atom pXmin = allocate(32),
         pXmax = pXmin+8,
         pYmin = pXmax+8,
         pYmax = pYmin+8
    integer mode = c_func(xcdfCanvasGetClipArea, {canvas, pXmin, pXmax, pYmin, pYmax})
    sequence res = mode & iup_peek_double({pXmin, 4})
    free(pXmin)
    return res
end function

------------------------------------------------------------------------------------------
----
----    clipping region 
----
--------------------------------------------------------------------------------------------
--constant
--  xcdCanvasIsPointInRegion    = iup_c_func(hCd, "cdCanvasIsPointInRegion", {P,I,I},I),
--  xcdCanvasOffsetRegion       = iup_c_proc(hCd, "cdCanvasOffsetRegion", {P,I,I}),
--  xcdCanvasGetRegionBox       = iup_c_proc(hCd, "cdCanvasGetRegionBox", {P,P,P,P,P,P}),
--  xcdCanvasRegionCombineMode  = iup_c_func(hCd, "cdCanvasRegionCombineMode", {P,I},I)

--global function canvas_is_point_in_region(cdCanvas canvas, atom x, atom y)
global function cdCanvasIsPointInRegion(cdCanvas canvas, atom x, atom y)
    return c_func(xcdCanvasIsPointInRegion, {canvas, x, y})
end function

--global procedure canvas_offset_region(cdCanvas canvas, atom x, atom y)
global procedure cdCanvasOffsetRegion(cdCanvas canvas, atom x, atom y)
    c_proc(xcdCanvasOffsetRegion, {canvas, x, y})
end procedure

--global function canvas_get_region_box(cdCanvas canvas)
global function cdCanvasGetRegionBox(cdCanvas canvas)
    atom pXmin = allocate(16),
         pXmax = pXmin+4,
         pYmin = pXmax+4,
         pYmax = pYmin+4
    c_proc(xcdCanvasGetRegionBox, {canvas, pXmin, pXmax, pYmin, pYmax})
    sequence box = peek4s({pXmin, 4})
    free(pXmin)
    return box
end function

--global procedure canvas_region_combine_mode(cdCanvas canvas, integer mode)
global procedure cdCanvasRegionCombineMode(cdCanvas canvas, integer mode)
    {} = c_func(xcdCanvasRegionCombineMode, {canvas, mode})
end procedure

--------------------------------------------------------------------------------------------
----
---- drawing primitives
----
--------------------------------------------------------------------------------------------
--constant
--  xcdCanvasPixel  = iup_c_proc(hCd, "cdCanvasPixel", {P,I,I,I}),
--  xcdCanvasMark   = iup_c_proc(hCd, "cdCanvasMark", {P,I,I}),
--  xcdCanvasLine   = iup_c_proc(hCd, "cdCanvasLine", {P,I,I,I,I}),
--  xcdLine         = iup_c_proc(hCd, "cdLine", {I,I,I,I}),
--  xcdCanvasBegin  = iup_c_proc(hCd, "cdCanvasBegin", {P,I}),
--  xcdCanvasVertex = iup_c_proc(hCd, "cdCanvasVertex", {P,I,I}),
--  xcdCanvasEnd    = iup_c_proc(hCd, "cdCanvasEnd", {P}),
--  xcdCanvasRect   = iup_c_proc(hCd, "cdCanvasRect", {P,I,I,I,I}),
--  xcdCanvasBox    = iup_c_proc(hCd, "cdCanvasBox", {P,I,I,I,I}),
--  xcdCanvasArc    = iup_c_proc(hCd, "cdCanvasArc", {P,I,I,I,I,D,D}),
--  xcdCanvasSector = iup_c_proc(hCd, "cdCanvasSector", {P,I,I,I,I,D,D}),
--  xcdCanvasChord  = iup_c_proc(hCd, "cdCanvasChord", {P,I,I,I,I,D,D}),
--  xcdCanvasText   = iup_c_proc(hCd, "cdCanvasText", {P,I,I,P}),
--  xcdText         = iup_c_proc(hCd, "cdText", {I,I,P})

global procedure cdCanvasPixel(cdCanvas canvas, atom x, y, integer color)
    c_proc(xcdCanvasPixel, {canvas, x, y, color})
end procedure

global procedure cdCanvasMark(cdCanvas canvas, atom x, y)
    c_proc(xcdCanvasMark, {canvas, x, y})
end procedure

global procedure cdCanvasLine(cdCanvas canvas, atom x1, y1, x2, y2)
    c_proc(xcdCanvasLine, {canvas, x1, y1, x2, y2})
end procedure

global procedure cdLine(atom x1, y1, x2, y2)
    c_proc(xcdLine, {x1, y1, x2, y2})
end procedure

global procedure cdCanvasBegin(cdCanvas canvas, integer mode)
    c_proc(xcdCanvasBegin, {canvas, mode})
end procedure

global procedure cdCanvasVertex(cdCanvas canvas, atom x, y)
    c_proc(xcdCanvasVertex, {canvas, x, y})
end procedure

global procedure cdCanvasEnd(cdCanvas canvas)
    c_proc(xcdCanvasEnd, {canvas})
end procedure

global procedure cdCanvasPathSet(cdCanvas canvas, integer action)
    c_proc(xcdCanvasPathSet, {canvas, action})
end procedure

global procedure cdCanvasRect(cdCanvas canvas, atom xmin, xmax, ymin, ymax)
    c_proc(xcdCanvasRect, {canvas, xmin, xmax, ymin, ymax})
end procedure

global procedure cdCanvasBox(cdCanvas canvas, atom xmin, xmax, ymin, ymax)
    c_proc(xcdCanvasBox, {canvas, xmin, xmax, ymin, ymax})
end procedure

global procedure cdCanvasArc(cdCanvas canvas, atom xc, atom yc, atom w, atom h, atom angle1, atom angle2)
    c_proc(xcdCanvasArc, {canvas, xc, yc, w, h, angle1, angle2})
end procedure

global procedure cdCanvasSector(cdCanvas canvas, atom xc, atom yc, atom w, atom h, atom angle1, atom angle2)
    c_proc(xcdCanvasSector, {canvas, xc, yc, w, h, angle1, angle2})
end procedure

global procedure cdCanvasCircle(cdCanvas canvas, atom x, y, r, boolean filled=false)
    if filled then
        cdCanvasSector(canvas,x,y,r,r,0,360)
    else
        cdCanvasArc(canvas,x,y,r,r,0,360)
    end if
end procedure

global procedure cdCanvasRoundedBox(cdCanvas canvas, atom xmin, atom xmax, atom ymin, atom ymax, atom width, atom height) 
    -- first draw the filled rectangle with straight-clipped corners (aka an octagon)
    cdCanvasBegin(canvas,CD_FILL)
    cdCanvasVertex(canvas,xmin+width,ymin)
    cdCanvasVertex(canvas,xmax-width,ymin)
    cdCanvasVertex(canvas,xmax,ymin+height)
    cdCanvasVertex(canvas,xmax,ymax-height)
    cdCanvasVertex(canvas,xmax-width,ymax)
    cdCanvasVertex(canvas,xmin+width,ymax)
    cdCanvasVertex(canvas,xmin,ymax-height)
    cdCanvasVertex(canvas,xmin,ymin+height)
    cdCanvasEnd(canvas)
    -- then round/fill in the corners using cdCanvasSector
--  cdCanvasSector(cdCanvas canvas, atom xc, atom yc, atom w, atom h, atom angle1, atom angle2) 
--  cdCanvasSetForeground(cddbuffer, CD_RED)
    cdCanvasSector(canvas, xmin+width,ymin+height,width*2,height*2,180,270)     -- btm left
    cdCanvasSector(canvas, xmax-width,ymin+height,width*2,height*2,270,0)       -- btm right
    cdCanvasSector(canvas, xmin+width,ymax-height,width*2,height*2,90,180)      -- top left
    cdCanvasSector(canvas, xmax-width,ymax-height,width*2,height*2,0,90)        -- top right
end procedure

global procedure cdCanvasRoundedRect(cdCanvas canvas, atom xmin, atom xmax, atom ymin, atom ymax, atom width, atom height) 
    -- first draw four edges, not-quite-meeting
    cdCanvasLine(canvas,xmin+width,ymin,xmax-width,ymin)
    cdCanvasLine(canvas,xmax,ymin+height,xmax,ymax-height)
    cdCanvasLine(canvas,xmax-width,ymax,xmin+width,ymax)
    cdCanvasLine(canvas,xmin,ymax-height,xmin,ymin+height)
    -- then round/connect the corners using cdCanvasArc
--  cdCanvasArc(cdCanvas canvas, atom xc, atom yc, atom w, atom h, atom a1, atom a2) 
--  cdCanvasSetForeground(cddbuffer, CD_RED)
    cdCanvasArc(canvas, xmin+width,ymin+height,width*2,height*2,180,270)
    cdCanvasArc(canvas, xmax-width,ymin+height,width*2,height*2,270,0)
    cdCanvasArc(canvas, xmin+width,ymax-height,width*2,height*2,90,180)
    cdCanvasArc(canvas, xmax-width,ymax-height,width*2,height*2,0,90)
end procedure

global procedure cdCanvasChord(cdCanvas canvas, atom xc, atom yc, atom w, atom h, atom a1, atom a2)
    c_proc(xcdCanvasChord, {canvas, xc, yc, w, h, a1, a2})
end procedure

global procedure cdCanvasText(cdCanvas canvas, atom x, atom y, string text)
    c_proc(xcdCanvasText, {canvas, x, y, text})
end procedure

global procedure cdText(atom x, atom y, string text)
    c_proc(xcdText, {x, y, text})
end procedure

-----------------------------------------------------------------------------------------
----
---- primitives with double as arguments instead of integer
----
-------------------------------------------------------------------------------------------
--constant
--  xcdfCanvasLine      = iup_c_proc(hCd, "cdfCanvasLine", {P,D,D,D,D}),
--  xcdfCanvasVertex    = iup_c_proc(hCd, "cdfCanvasVertex", {P,D,D}),
--  xcdfCanvasRect      = iup_c_proc(hCd, "cdfCanvasRect", {P,D,D,D,D}),
--  xcdfCanvasBox       = iup_c_proc(hCd, "cdfCanvasBox", {P,D,D,D,D}),
--  xcdfCanvasArc       = iup_c_proc(hCd, "cdfCanvasArc", {P,D,D,D,D,D,D}),
--  xcdfCanvasSector    = iup_c_proc(hCd, "cdfCanvasSector", {P,D,D,D,D,D,D}),
--  xcdfCanvasChord     = iup_c_proc(hCd, "cdfCanvasChord", {P,D,D,D,D,D,D}),
--  xcdfCanvasText      = iup_c_proc(hCd, "cdfCanvasText", {P,D,D,P})

--global procedure f_canvas_line(cdCanvas canvas, atom x1, atom y1, atom x2, atom y2)
----global procedure cdfCanvasLine(cdCanvas canvas, atom x1, atom y1, atom x2, atom y2)
--  c_proc(xcdfCanvasLine, {canvas, x1, y1, x2, y2})
--end procedure

--global procedure f_canvas_vertex(cdCanvas canvas, atom x1, atom y1)
----global procedure cdfCanvasVertex(cdCanvas canvas, atom x1, atom y1)
--  c_proc(xcdfCanvasVertex, {canvas, x1, y1})
--end procedure

--global procedure f_canvas_rect(cdCanvas canvas, atom xmin, atom ymin, atom xmax, atom ymax)
----global procedure cdfCanvasRect(cdCanvas canvas, atom xmin, atom ymin, atom xmax, atom ymax)
--  c_proc(xcdfCanvasRect, {canvas, xmin, ymin, xmax, ymax})
--end procedure

--global procedure f_canvas_box(cdCanvas canvas, atom xmin, atom ymin, atom xmax, atom ymax)
----global procedure cdfCanvasBox(cdCanvas canvas, atom xmin, atom ymin, atom xmax, atom ymax)
--  c_proc(xcdfCanvasBox, {canvas, xmin, ymin, xmax, ymax})
--end procedure

--global procedure f_canvas_arc(cdCanvas canvas, atom xc, atom yc, atom w, atom h, atom angle1, atom angle2)
----global procedure cdfCanvasArc(cdCanvas canvas, atom xc, atom yc, atom w, atom h, atom angle1, atom angle2)
--  c_proc(xcdfCanvasArc, {canvas, xc, yc, w, h, angle1, angle2})
--end procedure

--global procedure f_canvas_sector(cdCanvas canvas, atom xc, atom yc, atom w, atom h, atom angle1, atom angle2)
----global procedure cdfCanvasSector(cdCanvas canvas, atom xc, atom yc, atom w, atom h, atom angle1, atom angle2)
--  c_proc(xcdfCanvasSector, {canvas, xc, yc, w, h, angle1, angle2})
--end procedure

--global procedure f_canvas_chord(cdCanvas canvas, atom xc, atom yc, atom w, atom h, atom angle1, atom angle2)
----global procedure cdfCanvasChord(cdCanvas canvas, atom xc, atom yc, atom w, atom h, atom angle1, atom angle2)
--  c_proc(xcdfCanvasChord, {canvas, xc, yc, w, h, angle1, angle2})
--end procedure

--global procedure f_canvas_text(cdCanvas canvas, atom x1, atom y1, string text)
----global procedure cdfCanvasText(cdCanvas canvas, atom x1, atom y1, string text)
--  c_proc(xcdfCanvasText, {canvas, x1, y1, text})
--end procedure

------------------------------------------------------------------------------------------
----
---- attributes
----
--------------------------------------------------------------------------------------------
--constant
--  xcdCanvasSetForeground   = iup_c_proc(hCd, "cdCanvasSetForeground", {P,I}),
--  xcdCanvasSetBackground   = iup_c_proc(hCd, "cdCanvasSetBackground", {P,I}),
--  xcdCanvasForeground      = iup_c_func(hCd, "cdCanvasForeground", {P,L}, L),
--  xcdCanvasBackground      = iup_c_func(hCd, "cdCanvasBackground", {P,L}, L),
--  xcdForeground            = iup_c_proc(hCd, "cdForeground", {L}),
--  xcdBackground            = iup_c_proc(hCd, "cdBackground", {L}),
--  xcdCanvasBackOpacity     = iup_c_func(hCd, "cdCanvasBackOpacity", {P,I}, I),
--  xcdCanvasWriteMode       = iup_c_func(hCd, "cdCanvasWriteMode", {P,I}, I),
--  xcdCanvasLineStyle       = iup_c_func(hCd, "cdCanvasLineStyle", {P,I}, I),
--  xcdCanvasLineStyleDashes = iup_c_proc(hCd, "cdCanvasLineStyleDashes", {P,P, I}),
--  xcdCanvasLineWidth       = iup_c_func(hCd, "cdCanvasLineWidth", {P,I}, I),
--  xcdCanvasLineJoin        = iup_c_func(hCd, "cdCanvasLineJoin", {P,I}, I),
--  xcdCanvasLineCap         = iup_c_func(hCd, "cdCanvasLineCap", {P,I}, I),
--  xcdCanvasInteriorStyle   = iup_c_func(hCd, "cdCanvasInteriorStyle", {P,I}, I),
--  xcdCanvasHatch           = iup_c_func(hCd, "cdCanvasHatch", {P,I}, I),
--  xcdCanvasStipple         = iup_c_proc(hCd, "cdCanvasStipple", {P,I,I,P}),
--  xcdCanvasGetStipple      = iup_c_func(hCd, "cdCanvasGetStipple", {P,P,P}, P),
--  xcdCanvasPattern         = iup_c_proc(hCd, "cdCanvasPattern", {P,I,I,P}),
--  xcdCanvasGetPattern      = iup_c_func(hCd, "cdCanvasGetPattern", {P,P,P}, P),
--  xcdCanvasFillMode        = iup_c_func(hCd, "cdCanvasFillMode", {P,I}, I),
--  xcdCanvasFont            = iup_c_proc(hCd, "cdCanvasFont", {P,P,I,I}),
--  xcdCanvasGetFont         = iup_c_proc(hCd, "cdCanvasGetFont", {P,P,P,P}),
--  xcdCanvasNativeFont      = iup_c_func(hCd, "cdCanvasNativeFont", {P,P}, P),
--  xcdNativeFont            = iup_c_func(hCd, "cdNativeFont", {P}, P),
--  xcdCanvasTextAlignment   = iup_c_func(hCd, "cdCanvasTextAlignment", {P,I}, I),
--  xcdTextAlignment         = iup_c_func(hCd, "cdTextAlignment", {I}, I),
--  xcdCanvasTextOrientation = iup_c_func(hCd, "cdCanvasTextOrientation", {P,D}, D),
--  xcdCanvasMarkType        = iup_c_func(hCd, "cdCanvasMarkType", {P,I}, I),
--  xcdCanvasMarkSize        = iup_c_func(hCd, "cdCanvasMarkSize", {P,I}, I)

global procedure cdCanvasSetForeground(cdCanvas canvas, atom color)
    c_proc(xcdCanvasSetForeground, {canvas, color})
end procedure

global function cdCanvasGetForeground(cdCanvas canvas)
    integer color = c_func(xcdCanvasForeground, {canvas, CD_QUERY})
    return color
end function

global procedure cdSetForeground(atom color)
    iup_init_cd()
    c_proc(xcdForeground, {color})
end procedure

global function cdGetForeground()
    return cdCanvasGetForeground(cdActiveCanvas())
end function

global procedure cdCanvasSetBackground(cdCanvas canvas, atom color)
    c_proc(xcdCanvasSetBackground, {canvas, color})
end procedure

global function cdCanvasGetBackground(cdCanvas canvas)
    return c_func(xcdCanvasBackground, {canvas, CD_QUERY})
end function

global procedure cdSetBackground(atom color)
    c_proc(xcdBackground, {color})
end procedure

global function cdGetBackground()
    return cdCanvasGetBackground(cdActiveCanvas())
end function

--global function cdCanvasBackOpacity(cdCanvas canvas, atom opacity)
global procedure cdCanvasSetBackOpacity(cdCanvas canvas, integer opacity)
    opacity = c_func(xcdCanvasBackOpacity, {canvas, opacity})
end procedure

global function cdCanvasGetBackOpacity(cdCanvas canvas)
    integer opacity = c_func(xcdCanvasBackOpacity, {canvas, opacity})
    return opacity
end function

--global function canvas_write_mode(cdCanvas canvas, atom mode)
--global function cdCanvasWriteMode(cdCanvas canvas, atom mode)
global procedure cdCanvasSetWriteMode(cdCanvas canvas, atom mode)
    mode = c_func(xcdCanvasWriteMode, {canvas, mode})
end procedure

global function cdCanvasGetWriteMode(cdCanvas canvas)
    integer mode = c_func(xcdCanvasWriteMode, {canvas, CD_QUERY})
    return mode
end function

global procedure cdCanvasSetLineWidth(cdCanvas canvas, atom width)
--  if width<1 then ?9/0 end if
--  if width<1 then width=1 end if -- (it's an int)
    width = c_func(xcdCanvasLineWidth, {canvas, max(width,1)})
end procedure

--global procedure cdCanvasLineWidth(cdCanvas canvas, atom width)
--  cdCanvasSetLineWidth(canvas, width)
--end procedure

global function cdCanvasGetLineWidth(cdCanvas canvas)
    integer width = c_func(xcdCanvasLineWidth, {canvas, CD_QUERY})
    return width
end function

global procedure cdCanvasSetLineStyle(cdCanvas canvas, integer style)
    style = c_func(xcdCanvasLineStyle, {canvas, style})
end procedure

--removed 9/8/19 (as per updated docs, now needs Get/Set added)
--global procedure cdCanvasLineStyle(cdCanvas canvas, integer style)
--  cdCanvasSetLineStyle(canvas, style)
--end procedure

global function cdCanvasGetLineStyle(cdCanvas canvas)
    integer style = c_func(xcdCanvasLineStyle, {canvas, CD_QUERY})
    return style
end function

--DEV/SUG rename as cdCanvasSetLineStyleDashes?
global procedure cdCanvasLineStyleDashes(cdCanvas canvas, sequence dashes)
    integer count = length(dashes)
    atom pDashes = allocate(4*count)
    poke4(pDashes, dashes)
    c_proc(xcdCanvasLineStyleDashes, {canvas, pDashes, count})
    free(pDashes)
end procedure

--global function canvas_line_join(cdCanvas canvas, atom join)
--global function cdCanvasLineJoin(cdCanvas canvas, integer join_style)
--  return c_func(xcdCanvasLineJoin, {canvas, join_style})
--end function

global procedure cdCanvasSetLineJoin(cdCanvas canvas, integer join_style)
    join_style = c_func(xcdCanvasLineJoin, {canvas, join_style})
end procedure

global function cdCanvasGetLineJoin(cdCanvas canvas)
    integer join_style = c_func(xcdCanvasLineJoin, {canvas, CD_QUERY})
    return join_style
end function

--global function canvas_line_cap(cdCanvas canvas, atom cap)
--global function cdCanvasLineCap(cdCanvas canvas, atom cap_style)
--  return c_func(xcdCanvasLineCap, {canvas, cap_style})
--end function

global procedure cdCanvasSetLineCap(cdCanvas canvas, atom cap_style)
    cap_style = c_func(xcdCanvasLineCap, {canvas, cap_style})
end procedure

global function cdCanvasGetLineCap(cdCanvas canvas)
    integer cap_style = c_func(xcdCanvasLineCap, {canvas, CD_QUERY})
    return cap_style
end function

--global function cdCanvasInteriorStyle(cdCanvas canvas, integer style)
global procedure cdCanvasSetInteriorStyle(cdCanvas canvas, integer style)
    style = c_func(xcdCanvasInteriorStyle, {canvas, style})
end procedure

global function cdCanvasGetInteriorStyle(cdCanvas canvas)
    integer style = c_func(xcdCanvasInteriorStyle, {canvas, CD_QUERY})
    return style
end function

--global function canvas_hatch(cdCanvas canvas, atom style)
--global function cdCanvasHatch(cdCanvas canvas, atom style)
global procedure cdCanvasSetHatch(cdCanvas canvas, integer style)
    style = c_func(xcdCanvasHatch, {canvas, style})
end procedure

global function cdCanvasGetHatch(cdCanvas canvas)
    integer style = c_func(xcdCanvasHatch, {canvas, CD_QUERY})
    return style
end function

--global procedure canvas_stipple(cdCanvas canvas, atom width, atom height, sequence stipple)
--global procedure cdCanvasStipple(cdCanvas canvas, atom width, atom height, sequence stipple)
global procedure cdCanvasSetStipple(cdCanvas canvas, atom width, atom height, atom_string stipple)
--  atom pStipple = allocate(length(stipple))
--  poke(pStipple, stipple)
--  c_proc(xcdCanvasStipple, {canvas, width, height, pStipple})
    c_proc(xcdCanvasStipple, {canvas, width, height, stipple})
--  free(pStipple)
end procedure
--DEV cdCanvasSetStippleImImage (aka cdCanvasStippleImImage in C)

--global function canvas_get_stipple(cdCanvas canvas)
global function cdCanvasGetStipple(cdCanvas canvas)
    atom pW = allocate(8),
         pH = pW+4,
         fnVal = c_func(xcdCanvasGetStipple, {canvas, pW, pH})
    integer {w,h} = peek4s({pW, 2})
    free(pW)
    string stipple = peek({fnVal, w*h})
    return {w, h, stipple}
end function

--global procedure canvas_pattern(cdCanvas canvas, atom width, atom height, sequence pattern)
--global procedure cdCanvasPattern(cdCanvas canvas, atom width, atom height, sequence pattern)
global procedure cdCanvasSetPattern(cdCanvas canvas, atom width, atom height, sequence pattern)
    atom pPattern = allocate(4*length(pattern))
    poke4(pPattern, pattern)
    c_proc(xcdCanvasPattern, {canvas, width, height, pPattern})
    free(pPattern)
end procedure

--global function canvas_get_pattern(cdCanvas canvas)
global function cdCanvasGetPattern(cdCanvas canvas)
    atom pW = allocate(8),
         pH = pW+4,
         fnVal = c_func(xcdCanvasGetPattern, {canvas, pW, pH})
    integer {w,h} = peek4s({pW, 2})
    free(pW)
    sequence pattern = peek4s({fnVal, w*h})
    return {w, h, pattern}
end function

--global function canvas_fill_mode(cdCanvas canvas, atom mode)
--global function cdCanvasFillMode(cdCanvas canvas, integer mode)
global procedure cdCanvasSetFillMode(cdCanvas canvas, integer mode)
    mode = c_func(xcdCanvasFillMode, {canvas, mode})
end procedure

global function cdCanvasGetFillMode(cdCanvas canvas)
    integer mode = c_func(xcdCanvasFillMode, {canvas, CD_QUERY})
    return mode
end function

global procedure cdCanvasFont(cdCanvas canvas, nullable_string font, integer style, integer size)
    iup_init_cd()
    c_proc(xcdCanvasFont, {canvas, font, style, size})
end procedure

global function cdCanvasGetFont(cdCanvas canvas)
    atom pStyle = allocate(1024),
         pSize = pStyle+4,
         pFont = pSize+4
    c_proc(xcdCanvasGetFont, {canvas, pFont, pStyle, pSize})
    sequence font = {peek_string(pFont)} & peek4s({pStyle, 2})
    free(pStyle)
    return font
end function

--DEV doc: cdCanvasNativeFont/CD_QUERY
global procedure cdCanvasSetNativeFont(cdCanvas canvas, string font)
    atom pFont = c_func(xcdCanvasNativeFont, {canvas, font})
end procedure

global function cdCanvasGetNativeFont(cdCanvas canvas)
    atom pFont = c_func(xcdCanvasNativeFont, {canvas, CD_QUERY})
    string font = peek_string(pFont)
    return font
end function

global procedure cdSetNativeFont(string font)
    atom pFont = c_func(xcdNativeFont, {font})
end procedure

global function cdGetNativeFont()
    atom pFont = c_func(xcdNativeFont, {CD_QUERY})
    string font = peek_string(pFont)
    return font
end function

global function cdCanvasTextAlignment(cdCanvas canvas, integer alignment)
    integer prev_alignment = c_func(xcdCanvasTextAlignment, {canvas, alignment})
    return prev_alignment
end function

global function cdTextAlignment(integer alignment)
    integer prev_alignment = c_func(xcdTextAlignment, {alignment})
    return prev_alignment
end function

--global function canvas_text_orientation(cdCanvas canvas, atom angle)
global function cdCanvasTextOrientation(cdCanvas canvas, atom angle)
    return c_func(xcdCanvasTextOrientation, {canvas, angle})
end function

--global function canvas_mark_type(cdCanvas canvas, atom mtype)
global function cdCanvasMarkType(cdCanvas canvas, atom mtype)
    return c_func(xcdCanvasMarkType, {canvas, mtype})
end function

--global function canvas_mark_size(cdCanvas canvas, atom msize)
global function cdCanvasMarkSize(cdCanvas canvas, atom msize)
    return c_func(xcdCanvasMarkSize, {canvas, msize})
end function

-----------------------------------------------------------------------------------------
----
----    vector text
----
-------------------------------------------------------------------------------------------
--constant
--  xcdCanvasVectorText             = iup_c_proc(hCd, "cdCanvasVectorText", {P,I,I,P}),
--  xcdCanvasMultiLineVectorText    = iup_c_proc(hCd, "cdCanvasMultiLineVectorText", {P,I,I,P})

--global procedure canvas_vector_text(cdCanvas canvas, atom x, atom y, string text)
global procedure cdCanvasVectorText(cdCanvas canvas, atom x, atom y, string text)
    c_proc(xcdCanvasVectorText, {canvas, x, y, text})
end procedure

global procedure cdCanvasMultiLineVectorText(cdCanvas canvas, atom x, atom y, string text)
    c_proc(xcdCanvasMultiLineVectorText, {canvas, x, y, text})
end procedure

-----------------------------------------------------------------------------------------
----
---- vector text attributes
----
-------------------------------------------------------------------------------------------
--constant
--  xcdCanvasVectorFont             = iup_c_func(hCd, "cdCanvasVectorFont", {P,P},P),
--  xcdCanvasVectorFontSize         = iup_c_proc(hCd, "cdCanvasVectorFontSize", {P,D,D}),
--  xcdCanvasGetVectorFontSize      = iup_c_proc(hCd, "cdCanvasVectorFontSize", {P,P,P}),
--  xcdCanvasVectorTextDirection    = iup_c_proc(hCd, "cdCanvasVectorTextDirection", {P,I,I,I,I}),
--  xcdCanvasVectorTextTransform    = iup_c_func(hCd, "cdCanvasVectorTextTransform", {P,P},P),
--  xcdCanvasVectorTextSize         = iup_c_proc(hCd, "cdCanvasVectorTextSize", {P,I,I,P}),
--  xcdCanvasVectorCharSize         = iup_c_func(hCd, "cdCanvasVectorCharSize", {P,I},I)

global function cdCanvasVectorFont(cdCanvas canvas, nullable_string font)
    atom pFont = c_func(xcdCanvasVectorFont, {canvas, font})
    font = iff(pFont=NULL?NULL:peek_string(pFont))
    return font
end function

global procedure cdCanvasVectorFontSize(cdCanvas canvas, atom size_x, size_y)
    c_proc(xcdCanvasVectorFontSize, {canvas, size_x, size_y})
end procedure

global function cdCanvasGetVectorFontSize(cdCanvas canvas)
    atom pSize = allocate(8*2)
    c_proc(xcdCanvasGetVectorFontSize, {canvas, pSize, pSize+8})
    sequence sy_sy = iup_peek_double({pSize, 2})
    free(pSize)
    return sy_sy
end function

--DEV cdCanvasVectorTextBox

global procedure cdCanvasVectorTextDirection(cdCanvas canvas, integer x1, integer y1, integer x2, integer y2)
    c_proc(xcdCanvasVectorTextDirection, {canvas, x1, y1, x2, y2})
end procedure

--global function canvas_vector_text_transform(cdCanvas canvas, sequence matrix)
global function cdCanvasVectorTextTransform(cdCanvas canvas, sequence matrix)
    atom pMatrix = NULL
    if matrix!={} then
        if length(matrix)!=6 then ?9/0 end if
        pMatrix = allocate(8*6,1)
        iup_poke_double(pMatrix, matrix)
    end if
    atom pPrevMatrix = c_func(xcdCanvasVectorTextTransform, {canvas, pMatrix})
    if pPrevMatrix=NULL then
        matrix = {}
    else
        matrix = iup_peek_double({pPrevMatrix, 6})
    end if
    return matrix
end function

global procedure cdCanvasVectorTextSize(cdCanvas canvas, atom w, atom h, string text)
    c_proc(xcdCanvasVectorTextSize, {canvas, w, h, text})
end procedure

--global function canvas_vector_char_size(cdCanvas canvas, atom size)
global function cdCanvasVectorCharSize(cdCanvas canvas, atom size)
    return c_func(xcdCanvasVectorCharSize, {canvas, size})
end function

-----------------------------------------------------------------------------------------
----
----    vector text properties 
----
-------------------------------------------------------------------------------------------
--constant
--  xcdCanvasGetVectorTextSize      = iup_c_proc(hCd, "cdCanvasGetVectorTextSize", {P,P,P,P}),
--  xcdCanvasGetVectorTextBounds    = iup_c_proc(hCd, "cdCanvasGetVectorTextBounds", {P,P,I,I,P})

global function cdCanvasGetVectorTextSize(cdCanvas canvas, sequence text)
    atom pX = allocate(8),
         pY = pX+4
    c_proc(xcdCanvasGetVectorTextSize, {canvas, text, pX, pY})
    sequence x_y = peek4s({pX, 2})
    free(pX)
    return x_y
end function

global function cdCanvasGetVectorTextBounds(cdCanvas canvas, string text, integer px, integer py)
    atom pRect = allocate(8*4)
    c_proc(xcdCanvasGetVectorTextBounds, {canvas, text, px, py, pRect})
    sequence rect = peek4s({pRect, 8})
    free(pRect)
    return rect
end function

-----------------------------------------------------------------------------------------
----
---- properties --
----
-------------------------------------------------------------------------------------------
--constant
--  xcdCanvasGetFontDim     = iup_c_proc(hCd, "cdCanvasGetFontDim", {P,P,P,P,P}),
--  xcdCanvasGetTextSize    = iup_c_proc(hCd, "cdCanvasGetTextSize", {P,P,P,P}),
----    xcdCanvasGetTextBox     = iup_c_proc(hCd, "cdCanvasGetTextBox", {P,P,I,I,P,P,P,P}),
--  xcdfCanvasGetTextBox    = iup_c_proc(hCd, "cdfCanvasGetTextBox", {P,D,D,P,P,P,P,P}),
--  xcdCanvasGetTextBounds  = iup_c_proc(hCd, "cdCanvasGetTextBounds", {P,I,I,P,P}),
--  xcdCanvasGetColorPlanes = iup_c_func(hCd, "cdCanvasGetColorPlanes", {P},I)

global function cdCanvasGetFontDim(cdCanvas canvas)
    atom pWidth = allocate(16),
         pHeight = pWidth+4,
         pAscent = pHeight+4,
         pDescent = pAscent+4
    c_proc(xcdCanvasGetFontDim, {canvas, pWidth, pHeight, pAscent, pDescent})
    sequence font_metrics = peek4s({pWidth, 4})
    free(pWidth)
    return font_metrics -- {width, height, ascent, descent}
end function

global function cdCanvasGetTextSize(cdCanvas canvas, string text)
    atom pW = allocate(8),
         pH = pW+4
    c_proc(xcdCanvasGetTextSize, {canvas, text, pW, pH})
    sequence text_size = peek4s({pW, 2})
    free(pW)
    return text_size    -- {width, height}
end function

--global function cdCanvasGetTextBox(cdCanvas canvas, atom x, atom y, string text)
--  atom pXmin = allocate(32),
--       pXmax = pXmin+8,
--       pYmin = pXmax+8,
--       pYmax = pYmin+8
--  c_proc(xcdfCanvasGetTextBox, {canvas, x, y, text, pXmin, pXmax, pYmin, pYmax})
--  sequence box = iup_peek_double({pXmin, 4})
--  free(pXmin)
--  return box
--end function

global function cdCanvasGetTextBox(cdCanvas canvas, atom x, atom y, string text)
    atom pXmin = allocate(16),
         pXmax = pXmin+4,
         pYmin = pXmax+4,
         pYmax = pYmin+4
    c_proc(xcdCanvasGetTextBox, {canvas, x, y, text, pXmin, pXmax, pYmin, pYmax})
    sequence box = peek4s({pXmin, 4})
    free(pXmin)
    return box
end function

global function cdCanvasGetTextBounds(cdCanvas canvas, atom x, atom y, string text)
    atom pRect = allocate(32)
    c_proc(xcdCanvasGetTextBounds, {canvas, x, y, text, pRect})
    sequence bounds = peek4s({pRect, 8})
    free(pRect)
    return bounds
end function

--DEV crashes...
--global function cdCanvasGetTextBounds(cdCanvas canvas, atom x, atom y, string text)
--  atom pRect = allocate(64)
--  c_proc(xcdfCanvasGetTextBounds, {canvas, x, y, text, pRect})
--  sequence bounds = iup_peek_double({pRect, 8})
--  free(pRect)
--  return bounds
--end function

global function cdCanvasGetColorPlanes(cdCanvas canvas)
    integer p = c_func(xcdCanvasGetColorPlanes, {canvas})
    return p
end function

-----------------------------------------------------------------------------------------
----
---- color 
----
-------------------------------------------------------------------------------------------
--constant
--  xcdCanvasPalette = iup_c_proc(hCd, "cdCanvasPalette", {P,I,P,I})

--global procedure canvas_palette(cdCanvas canvas, sequence palette, integer mode)
global procedure cdCanvasPalette(cdCanvas canvas, sequence colours, integer mode=CD_POLITE)
-- mode is CD_POLITE or CD_FORCE
    integer n = length(colours)
    atom pPalette = allocate(4*n)
    poke4(pPalette, colours)
    c_proc(xcdCanvasPalette, {canvas, n, pPalette, mode})
    free(pPalette)
end procedure

-----------------------------------------------------------------------------------------
----
---- client images 
----
-------------------------------------------------------------------------------------------
--constant
--  xcdCanvasGetImageRGB        = iup_c_proc(hCd, "cdCanvasGetImageRGB", {P,P,P,P,I,I,I,I}),
--  xcdCanvasPutImageRectRGB    = iup_c_proc(hCd, "cdCanvasPutImageRectRGB", {P,I,I,P,P,P,I,I,I,I,I,I,I,I}),
--  xcdCanvasPutImageRectRGBA   = iup_c_proc(hCd, "cdCanvasPutImageRectRGBA", {P,I,I,P,P,P,P,I,I,I,I,I,I,I,I}),
--  xcdCanvasPutImageRectMap    = iup_c_proc(hCd, "cdCanvasPutImageRectMap", {P,I,I,P,P,I,I,I,I,I,I,I,I})

global function cdCanvasGetImageRGB(cdCanvas canvas, atom x, atom y, atom w, atom h)
    integer l = w*h
    atom pR = allocate(l),
         pG = allocate(l),
         pB = allocate(l)
--?pR
--?{xcdCanvasGetImageRGB,canvas,x,y,w,h}
    c_proc(xcdCanvasGetImageRGB, {canvas, pR, pG, pB, x, y, w, h})
--?pR
    sequence r = peek({pR, w*h}),
             g = peek({pG, w*h}),
             b = peek({pB, w*h})
    free({pR,pG,pB})
    return {r,g,b}
end function

global procedure cdCanvasPutImageRectRGB(cdCanvas canvas, atom iw, ih, sequence rgb3, 
                                         atom x=0, y=0, w=0, h=0, xmin=0, xmax=0, ymin=0, ymax=0)
    integer l = length(rgb3[1])
    if length(rgb3)!=3
    or length(rgb3[2])!=l
    or length(rgb3[3])!=l then
        ?9/0
    end if
    atom pR = allocate(l),
         pG = allocate(l),
         pB = allocate(l)
    poke(pR, rgb3[1])
    poke(pG, rgb3[2])
    poke(pB, rgb3[3])
    c_proc(xcdCanvasPutImageRectRGB, {canvas, iw, ih, pR, pG, pB, x, y, w, h, xmin, xmax, ymin, ymax})
    free({pR,pG,pB})
end procedure

global procedure cdCanvasPutImageRectRGBA(cdCanvas canvas, atom iw, atom ih, sequence rgba, 
                                          atom x=0, y=0, w=0, h=0, xmin=0, xmax=0, ymin=0, ymax=0)
    integer l = length(rgba[1])
    if length(rgba)!=4
    or length(rgba[2])!=l
    or length(rgba[3])!=l
    or length(rgba[4])!=l then
        ?9/0
    end if
    atom pR = allocate(l),
         pG = allocate(l),
         pB = allocate(l),
         pA = allocate(l)
    poke(pR, rgba[1])
    poke(pG, rgba[2])
    poke(pB, rgba[3])
    poke(pA, rgba[4])
    c_proc(xcdCanvasPutImageRectRGBA, {canvas, iw, ih, pR, pG, pB, pA, x, y, w, h, xmin, xmax, ymin, ymax})
    free({pR,pG,pB,pA})
end procedure

--global procedure canvas_put_image_rect_map(cdCanvas canvas, atom iw, atom ih, sequence index, sequence colors,
global procedure cdCanvasPutImageRectMap(cdCanvas canvas, atom iw, ih, sequence index, colors,
                                                          atom x, y, w, h, 
                                                          atom xmin, xmax, ymin, ymax)
    atom pColors = allocate(4*256+length(index)),
         pIndex = pColors+4*256
    poke4(pColors, colors)
    poke(pIndex, index)
    c_proc(xcdCanvasPutImageRectMap, {canvas, iw, ih, pIndex, pColors, x, y, w, h, xmin, xmax, ymin, ymax})
    free(pColors)
end procedure

-----------------------------------------------------------------------------------------
----
---- server images 
----
-------------------------------------------------------------------------------------------
--constant
--  xcdCanvasCreateImage    = iup_c_func(hCd, "cdCanvasCreateImage", {P,I,I},P),
--  xcdKillImage            = iup_c_proc(hCd, "cdKillImage", {P}),
--  xcdCanvasGetImage       = iup_c_proc(hCd, "cdCanvasGetImage", {P,P,I,I}),
--  xcdCanvasPutImageRect   = iup_c_proc(hCd, "cdCanvasPutImageRect", {P,P,I,I,I,I,I,I}),
--  xcdCanvasScrollArea     = iup_c_proc(hCd, "cdCanvasScrollArea", {P,I,I,I,I,I,I})

--DEV removed (as untested and undocumented) 12/8/19 *5:
--/*
--global function canvas_create_image(cdCanvas canvas, atom w, atom h)
global function cdCanvasCreateImage(cdCanvas canvas, atom w, atom h)
    return c_func(xcdCanvasCreateImage, {canvas, w, h})
end function

--global procedure kill_image(atom hCdImage)
global procedure cdKillImage(atom hCdImage)
    c_proc(xcdKillImage, {hCdImage})
end procedure

--global procedure canvas_get_image(cdCanvas canvas, atom hCdImage, atom x, atom y)
global procedure cdCanvasGetImage(cdCanvas canvas, atom hCdImage, atom x, atom y)
    c_proc(xcdCanvasGetImage, {canvas, hCdImage, x, y})
end procedure

--global procedure canvas_put_image_rect(cdCanvas canvas, atom hCdImage, atom x, atom y,
global procedure cdCanvasPutImageRect(cdCanvas canvas, atom hCdImage, atom x, atom y,
        atom xmin, atom xmax, atom ymin, atom ymax)
    c_proc(xcdCanvasPutImageRect, {canvas, hCdImage, x, y, xmin, xmax, ymin, ymax})
end procedure

--global procedure canvas_scroll_area(cdCanvas canvas, atom xmin, atom xmax,
global procedure cdCanvasScrollArea(cdCanvas canvas, atom xmin, atom xmax,
        atom ymin, atom ymax, atom dx, atom dy)
    c_proc(xcdCanvasScrollArea, {canvas, xmin, xmax, ymin, ymax, dx, dy})
end procedure
--*/

-----------------------------------------------------------------------------------------
----
---- bitmap 
----
-------------------------------------------------------------------------------------------
--constant
--  xcdCreateBitmap     = iup_c_func(hCd, "cdCreateBitmap", {I,I,I},P),
--  xcdInitBitmapRGB    = iup_c_func(hCd, "cdInitBitmap", {I,I,I,P,P,P},P), -- type CD_RGB
--  xcdInitBitmapRGBA   = iup_c_func(hCd, "cdInitBitmap", {I,I,I,P,P,P,P},P), -- type CD_RGBA
----PL unused
----    xcdInitBitmapMAP    = iup_c_func(hCd, "cdInitBitmap", {I,I,I,P,P},P), -- type CD_MAP
--  xcdKillBitmap       = iup_c_proc(hCd, "cdKillBitmap", {P}),
--  xcdBitmapGetData    = iup_c_func(hCd, "cdBitmapGetData", {P,I},P),
--  xcdBitmapSetRect    = iup_c_proc(hCd, "cdBitmapSetRect", {P,I,I,I,I}),
--  xcdCanvasPutBitmap  = iup_c_proc(hCd, "cdCanvasPutBitmap", {P,P,I,I,I,I}),
--  xcdCanvasGetBitmap  = iup_c_proc(hCd, "cdCanvasGetBitmap", {P,P,I,I}),
--  xcdBitmapRGB2Map    = iup_c_proc(hCd, "cdBitmapRGB2Map", {P,P})

--DEV doc:
--/*
Extras (Deprecated - use imImage)
The following functions are used only for encapsulating the several types of client images from the library in a single structure, simplifying their treatment. 

For such, a public structure was created, called cdBitmap, which will store the image. From this structure, the following fields are officially defined:

cdBitmap:
  int w      -image width                              bitmap:Width() -> w: number   [in Lua]
  int h      -image heigth                             bitmap:Height() -> h: number   [in Lua]
  int type   -image type: CD_RGBA, CD_RGB or CD_MAP    bitmap:Type() -> type: number   [in Lua]

cdBitmap* cdCreateBitmap(int w, int h, int type); [in C]

cd.CreateBitmap(w, h, type: number) -> (bitmap: cdBitmap) [in Lua]
Creates an image with width w, and height h and of type type. The type can be CD_RGBA, CD_RGB or CD_MAP. 
However, CD_MAP only means that the image will have 256 colors if type is greater than 0. 
It is assumed that the image will be MAP with the same number of colors in the palette as type. 
Internally, the color palette is always allocated with 256 entries, which may or may not be totally fulfilled. 
In this case, the value of type can be changed as wished.

cdBitmap* cdInitBitmap(int w, int h, int type, ...); [in C]

[There is no equivalent in Lua]
Similar to cdCreateBitmap, but it accepts the data area already allocated by the user. The parameters vary according to the image type.

CD_RGBA - (unsigned char* red, unsigned char* green, unsigned char* blue, unsigned char* alpha)
CD_RGB - (unsigned char* red, unsigned char* green, unsigned char* blue)
CD_MAP - (unsigned char* index, lont int* colors)

void cdKillBitmap(cdBitmap* image); [in C]

cd.KillBitmap(bitmap: cdBitmap) [in Lua]
Liberates the memory allocated for the image. If this function is not called in Lua, the garbage collector will call it.

unsigned char* cdBitmapGetData(cdBitmap* image, int dataptr); [in C]

cd.BitmapGetData(bitmap: cdBitmap; dataptr: number) -> (data: cdImageChannel) [in Lua]

Returns a pointer to the image's data area according to dataptr. The following values are defined for dataptr:

CD_IRED - red component of an RGB image. cdImageChannel in Lua.
CD_IGREEN - green component of an RGB image. cdImageChannel in Lua.
CD_IBLUE - blue component of an RGB image. cdImageChannel in Lua.
CD_IALPHA - alpha component of an RGBA image. cdImageChannel in Lua.
CD_INDEX - indices of a MAP image. cdImageChannel in Lua.
CD_COLORS - color table of a MAP image. In this case, a type conversion must be made to (long int*).  cdPalette in Lua.
In Lua, channels are also available as tables, see Data Access.  

void cdBitmapSetRect(cdBitmap* image, int xmin, int xmax, int ymin, int ymax); [in C]

cd.BitmapSetRect(bitmap: cdBitmap; xmin, xmax, ymin, ymax: number) [in Lua]
Allows specifying a region of interest inside the image to be used by the function cdPutBitmap. 
If no region was defined, the whole image is used, that is, (0, w-1, 0, h-1).

void cdCanvasPutBitmap(cdCanvas* canvas, cdBitmap* image, int x, int y, int w, int h); [in C]
void wdCanvasPutBitmap(cdCanvas* canvas, cdBitmap* image, double x, double y, double w, double h); (WC) [in C]

canvas:PutBitmap(image: cdBitmap; x, y, w, h: number) [in Lua]
canvas:wPutBitmap(bitmap: cdBitmap; x, y, w, h: number) (WC) [in Lua]
Draws the  image in the position (x,y), changing the scale. 
It encapsulates cdPutImageRectRGB, cdPutImageRectRGBA and cdPutImageRectMap. 
The region of the image drawn depends on the rectangle defined by cdBitmapSetRect. 
If no rectangle was defined, then the whole image is used.

The parameters w and h allow scaling the image, increasing or decreasing its dimensions when drawn. 
If w and/or h are 0, then no scale change is assumed. 

void cdCanvasGetBitmap(cdCanvas* canvas, cdBitmap* image, int x, int y); [in C]

canvas:GetBitmap(bitmap: cdBitmap; x, y: number) [in Lua]
Encapsulates cdGetImageRGB. Nothing happens if the image is MAP.

void cdBitmapRGB2Map(cdBitmap* image_rgb, cdBitmap* image_map); [in C]

cd.BitmapRGB2Map(bitmap_rgb: cdBitmap, bitmap_map: cdBitmap) [in Lua]
Encapsulates cdRGB2Map. The images must be of types RGB(A) and MAP, respectively.

Extras in Lua (Deprecated)
cd.CreateImageRGB(width, height: number) -> (imagergb: cdImageRGB)
Creates an RGB image in Lua. Deprecated use cd.CreateBitmap.

cd.KillImageRGB(imagergb: cdImageRGB)
Destroys the created RGB image and liberates allocated memory. 
If this function is not called in Lua, the garbage collector will call it. Deprecated use cd.KillBitmap.

cd.CreateImageRGBA(width, height: number) -> (imagergba: cdImageRGBA)
Creates an RGBA image in Lua. Deprecated use cd.CreateBitmap.

cd.KillImageRGBA(imagergba: cdImageRGBA)
Destroys the created RGBA image and liberates allocated memory. 
If this function is not called in Lua, the garbage collector will call it. Deprecated use cd.KillBitmap.

cd.CreateImageMap(width, height: number) -> (imagemap: cdImageMap)
Creates a Map image in Lua. Deprecated use cd.CreateBitmap.

cd.KillImageMap(imagemap: cdImageMap)
Destroys the created Map image and liberates allocated memory. 
If this function is not called in Lua, the garbage collector will call it. Deprecated use cd.KillBitmap.

Data Access
Data access in Lua is done directly using the operator "[y*width + x]" in image channels. 
Each channel works as a value table which should be consulted or modified in the following way:

image = cd.CreateBitmap(100, 200)
...
image.r[y*100 + x] = 255
image.g[y*100 + x] = 128
image.b[y*100 + x] = 0
...
green = image.g[y*100 + x] -- it will return 128
The order of the tables is important, so that image[n].r has no meaning to CDLua and the expression will cause an error. 
Finally, the user could expect the value of image[n] to be of type lightuserdata. 
Unfortunately, this is not the case, and such expression will cause the same error.

In the old cdImageMap images, the channel must be not specified: imagemap[y*100+x].

Known channel names are:

r - red channel of RGB or RGBA images.
g - gree channel of RGB or RGBA images.
b - blue channel of RGB or RGBA images.
a - alpha channel of RGBA images.
m - indices channel of MAP images (valid only for cdBitmap objects).
p - colors table of MAP images (valid only for cdBitmap objects). It is a cdPalette object.
--*/

--Removed (as untested and undocumented and marked "Deprecated - use imImage") 12/8/19 *8:
--/*
--global function canvas_create_bitmap(atom w, atom h, integer btype)
global function cdCreateBitmap(atom w, atom h, integer btype)
    return c_func(xcdCreateBitmap, {w, h, btype})
end function

--global function init_bitmap(atom w, atom h, integer btype, sequence data)
global function cdInitBitmap(atom w, atom h, integer btype, sequence data)
atom hCdBitmap, pR, pG, pB, pA, pIndex, pColors

    hCdBitmap = 0
    integer l = length(data[1])
    if btype=CD_RGB then
        pR = allocate(3*l)
        pG = pR+l
        pB = pG+l
        poke(pR, data[1])
        poke(pG, data[2])
        poke(pB, data[3])
        hCdBitmap = c_func(xcdInitBitmapRGB, {w, h, btype, pR, pG, pB})
        free(pR)
    elsif btype=CD_RGBA then
        pR = allocate(4*l)
        pG = pR+l
        pB = pG+l
        pA = pB+l
        poke(pR, data[1])
        poke(pG, data[2])
        poke(pB, data[3])
        poke(pA, data[4])
        hCdBitmap = c_func(xcdInitBitmapRGBA, {w, h, btype, pR, pG, pB, pA})
        free(pR)
    elsif btype=CD_MAP then
        pColors = allocate(4*256+l)
        pIndex = pColors+4*256
        poke4(pColors, data[2])
        poke(pIndex, data[1])
        ?9/0    -- DEV: clearly there is a missing c_func(xcdInitBitmapMAP... here
        free(pColors)
    end if
    return hCdBitmap
end function

--global procedure kill_bitmap(atom hCdBitmap)
global procedure cdKillBitmap(atom hCdBitmap)
    c_proc(xcdKillBitmap, {hCdBitmap})
end procedure

--global function bitmap_get_data(atom hCdBitmap, integer dataptr)
global function cdBitmapGetData(atom hCdBitmap, integer dataptr)
    return c_func(xcdBitmapGetData, {hCdBitmap, dataptr})
end function

--global procedure bitmap_set_rect(atom hCdBitmap, atom xmin, atom xmax, atom ymin, atom ymax)
global procedure cdBitmapSetRect(atom hCdBitmap, atom xmin, atom xmax, atom ymin, atom ymax)
    c_proc(xcdBitmapSetRect, {hCdBitmap, xmin, xmax, ymin, ymax})
end procedure

--global procedure canvas_put_bitmap(cdCanvas canvas, atom hCdBitmap, atom x, atom y, atom w, atom h)
global procedure cdCanvasPutBitmap(cdCanvas canvas, atom hCdBitmap, atom x, atom y, atom w, atom h)
    c_proc(xcdCanvasPutBitmap, {canvas, hCdBitmap, x, y, w, h})
end procedure

--global procedure canvas_get_bitmap(cdCanvas canvas, atom hCdBitmap, atom x, atom y)
global procedure cdCanvasGetBitmap(cdCanvas canvas, atom hCdBitmap, atom x, atom y)
    c_proc(xcdCanvasGetBitmap, {canvas, hCdBitmap, x, y})
end procedure

--global procedure bitmap_rgb_2_map(atom hCdBitmapRGB, atom hCdBitmapMAP)
global procedure cdBitmapRGB2Map(atom hCdBitmapRGB, atom hCdBitmapMAP)
    c_proc(xcdBitmapRGB2Map, {hCdBitmapRGB, hCdBitmapMAP})
end procedure
--*/

-----------------------------------------------------------------------------------------
----
---- color 
----
-------------------------------------------------------------------------------------------

global function cdEncodeColor(atom red, green, blue)
    iup_init_cd()
    integer color = c_func(xcdEncodeColor, {red, green, blue})
    return color
end function

global function cdEncodeColorAlpha(atom red, green, blue, alpha)
    iup_init_cd()
    atom color = c_func(xcdEncodeColorAlpha, {red, green, blue, alpha})
    return color
end function

--global function decode_color(atom color)
global function cdDecodeColor(atom color)
    atom pR = allocate(4),
         pG = pR+1,
         pB = pG+1
    c_proc(xcdDecodeColor, {color, pR, pG, pB})
    sequence rgb_tuple = peek({pR, 3})
    free(pR)
    return rgb_tuple
end function

global function cdDecodeColorAlpha(atom color)
    atom pR = allocate(4),
         pG = pR+1,
         pB = pG+1,
         pA = pB+1
    c_proc(xcdDecodeColor, {color, pR, pG, pB, pA})
    sequence rgb_quad = peek({pR, 4})
    free(pR)
    return rgb_quad
end function

--global function encode_alpha(atom color, integer alpha)
global function cdEncodeAlpha(atom color, integer alpha)
    return c_func(xcdEncodeAlpha, {color, alpha})
end function

--global function decode_alpha(atom color)
global function cdDecodeAlpha(atom color)
--DEV untested, as above unlikely to be right??
    return c_func(xcdDecodeAlpha, {color})
end function

--Removed (as untested and undocumented and clearly wrong - poke(pR,rgb)*[1..3]???) 12/8/19:
--/*
--global function rgb_2_map(atom w, atom h, sequence rgb, integer pal_size)
global function cdRGB2Map(atom w, atom h, sequence rgb, integer pal_size)
    if sequence(rgb) then end if    --DEV PL unused
    atom pR = allocate(4*w*h+1024),
         pG = pR+w*h,
         pB = pG+w*h,
         pI = pB+w*h,
         pC = pI+w*h
    c_proc(xcdRGB2Map, {w, h, pR, pG, pB, pI, pal_size, pC})
    sequence smap = {peek({pI, w*h}), peek4s({pC, pal_size})}
    free(pR)
    return smap
end function
--*/

-----------------------------------------------------------------------------------------
--
--  world coordinates
--
-----------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------
----
----    coordinate transformation
----
-------------------------------------------------------------------------------------------
--constant
--  xwdCanvasWindow             = iup_c_proc(hCd, "wdCanvasWindow", {P,D,D,D,D}),
--  xwdCanvasGetWindow          = iup_c_proc(hCd, "wdCanvasGetWindow", {P,P,P,P,P}),
--  xwdCanvasViewport           = iup_c_proc(hCd, "wdCanvasViewport", {P,I,I,I,I}),
--  xwdCanvasGetViewport        = iup_c_proc(hCd, "wdCanvasGetViewport", {P,P,P,P,P}),
--  xwdCanvasWorld2Canvas       = iup_c_proc(hCd, "wdCanvasWorld2Canvas", {P,D,D,P,P}),
--  xwdCanvasWorld2CanvasSize   = iup_c_proc(hCd, "wdCanvasWorld2CanvasSize", {P,D,D,P,P}),
--  xwdCanvasCanvas2World       = iup_c_proc(hCd, "wdCanvasCanvas2World", {P,I,I,D,D})

global procedure wdCanvasWindow(cdCanvas canvas, atom xmin, atom xmax, atom ymin, atom ymax)
    c_proc(xwdCanvasWindow, {canvas, xmin, xmax, ymin, ymax})
end procedure

global function wdCanvasGetWindow(cdCanvas canvas)
--DEV machine_bits?
    atom pXmin = allocate(4*8),
         pXmax = pXmin+8,
         pYmin = pXmax+8,
         pYmax = pYmin+8
    c_proc(xwdCanvasGetWindow, {canvas, pXmin, pXmax, pYmin, pYmax})
    sequence wdWindow = iup_peek_double({pXmin, 4})
    free(pXmin)
    return wdWindow
end function

global procedure wdCanvasViewport(cdCanvas canvas, atom xmin, atom xmax, atom ymin, atom ymax)
    c_proc(xwdCanvasViewport, {canvas, xmin, xmax, ymin, ymax})
end procedure

global function wdCanvasGetViewport(cdCanvas canvas)
    atom pXmin = allocate(4*4),
         pXmax = pXmin+4,
         pYmin = pXmax+4,
         pYmax = pYmin+4
    c_proc(xwdCanvasGetViewport, {canvas, pXmin, pXmax, pYmin, pYmax})
    sequence wdViewport = peek4s({pXmin, 4})
    free(pXmin)
    return wdViewport
end function

--global function wd_canvas_world2_canvas(cdCanvas canvas, atom xw, atom yw)
global function wdCanvasWorld2Canvas(cdCanvas canvas, atom xw, yw)
--DEV machine_bits?
    atom pX = allocate(2*4),
         pY = pX+4
    c_proc(xwdCanvasWorld2Canvas, {canvas, xw, yw, pX, pY})
--  integer {x,y} = peek4s({pX,2})
    sequence xy = peek4s({pX,2})
    free(pX)
--  return {x,y}
    return xy   -- integer {x,y}
end function

--global function wd_canvas_world2_canvas_size(cdCanvas canvas, atom ww, atom hw)
global function wdCanvasWorld2CanvasSize(cdCanvas canvas, atom ww, hw)
--DEV machine_bits?
    atom pW = allocate(8),
         pH = pW+4
    c_proc(xwdCanvasWorld2CanvasSize, {canvas, ww, hw, pW, pH})
--  integer {w,h} = peek4s({pW, 2})
    sequence wh = peek4s({pW, 2})
    free(pW)
--  return {w,h}
    return wh -- integer {w,h}
end function

--global function wd_canvas_canvas2_world(cdCanvas canvas, atom xv, atom yv)
global function wdCanvasCanvas2World(cdCanvas canvas, atom xv, yv)
--DEV machine_bits?
    atom pWx = allocate(2*8),
         pWy = pWx+8
    c_proc(xwdCanvasCanvas2World, {canvas, xv, yv, pWx, pWy})
    atom {x,y} = iup_peek_double({pWx, 2})
    free(pWx)
    return {x,y}
end function

-----------------------------------------------------------------------------------------
----
---- clipping region
----
-------------------------------------------------------------------------------------------
--constant
--  xwdCanvasClipArea           = iup_c_proc(hCd, "wdCanvasClipArea", {P,D,D,D,D}),
--  xwdCanvasGetClipArea        = iup_c_func(hCd, "wdCanvasGetClipArea", {P,P,P,P,P},I),
--  xwdCanvasIsPointInRegion    = iup_c_func(hCd, "wdCanvasIsPointInRegion", {P,D,D},I),
--  xwdCanvasOffsetRegion       = iup_c_proc(hCd, "wdCanvasOffsetRegion", {P,D,D}),
--  xwdCanvasGetRegionBox       = iup_c_proc(hCd, "wdCanvasGetRegionBox", {P,P,P,P,P}),
--  xwdCanvasHardcopy           = iup_c_proc(hCd, "wdCanvasHardcopy", {P,P,P,P})

--global procedure wd_canvas_clip_area(cdCanvas canvas, atom xmin, atom xmax, atom ymin, atom ymax)
global procedure wdCanvasClipArea(cdCanvas canvas, atom xmin, atom xmax, atom ymin, atom ymax)
    c_proc(xwdCanvasClipArea, {canvas, xmin, xmax, ymin, ymax})
end procedure

--global function wd_canvas_get_clip_area(cdCanvas canvas)
global function wdCanvasGetClipArea(cdCanvas canvas)
    atom pXmin = allocate(32),
         pXmax = pXmin+8,
         pYmin = pXmax+8,
         pYmax = pYmin+8
    integer clipping_status = c_func(xwdCanvasGetClipArea, {canvas, pXmin, pXmax, pYmin, pYmax})
    sequence area = iup_peek_double({pXmin, 4})
    free(pXmin)
    return clipping_status & area
end function

--global function wd_canvas_is_point_in_region(cdCanvas canvas, atom x, atom y)
global function wdCanvasIsPointInRegion(cdCanvas canvas, atom x, y)
    return c_func(xwdCanvasIsPointInRegion, {canvas, x, y})
end function

--global procedure wd_canvas_offset_region(cdCanvas canvas, atom x, atom y)
global procedure wdCanvasOffsetRegion(cdCanvas canvas, atom x, y)
    c_proc(xwdCanvasOffsetRegion, {canvas, x, y})
end procedure

--global function wd_canvas_get_region_box(cdCanvas canvas)
global function wdCanvasGetRegionBox(cdCanvas canvas)
    atom pXmin = allocate(32),
         pXmax = pXmin+8,
         pYmin = pXmax+8,
         pYmax = pYmin+8
    c_proc(xwdCanvasGetRegionBox, {canvas, pXmin, pXmax, pYmin, pYmax})
    sequence box = iup_peek_double({pXmin, 4})
    free(pXmin)
    return box
end function

--global procedure wd_canvas_hardcopy(cdCanvas canvas, atom hCdContext, atom pData, atom cbFct)
global procedure wdCanvasHardcopy(cdCanvas canvas, atom hCdContext, pData, cbFct)
    c_proc(xwdCanvasHardcopy, {canvas, hCdContext, pData, cbFct})
end procedure

-----------------------------------------------------------------------------------------
----
----    world draw primitives
----
-------------------------------------------------------------------------------------------
--constant
--  xwdCanvasPixel  = iup_c_proc(hCd, "wdCanvasPixel", {P,D,D,L}),
--  xwdCanvasMark   = iup_c_proc(hCd, "wdCanvasMark", {P,D,D}),
--  xwdCanvasLine   = iup_c_proc(hCd, "wdCanvasLine", {P,D,D,D,D}),
--  xwdCanvasVertex = iup_c_proc(hCd, "wdCanvasVertex", {P,D,D}),
--  xwdCanvasRect   = iup_c_proc(hCd, "wdCanvasRect", {P,D,D,D,D}),
--  xwdCanvasBox    = iup_c_proc(hCd, "wdCanvasBox", {P,D,D,D,D}),
--  xwdCanvasArc    = iup_c_proc(hCd, "wdCanvasArc", {P,D,D,D,D,D,D}),
--  xwdCanvasSector = iup_c_proc(hCd, "wdCanvasSector", {P,D,D,D,D,D,D}),
--  xwdCanvasChord  = iup_c_proc(hCd, "wdCanvasChord", {P,D,D,D,D,D,D}),
--  xwdCanvasText   = iup_c_proc(hCd, "wdCanvasText", {P,D,D,P})

--global procedure wd_canvas_pixel(cdCanvas canvas, atom x, atom y)
global procedure wdCanvasPixel(cdCanvas canvas, atom x, y, colour)
    c_proc(xwdCanvasPixel, {canvas, x, y, colour})
end procedure

--global procedure wd_canvas_mark(cdCanvas canvas, atom x, atom y)
global procedure wdCanvasMark(cdCanvas canvas, atom x, y)
    c_proc(xwdCanvasMark, {canvas, x, y})
end procedure

--global procedure wd_canvas_line(cdCanvas canvas, atom minX, minY, maxX, maxY)
global procedure wdCanvasLine(cdCanvas canvas, atom minX, minY, maxX, maxY)
    c_proc(xwdCanvasLine, {canvas, minX, minY, maxX, maxY})
end procedure

--global procedure wd_canvas_vertex(cdCanvas canvas, atom x, atom y)
global procedure wdCanvasVertex(cdCanvas canvas, atom x, y)
    c_proc(xwdCanvasVertex, {canvas, x, y})
end procedure

--global procedure wd_canvas_rect(cdCanvas canvas, atom minX, atom minY, atom maxX, atom maxY)
global procedure wdCanvasRect(cdCanvas canvas, atom minX, minY, maxX, maxY)
    c_proc(xwdCanvasRect, {canvas, minX, minY, maxX, maxY})
end procedure

--global procedure wd_canvas_box(cdCanvas canvas, atom minX, atom minY, atom maxX, atom maxY)
global procedure wdCanvasBox(cdCanvas canvas, atom minX, minY, maxX, maxY)
    c_proc(xwdCanvasBox, {canvas, minX, minY, maxX, maxY})
end procedure

--global procedure wd_canvas_arc(cdCanvas canvas, atom xc, atom yc, atom w, atom h, atom a1, atom a2)
global procedure wdCanvasArc(cdCanvas canvas, atom xc, yc, w, h, a1, a2)
    c_proc(xwdCanvasArc, {canvas, xc, yc, w, h, a1, a2})
end procedure

--global procedure wd_canvas_sector(cdCanvas canvas, atom xc, atom yc, atom w, atom h, atom a1, atom a2)
global procedure wdCanvasSector(cdCanvas canvas, atom xc, yc, w, h, a1, a2)
    c_proc(xwdCanvasSector, {canvas, xc, yc, w, h, a1, a2})
end procedure

--global procedure wd_canvas_chord(cdCanvas canvas, atom xc, atom yc, atom w, atom h, atom a1, atom a2)
global procedure wdCanvasChord(cdCanvas canvas, atom xc, yc, w, h, a1, a2)
    c_proc(xwdCanvasChord, {canvas, xc, yc, w, h, a1, a2})
end procedure

--global procedure wd_canvas_text(cdCanvas canvas, atom x, atom y, string text)
global procedure wdCanvasText(cdCanvas canvas, atom x, y, string text)
    c_proc(xwdCanvasText, {canvas, x, y, text})
end procedure

-----------------------------------------------------------------------------------------
----
---- world draw images
----
-------------------------------------------------------------------------------------------
--constant
--  xwdCanvasPutImageRect       = iup_c_proc(hCd, "wdCanvasPutImageRect", {P,P,D,D,D,D,D,D}),
--  xwdCanvasPutImageRectRGB    = iup_c_proc(hCd, "wdCanvasPutImageRectRGB", {P,I,I,P,P,P,D,D,D,D,D,D,D,D,D,D}),
--  xwdCanvasPutImageRectRGBA   = iup_c_proc(hCd, "wdCanvasPutImageRectRGBA", {P,I,I,P,P,P,P,D,D,D,D,D,D,D,D}),
--  xwdCanvasPutImageRectMap    = iup_c_proc(hCd, "wdCanvasPutImageRectMap", {P,I,I,P,P,D,D,D,D,D,D,D,D}),
--  xwdCanvasPutBitmap          = iup_c_proc(hCd, "wdCanvasPutBitmap", {P,P,D,D,D,D})

--/* [DEV] as per cdCanvasPutImageRect...
--global procedure wd_canvas_put_image_rect(cdCanvas canvas, atom hCdImage, atom x, atom y,
global procedure wdCanvasPutImageRect(cdCanvas canvas, atom hCdImage, atom x, atom y,
                                      atom xmin, atom xmax, atom ymin, atom ymax)
    c_proc(xwdCanvasPutImageRect, {canvas, hCdImage, x, y, xmin, xmax, ymin, ymax})
end procedure
--*/

--global procedure wd_canvas_put_image_rect_rgb(cdCanvas canvas, atom iw, atom ih,
global procedure wdCanvasPutImageRectRGB(cdCanvas canvas, atom iw, ih, sequence rgb, 
                                         atom x, y, w, h, xmin, xmax, ymin, ymax)
    integer l = length(rgb[1])
    atom pR = allocate(l),
         pG = allocate(l),
         pB = allocate(l)
    poke(pR, rgb[1])
    poke(pG, rgb[2])
    poke(pB, rgb[3])
    c_proc(xwdCanvasPutImageRectRGB, {canvas, iw, ih, pR, pG, pB, x, y, w, h, xmin, xmax, ymin, ymax})
    free({pR,pG,pB})
end procedure

--global procedure wd_canvas_put_image_rect_rgba(cdCanvas canvas, atom iw, atom ih,
global procedure wdCanvasPutImageRectRGBA(cdCanvas canvas, atom iw, ih, sequence rgba, 
                                          atom x, y, w, h, xmin, xmax, ymin, ymax)
    integer l = length(rgba[1])
    atom pR = allocate(l),
         pG = allocate(l),
         pB = allocate(l),
         pA = allocate(l)
    poke(pR, rgba[1])
    poke(pG, rgba[2])
    poke(pB, rgba[3])
    poke(pA, rgba[4])
    c_proc(xwdCanvasPutImageRectRGBA, {canvas, iw, ih, pR, pG, pB, pA, x, y, w, h, xmin, xmax, ymin, ymax})
    free({pR,pG,pB,pA})
end procedure

--global procedure wd_canvas_put_image_rect_map(cdCanvas canvas, atom iw, atom ih,
global procedure wdCanvasPutImageRectMap(cdCanvas canvas, atom iw, ih, sequence index, colors,
                                         atom x, y, w, h, xmin, xmax, ymin, ymax)
    atom pColors = allocate(4*256+length(index)),
         pIndex = pColors+4*256
    poke4(pColors, colors)
    poke(pIndex, index)
    c_proc(xwdCanvasPutImageRectMap, {canvas, iw, ih, pIndex, pColors, x, y, w, h, xmin, xmax, ymin, ymax})
    free(pColors)
end procedure

--/* as per cdCanvasPutBitmap
--global procedure wd_canvas_put_bitmap(cdCanvas canvas, atom hCdBitmap, atom x, atom y, atom w, atom h)
global procedure wdCanvasPutBitmap(cdCanvas canvas, atom hCdBitmap, atom x, y, w, h)
    c_proc(xwdCanvasPutBitmap, {canvas, hCdBitmap, x, y, w, h})
end procedure
--*/

-----------------------------------------------------------------------------------------
----
---- world draw attributes
----
-------------------------------------------------------------------------------------------
--constant
--  xwdCanvasLineWidth      = iup_c_func(hCd, "wdCanvasLineWidth", {P,D},D),
--  xwdCanvasFont           = iup_c_proc(hCd, "wdCanvasFont", {P,P,I,D}),
--  xwdCanvasGetFont        = iup_c_proc(hCd, "wdCanvasGetFont", {P,P,P,P}),
--  xwdCanvasGetFontDim     = iup_c_proc(hCd, "wdCanvasGetFontDim", {P,P,P,P,P}),
--  xwdCanvasMarkSize       = iup_c_func(hCd, "wdCanvasMarkSize", {P,D},D),
--  xwdCanvasGetTextSize    = iup_c_proc(hCd, "wdCanvasGetTextSize", {P,P,P,P}),
--  xwdCanvasGetTextBox     = iup_c_proc(hCd, "wdCanvasGetTextBox", {P,D,D,P,P,P,P,P}),
--  xwdCanvasGetTextBounds  = iup_c_proc(hCd, "wdCanvasGetTextBounds", {P,D,D,P,P}),
--  xwdCanvasStipple        = iup_c_proc(hCd, "wdCanvasStipple", {P,I,I,P,D,D}),
--  xwdCanvasPattern        = iup_c_proc(hCd, "wdCanvasPattern", {P,I,I,P,D,D})

--global function wd_canvas_line_width(cdCanvas canvas, atom width)
--global function wdCanvasLineWidth(cdCanvas canvas, atom width)
--  return c_func(xwdCanvasLineWidth, {canvas, width})
--end function

global procedure wdCanvasSetLineWidth(cdCanvas canvas, atom width)
    width = c_func(xwdCanvasLineWidth, {canvas, width})
end procedure

global function wdCanvasGetLineWidth(cdCanvas canvas)
    return c_func(xwdCanvasLineWidth, {canvas, CD_QUERY})
end function

global procedure wdCanvasFont(cdCanvas canvas, nullable_string font, integer style, atom size)
    c_proc(xwdCanvasFont, {canvas, font, style, size})
end procedure

--global function wd_canvas_get_font(cdCanvas canvas)
global function wdCanvasGetFont(cdCanvas canvas)
    atom pStyle = allocate(1024),
         pSize = pStyle+4,
         pFont = pSize+8
    c_proc(xwdCanvasGetFont, {canvas, pFont, pStyle, pSize})
    sequence font = {peek_string(pFont)} & peek4s(pStyle) & iup_peek_double(pSize)
    free(pStyle)
    return font
end function

global function wdCanvasGetFontDim(cdCanvas canvas)
    atom pWidth = allocate(32),
         pHeight = pWidth+8,
         pAscent = pHeight+8,
         pDescent = pAscent+8
    c_proc(xwdCanvasGetFontDim, {canvas, pWidth, pHeight, pAscent, pDescent})
    sequence font_metrics = iup_peek_double({pWidth, 4})
    free(pWidth)
    return font_metrics -- {width, height, ascent, descent}
end function

--global function wd_canvas_mark_size(cdCanvas canvas, atom msize)
global function wdCanvasMarkSize(cdCanvas canvas, atom msize)
    return c_func(xwdCanvasMarkSize, {canvas, msize})
end function

global function wdCanvasGetTextSize(cdCanvas canvas, sequence text)
    atom pW = allocate(16),
         pH = pW+8
    c_proc(xwdCanvasGetTextSize, {canvas, text, pW, pH})
    sequence text_size = iup_peek_double({pW, 2})
    free(pW)
    return text_size    -- {width,height}
end function

global function wdCanvasGetTextBox(cdCanvas canvas, atom x, y, string text)
    atom pXmin = allocate(32),
         pXmax = pXmin+8,
         pYmin = pXmax+8,
         pYmax = pYmin+8
    c_proc(xwdCanvasGetTextBox, {canvas, x, y, text, pXmin, pXmax, pYmin, pYmax})
    sequence box = iup_peek_double({pXmin, 4})
    free(pXmin)
    return box
end function

global function wdCanvasGetTextBounds(cdCanvas canvas, atom x, y, string text)
    atom pRect = allocate(64)
    c_proc(xwdCanvasGetTextBounds, {canvas, x, y, text, pRect})
    sequence bounds = iup_peek_double({pRect, 8})
    free(pRect)
    return bounds
end function

--global procedure wd_canvas_stipple(cdCanvas canvas, atom width, atom height, sequence stipple)
global procedure wdCanvasSetStipple(cdCanvas canvas, atom width, height, sequence stipple,
                                                     atom width_mm, height_mm)
    atom pStipple = allocate(length(stipple))
    poke(pStipple, stipple)
    c_proc(xwdCanvasStipple, {canvas, width, height, pStipple, width_mm, height_mm})
    free(pStipple)
end procedure

--global procedure wd_canvas_pattern(cdCanvas canvas, atom width, atom height, sequence pattern, atom width_mm, atom height_mm)
global procedure wdCanvasSetPattern(cdCanvas canvas, atom width, height, sequence pattern, atom width_mm, height_mm)
    atom pPattern = allocate(4*length(pattern))
    poke4(pPattern, pattern)
    c_proc(xwdCanvasPattern, {canvas, width, height, pPattern, width_mm, height_mm})
    free(pPattern)
end procedure

-----------------------------------------------------------------------------------------
----
---- world draw vector text
----
-------------------------------------------------------------------------------------------
--constant
--  xwdCanvasVectorTextDirection    = iup_c_proc(hCd, "wdCanvasVectorTextDirection", {P,D,D,D,D}),
--  xwdCanvasVectorTextSize         = iup_c_proc(hCd, "wdCanvasVectorTextSize", {P,D,D,P}),
--  xwdCanvasGetVectorTextSize      = iup_c_proc(hCd, "wdCanvasGetVectorTextSize", {P,P,P,P}),
--  xwdCanvasVectorCharSize         = iup_c_func(hCd, "wdCanvasVectorCharSize", {P,D},D),
--  xwdCanvasVectorText             = iup_c_proc(hCd, "wdCanvasVectorText", {P,D,D,P}),
--  xwdCanvasMultiLineVectorText    = iup_c_proc(hCd, "wdCanvasMultiLineVectorText", {P,D,D,P}),
--  xwdCanvasGetVectorTextBounds    = iup_c_proc(hCd, "wdCanvasGetVectorTextBounds", {P,P,D,D,P})

--global procedure wd_canvas_vector_text_direction(cdCanvas canvas, atom x1, atom y1, atom x2, atom y2)
global procedure wdCanvasVectorTextDirection(cdCanvas canvas, atom x1, atom y1, atom x2, atom y2)
    c_proc(xwdCanvasVectorTextDirection, {canvas, x1, y1, x2, y2})
end procedure

--global procedure wd_canvas_vector_text_size(cdCanvas canvas, atom w, atom h, string text)
global procedure wdCanvasVectorTextSize(cdCanvas canvas, atom w, atom h, string text)
    c_proc(xwdCanvasVectorTextSize, {canvas, w, h, text})
end procedure

--global function wd_canvas_vector_char_size(cdCanvas canvas, atom size)
global function wdCanvasVectorCharSize(cdCanvas canvas, atom size)
    return c_func(xwdCanvasVectorCharSize, {canvas, size})
end function

--global function wd_canvas_get_vector_text_size(cdCanvas canvas, string text)
global function wdCanvasGetVectorTextSize(cdCanvas canvas, string text)
    atom pX = allocate(16),
         pY = pX+8
    c_proc(xwdCanvasGetVectorTextSize, {canvas, text, pX, pY})
    sequence x_y = iup_peek_double({pX, 2})
    free(pX)
    return x_y
end function

global function wdCanvasGetVectorTextBounds(cdCanvas canvas, string text, atom px, atom py)
    atom pRect = allocate(8*8)
    c_proc(xwdCanvasGetVectorTextBounds, {canvas, text, px, py, pRect})
    sequence rect = iup_peek_double({pRect, 8})
    free(pRect)
    return rect
end function

--global procedure wd_canvas_vector_text(cdCanvas canvas, atom x, atom y, string text)
global procedure wdCanvasVectorText(cdCanvas canvas, atom x, atom y, string text)
    c_proc(xwdCanvasVectorText, {canvas, x, y, text})
end procedure

--global procedure wd_canvas_multi_line_vector_text(cdCanvas canvas, atom x, atom y, string text)
global procedure wdCanvasMultiLineVectorText(cdCanvas canvas, atom x, atom y, string text)
    c_proc(xwdCanvasMultiLineVectorText, {canvas, x, y, text})
end procedure

-- pplot.e: (not documented, use IupPlot instead)
--/*
atom
    hIupPPlot = 0,
    xIupPPlotOpen,
    xIupPPlot,
    xIupPPlotBegin,
    xIupPPlotAdd,
    xIupPPlotAddStr,
    xIupPPlotEnd,
    xIupPPlotInsert,
    xIupPPlotInsertStr,
    xIupPPlotTransform,
    xIupPPlotPaintTo

procedure iup_init_pplot()
    if hIupPPlot=0 then
        hIupPPlot = iup_open_dll({
                                  "iup_pplot.dll",
                                  "libiup_pplot.so",
                                  "libiup_pplot.dylib"
                                 })

        xIupPPlotOpen       = iup_c_proc(hIupPPlot, "IupPPlotOpen", {})
        xIupPPlot           = iup_c_func(hIupPPlot, "IupPPlot", {},P)
        xIupPPlotBegin      = iup_c_proc(hIupPPlot, "IupPPlotBegin", {P,I})
        xIupPPlotAdd        = iup_c_proc(hIupPPlot, "IupPPlotAdd", {P,F,F})
        xIupPPlotAddStr     = iup_c_proc(hIupPPlot, "IupPPlotAddStr", {P,P,F})
        xIupPPlotEnd        = iup_c_proc(hIupPPlot, "IupPPlotEnd", {P})
        xIupPPlotInsert     = iup_c_proc(hIupPPlot, "IupPPlotInsert", {P,I,I,F,F})
        xIupPPlotInsertStr  = iup_c_proc(hIupPPlot, "IupPPlotInsertStr", {P,I,I,P,F})
        xIupPPlotTransform  = iup_c_proc(hIupPPlot, "IupPPlotTransform", {P,F,F,P,P})
        xIupPPlotPaintTo    = iup_c_proc(hIupPPlot, "IupPPlotPaintTo", {P,P})
    end if
end procedure

integer did_pplot_open = 0

procedure pplot_open()
    did_pplot_open = 1
    iup_init_pplot()
    c_proc(xIupPPlotOpen, {})
end procedure

global function IupPPlot(string attributes="", sequence args={})
    if not did_pplot_open then
        pplot_open()
    end if

    Ihandle ih = c_func(xIupPPlot, {})

    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if

    return ih
end function

global procedure IupPPlotBegin(Ihandle ih, integer str_xdata)
    iup_init_pplot()
    c_proc(xIupPPlotBegin, {ih, str_xdata})
end procedure

global procedure IupPPlotAdd(Ihandle ih, atom x, atom y)
    c_proc(xIupPPlotAdd, {ih, x, y})
end procedure

global procedure add_str(Ihandle ih, string x, atom y)
    c_proc(xIupPPlotAddStr, {ih, x, y})
end procedure

global procedure IupPPlotEnd(Ihandle ih)
    c_proc(xIupPPlotEnd, {ih})
end procedure

global procedure insert_plot(Ihandle ih, integer index, integer sample_index, atom x, atom y)
    c_proc(xIupPPlotInsert, {ih, index, sample_index, x, y})
end procedure

global procedure insert_str(Ihandle ih, integer index, integer sample_index, string x, atom y)
    c_proc(xIupPPlotInsertStr, {ih, index, sample_index, x, y})
end procedure

global function transform_plot(Ihandle ih, atom x, atom y)
    atom pX = allocate(4),
         pY = allocate(4)

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
--*/

-- the new IupPlot:

atom
    hIupPlot = 0,
    xIupPlotOpen,
    xIupPlot,
    xIupPlotBegin,
    xIupPlotAdd,
    xIupPlotAddSegment,
    xIupPlotAddStr,
    xIupPlotEnd,
    xIupPlotLoadData,
    xIupPlotInsert,
    xIupPlotInsertSegment,
    xIupPlotInsertStr,
    xIupPlotInsertSamples,
    xIupPlotInsertStrSamples,
    xIupPlotAddSamples,
    xIupPlotAddStrSamples,
    xIupPlotGetSample,
    xIupPlotGetSampleStr,
    xIupPlotGetSampleSelection,
    xIupPlotGetSampleExtra,
    xIupPlotSetSample,
    xIupPlotSetSampleStr,
    xIupPlotSetSampleSelection,
    xIupPlotSetSampleExtra,
    xIupPlotTransform,
    xIupPlotTransformTo,
    xIupPlotFindSample,
    xIupPlotFindSegment,
    xIupPlotPaintTo,
--  xIupPlotSetFormula
    $

procedure iup_init_plot()
    if hIupPlot=0 then
        hIupPlot = iup_open_dll({
                                 "iup_plot.dll",
                                 "libiup_plot.so",
                                 "libiup_plot.dylib"
                                })

        xIupPlotOpen                = iup_c_proc(hIupPlot, "IupPlotOpen", {})
        xIupPlot                    = iup_c_func(hIupPlot, "IupPlot", {},P)
        xIupPlotBegin               = iup_c_proc(hIupPlot, "IupPlotBegin", {P,I})
        xIupPlotAdd                 = iup_c_proc(hIupPlot, "IupPlotAdd", {P,D,D})
        xIupPlotAddSegment          = iup_c_proc(hIupPlot, "IupPlotAddSegment", {P,D,D})
        xIupPlotAddStr              = iup_c_proc(hIupPlot, "IupPlotAddStr", {P,P,D})
        xIupPlotEnd                 = iup_c_func(hIupPlot, "IupPlotEnd", {P},I)
        xIupPlotLoadData            = iup_c_proc(hIupPlot, "IupPlotLoadData", {P,P,I})
        xIupPlotInsert              = iup_c_proc(hIupPlot, "IupPlotInsert", {P,I,I,D,D})
        xIupPlotInsertSegment       = iup_c_proc(hIupPlot, "IupPlotInsertSegment", {P,I,I,P,D})
        xIupPlotInsertStr           = iup_c_proc(hIupPlot, "IupPlotInsertStr", {P,I,I,P,D})
        xIupPlotInsertSamples       = iup_c_proc(hIupPlot, "IupPlotInsertSamples", {P,I,I,P,P,I})
        xIupPlotInsertStrSamples    = iup_c_proc(hIupPlot, "IupPlotInsertStrSamples", {P,I,I,P,P,I})
        xIupPlotAddSamples          = iup_c_proc(hIupPlot, "IupPlotAddSamples", {P,I,P,P,I})
        xIupPlotAddStrSamples       = iup_c_proc(hIupPlot, "IupPlotAddStrSamples", {P,I,P,P,I})
        xIupPlotGetSample           = iup_c_proc(hIupPlot, "IupPlotGetSample", {P,I,I,P,P})
        xIupPlotGetSampleStr        = iup_c_proc(hIupPlot, "IupPlotGetSampleStr", {P,I,I,P,P})
        xIupPlotGetSampleSelection  = iup_c_func(hIupPlot, "IupPlotGetSampleSelection", {P,I,I},I)
        xIupPlotGetSampleExtra      = iup_c_func(hIupPlot, "IupPlotGetSampleExtra", {P,I,I},D)
        xIupPlotSetSample           = iup_c_proc(hIupPlot, "IupPlotSetSample", {P,I,I,D,D})
        xIupPlotSetSampleStr        = iup_c_proc(hIupPlot, "IupPlotSetSampleStr", {P,I,I,P,D})
        xIupPlotSetSampleSelection  = iup_c_proc(hIupPlot, "IupPlotSetSampleSelection", {P,I,I,I})
        xIupPlotSetSampleExtra      = iup_c_proc(hIupPlot, "IupPlotSetSampleExtra", {P,I,I,D})
        xIupPlotTransform           = iup_c_proc(hIupPlot, "IupPlotTransform", {P,D,D,P,P})
        xIupPlotTransformTo         = iup_c_proc(hIupPlot, "IupPlotTransformTo", {P,D,D,P,P})
        xIupPlotFindSample          = iup_c_proc(hIupPlot, "IupPlotFindSample", {P,D,D,P,P})
        xIupPlotFindSegment         = iup_c_proc(hIupPlot, "IupPlotFindSegment", {P,D,D,P,P,P})
        xIupPlotPaintTo             = iup_c_proc(hIupPlot, "IupPlotPaintTo", {P,P})
--link error (using v3.17)
--      xIupPlotSetFormula          = iup_c_proc(hIupPlot, "IupPlotSetFormula", {P,I,P,P})
    end if
end procedure

integer did_plot_open = 0

global procedure IupPlotOpen()
    did_plot_open = 1
    iup_init_plot()
    c_proc(xIupPlotOpen, {})
end procedure

global function IupPlot(string attributes="", sequence args={})
    if not did_plot_open then
        IupPlotOpen()
    end if
    Ihandle ih = c_func(xIupPlot, {})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global procedure IupPlotBegin(Ihandle ih, boolean str_xdata=false)
    c_proc(xIupPlotBegin, {ih, str_xdata})
end procedure

global procedure IupPlotAdd(Ihandle ih, atom x, atom y)
    c_proc(xIupPlotAdd, {ih, x, y})
end procedure

global procedure IupPlotAddSegment(Ihandle ih, atom x, atom y)
    c_proc(xIupPlotAddSegment, {ih, x, y})
end procedure

global procedure IupPlotAddStr(Ihandle ih, string x, atom y)
    c_proc(xIupPlotAddStr, {ih, x, y})
end procedure

global function IupPlotEnd(Ihandle ih)
    integer ds_index = c_func(xIupPlotEnd, {ih})
    return ds_index
end function

global function IupPlotLoadData(Ihandle ih, string filename, integer str_xdata)
    integer res = c_func(xIupPlotLoadData,{ih, filename, str_xdata})
    return res
end function

global procedure IupPlotInsert(Ihandle ih, integer index, integer sample_index, atom x, atom y)
    c_proc(xIupPlotInsert, {ih, index, sample_index, x, y})
end procedure

global procedure IupPlotInsertSegment(Ihandle ih, integer index, integer sample_index, string x, atom y)
    c_proc(xIupPlotInsertSegment, {ih, index, sample_index, x, y})
end procedure

global procedure IupPlotInsertStr(Ihandle ih, integer index, integer sample_index, string x, atom y)
    c_proc(xIupPlotInsertStr, {ih, index, sample_index, x, y})
end procedure

--void IupPlotInsertSamples(Ihandle *ih, int ds_index, int sample_index, double* x, double* y, int count); 
global procedure IupPlotInsertSamples(Ihandle ih, integer index, integer sample_index, sequence x, sequence y, integer count)
    atom pX = allocate(count*8),
         pY = allocate(count*8)
--  if count<length(x) then ?9/0 end if
--  if length(x)!=length(y) then ?9/0 end if
    iup_poke_double(pX, x[1..count])
    iup_poke_double(pY, y[1..count])
    c_proc(xIupPlotInsertSamples, {ih, index, sample_index, pX, pY, count})
    free(pX)
    free(pY)
end procedure

global procedure IupPlotInsertStrSamples(Ihandle ih, integer index, integer sample_index, sequence xstrings, sequence y, integer count)
    atom pX = allocate(count*W),
         pY = allocate(count*8)
    iup_poke_string_pointer_array(pX, xstrings)
    iup_poke_double(pY, y)
    c_proc(xIupPlotInsertStrSamples, {ih, index, sample_index, pX, pY, count})
    free(pX)
    free(pY)
end procedure

global procedure IupPlotAddSamples(Ihandle ih, integer index, sequence x, sequence y, integer count)
    atom pX = allocate(count*8),
         pY = allocate(count*8)
    iup_poke_double(pX, x)
    iup_poke_double(pY, y)
    c_proc(xIupPlotAddSamples, {ih, index, pX, pY, count})
    free(pX)
    free(pY)
end procedure

global procedure IupPlotAddStrSamples(Ihandle ih, integer index, sequence xstrings, sequence y, integer count)
    atom pX = allocate(count*W),
         pY = allocate(count*8)
    iup_poke_string_pointer_array(pX, xstrings)
    iup_poke_double(pY, y)
    c_proc(xIupPlotAddStrSamples, {ih, index, pX, pY, count})
    free(pX)
    free(pY)
end procedure

--void IupPlotGetSample(Ihandle *ih, int ds_index, int sample_index, double *x, double *y);
global function IupPlotGetSample(Ihandle ih, integer ds_index, integer sample_index)
    atom pXY = allocate(8*2)
    c_proc(xIupPlotGetSample, {ih, ds_index, sample_index, pXY, pXY+8})
    sequence res = iup_peek_double({pXY,2})
    free(pXY)
    return res
end function

--void IupPlotGetSampleStr(Ihandle *ih, int ds_index, int sample_index, const char* *x, double *y);
global function IupPlotGetSampleStr(Ihandle ih, integer ds_index, sample_index)
    atom pX = allocate(8*2),
         pY = allocate(8)
    c_proc(xIupPlotGetSampleStr, {ih, ds_index, sample_index, pX, pY})
    sequence res = {peek_string(pX),iup_peek_double(pY)}
    free(pX)
    free(pY)
    return res
end function

--int IupPlotGetSampleSelection(Ihandle *ih, int ds_index, int sample_index);
global function IupPlotGetSampleSelection(Ihandle ih, integer ds_index, sample_index)
-- returns -1 if an error occurs (hence a bool res rather than boolean)
    bool selected = c_func(xIupPlotGetSampleSelection, {ih, ds_index, sample_index})
    return selected
end function

global function IupPlotGetSampleExtra(Ihandle ih, integer ds_index, sample_index)
    atom v = c_func(xIupPlotGetSampleExtra, {ih, ds_index, sample_index})
    return v
end function

--void IupPlotSetSample(Ihandle *ih, int ds_index, int sample_index, double x, double y);
global procedure IupPlotSetSample(Ihandle ih, integer ds_index, integer sample_index, atom x, atom y)
    c_proc(xIupPlotSetSample, {ih, ds_index, sample_index, x, y})
end procedure

--void IupPlotSetSampleStr(Ihandle *ih, int ds_index, int sample_index, const char* x, double y);
global procedure IupPlotSetSampleStr(Ihandle ih, integer ds_index, integer sample_index, string x, atom y)
    c_proc(xIupPlotSetSampleStr, {ih, ds_index, sample_index, x, y})
end procedure

--void IupPlotSetSampleSelection(Ihandle *ih, int ds_index, int sample_index, int selected);
global procedure IupPlotSetSampleSelection(Ihandle ih, integer ds_index, integer sample_index, boolean selected)
    c_proc(xIupPlotSetSampleSelection, {ih, ds_index, sample_index, selected})
end procedure

global procedure IupPlotSetSampleExtra(Ihandle ih, integer ds_index, integer sample_index, atom extra)
    c_proc(xIupPlotSetSampleExtra, {ih, ds_index, sample_index, extra})
end procedure

--void IupPlotTransform(Ihandle* ih, double x, double y, double *cnv_x, double *cnv_y); 
global function IupPlotTransform(Ihandle ih, atom x, atom y)
    atom pXY = allocate(8*2)
    c_proc(xIupPlotTransform, {ih, x, y, pXY, pXY+8})
    {x,y} = iup_peek_double({pXY,2})
    free(pXY)
    return {x,y}
end function

--void IupPlotTransformTo(Ihandle* ih, double cnv_x, double cnv_y, double *x, double *y); 
global function IupPlotTransformTo(Ihandle ih, atom x, atom y)
    atom pXY = allocate(8*2)
    c_proc(xIupPlotTransformTo, {ih, x, y, pXY, pXY+8})
    {x,y} = iup_peek_double({pXY,2})
    free(pXY)
    return {x,y}
end function

--int IupPlotFindSample(Ihandle* ih, double cnv_x, double cnv_y, int *ds_index, int *sample_index);
global function IupPlotFindSample(Ihandle ih, atom x, atom y)
    atom p_ds_index = allocate(W),
         p_sample_index = allocate(W)
    if c_func(xIupPlotFindSample, {ih, x, y, p_ds_index, p_sample_index}) then return 0 end if
    integer ds_index = peekNS(p_ds_index,W,0)
    integer sample_index = peekNS(p_sample_index,W,0)
    free(p_ds_index)
    free(p_sample_index)
    return {ds_index, sample_index}
end function

global function IupPlotFindSegment(Ihandle ih, atom x, atom y)
    atom p_ds_index = allocate(W*3),
         p_sample_index1 = p_ds_index+W,
         p_sample_index2 = p_ds_index+W*2
    if c_func(xIupPlotFindSegment, {ih, x, y, p_ds_index, p_sample_index1, p_sample_index2}) then return 0 end if
    integer ds_index = peekNS(p_ds_index,W,0)
    integer sample_index1 = peekNS(p_sample_index1,W,0),
            sample_index2 = peekNS(p_sample_index2,W,0)
    free(p_ds_index)
    return {ds_index, sample_index1, sample_index2}
end function

--void IupPlotPaintTo(Ihandle ih, cdCanvas cnv); 
global procedure IupPlotPaintTo(Ihandle ih, cdCanvas cnv)
    c_proc(xIupPlotPaintTo, {ih, cnv})
end procedure

--void IupPlotSetFormula(Ihandle* ih, int sample_count, const char* formula, const char* init); 
--global procedure IupPlotSetFormula(Ihandle ih, integer sample_count, string formula, nullable_string init)
--  c_proc(xIupPlotSetFormula, {ih, sample_count, formula, init})
--end procedure

atom
    hIupMglPlot = 0,
    xIupMglPlotOpen,
    xIupMglPlot,
    xIupMglPlotBegin,
    xIupMglPlotAdd1D,
    xIupMglPlotAdd2D,
    xIupMglPlotAdd3D,
    xIupMglPlotDrawLine,
    xIupMglPlotDrawMark,
    xIupMglPlotDrawText,
    xIupMglPlotEnd,
    xIupMglPlotInsert1D,
    xIupMglPlotInsert2D,
    xIupMglPlotInsert3D,
    xIupMglPlotLoadData,
    xIupMglPlotNewDataSet,
    xIupMglPlotPaintTo,
    xIupMglPlotSet1D,
    xIupMglPlotSet2D,
    xIupMglPlotSet3D,
    xIupMglPlotSetData,
    xIupMglPlotSetFormula,
    xIupMglPlotSetFromFormula,
    xIupMglPlotTransform,
    xIupMglPlotTransformTo,
    $

procedure iup_init_mglplot()
    if hIupMglPlot=0 then
        hIupMglPlot = iup_open_dll({
                                 "iup_mglplot.dll",
                                 "libiup_mglplot.so",
                                 "libiup_mglplot.dylib"
                                })

        xIupMglPlotOpen                 = iup_c_proc(hIupMglPlot, "IupMglPlotOpen", {})
        xIupMglPlot                     = iup_c_func(hIupMglPlot, "IupMglPlot", {},P)
        xIupMglPlotBegin                = iup_c_proc(hIupMglPlot, "IupMglPlotBegin", {P,I})
        xIupMglPlotAdd1D                = iup_c_proc(hIupMglPlot, "IupMglPlotAdd1D", {P,P,D})
        xIupMglPlotAdd2D                = iup_c_proc(hIupMglPlot, "IupMglPlotAdd2D", {P,D,D})
        xIupMglPlotAdd3D                = iup_c_proc(hIupMglPlot, "IupMglPlotAdd3D", {P,D,D,D})
        xIupMglPlotDrawLine             = iup_c_proc(hIupMglPlot, "IupMglPlotDrawLine", {P,D,D,D,D,D,D})
        xIupMglPlotDrawMark             = iup_c_proc(hIupMglPlot, "IupMglPlotDrawMark", {P,D,D,D})
        xIupMglPlotDrawText             = iup_c_proc(hIupMglPlot, "IupMglPlotDrawText", {P,P,D,D,D})
        xIupMglPlotEnd                  = iup_c_func(hIupMglPlot, "IupMglPlotEnd", {P},I)
        xIupMglPlotInsert1D             = iup_c_proc(hIupMglPlot, "IupMglPlotInsert1D", {P,I,I,P,P,I})
        xIupMglPlotInsert2D             = iup_c_proc(hIupMglPlot, "IupMglPlotInsert2D", {P,I,I,P,P,I})
        xIupMglPlotInsert3D             = iup_c_proc(hIupMglPlot, "IupMglPlotInsert3D", {P,I,I,P,P,P,I})
        xIupMglPlotLoadData             = iup_c_proc(hIupMglPlot, "IupMglPlotLoadData", {P,I,P,I,I,I})
        xIupMglPlotNewDataSet           = iup_c_func(hIupMglPlot, "IupMglPlotNewDataSet", {P,I},I)
        xIupMglPlotPaintTo              = iup_c_proc(hIupMglPlot, "IupMglPlotPaintTo", {P,P,I,I,D,P})
        xIupMglPlotSet1D                = iup_c_proc(hIupMglPlot, "IupMglPlotInsert1D", {P,I,I,P,P,I})
        xIupMglPlotSet2D                = iup_c_proc(hIupMglPlot, "IupMglPlotInsert2D", {P,I,I,P,P,I})
        xIupMglPlotSet3D                = iup_c_proc(hIupMglPlot, "IupMglPlotInsert3D", {P,I,I,P,P,P,I})
        xIupMglPlotSetData              = iup_c_proc(hIupMglPlot, "IupMglPlotSetData", {P,I,P,I,I,I})
        xIupMglPlotSetFormula           = iup_c_proc(hIupMglPlot, "IupMglPlotSetFormula", {P,I,P,P,P,I})
        xIupMglPlotSetFromFormula       = iup_c_proc(hIupMglPlot, "IupMglPlotSetFromFormula", {P,I,P,I,I,I})
        xIupMglPlotTransform            = iup_c_proc(hIupMglPlot, "IupMglPlotTransform", {P,D,D,D,P,P})
        xIupMglPlotTransformTo          = iup_c_proc(hIupMglPlot, "IupMglPlotTransformTo", {P,I,I,P,P,P})
    end if
end procedure

integer did_mglplot_open = 0

global procedure IupMglPlotOpen()
    did_mglplot_open = 1
    iup_init_mglplot()
    c_proc(xIupMglPlotOpen, {})
end procedure

global function IupMglPlot(string attributes="", sequence args={})
    if not did_mglplot_open then
        IupMglPlotOpen()
    end if
    Ihandle ih = c_func(xIupMglPlot, {})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global procedure IupMglPlotBegin(Ihandle ih, integer dim)
    c_proc(xIupMglPlotBegin, {ih,dim})
end procedure

global procedure IupMglPlotAdd1D(Ihandle ih, nullable_string name, atom y)
    c_proc(xIupMglPlotAdd1D, {ih,name,y})
end procedure

global procedure IupMglPlotAdd2D(Ihandle ih, atom x, y)
    c_proc(xIupMglPlotAdd2D, {ih,x,y})
end procedure

global procedure IupMglPlotAdd3D(Ihandle ih, atom x, y, z)
    c_proc(xIupMglPlotAdd3D, {ih,x,y,z})
end procedure

global function IupMglPlotEnd(Ihandle ih)
    integer ds_index = c_func(xIupMglPlotEnd,{ih})
    return ds_index
end function

global function IupMglPlotNewDataSet(Ihandle ih, integer dim)
    integer ds_index = c_func(xIupMglPlotNewDataSet, {ih,dim})
    return ds_index
end function

global procedure IupMglPlotInsert1D(Ihandle ih, integer ds_index, sample_index, object names, sequence y, integer count=-1)
    atom pNames = NULL
    if sequence(names) then
        if count=-1 then
            count = length(names)
        else
            if length(names)!=count then ?9/0 end if
        end if
        atom pX = allocate(count*W)
        iup_poke_string_pointer_array(pX, names)
    end if
    if count=-1 then
        count = length(y)
    else
        if length(y)!=count then ?9/0 end if
    end if
    atom pY = allocate(count*8)
    iup_poke_double(pY, y)
    c_proc(xIupMglPlotInsert1D, {ih, ds_index, sample_index, pNames, pY, count})
    if pNames!=NULL then
        free(pNames)
    end if
    free(pY)
end procedure

global procedure IupMglPlotInsert2D(Ihandle ih, integer ds_index, sample_index, sequence x, y, integer count=-1)
    if count=-1 then
        count = length(x)
    else
        if length(x)!=count then ?9/0 end if
    end if
    if length(y)!=count then ?9/0 end if

    atom pX = allocate(count*W),
         pY = allocate(count*W)
    iup_poke_double(pX, x)
    iup_poke_double(pY, y)
    c_proc(xIupMglPlotInsert2D, {ih, ds_index, sample_index, pX, pY, count})
    free(pX)
    free(pY)
end procedure

global procedure IupMglPlotInsert3D(Ihandle ih, integer ds_index, sample_index, sequence x, y, z, integer count=-1)
    if count=-1 then
        count = length(x)
    else
        if length(x)!=count then ?9/0 end if
    end if
    if length(y)!=count then ?9/0 end if
    if length(z)!=count then ?9/0 end if

    atom pX = allocate(count*W),
         pY = allocate(count*W),
         pZ = allocate(count*W)
    iup_poke_double(pX, x)
    iup_poke_double(pY, y)
    iup_poke_double(pZ, z)
    c_proc(xIupMglPlotInsert3D, {ih, ds_index, sample_index, pX, pY, pZ, count})
    free(pX)
    free(pY)
    free(pZ)
end procedure

global procedure IupMglPlotSet1D(Ihandle ih, integer ds_index, sample_index, object names, sequence y, integer count=-1)
    atom pNames = NULL
    if sequence(names) then
        if count=-1 then
            count = length(names)
        else
            if length(names)!=count then ?9/0 end if
        end if
        atom pX = allocate(count*W)
        iup_poke_string_pointer_array(pX, names)
    end if
    if count=-1 then
        count = length(y)
    else
        if length(y)!=count then ?9/0 end if
    end if
    atom pY = allocate(count*8)
    iup_poke_double(pY, y)
    c_proc(xIupMglPlotSet1D, {ih, ds_index, sample_index, pNames, pY, count})
    if pNames!=NULL then
        free(pNames)
    end if
    free(pY)
end procedure

global procedure IupMglPlotSet2D(Ihandle ih, integer ds_index, sample_index, sequence x, y, integer count=-1)
    if count=-1 then
        count = length(x)
    else
        if length(x)!=count then ?9/0 end if
    end if
    if length(y)!=count then ?9/0 end if

    atom pX = allocate(count*W),
         pY = allocate(count*W)
    iup_poke_double(pX, x)
    iup_poke_double(pY, y)
    c_proc(xIupMglPlotSet2D, {ih, ds_index, sample_index, pX, pY, count})
    free(pX)
    free(pY)
end procedure

global procedure IupMglPlotSet3D(Ihandle ih, integer ds_index, sample_index, sequence x, y, z, integer count=-1)
    if count=-1 then
        count = length(x)
    else
        if length(x)!=count then ?9/0 end if
    end if
    if length(y)!=count then ?9/0 end if
    if length(z)!=count then ?9/0 end if

    atom pX = allocate(count*W),
         pY = allocate(count*W),
         pZ = allocate(count*W)
    iup_poke_double(pX, x)
    iup_poke_double(pY, y)
    iup_poke_double(pZ, z)
    c_proc(xIupMglPlotSet3D, {ih, ds_index, sample_index, pX, pY, pZ, count})
    free(pX)
    free(pY)
    free(pZ)
end procedure

global procedure IupMglPlotSetFormula(Ihandle ih, integer ds_index, string formulaX, nullable_string formulaY, formulaZ, integer count)
    c_proc(xIupMglPlotSetFormula, {ih, ds_index, formulaX, formulaY, formulaZ, count})
end procedure

global procedure IupMglPlotSetData(Ihandle ih, integer ds_index, sequence data, integer count_x, count_y, count_z)
    if length(data)!=count_x*count_y*count_z then ?9/0 end if
    atom pData = allocate(count_x*count_y*count_z*8)
    iup_poke_double(pData, data)
    c_proc(xIupMglPlotSetData, {ih, ds_index, pData, count_x, count_y, count_z})
    free(pData)
end procedure

global procedure IupMglPlotLoadData(Ihandle ih, integer ds_index, string filename, integer count_x, count_y, count_z)
    c_proc(xIupMglPlotLoadData, {ih, ds_index, filename, count_x, count_y, count_z})
end procedure

global procedure IupMglPlotSetFromFormula(Ihandle ih, integer ds_index, string formula, integer count_x, count_y, count_z)
    c_proc(xIupMglPlotSetFromFormula, {ih, ds_index, formula, count_x, count_y, count_z})
end procedure

global function IupMglPlotTransform(Ihandle ih, atom x, y, z)
    atom pX = allocate(W*2),
         pY = pX+W
    c_proc(xIupMglPlotTransform, {ih, x, y, z, pX, pY})
    sequence ixy = peekns({pX, 2})  -- {ix,iy}
    free(pX)
    return ixy  -- {ix,iy}
end function

global function IupMglPlotTransformTo(Ihandle ih, integer ix, iy)
    atom pX = allocate(8*3),
         pY = pX+8,
         pZ = pY+8
    c_proc(xIupMglPlotTransformTo, {ih, ix, iy, pX, pY, pZ})
    sequence xyz = iup_peek_double({pX, 3})
    free(pX)
    return xyz  -- {x,y,z}
end function

global procedure IupMglPlotDrawMark(Ihandle ih, atom x, y, z)
    c_proc(xIupMglPlotDrawMark, {ih, x, y, z})
end procedure

global procedure IupMglPlotDrawLine(Ihandle ih, atom x1, y1, z1, x2, y2, z2)
    c_proc(xIupMglPlotDrawLine, {ih, x1, y1, z1, x2, y2, z2})
end procedure

global procedure IupMglPlotDrawText(Ihandle ih, string text, atom x, y, z)
    c_proc(xIupMglPlotDrawText, {ih, text, x, y, z})
end procedure

global procedure IupMglPlotPaintTo(Ihandle ih, string fmt, integer w, h, atom dpi, object data)
    c_proc(xIupMglPlotPaintTo, {ih, fmt, w, h, dpi, data})
end procedure

--
-- OpenGL Canvas
--
atom 
    hIupGL = 0,
    xIupGLCanvas,
    xIupGLCanvasOpen,
    xIupGLMakeCurrent,
    xIupGLIsCurrent,
    xIupGLSwapBuffers,
    xIupGLPalette,
    xIupGLUseFont,
    xIupGLWait

procedure iup_init_iupgl()
    if hIupGL=0 then
        hIupGL = iup_open_dll({"iupgl.dll", 
                               "libiupgl.so",
                               "libiupgl.dylib"})

        xIupGLCanvas      = iup_c_func(hIupGL, "IupGLCanvas", {P},P)
        xIupGLCanvasOpen  = iup_c_proc(hIupGL, "IupGLCanvasOpen", {})
        xIupGLMakeCurrent = iup_c_proc(hIupGL, "IupGLMakeCurrent", {P})
        xIupGLIsCurrent   = iup_c_func(hIupGL, "IupGLIsCurrent", {P},I)
        xIupGLSwapBuffers = iup_c_proc(hIupGL, "IupGLSwapBuffers", {P})
        xIupGLPalette     = iup_c_proc(hIupGL, "IupGLPalette", {P,I,F,F,F})
        xIupGLUseFont     = iup_c_proc(hIupGL, "IupGLUseFont", {P,I,I,I})
        xIupGLWait        = iup_c_proc(hIupGL, "IupGLWait", {I})
    end if
end procedure

integer did_gl_open = 0

global procedure IupGLCanvasOpen()
    if did_gl_open=0 then
        did_gl_open = 1
        iup_init_iupgl()
        c_proc(xIupGLCanvasOpen, {})
    end if
end procedure

global function IupGLCanvas(object action=NULL, object func=NULL, sequence attributes="", dword_seq args={})
    IupGLCanvasOpen()
    Ihandle ih = c_func(xIupGLCanvas, {NULL})
    {action,func,attributes,args} = paranormalise(action,func,attributes,args)
    if func!=NULL then
        if action=NULL then
            action = "ACTION"
        end if
        IupSetCallback(ih, action, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global procedure IupGLMakeCurrent(Ihandle ih)
    iup_init_iupgl()
    c_proc(xIupGLMakeCurrent, {ih})
end procedure

global function IupGLIsCurrent(Ihandle ih)
    iup_init_iupgl()
    boolean res = c_func(xIupGLIsCurrent, {ih})
--  boolean res = (c_func(xIupGLIsCurrent, {ih})!=0)    -- (may be rqd, untried)
    return res
end function

global procedure IupGLSwapBuffers(Ihandle ih)
    iup_init_iupgl()
    c_proc(xIupGLSwapBuffers, {ih})
end procedure

global procedure IupGLPalette(Ihandle ih, integer index, atom r, atom g, atom b)
    iup_init_iupgl()
    c_proc(xIupGLPalette, { ih, index, r, g, b })
end procedure

global procedure IupGLUseFont(Ihandle ih, integer first, integer count, integer list_base)
    iup_init_iupgl()
    c_proc(xIupGLUseFont, { ih, first, count, list_base })
end procedure

global procedure IupGLWait(integer gl)
    iup_init_iupgl()
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
--  xIupImageLibOpen = iup_c_func(hImageLib, "IupImageLibOpen", {},I)
    xIupImageLibOpen = iup_c_proc(hImageLib, "IupImageLibOpen", {})

--****
-- === Routines

global procedure IupImageLibOpen()
    c_proc(xIupImageLibOpen, {})
end procedure
--*/

--ole.e:

--
-- OLE Control
--
atom hOle = 0,
     xIupOleControlOpen,
     xIupOleControl

procedure iup_init_ole()
    if hOle=0 then
        hOle = iup_open_dll({"iupole.dll",
                             "?libiupole.so",
                             "?libiupole.dylib"})

        xIupOleControlOpen  = iup_c_proc(hOle, "IupOleControlOpen", {})
        xIupOleControl      = iup_c_func(hOle, "IupOleControl", {P},P)
    end if
end procedure

integer did_ole_open = 0

procedure ole_open()
    did_ole_open = 1
    iup_init_ole()
    c_proc(xIupOleControlOpen, {})
end procedure

--DEV doc (done to here)
--global function control(string prog_id="", string attributes="", sequence args={})
global function IupOleControl(string prog_id="", string attributes="", sequence args={})
    if not did_ole_open then
        ole_open()
    end if

    Ihandle ih = c_func(xIupOleControl, {prog_id})

    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
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
    xIupTuioOpen    = iup_c_proc(hTuio, "IupTuioOpen", {}),
    xIupTuioClient  = iup_c_func(hTuio, "IupTuioClient", {I},P)

integer did_tuio_open = 0

procedure tuio_open()
    did_tuio_open = 1

    c_proc(xIupTuioOpen, {})
end procedure

global function client(integer port = 3333, sequence attributes={}, sequence args={})
    if not did_tuio_open then
        tuio_open()
    end if

    Ihandle ih = c_func(xIupTuioClient, { port })

    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if

    return ih
end function
--*/

 -- tee\iup.e
-------------
--atom iup = open_dll({`win32\iup.dll`, "libiup.so"})
--if iup=0 then ?9/0 end if
--atom iupimglib = open_dll({`win32\iupimglib.dll`, "libiupimglib.so"})
--if iupimglib=0 then ?9/0 end if

--global constant EXIT_SUCCESS = 0
--global constant EXIT_FAILURE = 1

--/*
-- allocate an image to memory
global function allocate_image(sequence data, integer cleanup=0)
    atom buff = allocate_data(length(data), cleanup)
    poke(buff, data) return buff
end function
--*/

--public include iupkey.e
/* from 32 to 126, all character sets are equal, the key code is the same as the ASCii character code. */
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
global constant K_sSP = iup_XkeyShift(' ')
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
global constant K_cSP = iup_XkeyCtrl(' ')
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
global constant K_mSP = iup_XkeyAlt(' ')
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
global constant K_ySP = iup_XkeySys(' ')
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

--global constant -- function delcarations
--      xIupOpen                          = iup_c_func(iup, "+IupOpen", {P,P}, I),
--      xIupClose                         = iup_c_proc(iup, "+IupClose", {}),
--      xIupImageLibOpen                  = iup_c_proc(iupimglib, "+IupImageLibOpen", {}),
--      xIupMainLoop                      = iup_c_func(iup, "+IupMainLoop", {}, I),
--      xIupLoopStep                      = iup_c_func(iup, "+IupLoopStep", {}, I),
--      xIupLoopStepWait                  = iup_c_func(iup, "+IupLoopStepWait", {}, I),
--      xIupMainLoopLevel                 = iup_c_func(iup, "+IupMainLoopLevel", {}, I),
--      xIupFlush                         = iup_c_proc(iup, "+IupFlush", {}),
--      xIupExitLoop                      = iup_c_proc(iup, "+IupExitLoop", {}),
--      xIupRecordInput                   = iup_c_func(iup, "+IupRecordInput", {P,I}, I),
--      xIupPlayInput                     = iup_c_func(iup, "+IupPlayInput", {P}, I),
--      xIupUpdate                        = iup_c_proc(iup, "+IupUpdate", {P}),
--      xIupUpdateChildren                = iup_c_proc(iup, "+IupUpdateChildren", {P}),
--      xIupRedraw                        = iup_c_proc(iup, "+IupRedraw", {P,I}),
--      xIupRefresh                       = iup_c_proc(iup, "+IupRefresh", {P}),
--      xIupRefreshChildren               = iup_c_proc(iup, "+IupRefreshChildren", {P}),
--      xIupHelp                          = iup_c_func(iup, "+IupHelp", {P}, I),
--      xIupVersion                       = iup_c_func(iup, "+IupVersion", {}, P),
--      xIupVersionDate                   = iup_c_func(iup, "+IupVersionDate", {}, P),
--      xIupVersionNumber                 = iup_c_func(iup, "+IupVersionNumber", {}, I),
--      xIupSetLanguage                   = iup_c_proc(iup, "+IupSetLanguage", {P}),
--      xIupGetLanguage                   = iup_c_func(iup, "+IupGetLanguage", {}, P),
--      xIupSetLanguageString             = iup_c_proc(iup, "+IupSetLanguageString", {P,P}),
--      xIupStoreLanguageString           = iup_c_proc(iup, "+IupStoreLanguageString", {P,P}),
--      xIupGetLanguageString             = iup_c_func(iup, "+IupGetLanguageString", {P}, P),
--      xIupSetLanguagePack               = iup_c_proc(iup, "+IupSetLanguagePack", {P}),
--      xIupDestroy                       = iup_c_proc(iup, "+IupDestroy", {P}),
--      xIupDetach                        = iup_c_proc(iup, "+IupDetach", {P}),
--      xIupAppend                        = iup_c_func(iup, "+IupAppend", {P,P}, P),
--      xIupInsert                        = iup_c_func(iup, "+IupInsert", {P,P,P}, P),
--      xIupGetChild                      = iup_c_func(iup, "+IupGetChild", {P,I}, P),
--      xIupGetChildPos                   = iup_c_func(iup, "+IupGetChildPos", {P,P}, I),
--      xIupGetChildCount                 = iup_c_func(iup, "+IupGetChildCount", {P}, I),
--      xIupGetNextChild                  = iup_c_func(iup, "+IupGetNextChild", {P,P}, P),
--      xIupGetBrother                    = iup_c_func(iup, "+IupGetBrother", {P}, P),
--      xIupGetParent                     = iup_c_func(iup, "+IupGetParent", {P}, P),
--      xIupGetDialog                     = iup_c_func(iup, "+IupGetDialog", {P}, P),
--      xIupGetDialogChild                = iup_c_func(iup, "+IupGetDialogChild", {P,P}, P),
--      xIupReparent                      = iup_c_func(iup, "+IupReparent", {P,P,P}, I),
--      xIupPopup                         = iup_c_func(iup, "+IupPopup", {P,I,I}, I),
--      xIupShow                          = iup_c_func(iup, "+IupShow", {P}, I),
--      xIupShowXY                        = iup_c_func(iup, "+IupShowXY", {P,I,I}, I),
--      xIupHide                          = iup_c_func(iup, "+IupHide", {P}, I),
--      xIupMap                           = iup_c_func(iup, "+IupMap", {P}, I),
--      xIupUnmap                         = iup_c_proc(iup, "+IupUnmap", {P}),
--      xIupResetAttribute                = iup_c_proc(iup, "+IupResetAttribute", {P,P}),
--      xIupGetAllAttributes              = iup_c_func(iup, "+IupGetAllAttributes", {P,P,I}, I),
--      xIupSetAttributes                 = iup_c_func(iup, "+IupSetAttributes", {P,P}, P),
--      xIupGetAttributes                 = iup_c_func(iup, "+IupGetAttributes", {P}, P),
--      xIupSetAttribute                  = iup_c_proc(iup, "+IupSetAttribute", {P,P,P}),
--      xIupSetStrAttribute               = iup_c_proc(iup, "+IupSetStrAttribute", {P,P,P}),
--      xIupSetInt                        = iup_c_proc(iup, "+IupSetInt", {P,P,I}),
--      xIupSetFloat                      = iup_c_proc(iup, "+IupSetFloat", {P,P,F}),
--      xIupSetDouble                     = iup_c_proc(iup, "+IupSetDouble", {P,P,D}),
--      xIupSetRGB                        = iup_c_proc(iup, "+IupSetRGB", {P,P,UC,UC,UC}),
--      xIupGetAttribute                  = iup_c_func(iup, "+IupGetAttribute", {P,P}, P),
--      xIupGetInt                        = iup_c_func(iup, "+IupGetInt", {P,P}, I),
--      xIupGetInt2                       = iup_c_func(iup, "+IupGetInt2", {P,P}, I),
--      xIupGetIntInt                     = iup_c_func(iup, "+IupGetIntInt", {P,P,P,P}, I),
--      xIupGetFloat                      = iup_c_func(iup, "+IupGetFloat", {P,P}, F),
--      xIupGetDouble                     = iup_c_func(iup, "+IupGetDouble", {P,P}, D),
--      xIupGetRGB                        = iup_c_proc(iup, "+IupGetRGB", {P,P,P,P,P}),
--      xIupSetAttributeId                = iup_c_proc(iup, "+IupSetAttributeId", {P,P,I,P}),
--      xIupSetStrAttributeId             = iup_c_proc(iup, "+IupSetStrAttributeId", {P,P,I,P}),
--      xIupSetIntId                      = iup_c_proc(iup, "+IupSetIntId", {P,P,I,I}),
--      xIupSetFloatId                    = iup_c_proc(iup, "+IupSetFloatId", {P,P,I,F}),
--      xIupSetDoubleId                   = iup_c_proc(iup, "+IupSetDoubleId", {P,P,I,D}),
--      xIupSetRGBId                      = iup_c_proc(iup, "+IupSetRGBId", {P,P,I,UC,UC,UC}),
--      xIupGetAttributeId                = iup_c_func(iup, "+IupGetAttributeId", {P,P,I}, P),
--      xIupGetIntId                      = iup_c_func(iup, "+IupGetIntId", {P,P,I}, I),
--      xIupGetFloatId                    = iup_c_func(iup, "+IupGetFloatId", {P,P,I}, F),
--      xIupGetDoubleId                   = iup_c_func(iup, "+IupGetDoubleId", {P,P,I}, D),
--      xIupGetRGBId                      = iup_c_proc(iup, "+IupGetRGBId", {P,P,I,P,P,P}),
--      xIupSetAttributeId2               = iup_c_proc(iup, "+IupSetAttributeId2", {P,P,I,I,P}),
--      xIupSetStrAttributeId2            = iup_c_proc(iup, "+IupSetStrAttributeId2", {P,P,I,I,P}),
--      xIupSetIntId2                     = iup_c_proc(iup, "+IupSetIntId2", {P,P,I,I,I}),
--      xIupSetFloatId2                   = iup_c_proc(iup, "+IupSetFloatId2", {P,P,I,I,F}),
--      xIupSetDoubleId2                  = iup_c_proc(iup, "+IupSetDoubleId2", {P,P,I,I,D}),
--      xIupSetRGBId2                     = iup_c_proc(iup, "+IupSetRGBId2", {P,P,I,I,UC,UC,UC}),
--      xIupGetAttributeId2               = iup_c_func(iup, "+IupGetAttributeId2", {P,P,I,I}, P),
--      xIupGetIntId2                     = iup_c_func(iup, "+IupGetIntId2", {P,P,I,I}, I),
--      xIupGetFloatId2                   = iup_c_func(iup, "+IupGetFloatId2", {P,P,I,I}, F),
--      xIupGetDoubleId2                  = iup_c_func(iup, "+IupGetDoubleId2", {P,P,I,I}, D),
--      xIupGetRGBId2                     = iup_c_proc(iup, "+IupGetRGBId2", {P,P,I,I,P,P,P}),
--      xIupSetGlobal                     = iup_c_proc(iup, "+IupSetGlobal", {P,P}),
--      xIupSetStrGlobal                  = iup_c_proc(iup, "+IupSetStrGlobal", {P,P}),
--      xIupGetGlobal                     = iup_c_func(iup, "+IupGetGlobal", {P}, P),
--      xIupSetFocus                      = iup_c_func(iup, "+IupSetFocus", {P}, P),
--      xIupGetFocus                      = iup_c_func(iup, "+IupGetFocus", {}, P),
--      xIupPreviousField                 = iup_c_func(iup, "+IupPreviousField", {P}, P),
--      xIupNextField                     = iup_c_func(iup, "+IupNextField", {P}, P),
--      xIupGetCallback                   = iup_c_func(iup, "+IupGetCallback", {P,P}, P),
--      xIupSetCallback                   = iup_c_func(iup, "+IupSetCallback", {P,P,P}, P),
--      xIupGetFunction                   = iup_c_func(iup, "+IupGetFunction", {P}, P),
--      xIupSetFunction                   = iup_c_func(iup, "+IupSetFunction", {P,P}, P),
--      xIupGetHandle                     = iup_c_func(iup, "+IupGetHandle", {P}, P),
--      xIupSetHandle                     = iup_c_proc(iup, "+IupSetHandle", {P,P}),
--      xIupGetAllNames                   = iup_c_func(iup, "+IupGetAllNames", {P,I}, I),
--      xIupGetAllDialogs                 = iup_c_func(iup, "+IupGetAllDialogs", {P,I}, I),
--      xIupGetName                       = iup_c_func(iup, "+IupGetName", {P}, P),
--      xIupSetAttributeHandle            = iup_c_proc(iup, "+IupSetAttributeHandle", {P,P,P}),
--      xIupGetAttributeHandle            = iup_c_func(iup, "+IupGetAttributeHandle", {P,P}, P),
--      xIupGetClassName                  = iup_c_func(iup, "+IupGetClassName", {P}, P),
--      xIupGetClassType                  = iup_c_func(iup, "+IupGetClassType", {P}, P),
--      xIupGetAllClasses                 = iup_c_func(iup, "+IupGetAllClasses", {P,I}, I),
--      xIupGetClassAttributes            = iup_c_func(iup, "+IupGetClassAttributes", {P,P,I}, I),
--      xIupGetClassCallbacks             = iup_c_func(iup, "+IupGetClassCallbacks", {P,P,I}, I),
--      xIupSaveClassAttributes           = iup_c_proc(iup, "+IupSaveClassAttributes", {P}),
--      xIupCopyClassAttributes           = iup_c_proc(iup, "+IupCopyClassAttributes", {P,P}),
--      xIupSetClassDfltAttribute         = iup_c_proc(iup, "+IupSetClassDefaultAttribute", {P,P,P}),
--      xIupClassMatch                    = iup_c_func(iup, "+IupClassMatch", {P,P}, I),
--      xIupCreate                        = iup_c_func(iup, "+IupCreatev", {P,P}, P),
--      xIupFill                          = iup_c_func(iup, "+IupFill", {}, P),
--      xIupRadio                         = iup_c_func(iup, "+IupRadio", {P}, P),
--      xIupVbox                          = iup_c_func(iup, "+IupVboxv", {P}, P),
--      xIupZbox                          = iup_c_func(iup, "+IupZboxv", {P}, P),
--      xIupHbox                          = iup_c_func(iup, "+IupHboxv", {P}, P),
--      xIupNormalizer                    = iup_c_func(iup, "+IupNormalizerv", {P}, P),
--      xIupCboxv                         = iup_c_func(iup, "+IupCboxv", {P}, P),
--      xIupSbox                          = iup_c_func(iup, "+IupSbox", {P}, P),
--      xIupSplit                         = iup_c_func(iup, "+IupSplit", {P,P}, P),
--      xIupScrollBox                     = iup_c_func(iup, "+IupScrollBox", {P}, P),
--      xIupGridBox                       = iup_c_func(iup, "+IupGridBoxv", {P}, P),
--      xIupExpander                      = iup_c_func(iup, "+IupExpander", {P}, P),
--      xIupDetachBox                     = iup_c_func(iup, "+IupDetachBox", {P}, P),
--      xIupBackgroundBox                 = iup_c_func(iup, "+IupBackgroundBox", {P}, P),
--X     xIupFrame                         = iup_c_func(iup, "+IupFrame", {P}, P),
--      xIupImage                         = iup_c_func(iup, "+IupImage", {I,I,P}, P),
--      xIupImageRGB                      = iup_c_func(iup, "+IupImageRGB", {I,I,P}, P),
--      xIupImageRGBA                     = iup_c_func(iup, "+IupImageRGBA", {I,I,P}, P),
--      xIupItem                          = iup_c_func(iup, "+IupItem", {P,P}, P),
--      xIupSubmenu                       = iup_c_func(iup, "+IupSubmenu", {P,P}, P),
--      xIupSeparator                     = iup_c_func(iup, "+IupSeparator", {}, P),
--      xIupMenu                          = iup_c_func(iup, "+IupMenuv", {P}, P),
--      xIupButton                        = iup_c_func(iup, "+IupButton", {P,P}, P),
--      xIupCanvas                        = iup_c_func(iup, "+IupCanvas", {P}, P),
--      xIupDialog                        = iup_c_func(iup, "+IupDialog", {P}, P),
--      xIupUser                          = iup_c_func(iup, "+IupUser", {}, P),
--      xIupLabel                         = iup_c_func(iup, "+IupLabel", {P}, P),
--      xIupList                          = iup_c_func(iup, "+IupList", {P}, P),
--      xIupText                          = iup_c_func(iup, "+IupText", {P}, P),
--      xIupMultiLine                     = iup_c_func(iup, "+IupMultiLine", {P}, P),
--      xIupToggle                        = iup_c_func(iup, "+IupToggle", {P,P}, P),
--      xIupTimer                         = iup_c_func(iup, "+IupTimer", {}, P),
--      xIupClipboard                     = iup_c_func(iup, "+IupClipboard", {}, P),
--      xIupProgressBar                   = iup_c_func(iup, "+IupProgressBar", {}, P),
--      xIupVal                           = iup_c_func(iup, "+IupVal", {P}, P),
--      xIupTabs                          = iup_c_func(iup, "+IupTabsv", {P}, P),
--      xIupTree                          = iup_c_func(iup, "+IupTree", {}, P),
--      xIupLink                          = iup_c_func(iup, "+IupLink", {P,P}, P),
--x     xIupFlatButton                    = iup_c_func(iup, "+IupFlatButton", {P}, P),
--x     xIupSpin                          = iup_c_func(iup, "+IupSpin", {}, P),
--      xIupSpinbox                       = iup_c_func(iup, "+IupSpinbox", {P}, P),
--      xIupSaveImageAsText               = iup_c_func(iup, "+IupSaveImageAsText", {P,P,P,P}, I),
--      xIupTextConvertLinColToPos        = iup_c_proc(iup, "+IupTextConvertLinColToPos", {P,I,I,P}),
--      xIupTextConvertPosToLinCol        = iup_c_proc(iup, "+IupTextConvertPosToLinCol", {P,I,P,P}),
--      xIupConvertXYToPos                = iup_c_func(iup, "+IupConvertXYToPos", {P,I,I}, I),
--      xIupStoreGlobal                   = iup_c_proc(iup, "+IupStoreGlobal", {P,P}),
--      xIupStoreAttribute                = iup_c_proc(iup, "+IupStoreAttribute", {P,P,P}),
--      xIupStoreAttributeId              = iup_c_proc(iup, "+IupStoreAttributeId", {P,P,I,P}),
--      xIupStoreAttributeId2             = iup_c_proc(iup, "+IupStoreAttributeId2", {P,P,I,I,P}),
--      xIupTreeSetUserId                 = iup_c_func(iup, "+IupTreeSetUserId", {P,I,P}, I),
--      xIupTreeGetUserId                 = iup_c_func(iup, "+IupTreeGetUserId", {P,I}, P),
--      xIupTreeGetId                     = iup_c_func(iup, "+IupTreeGetId", {P,P}, I),
--      xIupTreeSetAttributeHandle        = iup_c_proc(iup, "+IupTreeSetAttributeHandle", {P,P,I,P}),
--      xIupTreeSetAttribute              = iup_c_proc(iup, "+IupTreeSetAttribute", {P,P,I,P}),
--      xIupTreeStoreAttribute            = iup_c_proc(iup, "+IupTreeStoreAttribute", {P,P,I,P}),
--      xIupTreeGetAttribute              = iup_c_func(iup, "+IupTreeGetAttribute", {P,P,I}, P),
--      xIupTreeGetInt                    = iup_c_func(iup, "+IupTreeGetInt", {P,P,I}, I),
--      xIupTreeGetFloat                  = iup_c_func(iup, "+IupTreeGetFloat", {P,P,I}, F),
--      xIupGetActionName                 = iup_c_func(iup, "+IupGetActionName", {}, P),
--      xIupMapFont                       = iup_c_func(iup, "+IupMapFont", {P}, P),
--      xIupUnMapFont                     = iup_c_func(iup, "+IupUnMapFont", {P}, P),
--      xIupFileDlg                       = iup_c_func(iup, "+IupFileDlg", {}, P),
--      xIupMessageDlg                    = iup_c_func(iup, "+IupMessageDlg", {}, P),
--      xIupFontDlg                       = iup_c_func(iup, "+IupFontDlg", {}, P),
--      xIupGetFile                       = iup_c_func(iup, "+IupGetFile", {P}, I),
--      xIupMessage                       = iup_c_proc(iup, "+IupMessage", {P,P}),
--      xIupAlarm                         = iup_c_func(iup, "+IupAlarm", {P,P,P,P,P}, I),
--      xIupListDialog                    = iup_c_func(iup, "+IupListDialog", {I,P,I,P,I,I,I,P}, I),
--      xIupGetText                       = iup_c_func(iup, "+IupGetText", {P,P}, I),
--      xIupGetColor                      = iup_c_func(iup, "+IupGetColor", {I,I,P,P,P}, I),
--      xIupGetParam                      = iup_c_func(iup, "+IupGetParamv", {P,P,P,P,I,I,P}, I),
--      xIupParamf                        = iup_c_func(iup, "+IupParamf", {P}, P),
--      xIupParamBox                      = iup_c_func(iup, "+IupParamBox", {P,P,I}, P),
--      xIupLayoutDialog                  = iup_c_func(iup, "+IupLayoutDialog", {P}, P),
--$

--if xIupSetCallback=0 then ?9/0 end if

global constant IUP_NAME = "IUP - Portable User Interface"
global constant IUP_DESCRIPTION = "Multi-platform Toolkit for Building Graphical User Interfaces"
global constant IUP_COPYRIGHT = "Copyright (C) 1994-2015 Tecgraf/PUC-Rio"
--Use IupVersion() etc instead:
--global constant IUP_VERSION = "3.16" /* bug fixes are reported only by IupVersion functions */
--global constant IUP_VERSION_NUMBER = 316000
--global constant IUP_VERSION_DATE = "2015/09/15" /* does not include bug fix releases */

----void IupSetLanguageString(const char* name, const char* str);
--global procedure IupSetLanguageString(string name, string str)
--  c_proc(xIupSetLanguageString, {name,str})
--end procedure
--
----void IupStoreLanguageString(const char* name, const char* str);
--global procedure IupStoreLanguageString(string name, string str)
--  c_proc(xIupStoreLanguageString, {name,str})
--end procedure
--
----char* IupGetLanguageString(const char* name);
--global function IupGetLanguageString(string name)
--  atom ptr = c_func(xIupGetLanguageString, {name})
--  sequence str = ""
--  if ptr!=NULL then str = peek_string(ptr) end if
--  return str
--end function
--
----void IupSetLanguagePack(Ihandle* ih);
--global procedure IupSetLanguagePack(atom ih)
--  c_proc(xIupSetLanguagePack, {ih})
--end procedure

----Icallback IupGetFunction(const char *name);
--global function IupGetFunction(string name)
global function IupGetGlobalFunction(string name)
    cbfunc result = c_func(xIupGetFunction, {name})
    return result
end function
--
----Icallback IupSetFunction(const char *name, Icallback func);
--global function IupSetGlobalFunction(string name, cbfunc func)
global procedure IupSetGlobalFunction(string name, cbfunc func)
    cbfunc prev = c_func(xIupSetFunction, {name,func})
--  return prev
--end function
end procedure

global function IupClassMatch(Ihandle ih, string classname)
    boolean result = c_func(xIupClassMatch, {ih,classname})
    return result   -- true(1) or false(0)
end function

/************************************************************************/
/*                        Elements                                      */
/************************************************************************/

global function IupScrollBox(Ihandln child=NULL, string attributes="", sequence args={})
    Ihandle ih = c_func(xIupScrollBox, {child})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupFlatScrollBox(Ihandln child=NULL, string attributes="", sequence args={})
    Ihandle ih = c_func(xIupFlatScrollBox, {child})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

--/*
-- The original proof-of-concept for IupFlowBox(), and full tests, preserved for posterity:
function reflow(sequence s, integer w)
    integer tgt = 1,
            rw = w
    for i=1 to length(s) do
        integer j = 1
        while j<=length(s[i]) do
            --?{i,j,rw,tgt,s}
            rw -= 10    -- (IupGetInt(?,"SIZE"))
            if rw<0 then
                tgt += 1
                if i=tgt-1 then
                    -- spill as rqd,
                    j = max(2,j) -- but keep at least 1
                    -- eg {{1,2,3,4,5,6},{}}
                    -- ==> {{1,2,3,4,5},{6}}
                    -- ==> {{1,2,3,4},{5,6}}
                    for k=length(s[i]) to j by -1 do
                        s[tgt] = prepend(s[tgt],s[i][k])
                        s[i] = s[i][1..k-1] -- trim right
                    end for
--                  -- (aside: perfectly valid, but pointless:)
--                  rw = w
--                  exit
                else
                    if j!=1 then ?9/0 end if -- (sanity [as below])
                end if
                rw = w
                -- (whereas an exit here is wrong for non-spill)
                -- [you wouldn't believe how many times I tried]
            elsif i>tgt then
                -- shuffle up as rqd
                -- eg {{1},{2},{3}} ==> {{1,2},{},{3}}
                -- or {{1,2},{},{3}} ==> {{1,2,3},{},{}}
                -- or {{1,2},{},{3}} ==> {{1,2},{3},{}}
                --  (just do them one at a time, here)
                if j!=1 then ?9/0 end if -- (sanity)
                -- ("" ie there should never be anything
                --  betwixt s[tgt][$] and s[i][j] here,
                --  that we haven't already moved up)
                s[tgt] = append(s[tgt],s[i][1])
                s[i] = s[i][2..$]   -- trim left
                -- (j stays the same, although now it
                --  actually refers to the next item)
            else
                j += 1 -- (already in place, move on)
            end if
        end while
    end for
    return s
end function

sequence s = columnize({tagset(10)}), s0 = s
?s
----trace(1)
--s = reflow(s,40)
--?s
--s = reflow(s,100)
--?s
------trace(1)
s = reflow(s,1)
?s
sequence r = repeat(0,10)
for i=1 to 10 do
    r[i] = reflow(s,i*10)
end for
pp(r,{pp_Nest,1})
for i=1 to 10 do
    for j=1 to 10 do
        if r[j]!=reflow(r[i],j*10) then ?9/0 end if
    end for
    if reflow(r[i],1)!=s0 then ?9/0 end if
end for
?"done"
{} = wait_key()
abort(0)
--*/

--function flow(Ihandle vbox, Ihandles hbox, integer w)
function flow(Ihandles hbox, integer w)
--?{"flow",vbox, hbox, w}
    integer tgt = 1,    -- fill/flow hbox
            rw = w,     -- remaining width
            err
    Ihandle ih, new_parent  -- (scratch vars)
    Ihandln ref_child       -- (     ""     )
    sequence s = repeat({},length(hbox))
    for i=1 to length(s) do
        for pos=0 to IupGetChildCount(hbox[i])-1 do
            s[i] &= IupGetChild(hbox[i],pos)
        end for
    end for
    bool bMoved = false
    for i=1 to length(s) do
        integer j = 1
        while j<=length(s[i]) do
            rw -= IupGetInt(s[i][j],"RASTERSIZE")
            if rw<0 then
                tgt += 1
                if i=tgt-1 then
                    -- spill as rqd,
                    j = max(2,j) -- but keep at least 1
                    -- eg {{1,2,3,4,5,6},{}}
                    -- ==> {{1,2,3,4,5},{6}}
                    -- ==> {{1,2,3,4},{5,6}}
                    for k=length(s[i]) to j by -1 do
                        ih = s[i][k]
                        new_parent = hbox[tgt]
                        ref_child = iff(length(s[tgt])?s[tgt][1]:NULL)
                        err = IupReparent(ih, new_parent, ref_child) 
                        if err!=IUP_NOERROR then ?9/0 end if
                        bMoved = true
                        s[tgt] = prepend(s[tgt],s[i][k])
                        s[i] = s[i][1..k-1] -- trim right
                    end for
--                  -- (aside: perfectly valid, but pointless:)
--                  rw = w
--                  exit
                else
                    if j!=1 then ?9/0 end if -- (sanity [as below])
                end if
                rw = w
                -- (whereas an exit here is wrong for non-spill)
                -- [you wouldn't believe how many times I tried]
            elsif i>tgt then
                -- shuffle up as rqd
                -- eg {{1},{2},{3}} ==> {{1,2},{},{3}}
                -- or {{1,2},{},{3}} ==> {{1,2,3},{},{}}
                -- or {{1,2},{},{3}} ==> {{1,2},{3},{}}
                --  (just do them one at a time, here)
                if j!=1 then ?9/0 end if -- (sanity)
                -- ("" ie there should never be anything
                --  betwixt s[tgt][$] and s[i][j] here,
                --  that we haven't already moved up)
                ih = s[i][1]
                new_parent = hbox[tgt]
                ref_child = NULL
                err = IupReparent(ih, new_parent, ref_child) 
                if err!=IUP_NOERROR then ?9/0 end if
                bMoved = true
                s[tgt] = append(s[tgt],s[i][1])
                s[i] = s[i][2..$]   -- trim left
                -- (j stays the same, although now it
                --  actually refers to the next item)
            else
                j += 1 -- (already in place, move on)
            end if
        end while
    end for
    return bMoved
end function

function find_flow(Ihandle ih, integer width)
    integer count = IupGetChildCount(ih)
    bool bFlow = find("FLOWBOX",IupGetAllAttributes(ih)) and 
                 IupGetAttribute(ih,"FLOWBOX")="FlowBox"
--  integer w = IupGetInt(ih,"CLIENTSIZE")  -- (maybe??)
    sequence children = {}
    bool bMoved = false
    for pos=0 to count-1 do
        Ihandle child = IupGetChild(ih,pos)
        if bFlow then
            children &= child
        end if
        bMoved = find_flow(child,width)
--      bMoved = find_flow(child,iff(bFlow?width:w)) -- (untested...)
    end for
    if bFlow then
--      bMoved = flow(ih,children,width)
        bMoved = flow(children,width)
    end if
    return bMoved
end function

global function IupResizeFlow_cb(Ihandle dlg, integer width, /*height*/)
--?{"IupResizeFlow_cb", dlg, width}
    if find_flow(dlg,width) then
        IupRefresh(dlg)
        {} = find_flow(dlg,width)
        IupRefresh(dlg)
    end if
    return IUP_DEFAULT
end function

global procedure IupSetResizeFlowCallback(Ihandle dlg)
    IupSetCallback(dlg,"RESIZE_CB",Icallback("IupResizeFlow_cb"))
end procedure

global procedure IupResizeFlow(Ihandle dlg)
    IupSetResizeFlowCallback(dlg)
end procedure

global function IupFlowBox(Ihandles children, string attributes="", sequence args={})
    for i=1 to length(children) do
        -- Put each child in it's own IupHbox. After the first flow(), 
        -- many may end up empty, but we never have to create any more.
        children[i] = IupHbox(children[i])
    end for
    Ihandle ih = IupVbox(children)
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    IupSetAttribute(ih,"FLOWBOX","FlowBox") -- and properly label it.
    return ih
end function

global function IupExpander(Ihandln child=NULL, string attributes="", sequence args={})
    Ihandle ih = c_func(xIupExpander, {child})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupDetachBox(Ihandln child=NULL, object action=NULL, object func=NULL, sequence attributes="", dword_seq args={})
    Ihandle ih = c_func(xIupDetachBox, {child})
    {action,func,attributes,args} = paranormalise(action,func,attributes,args)
    if func!=NULL then
        if action=NULL then
--          action = "ACTION"
            action = "DETACHED_CB"
        end if
        IupSetCallback(ih, action, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupBackgroundBox(Ihandln child=NULL, string attributes="", sequence args={})
    Ihandle ih = c_func(xIupBackgroundBox, {child})
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupLink(nullable_string url=NULL, nullable_string title=NULL, object action=NULL, object func=NULL, sequence attributes="", dword_seq args={})
    Ihandle ih = c_func(xIupLink, {url,title})
    {action,func,attributes,args} = paranormalise(action,func,attributes,args)
    if func!=NULL then
        if action=NULL then
            action = "ACTION"
        end if
        IupSetCallback(ih, action, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupFlatButton(nullable_string title=NULL, object action=NULL, object func=NULL, sequence attributes="", dword_seq args={})
    Ihandle ih = c_func(xIupFlatButton, {title})
    {action,func,attributes,args} = paranormalise(action,func,attributes,args)
    if func!=NULL then
        if action="ACTION" then ?9/0 end if -- see docs!
        if action=NULL then
            action = "FLAT_ACTION"
        end if
        IupSetCallback(ih, action, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

global function IupDropButton(Ihandln child=NULL, object action=NULL, object func=NULL, sequence attributes="", dword_seq args={})
    Ihandle ih = c_func(xIupDropButton, {child})
    {action,func,attributes,args} = paranormalise(action,func,attributes,args)
    if func!=NULL then
        if action=NULL then
            action = "ACTION"
        end if
        IupSetCallback(ih, action, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, args)
    end if
    return ih
end function

/************************************************************************/
/*                      Utilities                                       */
/************************************************************************/

--void IupStoreAttributeId(Ihandle *ih, const char* name, int id, const char *v);
global procedure IupStoreAttributeId(Ihandle ih, string name, integer id, nullable_string v=NULL)
    c_proc(xIupStoreAttributeId, {ih,name,id,v})
end procedure

--void IupStoreAttributeId2(Ihandle* ih, const char* name, int lin, int col, const char* v);
global procedure IupStoreAttributeId2(Ihandle ih, string name, atom lin, atom col, nullable_string v=NULL)
    c_proc(xIupStoreAttributeId2, {ih,name,lin,col,v})
end procedure

/* IupTree utilities */
--int IupTreeSetUserId(Ihandle* ih, int id, void* userid);
global procedure IupTreeSetUserId(Ihandle ih, integer id, atom userid)
    atom result = c_func(xIupTreeSetUserId, {ih,id,userid})
--  if result=0 then ?9/0 end if
    if result=0 then ?"9/0 pGUI.e line 8322" end if
end procedure

--void* IupTreeGetUserId(Ihandle* ih, int id);
global function IupTreeGetUserId(Ihandle ih, integer id)
atom result = c_func(xIupTreeGetUserId, {ih,id})
    return result
end function

--int IupTreeGetId(Ihandle* ih, void *userid);
global function IupTreeGetId(Ihandle ih, atom userid)
atom result = c_func(xIupTreeGetId, {ih,userid})
    return result
end function

--void IupTreeSetAttributeHandle(Ihandle* ih, const char* name, int id, Ihandle* ih_named);
--global procedure IupTreeSetAttributeHandle(Ihandle ih, string name, integer id, Ihandle ih_named)
--  c_proc(xIupTreeSetAttributeHandle, {ih,name,id,ih_named})
--end procedure

/* DEPRECATED IupTree utilities, use Iup*AttributeId functions. It will be removed in a future version.  */
--void IupTreeSetAttribute(Ihandle* ih, const char* name, int id, const char* v);
--global procedure IupTreeSetAttribute(Ihandle ih, string name, integer id, nullable_string v=NULL)
--  c_proc(xIupTreeSetAttribute, {ih,name,id,v})
--end procedure

--void IupTreeStoreAttribute(Ihandle* ih, const char* name, int id, const char* v);
--global procedure IupTreeStoreAttribute(Ihandle ih, string name, integer id, nullable_string v=NULL)
--  c_proc(xIupTreeStoreAttribute, {ih,name,id,v})
--end procedure

--char* IupTreeGetAttribute(Ihandle* ih, const char* name, int id);
--global function IupTreeGetAttribute(Ihandle ih, string name, integer id)
--  atom ptr = c_func(xIupTreeGetAttribute, {ih,name,id})
--  string str = ""
--  if ptr!=NULL then str = peek_string(ptr) end if
--  return str
--end function

--int IupTreeGetInt(Ihandle* ih, const char* name, int id);
--global function IupTreeGetInt(Ihandle ih, string name, integer id)
--  atom result = c_func(xIupTreeGetInt, {ih,name,id})
--  return result
--end function

--float IupTreeGetFloat(Ihandle* ih, const char* name, int id);
--global function IupTreeGetFloat(Ihandle ih, string name, integer id)
--  atom result = c_func(xIupTreeGetFloat, {ih,name,id})
--  return result
--end function

procedure iupTreeSetNodeAttributes(Ihandle tree, integer id, sequence attrs)
-- (internal routine)
    for i=1 to length(attrs) by 2 do
        string name = attrs[i]
        if not find(name,{"COLOR","STATE","TITLE","TITLEFONT","TOGGLEVALUE","TOGGLEVISIBLE",
                          "USERDATA","IMAGE","IMAGEEXPANDED","MARKED"}) then
            ?9/0
        end if
        object v = attrs[i+1]
        if string(v) then
            IupSetAttributeId(tree, name, id, v)
        elsif name="USERDATA" and integer(v) then
            IupTreeSetUserId(tree, id, v)
        else
            ?9/0
        end if
--?{"IupSetAttributeId",name, id, v}
    end for
end procedure

--procedure iupSetTreeNodeAttribute(Ihandle tree, string addlb, integer id, string desc)
---- (internal routine, purely for debugging purposes)
---- addlb should be "ADDLEAF" or "ADDBRANCH"
--  IupSetAttributeId(tree, addlb, id, desc)
--  ?{"IupSetAttributeId",addlb, id, desc}
--end procedure

function iupTreeAddNodesRec(Ihandle tree, sequence tree_nodes, integer id)
-- internal routine, the guts of IupTreeAddNodes, less the initial clear
string desc
integer next
    if string(tree_nodes) then
        -- leaf (no attributes)
        desc = tree_nodes
        IupSetAttributeId(tree, "ADDLEAF", id, desc)
--      iupSetTreeNodeAttribute(tree, "ADDLEAF", id, desc)
        next = id+1
    else
        sequence children = {}
        desc = tree_nodes[1]
        integer l = min(length(tree_nodes),3)
        if l=1 or atom(tree_nodes[l]) then
            -- also leaf (may have attributes)
            IupSetAttributeId(tree, "ADDLEAF", id, desc)
--          iupSetTreeNodeAttribute(tree, "ADDLEAF", id, desc)
        else -- (length 2 or 3)
            -- branch
            IupSetAttributeId(tree, "ADDBRANCH", id, desc)
--          iupSetTreeNodeAttribute(tree, "ADDBRANCH", id, desc)
            children = tree_nodes[l]
        end if
        id += 1
        next = id
        for i=length(children) to 1 by -1 do
            next = iupTreeAddNodesRec(tree, children[i], id)
        end for
        if l=3 then
            iupTreeSetNodeAttributes(tree, id, tree_nodes[2])
        end if
    end if
    return next
end function

global procedure IupTreeAddNodes(Ihandle tree, sequence tree_nodes, integer id=-1)
    IupSetInt(tree,"AUTOREDRAW",false)
    if id=-1 then
        IupSetAttributeId(tree,"DELNODE",0,"ALL")
        {} = iupTreeAddNodesRec(tree, tree_nodes, id)
    else
        -- tree_nodes is actually just children
        sequence children = tree_nodes
        id = IupTreeGetId(tree, id)
        if id=-1 then ?9/0 end if
        for i=length(children) to 1 by -1 do
            {} = iupTreeAddNodesRec(tree, children[i], id)
        end for
    end if
    IupSetInt(tree,"AUTOREDRAW",true)
end procedure

global function IupTreeView(sequence tree_nodes, atom branchopen_cb=NULL, string attributes="", sequence args={})
--
-- Creates an IupTree from a recursive [callback] data structure.
--
-- tree_nodes is as per IupTreeAddNodes()
-- branchopen_cb can be used for deferred loading, or NULL if tree_nodes is complete.
--
    Ihandle tree = IupFlatTree(attributes, args)
    if branchopen_cb!=NULL then
        IupSetCallback(tree, "BRANCHOPEN_CB",  branchopen_cb);
    end if
    IupTreeAddNodes(tree, tree_nodes)
    return tree
end function

--sug: IupDestroyTreeView(Ihandle tree)
--  -- use in place of IupDestroy() to ensure proper cleanup....


/* DEPRECATED font names. It will be removed in a future version.  */
--char* IupMapFont(const char *iupfont);
--global function IupMapFont(nullable_string iupfont=NULL)
--  atom ptr = c_func(xIupMapFont, {iupfont})
--  string str = ""
--  if ptr!=NULL then str = peek_string(ptr) end if
--  return str
--end function

--char* IupUnMapFont(const char *driverfont);
--global function IupUnMapFont(nullable_string driverfont=NULL)
--  atom ptr = c_func(xIupUnMapFont, {driverfont})
--  string str = ""
--  if ptr!=NULL then str = peek_string(ptr) end if
--  return str
--end function

/************************************************************************/
/*                      Pre-defined dialogs                           */
/************************************************************************/
--Ihandle* IupProgressDlg(void);
-- Creates a progress dialog element. It is a predefined dialog for displaying
-- the progress of an operation.
-- The dialog is meant to be shown with the show functions IupShow or IupShowXY.
--
-- Returns: the identifier of the created element, or NULL if an error occurs.
global function IupProgressDlg()
    Ihandle ih = c_func(xIupProgressDlg, {})
    return ih
end function

--Ihandle* IupParamf(const char* format);
--global function IupParamf(nullable_string fmt=NULL)
--  Ihandle ih = c_func(xIupParamf, {fmt})
--  return ih
--end function

--Ihandle* IupParamBox(Ihandle* parent, Ihandle** params, int count);
--global function IupParamBox(Ihandle parent, atom params, integer count)
--  Ihandle ih = c_func(xIupParamBox, {parent,params,count})
--  return ih
--end function

global function IupElementPropertiesDialog(Ihandln parent, Ihandle elem)
    Ihandle ih = c_func(xIupElementPropertiesDialog, {parent, elem})
    return ih
end function

global function IupClassInfoDialog(Ihandln parent)
    Ihandle ih = c_func(xIupClassInfoDialog, {parent})
    return ih
end function

/************************************************************************/
/*               SHOW_CB Callback Values                                */
/************************************************************************/
global enum
        IUP_SHOW = 0,
        IUP_RESTORE,
        IUP_MINIMIZE,
        IUP_MAXIMIZE,
        IUP_HIDE,
$
/************************************************************************/
/*               SCROLL_CB Callback Values                              */
/************************************************************************/
global enum
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
/*                      Pre-Defined Masks                               */
/************************************************************************/
global constant IUP_MASK_FLOAT = "[+/-]?(/d+/.?/d*|/./d+)"
global constant IUP_MASK_UFLOAT = "(/d+/.?/d*|/./d+)"
global constant IUP_MASK_EFLOAT = "[+/-]?(/d+/.?/d*|/./d+)([eE][+/-]?/d+)?"
global constant IUP_MASK_FLOATCOMMA = "[+/-]?(/d+/,?/d*|/,/d+)"
global constant IUP_MASK_UFLOATCOMMA = "(/d+/,?/d*|/,/d+)"
global constant IUP_MASK_INT = "[+/-]?/d+"
--global constant IUP_MASK_UINT = "/d+"
/* Old definitions for backward compatibility */
global constant IUPMASK_FLOAT = IUP_MASK_FLOAT
global constant IUPMASK_UFLOAT = IUP_MASK_UFLOAT
global constant IUPMASK_EFLOAT = IUP_MASK_EFLOAT
global constant IUPMASK_INT = IUP_MASK_INT
global constant IUPMASK_UINT = IUP_MASK_UINT
/************************************************************************/
--/*                     IupGetParam Callback situations                    */
--/************************************************************************/
--global constant IUP_GETPARAM_BUTTON1 = -1
--global constant IUP_GETPARAM_INIT = -2
--global constant IUP_GETPARAM_BUTTON2 = -3
--global constant IUP_GETPARAM_BUTTON3 = -4
--global constant IUP_GETPARAM_CLOSE = -5
--global constant IUP_GETPARAM_OK = IUP_GETPARAM_BUTTON1
--global constant IUP_GETPARAM_CANCEL = IUP_GETPARAM_BUTTON2
--global constant IUP_GETPARAM_HELP = IUP_GETPARAM_BUTTON3
/************************************************************************/
/*              Replacement for the WinMain in Windows,                 */
/*        this allows the application to start from "main".             */
/*        Used only for Watcom.                                         */
/************************************************************************/

--global constant -- function delcarations
--  xIupConfig                        = iup_c_func(iup, "+IupConfig", {}, P),
--  xIupConfigLoad                    = iup_c_func(iup, "+IupConfigLoad", {P}, I),
--  xIupConfigSave                    = iup_c_func(iup, "+IupConfigSave", {P}, I),
--  xIupConfigSetVariableStr          = iup_c_proc(iup, "+IupConfigSetVariableStr", {P,P,P,P}),
--  xIupConfigSetVariableStrId        = iup_c_proc(iup, "+IupConfigSetVariableStrId", {P,P,P,I,P}),
--  xIupConfigSetVariableInt          = iup_c_proc(iup, "+IupConfigSetVariableInt", {P,P,P,I}),
--  xIupConfigSetVariableIntId        = iup_c_proc(iup, "+IupConfigSetVariableIntId", {P,P,P,I,I}),
--  xIupConfigSetVariableDouble       = iup_c_proc(iup, "+IupConfigSetVariableDouble", {P,P,P,D}),
--  xIupConfigSetVariableDoubleId     = iup_c_proc(iup, "+IupConfigSetVariableDoubleId", {P,P,P,I,D}),
--  xIupConfigGetVariableStr          = iup_c_func(iup, "+IupConfigGetVariableStr", {P,P,P}, P),
--  xIupConfigGetVariableStrId        = iup_c_func(iup, "+IupConfigGetVariableStrId", {P,P,P,I}, P),
--  xIupConfigGetVariableInt          = iup_c_func(iup, "+IupConfigGetVariableInt", {P,P,P}, I),
--  xIupConfigGetVariableIntId        = iup_c_func(iup, "+IupConfigGetVariableIntId", {P,P,P,I}, I),
--  xIupConfigGetVariableDouble       = iup_c_func(iup, "+IupConfigGetVariableDouble", {P,P,P}, D),
--  xIupConfigGetVariableDoubleId     = iup_c_func(iup, "+IupConfigGetVariableDoubleId", {P,P,P,I}, D),
--  xIupConfigGetVariableStrDef       = iup_c_func(iup, "+IupConfigGetVariableStrDef", {P,P,P,P}, P),
--  xIupConfigGetVariableStrIdDef     = iup_c_func(iup, "+IupConfigGetVariableStrIdDef", {P,P,P,I,P}, P),
--  xIupConfigGetVariableIntDef       = iup_c_func(iup, "+IupConfigGetVariableIntDef", {P,P,P,I}, I),
--  xIupConfigGetVariableIntIdDef     = iup_c_func(iup, "+IupConfigGetVariableIntIdDef", {P,P,P,I,I}, I),
--  xIupConfigGetVariableDoubleDef    = iup_c_func(iup, "+IupConfigGetVariableDoubleDef", {P,P,P,D}, D),
--  xIupConfigGetVariableDoubleIdDef  = iup_c_func(iup, "+IupConfigGetVariableDoubleIdDef", {P,P,P,I,D}, D),
--  xIupConfigRecentInit              = iup_c_proc(iup, "+IupConfigRecentInit", {P,P,P,I}),
--  xIupConfigRecentUpdate            = iup_c_proc(iup, "+IupConfigRecentUpdate", {P,P}),
--  xIupConfigDialogShow              = iup_c_proc(iup, "+IupConfigDialogShow", {P,P,P}),
--  xIupConfigDialogClosed            = iup_c_proc(iup, "+IupConfigDialogClosed", {P,P,P}),
--  $


atom iup_scintilla = 0, xIupScintillaOpen, xIupScintilla, xIupScintillaSendMessage

-- (gets called automatically anyway)
global procedure IupScintillaOpen()
    if iup_scintilla=0 then
        iup_scintilla = iup_open_dll({"iup_scintilla.dll",
                                      "libiup_scintilla.so",
                                      "libiup_scintilla.dylib"})
--?iup_scintilla
        xIupScintillaOpen        = iup_c_proc(iup_scintilla, "IupScintillaOpen", {})
        xIupScintilla            = iup_c_func(iup_scintilla, "IupScintilla", {}, P)
        xIupScintillaSendMessage = iup_c_func(iup_scintilla, "IupScintillaSendMessage", {P,U,P,P}, P)
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


--dev WIERD ERROR...
--constant atom hCd = iup_open_dll({"cd.dll","libcd.so","libcd.dylib"})
--constant atom hCdIup = iup_open_dll({"iupcd.dll","libiupcd.so","libiupcd.dylib"})

--constant xwdCanvasGetImageRGB = iup_c_proc(hCd, "wdCanvasGetImageRGB", {P,P,P,P,D,D,I,I})

global function wdCanvasGetImageRGB(cdCanvas canvas, atom x, atom y, atom w, atom h)
    integer l = w*h
    atom pR = allocate(l),
         pG = allocate(l),
         pB = allocate(l)
    c_proc(xwdCanvasGetImageRGB, {canvas, pR, pG, pB, x, y, w, h})
    sequence r = peek({pR, l}),
             g = peek({pG, l}),
             b = peek({pB, l})
    free({pR,pG,pB})
    return {r,g,b}
end function

 -- vector text attributes
--------------------------
--constant
--  xcdCanvasVectorFont             = iup_c_func(hCd, "cdCanvasVectorFont", {P,P},P),
--  xcdCanvasVectorTextDirection    = iup_c_proc(hCd, "cdCanvasVectorTextDirection", {P,I,I,I,I}),
--  xcdCanvasVectorTextTransform    = iup_c_func(hCd, "cdCanvasVectorTextTransform", {P,P},P),
--  xcdCanvasVectorTextSize         = iup_c_proc(hCd, "cdCanvasVectorTextSize", {P,I,I,P}),
--  xcdCanvasVectorCharSize         = iup_c_func(hCd, "cdCanvasVectorCharSize", {P,I},I),
--  $

 -- world draw attributes
-------------------------
--constant 
--  xwdCanvasLineWidth      = iup_c_func(hCd, "wdCanvasLineWidth", {P,D},D),
--  xwdCanvasFont           = iup_c_proc(hCd, "wdCanvasFont", {P,P,I,D}),
--  xwdCanvasGetFont        = iup_c_proc(hCd, "wdCanvasGetFont", {P,P,P,P}),
--  xwdCanvasMarkSize       = iup_c_func(hCd, "wdCanvasMarkSize", {P,D},D),
--  xwdCanvasGetFontDim     = iup_c_proc(hCd, "wdCanvasGetFontDim", {P,P,P,P,P}),
--  xwdCanvasGetTextSize    = iup_c_proc(hCd, "wdCanvasGetTextSize", {P,P,P,P}),
--  xwdCanvasGetTextBox     = iup_c_proc(hCd, "wdCanvasGetTextBox", {P,D,D,P,P,P,P,P}),
--  xwdCanvasGetTextBounds  = iup_c_proc(hCd, "wdCanvasGetTextbounds", {P,D,D,P,P}),
--  xwdCanvasStipple        = iup_c_proc(hCd, "wdCanvasStipple", {P,I,I,P,D,D}),
--  xwdCanvasPattern        = iup_c_proc(hCd, "wdCanvasPattern", {P,I,I,P,D,D})
--  $

--/* DEV/SUG, from iup4eu3:
--  hIupSetCallbacks = define_c_func(iuplib, "IupSetCallbacks", {C_INT, C_POINTER, C_INT}, C_INT),
    hIupColorDlg = define_c_func(iuplib, "IupColorDlg", {}, C_INT),
    hIupGetFile = define_c_func(iuplib, "IupGetFile", {C_POINTER}, C_INT),
    hIupGetText = define_c_func(iuplib, "IupGetText", {C_POINTER, C_POINTER}, C_INT),

-- Associates several callbacks to an event.
-- To define each rid use the function Icallback.
-- Returns: the same widget handle.
--global function IupSetCallbacks(atom widget, string action, sequence rids)
--  return c_func(hIupSetCallbacks, {widget, action} & rids & {NULL})
--end function

-- Creates the Colour Dialog element. It is a predefined dialog for selecting a
-- color.
--
-- There are 3 versions of the dialog. One for Windows only, one for GTK only
-- and one for all systems, but it is based on the IupColorBrowser control
-- that depends on the CD library.
--
-- The Windows and GTK dialogs can be shown only with the IupPopup function.
-- The IupColorBrowser based dialog is an IupDialog that can be shown as any
-- regular IupDialog.
--
-- Returns: the identifier of the created element, or NULL if an error occurs.
--
-- Example:
-- <eucode>
-- --colordlg.exw
--
-- include iup.ew
--
-- IupOpen()
-- 
-- constant colordlg = IupColorDlg(),
--        title = "IupColorDlg"
--
-- IupPopup(colordlg, 10, 10)
--
-- string res = IupGetAttribute(colordlg, "VALUE")
--
-- if length(res) then
--      IupMessage(title, "You chose: '" & res & "'")
-- else
--      IupMessage(title, "You cancelled") 
-- end if
--
-- IupClose()
-- </eucode>
--global function IupColorDlg()
--  return c_func(hIupColorDlg, {})
--end function

-- Shows a modal dialog of the native interface system to select a filename.
-- Uses the IupFileDlg element.
--
-- Returns: a sequence containing:
--# a status code, whose values can be:
--** "1": New file.
--** "0": Normal, existing file.
--** "-1": Operation cancelled.
--# the name of the file selected.
--
-- Example:
-- <eucode>
-- --Example 3
--
-- include iup.ew
--
-- IupOpen()
-- sequence name, res
-- name = "*.txt"  -- name holds prompt
-- res = IupGetFile(name)
-- name = res[2]    -- name now holds chosen name
--
-- integer ret = res[1] -- status code
-- if ret = 1 then
--   IupMessage("New file", name)
-- elsif ret = 0 then
--   IupMessage("File already exists", name)
-- elsif ret = -1 then
--   IupMessage("IupGetFile", "Operation cancelled")
-- end if
-- IupClose()
-- </eucode>

-- Shows a modal dialog to edit a multiline text. The parameter ##text## defines
-- the initial value displayed in the dialog.
-- The C function called uses ##text## to hold the returned text.
-- Therefore it must have room for the edited string with at least
-- ##maxsize## length.
--
-- Returns: a sequence containing:
--# a status code; a non zero value if successful.
--# the full text in the modal dialog.
--
-- Example:
-- <eucode>
-- -- Example 4
-- -- Asking for Multiline Text
--
-- include iup.ew
--
-- procedure main()
--     sequence resp
--     IupOpen()
--     resp = IupGetText("Name", "")
--     if resp[1] = 1 then
--         IupMessage("Thanks!", resp[2])
--     end if
--     IupClose()
-- end procedure
--
-- main()
-- </eucode>

--*/

--(delete this lot (or move to another file) whenever you like)

 -- iup.dll, according to dependency walker:
--------------------------------------------
--/*
--IupAlarm
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
IupGetActionName    -- gone!
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
--IupLoad
--IupLoadBuffer
IupLoopStep
IupLoopStepWait
IupMainLoop
IupMainLoopLevel
IupMap
--IupMapFont    -- gone
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
--IupParamBox -- deliberately removed.... (unused/untested/undocumented)
--IupParamf -- gone (or rather IupParm now, but...)
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
--IupScrollBox
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
--IupTreeGetAttribute   -- gone
--IupTreeGetFloat   -- gone
IupTreeGetId
--IupTreeGetInt -- gone
IupTreeGetUserId
--IupTreeSetAttribute   -- gone
--IupTreeSetAttributeHandle -- gone
IupTreeSetfAttribute
IupTreeSetUserId
--IupTreeStoreAttribute -- gone
IupUnmap
--IupUnMapFont -- gone
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
--cdActiveCanvas
cdAlphaImage
cdArc
--cdBackground
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
--cdCanvasClipArea
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
--cdContextCGM
--cdContextClipboard
cdContextDBuffer
cdContextDBufferRGB
--cdContextDebug
--cdContextDGN
--cdContextDXF
--cdContextEMF
cdContextImage
cdContextImageRGB
cdContextIsPlus
--cdContextMetafile
cdContextNativeWindow
cdContextPicture
cdContextPrinter
cdContextPS
cdContextRegisterCallback
cdContextSVG
cdContextType
--cdContextWMF
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
--28/3/21
include IupFileList.e
