--
 -- pGUI.e
----------
-- =====
--
-- At last count (29/4/16) there are ~173 routines yet to be documented... 
--           [all done to IupSbox, ~line 2061] DEV
--  (11/11/16 all done to IupSpin, ~line 2790]
--  (none of which are used in any of the demos, and some of which may get culled)
--  (IupLayoutDialog skipped for now)

global type Ihandle(integer i)
    return i>0
end type

global type Ihandles(object o)
    if atom(o) then return Ihandle(o) end if
    for i=1 to length(o) do
        if not Ihandle(o[i]) then return 0 end if
    end for
    return 1
end type

global type Ihandln(integer i)
    return i>=0
end type

sequence callbacks = {}
sequence cbnames = {}

global type cbfunc(atom cb)
--  return cb=NULL or rfind(cb,callbacks)!=0    --DEV (no idea why rfind was being used...)
    return cb=NULL or find(cb,callbacks)!=0
end type

global function Icallback(string name, atom rid = routine_id(name))
    atom cb = call_back({'+', rid})
    integer k = find(cb,callbacks)
    if k=0 then
        callbacks = append(callbacks,cb)
        cbnames = append(cbnames,name)
    else
        if cbnames[k]!=name then ?9/0 end if
    end if
    return cb
end function

global function iup_name_from_cb(atom cb)
    return cbnames[find(cb,callbacks)]
end function

global function iup_cb_from_name(string name)
    integer idx = find(name,cbnames)
    if idx=0 then return NULL end if
    return callbacks[idx]
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

-- used by IupSetAttribute, IupSetGlobal, and cdCreateCanvas:
global type atom_string(object o)
    return string(o) 
        or (integer(o) and o>=NULL) 
        or (atom(o) and o>=NULL and o=floor(o))
end type

type bool(object o)
    return integer(o) and (o=0 or o=1)
end type

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

function paranormalise(object action=NULL, object func=NULL, sequence attributes="", dword_seq data={})
--
--  This routine implements a little trickery to make certain routines easier to use, for example the documentation contains
--
--      IupButton(string title, [[nullable_string action=NULL,] atom func=NULL,] string attributes="", sequence data={})
--
--  with the idea being that [action, func] are doubly-optional. I made up the term paranormalise as a jokey way to explain
--  that you have much more flexibility than normal in how you can provide the parameters. In short you can assume
--
--  1   IupButton(title) and
--  2   IupButton(title,action,func) [NB func is /not/ optional in this case] and
--  3   IupButton(title,func) and
--  4   IupButton(title,attributes[,data]) and
--  5   IupButton(title,func,attributes[,data]) and
--  6   IupButton(title,action,func,attributes[,data])
--
--  are all valid. Note that (unless using named parameters) you cannot change the order [afad], eg:
--
--      IupButton(title,attributes[,data][,action],func)    -- !INVALID!
--      IupButton(title,func,attributes,data,action)        -- !Stop this madness!
--      IupButton(action,attributes,data,func,title)        -- !You're being silly now!
--
--  The action and func parameters are actually declared as object, attributes as sequence, and data as dword_seq,
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
--      IupSetAttributes(button, attributes[, data]) 
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
-- This routine is designed to crash on the slightest oddity. It may be posssible to crib fatalN (eg from   [DEV/SUG]
--  builtins\VM\pcallfunc.e) and use crash_message() to get an error reported on offending user code line.
--

    if not nullable_string(action) then
        -- assume action omitted (and at least one other parameter passed)
        if atom(action) then    -- (and, of course, action is not NULL here)
            -- assume p1[action] is really func (cases 3,5)
            if length(data) then ?9/0 end if
            if length(attributes) then
                -- (func,attributes,data)
                data = attributes
--              if not dword_seq(data) then ?9/0 end if -- (ple/autoverified)
                attributes = func           -- (forced typecheck)
            elsif sequence(func) then
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
            -- assume p1 is attributes[, p2 is data]
            if length(attributes) or length(data) then ?9/0 end if
            if sequence(func) then
                data = func
--              if not dword_seq(data) then ?9/0 end if     -- (ple/autoverified)
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
--          if not dword_seq(data) then ?9/0 end if
--          if not string(attributes) then ?9/0 end if
--          if not cbfunc(func) then ?9/0 end if
--          if not nullable_string(action) then ?9/0 end if
        elsif length(data)!=0 then
            ?9/0    -- something odd passed (attributes="", data!={})
        elsif action!=NULL then
            -- assume IupButton(title,attributes)
            if length(data) then ?9/0 end if
            attributes = action -- p3:=p1
            if not string(attributes) then ?9/0 end if
            action = NULL
        end if
    elsif sequence(func) then
        -- assume (attributes,data), ie func (p2) is actually data
        -- [it would be odd for data (in func) to be {} here in a static call, but things might be table-driven]
        -- first, quickly check that p3 and p4 were /not/ passed:
        if length(data) then ?9/0 end if
        if length(attributes) then ?9/0 end if
        -- then validate action,func as attributes,data:
        if length(action)=0 and length(func)!=0 then ?9/0 end if -- (odd, as above)
        attributes = action     -- p3:=p1
        if not string(attributes) then ?9/0 end if
        data = func             -- p4:=p2
--      if not dword_seq(data) then ?9/0 end if     -- (ple/autoverified)
        action = NULL
        func = NULL
--  else
        -- assume 3 or 4 parameters were passed (action,func,attributes[,data])
    end if
--  if not dword_seq(data) then ?9/0 end if         -- (ple/autoverified)
    if not string(attributes) then ?9/0 end if
    if not cbfunc(func) then ?9/0 end if
    if not nullable_string(action) then ?9/0 end if
    return {action,func,attributes,data}
end function

function testcb()
    return 1
end function
constant tcb = Icallback("testcb")
--tests:
--  1   IupButton(title)
--  2   IupButton(title,action,func) [NB func is /not/ optional in this case]
--  3   IupButton(title,func)
--  4   IupButton(title,attributes[,data])
--  5   IupButton(title,func,attributes[,data])
--  6   IupButton(title,action,func,attributes[,data])
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

--DEV (make this a builtin)
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
        pstr = peekNS(ptr,machine_word(),0)
        if pstr=NULL then exit end if
        strings = append(strings, peek_string(pstr))
--      if length(strings)=n then exit end if
        ptr += machine_word()
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

procedure iup_poke_string_pointer_array(atom ptr, sequence strings)
    for i=1 to length(strings) do
        string si = strings[i]
        pokeN(ptr,si,machine_word())
        ptr += machine_word()
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

--/*
global function iup_isSysXkey( atom _c)
    return and_bits( _c, 0x80000000)
end function

--*/
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

include builtins\VM\\pcmdlnN.e      -- command_line()
include builtins\pgetpath.e         -- get_proper_dir()

procedure iup_link_error(sequence name)
    puts(1,"link error: "&name&"\n")
    ?9/0
end procedure

global -- for iup_layoutdlg.e
function iup_c_func(atom dll, sequence name, sequence args, atom result)
    integer handle = define_c_func(dll, name, args, result)
    if handle = -1 then iup_link_error(name) end if
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
global constant SLASH = iff(platform()=WINDOWS?'\\':'/')

function iup_open_dll(sequence libs)
string path = libs[libidx]
atom res
    if platform()=WINDOWS then
        string curr_dir = current_dir()
        if chdir(dll_path)=0 then ?9/0 end if
        path = current_dir()&SLASH&path
        res = open_dll(path)
        if chdir(curr_dir)=0 then ?9/0 end if
    elsif platform()=LINUX then
--      for now, see demo/pGUI/lnx/installation.txt...
--      path = dll_path&path
        res = open_dll(path)
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
    iup
atom
    xIupOpen,
    xIupClose,
--  xIupLoad,
--  xIupLoadBuffer,
--  xIupSetLanguage,
--  xIupGetLanguage,
--DEV these are documented in elements...
    xIupCreate,
--  xIupCreatev, -- (deliberately omitted)
    xIupDestroy,
    xIupMap,
    xIupUnmap,
--DEV attributes? (nah)
    xIupVersion,
    xIupVersionDate,
    xIupVersionNumber,

    xIupMainLoop,
    xIupMainLoopLevel,
    xIupLoopStep,
    xIupLoopStepWait,
    xIupExitLoop,
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
      xIupSetHandle,
    xIupSetAttributes,
    xIupResetAttribute,
    xIupGetAttribute,
     xIupGetAttributeId,
     xIupGetAttributeId2,
--   xIupGetAttributes, --(deprecated)
     xIupGetAllAttributes,
     xIupGetAttributeHandle,
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
    xIupSetCallback,
     xIupGetCallback,
    xIupGetClassName,
     xIupGetAllClasses,
     xIupGetClassType,
     xIupGetClassAttributes,
     xIupGetClassCallbacks,
     xIupSaveClassAttributes,
     xIupCopyClassAttributes,
     xIupSetClassDfltAttribute,

    xIupFill,
    xIupHboxv,
    xIupVboxv,
    xIupZboxv,
    xIupRadio,
    xIupNormalizerv,
    xIupCboxv,
    xIupSbox,
    xIupSplit,
    xIupGridBoxv,

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
    xIupProgressDlg,

    xIupButton,
    xIupCanvas,
    xIupFrame,
    xIupLabel,
    xIupList,
    xIupProgressBar,
    xIupSpin,
    xIupSpinbox,
    xIupTabsv,
    xIupText,
    xIupMultiLine,
    xIupTextConvertLinColToPos,
    xIupTextConvertPosToLinCol,
    xIupToggle,
    xIupTree,
    xIupVal,

    xIupConfig,
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
    xIupColorbar,
    xIupColorBrowser,
    xIupDial,
    xIupMatrix,
--  xIupMatSetAttribute,
--  xIupMatStoreAttribute,
--  xIupMatGetAttribute,
--  xIupMatGetInt,
--  xIupMatGetFloat,

    xIupLoad,
    xIupLoadBuffer,
    xIupSetLanguage,
    xIupGetLanguage,
    xIupSetLanguageString,
    xIupStoreLanguageString,
    xIupGetLanguageString,
    xIupSetLanguagePack,
    xIupGetFunction,
    xIupSetFunction,
    xIupClassMatch,
    xIupScrollBox,
    xIupExpander,
    xIupDetachBox,
    xIupBackgroundBox,
    xIupLink,
    xIupFlatButton,
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
    xiupKeyCodeToName,

    $

--DEV/SUG use this: sequence s = include_paths()
-- Note: pGUI.e must reside in a directory named pGUI; should you require a private copy
--        either to ensure that updates do not break anything, or you want users to be
--        able to run from source without installing Phix[??], do not place pGUI.e in
--        myproject/ but in myproject/pGUI/ along with whatever else you need, such as
--        and specifically the win32/win64/lnx32/lnx64 subfolders (opengl.e and glu.e 
--        are the only other things that I can think of).

procedure iup_init1(nullable_string dll_root)
if 0 then
string dll_abs -- (dll_root may be a relative path)
    if dll_root=NULL then
        dll_abs = get_proper_dir(command_line()[2])
    else
        dll_abs = get_proper_dir(dll_root)
    end if
--  dll_path = dll_root&sprintf("\\%s%d\\",{dirs[libidx],machine_bits()})
    dll_path = dll_abs&sprintf("%s%d%s",{dirs[libidx],machine_bits(),SLASH})
    if get_file_type(dll_path)!=FILETYPE_DIRECTORY then
        if dll_root=NULL then ?9/0 end if
        dll_abs = get_proper_dir(get_proper_dir(command_line()[2])&dll_root)
        dll_path = dll_abs&sprintf("%s%d%s",{dirs[libidx],machine_bits(),SLASH})
        if get_file_type(dll_path)!=FILETYPE_DIRECTORY then ?9/0 end if
    end if
else
    sequence s = include_paths()
    for i=1 to length(s) do
        sequence sip = split_path(s[i])
        if sip[$]="pGUI" then
--          dll_path = s[i]&sprintf("%s%d%s",{dirs[libidx],machine_bits(),SLASH})
--          dll_path = join_path({s[i],sprintf("%s%d",{dirs[libidx],machine_bits()})},1)
            sip = append(sip,sprintf("%s%d",{dirs[libidx],machine_bits()}))
            dll_path = join_path(sip,1)
--dll_root = dll_path
            exit
        end if
    end for
end if

--DEV:
    if platform()=WINDOWS then
        -- Aside: normally I'd expect msvcr120.dll to be loaded from system32/syswow64, 
        --        but if someone puts copies in pGUI\win32|64, it should be alright.
        --        You could also try deleting this test and see if it works anyway, but
        --        don't blame me if that gets you an even more cryptic error message.
        --        (This all depends on how the pre-built binaries were built, natch.)
        string curr_dir = current_dir()
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

    iup = iup_open_dll({"iup.dll",
                        "libiup.so",
                        "libiup.dylib"})

    -- Control
    xIupOpen            = iup_c_func(iup, "IupOpen", {I,P}, I)
    xIupClose           = iup_c_proc(iup, "IupClose", {})
--  xIupLoad            = iup_c_func(iup, "IupLoad", {P},P)
--  xIupLoadBuffer      = iup_c_func(iup, "IupLoadBuffer", {P}, P)
--  xIupSetLanguage     = iup_c_proc(iup, "IupSetLanguage", {P})
--  xIupGetLanguage     = iup_c_func(iup, "IupGetLanguage", {}, P)
--DEV these are documented in elements...
    xIupCreate          = iup_c_func(iup, "IupCreate", {P},P)
--  xIupCreatev         = iup_c_func(iup, "IupCreatev", {P,P},P) -- (deliberately omitted)
    xIupDestroy         = iup_c_proc(iup, "IupDestroy", {P})
    xIupMap             = iup_c_func(iup, "IupMap", {P},I)
    xIupUnmap           = iup_c_proc(iup, "IupUnmap", {P})
--DEV attributes? (nah)
    xIupVersion         = iup_c_func(iup, "IupVersion", {}, P)
    xIupVersionDate     = iup_c_func(iup, "IupVersionDate", {}, P)
    xIupVersionNumber   = iup_c_func(iup, "IupVersionNumber", {}, I)

    xIupMainLoop        = iup_c_proc(iup, "IupMainLoop", {})
    xIupMainLoopLevel   = iup_c_func(iup, "IupMainLoopLevel", {}, I)
    xIupLoopStep        = iup_c_func(iup, "IupLoopStep", {}, I)
    xIupLoopStepWait    = iup_c_func(iup, "IupLoopStepWait", {}, I)
    xIupExitLoop        = iup_c_proc(iup, "IupExitLoop", {})
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
      xIupSetHandle                 = iup_c_proc(iup, "IupSetHandle", {P,P})
    xIupSetAttributes               = iup_c_proc(iup, "IupSetAttributes", {P,P})
    xIupResetAttribute              = iup_c_proc(iup, "IupResetAttribute", {P,P})
    xIupGetAttribute                = iup_c_func(iup, "IupGetAttribute", {P,P}, P)
     xIupGetAttributeId             = iup_c_func(iup, "IupGetAttributeId", {P,P,I}, P)
     xIupGetAttributeId2            = iup_c_func(iup, "IupGetAttributeId2", {P,P,I,I}, P)
--   xIupGetAttributes              = iup_c_func(iup, "IupGetAttributes", {P}, P) --(deprecated)
     xIupGetAllAttributes           = iup_c_func(iup, "IupGetAllAttributes", {P,P,I}, I)
     xIupGetAttributeHandle         = iup_c_func(iup, "IupGetAttributeHandle", {P,P}, P)
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
    xIupSetCallback                 = iup_c_func(iup, "IupSetCallback", {P,P,P}, P)
     xIupGetCallback                = iup_c_func(iup, "IupGetCallback", {P,P}, P)
    xIupGetClassName                = iup_c_func(iup, "IupGetClassName", {P},P)
     xIupGetAllClasses              = iup_c_func(iup, "IupGetAllClasses", {P,I}, I)
     xIupGetClassType               = iup_c_func(iup, "IupGetClassType", {P}, P)
     xIupGetClassAttributes         = iup_c_func(iup, "IupGetClassAttributes", {P,P,I}, I)
     xIupGetClassCallbacks          = iup_c_func(iup, "IupGetClassCallbacks", {P,P,I}, I)
     xIupSaveClassAttributes        = iup_c_proc(iup, "IupSaveClassAttributes", {P})
     xIupCopyClassAttributes        = iup_c_proc(iup, "IupCopyClassAttributes", {P,P})
     xIupSetClassDfltAttribute      = iup_c_proc(iup, "IupSetClassDefaultAttribute", {P,P,P})

    xIupFill            = iup_c_func(iup, "IupFill", {}, P)
    xIupHboxv           = iup_c_func(iup, "IupHboxv", {P}, P)
    xIupVboxv           = iup_c_func(iup, "IupVboxv", {P}, P)
    xIupZboxv           = iup_c_func(iup, "IupZboxv", {P}, P)
    xIupRadio           = iup_c_func(iup, "IupRadio", {P}, P)
    xIupNormalizerv     = iup_c_func(iup, "IupNormalizerv", {P}, P)
    xIupCboxv           = iup_c_func(iup, "IupCboxv", {P}, P)
    xIupSbox            = iup_c_func(iup, "IupSbox", {P}, P)
    xIupSplit           = iup_c_func(iup, "IupSplit", {P,P}, P)
    xIupGridBoxv        = iup_c_func(iup, "IupGridBoxv", {P}, P)

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
    xIupMessageDlg      = iup_c_func(iup, "IupMessageDlg", {}, P)
    xIupCalendar        = iup_c_func(iup, "IupCalendar", {}, I)
    xIupColorDlg        = iup_c_func(iup, "IupColorDlg", {}, P)
    xIupDatePick        = iup_c_func(iup, "IupDatePick", {}, I)
    xIupFileDlg         = iup_c_func(iup, "IupFileDlg", {}, P)
    xIupFontDlg         = iup_c_func(iup, "IupFontDlg", {}, P)
    xIupGetColor        = iup_c_func(iup, "IupGetColor", {I,I,P,P,P}, I)
    xIupGetFile         = iup_c_func(iup, "IupGetFile", {P}, I)
    xIupGetParamv       = iup_c_func(iup, "IupGetParamv", {P,P,P,P,I,I,P}, I)
    xIupGetText         = iup_c_func(iup, "IupGetText", {P,P}, I)
    xIupListDialog      = iup_c_func(iup, "IupListDialog", {I,P,I,P,I,I,I,P}, I)
    xIupLayoutDialog    = iup_c_func(iup, "IupLayoutDialog", {P}, P)
    xIupProgressDlg     = iup_c_func(iup, "+IupProgressDlg", {}, P)

    xIupButton                  = iup_c_func(iup, "IupButton", {P,P}, P)
    xIupCanvas                  = iup_c_func(iup, "IupCanvas", {P}, P)
    xIupFrame                   = iup_c_func(iup, "IupFrame", {P}, P)
    xIupLabel                   = iup_c_func(iup, "IupLabel", {P}, P)
    xIupList                    = iup_c_func(iup, "IupList", {P}, P)
    xIupProgressBar             = iup_c_func(iup, "IupProgressBar", {}, P)
    xIupSpin                    = iup_c_func(iup, "IupSpin", {}, P)
    xIupSpinbox                 = iup_c_func(iup, "IupSpinbox", {P}, P)
    xIupTabsv                   = iup_c_func(iup, "IupTabsv", {P}, P)
    xIupText                    = iup_c_func(iup, "IupText", {P}, P)
    xIupMultiLine               = iup_c_func(iup, "IupMultiLine", {P},P)
    xIupTextConvertLinColToPos  = iup_c_proc(iup, "IupTextConvertLinColToPos", {P,I,I,P})
    xIupTextConvertPosToLinCol  = iup_c_proc(iup, "IupTextConvertPosToLinCol", {P,I,P,P})
    xIupToggle                  = iup_c_func(iup, "IupToggle", {P,P}, P)
    xIupTree                    = iup_c_func(iup, "IupTree", {}, P)
    xIupVal                     = iup_c_func(iup, "IupVal", {P}, P)

    xIupConfig                        = iup_c_func(iup, "IupConfig", {}, P)
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
    xIupColorbar            = iup_c_func(iupControls, "IupColorbar", {},P)
    xIupColorBrowser        = iup_c_func(iupControls, "IupColorBrowser", {},P)
    xIupDial                = iup_c_func(iupControls, "IupDial", {P},P)
    xIupMatrix              = iup_c_func(iupControls, "IupMatrix", {P},P)
--  xIupMatSetAttribute     = iup_c_proc(iupControls, "IupMatSetAttribute",{P,P,I,I,P})
--  xIupMatStoreAttribute   = iup_c_proc(iupControls, "IupMatStoreAttribute",{P,P,I,I,P})
--  xIupMatGetAttribute     = iup_c_func(iupControls, "IupMatGetAttribute",{P,P,I,I},P)
--  xIupMatGetInt           = iup_c_func(iupControls, "IupMatGetInt",{P,P,I,I},I)
--  xIupMatGetFloat         = iup_c_func(iupControls, "IupMatGetFloat",{P,P,I,I},F)

    xIupLoad                          = iup_c_func(iup, "+IupLoad", {P}, P)
    xIupLoadBuffer                    = iup_c_func(iup, "+IupLoadBuffer", {P}, P)
    xIupSetLanguage                   = iup_c_proc(iup, "+IupSetLanguage", {P})
    xIupGetLanguage                   = iup_c_func(iup, "+IupGetLanguage", {}, P)
    xIupSetLanguageString             = iup_c_proc(iup, "+IupSetLanguageString", {P,P})
    xIupStoreLanguageString           = iup_c_proc(iup, "+IupStoreLanguageString", {P,P})
    xIupGetLanguageString             = iup_c_func(iup, "+IupGetLanguageString", {P}, P)
    xIupSetLanguagePack               = iup_c_proc(iup, "+IupSetLanguagePack", {P})
    xIupGetFunction                   = iup_c_func(iup, "+IupGetFunction", {P}, P)
    xIupSetFunction                   = iup_c_func(iup, "+IupSetFunction", {P,P}, P)
    xIupClassMatch                    = iup_c_func(iup, "+IupClassMatch", {P,P}, I)
    xIupScrollBox                     = iup_c_func(iup, "+IupScrollBox", {P}, P)
    xIupExpander                      = iup_c_func(iup, "+IupExpander", {P}, P)
    xIupDetachBox                     = iup_c_func(iup, "+IupDetachBox", {P}, P)
    xIupBackgroundBox                 = iup_c_func(iup, "+IupBackgroundBox", {P}, P)
    xIupLink                          = iup_c_func(iup, "+IupLink", {P,P}, P)
    xIupFlatButton                    = iup_c_func(iup, "+IupFlatButton", {P}, P)
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
    xIupElementPropertiesDialog       = iup_c_func(iup, "+IupElementPropertiesDialog", {P}, P)
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

global procedure IupDestroy(Ihandles ih)
    if sequence(ih) then
        for i=1 to length(ih) do
            Ihandle ihi = ih[i]
            c_proc(xIupDestroy, {ihi})
        end for
    else
        c_proc(xIupDestroy, {ih})
    end if
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

 -- Attributes
--------------
--constant
--  xIupSetAttribute            = iup_c_proc(iup, "IupSetAttribute", {P,P,P}),
--   xIupSetAttributeId         = iup_c_proc(iup, "IupSetAttributeId", {P,P,I,P}),
--   xIupSetAttributeId2        = iup_c_proc(iup, "IupSetAttributeId2", {P,P,I,I,P}),
--   xIupSetStrAttribute        = iup_c_proc(iup, "IupSetStrAttribute", {P,P,P}),
--    xIupSetStrAttributeId     = iup_c_proc(iup, "IupSetStrAttributeId", {P,P,I,P}),
--    xIupSetStrAttributeId2    = iup_c_proc(iup, "IupSetStrAttributeId2", {P,P,I,I,P}),
--    xIupSetInt                = iup_c_proc(iup, "IupSetInt", {P,P,I}),
--    xIupSetIntId              = iup_c_proc(iup, "IupSetIntId", {P,P,I,I}),
--    xIupSetIntId2             = iup_c_proc(iup, "IupSetIntId2", {P,P,I,I,I}),
--    xIupSetFloat              = iup_c_proc(iup, "IupSetFloat", {P,P,F}),
--    xIupSetFloatId            = iup_c_proc(iup, "IupSetFloatId", {P,P,I,F}),
--    xIupSetFloatId2           = iup_c_proc(iup, "IupSetFloatId2", {P,P,I,I,F}),
--    xIupSetDouble             = iup_c_proc(iup, "IupSetDouble", {P,P,D}),
--    xIupSetDoubleId           = iup_c_proc(iup, "IupSetDoubleId", {P,P,I,D}),
--    xIupSetDoubleId2          = iup_c_proc(iup, "IupSetDoubleId2", {P,P,I,I,D}),
--    xIupSetRGB                = iup_c_proc(iup, "IupSetRGB", {P,P,UC,UC,UC}),
--    xIupSetRGBId              = iup_c_proc(iup, "IupSetRGBId", {P,P,I,UC,UC,UC}),
--    xIupSetRGBId2             = iup_c_proc(iup, "IupSetRGBId2", {P,P,I,I,UC,UC,UC}),
--    xIupStoreAttribute        = iup_c_proc(iup, "IupStoreAttribute", {P,P,P}),
--   xIupSetAttributeHandle     = iup_c_proc(iup, "IupSetAttributeHandle", {P,P,P}),
--    xIupSetHandle             = iup_c_proc(iup, "IupSetHandle", {P,P}),
--  xIupSetAttributes           = iup_c_proc(iup, "IupSetAttributes", {P,P}),
--  xIupResetAttribute          = iup_c_proc(iup, "IupResetAttribute", {P,P}),
--  xIupGetAttribute            = iup_c_func(iup, "IupGetAttribute", {P,P}, P),
--   xIupGetAttributeId         = iup_c_func(iup, "IupGetAttributeId", {P,P,I}, P),
--   xIupGetAttributeId2        = iup_c_func(iup, "IupGetAttributeId2", {P,P,I,I}, P),
----     xIupGetAttributes          = iup_c_func(iup, "IupGetAttributes", {P}, P), (deprecated)
--   xIupGetAllAttributes       = iup_c_func(iup, "IupGetAllAttributes", {P,P,I}, I),
--   xIupGetAttributeHandle     = iup_c_func(iup, "IupGetAttributeHandle", {P,P}, P),
--    xIupGetHandle             = iup_c_func(iup, "IupGetHandle", {P}, P),
--   xIupGetInt                 = iup_c_func(iup, "IupGetInt", {P,P}, I),
--   xIupGetInt2                = iup_c_func(iup, "IupGetInt2", {P,P}, I),
--   xIupGetIntInt              = iup_c_func(iup, "IupGetIntInt", {P,P,P,P}, I),
--   xIupGetIntId               = iup_c_func(iup, "IupGetIntId", {P,P,I}, I),
--   xIupGetIntId2              = iup_c_func(iup, "IupGetIntId2", {P,P,I,I}, I),
--   xIupGetFloat               = iup_c_func(iup, "IupGetFloat", {P,P}, F),
--   xIupGetFloatId             = iup_c_func(iup, "IupGetFloatId", {P,P,I}, F),
--   xIupGetFloatId2            = iup_c_func(iup, "IupGetFloatId2", {P,P,I,I}, F),
--   xIupGetDouble              = iup_c_func(iup, "IupGetDouble", {P,P}, D),
--   xIupGetDoubleId            = iup_c_func(iup, "IupGetDoubleId", {P,P,I}, D),
--   xIupGetDoubleId2           = iup_c_func(iup, "IupGetDoubleId2", {P,P,I,I}, D),
--   xIupGetRGB                 = iup_c_proc(iup, "IupGetRGB", {P,P,P,P,P}),
--   xIupGetRGBId               = iup_c_proc(iup, "IupGetRGBId", {P,P,I,P,P,P}),
--   xIupGetRGBId2              = iup_c_proc(iup, "IupGetRGBId2", {P,P,I,I,P,P,P}),
--  xIupSetGlobal               = iup_c_proc(iup, "IupSetGlobal", {P,P}),
--   xIupSetStrGlobal           = iup_c_proc(iup, "IupSetStrGlobal", {P,P}),
--   xIupStoreGlobal            = iup_c_proc(iup, "IupStoreGlobal", {P,P}),
--  xIupGetGlobal               = iup_c_func(iup, "IupGetGlobal", {P}, P),
--  xIupSetCallback             = iup_c_func(iup, "IupSetCallback", {P,P,P}, P),
--   xIupGetCallback            = iup_c_func(iup, "IupGetCallback", {P,P}, P),
--  xIupGetClassName            = iup_c_func(iup, "IupGetClassName", {P},P),
--   xIupGetAllClasses          = iup_c_func(iup, "IupGetAllClasses", {P,I}, I),
--   xIupGetClassType           = iup_c_func(iup, "IupGetClassType", {P}, P),
--   xIupGetClassAttributes     = iup_c_func(iup, "IupGetClassAttributes", {P,P,I}, I),
--   xIupGetClassCallbacks      = iup_c_func(iup, "IupGetClassCallbacks", {P,P,I}, I),
--   xIupSaveClassAttributes    = iup_c_proc(iup, "IupSaveClassAttributes", {P}),
--   xIupCopyClassAttributes    = iup_c_proc(iup, "IupCopyClassAttributes", {P,P}),
--   xIupSetClassDfltAttribute  = iup_c_proc(iup, "IupSetClassDefaultAttribute", {P,P,P})

global procedure IupSetAttribute(Ihandln ih, string name, atom_string v)
    if name!=upper(name) then ?9/0 end if
    c_proc(xIupSetAttribute, {ih, name, v})
end procedure

global procedure IupSetAttributePtr(Ihandln ih, string name, atom v)
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

global procedure IupSetStrAttribute(Ihandle ih, string name, nullable_string val, sequence data={})
    if name!=upper(name) then ?9/0 end if
    if length(data) then
        val = sprintf(val, data)
    end if
    c_proc(xIupSetStrAttribute, {ih, name, val})
end procedure

global procedure IupSetStrAttributeId(Ihandle ih, string name, integer id, nullable_string v=NULL, sequence data={})
    if length(data) then
        v = sprintf(v, data)
    end if
    c_proc(xIupSetStrAttributeId, {ih,name,id,v})
end procedure

global procedure IupSetStrAttributeId2(Ihandle ih, string name, integer lin, col, nullable_string v=NULL, sequence data={})
    if length(data) then
        v = sprintf(v, data)
    end if
    c_proc(xIupSetStrAttributeId2, {ih,name,lin,col,v})
end procedure

global procedure IupSetInt(Ihandles ih, string name, integer v)
    if sequence(ih) then
        for i=1 to length(ih) do
            IupSetInt(ih[i],name,v)
        end for
    else
        c_proc(xIupSetInt, {ih,name,v})
    end if
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

global procedure IupStoreAttribute(Ihandln ih, string name, nullable_string val, sequence data={})
    if name!=upper(name) then ?9/0 end if
    if length(data) then
        val = sprintf(val, data)
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
      iAttribCapture(env_buffer, "\"");
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
global procedure IupSetAttributes(Ihandle ih, string attributes, sequence data={})
    if length(data) then
        attributes = sprintf(attributes, data)
    end if
    c_proc(xIupSetAttributes, {ih, attributes})
end procedure

global function IupSetAttributesf(Ihandle ih, string attributes, sequence data={})
    IupSetAttributes(ih, attributes, data)
    return ih
end function

global procedure IupSetAttributeHandle(Ihandln ih, string name, Ihandln ih_named)
--  if name!=upper(name) then ?9/0 end if
    c_proc(xIupSetAttributeHandle, {ih, name, ih_named})
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

global function IupSetAtt(nullable_string name, Ihandle ih, sequence attribute_pairs)
    if and_bits(length(attribute_pairs),1) then ?9/0 end if
    for i=1 to length(attribute_pairs) by 2 do
        IupSetAttribute(ih, attribute_pairs[i], attribute_pairs[i+1])
    end for
    if name!=NULL then
        IupSetHandle(name, ih)
    end if
    return ih
end function

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
    atom ptr = allocate(machine_word()*n*8, 1)
    mem_set(ptr,0,machine_word()*n) -- (not strictly necessary)
    -- (aside: n may shrink here, as (ih,NULL,0) returns a count that includes
    --         internal attributes, which are filtered out in this return.)
    n = c_func(xIupGetAllAttributes, {ih,ptr,n})
    return iup_peek_string_pointer_array(ptr, n)
end function

global function IupGetAttributeHandle(Ihandln ih, string name)
    Ihandln ih_named = c_func(xIupGetAttributeHandle, {ih, name})
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
sequence res
atom pTwoInts = allocate(8)
integer count
    mem_set(pTwoInts,0,8)
    count = c_func(xIupGetIntInt, {ih,name,pTwoInts,pTwoInts+4})
--  if count!=2 then ?9/0 end if
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

global procedure IupSetCallback(Ihandle ih, string name, cbfunc func)
    atom prev = c_func(xIupSetCallback, {ih, name, func})
end procedure

global function IupSetCallbackf(Ihandle ih, string name, cbfunc func)
    IupSetCallback(ih, name, func)
    return ih
end function

global function IupSetCallbacks(Ihandle ih, sequence namefuncpairs)
    if and_bits(length(namefuncpairs),1) then ?9/0 end if
    for i=1 to length(namefuncpairs) by 2 do
        IupSetCallback(ih,namefuncpairs[i],namefuncpairs[i+1])
    end for
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

--constant
--  xIupFill        = iup_c_func(iup, "IupFill", {}, P),
--  xIupHboxv       = iup_c_func(iup, "IupHboxv", {P}, P),
--  xIupVboxv       = iup_c_func(iup, "IupVboxv", {P}, P),
--  xIupZboxv       = iup_c_func(iup, "IupZboxv", {P}, P),
--  xIupRadio       = iup_c_func(iup, "IupRadio", {P}, P),
--  xIupNormalizerv = iup_c_func(iup, "IupNormalizerv", {P}, P),
--  xIupCboxv       = iup_c_func(iup, "IupCboxv", {P}, P),
--  xIupSbox        = iup_c_func(iup, "IupSbox", {P}, P),
--  xIupSplit       = iup_c_func(iup, "IupSplit", {P,P}, P),
--  xIupGridBoxv    = iup_c_func(iup, "IupGridBoxv", {P}, P),
--  $
--
global function IupFill()
    Ihandle ih = c_func(xIupFill, {})
    return ih
end function

global function IupHbox(sequence children, string attributes="", sequence data={})
atom pChildren = iup_ptr_array(children)
    Ihandle ih = c_func(xIupHboxv, {pChildren})
    free(pChildren)
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupVbox(sequence children, string attributes="", sequence data={})
atom pChildren = iup_ptr_array(children)
    Ihandle ih = c_func(xIupVboxv, {pChildren})
    free(pChildren)
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupZbox(sequence children, string attributes="", sequence data={})
atom pChildren = iup_ptr_array(children)
    Ihandle ih = c_func(xIupZboxv, {pChildren})
    free(pChildren)
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupRadio(Ihandle child, string attributes="", sequence data={})
    Ihandle ih = c_func(xIupRadio, {child})
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupNormalizer(sequence ih_list, string attributes="", sequence data={})
    atom p_ih_list = iup_ptr_array(ih_list)
    Ihandle ih = c_func(xIupNormalizerv, {p_ih_list})
    free(p_ih_list)
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupNormaliser(sequence ih_list, string attributes="", sequence data={})
    return IupNormalizer(ih_list, attributes, data)
end function

global function IupCbox(sequence children, string attributes="", sequence data={})
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

--(I have no idea how these would be used)
--global enum IGBOX_HORIZONTAL, 
--          IGBOX_VERTICAL

global function IupGridBox(sequence children={}, string attributes="", sequence data={})
atom pChildren = iup_ptr_array(children)
    Ihandle ih = c_func(xIupGridBoxv, {pChildren})
    free(pChildren)
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
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
global procedure IupRefresh(Ihandle ih)
    c_proc(xIupRefresh, {ih})
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

global procedure IupRedraw(Ihandles ih, bool children=0)
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
--  xIupHide    = iup_c_proc(iup, "IupHide", {P})

--global function IupDialog(Ihandln child=NULL, object action=NULL, object func=NULL, sequence attributes="", dword_seq data={})
--  {action,func,attributes,data} = paranormalise(action,func,attributes,data)
global function IupDialog(Ihandln child=NULL, string attributes="", dword_seq data={})
    Ihandle ih = c_func(xIupDialog, {child})
--  if func!=NULL and action!=NULL then
--      IupSetCallback(ih, action, func)
--  end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global procedure IupPopup(Ihandle ih, integer x, integer y)
    if c_func(xIupPopup, {ih,x,y})!=IUP_NOERROR then ?9/0 end if
end procedure

global procedure IupShow(Ihandle ih)
integer r
    r = c_func(xIupShow, {ih})
    if r!=IUP_NOERROR then ?9/0 end if
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

--constant
--  xIupAlarm           = iup_c_func(iup, "IupAlarm", {P,P,P,P,P}, I),
--  xIupMessage         = iup_c_proc(iup, "IupMessage", {P,P}),
--  xIupMessageDlg      = iup_c_func(iup, "IupMessageDlg", {}, P),
--  xIupCalendar        = iup_c_func(iup, "IupCalendar", {}, I),
--  xIupColorDlg        = iup_c_func(iup, "IupColorDlg", {}, P),
--  xIupDatePick        = iup_c_func(iup, "IupDatePick", {}, I),  
--  xIupFileDlg         = iup_c_func(iup, "IupFileDlg", {}, P),
--  xIupFontDlg         = iup_c_func(iup, "IupFontDlg", {}, P),
--  xIupGetColor        = iup_c_func(iup, "IupGetColor", {I,I,P,P,P}, I),
--  xIupGetFile         = iup_c_func(iup, "IupGetFile", {P}, I),
--  xIupGetParamv       = iup_c_func(iup, "IupGetParamv", {P,P,P,P,I,I,P}, I),
--  xIupGetText         = iup_c_func(iup, "IupGetText", {P,P}, I),
--  xIupListDialog      = iup_c_func(iup, "IupListDialog", {I,P,I,P,I,I,I,P}, I),
--  xIupLayoutDialog    = iup_c_func(iup, "IupLayoutDialog", {P}, P),
--  xIupProgressDlg     = iup_c_func(iup, "+IupProgressDlg", {}, P)
--

global function IupAlarm(string title, string msg, string b1, nullable_string b2=NULL, nullable_string b3=NULL)
    return c_func(xIupAlarm, {title,msg,b1,b2,b3})
end function

global procedure IupMessage(nullable_string title=NULL, nullable_string msg=NULL, dword_seq data={})
    if length(data) then
        msg = sprintf(msg, data)
    end if
    c_proc(xIupMessage, {title,msg})
end procedure

global function IupMessageDlg()
    Ihandle ih = c_func(xIupMessageDlg, {})
    return ih
end function

global function IupCalendar(object action=NULL, object func=NULL, sequence attributes="", dword_seq data={})
    {action,func,attributes,data} = paranormalise(action,func,attributes,data)
    Ihandle ih = c_func(xIupCalendar, {})
    if func!=NULL then
        if action=NULL then
            action = "VALUECHANGED_CB"
        end if
        IupSetCallback(ih, action, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupColorDlg()
    Ihandle ih = c_func(xIupColorDlg, {})
    return ih
end function

global function IupDatePick()
    return c_func(xIupDatePick, {})
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
        return ""
    end if
    string filename = peek_string(pFilename)
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

global function IupGetText(string title, string text)
atom pText = allocate(10240,1)
    poke(pText, text&0)
    if c_func(xIupGetText, {title, pText})=0 then
        return ""
    end if
    text = peek_string(pText)
    return text
end function

global function IupListDialog(integer seltype, string title, sequence options, integer isel, integer maxCols, integer maxLines)
integer size = length(options), result
atom pOptions = allocate(machine_word()*size),
     pMark = NULL
sequence pOptAry = {}, marked, selected = {}

    for i=1 to size do
--      pOptAry &= allocate_string(options[i],1)
        pOptAry &= IupRawStringPtr(options[i])
    end for
    poke4(pOptions, pOptAry)
    if seltype=2 then
        -- multiple selection
        pMark = allocate(4*size)    --DEV machine_word()? [test on 64 bit]
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

----include controls.e
--constant
--  xIupButton                  = iup_c_func(iup, "IupButton", {P,P}, P),
--  xIupCanvas                  = iup_c_func(iup, "IupCanvas", {P}, P),
--  xIupFrame                   = iup_c_func(iup, "IupFrame", {P}, P),
--  xIupLabel                   = iup_c_func(iup, "IupLabel", {P}, P),
--  xIupList                    = iup_c_func(iup, "IupList", {P}, P),
--  xIupProgressBar             = iup_c_func(iup, "IupProgressBar", {}, P),
--  xIupSpin                    = iup_c_func(iup, "IupSpin", {}, P),
--  xIupSpinbox                 = iup_c_func(iup, "IupSpinbox", {P}, P),
--  xIupTabsv                   = iup_c_func(iup, "IupTabsv", {P}, P),
--  xIupText                    = iup_c_func(iup, "IupText", {P}, P),
--  xIupMultiLine               = iup_c_func(iup, "IupMultiLine", {P},P),
--  xIupTextConvertLinColToPos  = iup_c_proc(iup, "IupTextConvertLinColToPos", {P,I,I,P}),
--  xIupTextConvertPosToLinCol  = iup_c_proc(iup, "IupTextConvertPosToLinCol", {P,I,P,P}),
--  xIupToggle                  = iup_c_func(iup, "IupToggle", {P,P}, P),
--  xIupTree                    = iup_c_func(iup, "IupTree", {}, P),
--  xIupVal                     = iup_c_func(iup, "IupVal", {P}, P),
--  $

--global function IupButton(nullable_string title=NULL, nullable_string action=NULL, cbfunc func=NULL, string attributes="", sequence data={})
global function IupButton(object title=NULL, object action=NULL, object func=NULL, sequence attributes="", dword_seq data={})
    {action,func,attributes,data} = paranormalise(action,func,attributes,data)
    Ihandle ih = c_func(xIupButton, {title, action})
    if func!=NULL then
--      IupSetCallback(ih, ACTION, func)    -- action?
        if action=NULL then
--          action = ACTION
            action = "ACTION"
        end if
        IupSetCallback(ih, action, func)    -- action?
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupCanvas(object action=NULL, object func=NULL, sequence attributes="", dword_seq data={})
    {action,func,attributes,data} = paranormalise(action,func,attributes,data)
    Ihandle ih = c_func(xIupCanvas, {action})   --DEV NULL?
    if func!=NULL then
        if action=NULL then
            action = "ACTION"
        end if
        IupSetCallback(ih, action, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupFrame(Ihandle child, string attributes="", sequence data={})
    Ihandle ih = c_func(xIupFrame, {child})
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

--global function IupLabel(nullable_string title=NULL, string attributes="", sequence data={})
--global function IupLabel(object title=NULL, object action=NULL, object func=NULL, sequence attributes="", dword_seq data={})
--  {action,func,attributes,data} = paranormalise(action,func,attributes,data)
global function IupLabel(object title=NULL, sequence attributes="", dword_seq data={})
    Ihandle ih = c_func(xIupLabel, {title})
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupList(object action=NULL, object func=NULL, sequence attributes="", dword_seq data={})
    {action,func,attributes,data} = paranormalise(action,func,attributes,data)
    Ihandle ih = c_func(xIupList, {NULL})
    if func!=NULL then
        if action=NULL then
            action = "ACTION"
        end if
        IupSetCallback(ih, action, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupProgressBar()
    Ihandle ih = c_func(xIupProgressBar, {})
    return ih
end function

global function IupSpin()
    Ihandle ih = c_func(xIupSpin, {})
    return ih
end function

global function IupSpinBox(Ihandle child)
    Ihandle ih = c_func(xIupSpinbox, {child})
    return ih
end function

global function IupSpinbox(Ihandle child)
    return IupSpinBox(child)
end function

global function IupTabs(sequence children={})
atom pChildren = iup_ptr_array(children)
    Ihandle ih = c_func(xIupTabsv, {pChildren})
    free(pChildren)
    return ih
end function

global function IupText(object action=NULL, object func=NULL, sequence attributes="", dword_seq data={})
    {action,func,attributes,data} = paranormalise(action,func,attributes,data)
    Ihandle ih = c_func(xIupText, {NULL})
    if func!=NULL then
        if action=NULL then
            action = "ACTION"
        end if
        IupSetCallback(ih, action, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupMultiLine(object action=NULL, object func=NULL, sequence attributes="", dword_seq data={})
    {action,func,attributes,data} = paranormalise(action,func,attributes,data)
    Ihandle ih = c_func(xIupMultiLine, {NULL})
    if func!=NULL then
        if action=NULL then
            action = "ACTION"
        end if
        IupSetCallback(ih, action, func)
    end if
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

global function IupToggle(string title, object action=NULL, object func=NULL, sequence attributes="", dword_seq data={})
    {action,func,attributes,data} = paranormalise(action,func,attributes,data)
    Ihandle ih = c_func(xIupToggle, {title, NULL})
    if func!=NULL then
        if action=NULL then
            action = "ACTION"
        end if
        IupSetCallback(ih, action, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupTree(sequence attributes="", dword_seq data={})
    Ihandle ih = c_func(xIupTree, {})
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupValuator(nullable_string orientation=NULL, object action=NULL, object func=NULL, sequence attributes="", dword_seq data={})
    {action,func,attributes,data} = paranormalise(action,func,attributes,data)
    Ihandle ih = c_func(xIupVal, {orientation})
    if func!=NULL then
        if action=NULL then
            action = "ACTION"
        end if
        IupSetCallback(ih, action, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupVal(nullable_string orientation=NULL, object action=NULL, object func=NULL, sequence attributes="", dword_seq data={})
    return IupValuator(orientation, action, func, attributes, data)
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

global procedure IupConfigSetVariableStr(Ihandle ih, string group, string key, nullable_string v)
    c_proc(xIupConfigSetVariableStr, {ih,group,key,v})
end procedure

global procedure IupConfigSetVariableStrId(Ihandle ih, string group, string key, integer id, nullable_string v)
    c_proc(xIupConfigSetVariableStrId, {ih,group,key,id,v})
end procedure

global function IupConfigGetVariableInt(Ihandle ih, string group, string key, integer def=0)
    integer res = c_func(xIupConfigGetVariableIntDef, {ih,group,key,def})
    return res
end function

global function IupConfigGetVariableIntId(Ihandle ih, string group, string key, integer id, integer def=0)
    integer res = c_func(xIupConfigGetVariableIntIdDef, {ih,group,key,id,def})
    return res
end function

global function IupConfigGetVariableDouble(Ihandle ih, string group, string key, atom def=0)
    atom res = c_func(xIupConfigGetVariableDoubleDef, {ih,group,key,def})
    return res
end function

global function IupConfigGetVariableDoubleId(Ihandle ih, string group, string key, integer id, atom def=0)
    atom res = c_func(xIupConfigGetVariableDoubleIdDef, {ih,group,key,id,def})
    return res
end function

global function IupConfigGetVariableStr(Ihandle ih, string group, string key, nullable_string def=NULL)
    atom pString = c_func(xIupConfigGetVariableStrDef, {ih,group,key,def})
    string res = iff(pString=NULL?"":peek_string(pString))
    return res
end function

global function IupConfigGetVariableStrId(Ihandle ih, string group, string key, integer id, nullable_string def=NULL)
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

global procedure IupConfigDialogShow(Ihandle config, Ihandle dialog, string name, bool maximised=0)
integer m = IupConfigGetVariableInt(config, name, "Maximized", maximised)
    IupConfigSetVariableInt(config, name, "Maximized", m)
    c_proc(xIupConfigDialogShow, {config,dialog,name})
end procedure

global procedure IupConfigDialogClosed(Ihandle ih, Ihandle dialog, string name)
    c_proc(xIupConfigDialogClosed, {ih,dialog,name})
end procedure


--****
-- === Additional
--

global function IupCells(string attributes="", dword_seq data={})
    IupControlsOpen()
    Ihandle ih = c_func(xIupCells, {})
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

--DEV doc (done to here)
global function IupColorbar(string attributes="", dword_seq data={})
    IupControlsOpen()
    Ihandle ih = c_func(xIupColorbar, {})
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupColorBrowser(string attributes="", dword_seq data={})
    IupControlsOpen()
    Ihandle ih = c_func(xIupColorBrowser, {})
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupDial(string orientation, string attributes="", dword_seq data={})
    IupControlsOpen()
    Ihandle ih = c_func(xIupDial, {orientation})
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupMatrix(string attributes="", sequence data={})
    IupControlsOpen()
    Ihandle ih = c_func(xIupMatrix, {ACTION_CB})
--DEV tryme:
--  Ihandle ih = c_func(xIupMatrix, {NULL})
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

atom
    iupMatrixEx = 0,

    xIupMatrixEx,
    xIupMatrixExInit,
    xIupMatrixExOpen

procedure iup_init_matrix()
    IupControlsOpen()
    if iupMatrixEx=0 then
        iupMatrixEx = iup_open_dll({
                                    "iupmatrixex.dll",
                                    "libiupmatrixex.so",
                                    "libiupmatrixex.dylib"
                                   })

        xIupMatrixEx            = iup_c_func(iupMatrixEx, "IupMatrixEx", {},P)
        xIupMatrixExInit        = iup_c_proc(iupMatrixEx, "IupMatrixExInit", {P})
        xIupMatrixExOpen        = iup_c_proc(iupMatrixEx, "IupMatrixExOpen", {})
    end if
end procedure

integer matrixexopen = 0
global procedure IupMatrixExOpen()
    iup_init_matrix()
    if not matrixexopen then
        c_proc(xIupMatrixExOpen,{})
        matrixexopen = 1
    end if
end procedure

global function IupMatrixEx()
    IupMatrixExOpen()
    Ihandle res = c_func(xIupMatrixEx,{})
    return res
end function

global procedure IupMatrixExInit(Ihandle ih)
    IupMatrixExOpen()
    c_proc(xIupMatrixExInit,{ih})
end procedure

global procedure IupMatSetAttribute(Ihandle ih, string name, integer lin, integer col, atom_string v)
--  c_proc(xIupMatSetAttribute, {ih, name, lin, col, v})
    c_proc(xIupSetAttributeId2, {ih, name, lin, col, v})
end procedure

global procedure IupMatStoreAttribute(Ihandle ih, string name, integer lin, integer col, string val, sequence data={})
    if length(data) then
        val = sprintf(val, data)
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
     xIupGLToggle,
     xIupGLVal

procedure iup_gl_controls_init()
    if iupGLControls=NULL then
        iupGLControls = iup_open_dll({"iupglcontrols.dll",
                                      "libiupglcontrols.so",
                                      "libiupglcontrols.dylib"})
        xIupGLControlsOpen  = iup_c_proc(iupGLControls, "IupGLControlsOpen", {})
        xIupGLCanvasBoxv    = iup_c_func(iupGLControls, "IupGLCanvasBoxv", {P}, P)
        xIupGLButton        = iup_c_func(iupGLControls, "IupGLButton", {P}, P)
        xIupGLExpander      = iup_c_func(iupGLControls, "IupGLExpander", {P}, P)
        xIupGLFrame         = iup_c_func(iupGLControls, "IupGLFrame", {P}, P)
        xIupGLLabel         = iup_c_func(iupGLControls, "IupGLLabel", {P}, P)
        xIupGLLink          = iup_c_func(iupGLControls, "IupGLLink", {P,P}, P)
        xIupGLProgressBar   = iup_c_func(iupGLControls, "IupGLProgressBar", {}, P)
        xIupGLScrollBox     = iup_c_func(iupGLControls, "IupGLScrollBox", {P}, P)
        xIupGLSeparator     = iup_c_func(iupGLControls, "IupGLSeparator", {}, P)
        xIupGLSizeBox       = iup_c_func(iupGLControls, "IupGLSizeBox", {P}, P)
        xIupGLToggle        = iup_c_func(iupGLControls, "IupGLToggle", {P}, P)
        xIupGLVal           = iup_c_func(iupGLControls, "IupGLVal", {P}, P)
    end if
end procedure

global procedure IupGLControlsOpen()
    iup_gl_controls_init()
    c_proc(xIupGLControlsOpen, {})
end procedure

--DEV paranormalised etc the rest of them (I only did this one because I copied IupHbox)
global function IupGLCanvasBox(sequence children, string attributes="", sequence data={})
atom pChildren = iup_ptr_array(children)
    Ihandle ih = c_func(xIupGLCanvasBoxv, {pChildren})
    free(pChildren)
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupGLButton(nullable_string title)
Ihandle ih = c_func(xIupGLButton,{title})
    return ih
end function

global function IupGLExpander(Ihandln child)
Ihandle ih = c_func(xIupGLExpander,{child})
    return ih
end function

global function IupGLFrame(Ihandln child)
Ihandle ih = c_func(xIupGLFrame,{child})
    return ih
end function

global function IupGLLabel(nullable_string title)
Ihandle ih = c_func(xIupGLLabel,{title})
    return ih
end function

global function IupGLLink(nullable_string url, nullable_string title)
Ihandle ih = c_func(xIupGLLink,{url,title})
    return ih
end function

global function IupGLProgressBar()
Ihandle ih = c_func(xIupGLProgressBar,{})
    return ih
end function

global function IupGLScrollBox(Ihandln child)
Ihandle ih = c_func(xIupGLScrollBox,{child})
    return ih
end function

global function IupGLSeparator()
Ihandle ih = c_func(xIupGLSeparator,{})
    return ih
end function

global function IupGLSizeBox(Ihandln child)
Ihandle ih = c_func(xIupGLSizeBox,{child})
    return ih
end function

global function IupGLToggle(nullable_string title)
Ihandle ih = c_func(xIupGLToggle,{title})
    return ih
end function

global function IupGLVal(nullable_string orientation=NULL)
Ihandle ih = c_func(xIupGLVal,{orientation})
    return ih
end function

global function IupGLValuator(nullable_string orientation=NULL)
Ihandle ih = c_func(xIupGLVal,{orientation})
    return ih
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
    xIupDrawGetImageInfo,
    xIupLoadImage,
    xIupSaveImage,
    xIupSaveImageAsText,
    xIupGetNativeHandleImage,
    xIupGetImageNativeHandle,
    xIupImageFromImImage,
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
        xIupDrawGetImageInfo        = iup_c_proc(iup,"IupDrawGetImageInfo",{P,P,P,P})
        xIupLoadImage               = iup_c_func(iupIm, "IupLoadImage", {P}, P)
        xIupSaveImage               = iup_c_func(iupIm, "IupSaveImage", {P,P,P}, I)
        xIupSaveImageAsText         = iup_c_func(iup, "IupSaveImageAsText", {P,P,P,P}, I)
        xIupGetNativeHandleImage    = iup_c_func(iupIm, "IupGetNativeHandleImage", {P}, P)
        xIupGetImageNativeHandle    = iup_c_func(iupIm, "IupGetImageNativeHandle", {P}, P)
        xIupImageFromImImage        = iup_c_func(iupIm, "IupImageFromImImage", {P}, P)
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
        ximImageGetOpenGLData       = iup_c_proc(hIm, "imImageGetOpenGLData", {P,P})
    end if
end procedure

global function IupImage(integer width, integer height, sequence pixels)
atom pPixels = allocate(length(pixels))
    poke(pPixels, pixels)
    iup_image_init()
    Ihandle ih = c_func(xIupImage, {width, height, pPixels})
    free(pPixels)
    return ih
end function

global function IupImageA(integer width, integer height, atom pPixels)
    iup_image_init()
    Ihandle ih = c_func(xIupImage, {width, height, pPixels})
    return ih
end function

global function IupImageRGB(integer width, integer height, sequence pixels)
atom pPixels = allocate(length(pixels))
    poke(pPixels, pixels)
    iup_image_init()
    Ihandle ih = c_func(xIupImageRGB, {width, height, pPixels})
    free(pPixels)
    return ih
end function

global function IupImageRGBA(integer width, integer height, sequence pixels)
atom pPixels = allocate(length(pixels))
    poke(pPixels, pixels)
    iup_image_init()
    Ihandle ih = c_func(xIupImageRGBA, {width, height, pPixels})
    free(pPixels)
    return ih
end function

--void IupDrawGetImageInfo(const char* name, int *w, int *h, int *bpp)
global function IupDrawGetImageInfo(string name)
atom p_w = allocate(4, 1),
     p_h = allocate(4, 1),
     p_bpp = allocate(4, 1)
    c_proc(xIupDrawGetImageInfo,{name,p_w,p_h,p_bpp})
    return {peek4s(p_w),peek4s(p_h),peek4s(p_bpp)}
end function

global function IupLoadImage(string filename)
    iup_image_init()
    Ihandln ih = c_func(xIupLoadImage, {filename})
    return ih
end function

global function IupSaveImage(Ihandle ih, string file_name, string fmt)
--  iup_image_init()
--  integer result = c_func(xIupSaveImage, {ih,file_name,fmt})
--  return result
    if c_func(xIupSaveImage, {ih,file_name,fmt})=0 then
        return IupGetGlobal("IUPIM_LASTERROR")
    end if
    return 1
end function

global function IupSaveImageAsText(Ihandle ih, string filename, string fmt, nullable_string name=NULL)
--  iup_image_init()
    integer result = c_func(xIupSaveImageAsText, {ih, filename, fmt, name})
    return result   -- 0=failure
end function

--imImage* IupGetNativeHandleImage(void* handle);
global function IupGetNativeHandleImage(atom handle)
--  iup_image_init()
    imImage result = c_func(xIupGetNativeHandleImage, {handle})
    return result
end function

--void* IupGetImageNativeHandle(const imImage* image);
global function IupGetImageNativeHandle(imImage image)
--  iup_image_init()
    atom result = c_func(xIupGetImageNativeHandle, {image})
    return result
end function

--Ihandle* IupImageFromImImage(const imImage* image);
global function IupImageFromImImage(imImage image)
--  iup_image_init()
    Ihandle ih = c_func(xIupImageFromImImage, {image})
    return ih
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
atom pColour = allocate(12,1)
    if length(replace_color)!=3 then ?9/0 end if
    iup_poke_float(pColour,replace_color)
--  iup_image_init()
    c_proc(ximProcessRenderFloodFill,{image,start_x,start_y,pColour,tolerance})
end procedure

--/*
int imProcessRenderConstant(imImage *image, float *value)     
Render a constant. The number of values must match the depth of the image.
--*/
global procedure imProcessRenderConstant(imImage image, sequence v)
atom pColour = allocate(4*length(v),1)
    iup_poke_float(pColour,v)
--  iup_image_init()
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
--  iup_image_init()
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
In Lua the IM image metatable name is "imImage". When converted to a string will return "imImage(%p) [width=%d,height=%d,color_space=%s,data_type=%s,depth=%d]" where p is replaced by the userdata address, and other values are replaced by the respective attributes. If the image is already destroyed by im.ImageDestroy, then it will return also the suffix "-destroyed".
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
global procedure imImageDestroy(imImage image)
    iup_image_init()
    c_proc(ximImageDestroy,{image})
end procedure

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

global function im_depth(imImage image)
    integer depth = peek4s(image+20)
    return depth
end function

global function im_color_space(imImage image)
    integer color_space = peek4s(image+8)
    return color_space
end function

global function im_data(imImage image)
    atom dataptr = peekNS(image+40,machine_word(),0)
    return dataptr
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
global function IupMenu(sequence children)
atom pChildren = iup_ptr_array(children)
Ihandle ih = c_func(xIupMenuv, {pChildren})
    free(pChildren)
    return ih
end function

global function IupMenuItem(string title, object action=NULL, object func=NULL, sequence attributes="", dword_seq data={})
    {action,func,attributes,data} = paranormalise(action,func,attributes,data)
    Ihandle ih = c_func(xIupItem, {title, action})
    if func!=NULL then
        if action=NULL then
            action = "ACTION"
        end if
        IupSetCallback(ih, action, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupItem(string title, object action=NULL, object func=NULL, sequence attributes="", dword_seq data={})
    return IupMenuItem(title, action, func, attributes, data)
end function

global function IupSeparator()
    Ihandle ih = c_func(xIupSeparator, {})
    return ih
end function

--Ihandle* IupSubmenu(const char* title, Ihandle* child);
global function IupSubmenu(nullable_string title, Ihandln menu, string attributes="", sequence data={})
    Ihandle ih = c_func(xIupSubmenu, {title, menu})
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global function IupSubMenu(nullable_string title, Ihandln menu)
    return IupSubmenu(title, menu)
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

--constant
--  xIupClipboard = iup_c_func(iup, "IupClipboard", {}, P),
--  xIupTimer = iup_c_func(iup, "IupTimer", {}, P),
--  xIupUser = iup_c_func(iup, "IupUser", {}, P),
--  xIupHelp = iup_c_func(iup, "IupHelp", {P}, I)

global function IupClipboard()
    Ihandle ih = c_func(xIupClipboard, {})
    return ih
end function

global function IupTimer(atom func=NULL, integer msecs=0, integer active=1)
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

global function IupUser()
    Ihandle ih = c_func(xIupUser, {})
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
    CD_POLYCUSTOM = 10,
    --
    -- Bitmap Type Constants
    --
    --  These definitions are compatible with the IM library
    --
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
    -- Predefined Color Constants
    --
    -- These are simply for convenience
    --
    CD_RED          = #FF0000,
    CD_DARK_RED     = #800000,
    CD_ORANGE       = #FFA500,
    CD_DARK_ORANGE  = #FF8C00,
    CD_GREEN        = #00FF00,
    CD_DARK_GREEN   = #008000,
    CD_BLUE         = #0000FF,
    CD_DARK_BLUE    = #000080,
    CD_YELLOW       = #FFFF00,
    CD_DARK_YELLOW  = #808000,
    CD_MAGENTA      = #FF00FF,
    CD_DARK_MAGENTA = #800080,
    CD_CYAN         = #00FFFF,
    CD_DARK_CYAN    = #008080,
    CD_WHITE        = #FFFFFF,
    CD_BLACK        = #000000,
    CD_DARK_GRAY    = #808080,
    CD_DARK_GREY    = #808080,
    CD_GRAY         = #C0C0C0,
    CD_GREY         = #C0C0C0,
    CD_PARCHMENT    = #FFFFE0,
    CD_INDIGO       = #4B0082,
    CD_PURPLE       = #D080D0,
    CD_VIOLET       = #EE82EE,

    --
    -- Conversion Factor Constants
    --
    -- These are simply for convenience
    --
    CD_MM2PT = 2.83465,         -- Milimeters to Points (pt = CD_MM2PT * mm)
    CD_RAD2DEG = 57.2958,       -- Radians to Degrees (deg = CD_RAD2DEG * rad)
    CD_DEG2RAD = 0.0174533,     -- Degrees to Radians (rad = CD_DEG2RAD * deg)

    CD_IUP          = {"CD_IUP"},
    CD_PRINTER      = {"CD_PRINTER"},
    CD_PS           = {"CD_PS"},
    CD_PICTURE      = {"CD_PICTURE"},
    CD_GL           = {"CD_GL"},
    CD_IUPDBUFFER   = {"CD_IUPDBUFFER"},
--  CD_NATIVEWINDOW = {"CD_NATIVEWINDOW"},
    CD_DBUFFER      = {"CD_DBUFFER"},
    CD_IMIMAGE      = {"CD_IMIMAGE"},
    $

atom
    hCd = 0,
    hCdIup,
    hCdGL,
    hCdIm,

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
    xcdContextPrinter,
    xcdContextPS,
    xcdContextPicture,
    xcdContextGL,
    xcdContextIupDBuffer,
    xcdContextDBuffer,

    xcdContextNativeWindow,
    xcdGetScreenSize,
    xcdGetScreenColorPlanes,

    XCD_IUP,
    XCD_PRINTER,
    XCD_PS,
    XCD_PICTURE,
    XCD_GL,
    XCD_IUPDBUFFER,
--  XCD_NATIVEWINDOW,
    XCD_DBUFFER,
    XCD_IMIMAGE

sequence XCD_STR = {"CD_IUP",
                    "CD_PRINTER",
                    "CD_PS",
                    "CD_PICTURE",
                    "CD_GL",
                    "CD_IUPDBUFFER",
--                  "CD_NATIVEWINDOW",
                    "CD_DBUFFER",
                    "CD_IMIMAGE"},
         XCDS

atom 
    xcdCanvasSimulate,
    xcdCanvasFlush,
    xcdCanvasClear,
    xcdClear,
    xcdCanvasSaveState,
    xcdCanvasRestoreState,
    xcdCanvasReleaseState,
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
    xcdCanvasClipArea,
    xcdfCanvasClipArea,
    xcdCanvasGetClipArea,
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
    xcdfCanvasLine,
    xcdfCanvasVertex,
    xcdfCanvasRect,
    xcdfCanvasBox,
    xcdfCanvasArc,
    xcdfCanvasSector,
    xcdfCanvasChord,
    xcdfCanvasText,

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
    xcdCanvasCreateImage,
    xcdKillImage,
    xcdCanvasGetImage,
    xcdCanvasPutImageRect,
    xcdCanvasScrollArea,

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
    xcdDecodeColor,
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
--DEV...
--      xcdInitContextPlus  = iup_c_proc(hCd, "cdInitContextPlus", {})
        xcdInitContextPlus  = define_c_proc(hCd, "cdInitContextPlus", {})

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
        xcdContextPrinter       = iup_c_func(hCd, "cdContextPrinter", {}, P)
        xcdContextPS            = iup_c_func(hCd, "cdContextPS", {}, P)
        xcdContextPicture       = iup_c_func(hCd, "cdContextPicture", {}, P)
        xcdContextGL            = iup_c_func(hCdGL,"cdContextGL", {}, P)
        xcdContextIupDBuffer    = iup_c_func(hCdIup,"cdContextIupDBuffer", {}, P)
        xcdContextDBuffer       = iup_c_func(hCd, "cdContextDBuffer", {}, P)

--DEV..
if platform()=WINDOWS then
        xcdContextNativeWindow  = iup_c_func(hCd, "cdContextNativeWindow", {},P)
        xcdGetScreenSize        = iup_c_proc(hCd, "cdGetScreenSize", {P,P,P,P})
        xcdGetScreenColorPlanes = iup_c_func(hCd, "cdGetScreenColorPlanes", {},I)
end if


--DEV/doc:
-- Note: The constants CD_IUP etc are initialised with abtruse values: by "abtruse" (not a real word) I mean a value designed to trigger an error 
--      if used directly, that makes some sense when debugging said error, and is substituted (within the pGUI wrapper) by a proper value, ie/eg 
--      CD_IUP is {"CD_IUP"}, which is automatically replaced with the result of cdContextIup(). Should you need the proper values outside pGUI,
--      use the corresponding function call (search the source of pGUI.e to determine what that is [try "abtruse"!]).

        XCD_IUP          = c_func(xcdContextIup, {})
        XCD_PRINTER      = c_func(xcdContextPrinter, {})
        XCD_PS           = c_func(xcdContextPS, {})
        XCD_PICTURE      = c_func(xcdContextPicture, {})
        XCD_GL           = c_func(xcdContextGL, {})
        XCD_IUPDBUFFER   = c_func(xcdContextIupDBuffer, {})
--      XCD_NATIVEWINDOW = c_func(xcdContextNativeWindow, {})
        XCD_DBUFFER      = c_func(xcdContextDBuffer, {})
        XCD_IMIMAGE      = c_func(xcdContextImImage,{})

        XCDS = {XCD_IUP,
                XCD_PRINTER,
                XCD_PS,
                XCD_PICTURE,
                XCD_GL,
                XCD_IUPDBUFFER,
--              XCD_NATIVEWINDOW,
                XCD_DBUFFER,
                XCD_IMIMAGE}

        --
        -- control
        --
        xcdCanvasSimulate       = iup_c_func(hCd, "cdCanvasSimulate", {P,I},I)
        xcdCanvasFlush          = iup_c_proc(hCd, "cdCanvasFlush", {P})
        xcdCanvasClear          = iup_c_proc(hCd, "cdCanvasClear", {P})
        xcdClear                = iup_c_proc(hCd, "cdClear", {})
        xcdCanvasSaveState      = iup_c_func(hCd, "cdCanvasSaveState", {P},P)
        xcdCanvasRestoreState   = iup_c_proc(hCd, "cdCanvasRestoreState", {P,P})
        xcdCanvasReleaseState   = iup_c_proc(hCd, "cdReleaseState", {P})
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
        xcdCanvasClipArea       = iup_c_proc(hCd, "cdCanvasClipArea", {P,I,I,I,I})
        xcdfCanvasClipArea      = iup_c_proc(hCd, "cdfCanvasClipArea", {P,D,D,D,D})
        xcdCanvasGetClipArea    = iup_c_func(hCd, "cdCanvasGetClipArea", {P,P,P,P,P},I)
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
        xcdCanvasPixel  = iup_c_proc(hCd, "cdCanvasPixel", {P,I,I,I})
        xcdCanvasMark   = iup_c_proc(hCd, "cdCanvasMark", {P,I,I})
        xcdCanvasLine   = iup_c_proc(hCd, "cdCanvasLine", {P,I,I,I,I})
        xcdLine         = iup_c_proc(hCd, "cdLine", {I,I,I,I})
        xcdCanvasBegin  = iup_c_proc(hCd, "cdCanvasBegin", {P,I})
        xcdCanvasVertex = iup_c_proc(hCd, "cdCanvasVertex", {P,I,I})
        xcdCanvasEnd    = iup_c_proc(hCd, "cdCanvasEnd", {P})
        xcdCanvasRect   = iup_c_proc(hCd, "cdCanvasRect", {P,I,I,I,I})
        xcdCanvasBox    = iup_c_proc(hCd, "cdCanvasBox", {P,I,I,I,I})
        xcdCanvasArc    = iup_c_proc(hCd, "cdCanvasArc", {P,I,I,I,I,D,D})
        xcdCanvasSector = iup_c_proc(hCd, "cdCanvasSector", {P,I,I,I,I,D,D})
        xcdCanvasChord  = iup_c_proc(hCd, "cdCanvasChord", {P,I,I,I,I,D,D})
        xcdCanvasText   = iup_c_proc(hCd, "cdCanvasText", {P,I,I,P})
        xcdText         = iup_c_proc(hCd, "cdText", {I,I,P})

        --
        -- primitives with double as arguments instead of integer
        --
        xcdfCanvasLine      = iup_c_proc(hCd, "cdfCanvasLine", {P,D,D,D,D})
        xcdfCanvasVertex    = iup_c_proc(hCd, "cdfCanvasVertex", {P,D,D})
        xcdfCanvasRect      = iup_c_proc(hCd, "cdfCanvasRect", {P,D,D,D,D})
        xcdfCanvasBox       = iup_c_proc(hCd, "cdfCanvasBox", {P,D,D,D,D})
        xcdfCanvasArc       = iup_c_proc(hCd, "cdfCanvasArc", {P,D,D,D,D,D,D})
        xcdfCanvasSector    = iup_c_proc(hCd, "cdfCanvasSector", {P,D,D,D,D,D,D})
        xcdfCanvasChord     = iup_c_proc(hCd, "cdfCanvasChord", {P,D,D,D,D,D,D})
        xcdfCanvasText      = iup_c_proc(hCd, "cdfCanvasText", {P,D,D,P})

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
        xcdCanvasCreateImage    = iup_c_func(hCd, "cdCanvasCreateImage", {P,I,I},P)
        xcdKillImage            = iup_c_proc(hCd, "cdKillImage", {P})
        xcdCanvasGetImage       = iup_c_proc(hCd, "cdCanvasGetImage", {P,P,I,I})
        xcdCanvasPutImageRect   = iup_c_proc(hCd, "cdCanvasPutImageRect", {P,P,I,I,I,I,I,I})
        xcdCanvasScrollArea     = iup_c_proc(hCd, "cdCanvasScrollArea", {P,I,I,I,I,I,I})

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
        xcdEncodeColor  = iup_c_func(hCd, "cdEncodeColor", {UC,UC,UC},L)
        xcdDecodeColor  = iup_c_proc(hCd, "cdDecodeColor", {L,P,P,P})
        xcdDecodeAlpha  = iup_c_func(hCd, "cdDecodeAlpha", {L},UC)
        xcdEncodeAlpha  = iup_c_func(hCd, "cdEncodeAlpha", {L,UC},L)
        xcdRGB2Map      = iup_c_proc(hCd, "cdRGB2Map", {I,I,P,P,P,P,I,P})

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
--constant
--  xcdContextIup           = iup_c_func(hCdIup, "cdContextIup", {}, P),
--  xcdContextPrinter       = iup_c_func(hCd, "cdContextPrinter", {}, P),
--  xcdContextPS            = iup_c_func(hCd, "cdContextPS", {}, P),
--  xcdContextPicture       = iup_c_func(hCd, "cdContextPicture", {}, P),
--  xcdContextGL            = iup_c_func(hCdGL,"cdContextGL", {}, P),
--  xcdContextIupDBuffer    = iup_c_func(hCdIup,"cdContextIupDBuffer", {}, P)
--
----DEV/doc:
---- Note: The constants CD_IUP etc are initialised with abtruse values: by "abtruse" (not a real word) I mean a value designed to trigger an error 
----        if used directly, that makes some sense when debugging said error, and is substituted (within the pGUI wrapper) by a proper value, ie/eg 
----        CD_IUP is {"CD_IUP"}, which is automatically replaced with the result of cdContextIup(). Should you need the proper values outside pGUI,
----        use the corresponding function call (search the source of pGUI.e to determine what that is).
--global constant
--  CD_IUP          = c_func(xcdContextIup, {}),
--  CD_PRINTER      = c_func(xcdContextPrinter, {}),
--  CD_PS           = c_func(xcdContextPS, {}),
--  CD_PICTURE      = c_func(xcdContextPicture, {}),
--  CD_GL           = c_func(xcdContextGL, {}),
--  CD_IUPDBUFFER   = c_func(xcdContextIupDBuffer, {})
--
global function cdContextIup()
    iup_init_cd()
    return XCD_IUP
end function

--DEV more of the same...


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
atom w = allocate(24)
atom h = w+4
atom w_mm = h+4
atom h_mm = w_mm+8

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
----DEV...
----    xcdInitContextPlus  = iup_c_proc(hCd, "cdInitContextPlus", {})
--  xcdInitContextPlus  = define_c_proc(hCd, "cdInitContextPlus", {})
--
--DEV docs (and cdCanvan)
--global function cdCreateCanvas(atom hCdContext, atom_string data, sequence params={})
global function cdCreateCanvas(object hCdContext, atom_string data, sequence params={})
    iup_init_cd()
    if sequence(hCdContext) then
        hCdContext = XCDS[find(hCdContext[1],XCD_STR)]
    end if
    if length(params) then
        data = sprintf(data,params)
    end if
    cdCanvan hCdCanvas = c_func(xcdCreateCanvas,{hCdContext,data})
    return hCdCanvas
end function

global procedure cdKillCanvas(cdCanvas hCdCanvas)
    c_proc(xcdKillCanvas, {hCdCanvas})
end procedure

global function cdCanvasGetContext(cdCanvas hCdCanvas)
    atom hCdContext = c_func(xcdCanvasGetContext, {hCdCanvas})
    return hCdContext
end function

global procedure cdCanvasActivate(cdCanvas hCdCanvas)
    if c_func(xcdCanvasActivate, {hCdCanvas})!=CD_OK then ?9/0 end if
end procedure

global function cdActiveCanvas()
    iup_init_cd()
    cdCanvas res = c_func(xcdActiveCanvas,{})
    return res
end function

global procedure cavas_deactivate(cdCanvas hCdCanvas)
    c_proc(xcdCanvasDeactivate, {hCdCanvas})
end procedure

global procedure cdUseContextPlus(bool use)
    iup_init_cd()
    c_proc(xcdUseContextPlus, {use})
end procedure

global procedure cdInitContextPlus()
    iup_init_cd()
    c_proc(xcdInitContextPlus, {})
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
global function context_register_callback(atom hCdContext, integer cb, integer cbFunc)
    return c_func(xcdContextRegisterCallback, {hCdContext, cb, cbFunc})
end function

--DEV doc: (atom->object)
global function cdContextCaps(object hCdContext)
    iup_init_cd()
    if sequence(hCdContext) then
        hCdContext = XCDS[find(hCdContext[1],XCD_STR)]
    end if
    return c_func(xcdContextCaps, {hCdContext})
end function

-- ??????
global function canvas_play(cdCanvas hCdCanvas, atom hCdContext, integer xmin, integer xmax, integer ymin, integer ymax, atom_string data)
    integer res = c_func(xcdCanvasPlay, {hCdCanvas, hCdContext, xmin, xmax, ymin, ymax, data})
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
global function cdContextImImage()
    iup_init_cd()
--  return c_func(xcdContextImImage,{})
    return XCD_IMIMAGE
end function

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
--constant
--  xcdCanvasSimulate       = iup_c_func(hCd, "cdCanvasSimulate", {P,I},I),
--  xcdCanvasFlush          = iup_c_proc(hCd, "cdCanvasFlush", {P}),
--  xcdCanvasClear          = iup_c_proc(hCd, "cdCanvasClear", {P}),
--  xcdCanvasSaveState      = iup_c_func(hCd, "cdCanvasSaveState", {P},P),
--  xcdCanvasRestoreState   = iup_c_proc(hCd, "cdCanvasRestoreState", {P,P}),
--  xcdCanvasReleaseState   = iup_c_proc(hCd, "cdReleaseState", {P}),
--  xcdCanvasSetAttribute   = iup_c_proc(hCd, "cdCanvasSetAttribute", {P,P,P}),
--  xcdCanvasGetAttribute   = iup_c_func(hCd, "cdCanvasGetAttribute", {P,P},P)

global function canvas_simulate(cdCanvas hCdCanvas, integer mode)
    return c_func(xcdCanvasSimulate, {hCdCanvas, mode})
end function

global procedure cdCanvasFlush(cdCanvas hCdCanvas)
    c_proc(xcdCanvasFlush, {hCdCanvas})
end procedure

global procedure cdCanvasClear(cdCanvas hCdCanvas)
    c_proc(xcdCanvasClear, {hCdCanvas})
end procedure

global procedure cdClear()
    c_proc(xcdClear,{})
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

--void     cdCanvasSetfAttribute(cdCanvas* canvas, const char* name, const char* format, ...);
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
    if pMatrix!=NULL then
        free(pMatrix)
    end if
end procedure

global function cdCanvasGetTransform(cdCanvas hCdCanvas)
atom pMatrix
    pMatrix = c_func(xcdCanvasGetTransform, {hCdCanvas})
    if pMatrix=NULL then return NULL end if
    return iup_peek_double({pMatrix, 6})
end function

global procedure cdCanvasTransformMultiply(cdCanvas hCdCanvas, sequence matrix)
atom pMatrix
    pMatrix = allocate(6*8)
    iup_poke_double(pMatrix, matrix)
    c_proc(xcdCanvasTransformMultiply, {hCdCanvas, pMatrix})
    free(pMatrix)
end procedure

global procedure cdCanvasTransformRotate(cdCanvas hCdCanvas, atom angle)
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
----
----     clipping 
----
--------------------------------------------------------------------------------------------
--constant
--  xcdCanvasClip           = iup_c_func(hCd, "cdCanvasClip", {P,I},I),
--  xcdCanvasClipArea       = iup_c_proc(hCd, "cdCanvasClipArea", {P,I,I,I,I}),
--  xcdfCanvasClipArea      = iup_c_proc(hCd, "cdfCanvasClipArea", {P,D,D,D,D}),
--  xcdCanvasGetClipArea    = iup_c_func(hCd, "cdCanvasGetClipArea", {P,P,P,P,P},I),
--  xcdfCanvasGetClipArea   = iup_c_func(hCd, "cdfCanvasGetClipArea", {P,P,P,P,P},I)

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
----
----    clipping region 
----
--------------------------------------------------------------------------------------------
--constant
--  xcdCanvasIsPointInRegion    = iup_c_func(hCd, "cdCanvasIsPointInRegion", {P,I,I},I),
--  xcdCanvasOffsetRegion       = iup_c_proc(hCd, "cdCanvasOffsetRegion", {P,I,I}),
--  xcdCanvasGetRegionBox       = iup_c_proc(hCd, "cdCanvasGetRegionBox", {P,P,P,P,P,P}),
--  xcdCanvasRegionCombineMode  = iup_c_func(hCd, "cdCanvasRegionCombineMode", {P,I},I)

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

global procedure cdCanvasPixel(cdCanvas hCdCanvas, atom x, atom y, atom color)
    c_proc(xcdCanvasPixel, {hCdCanvas, x, y, color})
end procedure

global procedure canvas_mark(cdCanvas hCdCanvas, atom x, atom y)
    c_proc(xcdCanvasMark, {hCdCanvas, x, y})
end procedure

global procedure cdCanvasLine(cdCanvas hCdCanvas, atom x1, atom y1, atom x2, atom y2)
    c_proc(xcdCanvasLine, {hCdCanvas, x1, y1, x2, y2})
end procedure

global procedure cdLine(atom x1, atom y1, atom x2, atom y2)
    c_proc(xcdLine, {x1, y1, x2, y2})
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

global procedure cdCanvasRect(cdCanvas hCdCanvas, atom xmin, atom xmax, atom ymin, atom ymax)
    c_proc(xcdCanvasRect, {hCdCanvas, xmin, xmax, ymin, ymax})
end procedure

global procedure cdCanvasBox(cdCanvas hCdCanvas, atom xmin, atom xmax, atom ymin, atom ymax)
    c_proc(xcdCanvasBox, {hCdCanvas, xmin, xmax, ymin, ymax})
end procedure

global procedure cdCanvasArc(cdCanvas hCdCanvas, atom xc, atom yc, atom w, atom h, atom a1, atom a2)
    c_proc(xcdCanvasArc, {hCdCanvas, xc, yc, w, h, a1, a2})
end procedure

global procedure cdCanvasSector(cdCanvas hCdCanvas, atom xc, atom yc, atom w, atom h, atom angle1, atom angle2)
    c_proc(xcdCanvasSector, {hCdCanvas, xc, yc, w, h, angle1, angle2})
end procedure

global procedure canvas_chord(cdCanvas hCdCanvas, atom xc, atom yc, atom w, atom h, atom a1, atom a2)
    c_proc(xcdCanvasChord, {hCdCanvas, xc, yc, w, h, a1, a2})
end procedure

global procedure cdCanvasText(cdCanvas hCdCanvas, atom x, atom y, string text)
    c_proc(xcdCanvasText, {hCdCanvas, x, y, text})
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

global procedure f_canvas_text(cdCanvas hCdCanvas, atom x1, atom y1, string text)
    c_proc(xcdfCanvasText, {hCdCanvas, x1, y1, text})
end procedure

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

global procedure cdCanvasSetForeground(cdCanvas hCdCanvas, atom color)
    c_proc(xcdCanvasSetForeground, {hCdCanvas, color})
end procedure

global function cdCanvasGetForeground(cdCanvas hCdCanvas)
    integer color = c_func(xcdCanvasForeground, {hCdCanvas, CD_QUERY})
    return color
end function

global procedure cdSetForeground(atom color)
    c_proc(xcdForeground, {color})
end procedure

global function cdGetForeground()
    return cdCanvasGetForeground(cdActiveCanvas())
end function

global procedure cdCanvasSetBackground(cdCanvas hCdCanvas, atom color)
    c_proc(xcdCanvasSetBackground, {hCdCanvas, color})
end procedure

global function cdCanvasGetBackground(cdCanvas hCdCanvas)
    return c_func(xcdCanvasBackground, {hCdCanvas, CD_QUERY})
end function

global procedure cdSetBackground(atom color)
    c_proc(xcdBackground, {color})
end procedure

global function cdGetBackground()
    return cdCanvasGetBackground(cdActiveCanvas())
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

--[integer prevstyle = cdCanvasInteriorStyle(cdCanvas canvas, integer style) [default style of CD_SOLID should be fine]]
--/*
--DEV implement/doc:
int cdCanvasInteriorStyle(cdCanvas* canvas, int style)
Configures the current style for the area filling primitives: CD_SOLID, CD_HOLLOW, CD_HATCH, CD_STIPPLE or CD_PATTERN. (0..4)
Note that only CD_HATCH and CD_STIPPLE are affected by the backopacity. It returns the previous value. 
Default value: CD_SOLID. Value CD_QUERY simply returns the current value.

If a stipple or a pattern were not defined, when they are selected the state of the attribute is not changed. 

When the style CD_HOLLOW is defined, functions cdBox and cdSector behave as their equivalent cdRect and cdArc+Lines, and the polygons with style CD_FILL behave like CD_CLOSED_LINES.
--*/
global function cdCanvasInteriorStyle(cdCanvas hCdCanvas, integer style)
    integer prev = c_func(xcdCanvasInteriorStyle, {hCdCanvas, style})
    return prev
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
    iup_init_cd()
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

global procedure cdSetNativeFont(string font)
    atom pFont = c_func(xcdNativeFont, {font})
end procedure

global function cdGetNativeFont()
    atom pFont = c_func(xcdNativeFont, {CD_QUERY})
    string font = peek_string(pFont)
    return font
end function

global function cdCanvasTextAlignment(cdCanvas hCdCanvas, integer alignment)
    integer prev_alignment = c_func(xcdCanvasTextAlignment, {hCdCanvas, alignment})
    return prev_alignment
end function

global function cdTextAlignment(integer alignment)
    integer prev_alignment = c_func(xcdTextAlignment, {alignment})
    return prev_alignment
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
----
----    vector text
----
-------------------------------------------------------------------------------------------
--constant
--  xcdCanvasVectorText             = iup_c_proc(hCd, "cdCanvasVectorText", {P,I,I,P}),
--  xcdCanvasMultiLineVectorText    = iup_c_proc(hCd, "cdCanvasMultiLineVectorText", {P,I,I,P})

global procedure canvas_vector_text(cdCanvas hCdCanvas, atom x, atom y, string text)
    c_proc(xcdCanvasVectorText, {hCdCanvas, x, y, text})
end procedure

global procedure cdCanvasMultiLineVectorText(cdCanvas hCdCanvas, atom x, atom y, string text)
    c_proc(xcdCanvasMultiLineVectorText, {hCdCanvas, x, y, text})
end procedure

-----------------------------------------------------------------------------------------
----
---- vector text attributes
----
-------------------------------------------------------------------------------------------
--constant
--  xcdCanvasVectorFont             = iup_c_func(hCd, "cdCanvasVectorFont", {P,P},P),
--  xcdCanvasVectorTextDirection    = iup_c_proc(hCd, "cdCanvasVectorTextDirection", {P,I,I,I,I}),
--  xcdCanvasVectorTextTransform    = iup_c_func(hCd, "cdCanvasVectorTextTransform", {P,P},P),
--  xcdCanvasVectorTextSize         = iup_c_proc(hCd, "cdCanvasVectorTextSize", {P,I,I,P}),
--  xcdCanvasVectorCharSize         = iup_c_func(hCd, "cdCanvasVectorCharSize", {P,I},I)

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

global procedure cdCanvasVectorTextSize(cdCanvas hCdCanvas, atom w, atom h, string text)
    c_proc(xcdCanvasVectorTextSize, {hCdCanvas, w, h, text})
end procedure

global function canvas_vector_char_size(cdCanvas hCdCanvas, atom size)
    return c_func(xcdCanvasVectorCharSize, {hCdCanvas, size})
end function

-----------------------------------------------------------------------------------------
----
----    vector text properties 
----
-------------------------------------------------------------------------------------------
--constant
--  xcdCanvasGetVectorTextSize      = iup_c_proc(hCd, "cdCanvasGetVectorTextSize", {P,P,P,P}),
--  xcdCanvasGetVectorTextBounds    = iup_c_proc(hCd, "cdCanvasGetVectorTextBounds", {P,P,I,I,P})

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

--global function cdCanvasGetTextBox(cdCanvas hCdCanvas, atom x, atom y, string text)
--atom pXmin, pXmax, pYmin, pYmax
--sequence box
--  pXmin = allocate(32)
--  pXmax = pXmin+8
--  pYmin = pXmax+8
--  pYmax = pYmin+8
--  c_proc(xcdfCanvasGetTextBox, {hCdCanvas, x, y, text, pXmin, pXmax, pYmin, pYmax})
--  box = iup_peek_double({pXmin, 4})
--  free(pXmin)
--  return box
--end function

global function cdCanvasGetTextBox(cdCanvas hCdCanvas, atom x, atom y, string text)
atom pXmin, pXmax, pYmin, pYmax
sequence box
    pXmin = allocate(16)
    pXmax = pXmin+4
    pYmin = pXmax+4
    pYmax = pYmin+4
    c_proc(xcdCanvasGetTextBox, {hCdCanvas, x, y, text, pXmin, pXmax, pYmin, pYmax})
    box = peek4s({pXmin, 4})
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
----
---- color 
----
-------------------------------------------------------------------------------------------
--constant
--  xcdCanvasPalette = iup_c_proc(hCd, "cdCanvasPalette", {P,I,P,I})

global procedure canvas_palette(cdCanvas hCdCanvas, sequence palette, integer mode)
atom pPalette

    pPalette = allocate(4*length(palette))
    poke4(pPalette, palette)
    c_proc(xcdCanvasPalette, {hCdCanvas, length(palette), pPalette, mode})
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


global procedure canvas_put_image_rect_map(cdCanvas hCdCanvas, atom iw, atom ih, sequence index, sequence colors,
                                           atom x, atom y, atom w, atom h, 
                                           atom xmin, atom xmax, atom ymin, atom ymax)
atom pIndex, pColors
    pColors = allocate(4*256+length(index))
    pIndex = pColors+4*256
    poke4(pColors, colors)
    poke(pIndex, index)
    c_proc(xcdCanvasPutImageRectMap, {hCdCanvas, iw, ih, pIndex, pColors, x, y, w, h, xmin, xmax, ymin, ymax})
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
----
---- color 
----
-------------------------------------------------------------------------------------------
--constant
--  xcdEncodeColor  = iup_c_func(hCd, "cdEncodeColor", {UC,UC,UC},L),
--  xcdDecodeColor  = iup_c_proc(hCd, "cdDecodeColor", {L,P,P,P}),
--  xcdDecodeAlpha  = iup_c_func(hCd, "cdDecodeAlpha", {L},UC),
--  xcdEncodeAlpha  = iup_c_func(hCd, "cdEncodeAlpha", {L,UC},L),
--  xcdRGB2Map      = iup_c_proc(hCd, "cdRGB2Map", {I,I,P,P,P,P,I,P})

global function cdEncodeColor(integer red, integer green, integer blue)
    integer color = c_func(xcdEncodeColor, {red, green, blue})
    return color
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

global procedure wd_canvas_text(cdCanvas hCdCanvas, atom x, atom y, string text)
    c_proc(xwdCanvasText, {hCdCanvas, x, y, text})
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
        sequence index, sequence colors,
        atom x, atom y, atom w, atom h,
        atom xmin, atom xmax,
        atom ymin, atom ymax)

atom pIndex, pColors
    pColors = allocate(4*256+length(index))
    pIndex = pColors+4*256
    poke4(pColors, colors)
    poke(pIndex, index)
    c_proc(xwdCanvasPutImageRectMap, {hCdCanvas, iw, ih, pIndex, pColors, x, y, w, h, xmin, xmax, ymin, ymax})
    free(pColors)
end procedure

global procedure wd_canvas_put_bitmap(cdCanvas hCdCanvas, atom hCdBitmap, atom x, atom y, atom w, atom h)
    c_proc(xwdCanvasPutBitmap, {hCdCanvas, hCdBitmap, x, y, w, h})
end procedure

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

global procedure wd_canvas_vector_text_direction(cdCanvas hCdCanvas, atom x1, atom y1, atom x2, atom y2)
    c_proc(xwdCanvasVectorTextDirection, {hCdCanvas, x1, y1, x2, y2})
end procedure

global procedure wd_canvas_vector_text_size(cdCanvas hCdCanvas, atom w, atom h, string text)
    c_proc(xwdCanvasVectorTextSize, {hCdCanvas, w, h, text})
end procedure

global function wd_canvas_vector_char_size(cdCanvas hCdCanvas, atom size)
    return c_func(xwdCanvasVectorCharSize, {hCdCanvas, size})
end function

global function wd_canvas_get_vector_text_size(cdCanvas hCdCanvas, string text)
atom pX, pY
sequence x_y

    pX = allocate(16)
    pY = pX+8
    c_proc(xwdCanvasGetVectorTextSize, {hCdCanvas, text, pX, pY})
    x_y = iup_peek_double({pX, 2})
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


global procedure wd_canvas_vector_text(cdCanvas hCdCanvas, atom x, atom y, string text)
    c_proc(xwdCanvasVectorText, {hCdCanvas, x, y, text})
end procedure

global procedure wd_canvas_multi_line_vector_text(cdCanvas hCdCanvas, atom x, atom y, string text)
    c_proc(xwdCanvasMultiLineVectorText, {hCdCanvas, x, y, text})
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

global function IupPPlot(sequence attributes={}, sequence data={})
    if not did_pplot_open then
        pplot_open()
    end if

    Ihandle ih = c_func(xIupPPlot, {})

    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
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
    xIupPlotSetSample,
    xIupPlotSetSampleStr,
    xIupPlotSetSampleSelection,
    xIupPlotTransform,
    xIupPlotTransformTo,
    xIupPlotFindSample,
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
        xIupPlotSetSample           = iup_c_proc(hIupPlot, "IupPlotSetSample", {P,I,I,D,D})
        xIupPlotSetSampleStr        = iup_c_proc(hIupPlot, "IupPlotSetSampleStr", {P,I,I,P,D})
        xIupPlotSetSampleSelection  = iup_c_proc(hIupPlot, "IupPlotSetSampleSelection", {P,I,I,I})
        xIupPlotTransform           = iup_c_proc(hIupPlot, "IupPlotTransform", {P,D,D,P,P})
        xIupPlotTransformTo         = iup_c_proc(hIupPlot, "IupPlotTransformTo", {P,D,D,P,P})
        xIupPlotFindSample          = iup_c_proc(hIupPlot, "IupPlotFindSample", {P,D,D,P,P})
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

global function IupPlot(string attributes="", sequence data={})
    if not did_plot_open then
        IupPlotOpen()
    end if
    Ihandle ih = c_func(xIupPlot, {})
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global procedure IupPlotBegin(Ihandle ih, bool str_xdata=false)
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
atom pX = allocate(count*8)
atom pY = allocate(count*8)
--  if count<length(x) then ?9/0 end if
--  if length(x)!=length(y) then ?9/0 end if
    iup_poke_double(pX, x[1..count])
    iup_poke_double(pY, y[1..count])
    c_proc(xIupPlotInsertSamples, {ih, index, sample_index, pX, pY, count})
    free(pX)
    free(pY)
end procedure

global procedure IupPlotInsertStrSamples(Ihandle ih, integer index, integer sample_index, sequence xstrings, sequence y, integer count)
atom pX = allocate(count*machine_word())
atom pY = allocate(count*8)
    iup_poke_string_pointer_array(pX, xstrings)
    iup_poke_double(pY, y)
    c_proc(xIupPlotInsertStrSamples, {ih, index, sample_index, pX, pY, count})
    free(pX)
    free(pY)
end procedure

global procedure IupPlotAddSamples(Ihandle ih, integer index, sequence x, sequence y, integer count)
atom pX = allocate(count*8)
atom pY = allocate(count*8)
    iup_poke_double(pX, x)
    iup_poke_double(pY, y)
    c_proc(xIupPlotAddSamples, {ih, index, pX, pY, count})
    free(pX)
    free(pY)
end procedure

global procedure IupPlotAddStrSamples(Ihandle ih, integer index, sequence xstrings, sequence y, integer count)
atom pX = allocate(count*machine_word())
atom pY = allocate(count*8)
    iup_poke_string_pointer_array(pX, xstrings)
    iup_poke_double(pY, y)
    c_proc(xIupPlotAddStrSamples, {ih, index, pX, pY, count})
    free(pX)
    free(pY)
end procedure

--void IupPlotGetSample(Ihandle *ih, int ds_index, int sample_index, double *x, double *y);
global function IupPlotGetSample(Ihandle ih, integer ds_index, integer sample_index)
atom pXY = allocate(8*2)
sequence res
    c_proc(xIupPlotGetSample, {ih, ds_index, sample_index, pXY, pXY+8})
    res = iup_peek_double({pXY,2})
    free(pXY)
    return res
end function

--void IupPlotGetSampleStr(Ihandle *ih, int ds_index, int sample_index, const char* *x, double *y);
global function IupPlotGetSampleStr(Ihandle ih, integer ds_index, integer sample_index)
atom pX = allocate(8*2)
atom pY = allocate(8)
sequence res
    c_proc(xIupPlotGetSampleStr, {ih, ds_index, sample_index, pX, pY})
    res = {peek_string(pX),iup_peek_double(pY)}
    free(pX)
    free(pY)
    return res
end function

--int IupPlotGetSampleSelection(Ihandle *ih, int ds_index, int sample_index);
global function IupPlotGetSampleSelection(Ihandle ih, integer ds_index, integer sample_index)
    bool selected = c_func(xIupPlotGetSampleSelection, {ih, ds_index, sample_index})
    return selected
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
global procedure IupPlotSetSampleSelection(Ihandle ih, integer ds_index, integer sample_index, bool selected)
    c_proc(xIupPlotSetSampleSelection, {ih, ds_index, sample_index, selected})
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
atom p_ds_index = allocate(machine_word())
atom p_sample_index = allocate(machine_word())
    if c_func(xIupPlotFindSample, {ih, x, y, p_ds_index, p_sample_index}) then return 0 end if
    integer ds_index = peekNS(p_ds_index,machine_word(),0)
    integer sample_index = peekNS(p_sample_index,machine_word(),0)
    free(p_ds_index)
    free(p_sample_index)
    return {ds_index, sample_index}
end function

--void IupPlotPaintTo(Ihandle ih, cdCanvas cnv); 
global procedure IupPlotPaintTo(Ihandle ih, cdCanvas cnv)
    c_proc(xIupPlotPaintTo, {ih, cnv})
end procedure

--void IupPlotSetFormula(Ihandle* ih, int sample_count, const char* formula, const char* init); 
--global procedure IupPlotSetFormula(Ihandle ih, integer sample_count, string formula, nullable_string init)
--  c_proc(xIupPlotSetFormula, {ih, sample_count, formula, init})
--end procedure

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

global function IupGLCanvas(object action=NULL, object func=NULL, sequence attributes="", dword_seq data={})
    {action,func,attributes,data} = paranormalise(action,func,attributes,data)
    IupGLCanvasOpen()
    Ihandle ih = c_func(xIupGLCanvas, {NULL})
    if func!=NULL then
        if action=NULL then
            action = "ACTION"
        end if
        IupSetCallback(ih, action, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

global procedure IupGLMakeCurrent(Ihandle ih)
    iup_init_iupgl()
    c_proc(xIupGLMakeCurrent, {ih})
end procedure

global function IupGLIsCurrent(Ihandle ih)
    iup_init_iupgl()
    bool res = c_func(xIupGLIsCurrent, {ih})
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

global function control(string prog_id="", string attributes="", sequence data={})
    if not did_ole_open then
        ole_open()
    end if

    Ihandle ih = c_func(xIupOleControl, {prog_id})

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
    xIupTuioOpen    = iup_c_proc(hTuio, "IupTuioOpen", {}),
    xIupTuioClient  = iup_c_func(hTuio, "IupTuioClient", {I},P)

integer did_tuio_open = 0

procedure tuio_open()
    did_tuio_open = 1

    c_proc(xIupTuioOpen, {})
end procedure

global function client(integer port = 3333, sequence attributes={}, sequence data={})
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

global constant EXIT_SUCCESS = 0
global constant EXIT_FAILURE = 1

-- allocate an image to memory
global function allocate_image(sequence data, integer cleanup=0)
atom buff = allocate_data(length(data), cleanup)
    poke(buff, data) return buff
end function


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
--      xIupLoad                          = iup_c_func(iup, "+IupLoad", {P}, P),
--      xIupLoadBuffer                    = iup_c_func(iup, "+IupLoadBuffer", {P}, P),
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
--      xIupFrame                         = iup_c_func(iup, "+IupFrame", {P}, P),
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
--      xIupFlatButton                    = iup_c_func(iup, "+IupFlatButton", {P}, P),
--      xIupSpin                          = iup_c_func(iup, "+IupSpin", {}, P),
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
--      xIupProgressDlg                   = iup_c_func(iup, "+IupProgressDlg", {}, P),
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
--      xIupElementPropertiesDialog       = iup_c_func(iup, "+IupElementPropertiesDialog", {P}, P),
--$

--if xIupSetCallback=0 then ?9/0 end if

global constant IUP_NAME = "IUP - Portable User Interface"
global constant IUP_DESCRIPTION = "Multi-platform Toolkit for Building Graphical User Interfaces"
global constant IUP_COPYRIGHT = "Copyright (C) 1994-2015 Tecgraf/PUC-Rio"
global constant IUP_VERSION = "3.16" /* bug fixes are reported only by IupVersion functions */
global constant IUP_VERSION_NUMBER = 316000
global constant IUP_VERSION_DATE = "2015/09/15" /* does not include bug fix releases */

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

----int IupClassMatch(Ihandle* ih, const char* classname);
global function IupClassMatch(atom ih, string classname)
    bool result = c_func(xIupClassMatch, {ih,classname})
    return result   -- true(1) or false(0)
end function

/************************************************************************/
/*                        Elements                                      */
/************************************************************************/

--Ihandle* IupScrollBox(Ihandle* child);
global function IupScrollBox(Ihandln child)
    Ihandle result = c_func(xIupScrollBox, {child})
    return result
end function

--Ihandle* IupExpander(Ihandle *child);
global function IupExpander(Ihandln child)
    Ihandle result = c_func(xIupExpander, {child})
    return result
end function

--Ihandle* IupDetachBox(Ihandle *child);
global function IupDetachBox(Ihandln child)
    Ihandle result = c_func(xIupDetachBox, {child})
    return result
end function

--Ihandle* IupBackgroundBox(Ihandle *child);
global function IupBackgroundBox(Ihandln child)
    Ihandle result = c_func(xIupBackgroundBox, {child})
    return result
end function

--Ihandle* IupLink(const char* url, const char* title);
global function IupLink(nullable_string url=NULL, nullable_string title=NULL, object action=NULL, object func=NULL, sequence attributes="", dword_seq data={})
    {action,func,attributes,data} = paranormalise(action,func,attributes,data)
    Ihandle ih = c_func(xIupLink, {url,title})
    if func!=NULL then
        if action=NULL then
            action = "ACTION"
        end if
        IupSetCallback(ih, action, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
    end if
    return ih
end function

--Ihandle* IupFlatButton(const char* title);
global function IupFlatButton(nullable_string title=NULL, object action=NULL, object func=NULL, sequence attributes="", dword_seq data={})
    {action,func,attributes,data} = paranormalise(action,func,attributes,data)
    Ihandle ih = c_func(xIupFlatButton, {title})
    if func!=NULL then
        if action=NULL then
            action = "ACTION"
        end if
        IupSetCallback(ih, action, func)
    end if
    if length(attributes) then
        IupSetAttributes(ih, attributes, data)
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
global function IupTreeSetUserId(atom ih, atom id, atom userid)
atom result = c_func(xIupTreeSetUserId, {ih,id,userid})
    return result
end function

--void* IupTreeGetUserId(Ihandle* ih, int id);
global function IupTreeGetUserId(atom ih, atom id)
atom result = c_func(xIupTreeGetUserId, {ih,id})
    return result
end function

--int IupTreeGetId(Ihandle* ih, void *userid);
global function IupTreeGetId(atom ih, atom userid)
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
global function IupProgressDlg()
    Ihandle result = c_func(xIupProgressDlg, {})
    return result
end function

--Ihandle* IupParamf(const char* format);
--global function IupParamf(nullable_string fmt=NULL)
--  Ihandle result = c_func(xIupParamf, {fmt})
--  return result
--end function

--Ihandle* IupParamBox(Ihandle* parent, Ihandle** params, int count);
--global function IupParamBox(Ihandle parent, atom params, integer count)
--  Ihandle result = c_func(xIupParamBox, {parent,params,count})
--  return result
--end function

--Ihandle* IupElementPropertiesDialog(Ihandle* elem);
global function IupElementPropertiesDialog(Ihandle elem)
    Ihandle ih = c_func(xIupElementPropertiesDialog, {elem})
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


--constant
--  xiupKeyCodeToName = iup_c_func(iup, "iupKeyCodeToName", {I},P),
--  $
--
global function iupKeyCodeToName(atom ch)
    atom pKeyName = c_func(xiupKeyCodeToName,{ch})
    return peek_string(pKeyName)
end function


--dev WIERD ERROR...
--constant atom hCd = iup_open_dll({"cd.dll","libcd.so","libcd.dylib"})
--constant atom hCdIup = iup_open_dll({"iupcd.dll","libiupcd.so","libiupcd.dylib"})

--constant xwdCanvasGetImageRGB = iup_c_proc(hCd, "wdCanvasGetImageRGB", {P,P,P,P,D,D,I,I})

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
    hIupProgressDlg = define_c_func(iuplib, "IupProgressDlg", {}, C_INT),

-- Associates several callbacks to an event.
-- To define each rid use the function Icallback.
-- Returns: the same widget handle.
--global function IupSetCallbacks(atom widget, sequence action, sequence rids)
--  return c_func(hIupSetCallbacks, {widget, allocate_string(action)} &
--      rids & {NULL})
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
-- object res res = IupGetAttribute(colordlg, "VALUE")
--
-- if sequence(res) then
--         IupMessage(title, "You chose: '" & res & "'")
-- else
--         IupMessage(title, "You cancelled") 
-- end if
--
-- IupClose()
-- </eucode>
global function IupColorDlg()
    return c_func(hIupColorDlg, {})
end function

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

-- Creates a progress dialog element. It is a predefined dialog for displaying
-- the progress of an operation.
-- The dialog is meant to be shown with the show functions IupShow or IupShowXY.
--
-- Returns: the identifier of the created element, or NULL if an error occurs.
global function IupProgressDlg()
    return c_func(hIupProgressDlg, {})
end function

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
IupLoad
IupLoadBuffer
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

