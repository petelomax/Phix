--
-- pTrace.e
-- ========
--
-- The Phix debugger.
--  Author: Pete Lomax
--
-- This file is part of p[w].exe and implements source level debugging.
-- It is invoked via the optable and is therefore in builtins\VM, but
-- the relevant entries in said may be meaningless for .exe files other
-- than p[w].exe
--
constant WINDERS = platform()=WINDOWS -- (simplify dbg/port to LINUX)
--constant WINDERS = false

--DEV to do:
--      handle /* */ and --/* --*/ backticks treblequotes...
--      Integrate with Edita (or implement a cross-platform gui)
--      Dynamic screen size [IN PROGRESS]
--      Save/restore session info (new key rqd to clear it).
--      Allow inspection of s[i]
--
-- I should hardly need to say this, but you cannot use pTrace.e to debug
--  the code in pTrace.e; asking the trace routine to trace itself will 
--  inevitably lead to an infinite loop. During the very early stages of
--  development I had a test harness to get the display etc right. However
--  it quickly became more confusing than helpful, what with two programs
--  using the same tricks to save/restore nearly identical screen images.
--  Likewise, if running "p p test" (aka "p.exe p.exw test.exw") had both 
--  a pTrace.e in p.exe active on p.exw AND a pTrace.e in p.exw active on
--  test.exw, utter mayhem would ensue. Instead only the first is active.
--  Consequently and as an exception to the normal edit/run behaviour, all 
--  development and improvements are done "live", in suitably small steps, 
--  and without relying on trace(), but thankfully, at least, pdiag.e can
--  report any fatal run-time errors in this code as normal.
--
without trace
--with trace -- do not be surprised if this crashes!
--
-- After recompiling, run "p test\trace" to check behaviour. Note that you
--  cannot use "p p test\trace" as that persists with the "old" version in
--  p.exe and completely ignores any trace events in test\trace.
--
--DEV
-- The trace() function is only available during interpretation; it is not
--  and cannot be part of a compiled application, other than in p[w].exe. 
-- Any thoughts of "trace a compiled application" suggest that there is a 
--  behavioural discrepancy which should be directly addressed as a bug in
--  Phix. (I know of no such cases, but it is only reasonable to expect 
--  such to crop up every now and again, hopefully occuring quite rarely 
--  and promptly fixed.)
--
-- Differences with RDS/OpenEu:
--  The Phix debugger allows paging up and down the source and scrolling
--  left and right. Consequently downarrow is move down one line in Phix
--  rather than step over, which is F8 in Phix.
--
-- Technical note:
--  This should be coded as defensively as possible, rather than relying on
--  pdiag.e to give meaningful errors. This means (without going overboard)
--  that variables should be defined as object and then explicitly tested 
--  for the expected type (insted of being declared as the expected type 
--  and relying on the builtin type checking), that all subscripts should
--  be explicitly tested to be in range, and that all peeks checked first 
--  with xIsBadReadPtr, plus anything else you can think of!
--
-- **************************************
-- **** WARNING: FRAGILE CODE AHEAD! ****
-- **************************************
--
-- Any bugs in this code may be significantly harder to locate than similar 
-- errors in normal application code! This cannot debug itself!
--
-- No formal support is offered for changes made to this file; that will only
--  happen after I accept and incorporate your changes into the next official
--  release, and I will be very strict about the quality and quantity of code
--  that I accept.
--
-- One thing I seriously don't want to bother with is replicating that awful
-- "auto-display" of vars in the lower box. It may look cool to a novice, but
-- it is damn irritating to an experienced user as stuff they are trying to
-- watch suddenly vanishes. There is no equivalent of eu.ex's ops display_var,
-- erase_private_names|symbol, or update_globals, instead it is entirely the 
-- responsibility of this program to keep all vars up-to-date. I guess that
-- is mostly just a matter of a list of symtab indexes, although admittedly 
-- pruning such when the callstack shrinks may be a tad trickier.
--
-- A nice idea might be to allow double click on a visible variable name in
-- the on-screen source and/or a drop-down/search/tree-like listing.
--

--
-- Simple dos screen debugger outline for eu.ex.
-- Author Pete Lomax 17/12/2005
--

--
-- "without debug" suppresses the creation of line & file info, and hence
-- the dumping of local vars. While a compiled p.exe will not invoke trace
-- on itself, interpreting p.exw is a different matter; you might get away
-- with commenting out this statement (without debug), but attempting e.g.
-- to profile pTrace.e while an interpreted p.exw is attempting to profile
-- /trace a user app is bound to lead to trouble. Of course, removing this
-- (on a temporary basis only!) may leave some vital extra clues in ex.err
-- but asking the trace routine to trace itself will inevitably lead to an
-- infinite loop. Perhaps counter-intuitively, pdiag.e lends itself better
-- to self-analysis. In some cases, not setting/testing the K_wdb flag may
-- offer the vital hints needed to track down some thorny issue. YMMV.
--

--
-- NOTE:
-- -----
--  The symtab retrieved by opGetST is *not a real sequence*. 
--  Some of the entries may cause havoc: [S_il] is a real machine addresses and
--  may have bit 31 set, [S_value] may be "unassigned". While you may have some 
--  success when examining these it is not generally a safe thing to attempt.
--  Attempting eg to print the full contents of symtab will probably crash.
--  (Instead run "p -d test", with dumpSymTab=1 in plist.e, to view symtab.)
--
--  It is absolutely NOT POSSIBLE to modify symtab, at least not in hll code.
--
--UGH: old stuff:?
--  See docs/trace.htm.
--
-- Interface:
-- =========
--  show_trace(source,line) is called when a line trace event occurs.
--
--
--constant DOSdebug=1 -- 1=DOS, else Edita (not yet available) 
        -- the above should be in an ini file, or better a one-line include file,
        -- (or even better yet a new command line option)
        -- so it can still be changed under program control and prevent the
        -- windows part of this file from being compiled.

--/**/-- not strictly necessary, but reduces opCallOnce/fwd calls/onDeclaration
--/**/include image.e   -- save_text_image()

--include file.e    -- for seek()
--include machine.e -- allocate_string()
--include dll.e
--include misc.e    -- needed for WIN32
--include graphics.e    -- needed for BLACK etc
--include builtins\graphics.e -- video_config()
--include builtins\dll.e        -- open_dll() etc
--include builtins\image.e  -- save_text_image()

constant swod = 0   -- show without debug entries

function minI(integer a, integer b)
    if a<b then return a else return b end if
end function

function maxI(integer a, integer b)
    if a>b then return a else return b end if
end function

object SymTab
integer active_routine
atom --static_base,     -- where all globals and file-level vars live
--   ebp,               -- stack frame (at the point opLnt executed)
     vsb_root,          -- block containing ebp
     saved_root,        -- saved "" (for step over/out)
     saved_ebp,         --                  ""
     prev_ebp           --                  ""

integer ds4             -- start of data section/4 (as symtab[T_ds4], when interpreted ([T_EBP]!=0))
integer ebp4

integer prevcol =  1    -- previous value of column

integer column  =  1    -- column start display
integer toEnd   =  0        -- 1 if "End" just keyed

integer topline = -99   -- line at top of screen
integer ptrline = -99   -- line with "==>" /shown/

constant TRUE   = (1=1),
         FALSE  = (2=4)

integer debugOn
        debugOn = FALSE -- true if debug screen is showing

sequence runScreen, runPos, 
         dbgScreen, dbgPos, 
--       blankScreen = {}
         blankScreen

--DEV rename as traceon?
integer tracelevel = 0

--atom runBuffer,dbgBuffer  --DEV surely this would be better!

integer initD
        initD = 0

-- CONSOLE_SCREEN_BUFFER_INFO structure (xCSBI):
constant
    CSBI_SIZEX  = 0,    --  COORD      dwSize; 
    CSBI_SIZEY  = 2,
--  CSBI_CPOSX  = 4,    --  COORD      dwCursorPosition; 
--  CSBI_CPOSY  = 6,
    CSBI_ATTR   = 8,    --  WORD       wAttributes; 
--  CSBI_WINX1  = 10, --    SMALL_RECT srWindow; 
    CSBI_WINY1  = 12,
--  CSBI_WINX2  = 14,
    CSBI_WINY2  = 16,
--  CSBI_MAXX   = 18, --    COORD      dwMaximumWindowSize; 
--  CSBI_MAXY   = 20,
    sizeof_CSBI = 22,
    STD_OUTPUT_HANDLE = -11 -- #FFFFFFF5

integer screenLines, screenCols, 
        attr, 
        maxwid = 80,
        vararea = 4

sequence consoleSize

atom xGetStdHandle,
     xGetConsoleScreenBufferInfo,
     xSetConsoleScreenBufferSize,
     xSetConsoleTextAttribute,
     xGetKeyState,
     xIsBadReadPtr,
     xGetConsoleMode,
     xSetConsoleMode,
     stdout,
     xCSBI,
     pMode

procedure dinit()

    puts(1,"") -- ensure console exists
    if WINDERS then
        atom xKernel32 = open_dll("kernel32.dll"),
             xUser32   = open_dll("user32.dll")
        xGetStdHandle = define_c_func(xKernel32,"GetStdHandle",
            {C_LONG},   --  DWORD  nStdHandle   // input, output, or error device
            C_PTR)      -- HANDLE
        xGetConsoleScreenBufferInfo = define_c_func(xKernel32,"GetConsoleScreenBufferInfo",
            {C_PTR,     --  HANDLE  hConsoleOutput, // handle of console screen buffer
             C_PTR},    --  PCONSOLE_SCREEN_BUFFER_INFO  // address of screen buffer info
            C_INT)      -- BOOL
        xSetConsoleScreenBufferSize = define_c_func(xKernel32,"SetConsoleScreenBufferSize",
            {C_PTR,     --  HANDLE  hConsoleOutput, // handle of console screen buffer
             C_LONG},   --  COORD  coordSize    // new size in character rows and cols.
            C_INT)      -- BOOL
        xSetConsoleTextAttribute = define_c_func(xKernel32,"SetConsoleTextAttribute",
            {C_PTR,     --  HANDLE  hConsoleOutput, // handle of console screen buffer
             C_LONG},   --  WORD  wAttr         // text and background colors 
            C_INT)      -- BOOL
        xGetKeyState = define_c_func(xUser32,"GetKeyState",
            {C_INT},    --  int  nVirtKey       // virtual-key code
            C_INT)      -- SHORT
        xIsBadReadPtr = define_c_func(xKernel32,"IsBadReadPtr",
            {C_PTR,     --  CONST VOID  * lp,   // address of memory block
             C_INT},    --  UINT  ucb   // size of block
            C_INT)      -- BOOL
        xGetConsoleMode = define_c_func(xKernel32,"GetConsoleMode",
            {C_PTR,     --  HANDLE  hConsole,   // handle of console input or screen buffer
             C_PTR},    --  LPDWORD  lpMode     // current mode flags 
            C_INT)      -- BOOL
        xSetConsoleMode = define_c_func(xKernel32,"SetConsoleMode",
            {C_PTR,     --  HANDLE  hConsole,   // handle of console input or screen buffer
             C_LONG},   --  DWORD  fdwMode      // input or output mode to set 
            C_INT)      -- BOOL

        xCSBI = allocate(sizeof_CSBI)
        pMode = allocate(4)
        stdout = c_func(xGetStdHandle,{STD_OUTPUT_HANDLE})

        if not c_func(xGetConsoleScreenBufferInfo,{stdout,xCSBI}) then ?9/0 end if
        screenCols = peek2u(xCSBI+CSBI_SIZEX)
        screenLines = peek2u(xCSBI+CSBI_SIZEY)
        maxwid = screenCols-8
        dbgPos = {1,1}
        blankScreen = {}
        --
        -- 31/12/09...
        -- On Windows XP, the default console size is 80x25, with a screen buffer
        --  of 80x300, though you can change those in the properties dialogue.
        --  The problem is that at that size, a call to ReadConsoleOutput fails 
        --  with ERROR_NOT_ENOUGH_MEMORY (= 8) so here I reduce the number of 
        --  lines until it fits.
        --
        while 1 do
            consoleSize = {screenLines,screenCols}
--?{"pTrace.e dinit() line 293, consoleSize:",consoleSize}
            object dScreen = save_text_image({1,1},consoleSize)
            if sequence(dScreen) then
                dbgScreen = dScreen
                exit
            end if
            screenLines -= 1
        end while
        if not c_func(xSetConsoleScreenBufferSize,{stdout,screenLines*#10000+screenCols}) then
            puts(1,"error setting console size\n")
            ?consoleSize
            if getc(0) then end if
        end if
--?{"pTrace.e dinit() line 305, consoleSize:",consoleSize}

        --
        -- Although we save the entire screen buffer, base the
        --  display on the number of visible lines.
        --  DEV: To-do: re-check this periodically to allow the 
        --       debug window to be resized...
        --       (could only do so on +/- keystrokes?)
        --
        screenLines = peek2u(xCSBI+CSBI_WINY2)-peek2u(xCSBI+CSBI_WINY1)+1

--      free(xCSBI)

    else
        --DEV more...
        screenLines = 25
        screenCols = 80
        blankScreen = {}
        dbgScreen = {}
        dbgPos = {1,1}
        consoleSize = {screenLines,screenCols}
    end if
    initD = 1
end procedure

procedure debug_screen(integer on)
--
-- if required save/restore the current run-time/debug screen
--
--  if not initD then dinit() end if -- NO!
    if initD then
        if on and not debugOn then
            if WINDERS then
                poke4(xCSBI+CSBI_ATTR,-1)
                if not c_func(xGetConsoleScreenBufferInfo,{stdout,xCSBI}) then
                    puts(1,"oops:debug_screen\n")
                end if
                attr = peek2u(xCSBI+CSBI_ATTR)
                runPos = get_position()
                runScreen = save_text_image({1,1},consoleSize)
                display_text_image({1,1},dbgScreen)
            end if
            position(1,1)   -- ensure top line is visible
            position(dbgPos[1],dbgPos[2])
            debugOn = TRUE
        elsif not on and debugOn then
            dbgPos = get_position()
            if WINDERS then
                dbgScreen = save_text_image({1,1},consoleSize)
                display_text_image({1,1},runScreen)
                position(runPos[1],runPos[2])
                if c_func(xSetConsoleTextAttribute,{stdout,attr}) then end if
            end if
            debugOn = FALSE
        end if
    end if
end procedure

--function clear_debug()    -- invoked from the backend VM, eg during puts(1,"hello")
--  debug_screen(0)
--  return 1
--end function

-- Adjust to suit your monitor and your taste.
--  (DEV? save/load from an ini file?)
constant
    NORMAL_COLOR = BLACK, -- GRAY might look better         -- (0)
    COMMENT_COLOR = RED,
    KEYWORD_COLOR = BLUE,
    BUILTIN_COLOR = MAGENTA,
    STRING_COLOR = GREEN, -- BROWN might look better
    BRACKET_COLOR = {NORMAL_COLOR, YELLOW, BRIGHT_WHITE, 
                     BRIGHT_BLUE, BRIGHT_RED, BRIGHT_CYAN, 
                     BRIGHT_GREEN}

constant
        DIGIT           =  1,
        LETTER          =  2,   -- should be first two
        STRING          =  3,
        COMMENT         =  4,
        OTHER           =  5,
        WHITESPACE      =  6,
        OPENBRACKET     = 98,   -- should be last two
        CLOSEBRACKET    = 99

sequence charMap
    charMap = repeat(OTHER,255)
    charMap[' '] = WHITESPACE
    charMap['\t'] = WHITESPACE
    charMap['\r'] = WHITESPACE
    charMap['"'] = STRING
    charMap['\''] = STRING
    charMap['('] = OPENBRACKET
    charMap[')'] = CLOSEBRACKET
    charMap['['] = OPENBRACKET
    charMap[']'] = CLOSEBRACKET
    charMap['{'] = OPENBRACKET
    charMap['}'] = CLOSEBRACKET
    charMap['0'..'9'] = DIGIT
    charMap['A'..'Z'] = LETTER
    charMap['_'] = LETTER
    charMap['a'..'z'] = LETTER
    charMap['-'] = COMMENT

-- built in keywords and routines
--DEV:
--?: ilasm istype isinit isginfo MIN MAX MAXLEN
--forward public export enum ifdef elsifdef elsedef
--switch fallthru fallthrough jump_table case default break continue namespace
--?: licence strict profile profile_time trace warning type_check debug console gui

constant keywords = {
    "if", "then", "elsif", "else", "end", 
    "and", "or", "not", "xor",
    "global", "constant", "procedure", "function", "type", "return", 
    "for", "to", "by", "while", "do", "exit", 
    "include", "with", "without"}

constant builtins = {
    "integer", "atom", "sequence", "object",
    "append", "prepend", "compare", "equal", "find", "match",
    "clear_screen", "position", "pixel", "get_pixel", 
    "length", "floor", "rand", "remainder", "repeat", 
    "open", "close", "getc", "gets", "get_key", "print", "printf", "sprintf", "puts", 
    "sqrt", "sin", "cos", "tan", "log", "power", "arctan", 
    "and_bits", "or_bits", "not_bits", "xor_bits",
    "peek", "peek2s", "peek2u", "peek4s", "peek4u", "peek8s", "peek8u", 
    "poke", "poke4", "mem_copy", "mem_set",
    "call", "c_proc", "c_func", "routine_id", "call_proc", "call_func", 
    "machine_func", "machine_proc", "system", "system_exec", "abort", 
    "command_line", "date", "time", "getenv", "platform", "profile", "trace"}

integer blatit,     -- set 1 when updating whole screen, via display_text_image(),
                    -- otherwise 0 means use puts(1,)
        blat_bk,    -- background colour
        blat_txt,   -- text colour
        blat_X,     -- current blat X coordinate
        blat_Y      -- current blat Y coordinate

procedure set_colours(integer bkclr, integer txtclr)
    if tracelevel=1 then
        blat_bk = bkclr
        blat_txt = txtclr
    end if
end procedure

procedure blat(sequence txt)
integer x, attr, lt
sequence bl
    if not blatit then
        if tracelevel=1 then
            bk_color(blat_bk)
            text_color(blat_txt)
        end if
        puts(1,txt)
    else
        if not WINDERS then ?9/0 end if
        attr = blat_bk*16+blat_txt
        x = blat_X*2-1
        bl = dbgScreen[blat_Y]
        dbgScreen[blat_Y] = 0
        lt = length(txt)
        if x+lt*2-1>length(bl) then
            lt = (length(bl)-x+1)/2
        end if
        for i=1 to lt do
            bl[x] = txt[i]
            x += 1
            bl[x] = attr
            x += 1
        end for
        dbgScreen[blat_Y] = bl
        blat_X += lt
    end if
end procedure

procedure blatpos(integer X, integer Y, sequence txt)
    if blatit then
        blat_X = X
        blat_Y = Y
    else
        position(Y,X)
    end if
    blat(txt)
end procedure

--DEV try nicking the one from Edita instead...
procedure SynColour(sequence oneline, integer lo)
--
-- Display the current line (from column onwards), syntax coloured.
--
-- I was going to use syncolor.e from ed.ex but it started being out-by-one
-- and demanding trailing \n etc, so I just rewrote it. It would probably
-- have been better though, since it has been extensively tested. OTOH, this
-- is not particularly messy.
--
integer current_colour,     -- for oneline[shown..chidx-1] (if non-null)
        new_colour,         -- for oneline[chidx..chidx2-1] & onwards
        ctype,              -- the current character type
        chidx,              -- start of current "token" 
        chidx2,             -- end of current "token"+1
        shown,              -- bit of oneline already shown/off left margin.
        ch,                 -- current character
        ch2,                -- next/work character
        bracket_level       -- as per syncolor.e

sequence word

    current_colour = NORMAL_COLOR
    new_colour = NORMAL_COLOR
    shown = column
    chidx = 1
    bracket_level = 1
    if tracelevel=2 then                    -- B/W
        chidx = length(oneline)+1
    else                                    -- Colour
        while chidx<=length(oneline) do
            ch = oneline[chidx]
            ctype = charMap[ch]
            chidx2 = chidx+1
            if ctype=LETTER then
                while chidx2<=length(oneline)
                  and charMap[oneline[chidx2]]<=LETTER do
                    chidx2 += 1
                end while
                word = oneline[chidx..chidx2-1]
                if    find(word,keywords) then  new_colour = KEYWORD_COLOR
                elsif find(word, builtins) then new_colour = BUILTIN_COLOR
                else                            new_colour = NORMAL_COLOR end if
            elsif ctype >= OPENBRACKET then
                if ctype = OPENBRACKET then
                    bracket_level += 1
                end if
                if bracket_level >= 1
                and bracket_level <= length(BRACKET_COLOR) then
                    new_colour = BRACKET_COLOR[bracket_level]
                else
                    new_colour = NORMAL_COLOR
                end if
                if ctype = CLOSEBRACKET then
                    bracket_level -= 1
                end if
            elsif ctype=COMMENT and chidx2<=length(oneline) and oneline[chidx2]='-' then
                chidx2 = length(oneline)+1
                new_colour = COMMENT_COLOR
            elsif ctype=STRING then
                while chidx2<length(oneline) do
                    ch2 = oneline[chidx2]
                    chidx2 += 1
                    if ch2=ch then exit end if          -- closing quote found
                    if ch2='\\' then chidx2 += 1 end if -- skip escaped chars
                end while
                new_colour = STRING_COLOR
            elsif ctype!=WHITESPACE then
                new_colour = NORMAL_COLOR
            end if
            if new_colour!=current_colour then
                if chidx>lo then
                    chidx = lo+1
                end if
                if chidx>shown then
                    blat(oneline[shown..chidx-1])
                    shown = chidx
                end if
                current_colour = new_colour
                if shown=column+maxwid then exit end if
                blat_txt = new_colour
            end if
            chidx = chidx2
        end while
    end if
    if chidx>lo then
        chidx = lo+1
    end if
    if chidx>shown then
        blat(oneline[shown..chidx-1])
    end if
    if chidx<column+maxwid then
        blat(repeat(' ',column+maxwid-chidx))
    end if
end procedure

procedure put1(integer fileno, integer line)
sequence oneline
integer lo
sequence ptr
integer l,k,tx,bkclr

    oneline = exptext[fileno][line]

    l = length(oneline)
    if l and oneline[l]='\n' then
        l -= 1
        oneline = oneline[1..l]
        while 1 do
            k = find('\t',oneline)
            if k = 0 then exit end if
--          tx = remainder(k,4)
            tx = remainder(k,8)
            if tx = 0 then
                oneline[k] = ' '
            else
--              oneline = oneline[1..k-1]&repeat(' ',5-tx)&oneline[k+1..l]
                oneline = oneline[1..k-1]&repeat(' ',9-tx)&oneline[k+1..l]
--              l += 4-tx
                l += 8-tx
            end if
        end while
        exptext[fileno][line] = oneline
    end if
    lo = minI(column+maxwid-1,length(oneline))
    if line=ptrline then
        ptr = "==>"
        bkclr = CYAN
    else
        ptr = ":  "
        bkclr = WHITE
    end if
    ptr = sprintf("%5d%s",{line,ptr})
    set_colours(bkclr, NORMAL_COLOR)
    blatpos(1,line-topline+2,ptr)
    if column>lo then
        ptr = repeat(' ',maxwid)
        blat(ptr)
    else
        SynColour(oneline,lo)
    end if
end procedure

integer vars_shown = 0
sequence Qnames = {}, 
         Qsymidx = {}, 
         Qrtn = {},     -- (only valid/used if symtab[Qsymidx[i]][S_NTyp]==S_TVar)
         Qvalues = {},  -- (textual representation)
         Qdone = {}

procedure addvar(integer symidx)
integer k
    k = find(symidx,Qsymidx)
    if k!=0 then
        Qnames[k..k] = {}
        Qsymidx[k..k] = {}
        Qrtn[k..k] = {}
        Qvalues[k..k] = {}
        Qdone[k..k] = {}
    end if
    Qnames = prepend(Qnames,symtab[symidx][S_Name])
    Qsymidx = prepend(Qsymidx,symidx)
    Qrtn = prepend(Qrtn,active_routine)
    Qvalues = prepend(Qvalues,"")
    Qdone = prepend(Qdone,0)
end procedure

integer lc  -- limit counter (set to 500)
integer showellipse -- set if lc blown
integer novalue

function getVal(atom addr)
object  result
integer vtyp, len
    novalue = 0         -- control flag, to prevent ppExf of "<no value>" result
    if machine_bits()=32 then
        result = peek4s(addr)
        if result<#40000000 then    -- a 31-bit integer
            return result
        end if
        result -= #40000000
    else
        result = peek8s(addr)
        if result<#4000000000000000 then    -- a 63-bit integer
            return result
        end if
        result -= #4000000000000000
    end if
    if result=0 then
        novalue = 1
        return "<no value>"
    end if
    addr = result*4

    if c_func(xIsBadReadPtr,{addr,1}) then
        novalue = -1
        result = sprintf("<**pTrace.e: bad ptr** (#%08x)>\n",addr)
        return result
    end if

    vtyp = peek(addr-1)
    if vtyp=#12 then        -- a 64-bit float
--DEV 80-bit float
        if machine_bits()=32 then
            result = peek({addr,8})
            return float64_to_atom(result)
        else
            result = peek({addr,10})
            return float80_to_atom(result)
        end if
    end if
    if machine_bits()=32 then
        len = peek4s(addr-12)
    else
        len = peek8s(addr-24)
    end if
    if vtyp=#82 then        -- an 8-bit ascii string
        if len>lc then
            len = lc
            lc = 0
            showellipse = 1
        end if
        return peek({addr,len})
    end if
    if vtyp!=#80 then       -- sanity check: must be a sequence then.
        novalue = 1
        result = sprintf("<**GARBAGE/CORRUPT TYPE BYTE** (#%02x at [#%08x])>\n",{vtyp,addr-1})
        puts(1,result)
        return result
    end if
    result = {}
    while len and lc do
        lc -= 1
        len -= 1
        result = append(result,getVal(addr))
--DEV (untried)
--      addr += machine_word()
        if machine_bits()=32 then
            addr += 4
        else
            addr += 8
        end if
    end while
    if len then
        showellipse = 1
    end if
    return result
end function

function getValueX(integer symidx, integer limit)
object  o,
        ss   -- SymTab[symidx]
object si
integer nTyp, tidx

    lc = limit
    showellipse = 0
    si = SymTab[symidx]
    -- obviously none of these should ever happen, but if they do then leave
    --  as many clues as you can in the ex.err to help resolve things.
    if symidx<0 or symidx>length(SymTab) then
        return sprintf("pTrace:getValue bad symidx[=%d]",symidx)
    end if
    ss = SymTab[symidx]
    if atom(ss) then
        return sprintf("pTrace:symtab[symidx[=%d]] is an atom",symidx)
    end if
    nTyp = ss[S_NTyp]
    if nTyp>S_TVar or nTyp<S_Const then
        return sprintf("pTrace:getValue bad symtab[symidx][S_NTyp]=%d",nTyp)
    end if
--DEV 64-bit/see pdiagN.e
    if nTyp=S_TVar then
        tidx = ss[S_Tidx]
--      o = getVal(ebp+tidx*4)
        o = getVal(ebp4*4+tidx*4)
    else
        tidx = ss[S_Slink]
--      o = getVal(static_base+tidx*4-4)
--      o = getVal(ds4*4+tidx*4-4)
        o = getVal(ds4*4+tidx*4+16)
    end if
    if not novalue then
--DEV try that new routine here...?
        o = ppf(o)
        if showellipse then
            lc = find('\n',o)
            if lc then o = o[1..lc-1] end if
            lc = length(o)
            if o[lc]='}' then
                o[lc..lc] = ",...}"
            else
                o &= "..."
            end if
        end if
    end if
    return o
end function

integer F3help = 0
constant helptext = {{
"F3=toggle help, F6=animate, F7/Enter=step into, F8=step over, F9=step out",
"(page)up/down, left/right, (shift)tab=navigate source; home/end=fully tab",
"+=more varspace, -=less varspace, !=abort (create ex.err), ?=var lookup",
"q=resume execution,  Q=permanently"},{
--345678901234567890123456789012345678901234567890123456789012345678901234567890
"'3'=toggle help, '6'=animate, '7'/Enter=step into, '8'=step over, '9'=out",
"(upper=page)u=up/n=down, h=left/j=right, tab=navigate source; H=home",
"+=more varspace, -=less varspace, !=abort (create ex.err), ?=var lookup",
"q=resume execution,  Q=permanently"}}
constant ENABLE_WRAP_AT_EOL_OUTPUT = 2

procedure showvars(integer limit)
integer WrapMode
integer symidx
sequence oneline, fmt, onevar, name, valstr
integer lo, itemlen, padding
integer varline
object o

    if WINDERS then
        if not c_func(xGetConsoleMode,{stdout,pMode}) then ?9/0 end if
        WrapMode = peek4u(pMode)
        if and_bits(WrapMode,ENABLE_WRAP_AT_EOL_OUTPUT) then
            -- (This is only needed for the last line. Alternately we could limit
            --  that to 79 chars (using (80-(i=vararea)) instead of 80, I guess).)
            if not c_func(xSetConsoleMode,{stdout,WrapMode-ENABLE_WRAP_AT_EOL_OUTPUT}) then ?9/0 end if
        end if
    end if

    varline = 1
    if F3help then
        set_colours(BLACK, WHITE)
        integer hdx = (2-WINDERS)
        for i=1 to length(helptext[hdx]) do
            oneline = helptext[hdx][i]
            oneline &= repeat(' ',80-length(oneline))
            blatpos(1,screenLines-vararea+i,oneline)
            varline += 1
            if varline>vararea then exit end if
        end for
    else
        for i=1 to length(Qnames) do
            symidx = Qsymidx[i]
            if symtab[symidx][S_NTyp]=S_TVar and Qrtn[i]!=active_routine then
                Qdone[i] = 1  -- out of scope/hide (see lesson 1 in test\trace.exw)
            else
                Qdone[i] = 0
--              Qvalues[i] = getValueX(symidx,100)
                o = getValueX(symidx,100)
                Qvalues[i] = o
            end if          
        end for
        set_colours(BLACK, WHITE)
        while find(0,Qdone)!=0 do
            oneline = ""
            lo = 0
            for i=1 to length(Qnames) do
                if Qdone[i]=0 then
                    name = Qnames[i]
                    valstr = Qvalues[i]
                    itemlen = length(name)+1+length(valstr)
                    if lo=0
                    or lo+itemlen<80 then --DEV <=? (test)
                        padding = 20-remainder(itemlen,20)
                        if padding>=2 
                        and itemlen<80 then
                            fmt = "%s = %s%s"
                            padding -= 2
                        else
                            fmt = "%s=%s%s"
                        end if
                        onevar = sprintf(fmt,{name,valstr,repeat(' ',padding)})
                        oneline &= onevar
                        Qdone[i] = 1
                        lo = length(oneline)
                        if lo>=80 then exit end if
                    end if
                end if
                if limit then exit end if
            end for
            if lo<80 then
                oneline &= repeat(' ',80-lo)
            elsif lo>80 then
                oneline = oneline[1..78]&".."
            end if
            if varline=vararea and find(0,Qdone)!=0 then
                oneline[78..80] = "..."
            end if
            blatpos(1,screenLines-vararea+varline,oneline)
            if limit then exit end if
            varline += 1
            if varline>vararea then exit end if
        end while
    end if
    -- lastly blank out any remaining lines
    oneline = repeat(' ',80)
--  oneline = repeat(' ',79)
    if vars_shown>=vararea then
--      vars_shown = vararea-1
        vars_shown = vararea
    end if
    for i=varline to vars_shown do
        blatpos(1,screenLines-vararea+i,oneline)
    end for
    vars_shown = varline-1
    if WINDERS then
        position(1,1)
        if and_bits(WrapMode,ENABLE_WRAP_AT_EOL_OUTPUT) then
            if not c_func(xSetConsoleMode,{stdout,WrapMode}) then ?9/0 end if
        end if
    end if
end procedure


integer lastfileno
        lastfileno = -1
integer k
integer laff    -- length(allfiles[fileno])
        laff = -1

procedure update_screen(integer fileno, integer lineno, integer ascurrline)
integer maxline
integer prevptr
sequence ff -- copy of filenames[fileno]
integer lf  -- length(ff) [scratch var]

    maxline = topline+(screenLines-vararea-2)
    if lineno>=topline and lineno<maxline
    and column=prevcol
    and toEnd=0 
    and fileno=lastfileno then
        --
        -- just correct two lines
        --
        if ascurrline then
            blatit = 0
--          set_colours(WHITE, NORMAL_COLOR)
            prevptr = ptrline
            ptrline = lineno
            if prevptr>=topline and prevptr<maxline then
                -- clear prev "==>" if still on screen
                put1(fileno, prevptr)
            end if
            put1(fileno,lineno)
        end if
    else
        lastfileno = fileno
        --
        -- display whole screenful of text
        --
        if WINDERS then
            blatit = 1
            if length(blankScreen)=0 then
--if not initD then ?9/0 end if -- 14/2/19
                clear_screen()
                blankScreen = save_text_image({1,1},consoleSize)
            end if
            dbgScreen = blankScreen
        else
            blatit = 0
        end if
--DEV!
        if not toEnd then
--?     if ascurrline then
-- 1) work out filelength-screenlines-vararea thing
-- 2) if topline<1 or >"" then
--    if lineno<topline or lineno>topline+??
--      ? = lineno-7
--      IF ?<work then topline=? else topline=work

--          if prevcol!=column then
                prevcol = column
--              if lineno<topline or lineno>maxline then
--                  topline = max(lineno-7,1)
--              end if
--          else
                topline = maxI(lineno-7,1)
                k = ptrline-(screenLines-vararea-2)
                if k>=topline and k<lineno then
                    topline = k+1
                end if
--          end if
        end if
        ff = filenames[fileno]
        ff[1] = filepaths[ff[1]]
--DEV not down-arrow, F3=help maybe?
--  F6=animate  F7=step into  F8=step over  F9=step out  q=resume execution, Q=permanently
--  (page)up/down, left/right, (shift)tab=navigate source; home/end=full left/right tab
--  +=more varspace  -=less varspace  !=abort (create ex.err)  ?=var lookup
--      ff = sprintf(" %s%s  F1=main  F2=trace  Enter down-arrow  ?  q  Q  !                      ",ff)
--ff={
        if WINDERS then
            ff = sprintf(" %s%s  F1=main  F2=trace  F3=help  ?  q  Q  ! ",ff)
        else
            ff = sprintf(" %s%s  '3'=help '7'=step '8'=step over  ?  q  Q  ! ",ff)
        end if
        lf = length(ff)
        if lf<80 then
            ff &= repeat(' ',80-lf)
        else
            while lf>80 do
                ff[1..4] = " .."
                lf -= 1
            end while
        end if
        set_colours(BLUE,WHITE)
        blatpos(1,1,ff)
        set_colours(WHITE, NORMAL_COLOR)
        if ascurrline then
            ptrline = lineno
        end if
        maxline = topline+(screenLines-vararea-3)
        while 1 do  -- loop for toEnd processing
            for i=topline to maxline do
                if i>laff then exit end if
                if toEnd then
                    --
                    -- first pass, make sure column is far enough over
                    -- so that the end of line will be shown
                    --
                    if length(exptext[fileno][i])>=column+maxwid-1 then
                        toEnd = 2
                        exit
                    end if
                else
                    put1(fileno,i)
                end if
            end for
            if toEnd=0 then exit end if
            toEnd -= 1
            if toEnd then
                column += 1
                prevcol = column    -- (minor: avoids a full redisplay on next step)
            end if
        end while
        if WINDERS then
            display_text_image({1,1},dbgScreen)
        end if
        blatit = 0
    end if
    if ascurrline then
        showvars(0)
    end if
    position(lineno-topline+2,1)
    if tracelevel=1 then
        bk_color(BLACK)
        text_color(WHITE)
    end if
end procedure

function retD(atom ebp)
atom era        -- return address
    while 1 do
--DEV check me/64bit
        era = peek4u(ebp+16)        -- return address
        ebp = peek4u(ebp+20)
        if ebp=0 then return 0 end if
        if era!=0 then exit end if
    end while
    return ebp
end function

function scanGvar(integer gfile, sequence varname)
object si
    for i=T_constm1 to length(SymTab) do
        si = SymTab[i]
        if sequence(si)                     -- might not be dumped (unused)
        and equal(si[S_FPno],gfile)
        and equal(si[S_NTyp],S_GVar2)
--      and and_bits(si[S_State],K_wdb)     -- skip without debug items [DEV?]
        and equal(si[S_Name],varname) then
            return i
        end if
    end for
    return 0
end function

procedure var_lookup()
integer maxfiles
sequence scanned
integer segfile
object varname
sequence sr     -- symtab[rtn]
integer symidx
integer x
atom this_ebp
integer rtn
sequence matchset
integer k, lm
sequence si
sequence path,file
integer key

--DEV (2 lines 24/3/2013)
--  r = c_func(xGetConsoleScreenBufferInfo,{stdout,xCSBI})
integer y
    if WINDERS then
        if not c_func(xGetConsoleScreenBufferInfo,{stdout,xCSBI}) then ?9/0 end if
        y = peek2u(xCSBI+CSBI_WINY2)-peek2u(xCSBI+CSBI_WINY1)+1
        if y!=screenLines then
            --DEV force full redisplay here...
            screenLines = y
--          vars_shown = vararea
        end if
    end if
    x = screenLines-vararea
    position(x,1)
    if tracelevel=1 then
        bk_color(YELLOW)
        text_color(BLACK)
    end if
    puts(1,"variable name? ")
    puts(1,repeat(' ',65))
    position(x,16)
    varname = gets(0)           -- DEV another case for specialised gets(0)!!!
    if sequence(varname) 
    and length(varname) then
        if varname[length(varname)]='\n' then
            varname = varname[1..length(varname)-1]
        end if

        maxfiles = length(filenames)
        scanned = repeat(0,maxfiles)
        rtn = active_routine
        sr = symtab[rtn]
        symidx = sr[S_Parm1]
        while symidx do
            if symtab[symidx][S_Name]=varname then
                addvar(symidx)
                showvars(0)
                varname = ""
                exit
            end if
            symidx = symtab[symidx][S_Slink]
        end while

        if length(varname)!=0 then
            --
            -- not a local/parameter, scan the callstack
            --
--          this_ebp = ebp
            this_ebp = ebp4*4
            while 1 do
                if swod or and_bits(sr[S_State],K_wdb) then -- skip without debug items
                    segfile = sr[S_FPno]
                    if not scanned[segfile] then
                        scanned[segfile] = 1
                        symidx = scanGvar(segfile,varname)
                        if symidx!=0 then
                            addvar(symidx)
                            showvars(0)
                            varname = ""
                            exit
                        end if
                    end if
                end if  -- K_wdb
                this_ebp = retD(this_ebp)
                if this_ebp=0 then exit end if
                rtn = peek4u(this_ebp+8)
                sr = symtab[rtn]
            end while
        end if

        if length(varname)!=0 then
            --
            -- not local or in callstack, scan remaining global and local vars
            --
--DEV without this, get compiler funnies...
matchset = {}
            for i=1 to maxfiles do
                if not scanned[i] then
                    scanned[i] = 1
                    symidx = scanGvar(i,varname)
                    if symidx!=0 then
                        matchset &= symidx
                    end if
                end if
            end for
            lm = length(matchset)
            if lm=0 then
                puts(1," - not defined at this point")
            else
                symidx = matchset[1]
                addvar(symidx)
                if lm>1 then
                    k = 1
                    while 1 do
                        symidx = matchset[k]
                        si = symtab[symidx]
                        file = filenames[si[S_FPno]]
                        path = filepaths[file[1]]
                        file = file[2]
                        set_colours(YELLOW, BLACK)
                        blat(sprintf(" WARNING: showing match %d of %d [%s%s] (esc exits)   ",
                                     {k,lm,path,file}))
                        Qsymidx[1] = symidx
                        showvars(1)
                        set_colours(YELLOW, BLACK)
                        position((screenLines-vararea),16)
                        key = wait_key()
                        if key=27 then exit end if -- escape
                        blat(varname)
                        -- uparrow shows previous, any other key shows next
                        if key=328 then  k -= 1  if k<=0 then k = lm end if
                        else             k += 1  if k>lm then k = 1 end if
                        end if
                    end while
                    puts(1,repeat(' ',65))
                end if
                showvars(0)
            end if
        end if
    end if -- sequence(varname) and length(varname) check
end procedure

constant DOWN = +1, UP = -1
function move(integer line, object direction)
integer pageful, newline
integer y
    if WINDERS then
        if not c_func(xGetConsoleScreenBufferInfo,{stdout,xCSBI}) then ?9/0 end if
        y = peek2u(xCSBI+CSBI_WINY2)-peek2u(xCSBI+CSBI_WINY1)+1
        if y!=screenLines then
            --DEV force full redisplay here...
            screenLines = y
        end if
    end if

    if sequence(direction) then
        pageful = screenLines-vararea-2
        newline = line+pageful*direction[1]
    else
        newline = line+direction
    end if
--  if newline>=1 and newline<=laff then
--      line = newline
--  end if
    if newline<1 then
        newline = 1
--  elsif newline>laff-pageful then
    elsif newline>laff then
        newline = laff
    end if
--  return line
    return newline
end function

constant TAB = '\t'
constant VK_SHIFT = 16

procedure Tab()
-- note that wait_key() returns TAB('\t') whether shift is held down or not.
integer shift
    if WINDERS then
        shift = (floor(c_func(xGetKeyState,{VK_SHIFT})/2)!=0)
    else
        --DEV
        shift = 0
    end if
    if shift then
        if column>=5 then
            column -= 4
        else
            column = 1
        end if
    else
        column += 4
--?{"column",column}
    end if
end procedure

procedure varArea(integer adj)
    if (adj=-1 and vararea>1)
    or (adj=+1 and vararea<floor(screenLines/2)) then
        vararea += adj
        topline = -99
--      showvars(0)
    end if
end procedure

integer animate
        animate = 0

integer stepover
        stepover = 0

constant OVER = 0,
         OUT  = 1

procedure Step(integer flag)
    stepover = 1
    saved_root = vsb_root   
--  saved_ebp = ebp-flag
    saved_ebp = ebp4*4-flag
    prev_ebp = peek4u(ebp4*4+20)
end procedure


procedure abort_trace()
--DEV... see FatalN, do we need to restore ebp4/[ds+8]?
        -- calling convention
        --  mov ecx,imm32       -- no of frames to pop to obtain an era (>=1)
        --  mov al,imm          -- error code [1..length(msgs)-1, currently 122]
--      --  mov edi,ep1         -- [optional] (opUnassigned)
--      --  mov esi,ep2         -- [optional] (opUnassigned) [used for 110/ecx]
        --  jmp :!fatalN        -- fatalN(level,errcode,ep1,ep2)
--  ?9/0
--  e12pa
    #ilASM{
        [32]
            mov ecx,2
            mov al,12           -- e12pa
          @@:
--X         mov edx,[ebp+16]    -- era
--          mov edx,[ebp+28]    -- era
            mov edx,[ebp+12]    -- called from address (as set in opLnt below)
            mov ebp,[ebp+20]    -- (nb no local vars after this!)
            sub ecx,1
            jg @b
            sub edx,1
            jmp :!iDiag         -- fatal error (see pdiagN.e)
--          jmp :!fatalN        -- fatal error (see pdiagN.e)
            int3
        [64]
            mov rcx,2
            mov al,12           -- e12pa
          @@:
--X         mov rdx,[rbp+32]    -- era
--          mov rdx,[rbp+56]    -- era
            mov rdx,[rbp+24]    -- called from address (as set in opLnt below)
            mov rbp,[rbp+40]    -- (nb no local vars after this!)
            sub rcx,1
            jg @b
            sub rdx,1
            jmp :!iDiag         -- fatal error (see pdiagN.e)
--          jmp :!fatalN        -- fatal error (see pdiagN.e)
            int3
        []
          }
end procedure
--DEV integer?
--atom cbrDbg, cbrClrDbg, wascbrClrDbg

--integer traceon = 0
integer needclr = 0

string trace3
--string trace3 = join({
--"                                                                       ",
--"                                                                       ",
--"=== THE END ===                                                        ",
--"                                                                       ",
--"                                                                       ",
--"                                                                       ",},"\r\n")
integer trace3fn = 0, trace3pos

--function debug(atom fileno, atom line, atom trclvl)
--function debug(integer fileno, integer line, integer trclvl)
--procedure debug(integer line)
--DEV/SUG
procedure show_trace(integer line)
--
-- main command loop, invoked from opLnt
--
integer key, res, ascurrline
object crashmsg
--object crashfile
--sequence s8
atom vsb_prev
integer fileno

--maybe:
--  enter_cs()
--DEV
--printf(1,"show_trace(%d)\n",line)
    if rbldrqd then
--      rebuild_callback()
        --
        -- Invoke pemit2.e/rebuild_callback() if needed.
        -- Nicked from pdiagN.e, less the shadow copy of rbldrqd, 
        -- since that was working and the above hll call was not,
        -- plus of course this only occurs when interpreted. See
        -- pdiagN.e/pemit2, :!rbidscb/:!diagFrame for more details.
        --
--      rbldrqd = 0 -- NO!
        #ilASM{ 
            [32]
--DEV push/pop d/qword[ds+8] (not yet supported by pilasm.e)
                mov eax,[ds+8]
                push eax
                call :!rbidscb  -- (leaves return addr on stack)
                add esp,4       -- (discard "")
                pop eax
                mov [ds+8],eax
            [64]
                mov rax,[ds+8]
                push rax
                call :!rbidscb  -- (leaves return addr on stack)
                add rsp,8       -- (discard "")
                pop rax
                mov [ds+8],rax
            []
              }

    end if
    crashmsg = 0    -- get saved callstack
--DEV we don't want symtab, callstack -> ebp4 in #ilASM...
--DEV it may make sense, here and in pDiagN.e, to save/restore symptr for better diagnostics...
    #ilASM{ 
        [32]
            lea edi,[SymTab]
--          lea esi,[crashmsg]
--          lea ecx,[crashfile]
            call :%opGetST      -- [edi]:=symtab (see pStack.e)
            mov esi,[ebp4]
            lea edi,[vsb_root]
            mov edx,[ebx+esi*4+8]       -- rtn
            mov eax,[ebx+esi*4+24]      -- vsb_root
            mov [active_routine],edx
            call :%pStoreMint
        [64]
            lea rdi,[SymTab]
            call :%opGetST      -- [rdi]:=symtab (see pStack.e)
            mov rsi,[ebp4]
            lea rdi,[vsb_root]
            mov rdx,[rbx+rsi*4+16]      -- rtn
            mov rax,[rbx+rsi*4+48]      -- vsb_root
            mov [active_routine],rdx
            call :%pStoreMint
        []
          }

    -- (we have no use here for crashmsg/crashfile, but it costs nowt)

--printf(1,"active_routine = %d\n",active_routine)  -- 30/3/19: 0 (64 bit)
    fileno = symtab[active_routine][S_FPno]
--printf(1,"fileno = %d\n",fileno)
--?9/0 --DEV get this from prev_ebp...
--  s8 = symtab[T_callstk]  -- {ep1,ep2,era,etd,ern,ebp,vsb_root,dcount} (see pdiag.e for full details)
                            --   0   0   0                         0     (zero because of the si=1, btw) [DEV?]

--  active_routine = s8[5]
--DEV [rbp+8]
--  active_routine = peek4u(ebp4*4+4)
--  static_base = s8[4]
--DEV copied from pdiagN.e, cannot remember why... ah, from pglobals.e:
--              T_EBP       = 22,   -- compiled/listing=0, interpreted={ebp4,esp4,sym4} (set at last possible moment)
--              T_ds4       = 23,   -- compiled = start of data section, same but /4 when interpreted ([T_EBP]!=0)
--  if symtab[T_EBP]=0 then             -- compiled
--      ds4 = floor(symtab[T_ds4]/4)
--  else                                -- interpreted
        ds4 = symtab[T_ds4]
--  end if
--  ebp = s8[6]             -- current (at opLnt that is) stack frame
--  vsb_root = s8[7]        -- vsb_root containing "" ("")

    if stepover then
        -- return 1 (0 would turn off trace) to execute more user app:
        if vsb_root=saved_root then
            -- Same vsb, so direct frame pointer comparison tells all:
--          if ebp>saved_ebp then return 1 end if
            if ebp4*4>saved_ebp then return end if

--      elsif ebp!=prev_ebp then
        elsif ebp4*4!=prev_ebp then
            -- Not the same block and not the directly calling frame,
            --  so scan the vsb_prev chain checking for saved_root.
            -- This covers (at least) three cases:
            --  nested calls have spilled into a new vsb
            --  without debug/trace in effect in prev_ebp
            --  prev_ebp exited without hitting an opLnt
            vsb_prev = peek4u(vsb_root)
            while vsb_prev!=0 do
                if vsb_prev=saved_root then return end if
                vsb_prev = peek4u(vsb_prev)
            end while
        end if
        stepover = 0
    end if

--  tracelevel = trclvl

    needclr = 0
    if not initD then dinit() end if
--puts(1,"debug called...\n")
--?{fileno,line,tracelevel}

--DEV! (1371)
    -- stop the VM calling clear_debug() when we puts(1,xxx) etc 
    --  (from inside this source/until the other opSetDbg, that is)
--/*
    #ilASM{ mov edi,[cbrDbg]    -- (keep this the same, but)
            xor esi,esi         -- (replace a previously saved cbrClrDbg with 0)
>
            call %opSetDbg }    -- save edi,esi
--*/
--  wascbrClrDbg = cbrClrDbg
--  cbrClrDbg = 0

    if equal(expandedYet[fileno],0) then
        text = allfiles[fileno]
        exptext[fileno] = expandIntoLines()
        expandedYet[fileno] = linestarts
    end if
    laff = length(exptext[fileno])

    if tracelevel=3 then
--DEV
--      puts(1,"oops, trace(3) not yet supported (see pTrace.e)\n")
--      if getc(0) then end if
--      abort(1)
        if trace3fn=0 then
            trace3fn = open("ctrace.out","wb")
            trace3pos = 0
            trace3 = join({
"                                                                       ",
"                                                                       ",
"=== THE END ===                                                        ",
"                                                                       ",
"                                                                       ",
"                                                                       ",},"\r\n")
        else
            trace3pos += 73
            if trace3pos>505*73 then
                trace3pos = 0
            end if
        end if
--?{filenames[fileno][2],line,exptext[fileno][line]}
        string oneline = iff(line>length(exptext[fileno])?"???":trim(exptext[fileno][line],"\r\n"))
--      oneline = sprintf("%s:%d\t%s",{filenames[fileno][2],line,oneline})
        oneline = sprintf("%s:%-7d %s",{filenames[fileno][2],line,oneline})
        if length(oneline)>=71 then
            oneline = oneline[1..71]
        else
            oneline &= repeat(' ',71-length(oneline))
        end if
        trace3[1..71] = oneline
        if seek(trace3fn,trace3pos)!=SEEK_OK then
            puts(1,"seek() error in ctrace.out")
            ?9/0
        end if
        puts(trace3fn,trace3)
--DEV far too slow...
--      flush(trace3fn)
        return
    end if

    if not debugOn then
        debug_screen(1)
    end if
    if tracelevel=2 then
        blat_bk = BLACK
        blat_txt = WHITE
        bk_color(BLACK)
        text_color(WHITE)
    end if

    ascurrline = 1
    key = 0
    res = 1
    while 1 do
        if key!=315 then    -- F1
            update_screen(fileno,line,ascurrline)
            showvars(0)
        end if
        ascurrline = 0
        if animate then
            key = get_key()     -- (ie no wait)
            if key=-1 then exit end if  -- run a bit more user app then
            animate = 0
        end if
        key = wait_key()
        if not debugOn then --(if in F1)
            debug_screen(1)
            if not find(key,"!?qQ") then
                key = 0
            end if
        end if
        switch key do
            case 10,13,284: exit                                -- return (run to next opLnt)
            case 'n',336:   line = move(line,DOWN)              -- downarrow (NB "step over" is F8)
            case 'u',328:   line = move(line,UP)                -- uparrow
            case 'N',337:   line = move(line,{DOWN})            -- page down
            case 'U',329:   line = move(line,{UP})              -- page up
            case 'h',331:   column = maxI(column-1,1)           -- left arrow
            case 'j',333:   column += 1                         -- right arrow
            case 'H',327:   column = 1                          -- home
            case 'J',335:   column = 1 toEnd = 1                -- end
            case TAB:       Tab()                               -- tab and shift tab
            case '+':       varArea(+1)                         -- increase var area
            case '-':       varArea(-1)                         -- decrease var area
            case 'q':       res = 0 exit                        -- quit (resume normal execution)
            case 'Q':       res = -1 exit                       -- Quit ("", permanently)
            case '!':       abort_trace()                       -- abort (create ex.err)
            case '?':       var_lookup()
            case '1',315:   debug_screen(0)                     -- F1
            case '3',317:   F3help = 1-F3help                   -- F3 help
            case '6',320:   animate = 1 exit                    -- F6 (animate)
            case '7',321:   exit                                -- F7 (step into) [== return]
            case '8',322:   Step(OVER) exit                     -- F8 (step over)
            case '9',323:   Step(OUT) exit                      -- F9 (step out) [ie resume in callee]
            default:        printf(1,"unknown key (%d)\n",key)
        end switch
--/*
        if    key=10 then exit                                  -- 
        elsif key=13 then exit                                  -- return (run to next opLnt)
        elsif key=284 then exit                                 -- Enter
        elsif key=336 then line = move(line,DOWN)               -- downarrow (NB "step over" is F8)
        elsif key=328 then line = move(line,UP)                 -- uparrow
        elsif key=337 then line = move(line,{DOWN})             -- page down
        elsif key=329 then line = move(line,{UP})               -- page up
        elsif key=331 then column = maxI(column-1,1)            -- left arrow
        elsif key=333 then column += 1                          -- right arrow
        elsif key=327 then column = 1                           -- home
        elsif key=335 then column = 1 toEnd = 1                 -- end
        elsif key=TAB then Tab()                                -- tab and shift tab
        elsif key='+' then varArea(+1)                          -- increase var area
        elsif key='-' then varArea(-1)                          -- decrease var area
        elsif key='q' then res = 0 exit                         -- quit (resume normal execution)
        elsif key='Q' then res = -1 exit                        -- Quit ("", permanently)
--      elsif key='!' then res = -2 exit                        -- abort (create ex.err)
        elsif key='!' then abort_trace()                        -- abort (create ex.err)
        elsif key='?' then var_lookup()
        elsif key=315 then debug_screen(0)                      -- F1
--      elsif key=316 then debug_screen(1)                      -- F2 (any key will do this from the F1 state)
        elsif key=317 then F3help = 1-F3help                    -- F3 help
--      elsif key=318 then                                      -- F4 [spare]
--      elsif key=319 then                                      -- F5 [spare]
        elsif key=320 then animate = 1 exit                     -- F6 (animate)
        elsif key=321 then exit                                 -- F7 (step into) [== return]
        elsif key=322 then Step(OVER) exit                      -- F8 (step over)
        elsif key=323 then Step(OUT) exit                       -- F9 (step out) [ie resume in callee]
        end if
--*/
    end while
    if res<=0 then
        needclr = 0
        tracelevel = res
        debug_screen(0)
    else
        needclr = 1
    end if

--/*
    #ilASM{ mov edi,[cbrDbg]
            mov esi,[cbrClrDbg]
>
            call %opSetDbg }    -- save edi,esi
--*/
--  cbrClrDbg = wascbrClrDbg

--  return res
--end function
end procedure

--
-- Alternatively, trace in Edita (see peama.ew for clues)
-- Edita will run quite happily as is, until the debugger gives it a shout
--  (which is what the code below does). At that point, and only at that point,
--  Edita can [drastically] change it's behaviour.
-- Two-way communication, and timestamps, will be required.
--  The biggest problem is handling multiple copies/versions of files.
-- If lines are inserted/deleted in the source(s), Edita will have to make
--  appropriate adjustments when passed a line number:
--  save a table of line inserts/deletes by timestamp, and the timestamp of
--  the file on load. Edita does not need to do this until the debugger
--  give it the signal. If the timestamp on the source is after the timestamp
--  returned from this program then "timestamp error: the line numbers as
--  traced may not match those shown in the editor". If called on a line
--  which is known to have been deleted/amended then "debugged line has been 
--  deleted/amended", but carry on anyway. Edita can empty the line inserts/
--  deletes table if ipc_IsProcessRunning() returns false. 
--  This program should stall if ipc_RegisterProcessName is already running
--  otherwise Edita will not know which one to talk to, although at some
--  point (eg by passing a control sequence instead of a line number) we can
--  fix that and have several running at once, if the demand arises.
--  By no means easy, but the ability to edit sources during debugging as well
--  as examining other sources, performing global searches, etc. should make 
--  it worth doing.
-- The big downside to using Edita is of course that it is windows-only.
-- Edita will also need to split the screen to show variables etc.
--

--include ipc.ew
--constant path = `C:\Program Files\Edita` -- will need to prompt for/save this in an ini file.
--sequence filepath
--constant temp = `C:\Program Files\Phix\builtins\file.e`   -- always use fullpath
--constant AppName = "Edita"
--if not ipc_IsProcessRunning(path) then
--  filepath = path&".exw"
--  if length(dir(filepath))!=1 then
--      puts(1,filepath&" not found")   -- prompt?
--      if getc(0) then end if
--      abort(1)
--  end if
--  system("exw.exe "&filepath,2)
--end if
--constant xSetForegroundWindow = define_c_func(user32, "SetForegroundWindow", {C_POINTER}, C_POINTER )
--void = c_func(xSetForegroundWindow,{ipc_CallFunc(path,AppName&"File",{temp,12})})

--if 01 then
--puts(1,"setup\n")
--      cbrDbg = routine_id("show_trace")
--      if cbrDbg=-1 then puts(1,"cbrDbg=-1\n") abort(1) end if
--      cbrDbg = call_back(cbrDbg)
--      cbrClrDbg = routine_id("clear_debug")
--      if cbrClrDbg=-1 then puts(1,"cbrClrDbg=-1\n") abort(1) end if
--      cbrClrDbg = call_back(cbrClrDbg)

--
-- Note: This is a one-shot thing.
--       If p.exe has already opSetDbg, then
--       p.exe p.exw will not supplant it.....
--       You cannot test pTrace.e "live" in interpreted mode, unless
--       you build a p.exe without the setup call below first.
--  In other words:
--      p.exe test.exw obeys trace() commands in test.exw (doh),
--      p.exe p.exw test.exw obeys trace() commands in p.exw, but
--          *completely ignores* any in test.exw. This is because
--          the (active) instance of pTrace.e [being the one which
--          was earlier compiled into p.exe] has all the sources of 
--          p.exw to look at, NOT the sources of test.exw. Attempts 
--          to have two (active) copies of pTrace.e, one using the 
--          sources of p.exw and one using the sources of test.exw, 
--          but both using the same physical screen, is a bit silly 
--          when you stop and think about it anyway, although I can
--          accept that (a) only one might have any trace commands,
--          and (b) we could possibly hibernate the p.exw one over
--          the opInterp call for test.exw.
--      Likewise, p.exe p.exw p.exw p.exw test.exw only traces the
--          first p.exw, not the second, third, or test.exw. It is
--          of course at this point things could get really silly.
--      If this raises objections, maybe we could have a "-nodebug"
--          command line option to replace the "if 01 then" above,
--          and then maybe get "p -nodebug p -nodebug p test" to
--          trace test.exw...
-- Update/Clarification 12/07/13 what actually happens is:
--  ; pTrace.e calls opSetDbg,cbrDbg,cbrClrDbg initially, and
--  ;                opSetDbg,cbrDbg,0 to disable clear_debug temporarily, then
--  ;                opSetDbg,cbrDbg,cbrClrDbg to re-enable clear_debug.
--  in opSetDbg, once cbrDbg has been set you *cannot* change it, and further
--  you must re-pass the *same* value of cbrDbg to toggle cbrClrDbg on/off.
--
--/*
    #ilASM{ mov edi,[cbrDbg]
            mov esi,[cbrClrDbg]
>
            call %opSetDbg }    -- save edi,esi
--*/

--end if

--dev:
--!/*

--#ilASM{ jmp :fin
--#ilASM{ jmp :!opCallOnceYeNot (probably not)
#ilASM{ jmp :%opRetf
--#ilASM{ jmp :!opCallOnceYeNot

-- DEV: these need to be put in the optable... [DONE]

--/*
procedure :%opTrace(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opTrace
-------------
        [32]
            -- level in eax (0..3)
            -- This just sets tracelevel, it is opLnt which does the real work.
            -- calling convention:
            --  mov eax,[p1]
            --  call :%opTrace  -- trace(eax)
            cmp eax,3
            jbe @f
                pop edx
                mov al,86       -- e86attmbi03
                sub edx,1
                jmp :!iDiag
                int3
          @@:
            mov edi,[tracelevel]
--DEV:
--  cmp dword[ebpidx],4 ; only trace at top level 
--  jne @f      ; (ie "p.exe p.exw test.exw" traces p.exw not test.exw)
            cmp edi,-1  -- once set to -1 ('Q'), no trace ever again
            je @f
            cmp edi,eax
            je @f
--DEV: (just use the global in pemit2.e, plus a new flag, in show_trace() itself)
--              mov edx,[RbldIds]
--              xor ebx,ebx
--              test edx,edx
--              jz rblddone2
--              mov [RbldIds],ebx
--              push eax
--              call edx
--              pop eax
--            rblddone2:
                mov [tracelevel],eax
                call :%opClrDbg
--              cmp [needclr],ebx
--              je @f
--                  mov [needclr],ebx
--                  debug_screen(0)
--                  routine_id
         @@:
            ret
        [64]
            -- level in rax (0..3)
            -- This just sets tracelevel, it is opLnt which does the real work.
            -- calling convention:
            --  mov rax,[p1]
            --  call :%opTrace  -- trace(eax)
            cmp rax,3
            jbe @f
                pop rdx
                mov al,86       -- e86attmbi03
                sub rdx,1
                jmp :!iDiag
                int3
          @@:
            mov rdi,[tracelevel]
            cmp rdi,-1  -- once set to -1 ('Q'), no trace ever again
            je @f
            cmp rdi,rax
            je @f
                mov [tracelevel],rax
                call :%opClrDbg
         @@:
            ret
        []

--/*
procedure :%opLnt(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opLnt -- trace this line (only valid when interpreting)
-----------
        [32]
            -- calling convention:
            --  mov eax,imm32   -- line number
--          --  mov ecx,fileno  -- (you could do this as per opLnp/t, if it helps)
            --  call :%opLnt    -- show_trace(eax) [if traceon]
--          mov ecx,[tracelevel]
            mov edx,ebp
--          test ecx,ecx
            cmp [tracelevel],0
            jle :justret
                shr edx,2                       -- ebp4
                push eax                        --[1] lineno
                xor eax,eax
                call :%pWithJS
                mov [ebp4],edx                  -- (nb saved pre-pDebugN.e/show_trace())
                mov edx,routine_id(show_trace)  -- mov edx,imm32 (sets K_ridt)
                mov ecx,$_Ltot                  -- mov ecx,imm32 (=symtab[show_trace][S_Ltot])
                call :%opFrame
                mov edx,[esp+4]
                pop dword[ebp]                  --[1] lineno
--EXCEPT
--X             mov dword[ebp+16],:dbgret
                mov dword[ebp+28],:dbgret
                mov dword[ebp+12],edx           -- called from address
                jmp $_il                        -- jmp code:show_trace
              ::dbgret  

--              mov [needclr],1
----DEV do this in show_trace():
--              test eax,eax
--              jg @f
--                  mov [tracelevel],eax    -- 'q'(0), 'Q'(-1), or '!'(-2) keyed
--            @@:
--              pop ebp
                cmp eax,-2
--              je e12pa                -- program aborted
          ::justret
            ret
        [64]
            -- calling convention:
            --  mov rax,imm32   -- line number
            --  call :%opLnt    -- show_trace(eax) [if traceon]
            mov rdx,rbp
            cmp [tracelevel],0
            jle :justret
                shr rdx,2                       -- ebp4
                push rax                        --[1] lineno
                xor rax,rax
                call :%pWithJS
                mov [ebp4],rdx                  -- (nb saved pre-pDebugN.e/show_trace())
                mov rdx,routine_id(show_trace)  -- mov rdx,imm32 (sets K_ridt)
                mov rcx,$_Ltot                  -- mov rcx,imm32 (=symtab[show_trace][S_Ltot])
                call :%opFrame
                mov rdx,[rsp+8]
                pop qword[rbp]                  --[1] lineno
--EXCEPT
--X             mov qword[rbp+32],:dbgret
                mov qword[rbp+56],:dbgret
                mov qword[ebp+24],rdx           -- called from address
                jmp $_il                        -- jmp code:show_trace
              ::dbgret  
          ::justret
            ret
        []

--/*
procedure :%opClrDbg(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opClrDbg
--------------
--!/*
            -- calling convention:
            -- from deep within puts/getc and similar: [DEV not yet invoked properly, see "--DEV! (1371)" above]
            --  call :%opClrDbg
--/*
        [32]
            cmp [needclr],0
            je @f
                mov [needclr],0
                mov edx,routine_id(debug_screen)    -- mov edx,imm32 (sets K_ridt)
                mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[debug_screen][S_Ltot])
                call :%opFrame
                mov dword[ebp],ebx
X               mov dword[ebp+16],:clrdbgret
                mov dword[ebp+28],:clrdbgret
                jmp $_il                            -- jmp code:debug_screen(0)
              ::clrdbgret   
--*/
          }
--clear_debug()
            if needclr then
                needclr = 0
                debug_screen(0)
            end if
    #ilASM{
            ret
          }
--!*/
--/*
        [32]
            cmp [needclr],ebx
            je @f
--              mov eax,[cbrClrDbg]
                mov [needclr],ebx
--              call eax
--DEV
                mov edx,routine_id(debug_screen)    -- mov edx,imm32 (sets K_ridt)
                mov ecx,$_Ltot                      -- symtab[debug_screen][S_Ltot]
                call :%opFrame
                mov dword[ebp],ebx                  -- parameter 'on':=0
--X             mov [ebp+16],:justret
                mov [ebp+28],:justret
                jmp $_il                            -- (symtab[debug_screen][S_il])
        [64]
            pop rax
        []
          @@:
            ret

      }
--*/
--!*/

