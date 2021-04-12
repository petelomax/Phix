--
-- pwa\src\p2js_basics.e
-- ======================
--
--  standard includes, constants, globals, and utility routines for ../../pwa.exw
--
without debug -- (keep the ex.err reasonably clear, comment out if/when needed)
include pGUI.e
-- (while autoincludes,    """    for all these too:)
include builtins\VM\pcmdlnN.e
include builtins\pcurrdir.e
include builtins\pgetpath.e
include builtins\pfile.e
include builtins\VM\pprntfN.e
include builtins\get_routine_info.e
include builtins\scanf.e
include builtins\pdir.e
include builtins\penv.e
include builtins\ppp.e
include builtins\VM\pcfunc.e
include builtins\xml.e
include builtins\dict.e
include builtins\get_interpreter.e
include builtins\syswait.ew
with debug

--global constant pwadir = join_path({include_path(""),`pwa`})
global constant pwadir = join_path({get_file_path(get_interpreter()),`pwa`})
--global constant pwadir = `C:\Program Files (x86)\pwa`
--?pwadir

global string current_file
global string builtindir -- set in tokstack_clean()
global integer ext
--global string text
global string src
global integer lt   -- ==length(src)
global sequence textlines   -- (temp/debug)
sequence TOKTYPES
global sequence tokens
global integer src_offset   -- (for js embeddded in html)
global integer tdx -- index to tokens[]/terminate parser
global sequence clines

-- aside: pwa/p2js (as per the docs) does not support nested constants.
--/* -- backtrack, see p2js.exw
enum PHIX, JSS, HTML, C --, CSS, PYTH
constant extensions = {{PHIX, {"exw","ex","e","eu"}},
                       {JSS, {"js"}},
                       {HTML, {"html","htm"}},
                       {C, {"c"}}}
--                     {CSS, {"css"}}}
--*/
--global constant ext_names = {"phix","js","html","c","css","go","python"},
global constant ext_names = {"phix","js","html","c"},
                extensions = {{PHIX :=1, {"exw","ex","e","eu","ew"}},
                              {JSS  :=2, {"js"}}, -- (avoid clash with platform()'s JS)
                              {HTML :=3, {"html","htm"}},
                              {C    :=4, {"c"}}}
--                            {CSS  :=4, {"css"}},
--                            {GO   :=6, {"go"}},
--                            {PYTH :=7, {"py"}}}
--
-- note that both the tokeniser and the parser have a local phix_only() routine
--      that invokes is_phix(), before going on to invoke their own xxx_error().
--
global function is_phix() return ext==PHIX              end function
global function is_js()   return ext==JSS or ext==HTML  end function
global function is_html() return ext==HTML              end function
--global function is_css()  return ext==CSS                 end function
--global function is_go()   return ext==GO                  end function
global function is_C()    return ext==C                 end function
--global function is_py()   return ext==PYTH                end function

--/*
-- don't think this helped... (the better plan is html -> xml,
--                             then parse(xml[i],js/css) anyway,
--                             rather than HTML +=,-= JSS/CSS)
constant extensions = {{PHIX :=#01, {"exw","ex","e","eu"}},
                       {HTML :=#02, {"html","htm"}},
                       {CSS  :=#04, {"css"}},
                       {JSS  :=#08, {"js"}},
                       {C    :=#10, {"c"}}}
integer ext

--
-- note that both the tokeniser and the parser have a local phix_only() routine
--      that invokes is_phix(), before going on to invoke their own xxx_error().
-- also, (eg) html may temp. toggle ext to CSS/JSS at some point in the future.     
--
global function is_phix() return and_bits(ext,PHIX)!=0 end function
global function is_html() return and_bits(ext,HTML)!=0 end function
global function is_css()  return and_bits(ext,CSS )!=0 end function
global function is_js()   return and_bits(ext,JSS )!=0 end function
global function is_C()    return and_bits(ext,C   )!=0 end function
--*/

global function fatal(string msg)
--DEV IupMessage?
    puts(2,msg)
    {} = wait_key()
--  crash(msg)
    if CRASH then ?9/0 end if
    return false
end function

global function find_extension(string e)
    integer res = 0
    for i=1 to length(extensions) do
        if find(e,extensions[i][2]) then
            res = extensions[i][1]
            exit
        end if
    end for
    return res
end function

global function load_text(string txt, integer edx)
    src = txt
    ext = edx
-- temp/debug[?]: (maybe not...)
--  textlines = split(src,"\r\n")
    textlines = split(substitute(src,"\r\n","\n"),'\n',false)
--?{"length(textlines)",length(textlines)}
--integer h = length(textlines)
--while h>1 do
--  h = floor(h/2)
--  printf(1,"line %d:%s\n",{h,textlines[h]})
--end while
    lt = length(src)
    return true
end function

global procedure save_filename(string filename)
    current_file = filename -- (store for debugging purposes)
--?{"current_file",current_file}
end procedure

global function load_file(string filename)
    save_filename(filename)
    object txt = get_text(filename,GT_WHOLE_FILE)
    if not string(txt) then
        return fatal("unable to open "&filename)
    end if
    integer ext = find_extension(get_file_extension(filename))
    if ext=0 then
        return fatal("unsupported file extension: "&filename)
    end if
    return load_text(txt,ext)
end function

--
-- common to tokeniser and parser:
--
global enum TOKTYPE, TOKSTART, TOKFINISH, TOKLINE, TOKCOL, TOKALTYPE=$, TOKTTIDX, TOKENDLINE=$ -- (one token)
            -- TOKTYPE as below, if integer use tok_name(toktype) to get a human-readable string
            -- TOKTTIDX is only set on LETTER tokens and can be compared to T_integer, etc.
            -- TOKENDLINE only on '`' (aka `"""`) and BLK_CMT (no other tokens span lines)
            -- TOKALTYPE = TOKCOL is only/also used on parse tree LETTER/DIGIT/SYMBOL tokens.
--DEV/SUG
            -- note that [TOKFINISH] can be a string, for debugging purposes:
--/*
Searching for: {toktype,
 Files scanned 6, Directories scanned 1, Lines 2733
C:\Program Files (x86)\Phix\pwa\src\p2js_basics.e:126             -- note that [TOKFINISH] can be a string, for debugging purposes:
C:\Program Files (x86)\Phix\pwa\src\p2js_basics.e:197     integer {toktype,start,finish,line,col} = tok
C:\Program Files (x86)\Phix\pwa\src\p2js_parse.e:82         integer {toktype,start,finish,line} = tok
C:\Program Files (x86)\Phix\pwa\src\p2js_parse.e:111                         {toktype,start,finish} = tok
C:\Program Files (x86)\Phix\pwa\src\p2js_parse.e:157     integer {toktype,start,finish,line} = tok
C:\Program Files (x86)\Phix\pwa\src\p2js_parse.e:186 --  integer {toktype,start,finish,line} = tok
C:\Program Files (x86)\Phix\pwa\src\p2js_parse.e:233 --          {toktype,start,finish} = tok
C:\Program Files (x86)\Phix\pwa\src\p2js_parse.e:255     integer {toktype,start,finish,line} = tok
C:\Program Files (x86)\Phix\pwa\src\p2js_parse.e:389 --      integer {toktype,start,finish,line} = tok
C:\Program Files (x86)\Phix\pwa\src\p2js_parse.e:390         integer {toktype,start,finish} = tok
C:\Program Files (x86)\Phix\pwa\src\p2js_parse.e:522         integer {toktype,start,finish} = tok
C:\Program Files (x86)\Phix\pwa\src\p2js_parse.e:532             {toktype,start,finish} = tok
C:\Program Files (x86)\Phix\pwa\src\p2js_parse.e:577             {toktype,start,finish} = tok
C:\Program Files (x86)\Phix\pwa\src\p2js_parse.e:625     integer {toktype,start,finish} = tok
C:\Program Files (x86)\Phix\pwa\src\p2js_parse.e:673         integer {toktype,start,finish,line} = tok,
C:\Program Files (x86)\Phix\pwa\src\p2js_parse.e:706                             {toktype,start,finish} = tok
C:\Program Files (x86)\Phix\pwa\src\p2js_parse.e:1113 --  integer {toktype,start,finish,line} = tok
--*/

-- nb keep this enum and TOKTYPES in perfect step/exact same order.
global enum EOL, SPACE, DIGIT, LETTER, COMMENT, BLK_CMT, ILLEGAL, SYMBOL, TOKMAX=$
--SUG: (will it help or confuse?)
--          EOL = '\n', SPACE = ' ', DIGIT = '0', LETTER = 'A', COMMENT = '-', BLK_CMT = '*', ILLEGAL='?', SYMBOL = '$'
--if ILLEGAL>=' ' then ?9/0 end if

--erm, good news, both 39:
--?'''
--?'\''
--?'\\'     -- 92
--X--?'\'	-- illegal

-- (sequence rather than constant because we /want/ this in ex.err:)
-- (disney matter if damaged, as real deal in tok_names/show_tok anyway)
-- (sequence TOKTYPES itself is defined earlier, for debug reasons)
TOKTYPES = {{EOL,"EOL",false},      -- End of line
            {SPACE,"SPACE",false},  -- Spaces & tabs
-- BAND, BOR, MEQ, PEQ, TEQ, DEQ, BEQ, EEQ, EEEQ, LE, GE, LSHIFT, RSHIFT.
-- &&  , || , -= , += , *= , /= , := , == , === , <=, >=, <<    , >>    (and ...where's &= anyway)
--  (I accept that all multi-char SYMBOL need their own special TOKTYPE, btw)
            {DIGIT,"DIGIT",false},  -- 0..9
            {LETTER,"LETTER",false}, -- A..Z,a..z
--          {HEXSTR,"HEXSTR",false},    -- Hexadecimal Byte String [NO, already added to docs]
            {COMMENT,"COMMENT",false}, -- Phix only, -- .. \n
            {BLK_CMT,"BLK_CMT",false}, -- 
            {ILLEGAL,"ILLEGAL",false}, -- Illegal character
            {SYMBOL,"SYMBOL",false}}    -- General symbols !&|*+,.:;<=>?~\$%

--global constant {tok_chk, tok_names, show_tok} = columnize(TOKTYPES)
--?{tok_chk,tok_names,show_tok}  {} = wait_key()
--if tok_chk!=tagset(TOKMAX) then ?9/0 end if
if vslice(TOKTYPES,1)!=tagset(TOKMAX) then ?9/0 end if
constant tok_names = vslice(TOKTYPES,2),
         show_toks = vslice(TOKTYPES,3)
global constant show_any = find(true,show_toks)!=0

function set_chars(sequence s)
    -- (once-only set routine)
    string charset = repeat(ILLEGAL,256)
    for i=1 to length(s) do
        {object si, integer ctype} = s[i]
        --
        -- each si can be char, eg '-', 
        --           or string, eg "\r\n",
        --          or a range, eg {'0','9'}.
        -- ctype is a TOKTYPES enum (see above)
        --  (a toktype is technically the same,
        --   or rather the ctype of a run of
        --   characters with the same ctype,
        --   plus minor tweaks, like ELLIPSE.)
--?     -- a ctype of '=' is treated specially,
--?     --   for symbols, such as "+-(", etc.
        --
        if atom(si) then
            charset[si] = ctype
        elsif string(si) then
            for j=1 to length(si) do
                integer sij  = si[j]
--              charset[sij] = ctype
--              charset[sij] = iff(ctype=='='?sij:ctype)
                charset[sij] = iff(ctype=SYMBOL?sij:ctype)
--DEV/temp:
--              charset[sij] = iff(find(sij,"?(-/")?sij:ctype) -- !!SYMBOL!!
            end for
--SUG: (or as above)
--      elsif si='=' then
--          for j=1 to length(si) do
--              integer sij  = si[j]
--              charset[sij] = sij
--          end for
        else
            integer {lo,hi} = si
            charset[lo..hi] = ctype
        end if
    end for
    return charset
end function

global string charset = set_chars({{"\r\n",EOL},
                                   {" \t",SPACE},
                                   {`!&|*/+-,.:;<=>?~\$%([{}])`,SYMBOL},
--                                 {'$', LETTER}, -- iff is_js() in tokenise()
                                   {'"', '"'},
                                   {'`', '`'},
                                   {'#', '#'},
                                   {'\'', '\''},
                                   {{'0','9'}, DIGIT},
                                   {{'A','Z'}, LETTER},
                                   {{'a','z'}, LETTER},
                                   {{#80,#BF}, LETTER},
                                   {{#C2,#F4}, LETTER},
                                   {'_', LETTER}})

--/*
JavaScript escape sequences (I think we're ok):
\b: backspace (U+0008 BACKSPACE)                    -- (no)
\f: form feed (U+000C FORM FEED)                    -- (no)
\n: line feed (U+000A LINE FEED)
\r: carriage return (U+000D CARRIAGE RETURN)
\t: horizontal tab (U+0009 CHARACTER TABULATION)
\v: vertical tab (U+000B LINE TABULATION)           -- (no)
\0: null character (U+0000 NULL) (only if the next character is not a decimal digit; else it’s an octal escape sequence)
\': single quote (U+0027 APOSTROPHE)
\": double quote (U+0022 QUOTATION MARK)
\\: backslash (U+005C REVERSE SOLIDUS)
(but also, '\a'=='a', ie unneeded are ignored anyway)
--*/

global enum PASGN,      -- := += -= *= /= &&= ||=
            PANDO,      -- and  or  xor
--23/2/21...
--          PBITS,      -- && ||
            PCOMP,      -- =  !=
            PRELA,      -- < > <= >=
            PBITS,      -- && ||
            PAMPS,      -- &
            PSHFT,      -- << >> >>>
            PADDS,      -- + -
            PMORD,      -- * / %
            PUNY,       -- unary + - not ~ new
            PSBSC       -- subscripts/slices  .

--
-- >127 to avoid clashing with any single-char symbols,
--  odd to avoid clashing with any keywords (see p2js_keywords.e)
-- <256 so they all fit in strings and string-tables.
--
global constant precedences = {{BEQ     := 129, `:=`,   PASGN},
                               {PLUSEQ  := 131, `+=`,   PASGN},
                               {MNUSEQ  := 133, `-=`,   PASGN},
                               {AMPSEQ  := 135, `&=`,   PASGN},
                               {MULTEQ  := 137, `*=`,   PASGN},
                               {DIVDEQ  := 139, `/=`,   PASGN},
                               {ANDBEQ  := 141, `&&=`,  PASGN},
                               {ORBEQ   := 143, `||=`,  PASGN},
                               {AND     := 145, `and`,  PANDO},
                               {OR      := 147, `or`,   PANDO},
                               {XOR     := 149, `xor`,  PANDO},
                               {ANDBIT  := 151, `&&`,   PBITS}, -- PANDO for js
                               {ORBIT   := 153, `||`,   PBITS}, -- PANDO for js
--                             {EQ      := 155, `=`,    PCOMP},
--                             {EQ      := '=', `=`,    PCOMP},
                               {/*#3D=61*/ '=', `=`,    PCOMP},
                               {EEQ     := 157, `==`,   PCOMP},
                               {EEE     := 159, `===`,  PCOMP},
                               {NE      := 161, `!=`,   PCOMP},
                               {NEE     := 163, `!==`,  PCOMP},
--                             {LT      := 165, `<`,    PRELA},
--                             {LT      := '<', `<`,    PRELA},
                               {/*#3C=60*/ '<', `<`,    PRELA},
                               {LE      := 167, `<=`,   PRELA},
                               {GE      := 169, `>=`,   PRELA},
--                             {GT      := 171, `>`,    PRELA},
--                             {GT      := '>', `>`,    PRELA},
                               {           '>', `>`,    PRELA},
--                             {AMPSND  := 173, `&`,    PAMPS},
                               {           '&', `&`,    PAMPS}, -- PBITS for js
                               {RSH     := 175, `>>`,   PSHFT},
                               {LSH     := 177, `<<`,   PSHFT},
                               {RUSH    := 179, `>>>`,  PSHFT},
--                             {ADD     := 181, `+`,    PADDS},
                               {           '+', `+`,    PADDS},
--                             {SUB     := 183, `-`,    PADDS},
                               {           '-', `-`,    PADDS},
--                             {MULT    := 185, `*`,    PMORD},
                               {           '*', `*`,    PMORD},
                               {           '/', `/`,    PMORD},
--                             {MOD     := 189, `%`,    PMORD},
                               {           '%', `%`,    PMORD},
--                             {DOT     := 191, `.`,    PUNY},
                               {           '.', `.`,    PUNY},
                               {TWIDDLE := 191, `~`,    PUNY},
                               {NOT     := 193, `not`,  PUNY},
                               {NEW     := 195, `new`,  PUNY},
                               {SPREAD  := 197, `...`,  PUNY},
--                             {ELLIPSE := 199, `..`,   PSBSC},
                               {ELLIPSE := 199, `..`,   PASGN},
                               {MASS    := 201, `{`,    0},
                               {ARGS    := 203, `{`,    0}}
--                             {TRIQUOT := 203, `"""`,	0}}	-- nah, use '`'
global constant {mstoktypes,multisym,msprec} = columnize(precedences)
global sequence T_keywords,     -- ie {"if", "then", etc}
                T_toktypes,     -- ie TYPI..TYPR, see below
                T_reserved      -- ie {T_if, T_then, etc}

global function tok_name(integer toktype)
    if toktype>SYMBOL then
        integer k = find(toktype,mstoktypes)
        if k and string(multisym[k]) then
            return multisym[k]
        end if
        k = find(toktype, T_reserved)
        if k then
            return T_keywords[k]
        end if
        return toktype&"" -- eg '-' ==> "-"
    end if
    return tok_names[toktype]
end function

global function tok_string(sequence tok)
--  integer {toktype, tokstart, tokfinish, line} = tok
    integer {?, tokstart, tokfinish} = tok
--  tokline = line
    return src[tokstart..tokfinish]
end function

--global function show_tok(integer toktype)
--  return show_toks[min(toktype,SYMBOL)]
--end function

global procedure show_token(sequence tok, string prefix="")
    -- (diagnostic routine)
--object finish
    integer {toktype,start,finish,line,col} = tok
    if length(prefix) or show_toks[min(toktype,SYMBOL)] then
        tok[TOKTYPE] = tok_name(toktype)
--  if toktype<=TOKMAX then
--      tok[TOKTYPE] = tok_names[toktype]
--  else
--      tok[TOKTYPE] &= ""
--  end if
        tok = append(tok,shorten(src[start..finish]))
--  if length(prefix) then      -- (parser only)
--      tok = prepend(tok,prefix)
--  end if
        ?tok
        if length(prefix) then
            string curline = textlines[line], -- (for debug reasons)
                   padline = repeat(' ',col)&"^ "&prefix -- ("")
            printf(1,"Error in %s line %d, column %d\n%s\n%s\n",
                     {current_file,line,col+1,curline,padline})
            if CRASH then ?9/0 end if
        end if
    end if
end procedure

global constant INCLUDETOKS = `\/.<>"*`&'`'&LETTER&ELLIPSE&DIGIT

-- to show in ex.err as a useful lookup table during debugging:
sequence precedence_table = precedences

--also ?, : 
--/*
--Python:  := //[==floor division] and singles: | ^ & @[==matrix multiplication] % :(==slicing)
--C: ++ -- -> %= <<= >>= ^= |=
--JS: ?. ** >>> === !== ?? **= >>>= ??=
    {"args",                    T_args                      := 536},
/!*
== equal value
=== equal value and equal type
!= not equal value
!== not equal value or not equal type
< lexically ordered before
> lexically ordered after
*!/
--*/

-- <   >   <=  >=   =   !=   
-- and, or, xor, and not
-- + - * / || && << >>
--/*
11:                             subscripts/slices  .

10:                             unary-  unary+  not  ~

9:                              *  /

8:                              +  -
                                
7:                              >> <<

6:                              &

5:                              &lt;  &gt;  &lt;=  &gt;=
                                
4:                              =  !=
                                
3:                              && ||

2:                              and  or  xor

--*/
--
-- Note that TYP2..TY14 are perfectly valid as propagated types, 
-- however you cannot explicitly define variables of those types.
--
global constant TYPES = {{TYPI := 0b00000001,   "TYPI"},    -- 1: integer (aka int)
                         {TYP2 := 0b00000010,   "TYP2"},    -- 2: non-integer atom (constants only, aka flt)
                         {TYPN := 0b00000011,   "TYPN"},    -- 3: atom (flt|int)
                         {TYPQ := 0b00000100,   "TYP4"},    -- 4: dseq aka non-string sequence (TYPQ?)
                         {TYP5 := 0b00000101,   "TYP5"},    -- 5: int|dseq
                         {TYP6 := 0b00000110,   "TYP6"},    -- 6: ? (flt|dseq)
                         {TYP7 := 0b00000111,   "TYP7"},    -- 6: atom|dseq (aka int|flt|dseq)
                         {TYPS := 0b00001000,   "TYPS"},    -- 8: string (aka str)
                         {TYP9 := 0b00001001,   "TYP9"},    -- 9: nullable string (TYPNS?, aka int|str)
                         {TY10 := 0b00001010,   "TY10"},    -- 10: ? (flt|str)
--                       {TYPM := 0b00001011,   "TY11"},    -- 11: atom string (TYPM?, aka int|flt|str)
                         {TY11 := 0b00001011,   "TY11"},    -- 11: atom string (TYPM?, aka int|flt|str)
                         {TYPP := 0b00001100,   "TYPP"},    -- 12: sequence (aka dseq|str)
                         {TY13 := 0b00001101,   "TY13"},    -- 13: dseq|str|int
                         {TY14 := 0b00001110,   "TY14"},    -- 13: ? (dseq|str|flt)
                         {TYPO := 0b00001111,   "TYPO"},    -- 15: object (aka (int|flt|str|dseq)
--DEV cleanup?
                         {TY16 := 0b00010000,   "TY16"},    -- 16: ?
                         {TYPE := 0b00010001,   "TYPE"},    -- 17: type (/integer,0b10001)
                         {TY18 := 0b00010010,   "TY18"},    -- 18: ?
                         {TYPF := 0b00010011,   "TYPF"},    -- 19: func (/integer,0b10011)
                         {TY20 := 0b00010100,   "TY20"},    -- 20: ?
                         {TYPK := 0b00010101,   "TYPK"},    -- 21: keyword (0b10101)
                         {TY22 := 0b00010110,   "TY22"},    -- 22: ?
                         {TYPR := 0b00010111,   "TYPR"},    -- 23: proc (/integer,0b10111)
                         {BADT := 0b00011000,   "BADT"},    -- >=24 or <=0: bad type
                         {TYP0 := 0b00011000,   "TYP0"}}    -- 24: bad type/not supported
--/* (SUG)
                         {TYPE := 0b00010000,   "TYPE"},    -- 16: type (/integer,0b10000)
                         {TYPF := 0b00010001,   "TYPF"},    -- 17: func (/integer,0b10001)
                         {TYPR := 0b00010010,   "TYPR"},    -- 18: proc (/integer,0b10010)
                         {TYPK := 0b00010011,   "TYPK"},    -- 19: keyword       (0b10011)
                         {BADT := 0b00010100,   "BADT"}}    -- >=20 or <=0: bad type
--*/
