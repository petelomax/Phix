--
-- pwa\src\p2js_basics.e
-- ======================
--
--  standard includes, constants, globals, and utility routines for ../../pwa.exw
--
--without debug -- (keep the ex.err reasonably clear, comment out if/when needed)
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
integer ext
global string text
global integer lt   -- ==length(text)
global sequence textlines   -- (temp/debug)
sequence TOKTYPES
global sequence tokens

-- aside: pwa/p2js (as per the docs) does not support nested constants.
--/* -- backtrack, see p2js.exw
enum PHIX, HTML, CSS, JSS, C
constant extensions = {{PHIX, {"exw","ex","e","eu"}},
                       {HTML, {"html","htm"}},
                       {CSS, {"css"}},
                       {JSS, {"js"}},
                       {C, {"c"}}}
--*/
global constant ext_names = {"phix","html","css","js","go","c"}
constant extensions = {{PHIX :=1, {"exw","ex","e","eu","ew"}},
                       {HTML :=2, {"html","htm"}},
                       {CSS  :=3, {"css"}},
                       {JSS  :=4, {"js"}}, -- (avoid clash with platform()'s JS)
                       {GO   :=5, {"go"}},
                       {C    :=6, {"c"}}}
--
-- note that both the tokeniser and the parser have a local phix_only() routine
--      that invokes is_phix(), before going on to invoke their own xxx_error().
--
global function is_phix() return ext=PHIX end function
global function is_html() return ext=HTML end function
global function is_css()  return ext=CSS  end function
global function is_js()   return ext=JSS  end function
global function is_go()   return ext=GO   end function
global function is_C()    return ext=C    end function

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
    text = txt
    ext = edx
-- temp/debug[?]:
textlines = split(text,"\r\n")
    lt = length(text)
    return true
end function

global function load_file(string filename)
    current_file = filename -- (store for debugging purposes)
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
global enum TOKTYPE, TOKSTART, TOKFINISH, TOKLINE, TOKCOL, TOKTTIDX, TOKENDLINE=$ -- (one token)
            -- TOKTYPE is as per vslice(TOKTYPES,1) below
            -- TOKTTIDX is only set on LETTER tokens
            -- TOKENDLINE only on '`' (aka `"""`) and BLK_CMT (no other tokens span lines)
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
global enum EOL, SPACE, /*MINUS,*/ /*FWDSLASH,*/ /*DIVIDE=$,*/ --ELLIPSE, --SPREAD,
            /*BRACES,*/
--          ORB, OSB, OCB, CCB, CSB, CRB, 
--          ORB = '(', OSB, OCB, CCB, CSB, CRB, 
--          HEXDEC, --BINDEC, --SQUOTE, --DQUOTE, 
            /*FLOAT,*/
            DIGIT, LETTER, COMMENT, BLK_CMT, ILLEGAL, SYMBOL, TOKMAX=$
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
--          {MINUS,"MINUS",false},  -- Minus sign '-' (/comment)
--          {FWDSLASH,"FWDSLASH",false}, -- Forward slash   (   ""   )
--          {ELLIPSE,"ELLIPSE",false}, -- '..'
--          {SPREAD,"SPREAD",false}, -- '...'
-- BAND, BOR, MEQ, PEQ, TEQ, DEQ, BEQ, EEQ, EEEQ, LE, GE, LSHIFT, RSHIFT.
-- &&  , || , -= , += , *= , /= , := , == , === , <=, >=, <<    , >>    (and ...where's &= anyway)
--  (I accept that all multi-char SYMBOL need their own special TOKTYPE, btw)
--X         {BRACES,"BRACES",false},    -- ()[]{}
--          {ORB,"(",false},            -- opening round bracket
--          {OSB,"[",false},            -- opening square bracket
--          {OCB,"{",false},            -- opening curly bracket
--          {CCB,"}",false},            -- closing curly bracket
--          {CSB,"]",false},            -- closing square bracket
--          {CRB,")",false},            -- closing round bracket
--          {HEXDEC,"HEXDEC",false},    -- Hexadecimal (#) mark     -- DEV '#' instead?
--          {BINDEC,"BINDEC",false},    -- 0bNNN format digit
--          {SQUOTE,"SQUOTE",false},    -- Single quotation mark    -- DEV '\'' instead?
--          {DQUOTE,"DQUOTE",false},    -- Double quotation mark    -- DEV '"' instead?
--          {FLOAT,"FLOAT",false},  -- float, eg 1.0 or 1e4 (may yet be desired?)
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
constant tok_names = vslice(TOKTYPES,2)
constant show_toks = vslice(TOKTYPES,3)

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

global constant string charset = set_chars({{"\r\n",EOL},
                                            {" \t",SPACE},
--                                          {`!&|*+,.:;<=>?~\$%`,SYMBOL},
                                            {`!&|*/+-,.:;<=>?~\$%([{}])`,SYMBOL},
-- nah, just use eg '?' for QU, etc
-- BANG, AMPERSAND, BAR, TIMES, PLUS, COMMA, DOT, COLON, SEMICOLON, LT, EQ, GT, QU, TILDE, BKSLASH, DOLLAR, PERCENT,
--                                          {'-', MINUS},
--                                          {'/', FWDSLASH},
--
--                                          {'"', DQUOTE},
                                            {'"', '"'},
                                            {'`', '`'},
--                                          {'#', HEXDEC},
                                            {'#', '#'},
--                                          {'\'', SQUOTE},
                                            {'\'', '\''},
--                                          {'(',ORB},
--                                          {'[',OSB},
--                                          {'{',OCB},
--                                          {'}',CCB},
--                                          {']',CSB},
--                                          {')',CRB},
--                                          {"([{}])",BRACES},
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
            PBITS,      -- && ||
            PCOMP,      -- =  !=
            PRELA,      -- < > <= >=
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
                               {ANDBIT  := 151, `&&`,   PBITS},
                               {ORBIT   := 153, `||`,   PBITS},
--                             {EQ      := 155, `=`,    PCOMP},
--                             {EQ      := '=', `=`,    PCOMP},
                               {           '=', `=`,    PCOMP},
                               {EEQ     := 157, `==`,   PCOMP},
                               {EEE     := 159, `===`,  PCOMP},
                               {NE      := 161, `!=`,   PCOMP},
                               {NEE     := 163, `!==`,  PCOMP},
--                             {LT      := 165, `<`,    PRELA},
--                             {LT      := '<', `<`,    PRELA},
                               {           '<', `<`,    PRELA},
                               {LE      := 167, `<=`,   PRELA},
                               {GE      := 169, `>=`,   PRELA},
--                             {GT      := 171, `>`,    PRELA},
--                             {GT      := '>', `>`,    PRELA},
                               {           '>', `>`,    PRELA},
--                             {AMPSND  := 173, `&`,    PAMPS},
                               {           `&`, `&`,    PAMPS},
                               {RSH     := 175, `>>`,   PSHFT},
                               {LSH     := 177, `<<`,   PSHFT},
                               {RUSH    := 179, `>>>`,  PSHFT},
--                             {ADD     := 181, `+`,    PADDS},
                               {           '+', `+`,    PADDS},
--                             {SUB     := 183, `-`,    PADDS},
                               {           '-', `-`,    PADDS},
--                             {MULT    := 185, `*`,    PMORD},
                               {           '*', `*`,    PMORD},
--                             {DIVIDE  := 187, `/`,    PMORD},
                               {           '/', `/`,    PMORD},
--                             {MOD     := 189, `%`,    PMORD},
                               {           '%', `%`,    PMORD},
--                             {DOT     := 191, `.`,    PUNY},
                               {           '.', `.`,    PUNY},
                               {TWIDDLE := 191, `~`,    PUNY},
                               {NOT     := 193, `not`,  PUNY},
                               {NEW     := 195, `new`,  PUNY},
                               {SPREAD  := 197, `...`,  PUNY},
                               {ELLIPSE := 199, `..`,   PSBSC}}
global constant {mstoktypes,multisym,msprec} = columnize(precedences)

global function tok_name(integer toktype)
    if toktype>SYMBOL then
        integer k = find(toktype,mstoktypes)
        if k and string(multisym[k]) then
            return multisym[k]
        end if
        return toktype&""
    end if
    return tok_names[toktype]
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
        tok = append(tok,shorten(text[start..finish]))
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

--global constant INCLUDETOKS = `\/.<>`&'`'&DQUOTE&LETTER&ELLIPSE&DIGIT
global constant INCLUDETOKS = `\/.<>"`&'`'&LETTER&ELLIPSE&DIGIT

-- to show in ex.err as a useful lookup table during debugging:
sequence precedence_table = precedences

--also ?, : 
--/*
--Python:  := //[==floor division] and singles: | ^ & @[==matrix multiplication] % :(==slicing)
--C: ++ -- -> %= <<= >>= ^= |=
--JS: ?. ** >>> === !== ?? **= >>>= ??=
/*
== equal value
=== equal value and equal type
!= not equal value
!== not equal value or not equal type
< lexically ordered before
> lexically ordered after
*/
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

