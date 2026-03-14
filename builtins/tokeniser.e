--
-- builtins\tokeniser.e
-- ====================
--
--  A fast generic re-entrant tokeniser. Implements tok_init() etc (see docs)
--
--  Experimental: to be used and fully tested in anger in xpEditer.exw first.
--
--  Of course the whole point here is to merge everything I have learnt from v1
--  and p2js into one consistent file that can be used in both xpEditer and v2.
--  Likewise, just because I need a tokeniser for Phix does not mean that I do
--  not want (a generic) one that can deal with other programming languages.
--
--  The following global must be defined before this file is included, and in the
--  case of "include" (and similar, including testing), saved/restored as needed:
--
--      global string src
--
--  First and foremost it must handle the GT_WHOLE_FILE case efficiently/fast, as
--  that will be used in the phix compiler (v2) itself. GT_LF_STRIPPED is assumed
--  when src contains no '\n'. Note this re-uses GT_WHOLE_FILE/GT_LF_STRIPPED as
--  the clearest way I could think of to explain, not to call get_text() itself.
--  Tokenising a 54,000 line (2.2MB) file first took 0.031s, which I think'll do..
--  The GT_LF_STRIPPED mode is critical for xpEditer and can afford to be a smidge
--  slower, as in it must be pretty decent for the 25 or so lines you can see but
--  it would barely matter even if a full background scan on a 50,000 line file
--  was a whole second slower than GT_WHOLE_FILE. Not that I want to deliberately 
--  make it any slower, of course, just be clear about which one matters the most.
--  In GT_WHOLE_FILE mode, the tokeniser can modify src in situ, without changing
--  it's length, for instance to replace nested comments when transpiling code to
--  JavaScript (since that does not allow them), but not in GT_LF_STRIPPED, which
--  must also of course set src to each individual line one at a time around all 
--  the get_token() calls and manage tok_line (and any blank lines) all by itself.
--  UPDATE: in practice, I'm not going to guarantee src will not be modified when
--  in GT_LF_STRIPPED mode: xpEditer should use integer results on the original.
--
--...
--  tok_mode = iff(find('\n',src)?GT_WHOLE_FILE:GT_LF_STRIPPED)
--ERM, maybe I should have that strictly string and a TOK_MODE for continuation handling...
--  src may be a single string (as per GT_WHOLE_FILE) or a sequence of individual
--  '\n' (and '\r') stripped lines (as per GT_LF_STRIPPED). The former wants to be
--  as fast as possible, as will be used by Phix v2, but xpEditer in particular 
--  needs equivalent(ish) results from post-split()/being-edited text.
--
-- Several other things need initialising/saving/restoring, see tok_init/settings.
--...
--  It is however perfectly fine (/preferrable) to have duplicate code here and/or
--  force the calling code to (selectively) use different entry points for that,
--  and o/c use the results slightly differently, ie src[tok_start..tok_col-1] vs.
--  src[tok_line][tok_start..tok_col-1] (unless tok_type is all you need).
--
--  Note that col/tokstart are different for string/sequence handling, and linestart
--  is unused in the sequence lines case, but you can easily check they are indeed 
--  equivalent by subtracting linestart in the string case, or scan back for '\n'.
--
--  Be warned that while this file should deliver consistent line numbers for the
--  same binary-identical text file on the same platform, text files which have
--  *not* been saved with platform-specific line endings (such as the text files 
--  as shipped with Phix) may sometimes yield platform-specific line numbering.
--
global integer tok_lang = 0,    -- see extensions
               tok_mode = 0,    -- GT_WHOLE_FILE or GT_LF_STRIPPED (assumed)
               tok_line = 1,
                tok_col = 1,
          tok_linestart = 1,
              tok_start = 1,
               tok_type = 0,    -- see TOKTYPES
         tok_incomplete = false,
                src_len = 0     -- length(src)
--               tok_ch = '\0'  -- src[tok_col] or -1

global string tok_error_msg

with nested_globals
local constant TOKTYPES = {{EOL:=$,"EOL",false},            -- End of line
                           {SPACE:=$,"SPACE",false},        -- Spaces & tabs
                           {DIGIT:=$,"DIGIT",false},        -- 0..9
                           {LETTER:=$,"LETTER",false},      -- A..Z,a..z
                           {COMMENT:=$,"COMMENT",false},    -- Phix only, -- .. \n
                           {BLK_CMT:=$,"BLK_CMT",false},    -- 
                           {ILLEGAL:=$,"ILLEGAL",false},    -- Illegal character
                           {SYMBOL:=$,"SYMBOL",false},      -- General symbols !&|*+,.:;<=>?@~\$%
                           {ERROR:=$,"ERROR",false}}        -- see tok_error_msg

--global constant ext_names = {"phix","js","html","c","css","go","python"},
local constant ext_names = {"phix","js","html","c"},
              extensions = {{PHIX        :=1, {"exw","ex","e","eu","ew"}},
                            {JAVA_SCRIPT :=2, {"js"}}, -- (avoid clash with platform()'s JS)
                            {HTML        :=3, {"html","htm"}},
                            {C           :=4, {"c"}}}
--                          {CSS         :=4, {"css"}},
--                          {GO          :=6, {"go"}},
--                          {PYTH        :=7, {"py"}}}
without nested_globals

global function get_ext_lang(string ext)
    if find('.',ext) then
        ext = get_file_extension(ext)
    end if
    for e in extensions do
        if find(ext,e[2]) then return e[1] end if
    end for
    return 0 -- do not attempt to tokenise or syntax-colour
end function

global function get_lang_name(integer lang)
    return ext_names[lang]
end function

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
                charset[sij] = iff(ctype=SYMBOL?sij:ctype)
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

local sequence charset = set_chars({{"\r\n",EOL},
                                    {" \t",SPACE},
--                                  {' ',SPACE},
--                                  {0x9,SPACE},
--                                  {`!&|*/+-,.:;<=>?@~\$%([{}])^`,SYMBOL},
                                    {`!"#$%&()'*+,-./:;<=>?@[\]^{|}~`,SYMBOL},
--                                  {'$', LETTER}, -- iff is_js() in tokenise()
--                                  {'"', '"'},
                                    {'`', '`'},
--                                  {'#', '#'},
--                                  {'\'', '\''},
--                                  {"\"`#'", '='},
                                    {{'0','9'}, DIGIT},
                                    {{'A','Z'}, LETTER},
                                    {{'a','z'}, LETTER},
                                    -- (for unicode/UTF-8):
                                    {{#80,#BF}, LETTER},
                                    {{#C2,#F4}, LETTER},
                                    {'_', LETTER}})


global enum TOK_LINE, TOK_COL, TOK_LINESTART, TOK_LANG, TOK_INCOMPLETE, TOK_SETTINGS_SIZE = $

global procedure tok_init(sequence settings)
    --
    -- NB the global text must be set *before* invocation.
    -- settings should be from code based on tok_settings(),
    -- or settings of {1,1,1,PHIX,false} could be used instead,
    -- or the calling code can manage all these globals itself,
    -- with the one exception of charset['$'].
    --
--  tok_whole_file = string(src)
    src_len = length(src)
    tok_mode = iff(find('\n',src)?GT_WHOLE_FILE:GT_LF_STRIPPED)
    tok_line = settings[TOK_LINE]
    tok_col = settings[TOK_COL]
--  try
--      tok_ch = iff(tok_whole_file?src[tok_col]:src[tok_line][tok_col])
--          src_len = iff(tok_whole_file?length(src):length(src[tok_line]))
--  catch
--      tok_ch = -1
--          src_len = 0
--  end try
--  if tok_whole_file then
--      src_len = length(src)
--      tok_ch = iff(tok_col>src_len?-1:src[tok_col])
--  else
--  elsif tok_line<=length(src) then
--      src_len = iff(tok_line>length(src)?0:length(src[tok_line]))
--      tok_ch = iff(tok_col>src_len?-1:src[tok_col])
--      tok_ch = iff(src_len=0?-1:src[tok_line][tok_col])
--      tok_ch = src[tok_line][tok_col]
--  else
--      tok_ch = -1
--          src_len = 0
--  end if
    tok_linestart = settings[TOK_LINESTART]
    tok_lang = settings[TOK_LANG]
    charset['$'] = iff(tok_lang=JAVA_SCRIPT?LETTER:'$')
    tok_incomplete = settings[TOK_INCOMPLETE]
end procedure

-- erm...
--global procedure tok_next_line()
--  assert(settings[TOK_MODE]==GT_LF_STRIPPED)
--  src_len = length(src)
--end function

global function tok_settings()
    -- for eg saving position over an include statement
    sequence settings = repeat(0,TOK_SETTINGS_SIZE)
    settings[TOK_LINE] = tok_line
    settings[TOK_COL] = tok_col
    settings[TOK_LINESTART] = tok_linestart
    settings[TOK_LANG] = tok_lang
    settings[TOK_INCOMPLETE] = tok_incomplete
    return settings
end function

--local string ce, ddce

--global enum EOL, SPACE, DIGIT, LETTER, COMMENT, BLK_CMT, ILLEGAL, SYMBOL, TOKMAX=$
--            1,     2,     3,      4,       5,       6,       7,      8,        8
--SUG: (will it help or confuse? [or clash?])
--          EOL = '\n', SPACE = ' ', DIGIT = '0', LETTER = 'A', COMMENT = '-', BLK_CMT = '*', ILLEGAL='?', SYMBOL = '$'
--                  10,          32,          48,           65,            45,            42,          63,           36
--if ILLEGAL>=' ' then ?9/0 end if

--erm, good news, both 39:
--?'''
--?'\''
--?'\\'     -- 92
--X--?'\'	-- illegal

-- DOH, shd you want that, just have a separate sequence toktypes = TOKTYPES!
--X (sequence rather than constant because we /want/ this in ex.err:)
--X (disney matter if damaged, as real deal in tok_names/show_tok anyway)
--X (sequence TOKTYPES itself is defined earlier, for debug reasons)

global enum PASGN,      -- := += -= *= /= &&= ||=
            PANDO,      -- and  or  xor
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
-- >127 to avoid clashing with any single-char (AZaz09) symbols,
--  odd to avoid clashing with any keywords (see p2js_keywords.e)
-- <256 so they all fit in strings and string-tables.
--
with nested_globals
local constant precedences = {{BEQ      := 129, `:=`,   PASGN},
                              {PLUSEQ   := 131, `+=`,   PASGN},
                              {MNUSEQ   := 133, `-=`,   PASGN},
                              {AMPSEQ   := 135, `&=`,   PASGN},
                              {MULTEQ   := 137, `*=`,   PASGN},
                              {DIVDEQ   := 139, `/=`,   PASGN},
                              {ANDBEQ   := 141, `&&=`,  PASGN},
                              {ORBEQ    := 143, `||=`,  PASGN},
                              {AND      := 145, `and`,  PANDO},
                              {OR       := 147, `or`,   PANDO},
                              {XOR      := 149, `xor`,  PANDO},
                              {ANDBIT   := 151, `&&`,   PBITS}, -- PANDO for js
                              {ORBIT    := 153, `||`,   PBITS}, -- PANDO for js
                              {/*#3D=61*/  '=', `=`,    PCOMP},
                              {EEQ      := 157, `==`,   PCOMP},
                              {EEE      := 159, `===`,  PCOMP},
                              {NE       := 161, `!=`,   PCOMP},
                              {NEE      := 163, `!==`,  PCOMP},
                              {NEE            , `<>`,   PCOMP},
                              {/*#3C=60*/  '<', `<`,    PRELA},
                              {LE       := 167, `<=`,   PRELA},
                              {GE       := 169, `>=`,   PRELA},
                              {/*#3E=62*/  '>', `>`,    PRELA},
                              {/*#26=38*/  '&', `&`,    PAMPS}, -- PBITS for js
                              {/*#5E=94*/  '^', `^`,    PAMPS}, -- PBITS for js
                              {/*7C=124*/  '|', `|`,    PAMPS}, -- PBITS for js
                              {RSH      := 175, `>>`,   PSHFT},
                              {LSH      := 177, `<<`,   PSHFT},
                              {RUSH     := 179, `>>>`,  PSHFT},
                              {/*#2B=43*/  '+', `+`,    PADDS},
                              {/*#2D=45*/  '-', `-`,    PADDS},
                              {/*#2A=42*/  '*', `*`,    PMORD},
                              {/*#2F=47*/  '/', `/`,    PMORD},
                              {/*#25=37*/  '%', `%`,    PMORD},
                              {HEXSTR   := 189, `\x`,   PUNY},
                              {/*#2E=46*/  '.', `.`,    PUNY},
                              {TWIDDLE  := 191, `~`,    PUNY},
                              {NOT      := 193, `not`,  PUNY},
                              {NEW      := 195, `new`,  PUNY},
                              {SPREAD   := 197, `...`,  PUNY},
                              {ELLIPSE  := 199, `..`,   PASGN},
                              {MASS     := 201, `{`,    0},
                              {ARGS     := 203, `{`,    0}}
without nested_globals

local constant {mstoktypes,multisym,msprec} = columnize(precedences)

local constant tok_names = vslice(TOKTYPES,2),
               show_toks = vslice(TOKTYPES,3)

global function get_tok_type_str(integer tok_type)
    -- (for diagnostics)
    if tok_type>SYMBOL then 
        integer mdx = find(tok_type,mstoktypes)
        if mdx then return multisym[mdx] end if
        return ""&tok_type
    end if
    return tok_names[tok_type]
end function

global function get_show_tok(integer tok_type)
    -- (for diagnostics, with 0 yeilding a "show_any" bool)
    return iff(tok_type=0?find(true,show_toks)!=0:show_toks[tok_type])
end function

global function get_precedence(integer tok_type)
    return msprec[find(tok_type,mstoktypes)]
end function

--/!*
--
--  To do:
--  Under the control of p2js_parse.e, stack/restore tokens/i/ch/line/tokstart/toktype etc.
--  Have a lookahead on comments so that factor() etc does not need to do the wastdx stuff [?]
--   (plus whatever, if anything, is needed for any skipped comments warnings)
--
--include p2js_basics.e -- includes/constants/globals/utilities
--include p2js_tree.e   -- simple ternary tree for fast identifier lookup
--sequence p2js_kwdbg = p2js_keywords -- (debug aid)
--sequence tt_r = tt_reserved

--integer --i,  -- to src[]
--      src_len, -- length(src)
--      tok_ch, -- src[i]
--      tok_line,
--      tok_linestart,
--      tok_start
--      toktype -- charset[src[i]]

--procedure add_tok(sequence tok)
--  ?{"add_tok",tok}
----    tokens = append(tokens,tok)
----    tok_type = tok[TOKTYPE]
----    if tok_type>TOKMAX then tok_type=SYMBOL end if
----    if show_any then
----        show_token(tok)
----    end if
--end procedure

--from basics: global enum TOKTYPE, TOKSTART, TOKFINISH, TOKLINE, TOKCOL, TOKTTIDX, TOKENDLINE=$ -- (one token)
--procedure std_token()
----    ?"std_token"
--  ?9/0
----    add_tok({tok_type,tok_start,i,tok_line,tok_start-tok_linestart})
--end procedure

procedure std_ident()
--  ?"std_ident"
    ?9/0
--  integer ttidx = tt_idx(tok_start,i)
----    integer ttidx = tt_idx(tok_start,i,is_html())
--  add_tok({tok_type,tok_start,i,tok_line,tok_start-tok_linestart,ttidx})
--if ttidx=T_private then ?tokens[$] end if
end procedure

bool tok_bad = false

--DEV
constant integer src_offset = 0
constant current_file = "current_file"
constant CRASH = false

local integer tok_ch = '\0'

global function tok_error(string reason="", integer e=tok_col)
    --
    -- tok_error("why") displays why, sets flag and returns false.
    -- tok_error() returns false or true if "" has happened.
    -- note: tok_ch!=-1 is oft as good as or better than tok_error().
    --       return tok_error("why") should be standard practice.
    --
    -- e can/shd be used to set "shown code snippet's final char"
    --
    if length(reason) then
--      string lpad = repeat(' ',e-tok_linestart)
        string lpad = repeat(' ',max(e-tok_linestart,0))
--      if e=i then
            while e<length(src) and src[e+1]>=' ' do
                e += 1
            end while
--      end if
--DEV filename?
        string ltxt = src[tok_linestart..e],
--      string ltxt = src[tok_linestart-100..e],
                msg = sprintf("token error in %s on line %d:\n%s\n%s^%s\n",
                              {current_file,tok_line+src_offset,ltxt,lpad,reason})
        if CRASH then crash(msg) end if
        puts(1,msg)
--      tok_bad = true
        tok_ch = -1
    end if
    return tok_bad
end function

--DEV 
function is_phix() return tok_lang=PHIX end function
function is_C() return tok_lang=C end function

function phix_only(integer e=tok_col, string m="phix only")
    -- return true === an error
    if not is_phix() then
        if m="nnc" then ?9/0 end if
--      if m="nnc" then
--          m = "no nested comments in "&ext_names[ext]
--      end if
        return tok_error(m,e)
    end if
    return false
end function

procedure block_comment(integer tok_start, adj)
--
-- note: a comment is deemed to start with "--/*" if it ends with "--*/"
--       nested block comments are replaced, "/* /* */ */" ==> "/* /@ @/ */"
--       (since nested comments are *not* supported by js, html, c, or css!)
--
--  integer startline = tok_line, nest = 1
    integer startline = tok_line, startcol = tok_start-tok_linestart, nest = 1
--  integer startline = tok_line, startcol = col, nest = 1
    tok_col = tok_start+adj
    while true do
        if tok_col>length(src) then
            tok_line = startline
            {} = tok_error("no closing comment",tok_start)
            return 
        end if
        tok_ch = src[tok_col]
        integer cn = iff(tok_col<src_len?src[tok_col+1]:'\0')
--?{"?:"&tok_ch&cn,tok_line}
--31/1/22
--      if tok_ch='*' and cn='/' then
        if (tok_ch='*' and cn='/')
        or (tok_ch='#' and cn=']') then
--?{"-"&tok_ch&cn,tok_line}
            tok_col += 1
            nest -= 1
            if nest=0 then
                if tok_mode=GT_WHOLE_FILE then
                    src[tok_col-1] = '*'
                    src[tok_col] = '/'
                end if
                exit
            end if
            if tok_mode=GT_WHOLE_FILE then
                src[tok_col-1] = '@'
            end if
--31/1/22
--      elsif tok_ch='/' and cn='*' then
        elsif (tok_ch='/' and cn='*')
           or (tok_ch='#' and cn='[') then
--?{"+"&tok_ch&cn,tok_line}
            if phix_only(tok_col+1,"nnc") then return end if
            if tok_mode=GT_WHOLE_FILE then
                if tok_ch='#' then src[tok_col] = '[' end if
            end if
--          if tok_ch='#' then src[tok_col] = '/' end if
            tok_col += 1
            if tok_mode=GT_WHOLE_FILE then
                src[tok_col] = '@'
            end if
            nest += 1
        elsif tok_ch='\n' then
            tok_line += 1
            tok_linestart = tok_col+1
        end if
        tok_col += 1
    end while
--Ah: **ALL** comments in js/html/css **ARE** block comments, so we **DO** need a way
--      to convert them into line comments, roughly like this.
--X -- note: block comments distinguished from line comments by length. [erm, startline!=tok_line?]
    -- DEV add note re phix->js->phix handling, once we know what all the rules really are
    --      (I'm thinking genuine multiline must obvs. stay as they are, but 
    --       for single line phix prefers line comments, unless more tokens
    --       follow on the same line, eg in "exit_cb(Ihandle /*ih*/)", that
    --       trailing ')' forces it to stay in "/* */" format.
    --       There is also eg "let /* Ihandle */ ih" to consider...) [hence BLK_CMT added]
    -- perhaps a better way is simply "/*-- line comment */"
    -- (likewise, "/* some comment --*/" is really "--/* some comment --*/", when transpiling back to .exw)
    -- (and obviously "/* comment */" stays as-is... or rather " com " ==> "/* com */", 
    --                                                         "com --") ==> "--/* com --*/",
    --                                                         "-- com" ==> "-- com".)
    tok_type = BLK_CMT
--  ?"block_comment"
--  add_tok({tok_type,tok_start,i,startline,startcol,tok_line})
end procedure

procedure line_comment()
    -- (normal "--" or "//")
--  while true do 
    while tok_col<=length(src) and charset[src[tok_col]]!=EOL do
        tok_col += 1 -- (GT_WHOLE_FILE ensures we'll hit \n)
--      if i>length(src) then exit end if
--      tok_ch = src[i]
--      tok_type = charset[tok_ch]
--      if tok_type = EOL then exit end if
----        if charset[src[i]] = EOL then exit end if
    end while
--  tok_col -= 1 -- (let main loop of tokenise() handle the EOL, 
                 --  besides, we absolutely want that -1 here.)
--maybe:
    tok_type = COMMENT
--  std_token()
--  add_tok({COMMENT,tok_start,i,tok_line,tok_start-tok_linestart})
end procedure

--global procedure tokenise()
global procedure get_token()

-- or:  (why?: because a single active token is easier to debug...)
--  while ?? do tokens = append(tokens,get_token()) end while

--  {} = tok_error("just_clear_tok_bad",0)
--  tok_bad = false 
--  i = 1
--  tok_ch = ' '
--  tok_line = 1
--  tok_linestart = 1
--  tokens = {}
--  charset['$'] = iff(is_js()?LETTER:SYMBOL)
--  charset['$'] = iff(is_js()?LETTER:'$')

--if not tok_whole_file then ?9/0 end if --DEV
    if tok_incomplete then
        --...
        ?9/0
    end if
    while tok_col<=src_len and tok_ch!=-1 do
        tok_start = tok_col
        tok_ch = src[tok_col]
        tok_type = charset[tok_ch]
        switch tok_type
            case EOL:
                tok_line += (tok_ch='\n')
--if tok_line=58 then trace(1) end if
                tok_linestart = tok_col+1
            case SPACE:
                break
            case '-', '/':
                if tok_col<src_len then
                    integer cn = src[tok_col+1]
                    if cn=tok_ch then
                        -- comment
                        if tok_lang=PHIX then
                            string t4 = iff(tok_col+3<=src_len?src[tok_col..tok_col+2]:"")
                            if t4="--/" and src[tok_col+3]='*' then
--                          if phix_only(tok_col+3) then return end if
--                          if tok_lang=PHIX then
                                block_comment(tok_col,4)
--                          end if
                                return
                            elsif t4="--*" and src[tok_col+3]='/' then
--                              {} = tok_error("unexpected closing comment",tok_col+3)
                                tok_type = ERROR
                                tok_error_msg = "unexpected closing comment"
                                return
                            end if
                        end if
                        line_comment()
                        return
                    elsif (tok_type='/' and cn='*')
                       or (tok_type='#' and cn='[') then
                        if tok_type='#' then
                            if tok_mode=GT_WHOLE_FILE then
                                src[tok_col] = '/'
                                src[tok_col+1] = '*'
                            end if
                        end if
                        block_comment(tok_col,2)
                        return
                    end if
                end if
                fallthrough
--              return
            case '?','@','+','*','!','=',';',',','.','|',
                 '<','&',':','>','\\','$','%','~','^':
--DEV lang=0 treat them as singles?
                tok_col += 1
                while tok_col<=src_len and charset[src[tok_col]]>SYMBOL do
                    integer sdx = find(src[tok_start..tok_col],multisym)
                    if sdx=0 then exit end if
                    --            ^ ie not say "-(", "*-", "*(", ...
                    tok_type = mstoktypes[sdx]
                    tok_col += 1
                end while
--              fallthrough
                return
            case '(','[','{','}',']',')':
--              std_token()
--              break
                tok_col += 1
                return
            case '#':
--              if is_py() then
--                  line_comment()
--                  break
--              end if
                tok_col += 1
                tok_ch = src[tok_col]
                if tok_ch='i' then
                    if is_phix()
                    and tok_col+4<=src_len
                    and src[tok_col..tok_col+4]="ilASM" then
                        tok_col += 4
                    else
                        bool ok = is_C()
                        if ok then
                            ok = (tok_col+6<=src_len and src[tok_col..tok_col+6]="include") or
                                 (tok_col+5<=src_len and src[tok_col..tok_col+5]="ifndef")
                        end if
                        if not ok then
                            {} = tok_error("unrecognised")
                            return
                        end if
                        tok_col += 6
                    end if
                    tok_start += 1
                    tok_type = LETTER
                    std_ident()
                elsif tok_ch='!' then
                    if tok_mode=GT_WHOLE_FILE then
                        src[tok_col-1] = '/'
                        src[tok_col] = '/'
                    end if
                    line_comment()
                elsif tok_ch='[' then
                    if tok_mode=GT_WHOLE_FILE then
                        src[tok_col-1] = '/'
                        src[tok_col] = '*'
                    end if
                    tok_col -= 1
                    block_comment(tok_col,2)
                else
                    while find(upper(tok_ch),"0123456789ABCDEF_") do
                        tok_col += 1
                        if tok_col>length(src) then tok_ch = ' ' exit end if
                        tok_ch = src[tok_col]
                    end while
--                  tok_col -= 1
                    if tok_col<=tok_start
                    or find(charset[tok_ch],{'#','\'','"','`',DIGIT,LETTER}) then
--if ext!=4 then -- C[?!]
if not is_C() then
                        {} = tok_error("unrecognised",tok_col+1)
                        return
end if
                    end if
--                  std_token()
                    return
                end if
            case '\'':
                if is_phix() then
                    -- aside: GT_WHOLE_FILE ensures we'll hit \n
                    -- test set:  legal     illegal
                    --             'a'        '
                    --             '\n'       ''
                    --                        'ab'
                    tok_col += 1
                    tok_ch = src[tok_col]           -- (safe)
                    if tok_ch='\\' then
                        tok_col += 1
                        tok_ch = src[tok_col]       -- (safe)
                        if tok_ch='#'
                        or tok_ch='x' then
                            -- eg '\#62'    -- (treat as #62)
                            tok_type = '#'
                            tok_start += 2
                            for k=1 to 2 do
                                tok_col += 1
                                tok_ch = src[tok_col] -- (safe)
                                if not find(upper(tok_ch),"0123456789ABCDEF") then
                                    {} = tok_error("hex digit expected")
                                    return
                                end if
                            end for
--                          std_token()
--2/5/21 let p2js_emit.e deal with it instead:
--                      elsif tok_ch='t' then
--                          {} = tok_error("tab character:illegal",tok_col-1)
                        end if
                    elsif tok_ch='\'' then
                        {} = tok_error("illegal")
                        return
                    end if
                    tok_col += 1
                    if charset[tok_ch]=EOL
                    or src[tok_col]!='\'' then  -- (safe)
                        {} = tok_error(`' expected`)
                        return
                    end if
--                  if tok_type='#' then
--                      tok_col -= 1
--                      std_token()
--                      tok_col += 1
--                  else
--                      std_token()
--                  end if
--                  break
                    tok_col += 1
                    return
                end if
--              tok_ch = '\'' -- (still is)
                fallthrough
            case '"', '`':
                integer cq = tok_ch, startline = tok_line, midlinestart = tok_linestart
                while true do
                    tok_col += 1
                    tok_ch = src[tok_col]
                    if tok_ch='\\' and cq!='`' then
                        tok_col += 1
                        tok_ch = src[tok_col]
                    elsif tok_ch=cq then
                        if tok_col=tok_start+1 
--(28/2/21 taken out for inside script tags)
--                      and is_phix()
--                      and (is_phix() or is_py())
                        and tok_col<src_len
                        and src[tok_col+1]=cq then
                            -- triplequote handling...
                            integer cqc = 0  -- (start counting em to 3...)
                            tok_col += 1
                            while true do
                                tok_col += 1
                                if tok_col>src_len then
                                    tok_line = startline
                                    {} = tok_error("missing closing quote",tok_start+3)
                                    return
                                end if
                                tok_ch = src[tok_col]
                                if tok_ch=cq then
                                    cqc += 1
                                    if cqc=3 then exit end if
                                else
                                    cqc = 0
                                    if tok_ch='\n' then
                                        tok_line += 1
                                        midlinestart = tok_col+1
                                    end if
                                end if
                            end while
                            tok_start += 2
                            if tok_mode=GT_WHOLE_FILE then
                                src[tok_start] = '`'
                                tok_col -= 2 -- (undone rsn)
                                src[tok_col] = '`'
                            end if
                            tok_type = '`'
                        end if
                        exit
                    end if
                    if tok_type='`' then
                        if tok_col=src_len then
                            tok_line = startline
                            {} = tok_error("missing closing quote",tok_start)
                            return
                        end if
                        if tok_ch='\n' then
                            tok_line += 1
                            midlinestart = tok_col+1
                        end if
                    elsif charset[tok_ch]=EOL then
                        {} = tok_error("missing closing quote")
                        return
                    end if
                end while
                if tok_type='`' then
--                  add_tok({tok_type,tok_start,i,startline,tok_start-tok_linestart,tok_line})
                    if tok_mode=GT_WHOLE_FILE then
                        if cq='"' then tok_col += 2 end if -- triplequote
                    end if
--              else
--                  std_token()
                end if
                tok_linestart = midlinestart
                tok_col += 1
                return
            case DIGIT:
                while true do
                    tok_col += 1
                    if tok_col>src_len then exit end if
                    tok_ch = src[tok_col]
                    tok_type = charset[tok_ch]
                    if tok_type!=DIGIT and tok_ch!='_' then
--DEV 0o, 0t, 0x, 0(bb)...
--                      if tok_ch='b' and i=tok_start+1 and src[tok_start]='0' then
--                      if i=tok_start+1 and src[tok_start]='0' then
                        if (tok_ch='b' or lower(tok_ch)='x' or tok_ch='o' or tok_ch='d')
                        and tok_col=tok_start+1 and src[tok_start]='0' then
                            -- 0bNNNN or 0xNNNN format number
                            string valid = iff(tok_ch='b'?"01_":
                                           iff(tok_ch='o'?"01234567_":
                                           iff(tok_ch='d'?"0123456789_"
                                                         :"0123456789ABCDEF_")))
                            while true do
                                tok_col += 1
                                if tok_col>length(src) then exit end if
                                tok_ch = src[tok_col]
                                if not find(upper(tok_ch),valid) then exit end if
                            end while
                            tok_col -= 1
                            if tok_col<tok_start+2 then
                                {} = tok_error(valid&" expected")
                                return
                            end if
                            tok_type = '#'  -- will need this later...
--/*
                            string bNumber = src[tok_start..i],
--                                 hNumber = sprintf("0x%x",to_number(bNumber))
                                   hNumber = sprintf("#%x",to_number(bNumber))
                            integer wasi = i,
                                    l = length(hNumber)
                            bNumber[1..l] = hNumber
                            src[tok_start..i] = bNumber
                            i = tok_start+l-1
--*/
--                          std_token()
--                          i = wasi
                            tok_col += 1
                            return
                        elsif tok_ch='(' and tok_col=tok_start+1 and src[tok_start]='0' then
                            -- 0(bb)NNNN format number
                            integer base = 0
                            while true do
                                tok_col += 1
                                if tok_col>length(src) then exit end if
                                tok_ch = src[tok_col]
                                if not find(tok_ch,"0123456789") then exit end if
                                base = base*10+(tok_ch-'0')
                            end while
                            if base=0 or base>36 then
                                {} = tok_error("invalid base")
                                return
                            end if
                            if tok_ch!=')' then
                                {} = tok_error(") expected")
                                return
                            end if
                            string valid = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"[1..base]
                            integer actstart = tok_col
                            while true do
                                tok_col += 1
                                if tok_col>length(src) then exit end if
                                tok_ch = src[tok_col]
                                if not find(upper(tok_ch),valid) then exit end if
                            end while
                            tok_col -= 1
                            if tok_col=actstart then
                                {} = tok_error(valid&" expected")
                                return
                            end if
--erm...??
                            tok_type = '#'  -- will need this later...
--                          std_token()
--                          break
                            tok_col += 1
                            return
                        end if
                        for x=1 to 2 do
                            integer xc = ".e"[x]
                            if lower(tok_ch)=xc and (x=2 or src[tok_col+1]!='.') then
                                integer tok_col0 = tok_col
                                while true do
                                    tok_col += 1
                                    tok_ch = src[tok_col]
                                    tok_type = charset[tok_ch]
                                    if tok_type!=DIGIT
                                    and tok_ch!='_'
                                    and ((tok_col!=tok_col0+1) or not find(tok_ch,"-+")) then
                                        if tok_col=tok_col0+1 then
                                            -- eg "2. ", "3e "
                                            {} = tok_error("digit expected")
                                            return
                                        end if
                                        exit
                                    end if
                                end while
                            end if
                        end for
                        exit
                    end if
                end while
--              tok_col -= 1
                tok_type = DIGIT
--              std_token()
                return
            case LETTER:
                while true do
                    tok_col += 1
                    if tok_col>length(src) then exit end if
                    tok_ch = src[tok_col]
                    tok_type = charset[tok_ch]
                    if tok_type!=LETTER and tok_type!=DIGIT then exit end if
                end while
                if tok_ch='"' and tok_col=tok_start+1 and src[tok_start]='x' then
                    tok_start += 2 -- (skip the `x"`)
                    while true do
                        tok_col += 1
                        if tok_col>length(src) then
                            {} = tok_error("unexpected eof")
                            exit
                        end if
                        tok_ch = src[tok_col]
                        if tok_ch='"' then exit end if
                        if not find(upper(tok_ch)," 0123456789ABCDEF_") then
                            {} = tok_error("unexpected char:"&tok_ch)
                            exit
                        end if
                    end while
                    tok_type = HEXSTR
--                  tok_col -= 1    -- (omit closing dblquote)
--                  std_token()
                    tok_col += 1    -- (but treat it as done)
                    return
                else
                    tok_type = LETTER
--                  tok_col -= 1
--                  std_ident()
                    return
                end if
            default: -- (including ILLEGAL)
                {} = tok_error("unrecognised")
                return
        end switch
        tok_col += 1
    end while
--X -- (result in global tokens)
    tok_ch = -1
    tok_type = EOL
end procedure
--*!/

--/*
string
ts = """
_____this
     string\thing""",
tq = `
_____this
     string\thing`
--(me: drat, they're the same...)
?ts
?tq
--*/
--(me: so this [error] wouldn't really help anyway...)
--(me: anyway, the \r\n thing also makes it all a bit moot)
--                          {} = tok_error("use backtick not triplequote")
--                          return
--DEV then _ (underscore) handling has to be done at a later date...
--      we also need to substitute "\r\n" with "\n" throughout...
--/*
if 0 then
-- So anyway, I quickly knocked this up, not sure where it might get used yet:
string
ts = """
_____this
     string\thing""",
tq = `
_____this
     string\thing`
?ts
?tq
end if
with trace
--trace(1)
function backtick_cleanup(string s)
if match("\\r\\n",s) then ?9/0 end if -- (griefsaver?)
if s[1]='`' then ?9/0 end if -- (griefsaver?)
    s = substitute(s,"\r\n","\n")
    if length(s) and s[1]='\n' then
        s = s[2..$]
        integer k = 0
        while length(s) and s[1]='_' do
            k += 1
            s = s[2..$]
        end while
        if k then
            for i=length(s)-1 to 1 by -1 do
                -- trim at most k spaces from the start of every line
                if s[i]='\n' and s[i+1]=' ' then
                    integer z = 1
                    while z<k 
                      and i+z+1<=length(s)
                      and s[i+z+1]=' ' do
                        z += 1
                    end while
                    s[i+1..i+z] = ""
                end if
            end for
        end if
    end if
    return s
end function
--?backtick_cleanup("\r\n_____this\r\n_____string\\thing")
?backtick_cleanup("\r\n_____this\r\n     string\\thing")
--*/


