--
-- demo\rosetta\Compiler\core.e
-- ============================
--
--  Standard declarations and routines used by lex.exw, parse.exw, cgen.exw, and interp.exw
--
--
global constant EOF = -1, STDIN = 0, STDOUT = 1

global enum type nary NONE=0, UNARY=1, BINARY=2 end type

global sequence tkNames = {}    -- eg/ie {"Op_multiply","Op_divide",..}
global sequence precedences = {}
global sequence narys = {}  -- NONE/UNARY/BINARY
global sequence operators = {} -- eg/ie {"*","/","+","-","<","<=",..}
global sequence opcodes = {}    -- idx to tkNames, matching operators

global constant KEYWORDS = new_dict()   -- eg/ie {"if"=>idx to tkNames}

global enum OPERATOR=1, DIGIT, LETTER   -- character classes

global sequence charmap = repeat(0,255)
                charmap['0'..'9'] = DIGIT
                charmap['A'..'Z'] = LETTER
                charmap['a'..'z'] = LETTER
                charmap['_'] = LETTER

function tkName(string s, nary n = NONE, integer precedence = -1)
    tkNames = append(tkNames,s)
    narys = append(narys,n)
    precedences = append(precedences,precedence)
    return length(tkNames)
end function

function tkOp(string s, string op, nary n, integer precedence)
    integer res = tkName(s, n, precedence)
    operators = append(operators,op)
    opcodes = append(opcodes,res)
    for i=1 to length(op) do
        charmap[op[i]] = OPERATOR
    end for
    return res
end function

function tkKw(string s, string keyword)
    integer res = tkName(s)
    putd(keyword, res, KEYWORDS)
    return res
end function

global constant
    tk_EOI           = tkName("End_of_input"),                      --1
    tk_mul           = tkOp("Op_multiply",      "*", BINARY,13),    --2
    tk_div           = tkOp("Op_divide",        "/", BINARY,13),    --3
    tk_mod           = tkOp("Op_mod",           "%", BINARY,13),    --4
    tk_add           = tkOp("Op_add",           "+", BINARY,12),    --5
    tk_sub           = tkOp("Op_subtract",      "-", BINARY,12),    --6
    tk_neg           = tkName("Op_negate",           UNARY, 14),    --7
    tk_not           = tkOp("Op_not",           "!", UNARY, 14),    --8
    tk_lt            = tkOp("Op_less",          "<", BINARY,10),    --9
    tk_le            = tkOp("Op_lessequal",     "<=",BINARY,10),    --10
    tk_gt            = tkOp("Op_greater",       ">", BINARY,10),    --11
    tk_ge            = tkOp("Op_greaterequal",  ">=",BINARY,10),    --12
    tk_eq            = tkOp("Op_equal",         "==",BINARY, 9),    --13
    tk_ne            = tkOp("Op_notequal",      "!=",BINARY, 9),    --14
    tk_assign        = tkOp("Op_assign",        "=", NONE,  -1),    --15
    tk_and           = tkOp("Op_and",           "&&",BINARY, 5),    --16
    tk_or            = tkOp("Op_or",            "||",BINARY, 4),    --17
    tk_if            = tkKw("Keyword_if",   "if"),                  --18
    tk_else          = tkKw("Keyword_else", "else"),                --19
    tk_while         = tkKw("Keyword_while","while"),               --20
    tk_print         = tkKw("Keyword_print","print"),               --21
    tk_putc          = tkKw("Keyword_putc", "putc"),                --22
    tk_LeftParen     = tkOp("LeftParen",        "(", NONE,  -1),    --23
    tk_RightParen    = tkOp("RightParen",       ")", NONE,  -1),    --24
    tk_LeftBrace     = tkOp("LeftBrace",        "{", NONE,  -1),    --25
    tk_RightBrace    = tkOp("RightBrace",       "}", NONE,  -1),    --26
    tk_Semicolon     = tkOp("Semicolon",        ";", NONE,  -1),    --27
    tk_Comma         = tkOp("Comma",            ",", NONE,  -1),    --28
    tk_Identifier    = tkName("Identifier"),                        --29
    tk_Integer       = tkName("Integer"),                           --30
    tk_String        = tkName("String"),                            --31
    tk_Sequence      = tkName("Sequence"),                          --32
    tk_Prints        = tkName("tk_Prints"),                         --33
    tk_Printi        = tkName("tk_Printi")                          --34

global integer input_file = STDIN,
               output_file = STDOUT

type strint(object o)
    return string(o) or integer(o)
end type

global strint tok_line, -- save of line/col at the start of
              tok_col   -- token/comment, for result/errors

global object oneline = ""

constant errfmt = "Line %s column %s:\n%s%s"

function errline()
    oneline = substitute(trim(oneline,"\r\n"),"\t"," ")
    string padding = repeat(' ',tok_col)
    return sprintf("%s\n%s^ ",{oneline,padding})
end function

global procedure error(sequence msg, sequence args={})
    if length(args) then
        msg = sprintf(msg,args)
    end if
    string el = iff(atom(oneline)?"":errline())
    if integer(tok_line) then tok_line = sprintf("%d",tok_line) end if
    if integer(tok_col) then tok_col = sprintf("%d",tok_col) end if
    printf(STDOUT,errfmt,{tok_line,tok_col,el,msg})
    {} = wait_key()
    abort(1)
end procedure

function open_file(string file_name, string mode)
    integer fn = open(file_name, mode)
    if fn = -1 then
        printf(STDOUT, "Could not open %s", {file_name})
        {} = wait_key()
        abort(1)
    end if
    return fn
end function

global procedure open_files(sequence cl)
    if length(cl)>2 then
        input_file = open_file(cl[3],"r")
        if length(cl)>3 then
            output_file = open_file(cl[4],"w")
        end if
    end if
end procedure

global procedure close_files()
    if input_file!=STDIN then close(input_file) end if
    if output_file!=STDOUT then close(output_file) end if
end procedure

global function enquote(string s)
    return sprintf(`"%s"`,substitute(s,"\n","\\n"))
end function

global function unquote(string s)
    if s[1]!='\"' then ?9/0 end if
    if s[$]!='\"' then ?9/0 end if
    s = substitute(s[2..-2],"\\n","\n")
    return s
end function


--/*
constant DEBUG = 1  -- optional sanity checks (cost is quite low,
                    -- -- best left on until actually noticeable.)

global constant EOF = -1, STDIN = 0, STDOUT = 1

global enum type nary NONE=0, UNARY=1, BINARY=2 end type

sequence _tkNames = {}  -- eg/ie {"Op_multiply","Op_divide",..}
sequence _precedences = {}
sequence _narys = {}    -- NONE/UNARY/BINARY
sequence _operators = {} -- eg/ie {"*","/","+","-","<","<=",..}
sequence _opcodes = {}  -- idx to tkNames, matching operators

global constant KEYWORDS = new_dict()   -- eg/ie {"if"=>idx to tkNames}

global enum OPERATOR=1, DIGIT, LETTER   -- character classes

sequence _charmap = repeat(0,255)
         _charmap['0'..'9'] = DIGIT
         _charmap['A'..'Z'] = LETTER
         _charmap['a'..'z'] = LETTER
         _charmap['_'] = LETTER

function tkName(string s, nary n = NONE, integer precedence = -1)
    if DEBUG then -- <optional santity checks>
        if find(s,_tkNames) then ?9/0 end if
        for i=1 to length(s) do
            integer ctype = _charmap[s[i]]
            if ctype!=LETTER then
                if i=1 or ctype!=DIGIT then ?9/0 end if
            end if
        end for
    end if       -- </optional santity checks>
    _tkNames = append(_tkNames,s)
    _narys = append(_narys,n)
    _precedences = append(_precedences,precedence)
    return length(_tkNames)
end function

function tkOp(string s, string op, nary n, integer precedence)
    if DEBUG then -- <optional santity checks>
        if find(op,_operators) then ?9/0 end if
    end if        -- </optional santity checks>
    integer res = tkName(s, n, precedence)
    _operators = append(_operators,op)
    _opcodes = append(_opcodes,res)
    for i=1 to length(op) do
        _charmap[op[i]] = OPERATOR
    end for
    return res
end function

function tkKw(string s, string keyword)
    if DEBUG then -- <optional santity checks>
        if getd_index(keyword,KEYWORDS)!=0 then ?9/0 end if
    end if        -- </optional santity checks>
    integer res = tkName(s)
    putd(keyword, res, KEYWORDS)
    return res
end function

global constant
    tk_EOI           = tkName("End_of_input"),                      --1
    tk_mul           = tkOp("Op_multiply",      "*", BINARY,13),    --2
    tk_div           = tkOp("Op_divide",        "/", BINARY,13),    --3
    tk_mod           = tkOp("Op_mod",           "%", BINARY,13),    --4
    tk_add           = tkOp("Op_add",           "+", BINARY,12),    --5
    tk_sub           = tkOp("Op_subtract",      "-", BINARY,12),    --6
    tk_neg           = tkName("Op_negate",           UNARY, 14),    --7
    tk_not           = tkOp("Op_not",           "!", UNARY, 14),    --8
    tk_lt            = tkOp("Op_less",          "<", BINARY,10),    --9
    tk_le            = tkOp("Op_lessequal",     "<=",BINARY,10),    --10
    tk_gt            = tkOp("Op_greater",       ">", BINARY,10),    --11
    tk_ge            = tkOp("Op_greaterequal",  ">=",BINARY,10),    --12
    tk_eq            = tkOp("Op_equal",         "==",BINARY, 9),    --13
    tk_ne            = tkOp("Op_notequal",      "!=",BINARY, 9),    --14
    tk_assign        = tkOp("Op_assign",        "=", NONE,  -1),    --15
    tk_and           = tkOp("Op_and",           "&&",BINARY, 5),    --16
    tk_or            = tkOp("Op_or",            "||",BINARY, 4),    --17
    tk_if            = tkKw("Keyword_if",   "if"),                  --18
    tk_else          = tkKw("Keyword_else", "else"),                --19
    tk_while         = tkKw("Keyword_while","while"),               --20
    tk_print         = tkKw("Keyword_print","print"),               --21
    tk_putc          = tkKw("Keyword_putc", "putc"),                --22
    tk_LeftParen     = tkOp("LeftParen",        "(", NONE,  -1),    --23
    tk_RightParen    = tkOp("RightParen",       ")", NONE,  -1),    --24
    tk_LeftBrace     = tkOp("LeftBrace",        "{", NONE,  -1),    --25
    tk_RightBrace    = tkOp("RightBrace",       "}", NONE,  -1),    --26
    tk_Semicolon     = tkOp("Semicolon",        ";", NONE,  -1),    --27
    tk_Comma         = tkOp("Comma",            ",", NONE,  -1),    --28
    tk_Identifier    = tkName("Identifier"),                        --29
    tk_Integer       = tkName("Integer"),                           --30
    tk_String        = tkName("String"),                            --31
    tk_Sequence      = tkName("Sequence"),                          --32
    tk_Prints        = tkName("tk_Prints"),                         --33
    tk_Printi        = tkName("tk_Printi")                          --34

global constant operators = _operators
global constant opcodes = _opcodes
global constant precedences = _precedences
global constant narys = _narys
global constant charmap = _charmap
global constant tkNames = _tkNames

global integer input_file = STDIN,
               output_file = STDOUT

type strint(object o)
    return string(o) or integer(o)
end type

global strint tok_line, -- save of line/col at the start of
              tok_col   -- token/comment, for result/errors

global object oneline = ""

constant errfmt = "Line %s column %s:\n%s%s"

function errline()
    oneline = substitute(trim(oneline,"\r\n"),"\t"," ")
    string padding = repeat(' ',tok_col)
    return sprintf("%s\n%s^ ",{oneline,padding})
end function

global procedure error(sequence msg, sequence args={})
    if length(args) then
        msg = sprintf(msg,args)
    end if
    string el = iff(atom(oneline)?"":errline())
    if integer(tok_line) then tok_line = sprintf("%d",tok_line) end if
    if integer(tok_col) then tok_col = sprintf("%d",tok_col) end if
    printf(STDOUT,errfmt,{tok_line,tok_col,el,msg})
    {} = wait_key()
    abort(1)
end procedure

function open_file(string file_name, string mode)
    integer fn = open(file_name, mode)
    if fn = -1 then
        printf(STDOUT, "Could not open %s", {file_name})
        {} = wait_key()
        abort(1)
    end if
    return fn
end function

global procedure open_files(sequence cl)
    if length(cl)>2 then
        input_file = open_file(cl[3],"r")
        if length(cl)>3 then
            output_file = open_file(cl[4],"w")
        end if
    end if
end procedure

global procedure close_files()
    if input_file!=STDIN then close(input_file) end if
    if output_file!=STDOUT then close(output_file) end if
end procedure

global function enquote(string s)
    return sprintf("\"%s\"",substitute(s,"\n","\\n"))
end function

global function unquote(string s)
    if s[1]!='\"' then ?9/0 end if
    if s[$]!='\"' then ?9/0 end if
    s = substitute(s[2..-2],"\\n","\n")
    return s
end function
--*/
