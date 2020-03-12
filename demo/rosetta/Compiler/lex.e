--
-- demo\\rosetta\\Compiler\\lex.e
-- ==============================
--
--  The reusable part of lex.exw
--  This is only kept separate from core.e for consistency with later modules.

include core.e

integer ch = ' ',
        line = 0,
        col = 0

procedure eof(string s)
    error("%s in %s literal",{iff(ch=EOF?"EOF":"EOL"),s})
end procedure

function next_ch()
    while 1 do
        col += 1
        if oneline=EOF then
            ch = EOF
            exit
        elsif col>length(oneline) then
            line += 1
            col = 0
            oneline = gets(input_file)
        else
            ch = oneline[col]
            exit
        end if
    end while
    return ch
end function

constant whitespace = " \t\r\n\x0B\xA0"
-- (0x0B is Vertical Tab, 0xA0 is Non-breaking space)

procedure skipspacesandcomments()
    while 1 do
        if not find(ch,whitespace) then
            if ch='/' and col<length(oneline) and oneline[col+1]='*' then
                tok_line = line -- (in case of EOF error)
                tok_col = col
                ch = next_ch()  -- (can be EOF)
                ch = next_ch()  -- (    ""    )
                while 1 do
                    if ch='*' then
                        ch = next_ch()
                        if ch='/' then exit end if
                    elsif ch=EOF then
                        error("EOF in comment")
                    else
                        ch = next_ch()
                    end if
                end while
            else
                exit
            end if
        end if
        ch = next_ch()
    end while
end procedure

function escape_char(string s)
    ch = next_ch() -- (discard the '\\')
    if ch='n' then
        ch = '\n'
    elsif ch='\\' then
        ch = '\\'
    elsif ch=EOF
       or ch='\n' then
        eof(s)
    else
        error(`unknown escape sequence \%c`, {ch})
    end if
    return ch
end function

function char_lit()
integer startch = ch
integer res = next_ch() -- (skip opening quote, save res)
    if ch=startch then
        error("empty character constant")
    elsif ch='\\' then
        res = escape_char("character")
    end if
    ch = next_ch()
    if ch=EOF
    or ch='\n' then
        eof("character")
    elsif ch!=startch then
        error("multi-character constant")
    end if
    ch = next_ch()
    return {tk_Integer, res}
end function

function string_lit()
integer startch = ch
string text = ""
    while next_ch()!=startch do
        if ch=EOF
        or ch='\n' then
            eof("string")
        elsif ch='\\' then
            ch = escape_char("string")
        end if
        text &= ch
    end while
    ch = next_ch()
    return {tk_String, text}
end function

function op()
sequence operator = {ch}
    ch = next_ch()
    while charmap[ch]=OPERATOR
      and find(operator&ch,operators) do
        -- (^ ie/eg merge ">=", but not ");")
        operator &= ch
        ch = next_ch()
    end while
    integer k = find(operator,operators)
    if k=0 then error("unknown operator") end if
    return {opcodes[k], 0} -- (0 unused)
end function

function int()
integer i = 0
    while charmap[ch]=DIGIT do
        i = i*10 + (ch-'0')
        ch = next_ch()
    end while
    if charmap[ch]=LETTER then
        error("invalid number")
    end if
    return {tk_Integer, i}
end function

function ident()
string text = ""
    while find(charmap[ch],{LETTER,DIGIT}) do
        text &= ch
        ch = next_ch()
    end while
    integer keyword = getd(text,KEYWORDS)
    if keyword!=NULL then
        return {keyword, 0} -- (0 unused)
    end if
    return {tk_Identifier, text}
end function

function get_tok()
    skipspacesandcomments()
    tok_line = line
    tok_col  = col
    switch ch do
        case EOF  then return {tk_EOI, 0} -- (0 unused)
        case '\'' then return char_lit()
        case '"'  then return string_lit()
        else
            switch charmap[ch] do
                case OPERATOR then return op()
                case DIGIT then return int()
                case LETTER then return ident()
                else error("unrecognized character: (%d)", {ch})
            end switch
    end switch
end function

global function lex()
sequence toks = {}
    integer tok = -1
    object v
    while tok!=tk_EOI do
        {tok,v} = get_tok()
        toks = append(toks,{tok_line,tok_col,tok,v})
    end while
    return toks
end function

