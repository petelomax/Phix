--
-- pwa\src\p2js_tok.e
-- ==================
--
--  Tokeniser for pwa/p2js, creates table of type and start/end indexes into global "text".
--
--? Currently this builds tokens[], but may be changed to return one token at a time...
--
include p2js_basics.e   -- includes/constants/globals/utilities
include p2js_tree.e     -- simple ternary tree for fast identifier lookup
sequence p2js_kwdbg = p2js_keywords -- (debug aid)
--sequence tt_r = tt_reserved

integer i,  -- to text[]
--      lt, -- length(text)
        ch, -- test[i]
        line,
        linestart,
        tokstart,
        toktype -- charset[text[i]]

procedure add_tok(sequence tok)
    tokens = append(tokens,tok)
    toktype = tok[TOKTYPE]
    if toktype>TOKMAX then toktype=SYMBOL end if
--  if show_tok(toktype) then
--DEV/SUG:
--  if show_tok[min(toktype,SYMBOL)] then
        show_token(tok)
--  end if
end procedure

--from basics: global enum TOKTYPE, TOKSTART, TOKFINISH, TOKLINE, TOKCOL, TOKTTIDX, TOKENDLINE=$ -- (one token)
procedure std_token()
    add_tok({toktype,tokstart,i,line,tokstart-linestart})
end procedure

procedure std_ident()
    integer ttidx = tt_idx(tokstart,i)
--  integer ttidx = tt_idx(tokstart,i,is_html())
    add_tok({toktype,tokstart,i,line,tokstart-linestart,ttidx})
end procedure

bool tok_bad = false
global function tok_error(string reason="", integer e=i)
    --
    -- tok_error("why") displays why, sets flag and returns false.
    -- tok_error() returns false or true if "" has happened.
    -- note: ch!=-1 is oft as good as or better than tok_error().
    --       return tok_error("why") should be standard practice.
    --
    -- e can/shd be used to set "shown code snippet's final char"
    --
--  static bool tok_bad = false

    if length(reason) then
        if reason="just_clear_tok_bad" then
            tok_bad = false
            -- and quickly ensure printf() below won't crash
            i = 0
            line = 0
            linestart = 1
        else
--          printf(1,"token error: %s, line %d\n%s\n%s",
--                   {reason,line,text[linestart..e],
--                    repeat(' ',e-linestart)})
            string lpad = repeat(' ',e-linestart)
            if e=i then
                while e<length(text) and text[e+1]>=' ' do
                    e += 1
                end while
            end if
            string ltxt = text[linestart..e]
--DEV filename?
            printf(1,"token error on line %d:\n%s\n%s^%s\n",
                     {line,ltxt,lpad,reason})
            if CRASH then ?9/0 end if
            tok_bad = true
            ch = -1
        end if
    end if
    return tok_bad
end function

function phix_only(integer e=i)
    -- return true === an error
    if not is_phix() then
        return tok_error("phix only",e)
    end if
    return false
end function

procedure block_comment(integer tokstart,adj)
--
-- note: a comment is deemed to start with "--/*" if it ends with "--*/"
--       nested block comments are replaced, "/* /* */ */" ==> "/* /@ @/ */"
--       (since nested comments are *not* supported by js, html, c, or css!)
--
    integer startline = line, nest = 1
--  integer startline = line, startcol = col, nest = 1
    i = tokstart+adj
    while true do
        if i>length(text) then
            line = startline
            {} = tok_error("no closing comment",tokstart)
            return 
        end if
        ch = text[i]
        integer cn = iff(i<lt?text[i+1]:'\0')
        if ch='*' and cn='/' then
            i += 1
            nest -= 1
            if nest=0 then exit end if
            text[i-1] = '@'
        elsif ch='/' and cn='*' then
            if phix_only(i+1) then return end if
            i += 1
            text[i] = '@'
            nest += 1
        elsif ch='\n' then
            line += 1
--10/2/21...
            linestart = i+1
        end if
        i += 1
    end while
--Ah: **ALL** comments in js/html/css **ARE** block comments, so we **DO** need a way
--      to convert them into line comments, roughly like this.
--X -- note: block comments distinguished from line comments by length. [erm, startline!=line?]
    -- DEV add note re phix->js->phix handling, once we know what all the rules really are
    --      (I'm thinking genuine multiline must obvs. stay as they are, but 
    --       for single line phix prefers line comments, unless more tokens
    --       follow on the same line, eg in "exit_cb(Ihandle /*ih*/)", that
    --       trailing ')' forces it to stay in "/**/" format.
    --       There is also eg "var /* Ihandle */ ih" to consider...) [hence BLK_CMT added]
    -- perhaps a better way is simply "/*-- line comment */"
    -- (likewise, "/* some comment --*/" is really "--/* some comment --*/", when transpiling back to .exw)
    -- (and obviously "/* comment */" stays as-is... or rather " com " ==> "/* com */", 
    --                                                         "com --") ==> "--/* com --*/",
    --                                                         "-- com" ==> "-- com".)
--  add_tok({BLK_CMT,tokstart,i-2,startline,line,tokstart-linestart})
    add_tok({BLK_CMT,tokstart,i,startline,tokstart-linestart,line})
--  add_tok({toktype,tokstart,i,line,tokstart-linestart}) -- (std_tok)
--  add_tok({BLK_CMT,tokstart,i-2,startline,startcol,line,tokstart-linestart})
end procedure

procedure line_comment()
    while true do -- (normal "--" or "//")
        -- aside: GT_WHOLE_FILE ensures we'll hit \n
        i += 1
--      if i>length(text) then exit end if
        ch = text[i]
        toktype = charset[ch]
        if toktype = EOL then exit end if
    end while
    i -= 1
    add_tok({COMMENT,tokstart,i,line,tokstart-linestart})
end procedure

global procedure tokenise()
--sequence mcsyms = {"!=","||","==","&&","+=","<=",">=","..","-=","!==","===",">>","*=","&=",">>>",":="}    -- DEV/temp

    {} = tok_error("just_clear_tok_bad",0)
-- or:  (why?: because a single active token is easier to debug...)
--  while ?? do tokens = append(tokens,get_token()) end while
    i = 1
    ch = ' '
    line = 1
    tokens = {}

    while i<=lt and ch!=-1 do
        tokstart = i
        ch = text[i]
--if ch='-' then trace(1) end if
        toktype = charset[ch]
        if ch='$' and is_js() then
            toktype = LETTER
--      else
--          toktype = charset[ch]
        end if
--DEV/temp...
--      if toktype=SYMBOL and ch='`' then toktype='`' end if
        switch toktype
            case EOL:
                line += (ch='\n')
                linestart = i+1
            case SPACE:
                break
--          case MINUS, FWDSLASH:
            case '-', '/':
--if line=933 then trace(1) end if
                if i<lt then
                    integer cn = text[i+1]
                    if cn=ch then
                        -- comment
                        string t4 = iff(i+3<=lt?text[i..i+3]:"")
                        if t4="--/*" then
                            if phix_only(i+3) then return end if
--                          block_comment(i+4)
                            block_comment(i,4)
                            break
                        elsif t4="--*/" then
                            {} = tok_error("unexpected closing comment",i+3)
                            return
                        end if
                        line_comment()
--/*
                        while true do -- (normal "--" or "//")
                            -- aside: GT_WHOLE_FILE ensures we'll hit \n
                            i += 1
--                          if i>length(text) then exit end if
                            ch = text[i]
                            toktype = charset[ch]
                            if toktype = EOL then exit end if
                        end while
                        i -= 1
                        add_tok({COMMENT,tokstart,i,line,tokstart-linestart})
--*/
                        break
                    elsif toktype='/' and cn='*' then
--                      block_comment(i+2)
                        block_comment(i,2)
                        break
                    end if
                end if
                fallthrough
--          case SYMBOL:
            case '?','+','*','!','=',';',',','.','|','<','&',':','>','\\','$','%','~':
                while i<lt 
--                and charset[text[i+1]]=SYMBOL
                  and charset[text[i+1]]>SYMBOL do
                    integer sdx = find(text[tokstart..i+1],multisym)
                    if sdx=0 then exit end if
                    --            ^ ie not say "-(", "*-", "*(", ...
                    toktype = mstoktypes[sdx]
                    i += 1
                end while
--?{"SYMBOL",toktype,text[tokstart..i]}
--DEV just use a specific precedence for this... (PSBSC, though not here, erm??)
--if i>tokstart then
--  string mcsym = text[tokstart..i]
--  if not find(mcsym,mcsyms) then
--      ?{"p2js_tok.e line 199: precedence/special toktype for",mcsym}
--      mcsyms = append(mcsyms,mcsym)
--  end if
--end if
--              if i=tokstart+1 and text[tokstart..i]=".." then
--                  toktype = ELLIPSE
--              elsif i=tokstart+2 and text[tokstart..i]="..." then
--                  toktype = SPREAD
--              end if
                fallthrough
--          case BRACES:
            case '(','[','{','}',']',')':
                std_token()
                break
--          case HEXDEC:
            case '#':
                if is_py() then
                    line_comment()
                    break
                end if
                i += 1
                ch = text[i]
                if ch='i' then
                    if not is_C() -- allow "#include"
                    or i+6>lt 
                    or text[i..i+6]!="include" then
                        {} = tok_error("unrecognised")
                        return
                    end if
                    i += 6
                    tokstart += 1
                    toktype = LETTER
                    std_ident()
                else
                    while find(upper(ch),"0123456789ABCDEF_") do
                        i += 1
                        ch = text[i]
                    end while
                    i -= 1
                    if i<=tokstart
--                  or find(charset[ch],{HEXDEC,SQUOTE,DQUOTE,'`',DIGIT,LETTER}) then
                    or find(charset[ch],{'#','\'','"','`',DIGIT,LETTER}) then
                        {} = tok_error("unrecognised",i+1)
                        return
                    end if
                    std_token()
                end if
--          case SQUOTE:
            case '\'':
                if is_phix() then
                    -- aside: GT_WHOLE_FILE ensures we'll hit \n
                    -- test set:  legal     illegal
                    --             'a'        '
                    --             '\n'       ''
                    --                        'ab'
                    i += 1
                    ch = text[i]            -- (safe)
                    if ch='\\' then
                        i += 1
                        ch = text[i]        -- (safe)
                    elsif ch='\'' then
                        {} = tok_error("illegal")
                        return
                    end if
                    i += 1
                    if charset[ch]=EOL
                    or text[i]!='\'' then   -- (safe)
                        {} = tok_error(`' expected`)
                        return
                    end if
                    std_token()
                    break
                end if
--              ch = '\'' -- (still is)
                fallthrough
--          case DQUOTE, '`':
            case '"', '`':
                integer cq = ch, startline = line, midlinestart = linestart
                while true do
                    i += 1
                    ch = text[i]
                    if ch='\\' and cq!='`' then
                        i += 1
                        ch = text[i]
                    elsif ch=cq then
                        if i=tokstart+1 
--                      and is_phix()
                        and (is_phix() or is_py())
                        and i<lt
                        and text[i+1]=cq then
                            -- triplequote handling...
                            integer cqc = 0  -- (start counting em to 3...)
                            i += 1
                            while true do
                                i += 1
                                if i>lt then
                                    line = startline
                                    {} = tok_error("missing closing quote",tokstart+3)
                                    return
                                end if
                                ch = text[i]
                                if ch=cq then
                                    cqc += 1
                                    if cqc=3 then exit end if
                                else
                                    cqc = 0
                                    i += (ch='\\')
--10/2/21:
--                                  line += (ch='\n')
                                    if ch='\n' then
                                        line += 1
                                        midlinestart = i+1
                                    end if
                                end if
                            end while
                            tokstart += 2
                            text[tokstart] = '`'
                            i -= 2 -- (undone rsn)
                            text[i] = '`'
                            toktype = '`'
--                          {text[tokstart],text[i],toktype} @= '`'
                        end if
                        exit
                    end if
                    if toktype='`' then
                        if i=lt then
                            line = startline
                            {} = tok_error("missing closing quote",tokstart)
                            return
                        end if
--10/2/21
--                      line += (ch='\n')
                        if ch='\n' then
                            line += 1
                            midlinestart = i+1
                        end if
                    elsif charset[ch]=EOL then
                        {} = tok_error("missing closing quote")
                        return
                    end if
                end while
                if toktype='`' then
--removed 24/1/21:
--                  if phix_only(i) then return end if
--                  add_tok({'`',tokstart,i,startline,line,tokstart-linestart})
                    add_tok({'`',tokstart,i,startline,tokstart-linestart,line})
                    if cq='"' then i += 2 end if -- triplequote
                else
                    std_token()
                end if
                linestart = midlinestart
            case DIGIT:
                while true do
                    i += 1
                    ch = text[i]
                    toktype = charset[ch]
                    if toktype!=DIGIT then
--DEV 0o, 0t, 0x, 0(bb)...
                        if ch='b' and i=tokstart+1 and text[tokstart]='0' then
                            -- 0bNNNN format number
                            while true do
                                i += 1
                                ch = text[i]
                                if not find(ch,"01_") then exit end if
                            end while
                            i -= 1
                            if i<tokstart+2 then
                                {} = tok_error("0/1 expected")
                                return
                            end if
--                          toktype = BINDEC
                            toktype = 'b'   -- 0bNNN format
                            std_token()
                            break
                        end if
                        for x=1 to 2 do
                            integer xc = ".e"[x]
                            if lower(ch)=xc and (x=2 or text[i+1]!='.') then
                                integer i0 = i
                                while true do
                                    i += 1
                                    ch = text[i]
                                    toktype = charset[ch]
                                    if toktype!=DIGIT
                                    and ((i!=i0+1) or not find(ch,"-+")) then
                                        if i=i0+1 then
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
                i -= 1
                toktype = DIGIT
                std_token()
            case LETTER:
                while true do
                    i += 1
                    ch = text[i]
                    toktype = charset[ch]
--                  if toktype!=LETTER and toktype!=DIGIT then exit end if
                    if toktype!=LETTER and toktype!=DIGIT and (toktype!='$' or not is_js()) then exit end if
                end while
                toktype = LETTER
                i -= 1
                std_ident()
            default: -- (including ILLEGAL)
                {} = tok_error("unrecognised")
                return
        end switch
        i += 1
    end while
    -- (result in global tokens)
end procedure

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


