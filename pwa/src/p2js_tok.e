--
-- pwa\src\p2js_tok.e
-- ==================
--
--  Tokeniser for pwa/p2js, creates table of type and start/end indexes into global "src".
--
--? Currently this builds tokens[], but may be changed to return one token at a time...
--  To do:
--  Under the control of p2js_parse.e, stack/restore tokens/i/ch/line/tokstart/toktype etc.
--  Have a lookahead on comments so that factor() etc does not need to do the wastdx stuff [?]
--   (plus whatever, if anything, is needed for any skipped comments warnings)
--
include p2js_basics.e   -- includes/constants/globals/utilities
include p2js_tree.e     -- simple ternary tree for fast identifier lookup
--sequence p2js_kwdbg = p2js_keywords -- (debug aid)
--sequence tt_r = tt_reserved

integer i,  -- to src[]
--      lt, -- length(src)
        tok_ch, -- src[i]
        line,
        linestart,
        tokstart,
        toktype -- charset[src[i]]

procedure add_tok(sequence tok)
    tokens = append(tokens,tok)
    toktype = tok[TOKTYPE]
    if toktype>TOKMAX then toktype=SYMBOL end if
    if show_any then
        show_token(tok)
    end if
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
    -- note: tok_ch!=-1 is oft as good as or better than tok_error().
    --       return tok_error("why") should be standard practice.
    --
    -- e can/shd be used to set "shown code snippet's final char"
    --
    if length(reason) then
--      string lpad = repeat(' ',e-linestart)
        string lpad = repeat(' ',max(e-linestart,0))
--      if e=i then
            while e<length(src) and src[e+1]>=' ' do
                e += 1
            end while
--      end if
--DEV filename?
        string ltxt = src[linestart..e],
--      string ltxt = src[linestart-100..e],
                msg = sprintf("token error in %s on line %d:\n%s\n%s^%s\n",
                              {current_file,line+src_offset,ltxt,lpad,reason})
        if CRASH then crash(msg) end if
        puts(1,msg)
        tok_bad = true
        tok_ch = -1
    end if
    return tok_bad
end function

function phix_only(integer e=i, string m="phix only")
    -- return true === an error
    if not is_phix() then
        if m="nnc" then
            m = "no nested comments in "&ext_names[ext]
        end if
        return tok_error(m,e)
    end if
    return false
end function

procedure block_comment(integer tokstart,adj)
--
-- note: a comment is deemed to start with "--/*" if it ends with "--*/"
--       nested block comments are replaced, "/* /* */ */" ==> "/* /@ @/ */"
--       (since nested comments are *not* supported by js, html, c, or css!)
--
--  integer startline = line, nest = 1
    integer startline = line, startcol = tokstart-linestart, nest = 1
--  integer startline = line, startcol = col, nest = 1
    i = tokstart+adj
    while true do
        if i>length(src) then
            line = startline
            {} = tok_error("no closing comment",tokstart)
            return 
        end if
        tok_ch = src[i]
        integer cn = iff(i<lt?src[i+1]:'\0')
        if tok_ch='*' and cn='/' then
            i += 1
            nest -= 1
            if nest=0 then exit end if
            src[i-1] = '@'
        elsif tok_ch='/' and cn='*' then
            if phix_only(i+1,"nnc") then return end if
            i += 1
            src[i] = '@'
            nest += 1
        elsif tok_ch='\n' then
            line += 1
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
    --       There is also eg "let /* Ihandle */ ih" to consider...) [hence BLK_CMT added]
    -- perhaps a better way is simply "/*-- line comment */"
    -- (likewise, "/* some comment --*/" is really "--/* some comment --*/", when transpiling back to .exw)
    -- (and obviously "/* comment */" stays as-is... or rather " com " ==> "/* com */", 
    --                                                         "com --") ==> "--/* com --*/",
    --                                                         "-- com" ==> "-- com".)
    toktype = BLK_CMT
    add_tok({toktype,tokstart,i,startline,startcol,line})
end procedure

procedure line_comment()
    -- (normal "--" or "//")
--  while true do 
    while i<=length(src) and charset[src[i]]!=EOL do
        i += 1 -- (GT_WHOLE_FILE ensures we'll hit \n)
--      if i>length(src) then exit end if
--      tok_ch = src[i]
--      toktype = charset[tok_ch]
--      if toktype = EOL then exit end if
----        if charset[src[i]] = EOL then exit end if
    end while
    i -= 1 -- (let main loop of tokenise() handle the EOL, 
           --  besides, we absolutely want that -1 here.)
--maybe:
    toktype = COMMENT
    std_token()
--  add_tok({COMMENT,tokstart,i,line,tokstart-linestart})
end procedure

global procedure tokenise()

-- or:  (why?: because a single active token is easier to debug...)
--  while ?? do tokens = append(tokens,get_token()) end while

--  {} = tok_error("just_clear_tok_bad",0)
    tok_bad = false 
    i = 1
    tok_ch = ' '
    line = 1
    linestart = 1
    tokens = {}
--  charset['$'] = iff(is_js()?LETTER:SYMBOL)
    charset['$'] = iff(is_js()?LETTER:'$')

    while i<=lt and tok_ch!=-1 do
        tokstart = i
        tok_ch = src[i]
        toktype = charset[tok_ch]
        switch toktype
            case EOL:
                line += (tok_ch='\n')
--if line=58 then trace(1) end if
                linestart = i+1
            case SPACE:
                break
            case '-', '/':
                if i<lt then
                    integer cn = src[i+1]
                    if cn=tok_ch then
                        -- comment
                        string t4 = iff(i+3<=lt?src[i..i+3]:"")
                        if t4="--/*" then
                            if phix_only(i+3) then return end if
                            block_comment(i,4)
                            break
                        elsif t4="--*/" then
                            {} = tok_error("unexpected closing comment",i+3)
                            return
                        end if
                        line_comment()
                        break
                    elsif toktype='/' and cn='*' then
                        block_comment(i,2)
                        break
                    end if
                end if
                fallthrough
            case '?','+','*','!','=',';',',','.','|','<','&',':','>','\\','$','%','~','^':
                while i<lt 
                  and charset[src[i+1]]>SYMBOL do
                    integer sdx = find(src[tokstart..i+1],multisym)
                    if sdx=0 then exit end if
                    --            ^ ie not say "-(", "*-", "*(", ...
                    toktype = mstoktypes[sdx]
                    i += 1
                end while
                fallthrough
            case '(','[','{','}',']',')':
                std_token()
                break
            case '#':
--              if is_py() then
--                  line_comment()
--                  break
--              end if
                i += 1
                tok_ch = src[i]
                if tok_ch='i' then
                    if not is_C() -- allow "#include"
                    or i+6>lt 
                    or src[i..i+6]!="include" then
                        {} = tok_error("unrecognised")
                        return
                    end if
                    i += 6
                    tokstart += 1
                    toktype = LETTER
                    std_ident()
                else
                    while find(upper(tok_ch),"0123456789ABCDEF_") do
                        i += 1
                        if i>length(src) then tok_ch = ' ' exit end if
                        tok_ch = src[i]
                    end while
                    i -= 1
                    if i<=tokstart
                    or find(charset[tok_ch],{'#','\'','"','`',DIGIT,LETTER}) then
                        {} = tok_error("unrecognised",i+1)
                        return
                    end if
                    std_token()
                end if
            case '\'':
                if is_phix() then
                    -- aside: GT_WHOLE_FILE ensures we'll hit \n
                    -- test set:  legal     illegal
                    --             'a'        '
                    --             '\n'       ''
                    --                        'ab'
                    i += 1
                    tok_ch = src[i]             -- (safe)
                    if tok_ch='\\' then
                        i += 1
                        tok_ch = src[i]         -- (safe)
                        if tok_ch='#' then
                            -- eg '\#62'    -- (treat as #62)
                            toktype = '#'
                            tokstart += 2
                            for k=1 to 2 do
                                i += 1
                                tok_ch = src[i] -- (safe)
                                if not find(upper(tok_ch),"0123456789ABCDEF") then
                                    {} = tok_error("hex digit expected")
                                    return
                                end if
                            end for
--                          std_token()
--2/5/21 let p2js_emit.e deal with it instead:
--                      elsif tok_ch='t' then
--                          {} = tok_error("tab character:illegal",i-1)
                        end if
                    elsif tok_ch='\'' then
                        {} = tok_error("illegal")
                        return
                    end if
                    i += 1
                    if charset[tok_ch]=EOL
                    or src[i]!='\'' then    -- (safe)
                        {} = tok_error(`' expected`)
                        return
                    end if
                    if toktype='#' then
                        i -= 1
                        std_token()
                        i += 1
                    else
                        std_token()
                    end if
                    break
                end if
--              tok_ch = '\'' -- (still is)
                fallthrough
            case '"', '`':
                integer cq = tok_ch, startline = line, midlinestart = linestart
                while true do
                    i += 1
                    tok_ch = src[i]
                    if tok_ch='\\' and cq!='`' then
                        i += 1
                        tok_ch = src[i]
                        if tok_ch='t' then
                            {} = tok_error("tab character:illegal",i-1)
                        end if
                    elsif tok_ch=cq then
                        if i=tokstart+1 
--(28/2/21 taken out for inside script tags)
--                      and is_phix()
--                      and (is_phix() or is_py())
                        and i<lt
                        and src[i+1]=cq then
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
                                tok_ch = src[i]
                                if tok_ch=cq then
                                    cqc += 1
                                    if cqc=3 then exit end if
                                else
                                    cqc = 0
--doh (28/3/21)
--                                  i += (tok_ch='\\')
--10/2/21:
--                                  line += (tok_ch='\n')
                                    if tok_ch='\n' then
                                        line += 1
                                        midlinestart = i+1
                                    end if
                                end if
                            end while
                            tokstart += 2
                            src[tokstart] = '`'
                            i -= 2 -- (undone rsn)
                            src[i] = '`'
                            toktype = '`'
--                          {src[tokstart],src[i],toktype} @= '`'
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
--                      line += (tok_ch='\n')
                        if tok_ch='\n' then
                            line += 1
                            midlinestart = i+1
                        end if
                    elsif charset[tok_ch]=EOL then
                        {} = tok_error("missing closing quote")
                        return
                    end if
                end while
                if toktype='`' then
                    add_tok({toktype,tokstart,i,startline,tokstart-linestart,line})
                    if cq='"' then i += 2 end if -- triplequote
                else
                    std_token()
                end if
                linestart = midlinestart
            case DIGIT:
                while true do
                    i += 1
                    if i>lt then exit end if
                    tok_ch = src[i]
                    toktype = charset[tok_ch]
                    if toktype!=DIGIT and tok_ch!='_' then
--DEV 0o, 0t, 0x, 0(bb)...
--                      if tok_ch='b' and i=tokstart+1 and src[tokstart]='0' then
--                      if i=tokstart+1 and src[tokstart]='0' then
                        if (tok_ch='b' or lower(tok_ch)='x' or tok_ch='o' or tok_ch='d')
                        and i=tokstart+1 and src[tokstart]='0' then
                            -- 0bNNNN or 0xNNNN format number
                            string valid = iff(tok_ch='b'?"01_":
                                           iff(tok_ch='o'?"01234567_":
                                           iff(tok_ch='d'?"0123456789_"
                                                         :"0123456789ABCDEF_")))
                            while true do
                                i += 1
                                if i>length(src) then exit end if
                                tok_ch = src[i]
                                if not find(upper(tok_ch),valid) then exit end if
                            end while
                            i -= 1
                            if i<tokstart+2 then
                                {} = tok_error(valid&" expected")
                                return
                            end if
                            toktype = '#'   -- will need this later...
--/*
                            string bNumber = src[tokstart..i],
--                                 hNumber = sprintf("0x%x",to_number(bNumber))
                                   hNumber = sprintf("#%x",to_number(bNumber))
                            integer wasi = i,
                                    l = length(hNumber)
                            bNumber[1..l] = hNumber
                            src[tokstart..i] = bNumber
                            i = tokstart+l-1
--*/
                            std_token()
--                          i = wasi
                            break
                        elsif tok_ch='(' and i=tokstart+1 and src[tokstart]='0' then
                            -- 0(bb)NNNN format number
--                          i += 1
                            integer base = 0
                            while true do
                                i += 1
                                if i>length(src) then exit end if
                                tok_ch = src[i]
                                if not find(tok_ch,"0123456789") then exit end if
                                base = base*10+tok_ch-'0'
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
                            integer actstart = i
                            while true do
                                i += 1
                                if i>length(src) then exit end if
                                tok_ch = src[i]
                                if not find(upper(tok_ch),valid) then exit end if
                            end while
                            i -= 1
                            if i=actstart then
                                {} = tok_error(valid&" expected")
                                return
                            end if
--erm...??
                            toktype = '#'   -- will need this later...
                            std_token()
                            break
                        end if
                        for x=1 to 2 do
                            integer xc = ".e"[x]
                            if lower(tok_ch)=xc and (x=2 or src[i+1]!='.') then
                                integer i0 = i
                                while true do
                                    i += 1
                                    tok_ch = src[i]
                                    toktype = charset[tok_ch]
                                    if toktype!=DIGIT
                                    and ((i!=i0+1) or not find(tok_ch,"-+")) then
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
                    if i>length(src) then exit end if
                    tok_ch = src[i]
                    toktype = charset[tok_ch]
--20/3/21 put back now charset updated above... (remove in a couple of weeks time...)
                    if toktype!=LETTER and toktype!=DIGIT then exit end if
--                  if toktype!=LETTER and toktype!=DIGIT and (toktype!='$' or not is_js()) then exit end if
                end while
                if tok_ch='"' and i=tokstart+1 and src[tokstart]='x' then
                    tokstart += 2 -- (skip the `x"`)
                    while true do
                        i += 1
                        if i>length(src) then
                            {} = tok_error("unexpected eof")
                            exit
                        end if
                        tok_ch = src[i]
                        if tok_ch='"' then exit end if
                        if not find(upper(tok_ch)," 0123456789ABCDEF_") then
                            {} = tok_error("unexpected char:"&tok_ch)
                            exit
                        end if
                    end while
                    toktype = HEXSTR
                    i -= 1  -- (omit closing dblquote)
                    std_token()
                    i += 1  -- (but treat it as done)
                else
                    toktype = LETTER
                    i -= 1
                    std_ident()
                end if
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


