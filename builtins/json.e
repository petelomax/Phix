--
-- builtins/json.e
-- ===============
--
--  A simple json parser/printer/converter
--
--  Use:
--      object res = print_json(object fn, object o, bool trim_res=false)
--          fn: an open file handle (1 for the console), or "" to return a string
--          o: a JSON object to print
--          trim_res: ignored if fn is an integer, otherwise when true strips all 
--                      whitespace and newlines from the returned result
--      Returns 0 if parameter o is not a valid JSON object, otherwise
--      if fn was originally "", returns a string, else fn unaltered.
--      Note: it may get part way through before detecting an error.
--
--      object res = parse_json(string s)
--          s: should contain a valid string representation of a JSON object.
--
--      The result of parsing a JSON string is as follows:
--          Numbers and Strings (without quotes) are held natively
--          An object is held as a sequence with a first element of -1 (JSON_OBJECT), 
--                       and the rest pairs, the first of which is a string and the 
--                       second any of these types. Note the subscript of the first 
--                       pair in the object is therefore always [2].
--          An array is held as a sequence with a first element of -2 (JSON_ARRAY),
--                      and the rest any of these types. Note the subscript of the 
--                      first element in the array is therefore always [2].
--          A keyword is held as a sequence with a first element of -3 (JSON_KEYWORD),
--                       and precisely one more element, one of the strings "true",
--                       "false", or "null", with no unnecessary quotes.
--      For some valid (and invalid) JSON examples, see demo\rosetta\JSON.exw.
--
--      Attempts to parse an invalid JSON string yield {JSON_INVALID} ({-4}).
--      Attempts to convert an invalid JSON object to a JSON string yield 0, 
--                          possibly with partial results in the output file.
--
-- Note this does not enforce json string contents:
--  \-prefixes are not enforced (esp /)
--  \uHHHH by pure happy coincidence matches Phix native format, no more.
--  Unicode as utf8 is considered sufficient; no utf-16 surrogate pair stuff.
--  Mind you, http://json.org is brutally simplistic, and so is this - but 
--  real-world use is dealing with poor or buggy implementations at the other
--  end, and right now any such testing is completely and utterly lacking...
--
-- Performance note/disclaimer
--  No particular effort has been made regarding performance.
--  I would strongly expect the actual conversion to be almost completely IO bound, 
--  however for very large json objects that require significant processing, there 
--  may be a case for indexing the internal form - but that should be reasonably
--  straightforward and is deemed outside the responsibility of these routines. 
--  It would also be fair to say that the trim_res argument is lazily implemented, 
--  and it might be better not to add any whitespace in the first place, instead
--  of trimming it away at the last moment.
--
-- Compatibility note
--  OpenEuphoria cannot distinguish between {} and "" and consequently may accept
--  some things (specifically the first of those) as valid that Phix does not. 
--  Phix will reject {12} as invalid, because it is not a string, whereas OE will 
--  treat it as "\x0C".
--  Otherwise this component should run on both, mainly thanks to the following:
--

--/* OpenEuphoria compatibility:
global type string(object s)
object si
    if not sequence(s) then return 0 end if
    for i=1 to length(s) do
        si = s[i]
        if not integer(si) then return 0 end if
        if si<0 or si>255 then return 0 end if
    end for
    return 1
end type
global constant true=(1=1), false=(1=0)
type bool(object b)
    return integer(b) and (b=true or b=false)
end type
--include builtins/ptrim.e
include std\types.e
include std\text.e
include std\console.e
include std\utils.e
include std\error.e
include std\io.e
include std\get.e
include std\memory.e
include std\dll.e
--*/

global constant JSON_OBJECT = -1,
                JSON_ARRAY = -2, 
                JSON_KEYWORD = -3,
                JSON_INVALID = -4

bool pj_trim = false

function pj_print(object fn, string fmt, sequence args={})
-- helper routine for pj_rec
    if integer(fn) then
        printf(fn,fmt,args)
    elsif pj_trim then
        fn &= trim(sprintf(fmt,args))
    else
        fn &= sprintf(fmt,args)
    end if
    return fn
end function

function not_valid()
    printf(1,"ERROR: not a valid JSON object\n",{})
    return 0
end function

function pj_rec(object fn, object o, integer indent=0, bool addcomma=false)
-- (internal) recursive inner for print_json
    if atom(o) then
        fn = pj_print(fn,"%.10g",{o})
    elsif string(o) then
        fn = pj_print(fn,`"%s"`,{o})
    else
        integer len = length(o)
        integer o1 = JSON_INVALID
        if len!=0 and integer(o[1]) then
            o1 = o[1]
        end if
        if o1=JSON_OBJECT then
            -- object
            fn = pj_print(fn,"{\n")
            for i=2 to len do
                object name = JSON_INVALID
                if sequence(o[i]) and length(o[i])=2 then
                    name = o[i][1]
                end if
                if not string(name) then return not_valid() end if
                fn = pj_print(fn,`%s"%s":`,{repeat(' ',indent+1),name})
                fn = pj_rec(fn,o[i][2],indent+length(name)+4,i<len) 
                if equal(fn,0) then return 0 end if
            end for
            fn = pj_print(fn,"%s}",{repeat(' ',indent)})
        elsif o1=JSON_ARRAY then
            -- array
            fn = pj_print(fn,"[\n")
            for i=2 to len do
                fn = pj_print(fn,repeat(' ',indent+1))
                fn = pj_rec(fn,o[i],indent+1,i<len)
                if equal(fn,0) then return 0 end if
            end for
            fn = pj_print(fn,"%s]",{repeat(' ',indent)})
        elsif o1=JSON_KEYWORD
          and len=2 
          and find(o[2],{"true","false","null"}) then
            -- keyword
            fn = pj_print(fn,o[2])
        else
            return not_valid()
        end if
    end if
    fn = pj_print(fn,iff(addcomma,",\n","\n"))
    return fn
end function

global function print_json(object fn, object o, bool trim_res=false)
--
-- fn: an open file handle (1 for the console), or "" to return a string
-- o: a JSON object to print
-- trim_res: ignored if fn is an integer, otherwise when true strips all 
--  whitespace and newlines from the returned result
-- Returns 0 if parameter o is not a valid JSON object, otherwise
-- if fn was originally "", returns a string, else fn unaltered.
-- Note it may get part way through before detecting an error.
--
    pj_trim = trim_res
    return pj_rec(fn,o)
end function

--
-- The parser
--  Based on the same technology used in the compiler, simplified as in plade/rein
--

constant SYMBOL  = 1,
         DQUOTE  = 2,
         DOT     = 3,
         DIGIT   = 4,
         FLOAT   = 5,
         LETTER  = 10,
         ILLEGAL = 99

function setCharClass()
sequence charClass = repeat(ILLEGAL,255)
    charClass['"'] = DQUOTE
    charClass[','] = SYMBOL
    charClass['-'] = SYMBOL
    charClass['.'] = DOT
    charClass['0'..'9'] = DIGIT
    charClass[':'] = SYMBOL
    charClass['A'..'Z'] = LETTER
    charClass['['] = SYMBOL
    charClass[']'] = SYMBOL
    charClass['a'..'z'] = LETTER
    charClass['{'] = SYMBOL
    charClass['}'] = SYMBOL
    return charClass
end function
sequence charClass = setCharClass()

string text, token
integer invalid -- 0 = no error,
                -- 2 = not yet reported
                -- 1 = reported
integer col, textlen, Ch, toktype
atom tokint -- (PL 7/1/18: int->atom; unix timestamps exceed #3FFFFFFF)
atom tokatm

procedure SkipSpaces()
    while Ch=' ' 
       or Ch='\t' 
       or Ch='\n'
       or Ch='\r' do
        col += 1
        if col>textlen then
            Ch = -1
            return
        end if
        Ch = text[col]
    end while
end procedure

procedure nextCh()
    if col<=textlen then
        Ch = text[col]
        SkipSpaces()
    else
        Ch = -1
    end if
end procedure

procedure getFloat()
-- finish off a float. The first few DIGITS (if any) have been processed;
-- continue from '.' or 'eE'
integer exponent, esign
atom dec, fraction
    tokatm = tokint
    if Ch='.' then
        toktype = FLOAT
        dec = 1
        fraction = 0
        while 1 do
            col += 1
            if col>textlen then exit end if
            Ch = text[col]
            if charClass[Ch]!=DIGIT then
                if Ch!='_' then exit end if
            else
                fraction = fraction*10+(Ch-'0')
                dec *= 10
            end if
        end while
        tokatm += fraction/dec
    end if
    if find(Ch,"eE")
    and col<textlen then
        toktype = FLOAT
        Ch = text[col+1]
--      esign = iff(Ch='-'?-1:+1)
        esign = iff(Ch='-',-1,+1)
        if Ch='-' or Ch='+' then
            col += 1
            Ch = text[col+1]
        end if
        exponent = 0
        while 1 do
            col += 1
            if col>textlen then exit end if
            Ch = text[col]
            if charClass[Ch]!=DIGIT then
                if Ch!='_' then exit end if
            else
                exponent = exponent*10+Ch-'0'
            end if
        end while
        tokatm = tokatm*power(10,exponent*esign)
    end if
    nextCh()
end procedure

procedure getToken()
integer nxtCh
integer tokstart
--  if Ch=-1 then
    if Ch<=0 then
        if invalid=0 then
            invalid = 2
        end if
        return
    end if
    tokstart = col
    token = ""
    toktype = charClass[Ch]
    if col<textlen then
        nxtCh = text[col+1]
    else
        nxtCh = -1
    end if
    if toktype=LETTER then
        while 1 do
            col += 1
            if col>textlen then exit end if
            nxtCh = charClass[text[col]]
            if nxtCh<DIGIT then exit end if
            if nxtCh>LETTER then exit end if
        end while
        token = text[tokstart..col-1]
        nextCh()
    elsif toktype=SYMBOL then
        col += 1
        token = text[tokstart..col-1]
        nextCh()
    elsif toktype=DQUOTE then
        while 1 do
            col += 1
            if col>textlen then
--              Abort(`missing "`)
                invalid = 2
                exit
            end if
            nxtCh = text[col]
            if nxtCh='\\' then
                col += 1
            elsif nxtCh='"' then
                col += 1
                exit
            end if
        end while
        token = text[tokstart+1..col-2]
        nextCh()
    elsif toktype=DOT then
        tokint = 0
        getFloat()
    elsif toktype=DIGIT then
        tokint = Ch-'0'
        while 1 do
            col += 1
            if col>textlen then exit end if
            Ch = text[col]
            if charClass[Ch]!=DIGIT then
                if Ch!='_' then exit end if
            else
                tokint = tokint*10+Ch-'0'
            end if
        end while
        if find(Ch,".eE") then
            getFloat()
        else
            nextCh()
        end if
    else
--      Abort("unrecognised")
        invalid = 2
    end if
end procedure

procedure Match(sequence text)
    if not equal(token,text) then
--      Abort(text&" expected")
        invalid = 2
    end if
    getToken()
end procedure

function parse_json_rec()
object res
    if toktype=LETTER
    and find(token,{"true","false","null"}) then
        res = {JSON_KEYWORD,token}
    elsif toktype=SYMBOL and equal(token,"{") then
        -- object
        res = {JSON_OBJECT}
        Match("{")
        if toktype!=SYMBOL or not equal(token,"}") then
            while 1 do
                if toktype!=DQUOTE then invalid = 2 exit end if
                string name = token
                getToken()
                Match(":")
                if invalid then exit end if
                res = append(res,{name,parse_json_rec()})
                if invalid then exit end if
                if toktype!=SYMBOL then invalid = 2 exit end if
                if equal(token,"}") then exit end if
                Match(",")
            end while
        end if
    elsif toktype=SYMBOL and equal(token,"[") then
        -- array
        res = {JSON_ARRAY}
        Match("[")
        if toktype!=SYMBOL or not equal(token,"]") then
            while 1 do
                res = append(res,parse_json_rec())
                if invalid then exit end if
                if toktype!=SYMBOL then invalid = 2 exit end if
                if equal(token,"]") then exit end if
                Match(",")
            end while
        end if
    elsif toktype=SYMBOL and equal(token,"-") then
        Match("-")
        if toktype=DIGIT then
            res = -tokint
        elsif toktype=FLOAT then
            res = -tokatm
        elsif invalid=0 then
            invalid = 2
        end if
    elsif toktype=DQUOTE then
        res = token
    elsif toktype=DIGIT then
        res = tokint
    elsif toktype=FLOAT then
        res = tokatm
    elsif invalid=0 then
        invalid = 2
    end if
    if invalid=2 then
        if not_valid() then end if
        invalid = 1
    end if
    if invalid then
        return {JSON_INVALID}
    end if
    getToken()
    return res
end function

global function parse_json(string s)
-- s should contain a valid string representation of a JSON object.
    invalid = 0
    text = s
    Ch = ' '
    col = 0
    textlen = length(text)
    SkipSpaces()
    getToken()
    return parse_json_rec()
end function

global function extract_json_field(sequence json_object, string name, object dflt="?9/0")
    if json_object[1]!=JSON_OBJECT then ?9/0 end if
    for i=2 to length(json_object) do
        if json_object[i][1]=name then
            return json_object[i][2]
        end if
    end for
    if dflt="?9/0" then ?9/0 end if
    return dflt
end function

