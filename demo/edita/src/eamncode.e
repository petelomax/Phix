--
-- eamncode.e
--
-- Macro encode and decode routines.
--
--  encode converts eg {{1, {49,0,0}}, {1, {55,0,0}}, {1, {51,0,0}},{2,{117,0,0}}}
--  ======          to "{{MR_CHAR,"173"},{MR_VIRT,{VK_F6,0,0}}}"
--
--  decode goes back from the slightly more human-readable form to the runnable form.
--  ======

--without warning

--include builtins\ppp.e

constant vkeys={
    VK_LBUTTON,"VK_LBUTTON",
    VK_RBUTTON,"VK_RBUTTON",
    VK_CANCEL,"VK_CANCEL",
    VK_MBUTTON,"VK_MBUTTON",
    VK_BACK,"VK_BACK",
    VK_TAB,"VK_TAB",
    VK_CLEAR,"VK_CLEAR",
    VK_RETURN,"VK_RETURN",
    VK_SHIFT,"VK_SHIFT",
    VK_CONTROL,"VK_CONTROL",
    VK_MENU,"VK_MENU",
    VK_PAUSE,"VK_PAUSE",
    VK_PRINT,"VK_PRINT",
    VK_CAPITAL,"VK_CAPITAL",
    VK_ESCAPE,"VK_ESCAPE",
    VK_SPACE,"VK_SPACE",
    VK_PRIOR,"VK_PRIOR",
    VK_NEXT,"VK_NEXT",
    VK_END,"VK_END",
    VK_HOME,"VK_HOME",
    VK_LEFT,"VK_LEFT",
    VK_UP,"VK_UP",
    VK_RIGHT,"VK_RIGHT",
    VK_DOWN,"VK_DOWN",
    VK_SELECT,"VK_SELECT",
    VK_EXECUTE,"VK_EXECUTE",
    VK_SNAPSHOT,"VK_SNAPSHOT",
    VK_INSERT,"VK_INSERT",
    VK_DELETE,"VK_DELETE",
    VK_HELP,"VK_HELP",
--  VK_0,"VK_0",
--  VK_1,"VK_1",
--  VK_2,"VK_2",
--  VK_3,"VK_3",
--  VK_4,"VK_4",
--  VK_5,"VK_5",
--  VK_6,"VK_6",
--  VK_7,"VK_7",
--  VK_8,"VK_8",
--  VK_9,"VK_9",
--  VK_A,"VK_A",
--  VK_B,"VK_B",
--  VK_C,"VK_C",
--  VK_D,"VK_D",
--  VK_E,"VK_E",
--  VK_F,"VK_F",
--  VK_G,"VK_G",
--  VK_H,"VK_H",
--  VK_I,"VK_I",
--  VK_J,"VK_J",
--  VK_K,"VK_K",
--  VK_L,"VK_L",
--  VK_M,"VK_M",
--  VK_N,"VK_N",
--  VK_O,"VK_O",
--  VK_P,"VK_P",
--  VK_Q,"VK_Q",
--  VK_R,"VK_R",
--  VK_S,"VK_S",
--  VK_T,"VK_T",
--  VK_U,"VK_U",
--  VK_V,"VK_V",
--  VK_W,"VK_W",
--  VK_X,"VK_X",
--  VK_Y,"VK_Y",
--  VK_Z,"VK_Z",
    VK_APPS,"VK_APPS",
    VK_CSM,"VK_CSM",
    VK_NUMPAD0,"VK_NUMPAD0",
    VK_NUMPAD1,"VK_NUMPAD1",
    VK_NUMPAD2,"VK_NUMPAD2",
    VK_NUMPAD3,"VK_NUMPAD3",
    VK_NUMPAD4,"VK_NUMPAD4",
    VK_NUMPAD5,"VK_NUMPAD5",
    VK_NUMPAD6,"VK_NUMPAD6",
    VK_NUMPAD7,"VK_NUMPAD7",
    VK_NUMPAD8,"VK_NUMPAD8",
    VK_NUMPAD9,"VK_NUMPAD9",
    VK_MULTIPLY,"VK_MULTIPLY",
    VK_ADD,"VK_ADD",
    VK_SEPARATOR,"VK_SEPARATOR",
    VK_SUBTRACT,"VK_SUBTRACT",
    VK_DECIMAL,"VK_DECIMAL",
    VK_DIVIDE,"VK_DIVIDE",
    VK_F1,"VK_F1",
    VK_F2,"VK_F2",
    VK_F3,"VK_F3",
    VK_F4,"VK_F4",
    VK_F5,"VK_F5",
    VK_F6,"VK_F6",
    VK_F7,"VK_F7",
    VK_F8,"VK_F8",
    VK_F9,"VK_F9",
    VK_F10,"VK_F10",
    VK_F11,"VK_F11",
    VK_F12,"VK_F12",
    VK_F13,"VK_F13",
    VK_F14,"VK_F14",
    VK_F15,"VK_F15",
    VK_F16,"VK_F16",
    VK_F17,"VK_F17",
    VK_F18,"VK_F18",
    VK_F19,"VK_F19",
    VK_F20,"VK_F20",
    VK_F21,"VK_F21",
    VK_F22,"VK_F22",
    VK_F23,"VK_F23",
    VK_F24,"VK_F24",
    VK_NUMLOCK,"VK_NUMLOCK",
    VK_SCROLL,"VK_SCROLL",
    VK_LSHIFT,"VK_LSHIFT",
    VK_LCONTROL,"VK_LCONTROL",
    VK_LMENU,"VK_LMENU",
    VK_RSHIFT,"VK_RSHIFT",
    VK_RCONTROL,"VK_RCONTROL",
    VK_RMENU,"VK_RMENU",
    VK_PROCESSKEY,"VK_PROCESSKEY"}

integer didx
global integer valid, retryX
sequence s
integer ch -- the current character

constant EOF = -1

function dfail(sequence msg)
    valid = False
    if ch!=EOF then
        didx -= 1
    end if
    retryX = (proemh("Error",{s,didx,msg},MB_RETRYCANCEL) = IDRETRY)
    return {}
end function

---- modified routines from get.e (to invoke dfail):

constant DIGITS = "0123456789",
    HEX_DIGITS = DIGITS & "ABCDEF",
    START_NUMERIC = DIGITS & "-+.#"

procedure get_ch()
-- set ch to the next character in the input stream (either string or file)

    if didx<=length(s) then
        ch = s[didx]
        didx += 1
    else
        ch = EOF
    end if
end procedure

procedure skip_blanks()
--
-- skip white space
-- ch is "live" at entry and exit
--
    while find(ch," \t\n\r") do
        get_ch()
    end while
end procedure


constant ESCAPE_CHARS = "nt'\"\\r",
         ESCAPED_CHARS = "\n\t'\"\\\r"

function escape_char(integer c)
-- return escape character
integer i

    i = find(c, ESCAPE_CHARS)
    if i=0 then
        return dfail("unrecognised escape")
    end if
    return ESCAPED_CHARS[i]
end function

function get_qchar()
-- get a single-quoted character
-- ch is "live" at exit
integer c

    get_ch()
    c = ch
    if ch='\\' then
        get_ch()
        return escape_char(ch)
    elsif ch='\'' then
        return dfail("'' invalid")
    end if
    get_ch()
    if ch!='\'' then
        return dfail("missing '")
    end if
    get_ch()
    return c
end function

function get_string()
-- get a double-quoted character string
-- ch is "live" at exit
sequence text

    text = ""
    while valid do
        get_ch()
        if ch=EOF or ch='\n' then
            return dfail("missing \"")
        elsif ch='"' then
            get_ch()
            return text
        elsif ch='\\' then
            get_ch()
            return escape_char(ch)
        end if
        text = text & ch
    end while
end function

function get_number()
-- read a number
-- ch is "live" at entry and exit
integer sign, e_sign
integer ndigits
integer hex_digit
atom mantissa, fraction, dec, e_mag

    sign = +1
    mantissa = 0
    ndigits = 0

    -- process sign
    if ch='-' then
        sign = -1
        get_ch()
    elsif ch='+' then
        get_ch()
    end if

    -- get mantissa
    if ch='#' then
        -- process hex integer and return
        get_ch()
        while valid do
            hex_digit = find(ch, HEX_DIGITS)-1
            if hex_digit>=0 then
                ndigits += 1
                mantissa = mantissa * 16 + hex_digit
                get_ch()
            else
                if ndigits>0 then
                    return sign * mantissa
                else
                    return dfail("# missing digits")
                end if
            end if
        end while 
    end if

    -- decimal integer or floating point
    while ch>='0' and ch<='9' do
        ndigits += 1
        mantissa = mantissa*10 + (ch-'0')
        get_ch()
    end while

    if ch='.' then
        -- get fraction
        get_ch()
        dec = 1
        fraction = 0
        while ch>='0' and ch<='9' do
            ndigits += 1
            fraction = fraction*10 + ch-'0'
            dec *= 10
            get_ch()
        end while
        mantissa += fraction/dec
    end if

    if ndigits=0 then
        return dfail("missing digits")
    end if

    mantissa = sign*mantissa

    if ch='e' or ch='E' then
        -- get exponent sign
        e_sign = +1
        e_mag = 0
        get_ch()
        if ch='-' then
            e_sign = -1
            get_ch()
        elsif ch='+' then
            get_ch()
        end if
        -- get exponent magnitude 
        if ch>='0' and ch<='9' then
            e_mag = ch-'0'
            get_ch()
            while ch>='0' and ch<='9' do
                e_mag = e_mag*10 + ch-'0'
                get_ch() 
            end while
        else
            return dfail("no exponent")
        end if
        e_mag *= e_sign 
        if e_mag>308 then
            -- rare case: avoid power() overflow
            mantissa *= power(10,308)
            if e_mag>1000 then
                e_mag = 1000 
            end if
            for i=1 to e_mag-308 do
                mantissa *= 10
            end for
        else
            mantissa *= power(10,e_mag)
        end if
    end if

    return mantissa
end function

function convert()
-- read a Euphoria/Phix data object as a string of characters
-- and return {error_flag, value}
-- Note: ch is "live" at entry and exit of this routine
sequence s
object e

    skip_blanks()

    if find(ch, START_NUMERIC) then
        return get_number()

    elsif ch='{' then
        -- process a sequence
        s = {}
        get_ch()
        skip_blanks()
        if ch='}' then
            get_ch()
            return {} -- empty sequence
        end if

        while valid do
            e = convert() -- read next element
            if not valid then exit end if
            s = append(s, e)
            skip_blanks()
            if ch='}' then
                get_ch()
                exit
            elsif ch=EOF then
                return dfail("'}' expected")
            elsif ch!=',' then
                return dfail("',' expected")
            end if
            get_ch() -- skip comma
        end while
        return s

    elsif ch='\"' then
        return get_string()

    elsif ch='\'' then
        return get_qchar()

    elsif ch=EOF then
        return dfail("unexpected end?")

    else
        return dfail("number, {, \", or ' expected")

    end if
end function
-------------------------------------------------------

global function decode(sequence ds)
-- convert text to Eu sequence
integer ls, tokstart, tokend, c, k
object res
sequence token, exp

    s = ds
    valid = True
    ch = EOF

    ls = length(s)
    tokstart = 1
    while tokstart<=ls do
        c = s[tokstart]
        if find(c," \t\r\n") then
            s[tokstart] = ' '
        elsif c='\"' then
            while 1 do
                tokstart += 1
                if tokstart>ls then exit end if
                c = s[tokstart]
                if c='\\' then
                    tokstart += 1
                elsif c='\"' then
                    tokstart += 1
                    exit
                end if
            end while
--      elsif charMap[c+1]=TokenStart then
        elsif c<128
          and charMap[c+1]=TokenStart 
          and (c<'0' or c>'9') then
--      elsif c>='A' and c<='z' then
            tokend = tokstart+1
            while tokend<=ls do
                c = s[tokend]
                if c>128 or charMap[c+1]>TokenChar then exit end if
--              if c<'0' then exit end if
----                if c>'9' and c<'A' then exit end if
--              if c>'z' then exit end if
--              if find(c,":;<=>?@[\\]^`") then exit end if
                tokend += 1
            end while
            token = s[tokstart..tokend-1]
            k = find(token,{"MR_CHAR","MR_VIRT","MR_FIND"})
            if k then
                s = s[1..tokstart-1] & k+'0' & s[tokend..ls]
                ls = length(s)
                if k=MR_CHAR
                and tokstart+2<=ls 
                and s[tokstart+2]='\"' then
                    exp = sprintf("{%d,0,0}",s[tokstart+3])
                    for j=tokstart+4 to ls do
                        if s[j]='\"' then
                            s = s[1..tokstart+1] & exp & s[j+1..ls]
                            ls = length(s)
                            tokstart += length(exp) --??
                            exit
                        end if
                        exp &= sprintf("},{1,{%d,0,0}",s[j])
                    end for
                end if
            else
                k = find(token,vkeys)
                if k then
                    exp = sprintf("%d",vkeys[k-1])
                    s = s[1..tokstart-1] & exp & s[tokend..ls]
                    ls = length(s)
                    tokstart += length(exp) --??
                else
                    didx = tokstart+1
                    return dfail("invalid name ["&token&"]")
                end if
            end if
        end if
        tokstart += 1
    end while
    while 1 do
        ls = length(s)
        if ls=0 then return "" end if
        if s[ls]!=' ' then exit end if
        s = s[1..ls-1]
    end while

    didx = 1
    get_ch()
    res = convert() -- derivative of value()
    if valid then   -- message not already given
        if ch!=EOF then
            return dfail("extra characters detected")
        elsif not sequence(res) then
            return dfail("sequence expected")
        end if
    end if
    return res
end function

integer otype
function mergable_char(object o)
-- returns 1 if o is graphic char, not shift or control.
--  also sets otype for all valid keys.
    otype = 0
    if sequence(o)
    and length(o)=2
    and integer(o[1]) then
        otype = o[1]
        o = o[2]
        if sequence(o)
        and length(o)=3
        and integer(o[1]) and integer(o[2]) and integer(o[3]) then
            if otype=MR_CHAR
            and o[1]>=32 and o[1]<=126
            and o[2]=0 and o[3]=0 then
                return 1
            end if
        elsif otype=MR_VIRT then
            otype = 0
        elsif otype=MR_FIND then
            if not sequence(o)
            or length(o)!=1 then
                otype = 0
            else
                o = o[1]
                if not sequence(o)
                or length(o)!=5
                or not sequence(o[1])
                or not integer(o[2])
                or not integer(o[3])
                or not integer(o[4])
                or not integer(o[5]) then
                    otype = 0
                end if
            end if
        end if      
    end if
    return 0
end function

global function encode(sequence es, integer withreturns)
-- convert Eu sequence to text
sequence res
object o
sequence str
integer vk
    s = es
    if length(s)=0 then return "{}" end if
    res = "{"
    didx = 1
    while didx<=length(s) do
        o = s[didx]
        if mergable_char(o) then
            str = "{MR_CHAR,\""&o[2][1]
            while didx<length(s) do
                o = s[didx+1]
                if not mergable_char(o) then exit end if
                str &= o[2][1]
                didx += 1
            end while       
            res &= str&"\"}"
        elsif otype=MR_CHAR then    -- either ctrl or shift pressed...
            res &= sprintf("{MR_CHAR,{%d,%d,%d}}",o[2])
        elsif otype=MR_VIRT then
            o = o[2]
            vk = o[1]
            o[1] = sprintf("%d",vk)
            for i=1 to length(vkeys) by 2 do
                if vk=vkeys[i] then
                    o[1] = vkeys[i+1]
                    exit
                end if
            end for
            res &= sprintf("{MR_VIRT,{%s,%d,%d}}",o)
        elsif otype=MR_FIND then
            res &= sprintf("{MR_FIND,{{\"%s\",%d,%d,%d,%d}}}",o[2][1])
        else    -- generic catch-all
            res &= ppExf(o,{pp_Maxlen,999999999})
        end if
        if didx<length(s) then
            if withreturns then
                res &= ",\r\n"
            else
                res &= ","
            end if
        else
            res &= "}"
            exit
        end if
        didx += 1
    end while
    return res
end function

