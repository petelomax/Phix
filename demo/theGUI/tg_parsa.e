--
-- tg_parsa.e (part of theGUI.e)
--

-- (this might one day become a builtin??)
local procedure crash_hat(string msg, txt, integer carat, nFrames=1)
    -- neither txt nor "^ msg" should fall off rhs (79 chars)
    integer lm = carat+1+length(msg)
    if lm>79 then
        txt = `...`&txt[lm-75..$]
        carat -= lm-79
    end if
    if length(txt)>79 then
        txt = txt[1..76]&`...`
    end if
    msg = sprintf("%s\n%s^ %s\n",{txt,repeat(' ',carat-1),msg})
    crash(msg,nFrames:=nFrames)
end procedure

local constant XPGLEX_TK_END    = 0,
               XPGLEX_TK_SET    = 1,
               XPGLEX_TK_COMMA  = 2,
               XPGLEX_TK_NAME   = 3,
               XPGLEX_TK_HEX    = 4;

local function tg_parse_attributes(string attributes, sequence args, integer nFrames=1)
    // (manually translated from iup_attrib.c)
    if length(args) then attributes = sprintf(attributes,args) end if
    args = {}
    integer i = 1,
            l = length(attributes)
    string token = ``
    object name = null,
            val = null
    bool get_name = true // (else get_value)
    while true do
        integer toktype = XPGLEX_TK_END
        while i<=l do
            integer ch = attributes[i]
            i += 1
            string delims = ``
            switch ch do
                case '%':   // Skip comment
                    while i<=l do
                        ch = attributes[i]
                        i += 1
                        if ch=='\n' then exit end if
                    end while
                case '#':   // Hex constant
                    toktype = XPGLEX_TK_HEX
                    i -= 1;                     // unget first character
                    delims = ", \n\r\t"         // get until delimiter
                case ' ', '\n', '\r':
                    break
                case '=':
                    toktype = XPGLEX_TK_SET
                case ',':
                    toktype = XPGLEX_TK_COMMA
                case '"':                       // string
                    delims = `"`
                case '{':
                    delims = `}`
                default:
                    if ch>' ' then              // identifier
                        i -= 1;                 // unget first character
                        delims = "=, \n\r\t"    // get until delimiter
                    end if
            end switch
            if length(delims) then
                token = ``
                while i<=l do
                    ch = attributes[i]
                    i += 1
                    if find(ch,delims) then exit end if
                    if toktype=XPGLEX_TK_HEX then
                        --DEV/SUG "source line with ^"-style error, if we can:
                        -- maybe crash_hat(`bad hex char`,attributes,i,1,nFrames)
                        --   ==>    gSetAttributes(dlg,`BGCLR=#1234J678`)
                        --                                         ^ bad hex char
                        -- or       bad hex char (`...234(>>J<<)678...`) if no match
                        -- better:      `...234J678...`
                        --                     ^ bad hex char
                        -- crash_hat() attempts to deliver a source-level error with
                        -- a caret indicating the exact point of failure, however and
                        -- of course a compiled program has no access to source code 
                        -- to point at, so that will be more like a normal crash().
                        -- You provide a code fragment, offset and length, which it
                        -- tries to match against the source code line if it can.
                        -- To keep things simple, there is no auto-sprintf(msg,args)
                        -- option in crash_hat(). For an example see theGUI.e
--                      assert(find(ch,`#01234567890ABCDEFabcdef`)!=0,`bad hex char`)
--DEV untested:
                        if find(ch,`#01234567890ABCDEFabcdef`)=0
                        or (ch='#' and token!=``) then
                            crash_hat(`bad hex char`,attributes,i-1,2)
                        end if
--                      assert(ch!='#' or token=``,`multiple # chars?`)
--                      if ch='#' and token!=`` then
--                          crash_hat(`multiple # chars?`,attributes,i-1,2)
--                      end if
                    end if
                    token &= ch
                end while
                if length(delims)>1 and find(ch,delims) then
                    i -= 1  // unget delimiter
                end if
                if toktype!=XPGLEX_TK_HEX then
                    toktype = XPGLEX_TK_NAME
                end if
            end if
            if toktype!=XPGLEX_TK_END then exit end if
        end while
        switch toktype do
--DEV compiler error... (with nFrames:=nFrames+1)
--          case XPGLEX_TK_END:
--              bEnd = true
--              fallthrough
--          case XPGLEX_TK_COMMA:
            case XPGLEX_TK_END,
                 XPGLEX_TK_COMMA:
                if name!=NULL then
--                  gSetAttribute(id,name,val,nFrames:=nFrames+1)
                    args = append(args,{name,val})
--                  gSetAttribute(id,name,val,nFrames:=nFrames)
--                  names = append(names,name)
--                  vals = append(vals,name)
                end if
--              if bEnd then return end if
                if toktype=XPGLEX_TK_END then return args end if
                name = null
                val = null
                get_name = true
            case XPGLEX_TK_SET:
                get_name = false
            case XPGLEX_TK_NAME:
                if get_name then
                    name = token
                else
                    val = token
                end if
            case XPGLEX_TK_HEX:
                assert(not get_name)
                val = token
        end switch
    end while
    return args
end function
gSetGlobal(`tg_parse_attr`,tg_parse_attributes)
--</include>

