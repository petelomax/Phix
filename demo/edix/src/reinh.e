--
-- edix\src\reinh.e
--
-- Note: this only works on Phix; should you be running Edita on RDS/OpenEu,
--                           this file should not be included (see earein.ew,
--                           which does the standard "--/**/include" thing)
--       reasons why include: string, variable length slice assignment (lots), 
--                            direct comparison (eg if tagname="br"), negative
--                            slices (eg tagstack=tagstack[1..-2]), and lastly
--                            multiple assignment (eg {...} = tags[i]). Feel
--                            free to waste your time trying to port this!
--                            (obviously, not likely to be very difficult.)
--

constant OPENING      = #00,    -- '<' with none of the following detected
         CLOSING      = #01,    -- "</" (start of closing tag)
         BANG         = #02,    -- "<!" (comment, CDATA, DOCTYPE etc)
--       CLOSEBANG    = #03,    -- "</... -->"
         SELF_CLOSING = #04,    -- "/>" (self-closing tag)
$

constant TAG_NAME   = 1,        -- doh: eg div, span, a, etc
         TAG_START  = 2,        -- "flat" idx of opening '<'
--       TAG_FLAGS  = 3,        -- as above (OPENING..SELF_CLOSING)
         TAG_INDENT = 4,        -- self explanatory
         TAG_LINK   = 5,        -- eg <div> points to </div> entry
--       TAG_CLOSE  = 6,        -- "flat" idx of closing '>'
         TAG_ULNK   = 7,        -- unclosed chain
         UCTIDX     = 1,        -- tags idx of unclosed entry
         UCNXT      = 2,        -- next in unclosed chain, or 0
$

function flattenX(sequence ftxt)
-- convert {"this","that","them"} to "this\nthat\nthem\n"
integer len = 0
integer otw = -1
integer start, k, l
sequence fi
string res
integer ch
    if isReplaceTabs then
        otw = 0
--  elsif isTabWidth!=8 or (isEu and Xtrans) then
--  elsif isTabWidth!=8 or isEu then
    elsif isTabWidth!=8 then
        otw = 8
    end if
    for i=1 to length(ftxt) do
        fi = ftxt[i]
        if otw!=-1 then
            fi = ConvertTabs(fi,isTabWidth,otw)
        end if
        -- treat/replace (eg) "<em><</em>" as/with "<em>&lt;</em>"
        while 1 do
            k = match("<<",fi)
            if k=0 then exit end if
            fi[k..k+1] = "&lt;<"
        end while
        ftxt[i] = fi
        len += length(fi)+1
    end for
    res = repeat(' ',len)
    len = 1
    for i=1 to length(ftxt) do
        fi = ftxt[i]
        start = len
        l = length(fi)
        if l then
            len += l
--DEV (quick fix for type check error on res) 12/9/14
if string(fi) then
            res[start..len-1] = fi
else
            for j=1 to length(fi) do
                ch = and_bits(fi[j],#FF)
                res[start] = ch
                start += 1
            end for
end if
        end if
        res[len] = '\n'
        len += 1
    end for
    return res
end function

function tolines(string s)
-- convert "this\nthat\nthem\n" to {"this","that","them"} 
sequence res = {}
integer k
    while 1 do
        k = find('\n',s)
        if k=0 then exit end if
        res = append(res,s[1..k-1])
        s = s[k+1..$]
    end while
--DEV this might want an "if length(s)" on it...
    if length(s) then
        res = append(res,s)
    end if
    return res
end function

global procedure htmlreindent()
string text
sequence res
sequence tags = {}  -- {tagname,startidx,flags,indent,link,closeidx,ulnk} as per TAG_XXX
sequence tagstack = {}  -- still_open idx to above (ie in progress/expected)
sequence unclosed = {}  -- {tagidx,next} pairs
integer autoclosed = 0
string tagclose, tagnotes
sequence chunks
integer endtxt
string tagname,
       expected
integer idx = 1,
        ch,
        startidx,   -- save of idx when '<' found
        flags,      -- as per TAG_XXX above
        indent = 0, -- (==length(tagstack) at open)
        link,       -- (closing tags point to the opening tag)
        closeidx,   -- save of idx when '>' found
        ulnk        -- tags->unclosed, if any
integer landing
string errormsg = "", errortitle = "error"

    text = flattenX(filetext[currfile])
    while idx<=length(text) do
        ch = text[idx]
        if ch='<' then
            startidx = idx
            idx += 1
            tagname = ""
            ch = text[idx]
            flags = OPENING
            if ch='/' then
                flags = CLOSING
                idx += 1
            elsif ch='!' then
                flags = BANG
                idx += 1
-- 2/10/14:
if text[idx]='-' 
and text[idx+1]='-' then
    idx += 2
end if
            end if
            while 1 do
                ch = text[idx]
                if ch<'0' then exit end if
                if ch>'9' and ch<'A' then exit end if
                if ch>'Z' and ch<'a' then exit end if
                if ch>'z' then exit end if
                tagname &= ch
                idx += 1
            end while
-- 2/10/14:
if flags=BANG and tagname="eucode" then
    idx = match("</eucode-->",text,idx)
    if idx=0 then
        errormsg = "</eucode--> missing"
        tagstack = {}
        exit
    end if
    idx += length("</eucode-->")
elsif tagname="pre" then
    idx = match("</pre>",text,idx)
    if idx=0 then
        errormsg = "</pre> missing"
        tagstack = {}
        exit
    end if
    idx += length("</pre>")
else
            tagname = lower(tagname)
--          if idx+2<=length(text)
--          and text[idx]='-'
--          and text[idx+1]='-'
--          and text[idx+2]='>' then
--              flags = CLOSEBANG
--          end if
            while idx<length(text) do
                ch = text[idx]
                if ch='>' then exit end if
                if ch='"' then
                    idx = find('"',text,idx+1)
                    if idx=0 then exit end if
                end if
                idx += 1
            end while
            if idx=0 or idx>length(text) then
                errormsg = "missing '>'"
                tagstack = {}
                exit
            end if
            if text[idx-1]='/'
            or tagname="br"
            or tagname="wbr"
            or tagname="hr" then
                if flags=CLOSING then
                    errormsg = "self-closing closing tag found!? (eg </p/>)"
                    tagstack = {}
                    exit
                end if
                flags = SELF_CLOSING
            end if
--          if not find(tagname,{"strong","em"}) then   -- (prolly good idea for i,b,etc. too)
            if not find(tagname,{"strong","em","b","i","li","a"}) then  -- (prolly good idea for i,b,etc. too)
                link = length(tagstack)                 -- (just for the following "pre" test)
                if tagname!="span"                      -- (do not indent <span> within <pre>)
--              if find(tagname,{"font","span"})=0      -- (do not indent <font/span> within <pre>)
                or link=0
                or tags[tagstack[link]][TAG_NAME]!="pre" then
                    indent = length(tagstack)
                    link = 0
                    tags = append(tags,{tagname,startidx,flags,indent,link,idx,0})
                    if flags!=BANG then
--                  and flags!=CLOSEBANG then
                        if flags=0 then
                            tagstack = append(tagstack,length(tags))
                            if tagname="script" then
                                -- must avoid treating eg "if (a<b) {" as a tag start 
                                idx = match("</script>",text,idx)
                                if idx=0 then exit end if
                                idx -= 1
                            end if
                        elsif flags=CLOSING then
                            while length(tagstack) do
                                link = tagstack[$]
                                expected = tags[link][TAG_NAME]
                                if tagname=expected then exit end if
                                unclosed = append(unclosed,{link,tags[$][TAG_ULNK]})
                                tags[$][TAG_ULNK] = length(unclosed)
                                tagstack = tagstack[1..-2]
                            end while
                            if length(tagstack)=0 then
                                tags[$][TAG_ULNK] = 0
                                errormsg = "no opening "&tagname
                                exit
                            end if
                            tags[$][TAG_LINK] = link
                            tagstack = tagstack[1..-2]
                            tags[$][TAG_INDENT] = length(tagstack)
                            if tagname="OBJECT" 
                            and length(tagstack) 
                            and tags[tagstack[$]][TAG_NAME]="LI" then
                                -- in hhk and hhc, </OBJECT> also closes the <LI>
                                tagstack = tagstack[1..-2]
                            end if
                        end if
                    end if
                end if
            end if
            idx += 1
end if
        elsif ch='"' then
            idx = find('"',text,idx+1)+1
        else
            idx = find('<',text,idx)
            if idx=0 then exit end if
            if text[idx]!='<' then ?9/0 end if
        end if
    end while
    if length(tagstack) then
        tagname = tags[tagstack[1]][TAG_NAME]
        for i=2 to length(tagstack) do
            tagname &= ","&tags[tagstack[i]][TAG_NAME]
        end for
        errormsg = tagname
        errortitle = "missing closing tags"
    end if

    chunks = {}
    endtxt = length(text)
    for i=length(tags) to 1 by -1 do
        {tagname,startidx,flags,indent,link,closeidx,ulnk} = tags[i]
        ch = text[startidx]
        if ch!='<' then ?9/0 end if
        if flags!=CLOSING
--      if (flags!=CLOSING and flags!=CLOSEBANG)
        or link!=i-1
        or find('\n',text[tags[i-1][TAG_START]..startidx]) then
            if tagname!="OBJECT" or flags=CLOSING then
                idx = startidx-1
                while idx>0 and find(text[idx]," \t\r\n") do
                    idx -= 1
                end while
                if indent or i>1 then
                    chunks = append(chunks,repeat(' ',indent)&text[startidx..endtxt])
                    endtxt = idx
                    if ulnk then
                        tagclose = ""
                        tagnotes = "<!-- autoclosed:"
                        while ulnk do
                            link = unclosed[ulnk][UCTIDX]
                            tagname = tags[link][TAG_NAME]
                            tagnotes &= tagname&","
--                          tagclose = "</"&tagdesc&">" & tagclose
                            -- or maybe:
                            tagclose = repeat(' ',tags[link][TAG_INDENT])&"</"&tagname&">\n" & tagclose
                            autoclosed += 1
                            ulnk = unclosed[ulnk][UCNXT]
                        end while
                        tagnotes[$..$] = "-->\n"
                        tagclose = tagclose[1..-2]
                        chunks = append(chunks,tagnotes&tagclose)
                    end if
                end if
            end if
        end if
    end for
    res = tolines(text[1..endtxt])
    for i=length(chunks) to 1 by -1 do
        landing = length(res) -- (in case of error)
        res &= tolines(chunks[i])
    end for
    selectAll()
    if deleteSelection() then end if
    addAction(INSERTBLOCK,res)
    InsertBlock(res)
    if length(chunks) then
        CursorY = landing-1
    end if
    forceCursorOnscreen()
    if length(errormsg) then
--      void = messageBox(errortitle,errormsg,MB_OK)
        IupMessage(errortitle,errormsg)
    end if
    if autoclosed!=0 then
--      void = messageBox("warning",sprintf("%d tags auto-closed (use [Shift] F4 to find them)",autoclosed),MB_OK)
        IupMessage("warning",sprintf("%d tags auto-closed (use [Shift] F4 to find them)",autoclosed))
    end if
end procedure



