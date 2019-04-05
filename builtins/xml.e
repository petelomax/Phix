--
-- builtins/xml.e
-- ==============
--
-- Converts xml (text) <--> DOM (nested structure)
--
-- Deliberately kept as simple as possible, to simplify modification.
--  (I fully expect problems the first time this is used in anger!)
--
-- Does not use/validate against DTDs (Document Type Definitions) or 
--  XSLs (eXtensible Stylesheet Language).
--
-- Comments:
--  Comments are only supported after the XMLdeclaration (if present)
--  and either before or after the top-level element, not within it.
--
-- Unicode handling:
--  Via utf-8. I have tested this on some fairly outlandish-looking
--  samples without any problems. However it contains very little 
--  code to actually deal with unicode, instead relying on utf8 to
--  not embed any critical control characters (such as '<') within 
--  any multibyte encodings, and even wrote a quick test program.
--  Should you need to process utf-16 (or utf-32) then it must be
--  converted to utf-8 beforehand, and possibly the output back.
--  One thing it does actually do is skip a utf-8 BOM at the start
--  of the xml input (string/file), however there is nothing in
--  here to help with writing one back (not that prefixing one on
--  the output [externally] should in any way prove difficult).
--
-- Personal note: json is widely considered a better choice for data transfer.
--  It is of course more efficient, but also less descriptive and does not
--  support comments or any form of self-validation, and may prove more
--  brittle, unless the provider has the common sense to include a field
--  that adequately specifies the precise version/format being sent (but
--  in my experience they rarely do). The bottom line is you should use xml 
--  in cases where you really benefit from it, which is /not/ everywhere, 
--  eg/ie: use xml for config-type-data, but json for bulk data.
--

--ERM:...
--  It is important to note that builtins/xml.e currently only supports XPath minimally.
--  (currently a very early work in progress/draft library; design comments welcome)



--DOC:
--/*
constant eg1 = """
<?xml version="1.0" ?>
<root>
  <element>Some text here</element>
</root>
"""
ppOpt({pp_Nest,5,pp_Pause,0})
pp(xml_parse(eg1))
--/*
{"document",                    -- XML_DOCUMENT
 {"<?xml version=\"1.0\" ?>"},  -- XML_PROLOGUE
 {"root",                       -- XML_CONTENTS[XML_TAGNAME]
  {},                           --  XML_ATTRIBUTES
  {{"element",                  --  XML_CONTENTS[XML_TAGNAME]
    {},                         --   XML_ATTRIBUTES
    "Some text here"}}},        --   XML_CONTENTS
 {}}                            -- XML_EPILOGUE
--Note the three uses of XML_CONTENTS. The first is the one and only top-level element,
--the second is a sequence of elements, which happens to be one long, and the third is
--a string of the "string, or sequence of nested tags" fame. The difference between the
--first two of those cannot be stressed enough: top-level has precisely one '{' before 
--it, whereas any and all more deeply nested elements always have two, ie "{{".
--Obviously XML_CONTENTS[XML_TAGNAME] means that XML_CONTENTS is a sequence of length
--3 starting at that point, and XML_TAGNAME is the first element of that.
--*/
constant eg2 = """
<Address>
  <Number Flat="b">2</Number>
  <Street>Erdzinderand Beat</Street>
  <District>Stooingder</District>
  <City>Bush</City>
</Address>
"""
pp(xml_parse(eg2))
--/*
{"document",                    -- XML_DOCUMENT
 {},                            -- XML_PROLOGUE
 {"Address",                    -- XML_CONTENTS[XML_TAGNAME]
  {},                           --  XML_ATTRIBUTES
  {{"Number",                   --  XML_CONTENTS[XML_TAGNAME]
    {{"Flat"},                  --   XML_ATTRIBUTES[XML_ATTRNAMES]
     {"b"}},                    --    XML_ATTRVALUES
    "2"},                       --   XML_CONTENTS
   {"Street",                   --  XML_CONTENTS[XML_TAGNAME]
    {},                         --   XML_ATTRIBUTES
    "Erdzinderand Beat"},       --   XML_CONTENTS
   {"District",                 --  XML_CONTENTS[XML_TAGNAME]
    {},                         --   XML_ATTRIBUTES
    "Stooingder"},              --   XML_CONTENTS
   {"City",                     --  XML_CONTENTS[XML_TAGNAME]
    {},                         --   XML_ATTRIBUTES
    "Bush"}}},                  --   XML_CONTENTS
 {}}                            -- XML_EPILOGUE
--*/
--*/

--XML_PROLOGUE aka "XML declaration"

global enum XML_DOCUMENT,   -- must be "document"
            XML_PROLOGUE,   -- {} or eg {doctype,comments}
            XML_CONTENTS,   -- (must be a single element)
            XML_EPILOGUE,   -- {} or {final_comments}
            XML_DOCLEN = $  -- 4

--DEV (no longer used)
--global enum XML_COMMENTS = 2 -- (within [XML_PROLOGUE])

global enum XML_TAGNAME,    -- eg "Students"
            XML_ATTRIBUTES, -- as below, or {}
--          XML_CONTENTS    -- (string, or sequence of nested tags)
            XML_ELEMLEN     -- 3

global enum XML_ATTRNAMES,  -- eg {"Name","Gender",...}
            XML_ATTRVALUES  -- eg {"Alan","M",...}

global constant XML_DECODE = #0001, -- convert eg &gt; to '>'
                XML_ENCODE = #0002  -- reverse ""

string text
integer ch, col, textlen

procedure xml_skip_spaces()
    while ch=' ' 
       or ch='\t' 
       or ch='\n'
       or ch='\r'
       or ch=#A0 do     -- non-breaking space
        col += 1
        if col>textlen then
            ch = -1
            return
        end if
        ch = text[col]
    end while
end procedure

procedure xml_next_ch()
    if col<=textlen then
        ch = text[col]
        xml_skip_spaces()
    else
        ch = -1
    end if
end procedure

function xml_fatal(string msg)
--todo: add line number, etc. [DONE, untested]
    integer line = 1, linestart = 1, column
    for i=1 to col do
        if text[i]='\n' then
            line += 1
            linestart = i+1
        end if
    end for
--  text = text[col..$]
    text = text[linestart..find('\n',text,col)-1]
    if length(text)>80 then text = text[1..80] end if
--?9/0
    column = col-linestart
    return {-1, msg, line, column, text, col}
end function

global function is_xml_fatal(sequence x)
    return x[1]=-1
end function

global function xml_error(sequence x)
    if not is_xml_fatal(x) then ?9/0 end if
    return sprintf("Error :%s (line %d column %d)\n%s\n",x[2..-2])
end function

function xml_match(string x)
    for i=1 to length(x) do
        if col>textlen or text[col]!=x[i] then return false end if
        col += 1
    end for
    xml_next_ch()
    return true
end function

constant {entities,entitych} = columnize({{"&lt;",'<'},
                                          {"&gt;",'>'},
                                          {"&amp;",'&'},
                                          {"&apos;",'\''},
                                          {"&quot;",'\"'}})

-- not sure we really need this... (warning, largely untested)
-- also note there is no intention to encode these back, ever.
--  [the latter would require utf8_to_utf32(full string), then
--   process that, and a full utf32_to_utf8() back at the end.]
constant {extended,extendedch} = columnize({
    {"&Aacute;",    193},   {"&aacute;",    225},   {"&Acirc;",     194},
    {"&acirc;",     226},   {"&acute;",     180},   {"&AElig;",     198},
    {"&aelig;",     230},   {"&Agrave;",    192},   {"&agrave;",    224},
    {"&Alpha;",     913},   {"&alpha;",     945},   {"&Aring;",     197},
    {"&aring;",     229},   {"&Atilde;",    195},   {"&atilde;",    227},
    {"&Auml;",      196},   {"&auml;",      228},   {"&Beta;",      914},
    {"&beta;",      946},   {"&brkbar;",    166},   {"&brvbar;",    166},
    {"&bull;",      149},   {"&Ccedil;",    199},   {"&ccedil;",    231},
    {"&cedil;",     184},   {"&cent;",      162},   {"&Chi;",       935},
    {"&chi;",       967},   {"&clubs;",    9827},   {"&copy;",      169},
    {"&curren;",    164},   {"&dagger;",    134},   {"&Dagger;",    135},
    {"&darr;",     8595},   {"&deg;",       176},   {"&Delta;",     916},
    {"&delta;",     948},   {"&diams;",    9830},   {"&die;",       168},
    {"&divide;",    247},   {"&Dstrok;",    208},   {"&Eacute;",    201},
    {"&eacute;",    233},   {"&Ecirc;",     202},   {"&ecirc;",     234},
    {"&Egrave;",    200},   {"&egrave;",    232},   {"&Epsilon;",   917},
    {"&epsilon;",   949},   {"&Eta;",       919},   {"&eta;",       951},
    {"&ETH;",       208},   {"&eth;",       240},   {"&Euml;",      203},
    {"&euml;",      235},   {"&euro;",     8364},   {"&frac12;",    189},
    {"&frac14;",    188},   {"&frac34;",    190},   {"&frasl;",    8260},
    {"&Gamma;",     915},   {"&gamma;",     947},   {"&harr;",     8596},
    {"&hearts;",   9829},   {"&hibar;",     175},   {"&Iacute;",    205},
    {"&iacute;",    237},   {"&Icirc;",     206},   {"&icirc;",     238},
    {"&iexcl;",     161},   {"&Igrave;",    204},   {"&igrave;",    236},
    {"&Iota;",      921},   {"&iota;",      953},   {"&iquest;",    191},
    {"&Iuml;",      207},   {"&iuml;",      239},   {"&Kappa;",     922},
    {"&kappa;",     954},   {"&Lambda;",    923},   {"&lambda;",    955},
    {"&laquo;",     171},   {"&larr;",     8592},   {"&loz;",      9674},
    {"&lsaquo;",   8249},   {"&macr;",      175},   {"&mdash;",     151},
    {"&micro;",     181},   {"&middot;",    183},   {"&Mu;",        924},
    {"&mu;",        956},   {"&nbsp;",      160},   {"&ndash;",     150},
    {"&not;",       172},   {"&Ntilde;",    209},   {"&ntilde;",    241},
    {"&Nu;",        925},   {"&nu;",        957},   {"&Oacute;",    211},
    {"&oacute;",    243},   {"&Ocirc;",     212},   {"&ocirc;",     244},
    {"&oelig;",     156},   {"&Ograve;",    210},   {"&ograve;",    242},
    {"&Omega;",     937},   {"&omega;",     969},   {"&Omicron;",   927},
    {"&omicron;",   959},   {"&ordf;",      170},   {"&ordm;",      186},
    {"&Oslash;",    216},   {"&oslash;",    248},   {"&Otilde;",    213},
    {"&otilde;",    245},   {"&Ouml;",      214},   {"&ouml;",      246},
    {"&para;",      182},   {"&Phi;",       934},   {"&phi;",       966},
    {"&Pi;",        928},   {"&pi;",        960},   {"&plusmn;",    177},
    {"&pound;",     163},   {"&Psi;",       936},   {"&psi;",       968},
    {"&raquo;",     187},   {"&rarr;",     8594},   {"&reg;",       174},
    {"&Rho;",       929},   {"&rho;",       961},   {"&rsaquo;",   8250},
    {"&sect;",      167},   {"&shy;",       173},   {"&Sigma;",     931},
    {"&sigma;",     963},   {"&sigmaf;",    962},   {"&spades;",   9824},
    {"&sup1;",      185},   {"&sup2;",      178},   {"&sup3;",      179},
    {"&szlig;",     223},   {"&Tau;",       932},   {"&tau;",       964},
    {"&Theta;",     920},   {"&theta;",     952},   {"&THORN;",     222},
    {"&thorn;",     254},   {"&times;",     215},   {"&Uacute;",    218},
    {"&uacute;",    250},   {"&uarr;",     8593},   {"&Ucirc;",     219},
    {"&ucirc;",     251},   {"&Ugrave;",    217},   {"&ugrave;",    249},
    {"&uml;",       168},   {"&Upsilon;",   933},   {"&upsilon;",   965},
    {"&Uuml;",      220},   {"&uuml;",      252},   {"&Xi;",        926},
    {"&xi;",        958},   {"&Yacute;",    221},   {"&yacute;",    253},
    {"&yen",        165},   {"&Yuml;",      159},   {"&yuml;",      255},
    {"&Zeta;",      918},   {"&zeta;",      950}})

global function xml_decode(string s)
-- convert all eg &lt; to '<'
    integer k, l, startidx = 1, ch, nibble, digit
    string res = "", entity
    k = match("<![CDATA[",s)
    if k!=0 then
        if k>1 then
            res = xml_decode(s[1..k-1])
        end if
        startidx = k+9
        k = match("]]>",s,startidx)
        if k=0 then ?9/0 end if
        res &= s[startidx..k-1]     -- (unchanged)
        k += 3
        if k<=length(s) then
            res &= xml_decode(s[k..$])
        end if
        s = res
    else
        k = 1
        while 1 do
            k = find('&',s,k)
            if k=0 then exit end if
            res &= s[startidx..k-1]
            l = find(';',s,k)
            if l=0 then ?9/0 end if
            entity = s[k..l]
            ch = 0
            if entity[2]='#' then
                if entity[3]='x' then
                    for i=4 to length(entity)-1 do
                        nibble = upper(entity[i])
                        if nibble>='0' and nibble<='9' then
                            nibble -= '0'
                        elsif nibble>='A' and nibble<='F' then
                            nibble -= 'A'-10
                        else
                            ?9/0
                        end if
                        ch = ch*16+nibble
                    end for
                else
                    for i=3 to length(entity)-1 do
                        digit = entity[i]
                        if digit<'0' 
                        or digit>'9' then
                            ?9/0
                        end if
                        ch = ch*10+digit-'0'
                    end for
                end if
                if ch=0 then ?9/0 end if
                entity = utf32_to_utf8({ch})
            else
                k = find(entity,entities)
                if k!=0 then
                    entity = ""&entitych[k]
                else
                    k = find(entity,extended)
                    if k!=0 then
                        entity = utf32_to_utf8({extendedch[k]})
                    end if
                end if
            end if
            res &= entity
            k = l+1
            startidx = k
        end while
        if length(res) then
            res &= s[startidx..$]
            s = res
        end if
    end if
    return s
end function

global function xml_encode(string s)
--
-- Inverse of xml_decode, but without any CDATA handling
-- or any re-coding of anything except the five entitych
--
    string res = ""
    integer startidx = 1, ch, k
    for i=1 to length(s) do
        ch = s[i]
        k = find(ch,entitych)
        if k!=0 then
            res &= s[startidx..i-1]
            res &= entities[k]
            startidx = i+1
        end if
    end for
    if length(res) then
        res &= s[startidx..$]
        s = res
    end if
    return s
end function

function xml_parse_attributes(integer options)
--  if ch='<' then return xml_fatal("'<' *un*expected") end if
    if ch='<' then ?9/0 end if -- internal error
    sequence attributes = {{},{}}
    integer namestart = col
    while 1 do
        if ch='=' then
            string name = text[namestart..col-1]
            attributes[XML_ATTRNAMES] = append(attributes[XML_ATTRNAMES],name)
            col += 1
            xml_next_ch()
            if ch!='\"' and ch!='\'' then return xml_fatal("quote expected") end if
            integer valuestart = col,
                    valueend = find(ch,text,col+1)
            if valueend=0 then return xml_fatal("missing closing quote") end if
            string attr_value = text[valuestart+1..valueend-1]
            if and_bits(options,XML_DECODE) then
                attr_value = xml_decode(attr_value)
            end if
            attributes[XML_ATTRVALUES] = append(attributes[XML_ATTRVALUES],attr_value)
            col = valueend+1
            xml_next_ch()
            if find(ch,">/<") then exit end if
            namestart = col
        elsif ch=-1 or find(ch," \t\r\n>/<") then
            return xml_fatal("'=' expected")
        else
            col += 1
            xml_next_ch()
        end if
    end while       
    if ch='<' then return xml_fatal("'<' *not* expected") end if
    if not find(ch,">/") then ?9/0 end if -- internal error
    return attributes
end function

function xml_parse_tag(integer options)
    if ch!='<' then return xml_fatal("'<' expected") end if
    integer tagstart = col+1
    while 1 do
        col += 1
--      xml_next_ch()   -- NO!
        ch = text[col]
        if find(ch," \t\r\n/>") then exit end if
        if ch<#20
        or find(ch,"!\"#$%&'()*+,/;<=>?@[\\]^`{|}~") then
            return xml_fatal("illegal character in tag name")
        end if
    end while
    string tagname = text[tagstart..col-1]
    if find(tagname[1],"-.0123456789") then
        return xml_fatal("illegal tag start character")
    end if
    xml_skip_spaces()
    sequence attributes = {}
    if not find(ch,"/>") then
        attributes = xml_parse_attributes(options)
        if attributes[1]=-1 then return attributes end if
    end if
    sequence contents = {}
    if ch='/' then  -- self-closing tag
        col += 1
        xml_next_ch()
    else
        if ch!='>' then return xml_fatal("'>' expected") end if
        col += 1
        xml_next_ch()
        if ch='<' then
            while 1 do
                object tag = xml_parse_tag(options)
                if tag[1]=-1 then return tag end if
                contents = append(contents,tag)
                if ch=-1 or ch='>' then exit end if
                if ch='<' and text[col+1]='/' then exit end if
            end while               
        else
            integer cstart = col
            while 1 do
                col += 1
                xml_next_ch()
                if ch=-1 or find(ch,"<>") then exit end if
                if ch='/' and text[col+1]='>' then exit end if
            end while
            contents = text[cstart..col-1]
            if ch='/' then col+= 1 end if
        end if
        string endtag = "</"&tagname
        if not xml_match(endtag) then return xml_fatal(endtag&"> expected") end if
    end if
    if ch!='>' then return xml_fatal("'>' expected") end if
    col += 1
    xml_next_ch()
    return {tagname,attributes,contents}
end function

function xml_parse_comments()
    sequence comments = {}
    while true do
        integer comment_start = col,
                comment_end = match("-->",text,col)
        if comment_end=0 then return xml_fatal("--> missing") end if
        col = comment_end+2
        comments = append(comments,text[comment_start..col])
        col += 1
        xml_next_ch()
        if ch!='<' or text[col+1]!='!' then exit end if
    end while
    return comments
end function

global function xml_parse(string xml, integer options=NULL)
--
-- Convert an xml string into a nested structure.
-- Note the precise content of the result is not documented
-- beyond the XML_XXX constants; the programmer is expected
-- to examine the ouput from increasingly more complex, but
-- valid xml until they understand the structure and how to
-- use the XML_XXX constants, quite straightforward really.
-- [DOC] The examples below should get you started.
--
-- (At this point in time the structure is quite likely to
--  change with each release as more fuctionality is added.)
--
-- returns {-1,"message",...} if xml could not be parsed.
-- success can be determined by checking whether result[1]
-- is a string, or -1, or better yet =="document".
--
    sequence res = {"document",{},{},{}}
    text = xml
    textlen = length(text)
    ch = ' '
    col = 0
    xml_skip_spaces()
    if ch!='<' then
--/*
-- SUG: convert utf16 to utf8?
constant 
    UTF8    = "\#EF\#BB\#BF",
    UTF16BE = "\#FE\#FF",
    UTF16LE = "\#FF\#FE",
    UTF32BE = "\#00\#00\#FE\#FF",
    UTF32LE = "\#FF\#FE\#00\#00",
--*/
        -- allow "UTF-8 lead bytes" (0xef 0xbb 0xbf):
        if ch=#EF and text[col+1]=#BB and text[col+2]=#BF then
            --SUG: prolog &= "encoding="utf-8"?
            col += 3
            xml_next_ch()
        end if
        if ch!='<' then return xml_fatal("'<' expected") end if
    end if
--  if text[col+1]='?' then
    while text[col+1]='?' do
        integer prologue_start = col
        col = match("?>",text,col+2)
        if col=0 then return xml_fatal("?> missing") end if
        res[XML_PROLOGUE] = append(res[XML_PROLOGUE],text[prologue_start..col+1])
        col += 2
        xml_next_ch()
    end while
    if text[col+1]='!' then
--      res[XML_PROLOGUE] = {res[XML_PROLOGUE],xml_parse_comments()}
        res[XML_PROLOGUE] = append(res[XML_PROLOGUE],xml_parse_comments())
    end if
    object contents = xml_parse_tag(options)
    if not string(contents[1]) then return contents end if
    res[XML_CONTENTS] = contents
    if ch!=-1 then
        res[XML_EPILOGUE] = xml_parse_comments()
        if ch!=-1 then return xml_fatal("unrecognised") end if
    end if
    if length(res)!=XML_DOCLEN then ?9/0 end if
    return res
end function

function xml_sprint_contents(sequence xml, integer nest, options)
    if length(xml)!=XML_ELEMLEN then ?9/0 end if
    string name = xml[XML_TAGNAME],
           pad = repeat(' ',nest*2),
           res = pad&"<"&name
    sequence contents = xml[XML_CONTENTS]
    if xml[XML_ATTRIBUTES]!={} then
        sequence {n,v} = xml[XML_ATTRIBUTES]
        for i=1 to length(n) do
--DEV and whitespace normalisation... 
--(replace \r\n\t etc with space, multiple space with one, and trim())
            string vi = v[i]
            if and_bits(options,XML_ENCODE) then
                vi = substitute(vi,"\"","&quot;")
--              vi = xml_encode(vi,"\"","&quot;")??
            end if
            res &= sprintf(" %s=\"%s\"",{n[i],vi})
        end for
        if length(contents)=0 then
            res &= " "  -- optional??...
        end if
    end if
    if length(contents) = 0 then
        res &= "/>\n"
--      res &= " />\n"  -- ...or is this better??
    elsif string(contents) then
        if and_bits(options,XML_ENCODE) then
            contents = substitute(contents,"\"","&quot;")
--          contents = xml_encode(contents,"\"","&quot;")
        end if
        res &= ">"&contents&"</"&name&">\n"
    else
        res &= ">\n"
        for i=1 to length(contents) do
            res &= xml_sprint_contents(contents[i],nest+1,options)
        end for
        res &= pad&"</"&name&">\n"
    end if
    return res
end function

function xml_joint(sequence comments)
string res = ""
    for i=1 to length(comments) do
        res &= comments[i]&"\n"
    end for
    return res
end function

global function xml_sprint(sequence xml, integer options=NULL)
    if length(xml)!=XML_DOCLEN
    or xml[XML_DOCUMENT]!="document" then
        ?9/0
    end if
    string res = xml_joint(xml[XML_PROLOGUE])
--  string res = ""
--  sequence prolog = xml[XML_PROLOGUE]
--  if prolog!={} then
--      if string(prolog) then
--          res &= prolog&"\n"
--      else
--          if length(prolog[1]) then
--              res &= prolog[1]&"\n"
--          end if
--          prolog = prolog[XML_COMMENTS]
--          for i=1 to length(prolog) do
--              res &= prolog[i]&"\n"
--          end for
--      end if
--  end if
    res &= xml_sprint_contents(xml[XML_CONTENTS],0,options)
    res &= aml_joint(xml[XML_EPILOGUE])
--  sequence epilog = xml[XML_EPILOGUE]
--  if epilog!={} then
--      for i=1 to length(epilog) do
--          res &= epilog[i]&"\n"
--      end for
--  end if
    return res
end function

constant std_prolog = "<?xml version=\"1.0\" encoding=\"utf-8\" ?>"

global function xml_new_doc(sequence contents={}, prolog=std_prolog, epilog={})
    return {"document",prolog,contents,epilog}
end function

global function xml_new_element(string tagname, sequence contents)
--  return {tagname,{},{}}
--  if not string(contents) and contents!={} ... erm...
    return {tagname,{},contents}
end function

global function xml_get_attribute(sequence elem, string name)
--  if elem[XML_DOCUMENT]="document" then ?9/0 end if
    if length(elem)!=XML_ELEMLEN then ?9/0 end if
    integer ndx = find(name,elem[XML_ATTRIBUTES][XML_ATTRNAMES])
    return iff(ndx=0?"":elem[XML_ATTRIBUTES][XML_ATTRVALUES][ndx])
end function

global function xml_set_attribute(sequence elem, string attrib_name, attrib_value)
--  if xml[XML_DOCUMENT]="document" then ?9/0 end if
    if length(elem)!=XML_ELEMLEN then ?9/0 end if
    if elem[XML_ATTRIBUTES]={} then
        if length(attrib_value) then
            elem[XML_ATTRIBUTES] = {{attrib_name},{attrib_value}}
        end if
    else
        sequence {n,v} = elem[XML_ATTRIBUTES]
        elem[XML_ATTRIBUTES] = 0    -- (kill refcount)
        integer ndx = find(attrib_name,n)
        if ndx=0 then
            if length(attrib_value) then
                n = append(n,attrib_name)
                v = append(v,attrib_value)
            end if
        else
            if length(attrib_value) then
                v[ndx] = attrib_value
            else
                n[ndx..ndx] = {}
                v[ndx..ndx] = {}
            end if
        end if
        elem[XML_ATTRIBUTES] = {n,v}
    end if
    return elem
end function

global function xml_get_nodes(sequence xml, string tagname)
    sequence res = {},
             contents = xml[XML_CONTENTS]
    for i=1 to length(contents) do
        sequence content = contents[i]
        if content[XML_TAGNAME]=tagname then
            res = append(res,content)
        end if
    end for
    return res
end function

--global function get_xml_content(sequence node, string fieldname)
--  for 
--  string res = 

global function xml_add_comment(sequence xml, string comment, bool as_prolog=true)
    if xml[XML_DOCUMENT]!="document" then ?9/0 end if
    if length(xml)!=XML_DOCLEN then ?9/0 end if
    if comment[1]!='<' then
        if match("--",comment) then ?9/0 end if
        comment = "<!-- "&comment&" -->"
    end if
    if as_prolog then
--      sequence prolog = xml[XML_PROLOGUE]
--      xml[XML_PROLOGUE] = 0 -- (kill refcount, may as well)
--      if prolog={} or string(prolog) then
--          prolog = {prolog,{comment}}
--      else
--          prolog[XML_COMMENTS] = append(prolog[XML_COMMENTS],comment)
--      end if
--      xml[XML_PROLOGUE] = prolog
        xml[XML_PROLOGUE] = append(xml[XML_PROLOGUE],comment)
    else
        xml[XML_EPILOGUE] = append(xml[XML_EPILOGUE],comment)
    end if
    return xml
end function


