--
-- p2js_htmlise.e
-- ==============
--
--  Re-work of p2js/syntax_colour() which became idle_action() to use html spans.
--   (this is pretty fast, it's the ADDFORMATTAG_HANDLE operation that's slow)
--  Used for Ctrl M (Copy as MediaWiki) and (probably) for the web version of p2js.
--
constant {hchars,hsubs} = columnize({{"<","&lt;"},
--                                   {">","&gt;"},
                                     {">","&gt;"},
                                     {"'\\''","<nowiki>'\\''</nowiki>"}})
--                                   {"&","&amp;"},
--                                   {`"`,"&quot;"},
--                                   {`'`,"&apos;"}})

function space_indent(string s)
    bool bPEOL = false
    integer cr
    string res = ""
    for i=1 to length(s) do
        integer ch = s[i]
        bool bEOL = find(ch,"\r\n")
        if bEOL then
            if not bPEOL then
                cr = ch
            elsif ch=cr then
                res &= ' '
            end if
        elsif bPEOL then
            res &= ' '
        end if
        bPEOL = bEOL
        res &= ch
    end for
    if bPEOL then res &= ' ' end if
    res = substitute_all(res,hchars,hsubs)
    return res
end function

global function htmlise(string hsrc, sequence hokens)
--  string output = " "
--  string output = "<!--<lang Phix>-->\n ", colour, prev_colour = ""
    string output = "<!--<lang Phix>(phixonline)-->\n ", colour, prev_colour = ""
    integer done = 0, hdx = 1
    while hdx<=length(hokens) do
        integer {toktype, tokstart, tokfinish} = hokens[hdx] 
        if done<tokstart-1 then
            output &= space_indent(hsrc[done+1..tokstart-1])
            prev_colour = ""
            done = tokstart-1
        end if
        if toktype=COMMENT
        or toktype=BLK_CMT
        or toktype='`' then
            if toktype='`' then             -- multi-line string
                colour = IUP_DARK_GREEN
                if hsrc[tokstart-2..tokstart] = "\"\"`"
                and hsrc[tokfinish..tokfinish+2] = "`\"\"" then
                    -- note this is assuming "is_phix()"/"phix->phix"
                    if output[-2..-1]!=`""` then ?9/0 end if
                    output = output[1..-3]
                    hsrc[tokstart] = '"'
                    hsrc[tokfinish] = '"'
--                  tokstart -= 2
                    done -= 2
                    tokfinish += 2
                end if
            else
                while hdx<length(hokens)
                  and find(hokens[hdx+1][TOKTYPE],{COMMENT,BLK_CMT}) do
                    hdx += 1
                end while
                tokfinish = hokens[hdx][TOKFINISH]
                colour = IUP_NAVY & `;font-style:italic`
            end if
            output &= `<span style="color: ` & colour & `;">` & space_indent(hsrc[done+1..tokfinish]) & `</span>`
            prev_colour = ""
            done = tokfinish
        else
            if toktype=LETTER then
                toktype = hokens[hdx][TOKTTIDX]
            end if
            string txt = hsrc[done+1..tokfinish]
            if toktype=DIGIT
            or toktype='#' then
                colour = IUP_BLACK
            elsif find(toktype,{'"','\''}) then -- string
                colour = IUP_DARK_GREEN
                txt = substitute_all(txt,hchars,hsubs)
            elsif toktype<128                   -- symbols
              and toktype!=T_string
              and toktype!=T_nullable_string then
                colour = IUP_BLUE
            elsif toktype<256 
              and remainder(toktype,2)=1 then   -- multisym
                colour = IUP_BLUE
            elsif toktype<=T_object then        -- types
                colour = "#004080"
--20/10/21: (ah no, this be for rosettacode...)
--          elsif toktype<=T_trace then         -- keywords
--          elsif toktype<=T_try then           -- keywords
--              colour = IUP_RED
            elsif toktype<=T_xor then           -- keywords
                colour = IUP_TEAL
--          elsif toktype<=T_yield then         -- builtins
--          elsif toktype<=T_xor_bitsu then     -- builtins
            elsif toktype<=T_zIndex then        -- builtins
                colour = "#7060A8"
--          elsif toktype<=T_wait_key then      -- not pwa/p2js
            elsif toktype<=T_yield then         -- not pwa/p2js
--              colour = IUP_RED
                if find(toktype,{T_try,T_catch}) then
                    colour = IUP_TEAL
                else
                    colour = "#7060A8"
                end if
            elsif toktype<=T_WEB then           -- constants
                colour = "#004600"
            else                                -- everything else
                colour = IUP_BLACK
            end if
            if colour=prev_colour and output[-7..-1]=`</span>` then
                output = output[1..-8]          -- (extend existing)
            else
                output &= `<span style="color: ` & colour & `;">`
            end if
            output &= txt & `</span>`
            prev_colour = colour
            done = tokfinish
        end if
        hdx += 1
    end while
--  if done<length(hsrc) then --- we no need no stinky traily spacey!
    output &= "\n<!--</lang>-->"
    return output
end function

