--
-- src\bcfwd.e
-- src\syntax_colour.e
-- ===========
--
--  Complete rewrite/replacement/rethink of the equivalent in Edita/Edix.
--  Implements syntax_colour(), rebuild_bcfwd(), ...
--
--  Implements bracket-carry-forward: in the simplest possible case,
--      sequence s = {
--                    {"line 2"}
--                   }
--  line 1 has an unmatched {, line 2 must take that into account but does not 
--  need an entry since in===out, and line 3 has the closing }, hence needs an 
--  entry so that any subsequent lines would not re-apply the line 1 c/f. Note 
--  the absence of line 2 makes the line 1 c/f apply equally to both 2 and 3.
--  bcfwd might be {{1,0,0,"}"},{3,0,0,""}}, the precise details of which will
--  be explained below, but first you should recognise that nested syntax, and 
--  for that matter block comments and multiline strings need similar handling.
--
--  This is the nested syntax I have thus far defined, with no more planned,
--  that is, it genuinely covers /every/ need case I can currently think of:
--
--  Phix.syn:   NestedSyntax
--              FASM #ilASM{ }
--              **end NestedSyntax
--
--  FASM.syn:   NestedSyntax
--              ARM [ARM] [[PE|ELF][32|64]]
--              **end NestedSyntax
--
--  Html.syn:   NestedSyntax
--              pphp <?php ?>
--              js <script[_type="text/javascript"]> </script>
--              css <style> </style>
--              **end NestedSyntax
--  
--  So, Phix supports inline assembly, which by default is x86 but can have
--  further nested ARM snippets (based on standalone .asm/.arm files, not 
--  that the latter can nest since "[ARM]" is a Phix not FASM thing), and
--  Html can embed PHP, JavaScript, and CSS snippets.
--   (nb .php files are html, so I've made up ".pphp" for pure php files)		[DEV TODO...]
--   (ARM lowest priority, Edita/Edix now load but then ignore the above)
--   (FASM may one day get "js [JS] [*]" probably as first-only fragment)
--  
--  Each entry in bcfwd contains the following fields:
--
--/*
integer bcf_line,   -- sparse, otherwise self-explanatory
        bcf_block,  -- 0: none, 
                    -- +ve: block comment nesting level
                    -- -1: backtick multi-line string
                    -- -2: triplequote ""
                    -- -3: singlequote "" (not Phix)
        bcf_lang    -- language code or 0
object bcf_close_rqd -- 0 or string, such as ")}". (lifo)
--*/
enum bcf_line,  -- sparse, otherwise self-explanatory
    bcf_block,  -- 0: none, 
                -- +ve: block comment nesting level
                -- -1: backtick multi-line string
                -- -2: triplequote ""
                -- -3: singlequote "" (not Phix)
     bcf_lang,  -- language code or 0
  bcf_close_rqd -- 0 or string, such as ")}". (lifo)
--
--  Note that bcf_close_rqd is a lifo stack, hence after say ([,
--   [bcf_close_rqd] ends up as ")]", not "])".
--  If bcf_close_rqd is ")]" then ')' gets a syntax-red treatment,
--   whereas a line with just ']' inserts a replacement ")" entry.
--  Note that Phix->FASM->ARM nesting is hard-coded to "pop" FASM,
--     since there is no other double-nested syntax we care about,
--     and all other syntax "pop" can simply set [bcf_lang] to 0.
--  Also, theEditor assumes block comments are nestable, even for
--     programming languages which do not support that (see docs). 
--
--  There is, fairly obviously, some initialisation via background/idle
--  and bcfwd is updated after /*every*/ line is displayed. While vital
--  it gets the first display line spot-on, nothing after that actually
--  matters, as long as a jump-to completes things properly [via idle].
--  Wrong at first then rewdraw correctly 0.1s later is also fine, just
--  as long as it is prioritising the on-screen stuff properly.
--

global sequence bCfwd = {} -- DEV document properly... (aka let the fun begin!)

global sequence closeRqd = repeat(0,32) -- lifo stack of expected closing brackets
--global integer level = 0                  -- updated by sytax_colour(), ditto ""

-- essentially we're only looking out for "[{(<#>)}]" characters..
-- alsol quotes, comments, ...

--DEV I'm beginning to think maxline is not a good idea here...
--global procedure rebuild_bcfwd(integer maxline)
global procedure rebuild_bcfwd(integer fdx)
sequence lastset = {}
integer ch, ch2, ctype, chidx1
integer bracket_level = 0,
            lastlevel = 0,
                bcomm = 0,
            lastbcomm = 0
--integer EuSq -- euphoria/phix style single quote handling

    bCfwd = {}
    for i,line in filetext[fdx] do
--?{fdx,i,line}
        integer ll = length(line),
             chidx = 1
        while chidx<=ll do
            ch = line[chidx]
            if bcomm>=0 then
                for bci,bc in blockComment do
                    if begins(bc,line,chidx) then
                        if odd(bci) then -- opening
                            bcomm += 1
                        else             -- closing
                            bcomm = max(0,bcomm-1)
                        end if
                        chidx += length(bc)-1
                        exit
                    end if
                end for
            end if
            if bcomm=0 then
                bool cfound = false
                for c in lineComments do
                    cfound = begins(c,line,chidx)
                    if cfound then exit end if
                end for
                if cfound then exit end if
            end if
            if bcomm<=0 then
                ctype = iff(bcomm<0?String:iff(ch>128?TokenChar:charMap[ch+1]))
                if ctype=OpenBrace then
                    bracket_level += 1
                    if bracket_level>length(closeRqd) then
                        closeRqd &= 0
                    end if
                    if bracket_level>0 then
                        closeRqd[bracket_level] = ch+1+(ch!='(')
                    end if
                elsif ctype=CloseBrace then
                    bracket_level -= 1
                elsif ctype=String then
--DEV need special handling for `(backtick) and """(treble quote)
--(set to -1 for any non-closed lines)
                    chidx1 = chidx
                    -- Euphoria/Phix has special single quote handling --DEV .syn file option
                    bool EuSq = (ch='\'' and equal(SynNames[newSyntax],"Euphoria"))
                    if bcomm<0 then
--  if bcomm=-1 then
--      ch = '`'
--  elsif bcomm=-2 then
--      ch = '"'
--  else -- -3
--      ch = '\''
--  end if
                        ch = "\'\"`"[bcomm]
                        chidx -= 1
                    else
                        if ch='`' then
                            bcomm = -1
--                      elsif ch='"' and chidx<=ll-2 and equal(line[chidx..chidx+2],`"""`) then
                        elsif begins(`"""`,line,chidx) then
                            bcomm = -2
                            chidx += 2
                        end if
                    end if
                    while chidx<ll do
                        chidx += 1
                        ch2 = line[chidx]
                        if ch2=ch then      -- closing quote found
                            if bcomm=-2 then
--  -- we're really looking for `"""`
--  if chidx<=ll-2 and equal(line[chidx..chidx+2],`"""`) then
                                if begins(`"""`,line,chidx) then
                                    chidx += 2
                                    bcomm = 0
                                    exit
                                end if
                            else
                                bcomm = 0
                                if ch2='\''
                                and chidx=chidx1+1  -- so '' found,
                                and chidx<ll
                                and line[chidx+1]='\'' then
                                    -- but let ''' through as well/as one (fine on Phix, wrong for RDS Eu)
                                    chidx += 1
                                end if
                                exit
                            end if
                        elsif EuSq and ch='\'' and chidx!=chidx1+1 then     -- and not about to check 1st char.
                            chidx -= 1
                            exit
                        elsif length(Escapes[newSyntax]) 
                          and ch2=EscapeLeadIns[newSyntax] then
                            if chidx>=ll then
-- 27/12/20
                                if SynNames[newSyntax]="css" and ch='\'' and ch2='\\' then
                                    bcomm = -3
                                end if
                                exit
                            end if
                            ch2 = line[chidx+1]
                            if not find(ch2,Escapes[newSyntax]) then
-- 27/12/20
                                if SynNames[newSyntax]="css" and ch='\'' and ch2='\\' then
                                    bcomm = -3
                                end if
                                exit
                            end if
                            if ch2='#' then
                                if chidx>=ll-2
                                or not find(line[chidx+2],"0123456789ABCDEFabcdef")
                                or not find(line[chidx+3],"0123456789ABCDEFabcdef") then
                                    exit
                                end if
                                chidx += 3
                            else
                                chidx += 1
                            end if
                        end if
                    end while
                end if
            end if
            chidx += 1
        end while
        if bracket_level<0 then
            bracket_level = 0
        end if
        if bracket_level!=lastlevel or bcomm!=lastbcomm
        or (bracket_level>0 and not equal(lastset,closeRqd[1..bracket_level])) then
            lastlevel = bracket_level
            lastbcomm = bcomm
            lastset = closeRqd[1..bracket_level]
            bCfwd = append(bCfwd,{i,bcomm,lastlevel,lastset})
--?bCfwd
        end if
    end for
--  ?time()-t
--  ?length(bCfwd)
--?{fdx,bCfwd,SynNames[newSyntax]}
    bCfwds[fdx] = bCfwd
end procedure

--/* 5/8/25 looking pretty good to me:
    bCfwds[1] = {{286,1,0,{}},{347,0,0,{}},{349,1,0,{}},{635,0,0,{}}}
    bCfwds[2..3] = {{},{}}
    bCfwds[4][1..9] = {{2,1,0,{}},{8,0,0,{}},{10,1,0,{}},{12,2,0,{}},{14,3,0,{}},{16,4,0,{}},{18,3,0,{}},{19,2,0,{}},{20,1,0,{}}}
    bCfwds[4][10..18] = {{21,0,0,{}},{22,1,0,{}},{24,0,0,{}},{38,-2,0,{}},{40,0,0,{}},{42,-2,0,{}},{44,0,0,{}},{46,-1,0,{}},{47,0,0,{}}
    bCfwds[5][1..6] = {{19,0,1,{'}'}},{26,1,1,{'}'}},{31,0,1,{'}'}},{33,0,0,{}},{43,0,1,{'}'}},{50,1,1,{'}'}}}
    bCfwds[5][7..12] = {{58,0,1,{'}'}},{62,0,0,{}},{72,0,1,{'}'}},{79,1,1,{'}'}},{85,0,1,{'}'}},{89,0,0,{}}}
    bCfwds[5][13..14] = {{97,0,1,{'}'}},{99,0,0,{}}}
    bCfwds[6] = {{2,0,1,{'}'}},{8,0,0,{}},{29,0,1,{'}'}},{37,0,0,{}}}
    bCfwds[7] = {{6,0,1,{125'}'}},{13,0,0,{}},{19,0,1,{125'}'}},{21,0,2,{125'}',125'}'}},{27,0,1,{125'}'}},{28,0,0,{}}}
    bCfwds[8] = {}
--*/

integer startlevel, startblockcomment, startlang
sequence startCfwd
integer exactmatch

procedure getBrktCfwd(integer lineno)
    exactmatch = 0
--  if sequence(bCfwd) then --DEV prevent crash?
        for i=length(bCfwd) to 1 by -1 do
--          integer biline = bCfwd[i][B_line]
            integer biline = bCfwd[i][bcf_line]
            if lineno>biline then
--              startblockcomment = bCfwd[i][B_blockcomment]
                startblockcomment = bCfwd[i][bcf_block]
--              startlevel = bCfwd[i][B_level]
--              startCfwd = bCfwd[i][B_closesrqd]
                startCfwd = bCfwd[i][bcf_close_rqd]
                startlevel = length(startCfwd)
                closeRqd[1..startlevel] = startCfwd
                startlang = bCfwd[i][bcf_lang]
                return
            elsif lineno=biline then
                exactmatch = 1
            end if
        end for
--  end if
    startblockcomment = 0
    startlevel = 0
    startCfwd = {}
    startlang = 0
    return
end procedure

procedure cleanup(integer k)
--
-- Remove any obviously uneccessary entries around the insertion/update point.
-- It is not critical if a few odd ends get left in bCfwd, though.
--
    integer thisline = bCfwd[k][bcf_line]
    while k>1 do
        k -= 1
        if bCfwd[k][bcf_line]!=thisline then exit end if
        -- duplicate, from updateQJ()
        bCfwd = bCfwd[1..k-1]&bCfwd[k+1..length(bCfwd)]
    end while
--  if k>1 and equal(bCfwd[k-1][2..4],bCfwd[k][2..4]) then
    if k>1 and equal(bCfwd[k-1][bcf_block..bcf_close_rqd],
                     bCfwd[k  ][bcf_block..bcf_close_rqd]) then
        bCfwd = bCfwd[1..k-1]&bCfwd[k+1..length(bCfwd)]
    end if
end procedure

procedure setBrktCfwd(integer lineno, bcomm, level, lang)
-- Save the bracket carry-forward info for (the end of) the current line.
-- We can guarantee that getBrktCfwd has just been called for the current line.
    if level!=startlevel
    or bcomm!=startblockcomment
    or lang!=startlang
    or exactmatch
    or not equal(closeRqd[1..level],startCfwd) then
--      if sequence(bCfwd) then -- DEV hack!
            integer k = length(bCfwd)
            if level<0 then
                level = 0
            end if
            while k do
--DEV tryme:
--if 0 then
--              if lineno>=bCfwd[k][bcf_line] then exit end if
--else
                if lineno=bCfwd[k][bcf_line] then
                    bCfwd[k] = {lineno,bcomm,lang,closeRqd[1..level]}
                    cleanup(k)
--                  paintLast = 0
                    return
                elsif lineno>bCfwd[k][bcf_line] then
                    exit
                end if
--end if
                k -= 1
            end while
--DEV tryme:
--if 0 then
--          sequence this = {lineno,bcomm,lang,closeRqd[1..level]}
--          if k then
--              bCfwd[k] = this
--          else
--              bCfwd = prepend(bCfwd,this)
--          end if
--else
            bCfwd = bCfwd[1..k]&{{lineno,bcomm,lang,closeRqd[1..level]}}&bCfwd[k+1..length(bCfwd)]
--end if
            cleanup(k+1)
--          paintLast = 0
--      end if
    end if
end procedure

--DEV...
integer abcl
string abc
bool isMarginSpace = true, marginRqd = 0
bool selON = false
integer selX = 0, selY = 0

--global 
function curb(integer low, integer val, integer high)
-- eg curb(0,y,11) returns y if y>=0 and y<=11, 0 if y<0, or 11 if y>11.
--  if val<=low then return low elsif val>=high then return high else return val end if
    return iff(val<=low?low:iff(val>=high?high:val))
--  return max(low,min(high,val))
end function

-->>>
integer ch2                 -- next/work character
integer ctype               -- the current character type
integer chidx               -- start of current "token" 
integer chidx2              -- end of current "token"+1
integer bcomm               -- block comment flag
integer lt                  -- length(text)

sequence word

sequence backC,textC,attrC
         backC = {}

--with trace
procedure scanForUrls(sequence text)
-- Scan for urls (such as http::\\wikipedia.org)
-- See also https://rosettacode.org/wiki/Find_URI_in_text#Phix for a standalone version
integer chidx0, syntaxClass
--trace(1)
    abcl = length(abc)  -- (abc="" means line comment scanning)
    atom cmclr = ColourTab[Comments]
    while chidx2<=lt do
        ch2 = text[chidx2]
        ctype = iff(ch2>128?TokenChar:wordChar[ch2+1])
        if ctype=TokenChar then
            --
            -- mark first part as comment
            --
            textC[chidx..chidx2-1] = cmclr
            attrC[chidx..chidx2-1] = StyleTab[Comments]
--24/8/23: (urls at the start of the line)
            if chidx2-1>chidx or text[chidx]<=' ' then
                chidx = chidx2
            end if
            while chidx2<=lt do
                ch2 = text[chidx2]
                if ch2<=128 then
                    ctype = wordChar[ch2+1]
                    if ctype!=TokenChar then exit end if
                end if
                chidx2 += 1
            end while                   
            word = text[chidx..chidx2-1]
            if chidx2>lt then exit end if
            ch2 = text[chidx2]
--1/7/16:
--          if (ch2=':' and find(word,WordLists[1][1]))
            if (ch2=':' and length(WordLists[1])!=0 and find(word,WordLists[1][1]))
            or (ch2='.' and equal(word,"www")) then
                chidx2 += 1
                chidx0 = chidx2
                syntaxClass = Comments
                string bstack = ""
                while chidx2<=lt do
                    ch2 = text[chidx2]
                    if ch2='\"' and chidx2=chidx0 then
                        while chidx2<lt do
                            chidx2 += 1
                            ch2 = text[chidx2]
                            if ch2='\"' then
                                chidx2 += 1
                                syntaxClass = URLs
                                exit
                            end if
                        end while
                        exit
                    elsif find(ch2,"(<[{") then
                        bstack &= ch2+iff(ch2='('?1:2)
                    elsif find(ch2,")>]}") then
                        if length(bstack)=0 or bstack[$]!=ch2 then exit end if
                        bstack = bstack[1..$-1]
                    end if
--2/1/23: (one assumes utf32... I guess utf//8// might be ok in a url... )
--                  if urlChar[ch2+1]!=TokenChar then exit end if
                    if ch2>255 or urlChar[ch2+1]!=TokenChar then exit end if
                    syntaxClass = URLs
                    chidx2 += 1
                end while
                if syntaxClass=URLs
                and text[chidx2-1]='.' then
                    chidx2 -= 1
                end if
                textC[chidx..chidx2-1] = ColourTab[syntaxClass]
                attrC[chidx..chidx2-1] = StyleTab[syntaxClass]
--              if chidx0!=chidx2 then
--                  textC[chidx..chidx2-1] = ColourTab[URLs]
--                  attrC[chidx..chidx2-1] = StyleTab[URLs]
--              else
--                  textC[chidx..chidx2-1] = cmclr
--                  attrC[chidx..chidx2-1] = StyleTab[Comments]
--              end if
                chidx = chidx2
                if chidx2>lt then exit end if
            else
                chidx2 -= 1
            end if
--      elsif abcl and ch2=abc[1]
--      and (abcl<2 or (chidx2<lt and text[chidx2+1]=abc[2]))
--      and (abcl<3 or (chidx2+1<lt and text[chidx2+2]=abc[3]))
--      and (abcl<4 or (chidx2+2<lt and text[chidx2+3]=abc[4])) then
--          chidx2 += abcl
----DEV -=1??: (then again where's the +=1?)
----            bcomm = 0
----14/2/2011
--          bcomm -= 1
--          exit
        else
if abcl then -- not line comment scanning
--DEV see above... (bci)
            for bci=1 to length(blockComment) do
                abc = blockComment[bci]
                abcl = length(abc)
                if abcl and ch2=abc[1]
                and (abcl<2 or (chidx2<lt and text[chidx2+1]=abc[2]))
                and (abcl<3 or (chidx2+1<lt and text[chidx2+2]=abc[3]))
                and (abcl<4 or (chidx2+2<lt and text[chidx2+3]=abc[4])) then
                    if remainder(bci,2) then
                        bcomm += 1
                    else
--DEV? (bcomm of -1 now means outstanding backtick/treblequote, not in a block comment)
if bcomm>0 then
                        bcomm -= 1
end if
                    end if
--15/10/2020! (nested block comments, --/* /**/ --*/)
--                  chidx2 += abcl
                    chidx2 += abcl-1
                    exit
                end if
            end for
--08/11/2020
--          if bcomm=0 then exit end if
            if bcomm=0 then chidx2 += 1 exit end if
end if
        end if
        chidx2 += 1
    end while
end procedure

--global integer r_findGlobal
--with trace
global integer chunkMax = 0,
               chovline = 0,
               chovfrom,
               chovto,
               chhovprevsnyclr
--             chovinfo
--sequence res = repeat(0,32)

integer acfile = 0
integer accY
        accY = -1
sequence acpkey = {}, 
         acdata1 = {}, 
         acrest = ""
--/*

global procedure autocomplete(sequence partKey, sequence data1)
    acfile = currfile
    accY = CursorY
    acpkey = partKey
    acdata1 = data1
end procedure

global function isautocomplete(integer keycode)
    if acfile=currfile
    and accY=CursorY then
--      if keycode=VK_ESCAPE then   -- ?? K_ESC (=#FF1B)
--      if keycode=#1B then
        if keycode=VK_ESC then
            acfile = 0
        end if
        if length(acdata1)>length(acpkey) then
            return 1
        end if
    end if
    return 0
end function

global function getautocomplete()
    return acdata1[length(acpkey)+1..length(acdata1)]
end function

global function getcY()
    return accY
end function

global function getacrest()
    return acrest
end function

global procedure stripacchar(integer ch)
    if acfile=currfile
    and accY=CursorY
    and length(acdata1)>length(acpkey)
    and acdata1[length(acpkey)+1]=ch then
        acpkey &= ch
--      acdata1 = acdata1[2..length(acdata1)] --NO!
--  else
--      acfile = 0
    end if
end procedure
--*/

global procedure set_syntax(integer fdx)
    -- Note: newSyntax := 0 (after eg idle use) is/shd/may be used to "force a reset"
    newSyntax = find(get_file_extension(filenames[fdx]),Extensions)
    if newSyntax then
        newSyntax = ExtensionNos[newSyntax]
    else
        newSyntax = 1
    end if
    lineComments = LineComments[newSyntax]
    blockComment = BlockComments[newSyntax]
    charMap = charMaps[newSyntax]
    ColourTab = colourTabs[newSyntax]
--DEV??
--  isCurrLineColoured = (ColourTab[HighLine]!=ColourTab[Background])
    StyleTab = styleTabs[newSyntax]
--DEV??
--  autoComplete = AutoCompletes[newSyntax]
    nestedSyntax = NestedSyntax[newSyntax]
end procedure

with trace
global function syntaxColour(sequence text, integer lineno, integer marginWidth, integer firstch)
--
-- Create a "chunk" table of colours for the current line.
-- The chunk table is length, text colour, background colour, attr,
--  repeated as necessary.
-- The chunk table length is therefore always a multiple of 4.
-- lineno (0-based) is used to highlight the current selection
--
-- Note: this is rather legacy code...
--  Code clarity and ease of maintenance easily trumps raw performance here: even if we assume
--  a screenfull of text is 10K characters and takes 1M clocks to process, on a 1GHz chip that's
--  still fifty times faster than we'd need to achieve 50 FPS. So we are free to do all the things
--  we're trying to avoid on the larger scale, in particular breaking the text up into word-sized
--  chunks (we /might/ even get away with 10K API calls, but let's not even try that).
--
integer ch,                 -- current character
        bracket_level,      -- as per syncolor.e
        bl,                 -- BraceLevel[newSyntax]
        syntaxClass

atom bgclr = ColourTab[Background],
     cmclr = ColourTab[Comments],
     hlclr = ColourTab[Highlight]
--if match("tiles",text) then ?text end if

--DEV??
--  isCurrLineColoured = (ColourTab[HighLine]!=bgclr)

--sequence res
--integer cM4
-- 15/4/2010:
--atom syntaxClass
--integer syntaxClass

integer k
--integer current_len

integer fromX, fromY, toX, toY

--sequence fWord
--integer fDiff

integer resetBackSlash, wasSlash, wasDot

integer EuSq -- euphoria/phix style single quote handling

--integer allNumbers

--integer lc
--trace(1)
--if chovline!=0 then trace(1) end if
--if match("mov",text) then trace(1) end if
    lt = length(text)
--  lc = length(comment)
--  if not lt then return {} end if     18/7
    chunkMax = 0
    if not lt then
--DEV any point?
        getBrktCfwd(lineno)
        setBrktCfwd(lineno,startblockcomment,startlevel,startlang)
        return {}
    end if
--DEV make these permanent (for speed)... integer maxlt
    if length(backC)<lt then
        backC = repeat(bgclr,lt)
        textC = repeat(ColourTab[Other],lt) -- was Background 22/6/05
--      attrC = repeat(EA_Normal,lt)
--      attrC = repeat(CD_PLAIN,lt)
        attrC = repeat(TG_NORMAL,lt)
-- 29/04/2010 (replaced with else clause):
--  end if
----DEV machine exceptions (2.4)
----    backC[1..length(backC)]=bgclr
--  for i=1 to length(backC) do
--      backC[i] = bgclr
--  end for
    else
        for i=1 to lt do
            backC[i] = bgclr
        end for
    end if

--DEV if marginRqd then
    if marginWidth then
        textC[1..marginWidth] = firstch
        ch = text[marginWidth-isMarginSpace]
--      if ch>='0' and ch<='9' then
        if ch>='0' and ch<='>' then
--          attrC[1..marginWidth] = 4   -- EA_Normal
            attrC[1..marginWidth] = StyleTab[Linenos]
--      else
--          attrC[1..marginWidth] = 1   -- EA_Bold
        elsif ch='*' then
            attrC[1..marginWidth] = StyleTab[BookMarks]
--      elsif ch='>' then
--          attrC[1..marginWidth] = StyleTab[Linenos]
        end if
        backC[1..marginWidth] = ColourTab[Marginbg]
--      if backC[1]=White then
--          backC[1..marginWidth] = Black
--      else
--          backC[1..marginWidth] = White
--      end if
        if text[marginWidth-isMarginSpace]=' ' then
            text[marginWidth]='_'
            textC[1..marginWidth] = backC[1]
        end if
        chidx = marginWidth+1
    else
        chidx = 1
    end if
--if SynNames[newSyntax]="css" and lineno=131 then trace(1) end if

--  chidx = 1
--  chidx = from
    getBrktCfwd(lineno)
--if lineno=16 then trace(1) end if
    bracket_level = startlevel
    bcomm = startblockcomment
    resetBackSlash = 0
    bl = BraceLevels[newSyntax]
--  bl = 7
    while chidx<=lt do
        ch = text[chidx]
        if ch>128 then
            ctype = TokenChar
        else
            ctype = charMap[ch+1]
        end if
        chidx2 = chidx+1
-- if block comment start detected:
--      if length(blockComment) and bcomm=0 then
--      if length(blockComment) then
        if length(blockComment) and bcomm>=0 then
--          for bci=1 to length(blockComment) by 2 do
--DEV see above(bci)
            for bci=1 to length(blockComment) do
                abc = blockComment[bci]
                abcl = length(abc)
                if ch=abc[1]
                and (abcl<2 or (chidx2<=lt and text[chidx2]=abc[2]))
                and (abcl<3 or (chidx2<lt and text[chidx2+1]=abc[3]))
                and (abcl<4 or (chidx2+1<lt and text[chidx2+2]=abc[4])) then
--                  bcomm = bci
                    if remainder(bci,2) then
                        bcomm += 1
                    else
                        bcomm -= 1
                    end if
                    syntaxClass = Comments
                    ctype = Comment
                    chidx2 += abcl-1
                    exit
                end if
            end for
        end if
        if bcomm<0 then
            -- look for multi-line string end
            ctype = String
        end if
        if bcomm>0 then
            -- if in block comment scan for close block comment
--DEV crash here, bcomm was 1, blockComment was {}. [after unrecog. section]
--          abc = blockComment[bcomm+1]
            abc = blockComment[2] -- (signals NOT line comment scan)
--removed 8/7/19 (for pascal { } comments)
--          chidx2 -= 1
            scanForUrls(text)
            syntaxClass = Comments
        elsif find(ctype,{TokenStart,TokenFirst,TokenLast}) then
            while chidx2<=length(text) do
                if ctype=TokenLast then exit end if
                ch = text[chidx2]
                if ch>128 then
                    ctype = TokenChar
                else
                    ctype = charMap[ch+1]
                end if
                if ctype>TokenChar then exit end if
                chidx2 += 1
            end while
            word = text[chidx..chidx2-1]
            syntaxClass = Other
            if newSyntax!=1 then
--/*
if isEu then
--DEV include here
--DEV do we (really need to) do this or store wordlists from .syn files xlated?
-- 15/07/2013
--  if chovline=lineno
--  and chovfrom+marginWidth=chidx
--  and chovto+marginWidth=chidx2-1 then
--      syntaxClass = URLs
--  if sequence(FtoK(word)) then
--  els
    if not equal(word,FtoK(word)) then
--      syntaxClass = Other+bl+1
        syntaxClass = Other+8   --DEV ReservedWords
--      syntaxClass = BookMarks+8   --DEV ReservedWords
--  elsif sequence(FtoB(word)) then
    elsif not equal(word,FtoB(word)) then
--      syntaxClass = Other+bl+4
        syntaxClass = Other+11  --DEV Builtins
--      syntaxClass = BookMarks+11  --DEV Builtins
    end if
end if
--*/

if syntaxClass=Other and not resetBackSlash then
--              for i=Other+bl+1 to length(Sections[newSyntax]) do
--              for i=Other+8 to length(Sections[newSyntax]) do
                for i=BookMarks+8 to length(Sections[newSyntax]) do
--                  if find(word,WordLists[newSyntax][i-Other-bl]) then
--                  if find(word,WordLists[newSyntax][i-Other-7]) then
                    if find(word,WordLists[newSyntax][i-BookMarks-7]) then
                        syntaxClass = i
                        if equal(lower(word),"include")
                        and resetBackSlash=0 then
                            resetBackSlash = 1
                            wasSlash = charMap['\\'+1]
                            charMap['\\'+1] = Delimiter
                            wasDot = charMap['.'+1]
                            charMap['.'+1] = Delimiter
                        end if
                        exit
                    end if
                end for
end if
--if syntaxClass=Other then
--  k = find("Types",Sections[newSyntax])   --DEV Globals/set when newSyntax is set.
--  if k then
----        if length(call_func(r_findGlobal,{word})) then  --DEV way too slow! (use a hash table!)
--      if 1=0 then
--          syntaxClass = k
--      elsif find(word,routines[currfile][rtnNAMES]) then
--          k = find("Types",Sections[newSyntax])   --DEV Locals/set when newSyntax is set.
--          if k then
--              syntaxClass = k
--          end if
--      end if
--  end if
--end if
            end if
            if syntaxClass=Other and chidx2<length(text) then
                ch2 = text[chidx2]
                if (ch2=':' and find(word,WordLists[1][1]))
                or (ch2='.' and equal(word,"www")) then
--                  syntaxClass = URLs
                    chidx2 += 1
                    ch2 = text[chidx2]
                    if ch2='\"' then
-- Added 9/3/07:
--=========
--          for i=chidx+1 to chidx2 do
--              ch2 = text[i]
--              if ch2=':' or ch='.' then
--                  word = text[chidx+1..i-1]
--                  if find(word,WordLists[1][1])
--                  or equal(word,"www") then
--                      textC[chidx+1..chidx2-1] = ColourTab[URLs]
--                      attrC[chidx+1..chidx2-1] = StyleTab[URLs]
--                  end if
--                  exit
--              end if          
--          end for
                        chidx2 += 1
                        while chidx2<=length(text) do
                            if text[chidx2]='\"' then
                                syntaxClass = URLs
                                chidx2 += 1
                                exit
                            end if
                            chidx2 += 1
                        end while
--=========
                    else
                        while chidx2<=length(text) do
--24/8/23:
--                          if urlChar[text[chidx2]+1]!=TokenChar then exit end if
                            ch2 = text[chidx2]
                            if ch2>255 or urlChar[ch2+1]!=TokenChar then exit end if
                            syntaxClass = URLs
                            chidx2 += 1
                        end while
                    end if
                end if
            end if
-- 02/08/2013:
            if newSyntax!=1
--          and isEu            -- (broken anyway 5/10/16)
            and chovline=lineno
            and chovfrom+marginWidth=chidx
            and chovto+marginWidth=chidx2-1 then
                if syntaxClass=Other
--              or syntaxClass>=27 then -- erm...   (library routines/4/win32lib/constants)
                or syntaxClass>=24 then -- erm...
                    chhovprevsnyclr = syntaxClass -- 24/25/26 --> builtin routines/4/constants
                    syntaxClass = URLs
                else
                    chovline = 0
                end if
            end if
        elsif ctype=TokenChar
--17/8/18
--         or (ch='.' and chidx2<=length(text) and text[chidx2]>='0' and text[chidx2]<='9') then
           or (ch='.' and chidx2<=length(text) and text[chidx2]>='0' and text[chidx2]<='9' and bcomm=0) then
            integer allNumbers = (ch>='0' and ch<='9') or ch='.'
            while chidx2<=length(text) do
                ch = text[chidx2]
--              if charMap[ch+1]!=TokenChar then
                if ch<128 and charMap[ch+1]!=TokenChar then
                    if not allNumbers then exit end if
                    if ch!='.' then exit end if
                    allNumbers = false
                    if chidx2=length(text) then exit end if
                    ch = text[chidx2+1]
                    if ch<'0' or ch>'9' then exit end if
                    chidx2 += 1
                end if
                if ch<'0' or ch>'9' then
                    allNumbers = false
                end if
                chidx2 += 1
            end while
            syntaxClass = Other
        elsif ctype<=CloseBrace then
            if ctype=OpenBrace then
                bracket_level += 1
                if bracket_level>length(closeRqd) then
                    closeRqd &= 0
                end if
                if bracket_level>0 then
                    closeRqd[bracket_level] = ch+1+(ch!='(')
                end if
            else
                if bracket_level<1 or ch!=closeRqd[bracket_level] then
                    backC[chidx] = ColourTab[Illegals]
                end if
            end if
            integer wbl = bracket_level
            while wbl<1 do
                wbl += bl
            end while
            while wbl>bl do
                wbl -= bl
            end while
--          syntaxClass = Other+wbl
            syntaxClass = BookMarks+wbl
            if ctype=CloseBrace then
                bracket_level -= 1
            end if
--10/07/20 (ugh)
--      elsif lc and ch=comment[1]
--        and bcomm = 0 -- added 25/3/2013
--        and (lc<2 or (chidx2<=length(text) and text[chidx2]=comment[2]))
--        and (lc<3 or (chidx2<length(text) and text[chidx2+1]=comment[3])) then
        elsif bcomm=0
          and ((length(lineComments)>=1 and ch=lineComments[1][1]
                and (length(lineComments[1])<2 or (chidx2<=length(text) and text[chidx2]=lineComments[1][2]))
                and (length(lineComments[1])<3 or (chidx2<length(text) and text[chidx2+1]=lineComments[1][3]))) or
               (length(lineComments)>=2 and ch=lineComments[2][1]
                and (length(lineComments[2])<2 or (chidx2<=length(text) and text[chidx2]=lineComments[2][2]))
                and (length(lineComments[2])<3 or (chidx2<length(text) and text[chidx2+1]=lineComments[2][3]))) or
               (length(lineComments)>=3 and ch=lineComments[3][1]
                and (length(lineComments[3])<2 or (chidx2<=length(text) and text[chidx2]=lineComments[3][2]))
                and (length(lineComments[3])<3 or (chidx2<length(text) and text[chidx2+1]=lineComments[3][3])))) then
--        and begins(lineComments,text,chidx) then
--NO!         and match_any(lineComments,text,chidx) then
--/*
                for bci,bc in blockComment do
                    if begins(bc,line,chidx) then
                for c in lineComments do
                    cfound = begins(c,line,chidx)
---*/
            abc = ""  -- (signals line comment scan)
            scanForUrls(text)
            syntaxClass = Comments
--      elsif length(lineComments)>3 then
----    lineComments = {`;`,`@`,`//`}
--          ?9/0 -- placeholder for more code (four line comments?!?!)
--23/12/16:
        elsif bcomm=0 
--        and lineno=1  -- relaxed for htmilse code
          and ch='#'
          and chidx=marginWidth+1
          and length(text)>=chidx2
          and text[chidx2]='!' then
            chidx2=lt+1
            syntaxClass = Comments

        elsif ctype=String then
            chidx2 = chidx
            syntaxClass = Strings
            -- Euphoria/Phix has special single quote handling --DEV .syn file option
--          EuSq = (ch='\'' and equal(SynNames[newSyntax],"Euphoria"))
            EuSq = (ch='\'' and equal(SynNames[newSyntax],"Phix"))
--DEV
if bcomm<0 then
    if bcomm=-1 then
        ch = '`'
    elsif bcomm=-2 then
        ch = '"'
    else -- -3
        ch = '\''
    end if
    chidx2 -= 1
else
    if ch='`' then
        bcomm = -1
    elsif ch='"'
      and chidx<=length(text)-2
      and equal(text[chidx..chidx+2],`"""`) then
        bcomm = -2
        chidx2 += 2
    end if
end if
            while 1 do
                if chidx2=length(text) then
if bcomm=0 then
--27/12/20
        if SynNames[newSyntax]="css" and 0 then
?9/0
        else
                    -- using > and < to signify start/end, on a line such as
                    --   >puts(1,"hello <
                    -- rather than colour the final space red, do the 'o'
                    while chidx2>chidx do
                        chidx2 -= 1
                        if text[chidx2]!=' ' then exit end if
                    end while
                    syntaxClass = Illegals
        end if
end if
                    exit
                end if
                chidx2 += 1
                ch2 = text[chidx2]
                if ch2=ch then      -- closing quote found
-- 30/11/09 makes ''' legal on all files...
--                  if EuSq and ch2='\'' and chidx2=chidx+1 then    -- '' is illegal (but "" is not)
if bcomm=-2 then
    -- we're really looking for `"""`
    if chidx2<=length(text)-2 and equal(text[chidx2..chidx2+2],`"""`) then
--  if chidx2<=length(text)-1 and equal(text[chidx2..chidx2+1],`""`) then
        chidx2 += 2
        bcomm = 0
        exit
    end if
else
    bcomm = 0
                    if ch2='\'' and chidx2=chidx+1 then -- '' is illegal (but "" is not)
                        if chidx2<length(text) and text[chidx2+1]='\'' then
                            -- but let ''' through (fine on Phix, wrong for RDS Eu)
                            chidx2 += 1
-- 16/02/10
--                      else
                        elsif EuSq then
                            syntaxClass = Illegals
                        end if
                    end if
                    exit
end if
                elsif EuSq and ch='\'' and chidx2!=chidx+1 then     -- and not about to check 1st char.
                    chidx2 -= 1
                    syntaxClass = Illegals
                    exit
                elsif length(Escapes[newSyntax]) 
                and text[chidx2]=EscapeLeadIns[newSyntax]
                and bcomm=0 then
                    if chidx2>=length(text) then
-- 27/12/20
    if SynNames[newSyntax]="css" and ch='\'' and ch2='\\' then
                        bcomm = -3
    else
                        syntaxClass = Illegals
    end if
                        exit
                    end if
                    ch2 = text[chidx2+1]
--17/9/2020:
--                  if not find(ch2,Escapes[newSyntax]) then
                    if not find(ch2,Escapes[newSyntax])
                    or (EuSq and ch='\'' and find(ch2,"uU")) then
                        syntaxClass = Illegals
                        exit
                    end if
                    if ch2='#' or ch2='x' then
                        if chidx2>=length(text)-2
                        or not find(text[chidx2+2],"0123456789ABCDEFabcdef")
                        or not find(text[chidx2+3],"0123456789ABCDEFabcdef") then
                            syntaxClass = Illegals
                            exit
                        end if
                        chidx2 += 3
                    else
                        chidx2 += 1
                    end if
                end if
            end while
            textC[chidx..chidx2] = ColourTab[Strings]
            attrC[chidx..chidx2] = StyleTab[Strings]
--          if syntaxClass=Illegals then
--              backC[chidx2] = ColourTab[Illegals]
--              syntaxClass = Strings
--          end if
            chidx = chidx2
            chidx2 += 1
        elsif ctype=Operator then
--dev/sug
--          bool bProbableRegex = false
--  if we're on a /, it's not phix/asm/..., there is another / later on,
--  and our / is first non-whitespace or following one of "=,", then as one.
--  eg   const IDENT_RE = /[a-zA-Z]\w*/;
            while 1 do
                if chidx2>length(text) 
                or text[chidx2]>128
                or charMap[text[chidx2]+1]!=Operator then
                    -- match longest legal operator.
--DEV. I think this is because if a<-1 gets coloured wrong without it.
--DEV remainder should be a recognised unary op.
--  eg if a<*5 then ??
                    while chidx2>chidx+1
                    and not find(text[chidx..chidx2-1],OperatorSets[newSyntax]) do
                        chidx2 -= 1
                    end while
                    exit
                end if
                chidx2 += 1
            end while
            word = text[chidx..chidx2-1]
            if find(word,OperatorSets[newSyntax]) then  syntaxClass = Operators
            else                                        syntaxClass = Illegals
            end if
        elsif ctype=Delimiter then                      syntaxClass = Other
        elsif ctype=Illegal and newSyntax!=1 then       syntaxClass = Background
--      elsif ctype=Illegal and newSyntax!=1 then       syntaxClass = Other -- 08/05/07 (never tested)
            backC[chidx] = ColourTab[Illegals]
        elsif ctype=Whitespace then                     syntaxClass = Background
--          backC[chidx] = bgclr -- already set
-- 15/07/2013:
            while chidx2<=length(text) do
                ch = text[chidx2]
                if ch>=128 or charMap[ch+1]!=Whitespace then exit end if
                chidx2 += 1
            end while
        elsif ctype!=Comment then                       syntaxClass = Other
        end if
--27/1/18:
--if 0 then
----DEV test added 22/2/16...
-- if chidx2-1<=lt then
--      textC[chidx..chidx2-1] = ColourTab[syntaxClass]
--      attrC[chidx..chidx2-1] = StyleTab[syntaxClass]
-- end if
--else
        if chidx2-1>lt then
            chidx2=lt+1
        end if
        textC[chidx..chidx2-1] = ColourTab[syntaxClass]
        attrC[chidx..chidx2-1] = StyleTab[syntaxClass]
--end if

        chidx = chidx2
    end while
    if resetBackSlash then
        charMap['\\'+1] = wasSlash
        charMap['.'+1] = wasDot
    end if
    setBrktCfwd(lineno,bcomm,bracket_level,startlang)
--                                          ^^^ DEV
    --
    -- Now apply any selection to the line
    --
    if selON then
        fromX = selX+1+marginRqd    toX = CursorX+1+marginRqd
        fromY = selY+1              toY = CursorY+1
        if selON=2 then --column Mode
            if fromY>toY then
                fromY = toY  toY = selY+1
            end if
            if fromX>toX then
                fromX = toX  toX = selX+1+marginRqd
            end if
        elsif fromY>toY or (fromY=toY and fromX>toX) then   -- swap if rqd
            fromX = toX  toX = selX+1+marginRqd
            fromY = toY  toY = selY+1
        end if
        if fromY<=lineno and toY>=lineno then
            if selON=1 and fromY!=lineno then
                fromX = 1                           -- to sol if rqd
            end if
            if selON=1 and toY!=lineno then
                toX = lt+1                          -- to eol if rqd
            end if
            if selON!=2 or fromX<=length(backC) then
                fromX = curb(1+marginRqd,fromX,length(backC)+1)
                toX = curb(fromX,toX,length(backC)+1)
--              backC[fromX..toX-1] = hlclr
                for i=fromX to toX-1 do                             -- 08/05/07
                    if backC[i]=textC[i] then
                        textC[i] = hlclr
                    end if
                    backC[i] = hlclr
                end for
            end if
        end if
    end if

--  if acfile=currfile
--  and lineno = accY+1 then
--?text
--?acpkey
--?equal(text[length(text)-length(acpkey)+1..length(text)],acpkey)
--      trace(1)
--  end if

    acrest = ""
    if acfile=currfile
    and lineno = accY+1
    and CursorY = accY
    and MapToByte(filetext[currfile][lineno],CursorX)=0
    and length(text)>=length(acpkey)
    and equal(text[length(text)-length(acpkey)+1..length(text)],acpkey) then
        acrest = acdata1[length(acpkey)+1..length(acdata1)]
        text &= acrest
        k = lt
        lt = length(text)
        if length(backC)<lt then
--DEV this looks horribly wrong!!! (1/7/16!!)
            backC &= repeat(bgclr,lt-length(backC))
            textC &= repeat(cmclr,lt-length(backC))
--          attrC &= repeat(EA_Italic,lt-length(backC))
            attrC &= repeat(CD_ITALIC,lt-length(backC))
        end if
--DEV colour/attr of autocompletes should be configurable
        for i=k+1 to lt do
            backC[i] = bgclr
            textC[i] = cmclr
--          attrC[i] = EA_Italic
            attrC[i] = CD_ITALIC
        end for
    end if


    --
    -- Merge any blocks
    --
    k = 1
--trace(1)
    while k<=lt do  -- PL changed from < 23/4/07 (left trailing space as sep. chunk)
        if backC[k]=textC[k] then   -- "invisible" characters only
--      if backC[k]=textC[k]         -- "invisible" characters only
--      and find(text[k]," \t\r\n") then    -- For Jesus Consuegra 5/6/7
            if k<lt and backC[k]=backC[k+1] and backC[k+1]!=textC[k+1]
            and (attrC[k]=5)=(attrC[k+1]=5) then
                textC[k] = textC[k+1]
                attrC[k] = attrC[k+1]
                if k>1 then
                    k -= 2
                end if
            elsif k>1 and backC[k]=backC[k-1] and backC[k-1]!=textC[k-1]
            and (attrC[k]=5)=(attrC[k-1]=5) then
                textC[k] = textC[k-1]
                attrC[k] = attrC[k-1]
                k -= 2
            end if
        end if
        k += 1
    end while
    --
    -- finally build a block list
    --
    atom chunk_bg = backC[1],
         chunk_fg = textC[1],
         ch_style = attrC[1]
--  current_len = 0
    sequence res = {}
--  chunkMax = 0
    integer startpos = 1
    for i=1 to lt+1 do
        if i>lt
        or chunk_bg!=backC[i] 
        or chunk_fg!=textC[i]
        or ch_style!=attrC[i] then
--          res &= {current_len,chunk_fg,chunk_bg,ch_style}
--          chunkMax += 4
--          if length(res)<chunkMax then
--              res &= repeat(0,32)
--          end if
--          res[chunkMax-3] = current_len
--          res[chunkMax-2] = chunk_fg
--          res[chunkMax-1] = chunk_bg
--          res[chunkMax  ] = ch_style
--          current_len = 0
            sequence chunk = text[startpos..i-1]
            if chunk_bg!=bgclr then
                if ch_style=TG_NORMAL then
                    chunk = {chunk,{chunk_fg,chunk_bg}}
                else
                    chunk = {chunk,{chunk_fg,chunk_bg},ch_style}
                end if
            elsif chunk_fg!=TG_BLACK
               or ch_style!=TG_NORMAL then
                if ch_style=TG_NORMAL then
                    chunk = {chunk,chunk_fg}
                else
                    chunk = {chunk,chunk_fg,ch_style}
                end if
            end if
--          res &= {,clr,ch_style
            res = append(res,chunk)
            if i>lt then exit end if
            chunk_bg = backC[i]
            chunk_fg = textC[i]
            ch_style = attrC[i]
            startpos = i
        end if
--      current_len += 1
    end for
--  if current_len then
----        res &= {current_len,chunk_fg,chunk_bg,ch_style}
--      chunkMax += 4
--      if length(res)<chunkMax then
--          res &= repeat(0,32)
--      end if
--      res[chunkMax-3] = current_len
--      res[chunkMax-2] = chunk_fg
--      res[chunkMax-1] = chunk_bg
--      res[chunkMax  ] = ch_style
--  end if
    return res
--  return res[1..chunkMax]
end function
--<<<

object bcfwd_dbg
global procedure dump_snyclr_innards()
    -- Diagnostic. Invoke (perhaps) with mid-session ini_save().
    -- Then when I notice something wrong, snapshot, page up/down 
    -- correct, and tools/crash should then let me figure out 
    -- where it was going wrong, he says hopefully....
    -- Might need to be a little more involved than just this:
    bcfwd_dbg = deep_copy(bCfwd)
end procedure

-- TESTING:
constant B1 = TG_BLUE,
         B2 = TG_MAROON,
         B3 = TG_TEAL,
         OP = TG_BLUE,
         RW = TG_TEAL, -- reserved words, eg "end"
--       BIN = #A86070,     -- eg deep_copy
         BIN = #7060A8,     -- eg deep_copy
--       LIB = #5E005E,     -- Darker, eg printf?
         STR = TG_DARK_GREEN,
         TYPE = #004080,
         WITH = #2C5C2C,
         CONST = #004600

-- Qu: how does a bCfwd generated by hammering through this (and starting with {})
--     compare with one generated by rebuild_bcfwd()?

constant {fake,fres} = columnize({
    {"#!/bin/bash",
     {{"#!/bin/bash",TG_NAVY,TG_ITALIC}}},
    {"#[",
     {{"#[",TG_NAVY,TG_ITALIC}}},
    {"  echo Phix ignores all text between #[ and #] in exactly the same way as /* and */ ",
     {{"  echo Phix ignores all text between #[ and #] in exactly the same way as /* and */ ",TG_NAVY,TG_ITALIC}}},
    {`  echo (both "and" are nested comments), allowing arbitrary shell code, for example:`,
     {{`  echo (both "and" are nested comments), allowing arbitrary shell code, for example:`,TG_NAVY,TG_ITALIC}}},
    {"  cd /user/project/working",
     {{"  cd /user/project/working",TG_NAVY,TG_ITALIC}}},
    {`  exec /path/to/phix "$0" "$@"`,
     {{`  exec /path/to/phix "$0" "$@"`,TG_NAVY,TG_ITALIC}}},
    {"  exit # may be needed for the shell to ignore the rest of this file.",
     {{"  exit # may be needed for the shell to ignore the rest of this file.",TG_NAVY,TG_ITALIC}}},
    {"# comments(/shebang) ignored by Phix end here -> #]",
     {{"# comments(/shebang) ignored by Phix end here -> #]",TG_NAVY,TG_ITALIC}}},
    {"-- aside: few files have [multiline] shebangs like above, but this *is* a test file.",
     {{"-- aside: few files have [multiline] shebangs like above, but this *is* a test file.",TG_NAVY,TG_ITALIC}}},
    {"/*  ",
     {{"/*  ",TG_NAVY,TG_ITALIC}}},
    {"    Standard",
     {{"    Standard",TG_NAVY,TG_ITALIC}}},
    {"    /*",
     {{"    /*",TG_NAVY,TG_ITALIC}}},
    {"    **  Nested",
     {{"    **  Nested",TG_NAVY,TG_ITALIC}}},
    {"    **  /*",
     {{"    **  /*",TG_NAVY,TG_ITALIC}}},
    {"    **      Block",
     {{"    **      Block",TG_NAVY,TG_ITALIC}}},
    {"    *//*    /*",
     {{"    *//*    /*",TG_NAVY,TG_ITALIC}}},
    {"    **          Comments, of which Phix has quite a few [ok, six] varieties...",
     {{"    **          Comments, of which Phix has quite a few [ok, six] varieties...",TG_NAVY,TG_ITALIC}}},
    {"    **      */",
     {{"    **      */",TG_NAVY,TG_ITALIC}}},
    {"    *****/",
     {{"    *****/",TG_NAVY,TG_ITALIC}}},
    {"    **/",
     {{"    **/",TG_NAVY,TG_ITALIC}}},
    {"*/",
     {{"*/",TG_NAVY,TG_ITALIC}}},
    {"--/*",
     {{"--/*",TG_NAVY,TG_ITALIC}}},
    {"    Euphoria-compatibility-style nestable multiline comments/code",
     {{"    Euphoria-compatibility-style nestable multiline comments/code",TG_NAVY,TG_ITALIC}}},
    {"--*/",
     {{"--*/",TG_NAVY,TG_ITALIC}}},
    {"// This program should be runnable (C-style line comments, btw, too!)",
     {{"// This program should be runnable (C-style line comments, btw, too!)",TG_NAVY,TG_ITALIC}}},
    {"with javascript_semantics",
     {{"with javascript_semantics",WITH}}},
    {`string pe, te := iff(platform()==JS?"JavaScript/browser":"Phix/desktop")`,
     {{"string",TYPE}," pe",{",",OP}," te",{" :=",OP},{" iff",BIN},{"(",B1},{"platform",BIN},{"()",B2},
      {"==",OP},{"JS",CONST},{"?",OP},{`"JavaScript/browser"`,STR},{":",OP},
      {`"Phix/desktop"`,STR},{")",B1}}},
    {"ifdef PHIX then",
     {{"ifdef",RW}," PHIX",{" then",RW}}},
    {`    pe = "Phix"`,
     {"    pe",{" =",OP},{` "Phix"`,STR}}},
    {"elsedef",
     {{"elsedef",RW}}},
    {`    pe = "Euphoria" -- not that this file is compatible with that..`,
     {"    pe",{" =",OP},{` "Euphoria"`,STR},{" -- not that this file is compatible with that..",TG_NAVY,TG_ITALIC}}},
    {"end ifdef",
     {{"end ifdef",RW}}},
    {`printf(1,"This is %s (%s)\n",{pe,te}); -- the ; is entirely optional`,
     {{"printf",BIN},{"(",B1},"1",{",",OP},{`"This is %s (%s)\n"`,STR},{",",OP},{"{",B2},"pe",{`,`,OP},"te",{"}",B2},{`);`,B1},
       {" -- the ; is entirely optional",TG_NAVY,TG_ITALIC}}},
    {`assert("\n\r\b\t\\\"\'\0\e"=join({"",10,13,8,9,#5C,#22,#27,#00,#1B},""))`,
     {{"assert",BIN},{"(",B1},{`"\n\r\b\t\\\"\'\0\e"`,STR},{"=",OP},{"join",BIN},{`(`,B2},{"{",B3},{`""`,STR},{",",OP},
       "10",{",",OP},"13",{",",OP},"8",{",",OP},"9",{",",OP},"#5C",{",",OP},"#22",{",",OP},"#27",{",",OP},"#00",{",",OP},
       "#1B",{"}",B3},{",",OP},{`""`,STR},{")",B2},{")",B1}}},
    {`assert("\#42\x43\u4445\U00105678"="B"&'C'&"\xE4\x91\x85\xF4\x85\x99\xB8")`,
     {{"assert",BIN},{"(",B1},{`"\#42\x43\u4445\U00105678"`,STR},{"=",OP},{`"B"`,STR},{"&",OP},{"'C'",STR},{"&",OP},{`"\xE4\x91\x85\xF4\x85\x99\xB8"`,STR},{")",B1}}},
    {"",""},
    {"-- triplequote, triplequote indented, backtick, and doublequote strings:",
     {{"-- triplequote, triplequote indented, backtick, and doublequote strings:",TG_NAVY,TG_ITALIC}}},
    {`constant tqs = """`,
     {{"constant",RW}," tqs",{" =",OP},{` """`,STR}}},
    {`this`,
     {{`this`,STR}}},
    {`string\thing""",`,
     {{`string\thing"""`,STR},{",",OP}}},
    {"",""},
    {`tqis = """`,
     {"tqis",{" =",OP},{` """`,STR}}},
    {`_____this`,
     {{"_____this",STR}}},
    {`     string\thing""",`,
     {{`     string\thing"""`,STR},{",",OP}}},
    {"",""},
    {"bts = `this",
     {"bts",{" =",OP},{" `this",STR}}},
    {"string\\thing`,",
     {{"string\\thing`",STR},{",",OP}}},
    {"",""},
    {`dqs = "this\nstring\\thing"`,
     {"dqs",{" =",OP},{` "this\nstring\\thing"`,STR}}},
    {`assert(tqs==tqis and tqis==bts and bts==dqs and dqs==tqs)`,
     {{"assert",BIN},{"(",B1},"tqs",{"==",OP},"tqis",{" and",RW}," tqis",{"==",OP},"bts",{" and",RW},
       " bts",{"==",OP},"dqs",{" and",RW}," dqs",{"==",OP},"tqs",{")",B1}}},
    {`assert(x"1 2 34 5678_AbC"=={0x01, 0x02, 0x34, 0x56, 0x78, 0xAB, 0x0C})`,
     {{"assert",BIN},{"(",B1},"x",{`"1 2 34 5678_AbC"`,STR},{"==",OP},{"{",B2},
       "0x01",{",",OP}," 0x02",{",",OP}," 0x34",{",",OP}," 0x56",{",",OP}," 0x78",{",",OP}," 0xAB",{",",OP}," 0x0C",
       {"}",B2},{")",B1}}},
    {"",""},
    {"-- Beyond comments and strings, not much in Phix bothers a syntax colourer.",
     {{"-- Beyond comments and strings, not much in Phix bothers a syntax colourer.",TG_NAVY,TG_ITALIC}}},
    {"-- Originally I had another 380+ lines in here, that didn't prove anything.",
     {{"-- Originally I had another 380+ lines in here, that didn't prove anything.",TG_NAVY,TG_ITALIC}}},
    {"",""},
    {"function merge_sort(sequence x)",
     {{"function",RW}," merge_sort",{"(",B1},{"sequence",TYPE}," x",{")",B1}}},
    {"    -- put x into ascending order using a recursive merge sort",
     {{"    -- put x into ascending order using a recursive merge sort",TG_NAVY,TG_ITALIC}}},
    {"    if length(x)<=1 then",
     {{"    if",RW},{" length",BIN},{"(",B1},"x",{")<=",B1},"1",{" then",RW}}},
    {"        return x  -- trivial case",
     {{"        return",RW}," x ",{" -- trivial case",TG_NAVY,TG_ITALIC}}},
    {"    end if",
     {{"    end if",RW}}},
    {"    integer midpoint = floor(length(x)/2)",
     {{"    integer",TYPE}," midpoint",{" =",OP},{" floor",BIN},{"(",B1},{"length",BIN},{"(",B2},"x",{")",B2},{"/",OP},"2",{")",B1}}},
    {"    sequence merged = {},",
--   {{"    sequence ",TYPE},"merged ",{"= ",OP},{"{}",B1},{",",OP}}},
     {{"    sequence",TYPE}," merged",{" = {},",OP}}},
    {"         first_half = merge_sort(x[1..midpoint]),",
     {"         first_half",{" =",OP}," merge_sort",{"(",B1},"x",{"[",B2},"1",{"..",OP},"midpoint",{"]",B2},{"),",OP}}},
    {"        second_half = merge_sort(x[midpoint+1..$])",
     {"        second_half",{" =",OP}," merge_sort",{"(",B1},"x",{"[",B2},"midpoint",{"+",OP},"1",{"..",OP},"$",{"]",B2},{")",B1}}},
    {"    -- merge the two sorted halves into one",
     {{"    -- merge the two sorted halves into one",TG_NAVY,TG_ITALIC}}},
    {"    while length(first_half)>0",
     {{"    while",RW},{" length",BIN},{"(",B1},"first_half",{")>",OP},"0"}},
    {"      and length(second_half)>0 do",
     {{"      and",RW},{" length",BIN},{"(",B1},"second_half",{")>",OP},"0",{" do",RW}}},
    {"        if first_half[1]<=second_half[1] then",
     {{"        if",RW}," first_half",{"[",B1},"1",{"]<=",OP},"second_half",{"[",B1},"1",{"]",B1},{" then",RW}}},
    {"            merged = append(merged, first_half[1])",
     {"            merged",{" =",OP},{" append",BIN},{"(",B1},"merged",{",",OP}," first_half",{"[",B2},"1",{"]",B2},{")",B1}}},
    {"            first_half = first_half[2..$]",
     {"            first_half",{" =",OP}," first_half",{"[",B1},"2",{"..",OP},"$",{"]",B1}}},
    {"        else",
     {{"        else",RW}}},
    {"            merged = append(merged, second_half[1])",
     {"            merged",{" =",OP},{" append",BIN},{"(",B1},"merged",{",",OP}," second_half",{"[",B2},"1",{"]",B2},{")",B1}}},
    {"            second_half = second_half[2..$]",
     {"            second_half",{" =",OP}," second_half",{"[",B1},"2",{"..",OP},"$",{"]",B1}}},
    {"        end if",
     {{"        end if",RW}}},
    {"    end while",
     {{"    end while",RW}}},
    {"    -- result is the merged data plus any leftovers",
     {{"    -- result is the merged data plus any leftovers",TG_NAVY,TG_ITALIC}}},
    {"    return merged & first_half & second_half",
     {{"    return",RW}," merged",{" &",OP}," first_half",{" &",OP}," second_half"}},
    {"end function",
     {{"end function",RW}}},
    {"",""},
    {"?merge_sort({9, 10, 3, 1, 4, 5, 8, 7, 6, 2})",
     {{"?",OP},"merge_sort",{"(",B1},{"{",B2},"9",{",",OP}," 10",{",",OP}," 3",{",",OP}," 1",{",",OP},
      " 4",{",",OP}," 5",{",",OP}," 8",{",",OP}," 7",{",",OP}," 6",{",",OP}," 2",{"}",B2},{")",B1}}},
    {"?{1,0(2)10,0b11,0d4,0t5,0(6)10,0(7)10,0o10,0x9,#A} -- [the same 1..10 but with extra weirdness]",
     {{"?{",OP},"1",{",",OP},"0",{"(",B2},"2",{")",B2},"10",{",",OP},"0b11",{",",OP},"0d4",
      {",",OP},"0t5",{",",OP},"0",{"(",B2},"6",{")",B2},"10",{",",OP},"0",{"(",B2},"7",{")",B2},"10",
      {",",OP},"0o10",{",",OP},"0x9",{",",OP},"#A",{"}",B1},{" -- [the same 1..10 but with extra weirdness]",TG_NAVY,TG_ITALIC}}},
--Also perfectly acceptable, that is when "0(" /can/ be properly completed (using lookahead?): [whole thing red when not?]
--   {{"?{",OP},"1",{",",OP},"0(2)10",{",",OP},"0b11",{",",OP},"0d4",
--    {",",OP},"0t5",{",",OP},"0(6)10",{",",OP},"0(7)10",
--    {",",OP},"0o10",{",",OP},"0x9",{",",OP},"#A",{"} ",B1},{"-- [the same 1..10 but with extra weirdness]",TG_NAVY,TG_ITALIC}}},
    {"",""},
    {"wait_key()",
     {{"wait_key",BIN},{"()",B1}}},
    {"",""},
    {"-- this gets syntax-coloured by theEditor on dektop/Phix, but not Edita/Edix or pygments/highlightjs,",
     {{"-- this gets syntax-coloured by theEditor on dektop/Phix, but not Edita/Edix or pygments/highlightjs,",TG_NAVY,TG_ITALIC}}},
    {`--  inline assembly simply probably isn't ever going to work in any browser-based environment anyway:`,
     {{`--  inline assembly simply probably isn't ever going to work in any browser-based environment anyway:`,TG_NAVY,TG_ITALIC}}},
    {`--  (when I say "probably", I have not yet ruled out there one day being a #ilASM{ [JS] } kludge)`,
     {{`--  (when I say "probably", I have not yet ruled out there one day being a #ilASM{ [JS] } kludge)`,TG_NAVY,TG_ITALIC}}},
    {`--  (first step is to stop colouring mov in phix.syn, also note no 32|64 distinction in fasm.syn)`,
     {{`--  (first step is to stop colouring mov in phix.syn, also note no 32|64 distinction in fasm.syn)`,TG_NAVY,TG_ITALIC}}},
    {"",""},
    {`--integer mov`,
     {{`--integer mov`,TG_NAVY,TG_ITALIC}}},
    {"#ilASM{",
     {"#",{"ilASM",RW},{"{",B1}}},
    {"        [ARM]",
     {{"        [",B2},{"ARM",CONST},{"]",B2}}},
    {"            mov r0,1",
     {{"            mov",BIN}," r0",{",",OP},"1"}},
    {"        [32]",
     {{"        [",B2},"32",{"]",B2}}},
    {"            mov al,0",
     {{"            mov",BIN}," al",{",",OP},"0"}},
    {"        [64]",
     {{"        [",B2},"64",{"]",B2}}},
    {"            mov rax,0",
     {{"            mov",BIN}," rax",{",",OP},"0"}},
    {"        []",
     {{"        []",B2}}},
    {"            ret",
     {{"            ret",BIN}}},
    {"      }",
     {{"      }",B1}}}})

-->>>>>
--{{"        [",8388608},{"ARM",-1},{"]",255}}
--{{"        [",8388608},{"ARM",17920},{"]",8388608}}
--{{"        [",8388608},{"ARM",#4600},{"]",8388608}}
--error on line 92
--{{"            mov",32896}," r0",{",",255},"1"}
--{{"            mov",7364776}," r0",{",",255},"1"}
--error on line 93
--{{"            mov",-1}," al",{",",255},"0"}
--{{"            mov",7364776}," al",{",",255},"0"}
--{{"            mov",-1}," rax",{",",255},"0"}
--{{"            mov",7364776}," rax",{",",255},"0"}
--error on line 97
--{{"            ret",-1}}
--{{"            ret",7364776}}
--error on line 99
--<<<<<

global function fake_colour(string txt, integer line)
    if currfile=4 then
--  integer k = find(txt,fake)
--  if k then 
--?currfile
        string fl = fake[line]
--  txt = `?{1,0(2)10,0b11,0d4,0t5,0(6)10,0(7)10,0o10,0x9,#A} -- [the same 1..10 but with extra weirdness]`
        assert(txt==fl)
--?{line,txt==fl,txt}
--      return fres[k]
--if line=1 then
--  ?txt
--if line=33 then trace(1) end if
        sequence fr = fres[line],
                 sc = syntaxColour(txt,line,0,0)
        if fr!=sc then 
            ?fr
            ?sc
            printf(1,"error on line %d\n",line)
        end if
--  if fr!=sc then printf(1,"error on line %d\n",line) end if
--  "#!/bin/bash"
--   12345467890
--  {{"#!/bin/bash",128,2}}
--     12345678901
--  {11,0,14737632,0}
--- also perfectly acceptable: {s,e,c,t} = {{1,10,...}}[i]; chunk = txt[s..e]... [but why...]
--end if
--      return fr
        return sc
    end if
--?txt
    return txt
end function

--OLD:
--  bcfwd is the bracket(/comment/nested_syntax) carry-forward array.
--  After say "constant sequence s = {\n", the next line needs to be
--  syntax-coloured with a "}" of outstanding brackets to be closed.
--  Likewise after "/*\n" lines are comments, with "*/" outstanding,
--  and "#ilASM{\n", "<?php", "<script>", and "<style>" are used to
--  switch over to a nested syntax until the approprate closer (the
--  first phix-only, with eg [64/ARM], the latter three html-only).
--
--  Each element of bcfwd[file] is {line,comment,level,clsrqd,lang}
--  where

-- Aside: Edtia/Edix have B_level, but I think we can use length(""). [DEV - yep, we can]

--constant B_line=1, B_blockcomment=2, B_level=3, B_closesrqd=4, B_lang=5
--         bcf_line  bcf_block  length(startCfwd) bcf_close_rqd
-- each element is {line, blockcomment, level, sequence}, where
-- line is the _first_ line this carry forward occured on,
-- blockcomment is 1 if a block comment carries over the end of this line,
-- level is the number of carry forward brackets (may be -ve),
-- sequence is {} for level <=0, else chunk of closeRqd.
--  Note that closeRqd is a lifo stack, hence after ([,
--  closeRqd will be ")]", not "])".

--/*
C:\Program Files (x86)\Phix\demo\edix\edix.exw:5653 in function crash_cb()
attempt to divide by 0
 (^^^) call_back from OperatingSystem/sharedlib/asm
... called from C:\Program Files (x86)\Phix\demo\pGUI\pGUI.e:1992 in procedure IupMainLoop()
... called from C:\Program Files (x86)\Phix\demo\edix\edix.exw:8770 in procedure main()
    configname = `edix2`
... called from C:\Program Files (x86)\Phix\demo\edix\edix.exw:8777

Global & Local Variables

 C:\Program Files (x86)\Phix\demo\edix\edix.exw:
    filenames[1..2] = {`edix.cfg`,`edix.030919.cfg`}
    filenames[3] = `window.ew`,
    filenames[4..9] = {`toc.txt`,`index.txt`,`Euphoria.syn`,`Phix.syn`,`Phix.txt`,`Phix.txt`}
    filenames[10..17] = {`phix7zip.lst`,`parse.js`,`js.syn`,`blog.txt`,`helloworld.s`,`affichage.inc`,`test.py`,`fortune.exw`}
    filepaths[1..3] = {`C:\Users\Pete\`,`C:\Users\Pete\`,`C:\Program Files (x86)\Phix\demo\wee\`}
    filepaths[4..5] = {`C:\Program Files (x86)\Phix\docs\phix\src\`,`C:\Program Files (x86)\Phix\docs\phix\src\`}
    filepaths[6..7] = {`C:\Program Files (x86)\Phix\demo\edita\syn\`,`C:\Program Files (x86)\Phix\demo\edix\syn\`}
    filepaths[8..9] = {`C:\Program Files (x86)\Phix\demo\edix\help\`,`C:\Program Files (x86)\Phix\demo\edita\help\`}
    filepaths[10..11] = {`C:\Program Files (x86)\Phix\sfx\`,`C:\Program Files (x86)\Phix\pwa\js\`}
    filepaths[12..14] = {`C:\Program Files (x86)\Phix\demo\edix\syn\`,`C:\Program Files (x86)\Phix\`,`E:\downloads\misc\wren\qemu\putty\`}
    filepaths[15..17] = {`E:\downloads\misc\wren\qemu\putty\`,`C:\Program Files (x86)\Phix\`,`C:\Program Files (x86)\Phix\`}
    filetext[1][1..11] = {`dit=dit`,``,`[ExtDialog]`,`Height=348`,`MaxiSized=0`,`Maximized=0`,`Width=712`,`X=1041`,`Y=242`,``,`[Extensions]`}
    filetext[1][514] = `filepath99=C:\Program Files (x86)\Phix\demo\rosetta\Pierpont_primes.exw`
    filetext[2][1..10] = {``,`[ExtDialog]`,`Height=348`,`MaxiSized=0`,`Maximized=0`,`Width=712`,`X=1041`,`Y=242`,``,`[Extensions]`}
    filetext[2][518] = `filepath99=C:\Program Files (x86)\Phix\demo\misc\mpfr\Mpfr312Mpir260\MpfrMpir\Dynamic\Mpfr\gmp.h`
    filetext[3][1..8] = {`-- window.ew`,``,`--/*`,`include std/error.e`,`include std/machine.e`,`include std/dll.e`,`--*/`,``}
    filetext[3][9..12] = {`global function or_all(sequence s)`,`-- or together all elements of a sequence`,"\tatom result","\t"}
    filetext[3][13..17] = {"\tresult = 0","\tfor i = 1 to length(s) do","\t\tresult = or_bits(result, s[i])","\tend for","\treturn result"}
    filetext[3][18..23] = {`end function`,``,`global function HIWORD(atom x)`,"\treturn floor(x / 65536) + 65536*(x < 0)",`end function`,``}
    filetext[3][24..28] = {`global function LOWORD(atom x)`,"\treturn and_bits(x, #FFFF)",`end function`,``,`-- bytes must be a power of 2`}
    filetext[3][29..31] = {`function align(atom ptr, integer bytes)`,"\tinteger result","\tresult = and_bits(ptr, bytes - 1)"}
    filetext[3][32..37] = {"\tif result then","\t\tptr += bytes - result","\tend if","\treturn ptr",`end function`,``}
    filetext[3][38..40] = {`-- strings ------------------------------------------------------------------`,`sequence strings`,`strings = {}`}
    filetext[3][41..43] = {``,`-- allocate string that can be garbage collected later`,`global function alloc_string(object str)`}
    filetext[3][44..48] = {"\tatom ptr","\tif atom(str) then","\t\treturn str","\tend if","\tptr = allocate(length(str)+1)"}
    filetext[3][49..53] = {"\tstrings = append(strings, ptr)","\tpoke(ptr, str)","\tpoke(ptr + length(str), 0)","\treturn ptr",`end function`}
    filetext[3][54..57] = {``,`-- garbage collect strings`,`global procedure free_strings()`,"\tfor i = 1 to length(strings) do"}
    filetext[3][58..63] = {"\t\tfree(strings[i])","\tend for","\tstrings = {}",`end procedure`,``,`--/*`}
    filetext[3][64..69] = {`global function peek_string(atom a)`,"\tinteger i","\tsequence s","\ts = \"\"","\tif a then","\t\ti = peek(a)"}
    filetext[3][70..75] = {"\t\twhile i do","\t\t\ts = append(s, i)","\t\t\ta = a + 1","\t\t\ti = peek(a)","\t\tend while","\tend if"}
    filetext[3][76..80] = {"\treturn s",`end function`,`--*/`,``,`procedure pokeWideCharString(atom ptr, sequence str)`}
    filetext[3][81..83] = {"\tfor i = 1 to length(str) by 2 do","\t\tif i < length(str) then","\t\t\tpoke4(ptr, str[i] + str[i+1]*#10000)"}
    filetext[3][84..90] = {"\t\telse","\t\t\tpoke4(ptr, str[i])","\t\t\treturn","\t\tend if","\t\tptr += 4","\tend for","\tpoke(ptr, {0, 0})"}
    filetext[3][91..94] = {`end procedure`,``,`global function allocWideCharString(sequence s)`,"\tatom ptr"}
    filetext[3][95..97] = {"\tptr = allocate(length(s) * 2 + 2)","\tstrings = append(strings, ptr)","\tpokeWideCharString(ptr, s)"}
    filetext[3][98..103] = {"\treturn ptr",`end function`,``,`global function peekWideCharString(atom ptr)`,"\tsequence result","\tinteger ch"}
    filetext[3][104..109] = {"\tif ptr = 0 then","\t\treturn 0","\tend if","\tresult = \"\"","\tch = peek2u(ptr)","\twhile ch do"}
    filetext[3][110..117] = {"\t\tresult &= ch","\t\tptr += 2","\t\tch = peek2u(ptr)","\tend while","\treturn result",`end function`,``,``}
    filetext[3][118..119] = {`-- structs ------------------------------------------------------------------`,``}
    filetext[3][120..123] = {`-- pack/unpack pattern characters`,`-- "d" dword  4 bytes`,`-- "w" word   2 bytes`,`-- "b" byte   1 byte`}
    filetext[3][124..127] = {"-- \"i\" int\t  4 bytes",`-- "c" char   1 byte`,"-- \"q\" qword/int64\t8 bytes",`-- "p" pointer 4 or 8 bytes`}
    filetext[3][128..129] = {`-- "s" string pointer  4 or 8 bytes (alloc_string'd)`,`-- "m" multibyte string pointer`}
    filetext[3][130] = `-- "z" dword  4 bytes, size of struct, must be first in pattern`
    filetext[3][131..132] = {`-- "$" aligns the total struct size to the size of the largest member`,``}
    filetext[3][133] = `-- When converting Windows struct member types, this may be useful:`
    filetext[3][134..135] = {`-- https://msdn.microsoft.com/en-us/library/windows/desktop/aa383751(v=vs.85).aspx`,``}
    filetext[3][136..139] = {`function w_sizeof(sequence pattern)`,"\tinteger size, x, max_align","\tsize = 0","\tmax_align = 1"}
    filetext[3][140..144] = {"\tfor i = 1 to length(pattern) do","\t\tx = pattern[i]","\t\tif x = \'b\' ","\t\tor x = \'c\' then","\t\t\tx = 1"}
    filetext[3][145..149] = {"\t\telsif x = \'w\' then","\t\t\tx = 2","\t\telsif x = \'d\' ","\t\t   or x = \'i\' ","\t\t   or x = \'z\' then"}
    filetext[3][150..154] = {"\t\t\tx = 4","\t\telsif x = \'q\' then","\t\t\tx = 8","\t\telsif x = \'p\' ","\t\t   or x = \'s\' "}
    filetext[3][155..158] = {"\t\t   or x = \'m\' then","--/**/\t\tx = machine_word()\t\t--/*","\t\t\tifdef BITS64 then","\t\t\t\tx = 8"}
    filetext[3][159..162] = {"\t\t\telsedef","\t\t\t\tx = 4","\t\t\tend ifdef\t\t\t\t--*/","\t\telsif x = \'$\' then"}
    filetext[3][163..165] = {"\t\t\tsize = align(size, max_align)","\t\t\texit","\t\telse"}
    filetext[3][166..169] = {"\t\t\tprintf(1, \"unknown pack char %s\\n\", {{x}})","\t\t\tx = 0","\t\tend if","\t\tsize = align(size, x) + x"}
    filetext[3][170..176] = {"\t\tif x > max_align then","\t\t\tmax_align = x","\t\tend if","\tend for","\treturn size",`end function`,``}
    filetext[3][177..181] = {`atom pptr`,`global procedure pack(atom ptr, sequence pattern, sequence data)`,"\tobject tmp","\tinteger x",``}
    filetext[3][182..185] = {"\tfor i = 1 to length(pattern) do","\t\tx = pattern[i]","\t\ttmp = data[i]","\t\tif x = \'s\' then"}
    filetext[3][186..188] = {"\t\t\ttmp = alloc_string(tmp)","\t\telsif x = \'m\' then","\t\t\ttmp = allocWideCharString(tmp)"}
    filetext[3][189..193] = {"\t\telsif sequence(tmp) then","\t\t\ttmp = or_all(tmp)","\t\tend if","\t\tif x = \'b\' ","\t\tor x = \'c\' then"}
    filetext[3][194..197] = {"\t\t\tpoke(ptr, tmp)","\t\t\tptr += 1","\t\telsif x = \'w\' then","\t\t\tptr = align(ptr, 2)"}
    filetext[3][198..202] = {"\t\t\tpoke2(ptr, tmp)","\t\t\tptr += 2","\t\telsif x = \'d\' ","\t\t   or x = \'i\' ","\t\t   or x = \'z\' then"}
    filetext[3][203..206] = {"\t\t\tptr = align(ptr, 4)","\t\t\tpoke4(ptr, tmp)","\t\t\tptr += 4","\t\telsif x = \'q\' then"}
    filetext[3][207..209] = {"--/**/\t\tif machine_bits()=64 then","--/**/\t\t\tptr = align(ptr, 8)","--/**/\t\t\tpoke8(ptr, tmp)"}
    filetext[3][210..211] = {"--/**/\t\telse","--/**/\t\t\tpoke4(ptr, and_bits(tmp, #FFFFFFFF))"}
    filetext[3][212..215] = {"--/**/\t\t\tpoke4(ptr + 4, floor(tmp/#100000000))","--/**/\t\tend if",`--/*`,"\t\t\tifdef BITS64 then"}
    filetext[3][216..218] = {"\t\t\t\tptr = align(ptr, 8)","\t\t\t\tpoke8(ptr, tmp)","\t\t\telsedef"}
    filetext[3][219..221] = {"\t\t\t\tpoke4(ptr, and_bits(tmp, #FFFFFFFF))","\t\t\t\tpoke4(ptr + 4, floor(tmp/#100000000))","\t\t\tend ifdef"}
    filetext[3][222..226] = {`--*/`,"\t\t\tptr += 8","\t\telsif x = \'p\' ","\t\t   or x = \'s\' ","\t\t   or x = \'m\' then"}
    filetext[3][227..229] = {"--/**/\t\tif machine_bits()=64 then","--/**/\t\t\tptr = align(ptr, 8)","--/**/\t\t\tpoke8(ptr, tmp)"}
    filetext[3][230..233] = {"--/**/\t\t\tptr += 8","--/**/\t\telse","--/**/\t\t\tptr = align(ptr, 4)","--/**/\t\t\tpoke4(ptr, tmp)"}
    filetext[3][234..238] = {"--/**/\t\t\tptr += 4","--/**/\t\tend if",`--/*`,"\t\t\tifdef BITS64 then","\t\t\t\tptr = align(ptr, 8)"}
    filetext[3][239..242] = {"\t\t\t\tpoke8(ptr, tmp)","\t\t\t\tptr += 8","\t\t\telsedef","\t\t\t\tptr = align(ptr, 4)"}
    filetext[3][243..248] = {"\t\t\t\tpoke4(ptr, tmp)","\t\t\t\tptr += 4","\t\t\tend ifdef",`--*/`,"\t\telsif x = \'$\' then","\t\telse"}
    filetext[3][249..254] = {"\t\t\tprintf(1, \"unknown pack char %s\\n\", {{x}})","\t\tend if","\tend for","\tpptr = ptr",`end procedure`,``}
    filetext[3][255..257] = {`global function allocate_pack(sequence pattern, sequence data)`,"\tatom ptr","\tinteger size"}
    filetext[3][258..261] = {"\tsize = w_sizeof(pattern)","\tif pattern[1] = \'z\' then","\t\tdata = prepend(data, size)","\tend if"}
    filetext[3][262..265] = {"\tif pattern[$] = \'$\' then","\t\tdata = append(data, 0)","\tend if","\tptr = allocate(size)"}
    filetext[3][266..267] = {"\t--printf(1, \"ptr=%x pattern=%s size=%d\\n\", {ptr, pattern, size})","\tpack(ptr, pattern, data)"}
    filetext[3][268..269] = {"\tif pptr - ptr != size then","\t\tprintf(1, \"overflow! ptr=%x pattern=%s size=%d\\n\", {ptr, pattern, size})"}
    filetext[3][270..276] = {"\t\t?pptr-ptr","\t\t?machine_bits()","\t\t?data","\tend if","\treturn ptr",`end function`,``}
    filetext[3][277..280] = {`global function unpack(atom ptr, sequence pattern)`,"\tsequence data","\tobject tmp","\tinteger x"}
    filetext[3][281..283] = {"\tdata = repeat(0, length(pattern))","\tfor i = 1 to length(pattern) do","\t\tx = pattern[i]"}
    filetext[3][284..287] = {"\t\tif x = \'b\' then","\t\t\tdata[i] = peek(ptr)","\t\t\tptr += 1","\t\telsif x = \'c\' then"}
    filetext[3][288..291] = {"\t\t\ttmp = peek(ptr)","\t\t\tptr += 1","\t\t\tdata[i] = tmp - 2*and_bits(tmp, #80)","\t\telsif x = \'w\' then"}
    filetext[3][292..295] = {"\t\t\tptr = align(ptr, 2)","\t\t\tdata[i] = peek2u(ptr)","\t\t\tptr += 2","\t\telsif x = \'d\' or x = \'z\' then"}
    filetext[3][296..299] = {"\t\t\tptr = align(ptr, 4)","\t\t\tdata[i] = peek4u(ptr)","\t\t\tptr += 4","\t\telsif x = \'i\' then"}
    filetext[3][300..302] = {"\t\t\tptr = align(ptr, 4)","\t\t\tdata[i] = peek4s(ptr)","\t\t\tptr += 4"}
    filetext[3][303..304] = {"\t\telsif x = \'p\' or x = \'s\' or x = \'m\' then","--/**/\t\tif machine_bits()=64 then"}
    filetext[3][305..308] = {"--/**/\t\t\tptr = align(ptr, 8)","--/**/\t\t\ttmp = peek8u(ptr)","--/**/\t\t\tptr += 8","--/**/\t\telse"}
    filetext[3][309..313] = {"--/**/\t\t\tptr = align(ptr, 4)","--/**/\t\t\ttmp = peek4u(ptr)","--/**/\t\t\tptr += 4","--/**/\t\tend if",`--/*`}
    filetext[3][314..317] = {"\t\t\tifdef BITS64 then","\t\t\t\tptr = align(ptr, 8)","\t\t\t\ttmp = peek8u(ptr)","\t\t\t\tptr += 8"}
    filetext[3][318..322] = {"\t\t\telsedef","\t\t\t\tptr = align(ptr, 4)","\t\t\t\ttmp = peek4u(ptr)","\t\t\t\tptr += 4","\t\t\tend ifdef"}
    filetext[3][323..326] = {`--*/`,"\t\t\tif x = \'s\' then","\t\t\t\ttmp = peek_string(tmp)","\t\t\telsif x = \'m\' then"}
    filetext[3][327..331] = {"\t\t\t\ttmp = peekWideCharString(tmp)","\t\t\tend if","\t\t\tdata[i] = tmp","\t\telsif x = \'$\' then","\t\telse"}
    filetext[3][332..337] = {"\t\t\tprintf(1, \"unknown pack char %s\\n\", {{x}})","\t\tend if","\tend for","\treturn data",`end function`,``}
    filetext[3][338..340] = {`-- dll wrapping -------------------------------------------------------`,``,`constant`}
    filetext[3][341..343] = {"\tuser32 = open_dll(\"user32.dll\"),","\tgdi32 = open_dll(\"gdi32.dll\"),","\twinmm = open_dll(\"winmm.dll\"),"}
    filetext[3][344..346] = {"\tkernel32 = open_dll(\"kernel32.dll\"),","\tshell32 = open_dll(\"shell32.dll\")","\t"}
    filetext[3][347..349] = {`-- get handles to some dll routines`,``,`global constant`}
    filetext[3][350] = "\tLoadIcon = define_c_func(user32, \"LoadIconA\", {C_PTR, C_INT}, C_INT),"
    filetext[3][351] = "\tLoadCursor = define_c_func(user32, \"LoadCursorA\", {C_PTR, C_INT}, C_INT),"
    filetext[3][352] = "\tGetStockObject = define_c_func(gdi32, \"GetStockObject\", {C_INT}, C_INT),"
    filetext[3][353] = "\tDeleteObject = define_c_func(gdi32, \"DeleteObject\", {C_INT}, C_INT),"
    filetext[3][354] = "\tCreateFontIndirect = define_c_func(gdi32, \"CreateFontIndirectA\", {C_PTR}, C_INT),"
    filetext[3][355] = "\tCreateBitmap = define_c_func(gdi32, \"CreateBitmap\", {C_INT, C_INT, C_UINT, C_UINT, C_PTR}, C_INT),"
    filetext[3][356][1..100] = "\tCreateIcon = define_c_func(user32, \"CreateIcon\", {C_PTR, C_INT, C_INT, C_UINT, C_UINT, C_PTR, C_PTR"
    filetext[3][356][101..110] = `}, C_INT),`
    filetext[3][357] = "\tCreateIconIndirect = define_c_func(user32, \"CreateIconIndirect\", {C_PTR}, C_INT),"
    filetext[3][358] = "\tRegisterClassEx = define_c_func(user32, \"RegisterClassExA\", {C_PTR}, C_USHORT),"
    filetext[3][359] = "\tCreateWindowEx = define_c_func(user32, \"CreateWindowExA\", "
    filetext[3][360..361] = {"\t {C_UINT,C_PTR,C_PTR,C_UINT,C_INT,C_INT,C_INT,C_INT,C_UINT,C_UINT,C_UINT,C_PTR},","\t  C_UINT),"}
    filetext[3][362] = "\tCloseWindow = define_c_proc(user32, \"CloseWindow\", {C_PTR}),"
    filetext[3][363] = "\tDestroyWindow = define_c_func(user32, \"DestroyWindow\", {C_PTR}, C_INT),"
    filetext[3][364] = "\tMoveWindow = define_c_proc(user32, \"MoveWindow\", {C_PTR, C_INT, C_INT, C_INT, C_LONG, C_INT}),"
    filetext[3][365] = "\tShowWindow = define_c_proc(user32, \"ShowWindow\", {C_PTR, C_INT}),"
    filetext[3][366] = "\tUpdateWindow = define_c_proc(user32, \"UpdateWindow\", {C_PTR}),"
    filetext[3][367] = "\tRegisterWindowMessage = define_c_func(user32, \"RegisterWindowMessageA\", {C_PTR}, C_LONG),"
    filetext[3][368] = "\tPlaySound = define_c_proc(winmm, \"PlaySound\", {C_INT, C_INT, C_INT}),"
    filetext[3][369] = "\tBeginPaint = define_c_func(user32, \"BeginPaint\", {C_INT, C_PTR}, C_INT),"
    filetext[3][370] = "\tGetClientRect = define_c_proc(user32, \"GetClientRect\", {C_INT, C_PTR}),"
    filetext[3][371] = "\tGetWindowRect = define_c_proc(user32, \"GetWindowRect\", {C_INT, C_PTR}),"
    filetext[3][372] = "\tInvalidateRect = define_c_proc(user32, \"InvalidateRect\", {C_INT, C_PTR, C_INT}),"
    filetext[3][373] = "\tDrawText = define_c_proc(user32, \"DrawTextA\", {C_INT, C_INT, C_INT, C_INT, C_INT}),"
    filetext[3][374] = "\tEndPaint = define_c_proc(user32, \"EndPaint\", {C_INT, C_INT}),"
    filetext[3][375] = "\tPostQuitMessage = define_c_proc(user32, \"PostQuitMessage\", {C_PTR}),"
    filetext[3][376..377] = {"\tDefWindowProc = define_c_func(user32, \"DefWindowProcA\", {C_PTR, C_INT, C_PTR, C_PTR}, C_INT),",``}
    filetext[3][378] = "\tSetFocus = define_c_func(user32, \"SetFocus\", {C_LONG}, C_LONG),"
    filetext[3][379] = "\tGetFocus = define_c_func(user32, \"GetFocus\", {}, C_LONG),"
    filetext[3][380] = "\tGetActiveWindow = define_c_func(user32, \"GetActiveWindow\", {}, C_LONG),"
    filetext[3][381] = "\tEnableWindow = define_c_func(user32, \"EnableWindow\", {C_LONG, C_LONG}, C_LONG),"
    filetext[3][382] = "\tIsWindowEnabled = define_c_func(user32, \"IsWindowEnabled\", {C_LONG}, C_LONG),"
    filetext[3][383] = "\tGetScrollPos = define_c_func(user32, \"GetScrollPos\", {C_INT, C_INT}, C_INT),"
    filetext[3][384] = "\tSetScrollPos = define_c_func(user32, \"SetScrollPos\", {C_INT, C_INT, C_INT, C_INT}, C_INT),"
    filetext[3][385] = "\tGetKeyState = define_c_func(user32, \"GetKeyState\", {C_INT}, C_INT),"
    filetext[3][386] = "\tGetCursorPos = define_c_func(user32, \"GetCursorPos\", {C_PTR}, C_INT),"
    filetext[3][387..388] = {"\tScreenToClient = define_c_func(user32, \"ScreenToClient\", {C_LONG, C_PTR}, C_INT),",``}
    filetext[3][389] = "\tLoadLibrary = define_c_func(kernel32, \"LoadLibraryA\", {C_PTR}, C_LONG),"
    filetext[3][390] = "\tGetModuleHandle = define_c_func(kernel32, \"GetModuleHandleA\", {C_PTR}, C_INT),"
    filetext[3][391] = "\tGetLastError = define_c_func(kernel32, \"GetLastError\", {}, C_UINT),"
    filetext[3][392] = "\tShellExecute = define_c_func(shell32, \"ShellExecuteA\", {C_INT,C_INT,C_INT,C_INT,C_INT,C_INT}, C_INT),"
    filetext[3][393] = "\tSHGetFolderPath = define_c_func(shell32, \"SHGetFolderPathA\", {C_INT, C_INT, C_INT, C_LONG, C_PTR}, C_INT),"
    filetext[3][394] = "\tDragAcceptFiles = define_c_proc(shell32, \"DragAcceptFiles\", {C_INT, C_INT}),"
    filetext[3][395] = "\tDragFinish = define_c_proc(shell32, \"DragFinish\", {C_PTR}),"
    filetext[3][396] = "\tDragQueryFile = define_c_func(shell32, \"DragQueryFile\", {C_PTR, C_UINT, C_PTR, C_UINT}, C_UINT),"
    filetext[3][397..399] = {"\tDragQueryPoint = define_c_func(shell32, \"DragQueryPoint\", {C_PTR, C_PTR}, C_INT)",``,`global constant`}
    filetext[3][400..404] = {"\tCW_USEDEFAULT = #80000000,","\tMAX_PATH = #104,","\tCSIDL_LOCAL_APPDATA = 28,","\tCSIDL_FLAG_CREATE = #8000",``}
    filetext[3][405..409] = {`-- Class styles`,`global constant`,"\t\tCS_VREDRAW = #1,","\t\tCS_HREDRAW = #2,","\t\tCS_KEYCVTWINDOW = #4,"}
    filetext[3][410..413] = {"\t\tCS_DBLCLKS = #8,","\t\tCS_OWNDC = #20,","\t\tCS_CLASSDC = #40,","\t\tCS_PARENTDC = #80,"}
    filetext[3][414..417] = {"\t\tCS_NOKEYCVT = #100,","\t\tCS_NOCLOSE = #200,","\t\tCS_SAVEBITS = #800,","\t\tCS_BYTEALIGNCLIENT = #1000,"}
    filetext[3][418..422] = {"\t\tCS_BYTEALIGNWINDOW = #2000,","\t\tCS_PUBLICCLASS = #4000",``,`-- ShowWindow() Commands`,`global constant`}
    filetext[3][423..427] = {"\tSW_HIDE = 0,","\tSW_SHOWNORMAL = 1,","\tSW_NORMAL = 1,","\tSW_SHOWMINIMIZED = 2,","\tSW_SHOWMAXIMIZED = 3,"}
    filetext[3][428..432] = {"\tSW_MAXIMIZE = 3,","\tSW_SHOWNOACTIVATE = 4,","\tSW_SHOW = 5,","\tSW_MINIMIZE = 6,","\tSW_SHOWMINNOACTIVE = 7,"}
    filetext[3][433..438] = {"\tSW_SHOWNA = 8,","\tSW_RESTORE = 9,","\tSW_SHOWDEFAULT = 10,","\tSW_MAX = 10",``,`-- Window Messages`}
    filetext[3][439..444] = {`global constant`,"\tWM_NULL = #0,","\tWM_CREATE = #1,","\tWM_DESTROY = #2,","\tWM_MOVE = #3,","\tWM_SIZE = #5,"}
    filetext[3][445..451] = {``,"\tWM_ACTIVATE = #6,",`--`,"--\tWM_ACTIVATE state values",``,"\tWA_INACTIVE = 0,","\tWA_ACTIVE = 1,"}
    filetext[3][452..457] = {"\tWA_CLICKACTIVE = 2,",``,"\tWM_SETFOCUS = #7,","\tWM_KILLFOCUS = #8,","\tWM_ENABLE = #A,","\tWM_SETREDRAW = #B,"}
    filetext[3][458..462] = {"\tWM_SETTEXT = #C,","\tWM_GETTEXT = #D,","\tWM_GETTEXTLENGTH = #E,","\tWM_PAINT = #F,","\tWM_CLOSE = #10,"}
    filetext[3][463..466] = {"\tWM_QUERYENDSESSION = #11,","\tWM_QUIT = #12,","\tWM_QUERYOPEN = #13,","\tWM_ERASEBKGND = #14,"}
    filetext[3][467..470] = {"\tWM_SYSCOLORCHANGE = #15,","\tWM_ENDSESSION = #16,","\tWM_SHOWWINDOW = #18,","\tWM_WININICHANGE = #1A,"}
    filetext[3][471..474] = {"\tWM_DEVMODECHANGE = #1B,","\tWM_ACTIVATEAPP = #1C,","\tWM_FONTCHANGE = #1D,","\tWM_TIMECHANGE = #1E,"}
    filetext[3][475..478] = {"\tWM_CANCELMODE = #1F,","\tWM_SETCURSOR = #20,","\tWM_MOUSEACTIVATE = #21,","\tWM_CHILDACTIVATE = #22,"}
    filetext[3][479..483] = {"\tWM_QUEUESYNC = #23,",``,"\tWM_GETMINMAXINFO = #24",`-- Type MINMAXINFO`,`global constant`}
    filetext[3][484..486] = {"\tMINMAXINFO_ptReserved = 8,","\tMINMAXINFO_ptMaxSize = 16,","\tMINMAXINFO_ptMaxPosition = 24,"}
    filetext[3][487..490] = {"\tMINMAXINFO_ptMinTrackSize = 32,","\tMINMAXINFO_ptMaxTrackSize = 40",``,`global constant`}
    filetext[3][491..494] = {"\tWM_PAINTICON = #26,","\tWM_ICONERASEBKGND = #27,","\tWM_NEXTDLGCTL = #28,","\tWM_SPOOLERSTATUS = #2A,"}
    filetext[3][495..498] = {"\tWM_DRAWITEM = #2B,","\tWM_MEASUREITEM = #2C,","\tWM_DELETEITEM = #2D,","\tWM_VKEYTOITEM = #2E,"}
    filetext[3][499..502] = {"\tWM_CHARTOITEM = #2F,","\tWM_SETFONT = #30,","\tWM_GETFONT = #31,","\tWM_SETHOTKEY = #32,"}
    filetext[3][503..507] = {"\tWM_GETHOTKEY = #33,","\tWM_QUERYDRAGICON = #37,","\tWM_COMPAREITEM = #39,","\tWM_COMPACTING = #41,",``}
    filetext[3][508..512] = {"\tWM_WINDOWPOSCHANGING = #46,","\tWM_WINDOWPOSCHANGED = #47,",``,"\tWM_POWER = #48,",`--`}
    filetext[3][513..516] = {"--\twParam for WM_POWER window message and DRV_POWER driver notification",``,"\tPWR_OK = 1,","\tPWR_FAIL = (-1),"}
    filetext[3][517..521] = {"\tPWR_SUSPENDREQUEST = 1,","\tPWR_SUSPENDRESUME = 2,","\tPWR_CRITICALRESUME = 3,",``,"\tWM_COPYDATA = #4A,"}
    filetext[3][522..525] = {"\tWM_CANCELJOURNAL = #4B,",``,`-- Type COPYDATASTRUCT`,"\tCOPYDATASTRUCT_dwData = 4,"}
    filetext[3][526..530] = {"\tCOPYDATASTRUCT_cbData = 8,","\tCOPYDATASTRUCT_lpData = 12,",``,"\tWM_NOTIFY = #4E,",``}
    filetext[3][531..533] = {"\tWM_SETICON = #80, ICON_BIG = 1, ICON_SMALL = 0,","\tWM_NCCREATE = #81,","\tWM_NCDESTROY = #82,"}
    filetext[3][534..537] = {"\tWM_NCCALCSIZE = #83,","\tWM_NCHITTEST = #84,","\tWM_NCPAINT = #85,","\tWM_NCACTIVATE = #86,"}
    filetext[3][538..541] = {"\tWM_GETDLGCODE = #87,","\tWM_NCMOUSEMOVE = #A0,","\tWM_NCLBUTTONDOWN = #A1,","\tWM_NCLBUTTONUP = #A2,"}
    filetext[3][542..545] = {"\tWM_NCLBUTTONDBLCLK = #A3,","\tWM_NCRBUTTONDOWN = #A4,","\tWM_NCRBUTTONUP = #A5,","\tWM_NCRBUTTONDBLCLK = #A6,"}
    filetext[3][546..550] = {"\tWM_NCMBUTTONDOWN = #A7,","\tWM_NCMBUTTONUP = #A8,","\tWM_NCMBUTTONDBLCLK = #A9,",``,"\tWM_KEYFIRST = #100,"}
    filetext[3][551..555] = {"\tWM_KEYDOWN = #100,","\tWM_KEYUP = #101,","\tWM_CHAR = #102,","\tWM_DEADCHAR = #103,","\tWM_SYSKEYDOWN = #104,"}
    filetext[3][556..559] = {"\tWM_SYSKEYUP = #105,","\tWM_SYSCHAR = #106,","\tWM_SYSDEADCHAR = #107,","\tWM_KEYLAST = #108,"}
    filetext[3][560..563] = {"\tWM_INITDIALOG = #110,","\tWM_COMMAND = #111,","\tWM_SYSCOMMAND = #112,","\tWM_TIMER = #113,"}
    filetext[3][564..567] = {"\tWM_HSCROLL = #114,","\tWM_VSCROLL = #115,","\tWM_INITMENU = #116,","\tWM_INITMENUPOPUP = #117,"}
    filetext[3][568..572] = {"\tWM_MENUSELECT = #11F,","\tWM_MENUCHAR = #120,","\tWM_ENTERIDLE = #121,",``,"\tWM_CTLCOLORMSGBOX = #132,"}
    filetext[3][573..576] = {"\tWM_CTLCOLOREDIT = #133,","\tWM_CTLCOLORLISTBOX = #134,","\tWM_CTLCOLORBTN = #135,","\tWM_CTLCOLORDLG = #136,"}
    filetext[3][577..580] = {"\tWM_CTLCOLORSCROLLBAR = #137,","\tWM_CTLCOLORSTATIC = #138,",``,"\tWM_MOUSEFIRST = #200,"}
    filetext[3][581..584] = {"\tWM_MOUSEMOVE = #200,","\tWM_LBUTTONDOWN = #201,","\tWM_LBUTTONUP = #202,","\tWM_LBUTTONDBLCLK = #203,"}
    filetext[3][585..588] = {"\tWM_RBUTTONDOWN = #204,","\tWM_RBUTTONUP = #205,","\tWM_RBUTTONDBLCLK = #206,","\tWM_MBUTTONDOWN = #207,"}
    filetext[3][589..592] = {"\tWM_MBUTTONUP = #208,","\tWM_MBUTTONDBLCLK = #209,","\tWM_MOUSELAST = #209,","\tWM_MOUSEWHEEL = #20A,"}
    filetext[3][593..597] = {"\tWM_MOUSEHWHEEL = #20E,",``,"\tWM_PARENTNOTIFY = #210,","\tWM_ENTERMENULOOP = #211,","\tWM_EXITMENULOOP = #212,"}
    filetext[3][598..601] = {"\tWM_MDICREATE = #220,","\tWM_MDIDESTROY = #221,","\tWM_MDIACTIVATE = #222,","\tWM_MDIRESTORE = #223,"}
    filetext[3][602..605] = {"\tWM_MDINEXT = #224,","\tWM_MDIMAXIMIZE = #225,","\tWM_MDITILE = #226,","\tWM_MDICASCADE = #227,"}
    filetext[3][606..609] = {"\tWM_MDIICONARRANGE = #228,","\tWM_MDIGETACTIVE = #229,","\tWM_MDISETMENU = #230,","\tWM_DROPFILES = #233,"}
    filetext[3][610..615] = {"\tWM_MDIREFRESHMENU = #234,",``,"\tWM_CUT = #300,","\tWM_COPY = #301,","\tWM_PASTE = #302,","\tWM_CLEAR = #303,"}
    filetext[3][616..619] = {"\tWM_UNDO = #304,","\tWM_RENDERFORMAT = #305,","\tWM_RENDERALLFORMATS = #306,","\tWM_DESTROYCLIPBOARD = #307,"}
    filetext[3][620..622] = {"\tWM_DRAWCLIPBOARD = #308,","\tWM_PAINTCLIPBOARD = #309,","\tWM_VSCROLLCLIPBOARD = #30A,"}
    filetext[3][623..625] = {"\tWM_SIZECLIPBOARD = #30B,","\tWM_ASKCBFORMATNAME = #30C,","\tWM_CHANGECBCHAIN = #30D,"}
    filetext[3][626..628] = {"\tWM_HSCROLLCLIPBOARD = #30E,","\tWM_QUERYNEWPALETTE = #30F,","\tWM_PALETTEISCHANGING = #310,"}
    filetext[3][629..634] = {"\tWM_PALETTECHANGED = #311,","\tWM_HOTKEY = #312,",``,"\tWM_THEMECHANGED = #31A,",``,"\tWM_PENWINFIRST = #380,"}
    filetext[3][635..638] = {"\tWM_PENWINLAST = #38F,",``,`-- NOTE: All Message Numbers below 0x0400 are RESERVED.`,``}
    filetext[3][639..643] = {`-- Private Window Messages Start Here:`,"\tWM_USER = #400",``,``,`global constant`}
    filetext[3][644..647] = {"\t\tSND_FILENAME = #00020000,","\t\tSND_ASYNC\t = #00000001","\t\t ",`global constant`}
    filetext[3][648..653] = {"\t\tDT_SINGLELINE = #0020,","\t\tDT_CENTER\t  = #0001,","\t\tDT_VCENTER\t  = #0004","\t\t ",``,``}
    filetext[3][654..657] = {`-- Window Styles`,`global constant`,"\t\tWS_OVERLAPPED\t= #00000000,","\t\tWS_POPUP\t\t= #80000000,"}
    filetext[3][658..660] = {"\t\tWS_CHILD\t\t= #40000000,","\t\tWS_MINIMIZE \t= #20000000,","\t\tWS_VISIBLE\t\t= #10000000,"}
    filetext[3][661..663] = {"\t\tWS_DISABLED \t=  #8000000,","\t\tWS_CLIPSIBLINGS =  #4000000,","\t\tWS_CLIPCHILDREN =  #2000000,"}
    filetext[3][664..665] = {"\t\tWS_MAXIMIZE \t=  #1000000,","\t\tWS_CAPTION\t\t=\t#C00000,  --  WS_BORDER Or WS_DLGFRAME"}
    filetext[3][666..668] = {"\t\tWS_BORDER\t\t=\t#800000,","\t\tWS_DLGFRAME \t=\t#400000,","\t\tWS_VSCROLL\t\t=\t#200000,"}
    filetext[3][669..671] = {"\t\tWS_HSCROLL\t\t=\t#100000,","\t\tWS_SYSMENU\t\t=\t #80000,","\t\tWS_THICKFRAME\t=\t #40000,"}
    filetext[3][672..675] = {"\t\tWS_GROUP\t\t=\t #20000,","\t\tWS_TABSTOP\t\t=\t #10000,",``,"\t\tWS_MINIMIZEBOX = #20000,"}
    filetext[3][676..679] = {"\t\tWS_MAXIMIZEBOX = #10000,",``,"\t\tWS_TILED = WS_OVERLAPPED,","\t\tWS_ICONIC = WS_MINIMIZE,"}
    filetext[3][680..681] = {"\t\tWS_SIZEBOX = WS_THICKFRAME,","\t\tWS_OVERLAPPEDWINDOW = or_all({WS_OVERLAPPED, WS_CAPTION, WS_SYSMENU,"}
    filetext[3][682..683] = {"\t\t\t\t\t\t\t\t\t  WS_THICKFRAME, WS_MINIMIZEBOX,","\t\t\t\t\t\t\t\t\t  WS_MAXIMIZEBOX}),"}
    filetext[3][684..687] = {"\t\tWS_TILEDWINDOW = WS_OVERLAPPEDWINDOW",``,"--\t Common Window Styles",`global constant`}
    filetext[3][688..690] = {"\t\tWS_POPUPWINDOW = or_all({WS_POPUP, WS_BORDER, WS_SYSMENU}),","\t\tWS_CHILDWINDOW = (WS_CHILD)",``}
    filetext[3][691..694] = {`-- Extended Window Styles`,`global constant`,"\tWS_EX_DLGMODALFRAME = #1,","\tWS_EX_NOPARENTNOTIFY = #4,"}
    filetext[3][695..698] = {"\tWS_EX_TOPMOST = #8,","\tWS_EX_ACCEPTFILES = #10,","\tWS_EX_TRANSPARENT = #20,","\tWS_EX_MDICHILD = #40,"}
    filetext[3][699..701] = {"\tWS_EX_TOOLWINDOW = #80,","\tWS_EX_WINDOWEDGE = #100,","\tWS_EX_PALETTEWINDOW = #188,"}
    filetext[3][702..704] = {"\tWS_EX_CLIENTEDGE = #200,","\tWS_EX_OVERLAPPEDWINDOW = #300,","\tWS_EX_CONTEXTHELP = #400,"}
    filetext[3][705..707] = {"\tWS_EX_RIGHT = #1000,","\tWS_EX_LEFTSCROLLBAR = #4000,","\tWS_EX_CONTROLPARENT = #10000,"}
    filetext[3][708..714] = {"\tWS_EX_APPWINDOW = #40000",``,``,``,`-- Standard Cursor IDs`,`global constant`,"\t\tIDC_ARROW = 32512,"}
    filetext[3][715..718] = {"\t\tIDC_IBEAM = 32513,","\t\tIDC_WAIT = 32514,","\t\tIDC_CROSS = 32515,","\t\tIDC_UPARROW = 32516,"}
    filetext[3][719..722] = {"\t\tIDC_SIZE = 32640,","\t\tIDC_ICON = 32641,","\t\tIDC_SIZENWSE = 32642,","\t\tIDC_SIZENESW = 32643,"}
    filetext[3][723..726] = {"\t\tIDC_SIZEWE = 32644,","\t\tIDC_SIZENS = 32645,","\t\tIDC_SIZEALL = 32646,","\t\tIDC_NO = 32648,"}
    filetext[3][727..731] = {"\t\tIDC_APPSTARTING = 32650",``,`-- Standard Icon IDs`,`global constant`,"\t\tIDI_APPLICATION = 32512,"}
    filetext[3][732..736] = {"\t\tIDI_HAND = 32513,","\t\tIDI_QUESTION = 32514,","\t\tIDI_EXCLAMATION = 32515,","\t\tIDI_ASTERISK = 32516",``}
    filetext[3][737..742] = {``,`-- Stock Logical Objects`,`global constant`,"\tWHITE_BRUSH = 0,","\tLTGRAY_BRUSH = 1,","\tGRAY_BRUSH = 2,"}
    filetext[3][743..747] = {"\tDKGRAY_BRUSH = 3,","\tBLACK_BRUSH = 4,","\tNULL_BRUSH = 5,","\tHOLLOW_BRUSH = NULL_BRUSH,","\tWHITE_PEN = 6,"}
    filetext[3][748..752] = {"\tBLACK_PEN = 7,","\tNULL_PEN = 8,","\tOEM_FIXED_FONT = 10,","\tANSI_FIXED_FONT = 11,","\tANSI_VAR_FONT = 12,"}
    filetext[3][753..756] = {"\tSYSTEM_FONT = 13,","\tDEVICE_DEFAULT_FONT = 14,","\tDEFAULT_PALETTE = 15,","\tSYSTEM_FIXED_FONT = 16,"}
    filetext[3][757..763] = {"\tSTOCK_LAST = 16",``,``,`global constant`,`-- Dialog Box Command IDs`,"\tIDOK = 1,","\tIDCANCEL = 2,"}
    filetext[3][764..769] = {"\tIDABORT = 3,","\tIDRETRY = 4,","\tIDIGNORE = 5,","\tIDYES = 6,","\tIDNO = 7,",``}
    filetext[3][770..774] = {`-- Control Manager Structures and Definitions`,``,`-- Edit Control Styles`,"\tES_LEFT = #0,","\tES_CENTER = #1,"}
    filetext[3][775..779] = {"\tES_RIGHT = #2,","\tES_MULTILINE = #4,","\tES_UPPERCASE = #8,","\tES_LOWERCASE = #10,","\tES_PASSWORD = #20,"}
    filetext[3][780..783] = {"\tES_AUTOVSCROLL = #40,","\tES_AUTOHSCROLL = #80,","\tES_NOHIDESEL = #100,","\tES_OEMCONVERT = #400,"}
    filetext[3][784..788] = {"\tES_READONLY = #800,","\tES_WANTRETURN = #1000,","\tES_NUMBER = #2000,",``,`-- Edit Control Notification Codes`}
    filetext[3][789..792] = {"\tEN_SETFOCUS = #100,","\tEN_KILLFOCUS = #200,","\tEN_CHANGE = #300,","\tEN_UPDATE = #400,"}
    filetext[3][793..797] = {"\tEN_ERRSPACE = #500,","\tEN_MAXTEXT = #501,","\tEN_HSCROLL = #601,","\tEN_VSCROLL = #602,",``}
    filetext[3][798..802] = {`-- Edit Control Messages`,"\tEM_GETSEL = #B0,","\tEM_SETSEL = #B1,","\tEM_GETRECT = #B2,","\tEM_SETRECT = #B3,"}
    filetext[3][803..806] = {"\tEM_SETRECTNP = #B4,","\tEM_SCROLL = #B5,","\tEM_LINESCROLL = #B6,","\tEM_SCROLLCARET = #B7,"}
    filetext[3][807..810] = {"\tEM_GETMODIFY = #B8,","\tEM_SETMODIFY = #B9,","\tEM_GETLINECOUNT = #BA,","\tEM_LINEINDEX = #BB,"}
    filetext[3][811..814] = {"\tEM_SETHANDLE = #BC,","\tEM_GETHANDLE = #BD,","\tEM_GETTHUMB = #BE,","\tEM_LINELENGTH = #C1,"}
    filetext[3][815..819] = {"\tEM_REPLACESEL = #C2,","\tEM_GETLINE = #C4,","\tEM_LIMITTEXT = #C5,","\tEM_CANUNDO = #C6,","\tEM_UNDO = #C7,"}
    filetext[3][820..823] = {"\tEM_FMTLINES = #C8,","\tEM_LINEFROMCHAR = #C9,","\tEM_SETTABSTOPS = #CB,","\tEM_SETPASSWORDCHAR = #CC,"}
    filetext[3][824..826] = {"\tEM_EMPTYUNDOBUFFER = #CD,","\tEM_GETFIRSTVISIBLELINE = #CE,","\tEM_SETREADONLY = #CF,"}
    filetext[3][827..830] = {"\tEM_SETWORDBREAKPROC = #D0,","\tEM_GETWORDBREAKPROC = #D1,","\tEM_GETPASSWORDCHAR = #D2,",``}
    filetext[3][831..835] = {"\tEM_GETLIMITTEXT = #D5,","\tEM_SETLIMITTEXT = #C5,",``,"\tEM_CANPASTE = 1074,","\tEM_DISPLAYBAND = 1075,"}
    filetext[3][836..839] = {"\tEM_EXGETSEL = 1076,","\tEM_EXLIMITTEXT = 1077,","\tEM_EXLINEFROMCHAR = 1078,","\tEM_EXSETSEL = 1079,"}
    filetext[3][840..843] = {"\tEM_FINDTEXT = 1080,","\tEM_FORMATRANGE = 1081,","\tEM_GETCHARFORMAT = 1082,","\tEM_GETEVENTMASK = 1083,"}
    filetext[3][844..847] = {"\tEM_GETOLEINTERFACE = 1084,","\tEM_GETPARAFORMAT = 1085,","\tEM_GETSELTEXT = 1086,","\tEM_HIDESELECTION = 1087,"}
    filetext[3][848..851] = {"\tEM_PASTESPECIAL = 1088,","\tEM_REQUESTRESIZE = 1089,","\tEM_SELECTIONTYPE = 1090,","\tEM_SETBKGNDCOLOR = 1091,"}
    filetext[3][852..854] = {"\tEM_SETCHARFORMAT = 1092,","\tEM_SETEVENTMASK = 1093,","\tEM_SETOLECALLBACK = 1094,"}
    filetext[3][855..858] = {"\tEM_SETPARAFORMAT = 1095,","\tEM_SETTARGETDEVICE = 1096,","\tEM_STREAMIN = 1097,","\tEM_STREAMOUT = 1098,"}
    filetext[3][859..862] = {"\tEM_GETTEXTRANGE = 1099,","\tEM_FINDWORDBREAK = 1100,","\tEM_SETOPTIONS = 1101,","\tEM_GETOPTIONS = 1102,"}
    filetext[3][863..865] = {"\tEM_SETWORDBREAKPROCEX = 1105,","\tEM_FINDTEXTEX = 1103,","\tEM_GETWORDBREAKPROCEX = 1104,"}
    filetext[3][866..868] = {"\tEM_SETPUNCTUATION = 1124,","\tEM_GETPUNCTUATION = 1125,","\tEM_SETWORDWRAPMODE = 1126,"}
    filetext[3][869..872] = {"\tEM_GETWORDWRAPMODE = 1127,","\tEM_SETIMECOLOR = 1128,","\tEM_GETIMECOLOR = 1129,","\tEM_SETIMEOPTIONS = 1130,"}
    filetext[3][873..877] = {"\tEM_GETIMEOPTIONS = 1131,","\t",`-- RichEdit Control`,"\tCFM_BOLD = 1,","\tCFM_ITALIC = 2,"}
    filetext[3][878..882] = {"\tCFM_UNDERLINE = 4,","\tCFM_STRIKEOUT = 8,","\tCFM_PROTECTED = 16,","\tCFM_LINK = 32,","\tCFM_SIZE = #80000000,"}
    filetext[3][883..886] = {"\tCFM_COLOR = #40000000,","\tCFM_FACE = #20000000,","\tCFM_OFFSET = #10000000,","\tCFM_CHARSET = #08000000,"}
    filetext[3][887..890] = {"\tCFM_SUBSCRIPT = #00030000,","\tCFM_SUPERSCRIPT = #00030000,","\tCFE_BOLD = 1,","\tCFE_ITALIC = 2,"}
    filetext[3][891..894] = {"\tCFE_UNDERLINE = 4,","\tCFE_STRIKEOUT = 8,","\tCFE_PROTECTED = 16,","\tCFE_AUTOCOLOR = #40000000,"}
    filetext[3][895..896] = {"\tCFE_SUBSCRIPT = #00010000,","\tCFE_SUPERSCRIPT = #00020000,"}
    filetext[3][897][1..100] = "\tCFM_EFFECTS = or_all({CFM_BOLD, CFM_ITALIC, CFM_UNDERLINE, CFM_COLOR, CFM_STRIKEOUT, CFE_PROTECTED,"
    filetext[3][897][101..112] = ` CFM_LINK}),`
    filetext[3][898..901] = {"\tIMF_FORCENONE = 1,","\tIMF_FORCEENABLE = 2,","\tIMF_FORCEDISABLE = 4,","\tIMF_CLOSESTATUSWINDOW = 8,"}
    filetext[3][902..905] = {"\tIMF_VERTICAL = 32,","\tIMF_FORCEACTIVE = 64,","\tIMF_FORCEINACTIVE = 128,","\tIMF_FORCEREMEMBER = 256,"}
    filetext[3][906..910] = {"\tSEL_EMPTY = 0,","\tSEL_TEXT = 1,","\tSEL_OBJECT = 2,","\tSEL_MULTICHAR = 4,","\tSEL_MULTIOBJECT = 8,"}
    filetext[3][911..914] = {"\tMAX_TAB_STOPS = 32,","\tPFM_ALIGNMENT = 8,","\tPFM_NUMBERING = 32,","\tPFM_OFFSET = 4,"}
    filetext[3][915..918] = {"\tPFM_OFFSETINDENT = #80000000,","\tPFM_RIGHTINDENT = 2,","\tPFM_STARTINDENT = 1,","\tPFM_TABSTOPS = 16,"}
    filetext[3][919..924] = {"\tPFN_BULLET = 1,","\tPFA_LEFT = 1,","\tPFA_RIGHT = 2,","\tPFA_CENTER = 3,","\tSF_TEXT = 1,","\tSF_RTF = 2,"}
    filetext[3][925..928] = {"\tSF_RTFNOOBJS = 3,","\tSF_TEXTIZED = 4,","\tSF_UNICODE = 16,","\tSF_USECODEPAGE = 32,"}
    filetext[3][929..932] = {"\tSF_NCRFORNONASCII = 64,","\tSF_RTFVAL = #0700,","\tSFF_PWD = #0800,","\tSFF_KEEPDOCINFO = #1000,"}
    filetext[3][933..936] = {"\tSFF_PERSISTVIEWSCALE = #2000,","\tSFF_PLAINRTF = #4000,","\tSFF_SELECTION = #8000,","\tWB_CLASSIFY = 3,"}
    filetext[3][937..940] = {"\tWB_MOVEWORDLEFT = 4,","\tWB_MOVEWORDRIGHT = 5,","\tWB_LEFTBREAK = 6,","\tWB_RIGHTBREAK = 7,"}
    filetext[3][941..944] = {"\tWB_MOVEWORDPREV = 4,","\tWB_MOVEWORDNEXT = 5,","\tWB_PREVBREAK = 6,","\tWB_NEXTBREAK = 7,"}
    filetext[3][945..949] = {"\tWBF_WORDWRAP = 16,","\tWBF_WORDBREAK = 32,","\tWBF_OVERFLOW = 64,","\tWBF_LEVEL1 = 128,","\tWBF_LEVEL2 = 256,"}
    filetext[3][950..953] = {"\tWBF_CUSTOM = 512,","\tES_DISABLENOSCROLL = 8192,","\tES_EX_NOCALLOLEINIT = 16777216,","\tES_NOIME = 524288,"}
    filetext[3][954..957] = {"\tES_SAVESEL = 32768,","\tES_SELFIME = 262144,","\tES_SUNKEN = 16384,","\tES_VERTICAL = 4194304,"}
    filetext[3][958..960] = {"\tES_SELECTIONBAR = 16777216,","--\t  EM_CANPASTE = (WM_USER+50),","--\t  EM_DISPLAYBAND = (WM_USER+51),"}
    filetext[3][961..962] = {"--\t  EM_EXGETSEL = (WM_USER+52),","--\t  EM_EXLIMITTEXT = (WM_USER+53),"}
    filetext[3][963..965] = {"--\t  EM_EXLINEFROMCHAR = (WM_USER+54),","--\t  EM_EXSETSEL = (WM_USER+55),","--\t  EM_FINDTEXT = (WM_USER+56),"}
    filetext[3][966..967] = {"--\t  EM_FORMATRANGE = (WM_USER+57),","--\t  EM_GETCHARFORMAT = (WM_USER+58),"}
    filetext[3][968..969] = {"--\t  EM_GETEVENTMASK = (WM_USER+59),","--\t  EM_GETOLEINTERFACE = (WM_USER+60),"}
    filetext[3][970..971] = {"--\t  EM_GETPARAFORMAT = (WM_USER+61),","--\t  EM_GETSELTEXT = (WM_USER+62),"}
    filetext[3][972..973] = {"--\t  EM_HIDESELECTION = (WM_USER+63),","--\t  EM_PASTESPECIAL = (WM_USER+64),"}
    filetext[3][974..975] = {"--\t  EM_REQUESTRESIZE = (WM_USER+65),","--\t  EM_SELECTIONTYPE = (WM_USER+66),"}
    filetext[3][976..977] = {"--\t  EM_SETBKGNDCOLOR = (WM_USER+67),","--\t  EM_SETCHARFORMAT = (WM_USER+68),"}
    filetext[3][978..979] = {"--\t  EM_SETEVENTMASK = (WM_USER+69),","--\t  EM_SETOLECALLBACK = (WM_USER+70),"}
    filetext[3][980..981] = {"--\t  EM_SETPARAFORMAT = (WM_USER+71),","--\t  EM_SETTARGETDEVICE = (WM_USER+72),"}
    filetext[3][982..984] = {"--\t  EM_STREAMIN = (WM_USER+73),","--\t  EM_STREAMOUT = (WM_USER+74),","--\t  EM_GETTEXTRANGE = (WM_USER+75),"}
    filetext[3][985..986] = {"--\t  EM_FINDWORDBREAK = (WM_USER+76),","--\t  EM_SETOPTIONS = (WM_USER+77),"}
    filetext[3][987..988] = {"--\t  EM_GETOPTIONS = (WM_USER+78),","--\t  EM_FINDTEXTEX = (WM_USER+79),"}
    filetext[3][989..990] = {"--\t  EM_GETWORDBREAKPROCEX = (WM_USER+80),","--\t  EM_SETWORDBREAKPROCEX = (WM_USER+81),"}
    filetext[3][991..993] = {`-- RichEdit 2.0 messages`,"\tEM_SETUNDOLIMIT = (WM_USER+82),","\tEM_REDO = (WM_USER+84),"}
    filetext[3][994..996] = {"\tEM_CANREDO = (WM_USER+85),","\tEM_GETUNDONAME = (WM_USER+86),","\tEM_GETREDONAME = (WM_USER+87),"}
    filetext[3][997..999] = {"\tEM_STOPGROUPTYPING = (WM_USER+88),","\tEM_SETTEXTMODE = (WM_USER+89),","\tEM_GETTEXTMODE = (WM_USER+90),"}
    filetext[3][1000..1001] = {"\tEM_GETTEXTLENGTHEX = (WM_USER+95),","\tEM_SETLANGOPTIONS = (WM_USER+120),"}
    filetext[3][1002..1003] = {"\tEM_GETLANGOPTIONS = (WM_USER+121),","\tEM_GETIMECOMPMODE = (WM_USER+122),"}
    filetext[3][1004..1005] = {"\tEM_SETTYPOGRAPHYOPTIONS = (WM_USER+202),","\tEM_GETTYPOGRAPHYOPTIONS = (WM_USER+203),"}
    filetext[3][1006..1009] = {"\tEM_SETFONTSIZE = (WM_USER+223),","\tEM_GETZOOM = (WM_USER+224),","\tEM_SETZOOM = (WM_USER+225),",``}
    filetext[3][1010..1013] = {"\tTM_PLAINTEXT = 1,","\tTM_RICHTEXT = 2,","\tTM_SINGLELEVELUNDO = 4,","\tTM_MULTILEVELUNDO = 8,"}
    filetext[3][1014..1018] = {"\tTM_SINGLECODEPAGE = 16,","\tTM_MULTICODEPAGE = 32,",``,"\tEN_CORRECTTEXT = 1797,","\tEN_DROPFILES = 1795,"}
    filetext[3][1019..1022] = {"\tEN_IMECHANGE = 1799,","\tEN_MSGFILTER = 1792,","\tEN_OLEOPFAILED = 1801,","\tEN_PROTECTED = 1796,"}
    filetext[3][1023..1026] = {"\tEN_REQUESTRESIZE = 1793,","\tEN_SAVECLIPBOARD = 1800,","\tEN_SELCHANGE = 1794,","\tEN_STOPNOUNDO = 1798,"}
    filetext[3][1027..1030] = {"\tENM_NONE = 0,","\tENM_CHANGE = 1,","\tENM_CORRECTTEXT = 4194304,","\tENM_DROPFILES = 1048576,"}
    filetext[3][1031..1033] = {"\tENM_KEYEVENTS = 65536,","\tENM_MOUSEEVENTS = 131072,","\tENM_PROTECTED = 2097152,"}
    filetext[3][1034..1037] = {"\tENM_REQUESTRESIZE = 262144,","\tENM_SCROLL = 4,","\tENM_SELCHANGE = 524288,","\tENM_UPDATE = 2,"}
    filetext[3][1038..1041] = {"\tECO_AUTOWORDSELECTION = 1,","\tECO_AUTOVSCROLL = 64,","\tECO_AUTOHSCROLL = 128,","\tECO_NOHIDESEL = 256,"}
    filetext[3][1042..1045] = {"\tECO_READONLY = 2048,","\tECO_WANTRETURN = 4096,","\tECO_SAVESEL = #8000,","\tECO_SELECTIONBAR = #1000000,"}
    filetext[3][1046..1050] = {"\tECO_VERTICAL = #400000,","\tECOOP_SET = 1,","\tECOOP_OR = 2,","\tECOOP_AND = 3,","\tECOOP_XOR = 4,"}
    filetext[3][1051..1055] = {"\tSCF_DEFAULT = 0,","\tSCF_SELECTION = 1,","\tSCF_WORD = 2,","\tSCF_ALL = 4,","\tSCF_USEUIRULES = 8,"}
    filetext[3][1056..1060] = {"\tyHeightCharPtsMost = 1638,","\tlDefaultTab = 720,",``,`-- EDITWORDBREAKPROC code values`,"\tWB_LEFT = 0,"}
    filetext[3][1061..1065] = {"\tWB_RIGHT = 1,","\tWB_ISDELIMITER = 2,",``,`-- Button Control Styles`,"\tBS_PUSHBUTTON = #0,"}
    filetext[3][1066..1069] = {"\tBS_DEFPUSHBUTTON = #1,","\tBS_CHECKBOX = #2,","\tBS_AUTOCHECKBOX = #3,","\tBS_RADIOBUTTON = #4,"}
    filetext[3][1070..1073] = {"\tBS_3STATE = #5,","\tBS_AUTO3STATE = #6,","\tBS_GROUPBOX = #7,","\tBS_USERBUTTON = #8,"}
    filetext[3][1074..1078] = {"\tBS_AUTORADIOBUTTON = #9,","\tBS_OWNERDRAW = #B,","\tBS_LEFTTEXT = #20,",``,`-- User Button Notification Codes`}
    filetext[3][1079..1083] = {"\tBN_CLICKED = 0,","\tBN_PAINT = 1,","\tBN_HILITE = 2,","\tBN_UNHILITE = 3,","\tBN_DISABLE = 4,"}
    filetext[3][1084..1088] = {"\tBN_DOUBLECLICKED = 5,",``,`-- Button Control Messages`,"\tBM_GETCHECK = #F0,","\tBM_SETCHECK = #F1,"}
    filetext[3][1089..1092] = {"\tBM_GETSTATE = #F2,","\tBM_SETSTATE = #F3,","\tBM_SETSTYLE = #F4,","\tBST_CHECKED = 1,"}
    filetext[3][1093..1097] = {"\tBST_INDETERMINATE = 2,","\tBST_UNCHECKED = 0,",``,`-- Tab Control stuff`,"\tTCS_FORCEICONLEFT = 16,"}
    filetext[3][1098..1101] = {"\tTCS_FORCELABELLEFT = 32,","\tTCS_TABS = 0,","\tTCS_BUTTONS = 256,","\tTCS_SINGLELINE = 0,"}
    filetext[3][1102..1105] = {"\tTCS_MULTILINE = 512,","\tTCS_RIGHTJUSTIFY = 0,","\tTCS_FIXEDWIDTH = 1024,","\tTCS_RAGGEDRIGHT = 2048,"}
    filetext[3][1106..1108] = {"\tTCS_FOCUSONBUTTONDOWN = #1000,","\tTCS_OWNERDRAWFIXED = #2000,","\tTCS_TOOLTIPS = #4000,"}
    filetext[3][1109..1112] = {"\tTCS_FOCUSNEVER = #8000,","\tTCS_BOTTOM = 2,","\tTCS_RIGHT = 2,","\tTCS_VERTICAL = 128,"}
    filetext[3][1113..1115] = {"\tTCS_SCROLLOPPOSITE = #0001,","\tTCS_HOTTRACK = #0040,","\tTCS_MULTISELECT = #0004,"}
    filetext[3][1116..1118] = {"\tTCS_FLATBUTTONS = #0008,","\tTCS_EX_FLATSEPARATORS = #00000001,","\tTCS_EX_REGISTERDROP = #00000002,"}
    filetext[3][1119..1123] = {"\tTCIF_TEXT = 1,","\tTCIF_IMAGE = 2,","\tTCIF_RTLREADING = 4,","\tTCIF_PARAM = 8,","\tTCIF_STATE = 16,"}
    filetext[3][1124..1127] = {"\tTCIS_BUTTONPRESSED = 1,","\tTCIS_HIGHLIGHTED = 2,","\tTCM_FIRST = #1300,","\tTCM_GETIMAGELIST = (TCM_FIRST+2),"}
    filetext[3][1128..1130] = {"\tTCM_SETIMAGELIST = (TCM_FIRST+3),","\tTCM_GETITEMCOUNT = (TCM_FIRST+4),","\tTCM_GETITEMA = (TCM_FIRST+5),"}
    filetext[3][1131..1133] = {"\tTCM_GETITEMW = (TCM_FIRST+60),","\tTCM_SETITEMA = (TCM_FIRST+6),","\tTCM_SETITEMW = (TCM_FIRST+61),"}
    filetext[3][1134..1136] = {"\tTCM_INSERTITEMA = (TCM_FIRST+7),","\tTCM_INSERTITEMW = (TCM_FIRST+62),","\tTCM_DELETEITEM = (TCM_FIRST+8),"}
    filetext[3][1137..1139] = {"\tTCM_DELETEALLITEMS = (TCM_FIRST+9),","\tTCM_GETITEMRECT = (TCM_FIRST+10),","\tTCM_GETCURSEL = (TCM_FIRST+11),"}
    filetext[3][1140..1143] = {"\tTCM_SETCURSEL = (TCM_FIRST+12),","\tTCM_HITTEST = (TCM_FIRST+13),","\tTCM_SETITEMEXTRA = (TCM_FIRST+14),",``}
    filetext[3][1144..1146] = {"\tTCM_ADJUSTRECT = (TCM_FIRST+40),","\tTCM_SETITEMSIZE = (TCM_FIRST+41),","\tTCM_REMOVEIMAGE = (TCM_FIRST+42),"}
    filetext[3][1147..1149] = {"\tTCM_SETPADDING = (TCM_FIRST+43),","\tTCM_GETROWCOUNT = (TCM_FIRST+44),","\tTCM_GETTOOLTIPS = (TCM_FIRST+45),"}
    filetext[3][1150..1152] = {"\tTCM_SETTOOLTIPS = (TCM_FIRST+46),","\tTCM_GETCURFOCUS = (TCM_FIRST+47),","\tTCM_SETCURFOCUS = (TCM_FIRST+48),"}
    filetext[3][1153..1154] = {"\tTCM_SETMINTABWIDTH = (TCM_FIRST + 49),","\tTCM_DESELECTALL = (TCM_FIRST + 50),"}
    filetext[3][1155..1156] = {"\tTCM_HIGHLIGHTITEM = (TCM_FIRST + 51),","\tTCM_SETEXTENDEDSTYLE = (TCM_FIRST + 52),"}
    filetext[3][1157..1158] = {"\tTCM_GETEXTENDEDSTYLE = (TCM_FIRST + 53),","\t--TCM_SETUNICODEFORMAT = CCM_SETUNICODEFORMAT,"}
    filetext[3][1159..1161] = {"\t--TCM_GETUNICODEFORMAT = CCM_GETUNICODEFORMAT,","\tTCN_FIRST = -550,","\tTCN_LAST = -580,"}
    filetext[3][1162..1165] = {"\tTCN_KEYDOWN = TCN_FIRST,","\tTCN_SELCHANGE = (TCN_FIRST-1),","\tTCN_SELCHANGING = (TCN_FIRST-2),",``}
    filetext[3][1166..1169] = {"\tNM_FIRST = 0,","\tNM_LAST = NM_FIRST - 99,","\tNM_OUTOFMEMORY = NM_FIRST - 1,","\tNM_CLICK = NM_FIRST - 2,"}
    filetext[3][1170..1172] = {"\tNM_DBLCLK = NM_FIRST - 3,","\tNM_RETURN = NM_FIRST - 4,","\tNM_RCLICK = NM_FIRST - 5,"}
    filetext[3][1173..1175] = {"\tNM_RDBLCLK = NM_FIRST - 6,","\tNM_SETFOCUS = NM_FIRST - 7,","\tNM_KILLFOCUS = NM_FIRST - 8,"}
    filetext[3][1176..1178] = {"\tNM_CUSTOMDRAW = NM_FIRST - 12,","\tNM_HOVER = NM_FIRST - 13,","\tNM_NCHITTEST = NM_FIRST - 14,"}
    filetext[3][1179..1181] = {"\tNM_KEYDOWN = NM_FIRST - 15,","\tNM_RELEASEDCAPTURE = NM_FIRST - 16,","\tNM_SETCURSOR = NM_FIRST - 17,"}
    filetext[3][1182..1184] = {"\tNM_CHAR = NM_FIRST - 18,","\tNM_TOOLTIPSCREATED = NM_FIRST - 19,","\tNM_LDOWN = NM_FIRST - 20,"}
    filetext[3][1185..1187] = {"\tNM_RDOWN = NM_FIRST - 21,","\tNM_THEMECHANGED = NM_FIRST - 22,","\tNM_FONTCHANGED = NM_FIRST - 23,"}
    filetext[3][1188..1191] = {"\tNM_CUSTOMTEXT = NM_FIRST - 24,","\tNM_TVSTATEIMAGECHANGING = NM_FIRST - 24,","\t",`-- tooltip stuff`}
    filetext[3][1192..1195] = {"\tTTN_FIRST = -520,","\tTTN_GETDISPINFO = TTN_FIRST - 0,","\tTTN_SHOW = TTN_FIRST-1,","\tTTN_POP = TTN_FIRST-2,"}
    filetext[3][1196..1199] = {"\tTTN_LINKCLICK = TTN_FIRST-3,","\tTTN_NEEDTEXT = TTN_GETDISPINFO,","\t","\tTTS_ALWAYSTIP = #01,"}
    filetext[3][1200..1204] = {"\tTTS_NOPREFIX = #02,","\tTTS_NOANIMATE = #10,","\tTTS_NOFADE = #20,","\tTTS_BALLOON = #40,","\tTTS_CLOSE = #80,"}
    filetext[3][1205..1208] = {"\tTTS_USEVISUALSTYLE = #100,","\tTTF_IDISHWND = #0001,","\tTTF_CENTERTIP = #0002,","\tTTF_RTLREADING = #0004,"}
    filetext[3][1209..1212] = {"\tTTF_SUBCLASS = #0010,","\tTTF_TRACK = #0020,","\tTTF_ABSOLUTE = #0080,","\tTTF_TRANSPARENT = #0100,"}
    filetext[3][1213..1216] = {"\tTTF_PARSELINKS = #1000,","\tTTF_DI_SETITEM = #8000,","\t","\tTTM_ACTIVATE = WM_USER + 1,"}
    filetext[3][1217..1219] = {"\tTTM_SETDELAYTIME = WM_USER + 3,","\tTTM_ADDTOOL = WM_USER + 4,","\tTTM_DELTOOL = WM_USER + 5,"}
    filetext[3][1220..1222] = {"\tTTM_NEWTOOLRECT = WM_USER + 6,","\tTTM_RELAYEVENT = WM_USER + 7,","\tTTM_GETTOOLINFO = WM_USER + 8,"}
    filetext[3][1223..1225] = {"\tTTM_SETTOOLINFO = WM_USER + 9,","\tTTM_HITTEST = WM_USER + 10,","\tTTM_GETTEXT = WM_USER + 11,"}
    filetext[3][1226..1228] = {"\tTTM_UPDATETIPTEXT = WM_USER + 12,","\tTTM_GETTOOLCOUNT = WM_USER + 13,","\tTTM_ENUMTOOLS = WM_USER + 14,"}
    filetext[3][1229..1230] = {"\tTTM_GETCURRENTTOOL = WM_USER + 15,","\tTTM_WINDOWFROMPOINT = WM_USER + 16,"}
    filetext[3][1231..1233] = {"\tTTM_TRACKACTIVATE = WM_USER + 17,","\tTTM_TRACKPOSITION = WM_USER + 18,","\tTTM_SETTIPBKCOLOR = WM_USER + 19,"}
    filetext[3][1234..1236] = {"\tTTM_SETTIPTEXTCOLOR = WM_USER + 20,","\tTTM_GETDELAYTIME = WM_USER + 21,","\tTTM_GETTIPBKCOLOR = WM_USER + 22,"}
    filetext[3][1237..1238] = {"\tTTM_GETTIPTEXTCOLOR = WM_USER + 23,","\tTTM_SETMAXTIPWIDTH = WM_USER + 24,"}
    filetext[3][1239..1241] = {"\tTTM_GETMAXTIPWIDTH = WM_USER + 25,","\tTTM_SETMARGIN = WM_USER + 26,","\tTTM_GETMARGIN = WM_USER + 27,"}
    filetext[3][1242..1244] = {"\tTTM_POP = WM_USER + 29,","\tTTM_GETBUBBLESIZE = WM_USER + 30,","\tTTM_ADJUSTRECT = WM_USER + 31,"}
    filetext[3][1245..1249] = {"\tTTM_SETTITLE = WM_USER + 30,","\tLPSTR_TEXTCALLBACK = -1",``,`-- dialog specific stuff`,`global constant`}
    filetext[3][1250] = "\tEndDialog = define_c_func(user32, \"EndDialog\", {C_LONG, C_LONG}, C_LONG),"
    filetext[3][1251] = "\tGetDlgItem = define_c_func(user32, \"GetDlgItem\", {C_LONG, C_LONG}, C_LONG),"
    filetext[3][1252] = "\tSetDlgItemInt = define_c_func(user32, \"SetDlgItemInt\", {C_LONG, C_LONG, C_LONG, C_LONG}, C_LONG),"
    filetext[3][1253] = "\tGetDlgItemInt = define_c_func(user32, \"GetDlgItemInt\", {C_LONG, C_LONG, C_LONG, C_LONG}, C_LONG),"
    filetext[3][1254] = "\tSetDlgItemText = define_c_func(user32, \"SetDlgItemTextA\", {C_LONG, C_LONG, C_PTR}, C_LONG),"
    filetext[3][1255] = "\tGetDlgItemText = define_c_func(user32, \"GetDlgItemTextA\", {C_LONG, C_LONG, C_PTR, C_LONG}, C_LONG),"
    filetext[3][1256] = "--\t  CheckDlgButton = define_c_func(user32, \"CheckDLGButtonA\", {C_LONG, C_LONG, C_LONG}, C_LONG),"
    filetext[3][1257][1..99] = "--\t  CheckRadioButton = define_c_func(user32, \"CheckRadioButtonA\", {C_LONG, C_LONG, C_LONG, C_LONG}"
    filetext[3][1257][100..109] = `, C_LONG),`
    filetext[3][1258] = "\tIsDlgButtonChecked = define_c_func(user32, \"IsDlgButtonChecked\", {C_LONG, C_LONG}, C_LONG),"
    filetext[3][1259][1..99] = "\tSendDlgItemMessage = define_c_func(user32, \"SendDlgItemMessageA\", {C_LONG, C_LONG, C_LONG, C_INT, "
    filetext[3][1259][100..116] = `C_LONG}, C_LONG),`
    filetext[3][1260] = "\tGetNextDlgGroupItem = define_c_func(user32, \"GetNextDlgGroupItem\", {C_LONG, C_LONG, C_LONG}, C_LONG),"
    filetext[3][1261] = "\tGetNextDlgTabItem = define_c_func(user32, \"GetNextDlgTabItem\", {C_LONG, C_LONG, C_LONG}, C_LONG),"
    filetext[3][1262] = "\tGetDlgCtrlID = define_c_func(user32, \"GetDlgCtrlID\", {C_LONG}, C_LONG),"
    filetext[3][1263] = "\tGetDialogBaseUnits = define_c_func(user32, \"GetDialogBaseUnits\", {}, C_LONG),"
    filetext[3][1264] = "\tDefDlgProc = define_c_func(user32, \"DefDlgProcA\", {C_LONG, C_LONG, C_INT, C_LONG}, C_LONG),"
    filetext[3][1265][1..99] = "\tCreateDialogIndirectParam_ = define_c_func(user32, \"CreateDialogIndirectParamW\", {C_LONG, C_PTR, C"
    filetext[3][1265][100..130] = `_LONG, C_PTR, C_LONG}, C_LONG),`
    filetext[3][1266][1..99] = "\tDialogBoxIndirectParam_ = define_c_func(user32, \"DialogBoxIndirectParamW\", {C_LONG, C_PTR, C_LONG,"
    filetext[3][1266][100..123] = ` C_PTR, C_LONG}, C_LONG)`
    filetext[3][1267..1273] = {``,``,`global constant`,`-- Static Control Constants`,"\tSS_LEFT = #0,","\tSS_CENTER = #1,","\tSS_RIGHT = #2,"}
    filetext[3][1274..1278] = {"\tSS_ICON = #3,","\tSS_BLACKRECT = #4,","\tSS_GRAYRECT = #5,","\tSS_WHITERECT = #6,","\tSS_BLACKFRAME = #7,"}
    filetext[3][1279..1282] = {"\tSS_GRAYFRAME = #8,","\tSS_WHITEFRAME = #9,","\tSS_USERITEM = #A,","\tSS_SIMPLE = #B,"}
    filetext[3][1283..1285] = {"\tSS_LEFTNOWORDWRAP = #C,","\tSS_NOPREFIX = #80,\t\t\t --  Don\'t do \"\" character translation",``}
    filetext[3][1286..1290] = {`-- Static Control Mesages`,"\tSTM_SETICON = #170,","\tSTM_GETICON = #171,","\tSTM_MSGMAX = #172,",``}
    filetext[3][1291..1293] = {"\tWC_DIALOG = 8002,",``,"--\tGet/SetWindowWord/Long offsets for use with WC_DIALOG windows"}
    filetext[3][1294..1298] = {"\tDWL_MSGRESULT = 0,","\tDWL_DLGPROC = 4,","\tDWL_USER = 8,",``,`-- Dialog Manager Routines`}
    filetext[3][1299] = "\tIsDialogMessage = define_c_func(user32, \"IsDialogMessageA\", {C_PTR, C_PTR}, C_LONG),"
    filetext[3][1300] = "\tMapDialogRect = define_c_func(user32, \"MapDialogRect\", {C_LONG, C_PTR}, C_LONG),"
    filetext[3][1301..1302] = {"\tDlgDirList = define_c_func(user32, \"DlgDirListA\", {C_LONG, C_PTR, C_LONG, C_LONG, C_LONG}, C_LONG),",``}
    filetext[3][1303..1305] = {`-- DlgDirList, DlgDirListComboBox flags values`,"\tDDL_READWRITE = #0,","\tDDL_READONLY = #1,"}
    filetext[3][1306..1310] = {"\tDDL_HIDDEN = #2,","\tDDL_SYSTEM = #4,","\tDDL_DIRECTORY = #10,","\tDDL_ARCHIVE = #20,",``}
    filetext[3][1311..1314] = {"\tDDL_POSTMSGS = #2000,","\tDDL_DRIVES = #4000,","\tDDL_EXCLUSIVE = #8000,",``}
    filetext[3][1315] = "\tDlgDirSelectEx = define_c_func(user32, \"DlgDirSelectExA\", {C_LONG, C_PTR, C_LONG, C_LONG}, C_LONG),"
    filetext[3][1316][1..99] = "\tDlgDirListComboBox = define_c_func(user32, \"DlgDirListComboBoxA\", {C_LONG, C_PTR, C_LONG, C_LONG, "
    filetext[3][1316][100..116] = `C_LONG}, C_LONG),`
    filetext[3][1317][1..99] = "\tDlgDirSelectComboBoxEx = define_c_func(user32, \"DlgDirSelectComboBoxExA\", {C_LONG, C_PTR, C_LONG, "
    filetext[3][1317][100..116] = `C_LONG}, C_LONG),`
    filetext[3][1318..1321] = {``,`-- Dialog Styles`,"\tDS_ABSALIGN = #1,","\tDS_SYSMODAL = #2,"}
    filetext[3][1322] = "\tDS_LOCALEDIT = #20, \t\t --  Edit items get Local storage."
    filetext[3][1323] = "\tDS_SETFONT = #40,\t\t\t --  User specified font for Dlg controls"
    filetext[3][1324] = "\tDS_MODALFRAME = #80,\t\t --  Can be combined with WS_CAPTION"
    filetext[3][1325] = "\tDS_NOIDLEMSG = #100,\t\t --  WM_ENTERIDLE message will not be sent"
    filetext[3][1326..1329] = {"\tDS_SETFOREGROUND = #200,\t --  not in win3.1",``,"\tDM_GETDEFID = WM_USER + 0,","\tDM_SETDEFID = WM_USER + 1,"}
    filetext[3][1330..1332] = {"\tDC_HASDEFID = #534, \t --0x534B",``,`-- Dialog Codes`}
    filetext[3][1333] = "\tDLGC_WANTARROWS = #1,\t\t\t   --  Control wants arrow keys"
    filetext[3][1334] = "\tDLGC_WANTTAB = #2,\t\t\t\t   --  Control wants tab keys"
    filetext[3][1335] = "\tDLGC_WANTALLKEYS = #4,\t\t\t   --  Control wants all keys"
    filetext[3][1336] = "\tDLGC_WANTMESSAGE = #4,\t\t\t   --  Pass message to control"
    filetext[3][1337] = "\tDLGC_HASSETSEL = #8,\t\t\t   --  Understands EM_SETSEL message"
    filetext[3][1338] = "\tDLGC_DEFPUSHBUTTON = #10,\t\t   --  Default pushbutton"
    filetext[3][1339] = "\tDLGC_UNDEFPUSHBUTTON = #20, \t   --  Non-default pushbutton"
    filetext[3][1340..1341] = {"\tDLGC_RADIOBUTTON = #40, \t\t   --  Radio button","\tDLGC_WANTCHARS = #80,\t\t\t   --  Want WM_CHAR messages"}
    filetext[3][1342] = "\tDLGC_STATIC = #100, \t\t\t   --  Static item: don\'t include"
    filetext[3][1343..1346] = {"\tDLGC_BUTTON = #2000,\t\t\t   --  Button item: can be checked",``,"\tLB_CTLCODE = 0,",``}
    filetext[3][1347..1351] = {`-- Listbox Return Values`,"\tLB_OKAY = 0,","\tLB_ERR = (-1),","\tLB_ERRSPACE = (-2),",``}
    filetext[3][1352] = `-- The idStaticPath parameter to DlgDirList can have the following values`
    filetext[3][1353..1354] = {`-- ORed if the list box should show other details of the files along with`,`-- the name of the files;`}
    filetext[3][1355..1358] = {`-- all other details also will be returned`,``,`-- Listbox Notification Codes`,"\tLBN_ERRSPACE = (-2),"}
    filetext[3][1359..1364] = {"\tLBN_SELCHANGE = 1,","\tLBN_DBLCLK = 2,","\tLBN_SELCANCEL = 3,","\tLBN_SETFOCUS = 4,","\tLBN_KILLFOCUS = 5,",``}
    filetext[3][1365..1368] = {`-- Listbox messages`,"\tLB_ADDSTRING = #180,","\tLB_INSERTSTRING = #181,","\tLB_DELETESTRING = #182,"}
    filetext[3][1369..1372] = {"\tLB_SELITEMRANGEEX = #183,","\tLB_RESETCONTENT = #184,","\tLB_SETSEL = #185,","\tLB_SETCURSEL = #186,"}
    filetext[3][1373..1376] = {"\tLB_GETSEL = #187,","\tLB_GETCURSEL = #188,","\tLB_GETTEXT = #189,","\tLB_GETTEXTLEN = #18A,"}
    filetext[3][1377..1380] = {"\tLB_GETCOUNT = #18B,","\tLB_SELECTSTRING = #18C,","\tLB_DIR = #18D,","\tLB_GETTOPINDEX = #18E,"}
    filetext[3][1381..1384] = {"\tLB_FINDSTRING = #18F,","\tLB_GETSELCOUNT = #190,","\tLB_GETSELITEMS = #191,","\tLB_SETTABSTOPS = #192,"}
    filetext[3][1385..1387] = {"\tLB_GETHORIZONTALEXTENT = #193,","\tLB_SETHORIZONTALEXTENT = #194,","\tLB_SETCOLUMNWIDTH = #195,"}
    filetext[3][1388..1391] = {"\tLB_ADDFILE = #196,","\tLB_SETTOPINDEX = #197,","\tLB_GETITEMRECT = #198,","\tLB_GETITEMDATA = #199,"}
    filetext[3][1392..1395] = {"\tLB_SETITEMDATA = #19A,","\tLB_SELITEMRANGE = #19B,","\tLB_SETANCHORINDEX = #19C,","\tLB_GETANCHORINDEX = #19D,"}
    filetext[3][1396..1398] = {"\tLB_SETCARETINDEX = #19E,","\tLB_GETCARETINDEX = #19F,","\tLB_SETITEMHEIGHT = #1A0,"}
    filetext[3][1399..1402] = {"\tLB_GETITEMHEIGHT = #1A1,","\tLB_FINDSTRINGEXACT = #1A2,","\tLB_SETLOCALE = #1A5,","\tLB_GETLOCALE = #1A6,"}
    filetext[3][1403..1408] = {"\tLB_SETCOUNT = #1A7,","\tLB_MSGMAX = #1A8,",``,`-- Listbox Styles`,"\tLBS_NOTIFY = #1,","\tLBS_SORT = #2,"}
    filetext[3][1409..1412] = {"\tLBS_NOREDRAW = #4,","\tLBS_MULTIPLESEL = #8,","\tLBS_OWNERDRAWFIXED = #10,","\tLBS_OWNERDRAWVARIABLE = #20,"}
    filetext[3][1413..1416] = {"\tLBS_HASSTRINGS = #40,","\tLBS_USETABSTOPS = #80,","\tLBS_NOINTEGRALHEIGHT = #100,","\tLBS_MULTICOLUMN = #200,"}
    filetext[3][1417..1419] = {"\tLBS_WANTKEYBOARDINPUT = #400,","\tLBS_EXTENDEDSEL = #800,","\tLBS_DISABLENOSCROLL = #1000,"}
    filetext[3][1420..1422] = {"\tLBS_NODATA = #2000,","\tLBS_STANDARD = or_all({LBS_NOTIFY, LBS_SORT, WS_VSCROLL, WS_BORDER}),",``}
    filetext[3][1423..1427] = {`-- Combo Box return Values`,"\tCB_OKAY = 0,","\tCB_ERR = (-1),","\tCB_ERRSPACE = (-2),",``}
    filetext[3][1428..1431] = {`-- Combo Box Notification Codes`,"\tCBN_ERRSPACE = (-1),","\tCBN_SELCHANGE = 1,","\tCBN_DBLCLK = 2,"}
    filetext[3][1432..1436] = {"\tCBN_SETFOCUS = 3,","\tCBN_KILLFOCUS = 4,","\tCBN_EDITCHANGE = 5,","\tCBN_EDITUPDATE = 6,","\tCBN_DROPDOWN = 7,"}
    filetext[3][1437..1441] = {"\tCBN_CLOSEUP = 8,","\tCBN_SELENDOK = 9,","\tCBN_SELENDCANCEL = 10,",``,`-- Combo Box styles`}
    filetext[3][1442..1445] = {"\tCBS_SIMPLE = #1,","\tCBS_DROPDOWN = #2,","\tCBS_DROPDOWNLIST = #3,","\tCBS_OWNERDRAWFIXED = #10,"}
    filetext[3][1446..1449] = {"\tCBS_OWNERDRAWVARIABLE = #20,","\tCBS_AUTOHSCROLL = #40,","\tCBS_OEMCONVERT = #80,","\tCBS_SORT = #100,"}
    filetext[3][1450..1453] = {"\tCBS_HASSTRINGS = #200,","\tCBS_NOINTEGRALHEIGHT = #400,","\tCBS_DISABLENOSCROLL = #800,",``}
    filetext[3][1454..1457] = {`-- Combo Box messages`,"\tCB_GETEDITSEL = #140,","\tCB_LIMITTEXT = #141,","\tCB_SETEDITSEL = #142,"}
    filetext[3][1458..1461] = {"\tCB_ADDSTRING = #143,","\tCB_DELETESTRING = #144,","\tCB_DIR = #145,","\tCB_GETCOUNT = #146,"}
    filetext[3][1462..1465] = {"\tCB_GETCURSEL = #147,","\tCB_GETLBTEXT = #148,","\tCB_GETLBTEXTLEN = #149,","\tCB_INSERTSTRING = #14A,"}
    filetext[3][1466..1469] = {"\tCB_RESETCONTENT = #14B,","\tCB_FINDSTRING = #14C,","\tCB_SELECTSTRING = #14D,","\tCB_SETCURSEL = #14E,"}
    filetext[3][1470..1472] = {"\tCB_SHOWDROPDOWN = #14F,","\tCB_GETITEMDATA = #150,","\tCB_SETITEMDATA = #151,"}
    filetext[3][1473..1475] = {"\tCB_GETDROPPEDCONTROLRECT = #152,","\tCB_SETITEMHEIGHT = #153,","\tCB_GETITEMHEIGHT = #154,"}
    filetext[3][1476..1478] = {"\tCB_SETEXTENDEDUI = #155,","\tCB_GETEXTENDEDUI = #156,","\tCB_GETDROPPEDSTATE = #157,"}
    filetext[3][1479..1483] = {"\tCB_FINDSTRINGEXACT = #158,","\tCB_SETLOCALE = #159,","\tCB_GETLOCALE = #15A,","\tCB_MSGMAX = #15B,",``}
    filetext[3][1484..1488] = {`-- Scroll Bar Styles`,"\tSBS_HORZ = #0,","\tSBS_VERT = #1,","\tSBS_TOPALIGN = #2,","\tSBS_LEFTALIGN = #2,"}
    filetext[3][1489..1491] = {"\tSBS_BOTTOMALIGN = #4,","\tSBS_RIGHTALIGN = #4,","\tSBS_SIZEBOXTOPLEFTALIGN = #2,"}
    filetext[3][1492..1495] = {"\tSBS_SIZEBOXBOTTOMRIGHTALIGN = #4,","\tSBS_SIZEBOX = #8,",``,"--\tScroll bar messages"}
    filetext[3][1496..1497] = {"\tSBM_SETPOS = #E0, -- not in win3.1","\tSBM_GETPOS = #E1, -- not in win3.1"}
    filetext[3][1498..1499] = {"\tSBM_SETRANGE = #E2, -- not in win3.1","\tSBM_SETRANGEREDRAW = #E6, -- not in win3.1"}
    filetext[3][1500..1502] = {"\tSBM_GETRANGE = #E3, -- not in win3.1","\tSBM_ENABLE_ARROWS = #E4, -- not in win3.1",``}
    filetext[3][1503..1505] = {"\tMDIS_ALLCHILDSTYLES = #1,",``,`-- wParam values for WM_MDITILE and WM_MDICASCADE messages.`}
    filetext[3][1506..1509] = {"\tMDITILE_VERTICAL = #0,","\tMDITILE_HORIZONTAL = #1,","\tMDITILE_SKIPDISABLED = #2,",``}
    filetext[3][1510..1512] = {`-- Type MDICREATESTRUCT`,"\tMDICREATESTRUCT_szClass = 0,","\tMDICREATESTRUCT_szTitle = 4,"}
    filetext[3][1513..1515] = {"\tMDICREATESTRUCT_hOwner = 8,","\tMDICREATESTRUCT_x = 12,","\tMDICREATESTRUCT_y = 16,"}
    filetext[3][1516..1518] = {"\tMDICREATESTRUCT_cx = 20,","\tMDICREATESTRUCT_cy = 24,","\tMDICREATESTRUCT_style = 28,"}
    filetext[3][1519..1522] = {"\tMDICREATESTRUCT_lParam = 32,","\tSIZEOF_MDICREATESTRUCT = 36,",``,`-- Type CLIENTCREATESTRUCT`}
    filetext[3][1523..1524] = {"\tCLIENTCREATESTRUCT_hWindowMenu = 0,","\tCLIENTCREATESTRUCT_idFirstChild = 4,"}
    filetext[3][1525..1526] = {"\tSIZEOF_CLIENTCREATESTRUCT = 8,",``}
    filetext[3][1527] = "\tDefFrameProc = define_c_func(user32, \"DefFrameProcA\", {C_LONG, C_LONG, C_LONG, C_INT, C_LONG}, C_LONG),"
    filetext[3][1528..1529] = {"\tDefMDIChildProc = define_c_func(user32, \"DefMDIChildProcA\", {C_LONG, C_LONG, C_INT, C_LONG}, C_LONG),",``}
    filetext[3][1530..1531] = {"\tTranslateMDISysAccel = define_c_func(user32, \"TranslateMDISysAccel\", {C_LONG, C_PTR}, C_LONG),",``}
    filetext[3][1532..1533] = {"\tArrangeIconicWindows = define_c_func(user32, \"ArrangeIconicWindows\", {C_LONG}, C_LONG),",``}
    filetext[3][1534] = "\tCreateMDIWindow = define_c_func(user32, \"CreateMDIWindowA\", "
    filetext[3][1535..1537] = {"\t\t{C_PTR,C_PTR,C_LONG,C_LONG,C_LONG,C_LONG,C_LONG,C_LONG,C_LONG,C_LONG},C_LONG)",``,`global constant`}
    filetext[3][1538] = `-- Message Function Templates`
    filetext[3][1539] = "\tGetMessage = define_c_func(user32, \"GetMessageA\", {C_PTR, C_LONG, C_LONG, C_LONG}, C_LONG),"
    filetext[3][1540] = "\tTranslateMessage = define_c_func(user32, \"TranslateMessage\", {C_PTR}, C_LONG),"
    filetext[3][1541] = "\tDispatchMessage = define_c_func(user32, \"DispatchMessageA\", {C_PTR}, C_LONG),"
    filetext[3][1542..1543] = {"\tPeekMessage = define_c_func(user32, \"PeekMessageA\", {C_PTR, C_LONG, C_LONG, C_LONG, C_LONG}, C_LONG),",``}
    filetext[3][1544..1548] = {`-- PeekMessage() Options`,"\tPM_NOREMOVE = #0,","\tPM_REMOVE = #1,","\tPM_NOYIELD = #2,",``}
    filetext[3][1549] = "\tRegisterHotKey = define_c_func(user32, \"RegisterHotKey\", {C_LONG, C_LONG, C_LONG, C_LONG}, C_LONG),"
    filetext[3][1550..1552] = {"\tUnregisterHotKey = define_c_func(user32, \"UnregisterHotKey\", {C_LONG, C_LONG}, C_LONG),",``,"\tMOD_ALT = #1,"}
    filetext[3][1553..1556] = {"\tMOD_CONTROL = #2,","\tMOD_SHIFT = #4,",``,"\tIDHOT_SNAPWINDOW = (-1),\t--\tSHIFT-PRINTSCRN"}
    filetext[3][1557..1561] = {"\tIDHOT_SNAPDESKTOP = (-2),\t --  PRINTSCRN",``,"\tEWX_LOGOFF = 0,","\tEWX_SHUTDOWN = 1,","\tEWX_REBOOT = 2,"}
    filetext[3][1562..1567] = {"\tEWX_FORCE = 4,",``,"\tREADAPI = 0,\t\t--\tFlags for _lopen","\tWRITEAPI = 1,","\tREAD_WRITE = 2,",``}
    filetext[3][1568] = "--\t  ExitWindows = define_c_func(user32, \"ExitWindows\", {C_LONG, C_LONG}, C_LONG),"
    filetext[3][1569..1570] = {"\tExitWindowsEx = define_c_func(user32, \"ExitWindowsEx\", {C_LONG, C_LONG}, C_LONG),",``}
    filetext[3][1571] = "\tSwapMouseButton = define_c_func(user32, \"SwapMouseButton\", {C_INT}, C_INT),"
    filetext[3][1572] = "\tGetMessagePos = define_c_func(user32, \"GetMessagePos\", {}, C_UINT),"
    filetext[3][1573] = "\tGetMessageTime = define_c_func(user32, \"GetMessageTime\", {}, C_LONG),"
    filetext[3][1574] = "\tGetMessageExtraInfo = define_c_func(user32, \"GetMessageExtraInfo\", {}, C_PTR),"
    filetext[3][1575] = "\tSendMessage = define_c_func(user32, \"SendMessageA\", {C_PTR, C_UINT, C_PTR, C_PTR}, C_LONG),"
    filetext[3][1576][1..99] = "\tSendMessageTimeout = define_c_func(user32, \"SendMessageTimeoutA\", {C_LONG, C_LONG, C_INT, C_LONG, "
    filetext[3][1576][100..132] = `C_LONG, C_LONG, C_LONG}, C_LONG),`
    filetext[3][1577] = "\tSendNotifyMessage = define_c_func(user32, \"SendNotifyMessageA\", {C_LONG, C_LONG, C_INT, C_LONG}, C_LONG),"
    filetext[3][1578][1..99] = "\tSendMessageCallback = define_c_func(user32, \"SendMessageCallbackA\", {C_LONG, C_LONG, C_INT, C_LONG"
    filetext[3][1578][100..126] = `, C_LONG, C_LONG}, C_LONG),`
    filetext[3][1579] = "\tPostMessage = define_c_func(user32, \"PostMessageA\", {C_LONG, C_LONG, C_INT, C_LONG}, C_LONG),"
    filetext[3][1580..1581] = {"\tPostThreadMessage = define_c_func(user32, \"PostThreadMessageA\", {C_LONG, C_LONG, C_INT, C_LONG}, C_LONG),",``}
    filetext[3][1582..1585] = {`-- Special HWND value for use with PostMessage and SendMessage`,"\tHWND_BROADCAST = #FFFF",``,``}
    filetext[3][1586..1590] = {`global constant`,`-- Virtual Keys, Standard Set`,"\tVK_LBUTTON = #1,","\tVK_RBUTTON = #2,","\tVK_CANCEL = #3,"}
    filetext[3][1591..1595] = {"\tVK_MBUTTON = #4,\t\t\t --  NOT contiguous with L RBUTTON",``,"\tVK_BACK = #8,","\tVK_TAB = #9,",``}
    filetext[3][1596..1601] = {"\tVK_CLEAR = #C,","\tVK_RETURN = #D,",``,"\tVK_SHIFT = #10,","\tVK_CONTROL = #11,","\tVK_MENU = #12,"}
    filetext[3][1602..1608] = {"\tVK_PAUSE = #13,","\tVK_CAPITAL = #14,",``,"\tVK_ESCAPE = #1B,",``,"\tVK_SPACE = #20,","\tVK_PRIOR = #21,"}
    filetext[3][1609..1614] = {"\tVK_NEXT = #22,","\tVK_END = #23,","\tVK_HOME = #24,","\tVK_LEFT = #25,","\tVK_UP = #26,","\tVK_RIGHT = #27,"}
    filetext[3][1615..1619] = {"\tVK_DOWN = #28,","\tVK_SELECT = #29,","\tVK_PRINT = #2A,","\tVK_EXECUTE = #2B,","\tVK_SNAPSHOT = #2C,"}
    filetext[3][1620..1623] = {"\tVK_INSERT = #2D,","\tVK_DELETE = #2E,","\tVK_HELP = #2F,",``}
    filetext[3][1624] = `-- VK_A thru VK_Z are the same as their ASCII equivalents: 'A' thru 'Z'`
    filetext[3][1625..1627] = {`-- VK_0 thru VK_9 are the same as their ASCII equivalents: '0' thru '9'`,``,"\tVK_NUMPAD0 = #60,"}
    filetext[3][1628..1632] = {"\tVK_NUMPAD1 = #61,","\tVK_NUMPAD2 = #62,","\tVK_NUMPAD3 = #63,","\tVK_NUMPAD4 = #64,","\tVK_NUMPAD5 = #65,"}
    filetext[3][1633..1637] = {"\tVK_NUMPAD6 = #66,","\tVK_NUMPAD7 = #67,","\tVK_NUMPAD8 = #68,","\tVK_NUMPAD9 = #69,","\tVK_MULTIPLY = #6A,"}
    filetext[3][1638..1642] = {"\tVK_ADD = #6B,","\tVK_SEPARATOR = #6C,","\tVK_SUBTRACT = #6D,","\tVK_DECIMAL = #6E,","\tVK_DIVIDE = #6F,"}
    filetext[3][1643..1648] = {"\tVK_F1 = #70,","\tVK_F2 = #71,","\tVK_F3 = #72,","\tVK_F4 = #73,","\tVK_F5 = #74,","\tVK_F6 = #75,"}
    filetext[3][1649..1654] = {"\tVK_F7 = #76,","\tVK_F8 = #77,","\tVK_F9 = #78,","\tVK_F10 = #79,","\tVK_F11 = #7A,","\tVK_F12 = #7B,"}
    filetext[3][1655..1660] = {"\tVK_F13 = #7C,","\tVK_F14 = #7D,","\tVK_F15 = #7E,","\tVK_F16 = #7F,","\tVK_F17 = #80,","\tVK_F18 = #81,"}
    filetext[3][1661..1667] = {"\tVK_F19 = #82,","\tVK_F20 = #83,","\tVK_F21 = #84,","\tVK_F22 = #85,","\tVK_F23 = #86,","\tVK_F24 = #87,",``}
    filetext[3][1668..1671] = {"\tVK_NUMLOCK = #90,","\tVK_SCROLL = #91,",``,`--`}
    filetext[3][1672] = "--\t VK_L VK_R - left and right Alt, Ctrl and Shift virtual keys."
    filetext[3][1673] = "--\t Used only as parameters to GetAsyncKeyState() and GetKeyState()."
    filetext[3][1674..1676] = {"--\t No other API or message will distinguish left and right keys in this way.","--\t/","\tVK_LSHIFT = #A0,"}
    filetext[3][1677..1682] = {"\tVK_RSHIFT = #A1,","\tVK_LCONTROL = #A2,","\tVK_RCONTROL = #A3,","\tVK_LMENU = #A4,","\tVK_RMENU = #A5,",``}
    filetext[3][1683..1687] = {"\tVK_ATTN = #F6,","\tVK_CRSEL = #F7,","\tVK_EXSEL = #F8,","\tVK_EREOF = #F9,","\tVK_PLAY = #FA,"}
    filetext[3][1688..1694] = {"\tVK_ZOOM = #FB,","\tVK_NONAME = #FC,","\tVK_PA1 = #FD,","\tVK_OEM_CLEAR = #FE",``,``,``}
    filetext[3][1695..1696] = {`-- menu-specific commands`,`global constant`}
    filetext[3][1697] = "\tLoadMenu = define_c_func(user32, \"LoadMenuA\", {C_LONG, C_PTR}, C_LONG),"
    filetext[3][1698] = "\tLoadMenuIndirect = define_c_func(user32, \"LoadMenuIndirectA\", {C_LONG}, C_LONG),"
    filetext[3][1699] = "\tGetMenu = define_c_func(user32, \"GetMenu\", {C_LONG}, C_LONG),"
    filetext[3][1700] = "\tSetMenu = define_c_func(user32, \"SetMenu\", {C_LONG, C_LONG}, C_LONG),"
    filetext[3][1701] = "\tHiliteMenuItem = define_c_func(user32, \"HiliteMenuItem\", {C_LONG, C_LONG, C_LONG, C_LONG}, C_LONG),"
    filetext[3][1702] = "\tGetMenuString = define_c_func(user32, \"GetMenuStringA\", {C_LONG, C_LONG, C_PTR, C_LONG, C_LONG}, C_LONG),"
    filetext[3][1703] = "\tGetMenuState = define_c_func(user32, \"GetMenuState\", {C_LONG, C_LONG, C_LONG}, C_LONG),"
    filetext[3][1704] = "\tDrawMenuBar = define_c_func(user32, \"DrawMenuBar\", {C_LONG}, C_LONG),"
    filetext[3][1705] = "\tGetSystemMenu = define_c_func(user32, \"GetSystemMenu\", {C_LONG, C_LONG}, C_LONG),"
    filetext[3][1706] = "\tCreateMenu = define_c_func(user32, \"CreateMenu\", {}, C_LONG),"
    filetext[3][1707] = "\tCreatePopupMenu = define_c_func(user32, \"CreatePopupMenu\", {}, C_LONG),"
    filetext[3][1708] = "\tDestroyMenu = define_c_func(user32, \"DestroyMenu\", {C_LONG}, C_LONG),"
    filetext[3][1709] = "\tCheckMenuItem = define_c_func(user32, \"CheckMenuItem\", {C_LONG, C_LONG, C_LONG}, C_LONG),"
    filetext[3][1710][1..99] = "\tCheckMenuRadioItem = define_c_func(user32, \"CheckMenuRadioItem\", {C_LONG, C_LONG, C_LONG, C_LONG, "
    filetext[3][1710][100..116] = `C_LONG}, C_LONG),`
    filetext[3][1711] = "\tEnableMenuItem = define_c_func(user32, \"EnableMenuItem\", {C_LONG, C_LONG, C_LONG}, C_LONG),"
    filetext[3][1712] = "\tGetSubMenu = define_c_func(user32, \"GetSubMenu\", {C_LONG, C_LONG}, C_LONG),"
    filetext[3][1713] = "\tGetMenuItemID = define_c_func(user32, \"GetMenuItemID\", {C_LONG, C_LONG}, C_LONG),"
    filetext[3][1714..1715] = {"\tGetMenuItemCount = define_c_func(user32, \"GetMenuItemCount\", {C_LONG}, C_LONG),",``}
    filetext[3][1716] = "\tInsertMenu = define_c_func(user32, \"InsertMenuA\", {C_LONG, C_LONG, C_LONG, C_LONG, C_PTR}, C_LONG),"
    filetext[3][1717] = "\tAppendMenu = define_c_func(user32, \"AppendMenuA\", {C_LONG, C_LONG, C_LONG, C_PTR}, C_LONG),"
    filetext[3][1718] = "\tModifyMenu = define_c_func(user32, \"ModifyMenuA\", {C_LONG, C_LONG, C_LONG, C_LONG, C_PTR}, C_LONG),"
    filetext[3][1719] = "\tRemoveMenu = define_c_func(user32, \"RemoveMenu\", {C_LONG, C_LONG, C_LONG}, C_LONG),"
    filetext[3][1720] = "\tDeleteMenu = define_c_func(user32, \"DeleteMenu\", {C_LONG, C_LONG, C_LONG}, C_LONG),"
    filetext[3][1721][1..99] = "\tSetMenuItemBitmaps = define_c_func(user32, \"SetMenuItemBitmaps\", {C_LONG, C_LONG, C_LONG, C_LONG, "
    filetext[3][1721][100..116] = `C_LONG}, C_LONG),`
    filetext[3][1722] = "\tGetMenuCheckMarkDimensions = define_c_func(user32, \"GetMenuCheckMarkDimensions\", {}, C_LONG),"
    filetext[3][1723][1..99] = "\tTrackPopupMenu = define_c_func(user32, \"TrackPopupMenu\", {C_LONG, C_LONG, C_LONG, C_LONG, C_LONG, "
    filetext[3][1723][100..123] = `C_LONG, C_PTR}, C_LONG),`
    filetext[3][1724..1728] = {``,`-- Menu flags for Add/Check/EnableMenuItem()`,"\tMF_INSERT = #0,","\tMF_CHANGE = #80,","\tMF_APPEND = #100,"}
    filetext[3][1729..1734] = {"\tMF_DELETE = #200,","\tMF_REMOVE = #1000,",``,"\tMF_BYCOMMAND = #0,","\tMF_BYPOSITION = #400,",``}
    filetext[3][1735..1740] = {"\tMF_SEPARATOR = #800,",``,"\tMF_ENABLED = #0,","\tMF_GRAYED = #1,","\tMF_DISABLED = #2,",``}
    filetext[3][1741..1745] = {"\tMF_UNCHECKED = #0,","\tMF_CHECKED = #8,","\tMF_USECHECKBITMAPS = #200,",``,"\tMF_STRING = #0,"}
    filetext[3][1746..1750] = {"\tMF_BITMAP = #4,","\tMF_OWNERDRAW = #100,",``,"\tMF_POPUP = #10,","\tMF_MENUBARBREAK = #20,"}
    filetext[3][1751..1756] = {"\tMF_MENUBREAK = #40,",``,"\tMF_UNHILITE = #0,","\tMF_HILITE = #80,",``,"\tMF_SYSMENU = #2000,"}
    filetext[3][1757..1761] = {"\tMF_HELP = #4000,","\tMF_MOUSESELECT = #8000,",``,`-- Flags for TrackPopupMenu`,"\tTPM_LEFTBUTTON = #0,"}
    filetext[3][1762..1767] = {"\tTPM_RIGHTBUTTON = #2,","\tTPM_LEFTALIGN = #0,","\tTPM_CENTERALIGN = #4,","\tTPM_RIGHTALIGN = #8",``,``}
    filetext[3][1768..1769] = {`-- message box stuff`,`global constant`}
    filetext[3][1770] = "\tMessageBox = define_c_func(user32, \"MessageBoxA\", {C_LONG, C_PTR, C_PTR, C_LONG}, C_LONG),"
    filetext[3][1771..1772] = {"\tMessageBoxEx = define_c_func(user32, \"MessageBoxExA\", {C_LONG, C_PTR, C_PTR, C_LONG, C_LONG}, C_LONG),",``}
    filetext[3][1773..1776] = {`-- MessageBox() Flags`,"\tMB_OK = #0,","\tMB_OKCANCEL = #1,","\tMB_ABORTRETRYIGNORE = #2,"}
    filetext[3][1777..1781] = {"\tMB_YESNOCANCEL = #3,","\tMB_YESNO = #4,","\tMB_RETRYCANCEL = #5,",``,"\tMB_ICONHAND = #10,"}
    filetext[3][1782..1785] = {"\tMB_ICONQUESTION = #20,","\tMB_ICONEXCLAMATION = #30,","\tMB_ICONASTERISK = #40,",``}
    filetext[3][1786..1789] = {"\tMB_ICONINFORMATION = MB_ICONASTERISK,","\tMB_ICONSTOP = MB_ICONHAND,",``,"\tMB_DEFBUTTON1 = #0,"}
    filetext[3][1790..1794] = {"\tMB_DEFBUTTON2 = #100,","\tMB_DEFBUTTON3 = #200,",``,"\tMB_APPLMODAL = #0,","\tMB_SYSTEMMODAL = #1000,"}
    filetext[3][1795..1798] = {"\tMB_TASKMODAL = #2000,",``,"\tMB_NOFOCUS = #8000,","\tMB_SETFOREGROUND = #10000,"}
    filetext[3][1799..1803] = {"\tMB_DEFAULT_DESKTOP_ONLY = #20000,",``,"\tMB_TYPEMASK = #F,","\tMB_ICONMASK = #F0,","\tMB_DEFMASK = #F00,"}
    filetext[3][1804..1808] = {"\tMB_MODEMASK = #3000,","\tMB_MISCMASK = #C000",``,`-- Status Box flags`,`global constant`}
    filetext[3][1809..1812] = {"\tSB_GETBORDERS = 1031,","\tSB_GETPARTS = 1030,","\tSB_GETRECT = 1034,","\tSB_GETTEXTW = 1037,"}
    filetext[3][1813..1816] = {"\tSB_GETTEXTLENGTHW = 1036,","\tSB_SETTEXTW = 1035,","\tSB_GETTEXTA = 1026,","\tSB_GETTEXTLENGTHA = 1027,"}
    filetext[3][1817..1821] = {"\tSB_SETTEXTA = 1025,","\tSB_SETMINHEIGHT = 1032,","\tSB_SETPARTS = 1028,","\tSB_SIMPLE = 1033,",``}
    filetext[3][1822..1825] = {`-- Scroll Bar constants`,"\tSB_HORZ = 0,","\tSB_VERT = 0",``}
    filetext[3][1826..1829] = {`global constant SIZE_OF_MESSAGE = w_sizeof("pdppddd")`,``,``,`--constant SIZE_OF_WNDCLASS = 48`}
    filetext[3][1830..1831] = {`global function RegisterClass(sequence info)`,`-- info = {style, lpfnWndProc, cbClsExtra, cbWndExtra, hInstance,`}
    filetext[3][1832..1834] = {"--\t hIcon, hCursor, hbrBackground, lpszMenuName, lpszClassName, hIconSm}","\tatom WndClass, result","\t"}
    filetext[3][1835..1836] = {"\tWndClass = allocate_pack(\"zdpiippppssp\", info)","\tresult = c_func(RegisterClassEx, {WndClass})"}
    filetext[3][1837..1840] = {"\tif result = 0 then","\t\tprintf(1, \"%x\\n\", {c_func(GetLastError, {})})","\tend if","\tfree_strings()"}
    filetext[3][1841..1845] = {"\tfree(WndClass)","\treturn result",`end function`,``,`global function CreateWindow(sequence info)`}
    filetext[3][1846] = `-- info = {dwExStyle, lpClassName, lpWindowName, dwStyle, x, y, nWidth, nHeight,`
    filetext[3][1847..1849] = {"--\t hWndParent, hMenu, hInstance, lpParam}","\tatom result","\tif sequence(info[2]) then"}
    filetext[3][1850..1852] = {"\t\tinfo[2] = alloc_string(info[2])  -- lpClassName","\tend if","\tif sequence(info[3]) then"}
    filetext[3][1853..1855] = {"\t\tinfo[3] = alloc_string(info[3])  -- lpWindowName","\tend if","\tif sequence(info[4]) then"}
    filetext[3][1856..1859] = {"\t\tinfo[4] = or_all(info[4])  -- style","\tend if",``,"\tresult = c_func(CreateWindowEx, info)"}
    filetext[3][1860..1863] = {"\tif result = 0 then","\t\tprintf(1, \"%x\\n\", {c_func(GetLastError, {})})","\tend if","\tfree_strings()"}
    filetext[3][1864..1869] = {"\treturn result",`end function`,``,``,``,`function allocDialogTemplate(sequence info)`}
    filetext[3][1870] = "--\tatom data, ptr, style, exstyle, cdit, id, x, y, cx, cy, class, result"
    filetext[3][1871..1874] = {"\tatom data, ptr, style, exstyle, cdit, id, x, y, cx, cy, class","\tsequence text",``,"\tdata = allocate(1024)"}
    filetext[3][1875..1880] = {"\tptr = data",``,"\tcdit = length(info)-1",``,"\tfor i = 1 to length(info) do","\t\tif sequence(info[i][1]) then"}
    filetext[3][1881..1884] = {"\t\t\tstyle = or_all(info[i][1])","\t\telse","\t\t\tstyle = info[i][1]","\t\tend if"}
    filetext[3][1885..1888] = {"\t\tif sequence(info[i][2]) then","\t\t\texstyle = or_all(info[i][2])","\t\telse","\t\t\texstyle = info[i][2]"}
    filetext[3][1889..1893] = {"\t\tend if","\t\tx = info[i][3]","\t\ty = info[i][4]","\t\tcx = info[i][5]","\t\tcy = info[i][6]"}
    filetext[3][1894..1897] = {"\t\tid = info[i][7] -- menu or id","\t\tclass = info[i][8] -- window or dialog class","\t\ttext = info[i][9]",``}
    filetext[3][1898..1900] = {"\t\t-- struct DLGITEMTEMPLATE or DLGITEMTEMPLATE","\t\tpoke4(ptr, {","\t\t\tstyle,\t\t\t-- DWORD style;"}
    filetext[3][1901..1905] = {"\t\t\texstyle \t\t-- DWORD dwExtendedStyle;","\t\t})","\t\tptr += 8",``,"\t\tif i = 1 then"}
    filetext[3][1906..1910] = {"\t\t\tpoke(ptr, {cdit, 0}) -- WORD cdit;","\t\t\tptr += 2","\t\tend if",``,"\t\tpoke4(ptr, {"}
    filetext[3][1911..1912] = {"\t\t\tx + y * #10000, -- short x; short y;","\t\t\tcx+cy * #10000, -- short cx; short cy;"}
    filetext[3][1913..1918] = {"\t\t\tid\t\t\t\t-- WORD id; or menu","\t\t})","\t\tptr += 10",``,"\t\tif i = 1 then","\t\t\t-- window class"}
    filetext[3][1919..1922] = {"\t\t\tpoke4(ptr, class)","\t\t\tptr += 2","\t\telse ","\t\t\t-- ordinal class"}
    filetext[3][1923..1927] = {"\t\t\tpoke4(ptr, #FFFF + class * #10000)","\t\t\tptr += 4","\t\tend if",``,"\t\tpokeWideCharString(ptr, text)"}
    filetext[3][1928..1932] = {"\t\tptr += 2 * length(text) + 2",``,"\t\tif i != 1 then","\t\t\t-- no creation data","\t\t\tpoke(ptr, {0,0})"}
    filetext[3][1933..1941] = {"\t\t\tptr += 2","\t\tend if","\t\tptr = align(ptr, 4)",``,"\tend for","\treturn data",`end function`,``,``}
    filetext[3][1942] = `global function DialogBoxIndirectParam(atom inst, object info, atom hWndParent, atom dialogFunc, atom param)`
    filetext[3][1943..1945] = {"\tatom data, result",``,"\tif atom(info) then"}
    filetext[3][1946..1948] = {"\t\treturn c_func(DialogBoxIndirectParam_, {inst, info, hWndParent, dialogFunc, param})","\tend if",``}
    filetext[3][1949] = "\tdata = allocDialogTemplate(info)"
    filetext[3][1950..1951] = {"\tresult = c_func(DialogBoxIndirectParam_, {inst, data, hWndParent, dialogFunc, param})","\tfree(data)"}
    filetext[3][1952..1954] = {"\treturn result",`end function`,``}
    filetext[3][1955] = `global function CreateDialogIndirectParam(atom inst, object info, atom hWndParent, atom dialogFunc, atom param)`
    filetext[3][1956..1958] = {"\tatom data, result",``,"\tif atom(info) then"}
    filetext[3][1959..1961] = {"\t\treturn c_func(CreateDialogIndirectParam_, {inst, info, hWndParent, dialogFunc, param})","\tend if",``}
    filetext[3][1962] = "\tdata = allocDialogTemplate(info)"
    filetext[3][1963..1964] = {"\tresult = c_func(CreateDialogIndirectParam_, {inst, data, hWndParent, dialogFunc, param})","\tfree(data)"}
    filetext[3][1965..1971] = {"\treturn result",`end function`,``,``,``,`-- open/save filename common dialogs`,`global constant`}
    filetext[3][1972] = "\tcomdlg32 = open_dll(\"comdlg32.dll\"),"
    filetext[3][1973] = "\tGetOpenFileName_ = define_c_func(comdlg32, \"GetOpenFileNameA\", {C_PTR}, C_LONG),"
    filetext[3][1974] = "\tGetSaveFileName_ = define_c_func(comdlg32, \"GetSaveFileNameA\", {C_PTR}, C_LONG),"
    filetext[3][1975..1976] = {"\tGetFileTitle_ = define_c_func(comdlg32, \"GetFileTitleA\", {C_PTR, C_PTR, C_INT}, C_INT),",``}
    filetext[3][1977..1980] = {"\tOFN_READONLY = #1,","\tOFN_OVERWRITEPROMPT = #2,","\tOFN_HIDEREADONLY = #4,","\tOFN_NOCHANGEDIR = #8,"}
    filetext[3][1981..1983] = {"\tOFN_SHOWHELP = #10,","\tOFN_ENABLEHOOK = #20,","\tOFN_ENABLETEMPLATE = #40,"}
    filetext[3][1984..1986] = {"\tOFN_ENABLETEMPLATEHANDLE = #80,","\tOFN_NOVALIDATE = #100,","\tOFN_ALLOWMULTISELECT = #200,"}
    filetext[3][1987..1989] = {"\tOFN_EXTENSIONDIFFERENT = #400,","\tOFN_PATHMUSTEXIST = #800,","\tOFN_FILEMUSTEXIST = #1000,"}
    filetext[3][1990..1992] = {"\tOFN_CREATEPROMPT = #2000,","\tOFN_SHAREAWARE = #4000,","\tOFN_NOREADONLYRETURN = #8000,"}
    filetext[3][1993..1994] = {"\tOFN_NOTESTFILECREATE = #10000,","\tOFN_NONETWORKBUTTON = #20000,"}
    filetext[3][1995] = "\tOFN_NOLONGNAMES = #40000,\t\t\t\t\t   --  force no long names for 4.x modules"
    filetext[3][1996..1997] = {"\tOFN_EXPLORER = #80000,\t\t\t\t\t\t   --  new look commdlg","\tOFN_NODEREFERENCELINKS = #100000,"}
    filetext[3][1998..2000] = {"\tOFN_LONGNAMES = #200000,\t\t\t\t\t   --  force long names for 3.x modules",``,"\tOFN_SHAREFALLTHROUGH = 2,"}
    filetext[3][2001..2004] = {"\tOFN_SHARENOWARN = 1,","\tOFN_SHAREWARN = 0",``,`function peek_multi_strings(atom ptr)`}
    filetext[3][2005..2008] = {"\tsequence path, name, result","\tpath = peek_string(ptr)","\tptr += length(path) + 1","\tresult = {}"}
    filetext[3][2009..2013] = {"\tname = peek_string(ptr)","\tif length(name) = 0 then","\t\treturn path","\tend if","\twhile length(name) do"}
    filetext[3][2014..2016] = {"\t\tresult = append(result, path & \'\\\\\' & name)","\t\tptr += length(name) + 1","\t\tname = peek_string(ptr)"}
    filetext[3][2017..2022] = {"\tend while","\treturn result",`end function`,``,``,`function GetFileNameDialog(atom func, sequence info)`}
    filetext[3][2023] = `-- info = {hwndOwner, hInstance, lpstrFilter, nFilterIndex, lpstrFile, `
    filetext[3][2024] = "--\t lpstrInitialDir, lpstrTitle, Flags, lpstrDefExt}"
    filetext[3][2025..2027] = {`-- returns: {nFilterIndex, FileName, Flags, nFileOffset, nFileExtension}`,"\tatom struc","\tobject result"}
    filetext[3][2028..2031] = {"\tinteger maxsize","\tmaxsize = 260","\tif and_bits(info[8], OFN_ALLOWMULTISELECT) then","\t\tmaxsize = 4096"}
    filetext[3][2032..2034] = {"\tend if","\tif compare(info[5], 0) then ","\t\tinfo[5] = repeat(0, maxsize)"}
    filetext[3][2035..2037] = {"\telsif length(info[5]) < maxsize then","\t\tinfo[5] = info[5] & repeat(0, maxsize - length(info[5]))","\tend if"}
    filetext[3][2038..2040] = {``,"\tstruc = allocate_pack(\"zppspddsdppssdwwsppp\", {","\t\tinfo[1],\t\t -- hwndOwner"}
    filetext[3][2041..2043] = {"\t\tinfo[2],\t\t -- hInstance","\t\tinfo[3],\t\t -- lpstrFilter","\t\t0,\t\t\t\t -- lpstrCustomFilter"}
    filetext[3][2044..2046] = {"\t\t0,\t\t\t\t -- nMaxCustFilter","\t\tinfo[4],\t\t -- nFilterIndex","\t\tinfo[5],\t\t -- lpstrFile"}
    filetext[3][2047..2049] = {"\t\tlength(info[5]), -- nMaxFile","\t\t0,\t\t\t\t -- lpstrFileTitle","\t\t0,\t\t\t\t -- nMaxFileTitle"}
    filetext[3][2050..2052] = {"\t\tinfo[6],\t\t -- lpstrInitialDir","\t\tinfo[7],\t\t -- lpstrTitle","\t\tinfo[8],\t\t -- Flags"}
    filetext[3][2053..2055] = {"\t\t0,\t\t\t\t -- nFileOffset","\t\t0,\t\t\t\t -- nFileExtension","\t\tinfo[9],\t\t -- lpstrDefExt"}
    filetext[3][2056..2059] = {"\t\t0,\t\t\t\t -- lCustData","\t\t0,\t\t\t\t -- lpfnHook","\t\t0}) \t\t\t -- lpTemplateName","\t"}
    filetext[3][2060..2061] = {"\tif c_func(func, {struc}) then","\t\tresult = unpack(struc, \"zppppddpdppppdwwpppp\")"}
    filetext[3][2062..2064] = {"\t\tif and_bits(info[8], OFN_ALLOWMULTISELECT) then","\t\t\tresult[8] = peek_multi_strings(result[8])","\t\telse"}
    filetext[3][2065..2068] = {"\t\t\tresult[8] = peek_string(result[8])","\t\tend if","\t\tresult = {","\t\t\tresult[7], -- nFilterIndex"}
    filetext[3][2069..2071] = {"\t\t\tresult[8], -- lpstrFile","\t\t\tresult[14], -- Flags","\t\t\tresult[15], -- nFileOffset"}
    filetext[3][2072..2077] = {"\t\t\tresult[16]} -- nFileExtension","\telse","\t\tresult = {}","\tend if","\tfree(struc)","\tfree_strings()"}
    filetext[3][2078..2081] = {"\treturn result",`end function`,``,`global function GetOpenFileName(sequence info)`}
    filetext[3][2082] = `-- info = {hwndOwner, hInstance, lpstrFilter, nFilterIndex, lpstrFile, `
    filetext[3][2083] = "--\t lpstrInitialDir, lpstrTitle, Flags, lpstrDefExt}"
    filetext[3][2084] = `-- returns: {nFilterIndex, FileName, Flags, nFileOffset, nFileExtension}`
    filetext[3][2085..2087] = {"\treturn GetFileNameDialog(GetOpenFileName_, info)",`end function`,``}
    filetext[3][2088] = `global function GetSaveFileName(sequence info)`
    filetext[3][2089] = `-- info = {hwndOwner, hInstance, lpstrFilter, nFilterIndex, lpstrFile, `
    filetext[3][2090] = "--\t lpstrInitialDir, lpstrTitle, Flags, lpstrDefExt}"
    filetext[3][2091] = `-- returns: {nFilterIndex, FileName, Flags, nFileOffset, nFileExtension}`
    filetext[3][2092..2096] = {"\treturn GetFileNameDialog(GetSaveFileName_, info)",`end function`,``,``,`global constant`}
    filetext[3][2097..2098] = {"\tFILEOKSTRING = \"commdlg_FileNameOK\",","\tCOLOROKSTRING = \"commdlg_ColorOK\","}
    filetext[3][2099..2100] = {"\tSETRGBSTRING = \"commdlg_SetRGBColor\",","\tHELPMSGSTRING = \"commdlg_help\","}
    filetext[3][2101..2104] = {"\tFINDMSGSTRING = \"commdlg_FindReplace\"",``,`-- find/replace common dialogs`,`global constant`}
    filetext[3][2105..2109] = {"\tFR_DOWN = #1,","\tFR_WHOLEWORD = #2,","\tFR_MATCHCASE = #4,","\tFR_FINDNEXT = #8,","\tFR_REPLACE = #10,"}
    filetext[3][2110..2113] = {"\tFR_REPLACEALL = #20,","\tFR_DIALOGTERM = #40,","\tFR_SHOWHELP = #80,","\tFR_ENABLEHOOK = #100,"}
    filetext[3][2114..2117] = {"\tFR_ENABLETEMPLATE = #200,","\tFR_NOUPDOWN = #400,","\tFR_NOMATCHCASE = #800,","\tFR_NOWHOLEWORD = #1000,"}
    filetext[3][2118..2120] = {"\tFR_ENABLETEMPLATEHANDLE = #2000,","\tFR_HIDEUPDOWN = #4000,","\tFR_HIDEMATCHCASE = #8000,"}
    filetext[3][2121..2123] = {"\tFR_HIDEWHOLEWORD = #10000,",``,"\tFindText_ = define_c_func(comdlg32, \"FindTextA\", {C_PTR}, C_LONG),"}
    filetext[3][2124..2126] = {"\tReplaceText_ = define_c_func(comdlg32, \"ReplaceTextA\", {C_PTR}, C_LONG)",``,`constant `}
    filetext[3][2127..2128] = {"\tstruct_FINDREPLACE = \"zppdsswwppp\",","\tSIZE_OF_FINDREPLACE = w_sizeof(struct_FINDREPLACE),"}
    filetext[3][2129..2130] = {"\tFlags_offset = w_sizeof(struct_FINDREPLACE[1..3]),","\tFindWhat_offset = w_sizeof(struct_FINDREPLACE[1..4]), "}
    filetext[3][2131..2132] = {"\tReplaceWith_offset = w_sizeof(struct_FINDREPLACE[1..5])","\t-- 0  DWORD \t   lStructSize;"}
    filetext[3][2133..2135] = {"\t-- 4  HWND\t\t   hwndOwner;","\t-- 8  HINSTANCE    hInstance;","\t-- 12 DWORD \t   Flags;"}
    filetext[3][2136..2138] = {"\t-- 16 LPTSTR\t   lpstrFindWhat;","\t-- 20 LPTSTR\t   lpstrReplaceWith;","\t-- 24 WORD\t\t   wFindWhatLen;"}
    filetext[3][2139..2141] = {"\t-- 26 WORD\t\t   wReplaceWithLen;","\t-- 28 LPARAM\t   lCustData;","\t-- 32 LPFRHOOKPROC lpfnHook;"}
    filetext[3][2142..2144] = {"\t-- 36 LPCTSTR\t   lpTemplateName;",``,`global function GetFindFlags(atom struc)`}
    filetext[3][2145..2148] = {"\treturn peek4u(struc + Flags_offset)",`end function`,``,`global function GetFindWhat(atom struc)`}
    filetext[3][2149..2153] = {"\tsequence tmp","\ttmp = unpack(struc + FindWhat_offset, \"s\")","\treturn tmp[1]",`end function`,``}
    filetext[3][2154..2156] = {`global function GetReplaceWith(atom struc)`,"\tsequence tmp","\ttmp = unpack(struc + ReplaceWith_offset, \"s\")"}
    filetext[3][2157..2163] = {"\treturn tmp[1]",`end function`,``,`atom find_struc, hwnd_find`,`find_struc = 0`,`hwnd_find = 0`,``}
    filetext[3][2164][1..99] = `function FindReplaceText(atom func, atom hwndOwner, atom hInstance, atom Flags, sequence FindWhat, `
    filetext[3][2164][100..120] = `sequence ReplaceWith)`
    filetext[3][2165..2168] = {`-- returns: hwndFindDialog`,"\tatom junk","\tinteger Len","\tLen = 256"}
    filetext[3][2169] = `--/**/ if hInstance then end if -- suppress warnings`
    filetext[3][2170] = "\tif length(FindWhat) >= Len then FindWhat = FindWhat[1..Len-1] end if"
    filetext[3][2171..2172] = {"\tif length(ReplaceWith) >= Len then ReplaceWith = ReplaceWith[1..Len-1] end if","\tif find_struc then"}
    filetext[3][2173..2176] = {"\t\tif hwnd_find then","\t\t\tjunk = c_func(DestroyWindow, {hwnd_find})","\t\tend if","\telse"}
    filetext[3][2177..2178] = {"\t\tfind_struc = allocate(SIZE_OF_FINDREPLACE + Len + Len)","\t\tpack(find_struc, struct_FINDREPLACE, {"}
    filetext[3][2179..2182] = {"\t\t\tSIZE_OF_FINDREPLACE,","\t\t\thwndOwner,","\t\t\t0, -- hInstance","\t\t\tFlags,"}
    filetext[3][2183] = "\t\t\tfind_struc + SIZE_OF_FINDREPLACE, -- lpstrFindWhat"
    filetext[3][2184..2185] = {"\t\t\tfind_struc + SIZE_OF_FINDREPLACE + Len, --lpstrReplaceWith","\t\t\tLen, -- wFindWhatLen"}
    filetext[3][2186..2188] = {"\t\t\tLen, -- wReplaceWithLen","\t\t\tNULL, -- lCustData","\t\t\tNULL, -- lpfnHook"}
    filetext[3][2189..2193] = {"\t\t\tNULL -- lpTemplateName","\t\t\t})","\tend if","\tpack(find_struc, \"zppd\", {","\t\tSIZE_OF_FINDREPLACE,"}
    filetext[3][2194..2197] = {"\t\thwndOwner,","\t\t0,","\t\tFlags})","\tpoke(find_struc + SIZE_OF_FINDREPLACE, FindWhat & 0)"}
    filetext[3][2198..2200] = {"\tpoke(find_struc + SIZE_OF_FINDREPLACE + Len, ReplaceWith & 0)","\t","\thwnd_find = c_func(func, {find_struc})"}
    filetext[3][2201..2205] = {"\tif hwnd_find = 0 then","\t\tfree(find_struc)","\t\tfind_struc = 0","\tend if","\treturn hwnd_find"}
    filetext[3][2206..2208] = {`end function`,``,`global function FindText(atom hwndOwner, atom hInstance, atom Flags, sequence FindWhat)`}
    filetext[3][2209..2210] = {`-- returns: hwndFindDialog`,"\treturn FindReplaceText(FindText_, hwndOwner, hInstance, Flags, FindWhat, \"\")"}
    filetext[3][2211..2212] = {`end function`,``}
    filetext[3][2213][1..99] = `global function ReplaceText(atom hwndOwner, atom hInstance, atom Flags, sequence FindWhat, sequence`
    filetext[3][2213][100..112] = ` ReplaceWith)`
    filetext[3][2214] = `-- returns: hwndFindDialog`
    filetext[3][2215..2218] = {"\treturn FindReplaceText(ReplaceText_, hwndOwner, hInstance, Flags, FindWhat, ReplaceWith)",`end function`,``,``}
    filetext[3][2219..2220] = {`-- choose font common dialog`,`global constant`}
    filetext[3][2221..2222] = {"\tChooseFont_ = define_c_func(comdlg32, \"ChooseFontA\", {C_PTR}, C_INT),","\tCF_SCREENFONTS = 1,"}
    filetext[3][2223..2227] = {"\tCF_PRINTERFONTS = 2,","\tCF_BOTH = 3,","\tCF_SHOWHELP = 4,","\tCF_ENABLEHOOK = 8,","\tCF_ENABLETEMPLATE = 16,"}
    filetext[3][2228..2231] = {"\tCF_ENABLETEMPLATEHANDLE = 32,","\tCF_INITTOLOGFONTSTRUCT = 64,","\tCF_USESTYLE = 128,","\tCF_EFFECTS = 256,"}
    filetext[3][2232..2235] = {"\tCF_APPLY = 512,","\tCF_ANSIONLY = 1024,","\tCF_SCRIPTSONLY = CF_ANSIONLY,","\tCF_NOVECTORFONTS = 2048,"}
    filetext[3][2236..2239] = {"\tCF_NOOEMFONTS = 2048,","\tCF_NOSIMULATIONS = 4096,","\tCF_LIMITSIZE = 8192,","\tCF_FIXEDPITCHONLY = 16384,"}
    filetext[3][2240..2243] = {"\tCF_WYSIWYG = 32768,","\tCF_FORCEFONTEXIST = 65536,","\tCF_SCALABLEONLY = 131072,","\tCF_TTONLY = 262144,"}
    filetext[3][2244..2247] = {"\tCF_NOFACESEL = 524288,","\tCF_NOSTYLESEL = 1048576,","\tCF_NOSIZESEL = 2097152,","\tCF_SELECTSCRIPT = 4194304,"}
    filetext[3][2248..2250] = {"\tCF_NOSCRIPTSEL = 8388608,","\tCF_NOVERTFONTS = #1000000,","\tSIMULATED_FONTTYPE = #8000,"}
    filetext[3][2251..2254] = {"\tPRINTER_FONTTYPE = #4000,","\tSCREEN_FONTTYPE = #2000,","\tBOLD_FONTTYPE = #100,","\tITALIC_FONTTYPE = #0200,"}
    filetext[3][2255..2256] = {"\tREGULAR_FONTTYPE = #0400,","\tWM_CHOOSEFONT_GETLOGFONT = (WM_USER+1),"}
    filetext[3][2257..2259] = {"\tWM_CHOOSEFONT_SETLOGFONT = (WM_USER+101),","\tWM_CHOOSEFONT_SETFLAGS = (WM_USER+102)",``}
    filetext[3][2260] = `global function ChooseFont(atom hwndOwner, sequence font_name, integer font_height)`
    filetext[3][2261..2264] = {"--\tatom cf, lf, size, lf_facesize, lf_size, hedit","\tatom cf, lf, lf_facesize, lf_size","\tsequence result",``}
    filetext[3][2265..2267] = {"\tlf_facesize = 32","\tlf_size = 4*5 + 8 + lf_facesize","\tlf = allocate(lf_size)"}
    filetext[3][2268..2270] = {"\tpoke4(lf, {-floor((font_height*96+36)/72),0,0,0,0,0,0})","\tpoke(lf+lf_size-lf_facesize, font_name&0)",``}
    filetext[3][2271..2273] = {"\tcf = allocate_pack(\"zpppiddpppppwii$\", {","\t\thwndOwner, -- hwndOwner;","\t\tNULL, -- hDC;"}
    filetext[3][2274..2277] = {"\t\tlf, -- lpLogFont;","\t\t0, -- iPointSize","\t\tCF_INITTOLOGFONTSTRUCT, -- Flags","\t\t0, -- rgbColors"}
    filetext[3][2278..2281] = {"\t\tNULL, -- lCustData","\t\tNULL, -- lpfnHook","\t\tNULL, -- lpTemplateName","\t\tNULL, -- hInstance"}
    filetext[3][2282..2287] = {"\t\tNULL, -- lpszStyle","\t\t0, -- nFontType","\t\t0, -- nSizeMin","\t\t0  -- nSizeMax","\t\t})","\tresult = {}"}
    filetext[3][2288..2289] = {"\tif c_func(ChooseFont_, {cf}) then","\t\tfont_name = peek_string(lf+lf_size-lf_facesize)"}
    filetext[3][2290..2291] = {"\t\tfont_height = floor(peek4u(cf + w_sizeof(\"zppp\")) / 10)","\t\t--? font_height"}
    filetext[3][2292..2293] = {"\t\t--printf(1, \"height=%d %d width=%d weight=%d facename=%s pointsize=%d\\n\", {","\t\t--\t  peek4s(lf),"}
    filetext[3][2294..2296] = {"\t\t--\t -floor((font_height*96+36)/72),","\t\t--\t  peek4u(lf+4),","\t\t--\t  peek4u(lf+16),"}
    filetext[3][2297..2301] = {"\t\t--\t  font_name,","\t\t--\t  peek4u(cf+16)})","\t\tresult = {font_name, font_height}","\tend if","\tfree(cf)"}
    filetext[3][2302..2308] = {"\tfree(lf)","\treturn result",`end function`,``,``,`-- choose color common dialog`,`global constant`}
    filetext[3][2309..2310] = {"\tChooseColor_ = define_c_func(comdlg32, \"ChooseColorA\", {C_PTR}, C_INT),","\tCC_ANYCOLOR = #100,"}
    filetext[3][2311..2314] = {"\tCC_ENABLEHOOK = #10,","\tCC_ENABLETEMPLATE = #20,","\tCC_ENABLETEMPLATEHANDLE = #40,","\tCC_FULLOPEN = #2,"}
    filetext[3][2315..2319] = {"\tCC_PREVENTFULLOPEN = #4,","\tCC_RGBINIT = #1,","\tCC_SHOWHELP = #8,","\tCC_SOLIDCOLOR = #80",``}
    filetext[3][2320..2323] = {`sequence cur_palette`,`cur_palette = repeat(0, 16)`,``,`global function ChooseColor(atom hwnd, integer rgb)`}
    filetext[3][2324..2327] = {"\tatom struc, pal","\tsequence result","\tpal = allocate(4*length(cur_palette))","\tpoke4(pal, cur_palette)"}
    filetext[3][2328..2331] = {"\tstruc = allocate_pack(\"zppdpdppp\", {","\t\thwnd, -- hwndOwner","\t\t0, -- hInstance","\t\trgb, -- rgbResult"}
    filetext[3][2332..2335] = {"\t\tpal, -- lpCustColors","\t\tCC_RGBINIT+CC_FULLOPEN, -- Flags","\t\t0, -- lpCustData","\t\t0, -- lpfnHook"}
    filetext[3][2336..2338] = {"\t\t0 -- lpTemplateName","\t\t})","\tif c_func(ChooseColor_, {struc}) != 0 then"}
    filetext[3][2339..2341] = {"\t\tresult = unpack(struc, \"zppdpdppp\")","\t\trgb = result[4]","\tend if"}
    filetext[3][2342..2346] = {"\tcur_palette = peek4u({pal, length(cur_palette)})","\tfree(struc)","\tfree(pal)","\treturn rgb",`end function`}
    filetext[3][2347..2351] = {``,``,``,`----------------------------------------------------------------------------`,`global constant `}
    filetext[3][2352] = "\tSPI_GETNONCLIENTMETRICS = #29,"
    filetext[3][2353] = "\tSystemParametersInfo = define_c_func(user32, \"SystemParametersInfoA\", {C_INT, C_INT, C_PTR, C_INT}, C_INT)"
    filetext[3][2354] = ``
    filetext[4][1..4] = {`--`,`-- toc.txt`,`-- =======`,`--`}
    filetext[4][4060..4063] = {`ApplicationWindow `,`Pixbuf `,`IconTheme`,``}
    filetext[5][1..5] = {`--`,`-- index.txt`,`-- =========`,`--`,`-- format of each line is <keyword>[ <url>]`}
    filetext[5][3952..3953] = {`drawDib`,``}
    filetext[6][1..4] = {`Euphoria syntax file`,`BlockComment /* */ --/* --*/ #[ #]`,`LineComment -- //`,`--LineComment //`}
    filetext[6][5051..5055] = {"\t\tBuiltinRoutines #A86070","\t\tBuiltin4 #A86070","\t\tLibraryRoutines #A86070","\t\tLibrary4 #A86070",``}
    filetext[7][1..4] = {`Phix syntax file`,`BlockComment /* */ --/* --*/ #[ #]`,`LineComment -- //`,`--LineComment //`}
    filetext[7][5055..5056] = {"\t\tWin32lib Maroon",``}
    filetext[8][1..2] = {`Help file:"..\..\docs\phix\phix.chm"`,`--Help file:"C:\Program Files (x86)\Phix\docs\phix\phix.chm"`}
    filetext[8][3804..3806] = {"\tZipClose","\tZipCreateFile","\tZIPENTRY"}
    filetext[9][1..2] = {`Help file:"..\..\docs\phix\phix.chm"`,`--Help file:"C:\Program Files (x86)\Phix\docs\phix\phix.chm"`}
    filetext[9][3800..3802] = {"\tZipClose","\tZipCreateFile","\tZIPENTRY"}
    filetext[10][1..9] = {`p.exw`,`pw.exe`,`ok.ico`,`ptick.ico`,`pcross.ico`,`copyright.txt`,`eval.e`,`fortune.exw`,`Fortunes.txt`}
    filetext[10][3368..3374] = {`test\inc8\file2c.e`,`test\inc8\t59a.e`,`test\inc8\t59b.e`,`test\swtime.exw`,`test\swtime2.exw`,`test\trace.exw`,``}
    filetext[11][1..2] = {`// parse.js`,`// Parser for Simplified JavaScript written in Simplified JavaScript`}
    filetext[11][3..4] = {`// From Top Down Operator Precedence`,`// http://javascript.crockford.com/tdop/index.html`}
    filetext[11][5..9] = {`// Douglas Crockford`,`// 2010-06-26`,``,`let make_parse = function () {`,"\tlet scope;"}
    filetext[12][1..6] = {`js syntax file`,`BlockComment /* */`,`LineComment //`,`//`,`// About .syn files`,`// ================`}
    filetext[12][1823..1828] = {`// properties Purple`,` properties Maroon`,` lib Maroon`,`// Borland #681504`,` illegal #FF0000 Italic`,``}
    filetext[13][1][1..101] = `It may amuse you to know that I reached an unsettling conclusion about the nature of the universe aro`
    filetext[13][477..478] = {"--\tdon\'t tend to get all that on simpler systems is precisely because you /can/ get by without it.",``}
    filetext[14][1..4] = {`/* ARM assembly Raspberry PI  */`,"/*\tprogram helloworld.s   */",`/*`,`   create file helloworld.s  `}
    filetext[14][5] = `   compile it with : as -o helloworld.o helloworld.s`
    filetext[14][6..9] = {`   link  it with   : ld -o helloworld helloworld.o -e main`,"   execute it\t   :  helloworld",`*/`,`.data`}
    filetext[14][10..12] = {`szMessage: .asciz "Hello world. \n"   `,".equ LGMESSAGE, . -  szMessage\t@ compute length of message",`.text`}
    filetext[14][13..16] = {`.global main `,`iAdrMessage: .int szMessage`,"main:\t","\tmov r0, #1\t\t\t\t\t@ output std linux"}
    filetext[14][17..18] = {"@\tmov r0, #2\t\t\t\t\t@ output error linux","\tldr r1, iAdrMessage \t\t@ adresse of message"}
    filetext[14][19..20] = {"\tmov r2, #LGMESSAGE\t\t\t@ sizeof(message) ","\tmov r7, #4\t\t\t\t\t@ select system call \'write\' "}
    filetext[14][21..23] = {"\tswi #0\t\t\t\t\t\t@ perform the system call ",` `,"\tmov r0, #0\t\t\t\t\t@ return code"}
    filetext[14][24..26] = {"\tmov r7, #1\t\t\t\t\t@ request to exit program","\tswi 0 ",``}
    filetext[15][1..3] = {`/* file affichage.inc */`,`.data`,`/*************************************************/`}
    filetext[15][4..6] = {`szMessErr: .ascii "Error code hexa : "`,"sHexa:\t   .space 9,\' \'","\t\t   .ascii \"  decimal :  \""}
    filetext[15][7..9] = {"sDeci:\t   .space 15,\' \'","\t\t   .asciz \"\\n\"",`ptzZoneHeap:   .int zZoneHeap`}
    filetext[16][1..3] = {`## python -m pip install --upgrade pip`,`#from time import time`,`#t0 = time()`}
    filetext[17][1..4] = {`--`,`-- fortune.exw`,`--`,`integer fn = open("Fortunes.txt","r"), line = 0`}
    filetext[17][5..6] = {`string fortune = "There be no fortunes to be made here.\n"`,`if fn!=-1 then`}
    unicodefile = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0}
    filelinelengths[1][1..48] = {9,0,0,4,1,5,2,4,4,6,10,16,5,2,0,0,1,0,0,0,0,1,2,2,0,1,0,1,0,0,0,5,3,51,49,16,25,6,2,3,3,4,2,2,2,2,2,3}
    filelinelengths[1][49..97] = {3,4,3,2,5,7,2,2,7,9,14,20,12,13,12,10,15,12,12,13,6,6,4,6,2,6,4,4,3,0,2,1,3,3,2,1,5,5,1,0,0,1,0,2,0,0,0,0,1}
    filelinelengths[1][98..151] = {0,0,0,0,0,0,3,0,0,0,2,2,1,0,0,0,1,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    filelinelengths[1][152..160] = {0,0,0,0,0,0,0,0,1}
    filelinelengths[2][1..46] = {11,0,0,4,1,5,2,4,5,6,10,18,4,2,0,0,1,0,0,0,0,1,2,2,0,1,0,2,0,0,0,0,43,13,26,17,20,18,23,15,11,9,11,4,0,4}
    filelinelengths[2][47..97] = {5,3,3,5,4,1,0,3,1,7,4,5,14,8,20,16,14,13,7,8,5,6,9,6,7,5,3,4,1,7,2,0,0,3,1,2,1,0,0,2,0,0,1,0,1,1,0,0,0,2,2}
    filelinelengths[2][98..151] = {3,0,0,2,0,0,0,2,1,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    filelinelengths[2][152] = 1
    filelinelengths[3][1..25] = {223,0,3,0,19,1,0,0,3,4,28,7,44,17,32,48,39,69,59,76,81,93,117,101,107}
    filelinelengths[3][26..48] = {96,101,86,77,77,61,44,37,44,34,43,42,44,30,18,19,11,10,11,12,6,11,2}
    filelinelengths[3][49..99] = {5,1,7,6,11,11,6,7,2,0,1,3,0,2,5,3,5,11,5,4,3,4,4,10,8,14,1,10,4,4,1,3,5,4,4,6,7,7,0,5,1,1,1,5,2,2,3,7,0,3,3}
    filelinelengths[3][100..136] = {0,1,3,1,7,0,4,1,1,1,3,3,1,1,1,0,1,0,0,0,5,1,0,0,0,0,0,2,0,0,1,0,0,0,1,0,1}
    filelinelengths[4][1..23] = {140,2,15,5,61,141,152,196,219,253,221,216,243,192,172,147,175,101,99,118,74,50,46}
    filelinelengths[4][24..64] = {53,66,36,33,27,22,23,18,13,20,13,12,6,8,12,3,1,10,8,7,9,3,3,4,6,0,1,3,2,4,5,5,3,1,2,3,4,7,6,1,2}
    filelinelengths[4][65..106] = {2,2,4,4,5,4,4,7,1,8,12,12,20,18,18,34,21,13,19,28,18,13,15,14,14,17,14,15,12,10,7,9,8,5,3,7,5,6,7,3,8,6}
    filelinelengths[4][107..159] = {9,6,3,4,5,5,1,1,2,0,12,3,3,0,1,1,0,0,3,0,2,1,3,1,2,4,3,1,2,1,1,1,2,2,0,0,0,0,1,0,1,0,2,3,0,1,0,0,2,0,2,0,1}
    filelinelengths[4][160..197] = {0,0,0,0,1,0,0,1,1,2,1,0,0,0,0,2,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1}
    filelinelengths[5][1..20] = {6,0,7,16,35,27,50,75,57,68,58,93,79,83,90,110,78,78,93,79}
    filelinelengths[5][21..38] = {92,81,85,139,166,155,139,119,119,79,116,107,115,97,95,99,92,81}
    filelinelengths[5][39..68] = {63,62,49,39,62,44,56,40,31,19,23,19,31,14,20,16,24,15,13,17,15,10,11,4,4,7,8,6,5,6}
    filelinelengths[5][69..103] = {1,4,6,17,26,2,1,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,1}
    filelinelengths[6][1..28] = {69,0,19,0,0,2,3,3,10,8,77,181,185,254,252,341,329,354,406,397,353,342,222,252,180,123,145,84}
    filelinelengths[6][29..71] = {92,55,43,35,24,26,19,16,12,9,8,4,4,2,3,1,2,2,0,1,1,2,0,0,0,2,2,1,0,2,3,0,1,1,0,1,2,1,2,4,4,5,4}
    filelinelengths[6][72..116] = {4,2,5,5,5,5,9,8,5,3,6,3,3,0,1,1,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1}
    filelinelengths[7][1..28] = {65,0,19,0,0,2,3,3,14,8,77,181,185,253,252,341,330,354,406,396,352,343,221,253,180,124,144,84}
    filelinelengths[7][29..71] = {92,54,43,36,24,26,19,16,12,9,8,4,4,2,3,1,2,2,0,1,1,2,0,0,0,2,2,1,0,2,2,0,1,1,0,1,2,1,2,4,4,5,4}
    filelinelengths[7][72..116] = {4,2,5,5,5,5,9,8,5,3,6,3,3,0,1,1,2,0,0,0,0,0,0,2,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1}
    filelinelengths[8][1..26] = {0,0,0,0,0,0,25,141,185,226,228,271,260,264,312,286,269,245,160,188,138,102,117,63,76,46}
    filelinelengths[8][27..74] = {37,23,20,23,14,13,7,2,5,3,1,1,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,3,17,27,1,0,0,0,0,0,0,0,0,0,0}
    filelinelengths[8][75..93] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,1}
    filelinelengths[9][1..26] = {0,0,0,0,0,0,26,167,190,241,230,271,260,263,312,286,269,245,160,188,138,102,117,63,76,46}
    filelinelengths[9][27..75] = {37,23,20,23,14,13,7,2,5,3,1,1,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    filelinelengths[9][76..80] = {0,0,0,0,0}
    filelinelengths[10][1..28] = {36,0,0,0,0,2,6,11,10,6,5,10,7,9,30,74,72,89,73,75,80,97,130,132,167,197,178,185}
    filelinelengths[10][29..55] = {182,183,180,147,130,112,76,54,47,59,38,55,30,39,31,29,19,18,16,7,9,7,3,6,11,18,16}
    filelinelengths[10][56..80] = {8,9,16,14,18,13,16,19,15,13,7,10,4,5,3,1,0,0,0,0,0,0,0,0,0}
    filelinelengths[11][1..39] = {45,0,1,0,0,0,18,14,0,22,8,2,1,18,2,1,14,22,4,10,30,33,16,9,8,7,28,13,11,20,20,16,12,15,10,12,17,7,7}
    filelinelengths[11][40..80] = {6,8,7,4,0,2,3,1,2,1,3,4,2,0,2,1,1,1,2,0,1,0,0,1,1,0,2,1,1,1,1,0,0,0,0,0,0,0,0,2,0}
    filelinelengths[12][1..26] = {23,0,17,1,0,1,1,4,0,2,9,55,86,125,157,152,146,135,163,111,123,101,69,59,43,25}
    filelinelengths[12][27..75] = {32,23,18,17,10,6,6,4,2,0,1,3,2,4,1,2,1,1,2,2,0,0,0,0,1,0,0,1,0,1,0,0,1,0,1,0,0,2,0,1,2,4,3,1,3,5,2,4,5}
    filelinelengths[12][76..90] = {4,5,9,6,5,2,3,4,2,0,2,0,2,1,1}
    filelinelengths[13][1..51] = {80'P',0,1,3,1,3,1,0,2,2,3,5,5,1,3,3,4,3,1,5,2,3,4,6,0,5,1,1,3,1,2,0,0,4,4,2,2,4,1,2,2,0,0,2,0,1,0,1,0,1,0}
    filelinelengths[13][52..100] = {1,0,0,1,0,8,0,0,0,1,1,0,1,2,0,0,3,0,0,0,0,0,0,0,0,2,0,2,0,2,0,1,1,1,2,2,3,2,2,6,10,23,27,23,28,26,18,15,7}
    filelinelengths[13][101..153] = {1,4,1,3,0,1,0,2,2,1,1,4,5,3,7,5,7,8,8,4,2,0,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,2,0,0,0,0,0,0,0,0,0,0,0,0}
    filelinelengths[13][154..173] = {0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1}
    filelinelengths[14][1..53] = {1,1,2,0,0,2,0,0,1,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,2,0,0,2,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,2,0,3}
    filelinelengths[14][54..80] = {0,0,0,0,1,2,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    filelinelengths[15][1..52] = {1,6,10,1,7,3,0,0,5,3,10,5,2,23,7,5,5,1,0,7,8,5,4,3,6,1,2,2,0,4,2,5,1,3,4,0,4,3,3,1,3,6,5,6,2,6,4,2,5,3,5,9}
    filelinelengths[15][53..104] = {5,17,11,4,4,3,6,4,6,6,1,6,7,6,0,9,19,7,1,4,5,2,1,6,3,1,2,0,4,4,1,3,2,0,1,0,0,0,0,1,1,1,1,0,1,1,0,0,0,0,1,0}
    filelinelengths[15][105..116] = {0,0,0,0,0,0,0,0,0,0,2,1}
    filelinelengths[16][1..21] = {288,660,587,115,37,24,35,110,58,65,70,89,109,140,139,99,153,177,165,167,144}
    filelinelengths[16][22..43] = {172,218,179,145,167,209,165,172,140,174,135,182,132,149,144,114,126,111,110,81,76,74}
    filelinelengths[16][44..60] = {85,80,85,77,73,83,68,58,45,48,48,50,68,45,67,34,50}
    filelinelengths[16][61..83] = {52,49,55,52,40,56,30,21,36,44,43,48,20,19,21,28,32,33,22,26,24,17,26}
    filelinelengths[16][84..129] = {17,15,15,12,14,12,11,9,15,16,10,13,10,9,10,11,6,4,8,3,4,4,3,14,4,4,0,4,7,5,4,1,1,1,2,2,9,1,1,1,1,0,1,1,0,2}
    filelinelengths[16][130..182] = {2,0,3,2,0,2,1,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    filelinelengths[16][183..235] = {1,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    filelinelengths[16][236..288] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    filelinelengths[16][289..329] = {0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1}
    filelinelengths[17][1..53] = {2,0,2,0,0,0,1,0,0,0,0,0,0,2,3,1,0,1,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,1,0,0}
    filelinelengths[17][54..80] = {0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    filecursel[1..4] = {{0,9,0,0,0,0,9,0,0,0,5},{9,19,0,0,1,9,19,0,0,0,0},{0,0,0,0,0,0,0,0,0,0,0},{10,31,0,0,0,0,16,0,0,0,0}}
    filecursel[5..8] = {{0,0,0,0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0,0,0,0}}
    filecursel[9..12] = {{0,0,0,0,0,0,0,0,0,0,0},{9,12,0,0,0,0,0,0,0,0,0},{0,9,0,0,0,0,10,0,0,0,0},{23,473,428,0,0,0,473,0,0,0,0}}
    filecursel[13..16] = {{0,24,0,0,0,0,25,0,0,0,0},{0,21,0,0,0,5,11,0,0,0,0},{0,15,0,0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0,0,0,0}}
    filecursel[17] = {0,0,0,0,0,0,21,0,0,0,0}
    bCfwds[1..2] = {0,{}}
    bCfwds[3][1..9] = {{3,1,0,{}},{7,0,0,{}},{63,1,0,{}},{78,0,0,{}},{156,1,0,{}},{161,0,0,{}},{214,1,0,{}},{222,0,0,{}},{236,1,0,{}}}
    bCfwds[3][10..16] = {{246,0,0,{}},{313,1,0,{}},{323,0,0,{}},{359,0,1,{')'}},{361,0,0,{}},{681,0,2,{')','}'}},{683,0,0,{}}}
    bCfwds[3][17..22] = {{1534,0,1,{')'}},{1535,0,0,{}},{1899,0,2,{')','}'}},{1902,0,0,{}},{1910,0,2,{')','}'}},{1914,0,0,{}}}
    bCfwds[3][23..28] = {{2039,0,2,{')','}'}},{2058,0,0,{}},{2067,0,1,{'}'}},{2072,0,0,{}},{2178,0,2,{')','}'}},{2190,0,0,{}}}
    bCfwds[3][29..33] = {{2192,0,2,{')','}'}},{2196,0,0,{}},{2271,0,2,{')','}'}},{2286,0,0,{}},{2328,0,2,{')','}'}}}
    bCfwds[3][34] = {2337,0,0,{}}
    bCfwds[4..10] = {{},{},{},{},{},{},{}}
    bCfwds[11][1..4] = {{8,0,1,{'}'}},{15,0,2,{'}','}'}},{17,0,1,{'}'}},{19,0,2,{'}','}'}}}
    bCfwds[11][5..7] = {{20,0,3,{'}','}','}'}},{22,0,4,{'}','}','}','}'}},{23,0,5,{'}','}','}','}',')'}}}
    bCfwds[11][8..10] = {{25,0,4,{'}','}','}','}'}},{26,0,3,{'}','}','}'}},{35,0,2,{'}','}'}}}
    bCfwds[11][11..12] = {{36,0,3,{'}','}','}'}},{38,0,4,{'}','}','}','}'}}}
    bCfwds[11][13..14] = {{40,0,5,{'}','}','}','}','}'}},{42,0,4,{'}','}','}','}'}}}
    bCfwds[11][15..16] = {{44,0,5,{'}','}','}','}','}'}},{49,0,4,{'}','}','}','}'}}}
    bCfwds[11][17..19] = {{50,0,3,{'}','}','}'}},{'3',0,2,{'}','}'}},{52,0,3,{'}','}','}'}}}
    bCfwds[11][20..22] = {{54,0,2,{'}','}'}},{55,0,3,{'}','}','}'}},{56,0,4,{'}','}','}','}'}}}
    bCfwds[11][23..24] = {{58,0,3,{'}','}','}'}},{60,0,4,{'}','}','}','}'}}}
    bCfwds[11][25..26] = {{61,0,5,{'}','}','}','}','}'}},{63,0,4,{'}','}','}','}'}}}
    bCfwds[11][27..28] = {{64,0,5,{'}','}','}','}','}'}},{66,0,4,{'}','}','}','}'}}}
    bCfwds[11][29..32] = {{67,0,3,{'}','}','}'}},{70,0,2,{'}','}'}},{71,0,1,{'}'}},{73,0,2,{'}','}'}}}
    bCfwds[11][33..36] = {{79,0,1,{'}'}},{81,0,2,{'}','}'}},{83,0,3,{'}','}','}'}},{85,0,2,{'}','}'}}}
    bCfwds[11][37..39] = {{86,0,3,{'}','}','}'}},{89,0,2,{'}','}'}},{94,0,3,{'}','}','}'}}}
    bCfwds[11][40..42] = {{98,0,4,{'}','}','}','}'}},{100,0,3,{'}','}','}'}},{106,0,2,{'}','}'}}}
    bCfwds[11][43..46] = {{113,0,1,{'}'}},{'s',0,2,{'}','}'}},{120,0,3,{'}','}','}'}},{124,0,2,{'}','}'}}}
    bCfwds[11][47..50] = {{126,0,1,{'}'}},{128,0,2,{'}','}'}},{131,0,3,{'}','}','}'}},{135,0,2,{'}','}'}}}
    bCfwds[11][51..54] = {{137,0,3,{'}','}','}'}},{139,0,2,{'}','}'}},{142,0,1,{'}'}},{144,0,2,{'}','}'}}}
    bCfwds[11][55..57] = {{146,0,3,{'}','}','}'}},{147,0,4,{'}','}','}','}'}},{149,0,3,{'}','}','}'}}}
    bCfwds[11][58..60] = {{151,0,4,{'}','}','}','}'}},{153,0,3,{'}','}','}'}},{154,0,2,{'}','}'}}}
    bCfwds[11][61..64] = {{156,0,1,{'}'}},{158,0,2,{'}','}'}},{162,0,1,{'}'}},{164,0,2,{'}','}'}}}
    bCfwds[11][65..67] = {{165,0,3,{'}','}','}'}},{167,0,2,{'}','}'}},{168,0,3,{'}','}','}'}}}
    bCfwds[11][68..71] = {{170,0,2,{'}','}'}},{171,0,1,{'}'}},{173,0,2,{'}','}'}},{176,0,3,{'}','}','}'}}}
    bCfwds[11][72..74] = {{177,0,4,{'}','}','}','}'}},{179,0,3,{'}','}','}'}},{185,0,2,{'}','}'}}}
    bCfwds[11][75..78] = {{187,0,1,{'}'}},{189,0,2,{'}','}'}},{191,0,3,{'}','}','}'}},{196,0,2,{'}','}'}}}
    bCfwds[11][79..82] = {{199,0,1,{'}'}},{201,0,2,{'}','}'}},{203,0,3,{'}','}','}'}},{208,0,2,{'}','}'}}}
    bCfwds[11][83..86] = {{210,0,1,{'}'}},{212,0,2,{'}','}'}},{214,0,3,{'}','}','}'}},{219,0,2,{'}','}'}}}
    bCfwds[11][87..89] = {{221,0,1,{'}'}},{223,0,2,{'}','}'}},{224,0,4,{'}','}',')','}'}}}
    bCfwds[11][90..92] = {{225,0,5,{'}','}',')','}','}'}},{227,0,4,{'}','}',')','}'}},{233,0,2,{'}','}'}}}
    bCfwds[11][93..96] = {{234,0,1,{'}'}},{236,0,2,{'}','}'}},{238,0,3,{'}','}','}'}},{243,0,2,{'}','}'}}}
    bCfwds[11][97..101] = {{245,0,1,{'}'}},{247,0,2,{'}','}'}},{251,0,1,{'}'}},{272,0,2,{'}','}'}},{276,0,1,{'}'}}}
    bCfwds[11][102..104] = {{282,0,3,{'}',')','}'}},{289,0,1,{'}'}},{307,0,3,{'}',')','}'}}}
    bCfwds[11][105..107] = {{309,0,4,{'}',')','}','}'}},{311,0,3,{'}',')','}'}},{317,0,1,{'}'}}}
    bCfwds[11][108..110] = {{319,0,3,{'}',')','}'}},{325,0,1,{'}'}},{327,0,3,{'}',')','}'}}}
    bCfwds[11][111..112] = {{329,0,4,{'}',')','}','}'}},{338,0,5,{'}',')','}','}',')'}}}
    bCfwds[11][113..114] = {{340,0,5,{'}',')','}','}','}'}},{342,0,4,{'}',')','}','}'}}}
    bCfwds[11][115..116] = {{343,0,3,{'}',')','}'}},{344,0,4,{'}',')','}','}'}}}
    bCfwds[11][117..118] = {{345,0,5,{'}',')','}','}','}'}},{347,0,6,{'}',')','}','}','}','}'}}}
    bCfwds[11][119..120] = {{349,0,5,{'}',')','}','}','}'}},{351,0,4,{'}',')','}','}'}}}
    bCfwds[11][121..124] = {{352,0,3,{'}',')','}'}},{355,0,1,{'}'}},{362,0,3,{'}',')','}'}},{366,0,1,{'}'}}}
    bCfwds[11][125..127] = {{368,0,3,{'}',')','}'}},{371,0,4,{'}',')','}','}'}},{375,0,3,{'}',')','}'}}}
    bCfwds[11][128..129] = {{377,0,4,{'}',')','}','}'}},{378,0,5,{'}',')','}','}','}'}}}
    bCfwds[11][130..131] = {{379,0,6,{'}',')','}','}','}','}'}},{381,0,5,{'}',')','}','}','}'}}}
    bCfwds[11][132..133] = {{385,0,6,{'}',')','}','}','}','}'}},{387,0,5,{'}',')','}','}','}'}}}
    bCfwds[11][134..136] = {{389,0,4,{'}',')','}','}'}},{390,0,3,{'}',')','}'}},{399,0,1,{'}'}}}
    bCfwds[11][137..138] = {{401,0,3,{'}',')','}'}},{403,0,4,{'}',')','}','}'}}}
    bCfwds[11][139..140] = {{404,0,5,{'}',')','}','}','}'}},{406,0,6,{'}',')','}','}','}','}'}}}
    bCfwds[11][141..142] = {{408,0,5,{'}',')','}','}','}'}},{410,0,4,{'}',')','}','}'}}}
    bCfwds[11][143..145] = {{411,0,3,{'}',')','}'}},{416,0,1,{'}'}},{418,0,3,{'}',')','}'}}}
    bCfwds[11][146..147] = {{420,0,4,{'}',')','}','}'}},{421,0,5,{'}',')','}','}','}'}}}
    bCfwds[11][148..149] = {{423,0,6,{'}',')','}','}','}','}'}},{425,0,5,{'}',')','}','}','}'}}}
    bCfwds[11][150..151] = {{431,0,6,{'}',')','}','}','}','}'}},{433,0,5,{'}',')','}','}','}'}}}
    bCfwds[11][152..154] = {{435,0,4,{'}',')','}','}'}},{436,0,3,{'}',')','}'}},{441,0,1,{'}'}}}
    bCfwds[11][155..157] = {{444,0,3,{'}',')','}'}},{450,0,1,{'}'}},{452,0,3,{'}',')','}'}}}
    bCfwds[11][158..159] = {{454,0,4,{'}',')','}','}'}},{456,0,5,{'}',')','}','}','}'}}}
    bCfwds[11][160..161] = {{458,0,4,{'}',')','}','}'}},{461,0,5,{'}',')','}','}','}'}}}
    bCfwds[11][162..163] = {{468,0,4,{'}',')','}','}'}},{469,0,5,{'}',')','}','}','}'}}}
    bCfwds[11][164..166] = {{471,0,4,{'}',')','}','}'}},{473,0,3,{'}',')','}'}},{476,0,1,{'}'}}}
    bCfwds[11][167..169] = {{478,0,3,{'}',')','}'}},{483,0,4,{'}',')','}','}'}},{489,0,3,{'}',')','}'}}}
    bCfwds[11][170..172] = {{492,0,1,{'}'}},{494,0,3,{'}',')','}'}},{495,0,4,{'}',')','}','}'}}}
    bCfwds[11][173..175] = {{497,0,3,{'}',')','}'}},{499,0,4,{'}',')','}','}'}},{501,0,3,{'}',')','}'}}}
    bCfwds[11][176..178] = {{504,0,1,{'}'}},{506,0,3,{'}',')','}'}},{508,0,4,{'}',')','}','}'}}}
    bCfwds[11][179..182] = {{510,0,3,{'}',')','}'}},{513,0,1,{'}'}},{515,0,3,{'}',')','}'}},{522,0,1,{'}'}}}
    bCfwds[11][183..185] = {{524,0,2,{'}','}'}},{533,0,1,{'}'}},{534,0,0,{}}}
    bCfwds[12..13] = {{},{}}
    bCfwds[14] = {{3,1,0,{}},{8,0,0,{}}} -- helloworld.s, block comment on lines 3..8
    bCfwds[15] = {}
    bCfwds[16][1..7] = {{17,0,1,{')'}},{19,0,0,{}},{25,0,1,{')'}},{33,0,0,{}},{42,0,1,{')'}},{48,0,0,{}},{3572,-2,0,{}}}
    bCfwds[16][8..15] = {{3673,0,0,{}},{3675,-2,0,{}},{3760,0,0,{}},{4508,-2,0,{}},{4597,0,0,{}},{4599,-2,0,{}},{4639,0,0,{}},{4641,-2,0,{}}}
    bCfwds[16][16..23] = {{4702,0,0,{}},{4704,-2,0,{}},{4893,0,0,{}},{4895,-2,0,{}},{4928,0,0,{}},{4930,-2,0,{}},{4955,0,0,{}},{4957,-2,0,{}}}
    bCfwds[16][24..31] = {{5032,0,0,{}},{5034,-2,0,{}},{5072,0,0,{}},{5076,-2,0,{}},{5112,0,0,{}},{5115,-2,0,{}},{5141,0,0,{}},{5143,-2,0,{}}}
    bCfwds[16][32..39] = {{5227,0,0,{}},{5229,-2,0,{}},{5335,0,0,{}},{5337,-2,0,{}},{5365,0,0,{}},{5367,-2,0,{}},{5387,0,0,{}},{5389,-2,0,{}}}
    bCfwds[16][40..47] = {{5440,0,0,{}},{5442,-2,0,{}},{5701,0,0,{}},{5703,-2,0,{}},{5781,0,0,{}},{5836,-2,0,{}},{5842,0,0,{}},{5844,-2,0,{}}}
    bCfwds[16][48..55] = {{5875,0,0,{}},{6470,-2,0,{}},{6523,0,0,{}},{6525,-2,0,{}},{6548,0,0,{}},{6551,-2,0,{}},{6596,0,0,{}},{6598,-2,0,{}}}
    bCfwds[16][56..63] = {{6612,0,0,{}},{6614,-2,0,{}},{6645,0,0,{}},{6749,-2,0,{}},{6819,0,0,{}},{6821,-2,0,{}},{6849,0,0,{}},{6851,-2,0,{}}}
    bCfwds[16][64..71] = {{6881,0,0,{}},{6884,-2,0,{}},{6889,0,0,{}},{6891,-2,0,{}},{6976,0,0,{}},{6978,-2,0,{}},{7021,0,0,{}},{7023,-2,0,{}}}
    bCfwds[16][72..79] = {{7037,0,0,{}},{7039,-2,0,{}},{7055,0,0,{}},{7057,-2,0,{}},{7117,0,0,{}},{7119,-2,0,{}},{7159,0,0,{}},{7161,-2,0,{}}}
    bCfwds[16][80..87] = {{7183,0,0,{}},{7193,-2,0,{}},{7205,0,0,{}},{7225,-2,0,{}},{7244,0,0,{}},{7246,-2,0,{}},{7260,0,0,{}},{7262,-2,0,{}}}
    bCfwds[16][88..95] = {{7279,0,0,{}},{7281,-2,0,{}},{7385,0,0,{}},{7388,-2,0,{}},{7503,0,0,{}},{7504,-2,0,{}},{7512,0,0,{}},{7513,-2,0,{}}}
    bCfwds[16][96..103] = {{7562,0,0,{}},{7565,-2,0,{}},{7609,0,0,{}},{7611,-2,0,{}},{7620,0,0,{}},{7993,-2,0,{}},{8001,0,0,{}},{8003,-2,0,{}}}
    bCfwds[16][104..111] = {{8045,0,0,{}},{8047,-2,0,{}},{8113,0,0,{}},{8115,-2,0,{}},{8145,0,0,{}},{8147,-2,0,{}},{8232,0,0,{}},{8234,-2,0,{}}}
    bCfwds[16][112..119] = {{8266,0,0,{}},{8268,-2,0,{}},{8328,0,0,{}},{8330,-2,0,{}},{8373,0,0,{}},{8375,-2,0,{}},{8391,0,0,{}},{8393,-2,0,{}}}
    bCfwds[16][120..124] = {{8466,0,0,{}},{8468,-2,0,{}},{8505,0,0,{}},{8506,-2,0,{}},{8782,0,0,{}}}
    bCfwds[17] = {}
    filedt = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    unpacked = {}
    wrapped = {}
    bookmarks[1][1..514] = {0}
    bookmarks[2][1..518] = {0}
    bookmarks[3][1..2354] = {0}
    bookmarks[4][1..4063] = {0}
    bookmarks[5][1..3953] = {0}
    bookmarks[6][1..5055] = {0}
    bookmarks[7][1..5056] = {0}
    bookmarks[8][1..3806] = {0}
    bookmarks[9][1..3802] = {0}
    bookmarks[10][1..3374] = {0}
    bookmarks[11][1..534] = {0}
    bookmarks[12][1..1828] = {0}
    bookmarks[13][1..478] = {0}
    bookmarks[14] = {0}
    bookmarks[15][1..389] = {0}
    bookmarks[16][1..8782] = {0}
    bookmarks[17] = {0,0,0,0}

 C:\Program Files (x86)\Phix\demo\edix\edix.exw:
    routines[1..2] = {0,{{},{},{}}}
    routines[3][1][1..8] = {`or_all`,`HIWORD`,`LOWORD`,`align`,`alloc_string`,`free_strings`,`peek_string`,`pokeWideCharString`}
    routines[3][1][9..15] = {`allocWideCharString`,`peekWideCharString`,`w_sizeof`,`pack`,`allocate_pack`,`unpack`,`RegisterClass`}
    routines[3][1][16..20] = {`CreateWindow`,`allocDialogTemplate`,`DialogBoxIndirectParam`,`CreateDialogIndirectParam`,`peek_multi_strings`}
    routines[3][1][21..26] = {`GetFileNameDialog`,`GetOpenFileName`,`GetSaveFileName`,`GetFindFlags`,`GetFindWhat`,`GetReplaceWith`}
    routines[3][1][27..31] = {`FindReplaceText`,`FindText`,`ReplaceText`,`ChooseFont`,`ChooseColor`}
    routines[3][2][1..23] = {9,20,24,29,43,56,64,80,93,101,136,178,255,277,1830,1845,1869,1942,1955,2004,2022,2081,2088}
    routines[3][2][24..31] = {2144,2148,2154,2164,2208,2213,2260,2323}
    routines[3][3][1..22] = {18,22,26,36,53,61,77,91,99,115,175,253,275,336,1843,1865,1939,1953,1966,2019,2079,2086}
    routines[3][3][23..31] = {2093,2146,2152,2158,2206,2211,2216,2304,2346}
    routines[4][1] = {`results`,`open`,`close`,`p`,`somefunc`,`CB_somefunc1`,`CB_somefunc2`,`someproc`,`findit`,`tmp895`}
    routines[4][2..3] = {{1582,1689,1696,1815,1853,1857,1861,1886,1957,2303},{0,1694,1701,0,1855,1859,1863,1888,0,0}}
    routines[5..10] = {{{`functions`,`procedures`,`types`},{1069,3041,3483},{0,0,0}},{{},{},{}},{{},{},{}},{{},{},{}},{{},{},{}},{{},{},{}}}
    routines[11..12] = {{{},{},{}},{{},{},{}}}
    routines[13][1..2] = {{`decode_params`,`decode_params`,`somefunc`,`somefunc`,`somefunc`,`somefunc`},{341,350,385,389,406,422}}
    routines[13][3] = {343,358,387,395,412,424}
    routines[14..17] = {{{},{},{}},{{},{},{}},{{},{},{}},{{},{},{}}}
    actions = {{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{}}
    actionptr = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    actionsave = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    currfile = 3
    isDebug = 0
    isFoldMarginPerm = 1
    isWordWrapON = 0
    isAutoSaveTimer = 0
    isAutoBackup = 1
    isReplaceTabs = 1
    isTabWidth = 4
    isHomeFirstNonBlank = 1
    isLineNumbers = 1
    isMarginSpace = 1
    isAutoComplete = 1
    isClearOverStrike = 127
    isUndoTime = 0
    isRetainBackupsFor = 5
    bCfwd[1..9] = {{3,1,0,{}},{7,0,0,{}},{63,1,0,{}},{78,0,0,{}},{156,1,0,{}},{161,0,0,{}},{214,1,0,{}},{222,0,0,{}},{236,1,0,{}}}
    bCfwd[10..16] = {{246,0,0,{}},{313,1,0,{}},{323,0,0,{}},{359,0,1,{')'}},{361,0,0,{}},{681,0,2,{')','}'}},{683,0,0,{}}}
    bCfwd[17..22] = {{1534,0,1,{')'}},{1535,0,0,{}},{1899,0,2,{')','}'}},{1902,0,0,{}},{1910,0,2,{')','}'}},{1914,0,0,{}}}
    bCfwd[23..28] = {{2039,0,2,{')','}'}},{2058,0,0,{}},{2067,0,1,{'}'}},{2072,0,0,{}},{2178,0,2,{')','}'}},{2190,0,0,{}}}
    bCfwd[29..34] = {{2192,0,2,{')','}'}},{2196,0,0,{}},{2271,0,2,{')','}'}},{2286,0,0,{}},{2328,0,2,{')','}'}},{2337,0,0,{}}}
    paintRqd = 0
    paintLast = 0
    CursorX = 0
    CursorY = 0
    TopLine = 0
    Column = 0
    TopChunk = 0
    selON = 0
    selX = 0
    selY = 0
    marginRqd = 5
    linesPerPage = 49
    charsWide = 210
    landingChunk = 0
    endChunk = 0
    nacX = 0
    isFTP = 0
    checkProj = 1
    unicodeflag = 0

 C:\Program Files (x86)\Phix\demo\edix\src\auto.e:
    autoComplete[1] = {-1,{-3,'i','f'},' ',0,{{' ','t','h','e','n'},{'e','n','d',' ','i','f'}}}
    autoComplete[2][1..4] = {-1,{-3,'i','f','d'},'e',2}
    autoComplete[2][5][1] = {'f',' ',' ','t','h','e','n'}
    autoComplete[2][5][2] = {'e','n','d',' ','i','f','d','e','f'}
    autoComplete[3][1..4] = {-1,{-3,'f','o','r'},' ',0}
    autoComplete[3][5] = {{' ','t','o',' ',' ','d','o'},{'e','n','d',' ','f','o','r'}}
    autoComplete[4][1..4] = {-1,{-3,'w','h','i','l'},'e',1}
    autoComplete[4][5] = {{' ',' ','d','o'},{'e','n','d',' ','w','h','i','l','e'}}
    autoComplete[5] = {-1,{-3,'e','l','s','i','f'},' ',0,{{' ','t','h','e','n'}}}
    autoComplete[6] = {-1,{-3,'e','l','s','i','f','d'},'e',2,{{'f',' ',' ','t','h','e','n'}}}
    autoComplete[7] = {-1,{-3,'l','o','o'},'p',6,{{' ','d','o'},{'u','n','t','i','l',' '}}}
    autoComplete[8][1..4] = {-1,{-3,'s','w','i'},'t',3}
    autoComplete[8][5][1] = {'c','h',' ',' ','d','o'}
    autoComplete[8][5][2] = {'e','n','d',' ','s','w','i','t','c','h'}
    autoComplete[9] = {-1,{-3,'c','a','s','e',' '},'|',0,{{' ','t','h','e','n'}}}
    autoComplete[10][1..4] = {-1,{'f','u'},'n',6}
    autoComplete[10][5][1] = {'c','t','i','o','n',' ','(',')'}
    autoComplete[10][5][2] = {'e','n','d',' ','f','u','n','c','t','i','o','n'}
    autoComplete[11][1..4] = {-1,{'p','r'},'o',7}
    autoComplete[11][5][1] = {'c','e','d','u','r','e',' ','(',')'}
    autoComplete[11][5][2] = {'e','n','d',' ','p','r','o','c','e','d','u','r','e'}
    autoComplete[12][1..4] = {-1,{'t','y'},'p',2}
    autoComplete[12][5] = {{'e',' ','(',')'},{'e','n','d',' ','t','y','p','e'}}
    autoComplete[13] = {-1,{'g'},'l',5,{{'o','b','a','l',' '}}}
    autoComplete[14][1..4] = {-1,{'g','l','o','b','a','l',' ','f','u'},'n',6}
    autoComplete[14][5][1] = {'c','t','i','o','n',' ','(',')'}
    autoComplete[14][5][2] = {'e','n','d',' ','f','u','n','c','t','i','o','n'}
    autoComplete[15][1..4] = {-1,{'g','l','o','b','a','l',' ','p','r'},'o',7}
    autoComplete[15][5][1] = {'c','e','d','u','r','e',' ','(',')'}
    autoComplete[15][5][2] = {'e','n','d',' ','p','r','o','c','e','d','u','r','e'}
    autoComplete[16][1..4] = {-1,{'g','l','o','b','a','l',' ','t','y'},'p',2}
    autoComplete[16][5] = {{'e',' ','(',')'},{'e','n','d',' ','t','y','p','e'}}
    autoComplete[17] = {-1,{'p','u'},'b',4,{{'l','i','c',' '}}}
    autoComplete[18][1..4] = {-1,{'p','u','b','l','i','c',' ','f','u'},'n',6}
    autoComplete[18][5][1] = {'c','t','i','o','n',' ','(',')'}
    autoComplete[18][5][2] = {'e','n','d',' ','f','u','n','c','t','i','o','n'}
    autoComplete[19][1..4] = {-1,{'p','u','b','l','i','c',' ','p','r'},'o',7}
    autoComplete[19][5][1] = {'c','e','d','u','r','e',' ','(',')'}
    autoComplete[19][5][2] = {'e','n','d',' ','p','r','o','c','e','d','u','r','e'}
    autoComplete[20][1..4] = {-1,{'p','u','b','l','i','c',' ','t','y'},'p',2}
    autoComplete[20][5] = {{'e',' ','(',')'},{'e','n','d',' ','t','y','p','e'}}
    autoComplete[21] = {-1,{'e','x'},'p',4,{{'o','r','t',' '}}}
    autoComplete[22][1..4] = {-1,{'e','x','p','o','r','t',' ','f','u'},'n',6}
    autoComplete[22][5][1] = {'c','t','i','o','n',' ','(',')'}
    autoComplete[22][5][2] = {'e','n','d',' ','f','u','n','c','t','i','o','n'}
    autoComplete[23][1..4] = {-1,{'e','x','p','o','r','t',' ','p','r'},'o',7}
    autoComplete[23][5][1] = {'c','e','d','u','r','e',' ','(',')'}
    autoComplete[23][5][2] = {'e','n','d',' ','p','r','o','c','e','d','u','r','e'}
    autoComplete[24][1..4] = {-1,{'e','x','p','o','r','t',' ','t','y'},'p',2}
    autoComplete[24][5] = {{'e',' ','(',')'},{'e','n','d',' ','t','y','p','e'}}
    autoComplete[25..26] = {{-1,{'a'},'t',3,{{'o','m',' '}}},{-1,{'i','n'},'t',5,{{'e','g','e','r',' '}}}}
    autoComplete[27] = {-1,{'s','e'},'q',6,{{'u','e','n','c','e',' '}}}
    autoComplete[28] = {-1,{'s','t'},'r',4,{{'i','n','g',' '}}}
    autoComplete[29] = {-1,{'o','b'},'j',4,{{'e','c','t',' '}}}
    autoComplete[30] = {-1,{'c','o'},'n',6,{{'s','t','a','n','t',' '}}}
    autoComplete[31] = {-1,{'e','n'},'u',2,{{'m',' '}}}
    autoComplete[32] = {-1,{'g','l','o','b','a','l',' ','a'},'t',3,{{'o','m',' '}}}
    autoComplete[33][1..4] = {-1,{'g','l','o','b','a','l',' ','i','n'},'t',5}
    autoComplete[33][5] = {{'e','g','e','r',' '}}
    autoComplete[34][1..4] = {-1,{'g','l','o','b','a','l',' ','s','e'},'q',6}
    autoComplete[34][5] = {{'u','e','n','c','e',' '}}
    autoComplete[35] = {-1,{'g','l','o','b','a','l',' ','s','t'},'r',4,{{'i','n','g',' '}}}
    autoComplete[36] = {-1,{'g','l','o','b','a','l',' ','o','b'},'j',4,{{'e','c','t',' '}}}
    autoComplete[37][1..4] = {-1,{'g','l','o','b','a','l',' ','c','o'},'n',6}
    autoComplete[37][5] = {{'s','t','a','n','t',' '}}
    autoComplete[38] = {-1,{'g','l','o','b','a','l',' ','e','n'},'u',2,{{'m',' '}}}
    autoComplete[39] = {-1,{'p','u','b','l','i','c',' ','a'},'t',3,{{'o','m',' '}}}
    autoComplete[40][1..4] = {-1,{'p','u','b','l','i','c',' ','i','n'},'t',5}
    autoComplete[40][5] = {{'e','g','e','r',' '}}
    autoComplete[41][1..4] = {-1,{'p','u','b','l','i','c',' ','s','e'},'q',6}
    autoComplete[41][5] = {{'u','e','n','c','e',' '}}
    autoComplete[42] = {-1,{'p','u','b','l','i','c',' ','s','t'},'r',4,{{'i','n','g',' '}}}
    autoComplete[43] = {-1,{'p','u','b','l','i','c',' ','o','b'},'j',4,{{'e','c','t',' '}}}
    autoComplete[44][1..4] = {-1,{'p','u','b','l','i','c',' ','c','o'},'n',6}
    autoComplete[44][5] = {{'s','t','a','n','t',' '}}
    autoComplete[45] = {-1,{'p','u','b','l','i','c',' ','e','n'},'u',2,{{'m',' '}}}
    autoComplete[46] = {-1,{'e','x','p','o','r','t',' ','a'},'t',3,{{'o','m',' '}}}
    autoComplete[47][1..4] = {-1,{'e','x','p','o','r','t',' ','i','n'},'t',5}
    autoComplete[47][5] = {{'e','g','e','r',' '}}
    autoComplete[48][1..4] = {-1,{'e','x','p','o','r','t',' ','s','e'},'q',6}
    autoComplete[48][5] = {{'u','e','n','c','e',' '}}
    autoComplete[49] = {-1,{'e','x','p','o','r','t',' ','s','t'},'r',4,{{'i','n','g',' '}}}
    autoComplete[50] = {-1,{'e','x','p','o','r','t',' ','o','b'},'j',4,{{'e','c','t',' '}}}
    autoComplete[51][1..4] = {-1,{'e','x','p','o','r','t',' ','c','o'},'n',6}
    autoComplete[51][5] = {{'s','t','a','n','t',' '}}
    autoComplete[52] = {-1,{'e','x','p','o','r','t',' ','e','n'},'u',2,{{'m',' '}}}
    autoComplete[53..54] = {{-2,{'a'},'t',3,{{'o','m',' '}}},{-2,{'i','n'},'t',5,{{'e','g','e','r',' '}}}}
    autoComplete[55] = {-2,{'o','b'},'j',4,{{'e','c','t',' '}}}
    autoComplete[56] = {-2,{'s','e'},'q',6,{{'u','e','n','c','e',' '}}}
    autoComplete[57] = {-2,{'s','t'},'r',4,{{'i','n','g',' '}}}
    acBlockID = <novalue>
    acRemainder = ``
    acRemIdx = 0
    outerRem = ``

 C:\Program Files (x86)\Phix\demo\edix\src\synld.e:
    Extensions[1..21] = {{},`s`,`inc`,`arm`,`asm`,`bat`,`b`,`c`,`h`,`cpp`,`cxx`,`css`,`d`,`pas`,`fasm`,`bas`,`fb`,`bi`,`fsx`,`go`,`htm`}
    Extensions[22..41] = {`html`,`php`,`hhc`,`hhk`,`clr`,`java`,`js`,`json`,`jl`,`l`,`pl`,`p6`,`e`,`ex`,`ew`,`exw`,`exwc`,`eu`,`exu`,`exh`}
    Extensions[42..56] = {`pro`,`ewx`,`eui`,`edx`,`err`,`gtk`,`plugin`,`pmt`,`py`,`rkt`,`rb`,`rbw`,`sql`,`v`,`wren`}
    ExtensionNos[1..43] = {1,2,2,2,2,3,4,5,5,5,5,6,7,8,9,10,10,10,11,12,13,13,13,13,13,13,14,15,15,16,17,18,19,20,20,20,20,20,20,20,20,20,20}
    ExtensionNos[44..56] = {20,20,20,20,20,21,22,23,24,24,25,26,27}
    newSyntax = 20
    SynNames[1..17] = {`None`,`ARM`,`bat`,`Blade`,`C`,`css`,`D`,`Delphi`,`FASM`,`FreeBASIC`,`fsharp`,`Go`,`Html`,`Java`,`js`,`Julia`,`lisp`}
    SynNames[18..27] = {`perl`,`perl6`,`Phix`,`Phixmonti`,`Python`,`Racket`,`Ruby`,`SQL`,`V`,`Wren`}
    LineComments[1..14] = {{`--`},{`;`,`@`,`//`},{`::`},{`#`},{`//`},{`!?`},{`//`},{`//`},{`;`,`//`},{`'`},{`//`},{`//`},{`//`},{`//`}}
    LineComments[15..27] = {{`//`},{`#`},{`#`},{`#`},{`#`},{`--`,`//`},{`//`},{`#`},{`;`},{`#`},{`--`},{`//`},{`//`}}
    BlockComments[1..11] = {{},{`/*`,`*/`},{},{`/*`,`*/`},{`/*`,`*/`},{`/*`,`*/`},{`/*`,`*/`},{`{`,`}`},{},{`/'`,`'/`},{`(*`,`*)`}}
    BlockComments[12..19] = {{`/*`,`*/`},{`<!--`,`-->`},{`/*`,`*/`},{`/*`,`*/`},{`=begin`,`=end`,`#=`,`=#`},{},{},{}}
    BlockComments[20..25] = {{`/*`,`*/`,`--/*`,`--*/`,`#[`,`#]`},{`/#`,`#/`},{`/*`,`*/`},{`=begin`,`=end`},{`=begin`,`=end`},{`/*`,`*/`}}
    BlockComments[26..27] = {{`/*`,`*/`},{`/*`,`*/`}}
    AutoCompletes[1..19] = {{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{}}
    AutoCompletes[20][1][1..4] = {-1,{-3,'i','f'},' ',0}
    AutoCompletes[20][1][5] = {{' ','t','h','e','n'},{'e','n','d',' ','i','f'}}
    AutoCompletes[20][2][1..4] = {-1,{-3,'i','f','d'},'e',2}
    AutoCompletes[20][2][5][1] = {'f',' ',' ','t','h','e','n'}
    AutoCompletes[20][2][5][2] = {'e','n','d',' ','i','f','d','e','f'}
    AutoCompletes[20][3][1..4] = {-1,{-3,'f','o','r'},' ',0}
    AutoCompletes[20][3][5] = {{' ','t','o',' ',' ','d','o'},{'e','n','d',' ','f','o','r'}}
    AutoCompletes[20][4][1..4] = {-1,{-3,'w','h','i','l'},'e',1}
    AutoCompletes[20][4][5] = {{' ',' ','d','o'},{'e','n','d',' ','w','h','i','l','e'}}
    AutoCompletes[20][5] = {-1,{-3,'e','l','s','i','f'},' ',0,{{' ','t','h','e','n'}}}
    AutoCompletes[20][6][1..4] = {-1,{-3,'e','l','s','i','f','d'},'e',2}
    AutoCompletes[20][6][5] = {{'f',' ',' ','t','h','e','n'}}
    AutoCompletes[20][7] = {-1,{-3,'l','o','o'},'p',6,{{' ','d','o'},{'u','n','t','i','l',' '}}}
    AutoCompletes[20][8][1..4] = {-1,{-3,'s','w','i'},'t',3}
    AutoCompletes[20][8][5][1] = {'c','h',' ',' ','d','o'}
    AutoCompletes[20][8][5][2] = {'e','n','d',' ','s','w','i','t','c','h'}
    AutoCompletes[20][9] = {-1,{-3,'c','a','s','e',' '},'|',0,{{' ','t','h','e','n'}}}
    AutoCompletes[20][10][1..4] = {-1,{'f','u'},'n',6}
    AutoCompletes[20][10][5][1] = {'c','t','i','o','n',' ','(',')'}
    AutoCompletes[20][10][5][2] = {'e','n','d',' ','f','u','n','c','t','i','o','n'}
    AutoCompletes[20][11][1..4] = {-1,{'p','r'},'o',7}
    AutoCompletes[20][11][5][1] = {'c','e','d','u','r','e',' ','(',')'}
    AutoCompletes[20][11][5][2] = {'e','n','d',' ','p','r','o','c','e','d','u','r','e'}
    AutoCompletes[20][12][1..4] = {-1,{'t','y'},'p',2}
    AutoCompletes[20][12][5] = {{'e',' ','(',')'},{'e','n','d',' ','t','y','p','e'}}
    AutoCompletes[20][13] = {-1,{'g'},'l',5,{{'o','b','a','l',' '}}}
    AutoCompletes[20][14][1..4] = {-1,{'g','l','o','b','a','l',' ','f','u'},'n',6}
    AutoCompletes[20][14][5][1] = {'c','t','i','o','n',' ','(',')'}
    AutoCompletes[20][14][5][2] = {'e','n','d',' ','f','u','n','c','t','i','o','n'}
    AutoCompletes[20][15][1..4] = {-1,{'g','l','o','b','a','l',' ','p','r'},'o',7}
    AutoCompletes[20][15][5][1] = {'c','e','d','u','r','e',' ','(',')'}
    AutoCompletes[20][15][5][2] = {'e','n','d',' ','p','r','o','c','e','d','u','r','e'}
    AutoCompletes[20][16][1..4] = {-1,{'g','l','o','b','a','l',' ','t','y'},'p',2}
    AutoCompletes[20][16][5] = {{'e',' ','(',')'},{'e','n','d',' ','t','y','p','e'}}
    AutoCompletes[20][17] = {-1,{'p','u'},'b',4,{{'l','i','c',' '}}}
    AutoCompletes[20][18][1..4] = {-1,{'p','u','b','l','i','c',' ','f','u'},'n',6}
    AutoCompletes[20][18][5][1] = {'c','t','i','o','n',' ','(',')'}
    AutoCompletes[20][18][5][2] = {'e','n','d',' ','f','u','n','c','t','i','o','n'}
    AutoCompletes[20][19][1..4] = {-1,{'p','u','b','l','i','c',' ','p','r'},'o',7}
    AutoCompletes[20][19][5][1] = {'c','e','d','u','r','e',' ','(',')'}
    AutoCompletes[20][19][5][2] = {'e','n','d',' ','p','r','o','c','e','d','u','r','e'}
    AutoCompletes[20][20][1..4] = {-1,{'p','u','b','l','i','c',' ','t','y'},'p',2}
    AutoCompletes[20][20][5] = {{'e',' ','(',')'},{'e','n','d',' ','t','y','p','e'}}
    AutoCompletes[20][21] = {-1,{'e','x'},'p',4,{{'o','r','t',' '}}}
    AutoCompletes[20][22][1..4] = {-1,{'e','x','p','o','r','t',' ','f','u'},'n',6}
    AutoCompletes[20][22][5][1] = {'c','t','i','o','n',' ','(',')'}
    AutoCompletes[20][22][5][2] = {'e','n','d',' ','f','u','n','c','t','i','o','n'}
    AutoCompletes[20][23][1..4] = {-1,{'e','x','p','o','r','t',' ','p','r'},'o',7}
    AutoCompletes[20][23][5][1] = {'c','e','d','u','r','e',' ','(',')'}
    AutoCompletes[20][23][5][2] = {'e','n','d',' ','p','r','o','c','e','d','u','r','e'}
    AutoCompletes[20][24][1..4] = {-1,{'e','x','p','o','r','t',' ','t','y'},'p',2}
    AutoCompletes[20][24][5] = {{'e',' ','(',')'},{'e','n','d',' ','t','y','p','e'}}
    AutoCompletes[20][25] = {-1,{'a'},'t',3,{{'o','m',' '}}}
    AutoCompletes[20][26] = {-1,{'i','n'},'t',5,{{'e','g','e','r',' '}}}
    AutoCompletes[20][27] = {-1,{'s','e'},'q',6,{{'u','e','n','c','e',' '}}}
    AutoCompletes[20][28] = {-1,{'s','t'},'r',4,{{'i','n','g',' '}}}
    AutoCompletes[20][29] = {-1,{'o','b'},'j',4,{{'e','c','t',' '}}}
    AutoCompletes[20][30] = {-1,{'c','o'},'n',6,{{'s','t','a','n','t',' '}}}
    AutoCompletes[20][31] = {-1,{'e','n'},'u',2,{{'m',' '}}}
    AutoCompletes[20][32] = {-1,{'g','l','o','b','a','l',' ','a'},'t',3,{{'o','m',' '}}}
    AutoCompletes[20][33][1..4] = {-1,{'g','l','o','b','a','l',' ','i','n'},'t',5}
    AutoCompletes[20][33][5] = {{'e','g','e','r',' '}}
    AutoCompletes[20][34][1..4] = {-1,{'g','l','o','b','a','l',' ','s','e'},'q',6}
    AutoCompletes[20][34][5] = {{'u','e','n','c','e',' '}}
    AutoCompletes[20][35] = {-1,{'g','l','o','b','a','l',' ','s','t'},'r',4,{{'i','n','g',' '}}}
    AutoCompletes[20][36] = {-1,{'g','l','o','b','a','l',' ','o','b'},'j',4,{{'e','c','t',' '}}}
    AutoCompletes[20][37][1..4] = {-1,{'g','l','o','b','a','l',' ','c','o'},'n',6}
    AutoCompletes[20][37][5] = {{'s','t','a','n','t',' '}}
    AutoCompletes[20][38] = {-1,{'g','l','o','b','a','l',' ','e','n'},'u',2,{{'m',' '}}}
    AutoCompletes[20][39] = {-1,{'p','u','b','l','i','c',' ','a'},'t',3,{{'o','m',' '}}}
    AutoCompletes[20][40][1..4] = {-1,{'p','u','b','l','i','c',' ','i','n'},'t',5}
    AutoCompletes[20][40][5] = {{'e','g','e','r',' '}}
    AutoCompletes[20][41][1..4] = {-1,{'p','u','b','l','i','c',' ','s','e'},'q',6}
    AutoCompletes[20][41][5] = {{'u','e','n','c','e',' '}}
    AutoCompletes[20][42] = {-1,{'p','u','b','l','i','c',' ','s','t'},'r',4,{{'i','n','g',' '}}}
    AutoCompletes[20][43] = {-1,{'p','u','b','l','i','c',' ','o','b'},'j',4,{{'e','c','t',' '}}}
    AutoCompletes[20][44][1..4] = {-1,{'p','u','b','l','i','c',' ','c','o'},'n',6}
    AutoCompletes[20][44][5] = {{'s','t','a','n','t',' '}}
    AutoCompletes[20][45] = {-1,{'p','u','b','l','i','c',' ','e','n'},'u',2,{{'m',' '}}}
    AutoCompletes[20][46] = {-1,{'e','x','p','o','r','t',' ','a'},'t',3,{{'o','m',' '}}}
    AutoCompletes[20][47][1..4] = {-1,{'e','x','p','o','r','t',' ','i','n'},'t',5}
    AutoCompletes[20][47][5] = {{'e','g','e','r',' '}}
    AutoCompletes[20][48][1..4] = {-1,{'e','x','p','o','r','t',' ','s','e'},'q',6}
    AutoCompletes[20][48][5] = {{'u','e','n','c','e',' '}}
    AutoCompletes[20][49][1..4] = {-1,{'e','x','p','o','r','t',' ','s','t'},'r',4}
    AutoCompletes[20][49][5] = {{'i','n','g',' '}}
    AutoCompletes[20][50] = {-1,{'e','x','p','o','r','t',' ','o','b'},'j',4,{{'e','c','t',' '}}}
    AutoCompletes[20][51][1..4] = {-1,{'e','x','p','o','r','t',' ','c','o'},'n',6}
    AutoCompletes[20][51][5] = {{'s','t','a','n','t',' '}}
    AutoCompletes[20][52] = {-1,{'e','x','p','o','r','t',' ','e','n'},'u',2,{{'m',' '}}}
    AutoCompletes[20][53] = {-2,{'a'},'t',3,{{'o','m',' '}}}
    AutoCompletes[20][54] = {-2,{'i','n'},'t',5,{{'e','g','e','r',' '}}}
    AutoCompletes[20][55] = {-2,{'o','b'},'j',4,{{'e','c','t',' '}}}
    AutoCompletes[20][56] = {-2,{'s','e'},'q',6,{{'u','e','n','c','e',' '}}}
    AutoCompletes[20][57] = {-2,{'s','t'},'r',4,{{'i','n','g',' '}}}
    AutoCompletes[21..27] = {{},{},{},{},{},{},{}}
    EscapeLeadIns[1..16] = {-1,'\\','\\','\\','\\','\\','\\','\\','\\','\\','\\','\\','\\','\\','\\','\\'}
    EscapeLeadIns[17..27] = {'\\','\\','\\','\\','\\','\\','\\','\\',-1,'\\','\\'}
    Escapes[1..3] = {{},{'n','3','0','t','r'},{}}
    Escapes[4][1..17] = {'r','n','t','"','0','\\',''','x','a','b','f','v','u','/','%','e','s'}
    Escapes[4][18] = '<'
    Escapes[5] = {'r','n','t','"','0','1','2','3','4','5','6','7','\\',''','x','`'}
    Escapes[6][1..18] = {'r','n','t','"','0','\\',''','x','v','u','/','0','1','2','3','4','5','6'}
    Escapes[6][19..33] = {'7','8','9','A','B','C','D','E','F','a','b','c','d','e','f'}
    Escapes[7..8] = {{'r','n','t','\\',''','"','0','x'},{'r','n','t','\\',''','\\','"','#','x'}}
    Escapes[9] = {'\\','"',''','n','t','r','e'}
    Escapes[10] = {'r','n','t','\\',''','"','e','E','#','x','0','b','u','U'}
    Escapes[11] = {'r','n','t','"','0','\\',''','x','a','b','f','v','u','/','2'}
    Escapes[12] = {'r','n','t','b','\\',''','"','u'}
    Escapes[13] = {'r','n','t','u','\\',''','"','U','b','#','x','e','E','0','3'}
    Escapes[14] = {'r','n','t','"','0','\\',''','x'}
    Escapes[15][1..17] = {'r','n','t','"','0','\\',''','x','a','b','f','v','u','/','2','(',')'}
    Escapes[15][18..24] = {'s','{','}','+','[',']','#'}
    Escapes[16][1..17] = {'r','n','t','\\',''','"','=','<','+','-','*','/','s','d','e','{','.'}
    Escapes[16][18..19] = {'}','u'}
    Escapes[17][1..18] = {'r','n','t','\\',''','"','#','[','%','.','/','!',',','|',')','+','$','''}
    Escapes[17][19..23] = {'&','`','{','(','w'}
    Escapes[18][1..18] = {'r','n','t','\\',''','"','#','[','%','.','/','!',',','|',')','+','$','''}
    Escapes[18][19..23] = {'&','`','{','(','w'}
    Escapes[19][1..18] = {'r','n','t','\\',''','"','#','[','%','.','/','!',',','|',')','+','$','''}
    Escapes[19][19..23] = {'&','`','{','(','w'}
    Escapes[20] = {'r','n','t','\\',''','"','e','E','#','x','0','b','u','U'}
    Escapes[21] = {'r','n','t','"','0','\\',''','x','a','b','f','v'}
    Escapes[22][1..18] = {'r','n','s','t','\\',''','"','#','x','0','b','u','U','*','$','?','(',')'}
    Escapes[22][19..34] = {'[',']','d','.','S','w','+','|','^','{','}','A','B','-','*','W'}
    Escapes[23..25] = {{'r','n','t','\\',''','"'},{'r','n','t','\\',''','"'},{}}
    Escapes[26] = {'r','n','t','\\','''}
    Escapes[27] = {'r','n','t','"','0','\\',''','x','a','b','f','v','u','/','%','e'}
    BraceLevels = {0,5,5,5,5,5,7,7,5,7,5,7,5,5,5,7,7,7,7,7,5,7,7,7,5,5,5}
    OperatorSets[1..3] = {{},{`,`,`~`,`=`,`<`,`>`,`+`,`-`,`*`,`/`,`&`,`|`},{`,`,`~`,`=`,`<`,`>`,`+`,`-`,`*`,`/`,`&`,`|`,`;`}}
    OperatorSets[4][1..25] = {`<`,`<=`,`>`,`>=`,`=`,`!=`,`+`,`+=`,`-`,`-=`,`*`,`*=`,`/`,`/=`,`..`,`&`,`&=`,`|`,`||`,`^=`,`!`,`\`,`~`,`%`,`^`}
    OperatorSets[4][26..31] = {`.`,`;`,`:`,`,`,`?`,`@`}
    OperatorSets[5][1..25] = {`<`,`<=`,`>`,`>=`,`=`,`!=`,`+`,`+=`,`-`,`-=`,`*`,`*=`,`/`,`/=`,`..`,`&`,`&=`,`|`,`||`,`^=`,`!`,`\`,`~`,`%`,`^`}
    OperatorSets[5][26..30] = {`.`,`;`,`:`,`,`,`?`}
    OperatorSets[6] = {`.`,`:`,`;`,`>`,`%`,`!`,`@`,`/`,`+`,`*`,`=`,`^`}
    OperatorSets[7][1..25] = {`..`,`!`,`+=`,`=`,`==`,`?`,`:`,`.`,`%`,`<`,`<=`,`>`,`>=`,`>>`,`+`,`-`,`/`,`*`,`/=`,`~=`,`++`,`--`,`&`,`@`,`~`}
    OperatorSets[7][26..27] = {`|`,`||`}
    OperatorSets[8][1..24] = {`|`,`||`,`&&`,`.=`,`.`,`,`,`<`,`<=`,`>`,`>=`,`=`,`:=`,`!=`,`+`,`+=`,`-`,`-=`,`*`,`*=`,`/`,`/=`,`..`,`&`,`&=`}
    OperatorSets[8][25..36] = {`?`,`:`,`;`,`@`,`\`,`~`,`^`,`!`,"`",`$#`,`$`,`#`}
    OperatorSets[9] = {`,`,`~`,`=`,`<`,`>`,`+`,`-`,`*`,`/`,`&`,`|`}
    OperatorSets[10][1..24] = {`\`,`,`,`=`,`:=`,`==`,`!=`,`<`,`<=`,`>`,`>=`,`@`,`+`,`-`,`*`,`/`,`+=`,`-=`,`*=`,`/=`,`..`,`&`,`&=`,`?`,`;`}
    OperatorSets[10][25..28] = {`:`,`|`,`~`,`.`}
    OperatorSets[11][1..22] = {`<`,`<=`,`>`,`>=`,`=`,`<>`,`+`,`-`,`*`,`/`,`&&`,`&&&`,`||`,`|||`,`<<<`,`>>>`,`^^^`,`~~~`,`@`,`#`,`->`,`|>`}
    OperatorSets[11][23..44] = {`||>`,`|||>`,`>>`,`<<`,`<|`,`<||`,`<|||`,`:`,`:>`,`:?`,`:?>`,`^`,`.`,`::`,`[|`,`|]`,`<-`,`!`,`&`,`_`,`|`,`?`}
    OperatorSets[11][45..51] = {`~~`,`~-`,`~+`,`:=`,`%`,`<'`,`'`}
    OperatorSets[12] = {`<`,`<=`,`>`,`>=`,`:=`,`:`,`||`,`%`,`&`,`!`,`=`,`==`,`!=`,`+=`,`-=`,`/=`,`*=`,`+`,`-`,`/`,`*`,`[`,`]`,`^`,`|`}
    OperatorSets[13] = {`=`,`+`,`*`,`|`,`!`,`?`,`~`}
    OperatorSets[14][1..24] = {`<`,`<=`,`>`,`>=`,`=`,`!=`,`+`,`+=`,`-`,`-=`,`*`,`*=`,`/`,`/=`,`..`,`&`,`&=`,`|`,`||`,`^=`,`!`,`\`,`~`,`%`}
    OperatorSets[14][25..30] = {`^`,`.`,`;`,`:`,`,`,`?`}
    OperatorSets[15][1..24] = {`<`,`<=`,`>`,`>=`,`=`,`!=`,`+`,`+=`,`-`,`-=`,`*`,`*!/`,`*=`,`/`,`/=`,`&`,`&=`,`|`,`||`,`^=`,`!`,`\`,`~`,`%`}
    OperatorSets[15][25..30] = {`^`,`;`,`:`,`,`,`?`,`@`}
    OperatorSets[16][1..25] = {`<`,`<=`,`>`,`>=`,`:=`,`:`,`||`,`%`,`&`,`!`,`=`,`==`,`!=`,`+=`,`-=`,`/=`,`*=`,`+`,`-`,`/`,`*`,`[`,`]`,`^`,`@`}
    OperatorSets[16][26..30] = {`...`,`.`,`|`,`|=`,`~`}
    OperatorSets[17] = {`=:`,`*`,`*!/`,`?`,`+`,`-`,`:`,`::`,"`",`@`,`>`,`=`,`==`,`<>`,`'`,`<`,`.`}
    OperatorSets[18][1..24] = {`~`,`|`,`^`,`||`,`&&`,`.=`,`.`,`,`,`<`,`<=`,`>`,`>=`,`=`,`!=`,`+`,`+=`,`-`,`-=`,`*`,`*=`,`/`,`/=`,`..`,`&`}
    OperatorSets[18][25..30] = {`&=`,`?`,`;`,`\`,`@`,`!`}
    OperatorSets[19][1..24] = {`~`,`|`,`^`,`||`,`&&`,`.=`,`.`,`,`,`<`,`<=`,`>`,`>=`,`=`,`!=`,`+`,`+=`,`-`,`-=`,`*`,`*=`,`/`,`/=`,`..`,`&`}
    OperatorSets[19][25..30] = {`&=`,`?`,`;`,`\`,`@`,`!`}
    OperatorSets[20][1..24] = {`,`,`=`,`:=`,`==`,`!=`,`<`,`<=`,`>`,`>=`,`+`,`-`,`*`,`/`,`+=`,`-=`,`*=`,`/=`,`..`,`&`,`&=`,`?`,`;`,`:`,`|`}
    OperatorSets[20][25..28] = {`~`,`.`,`@`,`@@:`}
    OperatorSets[21] = {`+`,`-`,`*`,`/`,`==`,`<=`,`>=`}
    OperatorSets[22][1..24] = {`:`,`.`,`,`,`<`,`<=`,`>`,`>=`,`=`,`!=`,`+`,`+=`,`-`,`-=`,`*`,`*=`,`/`,`/=`,`..`,`&`,`&=`,`?`,`;`,`%`,`|=`}
    OperatorSets[22][25..29] = {`^=`,`|`,`\`,`^`,`@`}
    OperatorSets[23] = {`'`,`<`}
    OperatorSets[24][1..24] = {`<`,`<=`,`>`,`>=`,`::`,`=`,`==`,`!=`,`+=`,`-=`,`/=`,`*=`,`+`,`-`,`/`,`*`,`[`,`]`,`||`,`|`,`&:`,`%`,`:`,`.`}
    OperatorSets[24][25..31] = {`^`,`\`,`~`,`!`,`@`,`$`,`<<`}
    OperatorSets[25] = {`=`,`+`,`*`,`&`,`|`,`/`,`>`,`<`,`!`,`?`,`'`,`-`,`%`}
    OperatorSets[26][1..25] = {`<`,`>`,`;`,`=`,`.`,`+`,`-`,`,`,`:`,`*`,`/`,`||`,`:=`,`!`,`&&`,`<<`,`>>`,`&`,`|`,`^`,`++`,`--`,`+=`,`!=`,`==`}
    OperatorSets[26][26..27] = {`?`,`%`}
    OperatorSets[27][1..25] = {`<`,`<=`,`>`,`>=`,`=`,`!=`,`+`,`+=`,`-`,`-=`,`*`,`*=`,`/`,`/=`,`&`,`&=`,`|`,`||`,`^=`,`!`,`\`,`~`,`%`,`^`,`;`}
    OperatorSets[27][26..29] = {`:`,`,`,`?`,`@`}
    Indents[1..6] = {{{},{},{},{}},{{},{},{},{}},{{},{},{},{}},{{},{},`{}`,{1,-1}},{{},{},`{}`,{1,-1}},{{},{},`{}`,{1,-1}}}
    Indents[7..9] = {{{},{},`{}`,{1,-1}},{{`then`,`do`,`else`,`end`},{1,1,1,-1},{},{}},{{},{},`{}`,{1,-1}}}
    Indents[10..14] = {{{`then`,`do`,`else`,`end`},{1,1,1,-1},{},{}},{{},{},{},{}},{{},{},`{}`,{1,-1}},{{},{},{},{}},{{},{},`{}`,{1,-1}}}
    Indents[15..17] = {{{},{},`{}`,{1,-1}},{{},{},`{}`,{1,-1}},{{`then`,`do`,`else`,`end`},{1,1,1,-1},{},{}}}
    Indents[18..19] = {{{`then`,`do`,`else`,`end`},{1,1,1,-1},{},{}},{{`then`,`do`,`else`,`end`},{1,1,1,-1},{},{}}}
    Indents[20] = {{`then`,`do`,`else`,`end`},{1,1,1,-1},{},{}}
    Indents[21] = {{`if`,`else`,`for`,`def`,`while`,`endif`,`endfor`,`enddef`,`endwhile`},{1,1,1,1,1,-1,-1,-1,-1},{},{}}
    Indents[22..24] = {{{`then`,`do`,`else`,`end`},{1,1,1,-1},{},{}},{{},{},`()`,{1,-1}},{{`if`,`do`,`for`,`else`,`end`},{1,1,1,1,-1},{},{}}}
    Indents[25..27] = {{{},{},{},{}},{{},{},`{}`,{1,-1}},{{},{},`{}`,{1,-1}}}
    Sections[1][1..10] = {`Background`,`Comments`,`Highlight`,`Current Line`,`Strings`,`Illegals`,`Operators`,`URLs`,`Other`,`Marginbg`}
    Sections[1][11..20] = {`Linenos`,`Bookmarks`,`Brace1`,`Brace2`,`Brace3`,`Brace4`,`Brace5`,`Brace6`,`Brace7`,`URLS`}
    Sections[2][1..10] = {`Background`,`Comments`,`Highlight`,`Current Line`,`Strings`,`Illegals`,`Operators`,`URLs`,`Other`,`Marginbg`}
    Sections[2][11..20] = {`Linenos`,`Bookmarks`,`Brace1`,`Brace2`,`Brace3`,`Brace4`,`Brace5`,`Brace6`,`Brace7`,`--File Extensions`}
    Sections[2][21..25] = {`Reserved Words`,`Opcodes`,`Registers`,`Constants`,`Illegal`}
    Sections[3][1..10] = {`Background`,`Comments`,`Highlight`,`Current Line`,`Strings`,`Illegals`,`Operators`,`URLs`,`Other`,`Marginbg`}
    Sections[3][11..20] = {`Linenos`,`Bookmarks`,`Brace1`,`Brace2`,`Brace3`,`Brace4`,`Brace5`,`Brace6`,`Brace7`,`Commands`}
    Sections[4][1..10] = {`Background`,`Comments`,`Highlight`,`Current Line`,`Strings`,`Illegals`,`Operators`,`URLs`,`Other`,`Marginbg`}
    Sections[4][11..21] = {`Linenos`,`Bookmarks`,`Brace1`,`Brace2`,`Brace3`,`Brace4`,`Brace5`,`Brace6`,`Brace7`,`Reserved Words`,`Constants`}
    Sections[4][22..23] = {`Types`,`Library`}
    Sections[5][1..10] = {`Background`,`Comments`,`Highlight`,`Current Line`,`Strings`,`Illegals`,`Operators`,`URLs`,`Other`,`Marginbg`}
    Sections[5][11..21] = {`Linenos`,`Bookmarks`,`Brace1`,`Brace2`,`Brace3`,`Brace4`,`Brace5`,`Brace6`,`Brace7`,`Reserved Words`,`Constants`}
    Sections[5][22..25] = {`Preprocessor`,`Types`,`clib`,`Borland`}
    Sections[6][1..10] = {`Background`,`Comments`,`Highlight`,`Current Line`,`Strings`,`Illegals`,`Operators`,`URLs`,`Other`,`Marginbg`}
    Sections[6][11..21] = {`Linenos`,`Bookmarks`,`Brace1`,`Brace2`,`Brace3`,`Brace4`,`Brace5`,`Brace6`,`Brace7`,`Tags`,`Properties`}
    Sections[6][22] = `Constants`
    Sections[7][1..10] = {`Background`,`Comments`,`Highlight`,`Current Line`,`Strings`,`Illegals`,`Operators`,`URLs`,`Other`,`Marginbg`}
    Sections[7][11..22] = {`Linenos`,`Bookmarks`,`Brace1`,`Brace2`,`Brace3`,`Brace4`,`Brace5`,`Brace6`,`Brace7`,`Reserve Words`,`--`,`Types`}
    Sections[7][23..25] = {`Constants`,`Functions`,`Imports`}
    Sections[8][1..10] = {`Background`,`Comments`,`Highlight`,`Current Line`,`Strings`,`Illegals`,`Operators`,`URLs`,`Other`,`Marginbg`}
    Sections[8][11..20] = {`Linenos`,`Bookmarks`,`Brace1`,`Brace2`,`Brace3`,`Brace4`,`Brace5`,`Brace6`,`Brace7`,`Reserved Words`}
    Sections[8][21..24] = {`With Clauses`,`Types`,`Builtin Routines`,`Library Routines`}
    Sections[9][1..10] = {`Background`,`Comments`,`Highlight`,`Current Line`,`Strings`,`Illegals`,`Operators`,`URLs`,`Other`,`Marginbg`}
    Sections[9][11..21] = {`Linenos`,`Bookmarks`,`Brace1`,`Brace2`,`Brace3`,`Brace4`,`Brace5`,`Brace6`,`Brace7`,`Reserved Words`,`Opcodes`}
    Sections[9][22..27] = {`P6And Above`,`MMX`,`SSE`,`Registers`,`Windows API`,`Constants`}
    Sections[10][1..10] = {`Background`,`Comments`,`Highlight`,`Current Line`,`Strings`,`Illegals`,`Operators`,`URLs`,`Other`,`Marginbg`}
    Sections[10][11..21] = {`Linenos`,`Bookmarks`,`Brace1`,`Brace2`,`Brace3`,`Brace4`,`Brace5`,`Brace6`,`Brace7`,`Reserved Words`,`Types`}
    Sections[10][22..24] = {`Builtin Routines`,`Builtin Constants`,`Library Routines`}
    Sections[11][1..10] = {`Background`,`Comments`,`Highlight`,`Current Line`,`Strings`,`Illegals`,`Operators`,`URLs`,`Other`,`Marginbg`}
    Sections[11][11..21] = {`Linenos`,`Bookmarks`,`Brace1`,`Brace2`,`Brace3`,`Brace4`,`Brace5`,`Brace6`,`Brace7`,`Reserved Words`,`Constants`}
    Sections[11][22..25] = {`Types`,`properties`,`lib`,`illegal`}
    Sections[12][1..10] = {`Background`,`Comments`,`Highlight`,`Current Line`,`Strings`,`Illegals`,`Operators`,`URLs`,`Other`,`Marginbg`}
    Sections[12][11..21] = {`Linenos`,`Bookmarks`,`Brace1`,`Brace2`,`Brace3`,`Brace4`,`Brace5`,`Brace6`,`Brace7`,`Reserve Words`,`Types`}
    Sections[12][22..24] = {`Constants`,`Functions`,`Imports`}
    Sections[13][1..10] = {`Background`,`Comments`,`Highlight`,`Current Line`,`Strings`,`Illegals`,`Operators`,`URLs`,`Other`,`Marginbg`}
    Sections[13][11..22] = {`Linenos`,`Bookmarks`,`Brace1`,`Brace2`,`Brace3`,`Brace4`,`Brace5`,`Brace6`,`Brace7`,`Tags`,`Attributes`,`PHP`}
    Sections[14][1..10] = {`Background`,`Comments`,`Highlight`,`Current Line`,`Strings`,`Illegals`,`Operators`,`URLs`,`Other`,`Marginbg`}
    Sections[14][11..21] = {`Linenos`,`Bookmarks`,`Brace1`,`Brace2`,`Brace3`,`Brace4`,`Brace5`,`Brace6`,`Brace7`,`Reserved Words`,`Constants`}
    Sections[14][22..23] = {`Types`,`lib`}
    Sections[15][1..10] = {`Background`,`Comments`,`Highlight`,`Current Line`,`Strings`,`Illegals`,`Operators`,`URLs`,`Other`,`Marginbg`}
    Sections[15][11..21] = {`Linenos`,`Bookmarks`,`Brace1`,`Brace2`,`Brace3`,`Brace4`,`Brace5`,`Brace6`,`Brace7`,`Reserved Words`,`Constants`}
    Sections[15][22..26] = {`Types`,`properties`,`lib`,`illegal`,`--`}
    Sections[16][1..10] = {`Background`,`Comments`,`Highlight`,`Current Line`,`Strings`,`Illegals`,`Operators`,`URLs`,`Other`,`Marginbg`}
    Sections[16][11..21] = {`Linenos`,`Bookmarks`,`Brace1`,`Brace2`,`Brace3`,`Brace4`,`Brace5`,`Brace6`,`Brace7`,`Reserve Words`,`Constants`}
    Sections[16][22..23] = {`Types`,`Functions`}
    Sections[17][1..10] = {`Background`,`Comments`,`Highlight`,`Current Line`,`Strings`,`Illegals`,`Operators`,`URLs`,`Other`,`Marginbg`}
    Sections[17][11..20] = {`Linenos`,`Bookmarks`,`Brace1`,`Brace2`,`Brace3`,`Brace4`,`Brace5`,`Brace6`,`Brace7`,`Reserved Words`}
    Sections[17][21] = `Builtin Constants`
    Sections[18][1..10] = {`Background`,`Comments`,`Highlight`,`Current Line`,`Strings`,`Illegals`,`Operators`,`URLs`,`Other`,`Marginbg`}
    Sections[18][11..21] = {`Linenos`,`Bookmarks`,`Brace1`,`Brace2`,`Brace3`,`Brace4`,`Brace5`,`Brace6`,`Brace7`,`Reserved Words`,`Types`}
    Sections[18][22..24] = {`Builtin Routines`,`Builtin Constants`,`Library Routines`}
    Sections[19][1..10] = {`Background`,`Comments`,`Highlight`,`Current Line`,`Strings`,`Illegals`,`Operators`,`URLs`,`Other`,`Marginbg`}
    Sections[19][11..21] = {`Linenos`,`Bookmarks`,`Brace1`,`Brace2`,`Brace3`,`Brace4`,`Brace5`,`Brace6`,`Brace7`,`Reserved Words`,`Types`}
    Sections[19][22..24] = {`Builtin Routines`,`Builtin Constants`,`Library Routines`}
    Sections[20][1..10] = {`Background`,`Comments`,`Highlight`,`Current Line`,`Strings`,`Illegals`,`Operators`,`URLs`,`Other`,`Marginbg`}
    Sections[20][11..21] = {`Linenos`,`Bookmarks`,`Brace1`,`Brace2`,`Brace3`,`Brace4`,`Brace5`,`Brace6`,`Brace7`,`Reserved Words`,`Reserved4`}
    Sections[20][22..29] = {`With Clauses`,`Types`,`Builtin Routines`,`Builtin4`,`Builtin Constants`,`Library Routines`,`Library4`,`Win32lib`}
    Sections[20][30..31] = {`Win32lib Constants`,`Incompatible`}
    Sections[21][1..10] = {`Background`,`Comments`,`Highlight`,`Current Line`,`Strings`,`Illegals`,`Operators`,`URLs`,`Other`,`Marginbg`}
    Sections[21][11..21] = {`Linenos`,`Bookmarks`,`Brace1`,`Brace2`,`Brace3`,`Brace4`,`Brace5`,`Brace6`,`Brace7`,`Reserved Words`,`Constants`}
    Sections[21][22..23] = {`Types`,`lib`}
    Sections[22][1..10] = {`Background`,`Comments`,`Highlight`,`Current Line`,`Strings`,`Illegals`,`Operators`,`URLs`,`Other`,`Marginbg`}
    Sections[22][11..21] = {`Linenos`,`Bookmarks`,`Brace1`,`Brace2`,`Brace3`,`Brace4`,`Brace5`,`Brace6`,`Brace7`,`Reserved Words`,`Types`}
    Sections[22][22..24] = {`Builtin Routines`,`Builtin Constants`,`Library Routines`}
    Sections[23][1..10] = {`Background`,`Comments`,`Highlight`,`Current Line`,`Strings`,`Illegals`,`Operators`,`URLs`,`Other`,`Marginbg`}
    Sections[23][11..21] = {`Linenos`,`Bookmarks`,`Brace1`,`Brace2`,`Brace3`,`Brace4`,`Brace5`,`Brace6`,`Brace7`,`Reserve Words`,`Constants`}
    Sections[23][22..23] = {`Functions`,`Libraries`}
    Sections[24][1..10] = {`Background`,`Comments`,`Highlight`,`Current Line`,`Strings`,`Illegals`,`Operators`,`URLs`,`Other`,`Marginbg`}
    Sections[24][11..21] = {`Linenos`,`Bookmarks`,`Brace1`,`Brace2`,`Brace3`,`Brace4`,`Brace5`,`Brace6`,`Brace7`,`Reserve Words`,`Functions`}
    Sections[25][1..10] = {`Background`,`Comments`,`Highlight`,`Current Line`,`Strings`,`Illegals`,`Operators`,`URLs`,`Other`,`Marginbg`}
    Sections[25][11..21] = {`Linenos`,`Bookmarks`,`Brace1`,`Brace2`,`Brace3`,`Brace4`,`Brace5`,`Brace6`,`Brace7`,`Reserved Words`,`Types`}
    Sections[25][22] = `Builtins`
    Sections[26][1..10] = {`Background`,`Comments`,`Highlight`,`Current Line`,`Strings`,`Illegals`,`Operators`,`URLs`,`Other`,`Marginbg`}
    Sections[26][11..21] = {`Linenos`,`Bookmarks`,`Brace1`,`Brace2`,`Brace3`,`Brace4`,`Brace5`,`Brace6`,`Brace7`,`Reserved Words`,`Constants`}
    Sections[26][22..24] = {`Preprocessor`,`Types`,`clib`}
    Sections[27][1..10] = {`Background`,`Comments`,`Highlight`,`Current Line`,`Strings`,`Illegals`,`Operators`,`URLs`,`Other`,`Marginbg`}
    Sections[27][11..21] = {`Linenos`,`Bookmarks`,`Brace1`,`Brace2`,`Brace3`,`Brace4`,`Brace5`,`Brace6`,`Brace7`,`Reserved Words`,`Constants`}
    Sections[27][22..23] = {`Types`,`lib`}
    WordLists[1] = {{`ftp`,`gopher`,`http`,`https`,`mailto`,`news`,`nntp`,`telnet`,`wais`,`file`,`prospero`,`edit`,`tel`,`urn`}}
    WordLists[2][1] = {`s`,`inc`,`arm`}
    WordLists[2][2][1..12] = {`.align`,`.ascii`,`.asciz`,`.bss`,`.byte`,`.data`,`.double`,`.equ`,`.fill`,`.float`,`.global`,`.include`}
    WordLists[2][2][13..19] = {`.int`,`.skip`,`.space`,`.struct`,`.text`,`.word`,`FPSCR`}
    WordLists[2][3][1..13] = {`adc`,`adcs`,`adccc`,`adcccs`,`adccs`,`adccss`,`adceq`,`adceqs`,`adcge`,`adcges`,`adcgt`,`adcgts`,`adchi`}
    WordLists[2][3][14..26] = {`adchis`,`adchs`,`adchss`,`adcle`,`adcles`,`adclo`,`adclos`,`adcls`,`adclss`,`adclt`,`adclts`,`adcmi`,`adcmis`}
    WordLists[2][3][27..39] = {`adcne`,`adcnes`,`adcpl`,`adcpls`,`adcvc`,`adcvcs`,`adcvs`,`adcvss`,`adcal`,`adcals`,`add`,`adds`,`addcc`}
    WordLists[2][3][40..52] = {`addccs`,`addcs`,`addcss`,`addeq`,`addeqs`,`addge`,`addges`,`addgt`,`addgts`,`addhi`,`addhis`,`addhs`,`addhss`}
    WordLists[2][3][53..65] = {`addle`,`addles`,`addlo`,`addlos`,`addls`,`addlss`,`addlt`,`addlts`,`addmi`,`addmis`,`addne`,`addnes`,`addpl`}
    WordLists[2][3][66..78] = {`addpls`,`addvc`,`addvcs`,`addvs`,`addvss`,`addal`,`addals`,`and`,`ands`,`andcc`,`andccs`,`andcs`,`andcss`}
    WordLists[2][3][79..91] = {`andeq`,`andeqs`,`andge`,`andges`,`andgt`,`andgts`,`andhi`,`andhis`,`andhs`,`andhss`,`andle`,`andles`,`andlo`}
    WordLists[2][3][92..104] = {`andlos`,`andls`,`andlss`,`andlt`,`andlts`,`andmi`,`andmis`,`andne`,`andnes`,`andpl`,`andpls`,`andvc`,`andvcs`}
    WordLists[2][3][105..121] = {`andvs`,`andvss`,`andal`,`andals`,`b`,`bcc`,`bcs`,`beq`,`bge`,`bgt`,`bhi`,`bhs`,`ble`,`blo`,`bls`,`blt`,`bmi`}
    WordLists[2][3][122..136] = {`bne`,`bpl`,`bvc`,`bvs`,`bal`,`clz`,`clzcc`,`clzcs`,`clzeq`,`clzge`,`clzgt`,`clzhi`,`clzhs`,`clzle`,`clzlo`}
    WordLists[2][3][137..150] = {`clzls`,`clzlt`,`clzmi`,`clzne`,`clzpl`,`clzvc`,`clzvs`,`clzal`,`cmn`,`cmns`,`cmncc`,`cmnccs`,`cmncs`,`cmncss`}
    WordLists[2][3][151..163] = {`cmneq`,`cmneqs`,`cmnge`,`cmnges`,`cmngt`,`cmngts`,`cmnhi`,`cmnhis`,`cmnhs`,`cmnhss`,`cmnle`,`cmnles`,`cmnlo`}
    WordLists[2][3][164..176] = {`cmnlos`,`cmnls`,`cmnlss`,`cmnlt`,`cmnlts`,`cmnmi`,`cmnmis`,`cmnne`,`cmnnes`,`cmnpl`,`cmnpls`,`cmnvc`,`cmnvcs`}
    WordLists[2][3][177..189] = {`cmnvs`,`cmnvss`,`cmnal`,`cmnals`,`cmp`,`cmps`,`cmpcc`,`cmpccs`,`cmpcs`,`cmpcss`,`cmpeq`,`cmpeqs`,`cmpge`}
    WordLists[2][3][190..202] = {`cmpges`,`cmpgt`,`cmpgts`,`cmphi`,`cmphis`,`cmphs`,`cmphss`,`cmple`,`cmples`,`cmplo`,`cmplos`,`cmpls`,`cmplss`}
    WordLists[2][3][203..215] = {`cmplt`,`cmplts`,`cmpmi`,`cmpmis`,`cmpne`,`cmpnes`,`cmppl`,`cmppls`,`cmpvc`,`cmpvcs`,`cmpvs`,`cmpvss`,`cmpal`}
    WordLists[2][3][216..226] = {`cmpals`,`fabsd`,`fabsdcc`,`fabsdcs`,`fabsdeq`,`fabsdge`,`fabsdgt`,`fabsdhi`,`fabsdhs`,`fabsdle`,`fabsdlo`}
    WordLists[2][3][227..237] = {`fabsdls`,`fabsdlt`,`fabsdmi`,`fabsdne`,`fabsdpl`,`fabsdvc`,`fabsdvs`,`fabsdal`,`faddd`,`fadddcc`,`fadddcs`}
    WordLists[2][3][238..248] = {`fadddeq`,`fadddge`,`fadddgt`,`fadddhi`,`fadddhs`,`fadddle`,`fadddlo`,`fadddls`,`fadddlt`,`fadddmi`,`fadddne`}
    WordLists[2][3][249..259] = {`fadddpl`,`fadddvc`,`fadddvs`,`fadddal`,`fcmpd`,`fcmpdcc`,`fcmpdcs`,`fcmpdeq`,`fcmpdge`,`fcmpdgt`,`fcmpdhi`}
    WordLists[2][3][260..270] = {`fcmpdhs`,`fcmpdle`,`fcmpdlo`,`fcmpdls`,`fcmpdlt`,`fcmpdmi`,`fcmpdne`,`fcmpdpl`,`fcmpdvc`,`fcmpdvs`,`fcmpdal`}
    WordLists[2][3][271..281] = {`fcmpzd`,`fcpyd`,`fdivd`,`fdivdcc`,`fdivdcs`,`fdivdeq`,`fdivdge`,`fdivdgt`,`fdivdhi`,`fdivdhs`,`fdivdle`}
    WordLists[2][3][282..292] = {`fdivdlo`,`fdivdls`,`fdivdlt`,`fdivdmi`,`fdivdne`,`fdivdpl`,`fdivdvc`,`fdivdvs`,`fdivdal`,`fldd`,`fmdrr`}
    WordLists[2][3][293..303] = {`fmdrrcc`,`fmdrrcs`,`fmdrreq`,`fmdrrge`,`fmdrrgt`,`fmdrrhi`,`fmdrrhs`,`fmdrrle`,`fmdrrlo`,`fmdrrls`,`fmdrrlt`}
    WordLists[2][3][304..314] = {`fmdrrmi`,`fmdrrne`,`fmdrrpl`,`fmdrrvc`,`fmdrrvs`,`fmdrral`,`fmrrd`,`fmrrdcc`,`fmrrdcs`,`fmrrdeq`,`fmrrdge`}
    WordLists[2][3][315..325] = {`fmrrdgt`,`fmrrdhi`,`fmrrdhs`,`fmrrdle`,`fmrrdlo`,`fmrrdls`,`fmrrdlt`,`fmrrdmi`,`fmrrdne`,`fmrrdpl`,`fmrrdvc`}
    WordLists[2][3][326..337] = {`fmrrdvs`,`fmrrdal`,`fmrs`,`fmrx`,`fmrxcc`,`fmrxcs`,`fmrxeq`,`fmrxge`,`fmrxgt`,`fmrxhi`,`fmrxhs`,`fmrxle`}
    WordLists[2][3][338..349] = {`fmrxlo`,`fmrxls`,`fmrxlt`,`fmrxmi`,`fmrxne`,`fmrxpl`,`fmrxvc`,`fmrxvs`,`fmrxal`,`fmsr`,`fmstat`,`fmuld`}
    WordLists[2][3][350..360] = {`fmuldcc`,`fmuldcs`,`fmuldeq`,`fmuldge`,`fmuldgt`,`fmuldhi`,`fmuldhs`,`fmuldle`,`fmuldlo`,`fmuldls`,`fmuldlt`}
    WordLists[2][3][361..371] = {`fmuldmi`,`fmuldne`,`fmuldpl`,`fmuldvc`,`fmuldvs`,`fmuldal`,`fnegd`,`fnegdcc`,`fnegdcs`,`fnegdeq`,`fnegdge`}
    WordLists[2][3][372..382] = {`fnegdgt`,`fnegdhi`,`fnegdhs`,`fnegdle`,`fnegdlo`,`fnegdls`,`fnegdlt`,`fnegdmi`,`fnegdne`,`fnegdpl`,`fnegdvc`}
    WordLists[2][3][383..394] = {`fnegdvs`,`fnegdal`,`fmxr`,`fmxrcc`,`fmxrcs`,`fmxreq`,`fmxrge`,`fmxrgt`,`fmxrhi`,`fmxrhs`,`fmxrle`,`fmxrlo`}
    WordLists[2][3][395..406] = {`fmxrls`,`fmxrlt`,`fmxrmi`,`fmxrne`,`fmxrpl`,`fmxrvc`,`fmxrvs`,`fmxral`,`fpop`,`fpush`,`fsitod`,`fsqrtd`}
    WordLists[2][3][407..416] = {`fsqrtdcc`,`fsqrtdcs`,`fsqrtdeq`,`fsqrtdge`,`fsqrtdgt`,`fsqrtdhi`,`fsqrtdhs`,`fsqrtdle`,`fsqrtdlo`,`fsqrtdls`}
    WordLists[2][3][417..426] = {`fsqrtdlt`,`fsqrtdmi`,`fsqrtdne`,`fsqrtdpl`,`fsqrtdvc`,`fsqrtdvs`,`fsqrtdal`,`fstd`,`fsubd`,`fsubdcc`}
    WordLists[2][3][427..437] = {`fsubdcs`,`fsubdeq`,`fsubdge`,`fsubdgt`,`fsubdhi`,`fsubdhs`,`fsubdle`,`fsubdlo`,`fsubdls`,`fsubdlt`,`fsubdmi`}
    WordLists[2][3][438..451] = {`fsubdne`,`fsubdpl`,`fsubdvc`,`fsubdvs`,`fsubdal`,`ftosid`,`ftosizd`,`ja`,`jae`,`jb`,`jbe`,`jc`,`je`,`jg`}
    WordLists[2][3][452..469] = {`jge`,`jl`,`jle`,`jnae`,`jna`,`jnb`,`jnbe`,`jnc`,`jne`,`jng`,`jnge`,`jnl`,`jnle`,`jno`,`jnp`,`jns`,`jnz`,`jo`}
    WordLists[2][3][470..484] = {`jp`,`jpe`,`jpo`,`jz`,`lsr`,`lsrs`,`lsrcc`,`lsrccs`,`lsrcs`,`lsrcss`,`lsreq`,`lsreqs`,`lsrge`,`lsrges`,`lsrgt`}
    WordLists[2][3][485..497] = {`lsrgts`,`lsrhi`,`lsrhis`,`lsrhs`,`lsrhss`,`lsrle`,`lsrles`,`lsrlo`,`lsrlos`,`lsrls`,`lsrlss`,`lsrlt`,`lsrlts`}
    WordLists[2][3][498..510] = {`lsrmi`,`lsrmis`,`lsrne`,`lsrnes`,`lsrpl`,`lsrpls`,`lsrvc`,`lsrvcs`,`lsrvs`,`lsrvss`,`lsral`,`lsrals`,`mov`}
    WordLists[2][3][511..523] = {`movs`,`movcc`,`movccs`,`movcs`,`movcss`,`moveq`,`moveqs`,`movge`,`movges`,`movgt`,`movgts`,`movhi`,`movhis`}
    WordLists[2][3][524..536] = {`movhs`,`movhss`,`movle`,`movles`,`movlo`,`movlos`,`movls`,`movlss`,`movlt`,`movlts`,`movmi`,`movmis`,`movne`}
    WordLists[2][3][537..549] = {`movnes`,`movpl`,`movpls`,`movvc`,`movvcs`,`movvs`,`movvss`,`moval`,`movals`,`neg`,`negs`,`negcc`,`negccs`}
    WordLists[2][3][550..562] = {`negcs`,`negcss`,`negeq`,`negeqs`,`negge`,`negges`,`neggt`,`neggts`,`neghi`,`neghis`,`neghs`,`neghss`,`negle`}
    WordLists[2][3][563..575] = {`negles`,`neglo`,`neglos`,`negls`,`neglss`,`neglt`,`neglts`,`negmi`,`negmis`,`negne`,`negnes`,`negpl`,`negpls`}
    WordLists[2][3][576..588] = {`negvc`,`negvcs`,`negvs`,`negvss`,`negal`,`negals`,`orr`,`orrs`,`orrcc`,`orrccs`,`orrcs`,`orrcss`,`orreq`}
    WordLists[2][3][589..601] = {`orreqs`,`orrge`,`orrges`,`orrgt`,`orrgts`,`orrhi`,`orrhis`,`orrhs`,`orrhss`,`orrle`,`orrles`,`orrlo`,`orrlos`}
    WordLists[2][3][602..614] = {`orrls`,`orrlss`,`orrlt`,`orrlts`,`orrmi`,`orrmis`,`orrne`,`orrnes`,`orrpl`,`orrpls`,`orrvc`,`orrvcs`,`orrvs`}
    WordLists[2][3][615..628] = {`orrvss`,`orral`,`orrals`,`pop`,`push`,`rsb`,`rsbs`,`rsbcc`,`rsbccs`,`rsbcs`,`rsbcss`,`rsbeq`,`rsbeqs`,`rsbge`}
    WordLists[2][3][629..641] = {`rsbges`,`rsbgt`,`rsbgts`,`rsbhi`,`rsbhis`,`rsbhs`,`rsbhss`,`rsble`,`rsbles`,`rsblo`,`rsblos`,`rsbls`,`rsblss`}
    WordLists[2][3][642..654] = {`rsblt`,`rsblts`,`rsbmi`,`rsbmis`,`rsbne`,`rsbnes`,`rsbpl`,`rsbpls`,`rsbvc`,`rsbvcs`,`rsbvs`,`rsbvss`,`rsbal`}
    WordLists[2][3][655..671] = {`rsbals`,`ah`,`al`,`byte`,`call`,`cl`,`dword`,`eax`,`ebp`,`ebx`,`ecx`,`edi`,`edx`,`esi`,`esp`,`fcomp`,`fcompp`}
    WordLists[2][3][672..687] = {`fild`,`fist`,`fistp`,`fld`,`fldz`,`fnstsw`,`fxch`,`h4`,`idiv`,`imul`,`int3`,`jmp`,`lea`,`qword`,`ret`,`sahf`}
    WordLists[2][3][688..701] = {`seta`,`setae`,`setb`,`setbe`,`setc`,`sete`,`setg`,`setge`,`setl`,`setle`,`setnae`,`setna`,`setnb`,`setnbe`}
    WordLists[2][3][702..714] = {`setnc`,`setne`,`setng`,`setnge`,`setnl`,`setnle`,`setno`,`setnp`,`setns`,`setnz`,`seto`,`setp`,`setpe`}
    WordLists[2][3][715..731] = {`setpo`,`setz`,`shl`,`shr`,`st0`,`st1`,`st2`,`st3`,`st4`,`st5`,`st6`,`st7`,`test`,`word`,`xor`,`asr`,`bic`}
    WordLists[2][3][732..747] = {`bl`,`bx`,`eor`,`ldr`,`ldrb`,`ldreq`,`ldreqb`,`lsl`,`lsls`,`mul`,`muleq`,`mvn`,`mvnlt`,`nop`,`orr`,`orrcs`}
    WordLists[2][3][748..762] = {`ror`,`rrx`,`rsc`,`sbc`,`sbcs`,`smlal`,`smull`,`str`,`strb`,`streq`,`sub`,`subcs`,`subgt`,`subhs`,`suble`}
    WordLists[2][3][763..771] = {`sublt`,`subne`,`subs`,`svc`,`swi`,`teq`,`tst`,`umlal`,`umull`}
    WordLists[2][4][1..21] = {`fp`,`ip`,`lr`,`pc`,`r0`,`r1`,`r2`,`r3`,`r4`,`r5`,`r6`,`r7`,`r8`,`r9`,`r10`,`r11`,`sp`,`d0`,`d1`,`d2`,`d3`}
    WordLists[2][4][22..42] = {`d4`,`d5`,`d6`,`d7`,`d8`,`d9`,`d10`,`d11`,`d12`,`d13`,`d14`,`d15`,`s0`,`s1`,`s2`,`s3`,`s4`,`s5`,`s6`,`s7`,`s8`}
    WordLists[2][4][43..60] = {`s9`,`s10`,`s11`,`s12`,`s13`,`s14`,`s15`,`s16`,`s17`,`s18`,`s19`,`s20`,`s21`,`s22`,`s23`,`s24`,`s25`,`s26`}
    WordLists[2][4][61..65] = {`s27`,`s28`,`s29`,`s30`,`s31`}
    WordLists[2][5] = {}
    WordLists[2][6][1..12] = {`adr`,`APSR_nzcv`,`f32`,`f64`,`fabss`,`fabsscc`,`fabsscs`,`fabsseq`,`fabssge`,`fabssgt`,`fabsshi`,`fabsshs`}
    WordLists[2][6][13..23] = {`fabssle`,`fabsslo`,`fabssls`,`fabsslt`,`fabssmi`,`fabssne`,`fabsspl`,`fabssvc`,`fabssvs`,`fabssal`,`fadds`}
    WordLists[2][6][24..34] = {`faddscc`,`faddscs`,`faddseq`,`faddsge`,`faddsgt`,`faddshi`,`faddshs`,`faddsle`,`faddslo`,`faddsls`,`faddslt`}
    WordLists[2][6][35..45] = {`faddsmi`,`faddsne`,`faddspl`,`faddsvc`,`faddsvs`,`faddsal`,`fcmps`,`fcmpscc`,`fcmpscs`,`fcmpseq`,`fcmpsge`}
    WordLists[2][6][46..56] = {`fcmpsgt`,`fcmpshi`,`fcmpshs`,`fcmpsle`,`fcmpslo`,`fcmpsls`,`fcmpslt`,`fcmpsmi`,`fcmpsne`,`fcmpspl`,`fcmpsvc`}
    WordLists[2][6][57..67] = {`fcmpsvs`,`fcmpsal`,`fcmpzs`,`fcpys`,`fdivs`,`fdivscc`,`fdivscs`,`fdivseq`,`fdivsge`,`fdivsgt`,`fdivshi`}
    WordLists[2][6][68..78] = {`fdivshs`,`fdivsle`,`fdivslo`,`fdivsls`,`fdivslt`,`fdivsmi`,`fdivsne`,`fdivspl`,`fdivsvc`,`fdivsvs`,`fdivsal`}
    WordLists[2][6][79..89] = {`fldmfdd`,`fldmiad`,`flds`,`fmuls`,`fmulscc`,`fmulscs`,`fmulseq`,`fmulsge`,`fmulsgt`,`fmulshi`,`fmulshs`}
    WordLists[2][6][90..100] = {`fmulsle`,`fmulslo`,`fmulsls`,`fmulslt`,`fmulsmi`,`fmulsne`,`fmulspl`,`fmulsvc`,`fmulsvs`,`fmulsal`,`fnegs`}
    WordLists[2][6][101..111] = {`fnegscc`,`fnegscs`,`fnegseq`,`fnegsge`,`fnegsgt`,`fnegshi`,`fnegshs`,`fnegsle`,`fnegslo`,`fnegsls`,`fnegslt`}
    WordLists[2][6][112..122] = {`fnegsmi`,`fnegsne`,`fnegspl`,`fnegsvc`,`fnegsvs`,`fnegsal`,`fsitos`,`fsqrts`,`fsqrtscc`,`fsqrtscs`,`fsqrtseq`}
    WordLists[2][6][123..132] = {`fsqrtsge`,`fsqrtsgt`,`fsqrtshi`,`fsqrtshs`,`fsqrtsle`,`fsqrtslo`,`fsqrtsls`,`fsqrtslt`,`fsqrtsmi`,`fsqrtsne`}
    WordLists[2][6][133..143] = {`fsqrtspl`,`fsqrtsvc`,`fsqrtsvs`,`fsqrtsal`,`fsts`,`fstmfdd`,`fstmdbd`,`fsubs`,`fsubscc`,`fsubscs`,`fsubseq`}
    WordLists[2][6][144..154] = {`fsubsge`,`fsubsgt`,`fsubshi`,`fsubshs`,`fsubsle`,`fsubslo`,`fsubsls`,`fsubslt`,`fsubsmi`,`fsubsne`,`fsubspl`}
    WordLists[2][6][155..166] = {`fsubsvc`,`fsubsvs`,`fsubsal`,`ftosis`,`ftosizs`,`ftouid`,`ftouis`,`ftouizs`,`ftouizd`,`fuitos`,`fuitod`,`s32`}
    WordLists[2][6][167..176] = {`vabs`,`vabs.f64`,`vadd`,`vadd.f64`,`vcmp`,`vcmp.f64`,`vcvt`,`vcvt.f32.s32`,`vcvt.f64.s32`,`vcvt.s32.f32`}
    WordLists[2][6][177..187] = {`vcvt.s32.f64`,`vdiv`,`vdiv.f64`,`vldr`,`vldr.32`,`vldr.64`,`vldr.f32`,`vldr.f64`,`vmov`,`vmov.f64`,`vmrs`}
    WordLists[2][6][188..194] = {`vmul`,`vmul.f64`,`vneg.f64`,`vpop`,`vpush`,`vsub`,`vsub.f64`}
    WordLists[3][1][1..14] = {`cd`,`del`,`dir`,`copy`,`move`,`pause`,`echo`,`echo.`,`ton`,`off`,`exw24.exe`,`exw.exe`,`bind.ex`,`w32`}
    WordLists[3][1][15..26] = {`clear`,`icon`,`out`,`pkzipc`,`add`,`silent`,`max`,`path`,`current`,`none`,`title`,`set`}
    WordLists[4][1][1..14] = {`and`,`as`,`assert`,`break`,`catch`,`class`,`continue`,`def`,`default`,`die`,`echo`,`else`,`finally`,`for`}
    WordLists[4][1][15..27] = {`if`,`import`,`in`,`iter`,`or`,`parent`,`return`,`self`,`static`,`try`,`using`,`when`,`while`}
    WordLists[4][2..3] = {{`false`,`nil`,`true`},{`Exception`,`var`}}
    WordLists[4][4][1..13] = {`abs`,`bin`,`bytes`,`chr`,`delprop`,`file`,`getprop`,`hasprop`,`hex`,`id`,`instance_of`,`int`,`is_bool`}
    WordLists[4][4][14..22] = {`is_callable`,`is_class`,`is_dict`,`is_function`,`is_instance`,`is_int`,`is_list`,`is_number`,`is_object`}
    WordLists[4][4][23..34] = {`is_string`,`is_bytes`,`is_file`,`is_iterable`,`join`,`max`,`microtime`,`min`,`oct`,`ord`,`print`,`rand`}
    WordLists[4][4][35..45] = {`readline`,`setprop`,`sum`,`time`,`to_bool`,`to_dict`,`to_int`,`to_list`,`to_number`,`to_string`,`typeof`}
    WordLists[5][1][1..14] = {`break`,`case`,`catch`,`class`,`continue`,`default`,`delete`,`do`,`else`,`enum`,`for`,`goto`,`if`,`new`}
    WordLists[5][1][15..26] = {`private`,`protected`,`public`,`return`,`static_cast`,`std`,`switch`,`this`,`throw`,`try`,`using`,`virtual`}
    WordLists[5][1][27] = `while`
    WordLists[5][2][1..8] = {`TRUE`,`true`,`FALSE`,`false`,`BS_DEFPUSHBUTTON`,`CBS_DROPDOWNLIST`,`CBN_SELCHANGE`,`CB_GETCURSEL`}
    WordLists[5][2][9..13] = {`CB_GETLBTEXT`,`CB_INSERTSTRING`,`CB_SETCURSEL`,`COINIT_APARTMENTTHREADED`,`CONNECT_REDIRECT`}
    WordLists[5][2][14..19] = {`CLIP_DEFAULT_PRECIS`,`CLEARTYPE_QUALITY`,`COINIT_MULTITHREADED`,`CP_ACP`,`CREATE_ALWAYS`,`CREATE_SUSPENDED`}
    WordLists[5][2][20..25] = {`CS_HREDRAW`,`CS_VREDRAW`,`CSIDL_SYSTEM`,`CSIDL_PERSONAL`,`CSIDL_PROFILE`,`CSIDL_PROGRAM_FILES`}
    WordLists[5][2][26..30] = {`CSIDL_PROGRAM_FILESX86`,`CSIDL_WINDOWS`,`CW_USEDEFAULT`,`DEFAULT_CHARSET`,`DEFAULT_PITCH`}
    WordLists[5][2][31..36] = {`DLL_PROCESS_ATTACH`,`DLL_PROCESS_DETACH`,`DTR_CONTROL_DISABLE`,`EM_REPLACESEL`,`EM_SETSEL`,`EOF`}
    WordLists[5][2][37..40] = {`ERROR_ACCESS_DENIED`,`ERROR_ALREADY_ASSIGNED`,`ERROR_ALREADY_EXISTS`,`ERROR_ARENA_TRASHED`}
    WordLists[5][2][41..44] = {`ERROR_DIR_NOT_EMPTY`,`ERROR_FILE_NOT_FOUND`,`ERROR_FILE_SYSTEM_ERROR`,`ERROR_INVALID_ADDRESS`}
    WordLists[5][2][45..48] = {`ERROR_INVALID_NAME`,`ERROR_NOACCESS`,`ERROR_NO_MORE_ITEMS`,`ERROR_NO_MORE_SEARCH_HANDLES`}
    WordLists[5][2][49..53] = {`ERROR_PATH_NOT_FOUND`,`ERROR_SHARING_VIOLATION`,`ERROR_SUCCESS`,`ES_AUTOHSCROLL`,`ES_AUTOVSCROLL`}
    WordLists[5][2][54..60] = {`ES_MULTILINE`,`ES_READONLY`,`faArchive`,`faDirectory`,`faReadOnly`,`FILE_ACTION_ADDED`,`FILE_ACTION_MODIFIED`}
    WordLists[5][2][61..64] = {`FILE_ACTION_REMOVED`,`FILE_ACTION_RENAMED_NEW_NAME`,`FILE_ACTION_RENAMED_OLD_NAME`,`FILE_ATTRIBUTE_ARCHIVE`}
    WordLists[5][2][65..68] = {`FILE_ATTRIBUTE_DIRECTORY`,`FILE_ATTRIBUTE_HIDDEN`,`FILE_ATTRIBUTE_NORMAL`,`FILE_ATTRIBUTE_READONLY`}
    WordLists[5][2][69..72] = {`FILE_ATTRIBUTE_SYSTEM`,`FILE_FLAG_BACKUP_SEMANTICS`,`FILE_LIST_DIRECTORY`,`FILE_NOTIFY_CHANGE_ATTRIBUTES`}
    WordLists[5][2][73..75] = {`FILE_NOTIFY_CHANGE_CREATION`,`FILE_NOTIFY_CHANGE_DIR_NAME`,`FILE_NOTIFY_CHANGE_FILE_NAME`}
    WordLists[5][2][76..78] = {`FILE_NOTIFY_CHANGE_LAST_ACCESS`,`FILE_NOTIFY_CHANGE_LAST_WRITE`,`FILE_NOTIFY_CHANGE_SECURITY`}
    WordLists[5][2][79..83] = {`FILE_NOTIFY_CHANGE_SIZE`,`FILE_SHARE_DELETE`,`FILE_SHARE_READ`,`FILE_SHARE_WRITE`,`FindExInfoStandard`}
    WordLists[5][2][84..86] = {`FindExSearchLimitToDevices`,`FindExSearchLimitToDirectories`,`FindExSearchNameMatch`}
    WordLists[5][2][87..91] = {`FORMAT_MESSAGE_ALLOCATE_BUFFER`,`FORMAT_MESSAGE_FROM_SYSTEM`,`FW_BOLD`,`FW_NORMAL`,`FW_REGULAR`}
    WordLists[5][2][92..98] = {`GENERIC_READ`,`GENERIC_WRITE`,`GPTR`,`HCBT_ACTIVATE`,`HKEY_CURRENT_USER`,`HKEY_LOCAL_MACHINE`,`ICON_BIG`}
    WordLists[5][2][99..106] = {`ICON_SMALL`,`IDABORT`,`IDCANCEL`,`IDC_ARROW`,`IDC_CMDBAR`,`IDC_COMPORT`,`IDIGNORE`,`IDLE_PRIORITY_CLASS`}
    WordLists[5][2][107..115] = {`IDM_ABOUT`,`IDM_EXIT`,`IDNO`,`IDOK`,`IDRETRY`,`ID_RCVTEXT`,`ID_SENDBTN`,`ILD_TRANSPARENT`,`INFINITE`}
    WordLists[5][2][116..122] = {`INVALID_HANDLE_VALUE`,`KEY_ALL_ACCESS`,`KEY_READ`,`KEY_WRITE`,`LANG_NEUTRAL`,`LMEM_MOVEABLE`,`LMEM_ZEROINIT`}
    WordLists[5][2][123..129] = {`LPSTR_TEXTCALLBACK`,`LVCF_FMT`,`LVCF_SUBITEM`,`LVCF_TEXT`,`LVCF_WIDTH`,`LVCFMT_LEFT`,`LVCFMT_RIGHT`}
    WordLists[5][2][130..136] = {`LVIF_IMAGE`,`LVIF_PARAM`,`LVIF_STATE`,`LVIF_TEXT`,`LVIS_SELECTED`,`LVN_BEGINLABELEDIT`,`LVN_COLUMNCLICK`}
    WordLists[5][2][137..141] = {`LVN_ENDLABELEDIT`,`LVN_GETDISPINFO`,`LVN_HOTTRACK`,`LVN_ITEMACTIVATE`,`LVN_ITEMCHANGED`}
    WordLists[5][2][142..146] = {`LVS_EX_FULLROWSELECT`,`LVS_EX_ONECLICKACTIVATE`,`MAX_ALLOC_SIZE`,`MAX_PATH`,`MB_ABORTRETRYIGNORE`}
    WordLists[5][2][147..153] = {`MB_ICONERROR`,`MB_ICONINFORMATION`,`MB_ICONWARNING`,`MB_OK`,`MB_OKCANCEL`,`MB_SETFOREGROUND`,`MB_SYSTEMMODAL`}
    WordLists[5][2][154..162] = {`MB_TOPMOST`,`MB_YESNO`,`mrOk`,`MS_CTS_ON`,`NOERROR`,`NOPARITY`,`NO_ERROR`,`NO_WIN32_LEAN_AND_MEAN`,`NULL`}
    WordLists[5][2][163..170] = {`ONESTOPBIT`,`OPAQUE`,`OPEN_EXISTING`,`OUT_DEFAULT_PRECIS`,`O_APPEND`,`O_BINARY`,`O_CREAT`,`O_RDWR`}
    WordLists[5][2][171..176] = {`PROCESS_QUERY_INFORMATION`,`PURGE_RXCLEAR`,`PURGE_TXCLEAR`,`REG_DWORD`,`REG_OPTION_NON_VOLATILE`,`REG_SZ`}
    WordLists[5][2][177..181] = {`RESOURCE_CONNECTED`,`RESOURCE_CONTEXT`,`RESOURCE_GLOBALNET`,`RESOURCE_REMEMBERED`,`RESOURCETYPE_ANY`}
    WordLists[5][2][182..185] = {`RESOURCETYPE_DISK`,`RESOURCEUSAGE_ALL`,`RESOURCEUSAGE_ATTACHED`,`RESOURCEUSAGE_CONTAINER`}
    WordLists[5][2][186..190] = {`RTS_CONTROL_DISABLE`,`RTS_CONTROL_HANDSHAKE`,`SETDTR`,`SHCOLSTATE_ONBYDEFAULT`,`SHCOLSTATE_SLOW`}
    WordLists[5][2][191..196] = {`SHCOLSTATE_TYPE_INT`,`SHGFI_ICON`,`SHGFI_LARGEICON`,`SHGFI_OPENICON`,`SHGFI_SMALLICON`,`SHGFI_SYSICONINDEX`}
    WordLists[5][2][197..202] = {`SHGFI_TYPENAME`,`SHGFI_USEFILEATTRIBUTES`,`SHIL_EXTRALARGE`,`SM_CXICON`,`SM_CYICON`,`STD_OUTPUT_HANDLE`}
    WordLists[5][2][203..209] = {`STILL_ACTIVE`,`SUBLANG_DEFAULT`,`SWP_NOACTIVATE`,`SWP_NOMOVE`,`SWP_NOSIZE`,`SWP_NOZORDER`,`SW_HIDE`}
    WordLists[5][2][210..217] = {`SW_SHOWDEFAULT`,`SW_SHOWMINNOACTIVE`,`SYNCHRONIZE`,`S_FALSE`,`S_IREAD`,`S_IWRITE`,`S_OK`,`TBSTYLE_FLAT`}
    WordLists[5][2][218..224] = {`THREAD_PRIORITY_BELOW_NORMAL`,`TVM_EDITLABEL`,`UNLEN`,`VK_DELETE`,`VK_F2`,`VT_BSTR`,`WAIT_OBJECT_0`}
    WordLists[5][2][225..232] = {`WAIT_TIMEOUT`,`WH_CBT`,`WM_ACTIVATE`,`WM_CLOSE`,`WM_COMMAND`,`WM_CREATE`,`WM_DESTROY`,`WM_ICONERASEBKGND`}
    WordLists[5][2][233..240] = {`WM_NCCREATE`,`WM_NOTIFY`,`WM_SETFOCUS`,`WM_SETFONT`,`WM_SETICON`,`WM_SETTEXT`,`WM_SETTINGCHANGE`,`WM_SIZE`}
    WordLists[5][2][241..247] = {`WM_USER`,`WS_CHILD`,`WS_CLIPSIBLINGS`,`WS_EX_CLIENTEDGE`,`WS_OVERLAPPEDWINDOW`,`WS_VISIBLE`,`WS_VSCROLL`}
    WordLists[5][3][1..12] = {`#define`,`#if`,`#ifdef`,`#ifndef`,`#import`,`#include`,`#else`,`#endif`,`#pragma`,`#undef`,`defined`,`error`}
    WordLists[5][3][13..23] = {`hdrstop`,`link`,`package`,`resource`,`smart_init`,`HIWORD`,`LOWORD`,`MAKELANGID`,`STRICT`,`TEXT`,`USEFORM`}
    WordLists[5][3][24..26] = {`__fastcall`,`__stdcall`,`__classid`}
    WordLists[5][4][1..13] = {`typedef`,`struct`,`const`,`static`,`union`,`unsigned`,`extern`,`bool`,`BOOL`,`char`,`uchar`,`CHAR`,`UCHAR`}
    WordLists[5][4][14..27] = {`double`,`float`,`int`,`uint`,`INT`,`UINT`,`int64_t`,`long`,`ulong`,`LONG`,`ULONG`,`short`,`ushort`,`string`}
    WordLists[5][4][28..37] = {`time_t`,`uint64_t`,`volatile`,`wchar_t`,`APIENTRY`,`BSTR`,`BYTE`,`CALLBACK`,`COLORREF`,`CRITICAL_SECTION`}
    WordLists[5][4][38..46] = {`DLGPROC`,`DWORD`,`DWORD64`,`EXTINFO`,`FARPROC`,`FILE_NOTIFY_INFORMATION`,`HANDLE`,`HBITMAP`,`HBRUSH`}
    WordLists[5][4][47..58] = {`HCURSOR`,`HDC`,`HFONT`,`HGLOBAL`,`HHOOK`,`HICON`,`HINSTANCE`,`HIWORD`,`HKEY`,`HMODULE`,`HRESULT`,`HWND`}
    WordLists[5][4][59..66] = {`LARGE_INTEGER`,`LOGFONT`,`LOWORD`,`LPARAM`,`LPBYTE`,`LPCSHCOLUMNDATA`,`LPCSHCOLUMNID`,`LPCSHCOLUMNINIT`}
    WordLists[5][4][67..77] = {`LPCTSTR`,`LPDWORD`,`LPOVERLAPPED`,`LPSTR`,`LPTSTR`,`LPVOID`,`LPWSTR`,`LRESULT`,`LVITEM`,`LV_ITEM`,`MSG`}
    WordLists[5][4][78..85] = {`NETRESOURCE`,`PROCESS_INFORMATION`,`RECT`,`REFCLSID`,`REFIID`,`SHCOLUMNINFO`,`SHFILEINFO`,`STARTUPINFO`}
    WordLists[5][4][86..94] = {`STDAPI`,`STDMETHODIMP`,`TCHAR`,`VARIANT`,`VOID`,`void`,`WIN32_FIND_DATA`,`WINAPI`,`WINDOWPLACEMENT`}
    WordLists[5][4][95..103] = {`WNDCLASSW`,`WNDPROC`,`WORD`,`WPARAM`,`__int64`,`dwFileAttributes`,`cFileName`,`cAlternateFileName`,`hIcon`}
    WordLists[5][4][104..111] = {`HighPart`,`iIcon`,`LowPart`,`nFileSizeLow`,`nFileSizeHigh`,`QuadPart`,`CImageList`,`CListCtrl`}
    WordLists[5][4][112..115] = {`CCriticalSection`,`CFormView`,`CTime`,`CString`}
    WordLists[5][5][1..14] = {`abort`,`abs`,`acos`,`asctime`,`asin`,`assert`,`atan`,`atan2`,`atexit`,`atof`,`atoi`,`atol`,`atoll`,`bsearch`}
    WordLists[5][5][15..28] = {`bottom`,`calloc`,`ceil`,`clearerr`,`clock`,`close`,`cos`,`cosh`,`ctime`,`difftime`,`div`,`exit`,`exp`,`fabs`}
    WordLists[5][5][29..40] = {`fclose`,`fdopen`,`feof`,`ferror`,`fflush`,`fgetc`,`fgetpos`,`fgets`,`filelength`,`fileno`,`floor`,`fmod`}
    WordLists[5][5][41..53] = {`fopen`,`fprintf`,`fputc`,`fputs`,`fread`,`free`,`freopen`,`frexp`,`fscanf`,`fseek`,`ftell`,`fwrite`,`getc`}
    WordLists[5][5][54..65] = {`getenv`,`gets`,`gmtime`,`hypot`,`instr`,`isalnum`,`isalpha`,`isascii`,`isblank`,`isdigit`,`isxdigit`,`itoa`}
    WordLists[5][5][66..77] = {`labs`,`ldexp`,`ldiv`,`left`,`llabs`,`lldiv`,`localeconv`,`localtime`,`log`,`log10`,`longjmp`,`lstrcat`}
    WordLists[5][5][78..88] = {`lstrcmp`,`lstrcpy`,`lstrlen`,`malloc`,`mblen`,`mbstowcs`,`mbtowc`,`memchr`,`memcmp`,`memcpy`,`memicmp`}
    WordLists[5][5][89..101] = {`memmove`,`memset`,`mid`,`mktime`,`modf`,`perror`,`pow`,`printf`,`putc`,`putchar`,`putenv`,`puts`,`qsort`}
    WordLists[5][5][102..113] = {`raise`,`rand`,`read`,`realloc`,`regcomp`,`regerror`,`regexec`,`regfree`,`remove`,`rename`,`rewind`,`right`}
    WordLists[5][5][114..125] = {`scanf`,`setbuf`,`setjmp`,`setlocale`,`setvbuf`,`signal`,`sin`,`sinh`,`sizeof`,`snprintf`,`sprintf`,`sqrt`}
    WordLists[5][5][126..136] = {`srand`,`sscanf`,`strcasecmp`,`strcat`,`strchr`,`strcmp`,`strcoll`,`strcpy`,`strcspn`,`strdup`,`strerror`}
    WordLists[5][5][137..146] = {`strftime`,`stricmp`,`strlen`,`strncasecmp`,`strncat`,`strncmp`,`strncmpi`,`strncpy`,`strnicmp`,`strpbrk`}
    WordLists[5][5][147..157] = {`strrchr`,`strrev`,`strspn`,`strstr`,`strtod`,`strtok`,`strtol`,`strtoll`,`strtoul`,`strtoull`,`strupr`}
    WordLists[5][5][158..169] = {`strxfrm`,`system`,`tan`,`tanh`,`time`,`tmpfile`,`tmpnam`,`toascii`,`tolower`,`top`,`toupper`,`ungetc`}
    WordLists[5][5][170..179] = {`vfprintf`,`vfscanf`,`vprintf`,`vscanf`,`vsnprintf`,`vsprintf`,`vsscanf`,`wctomb`,`write`,`wsprintf`}
    WordLists[5][5][180..187] = {`wsprintfA`,`wvsprintf`,`_i64toa`,`_tsplitpath_s`,`_tprintf`,`_wopen`,`CallNextHookEx`,`ClientToScreen`}
    WordLists[5][5][188..194] = {`CloseHandle`,`CoInitialize`,`CoInitializeEx`,`CreateDialog`,`CreateDirectory`,`CreateEvent`,`CreateFile`}
    WordLists[5][5][195..200] = {`CreateFileA`,`CreateFontA`,`CreateFontIndirect`,`CreateProcess`,`CreateThread`,`CreateWindow`}
    WordLists[5][5][201..206] = {`CreateWindowEx`,`CreateWindowW`,`CreateWindowExW`,`DefWindowProc`,`DeleteCriticalSection`,`DeleteFile`}
    WordLists[5][5][207..212] = {`DeleteObject`,`DestroyIcon`,`DestroyWindow`,`DispatchMessage`,`EnableWindow`,`EnterCriticalSection`}
    WordLists[5][5][213..218] = {`EscapeCommFunction`,`ExitProcess`,`FileExists`,`FileGetAttr`,`FindClose`,`FindCloseChangeNotification`}
    WordLists[5][5][219..223] = {`FindFirstChangeNotification`,`FindFirstFile`,`FindFirstFileEx`,`FindNextChangeNotification`,`FindNextFile`}
    WordLists[5][5][224..229] = {`FormatMessage`,`FreeLibrary`,`GetClientRect`,`GetCommandLine`,`GetCommModemStatus`,`GetCommState`}
    WordLists[5][5][230..235] = {`GetCurrentThreadId`,`GetCurrentTime`,`GetDC`,`GetDiskFreeSpace`,`GetDlgItem`,`GetEnvironmentVariable`}
    WordLists[5][5][236..241] = {`GetExitCodeProcess`,`GetExitCodeThread`,`GetFileSize`,`GetLastError`,`GetMessage`,`GetModuleFileName`}
    WordLists[5][5][242..247] = {`GetModuleHandle`,`GetObject`,`GetProcAddress`,`GetProcessHeap`,`GetStartupInfo`,`GetStdHandle`}
    WordLists[5][5][248..253] = {`GetStockObject`,`GetUserName`,`GetTickCount`,`GetWindowFont`,`GetWindowPlacement`,`GetWindowRect`}
    WordLists[5][5][254..259] = {`GetWindowText`,`GetWindowTextLengthW`,`GlobalAlloc`,`GlobalFree`,`HeapAlloc`,`InitCommonControlsEx`}
    WordLists[5][5][260..265] = {`InitializeCriticalSection`,`KillTimer`,`LeaveCriticalSection`,`LoadCursorW`,`LoadIcon`,`LoadLibrary`}
    WordLists[5][5][266..272] = {`LocalAlloc`,`LocalFree`,`LocalLock`,`MessageBox`,`MoveWindow`,`MultiByteToWideChar`,`OleInitialize`}
    WordLists[5][5][273..278] = {`OleUninitialize`,`OpenProcess`,`PeekNamedPipe`,`PostMessage`,`PostQuitMessage`,`PurgeComm`}
    WordLists[5][5][279..283] = {`QueryPerformanceCounter`,`QueryPerformanceFrequency`,`ReadDirectoryChangesW`,`ReadFile`,`RegCloseKey`}
    WordLists[5][5][284..290] = {`RegCreateKeyEx`,`RegEnumValue`,`RegisterClassW`,`RegOpenKeyEx`,`RegQueryValueEx`,`RegSetValueEx`,`RemoveDir`}
    WordLists[5][5][291..297] = {`ResetEvent`,`ResumeThread`,`select`,`SelectObject`,`SendDlgItemMessage`,`SendMessage`,`SendMessageA`}
    WordLists[5][5][298..304] = {`SendNotifyMessage`,`SetBkMode`,`SetCommState`,`SetCommTimeouts`,`SetEvent`,`SetFocus`,`SetIcon`}
    WordLists[5][5][305..311] = {`SetPriorityClass`,`SetTextColor`,`SetThreadPriority`,`SetTimer`,`SetupComm`,`SetWindowPos`,`SetWindowsHookEx`}
    WordLists[5][5][312..318] = {`SetWindowText`,`ScreenToClient`,`ShellExecute`,`SHGetFileInfo`,`SHGetImageList`,`ShowWindow`,`Sleep`}
    WordLists[5][5][319..324] = {`TranslateMessage`,`UnhookWindowsHookEx`,`UpdateWindow`,`VirtualAlloc`,`VirtualFree`,`WaitForMultipleObjects`}
    WordLists[5][5][325..330] = {`WaitForSingleObject`,`WaitForSingleObjectEx`,`WinMain`,`WriteConsoleOut`,`WriteFile`,`WNetCloseEnum`}
    WordLists[5][5][331..334] = {`WNetEnumResource`,`WNetOpenEnum`,`WNetUseConnection`,`ZeroMemory`}
    WordLists[5][6][1..9] = {`__classid`,`__fastcall`,`__property`,`__published`,`Active`,`ActiveMDIChild`,`Add`,`AddIcon`,`AddNode`}
    WordLists[5][6][10..17] = {`AddObject`,`Alignment`,`AlphaSort`,`AnsiCompareIC`,`AnsiLastChar`,`AnsiPos`,`AnsiReplaceText`,`AnsiString`}
    WordLists[5][6][18..27] = {`Application`,`AutoCheck`,`BeginDrag`,`Bottom`,`c_str`,`caFree`,`Caption`,`Cascade`,`CaseSensitive`,`cd32Bit`}
    WordLists[5][6][28..36] = {`Checked`,`Clear`,`Close`,`ColorDepth`,`Columns`,`CommandText`,`CommandType`,`Connected`,`Connection`}
    WordLists[5][6][37..45] = {`ConnectionString`,`Contains`,`Count`,`CreateForm`,`Cut`,`Data`,`DateTimeToStr`,`Delete`,`DeleteChildren`}
    WordLists[5][6][46..53] = {`Deleting`,`DoubleBuffered`,`DrawingStyle`,`dsDragLeave`,`dsTransparent`,`Duplicates`,`EditCaption`,`Enabled`}
    WordLists[5][6][54..62] = {`EndDrag`,`Exception`,`Execute`,`First`,`GetHitTestInfoAt`,`GetIcon`,`GetItemAt`,`GetNextItem`,`GetNodeAt`}
    WordLists[5][6][63..71] = {`Handle`,`Height`,`Icon`,`ifExactString`,`ifPartialString`,`ImageIndex`,`Images`,`Index`,`IndexOf`}
    WordLists[5][6][72..81] = {`Initialize`,`Insert`,`isSelected`,`ItemIndex`,`Item`,`ItemId`,`Items`,`ItemsEx`,`LargeImages`,`LastChar`}
    WordLists[5][6][82..90] = {`LastDelimiter`,`Left`,`Length`,`MakeVisible`,`MDIChildCount`,`MDIChildren`,`MenuIndex`,`mbLeft`,`mbRight`}
    WordLists[5][6][91..101] = {`naAddChild`,`Now`,`Objects`,`OnClick`,`Open`,`OwnerData`,`ParamCheck`,`Parent`,`Pointer`,`Popup`,`Pos`}
    WordLists[5][6][102..111] = {`Previous`,`Provider`,`Recordset`,`Remove`,`Restore`,`Run`,`sdAll`,`SelCount`,`Selected`,`SelectedIndex`}
    WordLists[5][6][112..121] = {`Selection`,`SetLength`,`ShowException`,`ShowModal`,`Sleep`,`SmallImages`,`Sorted`,`ssCtrl`,`ssLeft`,`Strings`}
    WordLists[5][6][122..129] = {`SubItems`,`SubString`,`tbHorizontal`,`tbVertical`,`TADOCommand`,`TADOConnection`,`TADOQuery`,`TADOTable`}
    WordLists[5][6][130..137] = {`Tag`,`taRightJustify`,`TButton`,`TCheckBox`,`TCloseAction`,`TComboBox`,`TComponent`,`TDateTime`}
    WordLists[5][6][138..146] = {`TDateTimePicker`,`TDragObject`,`TDragState`,`TEdit`,`Text`,`TForm`,`TFrame`,`TGroupBox`,`THeaderControl`}
    WordLists[5][6][147..155] = {`THeaderSection`,`THitTests`,`TIcon`,`Tile`,`TileMode`,`TImageList`,`TimeToStr`,`TItemFind`,`TItemStates`}
    WordLists[5][6][156..165] = {`Title`,`TLabel`,`TList`,`TListBox`,`TListColumn`,`TListItem`,`TListView`,`TMainMenu`,`TMemo`,`TMenuItem`}
    WordLists[5][6][166..173] = {`TMessage`,`TMouseButton`,`TPageControl`,`TPanel`,`TProgressBar`,`TRadioButton`,`TRadioGroup`,`TSplitter`}
    WordLists[5][6][174..183] = {`TStringGrid`,`TStringList`,`TTabSheet`,`TTimer`,`TToolBar`,`TToolButton`,`TTreeView`,`TObject`,`Top`,`TPoint`}
    WordLists[5][6][184..191] = {`TPopupMenu`,`TRect`,`TSearchDirection`,`TShiftState`,`TStringList`,`TTreeNode`,`TTreeView`,`TViewStyle`}
    WordLists[5][6][192..200] = {`TWinControl`,`TWindowState`,`UnicodeString`,`UpdateItems`,`UpperCase`,`Visible`,`vsIcon`,`vsList`,`vsReport`}
    WordLists[5][6][201..206] = {`vsSmallIcon`,`w_str`,`WideString`,`Width`,`WindowState`,`wsNormal`}
    WordLists[6][1][1..13] = {`a`,`active`,`after`,`and`,`before`,`blockquote`,`body`,`calc`,`caption`,`checked`,`code`,`dd`,`div`}
    WordLists[6][1][14..27] = {`first-child`,`focus`,`format`,`h1`,`h2`,`h3`,`h4`,`h5`,`h6`,`hover`,`html`,`img`,`input`,`invert`}
    WordLists[6][1][28..40] = {`last-child`,`li`,`link`,`local`,`media`,`not`,`nth-child`,`ol`,`p`,`path`,`pre`,`radial-gradient`,`rgb`}
    WordLists[6][1][41..54] = {`rgba`,`rotate`,`rotateX`,`rotateY`,`rotateZ`,`screen`,`small`,`span`,`sub`,`sup`,`svg`,`table`,`tbody`,`td`}
    WordLists[6][1][55..66] = {`tfoot`,`th`,`thead`,`tr`,`translateX`,`translateY`,`translateZ`,`tt`,`type`,`ul`,`url`,`visited`}
    WordLists[6][2][1..7] = {`absolute`,`align`,`align-items`,`all`,`animation-duration`,`animation-iteration-count`,`animation-name`}
    WordLists[6][2][8..13] = {`animation-timing-function`,`auto`,`avg`,`background`,`background-color`,`background-image`}
    WordLists[6][2][14..21] = {`background-position`,`background-repeat`,`background-size`,`baseline`,`block`,`bold`,`border`,`border-box`}
    WordLists[6][2][22..26] = {`border-width`,`border-collapse`,`border-color`,`border-bottom`,`border-bottom-color`}
    WordLists[6][2][27..30] = {`border-bottom-left-radius`,`border-bottom-right-radius`,`border-bottom-width`,`border-left`}
    WordLists[6][2][31..36] = {`border-left-width`,`border-radius`,`border-right`,`border-right-width`,`border-spacing`,`border-style`}
    WordLists[6][2][37..42] = {`border-top`,`border-top-left-radius`,`border-top-right-radius`,`border-top-style`,`border-top-width`,`both`}
    WordLists[6][2][43..52] = {`bottom`,`box-shadow`,`box-sizing`,`break-word`,`center`,`clear`,`col-resize`,`collapse`,`color`,`column`}
    WordLists[6][2][53..62] = {`column-fill`,`content`,`content-box`,`contents`,`cursive`,`cursor`,`d`,`daily-samples`,`dashed`,`default`}
    WordLists[6][2][63..73] = {`display`,`dotted`,`ds-header`,`ease`,`ease-in`,`ellipsis`,`fill`,`filter`,`fit-content`,`fixed`,`flex`}
    WordLists[6][2][74..82] = {`flex-direction`,`flex-grow`,`flex-shrink`,`flex-start`,`flex-wrap`,`float`,`font`,`font-family`,`font-size`}
    WordLists[6][2][83..91] = {`font-style`,`font-weight`,`gap`,`grab`,`grabbing`,`grid`,`grid-area`,`grid-gap`,`grid-template-areas`}
    WordLists[6][2][92..99] = {`grid-template-columns`,`grid-template-rows`,`header`,`height`,`hidden`,`important`,`inherit`,`initial`}
    WordLists[6][2][100..108] = {`inline`,`inline-block`,`inline-flex`,`inline-grid`,`inline-table`,`inset`,`italic`,`justify-content`,`left`}
    WordLists[6][2][109..115] = {`line-height`,`list-item`,`list-style`,`list-style-image`,`list-style-type`,`lower-alpha`,`lower-roman`}
    WordLists[6][2][116..122] = {`margin`,`margin-block-end`,`margin-block-start`,`margin-bottom`,`margin-left`,`margin-right`,`margin-top`}
    WordLists[6][2][123..132] = {`max`,`max-height`,`max-width`,`med`,`middle`,`min`,`min-content`,`min-height`,`min-width`,`monospace`}
    WordLists[6][2][133..142] = {`moz-transition`,`no-repeat`,`none`,`normal`,`nowrap`,`o-transition`,`oblique`,`only`,`opacity`,`outline`}
    WordLists[6][2][143..149] = {`outline-offset`,`outline-width`,`overflow`,`overflow-x`,`overflow-y`,`padding`,`padding-bottom`}
    WordLists[6][2][150..156] = {`padding-inline-start`,`padding-left`,`padding-right`,`padding-top`,`perspective`,`place-items`,`pointer`}
    WordLists[6][2][157..166] = {`pointer-events`,`position`,`preserve-3d`,`relative`,`repeat`,`repeat-x`,`repeat-y`,`right`,`round`,`row`}
    WordLists[6][2][167..175] = {`row-resize`,`rt-header`,`running-trend`,`sans-serif`,`scroll`,`smaller`,`solid`,`space-around`,`space-evenly`}
    WordLists[6][2][176..184] = {`src`,`sticky`,`stretch`,`stroke`,`stroke-linecap`,`stroke-linejoin`,`stroke-width`,`super`,`table-cell`}
    WordLists[6][2][185..191] = {`table-column`,`table-layout`,`table-row`,`text-align`,`text-bottom`,`text-decoration`,`text-indent`}
    WordLists[6][2][192..199] = {`text-overflow`,`text-shadow`,`text-transform`,`thin`,`top`,`transform`,`transform-origin`,`transform-style`}
    WordLists[6][2][200..208] = {`transition`,`transparent`,`underline`,`uppercase`,`user-select`,`vertical-align`,`vertical-lr`,`vmax`,`vmin`}
    WordLists[6][2][209..217] = {`visibility`,`visible`,`webkit-transition`,`white-space`,`width`,`word-wrap`,`wrap`,`writing-mode`,`z-index`}
    WordLists[6][3][1..12] = {`Arial`,`black`,`blue`,`BlueViolet`,`CadetBlue`,`ch`,`cm`,`Consolas`,`Courier`,`Crimson`,`DarkGrey`,`darkgray`}
    WordLists[6][3][13..24] = {`darkorange`,`deg`,`DimGrey`,`DodgerBlue`,`em`,`ex`,`fr`,`Gainsboro`,`georgia`,`GhostWhite`,`green`,`hd`}
    WordLists[6][3][25..35] = {`Helvetica`,`hi`,`in`,`int`,`LightCoral`,`lightgrey`,`lightGray`,`LightSlateGray`,`LimeGreen`,`Menlo`,`mm`}
    WordLists[6][3][36..49] = {`orange`,`Orange`,`PaleVioletRed`,`pink`,`pt`,`purple`,`px`,`red`,`rem`,`silver`,`teal`,`ud`,`Verdana`,`vh`}
    WordLists[6][3][50..53] = {`vw`,`white`,`whitesmoke`,`yellow`}
    WordLists[7][1][1..13] = {`abstract`,`alias`,`auto`,`break`,`case`,`cast`,`catch`,`class`,`const`,`continue`,`default`,`else`,`enum`}
    WordLists[7][1][14..26] = {`extern`,`final`,`for`,`foreach`,`foreach_reverse`,`goto`,`if`,`immutable`,`import`,`in`,`inout`,`is`,`module`}
    WordLists[7][1][27..39] = {`new`,`nothrow`,`out`,`pragma`,`private`,`ref`,`return`,`safe`,`scope`,`static`,`struct`,`switch`,`throw`}
    WordLists[7][1][40..44] = {`trusted`,`try`,`version`,`void`,`while`}
    WordLists[7][2][1..13] = {`fallthrough`,`func`,`package`,`type`,`var`,`byte`,`float64`,`Int`,`interface`,`struct`,`uint`,`uint64`,`Add`}
    WordLists[7][2][14..25] = {`append`,`Args`,`Atoi`,`BitLen`,`Cmp`,`CombinedOutput`,`Command`,`copy`,`Errorf`,`Exit`,`Fatal`,`Fatalf`}
    WordLists[7][2][26..39] = {`Float64`,`Hostname`,`Inf`,`IsInf`,`len`,`make`,`map`,`MaxUint64`,`Mul`,`NArg`,`new`,`NewInt`,`Now`,`Parse`}
    WordLists[7][2][40..51] = {`Printf`,`Println`,`QuoRem`,`Run`,`Seed`,`Set`,`SetString`,`SetUint64`,`Sqrt`,`String`,`ToLower`,`Uint`}
    WordLists[7][2][52..65] = {`Uint64`,`Unix`,`Usage`,`big`,`exec`,`flag`,`fmt`,`log`,`math`,`os`,`rand`,`strconv`,`strings`,`time`}
    WordLists[7][3] = {`BigInt`,`bool`,`char`,`dchar`,`DWORD`,`File`,`HANDLE`,`int`,`long`,`LPWSTR`,`size_t`,`string`,`ubyte`,`WCHAR`}
    WordLists[7][4] = {`false`,`true`,`null`,`NO_ERROR`,`ERROR_ENVVAR_NOT_FOUND`,`SW_SHOWNORMAL`}
    WordLists[7][5][1..7] = {`assert`,`close`,`createEnv`,`empty`,`free`,`FreeEnvironmentStringsW`,`GetEnvironmentStringsW`}
    WordLists[7][5][8..15] = {`GetEnvironmentVariableW`,`GetLastError`,`join`,`length`,`malloc`,`retro`,`rev`,`SetEnvironmentVariableW`}
    WordLists[7][5][16..24] = {`SetLastError`,`ShellExecuteW`,`sizeof`,`start`,`strdup`,`tempCString`,`tempCStringW`,`Thread`,`write`}
    WordLists[7][5][25] = `writeln`
    WordLists[7][6][1..11] = {`algorithm`,`array`,`ascii`,`bigint`,`bigInt`,`comparison`,`conv`,`core`,`cstring`,`errno`,`exception`}
    WordLists[7][6][12..23] = {`execvp`,`fcntl`,`file`,`fork`,`format`,`functional`,`getenv`,`iteration`,`internal`,`path`,`poll`,`posix`}
    WordLists[7][6][24..34] = {`primitives`,`pthread`,`random`,`range`,`resource`,`searching`,`shellapi`,`signal`,`stat`,`std`,`stdc`}
    WordLists[7][6][35..47] = {`stddef`,`stdint`,`stdio`,`stdlib`,`sys`,`syserror`,`thread`,`typecons`,`uni`,`unistd`,`utf`,`uuid`,`wait`}
    WordLists[7][6][48..49] = {`wchar_`,`windows`}
    WordLists[8][1][1..13] = {`and`,`AND`,`begin`,`Begin`,`BEGIN`,`Break`,`BREAK`,`case`,`cdecl`,`class`,`const`,`constructor`,`Continue`}
    WordLists[8][1][14..27] = {`destructor`,`div`,`DIV`,`do`,`downto`,`else`,`end`,`END`,`endFor`,`endfor`,`endIf`,`endif`,`Exit`,`EXIT`}
    WordLists[8][1][28..39] = {`fillchar`,`finalization`,`for`,`For`,`forward`,`function`,`if`,`If`,`IF`,`implementation`,`in`,`inherited`}
    WordLists[8][1][40..52] = {`initialization`,`inline`,`interface`,`mod`,`MOD`,`not`,`of`,`or`,`OR`,`overload`,`override`,`packed`,`pascal`}
    WordLists[8][1][53..63] = {`private`,`procedure`,`program`,`property`,`public`,`read`,`record`,`repeat`,`resourcestring`,`result`,`set`}
    WordLists[8][1][64..76] = {`shl`,`shr`,`stdcall`,`then`,`to`,`type`,`unit`,`until`,`uses`,`var`,`while`,`with`,`With`}
    WordLists[8][2] = {`SysUtils`}
    WordLists[8][3][1..11] = {`AnsiChar`,`AnsiString`,`array`,`boolean`,`Boolean`,`byte`,`Byte`,`Cardinal`,`char`,`double`,`Exception`}
    WordLists[8][3][12..21] = {`int64`,`Integer`,`LongInt`,`LongWord`,`mpz_t`,`NativeInt`,`NativeUint`,`NativeUInt`,`PAnsiChar`,`pbyte`}
    WordLists[8][3][22..30] = {`pByte`,`PCardinal`,`PCppStdException`,`PExceptionRecord`,`Pointer`,`pWord`,`String`,`TDateTime`,`Uint32`}
    WordLists[8][3][31..33] = {`Uint64`,`word`,`Word`}
    WordLists[8][4][1..12] = {`Assert`,`Assigned`,`BeginThread`,`dec`,`Dec`,`EndThread`,`exit`,`GetTickCount64`,`Halt`,`high`,`High`,`inc`}
    WordLists[8][4][13..24] = {`Inc`,`include`,`InterLockedIncrement`,`length`,`low`,`Move`,`Ord`,`ORD`,`readln`,`setlength`,`SizeOf`,`sqr`}
    WordLists[8][4][25..35] = {`sqrt`,`str`,`StrComp`,`Trunc`,`trunc`,`WaitForThreadTerminate`,`write`,`Write`,`writeLn`,`writeln`,`Writeln`}
    WordLists[8][4][36..42] = {`BuiltinConstants`,`False`,`false`,`nil`,`now`,`True`,`true`}
    WordLists[8][5][1..8] = {`mpz_add_ui`,`mpz_clear`,`mpz_cmp_ui`,`mpz_init`,`mpz_init_set`,`mpz_init_set_si`,`mpz_mul`,`mpz_mul_ui`}
    WordLists[8][5][9..10] = {`mpz_sqrt`,`mpz_tdiv_q_ui`}
    WordLists[9][1][1..13] = {`align`,`at`,`begin`,`byte`,`case`,`cdata`,`code`,`coff`,`common`,`console`,`data`,`default`,`defined`}
    WordLists[9][1][14..29] = {`discardable`,`display`,`dll`,`db`,`dd`,`df`,`dp`,`dq`,`dt`,`du`,`dw`,`elf`,`else`,`end`,`enddata`,`endif`}
    WordLists[9][1][30..42] = {`endp`,`endswitch`,`entry`,`enum`,`enumflags`,`eq`,`eqtype`,`equ`,`executable`,`export`,`extrn`,`far`,`file`}
    WordLists[9][1][43..56] = {`fix`,`fixups`,`format`,`forward`,`from`,`gui`,`halt`,`heap`,`idata`,`if`,`import`,`include`,`invoke`,`label`}
    WordLists[9][1][57..70] = {`library`,`load`,`local`,`macro`,`ms`,`mz`,`native`,`ne`,`near`,`noframe`,`notpageable`,`offset`,`pe`,`proc`}
    WordLists[9][1][71..85] = {`ptr`,`public`,`readable`,`repeat`,`resource`,`restore`,`return`,`reverse`,`rva`,`rb`,`rd`,`rf`,`rp`,`rq`,`rt`}
    WordLists[9][1][86..97] = {`rw`,`section`,`segment`,`shareable`,`stack`,`stdcall`,`struc`,`struct`,`switch`,`times`,`type`,`udata`}
    WordLists[9][1][98..110] = {`use16`,`use32`,`use64`,`used`,`uses`,`var`,`virtual`,`writeable`,`word`,`dword`,`dqword`,`fword`,`pword`}
    WordLists[9][1][111..124] = {`qword`,`tbyte`,`DB`,`DLL`,`ELF`,`CONSOLE`,`GUI`,`INCLUDE`,`MS`,`COFF`,`MZ`,`NATIVE`,`PE`,`RVA`}
    WordLists[9][2][1..17] = {`aaa`,`aad`,`aam`,`aas`,`adc`,`add`,`and`,`arpl`,`bound`,`bsf`,`bsr`,`bswap`,`bt`,`btc`,`btr`,`bts`,`call`}
    WordLists[9][2][18..32] = {`cbw`,`cdq`,`clc`,`cld`,`cli`,`clts`,`cmc`,`cmp`,`cmps`,`cmpsb`,`cmpsd`,`cmpsw`,`cmpxchg`,`cmpxchg8b`,`cpuid`}
    WordLists[9][2][33..48] = {`cwd`,`cwde`,`cdqe`,`cqo`,`daa`,`das`,`dec`,`div`,`enter`,`esc`,`f2xm1`,`fabs`,`fadd`,`faddp`,`far`,`fbld`}
    WordLists[9][2][49..61] = {`fbstp`,`fchs`,`fclex`,`fnclex`,`fcom`,`fcomp`,`fcompp`,`fcos`,`fdecstp`,`fdiv`,`fdivp`,`fdivr`,`fidvrp`}
    WordLists[9][2][62..74] = {`ffree`,`fiadd`,`ficom`,`ficomp`,`fidiv`,`fidivr`,`fild`,`fimul`,`fincstp`,`finit`,`fninit`,`fist`,`fistp`}
    WordLists[9][2][75..87] = {`fisub`,`fisubr`,`fld`,`fld1`,`fldcw`,`fldenv`,`fldl2e`,`fldl2t`,`fldlg2`,`fldln2`,`fldpi`,`fldz`,`fmul`}
    WordLists[9][2][88..100] = {`fmulp`,`fnop`,`fnstcw`,`fpatan`,`fptan`,`fprem`,`fprem1`,`frndint`,`from`,`frstor`,`fsave`,`fnsave`,`fscale`}
    WordLists[9][2][101..113] = {`fsin`,`fsincos`,`fsqrt`,`fst`,`fstp`,`fstcw`,`fnstcw`,`fstenv`,`fnstenv`,`fstsw`,`fstsw`,`fnstsw`,`fsub`}
    WordLists[9][2][114..126] = {`fsubp`,`fsubr`,`fsubrp`,`ftst`,`fucom`,`fucomp`,`fucompp`,`fxam`,`fxch`,`fxtract`,`fwait`,`fyl2x`,`fyl2xp1`}
    WordLists[9][2][127..141] = {`fdisi`,`fndisi`,`feni`,`fneni`,`fsetpm`,`hlt`,`idiv`,`imul`,`in`,`inc`,`ins`,`insb`,`insd`,`insw`,`int`}
    WordLists[9][2][142..158] = {`int3`,`into`,`invd`,`invlpg`,`iret`,`iretd`,`iretw`,`ja`,`jae`,`jb`,`jbe`,`jc`,`jcxz`,`je`,`jecxz`,`jg`,`jge`}
    WordLists[9][2][159..176] = {`jl`,`jle`,`jo`,`jp`,`jpe`,`jpo`,`js`,`jz`,`jna`,`jnae`,`jnb`,`jnbe`,`jnc`,`jne`,`jng`,`jnge`,`jnl`,`jnle`}
    WordLists[9][2][177..193] = {`jno`,`jnp`,`jns`,`jnz`,`jmp`,`lahf`,`lar`,`lds`,`lea`,`les`,`lfs`,`lgs`,`lss`,`leave`,`lgdt`,`lidt`,`lldt`}
    WordLists[9][2][194..207] = {`lmsw`,`lsl`,`ltr`,`lock`,`lods`,`lodsb`,`lodsd`,`lodsw`,`lodsq`,`loop`,`loopd`,`loopw`,`loope`,`looped`}
    WordLists[9][2][208..219] = {`loopew`,`loopne`,`loopned`,`loopnew`,`loopz`,`loopzd`,`loopzw`,`loopnz`,`loopnzd`,`loopnzw`,`mod`,`mov`}
    WordLists[9][2][220..235] = {`movd`,`movs`,`movsb`,`movsd`,`movsw`,`movsx`,`movzx`,`mul`,`neg`,`nop`,`not`,`or`,`org`,`out`,`outs`,`outsb`}
    WordLists[9][2][236..249] = {`outsd`,`outsw`,`pause`,`pop`,`popa`,`popad`,`popd`,`popf`,`popfd`,`popw`,`push`,`pusha`,`pushad`,`pushd`}
    WordLists[9][2][250..263] = {`pushf`,`pushfd`,`pushw`,`rcl`,`rcr`,`rdmsr`,`rdpmc`,`rdtsc`,`ret`,`retd`,`retn`,`retnd`,`retnw`,`retf`}
    WordLists[9][2][264..279] = {`retfd`,`retfw`,`rol`,`ror`,`rsm`,`rep`,`repe`,`repne`,`repz`,`repnz`,`sahf`,`sal`,`sar`,`sbb`,`scas`,`scasb`}
    WordLists[9][2][280..293] = {`scasd`,`scasw`,`seta`,`setae`,`setb`,`setbe`,`setc`,`sete`,`setg`,`setge`,`setl`,`setle`,`seto`,`setp`}
    WordLists[9][2][294..306] = {`setpe`,`setpo`,`sets`,`setz`,`setna`,`setnae`,`setnb`,`setnbe`,`setnc`,`setne`,`setng`,`setnge`,`setnl`}
    WordLists[9][2][307..321] = {`setnle`,`setno`,`setnp`,`setns`,`setnz`,`sgdt`,`shl`,`shld`,`shr`,`shrd`,`sidt`,`sldt`,`smsw`,`stc`,`std`}
    WordLists[9][2][322..335] = {`sti`,`stos`,`stosb`,`stosd`,`stosq`,`stosw`,`str`,`sub`,`syscall`,`sysenter`,`sysexit`,`test`,`ud2`,`verr`}
    WordLists[9][2][336..348] = {`verw`,`wait`,`wbinvd`,`wdm`,`wrmsr`,`xadd`,`xchg`,`xlat`,`xlatb`,`xor`,`prefetchnta`,`movups`,`movntps`}
    WordLists[9][2][349..351] = {`movaps`,`sfence`,`rdrand`}
    WordLists[9][3][1..13] = {`cmova`,`cmovae`,`cmovb`,`cmovbe`,`cmovc`,`cmove`,`cmovg`,`cmovge`,`cmovl`,`cmovle`,`cmovo`,`cmovp`,`cmovpe`}
    WordLists[9][3][14..25] = {`cmovpo`,`cmovs`,`cmovz`,`cmovna`,`cmovnae`,`cmovnb`,`cmovnbe`,`cmovnc`,`cmovne`,`cmovng`,`cmovnge`,`cmovnl`}
    WordLists[9][3][26..36] = {`cmovnle`,`cmovno`,`cmovnp`,`cmovns`,`cmovnz`,`fcmovb`,`fvmovbe`,`fcmove`,`fcmovnb`,`fcmovnbe`,`fcmovne`}
    WordLists[9][3][37..42] = {`fcmovnu`,`fcmovu`,`fcomi`,`fcomip`,`fucomi`,`fucomip`}
    WordLists[9][4][1..12] = {`emms`,`movq`,`packssdw`,`packsswb`,`packuswb`,`paddb`,`paddd`,`paddw`,`paddsb`,`paddsw`,`paddusb`,`paddusw`}
    WordLists[9][4][13..24] = {`pand`,`pandn`,`pcmpeqb`,`pcmpeqd`,`pcmpeqw`,`pcmpgtb`,`pcmpgtd`,`pcmpgtw`,`pmaddwd`,`pmulhw`,`pmullw`,`por`}
    WordLists[9][4][25..37] = {`pxor`,`pslld`,`psllq`,`psllw`,`psrad`,`psraw`,`psrld`,`psrlq`,`psrlw`,`psubb`,`psubd`,`psubw`,`psubsb`}
    WordLists[9][4][38..46] = {`psubsw`,`psubusb`,`psubusw`,`punpckhbw`,`punpckhdq`,`punpckhwd`,`punpcklbw`,`punpckldq`,`punpcklwd`}
    WordLists[9][5][1..12] = {`movlps`,`movhps`,`movlhps`,`movhlps`,`movmskps`,`movss`,`addps`,`addss`,`subps`,`subss`,`mulps`,`mulss`}
    WordLists[9][5][13..25] = {`divps`,`divss`,`rcpps`,`rcpss`,`sqrtps`,`sqrtss`,`rsqrtps`,`rsqrtss`,`maxps`,`maxss`,`minps`,`minss`,`andps`}
    WordLists[9][5][26..36] = {`andnps`,`orps`,`xorps`,`cmpps`,`cmpss`,`cmpeqps`,`cmpleps`,`cmpltps`,`cmpunordps`,`cmpneqps`,`cmpnltps`}
    WordLists[9][5][37..46] = {`cmpnleps`,`cmpordps`,`cmpeqss`,`cmpless`,`cmpltss`,`cmpunordss`,`cmpneqss`,`cmpnltss`,`cmpnless`,`cmpordss`}
    WordLists[9][5][47..56] = {`comiss`,`ucomiss`,`shufps`,`unpckhps`,`unpcklps`,`cvtpi2ps`,`cvtsi2ss`,`cvtps2pi`,`cvttps2pi`,`cvtss2si`}
    WordLists[9][5][57..67] = {`cvttss2si`,`pextrw`,`pinsrw`,`pavgb`,`pavgw`,`pmaxub`,`pminub`,`pmaxsw`,`pminsw`,`pmulhuw`,`psadbw`}
    WordLists[9][5][68..77] = {`pmovmskb`,`pshufw`,`movntq`,`maskmovq`,`prefetcht0`,`prefetcht1`,`prefetcht2`,`ldmxcsr`,`stmxcsr`,`fxsave`}
    WordLists[9][5][78..89] = {`fxrstor`,`movapd`,`movupd`,`movlpd`,`movhpd`,`movmskpd`,`addpd`,`addsd`,`subpd`,`subsd`,`mulpd`,`mulsd`}
    WordLists[9][5][90..102] = {`divpd`,`divsd`,`sqrtpd`,`sqrtsd`,`maxpd`,`maxsd`,`minpd`,`minsd`,`andpd`,`andnpd`,`orpd`,`xorpd`,`cmppd`}
    WordLists[9][5][103..112] = {`cmpeqpd`,`cmplepd`,`cmpltpd`,`cmpunordpd`,`cmpneqpd`,`cmpnltpd`,`cmpnlepd`,`cmpordpd`,`cmpeqsd`,`cmplesd`}
    WordLists[9][5][113..122] = {`cmpltsd`,`cmpunordsd`,`cmpneqsd`,`cmpnltsd`,`cmpnlesd`,`cmpordsd`,`comisd`,`ucomisd`,`shufpd`,`unpckhpd`}
    WordLists[9][5][123..132] = {`unpcklpd`,`cvtps2pd`,`cvtpd2ps`,`cvtss2sd`,`cvtsd2ss`,`cvtpi2pd`,`cvtsi2sd`,`cvtpd2pi`,`cvttpd2pi`,`cvtsd2si`}
    WordLists[9][5][133..142] = {`cvttsd2si`,`cvtps2dq`,`cvttps2dq`,`cvtpd2dq`,`cvttpd2dq`,`cvtdq2ps`,`cvtdq2pd`,`movdqa`,`movdqu`,`movq2dq`}
    WordLists[9][5][143..153] = {`movdq2q`,`pshufhw`,`pshuflw`,`pshufd`,`paddq`,`psubq`,`pmuludq`,`pslldq`,`psrldq`,`punpckhqdq`,`punpcklqdq`}
    WordLists[9][5][154..164] = {`movntdq`,`movntpd`,`movnti`,`maskmovdqu`,`clflush`,`lfence`,`mfence`,`fisttp`,`movshdup`,`movshdup`,`movddup`}
    WordLists[9][5][165..176] = {`lddqu`,`addsubps`,`addsubpd`,`haddps`,`haddpd`,`monitor`,`mwait`,`femms`,`pavgusb`,`pmulhrw`,`pi2fd`,`pi2fw`}
    WordLists[9][5][177..189] = {`pi2id`,`pi2iw`,`pfadd`,`pfsub`,`pfsubr`,`pfmul`,`pfacc`,`pfnacc`,`pfpnacc`,`pfmax`,`pfmin`,`pswapd`,`pfrcp`}
    WordLists[9][5][190..198] = {`pfrsqrt`,`pfrcpit1`,`pfrcpit2`,`pfrsqit1`,`pfcmpeq`,`pfcmpge`,`pfcmpgt`,`prefetch`,`prefetchw`}
    WordLists[9][6][1..22] = {`ah`,`al`,`ax`,`bh`,`bl`,`bx`,`ch`,`cl`,`cx`,`dh`,`dl`,`dx`,`AH`,`AL`,`AX`,`BH`,`BL`,`BX`,`CH`,`CL`,`CX`,`DH`}
    WordLists[9][6][23..40] = {`DL`,`DX`,`eax`,`ecx`,`edx`,`ebx`,`esp`,`ebp`,`esi`,`edi`,`eip`,`EAX`,`ECX`,`EDX`,`EBX`,`ESP`,`EBP`,`ESI`}
    WordLists[9][6][41..58] = {`EDI`,`EIP`,`rax`,`rcx`,`rdx`,`rbx`,`rsp`,`rbp`,`rsi`,`rdi`,`r8`,`r9`,`r10`,`r11`,`r12`,`r13`,`r14`,`r15`}
    WordLists[9][6][59..76] = {`RAX`,`RCX`,`RDX`,`RBX`,`RSP`,`RBP`,`RSI`,`RDI`,`R8`,`R9`,`R10`,`R11`,`R12`,`R13`,`R14`,`R15`,`spl`,`bpl`}
    WordLists[9][6][77..93] = {`sil`,`dil`,`SPL`,`BPL`,`SIL`,`DIL`,`r8b`,`r9b`,`r10b`,`r11b`,`r12b`,`r13b`,`r14b`,`r15b`,`R8B`,`R9B`,`R10B`}
    WordLists[9][6][94..109] = {`R11B`,`R12B`,`R13B`,`R14B`,`R15B`,`r8w`,`r9w`,`r10w`,`r11w`,`r12w`,`r13w`,`r14w`,`r15w`,`R8W`,`R9W`,`R10W`}
    WordLists[9][6][110..125] = {`R11W`,`R12W`,`R13W`,`R14W`,`R15W`,`r8d`,`r9d`,`r10d`,`r11d`,`r12d`,`r13d`,`r14d`,`r15d`,`R8D`,`R9D`,`R10D`}
    WordLists[9][6][126..144] = {`R11D`,`R12D`,`R13D`,`R14D`,`R15D`,`cs`,`ds`,`es`,`fs`,`gs`,`ss`,`CS`,`DS`,`ES`,`FS`,`GS`,`SS`,`cr0`,`cr2`}
    WordLists[9][6][145..162] = {`cr3`,`cr4`,`dr0`,`dr1`,`dr2`,`dr3`,`dr6`,`dr7`,`dr_0`,`dr_1`,`dr_2`,`dr_3`,`dr_6`,`dr_7`,`sp`,`bp`,`si`,`di`}
    WordLists[9][6][163..181] = {`ip`,`st`,`SP`,`BP`,`SI`,`DI`,`IP`,`ST`,`mm0`,`mm1`,`mm2`,`mm3`,`mm4`,`mm5`,`mm6`,`mm7`,`MM0`,`MM1`,`MM2`}
    WordLists[9][6][182..199] = {`MM3`,`MM4`,`MM5`,`MM6`,`MM7`,`st0`,`st1`,`st2`,`st3`,`st4`,`st5`,`st6`,`st7`,`ST0`,`ST1`,`ST2`,`ST3`,`ST4`}
    WordLists[9][6][200..217] = {`ST5`,`ST6`,`ST7`,`tr0`,`tr1`,`tr2`,`tr3`,`tr4`,`tr5`,`tr6`,`tr7`,`TR0`,`TR1`,`TR2`,`TR3`,`TR4`,`TR5`,`TR6`}
    WordLists[9][6][218..232] = {`TR7`,`xmm0`,`xmm1`,`xmm2`,`xmm3`,`xmm4`,`xmm5`,`xmm6`,`xmm7`,`xmm8`,`xmm9`,`xmm10`,`xmm11`,`xmm12`,`xmm13`}
    WordLists[9][6][233..247] = {`xmm14`,`xmm15`,`XMM0`,`XMM1`,`XMM2`,`XMM3`,`XMM4`,`XMM5`,`XMM6`,`XMM7`,`XMM8`,`XMM9`,`XMM10`,`XMM11`,`XMM12`}
    WordLists[9][6][248..250] = {`XMM13`,`XMM14`,`XMM15`}
    WordLists[9][7][1..7] = {`_wsprintfA`,`AbortDoc`,`AbortPath`,`AbortPrinter`,`AbortSystemShutdown`,`accept`,`AccessCheck`}
    WordLists[9][7][8..12] = {`AccessCheckAndAuditAlarm`,`ActivateKeyboardLayout`,`AddAccessAllowedAce`,`AddAccessDeniedAce`,`AddAce`}
    WordLists[9][7][13..19] = {`AddAuditAccessAce`,`AddFontResource`,`AddForm`,`AddJob`,`AddMonitor`,`AddPort`,`AddPrinter`}
    WordLists[9][7][20..24] = {`AddPrinterConnection`,`AddPrinterDriver`,`AddPrintProcessor`,`AddPrintProvidor`,`AdjustTokenGroups`}
    WordLists[9][7][25..28] = {`AdjustTokenPrivileges`,`AdjustWindowRect`,`AdjustWindowRectEx`,`AdvancedDocumentProperties`}
    WordLists[9][7][29..34] = {`AllocateAndInitializeSid`,`AllocateLocallyUniqueId`,`AllocConsole`,`AngleArc`,`AnimatePalette`,`AnyPopup`}
    WordLists[9][7][35..40] = {`AppendMenu`,`Arc`,`ArcTo`,`AreAllAccessesGranted`,`AreAnyAccessesGranted`,`ArrangeIconicWindows`}
    WordLists[9][7][41..46] = {`AttachThreadInput`,`auxGetDevCaps`,`auxGetNumDevs`,`auxGetVolume`,`auxOutMessage`,`auxSetVolume`}
    WordLists[9][7][47..54] = {`BackupEventLog`,`BackupRead`,`BackupSeek`,`BackupWrite`,`Beep`,`BeginDeferWindowPos`,`BeginPaint`,`BeginPath`}
    WordLists[9][7][55..60] = {`BeginUpdateResource`,`bind`,`BitBlt`,`BringWindowToTop`,`BroadcastSystemMessage`,`BuildCommDCB`}
    WordLists[9][7][61..66] = {`BuildCommDCBAndTimeouts`,`CallMsgFilter`,`CallNamedPipe`,`CallNextHookEx`,`CallWindowProc`,`CancelDC`}
    WordLists[9][7][67..72] = {`CascadeWindows`,`ChangeClipboardChain`,`ChangeMenu`,`ChangeServiceConfig`,`CharLower`,`CharLowerBuff`}
    WordLists[9][7][73..79] = {`CharNext`,`CharPrev`,`CharToOem`,`CharToOemBuff`,`CharUpper`,`CharUpperBuff`,`CheckColorsInGamut`}
    WordLists[9][7][80..84] = {`CheckDlgButton`,`CheckMenuItem`,`CheckMenuRadioItem`,`CheckRadioButton`,`ChildWindowFromPoint`}
    WordLists[9][7][85..90] = {`ChildWindowFromPointEx`,`ChooseColor`,`ChooseFont`,`ChoosePixelFormat`,`Chord`,`ClearCommBreak`}
    WordLists[9][7][91..97] = {`ClearCommError`,`ClearEventLog`,`ClientToScreen`,`ClipCursor`,`CloseClipboard`,`CloseDesktop`,`CloseDriver`}
    WordLists[9][7][98..103] = {`CloseEnhMetaFile`,`CloseEventLog`,`CloseFigure`,`CloseHandle`,`CloseMetaFile`,`ClosePrinter`}
    WordLists[9][7][104..109] = {`CloseServiceHandle`,`closesocket`,`CloseWindow`,`CloseWindowStation`,`CoCreateInstance`,`CoInitialize`}
    WordLists[9][7][110..114] = {`ColorMatchToTarget`,`CombineRgn`,`CombineTransform`,`CommandLineToArgv`,`CommConfigDialog`}
    WordLists[9][7][115..120] = {`CommDlgExtendedError`,`CompareFileTime`,`CompareString`,`ConfigurePort`,`connect`,`ConnectNamedPipe`}
    WordLists[9][7][121..125] = {`ConnectToPrinterDlg`,`ContinueDebugEvent`,`ControlService`,`ConvertDefaultLocale`,`CopyAcceleratorTable`}
    WordLists[9][7][126..133] = {`CopyCursor`,`CopyEnhMetaFile`,`CopyFile`,`CopyIcon`,`CopyImage`,`CopyLZFile`,`CopyMemory`,`CopyMetaFile`}
    WordLists[9][7][134..139] = {`CopyRect`,`CopySid`,`CoUninitialize`,`CountClipboardFormats`,`CreateAcceleratorTable`,`CreateBitmap`}
    WordLists[9][7][140..144] = {`CreateBitmapIndirect`,`CreateBrushIndirect`,`CreateCaret`,`CreateColorSpace`,`CreateCompatibleBitmap`}
    WordLists[9][7][145..149] = {`CreateCompatibleDC`,`CreateConsoleScreenBuffer`,`CreateCursor`,`CreateDC`,`CreateDesktop`}
    WordLists[9][7][150..153] = {`CreateDialogIndirectParam`,`CreateDialogParam`,`CreateDIBitmap`,`CreateDIBPatternBrush`}
    WordLists[9][7][154..158] = {`CreateDIBPatternBrushPt`,`CreateDIBSection`,`CreateDirectory`,`CreateDirectoryEx`,`CreateDiscardableBitmap`}
    WordLists[9][7][159..164] = {`CreateEllipticRgn`,`CreateEllipticRgnIndirect`,`CreateEnhMetaFile`,`CreateEvent`,`CreateFile`,`CreateFileA`}
    WordLists[9][7][165..170] = {`CreateFileMapping`,`CreateFont`,`CreateFontIndirect`,`CreateHalftonePalette`,`CreateHatchBrush`,`CreateIC`}
    WordLists[9][7][171..175] = {`CreateIcon`,`CreateIconFromResource`,`CreateIconIndirect`,`CreateIoCompletionPort`,`CreateMailslot`}
    WordLists[9][7][176..181] = {`CreateMDIWindow`,`CreateMenu`,`CreateMetaFile`,`CreateMutex`,`CreateNamedPipe`,`CreatePalette`}
    WordLists[9][7][182..187] = {`CreatePatternBrush`,`CreatePen`,`CreatePenIndirect`,`CreatePipe`,`CreatePolygonRgn`,`CreatePolyPolygonRgn`}
    WordLists[9][7][188..192] = {`CreatePopupMenu`,`CreatePrivateObjectSecurity`,`CreateProcess`,`CreateProcessAsUser`,`CreateRectRgn`}
    WordLists[9][7][193..196] = {`CreateRectRgnIndirect`,`CreateRemoteThread`,`CreateRoundRectRgn`,`CreateScalableFontResource`}
    WordLists[9][7][197..202] = {`CreateSemaphore`,`CreateService`,`CreateSolidBrush`,`CreateTapePartition`,`CreateThread`,`CreateWindowEx`}
    WordLists[9][7][203..207] = {`CreateWindowExA`,`CreateWindowExW`,`DdeAbandonTransaction`,`DdeAccessData`,`DdeAddData`}
    WordLists[9][7][208..212] = {`DdeClientTransaction`,`DdeCmpStringHandles`,`DdeConnect`,`DdeConnectList`,`DdeCreateDataHandle`}
    WordLists[9][7][213..217] = {`DdeCreateStringHandle`,`DdeDisconnect`,`DdeDisconnectList`,`DdeEnableCallback`,`DdeFreeDataHandle`}
    WordLists[9][7][218..222] = {`DdeFreeStringHandle`,`DdeGetData`,`DdeGetLastError`,`DdeImpersonateClient`,`DdeInitialize`}
    WordLists[9][7][223..227] = {`DdeKeepStringHandle`,`DdeNameService`,`DdePostAdvise`,`DdeQueryConvInfo`,`DdeQueryNextServer`}
    WordLists[9][7][228..232] = {`DdeQueryString`,`DdeReconnect`,`DdeSetQualityOfService`,`DdeSetUserHandle`,`DdeUnaccessData`}
    WordLists[9][7][233..238] = {`DdeUninitialize`,`DebugActiveProcess`,`DebugBreak`,`DefDlgProc`,`DefDriverProc`,`DeferWindowPos`}
    WordLists[9][7][239..244] = {`DefFrameProc`,`DefineDosDevice`,`DefMDIChildProc`,`DefWindowProc`,`DefWindowProcA`,`DefWindowProcW`}
    WordLists[9][7][245..250] = {`DeleteAce`,`DeleteColorSpace`,`DeleteCriticalSection`,`DeleteDC`,`DeleteEnhMetaFile`,`DeleteFile`}
    WordLists[9][7][251..257] = {`DeleteForm`,`DeleteMenu`,`DeleteMetaFile`,`DeleteMonitor`,`DeleteObject`,`DeletePort`,`DeletePrinter`}
    WordLists[9][7][258..262] = {`DeletePrinterConnection`,`DeletePrinterDriver`,`DeletePrintProcessor`,`DeletePrintProvidor`,`DeleteService`}
    WordLists[9][7][263..267] = {`DeregisterEventSource`,`DescribePixelFormat`,`DestroyAcceleratorTable`,`DestroyCaret`,`DestroyCursor`}
    WordLists[9][7][268..272] = {`DestroyIcon`,`DestroyMenu`,`DestroyPrivateObjectSecurity`,`DestroyWindow`,`DeviceCapabilities`}
    WordLists[9][7][273..277] = {`DeviceIoControl`,`DialogBoxParam`,`DialogBoxParamA`,`DialogBoxIndirectParam`,`DisableThreadLibraryCalls`}
    WordLists[9][7][278..282] = {`DisconnectNamedPipe`,`DispatchMessage`,`DispatchMessageA`,`DispatchMessageW`,`DlgDirList`}
    WordLists[9][7][283..287] = {`DlgDirListComboBox`,`DlgDirSelectComboBoxEx`,`DlgDirSelectEx`,`DocumentProperties`,`DoEnvironmentSubst`}
    WordLists[9][7][288..294] = {`DosDateTimeToFileTime`,`DPtoLP`,`DragAcceptFiles`,`DragDetect`,`DragFinish`,`DragObject`,`DragQueryFile`}
    WordLists[9][7][295..301] = {`DragQueryPoint`,`DrawAnimatedRects`,`DrawCaption`,`DrawEdge`,`DrawEscape`,`DrawFocusRect`,`DrawFrameControl`}
    WordLists[9][7][302..308] = {`DrawIcon`,`DrawIconEx`,`DrawMenuBar`,`DrawState`,`DrawText`,`DrawTextEx`,`DrvGetModuleHandle`}
    WordLists[9][7][309..314] = {`DuplicateHandle`,`DuplicateIcon`,`DuplicateToken`,`Ellipse`,`EmptyClipboard`,`EnableMenuItem`}
    WordLists[9][7][315..321] = {`EnableScrollBar`,`EnableWindow`,`EndDeferWindowPos`,`EndDialog`,`EndDoc`,`EndDocPrinter`,`EndPage`}
    WordLists[9][7][322..327] = {`EndPagePrinter`,`EndPaint`,`EndPath`,`EndUpdateResource`,`EnterCriticalSection`,`EnumCalendarInfo`}
    WordLists[9][7][328..332] = {`EnumChildWindows`,`EnumClipboardFormats`,`EnumDateFormats`,`EnumDependentServices`,`EnumDesktops`}
    WordLists[9][7][333..338] = {`EnumDesktopWindows`,`EnumEnhMetaFile`,`EnumFontFamilies`,`EnumFontFamiliesEx`,`EnumFonts`,`EnumForms`}
    WordLists[9][7][339..345] = {`EnumICMProfiles`,`EnumJobs`,`EnumMetaFile`,`EnumMonitors`,`EnumObjects`,`EnumPorts`,`EnumPrinterDrivers`}
    WordLists[9][7][346..349] = {`EnumPrinterPropertySheets`,`EnumPrinters`,`EnumPrintProcessorDatatypes`,`EnumPrintProcessorDatatypes`}
    WordLists[9][7][350..354] = {`EnumPrintProcessors`,`EnumPrintProcessors`,`EnumProps`,`EnumPropsEx`,`EnumResourceLanguages`}
    WordLists[9][7][355..359] = {`EnumResourceNames`,`EnumResourceTypes`,`EnumServicesStatus`,`EnumServicesStatus`,`EnumSystemCodePages`}
    WordLists[9][7][360..365] = {`EnumSystemLocales`,`EnumThreadWindows`,`EnumTimeFormats`,`EnumWindows`,`EnumWindowStations`,`EqualPrefixSid`}
    WordLists[9][7][366..372] = {`EqualRect`,`EqualRgn`,`EqualSid`,`EraseTape`,`Escape`,`EscapeCommFunction`,`ExcludeClipRect`}
    WordLists[9][7][373..378] = {`ExcludeUpdateRgn`,`ExitProcess`,`ExitThread`,`ExitWindows`,`ExitWindowsEx`,`ExpandEnvironmentStrings`}
    WordLists[9][7][379..384] = {`ExtCreatePen`,`ExtCreateRegion`,`ExtEscape`,`ExtFloodFill`,`ExtractAssociatedIcon`,`ExtractIcon`}
    WordLists[9][7][385..390] = {`ExtractIconEx`,`ExtSelectClipRgn`,`ExtTextOut`,`FatalAppExit`,`FatalExit`,`FileTimeToDosDateTime`}
    WordLists[9][7][391..394] = {`FileTimeToLocalFileTime`,`FileTimeToSystemTime`,`FillConsoleOutputAttribute`,`FillConsoleOutputCharacter`}
    WordLists[9][7][395..400] = {`FillPath`,`FillRect`,`FillRgn`,`FindClose`,`FindCloseChangeNotification`,`FindClosePrinterChangeNotification`}
    WordLists[9][7][401..405] = {`FindEnvironmentString`,`FindExecutable`,`FindFirstChangeNotification`,`FindFirstFile`,`FindFirstFreeAce`}
    WordLists[9][7][406..408] = {`FindFirstPrinterChangeNotification`,`FindNextChangeNotification`,`FindNextFile`}
    WordLists[9][7][409..414] = {`FindNextPrinterChangeNotification`,`FindResource`,`FindResourceA`,`FindResourceEx`,`FindText`,`FindWindow`}
    WordLists[9][7][415..420] = {`FindWindowEx`,`FixBrushOrgEx`,`FlashWindow`,`FlattenPath`,`FloodFill`,`FlushConsoleInputBuffer`}
    WordLists[9][7][421..426] = {`FlushFileBuffers`,`FlushInstructionCache`,`FlushViewOfFile`,`FoldString`,`FormatMessage`,`FrameRect`}
    WordLists[9][7][427..432] = {`FrameRgn`,`FreeConsole`,`FreeDDElParam`,`FreeEnvironmentStrings`,`FreeLibrary`,`FreeLibraryAndExitThread`}
    WordLists[9][7][433..438] = {`FreeResource`,`FreeSid`,`GdiComment`,`GdiFlush`,`GdiGetBatchLimit`,`GdiSetBatchLimit`}
    WordLists[9][7][439..444] = {`GenerateConsoleCtrlEvent`,`gethostbyname`,`GetAce`,`GetAclInformation`,`GetACP`,`GetActiveWindow`}
    WordLists[9][7][445..449] = {`GetArcDirection`,`GetAspectRatioFilterEx`,`GetAsyncKeyState`,`GetBinaryType`,`GetBitmapBits`}
    WordLists[9][7][450..455] = {`GetBitmapDimensionEx`,`GetBkColor`,`GetBkMode`,`GetBoundsRect`,`GetBrushOrgEx`,`GetCapture`}
    WordLists[9][7][456..460] = {`GetCaretBlinkTime`,`GetCaretPos`,`GetCharABCWidths`,`GetCharABCWidthsFloat`,`GetCharacterPlacement`}
    WordLists[9][7][461..466] = {`GetCharWidth`,`GetCharWidth32`,`GetCharWidthFloat`,`GetClassInfo`,`GetClassLong`,`GetClassName`}
    WordLists[9][7][467..471] = {`GetClassWord`,`GetClientRect`,`GetClipboardData`,`GetClipboardFormatName`,`GetClipboardOwner`}
    WordLists[9][7][472..477] = {`GetClipboardViewer`,`GetClipBox`,`GetClipCursor`,`GetClipRgn`,`GetColorAdjustment`,`GetColorSpace`}
    WordLists[9][7][478..483] = {`GetCommandLine`,`GetCommConfig`,`GetCommMask`,`GetCommModemStatus`,`GetCommProperties`,`GetCommState`}
    WordLists[9][7][484..488] = {`GetCommTimeouts`,`GetCompressedFileSize`,`GetComputerName`,`GetComputerNameW`,`GetConsoleCP`}
    WordLists[9][7][489..493] = {`GetConsoleCursorInfo`,`GetConsoleMode`,`GetConsoleOutputCP`,`GetConsoleScreenBufferInfo`,`GetConsoleTitle`}
    WordLists[9][7][494..498] = {`GetCPInfo`,`GetCurrencyFormat`,`GetCurrentDirectory`,`GetCurrentDirectoryA`,`GetCurrentObject`}
    WordLists[9][7][499..503] = {`GetCurrentPositionEx`,`GetCurrentProcess`,`GetCurrentProcessId`,`GetCurrentThread`,`GetCurrentThreadId`}
    WordLists[9][7][504..510] = {`GetCurrentTime`,`GetCursor`,`GetCursorPos`,`GetDateFormat`,`GetDC`,`GetDCEx`,`GetDCOrgEx`}
    WordLists[9][7][511..515] = {`GetDefaultCommConfig`,`GetDesktopWindow`,`GetDeviceCaps`,`GetDeviceGammaRamp`,`GetDialogBaseUnits`}
    WordLists[9][7][516..522] = {`GetDIBColorTable`,`GetDIBits`,`GetDiskFreeSpace`,`GetDlgCtrlID`,`GetDlgItem`,`GetDlgItemInt`,`GetDlgItemText`}
    WordLists[9][7][523..527] = {`GetDoubleClickTime`,`GetDriverModuleHandle`,`GetDriveType`,`GetEnhMetaFile`,`GetEnhMetaFileBits`}
    WordLists[9][7][528..531] = {`GetEnhMetaFileDescription`,`GetEnhMetaFileHeader`,`GetEnhMetaFilePaletteEntries`,`GetEnvironmentStrings`}
    WordLists[9][7][532..536] = {`GetEnvironmentVariable`,`GetExitCodeProcess`,`GetExitCodeThread`,`GetExpandedName`,`GetFileAttributes`}
    WordLists[9][7][537..542] = {`GetFileInformationByHandle`,`GetFileSecurity`,`GetFileSize`,`GetFileTime`,`GetFileTitle`,`GetFileType`}
    WordLists[9][7][543..547] = {`GetFileVersionInfo`,`GetFileVersionInfoSize`,`GetFocus`,`GetFontData`,`GetFontLanguageInfo`}
    WordLists[9][7][548..553] = {`GetForegroundWindow`,`GetForm`,`GetFullPathName`,`GetGlyphOutline`,`GetGraphicsMode`,`GetHandleInformation`}
    WordLists[9][7][554..559] = {`GetICMProfile`,`GetIconInfo`,`GetInputState`,`GetJob`,`GetKBCodePage`,`GetKernelObjectSecurity`}
    WordLists[9][7][560..564] = {`GetKerningPairs`,`GetKeyboardLayout`,`GetKeyboardLayoutList`,`GetKeyboardLayoutName`,`GetKeyboardState`}
    WordLists[9][7][565..569] = {`GetKeyboardType`,`GetKeyNameText`,`GetKeyState`,`GetLargestConsoleWindowSize`,`GetLastActivePopup`}
    WordLists[9][7][570..575] = {`GetLastError`,`GetLengthSid`,`GetLocaleInfo`,`GetLocalTime`,`GetLogColorSpace`,`GetLogicalDrives`}
    WordLists[9][7][576..580] = {`GetLogicalDriveStrings`,`GetMailslotInfo`,`GetMapMode`,`GetMenu`,`GetMenuCheckMarkDimensions`}
    WordLists[9][7][581..585] = {`GetMenuContextHelpId`,`GetMenuDefaultItem`,`GetMenuItemCount`,`GetMenuItemID`,`GetMenuItemInfo`}
    WordLists[9][7][586..591] = {`GetMenuItemRect`,`GetMenuState`,`GetMenuString`,`GetMessage`,`GetMessageA`,`GetMessageW`}
    WordLists[9][7][592..597] = {`GetMessageExtraInfo`,`GetMessagePos`,`GetMessageTime`,`GetMetaFile`,`GetMetaFileBitsEx`,`GetMetaRgn`}
    WordLists[9][7][598..602] = {`GetMiterLimit`,`GetModuleFileName`,`GetModuleHandle`,`GetModuleHandleA`,`GetModuleHandleW`}
    WordLists[9][7][603..607] = {`GetNamedPipeHandleState`,`GetNamedPipeInfo`,`GetNearestColor`,`GetNearestPaletteIndex`,`GetNextDlgGroupItem`}
    WordLists[9][7][608..611] = {`GetNextDlgTabItem`,`GetNextWindow`,`GetNumberFormat`,`GetNumberOfConsoleInputEvents`}
    WordLists[9][7][612..616] = {`GetNumberOfConsoleMouseButtons`,`GetNumberOfEventLogRecords`,`GetObject`,`GetObjectType`,`GetOEMCP`}
    WordLists[9][7][617..620] = {`GetOldestEventLogRecord`,`GetOpenClipboardWindow`,`GetOpenFileName`,`GetOutlineTextMetrics`}
    WordLists[9][7][621..627] = {`GetOverlappedResult`,`GetPaletteEntries`,`GetParent`,`GetPath`,`GetPixel`,`GetPixelFormat`,`GetPolyFillMode`}
    WordLists[9][7][628..632] = {`GetPrinter`,`GetPrinterData`,`GetPrinterDriver`,`GetPrinterDriverDirectory`,`GetPrintProcessorDirectory`}
    WordLists[9][7][633..636] = {`GetPriorityClass`,`GetPriorityClipboardFormat`,`GetPrivateObjectSecurity`,`GetPrivateProfileInt`}
    WordLists[9][7][637..640] = {`GetPrivateProfileSection`,`GetPrivateProfileString`,`GetProcAddress`,`GetProcessAffinityMask`}
    WordLists[9][7][641..645] = {`GetProcessHeap`,`GetProcessHeaps`,`GetProcessShutdownParameters`,`GetProcessTimes`,`GetProcessWindowStation`}
    WordLists[9][7][646..650] = {`GetProcessWorkingSetSize`,`GetProfileInt`,`GetProfileSection`,`GetProfileString`,`GetProp`}
    WordLists[9][7][651..656] = {`GetQueuedCompletionStatus`,`GetQueueStatus`,`GetRasterizerCaps`,`GetRegionData`,`GetRgnBox`,`GetROP2`}
    WordLists[9][7][657..661] = {`GetSaveFileName`,`GetScrollInfo`,`GetScrollPos`,`GetScrollRange`,`GetSecurityDescriptorControl`}
    WordLists[9][7][662..664] = {`GetSecurityDescriptorDacl`,`GetSecurityDescriptorGroup`,`GetSecurityDescriptorLength`}
    WordLists[9][7][665..668] = {`GetSecurityDescriptorOwner`,`GetSecurityDescriptorSacl`,`GetServiceDisplayName`,`GetServiceKeyName`}
    WordLists[9][7][669..672] = {`GetShortPathName`,`GetSidIdentifierAuthority`,`GetSidLengthRequired`,`GetSidSubAuthority`}
    WordLists[9][7][673..677] = {`GetSidSubAuthorityCount`,`GetStartupInfo`,`GetStdHandle`,`GetStockObject`,`GetStretchBltMode`}
    WordLists[9][7][678..683] = {`GetStringTypeA`,`GetStringTypeEx`,`GetStringTypeW`,`GetSubMenu`,`GetSysColor`,`GetSysColorBrush`}
    WordLists[9][7][684..688] = {`GetSystemDefaultLangID`,`GetSystemDefaultLCID`,`GetSystemDirectory`,`GetSystemInfo`,`GetSystemMenu`}
    WordLists[9][7][689..693] = {`GetSystemMetrics`,`GetSystemPaletteEntries`,`GetSystemPaletteUse`,`GetSystemPowerStatus`,`GetSystemTime`}
    WordLists[9][7][694..698] = {`GetSystemTimeAdjustment`,`GetTabbedTextExtent`,`GetTapeParameters`,`GetTapePosition`,`GetTapeStatus`}
    WordLists[9][7][699..704] = {`GetTempFileName`,`GetTempPath`,`GetTextAlign`,`GetTextCharacterExtra`,`GetTextCharset`,`GetTextCharsetInfo`}
    WordLists[9][7][705..709] = {`GetTextColor`,`GetTextExtentExPoint`,`GetTextExtentPoint`,`GetTextExtentPoint32`,`GetTextFace`}
    WordLists[9][7][710..714] = {`GetTextMetrics`,`GetThreadContext`,`GetThreadDesktop`,`GetThreadLocale`,`GetThreadPriority`}
    WordLists[9][7][715..719] = {`GetThreadSelectorEntry`,`GetThreadTimes`,`GetTickCount`,`GetTimeFormat`,`GetTimeZoneInformation`}
    WordLists[9][7][720..724] = {`GetTokenInformation`,`GetTopWindow`,`GetUpdateRect`,`GetUpdateRgn`,`GetUserDefaultLangID`}
    WordLists[9][7][725..729] = {`GetUserDefaultLCID`,`GetUserName`,`GetUserNameW`,`GetUserObjectInformation`,`GetUserObjectSecurity`}
    WordLists[9][7][730..735] = {`GetVersion`,`GetVersionEx`,`GetViewportExtEx`,`GetViewportOrgEx`,`GetVolumeInformation`,`GetWindow`}
    WordLists[9][7][736..741] = {`GetWindowContextHelpId`,`GetWindowDC`,`GetWindowExtEx`,`GetWindowLong`,`GetWindowOrgEx`,`GetWindowPlacement`}
    WordLists[9][7][742..746] = {`GetWindowRect`,`GetWindowRgn`,`GetWindowsDirectory`,`GetWindowsDirectoryA`,`GetWindowText`}
    WordLists[9][7][747..751] = {`GetWindowTextLength`,`GetWindowThreadProcessId`,`GetWindowWord`,`GetWinMetaFileBits`,`GetWorldTransform`}
    WordLists[9][7][752..758] = {`GlobalAddAtom`,`GlobalAlloc`,`GlobalCompact`,`GlobalDeleteAtom`,`GlobalFindAtom`,`GlobalFix`,`GlobalFlags`}
    WordLists[9][7][759..765] = {`GlobalFree`,`GlobalGetAtomName`,`GlobalHandle`,`GlobalLock`,`GlobalMemoryStatus`,`GlobalReAlloc`,`GlobalSize`}
    WordLists[9][7][766..773] = {`GlobalUnfix`,`GlobalUnlock`,`GlobalUnWire`,`GlobalWire`,`GrayString`,`HeapAlloc`,`HeapCompact`,`HeapCreate`}
    WordLists[9][7][774..781] = {`HeapDestroy`,`HeapFree`,`HeapLock`,`HeapReAlloc`,`HeapSize`,`HeapUnlock`,`HeapValidate`,`HideCaret`}
    WordLists[9][7][782..787] = {`HiliteMenuItem`,`hread`,`hwrite`,`ImmAssociateContext`,`ImmConfigureIME`,`ImmCreateContext`}
    WordLists[9][7][788..792] = {`ImmDestroyContext`,`ImmEnumRegisterWord`,`ImmEscape`,`ImmGetCandidateList`,`ImmGetCandidateListCount`}
    WordLists[9][7][793..796] = {`ImmGetCandidateWindow`,`ImmGetCompositionFont`,`ImmGetCompositionString`,`ImmGetCompositionWindow`}
    WordLists[9][7][797..801] = {`ImmGetContext`,`ImmGetConversionList`,`ImmGetConversionStatus`,`ImmGetDefaultIMEWnd`,`ImmGetDescription`}
    WordLists[9][7][802..806] = {`ImmGetGuideLine`,`ImmGetIMEFileName`,`ImmGetOpenStatus`,`ImmGetProperty`,`ImmGetRegisterWordStyle`}
    WordLists[9][7][807..812] = {`ImmGetStatusWindowPos`,`ImmGetVirtualKey`,`ImmInstallIME`,`ImmIsIME`,`ImmIsUIMessage`,`ImmNotifyIME`}
    WordLists[9][7][813..816] = {`ImmRegisterWord`,`ImmReleaseContext`,`ImmSetCandidateWindow`,`ImmSetCompositionFont`}
    WordLists[9][7][817..820] = {`ImmSetCompositionString`,`ImmSetCompositionWindow`,`ImmSetConversionStatus`,`ImmSetOpenStatus`}
    WordLists[9][7][821..824] = {`ImmSetStatusWindowPos`,`ImmSimulateHotKey`,`ImmUnregisterWord`,`ImpersonateDdeClientWindow`}
    WordLists[9][7][825..829] = {`ImpersonateLoggedOnUser`,`ImpersonateNamedPipeClient`,`ImpersonateSelf`,`InflateRect`,`inet_ntoa`}
    WordLists[9][7][830..834] = {`InitCommonControls`,`InitCommonControlsEx`,`InitAtomTable`,`InitializeAcl`,`InitializeCriticalSection`}
    WordLists[9][7][835..839] = {`InitializeSecurityDescriptor`,`InitializeSid`,`InitiateSystemShutdown`,`InSendMessage`,`InsertMenu`}
    WordLists[9][7][840..844] = {`InsertMenuItem`,`InterlockedDecrement`,`InterlockedExchange`,`InterlockedIncrement`,`IntersectClipRect`}
    WordLists[9][7][845..850] = {`IntersectRect`,`InternetOpen`,`InternetOpenA`,`InternetOpenUrl`,`InternetOpenUrlA`,`InternetReadFile`}
    WordLists[9][7][851..856] = {`InternetCloseHandle`,`InvalidateRect`,`InvalidateRgn`,`InvertRect`,`InvertRgn`,`IsBadCodePtr`}
    WordLists[9][7][857..862] = {`IsBadHugeReadPtr`,`IsBadHugeWritePtr`,`IsBadReadPtr`,`IsBadStringPtr`,`IsBadWritePtr`,`IsCharAlpha`}
    WordLists[9][7][863..868] = {`IsCharAlphaNumeric`,`IsCharLower`,`IsCharUpper`,`IsChild`,`IsClipboardFormatAvailable`,`IsDBCSLeadByte`}
    WordLists[9][7][869..875] = {`IsDialogMessage`,`IsDlgButtonChecked`,`IsIconic`,`IsMenu`,`IsRectEmpty`,`IsTextUnicode`,`IsValidAcl`}
    WordLists[9][7][876..881] = {`IsValidCodePage`,`IsValidLocale`,`IsValidSecurityDescriptor`,`IsValidSid`,`IsWindow`,`IsWindowEnabled`}
    WordLists[9][7][882..888] = {`IsWindowUnicode`,`IsWindowVisible`,`IsZoomed`,`joyGetDevCaps`,`joyGetNumDevs`,`joyGetPos`,`joyGetPosEx`}
    WordLists[9][7][889..895] = {`joyGetThreshold`,`joyReleaseCapture`,`joySetCapture`,`joySetThreshold`,`keybd_event`,`KillTimer`,`lclose`}
    WordLists[9][7][896..903] = {`LCMapString`,`lcreat`,`LeaveCriticalSection`,`LineDDA`,`LineTo`,`listen`,`llseek`,`LoadAccelerators`}
    WordLists[9][7][904..911] = {`LoadBitmap`,`LoadCursor`,`LoadCursorA`,`LoadCursorW`,`LoadCursorFromFile`,`LoadIcon`,`LoadIconA`,`LoadIconW`}
    WordLists[9][7][912..918] = {`LoadImage`,`LoadKeyboardLayout`,`LoadLibrary`,`LoadLibraryEx`,`LoadMenu`,`LoadMenuIndirect`,`LoadModule`}
    WordLists[9][7][919..925] = {`LoadResource`,`LoadString`,`LocalAlloc`,`LocalCompact`,`LocalFileTimeToFileTime`,`LocalFlags`,`LocalFree`}
    WordLists[9][7][926..933] = {`LocalHandle`,`LocalLock`,`LocalReAlloc`,`LocalShrink`,`LocalSize`,`LocalUnlock`,`LockFile`,`LockFileEx`}
    WordLists[9][7][934..939] = {`LockResource`,`LockServiceDatabase`,`LockWindowUpdate`,`LogonUser`,`LookupAccountName`,`LookupAccountName`}
    WordLists[9][7][940..943] = {`LookupAccountSid`,`LookupAccountSid`,`LookupIconIdFromDirectory`,`LookupIconIdFromDirectoryEx`}
    WordLists[9][7][944..950] = {`LookupPrivilegeDisplayName`,`LookupPrivilegeName`,`LookupPrivilegeValue`,`lopen`,`LPtoDP`,`lread`,`lstrcat`}
    WordLists[9][7][951..961] = {`lstrcmp`,`lstrcmpi`,`lstrcpy`,`lstrcpyn`,`lstrlen`,`lwrite`,`LZClose`,`LZCopy`,`LZDone`,`LZInit`,`LZOpenFile`}
    WordLists[9][7][962..968] = {`LZRead`,`LZSeek`,`LZStart`,`MakeAbsoluteSD`,`MakeAbsoluteSD`,`MakeSelfRelativeSD`,`MapDialogRect`}
    WordLists[9][7][969..974] = {`MapGenericMask`,`MapViewOfFile`,`MapViewOfFileEx`,`MapViewOfFileEx`,`MapVirtualKey`,`MapVirtualKeyEx`}
    WordLists[9][7][975..980] = {`MapWindowPoints`,`MaskBlt`,`MaskBlt`,`mciExecute`,`mciGetCreatorTask`,`mciGetDeviceID`}
    WordLists[9][7][981..985] = {`mciGetDeviceIDFromElementID`,`mciGetErrorString`,`mciGetYieldProc`,`mciSendCommand`,`mciSendString`}
    WordLists[9][7][986..992] = {`mciSetYieldProc`,`MenuItemFromPoint`,`MessageBeep`,`MessageBox`,`MessageBoxA`,`MessageBoxW`,`MessageBoxEx`}
    WordLists[9][7][993..998] = {`MessageBoxIndirect`,`midiConnect`,`midiDisconnect`,`midiInAddBuffer`,`midiInClose`,`midiInGetDevCaps`}
    WordLists[9][7][999..1004] = {`midiInGetErrorText`,`midiInGetID`,`midiInGetNumDevs`,`midiInMessage`,`midiInOpen`,`midiInPrepareHeader`}
    WordLists[9][7][1005..1009] = {`midiInReset`,`midiInStart`,`midiInStop`,`midiInUnprepareHeader`,`midiOutCacheDrumPatches`}
    WordLists[9][7][1010..1014] = {`midiOutCachePatches`,`midiOutClose`,`midiOutGetDevCaps`,`midiOutGetErrorText`,`midiOutGetID`}
    WordLists[9][7][1015..1020] = {`midiOutGetNumDevs`,`midiOutGetVolume`,`midiOutLongMsg`,`midiOutMessage`,`midiOutOpen`,`midiOutPrepareHeader`}
    WordLists[9][7][1021..1025] = {`midiOutReset`,`midiOutSetVolume`,`midiOutShortMsg`,`midiOutUnprepareHeader`,`midiStreamClose`}
    WordLists[9][7][1026..1030] = {`midiStreamOpen`,`midiStreamOut`,`midiStreamPause`,`midiStreamPosition`,`midiStreamProperty`}
    WordLists[9][7][1031..1036] = {`midiStreamRestart`,`midiStreamStop`,`mixerClose`,`mixerGetControlDetails`,`mixerGetDevCaps`,`mixerGetID`}
    WordLists[9][7][1037..1041] = {`mixerGetLineControls`,`mixerGetLineInfo`,`mixerGetNumDevs`,`mixerMessage`,`mixerOpen`}
    WordLists[9][7][1042..1048] = {`mixerSetControlDetails`,`mmioAdvance`,`mmioAscend`,`mmioClose`,`mmioCreateChunk`,`mmioDescend`,`mmioFlush`}
    WordLists[9][7][1049..1055] = {`mmioGetInfo`,`mmioInstallIOProcA`,`mmioOpen`,`mmioRead`,`mmioRename`,`mmioSeek`,`mmioSendMessage`}
    WordLists[9][7][1056..1061] = {`mmioSetBuffer`,`mmioSetInfo`,`mmioStringToFOURCC`,`mmioWrite`,`mmsystemGetVersion`,`ModifyMenu`}
    WordLists[9][7][1062..1067] = {`ModifyWorldTransform`,`mouse_event`,`MoveFile`,`MoveFileEx`,`MoveToEx`,`MoveWindow`}
    WordLists[9][7][1068..1072] = {`MsgWaitForMultipleObjects`,`MulDiv`,`MultiByteToWideChar`,`NetApiBufferFree`,`Netbios`}
    WordLists[9][7][1073..1077] = {`NetLocalGroupDelMembers`,`NetLocalGroupGetMembers`,`NetRemoteTOD`,`NetUserAdd`,`NetUserChangePassword`}
    WordLists[9][7][1078..1082] = {`NetUserGetGroups`,`NetUserGetInfo`,`NetUserGetLocalGroups`,`NetWkstaGetInfo`,`NetWkstaUserGetInfo`}
    WordLists[9][7][1083..1086] = {`NotifyBootConfigStatus`,`NotifyChangeEventLog`,`ObjectCloseAuditAlarm`,`ObjectOpenAuditAlarm`}
    WordLists[9][7][1087..1093] = {`ObjectPrivilegeAuditAlarm`,`OemKeyScan`,`OemToChar`,`OemToCharBuff`,`OffsetClipRgn`,`OffsetRect`,`OffsetRgn`}
    WordLists[9][7][1094..1099] = {`OffsetViewportOrgEx`,`OffsetWindowOrgEx`,`OpenBackupEventLog`,`OpenClipboard`,`OpenDesktop`,`OpenDriver`}
    WordLists[9][7][1100..1106] = {`OpenEvent`,`OpenEventLog`,`OpenFile`,`OpenFileMapping`,`OpenIcon`,`OpenInputDesktop`,`OpenMutex`}
    WordLists[9][7][1107..1113] = {`OpenPrinter`,`OpenProcess`,`OpenProcessToken`,`OpenSCManager`,`OpenSemaphore`,`OpenService`,`OpenThreadToken`}
    WordLists[9][7][1114..1119] = {`OpenWindowStation`,`OutputDebugStr`,`OutputDebugString`,`PackDDElParam`,`PageSetupDlg`,`PaintDesktop`}
    WordLists[9][7][1120..1127] = {`PaintRgn`,`PatBlt`,`PathToRegion`,`PeekConsoleInput`,`PeekMessage`,`PeekNamedPipe`,`Pie`,`PlayEnhMetaFile`}
    WordLists[9][7][1128..1134] = {`PlayEnhMetaFileRecord`,`PlayMetaFile`,`PlayMetaFileRecord`,`PlaySound`,`PlgBlt`,`PolyBezier`,`PolyBezierTo`}
    WordLists[9][7][1135..1142] = {`PolyDraw`,`Polygon`,`Polyline`,`PolylineTo`,`PolyPolygon`,`PolyPolyline`,`PolyTextOut`,`PostMessage`}
    WordLists[9][7][1143..1148] = {`PostQuitMessage`,`PostThreadMessage`,`PrepareTape`,`PrintDlg`,`PrinterMessageBox`,`PrinterProperties`}
    WordLists[9][7][1149..1155] = {`PrivilegeCheck`,`PrivilegedServiceAuditAlarm`,`PtInRect`,`PtInRegion`,`PtVisible`,`PulseEvent`,`PurgeComm`}
    WordLists[9][7][1156..1159] = {`QueryDosDevice`,`QueryPerformanceCounter`,`QueryPerformanceFrequency`,`QueryServiceConfig`}
    WordLists[9][7][1160..1164] = {`QueryServiceLockStatus`,`QueryServiceObjectSecurity`,`QueryServiceStatus`,`RaiseException`,`ReadConsole`}
    WordLists[9][7][1165..1168] = {`ReadConsoleInput`,`ReadConsoleOutput`,`ReadConsoleOutputAttribute`,`ReadConsoleOutputCharacter`}
    WordLists[9][7][1169..1175] = {`ReadEventLog`,`ReadFile`,`ReadFileEx`,`ReadPrinter`,`ReadProcessMemory`,`RealizePalette`,`Rectangle`}
    WordLists[9][7][1176..1182] = {`RectInRegion`,`RectVisible`,`recv`,`RedrawWindow`,`RegCloseKey`,`RegConnectRegistry`,`RegCreateKey`}
    WordLists[9][7][1183..1189] = {`RegCreateKeyEx`,`RegCreateKeyExA`,`RegDeleteKey`,`RegDeleteValue`,`RegEnumKey`,`RegEnumKeyEx`,`RegEnumValue`}
    WordLists[9][7][1190..1195] = {`RegFlushKey`,`RegGetKeySecurity`,`RegisterClass`,`RegisterClassA`,`RegisterClassW`,`RegisterClassEx`}
    WordLists[9][7][1196..1200] = {`RegisterClassExA`,`RegisterClassExW`,`RegisterClipboardFormat`,`RegisterEventSource`,`RegisterHotKey`}
    WordLists[9][7][1201..1205] = {`RegisterServiceCtrlHandler`,`RegisterWindowMessage`,`RegLoadKey`,`RegNotifyChangeKeyValue`,`RegOpenKey`}
    WordLists[9][7][1206..1211] = {`RegOpenKeyEx`,`RegOpenKeyExA`,`RegQueryInfoKey`,`RegQueryInfoKey`,`RegQueryValue`,`RegQueryValueEx`}
    WordLists[9][7][1212..1217] = {`RegQueryValueExA`,`RegReplaceKey`,`RegRestoreKey`,`RegSaveKey`,`RegSetKeySecurity`,`RegSetValue`}
    WordLists[9][7][1218..1224] = {`RegSetValueEx`,`RegSetValueExA`,`RegUnLoadKey`,`ReleaseCapture`,`ReleaseDC`,`ReleaseMutex`,`ReleaseSemaphore`}
    WordLists[9][7][1225..1231] = {`RemoveDirectory`,`RemoveFontResource`,`RemoveMenu`,`RemoveProp`,`ReplaceText`,`ReplyMessage`,`ReportEvent`}
    WordLists[9][7][1232..1238] = {`ResetDC`,`ResetEvent`,`ResetPrinter`,`ResizePalette`,`RestoreDC`,`ResumeThread`,`ReuseDDElParam`}
    WordLists[9][7][1239..1245] = {`RevertToSelf`,`RoundRect`,`SaveDC`,`ScaleViewportExtEx`,`ScaleWindowExtEx`,`ScheduleJob`,`ScreenToClient`}
    WordLists[9][7][1246..1251] = {`ScrollConsoleScreenBuffer`,`ScrollDC`,`ScrollWindow`,`ScrollWindowEx`,`SearchPath`,`SelectClipPath`}
    WordLists[9][7][1252..1257] = {`SelectClipRgn`,`SelectObject`,`SelectPalette`,`send`,`SendDlgItemMessage`,`SendDlgItemMessageA`}
    WordLists[9][7][1258..1262] = {`SendDriverMessage`,`SendMessage`,`SendMessageCallback`,`SendMessageTimeout`,`SendNotifyMessage`}
    WordLists[9][7][1263..1268] = {`SetAbortProc`,`SetAclInformation`,`SetActiveWindow`,`SetArcDirection`,`SetBitmapBits`,`SetBitmapDimensionEx`}
    WordLists[9][7][1269..1275] = {`SetBkColor`,`SetBkMode`,`SetBoundsRect`,`SetBrushOrgEx`,`SetCapture`,`SetCaretBlinkTime`,`SetCaretPos`}
    WordLists[9][7][1276..1281] = {`SetClassLong`,`SetClassWord`,`SetClipboardData`,`SetClipboardViewer`,`SetColorAdjustment`,`SetColorSpace`}
    WordLists[9][7][1282..1287] = {`SetCommBreak`,`SetCommConfig`,`SetCommMask`,`SetCommState`,`SetCommTimeouts`,`SetComputerName`}
    WordLists[9][7][1288..1291] = {`SetConsoleActiveScreenBuffer`,`SetConsoleCP`,`SetConsoleCtrlHandler`,`SetConsoleCursorInfo`}
    WordLists[9][7][1292..1295] = {`SetConsoleCursorPosition`,`SetConsoleMode`,`SetConsoleOutputCP`,`SetConsoleScreenBufferSize`}
    WordLists[9][7][1296..1300] = {`SetConsoleTextAttribute`,`SetConsoleTitle`,`SetConsoleWindowInfo`,`SetCurrentDirectory`,`SetCursor`}
    WordLists[9][7][1301..1306] = {`SetCursorPos`,`SetDebugErrorLevel`,`SetDefaultCommConfig`,`SetDeviceGammaRamp`,`SetDIBColorTable`,`SetDIBits`}
    WordLists[9][7][1307..1312] = {`SetDIBitsToDevice`,`SetDlgItemInt`,`SetDlgItemText`,`SetDlgItemTextA`,`SetDoubleClickTime`,`SetEndOfFile`}
    WordLists[9][7][1313..1318] = {`SetEnhMetaFileBits`,`SetEnvironmentVariable`,`SetErrorMode`,`SetEvent`,`SetFileApisToANSI`,`SetFileApisToOEM`}
    WordLists[9][7][1319..1324] = {`SetFileAttributes`,`SetFilePointer`,`SetFileSecurity`,`SetFileTime`,`SetFocus`,`SetForegroundWindow`}
    WordLists[9][7][1325..1331] = {`SetForm`,`SetGraphicsMode`,`SetHandleCount`,`SetHandleInformation`,`SetICMMode`,`SetICMProfile`,`SetJob`}
    WordLists[9][7][1332..1337] = {`SetKernelObjectSecurity`,`SetKeyboardState`,`SetLastError`,`SetLastErrorEx`,`SetLocaleInfo`,`SetLocalTime`}
    WordLists[9][7][1338..1343] = {`SetMailslotInfo`,`SetMapMode`,`SetMapperFlags`,`SetMenu`,`SetMenuContextHelpId`,`SetMenuDefaultItem`}
    WordLists[9][7][1344..1348] = {`SetMenuItemBitmaps`,`SetMenuItemInfo`,`SetMessageExtraInfo`,`SetMessageQueue`,`SetMetaFileBitsEx`}
    WordLists[9][7][1349..1354] = {`SetMetaRgn`,`SetMiterLimit`,`SetNamedPipeHandleState`,`SetPaletteEntries`,`SetParent`,`SetPixel`}
    WordLists[9][7][1355..1360] = {`SetPixelFormat`,`SetPixelV`,`SetPolyFillMode`,`SetPrinter`,`SetPrinterData`,`SetPriorityClass`}
    WordLists[9][7][1361..1364] = {`SetPrivateObjectSecurity`,`SetPrivateObjectSecurity`,`SetProcessShutdownParameters`,`SetProcessWindowStation`}
    WordLists[9][7][1365..1371] = {`SetProcessWorkingSetSize`,`SetProp`,`SetRect`,`SetRectEmpty`,`SetRectRgn`,`SetROP2`,`SetScrollInfo`}
    WordLists[9][7][1372..1375] = {`SetScrollPos`,`SetScrollRange`,`SetSecurityDescriptorDacl`,`SetSecurityDescriptorGroup`}
    WordLists[9][7][1376..1379] = {`SetSecurityDescriptorOwner`,`SetSecurityDescriptorSacl`,`SetServiceBits`,`SetServiceObjectSecurity`}
    WordLists[9][7][1380..1385] = {`SetServiceStatus`,`SetStdHandle`,`SetStretchBltMode`,`SetSysColors`,`SetSystemCursor`,`SetSystemPaletteUse`}
    WordLists[9][7][1386..1390] = {`SetSystemPowerState`,`SetSystemTime`,`SetSystemTimeAdjustment`,`SetTapeParameters`,`SetTapePosition`}
    WordLists[9][7][1391..1395] = {`SetTextAlign`,`SetTextCharacterExtra`,`SetTextColor`,`SetTextJustification`,`SetThreadAffinityMask`}
    WordLists[9][7][1396..1401] = {`SetThreadContext`,`SetThreadDesktop`,`SetThreadLocale`,`SetThreadPriority`,`SetThreadToken`,`SetTimer`}
    WordLists[9][7][1402..1405] = {`SetTimeZoneInformation`,`SetTokenInformation`,`SetUnhandledExceptionFilter`,`SetupComm`}
    WordLists[9][7][1406..1410] = {`SetUserObjectInformation`,`SetUserObjectSecurity`,`SetViewportExtEx`,`SetViewportOrgEx`,`SetVolumeLabel`}
    WordLists[9][7][1411..1416] = {`SetWindowContextHelpId`,`SetWindowExtEx`,`SetWindowLong`,`SetWindowOrgEx`,`SetWindowPlacement`,`SetWindowPos`}
    WordLists[9][7][1417..1422] = {`SetWindowRgn`,`SetWindowsHook`,`SetWindowsHookEx`,`SetWindowText`,`SetWindowWord`,`SetWinMetaFileBits`}
    WordLists[9][7][1423..1428] = {`SetWorldTransform`,`SHAppBarMessage`,`Shell_NotifyIcon`,`ShellAbout`,`ShellExecute`,`ShellExecuteA`}
    WordLists[9][7][1429..1434] = {`SHFileOperation`,`SHFreeNameMappings`,`SHGetFileInfo`,`SHGetNewLinkInfo`,`ShowCaret`,`ShowCursor`}
    WordLists[9][7][1435..1441] = {`ShowOwnedPopups`,`ShowScrollBar`,`ShowWindow`,`ShowWindowAsync`,`SizeofResource`,`Sleep`,`SleepEx`}
    WordLists[9][7][1442..1448] = {`sndPlaySound`,`socket`,`StartDoc`,`StartDocPrinter`,`StartPage`,`StartPagePrinter`,`StartService`}
    WordLists[9][7][1449..1454] = {`StartServiceCtrlDispatcher`,`StretchBlt`,`StretchDIBits`,`StrokeAndFillPath`,`StrokePath`,`SubtractRect`}
    WordLists[9][7][1455..1460] = {`SuspendThread`,`SwapBuffers`,`SwapMouseButton`,`SwitchDesktop`,`SystemParametersInfo`,`SystemTimeToFileTime`}
    WordLists[9][7][1461..1465] = {`SystemTimeToTzSpecificLocalTime`,`TabbedTextOut`,`TabbedTextOut`,`TerminateProcess`,`TerminateThread`}
    WordLists[9][7][1466..1472] = {`TextOut`,`TileWindows`,`timeBeginPeriod`,`timeEndPeriod`,`timeGetDevCaps`,`timeGetSystemTime`,`timeGetTime`}
    WordLists[9][7][1473..1480] = {`timeKillEvent`,`timeSetEvent`,`TlsAlloc`,`TlsFree`,`TlsGetValue`,`TlsSetValue`,`ToAscii`,`ToAsciiEx`}
    WordLists[9][7][1481..1486] = {`ToUnicode`,`TrackPopupMenu`,`TrackPopupMenuEx`,`TransactNamedPipe`,`TransactNamedPipe`,`TranslateAccelerator`}
    WordLists[9][7][1487..1491] = {`TranslateCharsetInfo`,`TranslateMDISysAccel`,`TranslateMessage`,`TransmitCommChar`,`TransmitFile`}
    WordLists[9][7][1492..1496] = {`UnhandledExceptionFilter`,`UnhookWindowsHook`,`UnhookWindowsHookEx`,`UnionRect`,`UnloadKeyboardLayout`}
    WordLists[9][7][1497..1502] = {`UnlockFile`,`UnlockFileEx`,`UnlockServiceDatabase`,`UnmapViewOfFile`,`UnpackDDElParam`,`UnrealizeObject`}
    WordLists[9][7][1503..1508] = {`UnregisterClass`,`UnregisterHotKey`,`UpdateColors`,`UpdateResource`,`UpdateWindow`,`ValidateRect`}
    WordLists[9][7][1509..1515] = {`ValidateRgn`,`VerFindFile`,`VerInstallFile`,`VerLanguageName`,`VerQueryValue`,`VirtualAlloc`,`VirtualFree`}
    WordLists[9][7][1516..1522] = {`VirtualLock`,`VirtualProtect`,`VirtualProtectEx`,`VirtualQuery`,`VirtualQueryEx`,`VirtualUnlock`,`VkKeyScan`}
    WordLists[9][7][1523..1527] = {`VkKeyScanEx`,`WaitCommEvent`,`WaitForInputIdle`,`WaitForMultipleObjects`,`WaitForMultipleObjectsEx`}
    WordLists[9][7][1528..1532] = {`WaitForPrinterChange`,`WaitForSingleObject`,`WaitForSingleObjectEx`,`WaitMessage`,`WaitNamedPipe`}
    WordLists[9][7][1533..1538] = {`waveInAddBuffer`,`waveInClose`,`waveInGetDevCaps`,`waveInGetErrorText`,`waveInGetID`,`waveInGetNumDevs`}
    WordLists[9][7][1539..1544] = {`waveInGetPosition`,`waveInMessage`,`waveInOpen`,`waveInPrepareHeader`,`waveInReset`,`waveInStart`}
    WordLists[9][7][1545..1549] = {`waveInStop`,`waveInUnprepareHeader`,`waveOutBreakLoop`,`waveOutClose`,`waveOutGetDevCaps`}
    WordLists[9][7][1550..1554] = {`waveOutGetErrorText`,`waveOutGetID`,`waveOutGetNumDevs`,`waveOutGetPitch`,`waveOutGetPlaybackRate`}
    WordLists[9][7][1555..1560] = {`waveOutGetPosition`,`waveOutGetVolume`,`waveOutMessage`,`waveOutOpen`,`waveOutPause`,`waveOutPrepareHeader`}
    WordLists[9][7][1561..1565] = {`waveOutReset`,`waveOutRestart`,`waveOutSetPitch`,`waveOutSetPlaybackRate`,`waveOutSetVolume`}
    WordLists[9][7][1566..1571] = {`waveOutUnprepareHeader`,`waveOutWrite`,`WideCharToMultiByte`,`WidenPath`,`WindowFromDC`,`WindowFromPoint`}
    WordLists[9][7][1572..1577] = {`WinExec`,`WinExecError`,`WinHelp`,`WNetAddConnection`,`WNetAddConnection2`,`WNetAddConnection2`}
    WordLists[9][7][1578..1582] = {`WNetCancelConnection`,`WNetCancelConnection2`,`WNetCloseEnum`,`WNetConnectionDialog`,`WNetDisconnectDialog`}
    WordLists[9][7][1583..1587] = {`WNetEnumResource`,`WNetEnumResource`,`WNetGetConnection`,`WNetGetLastError`,`WNetGetUniversalName`}
    WordLists[9][7][1588..1593] = {`WNetGetUser`,`WNetOpenEnum`,`WNetOpenEnum`,`WriteConsole`,`WriteConsoleOutput`,`WriteConsoleOutputAttribute`}
    WordLists[9][7][1594..1598] = {`WriteConsoleOutputCharacter`,`WriteFile`,`WriteFileEx`,`WritePrinter`,`WritePrivateProfileSection`}
    WordLists[9][7][1599..1603] = {`WritePrivateProfileString`,`WriteProcessMemory`,`WriteProfileSection`,`WriteProfileString`,`WriteTapemark`}
    WordLists[9][7][1604..1608] = {`WSAAsyncSelect`,`WSACleanup`,`WSAStartup`,`wsprintf`,`ZeroMemory`}
    WordLists[9][8][1..6] = {`CREATE_ALWAYS`,`CREATE_NEW`,`FALSE`,`FILE_ACCESS_EXECUTE`,`FILE_ACCESS_READ`,`FILE_ACCESS_WRITE`}
    WordLists[9][8][7..11] = {`FILE_ATTRIBUTE_NORMAL`,`FILE_BEGIN`,`FILE_CREATE_ALWAYS`,`FILE_END`,`FILE_FLAG_RANDOM_ACCESS`}
    WordLists[9][8][12..17] = {`FILE_OPEN_ALWAYS`,`FILE_OPEN_EXISTING`,`FILE_SHARE_READ`,`FILE_SHARE_WRITE`,`GENERIC_EXECUTE`,`GENERIC_READ`}
    WordLists[9][8][18..22] = {`GENERIC_WRITE`,`GMEM_MOVEABLE`,`GMEM_ZEROINIT`,`HEAP_GENERATE_EXCEPTIONS`,`HEAP_REALLOC_IN_PLACE_ONLY`}
    WordLists[9][8][23..32] = {`HEAP_ZERO_MEMORY`,`IDABORT`,`IDCANCEL`,`IDCLOSE`,`IDHELP`,`IDIGNORE`,`IDNO`,`IDOK`,`IDRETRY`,`IDYES`}
    WordLists[9][8][33..38] = {`INVALID_HANDLE_VALUE`,`Linux`,`MB_ABORTRETRYIGNORE`,`MB_APPLMODAL`,`MB_DEFAULT_DESKTOP_ONLY`,`MB_DEFBUTTON1`}
    WordLists[9][8][39..44] = {`MB_DEFBUTTON2`,`MB_DEFBUTTON3`,`MB_DEFBUTTON4`,`MB_HELP`,`MB_ICONASTERISK`,`MB_ICONERROR`}
    WordLists[9][8][45..50] = {`MB_ICONEXCLAMATION`,`MB_ICONHAND`,`MB_ICONINFORMATION`,`MB_ICONQUESTION`,`MB_ICONSTOP`,`MB_ICONWARNING`}
    WordLists[9][8][51..57] = {`MB_NOFOCUS`,`MB_OK`,`MB_OKCANCEL`,`MB_RETRYCANCEL`,`MB_RIGHT`,`MB_RTLREADING`,`MB_SERVICE_NOTIFICATION`}
    WordLists[9][8][58..64] = {`MB_SETFOREGROUND`,`MB_SYSTEMMODAL`,`MB_TASKMODAL`,`MB_TOPMOST`,`MB_USERICON`,`MB_YESNO`,`MB_YESNOCANCEL`}
    WordLists[9][8][65..72] = {`Menuet`,`NULL`,`OPEN_ALWAYS`,`OPEN_EXISTING`,`STD_INPUT_HANDLE`,`STD_OUTPUT_HANDLE`,`STD_ERROR_HANDLE`,`TRUE`}
    WordLists[9][8][73..79] = {`TRUNCATE_EXISTING`,`Windows`,`WM_GETTEXT`,`WM_GETTEXTLENGTH`,`WM_SETTEXT`,`WS_BORDER`,`WS_CAPTION`}
    WordLists[9][8][80..87] = {`WS_DISABLED`,`WS_MINIMIZEBOX`,`WS_POPUP`,`WS_SYSMENU`,`WS_TABSTOP`,`WS_VISIBLE`,`WS_VSCROLL`,`AllocSeq`}
    WordLists[9][8][88..96] = {`AllocStr`,`AllocFlt`,`bytes_count`,`CatWrk`,`character`,`dealloc`,`FltWrk`,`FltWrk64`,`freeConsole`}
    WordLists[9][8][97..107] = {`HeapFail`,`hHeap`,`initConsole`,`LoadFlt`,`puthex32`,`puthex32a`,`puts1`,`putsint`,`stdin`,`stdout`,`stderr`}
    WordLists[9][8][108..117] = {`StoreFlt`,`totalloc`,`totfreed`,`wait_key`,`clear_debug`,`clrdbg`,`crashrtn`,`ebp0`,`ebpreal`,`ebpsave`}
    WordLists[9][8][118..127] = {`freeStrings`,`h4`,`HiCh`,`loadedia`,`LowCh`,`lpBuffer`,`lpEventsRead`,`makeFrame`,`MakeString`,`mslist`}
    WordLists[9][8][128..137] = {`newVSB`,`profileon`,`ptab`,`realline`,`traceon`,`tracertn`,`b_addr`,`b_addr_STPtr`,`b_bind`,`b_file_align`}
    WordLists[9][8][138..146] = {`b_image`,`b_image_size`,`b_infn`,`b_insize`,`b_nSections`,`b_optable`,`b_pad`,`b_pe_image_size`,`b_Rptr`}
    WordLists[9][8][147..155] = {`b_save4`,`b_sect1`,`b_section_align`,`b_section_size`,`b_size`,`b_STptr`,`b_Tptr`,`b_virtual_size`,`c_addr`}
    WordLists[9][8][156..168] = {`calcsize`,`clist`,`dend`,`dlist`,`dump`,`hwsb`,`hwsl`,`hwst`,`lenst`,`ofile`,`old_st_len`,`opskipraw`,`rlist`}
    WordLists[9][8][169..171] = {`straw`,`vmap`,`vmap2`}
    WordLists[10][1][1..13] = {`and`,`andalso`,`as`,`As`,`byref`,`Byref`,`byval`,`cls`,`const`,`Const`,`continue`,`Continue`,`data`}
    WordLists[10][1][14..28] = {`declare`,`dim`,`Dim`,`do`,`else`,`end`,`End`,`exit`,`Exit`,`for`,`For`,`function`,`Function`,`if`,`If`}
    WordLists[10][1][29..42] = {`inkey`,`Inkey`,`loop`,`mod`,`next`,`Next`,`not`,`Not`,`or`,`Or`,`orelse`,`OrElse`,`preserve`,`Preserve`}
    WordLists[10][1][43..55] = {`Randomize`,`read`,`redim`,`ReDim`,`return`,`Return`,`rset`,`RSet`,`shared`,`Shared`,`shl`,`shr`,`step`}
    WordLists[10][1][56..71] = {`Step`,`sub`,`Sub`,`swap`,`then`,`Then`,`this`,`to`,`To`,`type`,`until`,`using`,`Using`,`wend`,`Wend`,`while`}
    WordLists[10][1][72..82] = {`While`,`width`,`with`,`Once`,`#Define`,`#EndIf`,`#EndMacro`,`#endmacro`,`#Include`,`#Ifndef`,`#Macro`}
    WordLists[10][1][83] = `#macro`
    WordLists[10][2][1..13] = {`byte`,`Byte`,`double`,`integer`,`Integer`,`long`,`Long`,`longint`,`LongInt`,`string`,`String`,`ptr`,`Ptr`}
    WordLists[10][2][14..22] = {`uinteger`,`UInteger`,`ulong`,`ULong`,`ulongint`,`ULongInt`,`Mpz_ptr`,`__mpz_struct`,`__gmp_randstate_struct`}
    WordLists[10][2][23] = `ZString`
    WordLists[10][3][1..11] = {`abs`,`allocate`,`Allocate`,`cdbl`,`clngint`,`culngint`,`CULngInt`,`deallocate`,`DeAllocate`,`iif`,`inkey`}
    WordLists[10][3][12..27] = {`InKey`,`int`,`Int`,`len`,`Len`,`log`,`Log`,`print`,`Print`,`rnd`,`Rnd`,`sleep`,`Sleep`,`space`,`Space`,`str`}
    WordLists[10][3][28] = `Str`
    WordLists[10][4] = {`ErM`,`Timer`,`TRUE`,`True`,`FALSE`}
    WordLists[10][5][1..7] = {`gmp_randclear`,`gmp_randinit_mt`,`gmp_randseed`,`mpz_add_ui`,`mpz_clear`,`mpz_cmp`,`mpz_cmp_ui`}
    WordLists[10][5][8..15] = {`mpz_fdiv_q_2exp`,`mpz_get_str`,`Mpz_get_str`,`mpz_init`,`Mpz_init`,`mpz_mul_2exp`,`mpz_powm`,`mpz_powm_ui`}
    WordLists[10][5][16..23] = {`mpz_set`,`mpz_set_ui`,`mpz_set_str`,`Mpz_set_str`,`mpz_sub_ui`,`mpz_tstbit`,`mpz_urandomm`,`Sqr`}
    WordLists[11][1] = {`and`,`do`,`else`,`fun`,`if`,`in`,`inline`,`let`,`match`,`of`,`private`,`rec`,`then`,`type`,`while`,`with`}
    WordLists[11][2..3] = {{`false`},{`bigint`,`float`,`int32`,`int64`,`uint8`,`uint32`}}
    WordLists[11][4][1..12] = {`cache`,`create`,`DateTime`,`fill`,`filter`,`forall`,`item`,`length`,`Length`,`Now`,`pairwise`,`takeWhile`}
    WordLists[11][4][13..16] = {`Ticks`,`unfold`,`Value`,`zeroCreate`}
    WordLists[11][5..6] = {{`Array`,`int`,`None`,`Seq`,`Some`,`sqrt`,`System`},{`illegalthings`}}
    WordLists[12][1][1..13] = {`break`,`case`,`const`,`continue`,`default`,`else`,`fallthrough`,`for`,`func`,`if`,`import`,`package`,`range`}
    WordLists[12][1][14..17] = {`switch`,`type`,`return`,`var`}
    WordLists[12][2][1..13] = {`bool`,`byte`,`error`,`float32`,`float64`,`int`,`Int`,`int32`,`int64`,`interface`,`string`,`struct`,`uint`}
    WordLists[12][2][14..15] = {`uint64`,`Word`}
    WordLists[12][3] = {`false`,`nil`,`true`}
    WordLists[12][4][1..13] = {`Add`,`append`,`Args`,`Atoi`,`BitLen`,`Cmp`,`CombinedOutput`,`Command`,`copy`,`Cos`,`Errorf`,`Exit`,`Exp`}
    WordLists[12][4][14..27] = {`Fatal`,`Fatalf`,`Float64`,`Hostname`,`Inf`,`IsInf`,`Itoa`,`len`,`make`,`map`,`MaxUint64`,`Mul`,`NArg`,`new`}
    WordLists[12][4][28..40] = {`New`,`NewInt`,`Now`,`panic`,`Parse`,`Pi`,`Pow`,`Print`,`Printf`,`Println`,`ProbablyPrime`,`Quo`,`QuoRem`}
    WordLists[12][4][41..53] = {`Rsh`,`Run`,`Seed`,`Set`,`SetInt64`,`SetString`,`SetUint64`,`Shuffle`,`Sin`,`Sprintf`,`Sqrt`,`String`,`Sub`}
    WordLists[12][4][54..59] = {`ToLower`,`Uint`,`Uint64`,`Unix`,`UnixNano`,`Usage`}
    WordLists[12][5] = {`big`,`errors`,`exec`,`flag`,`fmt`,`log`,`math`,`os`,`rand`,`strconv`,`strings`,`time`}
    WordLists[13][1][1..14] = {`/`,`>`,`<php`,`</php`,`<a`,`</a`,`<A`,`</A`,`<address`,`</address`,`<ADDRESS`,`</ADDRESS`,`&alpha`,`&beta`}
    WordLists[13][1][15..27] = {`&gamma`,`&delta`,`&Delta`,`&epsilon`,`&zeta`,`&eta`,`&theta`,`&iota`,`&kappa`,`&lambda`,`&mu`,`&nu`,`&xi`}
    WordLists[13][1][28..40] = {`&omicron`,`&ominus`,`&pi`,`&rho`,`&sigma`,`&tau`,`&upsilon`,`&phi`,`&chi`,`&psi`,`&omega`,`&not`,`&middot`}
    WordLists[13][1][41..53] = {`&rarr`,`&rArr`,`&hArr`,`&forall`,`&part`,`&exist`,`&empty`,`&nabla`,`&isin`,`&notin`,`&prod`,`&sum`,`&radic`}
    WordLists[13][1][54..68] = {`&infin`,`&and`,`&or`,`&cap`,`&cup`,`&int`,`&asymp`,`&ne`,`&equiv`,`&le`,`&ge`,`&sub`,`&sup`,`&deg`,`&times`}
    WordLists[13][1][69..79] = {`&lfloor`,`&rfloor`,`&lceil`,`&rceil`,`&varepsilon`,`&vartheta`,`&varpi`,`&varrho`,`&varphi`,`&plusmn`,`&amp`}
    WordLists[13][1][80..92] = {`&apos`,`&setminus`,`<applet`,`</applet`,`<APPLET`,`</APPLET`,`<area`,`<AREA`,`<b`,`</b`,`<B`,`</B`,`<blink`}
    WordLists[13][1][93..101] = {`</blink`,`<BLINK`,`</BLINK`,`<blockquote`,`</blockquote`,`<BLOCKQUOTE`,`</BLOCKQUOTE`,`<body`,`</body`}
    WordLists[13][1][102..112] = {`<BODY`,`</BODY`,`<br`,`<BR`,`<button`,`</button`,`<BUTTON`,`</BUTTON`,`<canvas`,`</canvas`,`<CANVAS`}
    WordLists[13][1][113..122] = {`</CANVAS`,`<caption`,`</caption`,`<CAPTION`,`</CAPTION`,`<center`,`</center`,`<CENTER`,`</CENTER`,`<cite`}
    WordLists[13][1][123..133] = {`</cite`,`<CITE`,`</CITE`,`<code`,`</code`,`<CODE`,`</CODE`,`<col`,`</col`,`<colgroup`,`</colgroup`}
    WordLists[13][1][134..145] = {`<datalist`,`</datalist`,`<dd`,`</dd`,`<DD`,`</DD`,`<del`,`</del`,`<details`,`</details`,`<dfn`,`</dfn`}
    WordLists[13][1][146..161] = {`<DFN`,`</DFN`,`<div`,`</div`,`<DIV`,`</DIV`,`<dl`,`</dl`,`<DL`,`</DL`,`<dt`,`</dt`,`<DT`,`</DT`,`<em`,`</em`}
    WordLists[13][1][162..173] = {`<EM`,`</EM`,`<font`,`</font`,`<FONT`,`</FONT`,`<footer`,`</footer`,`<form`,`</form`,`<FORM`,`</FORM`}
    WordLists[13][1][174..187] = {`&frac13`,`&frac15`,`&frac18`,`&frac23`,`&frasl`,`&gt`,`&GT`,`<h1`,`</h1`,`<H1`,`</H1`,`<h2`,`</h2`,`<H2`}
    WordLists[13][1][188..203] = {`</H2`,`<h3`,`</h3`,`<H3`,`</H3`,`<h4`,`</h4`,`<H4`,`</H4`,`<h5`,`</h5`,`<H5`,`</H5`,`<head`,`</head`,`<HEAD`}
    WordLists[13][1][204..215] = {`</HEAD`,`<header`,`</header`,`<HEADER`,`</HEADER`,`<hr`,`</hr`,`<HR`,`</HR`,`<html`,`</html`,`<HTML`}
    WordLists[13][1][216..228] = {`</HTML`,`<i`,`</i`,`<I`,`</I`,`<iframe`,`</iframe`,`<IFRAME`,`</IFRAME`,`<img`,`<IMG`,`<input`,`</input`}
    WordLists[13][1][229..241] = {`<INPUT`,`</INPUT`,`<kbd`,`</kbd`,`<KBD`,`</KBD`,`<label`,`</label`,`<LABEL`,`</LABEL`,`<li`,`</li`,`<LI`}
    WordLists[13][1][242..255] = {`</LI`,`<link`,`</link`,`<LINK`,`</LINK`,`&ldquo`,`&lsquo`,`&lt`,`&LT`,`<main`,`</main`,`<map`,`</map`,`<MAP`}
    WordLists[13][1][256..268] = {`</MAP`,`<meta`,`</meta`,`<META`,`</META`,`<meter`,`</meter`,`<nav`,`</nav`,`<nobr`,`</nobr`,`<NOBR`,`</NOBR`}
    WordLists[13][1][269..278] = {`<noscript`,`</noscript`,`<NOSCRIPT`,`</NOSCRIPT`,`<object`,`</object`,`<OBJECT`,`</OBJECT`,`<ol`,`</ol`}
    WordLists[13][1][279..291] = {`<OL`,`</OL`,`&ouml`,`<option`,`</option`,`<OPTION`,`</OPTION`,`<output`,`</output`,`<p`,`</p`,`<P`,`</P`}
    WordLists[13][1][292..303] = {`<param`,`<PARAM`,`&pound`,`<pre`,`</pre`,`<PRE`,`</PRE`,`<progress`,`</progress`,`&quot`,`&rdquo`,`&rsquo`}
    WordLists[13][1][304..314] = {`<script`,`</script`,`<SCRIPT`,`</SCRIPT`,`<samp`,`</samp`,`<SAMP`,`</SAMP`,`<section`,`</section`,`<SECTION`}
    WordLists[13][1][315..325] = {`</SECTION`,`<select`,`</select`,`<SELECT`,`</SELECT`,`<small`,`</small`,`<SMALL`,`</SMALL`,`<span`,`</span`}
    WordLists[13][1][326..336] = {`<SPAN`,`</SPAN`,`<strong`,`</strong`,`<STRONG`,`</STRONG`,`<style`,`</style`,`<STYLE`,`</STYLE`,`<sub`}
    WordLists[13][1][337..348] = {`</sub`,`<SUB`,`</SUB`,`<summary`,`</summary`,`<sup`,`</sup`,`<SUP`,`</SUP`,`<table`,`</table`,`<TABLE`}
    WordLists[13][1][349..359] = {`</TABLE`,`<tbody`,`</tbody`,`<TBODY`,`</TBODY`,`<td`,`</td`,`<TD`,`</TD`,`<textarea`,`</textarea`}
    WordLists[13][1][360..371] = {`<TEXTAREA`,`</TEXTAREA`,`<tfoot`,`</tfoot`,`<TFOOT`,`</TFOOT`,`<th`,`</th`,`<TH`,`</TD`,`<thead`,`</thead`}
    WordLists[13][1][372..385] = {`<THEAD`,`</THEAD`,`tilde`,`<title`,`</title`,`<TITLE`,`</TITLE`,`<tr`,`</tr`,`<TR`,`</TR`,`<tt`,`</tt`,`<TT`}
    WordLists[13][1][386..401] = {`</TT`,`<u`,`</u`,`<U`,`</U`,`<ul`,`</ul`,`<UL`,`</UL`,`<var`,`</var`,`<VAR`,`</VAR`,`<wbr`,`<WBR`,`DOCTYPE`}
    WordLists[13][1][402..407] = {`doctype`,`PUBLIC`,`&nbsp`,`&zwj`,`>`,`/>`}
    WordLists[13][2][1..12] = {`accept`,`action`,`ACTION`,`align`,`ALIGN`,`alink`,`ALINK`,`allow`,`allowfullscreen`,`alt`,`ALT`,`background`}
    WordLists[13][2][13..21] = {`BACKGROUND`,`bgcolor`,`BGCOLOR`,`border`,`BORDER`,`bottom`,`BOTTOM`,`cellpadding`,`CELLPADDING`}
    WordLists[13][2][22..32] = {`cellspacing`,`CELLSPACING`,`center`,`CENTER`,`charset`,`class`,`CLASS`,`color`,`cols`,`colspan`,`COLSPAN`}
    WordLists[13][2][33..43] = {`compact`,`COMPACT`,`content`,`CONTENT`,`contenteditable`,`coords`,`COORDS`,`crossorigin`,`face`,`FACE`,`for`}
    WordLists[13][2][44..54] = {`frameborder`,`height`,`HEIGHT`,`href`,`HREF`,`http-equiv`,`HTTP-EQUIV`,`id`,`ID`,`integrity`,`lang`}
    WordLists[13][2][55..64] = {`language`,`LANGUAGE`,`left`,`LEFT`,`leftmargin`,`LEFTMARGIN`,`link`,`LINK`,`list`,`marginheight`}
    WordLists[13][2][65..74] = {`MARGINHEIGHT`,`marginwidth`,`MARGINWIDTH`,`max`,`maxlength`,`MAXLENGTH`,`media`,`MEDIA`,`method`,`METHOD`}
    WordLists[13][2][75..86] = {`middle`,`MIDDLE`,`min`,`name`,`NAME`,`noshade`,`NOSHADE`,`nowrap`,`NOWRAP`,`onchange`,`onLoad`,`onload`}
    WordLists[13][2][87..98] = {`onMouseOut`,`onMouseOver`,`placeholder`,`rel`,`REL`,`rev`,`REV`,`right`,`RIGHT`,`rows`,`rowspan`,`ROWSPAN`}
    WordLists[13][2][99..112] = {`size`,`SIZE`,`span`,`src`,`SRC`,`step`,`style`,`text`,`TEXT`,`title`,`TITLE`,`top`,`TOP`,`topmargin`}
    WordLists[13][2][113..125] = {`TOPMARGIN`,`type`,`TYPE`,`usemap`,`USEMAP`,`valign`,`VALIGN`,`value`,`VALUE`,`vlink`,`VLINK`,`width`,`WIDTH`}
    WordLists[13][2][126] = `xmlns`
    WordLists[13][3][1..12] = {`if`,`else`,`int`,`clearstatcache`,`echo`,`fclose`,`fflush`,`fgets`,`file_exists`,`flock`,`fopen`,`ftell`}
    WordLists[13][3][13..21] = {`ftruncate`,`fwrite`,`ignore_user_abort`,`print`,`rewind`,`false`,`LOCK_EX`,`LOCK_UN`,`true`}
    WordLists[14][1..3] = {{`continue`,`else`,`for`,`function`,`if`,`return`,`var`},{`false`,`true`},{}}
    WordLists[14][4] = {`document`,`getElementById`,`toNumber`,`toString`}
    WordLists[15][1][1..13] = {`arguments`,`async`,`await`,`break`,`case`,`catch`,`class`,`console`,`default`,`delete`,`do`,`else`,`exit`}
    WordLists[15][1][14..26] = {`export`,`for`,`function`,`if`,`in`,`instanceof`,`new`,`process`,`return`,`switch`,`this`,`throw`,`try`}
    WordLists[15][1][27..28] = {`typeof`,`while`}
    WordLists[15][2][1..10] = {`false`,`null`,`NULL`,`true`,`undefined`,`ANY_QUEUE`,`ASCENDING`,`DESCENDING`,`CD_AMBER`,`CD_BASE_CENTER`}
    WordLists[15][2][11..18] = {`CD_BASE_LEFT`,`CD_BASE_RIGHT`,`CD_BLACK`,`CD_BLUE`,`CD_BOLD`,`CD_BOLD_ITALIC`,`CD_BOX`,`CD_CENTER`}
    WordLists[15][2][19..25] = {`CD_CIRCLE`,`CD_CLOSED_LINES`,`CD_CONTINUOUS`,`CD_CUSTOM`,`CD_CYAN`,`CD_DARK_BLUE`,`CD_DARK_CYAN`}
    WordLists[15][2][26..32] = {`CD_DARK_GRAY`,`CD_DARK_GREY`,`CD_DARK_GREEN`,`CD_DARK_MAGENTA`,`CD_DARK_RED`,`CD_DARK_YELLOW`,`CD_DASH_DOT`}
    WordLists[15][2][33..40] = {`CD_DASH_DOT_DOT`,`CD_DASHED`,`CD_DBUFFER`,`CD_DEG2RAD`,`CD_DIAMOND`,`CD_DOTTED`,`CD_EAST`,`CD_EVENODD`}
    WordLists[15][2][41..49] = {`CD_FILL`,`CD_GL`,`CD_GRAY`,`CD_GREY`,`CD_GREEN`,`CD_HATCH`,`CD_HOLLOW`,`CD_HOLLOW_BOX`,`CD_HOLLOW_CIRCLE`}
    WordLists[15][2][50..56] = {`CD_HOLLOW_DIAMOND`,`CD_INDIGO`,`CD_ITALIC`,`CD_IUP`,`CD_LIGHT_BLUE`,`CD_LIGHT_GRAY`,`CD_LIGHT_GREY`}
    WordLists[15][2][57..63] = {`CD_LIGHT_GREEN`,`CD_LIGHT_PARCHMENT`,`CD_MAGENTA`,`CD_NAVY`,`CD_NORTH`,`CD_NORTH_EAST`,`CD_NORTH_WEST`}
    WordLists[15][2][64..71] = {`CD_OLIVE`,`CD_OPEN_LINES`,`CD_ORANGE`,`CD_PARCHMENT`,`CD_PATTERN`,`CD_PLAIN`,`CD_PLUS`,`CD_PURPLE`}
    WordLists[15][2][72..80] = {`CD_QUERY`,`CD_RAD2DEG`,`CD_RED`,`CD_SILVER`,`CD_SOLID`,`CD_SOUTH`,`CD_SOUTH_EAST`,`CD_SOUTH_WEST`,`CD_STAR`}
    WordLists[15][2][81..89] = {`CD_STIPPLE`,`CD_STRIKEOUT`,`CD_UNDERLINE`,`CD_VIOLET`,`CD_WEST`,`CD_WHITE`,`CD_WINDING`,`CD_X`,`CD_YELLOW`}
    WordLists[15][2][90..99] = {`D_NAME`,`D_ATTRIBUTES`,`D_SIZE`,`D_YEAR`,`D_MONTH`,`D_DAY`,`D_HOUR`,`D_MINUTE`,`D_SECOND`,`D_CREATION`}
    WordLists[15][2][100..108] = {`D_LASTACCESS`,`D_MODIFICATION`,`DT_DAY`,`DT_DOW`,`DT_DOY`,`DT_GMT`,`DT_HOUR`,`DT_MINUTE`,`DT_MONTH`}
    WordLists[15][2][109..119] = {`DT_MSEC`,`DT_SECOND`,`DT_YEAR`,`E_CODE`,`E_ADDR`,`E_LINE`,`E_RTN`,`E_NAME`,`E_FILE`,`E_PATH`,`E_USER`}
    WordLists[15][2][120..125] = {`EULER`,`FIFO_QUEUE`,`GL_ARRAY_BUFFER`,`GL_CLAMP_TO_BORDER`,`GL_CLAMP_TO_EDGE`,`GL_CLAMP`}
    WordLists[15][2][126..131] = {`GL_COLOR_BUFFER_BIT`,`GL_COMPILE_STATUS`,`GL_CULL_FACE`,`GL_DEPTH_BUFFER_BIT`,`GL_DEPTH_TEST`,`GL_FLOAT`}
    WordLists[15][2][132..138] = {`GL_FRAGMENT_SHADER`,`GL_LINEAR`,`GL_LINK_STATUS`,`GL_MODELVIEW`,`GL_NEAREST`,`GL_NO_ERROR`,`GL_PROJECTION`}
    WordLists[15][2][139..144] = {`GL_REPEAT`,`GL_RGB`,`GL_RGBA`,`GL_STATIC_DRAW`,`GL_TEXTURE_2D`,`GL_TEXTURE_MAG_FILTER`}
    WordLists[15][2][145..149] = {`GL_TEXTURE_MIN_FILTER`,`GL_TEXTURE_WRAP_S`,`GL_TEXTURE_WRAP_T`,`GL_TRIANGLES`,`GL_UNSIGNED_BYTE`}
    WordLists[15][2][150..157] = {`GL_VERTEX_SHADER`,`HSIEH30`,`INVLN10`,`IUP_BUTTON1`,`IUP_BUTTON2`,`IUP_BUTTON3`,`IUP_CENTER`,`IUP_CLOSE`}
    WordLists[15][2][158..165] = {`IUP_CONTINUE`,`IUP_DEFAULT`,`IUP_IGNORE`,`IUP_MASK_INT`,`IUP_MASK_UINT`,`IUP_MOUSEPOS`,`JAVASCRIPT`,`JS`}
    WordLists[15][2][166..180] = {`K_BS`,`K_CR`,`K_DEL`,`K_DOWN`,`K_END`,`K_ESC`,`K_F1`,`K_F2`,`K_F3`,`K_F4`,`K_F5`,`K_F6`,`K_F7`,`K_F8`,`K_F9`}
    WordLists[15][2][181..192] = {`K_F10`,`K_F11`,`K_F12`,`K_HOME`,`K_INS`,`K_F5`,`K_LEFT`,`K_MIDDLE`,`K_PGDN`,`K_PGUP`,`K_RIGHT`,`K_SP`}
    WordLists[15][2][193..209] = {`K_TAB`,`K_UP`,`K_a`,`K_b`,`K_c`,`K_d`,`K_e`,`K_f`,`K_g`,`K_h`,`K_i`,`K_j`,`K_p`,`K_r`,`K_s`,`K_cA`,`K_cC`}
    WordLists[15][2][210..220] = {`K_cD`,`LIFO_QUEUE`,`LINUX`,`MAX_HEAP`,`MIN_HEAP`,`MPFR_RNDN`,`MPFR$T`,`MPFR$N`,`MPFR$D`,`MPFR$E`,`MPFR$R`}
    WordLists[15][2][221..231] = {`MPFR$P`,`MPQ$N`,`MPQ$D`,`MPZ$B`,`NORMAL_ORDER`,`PI`,`pp_Ascii`,`pp_Brkt`,`pp_Date`,`pp_File`,`pp_FltFmt`}
    WordLists[15][2][232..240] = {`pp_Indent`,`pp_IntCh`,`pp_IntFmt`,`pp_Maxlen`,`pp_Nest`,`pp_Pause`,`pp_Q22`,`pp_StrFmt`,`REVERSE_ORDER`}
    WordLists[15][2][241..247] = {`SLASH`,`TEST_ABORT`,`TEST_CRASH`,`TEST_PAUSE`,`TEST_PAUSE_FAIL`,`TEST_QUIET`,`TEST_SHOW_ALL`}
    WordLists[15][2][248..256] = {`TEST_SHOW_FAILED`,`TEST_SUMMARY`,`WEB`,`WINDOWS`,`VC_COLOR`,`VC_MODE`,`VC_LINES`,`VC_COLUMNS`,`VC_XPIXELS`}
    WordLists[15][2][257..266] = {`VC_YPIXELS`,`VC_NCOLORS`,`VC_PAGES`,`VC_SCRNLINES`,`VC_SCRNCOLS`,`VK_BS`,`VK_CR`,`VK_DEL`,`VK_DOWN`,`VK_ESC`}
    WordLists[15][2][267..279] = {`VK_F1`,`VK_F2`,`VK_F3`,`VK_F4`,`VK_F5`,`VK_F6`,`VK_F7`,`VK_F8`,`VK_F9`,`VK_F10`,`VK_F11`,`VK_F12`,`VK_LEFT`}
    WordLists[15][2][280..288] = {`VK_PGDN`,`VK_PGUP`,`VK_RIGHT`,`VK_UP`,`XPG_CONTINUE`,`XPG_CLOSE`,`XPG_DEFAULT`,`XPG_IGNORE`,`XPG_CURRENT`}
    WordLists[15][2][289..295] = {`XPG_LEFT`,`XPG_RIGHT`,`XPG_CENTER`,`XPG_MOUSEPOS`,`XPG_LEFTPARENT`,`XPG_RIGHTPARENT`,`XPG_CENTERPARENT`}
    WordLists[15][2][296..302] = {`XPG_TOP`,`XPG_TOPPARENT`,`XPG_BOTTOM`,`XPG_BOTTOMPARENT`,`XPG_BLACK`,`XPG_DARK_CYAN`,`XPG_DARK_GREEN`}
    WordLists[15][2][303..309] = {`XPG_DARK_RED`,`XPG_DARK_VIOLET`,`XPG_DARK_YELLOW`,`XPG_OLIVE`,`XPG_DARK_BLUE`,`XPG_NAVY`,`XPG_DARK_PURPLE`}
    WordLists[15][2][310..317] = {`XPG_TEAL`,`XPG_GREY`,`XPG_GRAY`,`XPG_SILVER`,`XPG_SLATE`,`XPG_DARK_GREY`,`XPG_DARK_GRAY`,`XPG_RED`}
    WordLists[15][2][318..325] = {`XPG_GREEN`,`XPG_LIGHT_GREEN`,`XPG_YELLOW`,`XPG_BLUE`,`XPG_MAGENTA`,`XPG_CYAN`,`XPG_INDIGO`,`XPG_LIGHT_GREY`}
    WordLists[15][2][326..331] = {`XPG_LIGHT_GRAY`,`XPG_LIGHT_GREEN`,`XPG_LIGHT_BLUE`,`XPG_LIGHT_PARCHMENT`,`XPG_ORANGE`,`XPG_AMBER`}
    WordLists[15][2][332..338] = {`XPG_PARCHMENT`,`XPG_PURPLE`,`XPG_VIOLET`,`XPG_WHITE`,`XPG_CONTINUOUS`,`XPG_DASHED`,`XPG_DOTTED`}
    WordLists[15][2][339..345] = {`XPG_DASH_DOT`,`XPG_DASH_DOT_DOT`,`XPG_NORMAL`,`XPG_BOLD`,`XPG_ITALIC`,`XPG_BOLDITALIC`,`XPG_FILLED`}
    WordLists[15][2][346..356] = {`XPG_CHORD`,`XPG_SECTOR`,`XPG_N`,`XPG_NW`,`XPG_NE`,`XPG_W`,`XPG_E`,`XPG_SW`,`XPG_SE`,`XPG_S`,`XPG_C`}
    WordLists[15][2][357..364] = {`XPG_NORTH`,`XPG_NORTHWEST`,`XPG_NORTHEAST`,`XPG_WEST`,`XPG_EAST`,`XPG_SOUTHWEST`,`XPG_SOUTHEAST`,`XPG_SOUTH`}
    WordLists[15][2][365..375] = {`XPG_CENTRE`,`XPG_GTK`,`XPG_WINAPI`,`XPG_JS`,`XPG_DEG2RAD`,`XPG_RAD2DEG`,`BLACK`,`BLUE`,`GREEN`,`CYAN`,`RED`}
    WordLists[15][2][376..384] = {`MAGENTA`,`BROWN`,`WHITE`,`GRAY`,`BRIGHT_BLUE`,`BRIGHT_GREEN`,`BRIGHT_CYAN`,`BRIGHT_RED`,`BRIGHT_MAGENTA`}
    WordLists[15][2][385..386] = {`YELLOW`,`BRIGHT_WHITE`}
    WordLists[15][3][1..9] = {`ArrayBuffer`,`BigInt`,`const`,`DataView`,`Date`,`Error`,`let`,`RegExp`,`SpeechSynthesisUtterance`}
    WordLists[15][3][10..12] = {`Uint8Array`,`var`,`static`}
    WordLists[15][4][1..5] = {`.activeElement`,`.actualBoundingBoxLeft`,`.actualBoundingBoxRight`,`.alignItems`,`.AudioContext`}
    WordLists[15][4][6..14] = {`.background`,`.backgroundColor`,`backGround`,`baseURI`,`.body`,`.bottom`,`.button`,`.canvas`,`.charIndex`}
    WordLists[15][4][15..21] = {`.charLength`,`.checked`,`.childElementCount`,`.childNodes`,`.children`,`.classList`,`.className`}
    WordLists[15][4][22..29] = {`.clientHeight`,`.clientWidth`,`.clientX`,`.clientY`,`.clipboardData`,`.color`,`cssText`,`.ctrlKey`}
    WordLists[15][4][30..37] = {`currentScript`,`.currentTarget`,`.currentTime`,`.cursor`,`.data`,`.destination`,`.disabled`,`.display`}
    WordLists[15][4][38..45] = {`document`,`.documentElement`,`.E`,`.event`,`.fillStyle`,`.firstChild`,`.firstElementChild`,`.flex`}
    WordLists[15][4][46..51] = {`.flexDirection`,`.flexGrow`,`.font`,`.fontBoundingBoxAscent`,`.fontBoundingBoxDescent`,`.fontFamily`}
    WordLists[15][4][52..60] = {`.fontSize`,`.fontStyle`,`.fontWeight`,`.for`,`.frequency`,`.fullscreenElement`,`.gain`,`.height`,`.href`}
    WordLists[15][4][61..68] = {`.id`,`.innerHeight`,`.innerHTML`,`.innerText`,`.innerWidth`,`.isConnected`,`.justifyContent`,`.key`}
    WordLists[15][4][69..77] = {`.keyCode`,`.lastElementChild`,`.left`,`.length`,`.lineWidth`,`.localName`,`.location`,`.margin`,`.marginTop`}
    WordLists[15][4][78..85] = {`.marginLeft`,`.marginBottom`,`.marginRight`,`maxWidth`,`.message`,`.name`,`navigator`,`.nextElementSibling`}
    WordLists[15][4][86..93] = {`.nextSibling`,`nodeType`,`.offsetHeight`,`.offsetLeft`,`.offsetWidth`,`.offsetTop`,`onblur`,`onchange`}
    WordLists[15][4][94..102] = {`.onclick`,`oncontextmenu`,`.ondblclick`,`.ondragstart`,`onerror`,`.onfocus`,`oninput`,`.onkeydown`,`onkeyup`}
    WordLists[15][4][103..110] = {`onload`,`.onmousedown`,`.onmousemove`,`.onmouseup`,`onresize`,`onscroll`,`.onselectstart`,`.opacity`}
    WordLists[15][4][111..119] = {`overflow`,`paddingLeft`,`paddingRight`,`.pageX`,`.pageY`,`.parentNode`,`.pattern`,`.PI`,`.position`}
    WordLists[15][4][120..126] = {`.previousElementSibling`,`.rate`,`readyState`,`responseXML`,`.right`,`.scrollHeight`,`.scrollLeft`}
    WordLists[15][4][127..133] = {`.scrollTop`,`.scrollWidth`,`.selected`,`.selectedIndex`,`selectionEnd`,`selectionStart`,`.shiftKey`}
    WordLists[15][4][134..142] = {`.speechSynthesis`,`.src`,`.stack`,`status`,`.strokeStyle`,`.style`,`.target`,`.text`,`.textAlign`}
    WordLists[15][4][143..150] = {`.textBaseline`,`.textContent`,`.textDecoration`,`textOrientation`,`tFoot`,`tHead`,`.title`,`.top`}
    WordLists[15][4][151..157] = {`userAgent`,`.value`,`.valueAsNumber`,`visibility`,`.width`,`window`,`.zIndex`}
    WordLists[15][5][1..10] = {`.abs`,`.accessKey`,`.add`,`.addEventListener`,`alert`,`.appendChild`,`.appName`,`.appVersion`,`arc`,`Array`}
    WordLists[15][5][11..18] = {`asIntN`,`asUintN`,`.atan`,`.attachShader`,`.beginPath`,`.bezierCurveTo`,`bind`,`bindAttribLocation`}
    WordLists[15][5][19..27] = {`.bindBuffer`,`.bindTexture`,`Boolean`,`.bufferData`,`.call`,`charAt`,`charCodeAt`,`.clear`,`.clearColor`}
    WordLists[15][5][28..35] = {`clearInterval`,`clearRect`,`cloneNode`,`close`,`.closePath`,`.codePointAt`,`.compileShader`,`.concat`}
    WordLists[15][5][36..43] = {`.connect`,`constructor`,`.contains`,`.cos`,`.createBuffer`,`.createElement`,`.createElementNS`,`.createGain`}
    WordLists[15][5][44..49] = {`.createImageData`,`.createOscillator`,`.createProgram`,`.createShader`,`.createTextNode`,`.createTexture`}
    WordLists[15][5][50..56] = {`defineProperty`,`.deleteProgram`,`.deleteShader`,`devicePixelRatio`,`.disconnect`,`.drawArrays`,`.drawImage`}
    WordLists[15][5][57..64] = {`.ellipse`,`.enable`,`.enableVertexAttribArray`,`eval`,`.execCommand`,`.exitFullscreen`,`.fill`,`.fillRect`}
    WordLists[15][5][65..73] = {`.fillText`,`filter`,`Float32Array`,`.floor`,`.flush`,`.focus`,`.forEach`,`.from`,`fromCharCode`}
    WordLists[15][5][74..79] = {`.fromCodePoint`,`get`,`.getAttribLocation`,`.getAttribute`,`.getBoundingClientRect`,`getComputedStyle`}
    WordLists[15][5][80..85] = {`.getContext`,`.getDate`,`.getDay`,`.getElementById`,`.getElementsByClassName`,`.getElementsByTagName`}
    WordLists[15][5][86..92] = {`.getError`,`.getFloat32`,`.getFloat64`,`.getFullYear`,`.getHours`,`.getImageData`,`.getMilliseconds`}
    WordLists[15][5][93..98] = {`.getMinutes`,`.getMonth`,`.getProgramInfoLog`,`.getProgramParameter`,`.getSeconds`,`.getShaderInfoLog`}
    WordLists[15][5][99..104] = {`.getShaderParameter`,`getTime`,`.getUniformLocation`,`hasAttribute`,`.hasChildNodes`,`.hasOwnProperty`}
    WordLists[15][5][105..110] = {`.indexOf`,`.insertAdjacentElement`,`.insertAdjacentHTML`,`insertAdjacentText`,`.insertBefore`,`Int32Array`}
    WordLists[15][5][111..118] = {`IntersectionObserver`,`.isArray`,`.isFinite`,`.isInteger`,`.isIntersecting`,`.isNaN`,`isSafeInteger`,`.join`}
    WordLists[15][5][119..129] = {`JSON`,`.keys`,`.lastIndexOf`,`.lineTo`,`.linkProgram`,`localeCompare`,`.log`,`.map`,`Map`,`.match`,`Math`}
    WordLists[15][5][130..139] = {`.max`,`.measureText`,`.min`,`.moveTo`,`.now`,`Number`,`Object`,`.observe`,`.offsetParent`,`open`}
    WordLists[15][5][140..148] = {`parentElement`,`parseFloat`,`parseInt`,`peek`,`.platform`,`.pop`,`.pow`,`.preventDefault`,`prototype`}
    WordLists[15][5][149..154] = {`.propertyIsEnumerable`,`.push`,`.putImageData`,`.quadraticCurveTo`,`.querySelector`,`.querySelectorAll`}
    WordLists[15][5][155..162] = {`readFile`,`rect`,`.reduce`,`.remove`,`.removeAttribute`,`.removeChild`,`.removeEventListener`,`.repeat`}
    WordLists[15][5][163..168] = {`.replace`,`replaceChild`,`.requestAnimationFrame`,`.requestFullscreen`,`.requestIdleCallback`,`require`}
    WordLists[15][5][169..178] = {`.restore`,`reverse`,`.rotate`,`.round`,`.save`,`.select`,`send`,`set`,`.setAttribute`,`.setData`}
    WordLists[15][5][179..185] = {`.setFloat32`,`.setFloat64`,`setInterval`,`.setLineDash`,`setTimeout`,`.setValueAtTime`,`.shaderSource`}
    WordLists[15][5][186..196] = {`.sin`,`.size`,`.slice`,`.sort`,`.speak`,`splice`,`.split`,`.sqrt`,`.start`,`.stop`,`.stopPropagation`}
    WordLists[15][5][197..205] = {`String`,`stringify`,`.stroke`,`.strokeRect`,`.substr`,`.substring`,`.tan`,`.texImage2D`,`.texParameteri`}
    WordLists[15][5][206..213] = {`then`,`.toDataURL`,`.toExponential`,`.toFixed`,`.toggle`,`.toLowerCase`,`toNumber`,`.toPrecision`}
    WordLists[15][5][214..221] = {`.toString`,`.toUpperCase`,`.transform`,`.translate`,`trunc`,`.uniform1f`,`.uniform1i`,`.uniformMatrix4fv`}
    WordLists[15][5][222..229] = {`.unshift`,`.useProgram`,`.userAgent`,`.valueOf`,`.vertexAttribPointer`,`.viewport`,`which`,`XMLHttpRequest`}
    WordLists[15][5][230..238] = {`$bg_clr`,`$catch`,`$charArray`,`$conCat`,`$console_colours`,`$ctrlKey`,`$docBody`,`$eHeight`,`$eWidth`}
    WordLists[15][5][239..245] = {`$gb_freelist`,`$gi_freelist`,`$gimages`,`$gInit`,`$gpixbufs`,`$maxWindow`,`$ocument_createElement`}
    WordLists[15][5][246..250] = {`$paranormalise_traa`,`$paranormalise_qraa`,`$paranormalise_raa`,`$paranormalise_taa`,`$paranormalise_ptaab`}
    WordLists[15][5][251..260] = {`$pxFloat`,`$redraw`,`$repe`,`$repss`,`$resize_children`,`$shiftKey`,`$sidii`,`$storeAttr`,`$subse`,`$subss`}
    WordLists[15][5][261..267] = {`$timer`,`$to_bool`,`$topZindex`,`$tx_clr`,`$typeCheckError`,`$xpg_CloseOnEscape`,`$xpm_image_slot`}
    WordLists[15][5][268..276] = {`$xpm_pixbuf_slot`,`abort`,`abs`,`add_member`,`adjust_timedate`,`and_bits`,`and_bitsu`,`append`,`apply`}
    WordLists[15][5][277..285] = {`arccos`,`arcsin`,`arctan`,`assert`,`atan2`,`atom`,`atom_to_float32`,`atom_to_float64`,`bankers_rounding`}
    WordLists[15][5][286..293] = {`begins`,`binary_search`,`bits_to_int`,`bk_color`,`bool`,`bytes_to_int`,`call_func`,`call_proc`}
    WordLists[15][5][294..299] = {`cdCanvasActivate`,`cdCanvasArc`,`cdCanvasBegin`,`cdCanvasBox`,`cdCanvasChord`,`cdCanvasCircle`}
    WordLists[15][5][300..305] = {`cdCanvasClear`,`cdCanvasEnd`,`cdCanvasFlush`,`cdCanvasFont`,`cdCanvasGetSize`,`cdCanvasGetTextAlignment`}
    WordLists[15][5][306..311] = {`cdCanvasGetTextSize`,`cdCanvasLine`,`cdCanvasMark`,`cdCanvasMarkSize`,`cdCanvasMarkType`,`cdCanvasPixel`}
    WordLists[15][5][312..316] = {`cdCanvasRect`,`cdCanvasRoundedBox`,`cdCanvasRoundedRect`,`cdCanvasSector`,`cdCanvasSetAttribute`}
    WordLists[15][5][317..320] = {`cdCanvasSetBackground`,`cdCanvasSetFillMode`,`cdCanvasSetForeground`,`cdCanvasSetInteriorStyle`}
    WordLists[15][5][321..324] = {`cdCanvasSetLineWidth`,`cdCanvasSetTextAlignment`,`cdCanvasText`,`cdCanvasTextAlignment`}
    WordLists[15][5][325..329] = {`cdCanvasTextOrientation`,`cdCanvasVertex`,`cdCreateCanvas`,`cdDecodeAlpha`,`cdDecodeColor`}
    WordLists[15][5][330..335] = {`cdDecodeColorAlpha`,`cdEncodeAlpha`,`cdEncodeColor`,`cdEncodeColorAlpha`,`cdKillCanvas`,`ceil`}
    WordLists[15][5][336..341] = {`change_timezone`,`clear_screen`,`columnize`,`combinations`,`combinations_with_repetitions`,`command_line`}
    WordLists[15][5][342..350] = {`compare`,`cos`,`crash`,`date`,`decode_base64`,`deld`,`deep_copy`,`destroy_dict`,`destroy_queue`}
    WordLists[15][5][351..358] = {`destroy_stack`,`dict_name`,`dict_size`,`difference`,`dword_seq`,`elapsed`,`elapsed_short`,`encode_base64`}
    WordLists[15][5][359..368] = {`equal`,`even`,`exp`,`extract`,`factors`,`file_size_k`,`find`,`find_all`,`find_any`,`flatten`}
    WordLists[15][5][369..375] = {`float32_to_atom`,`float64_to_atom`,`floor`,`format_timedate`,`free_console`,`gButton`,`gCanvas`}
    WordLists[15][5][376..383] = {`gCanvasPixel`,`gCanvasGetPixel`,`gcd`,`gCheckbox`,`gClipboard`,`gDatePick`,`gDialog`,`gDrawArc`}
    WordLists[15][5][384..390] = {`gDrawCircle`,`gDrawCubicBezier`,`gDrawQuadBezier`,`gDrawImage`,`gDrawLine`,`gDrawPolygon`,`gDrawRect`}
    WordLists[15][5][391..396] = {`gDrawText`,`gDropDown`,`get_file_base`,`get_file_extension`,`get_file_name`,`get_file_name_and_path`}
    WordLists[15][5][397..402] = {`get_file_path`,`get_file_path_and_name`,`get_maxprime`,`get_prime`,`get_primes`,`get_primes_le`}
    WordLists[15][5][403..409] = {`get_proper_dir`,`get_proper_path`,`get_rand`,`get_routine_info`,`get_tzid`,`getd`,`getd_all_keys`}
    WordLists[15][5][410..416] = {`getd_by_index`,`getd_index`,`getd_partial_key`,`getdd`,`gFrame`,`gGetAlignName`,`gGetAttribute`}
    WordLists[15][5][417..424] = {`gGetBrother`,`gGetChild`,`gGetChildCount`,`gGetDialog`,`gGetDouble`,`gGetFocus`,`gGetGlobal`,`gGetGlobalInt`}
    WordLists[15][5][425..431] = {`gGetGlobalIntInt`,`gGetHandler`,`gGetInt`,`gGetIntInt`,`gGetParent`,`gGetSetAttribute`,`gGetTextExtent`}
    WordLists[15][5][432..439] = {`gHbox`,`gHide`,`gImage`,`gImage_from_XPM`,`gImage_from_Pixbuf`,`gImageDestroy`,`gLabel`,`glAttachShader`}
    WordLists[15][5][440..446] = {`glBindBuffer`,`glBindTexture`,`glBufferData`,`glClear`,`glClearColor`,`glCompileShader`,`glCreateBuffer`}
    WordLists[15][5][447..452] = {`glCreateProgram`,`glCreateShader`,`glCreateTexture`,`glDeleteProgram`,`glDeleteShader`,`glDrawArrays`}
    WordLists[15][5][453..458] = {`glEnable`,`glEnableVertexAttribArray`,`glFloat32Array`,`glFlush`,`glGetAttribLocation`,`glGetError`}
    WordLists[15][5][459..462] = {`glGetProgramInfoLog`,`glGetProgramParameter`,`glGetShaderInfoLog`,`glGetShaderParameter`}
    WordLists[15][5][463..468] = {`glGetUniformLocation`,`gList`,`glLinkProgram`,`glShaderSource`,`glSimpleA7texcoords`,`glTexImage2Dc`}
    WordLists[15][5][469..474] = {`glUniform1f`,`glUniform1i`,`glUniformMatrix4fv`,`glUseProgram`,`glVertexAttribPointer`,`glViewport`}
    WordLists[15][5][475..482] = {`gMainLoop`,`gMap`,`gMenu`,`gMenuGetAttribute`,`gMenuSetAttribute`,`gMsgBox`,`gPixbuf`,`gPixbuf_from_Image`}
    WordLists[15][5][483..489] = {`gPixbuf_from_Pixels`,`gPixbuf_take_Pixels`,`gPopupMenu`,`gProgressBar`,`gQuit`,`gRadio`,`gRadioItem`}
    WordLists[15][5][490..496] = {`gRedraw`,`gRotatePolygon`,`gSetAttribute`,`gSetAttributes`,`gSetDouble`,`gSetFocus`,`gSetGlobal`}
    WordLists[15][5][497..505] = {`gSetHandler`,`gSetHandlers`,`gSetInt`,`gShow`,`gSlider`,`gSpin`,`gSplit`,`gTable`,`gTableClearSelected`}
    WordLists[15][5][506..513] = {`gTableGetSelected`,`gTabs`,`gText`,`gTimer`,`gToggleInt`,`gTreeAddNodes`,`gTreeGetUserId`,`gTreeView`}
    WordLists[15][5][514..522] = {`gUseGTK`,`gVbox`,`gVersion`,`hcf`,`head`,`hsv_to_rgb`,`hsv_to_rgba`,`Icallback`,`include_file`}
    WordLists[15][5][523..531] = {`incl0de_file`,`insert`,`instance`,`int_to_bits`,`int_to_bytes`,`int`,`integer`,`intersection`,`is_dict`}
    WordLists[15][5][532..540] = {`is_empty`,`is_inf`,`is_member`,`is_nan`,`is_prime`,`is_subset`,`is_superset`,`iup_isdouble`,`iup_isprint`}
    WordLists[15][5][541..548] = {`iup_XkeyBase`,`IupButton`,`IupCanvas`,`IupClose`,`IupCloseOnEscape`,`IupDatePick`,`IupDestroy`,`IupDialog`}
    WordLists[15][5][549..555] = {`IupFill`,`IupFlatButton`,`IupFlatLabel`,`IupFlush`,`IupFrame`,`IupGetAttribute`,`IupGetAttributeId`}
    WordLists[15][5][556..561] = {`IupGetAttributePtr`,`IupGetBrother`,`IupGetChild`,`IupGetChildCount`,`IupGetDialog`,`IupGetDialogChild`}
    WordLists[15][5][562..568] = {`IupGetDouble`,`IupGetFocus`,`IupGetGlobal`,`IupGetGlobalInt`,`IupGetGlobalIntInt`,`IupGetInt`,`IupGetIntInt`}
    WordLists[15][5][569..575] = {`IupGetParent`,`IupGLCanvas`,`IupGLCanvasOpen`,`IupGLMakeCurrent`,`IupHbox`,`IupHide`,`iupKeyCodeToName`}
    WordLists[15][5][576..583] = {`IupLabel`,`IupList`,`IupMainLoop`,`IupMap`,`IupMessage`,`IupMultiBox`,`IupMultiLine`,`IupNextField`}
    WordLists[15][5][584..589] = {`IupOpen`,`IupPreviousField`,`IupRedraw`,`IupRefresh`,`IupRefreshChildren`,`IupSetAttribute`}
    WordLists[15][5][590..594] = {`IupSetAttributeHandle`,`IupSetAttributeId`,`IupSetAttributePtr`,`IupSetAttributes`,`IupSetCallback`}
    WordLists[15][5][595..600] = {`IupSetCallbacks`,`IupSetDouble`,`IupSetFocus`,`IupSetGlobal`,`IupSetGlobalFunction`,`IupSetGlobalInt`}
    WordLists[15][5][601..607] = {`IupSetInt`,`IupSetStrAttribute`,`IupShow`,`IupShowXY`,`IupStoreAttribute`,`IupTable`,`IupTableClearSelected`}
    WordLists[15][5][608..614] = {`IupTableClick_cb`,`IupTableGetSelected`,`IupTabs`,`IupText`,`IupTimer`,`IupToggle`,`IupToggleInt`}
    WordLists[15][5][615..623] = {`IupTreeAddNodes`,`IupTreeGetUserId`,`IupTreeView`,`IupUpdate`,`IupVbox`,`join`,`join_by`,`join_path`,`lcm`}
    WordLists[15][5][624..635] = {`length`,`ln`,`log`,`lower`,`machine_bits`,`machine_word`,`map`,`match`,`match_all`,`max`,`maxsq`,`min`}
    WordLists[15][5][636..643] = {`minsq`,`mod`,`mpfr`,`MPFR$COMMAFILL`,`MPFR$default_precision`,`MPFR$default_rounding`,`MPFR$GCD`,`MPFR$LCM`}
    WordLists[15][5][644..649] = {`MPFR$normalise`,`MPFR$ONE`,`MPFR$PREFIX`,`MPFR$precision_in_binary`,`MPFR$precision_in_dp`,`MPFR$replace_e`}
    WordLists[15][5][650..657] = {`mpfr_abs`,`mpfr_add`,`mpfr_add_d`,`mpfr_add_si`,`mpfr_addmul_si`,`mpfr_ceil`,`mpfr_cmp`,`mpfr_cmp_si`}
    WordLists[15][5][658..665] = {`mpfr_const_pi`,`mpfr_div`,`mpfr_div_si`,`mpfr_div_d`,`mpfr_div_z`,`mpfr_floor`,`mpfr_fmod`,`mpfr_free`}
    WordLists[15][5][666..669] = {`mpfr_get_d`,`mpfr_get_default_precision`,`mpfr_get_default_rounding_mode`,`mpfr_get_fixed`}
    WordLists[15][5][670..675] = {`mpfr_get_precision`,`mpfr_get_si`,`mpfr_init`,`mpfr_inits`,`mpfr_init_set`,`mpfr_init_set_q`}
    WordLists[15][5][676..683] = {`mpfr_init_set_z`,`mpfr_pow_si`,`mpfr_mul`,`mpfr_mul_si`,`mpfr_mul_d`,`mpfr_mul_z`,`mpfr_neg`,`mpfr_set`}
    WordLists[15][5][684..688] = {`mpfr_set_d`,`mpfr_set_default_precision`,`mpfr_set_default_rounding_mode`,`mpfr_set_precision`,`mpfr_set_q`}
    WordLists[15][5][689..696] = {`mpfr_set_si`,`mpfr_set_str`,`mpfr_set_z`,`mpfr_si_div`,`mpfr_si_sub`,`mpfr_sqrt`,`mpfr_sub`,`mpfr_sub_d`}
    WordLists[15][5][697..703] = {`mpfr_sub_si`,`mpfr_ui_pow`,`mpfr_ui_pow_ui`,`mpfr_si_pow_si`,`mpir_get_versions`,`mpir_open_dll`,`mpq`}
    WordLists[15][5][704..712] = {`mpq_abs`,`mpq_add`,`mpq_add_si`,`mpq_canonicalize`,`mpq_cmp`,`mpq_cmp_si`,`mpq_div`,`mpq_div_d`,`mpq_div_si`}
    WordLists[15][5][713..720] = {`mpq_div_2exp`,`mpq_free`,`mpq_get_d`,`mpq_get_den`,`mpq_get_num`,`mpq_get_str`,`mpq_init`,`mpq_init_set`}
    WordLists[15][5][721..727] = {`mpq_init_set_si`,`mpq_init_set_str`,`mpq_init_set_z`,`mpq_inits`,`mpq_inv`,`mpq_mul`,`mpq_mul_d`}
    WordLists[15][5][728..736] = {`mpq_mul_si`,`mpq_neg`,`mpq_set`,`mpq_set_si`,`mpq_set_str`,`mpq_set_z`,`mpq_sub`,`mpz`,`MPZ$GCD`}
    WordLists[15][5][737..744] = {`MPZ$modp47`,`MPZ$nthroot`,`MPZ$w`,`MPZ$witness_ranges`,`mpz_abs`,`mpz_add`,`mpz_addmul`,`mpz_addmul_ui`}
    WordLists[15][5][745..752] = {`mpz_addmul_si`,`mpz_add_si`,`mpz_add_ui`,`mpz_and`,`mpz_bin_uiui`,`mpz_cdiv_q`,`mpz_cmp`,`mpz_cmp_si`}
    WordLists[15][5][753..759] = {`mpz_divexact`,`mpz_divexact_ui`,`mpz_divisible_p`,`mpz_divisible_ui_p`,`mpz_even`,`mpz_fac_ui`,`mpz_factors`}
    WordLists[15][5][760..766] = {`mpz_factorstring`,`mpz_fdiv_ui`,`mpz_fdiv_q`,`mpz_fdiv_q_2exp`,`mpz_fdiv_q_ui`,`mpz_fdiv_qr`,`mpz_fdiv_r`}
    WordLists[15][5][767..773] = {`mpz_fib_ui`,`mpz_fib2_ui`,`mpz_fits_atom`,`mpz_fits_integer`,`mpz_free`,`mpz_gcd`,`mpz_gcd_ui`}
    WordLists[15][5][774..780] = {`mpz_get_atom`,`mpz_get_integer`,`mpz_get_short_str`,`mpz_get_str`,`mpz_init`,`mpz_init_set`,`mpz_inits`}
    WordLists[15][5][781..789] = {`mpz_invert`,`mpz_lcm`,`mpz_lcm_ui`,`mpz_max`,`mpz_min`,`mpz_mod`,`mpz_mod_ui`,`mpz_mul`,`mpz_mul_2exp`}
    WordLists[15][5][790..797] = {`mpz_mul_d`,`mpz_mul_si`,`mpz_neg`,`mpz_nextprime`,`mpz_nthroot`,`mpz_odd`,`mpz_pollard_rho`,`mpz_popcount`}
    WordLists[15][5][798..805] = {`mpz_pow_ui`,`mpz_powm`,`mpz_powm_ui`,`mpz_prime`,`mpz_prime_factors`,`mpz_prime_mr`,`mpz_rand`,`mpz_rand_ui`}
    WordLists[15][5][806..813] = {`mpz_re_compose`,`mpz_remove`,`mpz_scan0`,`mpz_scan1`,`mpz_set`,`mpz_set_d`,`mpz_set_q`,`mpz_set_si`}
    WordLists[15][5][814..821] = {`mpz_set_str`,`mpz_set_v`,`mpz_sign`,`mpz_sizeinbase`,`mpz_sqrt`,`mpz_sub`,`mpz_sub_si`,`mpz_sub_ui`}
    WordLists[15][5][822..828] = {`mpz_si_sub`,`mpz_tdiv_q_2exp`,`mpz_tdiv_r_2exp`,`mpz_tstbit`,`mpz_ui_pow_ui`,`mpz_xor`,`named_dict`}
    WordLists[15][5][829..838] = {`new_dict`,`new_queue`,`new_stack`,`not_bits`,`not_bitsu`,`nullable_string`,`number`,`object`,`odd`,`or_all`}
    WordLists[15][5][839..847] = {`or_allu`,`or_bits`,`or_bitsu`,`override_timezone`,`pad`,`pad_head`,`pad_tail`,`papply`,`parse_date_string`}
    WordLists[15][5][848..858] = {`peep`,`peepn`,`peep_dict`,`platform`,`pop`,`popn`,`pop_dict`,`power`,`pq_add`,`pq_destroy`,`pq_empty`}
    WordLists[15][5][859..868] = {`pq_new`,`pq_peek`,`pq_pop`,`pq_pop_data`,`pq_size`,`prepend`,`prime_factors`,`print`,`printf`,`product`}
    WordLists[15][5][869..878] = {`progress`,`push`,`pushn`,`putd`,`puts`,`queue_empty`,`queue_size`,`remove_member`,`rand`,`rand_range`}
    WordLists[15][5][879..890] = {`reinstate`,`remainder`,`repeat`,`repeatch`,`requires`,`rfind`,`rgb`,`rgba`,`rmatch`,`rmdr`,`rnd`,`round`}
    WordLists[15][5][891..898] = {`routine_id`,`seq`,`sequence`,`set_rand`,`set_timedate_formats`,`set_timezone`,`setd`,`setd_default`}
    WordLists[15][5][899..909] = {`shorten`,`sign`,`sin`,`sort`,`split`,`split_any`,`split_by`,`split_path`,`sprint`,`sprintf`,`string`}
    WordLists[15][5][910..919] = {`sq_abs`,`sq_add`,`sq_and`,`sq_and_bits`,`sq_arccos`,`sq_arcsin`,`sq_arctan`,`sq_atom`,`sq_ceil`,`sq_cmp`}
    WordLists[15][5][920..930] = {`sq_cos`,`sq_div`,`sq_eq`,`sq_floor`,`sq_floor_div`,`sq_ge`,`sq_gt`,`sq_int`,`sq_le`,`sq_log`,`sq_log10`}
    WordLists[15][5][931..941] = {`sq_log2`,`sq_lt`,`sq_max`,`sq_min`,`sq_mod`,`sq_mul`,`sq_ne`,`sq_not`,`sq_not_bits`,`sq_or`,`sq_or_bits`}
    WordLists[15][5][942..952] = {`sq_power`,`sq_rand`,`sq_rmdr`,`sq_rnd`,`sq_round`,`sq_seq`,`sq_sign`,`sq_sin`,`sq_sqrt`,`sq_str`,`sq_sub`}
    WordLists[15][5][953..961] = {`sq_tan`,`sq_trunc`,`sq_uminus`,`sq_xor`,`sq_xor_bits`,`sqrt`,`square_free`,`stack_empty`,`stack_size`}
    WordLists[15][5][962..971] = {`substitute`,`substitute_all`,`sum`,`tagset`,`tagstart`,`tail`,`tan`,`text_color`,`time`,`timedate_diff`}
    WordLists[15][5][972..979] = {`timedelta`,`to_rgb`,`to_rgba`,`trace`,`traverse_dict`,`traverse_dict_partial_key`,`trim`,`trim_head`}
    WordLists[15][5][980..988] = {`trim_tail`,`unique`,`union`,`unix_dict`,`upper`,`utf8_to_utf32`,`utf32_to_utf8`,`valid_index`,`version`}
    WordLists[15][5][989..993] = {`video_config`,`wait_key`,`xor`,`xor_bits`,`xor_bitsu`}
    WordLists[15][6][1..6] = {`$paranormalise`,`continue`,`elsif`,`gCanvasGetBackground`,`gCanvasGetForeground`,`gCanvasGetLineStyle`}
    WordLists[15][6][7] = `gCanvasGetLineWidth`
    WordLists[15][7][1..4] = {`gCanvasSetBackground`,`gCanvasSetForeground`,`gCanvasSetLineStyle`,`gCanvasSetLineWidth`}
    WordLists[15][7][5..9] = {`GL_INFO_LOG_LENGTH`,`GL_SHADER_SOURCE_LENGTH`,`glTexImage2D`,`mpfr_gamma`,`mpfr_gamma_inc`}
    WordLists[15][7][10..15] = {`mpfr_get_default_prec`,`mpfr_set_default_prec`,`srcElement`,`utf8_to_utf16`,`utf16_to_utf8`,`utf16_to_utf32`}
    WordLists[15][7][16..17] = {`utf32_to_utf16`,`wglGetProcAddress`}
    WordLists[16][1][1..14] = {`begin`,`break`,`catch`,`const`,`else`,`elseif`,`end`,`export`,`for`,`function`,`if`,`import`,`in`,`let`}
    WordLists[16][1][15..23] = {`module`,`mutable`,`return`,`sort`,`struct`,`try`,`using`,`where`,`while`}
    WordLists[16][2] = {`nothing`,`true`}
    WordLists[16][3][1..12] = {`Array`,`BigFloat`,`BigInt`,`Bool`,`Complex`,`Dict`,`Float32`,`Float64`,`Int`,`Integer`,`Matrix`,`Point2`}
    WordLists[16][3][13..18] = {`String`,`Tuple`,`UInt8`,`UInt16`,`UInt32`,`Vector`}
    WordLists[16][4][1..13] = {`abs`,`any`,`append`,`asind`,`atan`,`atand`,`captures`,`collect`,`cos`,`cosd`,`deepcopy`,`digits`,`display`}
    WordLists[16][4][14..24] = {`div`,`divrem`,`eltype`,`empty`,`enumerate`,`error`,`factor`,`factorial`,`fill`,`filter`,`findfirst`}
    WordLists[16][4][25..36] = {`findlast`,`findmax`,`findmin`,`first`,`floor`,`foreach`,`gcd`,`haskey`,`isempty`,`iszero`,`join`,`lcm`}
    WordLists[16][4][37..50] = {`length`,`map`,`match`,`max`,`min`,`minimum`,`new`,`one`,`parse`,`powermod`,`print`,`println`,`prod`,`push`}
    WordLists[16][4][51..63] = {`pushfirst`,`rand`,`reduce`,`reverse`,`round`,`rpad`,`show`,`sin`,`sincosd`,`sind`,`size`,`sleep`,`split`}
    WordLists[16][4][64..76] = {`sqrt`,`string`,`sum`,`time`,`typeof`,`unique`,`vec`,`zero`,`zip`,`bind`,`Button`,`Entry`,`formlayout`}
    WordLists[16][4][77..83] = {`Frame`,`get_value`,`Messagebox`,`pack`,`set_value`,`tcl`,`Toplevel`}
    WordLists[17][1][1..16] = {`!`,`and`,`append`,`apply`,`arg`,`args`,`asoq`,`atom`,`bit`,`bye`,`caar`,`caddr`,`cadr`,`can`,`car`,`case`}
    WordLists[17][1][17..31] = {`cdar`,`cdr`,`class`,`collect`,`commit`,`cond`,`cons`,`dat$`,`date`,`db`,`de`,`dec`,`def`,`default`,`delq`}
    WordLists[17][1][32..48] = {`disp`,`dm`,`echo`,`edit`,`eval`,`filter`,`find`,`flip`,`for`,`from`,`get`,`grid`,`if`,`ifn`,`in`,`inc`,`isa`}
    WordLists[17][1][49..63] = {`iter`,`last`,`length`,`let`,`line`,`link`,`list`,`lit`,`load`,`loop`,`make`,`mapc`,`mapcan`,`mapcar`,`match`}
    WordLists[17][1][64..80] = {`max`,`memq`,`more`,`n0`,`next`,`new`,`nond`,`not`,`off`,`or`,`out`,`pack`,`pass`,`pick`,`pool`,`pop`,`pp`}
    WordLists[17][1][81..94] = {`prinl`,`println`,`prop`,`push`,`put`,`quote`,`rand`,`read`,`rel`,`rest`,`rollback`,`scan`,`set`,`setq`}
    WordLists[17][1][95..108] = {`show`,`skip`,`split`,`super`,`till`,`tim$`,`time`,`tree`,`type`,`unless`,`update`,`use`,`what`,`when`}
    WordLists[17][1][109..113] = {`while`,`who`,`with`,`xchg`,`xor`}
    WordLists[17][2] = {`NIL`,`T`}
    WordLists[18][1] = {`chomp`,`defined`,`eq`,`else`,`exit`,`if`,`my`,`ne`,`next`,`print`,`shift`,`split`,`strict`,`use`,`warnings`}
    WordLists[18][2] = {`atom`,`integer`,`object`,`string`}
    WordLists[18][3][1..7] = {`AtomicSQL`,`CloseRecordset`,`Execute`,`Finish`,`GetField`,`GetFirstReturnedField`,`OpenRecordset`}
    WordLists[18][3][8] = `PrepareQuery`
    WordLists[18][4..5] = {{`D_ATTRIBUTES`,`D_DAY`,`D_HOUR`,`D_MINUTE`,`D_MONTH`,`D_NAME`,`D_SECOND`,`D_SIZE`,`D_YEAR`},{`all_palette`}}
    WordLists[19][1] = {`chomp`,`defined`,`eq`,`else`,`exit`,`if`,`my`,`ne`,`next`,`print`,`shift`,`split`,`strict`,`use`,`warnings`}
    WordLists[19][2] = {`atom`,`integer`,`object`,`string`}
    WordLists[19][3][1..7] = {`AtomicSQL`,`CloseRecordset`,`Execute`,`Finish`,`GetField`,`GetFirstReturnedField`,`OpenRecordset`}
    WordLists[19][3][8] = `PrepareQuery`
    WordLists[19][4..5] = {{`D_ATTRIBUTES`,`D_DAY`,`D_HOUR`,`D_MINUTE`,`D_MONTH`,`D_NAME`,`D_SECOND`,`D_SIZE`,`D_YEAR`},{`all_palette`}}
    WordLists[20][1][1..13] = {`abstract`,`and`,`as`,`break`,`by`,`case`,`catch`,`class`,`constant`,`default`,`do`,`dynamic`,`else`}
    WordLists[20][1][14..25] = {`elsedef`,`elsif`,`elsifdef`,`end`,`enum`,`exit`,`export`,`extends`,`fallthru`,`fallthrough`,`final`,`for`}
    WordLists[20][1][26..37] = {`forward`,`from`,`function`,`global`,`goto`,`if`,`ifdef`,`in`,`include`,`jump_table`,`local`,`namespace`}
    WordLists[20][1][38..50] = {`nested`,`not`,`nullable`,`or`,`private`,`procedure`,`public`,`return`,`static`,`struct`,`switch`,`then`,`to`}
    WordLists[20][1][51..57] = {`try`,`type`,`until`,`virtual`,`while`,`xor`,`ilASM`}
    WordLists[20][2] = {}
    WordLists[20][3][1..11] = {`with`,`without`,`withpop`,`profile`,`profile_time`,`trace`,`type_check`,`warning`,`strict`,`licence`,`debug`}
    WordLists[20][3][12..19] = {`console`,`gui`,`format`,`js`,`javascript`,`javascript_semantics`,`safe_mode`,`nested_globals`}
    WordLists[20][3][20] = `nested_locals`
    WordLists[20][4][1..12] = {`atom`,`bool`,`boolean`,`integer`,`int`,`lambda`,`number`,`object`,`sequence`,`seq`,`string`,`timedate`}
    WordLists[20][4][13..20] = {`a32Colour`,`a32Colour0`,`a32Dib`,`a32Dib0`,`sqlite3`,`sqlite3_stmt`,`complex`,`complexn`}
    WordLists[20][5][1..8] = {`abort`,`and_bits`,`and_bitsu`,`append`,`arctan`,`atom_to_float32`,`atom_to_float64`,`atom_to_float80`}
    WordLists[20][5][9..18] = {`bk_color`,`call`,`call_func`,`call_proc`,`chdir`,`clear_screen`,`close`,`compare`,`cos`,`define_c_func`}
    WordLists[20][5][19..26] = {`define_c_proc`,`define_c_var`,`equal`,`ffree`,`find`,`float32_to_atom`,`float64_to_atom`,`float80_to_atom`}
    WordLists[20][5][27..37] = {`floor`,`flush`,`free`,`free_console`,`get_key`,`get_position`,`get_proc_address`,`getc`,`gets`,`iff`,`iif`}
    WordLists[20][5][38..47] = {`instance`,`length`,`ln`,`lock_file`,`log`,`machine_bits`,`machine_word`,`match`,`mem_copy`,`mem_set`}
    WordLists[20][5][48..58] = {`not_bits`,`not_bitsu`,`open`,`open_dll`,`or_bits`,`or_bitsu`,`peek`,`peek1s`,`peek1u`,`peek2s`,`peek2u`}
    WordLists[20][5][59..70] = {`peek4s`,`peek4u`,`peek8s`,`peek8u`,`peekNS`,`peekns`,`peeknu`,`platform`,`poke`,`poke1`,`poke2`,`poke4`}
    WordLists[20][5][71..82] = {`poke8`,`pokeN`,`poken`,`position`,`power`,`prepend`,`puts`,`rand`,`remainder`,`repeat`,`repeatch`,`rfind`}
    WordLists[20][5][83..94] = {`rmatch`,`rmdr`,`seek`,`set_rand`,`sin`,`sleep`,`sqrt`,`tan`,`text_color`,`throw`,`time`,`unlock_file`}
    WordLists[20][5][95..99] = {`version`,`wait_key`,`where`,`xor_bits`,`xor_bitsu`}
    WordLists[20][6] = {`option_switches`,`peeks`}
    WordLists[20][7][1..10] = {`AF_INET`,`AF_UNIX`,`AF_UNSPEC`,`ANY_QUEUE`,`ARM`,`ASCENDING`,`BLACK`,`BLINKING`,`BLUE`,`BMP_INVALID_MODE`}
    WordLists[20][7][11..16] = {`BMP_OPEN_FAILED`,`BMP_SUCCESS`,`BMP_UNEXPECTED_EOF`,`BMP_UNSUPPORTED_FORMAT`,`BRIGHT_BLUE`,`BRIGHT_CYAN`}
    WordLists[20][7][17..25] = {`BRIGHT_GREEN`,`BRIGHT_MAGENTA`,`BRIGHT_RED`,`BRIGHT_WHITE`,`BROWN`,`BZ_RUN`,`BZ_FLUSH`,`BZ_FINISH`,`BZ_OK`}
    WordLists[20][7][26..32] = {`BZ_RUN_OK`,`BZ_FLUSH_OK`,`BZ_FINISH_OK`,`BZ_STREAM_END`,`BZ_SEQUENCE_ERROR`,`BZ_PARAM_ERROR`,`BZ_MEM_ERROR`}
    WordLists[20][7][33..38] = {`BZ_DATA_ERROR`,`BZ_DATA_ERROR_MAGIC`,`BZ_IO_ERROR`,`BZ_UNEXPECTED_EOF`,`BZ_OUTBUFF_FULL`,`BZ_CONFIG_ERROR`}
    WordLists[20][7][39..49] = {`C_BOOL`,`C_BYTE`,`C_CHAR`,`C_DOUBLE`,`C_DWORD`,`C_FLOAT`,`C_HANDLE`,`C_HWND`,`C_INT`,`C_INT64`,`C_LONG`}
    WordLists[20][7][50..60] = {`C_POINTER`,`C_PTR`,`C_QWORD`,`C_SHORT`,`C_UBYTE`,`C_UCHAR`,`C_UINT`,`C_ULONG`,`C_USHORT`,`C_WORD`,`CYAN`}
    WordLists[20][7][61..70] = {`D_ATTRIBUTES`,`D_DAY`,`D_HOUR`,`D_MINUTE`,`D_MONTH`,`D_NAME`,`D_SECOND`,`D_SIZE`,`D_YEAR`,`D_CREATION`}
    WordLists[20][7][71..76] = {`D_LASTACCESS`,`D_MODIFICATION`,`DB_EXISTS_ALREADY`,`DB_LOCK_EXCLUSIVE`,`DB_LOCK_FAIL`,`DB_LOCK_NO`}
    WordLists[20][7][77..83] = {`DB_LOCK_READ_ONLY`,`DB_LOCK_SHARED`,`DB_OK`,`DB_OPEN_FAIL`,`DESCENDING`,`DOS32`,`DRIVE_UNKNOWN`}
    WordLists[20][7][84..90] = {`DRIVE_NO_ROOT_DIR`,`DRIVE_REMOVABLE`,`DRIVE_FIXED`,`DRIVE_REMOTE`,`DRIVE_CDROM`,`DRIVE_RAMDISK`,`DT_YEAR`}
    WordLists[20][7][91..100] = {`DT_MONTH`,`DT_DAY`,`DT_HOUR`,`DT_MINUTE`,`DT_SECOND`,`DT_DOW`,`DT_MSEC`,`DT_DOY`,`DT_GMT`,`E_ATOM`}
    WordLists[20][7][101..111] = {`E_INTEGER`,`E_OBJECT`,`E_SEQUENCE`,`E_CODE`,`E_ADDR`,`E_LINE`,`E_RTN`,`E_NAME`,`E_FILE`,`E_PATH`,`E_USER`}
    WordLists[20][7][112..119] = {`EULER`,`FALSE`,`False`,`false`,`FIFO_QUEUE`,`FILETYPE_UNDEFINED`,`FILETYPE_NOT_FOUND`,`FILETYPE_FILE`}
    WordLists[20][7][120..127] = {`FILETYPE_DIRECTORY`,`FREEBSD`,`GRAY`,`GREEN`,`GT_WHOLE_FILE`,`GT_LF_STRIPPED`,`GT_LF_LEFT`,`GT_LF_LAST`}
    WordLists[20][7][128..138] = {`GT_KEEP_BOM`,`GT_BINARY`,`HSIEH30`,`id_bzs`,`IDABORT`,`IDCANCEL`,`IDIGNORE`,`IDNO`,`IDOK`,`IDRETRY`,`IDYES`}
    WordLists[20][7][139..146] = {`INADDR_ANY`,`INADDR_NONE`,`INVALID_SOCKET`,`JS`,`LIFO_QUEUE`,`LINUX`,`LOCK_EXCLUSIVE`,`LOCK_SHARED`}
    WordLists[20][7][147..152] = {`MAGENTA`,`MB_ABORTRETRYIGNORE`,`MB_APPLMODAL`,`MB_DEFAULT_DESKTOP_ONLY`,`MB_DEFBUTTON1`,`MB_DEFBUTTON2`}
    WordLists[20][7][153..159] = {`MB_DEFBUTTON3`,`MB_DEFBUTTON4`,`MB_HELP`,`MB_ICONASTERISK`,`MB_ICONERROR`,`MB_ICONEXCLAMATION`,`MB_ICONHAND`}
    WordLists[20][7][160..166] = {`MB_ICONINFORMATION`,`MB_ICONQUESTION`,`MB_ICONSTOP`,`MB_ICONWARNING`,`MB_OK`,`MB_OKCANCEL`,`MB_RETRYCANCEL`}
    WordLists[20][7][167..172] = {`MB_RIGHT`,`MB_RTLREADING`,`MB_SERVICE_NOTIFICATION`,`MB_SETFOREGROUND`,`MB_SYSTEMMODAL`,`MB_TASKMODAL`}
    WordLists[20][7][173..181] = {`MB_YESNO`,`MB_YESNOCANCEL`,`MIN_HEAP`,`MAX_HEAP`,`MPFR_RNDN`,`MPFR_RNDZ`,`MPFR_RNDU`,`MPFR_RNDD`,`MPFR_RNDA`}
    WordLists[20][7][182..192] = {`NORMAL_ORDER`,`NULL`,`null`,`p_bzs`,`PI`,`pp_Ascii`,`pp_Brkt`,`pp_Date`,`pp_File`,`pp_FltFmt`,`pp_Indent`}
    WordLists[20][7][193..200] = {`pp_IntFmt`,`pp_Maxlen`,`pp_Nest`,`pp_Pause`,`pp_StrFmt`,`pp_IntCh`,`RED`,`RE_BACKREFERENCES`}
    WordLists[20][7][201..206] = {`RE_CASEINSENSITIVE`,`RE_DOTMATCHESNL`,`RE_EARLY_EXIT`,`RE_PIKEVM`,`RE_RECURSIVE`,`REVERSE_ORDER`}
    WordLists[20][7][207..215] = {`S_ABSTRACT`,`S_CFFI`,`S_CLASS`,`S_DYNAMIC`,`S_NULLABLE`,`S_SEQUENCE`,`S_STRUCT`,`SD_BOTH`,`SD_RECEIVE`}
    WordLists[20][7][216..224] = {`SD_SEND`,`SEEK_OK`,`SF_PRIVATE`,`SF_PUBLIC`,`SF_FUNC`,`SF_PROC`,`SF_RTN`,`SO_RCVTIMEO`,`SOCK_DGRAM`}
    WordLists[20][7][225..231] = {`SOCK_STREAM`,`SOCKET_ERROR`,`SOL_SOCKET`,`SQLITE_OK`,`SQLITE_ERROR`,`SQLITE_INTERNAL`,`SQLITE_PERM`}
    WordLists[20][7][232..237] = {`SQLITE_ABORT`,`SQLITE_BUSY`,`SQLITE_LOCKED`,`SQLITE_NOMEM`,`SQLITE_READONLY`,`SQLITE_INTERRUPT`}
    WordLists[20][7][238..243] = {`SQLITE_IOERR`,`SQLITE_CORRUPT`,`SQLITE_NOTFOUND`,`SQLITE_FULL`,`SQLITE_CANTOPEN`,`SQLITE_PROTOCOL`}
    WordLists[20][7][244..249] = {`SQLITE_EMPTY`,`SQLITE_SCHEMA`,`SQLITE_TOOBIG`,`SQLITE_CONSTRAINT`,`SQLITE_MISMATCH`,`SQLITE_MISUSE`}
    WordLists[20][7][250..256] = {`SQLITE_NOLFS`,`SQLITE_AUTH`,`SQLITE_ROW`,`SQLITE_DONE`,`SQLITE3_FATAL`,`SQLITE3_NON_FATAL`,`SQLITE_INTEGER`}
    WordLists[20][7][257..264] = {`SQLITE_FLOAT`,`SQLITE_TEXT`,`SQLITE_BLOB`,`SQLITE_NULL`,`ST_ATOM`,`ST_INTEGER`,`ST_OBJECT`,`ST_SEQUENCE`}
    WordLists[20][7][265..271] = {`ST_STRING`,`TEST_ABORT`,`TEST_CRASH`,`TEST_PAUSE`,`TEST_PAUSE_FAIL`,`TEST_QUIET`,`TEST_SUMMARY`}
    WordLists[20][7][272..281] = {`TEST_SHOW_FAILED`,`TEST_SHOW_ALL`,`TRUE`,`True`,`true`,`YELLOW`,`VC_COLOR`,`VC_COLUMNS`,`VC_LINES`,`VC_MODE`}
    WordLists[20][7][282..290] = {`VC_NCOLORS`,`VC_PAGES`,`VC_SCRNCOLS`,`VC_SCRNLINES`,`VC_XPIXELS`,`VC_YPIXELS`,`W_BAD_PATH`,`WHITE`,`WIN32`}
    WordLists[20][7][291..299] = {`WINDOWS`,`CD_QUERY`,`CD_ERROR`,`CD_OK`,`CD_FILL`,`CD_OPEN_LINES`,`CD_CLOSED_LINES`,`CD_CLIP`,`CD_BEZIER`}
    WordLists[20][7][300..306] = {`CD_REGION`,`CD_PATH`,`CD_PATH_NEW`,`CD_PATH_MOVETO`,`CD_PATH_LINETO`,`CD_PATH_ARC`,`CD_PATH_CURVETO`}
    WordLists[20][7][307..313] = {`CD_PATH_CLOSE`,`CD_PATH_FILL`,`CD_PATH_STROKE`,`CD_PATH_FILLSTROKE`,`CD_PATH_CLIP`,`CD_RGB`,`CD_MAP`}
    WordLists[20][7][314..322] = {`CD_RGBA`,`CD_IRED`,`CD_IGREEN`,`CD_IBLUE`,`CD_IALPHA`,`CD_INDEX`,`CD_COLORS`,`CD_CLIPOFF`,`CD_CLIPAREA`}
    WordLists[20][7][323..329] = {`CD_CLIPPOLYGON`,`CD_CLIPREGION`,`CD_UNION`,`CD_INTERSECT`,`CD_DIFFERENCE`,`CD_NOTINTERSECT`,`CD_EVENODD`}
    WordLists[20][7][330..337] = {`CD_WINDING`,`CD_MITER`,`CD_BEVEL`,`CD_ROUND`,`CD_CAPFLAT`,`CD_CAPSQUARE`,`CD_CAPROUND`,`CD_OPAQUE`}
    WordLists[20][7][338..345] = {`CD_TRANSPARENT`,`CD_REPLACE`,`CD_XOR`,`CD_NOT_XOR`,`CD_POLITE`,`CD_FORCE`,`CD_CONTINUOUS`,`CD_DASHED`}
    WordLists[20][7][346..354] = {`CD_DOTTED`,`CD_DASH_DOT`,`CD_DASH_DOT_DOT`,`CD_CUSTOM`,`CD_PLUS`,`CD_STAR`,`CD_CIRCLE`,`CD_X`,`CD_BOX`}
    WordLists[20][7][355..360] = {`CD_DIAMOND`,`CD_HOLLOW_CIRCLE`,`CD_HOLLOW_BOX`,`CD_HOLLOW_DIAMOND`,`CD_HORIZONTAL`,`CD_VERTICAL`}
    WordLists[20][7][361..368] = {`CD_FDIAGONAL`,`CD_BDIAGONAL`,`CD_CROSS`,`CD_DIAGCROSS`,`CD_SOLID`,`CD_HATCH`,`CD_STIPPLE`,`CD_PATTERN`}
    WordLists[20][7][369..376] = {`CD_HOLLOW`,`CD_NORTH`,`CD_SOUTH`,`CD_EAST`,`CD_WEST`,`CD_NORTH_EAST`,`CD_NORTH_WEST`,`CD_SOUTH_EAST`}
    WordLists[20][7][377..384] = {`CD_SOUTH_WEST`,`CD_CENTER`,`CD_BASE_LEFT`,`CD_BASE_CENTER`,`CD_BASE_RIGHT`,`CD_PLAIN`,`CD_BOLD`,`CD_ITALIC`}
    WordLists[20][7][385..391] = {`CD_UNDERLINE`,`CD_STRIKEOUT`,`CD_BOLD_ITALIC`,`CD_SMALL`,`CD_STANDARD`,`CD_LARGE`,`CD_CAP_NONE`}
    WordLists[20][7][392..397] = {`CD_CAP_FLUSH`,`CD_CAP_CLEAR`,`CD_CAP_PLAY`,`CD_CAP_YAXIS`,`CD_CAP_CLIPAREA`,`CD_CAP_CLIPPOLY`}
    WordLists[20][7][398..403] = {`CD_CAP_REGION`,`CD_CAP_RECT`,`CD_CAP_CHORD`,`CD_CAP_IMAGERGB`,`CD_CAP_IMAGERGBA`,`CD_CAP_IMAGEMAP`}
    WordLists[20][7][404..408] = {`CD_CAP_GETIMAGERGB`,`CD_CAP_IMAGESRV`,`CD_CAP_BACKGROUND`,`CD_CAP_BACKOPACITY`,`CD_CAP_WRITEMODE`}
    WordLists[20][7][409..414] = {`CD_CAP_LINESTYLE`,`CD_CAP_LINEWITH`,`CD_CAP_FPRIMTIVES`,`CD_CAP_HATCH`,`CD_CAP_STIPPLE`,`CD_CAP_PATTERN`}
    WordLists[20][7][415..420] = {`CD_CAP_FONT`,`CD_CAP_FONTDIM`,`CD_CAP_TEXTSIZE`,`CD_CAP_TEXTORIENTATION`,`CD_CAP_PALETTE`,`CD_CAP_LINECAP`}
    WordLists[20][7][421..427] = {`CD_CAP_LINEJOIN`,`CD_CAP_PATH`,`CD_CAP_BEZIER`,`CD_CAP_ALL`,`CD_SIZECB`,`CD_ABORT`,`CD_CONTINUE`}
    WordLists[20][7][428..434] = {`CD_SIM_NONE`,`CD_SIM_LINE`,`CD_SIM_RECT`,`CD_SIM_BOX`,`CD_SIM_ARC`,`CD_SIM_SECTOR`,`CD_SIM_CHORD`}
    WordLists[20][7][435..441] = {`CD_SIM_POLYLINE`,`CD_SIM_POLYGON`,`CD_SIM_TEXT`,`CD_SIM_ALL`,`CD_SIM_LINES`,`CD_SIM_FILLS`,`CD_AMBER`}
    WordLists[20][7][442..449] = {`CD_BLACK`,`CD_BLUE`,`CD_CYAN`,`CD_DARK_BLUE`,`CD_DARK_CYAN`,`CD_DARK_GRAY`,`CD_DARK_GREY`,`CD_DARK_GREEN`}
    WordLists[20][7][450..457] = {`CD_DARK_MAGENTA`,`CD_DARK_RED`,`CD_DARK_YELLOW`,`CD_GRAY`,`CD_GREY`,`CD_GREEN`,`CD_INDIGO`,`CD_MAGENTA`}
    WordLists[20][7][458..464] = {`CD_NAVY`,`CD_OLIVE`,`CD_RED`,`CD_LIGHT_BLUE`,`CD_LIGHT_GRAY`,`CD_LIGHT_GREY`,`CD_LIGHT_GREEN`}
    WordLists[20][7][465..472] = {`CD_LIGHT_PARCHMENT`,`CD_ORANGE`,`CD_PARCHMENT`,`CD_PURPLE`,`CD_SILVER`,`CD_TEAL`,`CD_VIOLET`,`CD_WHITE`}
    WordLists[20][7][473..481] = {`CD_YELLOW`,`CD_MM2PT`,`CD_RAD2DEG`,`CD_DEG2RAD`,`CD_IUP`,`CD_IUPDRAW`,`CD_PRINTER`,`CD_PS`,`CD_PICTURE`}
    WordLists[20][7][482..489] = {`CD_GL`,`CD_IUPDBUFFER`,`CD_DBUFFER`,`CD_IMIMAGE`,`CD_PPTX`,`CD_IMAGERGB`,`CD_DBUFFERRGB`,`CD_CGM`}
    WordLists[20][7][490..499] = {`CD_METAFILE`,`CD_WMF`,`CD_EMF`,`CD_DEBUG`,`CD_DGN`,`CD_DXF`,`CD_PDF`,`CD_SVG`,`CD_CLIPBOARD`,`EXIT_SUCCESS`}
    WordLists[20][7][500..506] = {`EXIT_FAILURE`,`GL_POINTS`,`GL_LINES`,`GL_LINE_LOOP`,`GL_LINE_STRIP`,`GL_TRIANGLES`,`GL_TRIANGLE_STRIP`}
    WordLists[20][7][507..513] = {`GL_TRIANGLE_FAN`,`GL_QUADS`,`GL_QUAD_STRIP`,`GL_POLYGON`,`GL_NO_ERROR`,`GL_INVALID_ENUM`,`GL_INVALID_VALUE`}
    WordLists[20][7][514..519] = {`GL_INVALID_OPERATION`,`GL_STACK_OVERFLOW`,`GL_STACK_UNDERFLOW`,`GL_OUT_OF_MEMORY`,`GL_VENDOR`,`GL_RENDERER`}
    WordLists[20][7][520..526] = {`GL_VERSION`,`GL_EXTENSIONS`,`GL_FLAT`,`GL_SMOOTH`,`GL_CURRENT_BIT`,`GL_POINT_BIT`,`GL_LINE_BIT`}
    WordLists[20][7][527..531] = {`GL_POLYGON_BIT`,`GL_POLYGON_STIPPLE_BIT`,`GL_PIXEL_MODE_BIT`,`GL_LIGHTING_BIT`,`GL_FOG_BIT`}
    WordLists[20][7][532..536] = {`GL_DEPTH_BUFFER_BIT`,`GL_ACCUM_BUFFER_BIT`,`GL_STENCIL_BUFFER_BIT`,`GL_VIEWPORT_BIT`,`GL_TRANSFORM_BIT`}
    WordLists[20][7][537..542] = {`GL_ENABLE_BIT`,`GL_COLOR_BUFFER_BIT`,`GL_HINT_BIT`,`GL_EVAL_BIT`,`GL_LIST_BIT`,`GL_TEXTURE_BIT`}
    WordLists[20][7][543..549] = {`GL_SCISSOR_BIT`,`GL_ALL_ATTRIB_BITS`,`GL_MODELVIEW`,`GL_PROJECTION`,`GL_TEXTURE`,`GL_LIGHT0`,`GL_LIGHT1`}
    WordLists[20][7][550..557] = {`GL_LIGHT2`,`GL_LIGHT3`,`GL_LIGHT4`,`GL_LIGHT5`,`GL_LIGHT6`,`GL_LIGHT7`,`GL_AMBIENT`,`GL_DIFFUSE`}
    WordLists[20][7][558..563] = {`GL_LIGHTING`,`GL_COLOR_MATERIAL`,`GL_DEPTH_TEST`,`GL_SPECULAR`,`GL_POSITION`,`GL_SPOT_DIRECTION`}
    WordLists[20][7][564..567] = {`GL_SPOT_EXPONENT`,`GL_SPOT_CUTOFF`,`GL_CONSTANT_ATTENUATION`,`GL_LINEAR_ATTENUATION`}
    WordLists[20][7][568..572] = {`GL_QUADRATIC_ATTENUATION`,`GL_ARRAY_BUFFER`,`GL_CULL_FACE`,`GL_CURRENT_COLOR`,`GL_COMPILE_STATUS`}
    WordLists[20][7][573..579] = {`GL_DEPTH_TEST`,`GL_FALSE`,`GL_FRAGMENT_SHADER`,`GL_LINK_STATUS`,`GL_MODELVIEW`,`GL_NO_ERROR`,`GL_PROJECTION`}
    WordLists[20][7][580..585] = {`GL_STATIC_DRAW`,`GL_TRIANGLES`,`GL_TRUE`,`GL_VERTEX_SHADER`,`GL_CLAMP`,`GL_CLAMP_TO_BORDER`}
    WordLists[20][7][586..592] = {`GL_CLAMP_TO_EDGE`,`GL_LINEAR`,`GL_NEAREST`,`GL_REPEAT`,`GL_RGB`,`GL_RGBA`,`GL_TEXTURE_2D`}
    WordLists[20][7][593..597] = {`GL_TEXTURE_MAG_FILTER`,`GL_TEXTURE_MIN_FILTER`,`GL_TEXTURE_WRAP_S`,`GL_TEXTURE_WRAP_T`,`GL_UNSIGNED_BYTE`}
    WordLists[20][7][598..604] = {`IUP_ANYWHERE`,`IUP_BOTTOM`,`IUP_BOTTOMPARENT`,`IUP_BUTTON1`,`IUP_BUTTON2`,`IUP_BUTTON3`,`IUP_BUTTON4`}
    WordLists[20][7][605..611] = {`IUP_BUTTON5`,`IUP_CENTER`,`IUP_CENTERPARENT`,`IUP_CLOSE`,`IUP_CONTINUE`,`IUP_CURRENT`,`IUP_DEFAULT`}
    WordLists[20][7][612..616] = {`IUP_ERROR`,`IUP_GETPARAM_BUTTON1`,`IUP_GETPARAM_BUTTON2`,`IUP_GETPARAM_BUTTON3`,`IUP_GETPARAM_CANCEL`}
    WordLists[20][7][617..621] = {`IUP_GETPARAM_CLOSE`,`IUP_GETPARAM_MAP`,`IUP_GETPARAM_HELP`,`IUP_GETPARAM_INIT`,`IUP_GETPARAM_OK`}
    WordLists[20][7][622..628] = {`IUP_IGNORE`,`IUP_INVALID`,`IUP_INVALID_ID`,`IUP_LEFT`,`IUP_LEFTPARENT`,`IUP_MASK_UINT`,`IUP_MOUSEPOS`}
    WordLists[20][7][629..635] = {`IUP_NOERROR`,`IUP_OPENED`,`IUP_RECBINARY`,`IUP_RECTEXT`,`IUP_RIGHT`,`IUP_RIGHTPARENT`,`IUP_TOP`}
    WordLists[20][7][636..642] = {`IUP_TOPPARENT`,`IUP_BLACK`,`IUP_BLUE`,`IUP_CYAN`,`IUP_DARK_BLUE`,`IUP_DARK_CYAN`,`IUP_DARK_GRAY`}
    WordLists[20][7][643..649] = {`IUP_DARK_GREY`,`IUP_DARK_GREEN`,`IUP_DARK_MAGENTA`,`IUP_DARK_RED`,`IUP_DARK_YELLOW`,`IUP_GRAY`,`IUP_GREY`}
    WordLists[20][7][650..657] = {`IUP_GREEN`,`IUP_INDIGO`,`IUP_MAGENTA`,`IUP_NAVY`,`IUP_OLIVE`,`IUP_RED`,`IUP_LIGHT_BLUE`,`IUP_LIGHT_GRAY`}
    WordLists[20][7][658..664] = {`IUP_LIGHT_GREY`,`IUP_LIGHT_GREEN`,`IUP_ORANGE`,`IUP_AMBER`,`IUP_PARCHMENT`,`IUP_PURPLE`,`IUP_SILVER`}
    WordLists[20][7][665..673] = {`IUP_TEAL`,`IUP_VIOLET`,`IUP_WHITE`,`IUP_YELLOW`,`K_BS`,`K_SP`,`K_exclam`,`K_quotedbl`,`K_numbersign`}
    WordLists[20][7][674..681] = {`K_dollar`,`K_percent`,`K_ampersand`,`K_apostrophe`,`K_parentleft`,`K_parentright`,`K_asterisk`,`K_plus`}
    WordLists[20][7][682..695] = {`K_comma`,`K_minus`,`K_period`,`K_slash`,`K_0`,`K_1`,`K_2`,`K_3`,`K_4`,`K_5`,`K_6`,`K_7`,`K_8`,`K_9`}
    WordLists[20][7][696..707] = {`K_colon`,`K_semicolon`,`K_less`,`K_equal`,`K_greater`,`K_question`,`K_at`,`K_A`,`K_B`,`K_C`,`K_D`,`K_E`}
    WordLists[20][7][708..725] = {`K_F`,`K_G`,`K_H`,`K_I`,`K_J`,`K_K`,`K_L`,`K_M`,`K_N`,`K_O`,`K_P`,`K_Q`,`K_R`,`K_S`,`K_T`,`K_U`,`K_V`,`K_W`}
    WordLists[20][7][726..735] = {`K_X`,`K_Y`,`K_Z`,`K_bracketleft`,`K_backslash`,`K_bracketright`,`K_circum`,`K_underscore`,`K_grave`,`K_a`}
    WordLists[20][7][736..753] = {`K_b`,`K_c`,`K_d`,`K_e`,`K_f`,`K_g`,`K_h`,`K_i`,`K_j`,`K_k`,`K_l`,`K_m`,`K_n`,`K_o`,`K_p`,`K_q`,`K_r`,`K_s`}
    WordLists[20][7][754..766] = {`K_t`,`K_u`,`K_v`,`K_w`,`K_x`,`K_y`,`K_z`,`K_braceleft`,`K_bar`,`K_braceright`,`K_tilde`,`K_CR`,`K_DEL`}
    WordLists[20][7][767..780] = {`K_DOWN`,`K_END`,`K_ESC`,`K_F1`,`K_F2`,`K_F3`,`K_F4`,`K_F5`,`K_F6`,`K_F7`,`K_F8`,`K_F9`,`K_F10`,`K_F11`}
    WordLists[20][7][781..792] = {`K_F12`,`K_HOME`,`K_INS`,`K_LEFT`,`K_LF`,`K_Menu`,`K_MIDDLE`,`K_PAUSE`,`K_PGDN`,`K_PGUP`,`K_Print`,`K_RIGHT`}
    WordLists[20][7][793..803] = {`K_TAB`,`K_UP`,`K_LSHIFT`,`K_RSHIFT`,`K_LCTRL`,`K_RCTRL`,`K_LALT`,`K_RALT`,`K_NUM`,`K_SCROLL`,`K_CAPS`}
    WordLists[20][7][804..816] = {`K_cSP`,`K_csSP`,`K_cPlus`,`K_cMinus`,`K_cEqual`,`K_cA`,`K_cB`,`K_cC`,`K_cD`,`K_cE`,`K_cF`,`K_cG`,`K_cH`}
    WordLists[20][7][817..831] = {`K_cI`,`K_cJ`,`K_cK`,`K_cL`,`K_cM`,`K_cN`,`K_cO`,`K_cP`,`K_cQ`,`K_cR`,`K_cS`,`K_cT`,`K_cU`,`K_cV`,`K_cW`}
    WordLists[20][7][832..844] = {`K_cX`,`K_cY`,`K_cZ`,`K_cPGUP`,`K_cPGDN`,`K_cHOME`,`K_cEND`,`K_cF1`,`K_cF2`,`K_cF3`,`K_cF4`,`K_cF5`,`K_cF6`}
    WordLists[20][7][845..857] = {`K_cF7`,`K_cF8`,`K_cF9`,`K_cF10`,`K_cF11`,`K_cF12`,`K_csA`,`K_csB`,`K_csC`,`K_csD`,`K_csE`,`K_csF`,`K_csG`}
    WordLists[20][7][858..870] = {`K_csH`,`K_csI`,`K_csJ`,`K_csK`,`K_csL`,`K_csM`,`K_csN`,`K_csO`,`K_csP`,`K_csQ`,`K_csR`,`K_csS`,`K_csT`}
    WordLists[20][7][871..884] = {`K_csU`,`K_csV`,`K_csW`,`K_csX`,`K_csY`,`K_csZ`,`K_mA`,`K_mB`,`K_mC`,`K_mD`,`K_mE`,`K_mF`,`K_mG`,`K_mH`}
    WordLists[20][7][885..899] = {`K_mI`,`K_mJ`,`K_mK`,`K_mL`,`K_mM`,`K_mN`,`K_mO`,`K_mP`,`K_mQ`,`K_mR`,`K_mS`,`K_mT`,`K_mU`,`K_mV`,`K_mW`}
    WordLists[20][7][900..913] = {`K_mX`,`K_mY`,`K_mZ`,`K_msA`,`K_msB`,`K_msC`,`K_msD`,`K_msE`,`K_msF`,`K_msG`,`K_msH`,`K_msI`,`K_msJ`,`K_msK`}
    WordLists[20][7][914..926] = {`K_msL`,`K_msM`,`K_msN`,`K_msO`,`K_msP`,`K_msQ`,`K_msR`,`K_msS`,`K_msT`,`K_msU`,`K_msV`,`K_msW`,`K_msX`}
    WordLists[20][7][927..938] = {`K_msY`,`K_msZ`,`K_sSP`,`K_scSP`,`VK_APPS`,`VK_BS`,`VK_CAPSLOCK`,`VK_CR`,`VK_DEL`,`VK_DOWN`,`VK_END`,`VK_ESC`}
    WordLists[20][7][939..951] = {`VK_F1`,`VK_F2`,`VK_F3`,`VK_F4`,`VK_F5`,`VK_F6`,`VK_F7`,`VK_F8`,`VK_F9`,`VK_F10`,`VK_F11`,`VK_F12`,`VK_HOME`}
    WordLists[20][7][952..961] = {`VK_INS`,`VK_LALT`,`VK_LCTRL`,`VK_LEFT`,`VK_LF`,`VK_LSHIFT`,`VK_NUMLOCK`,`VK_PAUSE`,`VK_PGDN`,`VK_PGUP`}
    WordLists[20][7][962..971] = {`VK_POUND`,`VK_RALT`,`VK_RCTRL`,`VK_RIGHT`,`VK_RSHIFT`,`VK_SCROLL`,`VK_SP`,`VK_TAB`,`VK_UP`,`XPG_ABUT`}
    WordLists[20][7][972..979] = {`XPG_ABUTT`,`XPG_ABUTR`,`XPG_ABUTTR`,`XPG_CLOSE`,`XPG_CONTINUE`,`XPG_DEFAULT`,`XPG_IGNORE`,`XPG_BOTTOM`}
    WordLists[20][7][980..986] = {`XPG_BOTTOMPARENT`,`XPG_CENTER`,`XPG_CURRENT`,`XPG_CENTERPARENT`,`XPG_LEFT`,`XPG_LEFTPARENT`,`XPG_MOUSEPOS`}
    WordLists[20][7][987..993] = {`XPG_RIGHT`,`XPG_RIGHTPARENT`,`XPG_TOP`,`XPG_TOPPARENT`,`XPG_BLACK`,`XPG_DARK_CYAN`,`XPG_DARK_GREEN`}
    WordLists[20][7][994..1000] = {`XPG_DARK_RED`,`XPG_DARK_VIOLET`,`XPG_DARK_YELLOW`,`XPG_OLIVE`,`XPG_NAVY`,`XPG_DARK_PURPLE`,`XPG_TEAL`}
    WordLists[20][7][1001..1008] = {`XPG_GREY`,`XPG_GRAY`,`XPG_SILVER`,`XPG_SLATE`,`XPG_DARK_GREY`,`XPG_DARK_GRAY`,`XPG_RED`,`XPG_GREEN`}
    WordLists[20][7][1009..1015] = {`XPG_LIGHT_GREEN`,`XPG_YELLOW`,`XPG_BLUE`,`XPG_MAGENTA`,`XPG_CYAN`,`XPG_INDIGO`,`XPG_LIGHT_GREY`}
    WordLists[20][7][1016..1021] = {`XPG_LIGHT_GRAY`,`XPG_LIGHT_GREEN`,`XPG_LIGHT_BLUE`,`XPG_LIGHT_PARCHMENT`,`XPG_ORANGE`,`XPG_AMBER`}
    WordLists[20][7][1022..1028] = {`XPG_PARCHMENT`,`XPG_PURPLE`,`XPG_VIOLET`,`XPG_WHITE`,`XPG_CONTINUOUS`,`XPG_CUSTOM_DASH`,`XPG_DASHED`}
    WordLists[20][7][1029..1036] = {`XPG_DOTTED`,`XPG_DASH_DOT`,`XPG_DASH_DOT_DOT`,`XPG_FILLED`,`XPG_CHORD`,`XPG_SECTOR`,`XPG_N`,`XPG_NW`}
    WordLists[20][7][1037..1046] = {`XPG_NE`,`XPG_W`,`XPG_C`,`XPG_E`,`XPG_SW`,`XPG_SE`,`XPG_S`,`XPG_NORTH`,`XPG_NORTHWEST`,`XPG_NORTHEAST`}
    WordLists[20][7][1047..1054] = {`XPG_WEST`,`XPG_CENTRE`,`XPG_EAST`,`XPG_SOUTHWEST`,`XPG_SOUTHEAST`,`XPG_SOUTH`,`XPG_NORMAL`,`XPG_BOLD`}
    WordLists[20][7][1055..1061] = {`XPG_ITALIC`,`XPG_BOLDITALIC`,`XPG_DEG2RAD`,`XPG_RAD2DEG`,`XPG_SPACE_NONE`,`XPG_SPACE_LEFT`,`XPG_SPACE_TOP`}
    WordLists[20][7][1062..1067] = {`XPG_SPACE_RIGHT`,`XPG_SPACE_BOTTOM`,`XPG_SPACE_BETWEEN`,`XPG_SPACE_AROUND`,`XPG_SPACE_CENTRE`,`XPG_GTK`}
    WordLists[20][7][1068..1073] = {`XPG_WINAPI`,`XPG_JS`,`CURL_ERROR_SIZE`,`CURL_GLOBAL_ALL`,`CURL_GLOBAL_DEFAULT`,`CURL_LOCK_ACCESS_SHARED`}
    WordLists[20][7][1074..1077] = {`CURL_LOCK_ACCESS_SINGLE`,`CURL_LOCK_DATA_CONNECT`,`CURL_LOCK_DATA_COOKIE`,`CURL_LOCK_DATA_DNS`}
    WordLists[20][7][1078..1082] = {`CURL_LOCK_DATA_SSL_SESSION`,`CURL_SOCKET_BAD`,`CURL_WAIT_POLLIN`,`CURL_WAIT_POLLPRI`,`CURL_WAIT_POLLOUT`}
    WordLists[20][7][1083..1087] = {`CURLAUTH_BASIC`,`CURLAUTH_DIGEST`,`CURLAUTH_NEGOTIATE`,`CURLAUTH_NTLM`,`CURLAUTH_DIGEST_IE`}
    WordLists[20][7][1088..1092] = {`CURLAUTH_NTLM_WB`,`CURLAUTH_BEARER`,`CURLAUTH_ONLY`,`CURLAUTH_ANY`,`CURLAUTH_ANYSAFE`}
    WordLists[20][7][1093..1096] = {`CURLE_ABORTED_BY_CALLBACK`,`CURLE_AGAIN`,`CURLE_CANT_OPEN_FILE`,`CURLE_COULDNT_RESOLVE_HOST`}
    WordLists[20][7][1097..1101] = {`CURLE_FAILED_INIT`,`CURLE_HTTP_RETURNED_ERROR`,`CURLE_NOT_BUILT_IN`,`CURLE_OK`,`CURLE_OUT_OF_MEMORY`}
    WordLists[20][7][1102..1105] = {`CURLE_RECV_ERROR`,`CURLE_UNKNOWN_OPTION`,`CURLE_UNSUPPORTED_PROTOCOL`,`CURLINFO_ACTIVESOCKET`}
    WordLists[20][7][1106..1110] = {`CURLINFO_CERTINFO`,`CURLINFO_CONTENT_TYPE`,`CURLINFO_COOKIELIST`,`CURLINFO_LASTSOCKET`,`CURLINFO_PRIVATE`}
    WordLists[20][7][1111..1115] = {`CURLINFO_REDIRECT_URL`,`CURLINFO_RESPONSE_CODE`,`CURLINFO_TEXT`,`CURLINFO_HEADER_IN`,`CURLINFO_HEADER_OUT`}
    WordLists[20][7][1116..1120] = {`CURLINFO_DATA_IN`,`CURLINFO_DATA_OUT`,`CURLINFO_SSL_DATA_IN`,`CURLINFO_SSL_DATA_OUT`,`CURLINFO_END`}
    WordLists[20][7][1121..1125] = {`CURLM_OK`,`CURLM_UNKNOWN_OPTION`,`CURLMOPT_TIMERDATA`,`CURLMOPT_TIMERFUNCTION`,`CURLOPT_CAINFO`}
    WordLists[20][7][1126..1130] = {`CURLOPT_CERTINFO`,`CURLOPT_CONNECT_ONLY`,`CURLOPT_COOKIEFILE`,`CURLOPT_COOKIEJAR`,`CURLOPT_COOKIELIST`}
    WordLists[20][7][1131..1134] = {`CURLOPT_CUSTOMREQUEST`,`CURLOPT_DEBUGFUNCTION`,`CURLOPT_ERRORBUFFER`,`CURLOPT_FAILONERROR`}
    WordLists[20][7][1135..1139] = {`CURLOPT_FOLLOWLOCATION`,`CURLOPT_HEADER`,`CURLOPT_HEADERFUNCTION`,`CURLOPT_HTTPAUTH`,`CURLOPT_HTTPGET`}
    WordLists[20][7][1140..1144] = {`CURLOPT_HTTPHEADER`,`CURLOPT_MAIL_FROM`,`CURLOPT_MAIL_RCPT`,`CURLOPT_NOPROGRESS`,`CURLOPT_PIPEWAIT`}
    WordLists[20][7][1145..1149] = {`CURLOPT_POST`,`CURLOPT_POSTFIELDS`,`CURLOPT_POSTFIELDSIZE`,`CURLOPT_POSTFIELDSIZE_LARGE`,`CURLOPT_PRIVATE`}
    WordLists[20][7][1150..1154] = {`CURLOPT_PROGRESSDATA`,`CURLOPT_PROXY`,`CURLOPT_PROXYAUTH`,`CURLOPT_PROXYTYPE`,`CURLOPT_READDATA`}
    WordLists[20][7][1155..1159] = {`CURLOPT_READFUNCTION`,`CURLOPT_RESOLVE`,`CURLOPT_SHARE`,`CURLOPT_SSL_VERIFYHOST`,`CURLOPT_SSL_VERIFYPEER`}
    WordLists[20][7][1160..1165] = {`CURLOPT_UPLOAD`,`CURLOPT_URL`,`CURLOPT_USERAGENT`,`CURLOPT_USERPWD`,`CURLOPT_USERNAME`,`CURLOPT_PASSWORD`}
    WordLists[20][7][1166..1171] = {`CURLOPT_USE_SSL`,`CURLUSESSL_NONE`,`CURLUSESSL_TRY`,`CURLUSESSL_CONTROL`,`CURLUSESSL_ALL`,`CURLUSESSL_LAST`}
    WordLists[20][7][1172..1176] = {`CURLE_USE_SSL_FAILED`,`CURLOPT_VERBOSE`,`CURLOPT_WRITEDATA`,`CURLOPT_WRITEFUNCTION`,`CURLOPT_XFERINFODATA`}
    WordLists[20][7][1177..1181] = {`CURLOPT_XFERINFOFUNCTION`,`CURLPROXY_HTTP`,`CURLPROXY_HTTP_1_0`,`CURLPROXY_SOCKS4`,`CURLPROXY_SOCKS5`}
    WordLists[20][7][1182..1186] = {`CURLPROXY_SOCKS4A`,`CURLPROXY_SOCKS5_HOSTNAME`,`CURLSHOPT_SHARE`,`CURLSHOPT_UNSHARE`,`CURLSHOPT_LOCKFUNC`}
    WordLists[20][7][1187..1190] = {`CURLSHOPT_UNLOCKFUNC`,`CURLSHOPT_USERDATA`,`NOPOLL_UNKNOWN_OP_CODE`,`NOPOLL_CONTINUATION_FRAME`}
    WordLists[20][7][1191..1195] = {`NOPOLL_TEXT_FRAME`,`NOPOLL_BINARY_FRAME`,`NOPOLL_CLOSE_FRAME`,`NOPOLL_PING_FRAME`,`NOPOLL_PONG_FRAME`}
    WordLists[20][7][1196..1202] = {`NOPOLL_EWOULDBLOCK`,`EWOULD_BLOCK`,`NO_VALUE`,`JSON_ARRAY`,`JSON_INVALID`,`JSON_KEYWORD`,`JSON_OBJECT`}
    WordLists[20][7][1203..1209] = {`XML_DOCUMENT`,`XML_PROLOGUE`,`XML_CONTENTS`,`XML_EPILOGUE`,`XML_DOCLEN`,`XML_TAGNAME`,`XML_ATTRIBUTES`}
    WordLists[20][7][1210..1216] = {`XML_ELEMLEN`,`XML_ATTRNAMES`,`XML_ATTRVALUES`,`XML_DECODE`,`XML_ENCODE`,`CRASHFATAL`,`HTML_ATTRIBS`}
    WordLists[20][7][1217..1220] = {`HTML_CONTENTS`,`HTML_INPUT`,`HTML_TAGNAME`,`GDK_COLORSPACE_RGB`}
    WordLists[20][8][1..7] = {`all_palette`,`allocate_low`,`cursor`,`display_image`,`dos_interrupt`,`ellipse`,`get_active_page`}
    WordLists[20][8][8..14] = {`get_all_palette`,`get_display_page`,`get_mouse`,`free_low`,`lock_memory`,`mouse_events`,`mouse_pointer`}
    WordLists[20][8][15..22] = {`polygon`,`save_screen`,`scroll`,`set_active_page`,`set_display_page`,`sound`,`text_rows`,`tick_rate`}
    WordLists[20][8][23..30] = {`use_vesa`,`wrap`,`abs`,`accept`,`allocate`,`allocate_string`,`allocate_word`,`allocate_wstring`}
    WordLists[20][8][31..40] = {`allow_break`,`any_key`,`apply`,`arccos`,`arcsin`,`assert`,`asserteq`,`atan2`,`average`,`bankers_rounding`}
    WordLists[20][8][41..48] = {`binary_search`,`bind`,`bits_to_int`,`bytes_to_int`,`call_back`,`ceil`,`check_all_blocks`,`check_break`}
    WordLists[20][8][49..55] = {`check_calls`,`check_free_list`,`choose`,`clear_directory`,`closesocket`,`columnize`,`combinations`}
    WordLists[20][8][56..62] = {`combinations_with_repetitions`,`command_line`,`connect`,`copy_directory`,`copy_file`,`count_bits`,`crash`}
    WordLists[20][8][63..70] = {`crash_file`,`crash_message`,`crash_routine`,`create_directory`,`current_dir`,`custom_sort`,`c_func`,`c_name`}
    WordLists[20][8][71..78] = {`c_proc`,`date`,`db_close`,`db_compress`,`db_create`,`db_create_table`,`db_current_table`,`db_delete_record`}
    WordLists[20][8][79..85] = {`db_delete_table`,`db_dump`,`db_find_key`,`db_insert`,`db_open`,`db_record_data`,`db_record_key`}
    WordLists[20][8][86..91] = {`db_rename_table`,`db_replace_data`,`db_select`,`db_select_table`,`db_table_list`,`db_table_size`}
    WordLists[20][8][92..99] = {`db_fatal_id`,`delete`,`delete_file`,`delete_routine`,`dict_name`,`dict_size`,`difference`,`dir`}
    WordLists[20][8][100..107] = {`discard_set`,`display_text_image`,`edges_only`,`elapsed`,`elapsed_short`,`end_struct`,`eval`,`extend_struct`}
    WordLists[20][8][108..115] = {`factorial`,`factors`,`factor_count`,`factor_sum`,`fetch_field`,`field_dx`,`file_exists`,`find_all`}
    WordLists[20][8][116..124] = {`find_any`,`find_replace`,`flatten`,`for_each`,`gcd`,`get`,`get_bytes`,`get_management`,`get_maxprime`}
    WordLists[20][8][125..131] = {`get_members`,`get_prime`,`get_primes`,`get_primes_le`,`get_proper_dir`,`get_proper_path`,`get_routine_info`}
    WordLists[20][8][132..137] = {`get_possible_constant_names`,`get_screen_char`,`get_set_management`,`get_socket_error`,`get_text`,`get_tzid`}
    WordLists[20][8][138..148] = {`getenv`,`gethostbyname`,`getsockaddr`,`getsockname`,`getsockport`,`gmatch`,`gsub`,`has`,`hash`,`hcf`,`head`}
    WordLists[20][8][149..156] = {`hll_and_bits`,`hll_append`,`hll_arctan`,`hll_atom`,`hll_compare`,`hll_cos`,`hll_equal`,`hll_even`}
    WordLists[20][8][157..165] = {`hll_floor`,`hll_integer`,`hll_length`,`hll_ln`,`hll_log`,`hll_not_bits`,`hll_object`,`hll_odd`,`hll_or_bits`}
    WordLists[20][8][166..173] = {`hll_power`,`hll_prepend`,`hll_rand`,`hll_remainder`,`hll_repeat`,`hll_rmdr`,`hll_sequence`,`hll_sin`}
    WordLists[20][8][174..181] = {`hll_sqrt`,`hll_string`,`hll_tan`,`hll_xor_bits`,`hmac_digest`,`hmac_sha1`,`hmac_sha256`,`hmac_sha512`}
    WordLists[20][8][182..189] = {`htonl`,`htons`,`include_file`,`include_files`,`include_path`,`include_paths`,`inet_addr`,`insert`}
    WordLists[20][8][190..197] = {`int_to_bits`,`int_to_bytes`,`intersect`,`intersection`,`ip_to_string`,`is_empty`,`is_leap_year`,`is_member`}
    WordLists[20][8][198..207] = {`is_prime`,`is_prime2`,`is_struct`,`is_subset`,`is_superset`,`join`,`join_by`,`join_path`,`k_perm`,`keys`}
    WordLists[20][8][208..218] = {`largest`,`lcm`,`listen`,`load_map`,`log10`,`log2`,`lower`,`machine_func`,`machine_proc`,`map`,`match_all`}
    WordLists[20][8][219..228] = {`match_any`,`match_replace`,`max`,`maxsq`,`maybe_any_key`,`message_box`,`min`,`minsq`,`mod`,`move_file`}
    WordLists[20][8][229..240] = {`mulmod`,`named_dict`,`ntohl`,`ntohs`,`or_all`,`ord`,`ordinal`,`ordinant`,`pad`,`pad_head`,`pad_tail`,`pairs`}
    WordLists[20][8][241..250] = {`peek2s`,`peek2u`,`peek_string`,`peek_wstring`,`phi`,`poke_string`,`poke_wstring`,`powmod`,`pp`,`ppEx`}
    WordLists[20][8][251..260] = {`ppExf`,`ppf`,`ppOpt`,`pretty_print`,`prime_factors`,`prime_powers`,`print`,`printf`,`product`,`progress`}
    WordLists[20][8][261..269] = {`prompt_number`,`prompt_string`,`proper`,`put`,`put_screen_char`,`read_bitmap`,`read_lines`,`recv`,`regex`}
    WordLists[20][8][270..276] = {`regex_compile`,`regex_options`,`register_block`,`remove`,`remove_all`,`remove_directory`,`rename_file`}
    WordLists[20][8][277..287] = {`replace`,`reverse`,`rnd`,`routine_id`,`save_map`,`scanf`,`select`,`send`,`series`,`setsockopt`,`shift_bits`}
    WordLists[20][8][288..297] = {`shorten`,`shuffle`,`shutdown`,`smallest`,`sockaddr_in`,`socket`,`sort`,`sort_columns`,`splice`,`save_bitmap`}
    WordLists[20][8][298..306] = {`save_text_image`,`setenv`,`splice`,`split`,`split_any`,`split_by`,`split_path`,`sprint`,`sprintf`}
    WordLists[20][8][307..311] = {`still_has_delete_routine`,`store_field`,`store_field_element`,`strict_html_parse`,`struct_add_field`}
    WordLists[20][8][312..320] = {`struct_dx`,`struct_start`,`sum`,`system`,`system_exec`,`tagset`,`tagstart`,`tail`,`task_clock_start`}
    WordLists[20][8][321..327] = {`task_clock_stop`,`task_create`,`task_delay`,`task_list`,`task_schedule`,`task_self`,`task_status`}
    WordLists[20][8][328..336] = {`task_suspend`,`task_yield`,`to_string`,`trim`,`trim_head`,`trim_tail`,`trunc`,`unregister_block`,`upper`}
    WordLists[20][8][337..344] = {`valid_index`,`value`,`values`,`video_config`,`vlookup`,`walk_dir`,`wildcard_file`,`wildcard_match`}
    WordLists[20][8][345..351] = {`write_lines`,`WSACleanup`,`WSAStartup`,`add_member`,`add_members`,`adjust_timedate`,`allow_novalue`}
    WordLists[20][8][352..361] = {`b_a_exp`,`b_a_log`,`b_a_sqrt`,`ba_abs`,`ba_add`,`ba_ceil`,`ba_compare`,`ba_div`,`ba_divide`,`ba_euler`}
    WordLists[20][8][362..371] = {`ba_exp`,`ba_idiv`,`ba_idivide`,`ba_factorial`,`ba_floor`,`ba_frac`,`ba_gcd`,`ba_lcm`,`ba_log`,`ba_log10`}
    WordLists[20][8][372..380] = {`ba_logb`,`ba_mod`,`ba_mod_exp`,`ba_mul`,`ba_multiply`,`ba_new`,`ba_power`,`ba_print`,`ba_printf`}
    WordLists[20][8][381..389] = {`ba_remainder`,`ba_root`,`ba_scale`,`ba_scale_of`,`ba_sign`,`ba_sprint`,`ba_sqrt`,`ba_sub`,`ba_trunc`}
    WordLists[20][8][390..397] = {`ba_uminus`,`ba_round`,`ba_sprintf`,`beep`,`bigatom`,`bigatom_to_atom`,`BZ2_desc`,`BZ2_bzCompressInit`}
    WordLists[20][8][398..402] = {`BZ2_bzCompress`,`BZ2_bzCompressEnd`,`BZ2_bzDecompressInit`,`BZ2_bzDecompress`,`BZ2_bzDecompressEnd`}
    WordLists[20][8][403..409] = {`call_lambda`,`change_timezone`,`complex_new`,`complex_real`,`complex_imag`,`complex_norm`,`complex_abs`}
    WordLists[20][8][410..416] = {`complex_add`,`complex_neg`,`complex_sub`,`complex_conjugate`,`complex_mul`,`complex_inv`,`complex_div`}
    WordLists[20][8][417..424] = {`complex_arg`,`complex_theta`,`complex_rho`,`from_polar`,`with_theta`,`with_rho`,`complex_ln`,`complex_log`}
    WordLists[20][8][425..431] = {`complex_exp`,`complex_power`,`complex_sqrt`,`complex_sinh`,`complex_cosh`,`complex_sin`,`complex_cos`}
    WordLists[20][8][432..438] = {`complex_round`,`complex_sprint`,`create_thread`,`CURLcode`,`CURLMcode`,`CURLMoption`,`CURLSHcode`}
    WordLists[20][8][439..443] = {`CURLSHoption`,`CURLoption`,`curl_easy_cleanup`,`curl_easy_duphandle`,`curl_easy_get_file`}
    WordLists[20][8][444..448] = {`curl_easy_getinfo`,`curl_easy_init`,`curl_easy_perform`,`curl_easy_perform_ex`,`curl_easy_recv`}
    WordLists[20][8][449..453] = {`curl_easy_reset`,`curl_easy_send`,`curl_easy_setopt`,`curl_easy_strerror`,`curl_global_cleanup`}
    WordLists[20][8][454..458] = {`curl_global_init`,`curl_loadlib`,`curl_lock_access`,`curl_multi_add_handle`,`curl_multi_cleanup`}
    WordLists[20][8][459..463] = {`curl_multi_info_read`,`curl_multi_init`,`curl_multi_perform`,`curl_multi_remove_handle`,`curl_multi_setopt`}
    WordLists[20][8][464..468] = {`curl_multi_strerror`,`curl_multi_timeout`,`curl_multi_wait`,`curl_share_cleanup`,`curl_share_data`}
    WordLists[20][8][469..473] = {`curl_share_init`,`curl_share_setopt`,`curl_share_strerror`,`curl_slist_append`,`curl_slist_free_all`}
    WordLists[20][8][474..480] = {`curl_version`,`curl_version_info`,`day_of_week`,`day_of_year`,`days_in_month`,`decode_base64`,`decode_flags`}
    WordLists[20][8][481..487] = {`decode_url`,`deep_copy`,`define_cffi_func`,`define_cffi_proc`,`define_lambda`,`deld`,`delete_cs`}
    WordLists[20][8][488..495] = {`destroy_dict`,`destroy_queue`,`destroy_stack`,`encode_base64`,`ends`,`enter_cs`,`even`,`exit_thread`}
    WordLists[20][8][496..502] = {`extract`,`extract_json_field`,`file_size_k`,`filter`,`filter_count`,`format_timedate`,`g_object_unref`}
    WordLists[20][8][503..506] = {`gdk_pixbuf_get_width`,`gdk_pixbuf_get_height`,`gdk_pixbuf_get_rowstride`,`gdk_pixbuf_get_has_alpha`}
    WordLists[20][8][507..509] = {`gdk_pixbuf_get_colorspace`,`gdk_pixbuf_get_n_channels`,`gdk_pixbuf_get_bits_per_sample`}
    WordLists[20][8][510..516] = {`gdk_pixbuf_get_pixels`,`getd`,`getdd`,`getd_all_keys`,`getd_index`,`getd_by_index`,`getd_partial_key`}
    WordLists[20][8][517..522] = {`get_field_default`,`get_field_flags`,`get_field_type`,`get_file_base`,`get_file_date`,`get_file_extension`}
    WordLists[20][8][523..527] = {`get_file_name`,`get_file_name_and_path`,`get_file_path`,`get_file_path_and_name`,`get_file_size`}
    WordLists[20][8][528..533] = {`get_file_type`,`get_interpreter`,`get_logical_drives`,`get_rand`,`get_struct_fields`,`get_struct_flags`}
    WordLists[20][8][534..539] = {`get_struct_name`,`get_struct_type`,`get_thread_exitcode`,`get_thread_id`,`ini_as_string`,`ini_delete_key`}
    WordLists[20][8][540..548] = {`ini_filename`,`ini_keynames`,`ini_load`,`ini_restore`,`ini_save`,`ini_set`,`ini_setting`,`init_cs`,`is_dict`}
    WordLists[20][8][549..558] = {`is_inf`,`is_integer`,`is_nan`,`isatty`,`islower`,`isupper`,`leave_cs`,`map_get`,`median`,`mpfr`}
    WordLists[20][8][559..562] = {`mpir_open_dll`,`mpfr_get_versions`,`mpfr_get_default_precision`,`mpfr_set_default_precision`}
    WordLists[20][8][563..567] = {`mpfr_get_default_rounding_mode`,`mpfr_set_default_rounding_mode`,`mpfr_init`,`mpfr_inits`,`mpfr_init_set`}
    WordLists[20][8][568..574] = {`mpfr_init_set_q`,`mpfr_init_set_z`,`mpfr_set_d`,`mpfr_set_si`,`mpfr_set_str`,`mpfr_set_q`,`mpfr_set_z`}
    WordLists[20][8][575..580] = {`mpfr_set`,`mpfr_free`,`mpfr_get_precision`,`mpfr_set_precision`,`mpfr_const_euler`,`mpfr_const_pi`}
    WordLists[20][8][581..588] = {`mpfr_get_str`,`mpfr_get_fixed`,`mpfr_sprintf`,`mpfr_printf`,`mpfr_floor`,`mpfr_ceil`,`mpfr_abs`,`mpfr_neg`}
    WordLists[20][8][589..596] = {`mpfr_add`,`mpfr_add_d`,`mpfr_add_si`,`mpfr_addmul_si`,`mpfr_sub`,`mpfr_sub_d`,`mpfr_sub_si`,`mpfr_si_sub`}
    WordLists[20][8][597..604] = {`mpfr_mul`,`mpfr_mul_d`,`mpfr_mul_si`,`mpfr_mul_z`,`mpfr_div`,`mpfr_div_d`,`mpfr_div_si`,`mpfr_div_z`}
    WordLists[20][8][605..612] = {`mpfr_si_div`,`mpfr_fmod`,`mpfr_sqr`,`mpfr_sqrt`,`mpfr_sqrt_ui`,`mpfr_pow`,`mpfr_pow_si`,`mpfr_ui_pow`}
    WordLists[20][8][613..620] = {`mpfr_ui_pow_ui`,`mpfr_si_pow_si`,`mpfr_neg`,`mpfr_sin`,`mpfr_log`,`mpfr_exp`,`mpfr_gamma`,`mpfr_gamma_inc`}
    WordLists[20][8][621..628] = {`mpfr_zeta`,`mpfr_zeta_ui`,`mpfr_get_d_2exp`,`mpfr_get_si`,`mpfr_get_d`,`mpfr_cmp`,`mpfr_cmp_si`,`mpq`}
    WordLists[20][8][629..635] = {`mpq_init`,`mpq_inits`,`mpq_init_set`,`mpq_init_set_z`,`mpq_init_set_si`,`mpq_init_set_str`,`mpq_set`}
    WordLists[20][8][636..643] = {`mpq_set_z`,`mpq_set_si`,`mpq_set_str`,`mpq_free`,`mpq_get_d`,`mpq_get_num`,`mpq_get_den`,`mpq_get_str`}
    WordLists[20][8][644..652] = {`mpq_add`,`mpq_add_si`,`mpq_sub`,`mpq_mul`,`mpq_div`,`mpq_mul_2exp`,`mpq_div_2exp`,`mpq_neg`,`mpq_abs`}
    WordLists[20][8][653..660] = {`mpq_inv`,`mpq_canonicalize`,`mpq_cmp`,`mpq_cmp_si`,`mpz`,`mpz_or_string`,`mpz_bin_uiui`,`mpz_init`}
    WordLists[20][8][661..668] = {`mpz_inits`,`mpz_init_set`,`mpz_free`,`mpz_set`,`mpz_set_si`,`mpz_set_d`,`mpz_set_q`,`mpz_set_str`}
    WordLists[20][8][669..676] = {`mpz_set_v`,`mpz_import`,`mpz_export`,`mpz_add`,`mpz_add_ui`,`mpz_add_si`,`mpz_addmul`,`mpz_addmul_ui`}
    WordLists[20][8][677..684] = {`mpz_addmul_si`,`mpz_and`,`mpz_sub`,`mpz_sub_ui`,`mpz_ui_sub`,`mpz_sub_si`,`mpz_si_sub`,`mpz_submul`}
    WordLists[20][8][685..692] = {`mpz_submul_ui`,`mpz_submul_si`,`mpz_abs`,`mpz_neg`,`mpz_nextprime`,`mpz_mod`,`mpz_mod_ui`,`mpz_divexact`}
    WordLists[20][8][693..700] = {`mpz_divexact_ui`,`mpz_fdiv_ui`,`mpz_mul`,`mpz_mul_d`,`mpz_mul_si`,`mpz_mul_2exp`,`mpz_fdiv_q`,`mpz_fdiv_r`}
    WordLists[20][8][701..706] = {`mpz_fdiv_qr`,`mpz_fdiv_q_ui`,`mpz_fdiv_q_2exp`,`mpz_tdiv_q_2exp`,`mpz_tdiv_r_2exp`,`mpz_cdiv_q`}
    WordLists[20][8][707..712] = {`mpz_divisible_p`,`mpz_divisible_ui_p`,`mpz_divisible_2exp_p`,`mpz_fib_ui`,`mpz_fib2_ui`,`mpz_cmp`}
    WordLists[20][8][713..721] = {`mpz_cmp_si`,`mpz_min`,`mpz_max`,`mpz_sign`,`mpz_odd`,`mpz_even`,`mpz_scan0`,`mpz_scan1`,`mpz_tstbit`}
    WordLists[20][8][722..729] = {`mpz_pow_ui`,`mpz_powm`,`mpz_powm_ui`,`mpz_remove`,`mpz_root`,`mpz_nthroot`,`mpz_sqrt`,`mpz_sqrtrem`}
    WordLists[20][8][730..737] = {`mpz_ui_pow_ui`,`mpz_xor`,`mpz_popcount`,`mpz_fits_integer`,`mpz_fits_atom`,`mpz_gcd`,`mpz_gcd_ui`,`mpz_lcm`}
    WordLists[20][8][738..744] = {`mpz_lcm_ui`,`mpz_invert`,`mpz_fac_ui`,`mpz_get_integer`,`mpz_get_atom`,`mpz_size`,`mpz_sizeinbase`}
    WordLists[20][8][745..751] = {`mpz_get_short_str`,`mpz_get_str`,`mpz_prime`,`mpz_prime_factors`,`mpz_factorstring`,`mpz_rand`,`mpz_rand_ui`}
    WordLists[20][8][752..759] = {`mpz_re_compose`,`mpz_pollard_rho`,`mpz_factors`,`randstate`,`new`,`new_dict`,`new_dicts`,`new_queue`}
    WordLists[20][8][760..765] = {`new_set`,`new_stack`,`new_map`,`new_ZIPENTRY`,`nopoll_conn_close`,`nopoll_conn_close_ext`}
    WordLists[20][8][766..768] = {`nopoll_conn_complete_pending_write`,`nopoll_conn_connect_timeout`,`nopoll_conn_flush_writes`}
    WordLists[20][8][769..771] = {`nopoll_conn_get_accepted_protocol`,`nopoll_conn_get_connect_timeout`,`nopoll_conn_get_msg`}
    WordLists[20][8][772..775] = {`nopoll_conn_is_ok`,`nopoll_conn_is_ready`,`nopoll_conn_new`,`nopoll_conn_pending_write_bytes`}
    WordLists[20][8][776..779] = {`nopoll_conn_read`,`nopoll_conn_read_pending`,`nopoll_conn_send_text`,`nopoll_conn_send_text_fragment`}
    WordLists[20][8][780..783] = {`nopoll_conn_set_on_msg`,`nopoll_conn_wait_until_connection_ready`,`nopoll_ctx_new`,`nopoll_ctx_set_on_msg`}
    WordLists[20][8][784..787] = {`nopoll_loop_stop`,`nopoll_loop_wait`,`nopoll_msg_get_payload`,`nopoll_msg_get_payload_size`}
    WordLists[20][8][788..793] = {`nopoll_msg_opcode`,`nopoll_msg_opcode_desc`,`nopoll_sleep`,`nopoll_thread_handlers`,`odd`,`papply`}
    WordLists[20][8][794..803] = {`parse_date_string`,`parse_json`,`parse_url`,`permute`,`permutes`,`peep`,`peepn`,`peep_dict`,`pop`,`popn`}
    WordLists[20][8][804..813] = {`pop_dict`,`pq_new`,`pq_size`,`pq_empty`,`pq_add`,`pq_pop`,`pq_pop_data`,`pq_peek`,`pq_destroy`,`print_json`}
    WordLists[20][8][814..820] = {`pushn`,`putd`,`queue_empty`,`queue_size`,`rasterize_svg_file`,`rasterize_svg_pixbuf`,`rasterize_svg_text`}
    WordLists[20][8][821..828] = {`rid_string`,`reinstate`,`remove_dups`,`remove_member`,`remove_members`,`requires`,`resume_thread`,`round`}
    WordLists[20][8][829..835] = {`setd`,`setd_default`,`set_captures`,`set_field_default`,`set_file_date`,`set_file_size`,`set_ignore_atom`}
    WordLists[20][8][836..841] = {`set_librsvg_dir`,`set_management`,`set_mb_hwnd`,`set_size`,`set_system_doevents`,`set_test_abort`}
    WordLists[20][8][842..846] = {`get_test_abort`,`set_test_logfile`,`get_test_logfile`,`set_test_module`,`set_test_section`}
    WordLists[20][8][847..851] = {`set_test_verbosity`,`get_test_verbosity`,`set_timedate_formats`,`set_timezone`,`set_unicode`}
    WordLists[20][8][852..860] = {`set_test_pause`,`get_test_pause`,`get_tests_failed`,`sha1`,`sha256`,`sha512`,`sign`,`sizeof`,`sq_abs`}
    WordLists[20][8][861..870] = {`sq_add`,`sq_and`,`sq_and_bits`,`sq_arccos`,`sq_arcsin`,`sq_arctan`,`sq_atom`,`sq_cmp`,`sq_cos`,`sq_div`}
    WordLists[20][8][871..881] = {`sq_even`,`sq_eq`,`sq_floor`,`sq_floor_div`,`sq_ge`,`sq_gt`,`sq_int`,`sq_le`,`sq_ln`,`sq_log`,`sq_log10`}
    WordLists[20][8][882..892] = {`sq_log2`,`sq_lower`,`sq_lt`,`sq_max`,`sq_min`,`sq_mod`,`sq_mul`,`sq_ne`,`sq_not`,`sq_not_bits`,`sq_odd`}
    WordLists[20][8][893..902] = {`sq_or`,`sq_or_bits`,`sq_power`,`sq_rand`,`sq_remainder`,`sq_rmdr`,`sq_round`,`sq_ceil`,`sq_sign`,`sq_trunc`}
    WordLists[20][8][903..912] = {`sq_seq`,`sq_sin`,`sq_sqrt`,`sq_str`,`sq_sub`,`sq_tan`,`sq_uminus`,`sq_upper`,`sq_xor`,`sq_xor_bits`}
    WordLists[20][8][913..916] = {`sqlite3_bind_blob`,`sqlite3_bind_double`,`sqlite3_bind_int`,`sqlite3_bind_null`}
    WordLists[20][8][917..919] = {`sqlite3_bind_parameter_count`,`sqlite3_bind_parameter_index`,`sqlite3_bind_parameter_name`}
    WordLists[20][8][920..924] = {`sqlite3_bind_text`,`sqlite3_changes`,`sqlite3_close`,`sqlite3_column_blob`,`sqlite3_column_count`}
    WordLists[20][8][925..928] = {`sqlite3_column_decltype`,`sqlite3_column_double`,`sqlite3_column_int`,`sqlite3_column_name`}
    WordLists[20][8][929..933] = {`sqlite3_column_text`,`sqlite3_column_type`,`sqlite3_data_count`,`sqlite3_exec`,`sqlite3_finalize`}
    WordLists[20][8][934..938] = {`sqlite3_get_autocommit`,`sqlite3_get_table`,`sqlite3_last_insert_rowid`,`sqlite3_libversion`,`sqlite3_open`}
    WordLists[20][8][939..943] = {`sqlite3_open_dll`,`sqlite3_prepare`,`sqlite3_reset`,`sqlite3_set_fatal_id`,`sqlite3_step`}
    WordLists[20][8][944..950] = {`sqlite3_total_changes`,`square_free`,`stack_empty`,`stack_size`,`struct_mem`,`substitute`,`substitute_all`}
    WordLists[20][8][951..958] = {`suspend_thread`,`system_wait`,`system_open`,`tagset`,`temp_file`,`test_equal`,`test_fail`,`test_false`}
    WordLists[20][8][959..965] = {`test_not_equal`,`test_pass`,`test_summary`,`test_true`,`thread_safe_string`,`timedate_diff`,`timedelta`}
    WordLists[20][8][966..973] = {`to_integer`,`to_number`,`traverse_dict`,`traverse_dict_partial_key`,`try_cs`,`unique`,`union`,`unix_dict`}
    WordLists[20][8][974..979] = {`UnzipClose`,`UnzipFindItem`,`UnzipGetFileName`,`UnzipGetItem`,`UnzipGetItems`,`UnzipItemToFile`}
    WordLists[20][8][980..985] = {`UnzipOpenFile`,`UnzipSetBaseDir`,`url_element_desc`,`utf8_to_utf16`,`utf8_to_utf32`,`utf16_to_utf8`}
    WordLists[20][8][986..992] = {`utf16_to_utf32`,`utf32_to_utf8`,`utf32_to_utf16`,`wait_thread`,`week_number`,`xml_decode`,`xml_encode`}
    WordLists[20][8][993..998] = {`xml_parse`,`xml_sprint`,`xml_new_doc`,`xml_new_element`,`xml_get_attribute`,`xml_set_attribute`}
    WordLists[20][8][999..1005] = {`xml_get_nodes`,`xml_add_comment`,`ZipAddDir`,`ZipAddFile`,`ZipAddFolder`,`ZipClose`,`ZipCreateFile`}
    WordLists[20][8][1006..1010] = {`ZIPENTRY`,`adjustDibBrightness`,`adjustDibBrightnessAndContrast`,`adjustDibColors`,`adjustDibContrast`}
    WordLists[20][8][1011..1018] = {`blurDib`,`clearDib`,`clipToSource`,`colorizeDib`,`copyDib`,`copyDibToClipboard`,`copyDibToDib`,`copyToDib`}
    WordLists[20][8][1019..1024] = {`detectDibEdges`,`dibColor`,`drawDib`,`drawDibToDib`,`drawDibTransformedPolygon`,`drawShadedPolygonToDib`}
    WordLists[20][8][1025..1031] = {`embossDib`,`extractDib`,`fastGetDibPixel`,`fastPutDibPixel`,`filterDib`,`filterDib3x3`,`filterDibGray`}
    WordLists[20][8][1032..1038] = {`filterDibGray3x3`,`flipDibHor`,`flipDibVert`,`getDibFromClipboard`,`getDibPixel`,`getHwnd`,`invertDib`}
    WordLists[20][8][1039..1046] = {`killDib`,`newDib`,`loadDib`,`makeDibGray`,`putDibPixel`,`replaceDibColor`,`replaceDibColors`,`rotateDib`}
    WordLists[20][8][1047..1053] = {`rotateDibFree`,`saveDib`,`saveDibGray`,`saveDibReduced`,`scaleDib`,`scaleDibPct`,`sharpenDib`}
    WordLists[20][8][1054..1059] = {`subtleBlurDib`,`subtleSharpenDib`,`tileDibToDib`,`allocate_struct`,`define_struct`,`set_struct_field`}
    WordLists[20][8][1060..1065] = {`get_struct_field`,`get_struct_size`,`get_struct_string`,`get_field_details`,`deserialize`,`serialize`}
    WordLists[20][8][1066..1073] = {`sm_alloc_lpsz`,`sm_close`,`sm_create`,`sm_open`,`cdCanvan`,`cdCanvas`,`cdCanvasActivate`,`cdCanvasArc`}
    WordLists[20][8][1074..1079] = {`wdCanvasArc`,`cdCanvasGetBackOpacity`,`cdCanvasSetBackOpacity`,`cdCanvasBegin`,`cdCanvasBox`,`wdCanvasBox`}
    WordLists[20][8][1080..1085] = {`cdCanvasChord`,`wdCanvasChord`,`cdCanvasCircle`,`cdCanvasClear`,`cdCanvasClip`,`cdCanvasClipArea`}
    WordLists[20][8][1086..1090] = {`wdCanvasClipArea`,`wdCanvasGetClipArea`,`cdCanvasGetClipArea`,`cdCanvasDeactivate`,`cdCanvasEnd`}
    WordLists[20][8][1091..1095] = {`cdCanvasFlush`,`cdCanvasFont`,`cdCanvasGetAttribute`,`cdCanvasGetBackground`,`cdCanvasGetColorPlanes`}
    WordLists[20][8][1096..1100] = {`cdCanvasGetContext`,`cdCanvasGetFillMode`,`cdCanvasGetFont`,`cdCanvasGetFontDim`,`cdCanvasGetForeground`}
    WordLists[20][8][1101..1104] = {`cdCanvasGetHatch`,`cdCanvasGetInteriorStyle`,`cdCanvasGetImageRGB`,`cdCanvasGetLineStyle`}
    WordLists[20][8][1105..1108] = {`cdCanvasGetLineWidth`,`cdCanvasGetOrigin`,`cdCanvasGetNativeFont`,`cdCanvasGetRegionBox`}
    WordLists[20][8][1109..1113] = {`wdCanvasGetRegionBox`,`cdCanvasGetSize`,`cdCanvasGetStipple`,`cdCanvasGetPattern`,`cdCanvasGetTextAlignment`}
    WordLists[20][8][1114..1117] = {`cdCanvasGetTextBounds`,`cdCanvasGetTextBox`,`cdCanvasGetTextSize`,`cdCanvasGetTransform`}
    WordLists[20][8][1118..1121] = {`cdCanvasGetVectorTextBounds`,`cdCanvasGetVectorTextSize`,`cdCanvasInvertYAxis`,`cdCanvasIsPointInRegion`}
    WordLists[20][8][1122..1126] = {`wdCanvasIsPointInRegion`,`cdCanvasLine`,`wdCanvasLine`,`cdCanvasSetLineStyle`,`cdCanvasGetLineStyle`}
    WordLists[20][8][1127..1130] = {`cdCanvasSetWriteMode`,`cdCanvasGetWriteMode`,`cdCanvasLineStyleDashes`,`cdCanvasLineSetWidth`}
    WordLists[20][8][1131..1135] = {`cdCanvasLineGetWidth`,`cdCanvasGetLineJoin`,`cdCanvasSetLineJoin`,`cdCanvasSetLineCap`,`cdCanvasGetLineCap`}
    WordLists[20][8][1136..1141] = {`cdCanvasMark`,`wdCanvasMark`,`cdCanvasMarkType`,`cdCanvasMarkSize`,`wdCanvasMarkSize`,`cdCanvasMM2Pixel`}
    WordLists[20][8][1142..1145] = {`cdCanvasMultiLineVectorText`,`wdCanvasMultiLineVectorText`,`cdCanvasOffsetRegion`,`wdCanvasOffsetRegion`}
    WordLists[20][8][1146..1151] = {`cdCanvasOrigin`,`cdCanvasPalette`,`cdCanvasPathSet`,`cdCanvasPixel`,`wdCanvasPixel`,`cdCanvasPixel2MM`}
    WordLists[20][8][1152..1155] = {`cdCanvasPlay`,`cdCanvasPutImageRectRGB`,`wdCanvasPutImageRectRGB`,`cdCanvasPutImageRectRGBA`}
    WordLists[20][8][1156..1159] = {`wdCanvasPutImageRectRGBA`,`cdCanvasPutImageRectMap`,`wdCanvasPutImageRectMap`,`cdCanvasPutImImage`}
    WordLists[20][8][1160..1164] = {`cdCanvasRect`,`wdCanvasRect`,`cdCanvasRegionCombineMode`,`cdCanvasRestoreState`,`cdCanvasRoundedBox`}
    WordLists[20][8][1165..1169] = {`cdCanvasRoundedRect`,`cdCanvasSaveState`,`cdCanvasSector`,`wdCanvasSector`,`cdCanvasSetAttribute`}
    WordLists[20][8][1170..1173] = {`cdCanvasSetBackground`,`cdCanvasSetFillMode`,`cdCanvasSetForeground`,`cdCanvasSetHatch`}
    WordLists[20][8][1174..1177] = {`cdCanvasSetInteriorStyle`,`cdCanvasSetLineStyle`,`cdCanvasSetLineWidth`,`cdCanvasSetNativeFont`}
    WordLists[20][8][1178..1182] = {`cdCanvasSetStipple`,`cdCanvasSetPattern`,`cdCanvasSetTextAlignment`,`cdCanvasSimulate`,`cdCanvasText`}
    WordLists[20][8][1183..1186] = {`wdCanvasText`,`cdCanvasSetTextOrientation`,`cdCanvasGetTextOrientation`,`cdCanvasTransform`}
    WordLists[20][8][1187..1190] = {`cdCanvasTransformMultiply`,`cdCanvasTransformPoint`,`cdCanvasTransformRotate`,`cdCanvasTransformScale`}
    WordLists[20][8][1191..1194] = {`cdCanvasTransformTranslate`,`cdCanvasUpdateYAxis`,`cdCanvasVectorCharSize`,`wdCanvasVectorCharSize`}
    WordLists[20][8][1195..1198] = {`cdCanvasVectorFont`,`cdCanvasVectorFontSize`,`cdCanvasGetVectorFontSize`,`cdCanvasVectorText`}
    WordLists[20][8][1199..1202] = {`wdCanvasVectorText`,`cdCanvasVectorTextDirection`,`wdCanvasVectorTextDirection`,`cdCanvasVectorTextSize`}
    WordLists[20][8][1203..1207] = {`wdCanvasVectorTextSize`,`cdCanvasVectorTextTransform`,`cdCanvasVertex`,`wdCanvasVertex`,`cdContext`}
    WordLists[20][8][1208..1212] = {`cd_context`,`cdContextCaps`,`cdContextRegisterCallback`,`cdCreateCanvas`,`cdEncodeColor`}
    WordLists[20][8][1213..1217] = {`cdEncodeColorAlpha`,`cdEncodeAlpha`,`cdDecodeColor`,`cdDecodeColorAlpha`,`cdDecodeAlpha`}
    WordLists[20][8][1218..1222] = {`cdGetScreenColorPlanes`,`cdGetScreenSize`,`cdInitContextPlus`,`cdKillCanvas`,`cdReleaseState`}
    WordLists[20][8][1223..1228] = {`cdUseContextPlus`,`cdVersion`,`cdVersionDate`,`cdVersionNumber`,`wdCanvasFont`,`wdCanvasGetFont`}
    WordLists[20][8][1229..1233] = {`wdCanvasGetFontDim`,`wdCanvasGetImageRGB`,`wdCanvasSetLineWidth`,`wdCanvasGetLineWidth`,`wdCanvasSetStipple`}
    WordLists[20][8][1234..1237] = {`wdCanvasSetPattern`,`wdCanvasGetTextBounds`,`wdCanvasGetTextBox`,`wdCanvasGetTextSize`}
    WordLists[20][8][1238..1241] = {`wdCanvasGetVectorTextBounds`,`wdCanvasGetVectorTextSize`,`wdCanvasGetViewport`,`wdCanvasGetWindow`}
    WordLists[20][8][1242..1246] = {`wdCanvasViewport`,`wdCanvasWindow`,`wdCanvasWorld2Canvas`,`wdCanvasWorld2CanvasSize`,`wdCanvasCanvas2World`}
    WordLists[20][8][1247..1255] = {`wdCanvasHardcopy`,`hsv_to_rgb`,`imImage`,`Ihandle`,`Ihandles`,`Ihandln`,`Ihandlns`,`Icallback`,`Icallbacki`}
    WordLists[20][8][1256..1263] = {`atom_string`,`cbfunc`,`nullable_string`,`im_color_space`,`im_data`,`im_depth`,`im_height`,`im_pixel`}
    WordLists[20][8][1264..1269] = {`im_width`,`imConvertColorSpace`,`imConvertPacking`,`imFileImageLoadBitmap`,`imFileImageSave`,`imImageClone`}
    WordLists[20][8][1270..1274] = {`imImageCreate`,`imImageCreateBased`,`imImageDestroy`,`imImageGetAttribString`,`imImageGetOpenGLData`}
    WordLists[20][8][1275..1279] = {`imImageRemoveAlpha`,`imImageSetAttribString`,`imProcessFlip`,`imProcessMirror`,`imProcessNegative`}
    WordLists[20][8][1280..1283] = {`imProcessRenderConstant`,`imProcessRenderFloodFill`,`imProcessResize`,`imProcessRotate180`}
    WordLists[20][8][1284..1289] = {`imProcessRotate90`,`imProcessToneGamut`,`iup_isalt`,`iup_isAltXkey`,`iup_iscontrol`,`iup_isCtrlXkey`}
    WordLists[20][8][1290..1296] = {`iup_isbutton1`,`iup_isbutton2`,`iup_isbutton3`,`iup_isbutton4`,`iup_isbutton5`,`iup_isdouble`,`iup_isprint`}
    WordLists[20][8][1297..1302] = {`iup_isshift`,`iup_isShiftXkey`,`iup_issys`,`iup_isSysXkey`,`iupKeyCodeToName`,`iup_name_from_cb`}
    WordLists[20][8][1303..1309] = {`iup_cb_from_name`,`iup_XkeyBase`,`IupAlarm`,`IupAppend`,`IupBackgroundBox`,`IupButton`,`IupCalendar`}
    WordLists[20][8][1310..1316] = {`IupCanvas`,`IupCbox`,`IupCells`,`IupCheckVCRuntime`,`IupClassInfoDialog`,`IupClassMatch`,`IupClipboard`}
    WordLists[20][8][1317..1323] = {`IupClose`,`IupCloseOnEscape`,`IupColorbar`,`IupColorBrowser`,`IupColorDlg`,`IupConfig`,`IupConfigCopy`}
    WordLists[20][8][1324..1327] = {`IupConfigDialogClosed`,`IupConfigDialogShow`,`IupConfigGetVariableDouble`,`IupConfigGetVariableDoubleId`}
    WordLists[20][8][1328..1331] = {`IupConfigGetVariableInt`,`IupConfigGetVariableIntId`,`IupConfigGetVariableStr`,`IupConfigGetVariableStrId`}
    WordLists[20][8][1332..1336] = {`IupConfigLoad`,`IupConfigRecentInit`,`IupConfigRecentUpdate`,`IupConfigSave`,`IupConfigSetVariableDouble`}
    WordLists[20][8][1337..1339] = {`IupConfigSetVariableDoubleId`,`IupConfigSetVariableInt`,`IupConfigSetVariableIntId`}
    WordLists[20][8][1340..1343] = {`IupConfigSetVariableStr`,`IupConfigSetVariableStrId`,`IupControlsOpen`,`IupConvertXYToPos`}
    WordLists[20][8][1344..1349] = {`IupCopyAttributes`,`IupCopyClassAttributes`,`IupCreate`,`IupDatePick`,`IupDestroy`,`IupDetach`}
    WordLists[20][8][1350..1356] = {`IupDetachBox`,`IupDial`,`IupDialog`,`IupDrawBegin`,`IupDrawEnd`,`IupDrawSetClipRect`,`IupDrawGetClipRect`}
    WordLists[20][8][1357..1362] = {`IupDrawResetClip`,`IupDrawParentBackground`,`IupDrawLine`,`IupDrawRectangle`,`IupDrawArc`,`IupDrawPolygon`}
    WordLists[20][8][1363..1368] = {`IupDrawText`,`IupDrawImage`,`IupDrawSelectRect`,`IupDrawFocusRect`,`IupDrawGetSize`,`IupDrawGetTextSize`}
    WordLists[20][8][1369..1374] = {`IupDrawGetImageInfo`,`IupDropButton`,`IupElementPropertiesDialog`,`IupExitLoop`,`IupExpander`,`IupFileDlg`}
    WordLists[20][8][1375..1381] = {`IupFill`,`IupFlatButton`,`IupFlatFrame`,`IupFlatLabel`,`IupFlatList`,`IupFlatSeparator`,`IupFlatTabs`}
    WordLists[20][8][1382..1389] = {`IupFlatToggle`,`IupFlatTree`,`IupFlatVal`,`IupFlatValuator`,`IupFlowBox`,`IupFlush`,`IupFontDlg`,`IupFrame`}
    WordLists[20][8][1390..1395] = {`IupGauge`,`IupGetAllAttributes`,`IupGetAllClasses`,`IupGetAllDialogs`,`IupGetAllNames`,`IupGetAttribute`}
    WordLists[20][8][1396..1399] = {`IupGetAttributeHandle`,`IupGetAttributeHandleId`,`IupGetAttributeHandleId2`,`IupGetAttributeId`}
    WordLists[20][8][1400..1405] = {`IupGetAttributeId2`,`IupGetAttributePtr`,`IupGetBrother`,`IupGetCallback`,`IupGetChild`,`IupGetChildCount`}
    WordLists[20][8][1406..1410] = {`IupGetChildPos`,`IupGetClassAttributes`,`IupGetClassCallbacks`,`IupGetClassName`,`IupGetClassType`}
    WordLists[20][8][1411..1416] = {`IupGetColor`,`IupGetDialog`,`IupGetDialogChild`,`IupGetFile`,`IupGetFocus`,`IupGetGlobal`}
    WordLists[20][8][1417..1422] = {`IupGetGlobalFunction`,`IupGetGlobalInt`,`IupGetGlobalIntInt`,`IupGetHandle`,`IupGetInt`,`IupGetInt2`}
    WordLists[20][8][1423..1429] = {`IupGetIntInt`,`IupGetIntId`,`IupGetIntId2`,`IupGetFloat`,`IupGetFloatId`,`IupGetFloatId2`,`IupGetDouble`}
    WordLists[20][8][1430..1434] = {`IupGetDoubleId`,`IupGetDoubleId2`,`IupGetLanguage`,`IupGetLanguageString`,`IupGetName`}
    WordLists[20][8][1435..1439] = {`IupGetNativeHandleImage`,`IupGetImageNativeHandle`,`IupGetNextChild`,`IupGetRGB`,`IupGetRGBId`}
    WordLists[20][8][1440..1446] = {`IupGetRGBId2`,`IupGetParam`,`IupGetParent`,`IupGetText`,`IupGLBackgroundBox`,`IupGLButton`,`IupGLCanvas`}
    WordLists[20][8][1447..1451] = {`IupGLCanvasBox`,`IupGLCanvasOpen`,`IupGLControlsOpen`,`IupGLDrawGetImageInfo`,`IupGLDrawGetTextSize`}
    WordLists[20][8][1452..1458] = {`IupGLDrawImage`,`IupGLDrawText`,`IupExecute`,`IupExecuteWait`,`IupGLExpander`,`IupGLFrame`,`IupGLIsCurrent`}
    WordLists[20][8][1459..1464] = {`IupGLLabel`,`IupGLLink`,`IupGLMakeCurrent`,`IupGLPalette`,`IupGLProgressBar`,`IupGLScrollBox`}
    WordLists[20][8][1465..1471] = {`IupGLSeparator`,`IupGLSizeBox`,`IupGLSubCanvas`,`IupGLSwapBuffers`,`IupGLText`,`IupGLToggle`,`IupGLUseFont`}
    WordLists[20][8][1472..1479] = {`IupGLVal`,`IupGLValuator`,`IupGLWait`,`IupGlobalsDialog`,`IupGraph`,`IupGridBox`,`IupHbox`,`IupHelp`}
    WordLists[20][8][1480..1486] = {`IupHide`,`IupImage`,`IupImageFromImImage`,`IupImageGetHandle`,`IupImageLibOpen`,`IupImageRGB`,`IupImageRGBA`}
    WordLists[20][8][1487..1494] = {`IupImageToImImage`,`IupInsert`,`IupItem`,`IupLabel`,`IupLayoutDialog`,`IupLink`,`IupList`,`IupListDialog`}
    WordLists[20][8][1495..1501] = {`IupLoad`,`IupLoadBuffer`,`IupLoadImage`,`IupLog`,`IupLoopStep`,`IupLoopStepWait`,`IupMainLoop`}
    WordLists[20][8][1502..1508] = {`IupMainLoopLevel`,`IupMap`,`IupMatGetAttribute`,`IupMatGetInt`,`IupMatGetFloat`,`IupMatrix`,`IupMatrixEx`}
    WordLists[20][8][1509..1514] = {`IupMatrixList`,`IupMatSetAttribute`,`IupMatStoreAttribute`,`IupMenu`,`IupMenuItem`,`IupMessage`}
    WordLists[20][8][1515..1520] = {`IupMessageAlarm`,`IupMessageDlg`,`IupMessageError`,`IupMglPlot`,`IupMglPlotAdd1D`,`IupMglPlotAdd2D`}
    WordLists[20][8][1521..1525] = {`IupMglPlotAdd3D`,`IupMglPlotBegin`,`IupMglPlotDrawLine`,`IupMglPlotDrawMark`,`IupMglPlotDrawText`}
    WordLists[20][8][1526..1530] = {`IupMglPlotEnd`,`IupMglPlotInsert1D`,`IupMglPlotInsert2D`,`IupMglPlotInsert3D`,`IupMglPlotLoadData`}
    WordLists[20][8][1531..1535] = {`IupMglPlotNewDataSet`,`IupMglPlotOpen`,`IupMglPlotPaintTo`,`IupMglPlotSet1D`,`IupMglPlotSet2D`}
    WordLists[20][8][1536..1540] = {`IupMglPlotSet3D`,`IupMglPlotSetData`,`IupMglPlotSetFormula`,`IupMglPlotSetFromFormula`,`IupMglPlotTransform`}
    WordLists[20][8][1541..1547] = {`IupMglPlotTransformTo`,`IupMultiBox`,`IupMultiLine`,`IupNextField`,`IupNormaliser`,`IupNormalizer`,`IupOpen`}
    WordLists[20][8][1548..1554] = {`IupPlayInput`,`IupPlot`,`IupPlotOpen`,`IupPlotAdd`,`IupPlotAddSamples`,`IupPlotAddSegment`,`IupPlotAddStr`}
    WordLists[20][8][1555..1559] = {`IupPlotAddStrSamples`,`IupPlotBegin`,`IupPlotEnd`,`IupPlotFindSample`,`IupPlotFindSegment`}
    WordLists[20][8][1560..1564] = {`IupPlotGetSample`,`IupPlotGetSampleStr`,`IupPlotGetSampleSelection`,`IupPlotGetSampleExtra`,`IupPlotInsert`}
    WordLists[20][8][1565..1569] = {`IupPlotInsertSegment`,`IupPlotInsertStr`,`IupPlotInsertSamples`,`IupPlotInsertStrSamples`,`IupPlotLoadData`}
    WordLists[20][8][1570..1574] = {`IupPlotPaintTo`,`IupPlotSetSample`,`IupPlotSetSampleStr`,`IupPlotSetSampleSelection`,`IupPlotSetSampleExtra`}
    WordLists[20][8][1575..1580] = {`IupPlotTransform`,`IupPlotTransformTo`,`IupPopup`,`IupPostMessage`,`IupPreviousField`,`IupProgressBar`}
    WordLists[20][8][1581..1587] = {`IupProgressDlg`,`IupRadio`,`IupRawStringPtr`,`IupRecordInput`,`IupRedraw`,`IupRefresh`,`IupRefreshChildren`}
    WordLists[20][8][1588..1593] = {`IupReparent`,`IupResetAttribute`,`IupResizeFlow`,`IupResizeFlow_cb`,`IupSaveClassAttributes`,`IupSaveImage`}
    WordLists[20][8][1594..1600] = {`IupSaveImageAsText`,`IupSbox`,`IupSizeBox`,`IupScintilla`,`IupScrollBox`,`IupFlatScrollBox`,`IupSeparator`}
    WordLists[20][8][1601..1604] = {`IupSetAttribute`,`IupSetAttributeHandle`,`IupSetAttributeHandleId`,`IupSetAttributeHandleId2`}
    WordLists[20][8][1605..1609] = {`IupSetAttributeId`,`IupSetAttributeId2`,`IupSetAttributePtr`,`IupSetAttributes`,`IupSetCallback`}
    WordLists[20][8][1610..1614] = {`IupSetCallbackf`,`IupSetCallbacks`,`IupSetClassDefaultAttribute`,`IupSetFocus`,`IupSetGlobal`}
    WordLists[20][8][1615..1620] = {`IupSetGlobalFunction`,`IupSetGlobalInt`,`IupSetHandle`,`IupSetInt`,`IupSetIntId`,`IupSetIntId2`}
    WordLists[20][8][1621..1626] = {`IupSetDouble`,`IupSetDoubleId`,`IupSetDoubleId2`,`IupSetFloat`,`IupSetFloatId`,`IupSetFloatId2`}
    WordLists[20][8][1627..1631] = {`IupSetLanguage`,`IupSetLanguagePack`,`IupSetLanguageString`,`IupSetResizeFlowCallback`,`IupSetRGB`}
    WordLists[20][8][1632..1636] = {`IupSetRGBId`,`IupSetRGBId2`,`IupSetStrAttribute`,`IupSetStrAttributeId`,`IupSetStrAttributeId2`}
    WordLists[20][8][1637..1644] = {`IupSetStrGlobal`,`IupShow`,`IupShowXY`,`IupSpace`,`IupSpin`,`IupSpinBox`,`IupSpinbox`,`IupSplit`}
    WordLists[20][8][1645..1649] = {`IupStoreAttribute`,`IupStoreAttributeId`,`IupStoreAttributeId2`,`IupStoreGlobal`,`IupStoreLanguageString`}
    WordLists[20][8][1650..1655] = {`IupSubmenu`,`IupSubMenu`,`IupTable`,`IupTableClearSelected`,`IupTableClick_cb`,`IupTableEnterItem_cb`}
    WordLists[20][8][1656..1661] = {`IupTableGetData`,`IupTableSetData`,`IupTableGetSelected`,`IupTableResize_cb`,`IupTabs`,`IupText`}
    WordLists[20][8][1662..1667] = {`IupTextConvertLinColToPos`,`IupTextConvertPosToLinCol`,`IupTimer`,`IupToggle`,`IupToggleInt`,`IupTree`}
    WordLists[20][8][1668..1674] = {`IupTreeAddNodes`,`IupTreeGetId`,`IupTreeGetUserId`,`IupTreeSetUserId`,`IupTreeView`,`IupUnmap`,`IupUpdate`}
    WordLists[20][8][1675..1681] = {`IupUpdateChildren`,`IupUser`,`IupVal`,`IupValuator`,`IupVbox`,`IupVersion`,`IupVersionDate`}
    WordLists[20][8][1682..1687] = {`IupVersionNumber`,`IupVersionShow`,`IupWebBrowser`,`IupZbox`,`glAttachShader`,`glBindAttribLocation`}
    WordLists[20][8][1688..1694] = {`glBindBuffer`,`glBindTexture`,`glBufferData`,`glCanvasSpecialText`,`glClear`,`glClearColor`,`glClearDepth`}
    WordLists[20][8][1695..1700] = {`glCompileShader`,`glCreateBuffer`,`glCreateProgram`,`glCreateShader`,`glCreateTexture`,`glDeleteProgram`}
    WordLists[20][8][1701..1706] = {`glDeleteShader`,`glDetachShader`,`glDepthFunc`,`glDisable`,`glDisableVertexAttribArray`,`glDrawArrays`}
    WordLists[20][8][1707..1713] = {`glEnable`,`glEnableVertexAttribArray`,`glEndList`,`glFinish`,`glFloat32Array`,`glFlush`,`glFrustum`}
    WordLists[20][8][1714..1719] = {`glGenerateMipmap`,`glGenLists`,`glGetAttribLocation`,`glGetBooleanv`,`glGetDoublev`,`glGetError`}
    WordLists[20][8][1720..1724] = {`glGetFloatv`,`glGetIntegerv`,`glGetProgramInfoLog`,`glGetProgramParameter`,`glGetShaderInfoLog`}
    WordLists[20][8][1725..1730] = {`glGetShaderParameter`,`glGetString`,`glGetUniformLocation`,`glHint`,`glLinkProgram`,`glLoadIdentity`}
    WordLists[20][8][1731..1739] = {`glMaterial`,`glNewList`,`glOrtho`,`glRotate`,`glRotated`,`glRotatef`,`glScale`,`glScaled`,`glScalef`}
    WordLists[20][8][1740..1745] = {`glShadeModel`,`glShaderSource`,`glSimpleA7texcoords`,`glTexCoord`,`glTexImage2Dc`,`glTexParameteri`}
    WordLists[20][8][1746..1752] = {`glTranslate`,`glTranslated`,`glTranslatef`,`gluLookAt`,`gluPerspective`,`gluProject`,`gluUnProject`}
    WordLists[20][8][1753..1758] = {`glUniform1f`,`glUniform1i`,`glUniformMatrix4fv`,`glUseProgram`,`glVertexAttribPointer`,`glViewport`}
    WordLists[20][8][1759..1765] = {`m4_crossProduct`,`m4_inverse`,`m4_lookAt`,`m4_multiply`,`m4_normalize`,`m4_perspective`,`m4_subtractVectors`}
    WordLists[20][8][1766..1774] = {`m4_xRotate`,`m4_yRotate`,`rgb`,`to_rgb`,`wglGetProcAddress`,`wglUseFontOutlines`,`gBox`,`gButton`,`gCanvas`}
    WordLists[20][8][1775..1784] = {`gCanvasGetPixel`,`gCanvasPixel`,`gcd`,`gCheckbox`,`gClipboard`,`gcp`,`gcps`,`gDatePick`,`gdc`,`gDialog`}
    WordLists[20][8][1785..1792] = {`gdm`,`gdp`,`gDrawArc`,`gDrawCircle`,`gDrawCubicBezier`,`gDrawImage`,`gDrawLine`,`gDrawPolygon`}
    WordLists[20][8][1793..1800] = {`gDrawQuadBezier`,`gDrawRect`,`gDrawText`,`gDropDown`,`gdx`,`gFrame`,`gGetAlignName`,`gGetAttribute`}
    WordLists[20][8][1801..1807] = {`gGetBrother`,`gGetChild`,`gGetChildCount`,`gGetColourName`,`gGetDialog`,`gGetDouble`,`gGetFocus`}
    WordLists[20][8][1808..1814] = {`gGetGlobal`,`gGetGlobalInt`,`gGetGlobalIntInt`,`gGetHandler`,`gGetInheritedHandler`,`gGetInt`,`gGetIntInt`}
    WordLists[20][8][1815..1822] = {`gGetKeyName`,`gGetLineStyleName`,`gGetParent`,`gGetSetAttribute`,`gGetTextExtent`,`gGraph`,`gHbox`,`gHide`}
    WordLists[20][8][1823..1828] = {`gImage`,`gImage_from_Pixbuf`,`gImage_from_XPM`,`gImage_get_transparent`,`gImage_get_wh`,`gImageDestroy`}
    WordLists[20][8][1829..1836] = {`gLabel`,`gList`,`gMainLoop`,`gMap`,`gMenu`,`gMenuGetAttribute`,`gMenuSetAttribute`,`gMenuToggleCheck`}
    WordLists[20][8][1837..1842] = {`gMsgBox`,`gNormalise`,`gNormalize`,`gPixbuf`,`gPixbuf_from_Image`,`gPixbuf_from_Pixels`}
    WordLists[20][8][1843..1850] = {`gPixbuf_take_Pixels`,`gPopupMenu`,`gProgressBar`,`gQuit`,`gRadio`,`gRadioItem`,`gRedraw`,`gRotatePolygon`}
    WordLists[20][8][1851..1858] = {`gSetAttribute`,`gSetAttributes`,`gSetDouble`,`gSetFocus`,`gSetGlobal`,`gSetHandler`,`gSetHandlers`,`gSetInt`}
    WordLists[20][8][1859..1868] = {`gShow`,`gSlider`,`gSpin`,`gSplit`,`gTable`,`gTabs`,`gText`,`gTimer`,`gToggleInt`,`gTreeAddNodes`}
    WordLists[20][8][1869..1877] = {`gTreeGetUserId`,`gTreeView`,`gUseGTK`,`gVbox`,`gVersion`,`hsv_to_rgba`,`rgba`,`rtn`,`to_rgba`}
    WordLists[20][8][1878..1892] = {`xpGUI_from_pGUI`,`adc`,`add`,`asl`,`asr`,`bic`,`byte`,`cdq`,`cmn`,`cmp`,`dec`,`dword`,`eax`,`ebp`,`ebx`}
    WordLists[20][8][1893..1907] = {`ecx`,`edi`,`edx`,`esi`,`esp`,`eor`,`fcomp`,`fcompp`,`fild`,`fist`,`fistp`,`fld`,`fldd`,`fldmfdd`,`fldmiad`}
    WordLists[20][8][1908..1925] = {`fldz`,`fnstsw`,`fxch`,`h4`,`idiv`,`imul`,`inc`,`int3`,`ja`,`jae`,`jb`,`jbe`,`jc`,`je`,`jg`,`jge`,`jl`,`jle`}
    WordLists[20][8][1926..1943] = {`jnae`,`jna`,`jnb`,`jnbe`,`jnc`,`jne`,`jng`,`jnge`,`jnl`,`jnle`,`jno`,`jnp`,`jns`,`jnz`,`jo`,`jp`,`jpe`,`jpo`}
    WordLists[20][8][1944..1960] = {`jz`,`jmp`,`lea`,`lsl`,`lsr`,`mov`,`mvn`,`neg`,`orr`,`push`,`pop`,`qword`,`ret`,`rsb`,`rsc`,`sahf`,`sar`}
    WordLists[20][8][1961..1974] = {`sbc`,`seta`,`setae`,`setb`,`setbe`,`setc`,`sete`,`setg`,`setge`,`setl`,`setle`,`setnae`,`setna`,`setnb`}
    WordLists[20][8][1975..1987] = {`setnbe`,`setnc`,`setne`,`setng`,`setnge`,`setnl`,`setnle`,`setno`,`setnp`,`setns`,`setnz`,`seto`,`setp`}
    WordLists[20][8][1988..2004] = {`setpe`,`setpo`,`setz`,`shl`,`shr`,`st0`,`st1`,`st2`,`st3`,`st4`,`st5`,`st6`,`st7`,`sub`,`teq`,`test`,`tst`}
    WordLists[20][9][1..6] = {`db_cache_clear`,`db_clear_table`,`db_current`,`db_fetch_record`,`db_get_errors`,`db_get_recid`}
    WordLists[20][9][7..13] = {`db_record_recid`,`db_replace_recid`,`db_set_caching`,`absolute_path`,`add_to`,`all_left_units`,`all_matches`}
    WordLists[20][9][14..18] = {`all_right_units`,`allocate_code`,`allocate_data`,`allocate_protect`,`allocate_string_pointer_array`}
    WordLists[20][9][19..27] = {`amalgamated_sum`,`any_key`,`append_lines`,`approx`,`arccosh`,`arcsinh`,`arctanh`,`ascii_string`,`avedev`}
    WordLists[20][9][28..35] = {`begins`,`belongs_to`,`breakup`,`build_commandline`,`build_list`,`byte_range`,`calc_hash`,`calc_primes`}
    WordLists[20][9][36..43] = {`can_add`,`canon2win`,`canonical`,`canonical_path`,`cardinal`,`central_moment`,`change_target`,`char_test`}
    WordLists[20][9][44..51] = {`cmd_parse`,`combine_maps`,`compose_map`,`cosh`,`count_atoms`,`curdir`,`days_in_year`,`decanonical`}
    WordLists[20][9][52..58] = {`defaulted_value`,`defaultext`,`define_map`,`define_operation`,`dequote`,`diagram_commutes`,`difference`}
    WordLists[20][9][59..67] = {`dir_size`,`direct_map`,`dirname`,`disk_metrics`,`disk_size`,`distributes_over`,`driveid`,`dup`,`embed_union`}
    WordLists[20][9][68..76] = {`embedding`,`emovavg`,`error_no`,`exec`,`exp`,`fetch`,`fiber_over`,`fiber_product`,`file_length`}
    WordLists[20][9][77..84] = {`file_position`,`file_timestamp`,`file_type`,`filebase`,`fileext`,`find_all`,`find_any`,`find_nested`}
    WordLists[20][9][85..91] = {`find_replace_callback`,`find_replace_limit`,`frac`,`free_code`,`from_date`,`from_unix`,`geomean`}
    WordLists[20][9][92..97] = {`get_charsets`,`get_def_lang`,`get_dstring`,`get_encoding_properties`,`get_integer16`,`get_integer32`}
    WordLists[20][9][98..105] = {`get_lang_path`,`get_lcid`,`get_option`,`graphics_point`,`harmean`,`has_inverse`,`has_match`,`has_unit`}
    WordLists[20][9][106..113] = {`hex_text`,`init_curdir`,`intdiv`,`integer_array`,`is_associative`,`is_bijective`,`is_even`,`is_even_obj`}
    WordLists[20][9][114..121] = {`is_in_list`,`is_in_range`,`is_injective`,`is_match`,`is_subset`,`is_surjective`,`is_symmetric`,`is_unit`}
    WordLists[20][9][122..130] = {`is_win_nt`,`keyvalues`,`kurtosis`,`lang_load`,`left_shift`,`linear`,`locate_file`,`lock_type`,`machine_addr`}
    WordLists[20][9][131..140] = {`mapping`,`match_all`,`matches`,`mixture`,`money`,`movavg`,`new_time`,`next_prime`,`now_gmt`,`number_array`}
    WordLists[20][9][141..149] = {`pad_head`,`pad_tail`,`parse_commandline`,`pathinfo`,`pathname`,`peek_end`,`peek_top`,`positive_int`,`powof2`}
    WordLists[20][9][150..156] = {`pretty_sprint`,`prime_list`,`product_map`,`project`,`put_integer16`,`put_integer32`,`rand_range`}
    WordLists[20][9][157..164] = {`raw_frequency`,`read`,`read_file`,`receive`,`remove_dups`,`remove_from`,`remove_subseq`,`repeat_pattern`}
    WordLists[20][9][165..171] = {`replace_all`,`restrict`,`reverse_map`,`right_shift`,`rnd_1`,`sequence_array`,`sequence_to_set`}
    WordLists[20][9][172..176] = {`sequences_to_map`,`service_by_name`,`service_by_port`,`set_accumulate_summary`,`set_charsets`}
    WordLists[20][9][177..181] = {`set_decimal_mark`,`set_def_lang`,`set_default_charsets`,`set_encoding_properties`,`set_in_list`}
    WordLists[20][9][182..190] = {`set_in_range`,`set_lang_path`,`set_option`,`sim_index`,`sinh`,`skewness`,`small`,`split_limit`,`statistics`}
    WordLists[20][9][191..198] = {`std_library_address`,`stdev`,`subsets`,`subtract`,`sum_central_moments`,`t_alnum`,`t_alpha`,`t_ascii`}
    WordLists[20][9][199..207] = {`t_boolean`,`t_bytearray`,`t_cntrl`,`t_consonant`,`t_digit`,`t_display`,`t_graph`,`t_identifier`,`t_lower`}
    WordLists[20][9][208..217] = {`t_punct`,`t_space`,`t_specword`,`t_text`,`t_upper`,`t_vowel`,`t_xdigit`,`tanh`,`temp_file`,`to_unix`}
    WordLists[20][9][218..224] = {`trsprintf`,`uname`,`union`,`unsetenv`,`utf8_decode`,`valid_memory_protection_constant`,`vslice`}
    WordLists[20][9][225..229] = {`warning_file`,`weeks_day`,`write_file`,`wstring`,`years_day`}
    WordLists[20][10][1..10] = {`abortErr`,`addDIB`,`addEuBmp`,`addIcon`,`addItem`,`addLVItem`,`addStyle`,`addToBand`,`addTVItem`,`addXpm`}
    WordLists[20][10][11..18] = {`alignControls`,`appendText`,`assignFont`,`attachCleanUp`,`attachPopup`,`autoSelect`,`Beep`,`bitBlt`}
    WordLists[20][10][19..25] = {`buildDefaultOfn`,`canFocus`,`captureMouse`,`centerControl`,`classAutoSelect`,`classDefaults`,`clearWindow`}
    WordLists[20][10][26..32] = {`clickPointerLeft`,`ClientToScreen`,`closeApp`,`closeWindow`,`collapseItem`,`colorValue`,`convPctToPixel`}
    WordLists[20][10][33..39] = {`convPointsToLogical`,`copyBlt`,`copyFile`,`copyToBitmapFile`,`copyToTrueColorBitmap`,`create`,`createDIB`}
    WordLists[20][10][40..45] = {`createDirectory`,`createEx`,`createForm`,`createMousePointer`,`createMouseTrap`,`createXpm`}
    WordLists[20][10][46..51] = {`createXpmFromBmpFile`,`current_number`,`cut`,`defineMenuRadioGroup`,`defineUserProperty`,`define_series`}
    WordLists[20][10][52..58] = {`deleteFile`,`deleteItem`,`deleteLVColumn`,`deleteMouseTrap`,`deleteObject`,`deleteUserProperty`,`destroy`}
    WordLists[20][10][59..65] = {`detachCleanUp`,`disableControlSet`,`distributeControls`,`doEvents`,`dragPointerTo`,`drawArc`,`drawBitmap`}
    WordLists[20][10][66..73] = {`drawChord`,`drawEllipse`,`drawLine`,`drawLines`,`drawPie`,`drawPolygon`,`drawRectangle`,`drawRoundRect`}
    WordLists[20][10][74..81] = {`drawText`,`enableControlSet`,`enableMouseTrap`,`endDoc`,`endPage`,`eraseItems`,`expandItem`,`expandTV`}
    WordLists[20][10][82..87] = {`extractIcon`,`EzCreateFont`,`fetch_CHARFORMAT`,`fetch_SYSTEMTIME`,`filterMouseTrap`,`findChildren`}
    WordLists[20][10][88..94] = {`findItem`,`findParent`,`findParentWindow`,`findText`,`findTrackedObject`,`findWindow`,`floodFill`}
    WordLists[20][10][95..101] = {`formatDate`,`FormatMsg`,`getActiveWindow`,`getAppName`,`getCaption`,`getCharExtent`,`getChildren`}
    WordLists[20][10][102..107] = {`getClassName`,`getClientPoint`,`getClientRect`,`getClientSize`,`getColorDialog`,`getColumnHeadings`}
    WordLists[20][10][108..115] = {`getControlInfo`,`getCount`,`getCtlSize`,`getCurrentDirectory`,`getData`,`getDC`,`getFileInfo`,`getFindText`}
    WordLists[20][10][116..122] = {`getFocus`,`getFontDialog`,`getFontMetric`,`getFontSize`,`getFormIds`,`getFullPathName`,`getHandle`}
    WordLists[20][10][123..131] = {`getHandler`,`getHandles`,`getHint`,`getHScrollPos`,`getHWND`,`getId`,`getIdFromDC`,`getIdName`,`getIndex`}
    WordLists[20][10][132..138] = {`getItem`,`getKeyState`,`getLastMsg`,`getLocalTime`,`getLVAllChecked`,`getLVAllText`,`getLVChecked`}
    WordLists[20][10][139..144] = {`getLVCount`,`getLVItem`,`getLVItemlParam`,`getLVItemText`,`getLVSelected`,`getLVSelectedCount`}
    WordLists[20][10][145..151] = {`getLVSelectedText`,`getMainWindow`,`getMenuPosn`,`getMouseTrap`,`getMultIndices`,`getMultItems`,`getNameId`}
    WordLists[20][10][152..157] = {`getNameIdInContext`,`getNumber`,`getOpenFileName`,`getOpt`,`getPageSetup`,`getPageSetupEx`}
    WordLists[20][10][158..163] = {`getPathFromIDList`,`getPixel`,`getPointerPos`,`getPointerRelPos`,`getPosition`,`getPrintChoice`}
    WordLists[20][10][164..170] = {`getPrinter`,`getRandInt`,`getRect`,`getReplaceText`,`getReturnValue`,`getRichText`,`getSaveFileName`}
    WordLists[20][10][171..176] = {`getScrollChange`,`getScrollPos`,`getScrollRange`,`getSelectedDate`,`getSelectedDateRange`,`getSelectedText`}
    WordLists[20][10][177..183] = {`getSelf`,`getStream`,`getSysColor`,`getSystemMetrics`,`getSystemTime`,`getTabItem`,`getTagMouseTrap`}
    WordLists[20][10][184..190] = {`getTempFile`,`getTempPath`,`getText`,`getTextExtent`,`getTextHeight`,`getTextWidth`,`getThumbPos`}
    WordLists[20][10][191..197] = {`getToday`,`getTrackedObject`,`getTVIndex`,`getTVParent`,`getTVSelectedText`,`getTVText`,`getUserName`}
    WordLists[20][10][198..203] = {`getUserProperty`,`getValues`,`getVScrollPos`,`getWheelScrollLines`,`getWindowInfo`,`getWindowRect`}
    WordLists[20][10][204..210] = {`getWinVersion`,`get_series`,`hideControlSet`,`hitMouseTrap`,`hitTestLV`,`hitTestTT`,`hitTestTV`}
    WordLists[20][10][211..217] = {`insertItem`,`insertLVColumn`,`insertLVItem`,`insertText`,`invokeHandler`,`isChecked`,`isEnabled`}
    WordLists[20][10][218..224] = {`isMaximized`,`isMinimized`,`isScreenPointIn`,`isVisible`,`killTimer`,`limitText`,`loadBitmapFromFile`}
    WordLists[20][10][225..231] = {`loadCursor`,`loadForms`,`loadIconFromFile`,`loadLVInfo`,`makeFront`,`makeStandardName`,`manageToolTip`}
    WordLists[20][10][232..238] = {`messageBox`,`moveFile`,`moveWindow`,`moveZOrder`,`newUIObject`,`next_number`,`OleInitialize`}
    WordLists[20][10][239..246] = {`OleUninitialize`,`openDialog`,`openWindow`,`paste`,`playSound`,`printRichText`,`putStream`,`refreshWindow`}
    WordLists[20][10][247..251] = {`registerHotKey`,`registerRoutine`,`registerw32Function`,`registerw32Library`,`registerw32Procedure`}
    WordLists[20][10][252..258] = {`releaseDC`,`releaseMouse`,`releasePrinter`,`removeHandler`,`removeStyle`,`repaintFG`,`repaintRect`}
    WordLists[20][10][259..264] = {`repaintWindow`,`resetReturnValue`,`resetUserProperty`,`resizeLVColumns`,`restoreMousePointer`,`returnValue`}
    WordLists[20][10][265..270] = {`ScreenToClient`,`selectDirectory`,`sendMessage`,`setAcceleration`,`setAlignment`,`setAppName`}
    WordLists[20][10][271..277] = {`setAutoFocusLabels`,`setBackColor`,`setBitmap`,`setBuddy`,`setBullet`,`setCallback`,`setCheck`}
    WordLists[20][10][278..283] = {`setClientRect`,`setClipboardText`,`setColumn`,`setColumnHeadings`,`setContainer`,`setControlBlocks`}
    WordLists[20][10][284..289] = {`setControlSet`,`setCreateFont`,`setCtlPosition`,`setCtlSize`,`setCurrentDirectory`,`setDefaultFont`}
    WordLists[20][10][290..296] = {`setDragPointer`,`setEnable`,`setEndAction`,`setErasePolicy`,`setEventLoop`,`setFileAttr`,`setFocus`}
    WordLists[20][10][297..304] = {`setFont`,`setFontWeight`,`setHandler`,`setHint`,`setHintEx`,`setHintFont`,`setHintWidth`,`setHScrollPos`}
    WordLists[20][10][305..312] = {`setIcon`,`setIdle`,`setIdName`,`setImageList`,`setIndent`,`setIndex`,`setListHScroll`,`setLVAttr`}
    WordLists[20][10][313..318] = {`setLVChecked`,`setLVFormatRoutine`,`setLVInsert`,`setLVItem`,`setLVItemCount`,`setLVItemlParam`}
    WordLists[20][10][319..324] = {`setLVItemText`,`setLVStyle`,`setMainWindow`,`setMaxDateRange`,`setMonthColor`,`setMouseClick`}
    WordLists[20][10][325..330] = {`setMousePointer`,`setNotifyHandler`,`setPenBkColor`,`setPenBkMode`,`setPenBrushColor`,`setPenColor`}
    WordLists[20][10][331..338] = {`setPenMode`,`setPenPos`,`setPenStyle`,`setPenWidth`,`setPixel`,`setPixmap`,`setPointerPos`,`setReadOnly`}
    WordLists[20][10][339..344] = {`setReBarAutoSizing`,`setRect`,`setScrollChange`,`setScrollPos`,`setScrollRange`,`setSearchPaths`}
    WordLists[20][10][345..350] = {`setSelectedDate`,`setSelectedDateRange`,`setSelectedText`,`setStartupFont`,`setSubFields`,`setTabs`}
    WordLists[20][10][351..357] = {`setTabStops`,`setTagMouseTrap`,`setText`,`setTextAlign`,`setTextColor`,`setTextColour`,`setTimer`}
    WordLists[20][10][358..363] = {`setToday`,`setToolBarSize`,`setTransparentColor`,`setTVImages`,`setTVText`,`setUserLanguage`}
    WordLists[20][10][364..369] = {`setUserProperty`,`setVisible`,`setVScrollPos`,`setWarning`,`setWindowBackColor`,`setWindowRect`}
    WordLists[20][10][370..375] = {`setWindowScrollRange`,`setWinMsgHandler`,`shellExecute`,`shellExecuteEx`,`showChars`,`showControlSet`}
    WordLists[20][10][376..383] = {`showMessage`,`showWindow`,`sizeControls`,`skipF10`,`split_rgb`,`startApp`,`startDoc`,`startPage`}
    WordLists[20][10][384..389] = {`stretchBlt`,`struct_CHARFORMAT`,`struct_LVCOLUMN`,`struct_LVFINDINFO`,`struct_LVITEM`,`struct_MENUITEMINFO`}
    WordLists[20][10][390..396] = {`struct_SYSTEMTIME`,`struct_TOOLINFO`,`subClassControl`,`tab_direction`,`textOut`,`textRect`,`textToBitmap`}
    WordLists[20][10][397..404] = {`toggleItem`,`trackObject`,`transBlt`,`updateMouseTrap`,`useLogicalResolution`,`UseText`,`validId`,`w32abs`}
    WordLists[20][10][405..411] = {`w32acquire_mem`,`w32address`,`w32allot`,`w32alloted_handle`,`w32alloted_size`,`w32alloted_sofar`,`w32CType`}
    WordLists[20][10][412..419] = {`w32Encode`,`w32fetch`,`w32FileOpen`,`w32FindFile`,`w32findKey`,`w32findKeyEx`,`w32Func`,`w32GetCType`}
    WordLists[20][10][420..426] = {`w32get_bits`,`w32hi_word`,`w32iif`,`w32insertElement`,`w32llSetAbort`,`w32lookup`,`w32lo_word`}
    WordLists[20][10][427..433] = {`w32manage_mem`,`w32new_memset`,`w32or_all`,`w32pack_word`,`w32peek_string`,`w32Proc`,`w32release_mem`}
    WordLists[20][10][434..439] = {`w32removeIndex`,`w32removeItem`,`w32replaceItem`,`w32routine_id`,`w32SetCType`,`w32shortInt`}
    WordLists[20][10][440..446] = {`w32signed_word`,`w32split`,`w32store`,`w32string`,`w32TextToNumber`,`w32trim`,`w32trim_left`}
    WordLists[20][10][447..455] = {`w32trim_right`,`warnErr`,`WinMain`,`wPrint`,`wPrintf`,`wPuts`,`xpmToEuBmp`,`xpmToPixmap`,`zorderMouseTrap`}
    WordLists[20][11][1..9] = {`advapi32`,`Bitmap`,`Button`,`CancelButton`,`CCexflags`,`CCflags`,`CCwinstyle`,`CheckBox`,`Combo`}
    WordLists[20][11][10..18] = {`ComboBoxEx`,`comctl32`,`comdlg32`,`CText`,`Default`,`DefPushButton`,`DropDownList`,`EditText`,`FlatToolbar`}
    WordLists[20][11][19..26] = {`FLOODFILLBORDER`,`FLOODFILLSURFACE`,`ForPaint`,`ForProgram`,`gdi32`,`Group`,`GroupAdv`,`HScroll`}
    WordLists[20][11][27..35] = {`HTrackBar`,`Icon`,`kernel32`,`kLVColTypes`,`kLVSortRtn`,`kLVSortSeq`,`kMainMsg`,`kSubclassedMsg`,`List`}
    WordLists[20][11][36..45] = {`ListView`,`LText`,`Menu`,`MenuItem`,`MenuSpacer`,`MleText`,`MonthCalendar`,`ole32`,`PictureButton`,`Pixmap`}
    WordLists[20][11][46..55] = {`Popup`,`Printer`,`ProgressBar`,`PushButton`,`Radio`,`ReBar`,`riched32`,`RichEdit`,`RText`,`Screen`}
    WordLists[20][11][56..63] = {`SepButton`,`shell32`,`SimpleCombo`,`SortedCombo`,`SortedList`,`StatusBar`,`TabControl`,`TabItem`}
    WordLists[20][11][64..72] = {`ToggleButton`,`TogglePicture`,`ToolBar`,`ToolTip`,`TreeView`,`TriCheckBox`,`UpDown`,`user32`,`VScroll`}
    WordLists[20][11][73..80] = {`VTrackBar`,`w32AllItems`,`w32AltEdge`,`w32Edge`,`w32False`,`w32HActivate`,`w32HAfterEvent`,`w32HBreak`}
    WordLists[20][11][81..87] = {`w32HChange`,`w32HClick`,`w32HClose`,`w32HCloseUp`,`w32HDestroy`,`w32HDragAndDrop`,`w32HDropDown`}
    WordLists[20][11][88..95] = {`w32HEvent`,`w32HGotFocus`,`w32HIdle`,`w32HKeyDown`,`w32HKeyPress`,`w32HKeyUp`,`w32HLostFocus`,`w32HMouse`}
    WordLists[20][11][96..103] = {`w32HMouseTrap`,`w32HOpen`,`w32HPaint`,`w32HPause`,`w32HResize`,`w32HScroll`,`w32HTimer`,`w32InchScale`}
    WordLists[20][11][104..109] = {`w32KH_SetFocus`,`w32LV_EndSorting`,`w32LV_StartSorting`,`w32MillScale`,`w32MsgNum`,`w32PB_BITMAP`}
    WordLists[20][11][110..117] = {`w32PB_ICON`,`w32PixelScale`,`w32SameSize`,`w32True`,`w32TwipsScale`,`Win32LibVersion`,`Window`,`winmm32`}
    WordLists[20][12][1..4] = {`allocate_pointer_array`,`CURLOPT_DNS_USE_GLOBAL_CACHE`,`CURLOPT_PROGRESSFUNCTION`,`cdActiveCanvas`}
    WordLists[20][12][5..9] = {`cdBackground`,`cdCanvasBackground`,`cdCanvasBackOpacity`,`cdCanvasFillMode`,`cdCanvasForeground`}
    WordLists[20][12][10..14] = {`cdCanvasHatch`,`cdCanvasInteriorStyle`,`cdCanvasLineStyle`,`wdCanvasLineWidth`,`cdCanvasLineWidth`}
    WordLists[20][12][15..19] = {`cdCanvasLineJoin`,`cdCanvasLineCap`,`cdCanvasNativeFont`,`cdCanvasPattern`,`cdCanvasSetfAttribute`}
    WordLists[20][12][20..24] = {`cdCanvasStipple`,`cdCanvasTextAlignment`,`cdCanvasTextOrientation`,`cdCanvasWriteMode`,`cdClear`}
    WordLists[20][12][25..30] = {`cdCreateCanvasf`,`cdfCanvasArc`,`cdfCanvasBox`,`cdfCanvasChord`,`cdfCanvasClipArea`,`cdfCanvasGetClipArea`}
    WordLists[20][12][31..36] = {`cdfCanvasLine`,`cdfCanvasMark`,`cdfCanvasPixel`,`cdfCanvasRect`,`cdfCanvasSector`,`cdfCanvasText`}
    WordLists[20][12][37..42] = {`cdfCanvasVertex`,`cdForeground`,`cdGetBackground`,`cdGetForeground`,`cdGetNativeFont`,`cdLine`}
    WordLists[20][12][43..48] = {`cdLineStyleDashes`,`cdNativeFont`,`cdSetBackground`,`cdSetForeground`,`cdSetNativeFont`,`cdText`}
    WordLists[20][12][49..55] = {`cdTextAlignment`,`continue`,`entry`,`find_from`,`get_vector`,`gCanvasGetBackground`,`gCanvasGetForeground`}
    WordLists[20][12][56..59] = {`gCanvasGetLineStyle`,`gCanvasGetLineWidth`,`gCanvasSetForeground`,`gCanvasSetLineStyle`}
    WordLists[20][12][60..65] = {`gCanvasSetLineWidth`,`GL_INFO_LOG_LENGTH`,`GL_SHADER_SOURCE_LENGTH`,`glBegin`,`glCallList`,`glColor`}
    WordLists[20][12][66..73] = {`glColor3`,`glColor3b`,`glColor3bv`,`glColor3d`,`glColor3dv`,`glColor3f`,`glColor3fv`,`glColor3i`}
    WordLists[20][12][74..81] = {`glColor3iv`,`glColor3s`,`glColor3sv`,`glColor3ub`,`glColor3ubv`,`glColor3ui`,`glColor3uiv`,`glColor3us`}
    WordLists[20][12][82..89] = {`glColor3usv`,`glColor4b`,`glColor4bv`,`glColor4d`,`glColor4dv`,`glColor4f`,`glColor4fv`,`glColor4i`}
    WordLists[20][12][90..97] = {`glColor4iv`,`glColor4s`,`glColor4sv`,`glColor4ub`,`glColor4ubv`,`glColor4ui`,`glColor4uiv`,`glColor4us`}
    WordLists[20][12][98..103] = {`glColor4usv`,`glCreateShaderProgram`,`glEnd`,`glGenBuffers`,`glGenTextures`,`glGetProgramiv`}
    WordLists[20][12][104..111] = {`glGetShaderiv`,`glLight`,`glLightf`,`glLightfv`,`glLighti`,`glLightiv`,`glMaterialf`,`glMaterialfv`}
    WordLists[20][12][112..119] = {`glMateriali`,`glMaterialiv`,`glMatrixMode`,`glNormal`,`glNormal3`,`glNormal3b`,`glNormal3bv`,`glNormal3d`}
    WordLists[20][12][120..127] = {`glNormal3dv`,`glNormal3f`,`glNormal3fv`,`glNormal3i`,`glNormal3iv`,`glNormal3s`,`glNormal3sv`,`glPopMatrix`}
    WordLists[20][12][128..134] = {`glPushMatrix`,`glTexCoord1d`,`glTexCoord1f`,`glTexCoord1i`,`glTexCoord1s`,`glTexCoord2d`,`glTexCoord2f`}
    WordLists[20][12][135..141] = {`glTexCoord2i`,`glTexCoord2s`,`glTexCoord3d`,`glTexCoord3f`,`glTexCoord3i`,`glTexCoord3s`,`glTexCoord4d`}
    WordLists[20][12][142..148] = {`glTexCoord4f`,`glTexCoord4i`,`glTexCoord4s`,`glTexCoord1dv`,`glTexCoord1fv`,`glTexCoord1iv`,`glTexCoord1sv`}
    WordLists[20][12][149..154] = {`glTexCoord2dv`,`glTexCoord2fv`,`glTexCoord2iv`,`glTexCoord2sv`,`glTexCoord3dv`,`glTexCoord3fv`}
    WordLists[20][12][155..160] = {`glTexCoord3iv`,`glTexCoord3sv`,`glTexCoord4dv`,`glTexCoord4fv`,`glTexCoord4iv`,`glTexCoord4sv`}
    WordLists[20][12][161..167] = {`glTexImage2D`,`glUniform2f`,`glUniform3f`,`glUniform4f`,`glUniform2i`,`glUniform3i`,`glUniform4i`}
    WordLists[20][12][168..174] = {`glUniform1ui`,`glUniform2ui`,`glUniform3ui`,`glUniform4ui`,`glUniform1fv`,`glUniform2fv`,`glUniform3fv`}
    WordLists[20][12][175..181] = {`glUniform4fv`,`glUniform1iv`,`glUniform2iv`,`glUniform3iv`,`glUniform4iv`,`glUniform1uiv`,`glUniform2uiv`}
    WordLists[20][12][182..186] = {`glUniform3uiv`,`glUniform4uiv`,`glUniformMatrix2fv`,`glUniformMatrix2x3fv`,`glUniformMatrix2x4fv`}
    WordLists[20][12][187..190] = {`glUniformMatrix3fv`,`glUniformMatrix3x2fv`,`glUniformMatrix3x4fv`,`glUniformMatrix4x2fv`}
    WordLists[20][12][191..197] = {`glUniformMatrix4x3fv`,`glVertex`,`glVertex2d`,`glVertex2f`,`glVertex2i`,`glVertex2s`,`glVertex3d`}
    WordLists[20][12][198..205] = {`glVertex3f`,`glVertex3i`,`glVertex3s`,`glVertex4d`,`glVertex4f`,`glVertex4i`,`glVertex4s`,`glVertex2dv`}
    WordLists[20][12][206..212] = {`glVertex2fv`,`glVertex2iv`,`glVertex2sv`,`glVertex3dv`,`glVertex3fv`,`glVertex3iv`,`glVertex3sv`}
    WordLists[20][12][213..219] = {`glVertex4dv`,`glVertex4fv`,`glVertex4iv`,`glVertex4sv`,`gmp_randinit_mt`,`gmp_randseed`,`gmp_randclear`}
    WordLists[20][12][220..226] = {`JAVASCRIPT`,`loop`,`match_from`,`mpfr_init2`,`mpfr_clear`,`mpfr_clears`,`mpfr_free_str`}
    WordLists[20][12][227..232] = {`mpfr_get_default_prec`,`mpfr_set_default_prec`,`mpfr_get_prec`,`mpfr_set_prec`,`mpfr_out_str`,`mpfr_set_ui`}
    WordLists[20][12][233..239] = {`mpfr_add_ui`,`mpfr_sub_ui`,`mpfr_mul_ui`,`mpfr_div_ui`,`mpfr_pow_ui`,`mpfr_get_ui`,`mpfr_cmp_ui`}
    WordLists[20][12][240..245] = {`mpfr_inits2`,`mpfr_init_set_d`,`mpfr_init_set_si`,`mpfr_init_set_ui`,`mpfr_init_set_str`,`mpfr_asprintf`}
    WordLists[20][12][246..251] = {`mpfr_snprintf`,`mpfr_fprintf`,`mpfr_vfprintf`,`mpfr_vprintf`,`mpfr_vsprintf`,`mpfr_vsnprintf`}
    WordLists[20][12][252..258] = {`mpfr_vasprintf`,`mpq_clear`,`mpq_clears`,`mpq_set_ui`,`mpq_cmp_ui`,`mpz_init2`,`mpz_init_set_d`}
    WordLists[20][12][259..265] = {`mpz_init_set_si`,`mpz_init_set_str`,`mpz_clear`,`mpz_clears`,`mpz_get_si`,`mpz_get_ui`,`mpz_set_ui`}
    WordLists[20][12][266..272] = {`mpz_mul_ui`,`mpz_cmp_ui`,`mpz_sgn`,`mpz_odd_p`,`mpz_even_p`,`mpz_fits_slong_p`,`mpz_fits_ulong_p`}
    WordLists[20][12][273..278] = {`mpz_prime_mr`,`mpz_probable_prime_p`,`mpz_urandomm`,`nested_get`,`nested_put`,`new_extra`}
    WordLists[20][12][279..287] = {`new_from_kvpairs`,`new_from_string`,`optimize`,`override`,`rehash`,`retry`,`routine`,`set_vector`,`slice`}
    WordLists[20][12][288..290] = {`IupConfigGetVariableDoubleDef`,`IupConfigGetVariableDoubleIdDef`,`IupConfigGetVariableIntDef`}
    WordLists[20][12][291..293] = {`IupConfigGetVariableIntIdDef`,`IupConfigGetVariableStrDef`,`IupConfigGetVariableStrIdDef`}
    WordLists[20][12][294..299] = {`IupGetAttributes`,`IupGetFunction`,`IupMatrixExInit`,`IupMatrixExOpen`,`IupMatSetfAttribute`,`IupMessagef`}
    WordLists[20][12][300..305] = {`IupScanf`,`IupSetAtt`,`IupSetAttributesf`,`IupSetfAttribute`,`IupSetfAttributeId`,`IupSetfAttributeId2`}
    WordLists[20][12][306..313] = {`IupSetFunction`,`IupTabsv`,`IupThread`,`wdCanvasStipple`,`wdCanvasPattern`,`WEB`,`K_acute`,`K_ccedilla`}
    WordLists[20][12][314] = `K_diaeresis`
    WordLists[21][1..3] = {{`def`,`else`,`enddef`,`endfor`,`endif`,`endwhile`,`for`,`if`,`while`},{`nl`,`true`,`pi`},{`var`}}
    WordLists[21][4][1..15] = {`abs`,`and`,`arccos`,`arcsin`,`arctan`,`clear`,`cos`,`drop`,`dup`,`input`,`int`,`log`,`max`,`min`,`mod`}
    WordLists[21][4][16..30] = {`msec`,`neg`,`not`,`over`,`power`,`print`,`rand`,`sign`,`sin`,`sqrt`,`sum`,`swap`,`tan`,`tochar`,`tolist`}
    WordLists[21][4][31..32] = {`tonum`,`tostr`}
    WordLists[22][1][1..16] = {`and`,`as`,`break`,`class`,`def`,`elif`,`else`,`except`,`finally`,`for`,`from`,`if`,`import`,`in`,`is`,`not`}
    WordLists[22][1][17..24] = {`or`,`pass`,`raise`,`return`,`self`,`try`,`while`,`yield`}
    WordLists[22][2] = {`int`,`str`,`list`,`float`}
    WordLists[22][3][1..12] = {`append`,`bin`,`bytearray`,`chr`,`count`,`denominator`,`enumerate`,`file`,`find`,`isinstance`,`join`,`lambda`}
    WordLists[22][3][13..24] = {`len`,`map`,`numerator`,`ord`,`pop`,`print`,`range`,`raw_input`,`replace`,`split`,`update`,`zip`}
    WordLists[22][4..5] = {{`False`,`None`,`True`,`__name__`},{}}
    WordLists[23][1][1..16] = {`+`,`-`,`*`,`=`,`#`,`:`,`add1`,`cadr`,`car`,`coprime?`,`define`,`error`,`expt`,`factorize`,`for`,`for/fold`}
    WordLists[23][1][17..27] = {`for/list`,`for*!/list`,`for/or`,`in-range`,`#lang`,`list`,`lcm`,`match`,`modular-expt`,`modulo`,`racket`}
    WordLists[23][1][28..32] = {`require`,`sort`,`unless`,`when`,`zero`}
    WordLists[23][2..4] = {{`false`,`nil`,`true`},{`append`,`divisors`,`length`,`time`},{`math`}}
    WordLists[24][1][1..12] = {`and`,`attr_accessor`,`attr_reader`,`attr_writter`,`begin`,`case`,`class`,`def`,`do`,`else`,`elsif`,`end`}
    WordLists[24][1][13..25] = {`exit`,`false`,`for`,`if`,`in`,`include`,`initialize`,`loop`,`next`,`nil`,`private`,`raise`,`require`}
    WordLists[24][1][26..32] = {`return`,`self`,`true`,`unless`,`when`,`while`,`w`}
    WordLists[24][2][1..12] = {`any`,`chop`,`denominator`,`detect`,`downcase`,`each`,`each_char`,`elements`,`empty`,`floor`,`gets`,`inject`}
    WordLists[24][2][13..26] = {`join`,`key`,`lcm`,`log2`,`map`,`new`,`odd`,`prime_division`,`print`,`puts`,`rand`,`select`,`size`,`split`}
    WordLists[24][2][27..35] = {`sub`,`take`,`to_i`,`to_r`,`to_s`,`to_sym`,`nil`,`Enumerator`,`Vector`}
    WordLists[25][1][1..15] = {`ADD`,`ALTER`,`and`,`AND`,`And`,`ANSI_NULLS`,`as`,`AS`,`asc`,`begin`,`BEGIN`,`Begin`,`between`,`BETWEEN`,`by`}
    WordLists[25][1][16..28] = {`BY`,`By`,`case`,`CASE`,`Case`,`close`,`CLOSE`,`coalesce`,`COALESCE`,`COLUMN`,`commit`,`count`,`COUNT`}
    WordLists[25][1][29..36] = {`create`,`CREATE`,`Create`,`cursor`,`CURSOR`,`CURSOR_CLOSE_ON_COMMIT`,`CURSOR_DEFAULT`,`DATABASE`}
    WordLists[25][1][37..47] = {`deallocate`,`DEALLOCATE`,`declare`,`DECLARE`,`delete`,`DELETE`,`Delete`,`desc`,`DESC`,`distinct`,`DISTINCT`}
    WordLists[25][1][48..61] = {`drop`,`DROP`,`duplicates`,`else`,`ELSE`,`end`,`END`,`End`,`exec`,`EXEC`,`execute`,`exists`,`EXISTS`,`fetch`}
    WordLists[25][1][62..75] = {`FETCH`,`Fetch`,`for`,`FOR`,`from`,`FROM`,`From`,`function`,`FUNCTION`,`GLOBAL`,`GO`,`group`,`GROUP`,`having`}
    WordLists[25][1][76..90] = {`HAVING`,`if`,`IF`,`If`,`in`,`INDEX`,`inner`,`INNER`,`insert`,`INSERT`,`Insert`,`into`,`INTO`,`is`,`IS`}
    WordLists[25][1][91..105] = {`join`,`JOIN`,`left`,`LEFT`,`like`,`LIKE`,`next`,`NEXT`,`Next`,`nocount`,`NOCOUNT`,`not`,`NOT`,`null`,`NULL`}
    WordLists[25][1][106..120] = {`Null`,`OFF`,`on`,`ON`,`open`,`OPEN`,`or`,`OR`,`order`,`ORDER`,`Order`,`outer`,`OUTER`,`proc`,`procedure`}
    WordLists[25][1][121..129] = {`PROCEDURE`,`QUOTED_IDENTIFIER`,`raiserror`,`RAISERROR`,`Raiserror`,`return`,`RETURN`,`Return`,`returns`}
    WordLists[25][1][130..143] = {`RETURNS`,`rollback`,`select`,`SELECT`,`Select`,`set`,`SET`,`table`,`TABLE`,`then`,`THEN`,`top`,`TOP`,`tran`}
    WordLists[25][1][144..154] = {`transaction`,`TRANSACTION`,`TRUNCATE`,`union`,`UNION`,`UNIQUE`,`update`,`UPDATE`,`Update`,`values`,`VALUES`}
    WordLists[25][1][155..161] = {`when`,`WHEN`,`where`,`WHERE`,`Where`,`while`,`WHILE`}
    WordLists[25][2][1..13] = {`BIGINT`,`bit`,`char`,`Char`,`datetime`,`DATETIME`,`float`,`FLOAT`,`int`,`INT`,`integer`,`money`,`nchar`}
    WordLists[25][2][14..19] = {`ntext`,`smalldatetime`,`smallint`,`tinyint`,`varchar`,`VARCHAR`}
    WordLists[25][3][1..11] = {`abs`,`cast`,`CAST`,`charindex`,`convert`,`CONVERT`,`datediff`,`DATEDIFF`,`DATEADD`,`DATENAME`,`DATEPART`}
    WordLists[25][3][12..23] = {`getdate`,`GETDATE`,`ifnull`,`IFNULL`,`isnull`,`ISNULL`,`len`,`LEN`,`max`,`MAX`,`OBJECT_ID`,`OPENQUERY`}
    WordLists[25][3][24..35] = {`print`,`Print`,`REPLACE`,`Replace`,`right`,`rtrim`,`RTRIM`,`substring`,`sum`,`SUM`,`upper`,`UPPER`}
    WordLists[26][1][1..14] = {`break`,`const`,`continue`,`defer`,`else`,`enum`,`fn`,`for`,`go`,`goto`,`if`,`import`,`in`,`interface`}
    WordLists[26][1][15..22] = {`match`,`module`,`mut`,`or`,`pub`,`return`,`struct`,`type`}
    WordLists[26][2..3] = {{`true`,`false`},{`#define`,`#ifdef`,`#ifndef`,`#include`,`#endif`,`$if`,`$else`}}
    WordLists[26][4][1..16] = {`array`,`bool`,`byte`,`byteptr`,`f32`,`f64`,`i8`,`i16`,`i32`,`i64`,`int`,`rune`,`string`,`typedef`,`u8`,`u16`}
    WordLists[26][4][17..19] = {`u32`,`u64`,`voidptr`}
    WordLists[26][5][1..14] = {`os`,`time`,`strings`,`close`,`contains`,`exec`,`index`,`insert`,`join`,`left`,`len`,`right`,`slice`,`write`}
    WordLists[26][5][15..24] = {`writeln`,`create`,`dir`,`dir_exists`,`executable`,`File`,`file_exists`,`get_raw_line`,`getenv`,`getwd`}
    WordLists[26][5][25..35] = {`home_dir`,`ls`,`mkdir`,`read_file`,`rm`,`rmdir`,`setenv`,`system`,`user_os`,`write_file`,`all_after`}
    WordLists[26][5][36..44] = {`all_before_last`,`cstr`,`ends_with`,`replace`,`split`,`starts_with`,`substr`,`trim_space`,`Builder`}
    WordLists[26][5][45..48] = {`new_builder`,`panic`,`println`,`exit`}
    WordLists[27][1][1..13] = {`async`,`await`,`break`,`case`,`catch`,`class`,`console`,`const`,`construct`,`continue`,`default`,`do`,`else`}
    WordLists[27][1][14..27] = {`exit`,`for`,`function`,`if`,`import`,`in`,`init`,`instanceof`,`is`,`let`,`process`,`return`,`switch`,`this`}
    WordLists[27][1][28..30] = {`try`,`typeof`,`while`}
    WordLists[27][2..3] = {{`false`,`null`,`true`,`undefined`},{`var`,`static`}}
    WordLists[27][4][1..12] = {`.abort`,`.abs`,`.acosh`,`.add`,`.addAll`,`.addPair`,`.all`,`.and`,`.areEqual`,`.ascend`,`.asinh`,`.atan`}
    WordLists[27][4][13..24] = {`.atanh`,`.atoi`,`AudioEngine`,`Axes`,`background`,`Bag`,`BigInt`,`BigRat`,`.bin`,`.black`,`.blue`,`body`}
    WordLists[27][4][25..36] = {`Bool`,`.bool`,`.boolDesc`,`Boolean`,`bottom`,`.brown`,`.btoi`,`.bytes`,`.call`,`Canvas`,`.cbrt`,`.ceil`}
    WordLists[27][4][37..45] = {`charAt`,`charCodeAt`,`.chunks`,`Circle`,`.circle`,`.circlefill`,`classList`,`className`,`.clear`}
    WordLists[27][4][46..55] = {`clientHeight`,`clientWidth`,`clientX`,`clientY`,`.clock`,`.clone`,`Cloneable`,`CloneableSeq`,`.close`,`.cls`}
    WordLists[27][4][56..64] = {`Cmp`,`.codePoints`,`Color`,`Comparable`,`.comparable`,`.comparableDesc`,`.compare`,`compile`,`.contains`}
    WordLists[27][4][65..72] = {`.containsAll`,`.containsAny`,`.containsKey`,`.containsNone`,`.cos`,`.cosh`,`.count`,`.create`}
    WordLists[27][4][73..80] = {`createElement`,`createElementNS`,`cursor`,`.darkblue`,`.darkgray`,`.darkgreen`,`.darkpurple`,`.dec`}
    WordLists[27][4][81..89] = {`.default`,`.defaultDesc`,`.degrees`,`.delete`,`.den`,`.descend`,`.digits`,`.digitSum`,`.distinct`}
    WordLists[27][4][90..99] = {`.distinctCount`,`.divisors`,`.divMod`,`document`,`documentElement`,`.draw`,`.drawfill`,`.dz`,`.e`,`.E`}
    WordLists[27][4][100..110] = {`Ellipse`,`.ellipse`,`.ellipsefill`,`Enum`,`.except`,`.exists`,`.exit`,`.exp`,`Fiber`,`File`,`FileFlags`}
    WordLists[27][4][111..123] = {`FileUtil`,`.filled`,`filter`,`Find`,`find`,`.first`,`Flags`,`float`,`.floor`,`.flush`,`Fmt`,`Fn`,`Font`}
    WordLists[27][4][124..132] = {`.font`,`forEach`,`.fraction`,`.freeze`,`.from`,`.fromByte`,`.fromCenter`,`.fromCodePoint`,`.fromFloat`}
    WordLists[27][4][133..140] = {`.fromInt`,`fromMixedString`,`fromRationalString`,`.fromString`,`FrozenList`,`.fwrite`,`.gamma`,`.gcd`}
    WordLists[27][4][141..146] = {`.general`,`.generalDesc`,`.getPrintArea`,`getBoundingClientRect`,`getElementById`,`getElementsByClassName`}
    WordLists[27][4][147..158] = {`getElementsByTagName`,`.green`,`.h`,`.half`,`.height`,`.hex`,`.Hex`,`hsv`,`.hypot`,`idiv`,`.imag`,`.inc`}
    WordLists[27][4][159..168] = {`.index`,`.indexOf`,`.indigo`,`.individuals`,`innerHeight`,`innerHTML`,`innerWidth`,`Input`,`.insert`,`Int`}
    WordLists[27][4][169..177] = {`.int`,`.intersect`,`.isEmpty`,`.isInfinity`,`.isInteger`,`.isNan`,`.isNegative`,`.isPositive`,`.isPrime`}
    WordLists[27][4][178..186] = {`.isUnit`,`.isWindows`,`.isZero`,`.iterate`,`.iteratorValue`,`.itoc`,`.join`,`.justPressed`,`Keyboard`}
    WordLists[27][4][187..199] = {`.keys`,`.label`,`.last`,`.lcm`,`.lightgray`,`.line`,`List`,`.ln2`,`.ln10`,`.load`,`.log`,`.log10`,`Lst`}
    WordLists[27][4][200..210] = {`.map`,`.mark`,`Math`,`Matrix`,`.max`,`.maxSafeInt`,`.maxSafeInteger`,`.mdec`,`.merge`,`Meta`,`.min`}
    WordLists[27][4][211..222] = {`.minusOne`,`.modf`,`.modInv`,`Mouse`,`.nearest`,`.new`,`.nextPrime`,`.num`,`Num`,`.numDesc`,`Nums`,`.oct`}
    WordLists[27][4][223..231] = {`.one`,`onerror`,`offsetHeight`,`offsetLeft`,`offsetWidth`,`offsetTop`,`onclick`,`ondblclick`,`ondragstart`}
    WordLists[27][4][232..241] = {`onload`,`onmousedown`,`.open`,`.openWithFlags`,`.or`,`.orange`,`.ord`,`Output`,`pageX`,`pageY`}
    WordLists[27][4][242..252] = {`parentElement`,`parseFloat`,`parseInt`,`.pdec`,`.peach`,`.phi`,`.pi`,`.pink`,`Platform`,`.plot`,`Point`}
    WordLists[27][4][253..260] = {`.PointsToPairs`,`Polygon`,`.pop`,`position`,`.pow`,`.prefixes`,`preventDefault`,`.primeFactors`}
    WordLists[27][4][261..269] = {`.primeSieve`,`.print`,`Process`,`.properSubbagOf`,`.properSubsetOf`,`.pset`,`.purple`,`.push`,`.pushAll`}
    WordLists[27][4][270..280] = {`.quick`,`.radians`,`Rand`,`Random`,`Rat`,`.ratio`,`.read`,`.readBytes`,`readFile`,`.readLine`,`.real`}
    WordLists[27][4][281..290] = {`.realPath`,`Rectangle`,`.rect`,`.rectfill`,`.red`,`.reduce`,`.regular`,`.rem`,`.remove`,`.removeAll`}
    WordLists[27][4][291..299] = {`.removeAt`,`removeChild`,`removeEventListener`,`.replace`,`.replaceAll`,`require`,`.resize`,`.rgb`,`.round`}
    WordLists[27][4][300..310] = {`.s`,`Selectable`,`send`,`Sequence`,`Set`,`setAttribute`,`.showAsInt`,`.shuffle`,`.skip`,`.sign`,`.sin`}
    WordLists[27][4][311..322] = {`.sinh`,`.size`,`Sort`,`.sort`,`splice`,`.split`,`.sqrt`,`.startsWith`,`Stack`,`Stdin`,`Stdout`,`String`}
    WordLists[27][4][323..333] = {`.string`,`.stringDesc`,`Struct`,`style`,`Square`,`Stepped`,`Str`,`.subbagOf`,`.subsetOf`,`substr`,`.sum`}
    WordLists[27][4][334..345] = {`super`,`.superbagOf`,`.supersetOf`,`.swap`,`System`,`.tag`,`.take`,`.tan`,`.tanh`,`.ten`,`.tenth`,`.ter`}
    WordLists[27][4][346..355] = {`.text`,`.time`,`.title`,`.toBaseString`,`.toDecimal`,`.toFloat`,`.toInt`,`toggle`,`.toList`,`toLowerCase`}
    WordLists[27][4][356..365] = {`.toMap`,`.toMixedString`,`toNumber`,`top`,`.toString`,`.tprint`,`.trim`,`.trimEnd`,`.trimStart`,`.truncate`}
    WordLists[27][4][366..378] = {`Tuple`,`.two`,`.type`,`Union`,`.union`,`.upper`,`.v`,`.value`,`.where`,`.white`,`.width`,`window`,`Window`}
    WordLists[27][4][379..389] = {`.write`,`.writeAll`,`.writeBytes`,`.writeOnly`,`.x`,`.X`,`.Xz`,`XMLHttpRequest`,`.xor`,`.y`,`.yellow`}
    WordLists[27][4][390..392] = {`.yield`,`.zero`,`zIndex`}
    colourTabs[1] = {14737632,0,8421504,14745599,0,0,0,0,0,14737632,128,16776960,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    colourTabs[2][1..18] = {12632256,255,8421504,13158600,32768,16711680,255,16711935,0,16777215,128,16776960,128,32768,255,16711680,65280,0}
    colourTabs[2][19..31] = {0,0,32896,32896,32768,8388736,16711680,0,0,0,0,0,0}
    colourTabs[3][1..16] = {12632256,128,10526880,13158600,32768,16711680,255,8388736,0,14737632,8388736,16776960,128,32768,255,16711680}
    colourTabs[3][17..31] = {65280,0,0,32896,0,0,0,0,0,0,0,0,0,0,0}
    colourTabs[4][1..19] = {12632256,128,8421504,16777184,32768,16711680,255,0,0,16777215,128,16776960,255,8388736,8388608,8388736,32768,0,0}
    colourTabs[4][20..31] = {32896,128,32896,8421376,0,0,0,0,0,0,0,0}
    colourTabs[5][1..19] = {13487565,128,8421504,15329769,32768,16711680,255,0,0,14737632,128,16776960,255,8388736,8388608,8388736,32768,0,0}
    colourTabs[5][20..31] = {5197568,8421376,32896,7667712,8388608,5705022,0,0,0,0,0,0}
    colourTabs[6][1..19] = {12632256,128,8421504,15329769,32768,16711680,255,0,0,16777215,128,16776960,255,8388736,8388608,8388736,32768,0,0}
    colourTabs[6][20..31] = {32896,8388608,8388608,0,0,0,0,0,0,0,0,0}
    colourTabs[7][1..23] = {14737632,32768,8421504,14745599,32768,16711680,255,0,0,14737632,128,16776960,0,0,0,0,0,0,0,128,0,128,8388608}
    colourTabs[7][24..31] = {8388736,128,0,0,0,0,0,0}
    colourTabs[8][1..18] = {12632256,128,8421504,16777184,32768,16711680,255,0,0,16777215,128,16776960,255,16776960,8388608,8388736,32768,0}
    colourTabs[8][19..31] = {0,32896,32896,32896,24688,24688,0,0,0,0,0,0,0}
    colourTabs[9][1..18] = {12632256,255,8421504,13158600,32768,16711680,255,16711935,0,14737632,128,16776960,128,32768,255,16711680,65280,0}
    colourTabs[9][19..31] = {0,32896,32896,16711680,16711935,16711680,32768,8388736,8388736,0,0,0,0}
    colourTabs[10][1..17] = {12632256,128,8421504,14211288,32768,16711680,255,8421376,0,13290186,128,16776960,255,8388608,8388736,65280,128}
    colourTabs[10][18..31] = {32768,65535,32896,16512,7364776,17920,6160478,0,0,0,0,0,0,0}
    colourTabs[11][1..18] = {12632256,128,8421504,15329769,32768,16711680,255,0,0,16777215,128,16776960,255,8388736,8388608,8388736,32768,0}
    colourTabs[11][19..31] = {0,32896,17920,32896,8388608,8388608,16711680,0,0,0,0,0,0}
    colourTabs[12][1..17] = {14737632,32768,8421504,14745599,32768,16711680,255,0,0,14737632,128,16776960,255,8388736,8388608,8388736,32768}
    colourTabs[12][18..31] = {128,16776960,128,128,8388608,8388736,128,0,0,0,0,0,0,0}
    colourTabs[13][1..16] = {12632256,32896,8421504,12105912,8421376,16711680,128,8421376,0,14737632,128,16776960,255,8388608,128,16711680}
    colourTabs[13][17..31] = {16776960,0,0,255,8388608,32896,0,0,0,0,0,0,0,0,0}
    colourTabs[14][1..18] = {12632256,128,8421504,15329769,32768,16711680,255,0,0,16777215,128,16776960,255,8388736,8388608,8388736,32768,0}
    colourTabs[14][19..31] = {0,32896,8388608,16740721,8388608,0,0,0,0,0,0,0,0}
    colourTabs[15][1..18] = {12632256,128,8421504,15329769,32768,16711680,255,0,0,16777215,128,16776960,255,8388736,8388608,8388736,32768,0}
    colourTabs[15][19..31] = {0,32896,17920,32896,8388608,8388608,16711680,0,0,0,0,0,0}
    colourTabs[16][1..22] = {16777215,32768,8421504,16777184,32768,16711680,255,0,0,16777215,128,16776960,0,0,0,0,0,0,0,8421376,128,8388608}
    colourTabs[16][23..31] = {8388736,0,0,0,0,0,0,0,0}
    colourTabs[17][1..18] = {12632256,128,8421504,16777184,32768,16711680,255,0,0,16777215,128,16776960,255,16776960,8388608,8388736,32768,0}
    colourTabs[17][19..31] = {0,32896,0,0,0,0,0,0,0,0,0,0,0}
    colourTabs[18][1..19] = {16777215,255,8421504,16777184,32768,16711680,255,0,0,16777215,128,16776960,255,32896,8388608,65280,32768,0,0}
    colourTabs[18][20..31] = {32896,32896,32896,0,0,0,0,0,0,0,0,0}
    colourTabs[19][1..18] = {12632256,128,8421504,16777184,32768,16711680,255,0,0,16777215,128,16776960,255,16776960,8388608,8388736,32768,0}
    colourTabs[19][19..31] = {0,32896,32896,0,0,0,0,0,0,0,0,0,0}
    colourTabs[20][1..17] = {12632256,128,8421504,14211288,32768,16711680,255,8421376,0,13290186,128,16776960,255,8388608,8388736,65280,128}
    colourTabs[20][18..31] = {32768,65535,32896,105014,2907180,16512,7364776,7364776,17920,7364776,7364776,8388608,0,16711680}
    colourTabs[21][1..18] = {12632256,128,8421504,15329769,32768,16711680,255,0,0,16777215,128,16776960,255,8388736,8388608,8388736,32768,0}
    colourTabs[21][19..31] = {0,32896,32896,32896,32896,0,0,0,0,0,0,0,0}
    colourTabs[22][1..18] = {12632256,128,8421504,16777184,32768,16711680,255,0,0,16777215,128,16776960,255,16776960,8388608,8388736,32768,0}
    colourTabs[22][19..31] = {0,32896,32896,128,128,0,0,0,0,0,0,0,0}
    colourTabs[23][1..28] = {14737632,32768,8421504,14745599,0,16711680,255,0,0,14737632,128,16776960,0,0,0,0,0,0,0,128,0,128,128,0,0,0,0,0}
    colourTabs[23][29..31] = {0,0,0}
    colourTabs[24][1..25] = {16777215,32768,8421504,16777184,0,16711680,255,0,0,16777215,128,16776960,0,0,0,0,0,0,0,8421376,8388736,0,0,0,0}
    colourTabs[24][26..31] = {0,0,0,0,0,0}
    colourTabs[25][1..15] = {12632256,32896,10526880,12105912,8421376,16711680,255,8421376,0,14737632,128,16776960,32768,255,8388736}
    colourTabs[25][16..31] = {16711680,16776960,0,0,128,8388608,32896,0,0,0,0,0,0,0,0,0}
    colourTabs[26][1..18] = {12632256,128,8421504,15329769,32768,16711680,255,0,0,16777215,128,16776960,255,8388736,8388608,8388736,32768,0}
    colourTabs[26][19..31] = {0,32896,8421376,32896,6821124,8388608,0,0,0,0,0,0,0}
    colourTabs[27][1..18] = {12632256,128,8421504,15329769,32768,16711680,255,0,0,16777215,128,16776960,255,8388736,8388608,8388736,32768,0}
    colourTabs[27][19..31] = {0,32896,8388608,32896,8388608,0,0,0,0,0,0,0,0}
    styleTabs[1..2] = {{0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0},{0,2,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
    styleTabs[3..4] = {{0,2,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{0,2,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
    styleTabs[5..6] = {{0,2,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{0,2,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
    styleTabs[7..8] = {{0,2,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{0,2,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
    styleTabs[9..10] = {{0,2,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{0,2,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
    styleTabs[11..12] = {{0,2,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0},{0,2,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
    styleTabs[13..14] = {{0,2,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{0,2,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
    styleTabs[15..16] = {{0,2,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0},{0,2,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
    styleTabs[17..18] = {{0,2,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{0,2,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
    styleTabs[19] = {0,2,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    styleTabs[20] = {0,2,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    styleTabs[21] = {0,2,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    styleTabs[22] = {0,2,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    styleTabs[23] = {0,2,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    styleTabs[24] = {0,2,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    styleTabs[25] = {0,2,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    styleTabs[26] = {0,2,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    styleTabs[27] = {0,2,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    charMaps[1][1..40] = {11,11,11,11,11,11,11,11,11,7,7,11,11,7,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,7,11,8,8,11,11,11,10}
    charMaps[1][41..91] = {11,11,11,11,11,11,11,11,2,2,2,2,2,2,2,2,2,2,8,11,8,11,8,11,11,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1}
    charMaps[1][92..146] = {8,8,8,8,2,8,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,11,8,11,11,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[1][147..203] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,8,8,2,2,2,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[1][204..256] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[2][1..42] = {11,11,11,11,11,11,11,11,11,7,7,11,11,7,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,7,8,10,2,8,8,9,10,5,6}
    charMaps[2][43..98] = {9,9,9,9,1,9,2,2,2,2,2,2,2,2,2,2,8,11,9,9,9,8,11,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,8,6,8,1,10,1}
    charMaps[2][99..155] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,9,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[2][156..212] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,8,8,2,2,2,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[2][213..256] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[3][1..42] = {11,11,11,11,11,11,11,11,11,7,7,11,11,7,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,7,8,10,2,8,8,9,10,5,6}
    charMaps[3][43..99] = {9,9,9,9,1,9,2,2,2,2,2,2,2,2,2,2,8,9,9,9,9,8,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,8,6,8,1,8,1,1}
    charMaps[3][100..156] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,9,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[3][157..213] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,8,8,2,2,2,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[3][214..256] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[4][1..41] = {11,11,11,11,11,11,11,11,11,7,7,11,11,7,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,7,9,10,11,8,9,9,10,5}
    charMaps[4][42..98] = {6,9,9,9,9,9,9,2,2,2,2,2,2,2,2,2,2,9,9,9,9,9,9,9,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,9,1,10,1}
    charMaps[4][99..155] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,9,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[4][156..212] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,8,8,2,2,2,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[4][213..256] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[5][1..42] = {11,11,11,11,11,11,11,11,11,7,7,11,11,7,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,7,9,10,1,8,9,9,10,5,6}
    charMaps[5][43..98] = {9,9,9,9,9,9,2,2,2,2,2,2,2,2,2,2,9,9,9,9,9,9,11,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,9,1,10,1}
    charMaps[5][99..155] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,9,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[5][156..212] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,8,8,2,2,2,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[5][213..256] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[6][1..41] = {11,11,11,11,11,11,11,11,11,7,7,11,11,7,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,7,9,10,8,8,9,11,10,5}
    charMaps[6][42..97] = {6,9,9,8,2,9,9,2,2,2,2,2,2,2,2,2,2,9,9,11,9,9,8,9,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,11,6,9,1,10}
    charMaps[6][98..153] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,11,6,11,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[6][154..210] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,8,8,2,2,2,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[6][211..256] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[7][1..41] = {11,11,11,11,11,11,11,11,11,7,7,11,11,7,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,7,9,10,11,8,9,9,10,5}
    charMaps[7][42..97] = {6,9,9,8,9,9,9,2,2,2,2,2,2,2,2,2,2,9,8,9,9,9,9,9,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,11,6,11,2,10}
    charMaps[7][98..154] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,9,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[7][155..211] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,8,8,2,2,2,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[7][212..256] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[8][1..42] = {11,11,11,11,11,11,11,11,11,7,7,11,11,7,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,7,9,10,9,9,8,9,10,5,6}
    charMaps[8][43..99] = {9,9,9,9,9,9,2,2,2,2,2,2,2,2,2,2,9,9,9,9,9,9,9,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,9,2,9,1,1}
    charMaps[8][100..155] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,11,9,11,9,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[8][156..212] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,8,8,2,2,2,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[8][213..256] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[9][1..42] = {11,11,11,11,11,11,11,11,11,7,7,11,11,7,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,7,8,10,2,8,8,9,10,5,6}
    charMaps[9][43..98] = {9,9,9,9,1,9,2,2,2,2,2,2,2,2,2,2,8,11,9,9,9,8,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,8,6,8,1,10,1}
    charMaps[9][99..155] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,9,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[9][156..212] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,8,8,2,2,2,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[9][213..256] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[10][1..41] = {11,11,11,11,11,11,11,11,11,7,7,11,11,7,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,7,9,10,1,8,8,9,10,5}
    charMaps[10][42..97] = {6,9,9,9,9,9,9,2,2,2,2,2,2,2,2,2,2,9,9,9,9,9,9,9,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,8,1,10}
    charMaps[10][98..153] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,9,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[10][154..210] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,8,8,2,2,2,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[10][211..256] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[11][1..42] = {11,11,11,11,11,11,11,11,11,7,7,11,11,7,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,7,9,10,9,1,9,9,9,5,6}
    charMaps[11][43..98] = {9,9,8,9,9,9,2,2,2,2,2,2,2,2,2,2,9,8,9,9,9,9,9,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,11,6,9,9,10,1}
    charMaps[11][99..154] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,9,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[11][155..211] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,8,8,2,2,2,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[11][212..256] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[12][1..41] = {11,11,11,11,11,11,11,11,11,7,7,11,11,7,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,7,9,10,11,8,9,9,10,5}
    charMaps[12][42..96] = {6,9,9,8,9,8,9,2,2,2,2,2,2,2,2,2,2,9,8,9,9,9,8,11,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,11,6,9,2}
    charMaps[12][97..151] = {10,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,11,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[12][152..208] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,8,8,2,2,2,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[12][209..256] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[13][1..41] = {11,11,11,11,11,11,11,11,11,7,7,11,11,7,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,7,9,10,8,8,8,3,10,8}
    charMaps[13][42..97] = {8,9,9,8,2,8,2,2,2,2,2,2,2,2,2,2,2,8,8,3,9,4,9,8,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,2,6,8,2,10}
    charMaps[13][98..153] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,9,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[13][154..210] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,8,8,2,2,2,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[13][211..256] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[14][1..41] = {11,11,11,11,11,11,11,11,11,7,7,11,11,7,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,7,9,10,8,8,9,9,10,5}
    charMaps[14][42..97] = {6,9,9,9,9,9,9,2,2,2,2,2,2,2,2,2,2,9,9,9,9,9,9,11,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,9,1,10}
    charMaps[14][98..153] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,9,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[14][154..210] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,8,8,2,2,2,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[14][211..256] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[15][1..41] = {11,11,11,11,11,11,11,11,11,7,7,11,11,7,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,7,9,10,8,1,9,9,10,5}
    charMaps[15][42..97] = {6,9,9,9,9,3,9,2,2,2,2,2,2,2,2,2,2,9,9,9,9,9,9,9,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,9,1,10}
    charMaps[15][98..153] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,9,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[15][154..210] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,8,8,2,2,2,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[15][211..256] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[16][1..41] = {11,11,11,11,11,11,11,11,11,7,7,11,11,7,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,7,9,10,11,8,9,9,10,5}
    charMaps[16][42..97] = {6,9,9,8,9,9,9,2,2,2,2,2,2,2,2,2,2,9,8,9,9,9,8,9,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,11,6,9,2,10}
    charMaps[16][98..153] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,9,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[16][154..210] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,8,8,2,2,2,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[16][211..256] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[17][1..41] = {11,11,11,11,11,11,11,11,11,7,7,11,11,7,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,7,1,10,11,1,8,11,9,5}
    charMaps[17][42..96] = {6,9,9,11,9,9,9,2,2,2,2,2,2,2,2,2,2,9,11,9,9,9,9,9,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,11,6,11,2}
    charMaps[17][97..151] = {9,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,11,6,11,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[17][152..208] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,8,8,2,2,2,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[17][209..256] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[18][1..41] = {11,11,11,11,11,11,11,11,11,7,7,11,11,7,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,7,9,10,11,8,8,9,10,5}
    charMaps[18][42..97] = {6,9,9,9,9,9,9,2,2,2,2,2,2,2,2,2,2,8,9,9,9,9,9,9,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,9,2,10}
    charMaps[18][98..153] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,9,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[18][154..210] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,8,8,2,2,2,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[18][211..256] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[19][1..41] = {11,11,11,11,11,11,11,11,11,7,7,11,11,7,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,7,9,10,11,8,8,9,10,5}
    charMaps[19][42..97] = {6,9,9,9,9,9,9,2,2,2,2,2,2,2,2,2,2,8,9,9,9,9,9,9,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,9,2,10}
    charMaps[19][98..153] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,9,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[19][154..210] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,8,8,2,2,2,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[19][211..256] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[20][1..41] = {11,11,11,11,11,11,11,11,11,7,7,11,11,7,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,7,9,10,8,8,8,9,10,5}
    charMaps[20][42..97] = {6,9,9,9,9,9,9,2,2,2,2,2,2,2,2,2,2,9,9,9,9,9,9,9,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,8,6,8,1,10}
    charMaps[20][98..153] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,9,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[20][154..210] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,8,8,2,2,2,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[20][211..256] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[21][1..40] = {11,11,11,11,11,11,11,11,11,7,7,11,11,7,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,7,11,10,8,8,11,11,10}
    charMaps[21][41..95] = {5,6,9,9,8,9,8,9,2,2,2,2,2,2,2,2,2,2,8,8,9,9,9,8,11,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,11,6,11}
    charMaps[21][96..150] = {1,10,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,11,6,11,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[21][151..207] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,8,8,2,2,2,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[21][208..256] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[22][1..41] = {11,11,11,11,11,11,11,11,11,7,7,11,11,7,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,7,9,10,11,8,9,9,10,5}
    charMaps[22][42..97] = {6,9,9,9,9,9,9,2,2,2,2,2,2,2,2,2,2,9,9,9,9,9,9,9,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,9,1,10}
    charMaps[22][98..153] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,11,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[22][154..210] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,8,8,2,2,2,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[22][211..256] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[23][1..41] = {11,11,11,11,11,11,11,11,11,7,7,11,11,7,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,7,11,10,1,8,11,2,9,5}
    charMaps[23][42..96] = {6,1,1,8,1,8,2,2,2,2,2,2,2,2,2,2,2,8,8,9,1,11,2,11,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,11,6,11,2}
    charMaps[23][97..151] = {10,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,11,6,11,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[23][152..208] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,8,8,2,2,2,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[23][209..256] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[24][1..41] = {11,11,11,11,11,11,11,11,11,7,7,11,11,7,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,7,9,10,11,9,9,9,10,5}
    charMaps[24][42..97] = {6,9,9,8,9,9,9,2,2,2,2,2,2,2,2,2,2,9,8,9,9,9,8,9,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,9,2,10}
    charMaps[24][98..153] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,9,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[24][154..210] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,8,8,2,2,2,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[24][211..256] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[25][1..42] = {11,11,11,11,11,11,11,11,11,7,7,11,11,7,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,7,9,10,8,8,9,9,9,5,6}
    charMaps[25][43..98] = {9,9,8,9,8,9,2,2,2,2,2,2,2,2,2,2,8,8,9,9,9,9,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,2,6,8,2,10,1}
    charMaps[25][99..154] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,11,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[25][155..211] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,8,8,2,2,2,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[25][212..256] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[26][1..41] = {11,11,11,11,11,11,11,11,11,7,7,11,11,7,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,7,9,10,1,1,9,9,10,5}
    charMaps[26][42..96] = {6,9,9,9,9,9,9,2,2,2,2,2,2,2,2,2,2,9,9,9,9,9,9,11,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,11,6,9,1}
    charMaps[26][97..151] = {10,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,11,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[26][152..208] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,8,8,2,2,2,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[26][209..256] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[27][1..41] = {11,11,11,11,11,11,11,11,11,7,7,11,11,7,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,7,9,10,8,8,9,9,10,5}
    charMaps[27][42..97] = {6,9,9,9,9,3,9,2,2,2,2,2,2,2,2,2,2,9,9,9,9,9,9,9,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,9,1,10}
    charMaps[27][98..153] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,9,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[27][154..210] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,8,8,2,2,2,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMaps[27][211..256] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    MAXnColours = 31
    wordChar[1..39] = {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}
    wordChar[40..90] = {11,11,11,11,11,11,11,2,11,2,2,2,2,2,2,2,2,2,2,11,11,11,11,11,11,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    wordChar[91..144] = {2,11,11,11,11,2,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,11,11,11,11,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    wordChar[145..201] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,11,11,2,2,2,11,2,2,2,2,2,2,2,2,2,2,2,2,2}
    wordChar[202..256] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    standardColourNames[1..13] = {`Black`,`Maroon`,`Green`,`Olive`,`Navy`,`Purple`,`Teal`,`Gray`,`Silver`,`Red`,`Lime`,`Yellow`,`Blue`}
    standardColourNames[14..16] = {`Fuchsia`,`Aqua`,`White`}
    standardColours = {0,8388608,32768,8421376,128,8388736,32896,8421504,12632256,16711680,65280,16776960,255,16711935,65535,16777215}
    f = 3
    ch = -1
    lineno = 3807
    columnOne = 1
    errorAlready = 0
    errorTitle = <novalue>
    errorMsg = ``
    lineComments = {`--`,`//`}
    blockComment = {`/*`,`*/`,`--/*`,`--*/`,`#[`,`#]`}
    ColourTab[1..18] = {12632256,128,8421504,14211288,32768,16711680,255,8421376,0,13290186,128,16776960,255,8388608,8388736,65280,128,32768}
    ColourTab[19..31] = {65535,32896,105014,2907180,16512,7364776,7364776,17920,7364776,7364776,8388608,0,16711680}
    StyleTab = {0,2,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    fullname = `C:\Program Files (x86)\Phix\demo\edix\help\Phix.txt`
    charMap[1..44] = {11,11,11,11,11,11,11,11,11,7,7,11,11,7,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,7,9,10,8,8,8,9,10,5,6,9,9}
    charMap[45..103] = {9,9,9,9,2,2,2,2,2,2,2,2,2,2,9,9,9,9,9,9,9,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,8,6,8,1,10,1,1,1,1,1,1}
    charMap[104..162] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,9,6,9,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMap[163..221] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,8,8,2,2,2,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    charMap[222..256] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    urlChar[1..42] = {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,2,11,2,2,2,2,2,2,2}
    urlChar[43..98] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,11,11,2,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,11,2,11,11,2,11,2}
    urlChar[99..155] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,11,11,11,2,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    urlChar[156..213] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,11,11,2,2,2,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    urlChar[214..256] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    caseMap[1..59] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    caseMap[60..118] = {0,0,0,0,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1}
    caseMap[119..177] = {1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    caseMap[178..236] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    caseMap[237..255] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    i = 123
    i = 91
    newSections[1..10] = {`Background`,`Comments`,`Highlight`,`Current Line`,`Strings`,`Illegals`,`Operators`,`URLs`,`Other`,`Marginbg`}
    newSections[11..21] = {`Linenos`,`Bookmarks`,`Brace1`,`Brace2`,`Brace3`,`Brace4`,`Brace5`,`Brace6`,`Brace7`,`Reserved Words`,`Constants`}
    newSections[22..23] = {`Types`,`lib`}
    helpnames = {`Phix.txt`}
    helpfiles = {`C:\Program Files (x86)\Phix\docs\phix\phix.chm`}
    menuTxts = {`Phix.chm`}
    F1lists[1][1..9] = {`$conCat`,`$ocument_createElement`,`$storeAttr`,`a32Colour`,`a32Colour0`,`a32Dib`,`a32Dib0`,`abort`,`abs`}
    F1lists[1][10..21] = {`abstract`,`accept`,`ACTION`,`ACTIVE`,`activeElement`,`adc`,`adcs`,`adccc`,`adcccs`,`adccs`,`adccss`,`adceq`}
    F1lists[1][5110..5114] = {`ZipAddFile`,`ZipAddFolder`,`ZipClose`,`ZipCreateFile`,`ZIPENTRY`}

 C:\Program Files (x86)\Phix\demo\edix\src\tabs.e:
    showSpecials = 0
    nexttabi = <novalue>
    fileTabLength = 8
    otxt[1..20] = {'-','-','{','}',' ','=',' ','w','a','i','t','_','k','e','y','(',')',9,9,'h'}
    otxt[21..38] = {'e','n',' ','f','o','r','t','u','n','e',' ','=',' ','n','e','x','t',' '}
    otxt[39..56] = {'e','n','d',' ','i','f',' ','m','a','d','e',' ','h','e','r','e','.','\\'}
    otxt[57..76] = {'n','"',')','4','6','8','6','1','6','E','2','0','7','6','6','1','7','3','6','5'}
    otxt[77..96] = {'6','C','6','9','6','E','6','5','0','D','0','A','0','0','0','0',',','1','2',','}
    otxt[97..115] = {'6',',','0',')',']','r','i','z','e','!','"',')','3',' ','1','|','1','3',' '}
    otxt[116..134] = {'1','4',' ','1','|','1','4',' ','1','5',' ','1','|','0',' ','4',' ','1','|'}
    otxt[135..153] = {'4',' ','8',' ','1','|','8',' ','1','2',' ','1','|','1',' ','5',' ','1','|'}
    otxt[154..172] = {'5',' ','9',' ','1','|','9',' ','1','3',' ','1','|','2',' ','6',' ','1','|'}
    otxt[173..192] = {'6',' ','1','0',' ','1','|','1','0',' ','1','4',' ','1','|','3',' ','7',' ','1'}
    otxt[193..212] = {'|','7',' ','1','1',' ','1','|','1','1',' ','1','5',' ','1','"',')','9','2','4'}
    otxt[213..232] = {'6',')',']',',',' ','[','(','1','.','5','4','9','3','6','7','8','3','6','9','0'}
    otxt[233..252] = {'1','0','3','7','3',',',' ','4','.','4','5','0','6','3','2','1','6','3','0','9'}
    otxt[253..272] = {'8','9','6','2','5',')',',',' ','(','1','.','7','5','5','7','2','4','8','6','8'}
    otxt[273..292] = {'9','0','2','7','7','9','3',',',' ','5','.','0',')',',',' ','(','1','.','6','4'}
    otxt[293..312] = {'3','5','2','2','0','7','9','6','3','2','7','9','7','6',',',' ','5','.','3','5'}
    otxt[313..328] = {'6','4','7','7','9','2','0','3','6','7','2','0','3',')',']',']'}
    spare = 328

 C:\Program Files (x86)\Phix\demo\edix\src\synclr.e:
    Xline = <novalue>
    Xno = -1
    closeRqd = {')',')',')',')','}',']',')',0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    wbl = 1
    startlevel = 0
    startblockcomment = 0
    startCfwd = {}
    exactmatch = 0
    abc = `#!]`
    abcl = 2
    ch2 = 44
    ctype = 6
    chidx = 40
    chidx2 = 40
    bcomm = 0
    lt = 39
    word = `ptr`
    backC[1..13] = {13290186,13290186,13290186,13290186,13290186,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256}
    backC[14..26] = {12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256}
    backC[27..39] = {12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256}
    backC[40..52] = {12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256}
    backC[53..65] = {12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256}
    backC[66..78] = {12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256}
    backC[79..91] = {12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256}
    backC[92..104] = {12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256}
    backC[105..117] = {12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256}
    backC[118..130] = {12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256}
    backC[131..140] = {12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256,12632256}
    textC[1..31] = {128,128,128,128,128,0,0,0,0,0,0,0,0,0,0,0,255,255,7364776,7364776,7364776,7364776,7364776,7364776,7364776,255,0,0,0,0,0}
    textC[32..65] = {0,0,255,0,0,0,0,255,16512,0,0,0,0,255,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128}
    textC[66..91] = {128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,17920,17920,17920,255,17920,17920,17920,17920,17920}
    textC[92..111] = {17920,255,17920,17920,17920,17920,17920,17920,255,17920,17920,17920,17920,17920,17920,8388608,255,17920,17920,17920}
    textC[112..139] = {17920,17920,17920,255,0,255,0,0,0,0,0,0,0,255,17920,17920,17920,17920,255,255,17920,255,255,17920,17920,17920,17920,255}
    textC[140] = 255
    attrC[1..60] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
    attrC[61..120] = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    attrC[121..140] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    r_findGlobal = 12393
    chunkMax = 36
    res[1..29] = {5,128,13290186,0,11,0,12632256,0,2,255,12632256,0,7,7364776,12632256,0,1,255,12632256,0,7,0,12632256,0,1,255,12632256,0,4}
    res[30..58] = {0,12632256,0,1,255,12632256,0,1,255,12632256,0,1,0,12632256,0,1,255,12632256,0,2,0,12632256,0,1,255,12632256,0,2,255}
    res[59..85] = {12632256,0,1,255,12632256,0,6,17920,12632256,0,1,8388608,12632256,0,1,255,12632256,0,6,17920,12632256,0,1,255,12632256,0,1}
    res[86..114] = {255,12632256,0,1,0,12632256,0,1,255,12632256,0,1,0,12632256,0,1,255,12632256,0,1,0,12632256,0,1,255,12632256,0,1,0}
    res[115..153] = {12632256,0,1,255,12632256,0,1,0,12632256,0,1,8388608,12632256,0,1,255,12632256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    res[154..160] = {0,0,0,0,0,0,0}
    chovline = 0
    chovfrom = <novalue>
    chovto = <novalue>
    chhovprevsnyclr = <novalue>
    chovinfo = <novalue>
    acfile = 0
    accY = -1
    acpkey = <novalue>
    acdata1 = <novalue>
    acrest = ``

 C:\Program Files (x86)\Phix\demo\edix\src\dateadj.e:
    dom = {31,28,31,30,31,30,31,31,30,31,30,31}

 C:\Program Files (x86)\Phix\demo\edix\src\eaedb.e:
    dbversion = {0,3,3}
    gMFstate = 0
    isOpen = 0
    alreadyBackGroundProcessing = 0
    knownDirectories[1..17] = {`C:\Program Files (x86)\Phix\docs\phix\src\`,0,0,0,0,0,0,0,0,0,0,0,0,0,0,`C:\Program Files (x86)\Phix\sfx\`,0}
    knownDirectories[18..19] = {`C:\Program Files (x86)\Phix\demo\edita\syn\`,`C:\Program Files (x86)\Phix\demo\edix\syn\`}
    knownDirectories[20..29] = {`C:\Program Files (x86)\Phix\demo\edita\help\`,`C:\Program Files (x86)\Phix\demo\edix\help\`,0,0,0,0,0,0,0,0}
    knownDirectories[30..47] = {0,0,0,0,`C:\Program Files (x86)\Phix\`,0,0,0,0,0,0,0,0,0,0,0,`C:\Program Files (x86)\Phix\pwa\js\`,0}
    knownDirectories[48..83] = {`C:\Program Files (x86)\Phix\demo\wee\`,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownDirectories[84..130] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,`C:\Users\Pete\`,0,0,0,0,0,0,0,0,0}
    knownDirectories[131..185] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownDirectories[186..240] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownDirectories[241..256] = {0,0,0,0,0,0,0,0,0,0,0,0,`E:\downloads\misc\wren\qemu\putty\`,0,0,0}
    knownFiles[1..46] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,{16,`phix7zip.lst`},0}
    knownFiles[47..61] = {{1,`toc.txt`},{1,`index.txt`},{18,`Euphoria.syn`},{19,`Phix.syn`},{20,`Phix.txt`},{21,`Phix.txt`},0,0,0,0,0,0,0,0,0}
    knownFiles[62..119] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[120..177] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[178..235] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[236..287] = {{19,`js.syn`},0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[288..336] = {0,0,0,0,0,{48,`window.ew`},0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[337..394] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[395..452] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[453..510] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[511..568] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[569..626] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[627..684] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[685..742] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[743..796] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[797..841] = {{'y',`edix.030919.cfg`},0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[842..899] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[900..957] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[958..1015] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[1016..1064] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,{46,`parse.js`},0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[1065..1122] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[1123..1180] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[1181..1238] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[1239..1296] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[1297..1354] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[1355..1412] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[1413..1470] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[1471..1528] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[1529..1586] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[1587..1644] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[1645..1702] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[1703..1760] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[1761..1818] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[1819..1876] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[1877..1926] = {0,0,{34,`test.py`},0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[1927..1984] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[1985..2042] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[2043..2100] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[2101..2158] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[2159..2216] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[2217..2274] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[2275..2332] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[2333..2390] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[2391..2448] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[2449..2506] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[2507..2564] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[2565..2622] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[2623..2680] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[2681..2738] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[2739..2796] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[2797..2854] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[2855..2894] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,{253,`helloworld.s`},0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[2895..2942] = {{253,`affichage.inc`},0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[2943..3000] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[3001..3058] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[3059..3116] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[3117..3174] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[3175..3232] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[3233..3290] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[3291..3348] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[3349..3406] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[3407..3464] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[3465..3522] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[3523..3580] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[3581..3638] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[3639..3696] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[3697..3754] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[3755..3812] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[3813..3870] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[3871..3928] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[3929..3986] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[3987..4044] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[4045..4102] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[4103..4160] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[4161..4218] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[4219..4266] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,{34,`fortune.exw`},0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[4267..4324] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[4325..4382] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[4383..4440] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[4441..4498] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[4499..4556] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[4557..4614] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[4615..4672] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[4673..4730] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[4731..4788] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[4789..4846] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[4847..4904] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[4905..4962] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[4963..5020] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[5021..5078] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[5079..5136] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[5137..5194] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[5195..5252] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[5253..5310] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[5311..5368] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[5369..5426] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[5427..5484] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[5485..5542] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[5543..5600] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[5601..5658] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[5659..5716] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[5717..5774] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[5775..5832] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[5833..5881] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,{34,`blog.txt`},0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    knownFiles[5882..5888] = {0,0,0,0,0,0,0}
    showErrors = 0
    guniq = 797
    gtkey = {121,`edix.030919.cfg`}
    gsize = 0
    gdate = ``
    gpath = `C:\Users\Pete\`
    globalCache[1..8] = {`or_all`,`HIWORD`,`LOWORD`,`alloc_string`,`free_strings`,`peek_string`,`allocWideCharString`,`peekWideCharString`}
    globalCache[9..14] = {`pack`,`allocate_pack`,`unpack`,`RegisterClass`,`CreateWindow`,`DialogBoxIndirectParam`}
    globalCache[15..20] = {`CreateDialogIndirectParam`,`GetOpenFileName`,`GetSaveFileName`,`GetFindFlags`,`GetFindWhat`,`GetReplaceWith`}
    globalCache[21..24] = {`FindText`,`ReplaceText`,`ChooseFont`,`ChooseColor`}
    globalFsets[1..2] = {{421,646,669,293,934,991,1851,3408,3419,730,4191,4216,4497,5785},{645,679,293,924,934,2868}}
    globalFsets[3..6] = {{645,679,293,924,934,2868},{293,934},{293,934,1851,3419},{403,421,669,293,934,991,1851,3012,3419,4191,4216}}
    globalFsets[7..13] = {{293,934},{293,934},{293,934,3169,3172,3605},{293,934},{293,934,3169,3172,3605},{293,934},{679,293,934,3376}}
    globalFsets[14..22] = {{293,934},{293,934},{679,293,934,4195},{293,934},{293,934},{293,934},{293,934},{293,934,5660,5669},{293,934}}
    globalFsets[23..24] = {{293,934,3376,5660,5669},{293,934}}
    rebuildProjectSet = 0
    bgIdx = <novalue>
    runFullBackgroundScan = 0
    purgeBackups = 2
    MacroKeys = {{},{},{},{}}

 C:\Program Files (x86)\Phix\demo\edix\edix.exw:
    insertMode = 1
    maxlen = 0
    toolb_rtns = 60654288
    prevRtnIdx = 0
    srl_once = 1
    lastFold = 0
    lastFoldEnd = 0
    cX = <novalue>

 C:\Program Files (x86)\Phix\demo\edix\src\ini.e:
    DATATYPE = {7,2,7,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,7,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,7,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5}
    HEADING[1..29] = {`Main`,`currfile`,`Extensions`,{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},`RunWiths`,{},{},{},{},{},{},{},{},{},{}}
    HEADING[30..52] = {{},{},{},{},{},`previous session`,{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{}}
    RESFLAG = {1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    VALUE[1..19] = {{},14,{},`bas`,`bat`,`eu`,`ex`,`exw`,`fb`,`go`,`htm`,`html`,`jl`,`php`,`pl`,`py`,`py35`,`rb`,{}}
    VALUE[20..24] = {`"C:\Go\FreeBASIC-1.05.0-win32\fbc.exe"`,`open`,`#exw`,`#exw`,`"C:\Program Files (x86)\Phix\pw9.exe"`}
    VALUE[25..28] = {`"C:\Go-old\FreeBASIC-1.05.0-win32\fbc.exe" -b`,`"C:\Go\bin\go.exe" run`,`open`,`open`}
    VALUE[29..31] = {`"C:\Users\Pete\AppData\Local\Programs\Julia\Julia-1.5.3\bin\julia.exe"`,`open`,`"C:\Strawberry\perl\bin\perl.exe"`}
    VALUE[32..35] = {`py27`,`"C:\Program Files (x86)\Python35-32\pythonw.exe"`,`"C:\Ruby22\bin\ruby.exe"`,{}}
    VALUE[36..37] = {`C:\Users\Pete\edix.cfg,{0,9,0,0,0,0,9,0,0,0,4}`,`C:\Users\Pete\edix.030919.cfg,{9,19,0,0,1,9,19,0,0,0,4}`}
    VALUE[38] = `C:\Program Files (x86)\Phix\demo\wee\window.ew,{0,0,0,0,0,0,0,0,0,0,4}`
    VALUE[39] = `C:\Program Files (x86)\Phix\docs\phix\src\toc.txt,{10,31,0,0,0,0,16,0,0,0,4}`
    VALUE[40] = `C:\Program Files (x86)\Phix\docs\phix\src\index.txt,{0,0,0,0,0,0,0,0,0,0,4}`
    VALUE[41] = `C:\Program Files (x86)\Phix\demo\edita\syn\Euphoria.syn,{0,0,0,0,0,0,0,0,0,0,4}`
    VALUE[42] = `C:\Program Files (x86)\Phix\demo\edix\syn\Phix.syn,{0,0,0,0,0,0,0,0,0,0,4}`
    VALUE[43] = `C:\Program Files (x86)\Phix\demo\edix\help\Phix.txt,{0,0,0,0,0,0,0,0,0,0,4}`
    VALUE[44] = `C:\Program Files (x86)\Phix\demo\edita\help\Phix.txt,{0,0,0,0,0,0,0,0,0,0,4}`
    VALUE[45] = `C:\Program Files (x86)\Phix\sfx\phix7zip.lst,{9,12,0,0,0,0,0,0,0,0,4}`
    VALUE[46] = `C:\Program Files (x86)\Phix\pwa\js\parse.js,{0,9,0,0,0,0,10,0,0,0,4}`
    VALUE[47] = `C:\Program Files (x86)\Phix\demo\edix\syn\js.syn,{23,473,428,0,0,0,473,0,0,0,4}`
    VALUE[48] = `C:\Program Files (x86)\Phix\blog.txt,{0,24,0,0,0,0,25,0,0,0,4}`
    VALUE[49] = `E:\downloads\misc\wren\qemu\putty\helloworld.s,{0,21,0,0,0,5,11,0,0,0,0}`
    VALUE[50] = `E:\downloads\misc\wren\qemu\putty\affichage.inc,{0,15,0,0,0,0,0,0,0,0,4}`
    VALUE[51] = `C:\Program Files (x86)\Phix\test.py,{0,0,0,0,0,0,0,0,0,0,4}`
    VALUE[52] = `C:\Program Files (x86)\Phix\fortune.exw,{0,0,0,0,0,0,21,0,0,0,4}`
    groupstart = 100
    groupend = 99
    completeinipath = `C:\Program Files (x86)\Phix\demo\edix\edix2.ini`
    wasNewIni = 0
    prevfiles = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    prevcursel[1..4] = {{0,9,0,0,0,0,9,0,0,0,5},{9,19,0,0,1,9,19,0,0,0,5},{0,0,0,0,0,0,0,0,0,0,5},{10,31,0,0,0,0,16,0,0,0,5}}
    prevcursel[5..8] = {{0,0,0,0,0,0,0,0,0,0,5},{0,0,0,0,0,0,0,0,0,0,5},{0,0,0,0,0,0,0,0,0,0,5},{0,0,0,0,0,0,0,0,0,0,5}}
    prevcursel[9..12] = {{0,0,0,0,0,0,0,0,0,0,5},{9,12,0,0,0,0,0,0,0,0,5},{0,9,0,0,0,0,10,0,0,0,5},{23,473,428,0,0,0,473,0,0,0,5}}
    prevcursel[13..16] = {{0,24,0,0,0,0,25,0,0,0,5},{0,21,0,0,0,5,11,0,0,0,1},{0,15,0,0,0,0,0,0,0,0,5},{0,0,0,0,0,0,0,0,0,0,5}}
    prevcursel[17] = {0,0,0,0,0,0,21,0,0,0,5}
    newcurrfile = 14

 C:\Program Files (x86)\Phix\demo\edix\src\ext.e:
    ext_dlg = 0
    matrix = <novalue>
    extname = <novalue>
    erunwith = <novalue>
    bt_help = <novalue>
    bt_save = <novalue>
    bt_okc = <novalue>
    titles = {`Extension`,`Run With`}
    extensions = {`bas`,`bat`,`eu`,`ex`,`exw`,`fb`,`go`,`htm`,`html`,`jl`,`php`,`pl`,`py`,`py35`,`rb`}
    runwiths[1..5] = {`"C:\Go\FreeBASIC-1.05.0-win32\fbc.exe"`,`open`,`#exw`,`#exw`,`"C:\Program Files (x86)\Phix\pw9.exe"`}
    runwiths[6..9] = {`"C:\Go-old\FreeBASIC-1.05.0-win32\fbc.exe" -b`,`"C:\Go\bin\go.exe" run`,`open`,`open`}
    runwiths[10..12] = {`"C:\Users\Pete\AppData\Local\Programs\Julia\Julia-1.5.3\bin\julia.exe"`,`open`,`"C:\Strawberry\perl\bin\perl.exe"`}
    runwiths[13..15] = {`py27`,`"C:\Program Files (x86)\Python35-32\pythonw.exe"`,`"C:\Ruby22\bin\ruby.exe"`}
    tags = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}
    ext_init = 1
    sortcol = 0
    sortdir = 1
    browse_dlg = 0

 C:\Program Files (x86)\Phix\demo\edix\src\find.e:
    find_dlg = 0
    find_txt = <novalue>
    find_case = <novalue>
    find_txts = {}
    find_start = 0
    eof_msg = 0

 C:\Program Files (x86)\Phix\demo\edix\src\dir.e:
    extensions = {58':'}
    treeItems = {}
    projItems = {{0,`edita!?!`,0,0}}
    backItems = {}
    resetPrev = 1
    lastfile = ``
    lastpath = <novalue>
    lastshow = 0
    currProjFileSet = {}

 C:\Program Files (x86)\Phix\builtins\dict.e:
    trees[1] = {}
    trees[2][1..5] = {1073807359.0,0,0,1,0}
    trees[2][6..10] = {1073807296.0,65472,16,2,1}
    trees[2][11..15] = {1073741895.0,536870983,41,3,6}
    trees[2][16..20] = {1073807203.0,536870974,0,1,0}
    trees[2][21..25] = {536870921,1073741886.0,51,2,0}
    trees[2][26..30] = {536936447,536871000,0,1,0}
    trees[2][31..35] = {536936291,536870979,21,3,26}
    trees[2][36..40] = {805306377,1073741884.0,31,4,11}
    trees[2][41..45] = {805371903,805306456,46,2,0}
    trees[2][46..50] = {805371747,805306435,0,1,0}
    trees[2][51..55] = {268500835,536870998,0,1,0}
    trees[3][1..5] = {`Ctrl V`,`Shift Insert`,26,3,16}
    trees[3][6..10] = {`Alt '>'`,`Ctrl '\t'`,21,3,41}
    trees[3][11..15] = {`Ctrl C`,`Ctrl Insert`,6,4,1}
    trees[3][16..20] = {`Ctrl X`,`Ctrl Delete`,0,2,46}
    trees[3][21..25] = {`Alt '<'`,`Ctrl Shift '\t'`,51,2,0}
    trees[3][26..30] = {`Ctrl Shift C`,`Ctrl Shift Insert`,36,2,31}
    trees[3][31..35] = {`Ctrl Shift X`,`Ctrl Shift Delete`,0,1,0}
    trees[3][36..40] = {`Ctrl G`,`Alt G`,0,1,0}
    trees[3][41..45] = {`Ctrl '>'`,`Alt Insert`,0,1,0}
    trees[3][46..50] = {`F3`,`Alt F3`,0,1,0}
    trees[3][51..55] = {"'???#00???'",`Alt Delete`,0,1,0}
    treenames = {`1`,``,``}
    roots = {0,36,11}
    sizes = {0,11,11}
    defaults = {0,0,0}
    freelists = {0,0,0}
    free_trees = 0
    init_dict = 1

 C:\Program Files (x86)\Phix\demo\edix\src\keyboard.e:
    KEYDICT = 2
    INVKEY = 3
    key_dlg = 0
    matrix = <novalue>
    tg_act = <novalue>
    tg_cxv = <novalue>
    bt_help = <novalue>
    bt_ok = <novalue>
    keymaps = {}
    standards = {}
    descs = {}
    tags = <novalue>
    selected_line = 0
    redraw_all = 0
    sortcol = 0
    sortdir = 1

 C:\Program Files (x86)\Phix\demo\edix\src\qj.e:
    ROUTINEWINDOW = 0
    All = <novalue>
    Globals = <novalue>
    Sections = <novalue>
    Filtxt = <novalue>
    Filter = <novalue>
    ROUTINELIST = <novalue>

 C:\Program Files (x86)\Phix\builtins\cffi.e:
    s = <novalue>
    ch = <novalue>
    sidx = <novalue>
    L = 4
    SizeNames = <novalue>
    SizeSigns = <novalue>
    Sizes = <novalue>
    as_char = <novalue>
    as_uchar = <novalue>
    as_int = <novalue>
    as_uint = <novalue>
    as_ptr = <novalue>
    as_short = <novalue>
    as_ushort = <novalue>
    as_long = <novalue>
    as_ulong = <novalue>
    as_float = <novalue>
    as_double = <novalue>
    as_int64 = <novalue>
    as_uint64 = <novalue>
    AltNames = <novalue>
    AltSize = <novalue>
    unicode = 0
    UnicodeNames = <novalue>
    UnicodeAs = <novalue>
    convert_types = 0
    structs = <novalue>
    stsizes = <novalue>
    saligns = <novalue>
    smembers = <novalue>
    dll_names = <novalue>
    dll_addrs = <novalue>
    C_SIZES = <novalue>
    C_CONSTS = <novalue>
    cffi_init = 0

 C:\Program Files (x86)\Phix\demo\edix\src\qj.e:
    routinescope = {}
    routinenames = {}
    routineparams = {}
    routinelinenumbers = {}
    routineends = {}
    BuiltinsPath = `"C:\Program Files (x86)\Phix\builtins`
    scope = <novalue>
    onScreen = {}
    clearFilterIfEmpty = <novalue>
    justDblClick = 0
    initialShow = 0
    RtnCharMap[1..57] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-2,0,1,0,0,0,0,0,0,0,0,-1,0,0,2,2,2,2,2,2,2,2,2}
    RtnCharMap[58..115] = {2,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1}
    RtnCharMap[116..173] = {1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    RtnCharMap[174..231] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    RtnCharMap[232..256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    params = `(atom hwnd, integer rgb)`
    incset = {}
    expro = 0
    isErrFile = 0
    HelpWin = 0
    HelpList = <novalue>
    Hexmncd = <novalue>
    HWclose = <novalue>
    Hkeybdh = <novalue>
    EurefWord = <novalue>
    KeyHelpText = 0
    targetFile = <novalue>
    targetLine = <novalue>
    text = <novalue>
    doConvTab = <novalue>
    fsIdx = <novalue>
    filepathname = <novalue>
    fileset = <novalue>
    word = <novalue>
    Xword = <novalue>
    Yword = <novalue>
    target = <novalue>
    lastHHname = {}
    xHtmlHelp = 0
    idHH_AKLINK = <novalue>
    xIsWindow = <novalue>
    xGetForegroundWindow = <novalue>
    xRegisterHotKey = <novalue>
    xUnregisterHotKey = <novalue>
    xSetWindowLongPtr = <novalue>
    xCallWindowProc = <novalue>
    xSendMessage = <novalue>
    chmwnd = 0
    lpPrevWndProc = 0
    hwnd = 0
    isRegistered = 0
    chmesc = 0
    rcX = <novalue>
    rcY = <novalue>

 C:\Program Files (x86)\Phix\demo\edix\edix.exw:
    cb_update_time = 13163912
    mouseDown = 0
    isCurrLineColoured = 1
    rCompareTarget = 0
    backdirs = {}
    backrev = {}
    backrevd = {}
    backpurge = {}
    toolb_new = 60634136
    toolb_open = 60636248
    toolb_save = 60637040
    toolb_close = 60639328
    toolb_prntp = 60637656
    toolb_print = 60640032
    toolb_cut = 60642144
    toolb_copy = 60643816
    toolb_paste = 60643552
    toolb_undo = 60642584
    toolb_redo = 60647864
    toolb_find = 60645576
    toolb_fnxt = 60649712
    toolb_fprv = 60650944
    toolb_zoomin = 60649272
    toolb_zoomout = 60651384
    toolb_back = 60651648
    toolb_run = 60655432

 C:\Program Files (x86)\Phix\demo\edix\src\macro.e:
    MacroLearn = 0
    MacroPlaying = 0
    r_macro_key = 20189

 C:\Program Files (x86)\Phix\demo\edix\src\undo.e:
    action = <novalue>

 C:\Program Files (x86)\Phix\demo\edix\src\withjs.e:
    js_dlg = 0
    main_dlg = <novalue>
    btns = <novalue>
    directives[1..4] = {`with javascript_semantics`,`without js -- (file i/o)`,`without js -- (libcurl)`,`without js -- (OpenGL 1.0)`}
    directives[5..7] = {`without js -- (multitasking)`,`without js -- (no class under p2js)`,`(none)`}
    js_callback = <novalue>
    srcline = 0
    insline = 0

 C:\Program Files (x86)\Phix\demo\edix\src\rein.e:
    isAlignIfdef = 1
    isStripSpaces = 1
    nest = <novalue>
    also = <novalue>
    reformat = <novalue>
    lexer_tokno = <novalue>
    parser_tokno = <novalue>
    r_CompleteIndents = 17451
    indentandor = <novalue>
    textlen = <novalue>
    CurrLine = <novalue>
    col = <novalue>
    tokline = <novalue>
    tokstart = <novalue>
    tokend = <novalue>
    toktype = <novalue>
    token = <novalue>
    withdefs = <novalue>
    vartypes = <novalue>
    stripleading = <novalue>
    striptrailing = <novalue>
    insertspaces = <novalue>
    adjust_count = <novalue>
    nTabs = <novalue>
    nSpaces = <novalue>
    Ch = <novalue>
    errtext = <novalue>
    xlt = <novalue>
    nWarns = <novalue>
    oline = <novalue>
    ocol = <novalue>
    charClass[1..19] = {99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c'}
    charClass[20..47] = {99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',1,2,6,99'c',99'c',1,4,1,1,1,1,1,1,5,1}
    charClass[48..90] = {7,7,7,7,7,7,7,7,7,7,1,1,1,1,1,1,99'c',10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}
    charClass[91..127] = {1,99'c',1,99'c',10,3,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,1,99'c',1,1,99'c'}
    charClass[128..146] = {99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c'}
    charClass[147..165] = {99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c'}
    charClass[166..184] = {99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c'}
    charClass[185..203] = {99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c'}
    charClass[204..222] = {99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c'}
    charClass[223..241] = {99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c'}
    charClass[242..255] = {99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c',99'c'}
    mapEndToMinusOne = 0
    base = -1
    ifdefstackidx = <novalue>
    ifdefnests = <novalue>
    ifdefnestz = <novalue>
    ifdefstate = <novalue>
    ifdefflags = <novalue>
    pptokline = <novalue>
    pptokstart = <novalue>
    pptokend = <novalue>
    pplastline = <novalue>
    pplaststart = <novalue>
    r_preprocess = 17435
    nPreprocess = 0
    nestAtStatementStart = 0
    ifdefcheck = <novalue>
    inexpression = 0
    r_completeifdef = 17363
    fromsubss = 0
    lastIline = <novalue>
    lastIaction = <novalue>
    lastIcount = <novalue>
    lastRexpr = <novalue>
    returnexpr = -1
    treatColonAsThen = <novalue>
    allow_dollar = 0
    in_class = 0
    fwd = 0
    space = 1
    align = 1
    status = <novalue>

 C:\Program Files (x86)\Phix\demo\edix\edix.exw:
    backgroundidx = 4
    charheight = 19
    charwidth = 9
    fontname = `Consolas`
    fontstyle = 0
    fontsize = 12
    cursorblink = 60657720
    cursorx = 46
    cursory = 916.375
    cursorc = 14211288
    file_new = 60612928
    file_open = 60614248
    file_reopen = 60612752
    file_save = 60612136
    file_saveas = 60614512
    file_list = 60615568
    file_close = 60614952
    file_prntp = 60616888
    file_print = 60615744
    recent_menu = 60617064
    file_recent = 60614424
    file_exit = 60068808

 C:\Program Files (x86)\Phix\demo\edix\src\cflow.e:
    oX = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    oY = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    oType = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    level = <novalue>
    targetLevel = <novalue>
    bX = <novalue>
    cB_end = `end`
    cB_endn = `end nested`
    cB_nestd = `nested`
    cB_global = {`global`,`local`,`public`,`export`,`override`,`nested`}
    cB_globali = <novalue>
    cB_set1 = {`if`,`switch`,`for`,`while`,`procedure`,`function`,`type`,`try`}
    cB_set2 = {`end`,`elsif`,`else`,`case`,`default`}
    cB_then = `then`
    cbX = <novalue>
    cbY = <novalue>

 C:\Program Files (x86)\Phix\demo\edix\edix.exw:
    backtimer = 60658336
    rotate_images = {`PNG_rotate1`,`PNG_rotate2`,`PNG_rotate6`,`PNG_rotate7`}
    rot_idx = 0

 C:\Program Files (x86)\Phix\demo\edix\src\run.e:
    msgcs = 0
    rmsg = 0
    wcount = 0
    rmsgs = <novalue>
    listener = <novalue>
    run_dlg = 0
    run_txt = <novalue>
    t_comp = <novalue>
    t_list = <novalue>
    l_type = <novalue>
    t_diag = <novalue>
    t_nrun = <novalue>
    t_lint = <novalue>
    last_cmd = ``
    prior_cmds = {}

 C:\Program Files (x86)\Phix\demo\edix\src\plan9.e:
    s1 = <novalue>
    s2 = <novalue>
    m = <novalue>
    n = <novalue>
    delta = <novalue>
    stack = <novalue>
    used = <novalue>
    width = <novalue>
    maxwidth = <novalue>
    stretch = <novalue>
    maxstretch = <novalue>
    complexity = <novalue>
    editdistance = <novalue>
    terminate = <novalue>

 C:\Program Files (x86)\Phix\demo\edix\src\htmlise.e:
    closing = 0
    r = <novalue>
    ct = <novalue>
    cfound = <novalue>
    hout[1..40] = {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}
    hout[41..84] = {-1,-1,-1,-1,-1,-1,-1,0,1,2,3,4,5,6,7,8,9,-1,-1,-1,-1,-1,-1,-1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}
    hout[85..124] = {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}
    hout[125..164] = {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}
    hout[165..204] = {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}
    hout[205..244] = {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}
    hout[245..256] = {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}
    i = 10
    i = 16
    hEstate = <novalue>

 C:\Program Files (x86)\Phix\demo\edix\src\goto.e:
    goto_dlg = 0
    goto_lbl = <novalue>
    goto_txt = <novalue>

 C:\Program Files (x86)\Phix\demo\edix\edix.exw:
    edit_undo = 59618568
    edit_redo = 61006360
    edit_cut = 61007064
    edit_cuta = 61007240
    edit_cutp = 61006800
    edit_copy = 61008560
    edit_copya = 61009616
    edit_copyp = 61009704
    edit_paste = 61009440
    copy_fname = 61012608
    edit_clip = 61011904
    edit_clips = 61014104
    edit_delete = 61011992
    edit_find = 61013928
    find_next = 61011464
    find_prev = 61013488
    edit_nextcs = 61011728
    edit_prevcs = 61015776
    edit_goto = 61016480
    edit_selall = 61014720
    toolm_run = 61015688
    toolm_prun = 61018592
    toolm_jerr = 61017360
    toolm_comp = 61019384
    toolm_fif = 61022024
    toolm_html = 61021496
    toolm_rein = 61021232
    toolm_cca = 61023344
    toolm_qjmp = 61023960
    toolm_recvr = 61022640
    toolm_desgn = 61025720
    toolm_dbvwr = 61025368
    toolm_dbvfy = 61026248
    toolm_showf = 61028536
    toolm_crash = 61029856
    macro_rec6 = 61031704
    macro_rec7 = 61031176
    macro_rec8 = 61033376
    macro_rec9 = 61032496
    macro_play6 = 61033288
    macro_play7 = 61035664
    macro_play8 = 61034872
    macro_play9 = 61036280
    reset_help_timer = 60612840
    reset_help = 0
    dumwin = 0
    options_file_panel = 61040592

 C:\Program Files (x86)\Phix\demo\edix\src\fif.e:
    fifdlg = 0
    find_text = <novalue>
    match_case = <novalue>
    whole_word = <novalue>
    save_settings = <novalue>
    dtext = <novalue>
    browse = <novalue>
    matrix = <novalue>
    progress = <novalue>
    bt_help = <novalue>
    bt_ok = <novalue>
    bt_cancel = <novalue>
    lines = {}
    extensions = {}
    elines = {}
    directories = {}
    dlines = {}
    degrid = {}
    tags = <novalue>
    ldrunning = 0
    todo = <novalue>
    todor = <novalue>
    vlines = 1
    hits = <novalue>
    charmash = <novalue>
    searchstring = <novalue>
    result = <novalue>
    linesave[1..59] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    linesave[60..80] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    col = <novalue>
    line_no = <novalue>
    fileNum = <novalue>
    files_scanned = <novalue>
    directories_scanned = <novalue>
    lines_scanned = <novalue>
    semiperm = <novalue>
    sortcol = 0
    sortdir = 1
    find_texts = {}
    browsedlg = 0

 C:\Program Files (x86)\Phix\demo\edix\edix.exw:
    F1menuitem = 60670128
    tmenu = 0
    t_new = <novalue>
    t_open = <novalue>
    t_reopen = <novalue>
    t_save = <novalue>
    t_saveas = <novalue>
    t_close = <novalue>
    cmenu = 0
    cm_cut = <novalue>
    cm_cuta = <novalue>
    cm_cutp = <novalue>
    cm_copy = <novalue>
    cm_copya = <novalue>
    cm_copyp = <novalue>
    cm_paste = <novalue>
    PTICK = `PNG_Ptick`
    PBACK = 0
    tproject = 60663088
    tdirectory = 60664848
--*/
