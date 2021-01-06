--
-- easynclr.e
--
-- syntax colour text, including current selection highlighting.
--
--DEV: 16/02/10 in PHP, '' is the null string.
--
include src\tabs.e

--without trace
--with trace
global sequence Xline
global integer Xno
               Xno = -1

sequence closeRqd   -- lifo stack of expected closing brackets
         closeRqd = repeat(0,32)

integer wbl

--global sequence bCfwd     -- defined in edix.exw
         bCfwd = {}
constant B_line=1, B_blockcomment=2, B_level=3, B_closesrqd=4
-- each element is {line, blockcomment, level, sequence}, where
-- line is the _first_ line this carry forward occured on,
-- blockcomment is 1 if a block comment carries over the end of this line,
-- level is the number of carry forward brackets (may be -ve),
-- sequence is {} for level <=0, else chunk of closeRqd.
--  Note that closeRqd is a lifo stack, hence after ([,
--  closeRqd will be ")]", not "])".


integer startlevel, startblockcomment
sequence startCfwd
integer exactmatch

procedure getBrktCfwd(integer lineno)
    exactmatch = 0
    if sequence(bCfwd) then --DEV prevent crash?
        for i=length(bCfwd) to 1 by -1 do
            if lineno>bCfwd[i][B_line] then
                startblockcomment = bCfwd[i][B_blockcomment]
                startlevel = bCfwd[i][B_level]
                startCfwd = bCfwd[i][B_closesrqd]
                closeRqd[1..startlevel] = startCfwd
                return
            elsif lineno=bCfwd[i][B_line] then
                exactmatch = 1
            end if
        end for
    end if
    startblockcomment = 0
    startlevel = 0
    startCfwd = {}
    return
end procedure

procedure cleanup(integer k)
--
-- Remove any obviously uneccessary entries around the insertion/update point.
-- It is not critical if a few odd ends get left in bCfwd, though.
--
integer thisline
    thisline = bCfwd[k][B_line]
    while k>1 do
        k -= 1
        if bCfwd[k][B_line]!=thisline then exit end if
        -- duplicate, from updateQJ()
        bCfwd = bCfwd[1..k-1]&bCfwd[k+1..length(bCfwd)]
    end while
    if k>1 and equal(bCfwd[k-1][2..4],bCfwd[k][2..4]) then
        bCfwd = bCfwd[1..k-1]&bCfwd[k+1..length(bCfwd)]
    end if
end procedure

procedure setBrktCfwd(integer lineno, integer bcomm, integer level)
-- Save the bracket carry-forward info for (the end of) the current line.
-- We can guarantee that getBrktCfwd has just been called for the current line.
integer k
sequence this
    if level!=startlevel
    or bcomm!=startblockcomment
    or exactmatch
    or not equal(closeRqd[1..level],startCfwd) then
        if sequence(bCfwd) then -- DEV hack!
            k = length(bCfwd)
            if level<0 then
                level = 0
            end if
--DEV?
--          if bcomm<0 then
--              bcomm = 0
--          end if
            while k do
--DEV tryme:
if 0 then
                if lineno>=bCfwd[k][B_line] then exit end if
else
                if lineno=bCfwd[k][B_line] then
                    bCfwd[k] = {lineno,bcomm,level,closeRqd[1..level]}
                    cleanup(k)
                    paintLast = 0
                    return
                elsif lineno>bCfwd[k][B_line] then
                    exit
                end if
end if
                k -= 1
            end while
--DEV tryme:
if 0 then
            this = {lineno,bcomm,level,closeRqd[1..level]}
            if k then
                bCfwd[k] = this
            else
                bCfwd = prepend(bCfwd,this)
            end if
else
            bCfwd = bCfwd[1..k]&{{lineno,bcomm,level,closeRqd[1..level]}}&bCfwd[k+1..length(bCfwd)]
end if
            cleanup(k+1)
            paintLast = 0
        end if
    end if
end procedure

sequence abc    -- a block comment open/close (scratch var)
integer abcl    -- length("")                 (ditto)

--with trace
global procedure rebuildbCfwd(integer maxline)
sequence line
integer chidx, ch, ch2, ctype, chidx1
integer bracket_level, lastlevel, bcomm, lastbcomm
--integer newbClen
sequence lastset
integer ll      -- length(line)
integer EuSq -- euphoria/phix style single quote handling
--integer lc
--atom t
--  t = time()

--DEV (temp??)
if maxline>length(filetext[currfile]) then
 puts(1,"oops, maxline>length(filetext[currfile])\n")
 maxline = length(filetext[currfile])
end if

    bracket_level = 0
    lastlevel = 0
    bcomm = 0
    lastbcomm = 0
    lastset = {}
    bCfwd = {}
--  bCfwd = repeat(0,256)
--  newbClen = 0
--  lc = length(comment)
--  for i=1 to length(filetext[currfile]) do
    for i=1 to maxline do
        line = filetext[currfile][i]
        ll = length(line)
        chidx = 1
        while chidx<=ll do
            ch = line[chidx]
if bcomm>=0 then
            -- check for block comment start, if not already in one:
--          if length(blockComment) and bcomm=0 then
            if length(blockComment) then
--              for bci=1 to length(blockComment) by 2 do
                for bci=1 to length(blockComment) do
                    abc = blockComment[bci]
                    abcl = length(abc)
                    if ch=abc[1]
                    and (abcl<2 or (chidx<ll and line[chidx+1]=abc[2]))
                    and (abcl<3 or (chidx+1<ll and line[chidx+2]=abc[3]))
                    and (abcl<4 or (chidx+2<ll and line[chidx+3]=abc[4])) then
--                      bcomm = bci
                        if remainder(bci,2) then
                            bcomm += 1
                        else
--DEV? (bcomm of -1 now means outstanding backtick/treblequote, not in a block comment)
--if bcomm then
if bcomm>0 then
                            bcomm -= 1
end if
                        end if
                        chidx += abcl-1
                        exit
                    end if
                end for
            end if
end if
            -- if in block comment scan for close block comment
--          if bcomm then
--for bci=2 to length(blockComment) by 2 do
----                abc = blockComment[bcomm+1] -- closing block comment
--              abc = blockComment[bci] -- closing block comment (allow any)
--              abcl = length(abc)
--              if ch=abc[1]
--              and (abcl<2 or (chidx<ll and line[chidx+1]=abc[2]))
--              and (abcl<3 or (chidx+1<ll and line[chidx+2]=abc[3]))
--              and (abcl<4 or (chidx+2<ll and line[chidx+3]=abc[4])) then
--                  chidx += abcl-1
--                  bcomm = 0
--exit
--              end if
--end for
--          else
--          if not bcomm then
            if bcomm<=0 then
                --DEV use the proper comment??
--              if ch='-' and chidx<ll and line[chidx+1]='-' then exit end if
--15/03/2010 replaced above:
if bcomm=0 then -- added 25/3/2013: (no help for the case I had found, but still might be rqd)
--10/07/20
--              if lc and ch=comment[1]
--              and (lc<2 or (chidx<ll and line[chidx+1]=comment[2]))
--              and (lc<3 or (chidx<ll-1 and line[chidx+2]=comment[3])) then
--                  exit
--              end if
                bool cfound = false
                for c=1 to length(lineComments) do
                    string comment = lineComments[c]
                    integer lc = length(comment)
                    if lc and ch=comment[1]
                    and (lc<2 or (chidx<ll and line[chidx+1]=comment[2]))
                    and (lc<3 or (chidx<ll-1 and line[chidx+2]=comment[3])) then
                        cfound = true
                        exit
                    end if
                end for
                if cfound then exit end if
end if
                if ch>128 then
                    ctype = TokenChar
                else
                    ctype = charMap[ch+1]
                end if
-- added 15/11/13:
                if bcomm<0 then
                    -- look for multi-line string end
                    ctype = String
                end if
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
--Rewrite 25/2/10:
--              elsif ctype=String then
--                  -- DEV this may need improving, esp for missing closing quotes:
----                    while chidx<length(line) do
--                  while chidx<ll do
--                      chidx += 1
--                      ch2 = line[chidx]
--                      if ch2=ch then exit end if          -- closing quote found
----                        if ch2=EscapeLeadIns[newSyntax] and length(Escapes[newSyntax]) then
--                      if ch2='\\' then
--                          chidx += 1
--                      end if  -- skip escaped chars
--                  end while
--===============
                elsif ctype=String then
--DEV need special handling for `(backtick) and """(treble quote)
--(set to -1 for any non-closed lines)
                    chidx1 = chidx
                    -- Euphoria/Phix has special single quote handling --DEV .syn file option
                    EuSq = (ch='\'' and equal(SynNames[newSyntax],"Euphoria"))
-- 15/11/13:
if bcomm<0 then
    if bcomm=-1 then
        ch = '`'
    elsif bcomm=-2 then
        ch = '"'
    else -- -3
        ch = '\''
    end if
    chidx -= 1
else
    if ch='`' then
        bcomm = -1
    elsif ch='"' and chidx<=ll-2 and equal(line[chidx..chidx+2],`"""`) then
        bcomm = -2
        chidx += 2
    end if
end if
                    while chidx<ll do
                        chidx += 1
                        ch2 = line[chidx]
                        if ch2=ch then      -- closing quote found
if bcomm=-2 then
    -- we're really looking for `"""`
    if chidx<=ll-2 and equal(line[chidx..chidx+2],`"""`) then
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
--DEV?
--      if bcomm<0 then
--          bcomm = 0
--      end if
        if bracket_level!=lastlevel or bcomm!=lastbcomm
        or (bracket_level>0 and not equal(lastset,closeRqd[1..bracket_level])) then
            lastlevel = bracket_level
            lastbcomm = bcomm
--          if bracket_level<=0 then
--              lastset = {}
--          else
                lastset = closeRqd[1..bracket_level]
--          end if
--if i=1092 then trace(1) end if
            bCfwd = append(bCfwd,{i,bcomm,lastlevel,lastset})
--          newbClen += 1
--          if newbClen>length(bCfwd) then
--              bCfwd &= repeat(0,256)
--          end if
--          bCfwd[newbClen] = {i,??,lastlevel,lastset}
        end if
    end for
--  bCfwd = bCfwd[1..newbClen]
--  ?time()-t
--  ?length(bCfwd)
end procedure

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
-- Scan for urls
integer chidx0
integer syntaxClass
--trace(1)
    abcl = length(abc)  -- (abc="" means line comment scanning)
    while chidx2<=lt do
        ch2 = text[chidx2]
        if ch2>128 then
            ctype = TokenChar
        else
            ctype = wordChar[ch2+1]
        end if
        if ctype=TokenChar then
            --
            -- mark first part as comment
            --
            textC[chidx..chidx2-1] = ColourTab[Comments]
            attrC[chidx..chidx2-1] = StyleTab[Comments]
            chidx = chidx2
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
                    end if
                    if urlChar[ch2+1]!=TokenChar then exit end if
                    syntaxClass = URLs
                    chidx2 += 1
                end while
                textC[chidx..chidx2-1] = ColourTab[syntaxClass]
                attrC[chidx..chidx2-1] = StyleTab[syntaxClass]
--              if chidx0!=chidx2 then
--                  textC[chidx..chidx2-1] = ColourTab[URLs]
--                  attrC[chidx..chidx2-1] = StyleTab[URLs]
--              else
--                  textC[chidx..chidx2-1] = ColourTab[Comments]
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

global integer r_findGlobal
--with trace
global integer chunkMax
               chunkMax = 0
sequence res
         res = repeat(0,32)

global integer chovline
               chovline = 0
global integer chovfrom,
               chovto,
               chhovprevsnyclr,
               chovinfo

integer acfile
        acfile = 0
integer accY
        accY = -1
sequence acpkey, 
         acdata1, 
         acrest
         acrest = ""

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
        if keycode=K_ESC then
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

--with trace
global function syntaxColour(sequence text, integer lineno, integer marginWidth, integer firstch)
--
-- Create a "chunk" table of colours for the current line.
-- The chunk table is length, text colour, background colour, attr,
--  repeated as necessary.
-- The chunk table length is therefore always a multiple of 4.
-- lineno (0-based) is used to highlight the current selection
--
integer
        ch,                 -- current character
        bracket_level,      -- as per syncolor.e
        bl                  -- BraceLevel[newSyntax]


--sequence res
--integer cM4
-- 15/4/2010:
--atom syntaxClass
integer syntaxClass

integer k
atom current_back, current_text, current_attr
integer current_len

integer fromX, fromY, toX, toY

--sequence fWord
--integer fDiff

integer resetBackSlash, wasSlash, wasDot

integer EuSq -- euphoria/phix style single quote handling

integer allNumbers

--integer lc
--trace(1)
--if chovline!=0 then trace(1) end if
    lt = length(text)
--  lc = length(comment)
--  if not lt then return {} end if     18/7
    chunkMax = 0
    if not lt then
        getBrktCfwd(lineno)
        setBrktCfwd(lineno,startblockcomment,startlevel)
        return {}
    end if
--DEV make these permanent (for speed)... integer maxlt
    if length(backC)<lt then
        backC = repeat(ColourTab[Background],lt)
        textC = repeat(ColourTab[Other],lt) -- was Background 22/6/05
--      attrC = repeat(EA_Normal,lt)
        attrC = repeat(CD_PLAIN,lt)
-- 29/04/2010 (replaced with else clause):
--  end if
----DEV machine exceptions (2.4)
----    backC[1..length(backC)]=ColourTab[Background]
--  for i=1 to length(backC) do
--      backC[i] = ColourTab[Background]
--  end for
    else
        for i=1 to lt do
            backC[i] = ColourTab[Background]
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
                            if urlChar[text[chidx2]+1]!=TokenChar then exit end if
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
            allNumbers = (ch>='0' and ch<='9') or ch='.'
            while chidx2<=length(text) do
                ch = text[chidx2]
--              if charMap[ch+1]!=TokenChar then
                if ch<128 and charMap[ch+1]!=TokenChar then
                    if not allNumbers then exit end if
                    if ch!='.' then exit end if
                    allNumbers = 0
                    if chidx2=length(text) then exit end if
                    ch = text[chidx2+1]
                    if ch<'0' or ch>'9' then exit end if
                    chidx2 += 1
                end if
                if ch<'0' or ch>'9' then
                    allNumbers = 0
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
            wbl = bracket_level
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
          and ((length(lineComments) and ch=lineComments[1][1]
                and (length(lineComments[1])<2 or (chidx2<=length(text) and text[chidx2]=lineComments[1][2]))
                and (length(lineComments[1])<3 or (chidx2<length(text) and text[chidx2+1]=lineComments[1][3]))) or
               (length(lineComments)=2 and ch=lineComments[2][1]
                and (length(lineComments[2])<2 or (chidx2<=length(text) and text[chidx2]=lineComments[2][2]))
                and (length(lineComments[2])<3 or (chidx2<length(text) and text[chidx2+1]=lineComments[2][3])))) then
            abc = ""  -- (signals line comment scan)
            scanForUrls(text)
            syntaxClass = Comments
        elsif length(lineComments)>2 then
            ?9/0 -- placeholder for more code (three line comments?!)
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
--          backC[chidx] = ColourTab[Background]    -- already set
-- 15/07/2013:
            while chidx2<=length(text) do
                ch = text[chidx2]
                if ch>=128 or charMap[ch+1]!=Whitespace then exit end if
                chidx2 += 1
            end while
        elsif ctype!=Comment then                       syntaxClass = Other
        end if
--27/1/18:
if 0 then
--DEV test added 22/2/16...
 if chidx2-1<=lt then
        textC[chidx..chidx2-1] = ColourTab[syntaxClass]
        attrC[chidx..chidx2-1] = StyleTab[syntaxClass]
 end if
else
        if chidx2-1>lt then
            chidx2=lt+1
        end if
        textC[chidx..chidx2-1] = ColourTab[syntaxClass]
        attrC[chidx..chidx2-1] = StyleTab[syntaxClass]
end if

        chidx = chidx2
    end while
    if resetBackSlash then
        charMap['\\'+1] = wasSlash
        charMap['.'+1] = wasDot
    end if
    setBrktCfwd(lineno,bcomm,bracket_level)
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
--              backC[fromX..toX-1] = ColourTab[Highlight]
                for i=fromX to toX-1 do                             -- 08/05/07
                    if backC[i]=textC[i] then
                        textC[i] = ColourTab[Highlight]
                    end if
                    backC[i] = ColourTab[Highlight]
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
            backC &= repeat(ColourTab[Background],lt-length(backC))
            textC &= repeat(ColourTab[Comments],lt-length(backC))
--          attrC &= repeat(EA_Italic,lt-length(backC))
            attrC &= repeat(CD_ITALIC,lt-length(backC))
        end if
--DEV colour/attr of autocompletes should be configurable
        for i=k+1 to lt do
            backC[i] = ColourTab[Background]
            textC[i] = ColourTab[Comments]
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
    current_back = backC[1]
    current_text = textC[1]
    current_attr = attrC[1]
    current_len = 0
--  res = {}
--  chunkMax = 0
    for i=1 to lt do
        if current_back!=backC[i] 
        or current_text!=textC[i]
        or current_attr!=attrC[i] then
--          res &= {current_len,current_text,current_back,current_attr}
            chunkMax += 4
            if length(res)<chunkMax then
                res &= repeat(0,32)
            end if
            res[chunkMax-3] = current_len
            res[chunkMax-2] = current_text
            res[chunkMax-1] = current_back
            res[chunkMax  ] = current_attr
            current_len = 0
            current_back = backC[i]
            current_text = textC[i]
            current_attr = attrC[i]
        end if
        current_len += 1
    end for
    if current_len then
--      res &= {current_len,current_text,current_back,current_attr}
        chunkMax += 4
        if length(res)<chunkMax then
            res &= repeat(0,32)
        end if
        res[chunkMax-3] = current_len
        res[chunkMax-2] = current_text
        res[chunkMax-1] = current_back
        res[chunkMax  ] = current_attr
    end if
    return res
end function
