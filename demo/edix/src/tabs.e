--
-- eatabs.e
--
-- ExpandTabs, ExpLength, MapToByte, CursorLeft, WordLeft, CursorRight, WordRight, initT, packTabs
-- DEV ConvertTabs
--
--without trace
--with trace

constant 
--       QUOTE  = '\"',
--       ESCAPE = '\\',
         SPACE  = ' ',
         TAB    = '\t'

--Note: lines/second values are on a 233MHz (ie 0.2GHz) six-year-old AMD K6 with 64MB ram

--DEV broken (unless you disable/do not enable UTF8MODE)
-- what we need: ascii files held as strings, unicode files as utf8 strings, but the current line
--  should be expanded (iff any #80 spotted) to a dword-sequence. Where we currently have string,
--  that needs to be sequence s/if not string(s) and not unicodefile[currfile] then ?9/0 end if.
--  We should also strip the BOM. When displaying, need to pack back to utf8.
global integer showSpecials -- set via eamenus (toggleSpecials)
               showSpecials = 0


--back in easynld 29/7:
--global sequence wordChar      -- set by easynld.e
--global constant
-- TokenStart   = 1,
-- TokenChar    = 2,
---- TokenFirst = n(),      -- Not yet supported
---- TokenLast  = n(),      -- Not yet supported
-- OpenBrace    = 3,
-- CloseBrace   = 4,
-- Whitespace   = 5,
-- Delimiter    = 6,
-- Operator     = 7,                 
-- String       = 8,
-- Illegal      = 9

--global integer isEu                   -- must be set before calls to ExpandTabs/PackTabs.
--               isEu = 0


global function ExpandTabs(sequence text)
-- Replace any tabs with spaces.
-- NB: text is ansi string or UTF32 dword-sequence, never utf8
-- Fast (92,000 lines/second or better)
integer tab
    while 1 do
        tab = find(TAB,text)
        if not tab then exit end if
        text = text[1..tab-1]&
               repeat(SPACE,isTabWidth-remainder(tab-1,isTabWidth))&
               text[tab+1..length(text)]
    end while
    while 1 do
        tab = find(#0C,text)
        if not tab then exit end if
--DEV/SUG if string(text) then... (same several times)
        text[tab] = #A7                 -- form feed (page break)
--      text[tab..tab] = "\xC2\xA7"     -- form feed (page break)
    end while
    return text
end function

global function ExpandTabSpecials(sequence text)
-- Replace any tabs with spaces.
-- NB: text is ansi string or UTF32 dword-sequence, never utf8
-- Fast (92,000 lines/second or better)
integer tab, ch
--, c2c = 0
--  if showSpecials then
--      for i=1 to length(text) do
        for i=length(text) to 1 by -1 do
            ch = text[i]
            if ch=' ' then
--?unicodeflag
--              if unicodeflag=? then
--              end if
                text[i] = #B7
--              text[i..i] = "\xE2\x80\xA2"
--              text[i..i] = "\xC2\xB0"
--              text[i..i] = "\xC2\xB7"     -- middle dot
--              c2c += 1
            end if
        end for
--      tab = 1
--      while tab<=length(text) do
--          ch = text[tab]
--          if ch='\t' then
--              integer tw = isTabWidth-remainder(tab-c2c-1,isTabWidth)-1
--              text[tab..tab] = "\xC2\xBB"&repeat(SPACE,tw)
--              tab += tw+1
--              c2c += 1
--          end if
--          tab += 1
--      end while
        text = append(text,#B6) -- paragraph mark
--      text &= "\xC2\xB6"  -- paragraph mark
        while 1 do
            tab = find(TAB,text)
            if not tab then exit end if
            text = text[1..tab-1]&#BB&
--          text = text[1..tab-1]&#C2&#BB&
                   repeat(SPACE,isTabWidth-remainder(tab-1,isTabWidth)-1)&
--                 repeat(SPACE,isTabWidth-remainder(tab-c2c-1,isTabWidth)-1)&
                   text[tab+1..length(text)]
--          c2c += 1
        end while
--  else
--      while 1 do
--          tab = find(TAB,text)
--          if not tab then exit end if
--          text = text[1..tab-1]&
--                 repeat(SPACE,isTabWidth-remainder(tab-1,isTabWidth))&
--                 text[tab+1..length(text)]
--      end while
--  end if
    while 1 do
        tab = find(#0C,text)
        if not tab then exit end if
        text[tab] = #A7                 -- form feed (page break)
--      text[tab..tab] = "\xC2\xA7"     -- form feed (page break)
    end while
    return text
end function

global function ExpLength(sequence text)
-- calculate the length ExpandTabs would return (without all that slicing)
-- NB: text is ansi string or UTF32 dword-sequence, never utf8
-- Fast (147,000 lines/second or better)
integer l
    l = 0
    for i=1 to length(text) do
        if text[i]=TAB then
            l += isTabWidth-remainder(l,isTabWidth)
        else
            l += 1
        end if
    end for
    return l
end function

global function ExpLength8(sequence text)
-- NB: text is ansi string or UTF32 dword-sequence, never utf8
-- modified version of above for new tab8 handling
integer l
    l = 0
    for i=1 to length(text) do
        if text[i]=TAB then
            l += 8-remainder(l,8)
        else
            l += 1
        end if
    end for
    return l
end function


global function leadingWhiteSpaceLength(sequence text)
-- calculate the length of leading spaces and tabs.
-- NB: text is ansi string or UTF32 dword-sequence, never utf8
integer l, ch
    l = 0
    for i=1 to length(text) do
        ch = text[i]
        if ch=TAB then
            l += isTabWidth-remainder(l,isTabWidth)
        elsif ch=' ' then
            l += 1
        else
            return l
        end if
    end for
    return l
end function


global function MapToByte(sequence text, integer cX)
-- Map a screen column to a character byte.
-- text is packed (spaces replaced with tabs), 
-- NB: text is ansi string or UTF32 dword-sequence, never utf8
-- cX is zero-based (usually CursorX+Column).
-- result is an index (1..length(text)) or zero if past/on end.
integer l, ch
    l = 0
    for i=1 to length(text) do
        if l=cX then return i end if
        ch = text[i]
        if ch=TAB then
            l += isTabWidth-remainder(l,isTabWidth)
            if l>cX then return i end if    -- 18/5/07
        else
--      elsif ch!=#C2 then
            l += 1
        end if
    end for
    return 0
end function

global function CursorLeft(sequence text, integer CursorX)
-- Calculate next legal cursor position if moving left
-- text is packed. CursorX is 0-based.
-- NB: text is ansi string or UTF32 dword-sequence, never utf8
-- CursorX may be larger than ExpLength(txt)+1 (eg cursor up/down to a 
-- shorter line), in which case ExpLength(text) will be returned (which
-- places the cursor on the insertion point at the end of the line, past
-- the last character). If CursorX is zero, then -1 will be returned,
-- indicating the cursor should wrap to the end of the previous line.
-- Moderately Fast (65,000 lines/second or better)
integer l, d, lt
    l = 0
    d = 1
    lt = length(text)+1
    for i=1 to lt do
        if l+d>CursorX then exit end if
        l += d
        if i<lt and text[i]=TAB then
            d = isTabWidth-remainder(l-1,isTabWidth)
        else
            d = 1
        end if
    end for
    return l-1  -- -1 may be returned (ie cursor left in column 1(aka 0))
end function

--with trace
global function WordLeft(sequence text, integer CursorX, integer alt)
-- Calculate next word start (end if alt=1) to the left.
-- text is packed. CursorX is 0-based.
-- NB: text is ansi string or UTF32 dword-sequence, never utf8
-- -1 is returned if Cursor should go to previous line (if possible)
-- in no way is this meant/needed to be particularly fast.
integer l, charwidth, lt, pc, ws, ch
--trace(1)
    lt = length(text)
    while 1 do
        l = match("..",text)
        if l=0 then exit end if
        -- remove "..", "...", etc:
        for i=l to lt do
            ch = text[i]
            if ch!='.' then exit end if
            text[i] = ' '
        end for
    end while
--  l = 0 (already set from above)
    charwidth = 1
    pc = 0
    lt += 1
    ws = 0
    for i=1 to lt do
        if l+charwidth>CursorX then exit end if
        l += charwidth
        charwidth = 1
        if i<lt then
            ch = text[i]
            if ch=TAB then
                charwidth = isTabWidth - remainder(l-1,isTabWidth)
                if pc and alt then
                    ws = l
                end if
                pc = 0
            elsif ch>128 or wordChar[ch+1]=TokenChar then
                if alt then
                    if CursorX>l and i=lt-1 then
                        ws = l+1
                    end if
                else
                    if not pc then
                        ws = l
                    elsif ch='.' and CursorX>l then
-- 5/12/2010:
--                      ch = text[ws]
                        ch = text[i-1]
                        if ch<'0' or ch>'9' then
                            ws = l+1
                        end if
                    end if
                end if
                pc = 1
            else
                if pc and alt then
                    ws = l
                end if
                pc = 0
            end if
        end if
    end for
    return ws-1
end function

global function CursorRight(sequence text, integer CursorX)
-- Calculate next legal cursor position if moving right.
-- text is packed. CursorX is 0-based.
-- NB: text is ansi string or UTF32 dword-sequence, never utf8
-- Result will not be greater than ExpLength(text), which places the cursor 
-- on the insertion point at the end of the line, past the last character.
-- A result of -1 is returned to signal the cursor should wrap to the start 
-- of the next line.
-- Fast (124,000 lines/second or better)
integer l
    l = 0
    for i=1 to length(text) do
        if text[i]=TAB then
            l += isTabWidth-remainder(l,isTabWidth)
        else
            l += 1
        end if
        if l>CursorX then exit end if
    end for
    if l<=CursorX then return -1 end if -- line wrap rqd
    return l
end function

global function WordRight(sequence text, integer CursorX, integer alt)
-- Calculate next word start to right.
-- text is packed. CursorX is 0-based.
-- NB: text is ansi string or UTF32 dword-sequence, never utf8
-- CursorX may (specially) be -1 to indicate it should stop on
-- a word beginning in column 1. If CursorX=0 it should not.
-- -1 is returned if Cursor should go to next line (if possible)
-- in no way is this meant/needed to be fast.
integer l, ch, seenword, seengap, lt, r
    lt = length(text)
    while 1 do
        l = match("..",text)
        if l=0 then exit end if
        -- remove "..", "...", etc:
        for i=l to lt do
            ch = text[i]
            if ch!='.' then exit end if
            text[i] = ' '
        end for
    end while
--  l = 0 (already set from above)
    seenword = 0
    seengap = CursorX < 0
    lt = length(text)
    r = -1
    for i=1 to lt do
        ch = text[i]
        if ch>128 or wordChar[ch+1]=TokenChar then
            if l>CursorX then
                if alt then
                    if i=lt then r = l+1 exit end if
                else
                    if seengap then r = l exit end if
--                  if ch='.' then
--                      if sch<'0' or sch>'9' then
--                          
--                      end if
--                  end if
                end if
            end if
--          if seengap then
--              sch = ch
--          end if
            seengap = 0
            seenword = 1
            l += 1
        else
            if seenword and alt then
                if l>CursorX then r = l exit end if
                seengap = 0
                seenword = 0
            end if
            if ch=TAB then
                l += isTabWidth-remainder(l,isTabWidth)
            else
                l += 1
            end if
            if seenword 
            or l>=CursorX then
                seengap = 1
            end if
        end if
    end for
    return r
end function

--with trace
--
-- tabi (in PackTabs) is used to check for sufficient trailing spaces.
-- ie if column 1 is a space, and the next isTabWidth - 1 characters are 
-- also all spaces, then the lot can become a single tab character.
-- In column 2, we need one less trailing space, and so on.
-- For some columns, a tab and a space are equivalent (hence the 0 in 
-- nexttabi[2]), so we leave the space alone.
--
sequence nexttabi       -- eg {3,0,1,2} if isTabWidth is 4
--DEV no need to have this global, just use a flag (initT).
global procedure initT()        -- called once isTabWidth is known (see eaini.e)
    nexttabi = repeat(isTabWidth-1,isTabWidth)
--  nexttabi = repeat(7,8)
    for i=2 to isTabWidth do
--  for i=2 to 8 do
        nexttabi[i] = i-2
    end for
--  nexttabi = {7,0,1,2,3,4,5,6}
end procedure

--constant blank = repeat(' ',isTabWidth)

global integer fileTabLength
               fileTabLength=8
--with trace

--/*
--global --DEV [NOW UNUSED]
function PackTabs(sequence text, integer tabfound)
-- Replace multiple spaces with tab characters.
-- Moderately Fast (40,000 lines/second or better). --ERM...
-- tabfound should be zero for use in Edita, 1 for use in print preview.
integer i, tabi, replace, ch, wordstart
integer l
sequence word
object fWord
integer myIsEu
--integer tabfound
--trace(1)
    myIsEu = isEu
    i = 1
    tabi = isTabWidth - 1
--      tabi = 8 - 1
--      tabi = 7
--      tabfound = 0
    while i<=length(text) do
        ch = text[i]
        if ch=TAB then
--sort of like this...:
--                      tablen = isTabWidth-remainder(tab-1,isTabWidth)
            l = 0
            for j=1 to i-1 do
                if text[i]=TAB then
                    l += isTabWidth-remainder(l,isTabWidth)
                else
                    l += 1
                end if
            end for
            l = fileTabLength-remainder(l+1,fileTabLength)
            if l>tabi then
                if l=tabi+isTabWidth then
                    text = text[1..i]&text[i..length(text)] -- double up the tab
                    i += 1  -- and skip reprocessing it
--                                      tabi = nexttabi[tabi + 1]
                    tabi = 0
                else                                                                            -- insert excess spaces after the tab
                                                                                                                        -- (which must become spaces unless..)
                    text = text[1..i]&repeat(SPACE,l-tabi)&text[i+1..length(text)]
                    tabi = 0
                end if
            elsif l!=tabi then      -- less than, then: must become spaces anyway.
                text = text[1..i-1]&repeat(SPACE,l)&text[i+1..length(text)]
                ch = SPACE
            end if
--                      text = text[1..tab-1]&repeat(SPACE,tablen)&text[tab+1..length(text)]
--                      tab += tablen-1
--tabfound = 1
        end if
        if ch=SPACE then
--                      I tried this, but it's slower (!!) [get rid of "replace" var]
--                      if tabi and i+tabi<=length(text) and match(text[i+1..i+tabi],blank) then
            if tabi and not tabfound then
                replace = 1
                for j=i+1 to i+tabi do
                    if j>length(text)
                    or text[j]!=SPACE then
                        replace = 0
                        exit
                    end if
                end for
                if replace then
--                                      I tried this, too, but it's also slower (!!)
--                                      text[i] = TAB
--                                      text = text[1..i] & text[i+tabi+1..length(text)]
                    text = text[1..i-1] & TAB & text[i+tabi+1..length(text)]
                    tabi = 0
                end if
            end if
        elsif ch=QUOTE then -- skip strings (double quoted only)
            while i<length(text) do
                i += 1
                tabi = nexttabi[tabi + 1]
                ch = text[i]
                if ch=QUOTE then exit end if
                if ch=ESCAPE then
                    i += 1
                    tabi = nexttabi[tabi + 1]
                end if
            end while
        elsif ch='\'' then  -- char literals
            if i<length(text) and text[i+1]=ESCAPE then
                i += 1
                tabi = nexttabi[tabi+1]
            end if
            i += 1
            tabi = nexttabi[tabi+1]
        elsif ch = '-' and i<length(text) and text[i+1]='-' then
            myIsEu = 0
--DEV may need tweak for TokenFirst/Last:
        elsif Xtrans and myIsEu and ch<128 and charMap[ch+1]=TokenStart then
            wordstart = i
            while i<length(text) do
                i += 1
                ch = text[i]
                if ch>128 or charMap[ch+1]>TokenChar then
--??                                    if ctype=TokenLast then chidx2+=1 end if
                    i -= 1
                    exit
                end if
                tabi = nexttabi[tabi+1]
            end while
            word = text[wordstart..i]
            --                      fWord = xlQ(word) --DEV subset!
            --                      if not equal(fWord,word) then
            fWord = KtoF(word)
            --if atom(fWord) then   -- and on database...?
            if equal(fWord,word) then
                fWord = BtoF(word)
            end if
            --if sequence(fWord) then
            if not equal(fWord,word) then
                text = text[1..wordstart-1]&fWord&text[i+1..length(text)]
                i += length(fWord)-length(word) -- DEV fouls up nexttabi???
                tabi = isTabWidth-1-remainder(i-1,isTabWidth)
            end if
        end if
        i += 1
        tabi = nexttabi[tabi+1]
    end while
    return text
end function

--DEV this needs some serious testing! [NOW UNUSED]
--with trace
--global 
function UnPackTabs(sequence text)
-- Replace spaces with tabs and "untranslate" any keywords.
-- Replace tabs with spaces and "untranslate" any keywords.
integer tab, tablen, ch, wordstart
sequence word
object fWord
integer myIsEu
    myIsEu = isEu
--      if isEu then
--trace(1)
    tab = 1
    while tab<=length(text) do
        ch = text[tab]
        if ch=TAB then
            tablen = isTabWidth-remainder(tab-1,isTabWidth)
--          tablen = 8-remainder(tab-1,8)
            text = text[1..tab-1]&repeat(SPACE,tablen)&text[tab+1..length(text)]
            tab += tablen-1
        elsif ch=QUOTE then -- skip strings (double quoted only)
            while tab<length(text) do
                tab += 1
                ch = text[tab]
                if ch=QUOTE then exit end if
                if ch=ESCAPE then
                    tab += 1
                end if
            end while
        elsif ch='\'' then  -- char literals
            if tab<length(text) and text[tab+1]=ESCAPE then
                tab += 1
            end if
            tab += 1
        elsif ch='-' and tab<length(text) and text[tab+1]='-' then
            myIsEu = 0
--DEV may need tweak for TokenFirst/Last:
        elsif Xtrans and myIsEu and ch<128 and charMap[ch+1]=TokenStart then
            wordstart = tab
            while tab<length(text) do
                tab += 1
                ch = text[tab]
                if ch>128 or charMap[ch+1]>TokenChar then
--??            if ctype=TokenLast then chidx2+=1 end if
                    tab -= 1
                    exit
                end if
            end while
            word = text[wordstart..tab]
            fWord = FtoK(word)
--          if atom(fWord) then
            if equal(fWord,word) then
                fWord = FtoB(word)
            end if
--          if sequence(fWord) and not equal(fWord,word) then
            if not equal(fWord,word) then
                text = text[1..wordstart-1]&fWord&text[tab+1..length(text)]
                tab += length(fWord)-length(word)
            end if
        end if
        tab += 1
    end while
    return text
end function

if 0 then   -- suppress unused warnings [DEV]
    if PackTabs("", 0)!=UnPackTabs("") then end if
end if
--*/

sequence otxt
         otxt = ""
integer spare

procedure extend()
-- should only be invoked when processing a line longer than any prior.
-- (provided that "if spare<0 then" at start of ct is not commented out)
    while spare<0 do
        otxt &= ' '
        spare += 1
    end while
end procedure

--with trace
--DEV we need another flag here for translation...
global function ConvertTabs(sequence intxt, integer itw, integer otw)
--
-- input intxt, where tab width is itw,
-- returns text with tab width of otw.
--
-- Eg "\tZZ" input with itw=8 returns "\t\tZZ" when otw=4,
--                             or "        ZZ" when otw=0.
--
-- Note: This will crash if itw is zero and a tab is present in intxt,
--       though itw and/or otw of 0 generally/validly mean "all spaces".
-- NB: intxt/result is ansi string or UTF32 dword-sequence, never utf8
--
-- Multiple spaces/tabs within double quotes are not converted.
--
integer idx,    -- index to intxt
        odx,    -- index to otxt
        adx     -- actual char column
integer lt, ch, pc, k, ss, k2, ntabs, qc
--  if itw=otw then return intxt end if -- DEV later optimisation?
    lt = length(intxt)
    spare = length(otxt)-lt
    if spare<0 then
        otxt = repeat(' ',lt)
        spare = 0
    end if
--otxt=repeat(0,lt)
    pc = -1     -- previous char is not space
    idx = 1
    while idx<=lt do
        ch = intxt[idx]
        -- convert tabs (obviously), and multiple spaces which land on an otw boundary:
        if ch='\t' or (ch=' ' and pc=' ' and otw!=0 and remainder(idx,otw)=0) then
            adx = idx
            odx = idx-1
            while 1 do
                if ch='\t' or ch=' ' then
                    -- set k to 'additional' spaces the tab creates:
                    if ch='\t' then
                        k = itw-remainder(adx-1,itw)-1
                    else
                        k = 0
                    end if
                    -- accumulate all whitespace into one block.
                    --  first, pick up any skipped spaces:
                    ss = idx
                    while ss>1 and intxt[ss-1]=' ' do
                        ss -= 1
                    end while
                    ss = idx-ss
                    if ss then
                        k += ss
                        odx -= ss
                        adx -= ss
                    end if
                    --  then all consecutive tabs and spaces
                    for j=idx+1 to lt do
                        ch = intxt[j]
                        if ch!='\t' then
                            if ch!=' ' then exit end if
                            k += 1
                        else
                            k += itw-remainder(adx+k,itw)
                        end if
                        idx = j -- same as +=1 (but slightly faster!)
                        ss += 1
                    end for
                    -- now replace the lot with otw-sized tabs...
                    --  k now contains (whitespace_length-1), and
                    --  we are about to replace ss+1 characters.
                    if otw then
                        -- ... the first char is bound to fit (thankfully!)
                        -- set k2 to 'additional' spaces a tab would represent:
                        k2 = otw-1-remainder(adx-1,otw)
                        adx += k+1  -- update logical/actual char column with the lot now.
                        odx += 1
                        if k2 and k2<=k then
                            -- "" avoids replacing single spaces, 
                            -- and putting in a tab which don't fit.
                            --   (eg two spaces might need to be left as-is,
                            --       at least that is during >=second loop.)
                            otxt[odx] = '\t'
                            k -= k2
                        else
                            -- we may be replacing an input tab with spaces
                            -- here, or leaving a space as-is:
                            otxt[odx] = ' '
                        end if
                        -- check spare space in otxt...
                        ntabs = floor(k/otw)    -- number of tabs,
                        k = remainder(k,otw)    -- and remaining spaces
                        spare -= ntabs+k-ss
                        if spare<0 then extend() end if

                        if ntabs then
                            k2 = odx+1
                            odx += ntabs
                            otxt[k2..odx] = '\t'
                        end if
                        --                      while k>=otw do
                        --                          odx+=1
                        --                          otxt[odx]='\t'
                        --                          k-=otw
                        --                      end while
                    else
                        -- replacing all with spaces:
                        spare -= (k-ss) 
                        if spare<0 then extend() end if
                        k += 1 -- (non-0 otw branch does first specially, so one more space here)
                        adx += k
                    end if

                    k2 = odx+1
                    odx += k
                    otxt[k2..odx] = ' '
                else
                    odx += 1
                    adx += 1
                    otxt[odx] = ch
                    if ch='\"' or ch='\'' then
                        qc = ch
                        -- copy all chars verbatim until closing quote, with \" handling.
                        while 1 do
                            idx += 1
                            if idx>lt then exit end if
                            ch = intxt[idx]
                            odx += 1
                            adx += 1
                            otxt[odx] = ch

                            if ch=qc then exit end if
                            if ch='\\' then
                                -- ensure \" does not terminate the loop
                                idx += 1
                                if idx>lt then exit end if
                                ch = intxt[idx]
                                odx += 1
                                adx += 1
                                otxt[odx] = ch
                            end if
                        end while
--                      if idx>lt then exit end if -- handled well enough next anyway
                    end if
                end if
                idx += 1
                if idx>lt then exit end if
                ch = intxt[idx]
            end while
            return otxt[1..odx]
        elsif ch='\"' or ch='\'' then
            qc = ch
            -- copy all chars verbatim until closing quote, with \" handling.
            while 1 do
                otxt[idx] = ch
                idx += 1
                if idx>lt then exit end if
                ch = intxt[idx]
                if ch=qc then exit end if
                if ch='\\' then
                    otxt[idx] = ch
                    idx += 1
                    if idx>lt then exit end if
                    ch = intxt[idx]
                end if
            end while
            if idx>lt then exit end if

        end if
        otxt[idx] = ch
        pc = ch
        idx += 1
--      if idx>lt then exit end if
    end while
    return intxt    -- as it is unaltered (same as otxt[1..odx] by now, slice unnecc.)
end function

