--
-- cflow.e
-- =======
--
-- routines for control flow (structure) and bracket matching:
--

procedure mismatch()
--  void = messageBox("Warning","Bracket/Control structure mismatch",0)
    IupMessage("Warning","Bracket/Control structure mismatch")
end procedure

sequence oX,        -- The Stack: the position of opening '{', "if", etc are saved
         oY,        -- and either used or trashed when the '}', "end" etc found.
         oType      -- type info.
    oX = repeat(0,32)
    oY = repeat(0,32)
    oType = repeat(0,32)

integer level       -- stack pointer of saved stuff
integer targetLevel -- want stuff at this level
integer bX          -- CursorX of begin position, adjusted


procedure incLevel(integer x, integer y, integer eType)
-- save an opening '{', "if", etc.
    level += 1
    if level>0 then
        if level>length(oX) then
            oX &= repeat(0,32)
            oY &= repeat(0,32)
            oType &= repeat(0,32)
        end if
--DEV ioob here: (level=0)
        oX[level] = x
        oY[level] = y
        oType[level] = eType
        if y=CursorY+1 and x=bX then
            targetLevel = level                 -- find close (or elsif/else) for this
        end if
    end if
end procedure

--DEV??
function KtoF(string s) return s end function

--with trace
sequence cB_end, cB_global, cB_globali, cB_set1, cB_set2
    cB_end = KtoF("end")
--DEV public/export/override
--  cB_global = KtoF("global")
    cB_global = {KtoF("global"),KtoF("public"),KtoF("export"),KtoF("override")}
    cB_set1 = {KtoF("if"),KtoF("switch"),KtoF("for"),KtoF("while"),
-- 09/10/2020
--             KtoF("procedure"),KtoF("function"),KtoF("type")}
               KtoF("procedure"),KtoF("function"),KtoF("type"),KtoF("try")}
--DEV case/cB_set3
--  cB_set2 = {cB_end, KtoF("elsif"),KtoF("else")}
    cB_set2 = {cB_end, KtoF("elsif"),KtoF("else"),KtoF("case"),KtoF("default")}

--with trace
global integer cbX, cbY

global procedure ctrlBracket(integer direction)
--
-- process ctrl [ and ctrl ].
-- jump to matching [end] procedure/function/type/for/while;
-- cycle fwd/back through if/{elsif}/else/endif;
-- jump to matching [,], (,), {,}.
--
-- direction is -1 for backward, +1 for normal forward, or 
-- 0 for forward skipping elsif/else. (fold handling)
--
-- Result in cbX, cbY
--
-- Based on an idea by Bob Elia
--
sequence oneline    -- scratch var
integer k, k2, newY,
        ch,
        bracketmatch, -- 0=go for if/while etc; else go for {} etc
        bType,
        wordstart, wordend
sequence onword
sequence r          -- local copy of routines[currfile], speedwise
integer cS,         -- scratch var
        chunkStart, -- 1..start of 1st routine-1, or:
        chunkEnd    -- start..end of current routine, or:
                    -- end prev rtn+1.. start nxt rtn-1, or:
                    -- end last rtn+1.. end of file.
integer len,        -- length(oneline), speedwise
        skipword,   -- after "end"
        eWord       -- scratch (1=end, 2/3=elsif/else)

sequence diag       -- for crash problems

integer d0full      -- PL 20051106: (when direction=0 and elsif/else on CursorY+2, this
                    --               is set to 1 to force fold all the way to end if)
integer pchar

    d0full = 0

    cbX = CursorX
    cbY = CursorY
--?{"ctrlBracket",currfile,routines[currfile]}  -- routines not yet set [DEV]
--DEV rtnList??
--  if isEu and rtnList and currfile and sequence(routines[currfile]) then
    if currfile and sequence(routines[currfile]) then
--      if isEnabled(rtnList) then
            --
            -- First determine what we seek
            --
            oneline = filetext[currfile][CursorY+1]
            k = MapToByte(oneline,CursorX)
            if k then
                ch = oneline[k]
                -- 25/04/07 Match dbl quotes if we're on one:
                if ch='\"' or (k>1 and oneline[k-1]='\"') then
                    pchar = not(ch='\"')
                    r = syntaxColour(ExpandTabs(oneline),-1,0,0)
                    len = 0
                    for i=1 to chunkMax by 4 do
                        k2 = len+r[i]
                        if CursorX=len+pchar then cbX = k2-pchar return end if          -- start of string+/-1 -> end-/+1
                        if CursorX=k2-(1-pchar) then cbX = len+(1-pchar) return end if  -- end of string-/+1 -> start+/-1
                        len = k2
                    end for
                    r = {}
                end if
                bracketmatch = find(ch,"(){}[]")
            else
                k = length(oneline)+1
                bracketmatch = 0
            end if
            pchar = 0
            if not bracketmatch and k>1 then
                bracketmatch = find(oneline[k-1],"(){}[]")
                if bracketmatch then
                    pchar = 1
                    k -= 1
                end if
            end if
            if not bracketmatch then
                -- process eg "else" on the e, l, s, e, or just past, equally..
--              while k>1 and wordChar[oneline[k-1]+1]=TokenChar do
                while k>1 do
                    ch = oneline[k-1]
                    if ch<=128 and wordChar[ch+1]!=TokenChar then exit end if
                    k -= 1
                end while
                -- check for preceding "end" and move back onto it
--DEV getPrevWord()
                k2 = k
                while k2>1 and find(oneline[k2-1]," \t") do
                    k2 -= 1
                end while
                if k2>=length(cB_end)+1 and equal(oneline[k2-length(cB_end)..k2-1],cB_end) then
                    k = k2-length(cB_end)
                else
--DEV getOnWord()
                    -- check for leading "global" and move forward past it
                    for i=1 to length(cB_global) do
                        cB_globali = cB_global[i]
                        if k<=length(oneline)-length(cB_globali)
                        and equal(oneline[k..k+length(cB_globali)-1],cB_globali) then
                            k += length(cB_globali)
                            exit
                        end if
                    end for
                    -- skip spaces so we are on a recognised word
                    while k<=length(oneline) and find(oneline[k]," \t") do
                        k += 1
                    end while
                end if
            end if
            bX = k
            --
            -- Calculate the smallest area to scan
            --   routines[currfile] contains {names,starts,ends},
            --   eg {{"me","my"},{2,10},{8,18}}
            --
            r = routines[currfile]
            chunkStart = 1
            chunkEnd = length(filetext[currfile])
            for i=1 to length(r[rtnNAMES]) do
                cS = r[rtnSTART][i]
                if cS>CursorY+1 then chunkEnd = cS-1 exit end if
                chunkStart = cS
                cS = r[rtnEND][i]
                if cS>=CursorY+1 then chunkEnd = cS exit end if
                chunkStart = cS+1
            end for
            --setText(Main,sprint({chunkStart,chunkEnd}))
            --
            -- And scan!
            --
            level = 0
            targetLevel = -1
            skipword = 0
            if chunkStart<1 then return end if -- DEV 18/1/08 avoid "slice lower index is less than 1" (etc)
            diag = filetext[currfile][chunkStart..chunkEnd] -- save in case of crash
            for i=chunkStart to chunkEnd do
                oneline = filetext[currfile][i]
                len = length(oneline)
                k = 1
                while k<=len do
                    ch = oneline[k]
--                  if ch='-' and k<len and oneline[k+1]='-' then       -- skip comments,
                    if find(ch,"-/") and k<len and oneline[k+1]=ch then -- skip comments, (-- or //)
                        exit
                    elsif ch='\"' then                                  -- .. and strings,
                        while k<len do
                            k += 1
                            ch = oneline[k]
                            if ch='\"' then exit end if
                            if ch='\\' and k<len then k += 1 end if
                        end while
--21/01/2021
                    elsif ch='`' then                                   -- .. and backtick strings,
                        while k<len do
                            k += 1
                            ch = oneline[k]
                            if ch='`' then exit end if
                        end while
                    elsif ch='\'' then                                  -- .. and char lits (esp '\"'!)
                        if k<len and oneline[k+1]='\\' then
                            k += 1
                        end if
                        k += 2
                    elsif bracketmatch then                             -- count {} etc
                        bType = find(ch,"({[)}]")
                        if bType then
                            if bType<=3 then    -- "({["
                                incLevel(k,i,bType)                     -- remember open point
                            else                -- ")}]"
                                if i=CursorY+1 and k=bX then            -- jump to saved open point
                                    if level<=0 then return end if  -- (sanity check)
                                    if oType[level]!=bType-3 then
                                        mismatch()
                                    end if
--                                  cbX = ExpLength(filetext[currfile][CursorY+1][1..oX[level]-1])
--                                  cbX = ExpLength(filetext[currfile][CursorY+1][1..oX[level]-1])+(1-pchar)
                                    cbY = oY[level]-1
                                    cbX = ExpLength(filetext[currfile][cbY+1][1..oX[level]-1])+(1-pchar)
                                    return
                                elsif level=targetLevel and level!=-1 then      -- we wanted this close point
                                    if oType[level]!=bType-3 then
                                        mismatch()
                                    end if
--                                  cbX = ExpLength(oneline[1..k-1])
                                    cbX = ExpLength(oneline[1..k])-pchar
                                    cbY = i-1
                                    return
                                end if
                                level -= 1
                            end if
                        end if
                    elsif k<=length(oneline) and (ch>128 or wordChar[ch+1]=TokenChar) then
--DEV getOnWord()
                        wordstart = k
--                      while k<length(oneline) and wordChar[oneline[k+1]+1]=TokenChar do
                        while k<length(oneline) do
                            ch = oneline[k+1]
                            if ch<=128 and wordChar[ch+1]!=TokenChar then exit end if
                            k += 1
                        end while
                        wordend = k
                        if skipword then
                            skipword = 0
                        else
                            onword = oneline[wordstart..wordend]
                            bType = find(onword,cB_set1)    -- if/for/while/procedure/function/type
                            if bType then
                                incLevel(wordstart,i,bType)             -- remember control struct start
                            else
                                if direction=0 and d0full then  -- PL 20051106
                                    eWord = equal(onword,cB_end)
                                else
-- 11/3/11
--                                  eWord = find(onword,cB_set2)    -- end/elsif/else
                                    eWord = find(onword,cB_set2)    -- end/elsif/else/case/default
                                end if
                                if eWord then
                                    if i=CursorY+1 and wordstart=bX     -- on current word
                                    and (direction=-1 or eWord=1) then  -- going back or at end
                                        if level<=0 then return end if  -- (crash prevention)
                                        if eWord=1 then
--DEV getOnWord()
                                            while k<length(oneline) and find(oneline[k+1]," \t") do
                                                k += 1
                                            end while
                                            wordstart = k+1
--                                          while k<length(oneline) and wordChar[oneline[k+1]+1]=TokenChar do
                                            while k<length(oneline) do
                                                ch = oneline[k+1]
                                                if ch<=128 and wordChar[ch+1]!=TokenChar then exit end if
                                                k += 1
                                            end while
                                            wordend = k
                                            onword = oneline[wordstart..wordend]
                                            bType = find(onword,cB_set1)
                                            if oType[level]!=bType then
                                                mismatch()
                                            end if
--DEV (ditto below)
--                                      elsif oType[level]!=1 then  -- elsif/else must match "if"
                                        elsif (oType[level]<=0 or oType[level]>2)   -- if/switch only
                                           or (eWord=2 and oType[level]!=1)         -- elsif must match "if"
                                           or (eWord>3 and oType[level]!=2) then    -- case/default must match "switch"
                                            mismatch()
                                        end if
                                        newY = oY[level]
                                        oneline = filetext[currfile][newY][1..oX[level]-1]
                                        -- include preceding "global" (and spaces[s]) if present
                                        k = length(oneline)
                                        while k and find(oneline[k]," \t") do
                                            k -= 1
                                        end while
                                        for ig=1 to length(cB_global) do
                                            cB_globali = cB_global[ig]
                                            if k>=length(cB_globali)
                                            and equal(oneline[k-length(cB_globali)+1..k],cB_globali) then
                                                oneline = oneline[1..k-length(cB_globali)]
                                                exit
                                            end if
                                        end for
--                                      jumpTo(ExpLength(oneline),newY-1)
                                        cbX = ExpLength(oneline)
                                        cbY = newY-1
                                        return
                                    -- PL 20051106:
                                    elsif level=targetLevel
                                      and direction=0 and eWord>1       -- else/elsif found
                                      and i=CursorY+2 and d0full=0 then -- on very next line
                                        d0full = 1                          -- so fold to end if.
                                    elsif level=targetLevel             -- this level we want
                                      and (direction>=0     -- going forward, so "next" is ok, or...
                                           or eWord=1) then -- if going backward, wait for "end"
                                        -- jump over "end" and if/while/for/procedure/function/type:
                                        if eWord=1 then
--DEV getOnWord()
                                            wordstart += 3
                                            while wordstart<=length(oneline)
                                              and find(oneline[wordstart]," \t") do
                                                wordstart += 1
                                            end while
                                            wordend = wordstart
--                                          while wordend<=length(oneline) and wordChar[oneline[wordend]+1]=TokenChar do
                                            while wordend<=length(oneline) do
                                                ch = oneline[wordend]
                                                if ch<=128 and wordChar[ch+1]!=TokenChar then exit end if
                                                wordend += 1
                                            end while
                                            onword = oneline[wordstart..wordend-1]
                                            bType = find(onword,cB_set1)
                                            if level>0               -- 2/1/06 crash prevention
                                            and oType[level]!=bType then
                                                mismatch()
                                            end if
                                            wordstart = wordend
                                        elsif level>0                -- 2/1/06 crash prevention
--                                        and oType[level]!=1 then      -- elsif/else must match "if"
                                          and ((oType[level]<=0 or oType[level]>2) or   -- if/switch only
                                               (eWord=2 and oType[level]!=1) or -- elsif must match "if"
                                               (eWord>3 and oType[level]!=2)) then  -- case/default must match "switch"
                                            mismatch()
                                        -- PL 20051106:
                                        elsif direction=0 and d0full=0 then -- on elsif/else
                                            cbX = ExpLength(oneline[1..wordstart-1])
                                            cbY = i-2   -- don't include this line.
                                            return
                                        end if
--                                      jumpTo(ExpLength(oneline[1..wordstart-1]),i-1)
                                        cbX = ExpLength(oneline[1..wordstart-1])
                                        cbY = i-1
                                        return
--                                  elsif eWord!=1 and direction>=0 and i=CursorY+1 and wordstart=bX then
                                    elsif direction>=0 and i=CursorY+1 and wordstart=bX then
                                        -- fwd, on elsif/else: find next elsif/else/end at this level
                                        targetLevel = level
                                    end if
                                    if eWord=1 then
                                        skipword = 1
                                        level -= 1
                                    elsif direction=-1              -- jumping backward:
                                      and level>0 then                  -- (sanity chk: structure found)
                                        -- keep track; replace "if" with "elsif"/"else": 
                                        oX[level] = wordstart
                                        oY[level] = i
                                        --oType[level] remains unchanged (_should_ be 1, but let's not panic if not)
                                    end if
                                end if
                            end if
                        end if
                    end if
                    k += 1
                end while
            end for
--      end if
    end if
end procedure

