--
-- eacca.e
-- =======
--
-- Common code analysis
-- Author Pete Lomax 8th September 2004
--
-- Creates a listing file (ccaout.e) showing equivalent groups of lines of code,
--  with preference to the largest matching blocks. The idea is to help reduce code duplication.
--  Comments, whitespace (but not line breaks), and ubiquitous lines (eg "end if") are skipped.
--  Reasonably fast, eg win32lib should complete in under a minute.
--
-- Example output:
--
-->9093  9148 12471 13056 14310  +      hWnd = getHandle( id )
-- 9094                                     curStyle = w32Func( xGetWindowLong, { hWnd, GWL_STYLE })
-->9095  9105                               styleMask = xor_bits( #FFFFFFFF, style )
-- 9096    |                                style = and_bits( styleMask ,curStyle )
-- 9097  9107  9159  9168                   if style != curStyle then
-- 9098        9160                             VOID = w32Func( xSetWindowLong,{ hWnd, GWL_STYLE, style })
--
-- Line 9093 matches more than 5 other lines so has a '+' in column 6.
-- Line 9094 does not match any other line in the source, so does not begin with '>'
--  (The '>' allow F4 to jump to the next line/group of interest.)
--  ('>' are only printed in column 1 for the start of groups.)
-- The largest group, {9095,9096,9097}==(9105,9106,9107}) is shown in the left column
--  Lines between the first and last are shown as '|' to make large groups stand out.
-- The second largest group ({9097,9098}=={9159,9160}) is shown in the next column.
--  Note how it is vertically aligned.
-- The smallest groups are added last when filling the matching line columns, which
--  are always filled left to right avoiding overlap with any previous (larger) groups.
-- Lastly, group analysis deliberately skips equal numbers of blank or comment-only lines.
--  For example if lines 9096 and 9106 were both commented out, it would still be
--  reported as a group of three matching lines, unless extra comment lines are inserted
--  in one and not the other. (not shown in the above example)
--

without trace

--/* 4.0.2
include builtins\sort.e
include builtins\file.e
--*/

sequence result

sequence text
sequence wordlist, uniqlines, matchtable, linetable, oneline

integer lineno, ch, baseline

integer cancelled, PROGvis
        PROGvis = 0

constant CCPROGRESS = create(Window,"Progress",0,Main,216,232,643,162,0)
constant CCPtext = create(Label,"",0,CCPROGRESS,20,28,599,20,0)
constant CCcncl = create(Button,"Cancel",0,CCPROGRESS,203,79,75,30,0)

function cancelHandler(integer id, integer msg, atom wParam, object lParam)
    if wParam or object(lParam) then end if -- suppress warnings
    if id=CCcncl and msg=WM_COMMAND then
        lineno = length(text)
        cancelled = 1
    end if
    return 0
end function
setHandler(CCcncl,routine_id("cancelHandler"))

procedure progress(sequence msg)
    setText(CCPtext,msg)
    if not PROGvis then
        openWindow(CCPROGRESS,SW_NORMAL)
        addFocus(CCPROGRESS)
        PROGvis = 1
    end if
    doEvents(0)
end procedure

function abs(integer i)
    if i<0 then return -i else return i end if
end function


procedure CCopen()
    wordlist = {}   -- list of encountered words/symbols (unique).
    oneline = {}    -- a single packed line from the source (list of indexes to wordlist).
    uniqlines = {}  -- previously seen source lines (copies of oneline).
    matchtable = {} -- list of source line nos corresponding to each uniqline entry.
    linetable = repeat(0,32)    -- for each source line, index to uniqlines.

    lineno = 1
    cancelled = 0

    if not selON or (abs(selY-CursorY)<10) then
        text = filetext[currfile]
        baseline = 0
    else
        text = getSelection(SEL_COPY)
        baseline = Min(selY,CursorY)
    end if
end procedure

procedure insertword(sequence word)
integer k
    k = find(word,wordlist)
    if not k then
        wordlist = append(wordlist,word)
        k = length(wordlist)
    end if
    oneline = append(oneline,k)
end procedure

constant symbol = "!#$%&()*+,./:;<=>?@]\\[^{|}",
         digits = "0123456789"
sequence alphanum
         alphanum = repeat(0,256)
         alphanum['0'+1..'9'+1] = 1
         alphanum['A'+1..'Z'+1] = 1
         alphanum['_'+1] = 1
         alphanum['a'+1..'z'+1] = 1
         alphanum[127..256] = 1

integer column
        column = 0

function GetCh()
    if lineno>length(text) then
        return -1
    end if
    column += 1
    if column>length(text[lineno]) then
        column = 0
--      lineno += 1
        return '\n'
    end if
    return text[lineno][column]
end function

procedure ccabort(sequence msg)
    void = messageBox("Error",msg&sprintf(" on line %d",lineno),0)
    jumpTo(0,lineno-1)
    lineno = length(text)+1
    cancelled = 1
end procedure

function getpackedline()
sequence oneword

    oneline = {}
    while 1 do
        if ch=-1 then return length(oneline) end if
        if find(ch,"\r\n") then
            while find(ch,"\r\n") do
                if ch='\n' then
                    if length(oneline) then return 1 end if
                    lineno += 1
                end if
                ch = GetCh()
            end while
        elsif find(ch," \t") then
            while find(ch," \t") do
                ch = GetCh()
            end while
        elsif ch='-' then
            ch = GetCh()
            if ch='-' then
                while not find(ch,{'\r','\n',-1}) do
                    ch = GetCh()
                end while
            else
                insertword("-")
            end if
        elsif ch='\'' then
            ch = GetCh()
            if ch='\\' then
                ch = GetCh()
                insertword('\\'&ch)
            else
                insertword({ch})
            end if
            ch = GetCh()
            if ch!='\'' then
                ccabort("trailing \' missing")
                return 0
            end if
            ch = GetCh()
        elsif ch='\"' then
            oneword = ""
            while 1 do
                oneword &= ch
                ch = GetCh()
                if ch='\\' then
                    ch = GetCh()
                elsif ch='\"' then
                    exit
                elsif find(ch,{'\t','\r','\n',-1}) then
                    ccabort("malformed string")
                    return 0
                end if
            end while
            ch = GetCh()
            if ch='\"' and oneword=`"` then
                -- tripe-quote
                oneword = `"""`
                integer tqc = 0
                while true do
                    ch = GetCh()
                    oneword &= ch
                    if ch=-1 then
                        ccabort("missing closing triplequote")
                        return 0
                    end if
                    if ch='\"' then
                        tqc += 1
                        if tqc=3 then exit end if
                    else
                        tqc = 0
                    end if
                end while
                ch = GetCh()
                insertword(oneword)
            else
                insertword(oneword&`"`)
            end if
        elsif ch='`' then
            -- tripe-quote
            oneword = `"`
            while true do
                ch = GetCh()
                oneword &= ch
                if ch=-1 then
                    ccabort("missing closing backtick")
                    return 0
                end if
                if ch='`' then exit end if
            end while
            ch = GetCh()
            insertword(oneword)
        elsif find(ch,symbol) then
--PL this allowed eg "]&" to be recorded as one symbol...
--            oneword = {}
--            while find(ch,symbol) do
--                oneword &= ch
--                ch = GetCh()
--            end while
--            insertword(oneword)
            insertword({ch})
            ch = GetCh()
        elsif find(ch,digits) then
            oneword = {}
            while find(ch,digits) do
                oneword &= ch
                ch = GetCh()
            end while
            insertword(oneword)
        else
            oneword = {}
            while alphanum[ch+1] do
                oneword &= ch
                ch = GetCh()
            end while
            if not length(oneword) then
                ccabort("length zero word") -- should not happen
                return 0
            end if
            insertword(oneword)
        end if
    end while
end function

integer k
procedure insertline()
    k = find(oneline,uniqlines)
    if k then
        if length(matchtable[k]) then   -- skipping ignore'd lines
            matchtable[k] = matchtable[k] & lineno
        end if
    else
        uniqlines = append(uniqlines,oneline)
        matchtable = append(matchtable,{lineno})
        k = length(matchtable)
    end if
    while lineno>length(linetable) do
        linetable &= repeat(0,32)
    end while
    linetable[lineno] = k
end procedure

procedure ignore(sequence phrase)
    oneline = {}
    for i=1 to length(phrase) do
        insertword(phrase[i])
    end for
    insertline()
    matchtable[length(matchtable)] = {}
end procedure
    --
    -- Initialisation:
    -- (These are *complete* lines we're not interested in)
    --
procedure CCinit()
    ignore({"end","if"})
    ignore({"end","for"})
    ignore({"end","while"})
    ignore({"then"})
    ignore({"else"})
    ignore({"end","function"})
    ignore({"end","procedure"})
    ignore({"end","type"})
    ignore({"with","trace"})
    ignore({"without","trace"})
    ignore({"return"})
    ignore({"return","0"})
    ignore({"return","1"})
    ignore({"{"})
    ignore({"}"})
    ignore({"})"})
    ignore({"}",")"})
    ignore({")"})
    ignore({"global","constant"})
    ignore({"global","integer"})
    ignore({"global","atom"})
    ignore({"global","sequence"})
    ignore({"constant"})
    ignore({"integer"})
    ignore({"atom"})
    ignore({"sequence"})
    ignore({"exit"})
    result = {"-- Common Code Analysis of "&filepaths[currfile]&filenames[currfile]&" --"}
    result = append(result,repeat('-',length(result[1])))
    result = append(result,"-- Consider migrating duplicate code to a single subroutine.")
    result = append(result,"-- Press F4 to jump to the next area of interest.")
end procedure

constant interval = 1   -- Progress reports
atom t

procedure CCload()
    --
    -- Load phase:
    --
    t = time()+interval
    ch = GetCh()
    while getpackedline() do
        if time()>t then
            progress(sprintf("Loading line %d of %d",{lineno,length(text)}))
            t = time()+interval
        end if
        insertline()
        if cancelled then exit end if
    end while
end procedure

--with trace
sequence groupset
procedure CCgroupanalysis()
--
-- Group analysis:
--
integer packedlineref   -- index to uniqlines for current line
sequence matchlineset   -- set of (equal) lines "" corresponds to
integer matchline       -- one of ""
integer topofgroup      -- flag for checking for top of group
integer prevlineref     -- index to uniqlines for previous line(s)
sequence prevlineset    -- set of (equal) lines "" corresponds to
integer prevmatchline   -- corresponding to matchline
--integer prevmatchref  -- which is here in uniqlines
integer endofgroup      -- last line found so far in group
integer nextlineref     -- index to uniqlines for following line(s)
integer nextmatchline   -- corresponding to matchline
integer nextmatchref    -- which is here in uniqlines
sequence nextlineset
sequence bestgroup
    groupset = {}
    t = time()+interval
    for sourceline=1 to length(linetable) do
        if time()>t then
            progress(sprintf("Analysing line %d (%.2f%%)",{sourceline,sourceline/length(linetable)*100}))
            t = time()+interval
        end if
        packedlineref = linetable[sourceline]           -- get uniqline index
        if packedlineref then
            matchlineset = matchtable[packedlineref]        -- the set it is equal to
            for matchlineref=1 to length(matchlineset) do
                matchline = matchtable[packedlineref][matchlineref] -- each one...
                if matchline>sourceline then
                    --
                    -- Scan back to make sure we have not already processed this (group)
                    --
                    topofgroup = 1
                    for prevline=sourceline-1 to 1 by -1 do     -- loop back (to first real line)
                        prevlineref = linetable[prevline]       -- get uniqline index
                        if prevlineref then                     -- is it real?
                            prevlineset = matchtable[prevlineref] -- the set that is equal to
                            if length(prevlineset) then
                                if find(prevline,prevlineset)=0 then --sanity check
                                    void = messageBox("Error","internal error",0)
                                end if
                                prevmatchline = matchline-(sourceline-prevline) --get equiv prec. line
                                if find(prevmatchline,prevlineset) then -- if it was a match...
                                    topofgroup = 0              -- ...then this is not start of group.
                                end if
                                exit                            -- any real line terminates scan
                            end if
                        end if
                    end for
                    if topofgroup then
                        --
                        -- Calculate how long the group is
                        --
                        endofgroup = sourceline --+1 made no difference..
                        matchline = matchtable[packedlineref][matchlineref]
                        for nextline=sourceline+1 to length(linetable) do
                            nextlineref = linetable[nextline]
                            if nextlineref then
                                nextlineset = matchtable[nextlineref]   -- 06/07/2005 (0.1.8)
                                if length(nextlineset) then
                                    nextmatchline = matchline+(nextline-sourceline)
                                    if nextmatchline>length(linetable) then exit end if
                                    nextmatchref = linetable[nextmatchline]
                                    if nextmatchref=0
                                    or find(nextline,matchtable[nextmatchref])=0 then
                                        exit
                                    end if
                                    endofgroup = nextline
                                end if
                            end if
                        end for
                        groupset = append(groupset,{sourceline-endofgroup,sourceline,matchline})
                    end if
                end if
            end for
        end if
        if cancelled then exit end if
    end for
    --
    -- Sort the groups found largest first
    --
    if length(groupset) then
        groupset = sort(groupset)
        bestgroup = groupset[1]
        bestgroup[1] = 1-bestgroup[1]
        bestgroup[2] = bestgroup[2]+baseline
        bestgroup[3] = bestgroup[3]+baseline
        result = append(result,
                        sprintf("-- Largest group: %d lines starting at %d and %d.",bestgroup))
    end if
end procedure

sequence lhs
constant maxcols=5

procedure initoneline()
    if lineno>length(lhs) then
        oneline = ""
    else
        if length(linetable)>9999 then
            oneline = sprintf("%6d    ",{lineno+baseline})   -- don't overwrite 1st digit, keep tabs aligned(4)
        else
            oneline = sprintf("%5d ",{lineno+baseline})
        end if
        for i=1 to maxcols do
            if lhs[lineno][i]=0 then
                oneline &= "      "
            else
                if lhs[lineno][i]=-1 then
                    oneline &= "   |  "
                elsif lhs[lineno][i]=-2 then
                    oneline &= "   +  "
                else
                    oneline &= sprintf("%5d ",{lhs[lineno][i]+baseline})
                    if i=1 then
                        if lineno=1
                        or (lhs[lineno-1][1]+1!=lhs[lineno][1]
                            and lhs[lineno-1][1]!=-1) then
                            oneline[1] = '>'
                        end if
                    end if
                end if
            end if
        end for
    end if
end procedure

procedure CCoutput()
    --
    -- Now set up the left hand side 'same as' columns,
    -- which contain best match info leftmost.
    --
integer column, col2, col3, m1
integer linelength

sequence ccaout

    lhs = repeat(repeat(0,maxcols),length(linetable))
    for i=1 to length(groupset) do
        if time()>t then
            progress(sprintf("Arranging group sets: line %d (%.2f%%)\n",{i,i/length(groupset)*100}))
            t = time()+interval
        end if
        column = 1
        col2 = 0
        col3 = 0
        while column<=maxcols do
            for j=-groupset[i][1] to 0 by -1 do
                if lhs[groupset[i][2]+j][column]!=0 then exit end if
                if j=0 then
                    col2 = column
                    column = maxcols
                end if
            end for
            column += 1
        end while
        column = 1
        while column<=maxcols do
            for j=-groupset[i][1] to 0 by -1 do
                if lhs[groupset[i][3]+j][column]!=0 then exit end if
                if j=0 then
                    col3 = column
                    column = maxcols
                end if
            end for
            column += 1
        end while
        m1 = 0
        for j=-groupset[i][1] to 0 by -1 do
            if col2 then
                if m1 then
                    lhs[groupset[i][2]+j][col2] = -1
                else
                    lhs[groupset[i][2]+j][col2] = groupset[i][3]+j
                end if
            else
                lhs[groupset[i][2]+j][maxcols] = -2
            end if
            if col3 then
                if m1 then
                    lhs[groupset[i][3]+j][col3] = -1
                else
                    lhs[groupset[i][3]+j][col3] = groupset[i][2]+j
                end if
            else
                lhs[groupset[i][3]+j][maxcols] = -2
            end if
            m1 = 1
            if j=1 then
                m1 = 0
            end if
        end for
        if cancelled then exit end if
    end for
    --
    -- And finally, print.
    --
    lineno = 1
    column = 0
    ch = GetCh()
    initoneline()
    while 1 do
        if time()>t then
            progress(sprintf("Printing: line %d (%.2f%%)\n",{lineno,lineno/length(linetable)*100}))
            t = time()+interval
        end if
        if ch=-1 then
            result = append(result,oneline)
            exit
        end if
        if find(ch,"\r\n") then
            while find(ch,"\r\n") do
                if ch='\n' then
                    result = append(result,oneline)
                    lineno += 1
                    initoneline()
                end if
                ch = GetCh()
            end while
            if ch=-1 then exit end if
        else
            while not find(ch,{'\r','\n',-1}) do
-- 1/5/2010:
                oneline &= ch
-- (and removed...)
--              if ch='\t' then
----                    oneline &= repeat(SPACE,isTabWidth-remainder(length(oneline)-1,isTabWidth))
--                  oneline &= repeat(SPACE,8-remainder(length(oneline)-1,8))
--              else
--                  oneline &= ch
--              end if
                ch = GetCh()
            end while
        end if
        if cancelled then exit end if
    end while

    if not cancelled then
        ccaout = filepaths[currfile]&"ccaout.e"
        if atom(dir(ccaout)) then
            close(open(ccaout,"w"))
        end if

        if not openFile(ccaout,0,isLegacyTabHandling) then
            void = messageBox(xl("Error"),
                              xl("Couldn't open ") & ccaout,
                              MB_ICONEXCLAMATION + MB_OK)
        else
            --
            -- lastly, build a new linelengths table for edita
            --
            linelengths = repeat(0,80)
            for i=1 to length(result) do
                linelength = ExpLength(result[i])
                if linelength>=length(linelengths) then
                    linelengths &= repeat(0,linelength-length(linelengths)+1)
                end if
                linelengths[linelength+1] = linelengths[linelength+1] + 1
            end for
            filelinelengths[currfile] = linelengths
            filetext[currfile] = result
            bookmarks[currfile] = repeat(0,length(result))
            actions[currfile] = {}
            actionptr[currfile] = 0
            actionsave[currfile] = 0

            TopLine = 0
            CursorY = 0
            CursorX = 0
            selON = 0
            forceCursorOnscreen()
            paintall()
        end if
    end if

end procedure

procedure CCA()
    if currfile then
        CCopen()
        CCinit()
        CCload()
        if not cancelled then CCgroupanalysis() end if
        if not cancelled then CCoutput() end if
    end if

    -- destroy internal tables to release memory.

    result = {}
    text = {}
    wordlist = {}
    uniqlines = {}
    matchtable = {}
    linetable = {}
    groupset = {}
    lhs = {}

    if PROGvis then
        setVisible(CCPROGRESS,False)
        removeFocus(CCPROGRESS)
        PROGvis = 0
    end if
end procedure
global constant r_CCA=routine_id("CCA")
