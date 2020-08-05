--
-- synld.e
--
-- load syntax files
--

--without trace
--with trace

global sequence Extensions,     -- eg {"e","ex","ew","exw","html"}
                ExtensionNos    -- eg { 1 , 1  , 1  , 1   , 2}

global integer newSyntax        -- from ExtensionNos, or 1 if none
                newSyntax=1

global sequence SynNames,       -- eg "Euphoria" (nb Phix uses "Euphoria")
                LineComments,   -- eg {"--"}
                BlockComments,  -- eg {"<!--","-->"}
                AutoCompletes,  -- see eauto.e
                EscapeLeadIns,  -- eg \         (quotes omitted for clarity)
                Escapes,        -- eg nrt\'"    (           ""             )    
                BraceLevels,    -- 1..9
                OperatorSets,   -- eg {"<","<=","=","!=",...
                Indents,        -- eg {{"then","do","else","end"},{1,1,1,-1},{},{}}
                Sections,       -- eg (..."Reserved Words","Builtins"...
                WordLists,      -- eg {...{"if","then",..},{"abort","system",...
                colourTabs,     -- eg {...Purple,           Green,...}
                styleTabs,      -- eg {...CD_PLAIN,         CD_ITALIC,...}
                charMaps        -- character Types for #00..#FF

global integer MAXnColours
               MAXnColours=16   -- (minimum possible) [??]


constant standardThings={"Background", "Comments", "Highlight", "Current Line", "Strings",
--                       "Illegals", "Operators", "URLs", "Other"}
                         "Illegals", "Operators", "URLs", "Other",
                         "Marginbg","Linenos","Bookmarks"}

global constant Background=1, Comments=2, Highlight=3, HighLine=4, Strings=5, 
                Illegals=6, Operators=7, URLs=8, Other=9,
                Marginbg=10, Linenos=11, BookMarks=12

--DEV use CD_PLAIN etc...
--global constant EA_Normal=4, EA_Bold=1, EA_Italic=2 -- So we can have Bold+Italic(=3)

global constant
 TokenStart = 1,
 TokenChar  = 2,
 TokenFirst = 3,
 TokenLast  = 4,
 OpenBrace  = 5,
 CloseBrace = 6,
 Whitespace = 7,
 Delimiter  = 8,
 Operator   = 9,
 String     = 10,
 Illegal    = 11,
 Comment    = 12

global sequence wordChar    -- set by easynld.e (all are TokenChar, not TokenStart/First/Last)


global sequence standardColourNames,standardColours
standardColourNames={} standardColours={}

--procedure addStandardColour(sequence text, integer r, integer g, integer b)
procedure addStandardColour(sequence text, integer b, integer g, integer r)
integer colour
    standardColourNames = append(standardColourNames,text)
    colour = r+g*#100+b*#10000
    standardColours = append(standardColours,colour)
end procedure
addStandardColour("Black",    0,  0,  0)
addStandardColour("Maroon", 128,  0,  0)
addStandardColour("Green",    0,128,  0)
addStandardColour("Olive",  128,128,  0)
addStandardColour("Navy",     0,  0,128)
addStandardColour("Purple", 128,  0,128)
addStandardColour("Teal",     0,128,128)
addStandardColour("Gray",   128,128,128)
addStandardColour("Silver", 192,192,192)
addStandardColour("Red",    255,  0,  0)
addStandardColour("Lime",     0,255,  0)
addStandardColour("Yellow", 255,255,  0)
addStandardColour("Blue",     0,  0,255)
addStandardColour("Fuchsia",255,  0,255)
addStandardColour("Aqua",     0,255,255)
addStandardColour("White",  255,255,255)


--include builtins\file.e -- seek() and where()

integer f,          -- file handle
        ch,         -- a character
        lineno, 
        columnOne,  -- 1 if in column 1, else 0
        errorAlready

string errorTitle, errorMsg

--global sequence comment -- eg "--"    -- set by changeTo()
global sequence lineComments -- eg {"--"}   -- set by changeTo()
--DEV
--global sequence blockComment              -- ""
global sequence blockComment = ""           -- ""
global sequence ColourTab                   -- ""
global sequence StyleTab                    -- ""

sequence fullname   -- eg "syn\Euphoria.syn" (same for Phix)

global sequence charMap     -- set by changeTo()

global sequence urlChar     -- all are TokenChar, not TokenStart/First/Last

procedure setCharMap(object charset, integer chartype)
    if atom(charset) then
        charMap[charset+1] = chartype
        return
    end if
    for i=1 to length(charset) do
        charMap[charset[i]+1] = chartype
    end for
end procedure

charMap = repeat(Illegal,256)
--charMap[128..255]=TokenChar
--for i=128 to 255 do
--  setCharMap(i,TokenChar)
--end for
charMap[129..256] = TokenChar -- #7F to #FF really
setCharMap({#B6,#B7,#BB},Illegal)   -- visible space, tab, and paragraph mark. (treat as spaces)
setCharMap("ABCDEFGHIJKLMNOPQRSTUVWXYZ",TokenChar)
setCharMap("abcdefghijklmnopqrstuvwxyz",TokenChar)
setCharMap("_0123456789.",TokenChar)
wordChar = charMap  -- in eatabs.e (all are TokenChar, not TokenStart/First/Last)
--setCharMap(":/-'*%$()=&?@+~,!.\\#",TokenChar)
setCharMap(`:/-'*%$()=&?@+~,!.\#`,TokenChar)
urlChar = charMap   -- (all are TokenChar, not TokenStart/First/Last)

procedure fatal(sequence msg)
    if not errorAlready and newSyntax!=1 then 
        errorTitle = sprintf("Error: %s line %d",{fullname,lineno})
        errorMsg = msg
    end if
    errorAlready = 1
    newSyntax = 1 -- added 1/7...
    ch = -1
end procedure

procedure skipSpaces()
    while 1 do
        while ch<=' ' do    -- spaces, tabs, newline, return.
            if ch='\r' then
                lineno += 1
                columnOne = 1
            elsif ch=-1 then
                return
            elsif ch!='\n' then
                columnOne = 0
            end if
            ch = getc(f)
        end while
--      if not length(comment) then return end if
--      for i=1 to length(comment) do
--          if ch!=comment[i] then
--              if i>1 then
--                  if seek(f,where(f)-i+1)!=SEEK_OK then ?9/0 end if
--                  ch = comment[1]
--              end if
--              return
--          end if
--          ch = getc(f)
--      end for
        bool found = false
        for c=1 to length(lineComments) do
            string comment = lineComments[c]
            found = true
            for i=1 to length(comment) do
                if ch!=comment[i] then
                    if i>1 then
                        {} = seek(f,where(f)-i+1)
                        ch = comment[1]
                    end if
                    found = false
                    exit
                end if
                ch = getc(f)
            end for
            if found then exit end if
        end for
        if not found then return end if
        while ch>=' ' or ch='\t' do
            ch = getc(f)    -- skip to end of line then
        end while
    end while
end procedure


--with trace
procedure loadAutoComplete()
-- see eauto.e for syntax details.
--DEV grab each whole line and translate it. [DONE, cleanup pending]
integer LinePos,TriggerCh,CursorMove, state
sequence LineMatch,InsertLine,InsertLines
sequence line
integer lidx
--  autoComplete = {}
--trace(1)
    state = 0 -- 0 = initialisation, 1=linematch processing, 2=insertblock processing
    line = {}
--  lidx = 1
    while 1 do
        if state=0 or ch='\n' then
--      if state=0 or lidx>length(line) then
            if state then
--              line = xlQ(line)
                lidx = 1
                while lidx<=length(line) do
                    ch = line[lidx]
                    if ch = '&' then
--                      TriggerCh = getc(f)
                        lidx += 1
                        if lidx>length(line) then
                            fatal("& at end of line")
                            return
                        end if
                        TriggerCh = line[lidx]
                        state = 2
                    elsif ch='|' then
                        CursorMove = length(InsertLine)
                    else
                        if ch='^' then
--                          ch = getc(f)
                            lidx += 1
                            if lidx>length(line) then
                                fatal("^ at end of line")
                                return
                            end if
                            ch = line[lidx]
                            if ch=' ' then
                                ch = acWHITESPACE
                            elsif ch='p' then
                                if state != 2 then fatal("^p before trigger") return end if
                                InsertLines = append(InsertLines,InsertLine)
                                InsertLine = {}
                                ch = 0
                            elsif ch='t' then
                                ch = '\t'
--                          elsif ch='b' then
--                              TriggerCh = 8
--                              ch = 0
--                              state = 2
                            elsif ch=',' then
                                LinePos = acCOMMA
                                ch = 0
                            else
                                fatal("unknown trigger")
                                return
                            end if
                        end if
                        if ch then
                            if state=1 then
                                LineMatch = append(LineMatch,ch)
                            else
                                InsertLine = append(InsertLine,ch)
                            end if
                        end if
                    end if
                    lidx += 1
                end while
                ch = '\n'
                line = {}
                if TriggerCh=0 then fatal("missing trigger") return end if
                InsertLines = append(InsertLines,InsertLine)
                autoComplete = append(autoComplete,{LinePos,LineMatch,TriggerCh,CursorMove,InsertLines})
            end if
            state = 1
            LinePos = acEOL
            LineMatch = {}
            TriggerCh = 0
            CursorMove = 0
            InsertLine = {}
            InsertLines = {}
            skipSpaces()
        end if
        if ch='#' then exit end if
        if ch=-1 then
            fatal("#end AutoComplete missing")
            return
        elsif ch!='\r' then
            line &= ch
        end if
        ch = getc(f)
    end while
    -- .. should end up with something like this:
--  ^ i&f | then^pend if
--  autoComplete={{acEOL,{acWHITESPACE,'i'},'f',+1,{"  then","end if"}}
--               }

    -- leave the remainder for Expect("#end AutoComplete")
end procedure


function Expect(sequence txt)
    if errorAlready then return (newSyntax!=1) end if
    if not columnOne then fatal("Not in column 1") return (newSyntax!=1) end if
    for i=1 to length(txt) do
        if ch!=txt[i] then 
            if upper(ch)!=upper(txt[i]) then
                fatal(sprintf(`"%s" expected`,{txt}))
                return (newSyntax!=1)
            end if
        end if
        ch = getc(f)
    end for
    skipSpaces()
    return 1
end function

function ifItReallyIs(sequence txt)
-- we know ch matches txt[1]. Check the rest & return True/reset & return False.
    if not errorAlready then
        if not columnOne then return 0 end if
        for i=1 to length(txt) do
            if ch!=txt[i] then 
                if i>1 then
                    if seek(f,where(f)-i+1)!=SEEK_OK then ?9/0 end if
                    ch = txt[1]
                end if
                return 0
            end if
            ch = getc(f)
        end for
    end if
    skipSpaces()
    return 1
end function

function getWord()
sequence word
--  word = {}
    word = ""
    if not errorAlready then
        while ch>' ' do
            word &= ch
            ch = getc(f)
        end while
        skipSpaces()
    end if
    return word
end function


constant Lower=1, Upper=2
sequence caseMap
    caseMap = repeat(0,255)
    for i='a' to 'z' do caseMap[i] = Lower end for
--  caseMap['a'..'z']=Lower
    for i='A' to 'Z' do caseMap[i] = Upper end for
--  caseMap['A'..'Z']=Upper -- lots of machine errors (DEV!)

function spaceOutSectionName(sequence name)
    for i=length(name) to 2 by -1 do
        if caseMap[name[i-1]]=Lower and caseMap[name[i]]=Upper then
            name = name[1..i-1]&' '&name[i..length(name)]
        end if
    end for
    return name
end function

procedure defaultColourTab()
-- Note: this needs to be called in several cases:
-- 1) at startup/as default for None
-- 2) to create table for Scheme at end of .syn
-- 3) to create table for .clr file
--  Note that 2) may be absent but 3) not.
    if not length(ColourTab) then
--      ColourTab = repeat(0,MAXnColours) -- BLACKNESS!!
        ColourTab = repeat(CD_BLACK,MAXnColours)
--      ColourTab[Background] = White
        ColourTab[Background] = CD_WHITE
--  CD_WHITE        = #FFFFFF,
--erm:
--  CD_WHITE        = #E0E0E0,
--  CD_BRIGHTWHITE  = #FFFFFF,
--  White           = rgb(224, 224, #E0),
--  BrightWhite     = rgb(255, 255, 255)
--      ColourTab[HighLine] = Parchment
        ColourTab[HighLine] = CD_PARCHMENT
--      ColourTab[Highlight] = Gray
        ColourTab[Highlight] = CD_DARK_GRAY
--  Gray            = rgb(128, 128, #80),
--  CD_DARK_GRAY    = #808080,
--  CD_GRAY         = #C0C0C0,
--      ColourTab[Marginbg] = White
        ColourTab[Marginbg] = CD_WHITE
--      ColourTab[Linenos] = Blue
        ColourTab[Linenos] = CD_DARK_BLUE
--  Blue            = rgb(  0,   0, 128),
--  CD_BLUE         = #0000FF,
--  CD_DARK_BLUE    = #000080,
--      ColourTab[BookMarks] = Yellow
        ColourTab[BookMarks] = CD_YELLOW
--  Yellow          = rgb(255, 255,   0),
--  CD_YELLOW       = #FFFF00,
--  CD_DARK_YELLOW  = #808000,
--      StyleTab = repeat(EA_Normal,MAXnColours)
        StyleTab = repeat(CD_PLAIN,MAXnColours)
--      StyleTab[URLs] = 5
        StyleTab[URLs] = CD_BOLD+CD_UNDERLINE
    end if
end procedure

--sequence name
sequence newSections

procedure setMAXnColours()
    if length(newSections)>MAXnColours then
        MAXnColours = length(newSections)
    end if
end procedure

constant deprecated={"BuiltinProcedures","BuiltinFunctions","Builtins",
                     "LibraryProcedures","LibraryFunctions","database.e"},
         replacements={"BuiltinRoutines","BuiltinRoutines","BuiltinRoutines",
                     "LibraryRoutines","LibraryRoutines","LibraryRoutines"}

procedure loadScheme()
sequence word
integer sectionNo
integer newcolour, k, l
    sectionNo = 0
    defaultColourTab()
    while ch!=-1 do
        word = getWord()
        if equal(word,"Symbols") then word="Operators" end if   -- changed 1/7/2005.
        k = find(spaceOutSectionName(word),newSections)
        if k then
            sectionNo = k
--          StyleTab[sectionNo] = 4 -- set EA_Normal (thanks to Al Getz)
            StyleTab[sectionNo] = CD_PLAIN
        else
            k = find(word,deprecated)
            if k then
                sectionNo = find(spaceOutSectionName(replacements[k]),newSections)
--              StyleTab[sectionNo] = 4 -- set EA_Normal (thanks to Al Getz)
                StyleTab[sectionNo] = CD_PLAIN
            else
                k = find(word,standardColourNames)
                if k and sectionNo then
                    ColourTab[sectionNo] = standardColours[k]
                else
                    k = find(word,{"Bold","Italic"})
                    if k and sectionNo then
                        k = or_bits(k,StyleTab[sectionNo])
--                      StyleTab[sectionNo] = and_bits(k,3)
                        StyleTab[sectionNo] = and_bits(k,CD_BOLD_ITALIC)
                    else
                        if length(word) and word[1]='#' then
                            word = word[2..length(word)]
                        end if
                        l = 0
                        if length(word)=6 then
                            newcolour = 0
                            for i=1 to 6 do
                                l = find(word[i],"0123456789ABCDEF")
                                if not l then exit end if
                                newcolour = newcolour*16+l-1
                            end for
                        end if
                        if l then
                            ColourTab[sectionNo] = newcolour
                        else
                        fatal(`unrecognised section/colour/attribute "`&word&`"`)
                        return
                        end if
                    end if
                end if
            end if
        end if
    end while
--  StyleTab[URLs] = 5
    StyleTab[URLs] = CD_BOLD+CD_UNDERLINE
end procedure

--with trace
constant TokenTypes={"Start","Char","First","Last"}

procedure loadSyn(sequence name)
--
-- Load a syntax file. Note that the first call ("None") can complain, perhaps,
-- if it can't find the file etc, but it must complete this routine fully.
-- Subsequent files can perform an early exit.
--
integer sectionNo,
        ecb     -- expected close brace
sequence word

-- first off, everything is shoved into local vars.
object escapeLeadIn
sequence escapes
integer bracelevel
sequence operatorset
sequence indentset
sequence newExtensions
sequence newWordLists
sequence linecomment, blockcomment

integer indentType, indentTypeSave

integer TokenType

    newSyntax = length(SynNames)+1
    lineno = 1
    columnOne = 1
    linecomment = {}
    blockcomment = {}
--  escapeLeadIn = {} -- impossible char!
    escapeLeadIn = -1
    escapes = {}
    bracelevel = 0
    operatorset = {}
    indentset = repeat({},4)
    newSections = standardThings
    autoComplete = {}
    ColourTab = {}
--defaultColourTab()    -- 4/7  StyleTab is left tiddly
    newWordLists = {}
    if newSyntax=1 then
        newExtensions = {{}}
        charMap = repeat(Illegal,256)
--      charMap[128..255] = TokenChar
--      for i=128 to 255 do setCharMap(i,TokenChar) end for
        charMap[129..256] = TokenChar -- #7F to #FF really
    end if

--  fullname = initialcurrentdir&`syn\`&name&".syn"
    fullname = join_path({initialcurrentdir,"syn",name&".syn"})
    f = open(fullname,"rb")
    if f=-1 then
        fatal(`cannot open file "`&fullname&`"`)
        if newSyntax!=1 then return end if
        setMAXnColours()
        defaultColourTab()
    else
--      comment = {}
        lineComments = {}
        ch = getc(f)
        -- (Note that Expect always returns true when newSyntax=1)
        if not Expect(name&" syntax file") then close(f) return end if
        if ch='B' then
            if not Expect("BlockComment") then close(f) return end if
            while not columnOne and ch>' ' do
                word = getWord()
                blockcomment = append(blockcomment,word)
                skipSpaces()
            end while
            if and_bits(length(blockcomment),1) then
                fatal("missing block comment end")
                close(f)
                return
            end if
        end if
        if not Expect("LineComment") then close(f) return end if
--      if not columnOne then   -- language does not support line comments!
--          comment = getWord()
--      end if
--      skipSpaces()
--      linecomment = comment
        while not columnOne and ch>' ' do
            word = getWord()
            lineComments = append(lineComments,word)
            skipSpaces()
        end while
        linecomment = lineComments
        if newSyntax!=1 then
            charMap = repeat(Illegal,256)
--          charMap[128..255] = TokenChar
--          for i=128 to 255 do setCharMap(i,TokenChar) end for
            charMap[129..256] = TokenChar -- #7F to #FF really
            newExtensions = {}
        end if
        setCharMap(#B6,Delimiter)   -- paragraph mark
        setCharMap(#BB,Delimiter)   -- tab character
        setCharMap(#B7,Delimiter)   -- space character
        if not Expect("Token") then close(f) return end if
        while 1 do
            TokenType = find(ch,"SCFL") --(Start/Char/First/Last, ie TokenStart .. TokenLast)
            if TokenType=0 then fatal("unrecognised") close(f) return end if
            if not Expect(TokenTypes[TokenType]) then close(f) return end if
            while ch>' ' do
                setCharMap(ch,TokenType)
                ch = getc(f)
            end while
            skipSpaces()
            if ch!='T' then exit end if
            if not ifItReallyIs("Token") then exit end if
        end while
        skipSpaces()
--      escapeLeadIn = {} -- impossible char!
--      escapes = {}
        if ch='E' then
            if not Expect("Escapes") then close(f) return end if
            escapeLeadIn = ch
            ch = getc(f)
            while ch>' ' do
                escapes &= ch
                ch = getc(f)
            end while
            skipSpaces()
        end if
        setCharMap(" \t\r\n",Whitespace)
--      setCharMap("\"\'",String)
        setCharMap("\"\'`",String)
--      end if
        if newSyntax!=1 or ch = 'D' then
            if not Expect("Delimiters") then close(f) return end if
            while ch>' ' do
                setCharMap(ch,Delimiter)
                ch = getc(f)
            end while
            skipSpaces()
        end if
--      operatorset = {}
        if ch='O' then
            if not Expect("Operators") then close(f) return end if
            while not columnOne and ch>' ' do
                word = getWord()
                operatorset = append(operatorset,word)
                setCharMap(word,Operator)
                skipSpaces()
            end while
        end if
        if ch='I' then
            if not Expect("Illegal") then close(f) return end if
            while ch>' ' do
                setCharMap(ch,Illegal)
                ch = getc(f)
            end while
            skipSpaces()
        end if
--      bracelevel = 0
        if ch='B' then
            if not Expect("Braces") then close(f) return end if
            if ch<'1' or ch>'9' then fatal("1-9 expected") close(f) return end if
            bracelevel = min(ch-'0',7)
            ch = getc(f)
            skipSpaces()
            while ch>' ' do
                if    ch='(' then ecb = ')'
                elsif ch='{' then ecb = '}'
                elsif ch='[' then ecb = ']'
                else fatal("(, [, or { expected") close(f) return
                end if
                setCharMap(ch,OpenBrace)
                ch = getc(f)
                if ch!=ecb then fatal(ecb&" expected") close(f) return end if
                ch = getc(f)
                setCharMap(ecb,CloseBrace)
            end while
            skipSpaces()
        end if
    
--      newExtensions={}
        if not Expect("FileExtensions") then close(f) return end if
        while 1 do
            while not columnOne and ch>' ' do
                word = getWord()
                if find(word,Extensions) 
                or find(word,newExtensions) then
                    fatal("duplicate extension "&word)
                    close(f)
                    return
                end if
                newExtensions = append(newExtensions,word)
                skipSpaces()
            end while
            if ch!='F' then exit end if
            if not ifItReallyIs("FileExtensions") then exit end if
        end while
--      indentset=repeat({},4)
        if ch='I' then
            if not Expect("Indents") then close(f) return end if
            indentTypeSave = 0
            while not columnOne and ch>' ' do
                word = getWord()
                skipSpaces()
                indentType = find(word,{"+","-"})
                if indentType!=0 then
                    if indentType=1 then
                        indentTypeSave = +1
                    else
                        indentTypeSave = -1
                    end if
                    if columnOne or ch<=' ' then exit end if
                    word = getWord()
                    skipSpaces()
                end if
                if length(indentset)=0 and indentTypeSave=0 then
                    fatal("'+' or '-' missing")
                    close(f)
                    return
                end if
--              word = KtoF(word)
                if length(word)=1 then  -- eg {, }
                    indentset[3] = indentset[3]&word
                    indentset[4] = indentset[4]&indentTypeSave
                else
                    indentset[1] = append(indentset[1],word)
                    indentset[2] = append(indentset[2],indentTypeSave)
                end if
            end while
        end if
--      autoComplete = {}
        if ch='A' then
            if not Expect("AutoComplete") then close(f) return end if
            loadAutoComplete()
            if not Expect("##end AutoComplete") then close(f) return end if
        end if
--      newSections = standardThings
--      for i=1 to bracelevel do
        for i=1 to 7 do
--if isDebug then
--          newSections = append(newSections,"(")
--else
            newSections = append(newSections,sprintf("Brace%d",{i}))
--end if
        end for
--      ColourTab = {}
--      newWordLists = {}
        while 1 do
            if not columnOne then fatal("not in column 1") close(f) return end if
            word = spaceOutSectionName(getWord())
            skipSpaces()
            sectionNo = find(word,newSections)
            if not sectionNo then
                newSections = append(newSections,word)
                sectionNo = length(newSections)
                newWordLists = append(newWordLists,{})
            end if
--          sectionNo -= Other+bracelevel
--          sectionNo -= Other+7
            sectionNo -= BookMarks+7
if sectionNo<=0 then fatal("not allowed") close(f) return end if
            while not columnOne and ch>' ' do
                word = getWord()
--DEV crash here with sectionNo of -13 when I tried adding a section named Illegals (hopefully fixed by the above)
                newWordLists[sectionNo] = append(newWordLists[sectionNo],word)
                skipSpaces()
            end while
            if ch=-1 then exit end if   --EOF
            if ch='S' then
                if ifItReallyIs("Scheme") then
                    setMAXnColours()
                    loadScheme()
                    exit
                end if
            end if
        end while   
        close(f)
        setMAXnColours()
        defaultColourTab()
        f = length(fullname)
        fullname[f-2..f] = "clr"
        f = open(fullname,"rb")
        if f!=-1 then
            lineno = 1
            columnOne = 1
            ch = getc(f)
            if not Expect(name&" colour scheme") then close(f) return end if
            loadScheme()
            close(f)
        end if
    end if
--printf(1,"%s:%d; Max=%d\n",{name,length(newSections),MAXnColours})
--
-- All done, update the global tables
--
    if (not errorAlready) or newSyntax=1 then
        SynNames = append(SynNames,name)
        LineComments = append(LineComments,linecomment)
        BlockComments = append(BlockComments,blockcomment)
        AutoCompletes = append(AutoCompletes,autoComplete)
        EscapeLeadIns = append(EscapeLeadIns,escapeLeadIn)
        Escapes = append(Escapes,escapes)
        BraceLevels &= bracelevel
        OperatorSets = append(OperatorSets,operatorset)
        Indents = append(Indents,indentset)
        charMaps = append(charMaps,charMap)
        Extensions &= newExtensions
        ExtensionNos &= repeat(newSyntax,length(newExtensions))
        Sections = append(Sections,newSections)
        WordLists = append(WordLists,newWordLists)
        colourTabs = append(colourTabs,ColourTab)
        styleTabs = append(styleTabs,StyleTab)
--  else
--      newSyntax = 1
    end if
end procedure

global sequence helpnames, helpfiles, menuTxts, F1lists

--with trace
procedure LoadHCF(sequence name)
sequence helpfile, word 
object mtxt, wordlist
integer lh, f2, k, b1, b2
    lineno = 1
    columnOne = 1
    fullname = initialcurrentdir&`help\`&name
    fullname = get_proper_path(fullname) 
    f = open(fullname,"rb") -- closed by caller (if f!=-1).
    if f=-1 then
        fatal(`cannot open file "`&fullname&`"`)
    else
        ch = getc(f)
        -- (Note that Expect always returns true when newSyntax=1)
--      comment = "--"
        lineComments = {"--"}
        if not Expect("Help file") then return end if
        if ch='?' then return end if
        if ch!=':' then fatal("? or : expected") return end if
        ch = getc(f)
        skipSpaces()
        helpfile = ""
--  if not errorAlready then
        while ch>=' ' do
            helpfile &= ch
            ch = getc(f)
        end while
        wordlist = {}
        mtxt = 0
        lh = length(helpfile)
        if lh and helpfile[1]='\"' and helpfile[lh]='\"' then
            helpfile = helpfile[2..lh-1]
        end if
--      k = find(getFileExtension(helpfile),{"hlp","chm"})
        k = find(get_file_extension(helpfile),{"hlp","chm"})
        if k=0 then
            fatal("unknown help file extension")
            return
        end if
        if helpfile[2]!=':' then
            helpfile = get_proper_path(initialcurrentdir&helpfile)
        else
            helpfile = get_proper_path(helpfile)
        end if
        f2 = open(helpfile,"r")
--/*
        if f2=-1 and helpfile[2]!=':' then
--          helpfile = initialcurrentdir&helpfile
            helpfile = get_proper_path(initialcurrentdir&helpfile)
            f2 = open(helpfile,"r")
        end if
--*/
        if f2=-1 then
            fatal("cannot open "&helpfile)
            wordlist = 0    -- use atom to signal file not found
        else
            if k=1 then
                b1 = getc(f2)
                b2 = getc(f2)
            end if
            close(f2)
            if k=1 and (b1!=#3F or b2!=#5F) then
                -- Not sure about this.
                -- If the .hlp file works fine when you double click on it,
                -- but triggers this error, please let me know.
                -- An alternative test might be (b1='@' and b2='@') with a
                -- message "system files beginning "@@" are not pukka .hlp"
                fatal(".hlp file does not begin #3F5F")
                return
            end if
            skipSpaces()
            if ch='M' then
                if not Expect("Menu Text:") then return end if
                mtxt = ""
                while ch>=' ' do
                    mtxt &= ch
                    ch = getc(f)
                end while
                skipSpaces()
            end if
            if ch='F' then
                if not Expect("F1 keys:") then return end if
                while not columnOne and ch>' ' do
                    word = getWord()
                    wordlist = append(wordlist,word)
                    skipSpaces()
                end while
            else
                if atom(mtxt) then
                    fatal("Menu Text or F1 keys expected")
                    return
                end if
            end if
        end if
        helpnames = append(helpnames,name)
        helpfiles = append(helpfiles,helpfile)
        menuTxts = append(menuTxts,mtxt)
        F1lists = append(F1lists,wordlist)
    end if
end procedure

global procedure initSyn()
object dirlist
sequence filename
integer len

    Extensions = {}
    ExtensionNos = {}
    SynNames = {}
    EscapeLeadIns = {}
    Escapes = {}
    BraceLevels = {} 
    OperatorSets = {}
    Indents = {}
    Sections = {}
    WordLists = {}
    LineComments = {}
    BlockComments = {}
    AutoCompletes = {}
    charMaps = {}

    colourTabs = {}
    styleTabs = {}

    errorAlready = 0
    errorMsg = ""

    loadSyn("None")
----/*
--      dirlist = dir(initialcurrentdir&"syn")
----*/
----**/ dirlist = pdir:dir(initialcurrentdir&"syn")
    dirlist = dir(initialcurrentdir&"syn")
    if sequence(dirlist) then
        for i=1 to length(dirlist) do
            if not find('d',dirlist[i][2]) then
                filename = dirlist[i][1]
                len = length(filename)
                if len>4 
                and equal(".syn",lower(filename[len-3..len])) then
                    filename = filename[1..len-4]
                    if not equal(filename,"None") then
--if equal(filename,"Html") then
                        loadSyn(filename)
--                      if errorAlready then exit end if
                        errorAlready = 0
--end if
                    end if
                end if
            end if
        end for
    end if
    --
    -- Finally make all colourTabs the right length. (DEV - why?)
    --
    for i=1 to length(colourTabs) do
        len = length(colourTabs[i])
        if len<MAXnColours then
            colourTabs[i] = colourTabs[i] & repeat(0,MAXnColours-len)   -- BLACKNESS
        end if
    end for
    newSyntax = 1 -- revert to 'None'
    charMap = charMaps[1]
    ColourTab = colourTabs[1]
    StyleTab = styleTabs[1]

    if length(errorMsg) then
        IupMessage(errorTitle,errorMsg)
    end if
end procedure
initSyn()

global procedure initHlp()
--
-- (Re-)load any help control files:
--
object dirlist
integer k, len
sequence filename
integer wasSyntax
    helpfiles = {}
    helpnames = {}
    dirlist = dir(initialcurrentdir&"help")
    if sequence(dirlist) then
        k = 0
        for i=1 to length(dirlist) do
            if not find('d',dirlist[i][2]) then
                filename = dirlist[i][1]
                len = length(filename)
                if len>4 
                and find('.',filename)=len-3
                and equal(".txt",lower(filename[len-3..len]))
                and not equal("readme.txt",lower(filename)) then
                    k += 1
                    dirlist[k] = filename
                end if
            end if
        end for
        dirlist = dirlist[1..k]
        dirlist = sort(dirlist)
        menuTxts = {}
        F1lists = {}
        errorAlready = 0
        errorMsg = ""
        wasSyntax = newSyntax
        newSyntax = 0 -- re-enables error handling
        for i=1 to length(dirlist) do
            LoadHCF(dirlist[i])
            if f!=-1 then close(f) end if
        end for
        newSyntax = wasSyntax
        if length(errorMsg) then
            IupMessage(errorTitle,errorMsg)
        end if
--      pp(dirlist)
--      pp({helpnames,
--          helpfiles,
--          menuTxts,
--          F1lists})
    end if
end procedure
initHlp()

--/*
--DEV we ain't gonna be using these in pGUI!!!
global sequence sampleBrush,    -- brushes (must be kept up to date)
                sampleBrushClr -- actual colours of ""

        sampleBrush = repeat(0,MAXnColours)
--      sampleBrushClr = ColourTab+1    -- Force reBrush to create new brushes on first file open (below 12/7)
        sampleBrushClr = ColourTab
--      for i=1 to length(sampleBrushClr) do sampleBrushClr[i] += 1 end for
        for i=1 to length(sampleBrushClr) do
            sampleBrushClr[i] = sampleBrushClr[i] + 1
        end for

global atom backBrush

global atom wwrapPen

global function reBrush(sequence cnew, sequence cold)
atom oldBrush
integer oldColour
    for i=1 to length(cnew) do
        if cnew[i]!=cold[i] then
            oldBrush = sampleBrush[i]
            if oldBrush then {} = c_func(xDeleteObject,{oldBrush}) end if
            oldColour = cold[i]
            sampleBrush[i] = c_func(xCreateSolidBrush, {oldColour})
            sampleBrushClr[i] = oldColour
            if i=1 then
                backBrush = sampleBrush[1]
--          elsif i=Other then
                if oldBrush then {} = c_func(xDeleteObject,{wwrapPen}) end if
                wwrapPen = c_func(xCreatePen,{2,1,not_bits(oldColour)})
                {} = c_func(xSelectObject,{mainDC,wwrapPen})
            end if
            cnew[i] = oldColour
        end if
    end for
    return cnew
end function
sampleBrushClr = reBrush(sampleBrushClr,ColourTab)  -- added 12/7
--*/
