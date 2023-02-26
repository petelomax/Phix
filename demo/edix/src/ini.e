--without trace

        ------------------------------
        --          ini.e           --
        ------------------------------

-- this code will manage an ini file that is similar but probably better than the
-- standard MS functions because it is easy-to-use and much more comprehensive as
-- it allows the pre-defined data types to be created/retrieved at will without any
-- annoying intermediate processing step.


-- there are 2 categories of lines: Groups and Entries

-- A Group heading is the name of particular set entries and defined by
-- bracketed text, eg: [dummy group] -- note that spaces are allowed *within* the name

-- Elements (entries) within groups are defined as:
-- heading = data

-- the heading part of an entry line is any name you want except it should be unique
-- amongst the other names in that particular group. It too can have spaces *within*
-- the name, eg:
-- window size = 600, 400

-- The data part of an entry line can be an integer (with or without sign),
-- or a boolean value 0 or 1,
-- or a string of TWO OR MORE integers separated by commas (whitespace ignored)
-- or plain text, eg:

-- variable x = 1234
-- auto complete mode = 1
-- isWindowMaximized = 1
-- window pos = 1,1, 300 ,200
-- param string = c:\rds\bin\ed.e

-- NB: if the 'text' consists of digits (and separators) then it will be
-- interpreted as an integer or string of integers

-- variable length text lists are also possible but should have own group
-- and, unlike normal entries, have no assignment, eg..

-- [previous session files]
-- c:\euphoria\bin\ed.ex
-- c:\euphoria\project\enh_ed.ex
-- c:\euphoria\project\virus.ex
-- c:\borland\prj\crash_ms.cpp

-- NB: any whitespace on the ends of text or headings or even ascii text lists are removed
-- Eu-type comments can be put anywhere (manually) but will be viewed as part of the closest
-- preceding group declaration in the ini file. Any comments that precede the
-- first group declaration will stay in that area. Any group with all redundant entries
-- in it will be removed.
-- Any non-ascii lines (OR BLANK LINES)loaded in will be ignored (dumped).
-- There are few restrictions for group & entry names:
--  (i)Each group name must be unique amongst the other group names
--  (ii)An entry name must be unique amongst the other entry names within it's group
--  (iii)Spaces(or any whitespace) on the ends of any group or entry name are removed
--  (iv)Spaces within the bounds of a name are retained

        --=======
        -- TO USE
        --=======
-- put this line near the start of the program
-- include ini.e

-- make a group somewhere
-- defineIniGroup("hi scores")

-- make entry headings for the just-defined group
-- defineIniHeading("Highest Score #1", INT)
-- defineIniHeading("Highest Score #2", INT)
-- defineIniHeading("Highest Score #3", INT)

-- now, somewhere after these defines make a routine to capture the initial values
-- procedure initializeHiScores()
-- switchToIniGroup("hi scores") -- NB this is needed ONLY where more than 1 group is defined
--  hi_score1 = getIniValue("Highest Score #1", 0) -- note default values
--  hi_score2 = getIniValue("Highest Score #2", 0)
--  hi_score = getIniValue("Highest Score #3", 0)
-- end procedure

-- and a routine to set the values back into the ini.e
-- procedure store_hi_scores()
--  setIniValue("Highest Score #1", hi_score1)
--  setIniValue("Highest Score #2", hi_score2)
--  setIniValue("Highest Score #3", hi_score3)
-- end procedure

-- and now finally, write the ini file to disk
-- saveIniToDisk()

-- when the program is run the next time then the ini structure will *automatically*
-- be populated with the groups, entries & values and a call to getIniValue() will
-- retrieve the saved items and ignore the default values

        --=======
        -- END
        --=======

-- Usage notes:
-- Access to/from the ini can be spread around several object files in an app but
-- when the saveIniToDisk() routine is invoked only one object should do this.
-- When the ini file is "included" the data from the file *.ini is automatically loaded
-- The name of the ini file is same as the executable program (I stole this idea from ee:cs Hehehe...)
-- so, editor.exw will have editor.ini created etc..
-- Comments (EU-style) can only be externally edited into the *.ini file and there they shall remain
-- The lines in the ini file are re-written during each save so any group or entry names can be edited in the
-- object include file with impunity and the ini system will absorb these while quietly disposing
-- of the redundant names - very clever methinks!
-- Decimal values (eg, ATOM) are NOT allowed at this time (but would be trivial to implement).
-- Multi-dimensional sequences are also NOT allowed (as a native type)but a user
-- could still capture them by using setIniTextValue() & getIniTextValues() which deal with *single* lines of random text.
-- When an entry is initially defined, a flag is added to specify what TYPE of value will be set into it
-- and if an attempt is made to set the value using an illegal type then ini.e will crash
-- thereby alerting the programmer to correct the situation
-- An entry defined as SEQ must have 2 or more integic elements but the calling routine
-- should check that the length is correct because ini.e will not know if it is or not.
-- Whitespace formatting of *.ini is done automatically

--- data types
global 
constant
    BOOL = 1, -- boolean type True or False, 1 or 0
    INT = 2, -- integer
    SEQ = 3, -- sequence of integers
    TEXT = 4  -- string of text
constant
    ASC = 5, -- text line (used for variable length 'pure' text lists/lines)
    COM = 6, -- comment
    GRP = 7  -- group

-- Attribute lists
sequence
    DATATYPE,
    HEADING,
    RESFLAG,
    VALUE

-- keep track of current group & entry
integer groupstart = 0,
        groupend = 0

-- keep track of ini file name
string completeinipath = ""

procedure halt()
    ? 9/0   -- [leave in]
end procedure

type isNumber(integer c)
    return c>='0' and c<='9'
end type

type isWhiteSpace(integer c)
    return c==' ' or c=='\t' or c=='\n'
end type

type isArithmeticSign(integer c)
    return c=='-' or c=='+'
end type

type isTitleChar(integer c)
    return (c>='a' and c<='z')
        or (c>='A' and c<='Z')
        or (c>='0' and c<='9')
        or  c==' '
        or  c=='_'
end type

function splitTitleAndData(string line)
    line = trim(line)
    integer e = find('=', line)
    if e then
        return {trim(line[1..e-1]), trim(line[e+1..$])}
    end if
    return {"", line }
end function

function allNumbersAndSeparators(string line)
    for i=1 to length(line) do
        integer c = line[i]
        if not (isArithmeticSign(c) or isNumber(c) or c==',' or isWhiteSpace(c)) then
            return False 
        end if
    end for
    return True
end function

function allAsciiValues(string line)
    for i=1 to length(line) do
        integer c = line[i]
        if not ((c>=' ' and c<=127) or (c=='\t' or c=='\n')) then
            return False 
        end if
    end for
    return True 
end function    

function removeAllWhiteSpace(string line)
    string s = ""
    for i=1 to length(line) do
        integer c = line[i]
        if not isWhiteSpace(c) then
            s &= c
        end if
    end for
    return s
end function

function extractDataValue(string line)
    sequence result = {}
    string numstr = ""

    -- remove any whitespace
    line = removeAllWhiteSpace(line)

    -- accumulate number chars and convert at each separator
    for i=1 to length(line) do
        integer char = line[i]
        if char==',' then -- separator found
            if length(numstr) then -- ok, convert to integer
                result &= to_integer(numstr)
                numstr = ""
            end if
        elsif isNumber(char) or isArithmeticSign(char) then
            numstr &= char
        else -- error
            halt()
        end if
    end for

    -- tidy last piece
    if length(numstr) then -- ok, convert to integer
        result &= to_integer(numstr)
    end if

    -- return integer if only one numeric element
    if length(result)==1 then
        return result[1]
    end if

    return result

end function


function parseLine(string line) -- returns {type, data [, title]}
    string heading

    -- prepare line
    line = trim(line)
    integer len = length(line)

    -- corrupted line?
    if not allAsciiValues(line) or len==0 then
        return False
    end if

    -- group line?
    if line[1]=='[' then
        if line[len]==']' then
            string group = trim(line[2..$-1])
            if group=="" then
                return False
            else
                return { GRP, group, False, {} }
            end if
        else -- corrupted line
            return { ASC, {}, False, line } 
        end if
    end if

    -- comment line?
    if len>1 then
        if line[1..2]=="--" then
            return { COM, line, True, line }
        end if
    end if

    -- can we split title from data?
    {heading,line} = splitTitleAndData(line)

    -- if no title then assume line is ascii
    if heading=="" then
        if line=="" then
            return False
        else
            return {ASC, {}, False, line }
        end if
    end if

    -- now, deal to 'assignment' lines
    if allNumbersAndSeparators(line) then -- integer or string of integers
        object data = extractDataValue(line)
        if integer(data) then
            return { INT, heading, False, data }
        elsif sequence(data) then
            return { SEQ, heading, False, data }
        end if

    else 
        return { TEXT, heading, False, line }

    end if

end function

function checkData(integer entryIdx, object val)
    integer flag = DATATYPE[entryIdx]
    switch flag do
        case BOOL:  return integer(val) and (val==1 or val==0)
        case INT:   return integer(val)
        case SEQ:   return sequence(val) and length(val)>=2
        case TEXT:  return sequence(val) and length(val)>=0
        case ASC:   return true
    end switch
    return 9/0
end function

function getWriteString(integer entryIdx)
    string result
    integer flag = DATATYPE[entryIdx]
    object val = VALUE[entryIdx]

    if flag<ASC then
        result = "  " & HEADING[entryIdx] & " = "
    end if

    if flag<=INT then -- covers BOOL as well
        result &= sprintf("%d", val)

    elsif flag==SEQ then
        for i=1 to length(val) do
            result &= sprintf("%d", val[i])
            if i<length(val) then
                result &= ", "
            end if
        end for

    elsif flag==TEXT then
        result &= val

    elsif flag==ASC then
        result = "  " & val

    elsif flag==COM then
        result = val

    elsif flag==GRP then
        result = "[" & HEADING[entryIdx] & "]"

    else -- oops
        halt()

    end if

    return result
end function


procedure set_groupend()
    groupend = length(DATATYPE)
    for i=groupstart+1 to length(DATATYPE) do
        if DATATYPE[i]==GRP then
            groupend = i-1
            return
        end if
    end for
end procedure

function findGroup(sequence groupname)
    for i=1 to length(DATATYPE) do
        if DATATYPE[i]==GRP
        and groupname==HEADING[i] then
            return i
        end if
    end for
    return 0
end function

function getGroups()
    sequence groupnames = {}
    for i=1 to length(DATATYPE) do
        if DATATYPE[i]==GRP then
            groupnames = append(groupnames,HEADING[i])
        end if
    end for
    return groupnames
end function

function findEntry(sequence entryname) -- from the current group find the entry
    set_groupend()
    for i=groupstart+1 to groupend do
        if entryname==HEADING[i] then
            return i
        end if
    end for
    return -1 - groupend
end function

function findGroupOwner(integer entryIdx)
    for i=entryIdx-1 to 1 by -1 do
        if DATATYPE[i]==GRP then
            return i
        end if
    end for
    return entryIdx -- this line will occur where a comment precedes the first group
end function

procedure insertBlankEntry(integer idx)

    DATATYPE &= 0
    HEADING &= 0
    RESFLAG &= 0
    VALUE &= 0
    integer len = length(DATATYPE)

    -- move entries beyond insertion point along 1
    DATATYPE[idx+1..len] = DATATYPE[idx..len-1]
    HEADING[idx+1..len] = HEADING[idx..len-1]
    RESFLAG[idx+1..len] = RESFLAG[idx..len-1]
    VALUE[idx+1..len] = VALUE[idx..len-1]

end procedure

procedure setEntry(integer idx, integer datatype, sequence heading, integer flag, object val)
    DATATYPE[idx] = datatype
    HEADING[idx] = heading
    RESFLAG[idx] = flag
    VALUE[idx] = val
end procedure

global 
procedure switchToIniGroup(object group)

    if string(group) then
        group = findGroup(group)
        if group==0 then
            halt()
        end if
    elsif DATATYPE[group]!=GRP then
        halt()
    end if

    groupstart = group
    set_groupend()

end procedure

procedure resolveEntry(integer entryIdx)
    RESFLAG[entryIdx] = True
    RESFLAG[groupstart] = True
end procedure

--global 
procedure defineIniGroup(string groupname)

    -- test grp already exists
    integer grp = findGroup(groupname)

    -- if not then add new one & switch to it
    if grp==0 then
        grp = length(DATATYPE) + 1
        insertBlankEntry(grp) -- add to end of list
        setEntry(grp, GRP, groupname, False, {})
    end if

    -- set to unresolved
    RESFLAG[grp] = False

    -- switch to this group
    switchToIniGroup(grp)

end procedure


global 
procedure defineIniHeading(string entryname, integer entrytype)

    integer entryIdx = findEntry(entryname)

    if entryIdx<0 then -- have to insert
        entryIdx = 0 - entryIdx
        insertBlankEntry(entryIdx)
        HEADING[entryIdx] = entryname

        -- ensure that 'blank' value not mistaken for the real thing
        -- by setting a definitely faulty type-value
        if entrytype==INT or entrytype==BOOL then
            VALUE[entryIdx] = {}
        else
            VALUE[entryIdx] = 0
        end if

    end if

    -- update these flags for all
    DATATYPE[entryIdx] = entrytype
    RESFLAG[entryIdx] = False

end procedure


global 
procedure setIniValue(sequence entryname, object val)
    integer entryIdx

    -- if heading string is null then act as if for text (ASC) line, etc..
    if entryname=="" then -- ascii line
        entryIdx = findEntry({{}}) -- unique construction to find insertion point
        entryIdx = 0 - entryIdx
        insertBlankEntry(entryIdx)
        DATATYPE[entryIdx] = ASC
        if integer(val) then -- have to convert this to string
            val = { val }
        end if

    else
        -- test that heading is in current group
        entryIdx = findEntry(entryname)
        if entryIdx<0 then -- oops, not there
            halt()
        end if

    end if

    -- abort if value does not agree with type
    if not checkData(entryIdx, val) then
        halt()
    end if

    -- set value
    VALUE[entryIdx] = val

    -- set resolved
    resolveEntry(entryIdx)

end procedure

global 
procedure setIniTextValue(object val)
    setIniValue("", val)
end procedure

global 
function getIniValue(string entryname, object defaultvalue)

    -- test that heading is in current grp
    integer entryIdx = findEntry(entryname)

    -- halt if not
    if entryIdx<0 then
        halt()
    end if

    -- if unresolved then test value against type, then default value against type
    if RESFLAG[entryIdx]==False then
        if not checkData(entryIdx, VALUE[entryIdx]) then
            if checkData(entryIdx, defaultvalue) then
                VALUE[entryIdx] = defaultvalue
            else
                halt() -- default value is wrong type
            end if
        end if

    end if

    resolveEntry(entryIdx)

    return VALUE[entryIdx]

end function


global 
function getIniTextValues()

    set_groupend()
    sequence result = {}

    for i=groupstart to groupend do
        if DATATYPE[i]==ASC then
            result = append(result, VALUE[i])
        end if
    end for

    if length(result) then
        resolveEntry(groupstart)
    end if

    return result

end function


/*
--with trace
function getIniPathName()

    -- name already acquired?
    if length(completeinipath)=0 then
    
        -- look for where ini file should be
        sequence path = command_line()
        string name = get_file_name(path[2])
        path = get_file_path(path[2])

        name = get_file_base(name)&".ini"

        completeinipath = join_path({path,name})
    end if

    return completeinipath
end function
--without trace
*/

integer wasNewIni

-- load ini file back from disk
--global 
--procedure loadIniFile(string inipathname=completeinipath)?
procedure loadIniFile(string inipathname)
    DATATYPE = {}
    HEADING = {}
    RESFLAG = {}
    VALUE = {}
    -- try to open a file that should be there
--  id = open(getIniPathName(), "r")
    completeinipath = inipathname
    integer id = open(inipathname, "r")
    if id!=-1 then -- (file not present or disk error?)
        while 1 do
            object line = gets(id)
            if integer(line) then exit end if
            line = parseLine(line)
            if sequence(line) then
                integer p = length(DATATYPE) + 1
                insertBlankEntry(p)
                setEntry(p, line[1], line[2], line[3], line[4])
                if line[1]==COM then -- assert group as true
                    p = findGroupOwner(p)
                    RESFLAG[p] = True
                end if
            end if
        end while
        close(id)
    end if
    wasNewIni = length(DATATYPE)=0
end procedure

function getIniWriteData()
    sequence result = {}
    groupstart = 1
    while 1 do
        set_groupend()
        if DATATYPE[groupstart]!=GRP
        or RESFLAG[groupstart]==true then
            -- add space between groups
            if groupstart!=1 then
                result = append(result, "")
            end if
            -- cycle through group extracting all resolved entries
            for i=groupstart to groupend do
                if RESFLAG[i]==True then
                    result = append(result, getWriteString(i))
                end if
            end for
        end if
        groupstart = groupend + 1
        if groupstart>length(DATATYPE) then
            exit
        end if
    end while
    return result
end function

--global 
procedure saveIniToDisk()
-- save the ini details to disk as native Eu sequence
    integer id = open(completeinipath, "w")
    if id!=-1 then -- no error has occurred
        sequence lines = getIniWriteData()
        for i=1 to length(lines) do
            puts(id, lines[i] & '\n')
        end for
        close(id)
    end if
end procedure


--loadIniFile()

--DEV PL:
global function isNewIni()
    return wasNewIni
end function

global function decode11(string s, integer i)
-- (retrieves cursel session info from the config file)
    s = trim(s,"{}")
    sequence res = repeat(0,11),
            work = split(s,',')
    if length(work)=11 then
        for i=1 to 11 do
            string numstr = work[i]
            integer ri = 0
            for j=1 to length(numstr) do
                integer ch = numstr[j]
                if ch<'0' or ch>'9' then
                    -- (should never trigger)
                    ?"warning: ini cursel decode error"
                    ri = 0
                    exit
                end if
                ri = ri*10+(ch-'0')
            end for
            res[i] = ri
        end for
    else
        printf(1,"warning: decode11(%s) fail on file[%d]\n",{s,i})
    end if
    res[11] += 1    -- increment "un-visted count"
    return res
end function

function mini(sequence Name, integer Type, object Default)
    defineIniHeading(Name, Type)
    return getIniValue(Name, Default)
end function

sequence prevfiles, prevcursel
integer newcurrfile

--with trace
global procedure loadINI(string configname=completeinipath)
    string inipathname = join_path({initialcurrentdir,configname&".ini"})
    loadIniFile(inipathname)
    defineIniGroup("Main")
--trace(1)
    newcurrfile = mini("currfile", INT, 0)
--?{"newcurrfile = ",newcurrfile}
--X currfile = mini("currfile", INT, 0)

    defineIniGroup("Extensions")
    defineIniGroup("RunWiths")
--  defineIniGroup("KeyMappings")

    defineIniGroup("previous session")
    prevfiles = getIniTextValues()
end procedure

global procedure iniSetPrev(sequence _prevfiles, integer _newcurrfile)
    prevfiles = _prevfiles
    newcurrfile = _newcurrfile
end procedure

global procedure iniCreateTabs()
    prevcursel = repeat(0,length(prevfiles))
    for i=1 to length(prevfiles) do
        integer k = find(',',prevfiles[i])
        prevcursel[i] = decode11(prevfiles[i][k+1..$],i)
--      prevfiles[i] = prevfiles[i][1..k-1]
        string filepath = trim(prevfiles[i][1..k-1]),
                  title = get_file_name(filepath),
                   path = get_file_path(filepath,dropslash:=false)
        prevfiles[i] = filepath
--      add_tab(integer opening, string path="", string title=untitled, sequence text={}, sequence linelengths={}, integer encoding=ENC_NONE)
        add_tab(true, path, title, {"Loading..."}, {10})
    end for
    sequence verify = getGroups()
    for i=1 to length(verify) do
        if match("Verify",verify[i])=1 then
            defineIniGroup(verify[i])
            sequence groups = getIniTextValues()
            integer k = find(groups[1],prevfiles)
            for j=1 to length(groups) do
                string group = trim(groups[j])
                setIniTextValue(group)
                if prevfiles[i+k-1]!=group then
?"gr-oops ini.e line 764"
--                  ?9/0
                end if
            end for
        end if
    end for
--  ?inipathname
--?{"newcurrfile,currfile",newcurrfile,currfile}
    currfile = newcurrfile
--?currfile
end procedure

global function load_prev_file()
    -- load one file at a time, as part of the idle handling
    integer i = currfile
--  integer i = newcurrfile
--  newcurrfile = mini("currfile", INT, 0)
--  currfile = mini("currfile", INT, 0)

    if i=0 or i>length(prevfiles) or prevfiles[i]=0 then
        i = 0
        for j=1 to length(prevfiles) do
            if prevfiles[j]!=0 then i=j exit end if
        end for
    end if
    if i!=0 then
--?9/0 -- tbc
--      if i=newcurrfile then
--          currfile = newcurrfile
--      end if
--?currfile
        if openFile(true, prevfiles[i], -1) then
--?{{{currfile}}}
if length(filecursel)!=length(prevcursel) then
    ?{"oops, ini.e line 816...",prevfiles[i]}
else
--end if
            filecursel[i] = prevcursel[i]
            restcursel()
            if i=currfile then
--          if i=newcurrfile then
--              currfile = newcurrfile
                IupRefresh(tabs)
                appUpdateRender()
            end if
--          prevfiles[i] = 0
end if
        else
?{"oops, ini.e line 826",prevfiles[i]}
--?9/0
        end if
        prevfiles[i] = 0
        return true
    end if
    return false
end function

--global procedure saveINI()    -- can be called many times. [???]
global procedure saveINI(sequence fdii={}, integer newcur=currfile) -- can be called many times. [???]
--global procedure saveINI(sequence fdii, integer newcur) -- must only be called once...
    sequence restore = {DATATYPE,HEADING,RESFLAG,VALUE}
    save_extensions()
    switchToIniGroup("Main")
--  setIniValue("currfile", currfile)
    setIniValue("currfile", newcur)
    switchToIniGroup("previous session")
--  for i=1 to length(filecursel) do
    if fdii={} then fdii=tagset(length(filenames)) end if
    for i=1 to length(fdii) do
        integer fi = fdii[i]
--      setIniTextValue(filepaths[i]&filenames[i]&','&sprint(filecursel[i]))
        setIniTextValue(filepaths[fi]&filenames[fi]&','&sprint(filecursel[fi]))
    end for
    saveIniToDisk()
    {DATATYPE,HEADING,RESFLAG,VALUE} = restore
end procedure

--/*
USEINI

defineIniGroup: only used in eaini.
switchToIniGroup: only used in eaini.
defineIniHeading: only used in eaini.
setIniValue: only used in eaini.
setIniTextValue: only used in eaini.
getIniValue: only used in eaini.
getIniTextValues: only used in eaini.
loadIniFile: only used locally(!)
saveIniToDisk: only used in eaini.
isNewIni: only used in eaini.

eaini:
loadINI (local, just once)
loadIniPart2: basically onload, after resize
saveINI: just the once


cursel96={26,953,941,0,1,16,953,0,0,0,14}
filepath96=C:\Program Files (x86)\Phix\demo\pGUI\pdemo\demo.ew
procedure savecursel()
    if currfile then
        filecursel[currfile] = {CursorX,CursorY,TopLine,Column,selON,selX,selY,
                                lastFold,lastFoldEnd,TopChunk,0}
        bCfwds[currfile] = bCfwd
    end if
end procedure

sug:
[previous session]
  C:\Program Files (x86)\Phix\p.exw,0,2859,2847,0,0,5,2795,0,0,0,0,0

a) pathname canot have a ',' in it.
b) each line must contain exactly 12 commas, 13 fields.
c) decode by find quote, strip, then split().

save is:
        switchToIniGroup("previous session")
        for i=1 to length(filecursel) do
            setIniTextValue(filepaths[i]&filenames[i]&','&sprint(filecursel[i]))
        end for
restore is (assuming deferred open):
(let's open currfile first, do this in the idle loop...)
        ini:defineIniGroup("previous session")
        prevfiles = ini:getIniTextValues()
-- or as part of background??
        filecursel = repeat(0,length(prevfiles))
        for i=1 to length(prevfiles) do
            integer k = find(',',prevfiles[i])
            filecursel[i] = decode11(prevfiles[i][k+1..$])
            prevfiles[i] = prevfiles[i][1..k-1]
        end for

get rid of [Session] from .cfg, ditto 
[KeyMappings]
<Alt G>=<Ctrl G>
<Alt Insert>=<Ctrl '>'>
<Ctrl '\t'>=<Alt '>'>
<Ctrl Delete>=<Ctrl X>
<Ctrl Insert>=<Ctrl C>
<Ctrl Shift '\t'>=<Alt '<'>
<Ctrl Shift Delete>=<Ctrl Shift X>
<Ctrl Shift Insert>=<Ctrl Shift C>
<Shift Insert>=<Ctrl V>
Active=1
KEYS=<Alt Delete>,<Alt G>,<Alt Insert>,<Ctrl '\t'>,<Ctrl Delete>,<Ctrl Insert>,<Ctrl Shift '\t'>,<Ctrl Shift Delete>,<Ctrl Shift Insert>,<Shift Insert>

and 
[Extensions]
KEYS=bas,bat,eu,ex,exw,fb,go,htm,html,jl,php,pl,py,py35,rb
bas="C:\Go\FreeBASIC-1.05.0-win32\fbc.exe"
bat=open
eu==exw
ex==exw
exw="C:\Program Files (x86)\Phix\pw9.exe"
fb="C:\Go-old\FreeBASIC-1.05.0-win32\fbc.exe" -b
go="C:\Go\bin\go.exe" run
htm=open
html=open
jl="C:\Users\Pete\AppData\Local\Programs\Julia\Julia-1.5.3\bin\julia.exe"
php=open
pl="C:\Strawberry\perl\bin\perl.exe"
py=py27
py35="C:\Program Files (x86)\Python35-32\pythonw.exe"
rb="C:\Ruby22\bin\ruby.exe"

with (amongst others)
[Main]
  currfile = 38
Max=282
current=278


IDLE_ACTION  Predefined IUP action, generated when there are no events or messages to be processed. Often used to perform background operations. 

function idle_action()
Returns: if IUP_CLOSE is returned the current loop will be closed and the callback will be removed. 
If IUP_IGNORE is returned the callback is removed and normal processing continues. 

Notes
The Idle callback will be called whenever there are no messages left to be processed. 
This may occur more frequently than expected, for example if you move the mouse over the application the idle callback will be called
many times because the mouse move message is processed so fast that the Idle will be called before another mouse move message occurs.
[No mouse(/user) is likely to generate 100 messages/s, which on a GHz box means ten million clocks between each in which toperform some idle processing.] 

So this callback changes the message loop to a more CPU consuming one. 
It is important it be set NULL when not used, otherwise the application consumes CPU even if the callback is doing nothing. 

It can only be set using (eg) IupSetGlobalFunction("IDLE_ACTION", Icallback("idle_action")). 
 

Have a [set of] section headers to verify, eg:
[Verify]
makephix.exw
toc.txt
index.txt
phix.txt*2
euphoria.syn
phix.syn

a) verify on load and insert/move/alert as rqd
b) verify on save
c) verify anytime

open tab next to...
*/
