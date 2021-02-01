without trace

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
-- or a string of TWO OR MORE integers separated by commas( whitespace ignored)
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
global constant
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
    VALUE,
    RAWLINE

procedure resetIniLists()
    DATATYPE = {}
    HEADING = {}
    RESFLAG = {}
    VALUE = {}
    RAWLINE = {}
end procedure

-- keep track of current group & entry
integer
    currententry,
    groupstart,
    groupend

    currententry = 0
    groupstart = 0
    groupend = 0

-- keep track of ini file name
sequence completeinipath completeinipath = {}

procedure halt()
    ? 9/0   -- [leave in]
end procedure


global type isLetter(integer c)
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z')
end type

global type isNumber(integer c)
    return c >= '0' and c <= '9'
end type

global type isWhiteSpace(integer c)
    return c = ' ' or c = '\t' or c = '\n'
end type

type isArithmeticSign(integer c)
    return c = '-' or c = '+'
end type

type isTitleChar(integer c)
    return isLetter(c) or isNumber(c) or c = ' ' or c = '_'
end type

function splitTitleAndData(sequence line)
integer e, len

    len = length(line)
    e = find('=', line)

    if e then
        for i = 1 to e do
            if not isTitleChar(line[i]) then
                exit
            end if
        end for
        return { line[1..e-1], line[e+1..len] }
    end if

    return { {}, line }

end function

function allNumbersAndSeparators(sequence line)
integer c
    for i = 1 to length(line) do
        c = line[i]
        if not (isArithmeticSign(c) or isNumber(c) or c = ',' or isWhiteSpace(c)) then
            return False 
        end if
    end for
    return True
end function


function allAsciiValues(sequence line)
integer c
    for i = 1 to length(line) do
        c = line[i]
        if not ( (c >= ' ' and c <= 127)  or  ( c='\t' or c='\n' ) ) then
            return False 
        end if
    end for
    return True 
end function    


global function convertTextNumberToInteger(sequence numbre)
integer r, len, pwr, c, neg
sequence s

    neg = 0
    pwr = 1

    -- remove any sign symbols
    s = ""
    for i = 1 to length(numbre) do
        if isArithmeticSign(numbre[i]) then
            if numbre[i] = '-' then
                neg = 1
            end if
        else
            s &= numbre[i]
        end if
    end for
    numbre = s
        
    len = length(numbre)
    r = numbre[len] - '0'

    -- calculate number
    for i = len-1 to 1 by -1 do
        c = numbre[i]
        pwr *= 10
        r += (c - '0') * pwr
    end for

    -- take care of negative numbre
    if neg then
        r = 0 - r
    end if

    -- exit
    return r
end function

function removeEndWhiteSpace(sequence line)
integer len, a, b

    len = length(line)

    if not len then
        return line
    end if

    a = 0
    b = 0

    -- establish first non-whitespace char
    for i = 1 to len do
        if not isWhiteSpace( line[i] ) then
            a = i
            exit
        end if
    end for

    -- exit if all whitespace
    if a = 0 then
        return {}
    end if

    -- establish last non-whitespace char
    for i = len to 1 by -1 do
        if not isWhiteSpace( line[i] ) then
            b = i
            exit
        end if
    end for

    return line[a .. b]

end function


global function removeAllWhiteSpace(sequence line)
sequence s
integer c

    s = {}

    for i = 1 to length(line) do
        c = line[i]
        if not isWhiteSpace(c) then
            s &= c
        end if
    end for

    return s
end function


function extractDataValue(sequence line)
sequence result, numbre
integer char

    result = {}
    numbre = {}

    -- remove any whitespace
    line = removeAllWhiteSpace( line )

    -- accumulate number chars and convert at each separator
    for i = 1 to length(line) do
        char = line[i]
        if char = ',' then -- separator found
            if length(numbre) then -- ok, convert to integer
                result &= convertTextNumberToInteger(numbre)
                numbre = {}
            end if
        elsif isNumber(char) or isArithmeticSign(char) then
            numbre &= char
        else -- error
            halt()
        end if
    end for

    -- tidy last piece
    if length(numbre) then -- ok, convert to integer
        result &= convertTextNumberToInteger(numbre)
    end if

    -- return integer if only one numeric element
    if length(result) = 1 then
        return result[1]
    end if

    return result

end function


function parseLine(sequence line) -- returns {type, data [, title]}
integer len
sequence junk, heading
object data

    -- prepare line
    line = removeEndWhiteSpace(line)
    len = length(line)

    -- corrupted line?
    if not allAsciiValues(line) or len = 0 then
        return False
    end if

    -- group line?
    if line[1] = '[' then
        if line[len] = ']' then
            junk = line[2 .. len - 1]
            junk = removeEndWhiteSpace(junk)
            if equal("", junk) then
                return False
            else
                return { GRP, junk, False, {} }
            end if
        else -- corrupted line
            return { ASC, {}, False, line } 
        end if
    end if

    -- comment line?
    if len > 1 then
        if equal(line[1..2], "--") then
            return { COM, line, True, line }
        end if
    end if

    -- can we split title from data?
    junk = splitTitleAndData(line)
    heading = removeEndWhiteSpace( junk[1] )
    line = removeEndWhiteSpace( junk[2] )

    -- if no title then assume line is ascii
    if equal("", heading) then
        if equal("", line) then
            return False
        else
            return {ASC, {}, False, line }
        end if
    end if

    -- now, deal to 'assignment' lines
    if allNumbersAndSeparators(line) then -- integer or string of integers
        data = extractDataValue(line)
        if integer(data) then
            return { INT, heading, False, data }
        elsif sequence(data) then
            return { SEQ, heading, False, data }
        end if

    else 
        return { TEXT, heading, False, line }

    end if

end function

function checkData(integer entryIdx, object value)
integer flag

    flag = DATATYPE[entryIdx]

    if flag = BOOL then
        if integer(value) and (value = 1 or value = 0) then
            return True
        end if

    elsif flag = INT then
        if integer(value) then
            return True
        end if

    elsif flag = SEQ then
        if sequence(value) and length(value) >= 2 then
            return True
        end if
    elsif flag = TEXT then
        if sequence(value) and length(value) >= 0 then
            return True
        end if

    else -- flag = ASC
        return True

    end if

    return False

end function

function getWriteString(integer entryIdx)
integer flag
object value
sequence result

    flag = DATATYPE[entryIdx]
    value = VALUE[entryIdx]

    if flag < ASC then
        result = "  " & HEADING[entryIdx] & " = "
    end if

    if flag <= INT then -- covers BOOL as well
        result &= sprintf("%d", value)

    elsif flag = SEQ then
        for i = 1 to length(value) do
            result &= sprintf("%d", value[i])
            if i < length(value) then
                result &= ", "
            end if
        end for

    elsif flag = TEXT then
        result &= value

    elsif flag = ASC then
        result = "  " & value

    elsif flag = COM then
        result = value

    elsif flag = GRP then
        result = "[" & HEADING[entryIdx] & "]"

    else -- oops
        halt()

    end if

    return result
end function


procedure set_groupend()
    groupend = length(DATATYPE)
    for i = groupstart+1 to length(DATATYPE) do
        if DATATYPE[i] = GRP then
            groupend = i-1
            return
        end if
    end for
end procedure


function findGroup(sequence groupname)
    for i = 1 to length(DATATYPE) do
        if DATATYPE[i] = GRP then
            if equal( groupname, HEADING[i] ) then
                return i
            end if
        end if
    end for
    return 0
end function


function findEntry(sequence entryname) -- from the current group find the entry
    set_groupend()
    for i = groupstart+1 to groupend do
        if equal( entryname, HEADING[i] ) then
            return i
        end if
    end for
    return -1 - groupend
end function

function findGroupOwner(integer entryIdx)
    for i = entryIdx-1 to 1 by -1 do
        if DATATYPE[i] = GRP then
            return i
        end if
    end for
    return entryIdx -- this line will occur where a comment precedes the first group
end function

procedure insertBlankEntry(integer insert)
integer len

    DATATYPE &= 0
    HEADING &= 0
    RESFLAG &= 0
    VALUE &= 0
    len = length(DATATYPE)

    -- move entries beyond insertion point along 1
    DATATYPE[insert + 1 .. len] = DATATYPE[insert .. len - 1]
    HEADING[insert + 1 .. len] = HEADING[insert .. len - 1]
    RESFLAG[insert + 1 .. len] = RESFLAG[insert .. len - 1]
    VALUE[insert + 1 .. len] = VALUE[insert .. len - 1]

end procedure

procedure setEntry
(integer insert, integer datatype, sequence heading, integer flag, object val)
    DATATYPE[insert] = datatype
    HEADING[insert] = heading
    RESFLAG[insert] = flag
    VALUE[insert] = val
end procedure

global procedure switchToIniGroup( object group )

    if sequence(group) then
        group = findGroup(group)
        if group = 0 then
            halt()
        end if
    elsif DATATYPE[group] != GRP then
        halt()
    end if

    groupstart = group
    set_groupend()

end procedure


procedure resolveEntry(integer entryIdx)
    RESFLAG[entryIdx] = True
    RESFLAG[groupstart] = True
end procedure

global procedure defineIniGroup(sequence groupname)
integer grp

    -- test grp already exists
    grp = findGroup(groupname)

    -- if not then add new one & switch to it
    if grp = 0 then
        grp = length(DATATYPE) + 1
        insertBlankEntry( grp ) -- add to end of list
        setEntry(grp, GRP, groupname, False, {})
    end if

    -- set to unresolved
    RESFLAG[grp] = False

    -- switch to this group
    switchToIniGroup(grp)

end procedure


global procedure defineIniHeading(sequence entryname, integer entrytype)
integer entryIdx

    entryIdx = findEntry(entryname)

    if entryIdx < 0 then -- have to insert
        entryIdx = 0 - entryIdx
        insertBlankEntry( entryIdx )
        HEADING[entryIdx] = entryname

        -- ensure that 'blank' value not mistaken for the real thing
        -- by setting a definitely faulty type-value
        if entrytype = INT or entrytype = BOOL then
            VALUE[entryIdx] = {}
        else
            VALUE[entryIdx] = 0
        end if

    end if

    -- update these flags for all
    DATATYPE[entryIdx] = entrytype
    RESFLAG[entryIdx] = False

end procedure


global procedure setIniValue(sequence entryname, object value)
integer entryIdx

    -- if heading string is null then act as if for text (ASC) line, etc..
    if equal(entryname, "") then -- ascii line
        entryIdx = findEntry({{}}) -- unique construction to find insertion point
        entryIdx = 0 - entryIdx
        insertBlankEntry( entryIdx )
        DATATYPE[entryIdx] = ASC
        if integer(value) then -- have to convert this to string
            value = { value }
        end if

    else
        -- test that heading is in current group
        entryIdx = findEntry(entryname)
        if entryIdx < 0 then -- oops, not there
            halt()
        end if

    end if

    -- abort if value does not agree with type
    if not checkData(entryIdx, value) then
        halt()
    end if

    -- set value
    VALUE[entryIdx] = value

    -- set resolved
    resolveEntry(entryIdx)

end procedure

global procedure setIniTextValue(object value)
    setIniValue("", value)
end procedure

global function getIniValue(sequence entryname, object defaultvalue)
integer entryIdx

    -- test that heading is in current grp
    entryIdx = findEntry(entryname)

    -- halt if not
    if entryIdx < 0 then
        halt()
    end if

    -- if unresolved then test value against type, then default value against type
    if RESFLAG[entryIdx] = False then
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


global function getIniTextValues()
sequence result

    set_groupend()
    result = {}

    for i = groupstart to groupend do
        if DATATYPE[i] = ASC then
            result = append( result, VALUE[i] )
        end if
    end for

    if length(result) then
        resolveEntry(groupstart)
    end if

    return result

end function

----DEV use the global one in disk.e
--function extractPathAndName(sequence pathname)
---- extract path & name information from complete pathname
--sequence path, name
--integer len, pos
--
--  len = length(pathname)
--  pos = 0
--
--  for i = len to 1 by -1 do
--      if pathname[i] = '\\' then -- delimiter found, must always exist
--          pos = i
--          exit
--      end if
--  end for
--
--  path = pathname[1..pos]
--  name = pathname[pos+1..len]
--
--  return {path, name}
--end function
--
----------------------------------------------------------------------------------
--function extractNameAndExtn(sequence filename)
--sequence name, extn
--
--  name = filename
--  extn = ""
--
--  for i=length(filename) to 1 by -1 do
--      if filename[i] = '.' then
--          name = filename[1 .. i-1]
--          extn = filename[i + 1 .. length(filename)]
--          exit
--      end if
--  end for
--
--  return { name, extn }
--end function



--with trace
function getIniPathName()
sequence path, name

    -- name already acquired?
    if length(completeinipath) then
        return completeinipath
    end if
    
    -- look for where ini file should be
    path = command_line()
    path = extractPathAndName(path[2]) -- look at executable file dir

    name = path[2]
    path = path[1]
--DEV fails on 2.5:
--if not equal(path,initialcurrentdir) then trace(1) end if

    name = extractNameAndExtn(name)
    name = name[1] & ".ini" -- ini file will have same name as executable

    completeinipath = path & name

    return completeinipath
end function
--without trace

-- load ini file back from disk
global procedure loadIniFile()
integer id, p
object line

    resetIniLists()

    -- try to open a file that should be there
    id = open( getIniPathName(), "r" )
    if id = -1 then -- file not present or disk error
        return
    end if

    while 1 do
        line = gets(id)
        if integer(line) then
            exit
        end if
        line = parseLine(line)
        if sequence (line) then

            p = length(DATATYPE) + 1
            insertBlankEntry( p )
            setEntry( p, line[1], line[2], line[3], line[4] )

            if line[1] = COM then -- assert group as true
                p = findGroupOwner(p)
                RESFLAG[p] = True
            end if

        end if
    end while

    close(id)

end procedure



function getIniWriteData()
sequence result

    result = {}
    groupstart = 1

    while 1 do

        set_groupend()

        if DATATYPE[groupstart] = GRP
        and RESFLAG[groupstart] = False then -- skip

        else

            -- add space between groups
            if groupstart != 1 then
                result = append(result, "" )
            end if

            -- cycle through group extracting all resolved entries
            for i = groupstart to groupend do
                if RESFLAG[i] = True then
                    result = append(result, getWriteString(i) )
                end if
            end for
        
        end if

        groupstart = groupend + 1
        if groupstart > length(DATATYPE) then
            exit
        end if

    end while

    return result

end function

global procedure saveIniToDisk()
-- save the ini details to disk as native Eu sequence
integer id
sequence lines

    -- attempt to open the file
    id = open(getIniPathName(), "w")

    -- exit on error
    if id = -1 then -- error has occurred
        return
    end if

    -- write out data to file
    lines = getIniWriteData()
    for i = 1 to length(lines) do
        puts(id, lines[i] & '\n')
    end for

    -- exit
    close(id)

end procedure



loadIniFile()

--DEV PL:
global function isNewIni()
    return length(DATATYPE)=0
end function

--global procedure putini()
--  for i = 1 to length(DATATYPE) do
--      if RESFLAG[i]=0 then
--          if DATATYPE[i] = ASC then
--              puts(1,"\n" & VALUE[i] )
--          else
--              puts(1,"\n" & HEADING[i] )
--          end if
--      end if
--  end for
--puts(1, "\n")
--end procedure


-- putini()
