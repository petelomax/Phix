--
-- pdir.e
-- ======
--
-- The Phix dir function.
--
--/* DEV (from the mailing list, no changes attempted)
DerekParnell said...
kat said...

Is calling a .zip file a dir a random thing in Euphoria?

No. It only seems to happen for directory names that end with ".zip".

I believe this is actually a Windoze bug.

From http://msdn.microsoft.com/en-us/library/aa364428(v=vs.85).aspx
msdn said...

Note In rare cases or on a heavily loaded system, file attribute information on NTFS file systems 
may not be current at the time this function is called. To be assured of getting the current NTFS 
file system file attributes, call the GetFileInformationByHandle function.

Likewise, from http://msdn.microsoft.com/en-us/library/windows/desktop/aa364418(v=vs.85).aspx
msdn said...

If you are writing a 32-bit application to list all the files in a directory and the application 
may be run on a 64-bit computer, you should call the Wow64DisableWow64FsRedirectionfunction before 
calling FindFirstFile and call Wow64RevertWow64FsRedirection after the last call to FindNextFile. 
For more information, see File System Redirector.
--*/

--/**/-- not strictly necessary, but reduces opCallOnce/fwd calls/onDeclaration
--/**/include builtins\platform.e   -- platform()
include builtins\wildcard.e -- wildcard_file()

constant
    -- FILETIME structure:
--  FTdwLowDateTime     = 0,    --  DWORD dwLowDateTime
--  FTdwHighDateTime    = 4,    --  DWORD dwHighDateTime
--  FTsize              = 8,
    -- WIN32_FIND_DATA structure:
    FDwFileAttributes   = 0,    --  DWORD dwFileAttributes
--  FDtCreationTime     = 4,    --  FILETIME ftCreationTime
--  FDtLastAccessTime   = 12,   --  FILETIME ftLastAccessTime
    FDtLastWriteTime    = 20,   --  FILETIME ftLastWriteTime
    FDnFileSizeHigh     = 28,   --  DWORD    nFileSizeHigh
    FDnFileSizeLow      = 32,   --  DWORD    nFileSizeLow
--  FDwReserved0        = 36,   --  DWORD    dwReserved0
--  FDwReserved1        = 40,   --  DWORD    dwReserved1
    FDcFileName         = 44,   --  rb 260  ;[ MAX_PATH ]; TCHAR     cFileName[ MAX_PATH ]
--  FDcAltFileName      = 304,  --  rb 14   TCHAR    cAlternateFileName[ 14 ]
    FDsize              = 318,
    -- SYSTEMTIME structure:
    STwYear             = 0,    --  WORD wYear
    STwMonth            = 2,    --  WORD wMonth
--  STwDayOfWeek        = 4,    --  WORD wDayOfWeek
    STwDay              = 6,    --  WORD wDay
    STwHour             = 8,    --  WORD wHour
    STwMinute           = 10,   --  WORD wMinute
    STwSecond           = 12,   --  WORD wSecond
--  STwMillisecs        = 14,   --  WORD wMilliseconds
    STsize = 16,

--  INVALID_HANDLE_VALUE = -1
    INVALID_HANDLE_VALUE = #FFFFFFFF    -- 1/5/09

integer dinit dinit = 0
atom kernel32
atom xFindFirstFile, xFindNextFile, xFindClose,
     xFileTimeToLocalFileTime, xFileTimeToSystemTime
sequence attrbits
sequence attrchar

procedure initD()
--DEV locking as per pprntf
    kernel32 = open_dll("kernel32.dll")

--#without reformat
    xFindFirstFile = define_c_func(kernel32,"FindFirstFileA",
        {C_POINTER, --  LPCTSTR  lpFileName, // address of name of file to search for
         C_POINTER},--  LPWIN32_FIND_DATA  lpFindFileData   // address of returned information
        C_POINTER)  -- HANDLE for FindNextFile/FindClose

    xFindNextFile = define_c_func(kernel32,"FindNextFileA",
        {C_POINTER, --  HANDLE  hFindFile, // handle of search
         C_POINTER},--  LPWIN32_FIND_DATA lpFindFileData // address of structure for data on found file
        C_INT)      -- BOOL

    xFindClose = define_c_func(kernel32,"FindClose",
        {C_POINTER},--  HANDLE  hFindFile   // file search handle 
        C_INT)      -- BOOL

    xFileTimeToLocalFileTime = define_c_func(kernel32,"FileTimeToLocalFileTime",
        {C_POINTER, --  CONST FILETIME *  lpFileTime,   // address of UTC file time to convert
         C_POINTER},--  LPFILETIME  lpLocalFileTime     // address of converted file time
        C_INT)      -- BOOL

    xFileTimeToSystemTime = define_c_func(kernel32,"FileTimeToSystemTime",
        {C_POINTER, --  CONST FILETIME *  lpFileTime,   // pointer to file time to convert
         C_POINTER},--  LPSYSTEMTIME  lpSystemTime  // pointer to structure to receive system time
        C_INT)      -- BOOL
--#with reformat

    attrbits = {#01,#02,#04,#00,#10,#20}    -- no volume_id
    attrchar = {'r','h','s','v','d','a'}

    dinit = 1
end procedure

--DEV new builtin? [done for newEmit]
--DEV quick fix, needs properly sorting out:
--function peek2u(object addr)
function peek2uX(object addr)
sequence res
    if atom(addr) then
        return peek(addr)+peek(addr+1)*256
    end if
    res = repeat(0,addr[2])
    addr = addr[1]
    for i=1 to length(res) do
        res[i] = peek(addr)+peek(addr+1)*256
        addr += 2
    end for
    return res
end function

function ConvertAttributes(integer c)
--
-- Convert the bitmap of file attributes into a text string
--
sequence res
integer b
    res = ""
    for i=1 to length(attrbits) do
        b = attrbits[i]
        if and_bits(c,b) then
            res &= attrchar[i]
        end if
    end for
    return res
end function

--/* Not required for Phix (defined psym.e):
global constant 
    D_NAME = 1,
    D_ATTRIBUTES = 2,
    D_SIZE = 3,

    D_YEAR = 4,
    D_MONTH = 5,
    D_DAY = 6,

    D_HOUR = 7,
    D_MINUTE = 8,
    D_SECOND = 9
--*/

global function dir(sequence path)
--global function dir(sequence path,integer list_directory=1) --DEV: (just an idea, see below)
--
-- Returns a list of files that match the specified path.
-- If the path is a directory, eg "C:Windows" then details of all files in 
-- that directory are returned. A trailing slash is optional.
-- Relative paths may be specified, eg "builtins\\" will return a list of 
-- files in the builtins subdirectory of the current working directory.
-- If a specific file is used, eg "C:\\autoexec.bat" then details for just 
-- that file are returned.
-- The path may end with a wildcard, eg "test\\f*" returns a list of files in 
-- the current directory beginning with 'f'.
-- Either backslashes (which must be entered as \\ in strings, remember) or 
-- forward slashes (/) may be used as path separators.
--
-- If no such file or directory exists, -1 is returned.
--
-- Otherwise, the entry for each file is a sequence of 9 items:
--  name[1]         -- a long file name, eg "Quick_Allocations.ew"
--  attributes[2]   -- a string of characters from "drhsvda":
--                  -- 'r'ead-only
--                  -- 'h'idden
--                  -- 's'ystem
--                  -- 'v'olume-id [obsolete, I think]
--                  -- 'd'irectory
--                  -- 'a'rchive bit is set
--  size[3]         -- in bytes [accurate to ~9000 GB)
--  year[4]         -- eg 2004  }
--  month[5]        -- 1..12    } date file last written to
--  day[6]          -- 1..31    }
--  hour[7]         -- 0..23    }
--  minute[8]       -- 0..59    } time file last written to
--  second[9]       -- 0..59    }
--
-- Note that filenames may be in mixed case, so you may need 
-- to use upper() or lower() before comparing.
--
--DEV: Optional second parameter.
--DEV: If a second parameter of zero is passed, then dir() will NOT list the
--DEV: directory contents, but just return info for the directory itself.
--DEV: This may be useful, for example, if you just want to know whether or
--DEV: not a directory exists, and prefer not to get the complete list of 
--DEV: contents, which you'll throw away.
--
integer d, k, lp
integer slash, rslash
atom lpPath, h, size
sequence res, this, attr
object pattern
atom xSystemTime, xLocalFileTime,
     xFindData = allocate(FDsize)

    if not dinit then initD() end if
    if platform()=2 then    --Win32
        slash = '\\'
        rslash = '/'
    elsif platform()=3 then --Linux
        slash = '/'
        rslash = '\\'
    end if
    k = 1
    while 1 do
        --
        -- Force consistent use of '\\' vs '/'.
        --
        d = find(rslash,path)
        if d=0 then exit end if
        path[d] = slash
        k = d+1
    end while
    lp = length(path)
    for i=lp to k by -1 do
        if path[i]=slash then
            k = i+1
            exit
        end if
    end for
--DEV root=path[1..pattern-1]
    pattern = path[k..lp]
--    if length(path) and path[-1]=slash then
    if lp and path[lp]=slash then
        path &= "*.*"
        pattern = "*.*"
    elsif not find('*',path) and not find('?',path) then
        --
        -- Check if the passed path is a directory
        --
--DEV:  if list_directory then
        lpPath = allocate_string(path)
        h = c_func(xFindFirstFile,{lpPath,xFindData})
        if h!=INVALID_HANDLE_VALUE then
            if and_bits(peek(xFindData+FDwFileAttributes),#10) then
                path &= slash&"*.*"
                pattern = "*.*"
            end if
            if c_func(xFindClose,{h}) then end if
        end if
        free(lpPath)
--      end if
    end if
--DEV 
--"argument to dir must be string\n",   -- e68atcdmbs
    lpPath = allocate_string(path)
    h = c_func(xFindFirstFile,{lpPath,xFindData})
    free(lpPath)
    if h=INVALID_HANDLE_VALUE then
        free(xFindData)
        return -1
    end if
    res = {}
    xSystemTime = allocate(STsize)
    xLocalFileTime = allocate(8)
    while 1 do
        this = peek_string(xFindData+FDcFileName)
        if wildcard_file(pattern,this) then
            if c_func(xFileTimeToLocalFileTime,{xFindData+FDtLastWriteTime,xLocalFileTime}) then end if
            if c_func(xFileTimeToSystemTime,{xLocalFileTime,xSystemTime}) then end if
            size = peek4s(xFindData+FDnFileSizeHigh)*#100000000+peek4u(xFindData+FDnFileSizeLow)
            attr = ConvertAttributes(peek4u(xFindData+FDwFileAttributes))
--DEV root&this?
            res = append(res,{this,                                 -- D_NAME = 1,
                              attr,                                 -- D_ATTRIBUTES = 2,
                              size,                                 -- D_SIZE = 3,
                              peek2uX(xSystemTime+STwYear),         -- D_YEAR = 4,
                              peek2uX(xSystemTime+STwMonth),        -- D_MONTH = 5,
                              peek2uX(xSystemTime+STwDay),          -- D_DAY = 6,
                              peek2uX(xSystemTime+STwHour),         -- D_HOUR = 7,
                              peek2uX(xSystemTime+STwMinute),       -- D_MINUTE = 8,
                              peek2uX(xSystemTime+STwSecond)})      -- D_SECOND = 9
        end if
        if not c_func(xFindNextFile,{h,xFindData}) then exit end if
    end while
    if c_func(xFindClose,{h}) then end if
    free(xSystemTime)
    free(xLocalFileTime)
    free(xFindData)
    return res
end function

