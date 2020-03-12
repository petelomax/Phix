--
-- pdir.e
-- ======
--
-- The Phix dir function.
--

--/**/-- not strictly necessary, but reduces opCallOnce/fwd calls/onDeclaration
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

    INVALID_HANDLE_VALUE = -1,
--  INVALID_HANDLE_VALUE = #FFFFFFFF,   -- 1/5/09
    ERROR_FILE_NOT_FOUND = 2

integer dinit dinit = 0
atom kernel32
atom xFindFirstFile, xFindNextFile, xFindClose,
     xFileTimeToLocalFileTime, xFileTimeToSystemTime
sequence attrbits
sequence attrchar

--Linux:
constant W = machine_bits()=64

atom libc,
     xopendir,
     xreaddir,
     xclosedir,
     xlocaltime

--constant              --   64  32
--  DIRENT_INO      = iff(W?  0,  0), -- ino_t 
--  DIRENT_OFF      = iff(W?  8,  4), -- off_t 
--  DIRENT_RECLEN   = iff(W? 16,  8), -- unsigned short int 
--  DIRENT_TYPE     = iff(W? 18, 10), -- unsigned char 
--  DIRENT_NAME     = iff(W? 19, 11), -- char[256] 
--  DIRENT_TYPE     = 10, -- unsigned char 
--  DIRENT_NAME     = 11, -- char[256] 
--  SIZEOF_DIRENT   = iff(W?280,272) 
--$

-- now set in initD():
integer DIRENT_TYPE,
        DIRENT_NAME

constant DT_DIR = #04   -- (the only DIRENT_TYPE setting we care about)

-- NB: from experimentation, and not tested on 64 bit:
--                             64  32
--DEV I thought these would be resolved at compile-time...
--constant ST_SIZE    = iff(W? 48: 44),
--       ST_MTIME     = iff(W? 76: 72),
--       SIZEOFSTAT64 = iff(W?144:104)
constant ST_SIZE      = 44,
--       ST_ATIME     = 64,     -- (guess)
         ST_MTIME     = 72,
--       ST_CTIME     = 80,     -- (guess)
         SIZEOFSTAT64 = 104

--/*
struct stat {
    dev_t     st_dev;     /* ID of device containing file */
    ino_t     st_ino;     /* inode number */
    mode_t    st_mode;    /* protection */
    nlink_t   st_nlink;   /* number of hard links */
    uid_t     st_uid;     /* user ID of owner */
    gid_t     st_gid;     /* group ID of owner */
    dev_t     st_rdev;    /* device ID (if special file) */
    off_t     st_size;    /* total size, in bytes */
    blksize_t st_blksize; /* blocksize for file system I/O */
    blkcnt_t  st_blocks;  /* number of 512B blocks allocated */
    time_t    st_atime;   /* time of last access */
    time_t    st_mtime;   /* time of last modification */
    time_t    st_ctime;   /* time of last status change */
};
--*/

procedure initD()
    if platform()=WINDOWS then
--DEV locking as per pprntf
        -- added 25/11/16:
        enter_cs()
        kernel32 = open_dll("kernel32.dll")

    --#without reformat
        xFindFirstFile = define_c_func(kernel32,"FindFirstFileA",
            {C_PTR,     --  LPCTSTR  lpFileName, // address of name of file to search for
             C_PTR},    --  LPWIN32_FIND_DATA  lpFindFileData   // address of returned information
    --      C_PTR)      -- HANDLE for FindNextFile/FindClose
            C_INT)      -- HANDLE for FindNextFile/FindClose

        xFindNextFile = define_c_func(kernel32,"FindNextFileA",
            {C_PTR,     --  HANDLE  hFindFile, // handle of search
             C_PTR},    --  LPWIN32_FIND_DATA lpFindFileData // address of structure for data on found file
            C_INT)      -- BOOL

        xFindClose = define_c_func(kernel32,"FindClose",
            {C_PTR},    --  HANDLE  hFindFile   // file search handle 
            C_INT)      -- BOOL

        xFileTimeToLocalFileTime = define_c_func(kernel32,"FileTimeToLocalFileTime",
            {C_PTR,     --  CONST FILETIME *  lpFileTime,   // address of UTC file time to convert
             C_PTR},    --  LPFILETIME  lpLocalFileTime     // address of converted file time
            C_INT)      -- BOOL

        xFileTimeToSystemTime = define_c_func(kernel32,"FileTimeToSystemTime",
            {C_PTR,     --  CONST FILETIME *  lpFileTime,   // pointer to file time to convert
             C_PTR},    --  LPSYSTEMTIME  lpSystemTime  // pointer to structure to receive system time
            C_INT)      -- BOOL
    --#with reformat

        attrbits = {#01,#02,#04,#00,#10,#20}    -- no volume_id
        attrchar = {'r','h','s','v','d','a'}

        dinit = 1
        leave_cs()
    else
        libc = open_dll("libc.so.6")
        xopendir    = define_c_func(libc, "opendir", {C_PTR}, C_PTR)
        -- (not thread safe, may want to use readdir_r?)
        xreaddir    = define_c_func(libc, "readdir", {C_PTR}, C_PTR)
        xclosedir   = define_c_func(libc, "closedir", {C_PTR}, C_INT)
        -- (not thread safe, may want to use localtime_r?)
        xlocaltime  = define_c_func(libc, "localtime", {C_PTR}, C_PTR)

        DIRENT_TYPE  = iff(W? 18, 10)
        DIRENT_NAME  = iff(W? 19, 11)
--      ST_SIZE      = iff(W? 48: 44)
--      ST_MTIME     = iff(W? 76: 72)
--      SIZEOFSTAT64 = iff(W?144:104)

        dinit = 1
    end if
end procedure

--DEV new builtin? [done for newEmit]
--DEV quick fix, needs properly sorting out:
--function peek2u(object addr)
--function peek2uX(object addr)
--sequence res
--  if atom(addr) then
--      return peek(addr)+peek(addr+1)*256
--  end if
--  res = repeat(0,addr[2])
--  addr = addr[1]
--  for i=1 to length(res) do
--      res[i] = peek(addr)+peek(addr+1)*256
--      addr += 2
--  end for
--  return res
--end function

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

--/*
struct tm {
   int tm_sec;         /* seconds,  range 0 to 59          */
   int tm_min;         /* minutes, range 0 to 59           */
   int tm_hour;        /* hours, range 0 to 23             */
   int tm_mday;        /* day of the month, range 1 to 31  */
   int tm_mon;         /* month, range 0 to 11             */
   int tm_year;        /* The number of years since 1900   */
   int tm_wday;        /* day of the week, range 0 to 6    */
   int tm_yday;        /* day in the year, range 0 to 365  */
   int tm_isdst;       /* daylight saving time             */
};
--*/
constant
    TM_SECS = 0,
    TM_MINS = 4,
    TM_HOUR = 8,
    TM_DAY  = 12,
    TM_MNTH = 16,
    TM_YEAR = 20
--  TM_DOW  = 24,
--  TM_DOY  = 28

function stat(string name, atom pBuff)  -- (Linux ony)
--DEV this should probably use libc, but good enough for now
integer statres
    #ilASM{
        [ELF32]
            mov eax,[pBuff]
            call :%pLoadMint
            mov ebx,[name]
            mov ecx,eax         -- struct stat64 *statbuf
            shl ebx,2           -- char *filename
            mov eax,195         -- sys_stat64
            int 0x80
            xor ebx,ebx         -- (common requirement after int 0x80)
            mov [statres],1
            cmp eax, -4069 
            jbe @f
                mov [statres],ebx
          @@:
        [ELF64]
            mov rax,[pBuff]
            call :%pLoadMint
            mov rdi,[name]
            mov rsi,rax         -- struct stat *statbuf
            shl rdi,2           -- char *filename
            mov rax,4           -- sys_stat
            syscall
            mov [statres],1
            or rax,rax 
            jns @f
                mov [statres],rbx
          @@:
        []
          }
    return statres
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

--global function dir(sequence path)
global function dir(sequence path, integer date_type=D_MODIFICATION)
--global function dir(string path)
--global function dir(sequence path,integer list_directory=1) --DEV: (just an idea, see below)
--
-- Returns a list of files that match the specified path.
-- If the path is a directory, eg "C:Windows" then details of all files in 
-- that directory are returned. A trailing slash is optional.
-- Relative paths may be specified, eg `builtins\` will return a list of 
-- files in the builtins subdirectory of the current working directory.
-- If a specific file is used, eg `C:\autoexec.bat` then details for just 
-- that file are returned.
-- The path may end with a wildcard, eg `test\f*` returns a list of files in 
-- the current directory beginning with 'f'.
-- Either backslashes (which must be entered as \\ in double-qoute strings, remember) or 
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
atom xSystemTime, xLocalFileTime, xFindData

    if not dinit then initD() end if
    if platform()=WINDOWS then
        xFindData = allocate(FDsize)
        slash = '\\'
        rslash = '/'
    elsif platform()=LINUX then
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
    if platform()=WINDOWS then
        if lp and path[lp]=slash then
            path &= "*.*"
            pattern = "*.*"
        elsif not find('*',path) and not find('?',path) then
            --
            -- Check if the passed path is a directory
            --
--DEV:      if list_directory then
            lpPath = allocate_string(path)
--?path
--if path=`C:\Program Files\Phix\pw.exe` then
--  ?1
--end if
            h = c_func(xFindFirstFile,{lpPath,xFindData})
--?h
--21/11/16:
--          if h!=INVALID_HANDLE_VALUE then
            if h!=INVALID_HANDLE_VALUE
            and h!=ERROR_FILE_NOT_FOUND
            and h!=NULL then
                if and_bits(peek(xFindData+FDwFileAttributes),#10) then
                    path &= slash&"*.*"
                    pattern = "*.*"
                end if
                if c_func(xFindClose,{h}) then end if
            end if
            free(lpPath)
--          end if
        end if
--DEV 
--"argument to dir must be string\n",   -- e68atcdmbs
        lpPath = allocate_string(path)
        h = c_func(xFindFirstFile,{lpPath,xFindData})
--?{h}
        free(lpPath)
-- 21/11/16:
--      if h=INVALID_HANDLE_VALUE then
        if h=INVALID_HANDLE_VALUE
        or h=ERROR_FILE_NOT_FOUND
        or h=NULL then
            free(xFindData)
            return -1
        end if
        res = {}
        xSystemTime = allocate(STsize)
        xLocalFileTime = allocate(8)
        while 1 do
            this = peek_string(xFindData+FDcFileName)
            if wildcard_file(pattern,this) then
--              if c_func(xFileTimeToLocalFileTime,{xFindData+FDtLastWriteTime,xLocalFileTime}) then end if
                integer date_offset = FDtLastWriteTime+{-16,-8,0}[date_type]
                if c_func(xFileTimeToLocalFileTime,{xFindData+date_offset,xLocalFileTime}) then end if
                if c_func(xFileTimeToSystemTime,{xLocalFileTime,xSystemTime}) then end if
                size = peek4s(xFindData+FDnFileSizeHigh)*#100000000+peek4u(xFindData+FDnFileSizeLow)
                attr = ConvertAttributes(peek4u(xFindData+FDwFileAttributes))
--DEV root&this?
                res = append(res,{this,                             -- D_NAME = 1,
                                  attr,                             -- D_ATTRIBUTES = 2,
                                  size,                             -- D_SIZE = 3,
                                  peek2u(xSystemTime+STwYear),      -- D_YEAR = 4,
                                  peek2u(xSystemTime+STwMonth),     -- D_MONTH = 5,
                                  peek2u(xSystemTime+STwDay),       -- D_DAY = 6,
                                  peek2u(xSystemTime+STwHour),      -- D_HOUR = 7,
                                  peek2u(xSystemTime+STwMinute),    -- D_MINUTE = 8,
                                  peek2u(xSystemTime+STwSecond)})   -- D_SECOND = 9
            end if
            if not c_func(xFindNextFile,{h,xFindData}) then exit end if
        end while
        if c_func(xFindClose,{h}) then end if
        free(xSystemTime)
        free(xLocalFileTime)
        free(xFindData)
    elsif platform()=LINUX then

 
--function listdir(sequence path)
        atom dirp, dirent 
        atom pBuff = allocate(SIZEOFSTAT64)
        integer statres, year, month, day, hour, minutes, seconds
--, dow, doy
--      atom size
        string name
--      , attr
--      sequence res = {}
        res = {}
 
        dirp = c_func(xopendir, {path}) 
        if dirp=NULL then 
            statres = stat(path, pBuff)
            if statres=0 then
                free(pBuff)
                return -1
            end if
            name = get_file_name(path)
            attr = ""
        end if 
         
        while 1 do
            if dirp!=NULL then
                dirent = c_func(xreaddir,{dirp})
                if dirent=NULL then exit end if
                name = peek_string(dirent+DIRENT_NAME)
                string fullpath = join_path({path,name})
                statres = stat(fullpath, pBuff)
                integer t = peek(dirent+DIRENT_TYPE)
                attr = iff(t=DT_DIR?"d":"")
            end if
--DEV not tried...
--          if wildcard_file(pattern,name) then
                if statres=0 then
                    size = 0
                    seconds = 0
                    minutes = 0
                    hour = 0
                    day = 0
                    month = 0
                    year = 0
--          dow = 0
--          doy = 0
                else
                    -- Aside: this use on 32bit is noted in the docs of peek8s.
                    size    = peek8s(pBuff+ST_SIZE)
--                  atom pTime = c_func(xlocaltime,{pBuff+ST_MTIME})
                    integer date_offset = ST_MTIME+{+8,-8,0}[date_type]
                    atom pTime = c_func(xlocaltime,{pBuff+date_offset})
                    seconds = peek4s(pTime+TM_SECS)
                    minutes = peek4s(pTime+TM_MINS)
                    hour    = peek4s(pTime+TM_HOUR)
                    day     = peek4s(pTime+TM_DAY)
                    month   = peek4s(pTime+TM_MNTH)+1
                    year    = peek4s(pTime+TM_YEAR)+1900
--          dow     = peek4s(pTime+TM_DOW)
--          doy     = peek4s(pTime+TM_DOY)
                end if
                res = append(res,{name,attr,size,year,month,day,hour,minutes,seconds})
--          end if
--      printf( 1, "%s [%s] ", {name,attr} ) 
--      ?{size,year,month,day,hour,minutes,seconds}--,dow,doy}
--      dirent = readdir( dirp ) 
            if dirp=NULL then exit end if
--      statres = 0
        end while 
        if dirp!=NULL then
            {} = c_func(xclosedir,{dirp})
        end if
        free(pBuff)
--  return res
--end function 

--/*
        if lp and path[lp]=slash then
--          path &= "*.*"
            pattern = "*.*"
        else
            --DEV temp (for return type)
            return -1
        end if
--          if wildcard_file(pattern,this) then
        ?9/0    
--*/    
    end if
    return res
end function

