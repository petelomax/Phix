--
-- pfileio.e
-- =========
--
--  STATUS: EXPERIMENTAL (now replaced with builtins\VM\pfileioN.e)
--
--  A re-implementation of file I/O in hll, to compare performance with the
--  asm-coded routines.
--
--  The evidence so far indicates this hll is about 10x slower than asm, on
--  the critical getc/puts(,ch) routines...
--    (suspected culprits are c_func, peek, subscripts, in that order)
--DEV re-try with ilasm things...
-- it is not c_func...
--without debug
--
/*
This will not work on RDS Eu/OpenEuphoria!!
*/
constant
    INVALID_HANDLE_VALUE     = -1,
    STD_INPUT_HANDLE         = -10,
    STD_OUTPUT_HANDLE        = -11,
    STD_ERROR_HANDLE         = -12,

    C_PTR = C_POINTER,

    BUFFERSIZE = 8192
--  BUFFERSIZE = 8
--  BufferSize = 8192-9     -- leave space for length, type, and null terminator

object void

---- COORD structure (pDEST):
--constant
----    C_SIZEX = 0,
----    C_SIZEY = 2,
--  sizeof_COORD = 4

-- CONSOLE_SCREEN_BUFFER_INFO structure (pCSBI):
constant
    CSBI_SIZEX  = 0,    --  COORD      dwSize; 
    CSBI_SIZEY  = 2,
--  CSBI_CPOSX  = 4,    --  COORD      dwCursorPosition; 
--  CSBI_CPOSY  = 6,
    CSBI_ATTR   = 8,    --  WORD       wAttributes; 
--  CSBI_WINX1  = 10,   --  SMALL_RECT srWindow; 
--  CSBI_WINY1  = 12,
--  CSBI_WINX2  = 14,
--  CSBI_WINY2  = 16,
--  CSBI_MAXX   = 18,   --  COORD      dwMaximumWindowSize; 
--  CSBI_MAXY   = 20,
    sizeof_CSBI = 22

-- SMALL_RECT structure (pSMALLRECT):
constant 
--  SR_Left = 0,
--  SR_Top = 2,
--  SR_Right = 4,
--  SR_Bottom = 6,
    sizeof_SMALL_RECT = 8

-- CHAR_INFO structure (pCHARINFO):
constant 
--  CI_UnicodeChar = 0,     -- Unicode or ANSI character
--  CI_Attributes = 2,      -- text and background colors 
    sizeof_CHAR_INFO = 4


atom kernel32,
    xGetStdHandle,xCreateFile,
    xReadFile,
    xWriteFile,
    xSetFilePointer,
    xFlushFileBuffers,
    xGetFileSize,
    xCloseHandle,
    xAllocConsole,
    xSetConsoleMode,
--  xReadConsole,
    xWriteConsole,
    xGetConsoleMode,
--? xReadConsoleOutput,xWriteConsoleOutput,
    xGetConsoleScreenBufferInfo,
    xSetConsoleScreenBufferSize,
--? xSetConsoleTextAttribute,
--? xSetConsoleCursorPosition,
    xFillConsoleOutputCharacter,
    xFillConsoleOutputAttribute,
    xScrollConsoleScreenBuffer,
    xGetLastError,
--  xRect,xCSBI,
--? InBuf,
--DEV not thread-safe:
    pDword,         -- general purpose pointer to dword (bytes read/written/etc) [should be write-only]
    pCSBI,
    pSMALLRECT,
--  pDEST,
    pCHARINFO,
    character,
--  LF,
    CRLF,
    back3,
    pHigh

constant
    CREATE_NEW               = 1,
--  CREATE_ALWAYS            = 2,
    OPEN_EXISTING            = 3,
    OPEN_ALWAYS              = 4,
    TRUNCATE_EXISTING        = 5,
--  GENERIC_READ             = #80000000,   -- Too big for forward init!
--  GENERIC_WRITE            = #40000000,   -- ""
    FILE_SHARE_READ          = 1,
    FILE_SHARE_WRITE         = 2,
    FILE_ATTRIBUTE_NORMAL    = #80,
--  FILE_FLAG_RANDOM_ACCESS  = #10000000,
-- SetFilePointer methods
    FILE_BEGIN               = 0,
--  FILE_CURRENT             = 1,
    FILE_END                 = 2,

    ENABLE_PROCESSED_INPUT   = 1,
--  ENABLE_LINE_INPUT        = 2,
--  ENABLE_ECHO_INPUT        = 4,
--  ENABLE_WINDOW_INPUT      = 8,
--  ENABLE_MOUSE_INPUT       = #10,
    ENABLE_PROCESSED_OUTPUT  = 1,
    ENABLE_WRAP_AT_EOL_OUTPUT = 2

    --DEV (suppress unused warnings, should prolly be using these in h_puts)
    if ENABLE_PROCESSED_OUTPUT or ENABLE_WRAP_AT_EOL_OUTPUT then end if

atom GENERIC_READ,      -- = #80000000,   -- Too big for forward init!
     GENERIC_WRITE      -- = #40000000,   -- ""

-- bit settings for fmode:
constant F_CLOSED   = #00,  -- file is closed/available for re-use (see flist)
         F_READ     = #01,  -- file has read permission
         F_WRITE    = #02,  -- file has write permission
         F_BINARY   = #04,  -- binary mode
         F_DIRTY    = #08   -- write cache flag

--DEV not thread-safe * lots:
-- Scratch/local work vars:
atom --fhandle,
--   fmode,     -- 0=closed (free), #01 = read, #02 = write, #04 = binary, #08 = dirty
--   fposn,
--   fend,
     frealposn  -- actual file position. NB when F_DIRTY is set, this must exactly
                --  correspond to fbuffer[1].

--string fbuffer

-- Master tables of the above for all files we know about:
-- nb: file nos are 0-based, ie 0 (stdin) is kept in fxxx[1], 
--      1(stdout) in fxxx[2] etc. The first time open() is called 
--      (which may be from pdiag.e, pdebug.e, database.e, etc.)
--      it returns 3, which refers to fxxx[4].

sequence fhandles,  -- file handle, also used for freelist
         fmodes,    -- file mode
         fposns,    -- location / (1-based) idx to fbuffers where next byte is
         fends,     -- used part of fbuffers
         frealposns,-- actual file position, see notes below
         fbuffers   -- strings of length BUFFERSIZE

-- If fmode has the F_DIRTY bit set, then frealposn corresponds to the start
--  of fbuffer, otherwise it corresponds to the fend of the fbuffer.
-- A fposn of 1 and fend of 0 is the standard "empty" state.

integer flist   -- free list pointer for fhandles
        flist = 0

--;  +0  handle
--;  +4  mode    ; #00 = file closed (slot unused/available for re-use)
--;              ; #01 = read permission
--;              ; #02 = write permission
--;              ; #04 = binary mode
--;              ; #08 = write cache flag (aka 'dirty')
--;  +8  next
--;  +12 prev
--;  +16 fno     ; file number
--;  +20 fposn
--;  +24 fend
--;  +28 frealposn
--;  +32 buffer (8192-32 = 8160 bytes)


integer finit
        finit = 0

procedure initF()
    kernel32 = open_dll("kernel32.dll")
--#without reformat
    xGetStdHandle = define_c_func(kernel32,"GetStdHandle",
        {C_UINT},   --  DWORD  nStdHandle   // input, output, or error device
--DEV C_PTR?
        C_INT)      -- HANDLE
    xCreateFile = define_c_func(kernel32,"CreateFileA",
        {C_PTR,     --  LPCTSTR  lpFileName,    // address of name of the file
         C_LONG,    --  DWORD  dwDesiredAccess, // access (read-write) mode
         C_LONG,    --  DWORD  dwShareMode, // share mode
         C_PTR,     --  LPSECURITY_ATTRIBUTES  lpSecurityAttributes,    // address of security descriptor
         C_LONG,    --  DWORD  dwCreationDistribution,  // how to create
         C_LONG,    --  DWORD  dwFlagsAndAttributes,    // file attributes
         C_PTR},    --  HANDLE  hTemplateFile   // handle of file with attributes to copy
        C_INT)      -- HANDLE
    xReadFile   = define_c_func(kernel32,"ReadFile",
        {C_PTR,     --  HANDLE  hFile,  // handle of file to read
         C_PTR,     --  LPVOID  lpBuffer,   // address of buffer that receives data
         C_LONG,    --  DWORD  nNumberOfBytesToRead,    // number of bytes to read
         C_PTR,     --  LPDWORD  lpNumberOfBytesRead,   // address of number of bytes read
         C_PTR},    --  LPOVERLAPPED  lpOverlapped  // address of structure for data
        C_INT)      -- BOOL
    xWriteFile  = define_c_func(kernel32,"WriteFile",
        {C_PTR,     --  HANDLE  hFile,  // handle of file to write to
         C_PTR,     --  LPCVOID  lpBuffer,  // address of data to write to file
         C_LONG,    --  DWORD  nNumberOfBytesToWrite,   // number of bytes to write
         C_PTR,     --  LPDWORD  lpNumberOfBytesWritten,    // address of number of bytes written
         C_PTR},    --  LPOVERLAPPED  lpOverlapped  // addr. of structure needed for overlapped I/O
        C_INT)      -- BOOL
    pHigh = allocate(4)     -- for SetFilePointer's lpDistanceToMoveHigh
    xSetFilePointer = define_c_func(kernel32,"SetFilePointer",
        {C_PTR,     --  HANDLE  hFile,  // handle of file
         C_LONG,    --  LONG  lDistanceToMove,  // number of bytes to move file pointer
         C_PTR,     --  PLONG  lpDistanceToMoveHigh,    // address of high-order word of distance to move
         C_LONG},   --  DWORD  dwMoveMethod     // how to move
        C_LONG)     -- DWORD
    xFlushFileBuffers   = define_c_func(kernel32,"FlushFileBuffers",
        {C_PTR},    --  HANDLE  hFile   // open handle to file whose buffers are to be flushed
        C_INT)      -- BOOL
    xGetFileSize = define_c_func(kernel32,"GetFileSize",
        {C_PTR,     --  HANDLE  hFile,  // handle of file to get size of
         C_PTR},    --  LPDWORD  lpFileSizeHigh,    // address of high-order word for file size
        C_LONG)     -- DWORD
    xCloseHandle = define_c_func(kernel32,"CloseHandle",
        {C_PTR},    --  HANDLE  hObject     // handle of object to close
        C_INT)      -- BOOL
    xAllocConsole = define_c_func(kernel32,"AllocConsole",
        {},         --  no parameters
        C_INT)      -- BOOL
    xSetConsoleMode = define_c_func(kernel32,"SetConsoleMode",
        {C_PTR,     --  HANDLE  hConsole,   // handle of console input or screen buffer
         C_LONG},   --  DWORD  fdwMode      // input or output mode to set 
        C_INT)      -- BOOL
    xGetConsoleMode = define_c_func(kernel32,"GetConsoleMode",
        {C_PTR,     --  HANDLE  hConsole,   // handle of console input or screen buffer
         C_PTR},    --  LPDWORD  lpMode     // current mode flags 
        C_INT)      -- BOOL
--  xReadConsole = define_c_func(kernel32,"ReadConsoleA",
--      {C_PTR,     --  HANDLE  hConsoleInput,      // handle of a console input buffer
--       C_PTR,     --  LPVOID  lpvBuffer,  // address of buffer to receive data
--       C_LONG,    --  DWORD  cchToRead,   // number of characters to read
--       C_PTR,     --  LPDWORD  lpcchRead, // address of number of characters read
--       C_PTR},    --  LPVOID  lpvReserved         // reserved
--      C_INT)      -- BOOL
    xWriteConsole = define_c_func(kernel32,"WriteConsoleA",
        {C_PTR,     --  HANDLE  hConsoleOutput,     // handle of a console screen buffer 
         C_PTR,     --  CONST VOID  *lpvBuffer,     // address of buffer to write from 
         C_LONG,    --  DWORD  cchToWrite,  // number of characters to write 
         C_PTR,     --  LPDWORD  lpcchWritten,      // address of number of characters written 
         C_PTR},    --  LPVOID  lpvReserved     // reserved
        C_INT)      -- BOOL
    xGetConsoleScreenBufferInfo = define_c_func(kernel32,"GetConsoleScreenBufferInfo",
        {C_PTR,     --  HANDLE  hConsoleOutput, // handle of console screen buffer
         C_PTR},    --  PCONSOLE_SCREEN_BUFFER_INFO  // address of screen buffer info
        C_INT)      -- BOOL
    xSetConsoleScreenBufferSize = define_c_func(kernel32,"GetConsoleScreenBufferInfo",
        {C_PTR,     --  HANDLE  hConsoleOutput, // handle of console screen buffer
         C_INT},    --  COORD  coordSize    // new size in character rows and cols
        C_INT)      -- BOOL
    xScrollConsoleScreenBuffer = define_c_func(kernel32,"ScrollConsoleScreenBufferA",
        {C_PTR,     --  HANDLE  hConsoleOutput, // handle of console screen buffer
         C_PTR,     --  PSMALL_RECT  psrctSourceRect, // address of screen buffer rect. to move
         C_PTR,     --  PSMALL_RECT  psrctClipRect, // address of affected screen buffer rect. 
         C_INT,     --  COORD  coordDestOrigin, // new location of screen buffer rect.
         C_PTR},    --  PCHAR_INFO  pchiFill    // address of fill character and color 
        C_INT)      -- BOOL
    xFillConsoleOutputCharacter = define_c_func(kernel32,"FillConsoleOutputCharacterA",
        {C_PTR,     --  HANDLE  hConsoleOutput, // handle of screen buffer
         C_USHORT,  --  TCHAR  cCharacter,  // character to write
         C_UINT,    --  DWORD  nLength, // number of character cells to write to
         C_UINT,    --  COORD  dwWriteCoord,    // x- and y-coordinates of first cell
         C_PTR},    --  LPDWORD  lpNumberOfCharsWritten     // address of number of cells written to
        C_INT)      -- BOOL
    xFillConsoleOutputAttribute = define_c_func(kernel32,"FillConsoleOutputAttribute",
        {C_PTR,     --  HANDLE  hConsoleOutput, // handle of screen buffer
         C_USHORT,  --  WORD  wAttribute,   // color attribute to write
         C_UINT,    --  DWORD  nLength, // number of character cells to write to
         C_UINT,    --  COORD  dwWriteCoord,    // x- and y-coordinates of first cell
         C_PTR},    --  LPDWORD  lpNumberOfAttrsWritten     // address of number of cells written to
        C_INT)      -- BOOL

    xGetLastError = define_c_func(kernel32, "GetLastError",
        {},
        C_INT)      -- DWORD
--#without reformat
    --
    -- These must be initialised inside initF() because they are stored
    -- as floats, ... and only short literal integers are auto-initialised
    -- in forward referenced routines!
    --
    GENERIC_READ             = #80000000    -- Too big for forward init!
    GENERIC_WRITE            = #40000000    -- ""
--  GENERIC_READ             = #80000000    -- Too big for forward init! (mapped as #01 internally)
--  GENERIC_WRITE            = #40000000    -- ""                        (mapped as #04 internally)

                --  read            write             append                    update
--? createmodes = {OPEN_EXISTING,TRUNCATE_EXISTING,OPEN_ALWAYS,               OPEN_ALWAYS}
--? accessmodes = {GENERIC_READ, GENERIC_WRITE,    GENERIC_READ+GENERIC_WRITE,GENERIC_WRITE}

--? fhandles = {-1,-1,-1, -- stdin,stdout,stderr (see initC())
--           5,6,7,8,0} -- initial free list values
--? flist = 4
    fhandles = {-1,-1,-1}   -- slots reserved for stdin/stdout/stderr (see initConsole())
    fmodes = {0,0,0}
    fposns = {0,0,0}
    fends = {0,0,0}
    frealposns = {0,0,0}
--DEV I don't /think/ we want any buffers here...
    fbuffers = {0,0,0}--(fbuffers,repeat(' ',BUFFERSIZE))

--DEV (untried)
--  stdin is buffered (in case it is redirected), but stout & stderr are not:
--  fbuffers = {repeat(' ',BUFFERSIZE),0,0}

--? InBuf = allocate(4)
    pDword = allocate(4)
    character = allocate(1)
--  LF = allocate(1)
    CRLF = allocate(2)
    poke(CRLF,"\r\n")
--  LF = CRLF+1
    back3 = allocate(3)
    poke(back3,{8,' ',8})

    finit = 1
end procedure
--procedure initF()
----#without reformat
--  xReadConsoleOutput = define_c_func(kernel32,"ReadConsoleOutputA",
--      {C_PTR,     --  HANDLE  hConsoleOutput, // handle of a console screen buffer
--       C_PTR,     --  PCHAR_INFO  pchiDestBuffer, // address of buffer that receives data
--       C_UINT,    --  COORD  coordDestBufferSize, // column-row size of destination buffer
--       C_UINT,    --  COORD  coordDestBufferCoord, // upper-left cell to write to
--       C_PTR},    --  PSMALL_RECT  psrctSourceRect // address of rectangle to read from
--      C_INT)      -- BOOL
--
--  xWriteConsoleOutput = define_c_func(kernel32,"WriteConsoleOutputA",
--      {C_PTR,     --  HANDLE  hConsoleOutput, // handle of a console screen buffer
--       C_PTR,     --  PCHAR_INFO  pchiSrcBuffer,  // address of buffer with data to write
--       C_UINT,    --  COORD  coordSrcBufferSize,  // column-row size of source buffer
--       C_UINT,    --  COORD  coordSrcBufferCoord, // upper-left cell to write from
--       C_PTR},    --  PSMALL_RECT  psrctDestRect  // address of rectangle to write to
--      C_INT)      -- BOOL
--
--  xSetConsoleTextAttribute = define_c_func(kernel32,"SetConsoleTextAttribute",
--      {C_PTR,     --  HANDLE  hConsoleOutput, // handle of console screen buffer
--       C_USHORT}, --  WORD  wAttr     // text and background colors
--      C_INT)      -- BOOL
--
--  xSetConsoleCursorPosition = define_c_func(kernel32,"SetConsoleCursorPosition",
--      {C_PTR,     --  HANDLE  hConsoleOutput, // handle of console screen buffer
--       C_UINT},   --  COORD  coordCursor  // new cursor position coordinates
--      C_INT)      -- BOOL
--
--
--
----#without reformat
--
--  InBuf = allocate(BUFFERSIZE)
--
--  finit=1
--end procedure

global function h_open(sequence filepath, object openmode)
--
-- Open a file or device, to get the file number. -1 is returned if the open fails.
-- single character modes are used for text handling: "r"ead, "w"rite, "u"pdate, "a"ppend.
-- Output to text files will have carriage-return characters (\r) automatically added before
-- linefeed characters (\n). On input, these carriage-return characters are removed.
-- A control-Z character (ASCII 26) will signal an immediate end of file. [DEV]
-- Binary mode uses the same one-character mode as above plus a "b", ie "rb","wb","ub","ab".
--
-- Note: open(xxx,'a') is perfectly valid and the same as open(xxx,"a"), but on Phix only.
--
integer res, imode, fmode
atom accessmode, createmode, sharemode
atom fhandle
    if not finit then initF() end if
    fmode = 0                                   -- assume text mode
    if sequence(openmode) then                  -- allow open(<file>,'r') as well as open(<file>,"r")
        if length(openmode)=2 then
--          if find(openmode[2],"bB") then      -- if binary mode
            if openmode[2]='b' then             -- if binary mode
                fmode = F_BINARY                -- set flag and
                openmode = openmode[1]          -- make openmode a char
             end if
        elsif length(openmode)=1 then
            openmode = openmode[1]              -- make openmode a char
--      else invalid length, triggers invalid open mode next
        end if
    end if

    if flist=0 then
        fhandles = append(fhandles,0)
        fmodes = append(fmodes,0)
--      fposns = append(fposns,0)
        fposns = append(fposns,1)
        fends = append(fends,0)
        frealposns = append(frealposns,0)
        fbuffers = append(fbuffers,repeat(' ',BUFFERSIZE))
        flist = length(fhandles)
    end if

    if openmode='r' then        -- read
        accessmode = GENERIC_READ
        sharemode = FILE_SHARE_READ
        createmode = OPEN_EXISTING
        imode = F_READ
    elsif openmode='w' then     -- write
        accessmode = GENERIC_WRITE
        sharemode = FILE_SHARE_WRITE
        createmode = TRUNCATE_EXISTING
        imode = F_WRITE
    elsif openmode='u' then     -- update
        accessmode = GENERIC_READ+GENERIC_WRITE
        sharemode = FILE_SHARE_READ+FILE_SHARE_WRITE
        createmode = OPEN_EXISTING
        imode = F_READ+F_WRITE
    elsif openmode='a' then     -- append
        accessmode = GENERIC_WRITE
        sharemode = FILE_SHARE_WRITE
        createmode = OPEN_ALWAYS
        imode = F_WRITE
    else
--      iofatal(61,"invalid open mode\n")
--/**/  #ilASM{ mov al,61
--/**/          xor edi,edi     -- ep1 unused
--/**/          xor esi,esi     -- ep2 unused
--!/**/         call %opRTErn}  -- fatal error
--/**/          int3}           -- fatal error
        ?9/0
    end if

    --
    -- If opening for write and the file does not exist, TRUNCATE_EXISTING will
    -- fail, so in that one case retry with CREATE_NEW.
    --
    while 1 do  -- (max 2 iterations)
        fhandle = c_func(xCreateFile,{filepath,accessmode,sharemode,0,createmode,FILE_ATTRIBUTE_NORMAL,0})

        if fhandle!=INVALID_HANDLE_VALUE then exit end if                   -- success!

        -- loop once if 'w'/TRUNCATE_EXISTING, retry as CREATE_NEW:
        if createmode!=TRUNCATE_EXISTING then return -1 end if              -- failure!
        createmode = CREATE_NEW
    end while

    res = flist
    flist = fhandles[flist]
    if fmodes[res]!=0 then ?9/0 end if

    fhandles[res] = fhandle
    fmodes[res] = fmode+imode
    fposns[res] = 1
    fends[res] = 0
    frealposns[res] = 0

    if openmode='a' then
        frealposn = c_func(xSetFilePointer, {fhandle, 0, NULL, FILE_END})
        if frealposn=-1 then
--          iofatal(64,"seek fail on append/write open\n")
--/**/      #ilASM{ mov al,64
--/**/              xor edi,edi     -- ep1 unused
--/**/              xor esi,esi     -- ep2 unused
--!/**/             call %opRTErn}  -- fatal error
--/**/              int3}           -- fatal error
            ?9/0
        end if
        frealposns[res] = frealposn
    end if
    res -= 1
    return res
end function

atom stdin, stdout, stderr

integer cinit = 0
integer stdin_redirected = 0

procedure initConsole()
    if not finit then initF() end if
    void = c_func(xAllocConsole,{})
    stdin = c_func(xGetStdHandle,{STD_INPUT_HANDLE})
    stdout = c_func(xGetStdHandle,{STD_OUTPUT_HANDLE})
    stderr = c_func(xGetStdHandle,{STD_ERROR_HANDLE})
    -- nb following is not ENABLE_LINE_INPUT and not ENABLE_ECHO_INPUT
    if not c_func(xSetConsoleMode,{stdin,ENABLE_PROCESSED_INPUT}) then
-- triggers when redirected, ignore...
--      ? c_func(xGetLastError,{})
--      ?9/0
        stdin_redirected = 1
    end if
    fhandles[1..3] = {stdin,stdout,stderr}
    fmodes[1..3] = {F_READ,F_WRITE+F_BINARY,F_WRITE+F_BINARY}
--  xRect = allocate(8)
    pCSBI = allocate(sizeof_CSBI)
    pSMALLRECT = allocate(sizeof_SMALL_RECT)
--  pDEST = allocate(sizeof_COORD)
    pCHARINFO = allocate(sizeof_CHAR_INFO)
--  -- set initial foreground and background colours
--  getConsoleScreenBufferInfo()
--  fg_colour = peek2u(xCSBI+CSBI_ATTR)
--  bg_colour = and_bits(fg_colour,#F0)/#10
--  fg_colour = and_bits(fg_colour,#F)
    cinit = 1
end procedure

procedure flushfidx(integer fidx, integer fmode, atom fhandle)
--
-- Internal routine. Specifically this must not call xFlushFileBuffers;
--  the first working version called h_flush(fn) from eg puts(), and as
--  a consequence was literally hundreds of times slower.
-- fidx(>=4), fhandle, and fmode (with F_DIRTY bit) must be set by callee.
--
--  fmode -= F_DIRTY
string fbuffer
integer fend
    fmodes[fidx] = fmode        -- clear cached writes flag
    fend = fends[fidx]
--printf(1,"flushfidx called, fend=%d\n",fend)
--DEV this test may not be necessary?
--   (only puts() sets F_DIRTY, but puts(fn,{}) or puts(fn,"") may leave
--    fend zero, whereas possibly/arguably they should not... )
    if fend then
        frealposn = frealposns[fidx]
        frealposn += fend
        frealposns[fidx] = frealposn
        fbuffer = fbuffers[fidx]
--pp(fbuffer[1..fend])
--pp(fbuffer[1..fend])
--pp(fbuffer[fend])
--?9/0
        if not c_func(xWriteFile,{fhandle,fbuffer,fend,pDword,0}) then
--          iofatal(98,"flush error\n")
--/**/      #ilASM{ mov al,98
--/**/              xor edi,edi     -- ep1 unused
--/**/              xor esi,esi     -- ep2 unused
--!/**/             call %opRTErn}  -- fatal error
--/**/              int3}           -- fatal error
            ?9/0
        end if
    end if
end procedure

global procedure h_flush(integer fn)
-- write any output still in a buffer to disk.
-- this is performed automatically by close.
-- flush(1) may sometimes be needed (eg when i/o is redirected) before 
-- a getc or gets(0), to force display before input.
-- flush(-1) flushes all open files (but not the console).
integer fidx
integer fmode
atom fhandle
    if not finit then initF() end if
    if fn=-1 then
        for i=4 to length(fhandles) do  -- files 3 and up really
            fmode = fmodes[i]
            if and_bits(fmode,F_DIRTY) then
                fhandle = fhandles[i]
                flushfidx(i, fmode-F_DIRTY, fhandle)
            end if
        end for
        return
    end if
    fidx = fn+1
    fhandle = fhandles[fidx]
    if fn>2 then
        fmode = fmodes[fidx]
        if fmode=0 then
--          iofatal(62,"file number %d is not open\n",fn)
--/**/      #ilASM{ mov al,62
--/**/              mov edi,[fn]
--/**/              xor esi,esi     -- ep2 unused
--!/**/             call %opRTErn}  -- fatal error
--/**/              int3}           -- fatal error
            ?9/0
        elsif and_bits(fmode,F_DIRTY) then
            flushfidx(fidx, fmode-F_DIRTY, fhandle)
        end if
    else
        if not cinit then initConsole() end if
    end if
    if c_func(xFlushFileBuffers,{fhandle}) then end if
end procedure


global procedure h_close(integer fn)
--
-- Close a file or device and flush out any still-buffered characters. 
-- It is not particularly an error to close the same file twice, the
-- second and subsequent calls will have no effect.
-- close(-1) closes all open files. Be warned however that if an open
-- returns -1 you must avoid calling close(-1) unless this is wanted.
--DEV should I not therefore make it close(-2) and have close(-1) do nowt?
--
integer fidx
integer fmode
atom fhandle
    if not finit then initF() end if
    if fn=-1 then
        for i=4 to length(fhandles) do  -- files 3 and up really
            if fmodes[i] then
                h_close(i-1)
            end if
        end for
    elsif fn>=3 then
        fidx = fn+1
--DEV error message??
        if fidx<=length(fhandles) then
            fhandle = fhandles[fidx]
            fmode = fmodes[fidx]
            if fmode then
                if and_bits(fmode,F_DIRTY) then
                    flushfidx(fidx, F_CLOSED, fhandle)
                end if
                fmodes[fidx] = F_CLOSED             -- clear mode, mark as closed
                if c_func(xCloseHandle,{fhandle}) then end if
                -- and add to freelist
                fhandles[fidx] = flist
                flist = fidx
            end if
        end if
    end if
end procedure
--DEV set callback here... (for opClosem1)

procedure iofatal58(integer fn)
--"invalid file number (%d)\n"
--/**/  #ilASM{ mov al,58
--/**/          mov edi,[fn]
--/**/          xor esi,esi     -- ep2 unused
--!/**/         call %opRTErn}  -- fatal error
--/**/          int3}           -- fatal error
        ?9/0
end procedure

-- seek
global function h_seek(integer fn,atom pos)
--
-- returns 0 if the seek was successful, 1 if it was not.
-- eg/ie (**NB**):
--      if seek(fn,pos) then
--          -- seek failed!
--          ?9/0
--      else
--          -- seek ok!
--          ...
--      end if
-- In particular the instinctive "if not seek()" is usually wrong,
-- therfore perhaps better is something like:
--  constant SEEK_OK = 0
--      ...
--      if seek(fn,pos)!=SEEK_OK then ?9/0 end if
--
integer r
atom hipos
integer fidx
integer fmode
integer fend
atom fhandle
integer fposn

    if not finit then initF() end if
    fidx = fn+1
    if fn<=2
    or fidx>length(fhandles)
    or fmodes[fidx]=0 then
        iofatal58(fn)--"invalid file number (%d)\n"
    end if
    fmode = fmodes[fidx]
    fhandle = fhandles[fidx]
    if pos=-1 then
        if and_bits(fmode,F_DIRTY) then
            flushfidx(fidx, fmode-F_DIRTY, fhandle)
        end if
        r = c_func(xSetFilePointer,{fhandle,0,NULL,FILE_END})
        if r=-1 then return 1 end if        -- failure
        fends[fidx] = 0
        fposns[fidx] = 1
        frealposns[fidx] = r
        return 0                            -- success
    end if

    frealposn = frealposns[fidx]
    fend = fends[fidx]
    if and_bits(fmode,F_DIRTY) then
        -- realposn corresponds to the \\start\\ of the buffer,
        -- and we can extend/write beyond fend upto BUFFERSIZE.
        fposn = pos-frealposn+1
        if fposn>=1 and fposn<=BUFFERSIZE and fposn<=fend+1 then
            fposns[fidx] = fposn
            return 0                            -- success
        end if
        flushfidx(fidx, fmode-F_DIRTY, fhandle)
    else
        -- realposn corresponds to the \\end\\ of the buffer, but
        -- in this case fend is the hard limit of readable bytes.
        fposn = pos-(frealposn-fend)+1
        if fposn>=1 and fposn<=fend then
            fposns[fidx] = fposn
            return 0                            -- success
        end if
    end if
--DEV #100000000 surely!
    hipos = floor(pos/#10000000)
    pos = remainder(pos,#10000000)
    if hipos then
--DEV pHigh is not thread-safe here:
        poke4(pHigh,hipos)
        r = c_func(xSetFilePointer,{fhandle,pos,pHigh,FILE_BEGIN})
    else
        r = c_func(xSetFilePointer,{fhandle,pos,NULL,FILE_BEGIN})
    end if
    if r=-1 then return 1 end if            -- failure
    fends[fidx] = 0
    fposns[fidx] = 1
    frealposns[fidx] = r
    return 0                                -- success
end function

global function h_where(integer fn)
integer fidx
integer fmode
integer fend
integer fposn

    if not finit then initF() end if
    if fn<=2 or fn>=length(fhandles) then
        iofatal58(fn)--"invalid file number (%d)\n"
    end if
    fidx = fn+1
    frealposn = frealposns[fidx]
    fposn = fposns[fidx]
    fposn += frealposn-1
    fmode = fmodes[fidx]
    if not and_bits(fmode,F_DIRTY) then
        fend = fends[fidx]
        fposn -= fend
    end if
    return fposn
end function


procedure iofatal59()
--      iofatal59()--"wrong file mode for attempted operation\n")
--/**/  #ilASM{ mov al,59
--/**/          xor edi,edi     -- ep1 unused
--/**/          xor esi,esi     -- ep2 unused
--!/**/         call %opRTErn}  -- fatal error
--/**/          int3}           -- fatal error
        ?9/0
end procedure

global function h_getc(integer fn)
--
-- Get the next character (byte) from file or device fn.
-- The character will have a value from 0 to 255. -1 is returned at end of file.
--
integer ch
integer fidx
integer fmode
string fbuffer
integer fend
atom fhandle
integer fposn

    if not finit then initF() end if
    fidx = fn+1
    if fidx<1 or fidx>length(fmodes) then
        iofatal58(fn)--"invalid file number (%d)\n"
    end if
    if fn>2 then
        fmode = fmodes[fidx]
        if fmode=0 then
            iofatal58(fn)--"invalid file number (%d)\n"
        end if
        if not and_bits(fmode,F_READ) then
            iofatal59()--"wrong file mode for attempted operation\n")
        end if
        fbuffer = fbuffers[fidx]
        fend = fends[fidx]
        fposn = fposns[fidx]
        while 1 do
--          if fposn>=fend then
            if fposn>fend then
                fhandle = fhandles[fidx]
                if and_bits(fmode,F_DIRTY) then
                    flushfidx(fidx, fmode-F_DIRTY, fhandle)
                end if
--DEV pDword is not thread-safe here:
                if not c_func(xReadFile,{fhandle,fbuffer,BUFFERSIZE,pDword,0}) then return -1 end if
                if peek4s(pDword)=0 then return -1 end if
                fend = peek4u(pDword)   -- no of bytes read
                fends[fidx] = fend
                if fend=0 then return -1 end if
                frealposn = frealposns[fidx]
                frealposn += fend
                fposn = 1
--              fbuffers[fidx] = fbuffer    -- no matter
                fposns[fidx] = fposn
                frealposns[fidx] = frealposn
            end if
            ch = fbuffer[fposn]
            fposn += 1
            if ch!='\r' then exit end if                -- skip cr...
            if and_bits(fmode,F_BINARY) then exit end if -- ...when in text (not binary) mode
        end while
        fposns[fidx] = fposn
    elsif fn!=0 then
        -- gets() on stout/stderr invalid
        iofatal59()--"wrong file mode for attempted operation\n")
    else -- fn=0
        if not cinit then initConsole() end if
--      fmode = fmodes[fidx]
-- call clear_debug...
--      #ilASM{call %opClrDbg}

--      -- nb following is not ENABLE_LINE_INPUT and not ENABLE_ECHO_INPUT
--      if not c_func(xSetConsoleMode,{stdin,ENABLE_PROCESSED_INPUT}) then
--          ?9/0
--      end if

--      while 1 do
--off:
--ENABLE_LINE_INPUT (2)
--ENABLE_ECHO_INPUT (4)
--on:
--ENABLE_PROCESSED_INPUT (1)
--ENABLE_PROCESSED_OUTPUT (1)
--ENABLE_WRAP_AT_EOL_OUTPUT (2) -- wrap?
--  ENABLE_PROCESSED_INPUT   = 1,
--  ENABLE_LINE_INPUT        = 2,
--  ENABLE_ECHO_INPUT        = 4,
----    ENABLE_WINDOW_INPUT      = 8,
----    ENABLE_MOUSE_INPUT       = #10,
--  ENABLE_PROCESSED_OUTPUT  = 1,
--  ENABLE_WRAP_AT_EOL_OUTPUT = 2


--DEV pDword is not thread-safe here:
--DEV character is not thread-safe here:
            if not c_func(xReadFile,{stdin,character,1,pDword,0}) then return -1 end if
--          if not c_func(xReadConsole,{stdin,character,1,pDword,0}) then return -1 end if
--=
--;     invoke ReadConsole, [stdin], character, 1, pDword, 0
--;---
--  invoke ReadConsoleInput,[stdin],lpBuffer,1,lpEventsRead 
--  cmp [lpBuffer.EventType],0x0001 ;KEY_EVENT 
--  jne opGetcLoopCR
--  cmp [lpBuffer.keyDown],0x00000001 ;ignore key up events
--  jne opGetcLoopCR
--  xor eax,eax
--  mov al,[lpBuffer.keyChar]
--=
--DEV as per gets....
            if peek4s(pDword)=0 then return -1 end if
            ch = peek(character)
--DEV 0 & 9 loop, 26 -> -1
--          if ch!='\r' then exit end if                -- skip cr...
--          -- (file 0 is always text mode)
--      end while
    end if
    return ch
end function

--integer lastch = 0
--integer bufmax    -- stdin buffering

global function h_gets(integer fn)
--
-- Get the next sequence (one line, including '\n') of characters from file or device fn.
-- The characters will have values from 0 to 255. The atom -1 is returned on end of file.   
--
integer ch
string r
integer bytecount
integer fidx
integer fmode
string fbuffer
integer res
integer fend
atom fhandle
integer fposn

    if not finit then initF() end if
    fidx = fn+1
    if fn<=2 then
        if fn!=0 then
            -- gets() on stout/stderr invalid
            iofatal59()--"wrong file mode for attempted operation\n")
        end if
        if not cinit then initConsole() end if
--      call clear_debug
--      #ilASM{call %opClrDbg}

--      if not c_func(xSetConsoleMode,{stdin,ENABLE_PROCESSED_INPUT+ENABLE_LINE_INPUT+ENABLE_ECHO_INPUT}) then
--      if not c_func(xSetConsoleMode,{stdin,ENABLE_PROCESSED_INPUT}) then
----        if not c_func(xSetConsoleMode,{stdin,ENABLE_LINE_INPUT+ENABLE_ECHO_INPUT}) then
--          ? c_func(xGetLastError,{})
--          ?9/0
--      end if

        r = ""
        while 1 do
--    opGetsLoopCR:
--  ;       invoke ReadConsole, [stdin], character, 1, pDword, 0
--          invoke ReadConsoleInput,[stdin],lpBuffer,1,lpEventsRead 
--              treat leftarrow as backspace(8), display 8,32,8
--              ignore tab, 
--              if ctrl ctrlZ then if length(res)=0 then return -1 else return res
--              13->10
--              res &= ch
--              echo ch (but not 10):
--          invoke WriteConsole, [stdout], lpBuffer.keyChar, 1, pDword, 0
--DEV make this a bigger buffer???!!!!
--DEV pDword is not thread-safe here:
--DEV character is not thread-safe here:
            if not c_func(xReadFile,{stdin,character,1,pDword,0}) then
--          if not c_func(xReadConsole,{stdin,character,1,pDword,0}) then
--              ? c_func(xGetLastError,{})
                bytecount = 0
            else
                bytecount = peek4u(pDword)  -- no of bytes read
            end if
--?bytecount
            if bytecount=0 then
--              ch = -1
                ch = 26
            else
--              bufmax = bytecount
                ch = peek(character)
            end if
--          if ch!='\n' or lastch!='\r' then
--              lastch = ch
--          end if
            if ch='\r' then         -- CR
--DEV test added 25/2/2013: (assume there is a \n to be eaten)
if stdin_redirected then
                if not c_func(xReadFile,{stdin,character,1,pDword,0}) then end if
end if
                exit
            elsif ch=26 then        -- Ctrl Z
                if length(r)=0 then
                    return -1
                else
                    return r
                end if
            elsif ch=8 then         -- backspace
                if length(r) then
                    r = r[1..-2]
                    if not stdin_redirected then
--?"aa\n"
                        if c_func(xWriteConsole,{stdout,back3,3,pDword,0}) then end if
                    end if
                end if
            else
--          elsif ch!='\n' or lastch!='\r' then
                r &= ch
                if not stdin_redirected then
--?"bb\n"
                    if c_func(xWriteConsole,{stdout,character,1,pDword,0}) then end if
                end if
            end if
--          lastch = ch
        end while
        r &= '\n'
        return r
    end if

    -- fn>2:

    fmode = 0
    if fidx<=length(fmodes) then
        fmode = fmodes[fidx]
    end if
    if fmode=0 then
        iofatal58(fn)--"invalid file number (%d)\n"
    end if
    if not and_bits(fmode,F_READ) then
        iofatal59()--"wrong file mode for attempted operation\n")
    end if
    r = ""
    fbuffer = fbuffers[fidx]
    fend = fends[fidx]
    fposn = fposns[fidx]
    while 1 do
--      if fposn>=fend then
        if fposn>fend then
            fhandle = fhandles[fidx]
            if and_bits(fmode,F_DIRTY) then
                flushfidx(fidx, fmode-F_DIRTY, fhandle)
            end if
--DEV pDword is not thread-safe here:
--          if not c_func(xReadFile,{fhandle,fbuffer,BUFFERSIZE,pDword,0}) then return -1 end if
if 1 then
            if not c_func(xReadFile,{fhandle,fbuffer,BUFFERSIZE,pDword,0}) then exit end if
            fend = peek4u(pDword)   -- no of bytes read
else
            #ilASM{ lea edx,[fend]
                    push ebx                            -- lpOverlapped (NULL)
                    push edx                            -- lpbyesread (==addr fend)
                    push #2000      -- bytes to read (BUFFERSIZE, #2000 = 8192)
                    mov ecx,[fbuffer]
                    mov eax,[fhandle]
                    shl ecx,2   -- (convert ref to raw ptr)
                    cmp eax,h4
                    jl @f
                        -- values>#3FFFFFFF stored as float:
                        push ebx                        --    create space
                        fld qword[ebx+eax*4]
                        fistp dword[esp]
                        pop eax
                @@:
                    push ecx                            -- fbuffer
                    push eax                            -- fhandle
                    call "kernel32.dll","ReadFile"
                    mov [res],eax}
--          if res=0 or fend=0 then return -1 end if
            if res=0 then exit end if   -- DEV??
end if
            fends[fidx] = fend
--DEV???
--          if fend=0 then return -1 end if
--          if fend=0 then exit end if
            if fend=0 then 
                if length(r)=0 then return -1 end if
                exit 
            end if
            frealposn = frealposns[fidx]
            frealposn += fend
            fposn = 1
--          fbuffers[fidx] = fbuffer    -- no matter
            fposns[fidx] = fposn
            frealposns[fidx] = frealposn
        end if
--erm... (from here to end while is accounting for 98% of execution time!)
        ch = fbuffer[fposn]
        fposn += 1
        if ch=-1 then   -- or 26??
            if length(r)=0 then return -1 end if
            exit
        end if
--DEV in non-binary mode, terminate loop and skip \r\n pair??
        if ch!='\r' or and_bits(fmode,F_BINARY) then
            r &= ch
        end if
        if ch='\n' then exit end if
--/erm
--      opLoadmem,%esi,fbuffer,
--      shl_esi_imm8,2,
--      ecx = fposn (set len on exit)
--      edx = fend (for loop test)
--      [?] = length
--      ch = al
--      loop until end of buffer or cr/lf found.
--from book:
--  if mt then refil/rtn-1 end if
--  ilasm()
--  if ?>fposn then res = fbuffer[nposn..?] fposn = ? else res = -1 end if
--  while 1 do
--      if mt then refill end if
--      if ch='\n' then skip('\r') exit end if
--      if ch='\r' then skip('\n') res[$] = '\n' exit end if
--      ilasm()
--      if ?<fposn then exit end if
--      tmp = fbuffer[fposn..?]
--      fposn = ?
--      if atom(res) then res = tmp 
--                   else res &= tmp end if
--  end while
--  return res
    end while
    fposns[fidx] = fposn
--  if length(r)=0 then return -1 end if
    return r
end function


-- get_text() variables:
integer i, start, option, filesize
string src
sequence res
procedure addline()
--
-- Internal routine. Bolt another line onto res.
-- All six of the variables i, start, option, src,
-- filesize, and res must be set by the callee(!!)
-- Factored out so we can call it both when we
-- find a \r or \n, and also at end of file.
-- The "(use a shared constant)" fairly obviously
-- makes multiple references to one object rather
-- than multiple such things with ref counts of 1.
--
sequence oneline
integer lend
    lend = i-1
    if option=-1 then   -- CR_STRIPPED
        if start>lend then
            oneline = ""    -- (use a shared constant)
        else
            oneline = src[start..lend]
        end if
    else
        if start>lend then
            oneline = "\n"  -- (use a shared constant)
        elsif i<=filesize then
            src[i] = '\n'   -- \r\n --> \n
            oneline = src[start..i]
        else -- eof case
            oneline = src[start..filesize]
        end if
    end if
    res = append(res,oneline)
end procedure

global function h_get_text(integer fn, integer opt) -- = -2)    -- = WHOLE_FILE)
--
--  option is one of:
--  -2: (WHOLE_FILE) get whole file as one long string, plus final '\n' if missing.
--      WHOLE_FILE (-2) leaves any embedded CR,LF,CRLF,LFCR as-is, whereas 
--      no CR are returned from the other options. WHOLE_FILE is however the
--      fastest way to read a large file (WHOLE_FILE is what p.exw uses). [DEV?]
--       (tests: "a\nb\n" from both "a\nb" and "a\nb\n")
--  -1: (CR_STRIPPED) cr-stripped lines
--       (eg: {"a","b"} from both "a\nb" and "a\nb\n")
--       (tests: {"a","b"} from "a\nb", "a\nb\n", "a\r\nb", "a\r\nb\r\n", "a\n\rb\n\r", "a\rb", "a\rb\r")
--   0: (LF_LEFT) '\n' left on lines,
--       (tests: {"a\n","b"} from "a\nb" but {"a\n","b\n"} from "a\nb\n")
--  +1: (LF_LAST) '\n' left on lines, and put on last line if missing.
--       (tests: {"a\n","b\n"} from both "a\nb" and "a\nb\n")
--
--  NB: the constants WHOLE_FILE, CR_STRIPPED, LF_LEFT, and LF_LAST are defined
--      in builtins\pgettext.e [DEV]
--
--
--  It should make no difference if fn is open in binary or text mode.
--
--integer l, ch
integer fidx, ch
integer fmode
atom fhandle
--  if not finit then initF() end if
    option = opt
    fidx = fn+1
    fmode = 0
    if finit and fidx<=length(fmodes) then
        fmode = fmodes[fidx]
    end if
    if fmode=0 or fn<=2 then
        iofatal58(fn)--"invalid file number (%d)\n"
    end if
    if not and_bits(fmode,F_READ)
    or and_bits(fmode,F_WRITE+F_DIRTY) then
        iofatal59()--"wrong file mode for attempted operation\n")
    end if

    if option<-2 or option>1 then return -1 end if

    fposns[fidx] = 1    -- unnecessary?
    fends[fidx] = 0
    frealposns[fidx] = 0

    fhandle = fhandles[fidx]
    frealposn = c_func(xSetFilePointer,{fhandle,0,NULL,FILE_BEGIN})

--DEV pDword is not thread-safe here:
    filesize = c_func(xGetFileSize,{fhandle,pDword})
    if filesize=0 then
        if option=-2 then   -- WHOLE_FILE
            return "\n"
        else
            return {}
        end if
    end if
    if peek4s(pDword) then
        -- attempt to read >4GB file!
        -- (you can read it line-by-line, byte-by-byte, or via seeks,
        --  but not load the whole thing into memory at once!)
        ?9/0
    end if

    if option=0 then    -- LF_LEFT
        src = repeat('\n',filesize)
    else
        src = repeat('\n',filesize+1)       -- add/plant "safety lf"
    end if
--DEV pDword is not thread-safe here:
    if not c_func(xReadFile,{fhandle,src,filesize,pDword,0}) then return -1 end if
--DEV what if/does ReadFile return with pDword<filesize? 
    if peek4s(pDword)=0 then return -1 end if

    frealposn = c_func(xSetFilePointer,{fhandle,0,NULL,FILE_BEGIN})

    if option!=0 then   -- not LF_LEFT
        ch = src[filesize]
        if ch='\n' then
            -- "...\n<safety\n>" ==> "...\n"
            src = src[1..-2]                -- remove that "safety lf"
        elsif ch='\r' then
            if filesize>1 and src[filesize-1]='\n' then
                -- "...\n\r<safety\n>" ==> "...\n"
                src = src[1..-3]
            else
                -- "...X\r<safety\n>" ==> "...X\n" (where X!='\n')
                src = src[1..-2]
                src[-1] = '\n'
            end if
        end if
    end if

    if option=-2 then   -- WHOLE_FILE
        return src
    end if

    res = {}
    i = 1
    start = 1
    while i<=filesize do
        ch = src[i]
        if ch='\r' or ch='\n' then
            addline()
            ch = xor_bits(ch,0b0111)    -- '\n' <==> '\r'
            i += 1
            if i<=filesize and src[i]=ch then
                i += 1
            end if
            start = i
        else
            i += 1
        end if
    end while
    if start<=filesize then
        addline()
    end if
    return res
end function

--
--integer flag
--sequence expected
--procedure test(sequence src)
--sequence res
--  res = h_get_text(src,flag)
--  if res!=expected then ?9/0 end if
--end procedure
--if 01 then
----    -2: (WHOLE_FILE) get whole file as one long string, plus final '\n' if missing.
----         (tests: "a\nb\n" from both "a\nb" and "a\nb\n")
--  flag = -2
--  expected = "a\nb\n"
--  test("a\nb")
--  test("a\nb\n")
--
----    -1: (CR_STRIPPED) cr-stripped lines
----         (eg: {"a","b"} from both "a\nb" and "a\nb\n")
----         (tests: {"a","b"} from "a\nb", "a\nb\n", "a\r\nb", "a\r\nb\r\n", "a\n\rb\n\r", "a\rb", "a\rb\r")
--  flag = -1
--  expected = {"a","b"}
--  test("a\nb")
--  test("a\nb\n")
--  test("a\r\nb\n")
--  test("a\r\nb\r")
--  test("a\r\nb")
--  test("a\r\nb\r\n")
--  test("a\n\rb\n\r")
--  test("a\rb")
--  test("a\rb\r")
--  expected = {"a",""}
--  test("a\r\r")
--  test("a\n\n")
--  test("a\n\r\n\r")
--  test("a\r\n\r\n")
--
----     0: (LF_LEFT) '\n' left on lines,
----         (tests: {"a\n","b"} from "a\nb" but {"a\n","b\n"} from "a\nb\n")
----        (it should be clear from these tests that being fussy about the
----         last line probably just complicates things unnecessarily.)
--  flag = 0
--  expected = {"a\n","b"}
--  test("a\nb")
--  expected = {"a\n","b\n"}
--  test("a\nb\n")
--  test("a\r\nb\n")
--  test("a\r\nb\r")
--  expected = {"a\n","b"}
--  test("a\r\nb")
--  expected = {"a\n","b\n"}
--  test("a\r\nb\r\n")
--  test("a\n\rb\n\r")
--  expected = {"a\n","b"}
--  test("a\rb")
--  expected = {"a\n","b\n"}
--  test("a\rb\r")
--  expected = {"a\n","\n"}
--  test("a\r\r")
--  test("a\n\n")
--  test("a\n\r\n\r")
--  test("a\r\n\r\n")
--
----    +1: (LF_LAST) '\n' left on lines, and put on last line if missing.
----         (tests: {"a\n","b\n"} from both "a\nb" and "a\nb\n")
--  flag = +1
--  expected = {"a\n","b\n"}
--  test("a\nb")
--  test("a\nb\n")
--  test("a\r\nb\n")
--  test("a\r\nb\r")
--  test("a\r\nb")
--  test("a\r\nb\r\n")
--  test("a\n\rb\n\r")
--  test("a\rb")
--  test("a\rb\r")
--  expected = {"a\n","\n"}
--  test("a\r\r")
--  test("a\n\n")
--  test("a\n\r\n\r")
--  test("a\r\n\r\n")
--end if

procedure iofatal65()
--sequence found in character string
--/**/  #ilASM{ mov al,65
--/**/          xor edi,edi     -- ep1 unused
--/**/          xor esi,esi     -- ep2 unused
--!/**/         call %opRTErn}  -- fatal error
--/**/          int3}           -- fatal error
        ?9/0
end procedure

--integer dbg
global procedure h_puts(integer fn, object x)
-- Output, to file or device fn, a single byte (atom) or sequence of bytes or string.
integer lx, k, newend, ch
object xi
--integer sssi
integer fidx
integer fmode
integer fend
atom fhandle
integer fposn

    if not finit then initF() end if
    fidx = fn+1
    if fn<=2 then
--dbg = 1
        if fn<=0 then
            iofatal58(fn)--"invalid file number (%d)\n"
        end if
        -- stdout/stderr: non-buffered output
        if not cinit then initConsole() end if
-- call clear_debug...
--      #ilASM{call %opClrDbg}
        fhandle = fhandles[fidx]    -- stdin or stderr
--      fmode = fmodes[fidx]
        if atom(x) then
--DEV character is not thread-safe here:
            poke(character,x)
            if not c_func(xWriteFile,{fhandle,character,1,pDword,0}) then
            end if
        else
            lx = length(x)
            if string(x) then
--DOH! stout/stderr have F_BINARY
                if not c_func(xWriteFile,{fhandle,x,lx,pDword,0}) then
                end if
            else
                for i=1 to lx do
                    xi = x[i]
                    if not atom(xi) then
                        iofatal65()--sequence found in character string
--                  elsif not integer(xi) then
--                      xi=floor(xi)
                    end if
--DEV try (outside loop) a single allocate/poke/write (toString?)
--DEV character is not thread-safe here:
                    poke(character,xi)
                    if not c_func(xWriteFile,{fhandle,character,1,pDword,0}) then
                    end if
--                  if and_bits(fmode,F_BINARY)=0 then
--                      if xi='\r' then
--                          if not c_func(xWriteFile,{fhandle,LF,1,pDword,0}) then
--                          end if
--                      end if
--                  end if
                end for
            end if
        end if
        return
    end if
--dbg = 2
    if fidx>length(fhandles) or fmodes[fidx]=0 then
        iofatal58(fn)--"invalid file number (%d)\n"
    end if
    fmode = fmodes[fidx]
    if not and_bits(fmode,F_WRITE) then
        iofatal59()--"wrong file mode for attempted operation\n")
    end if
    fhandle = fhandles[fidx]
    fposn = fposns[fidx]
    fend = fends[fidx]
    frealposn = frealposns[fidx]

    if not and_bits(fmode,F_DIRTY) then
        fmode = or_bits(fmode,F_DIRTY)
        fmodes[fidx] = fmode
        if fposn<=fend then          -- some (worthwhile read cache) data in buffer
--?     if fposn<fend then           -- some (worthwhile read cache) data in buffer
            frealposn -= fend
--DEV BUG: p3 is *lp*DistanceToMoveHigh, not a dword value... (use [local]pHigh)
--          frealposn = c_func(xSetFilePointer,{fhandle,remainder(frealposn,#10000000),
--                                              floor(frealposn/#10000000),FILE_BEGIN})
            frealposn = c_func(xSetFilePointer,{fhandle,remainder(frealposn,#100000000),
                                                floor(frealposn/#100000000),FILE_BEGIN})
            frealposns[fidx] = frealposn
        else
            fposn = 1
            fend = 0
        end if
    end if

    if atom(x) then
--DEV if ch=\n shouldn't we put \r\n?
        ch = and_bits(x,#FF)
        if ch='\n' then
            x = "\n"
        else
--      if not integer(x) then
--          x=floor(x)
--      end if
--dbg = 4
            if fposn>BUFFERSIZE then
                flushfidx(fidx, fmode, fhandle)
--              fmode += F_DIRTY
--              fmodes[fidx] = fmode
                fposn = 1
                fend  = 0
            end if
--if ch='\n' then
--      if fposn=1 then
--          fposn = 2
--      end if
--      fbuffers[fidx][fposn-1] = '\r'
--      fbuffers[fidx][fposn] = '\n'
--else
            fbuffers[fidx][fposn] = ch
--end if
            if fposn>fend then
                fend = fposn
                fends[fidx] = fend
            end if
            fposn += 1
            fposns[fidx] = fposn
            return
        end if
    end if
--  else
        lx = length(x)
        if string(x) then
            if and_bits(fmode,F_BINARY) then
                -- will it fit in the buffer?
                newend = fposn+lx-1
                if newend<=BUFFERSIZE then
                    fbuffers[fidx][fposn..newend] = x
                    fposn += lx
                    fposns[fidx] = fposn
                    if newend>fend then
                        fend = newend
                        fends[fidx] = fend
                    end if
                else
                    if fend then
                        flushfidx(fidx, fmode-F_DIRTY, fhandle)
                        fposn = 1
                        fend = 0
                        fends[fidx] = fend
                        fposns[fidx] = fposn
                    end if
--DEV if lx>BUFFERSIZE...
                    if not c_func(xWriteFile,{fhandle,x,lx,pDword,0}) then end if
                    frealposn += lx
                    frealposns[fidx] = frealposn
                end if
            else
                -- not binary; perform \n --> \r\n conversion
                while 1 do
                    k = find('\n',x)
                    if k=0 then exit end if
                    -- will it fit in the buffer?
                    newend = fposn+k    -- (includes space for \n --> \r\n)
                    if newend<=BUFFERSIZE then
--                      for i=fposn to k-1 do
                        for i=1 to k-1 do
                            ch = x[i]
                            fbuffers[fidx][fposn] = ch
                            fposn += 1
                        end for
                        fbuffers[fidx][fposn..fposn+1] = "\r\n"
                        fposn += 2
                        fposns[fidx] = fposn
-- removed 27/5/2013:
--                      newend += 1
                        if newend>fend then
                            fend = newend
                            fends[fidx] = fend
                        end if
--added 28/5/2013:
                        fmode = fmodes[fidx]
                        if not and_bits(fmode,F_DIRTY) then
                            fmode += F_DIRTY
                            fmodes[fidx] = fmode
                        end if

                    else
                        if fend then
--!!??!!                    fends[fidx] = BUFFERSIZE
--?? (maybe, untested)      fends[fidx] = fposn-1
--                          fends[fidx] = fposn-1
                            flushfidx(fidx, fmode-F_DIRTY, fhandle)
                            fposn = 1
                            fend = 0
                            fends[fidx] = fend
                            fposns[fidx] = fposn
                        end if
--DEV if k+2>BUFFERSIZE...
                        if not c_func(xWriteFile,{fhandle,x,k-1,pDword,0}) then end if
--DEV probably better to always leave this in the buffer...
                        if not c_func(xWriteFile,{fhandle,CRLF,2,pDword,0}) then end if
                        frealposn += k+1
                        frealposns[fidx] = frealposn
                    end if
                    x = x[k+1..lx]
                    lx = length(x)
                end while
                if lx then
                    -- last segment, no more \n to process.
                    -- will it fit in the buffer?
                    newend = fposn+lx-1
                    if newend<=BUFFERSIZE then
                        fbuffers[fidx][fposn..newend] = x
                        fposn += lx
                        fposns[fidx] = fposn
--added 28/5/2013:
                        fmode = fmodes[fidx]
                        if not and_bits(fmode,F_DIRTY) then
                            fmode += F_DIRTY
                            fmodes[fidx] = fmode
                        end if
                        
                        if newend>fend then
                            fend = newend
                            fends[fidx] = fend
                        end if
                    else
                        if fend then
--!!??!!                    fends[fidx] = BUFFERSIZE
                            flushfidx(fidx, fmode-F_DIRTY, fhandle)
                            fposn = 1
                            fend = 0
                            fends[fidx] = fend
                            fposns[fidx] = fposn
                        end if
--DEV ditto
                        if not c_func(xWriteFile,{fhandle,x,lx,pDword,0}) then end if
                        frealposn += lx
                        frealposns[fidx] = frealposn
                    end if
                end if
            end if
        else
            -- x is a dword-sequence.
            for i=1 to lx do
--sssi = i
--?sssi
                xi = x[i]
                if not atom(xi) then
                    iofatal65()--sequence found in character string
                end if
                if fposn>BUFFERSIZE then
                    fends[fidx] = BUFFERSIZE    -- must be full!
                    flushfidx(fidx, fmode, fhandle)
--                  fmode += F_DIRTY
--                  fmodes[fidx] = fmode
                    fposn = 1
                    fend = 0
                    fends[fidx] = fend
                    fposns[fidx] = fposn
                end if
                xi = and_bits(xi,#FF)
                if and_bits(fmode,F_BINARY)=0 then
                    if xi='\n' then
                        fbuffers[fidx][fposn] = '\r'
                        fposn += 1
                        if fposn>BUFFERSIZE then
                            fends[fidx] = BUFFERSIZE    -- must be full!
                            flushfidx(fidx, fmode, fhandle)
--                          fmode += F_DIRTY
--                          fmodes[fidx] = fmode
                            fposn = 1
                            fend = 0
                            fends[fidx] = fend
                            fposns[fidx] = fposn
                        end if
                    end if
                end if
                fbuffers[fidx][fposn] = xi
                fposn += 1
            end for
--sssi = 0
            fposns[fidx] = fposn
            k = fposn-1
            if k>fend then
                fends[fidx] = k
            end if
        end if
--  end if
end procedure

--DEV do not forget to add this to psym.e:
global procedure h_wrap(integer flag)
atom pMode = allocate(4)
integer Mode, blSet

    if not cinit then initConsole() end if
    if not c_func(xGetConsoleMode,{stdout,pMode}) then ?9/0 end if
    Mode = peek4u(pMode)
    blSet = (and_bits(Mode,ENABLE_WRAP_AT_EOL_OUTPUT)!=0)
--  if blSet!=flag then
    if blSet=(not flag) then -- (as "", but treats all flag!=0 as true)
        if flag=0 then
            Mode -= ENABLE_WRAP_AT_EOL_OUTPUT
        else
            Mode += ENABLE_WRAP_AT_EOL_OUTPUT
        end if
        if not c_func(xSetConsoleMode,{stdout,Mode}) then ?9/0 end if
    end if
end procedure

global procedure h_scroll(integer amount, integer top, integer bottom)
integer right
integer attributes
atom dest
atom origin

    if not cinit then initConsole() end if
    if not c_func(xGetConsoleScreenBufferInfo,{stdout, pCSBI}) then ?9/0 end if
    right = peek2u(pCSBI+CSBI_SIZEX)-1
    poke2(pSMALLRECT,{0,top-1,right,bottom-1})
    attributes = peek2u(pCSBI+CSBI_ATTR)
    poke2(pCHARINFO,{' ',attributes})
    if abs(amount)>abs(bottom-top) then
        for i=top to bottom do
            origin = (i-1)*#10000   -- a COORD of {0,i-1}
            if not c_func(xFillConsoleOutputCharacter,{stdout,' ',right,origin,pDword}) then ?9/0 end if
            if not c_func(xFillConsoleOutputAttribute,{stdout,attributes,right,origin,pDword}) then ?9/0 end if
        end for
    else
        dest = (top-1-amount)*#10000    -- a COORD of {0,top-1-amount}
        if not c_func(xScrollConsoleScreenBuffer,{stdout,pSMALLRECT,NULL,dest,pCHARINFO}) then ?9/0 end if
    end if
end procedure

global function h_text_rows(integer newrows)
integer X
atom newsize
    if newrows<1 then ?9/0 end if
    if not cinit then initConsole() end if
    if not c_func(xGetConsoleScreenBufferInfo,{stdout, pCSBI}) then ?9/0 end if
    X = peek2u(pCSBI+CSBI_SIZEX)
    newsize = newrows*#10000 + X    -- a COORD of {X,newrows)
--  if not c_func(xSetConsoleScreenBufferSize,{stdout, newsize}) then ?9/0 end if
    if c_func(xSetConsoleScreenBufferSize,{stdout, newsize}) then end if
    if not c_func(xGetConsoleScreenBufferInfo,{stdout, pCSBI}) then ?9/0 end if
    return peek2u(pCSBI+CSBI_SIZEY) 
end function    

--/*
static object TextRows(integer new_rows)
/* text_rows built-in */
{
        CONSOLE_SCREEN_BUFFER_INFO info;
        COORD newsize;

        GetConsoleScreenBufferInfo(console_output, &info);
--      line_max = info.dwSize.Y;
        col_max = info.dwSize.X;
        newsize.X = col_max;
        newsize.Y = new_rows;
        SetConsoleScreenBufferSize(console_output, newsize);
--      GetConsoleScreenBufferInfo(console_output, &info);
}

--*/
